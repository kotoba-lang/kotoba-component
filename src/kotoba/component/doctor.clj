(ns kotoba.component.doctor
  "Toolchain preflight for the component pipeline.

  ## Why this exists

  This repository pins three external binaries. Until now, a machine missing
  them failed in two unhelpful ways:

  - the wrong `wasm-tools` produced 107 identical `wasm-tools version is not
    pinned` errors, one per affected test; and
  - an absent `wasmtime` or `wac` produced raw
    `java.io.IOException: Cannot run program \"wasmtime\"` — 82 of them.

  Neither says \"your toolchain is wrong, here is how to fix it\". Worse, the
  suite still reported `0 failures`, because a test that cannot run does not
  fail — it errors, and a reader skimming for failures sees green. On the
  machine that prompted this, roughly three quarters of the component suite's
  assertions were silently not executing (303 of 1148), and the compiler suite
  could not complete at all.

  A preflight turns that into one actionable report.

  ## What it checks, and why the drift check matters most

  The pinned versions are declared in `component-model-v1.edn` — but nothing
  reads them. Enforcement lives in two hardcoded defs in two different
  repositories (`kotoba.wasm.tools/version`, and
  `kotoba.component.composition/wac-version`), and `:minimum-wasmtime-major`
  is enforced nowhere at all. So the contract and the code can disagree
  without anything noticing, and for wasmtime the contract was already
  decorative.

  `contract-drift` compares them. It needs no binaries installed, so it runs
  as an ordinary test."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [kotoba.component.composition :as composition]
            [kotoba.wasm.tools :as wasm-tools]))

(def contract
  (edn/read-string (slurp (io/resource "kotoba/lang/component-model-v1.edn"))))

(def pins
  "The declared toolchain, read from the contract rather than restated here."
  (let [wasi (get-in contract [:spec-baseline :wasi])]
    {:wasm-tools {:version (get-in wasi [:toolchain :wasm-tools])
                  :match :exact}
     :wac        {:version (get-in wasi [:toolchain :wac-cli])
                  :match :exact}
     :wasmtime   {:major (:minimum-wasmtime-major wasi)
                  :match :minimum-major}}))

(def install-hint
  {:wasm-tools "cargo install wasm-tools --version %s --locked"
   :wac        "cargo install wac-cli --version %s --locked"
   :wasmtime   "cargo install wasmtime-cli --locked   # any major >= %s"})

;; ---------------------------------------------------------------------------
;; pure helpers (no subprocess, so they are testable anywhere)

(defn parse-version
  "First dotted version token in a `--version` line, or nil.
  `wasm-tools 1.243.0 (abc123)` -> \"1.243.0\"."
  [output]
  (when (string? output)
    (second (re-find #"(\d+\.\d+\.\d+)" output))))

(defn major [version]
  (some-> version (str/split #"\.") first parse-long))

(defn evaluate
  "Compare one tool's reported OUTPUT against its pin. Pure."
  [tool output]
  (let [{:keys [version major match]} (get pins tool)
        actual (parse-version output)]
    (cond
      (nil? output)  {:tool tool :ok? false :reason :absent}
      (nil? actual)  {:tool tool :ok? false :reason :unparseable :actual output}
      (= :exact match)
      {:tool tool :ok? (= version actual) :reason (when-not (= version actual) :version-mismatch)
       :expected version :actual actual}
      :else
      (let [m (kotoba.component.doctor/major actual)]
        {:tool tool :ok? (and m (>= m major))
         :reason (when-not (and m (>= m major)) :below-minimum)
         :expected (str ">= " major) :actual actual}))))

(defn contract-drift
  "Pins declared in the contract vs the constants that actually enforce them.

  A mismatch here is worse than a missing binary: the build would enforce a
  version the contract does not name, so the contract would be documentation
  of something untrue."
  []
  (cond-> []
    (not= (get-in pins [:wasm-tools :version]) wasm-tools/version)
    (conj {:pin :wasm-tools
           :contract (get-in pins [:wasm-tools :version])
           :enforced-by "kotoba.wasm.tools/version"
           :enforced wasm-tools/version})

    (not= (get-in pins [:wac :version]) composition/wac-version)
    (conj {:pin :wac
           :contract (get-in pins [:wac :version])
           :enforced-by "kotoba.component.composition/wac-version"
           :enforced composition/wac-version})))

;; ---------------------------------------------------------------------------
;; effectful probe

(defn- probe
  "`<binary> --version`, or nil when the binary is absent. An absent tool is a
  finding, not an exception."
  [binary]
  (try
    (str/trim (wasm-tools/run-command! [binary "--version"]))
    (catch Exception _ nil)))

(defn check []
  (mapv (fn [tool] (evaluate tool (probe (name tool))))
        [:wasm-tools :wac :wasmtime]))

(defn report [results drift]
  (str/join
   "\n"
   (concat
    ["kotoba component toolchain preflight" ""]
    (map (fn [{:keys [tool ok? reason expected actual]}]
           (format "  %-4s %-11s %s"
                   (if ok? "ok" "FAIL")
                   (name tool)
                   (case reason
                     nil (str actual)
                     :absent "not installed"
                     :unparseable (str "unparseable --version output: " actual)
                     (format "%s (expected %s)" actual expected))))
         results)
    (when-let [bad (seq (remove :ok? results))]
      (cons "\ninstall:"
            (map (fn [{:keys [tool]}]
                   (let [{:keys [version major]} (get pins tool)]
                     (str "  " (format (install-hint tool) (str (or version major))))))
                 bad)))
    (when (seq drift)
      (cons "\nCONTRACT DRIFT (the contract names a version the code does not enforce):"
            (map (fn [{:keys [pin contract enforced-by enforced]}]
                   (format "  %s: contract %s, %s = %s"
                           (name pin) contract enforced-by enforced))
                 drift))))))

(defn -main [& _]
  (let [results (check)
        drift (contract-drift)]
    (println (report results drift))
    (if (and (every? :ok? results) (empty? drift))
      (System/exit 0)
      (System/exit 1))))
