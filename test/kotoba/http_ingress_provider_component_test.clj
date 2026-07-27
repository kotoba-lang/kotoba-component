(ns kotoba.http-ingress-provider-component-test
  "W5 family-3 second slice: synthetic http-ingress dual-export wasm provider.
   Multi-step Wasmtime accept→none and reply true-sum sequences as deepen slices."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private wasmtime-binary
  (let [pinned (io/file ".tools" "wasmtime" "wasmtime")]
    (if (.canExecute pinned) (.getPath pinned) "wasmtime")))

(defn- ref-ify
  [descriptor]
  (let [schemas (atom {})]
    (letfn [(walk [d]
              (cond
                (and (vector? d) (= :record (first d)))
                (let [[_ name fields] d
                      walked (mapv (fn [[f t]] [f (walk t)]) fields)
                      rec [:record name walked]]
                  (swap! schemas assoc name rec)
                  [:ref name])
                (and (vector? d) (= :variant (first d)))
                (let [[_ name cases] d
                      walked (mapv (fn [[tag p]] [tag (walk p)]) cases)
                      var [:variant name walked]]
                  (swap! schemas assoc name var)
                  [:ref name])
                (and (vector? d) (#{:set :list :option} (first d)))
                (into [(first d)] (map walk (rest d)))
                :else d))]
      (let [root (walk descriptor)]
        {:descriptor root :schemas @schemas}))))

(defn- http-ingress-descriptors
  []
  (let [header [:record :kotoba.http/header [[:name :keyword] [:value :string]]]
        accept-raw [:record :kotoba.http/accept-request [[:slot :i64]]]
        incoming-raw
        [:record :kotoba.http/incoming-request
         [[:method :keyword] [:path :string]
          [:headers [:set header]] [:body :string]]]
        reply-raw
        [:record :kotoba.http/response
         [[:status :i64] [:headers [:set header]] [:body :string]]]
        accept (ref-ify accept-raw)
        incoming (ref-ify incoming-raw)
        reply (ref-ify reply-raw)
        schemas (merge (:schemas accept) (:schemas incoming) (:schemas reply))]
    {:accept-req (:descriptor accept)
     :accept-res [:option (:descriptor incoming)]
     :reply-req (:descriptor reply)
     :reply-res :bool
     :schemas schemas}))

(deftest http-ingress-provider-rejects-wrong-shape
  (let [d (http-ingress-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.http/accept-request
                   [:record :kotoba.http/accept-request [[:x :bool]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"http-ingress-v1 shapes"
         (composition/package-http-ingress-provider
          (:accept-req d) (:accept-res d)
          (:reply-req d) (:reply-res d) bad)))))

(deftest http-ingress-provider-packages-and-validates
  (let [d (http-ingress-descriptors)
        provider (composition/package-http-ingress-provider
                  (:accept-req d) (:accept-res d)
                  (:reply-req d) (:reply-res d)
                  (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :http/accept (:capability provider)))
    (is (= [:http/accept :http/reply] (:capabilities provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "http-ingress-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest http-ingress-provider-wat-exports-and-bounds
  (let [d (http-ingress-descriptors)
        wat (component-core/http-ingress-provider-wat
             {:interface "http-ingress" :function "accept"}
             {:interface "http-ingress" :function "reply"}
             (:accept-req d) (:accept-res d)
             (:reply-req d) (:reply-res d)
             (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/http-ingress@1\|accept" wat))
    (is (re-find #"cm32p2\|kotoba:application/http-ingress@1\|reply" wat))
    (is (re-find #"i64.const 100" wat))
    (is (re-find #"i64.const 599" wat))
    (is (re-find #"i32.const 1\)" wat)))) ;; reply always true

(defn- http-accept-sequence-driver-wit
  "Application WIT importing http-ingress.accept only; scalar multi-step run."
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-http-accept-request {\n"
   "    slot: s64,\n"
   "  }\n"
   "  record kotoba-http-header {\n"
   "    name: string,\n"
   "    value: string,\n"
   "  }\n"
   "  record kotoba-http-incoming-request {\n"
   "    method: string,\n"
   "    path: string,\n"
   "    headers: list<kotoba-http-header>,\n"
   "    body: string,\n"
   "  }\n"
   "}\n\n"
   "interface http-ingress {\n"
   "  use types.{kotoba-http-accept-request, kotoba-http-incoming-request};\n"
   "  accept: func(request: kotoba-http-accept-request) -> option<kotoba-http-incoming-request>;\n"
   "}\n\n"
   "world driver {\n"
   "  import http-ingress;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- http-accept-sequence-driver-wat
  "Two accept(slot=0) calls; synthetic provider always returns option none
  (disc=0). Return (1-d1)+(1-d2) = 2 when both none.
  Canonical ABI: option result uses retptr as last param (slot i64, retptr)."
  []
  (let [mod "cm32p2|kotoba:application/http-ingress@1"
        export-run "cm32p2||run"
        r1-base 64
        r2-base 128
        push-accept
        (fn [ret-base]
          (str
           "    i64.const 0\n"            ;; slot 0
           "    i32.const " ret-base "\n"
           "    call $accept\n"))]
    (str
     "(module\n"
     "  (import \"" mod "\" \"accept\""
     " (func $accept (param i64 i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz if (result i32) i32.const 256 else local.get $old end)\n"
     "  (func (export \"" export-run "\") (result i64)\n"
     "    (local $d1 i32) (local $d2 i32)\n"
     (push-accept r1-base)
     "    i32.const " r1-base " i32.load8_u offset=0 local.set $d1\n"
     (push-accept r2-base)
     "    i32.const " r2-base " i32.load8_u offset=0 local.set $d2\n"
     ;; none disc=0 → count as 1 each
     "    i32.const 1 local.get $d1 i32.sub\n"
     "    i32.const 1 local.get $d2 i32.sub\n"
     "    i32.add i64.extend_i32_s)\n"
     "  (func (export \"" export-run "_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     ")\n")))

(defn- package-http-accept-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-http-accept-sequence-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (http-accept-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (http-accept-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:http/accept]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest http-accept-sequence-driver-closes-and-wasmtime-returns-none-count
  "Multi-step deepen (ADR 0109): compose real http-ingress provider with a
   driver that performs two accepts; Wasmtime returns none-count 2."
  (let [d (http-ingress-descriptors)
        provider (composition/package-http-ingress-provider
                  (:accept-req d) (:accept-res d)
                  (:reply-req d) (:reply-res d)
                  (:schemas d))
        driver (package-http-accept-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-http-accept-sequence-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (is (= [:http/accept] (:application-imports closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "2" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))

(defn- http-reply-sequence-driver-wit
  "Application WIT importing http-ingress.reply only; scalar multi-step run."
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-http-header {\n"
   "    name: string,\n"
   "    value: string,\n"
   "  }\n"
   "  record kotoba-http-response {\n"
   "    status: s64,\n"
   "    headers: list<kotoba-http-header>,\n"
   "    body: string,\n"
   "  }\n"
   "}\n\n"
   "interface http-ingress {\n"
   "  use types.{kotoba-http-response};\n"
   "  reply: func(request: kotoba-http-response) -> bool;\n"
   "}\n\n"
   "world driver {\n"
   "  import http-ingress;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- http-reply-sequence-driver-wat
  "Two reply(status=200) calls with empty headers/body; always-true sum = 2.
  Bool is a flat i32 result (no retptr)."
  []
  (let [mod "cm32p2|kotoba:application/http-ingress@1"
        export-run "cm32p2||run"
        push-reply
        (fn []
          (str
           "    i64.const 200\n"          ;; status
           "    i32.const 0\n"            ;; headers ptr
           "    i32.const 0\n"            ;; headers len
           "    i32.const 0\n"            ;; body ptr
           "    i32.const 0\n"            ;; body len
           "    call $reply\n"))]
    (str
     "(module\n"
     "  (import \"" mod "\" \"reply\""
     " (func $reply (param i64 i32 i32 i32 i32) (result i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz if (result i32) i32.const 256 else local.get $old end)\n"
     "  (func (export \"" export-run "\") (result i64)\n"
     "    (local $a i32) (local $b i32)\n"
     (push-reply)
     "    local.set $a\n"
     (push-reply)
     "    local.set $b\n"
     "    local.get $a local.get $b i32.add i64.extend_i32_u)\n"
     "  (func (export \"" export-run "_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     ")\n")))

(defn- package-http-reply-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-http-reply-sequence-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (http-reply-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (http-reply-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:http/reply]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest http-reply-sequence-driver-closes-and-wasmtime-returns-true-sum
  "Multi-step deepen (ADR 0110): compose real http-ingress provider with a
   driver that performs two replies; Wasmtime returns true-sum 2.
   Provider primary capability is remapped to :http/reply for compose-closed
   (dual-export still packages both accept and reply)."
  (let [d (http-ingress-descriptors)
        provider (-> (composition/package-http-ingress-provider
                      (:accept-req d) (:accept-res d)
                      (:reply-req d) (:reply-res d)
                      (:schemas d))
                     (assoc :capability :http/reply))
        driver (package-http-reply-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-http-reply-sequence-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (is (= [:http/reply] (:application-imports closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "2" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))
