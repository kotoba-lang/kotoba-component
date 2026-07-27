(ns kotoba.component.artifact
  "Validated Component Model packaging for qualified Canonical ABI slices."
  (:require [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]))

(def tool-version wasm-tools/version)
(def target :wasm-component-kotoba-v1)

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :component-artifact))))

(defn- sha256 [^bytes bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn assert-qualified-slice! [kir wit]
  (let [lowering (component-core/assert-supported! kir)]
    (when (and (seq (:imports wit))
               (not (contains? #{:scalar-capability-call :record-capability-call
                                  :variant-capability-call
                                  :structural-union-capability-call
                                  :different-variant-capability-call
                                  :structural-union-match-module
                                  ;; ADR 0076 increment 1: the general lowering
                                  ;; now emits per-capability typed imports, so
                                  ;; it may carry WIT imports too.
                                  :scalar-with-capabilities
                                  :scalar-literal-capability-call
                                  :string-literal-unit-capability-call
                                  :vector-i64-identity
                                  :vector-i64-literal}
                                lowering)))
      (reject "component capability imports require Canonical provider lowering"
              {:imports (:imports wit)}))
    lowering))

(defn assert-scalar-slice!
  "Compatibility entry point retained for callers of the first artifact slice."
  [kir wit]
  (assert-qualified-slice! kir wit)
  true)

(defn package
  "Embed WIT metadata into a compatible core module and produce a validated
  Component Model binary with the pinned official toolchain."
  [core-bytes kir wit]
  (let [lowering (assert-qualified-slice! kir wit)]
    (wasm-tools/assert-version!)
    (let [dir (Files/createTempDirectory "kotoba-component-" (make-array FileAttribute 0))
        world (.resolve dir "world.wit")
        core (.resolve dir "core.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "component.wasm")]
    (try
      (Files/writeString world ^String (:source wit) (make-array java.nio.file.OpenOption 0))
      (Files/write core ^bytes core-bytes (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       (cond-> ["wasm-tools" "component" "embed" (str world) (str core)
                "--encoding" "utf8" "-o" (str embedded)]
         (:world wit) (into ["--world" (:world wit)])))
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      (let [bytes (Files/readAllBytes component)]
        (when-not (= [0 97 115 109 13 0 1 0]
                     (mapv #(bit-and (int %) 0xff) (take 8 bytes)))
          (reject "component binary preamble is invalid" {}))
        {:format :wasm-component/v1 :target (or (:target wit) target)
         :component-world (:world wit)
         :wasi-version "0.3.0"
         :bytes bytes :sha256 (sha256 bytes) :wit-sha256 (:sha256 wit)
         :imports (:imports wit) :exports (:exports wit)
         :canonical-name-encoding :component-model/standard32
         :canonical-lowering lowering
         :tool {:name :wasm-tools :version tool-version}})
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir))))))
