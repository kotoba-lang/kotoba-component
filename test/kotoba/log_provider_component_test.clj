(ns kotoba.log-provider-component-test
  "W5 third slice: real log-v1 dual-export wasm component provider.
   Multi-step Wasmtime append and append+read sequences driver (ADR 0102 deepen)."
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

(defn- wat-data
  [bytes]
  (apply str (map #(format "\\%02x" (bit-and (int %) 0xff)) bytes)))

(defn- ref-ify-record
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
                (and (vector? d) (#{:set :list} (first d)))
                [(first d) (walk (second d))]
                :else d))]
      (let [root (walk descriptor)]
        {:descriptor root :schemas @schemas}))))

(defn- log-v1-descriptors
  []
  (let [field [:record :kotoba.log/field [[:key :keyword] [:value :string]]]
        append-req-raw
        [:record :kotoba.log/append-request
         [[:level :keyword] [:event :keyword] [:message :string]
          [:fields [:set field]]]]
        append-res-raw
        [:record :kotoba.log/append-result [[:sequence :i64]]]
        read-req-raw
        [:record :kotoba.log/read-request
         [[:after-sequence :i64] [:limit :i64]]]
        entry [:record :kotoba.log/entry
               [[:sequence :i64] [:level :keyword] [:event :keyword]
                [:message :string] [:fields [:set field]]]]
        read-res-raw
        [:record :kotoba.log/read-result
         [[:oldest-sequence :i64] [:latest-sequence :i64] [:truncated :bool]
          [:entries [:set entry]]]]
        append-req (ref-ify-record append-req-raw)
        append-res (ref-ify-record append-res-raw)
        read-req (ref-ify-record read-req-raw)
        read-res (ref-ify-record read-res-raw)]
    {:append-req (:descriptor append-req)
     :append-res (:descriptor append-res)
     :read-req (:descriptor read-req)
     :read-res (:descriptor read-res)
     :schemas (merge (:schemas append-req) (:schemas append-res)
                     (:schemas read-req) (:schemas read-res))}))

(deftest log-provider-rejects-wrong-shape
  (let [d (log-v1-descriptors)
        bad-schemas (assoc (:schemas d)
                           (second (:append-res d))
                           [:record (second (:append-res d))
                            [[:sequence :bool]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"log-v1's own literal request/result shapes"
         (composition/package-log-provider
          (:append-req d) (:append-res d)
          (:read-req d) (:read-res d)
          bad-schemas)))))

(deftest log-provider-packages-and-validates
  (let [d (log-v1-descriptors)
        provider (composition/package-log-provider
                  (:append-req d) (:append-res d)
                  (:read-req d) (:read-res d)
                  (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :log/append (:capability provider)))
    (is (= [:log/append :log/read] (:capabilities provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "log-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest log-provider-wat-exports-append-and-read
  (let [d (log-v1-descriptors)
        wat (component-core/log-provider-wat
             {:interface "log" :function "append"}
             {:interface "log" :function "read"}
             (:append-req d) (:append-res d)
             (:read-req d) (:read-res d)
             (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/log@1\|append" wat))
    (is (re-find #"cm32p2\|kotoba:application/log@1\|read" wat))
    (is (re-find #"global \$seq" wat))
    (is (re-find #"global \$count" wat))
    (is (re-find #"global \$head" wat))))

(defn- log-append-sequence-driver-wit
  "Application WIT importing log.append only; exports scalar multi-step run."
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-log-field {\n"
   "    key: string,\n"
   "    value: string,\n"
   "  }\n"
   "  record kotoba-log-append-request {\n"
   "    level: string,\n"
   "    event: string,\n"
   "    message: string,\n"
   "    fields: list<kotoba-log-field>,\n"
   "  }\n"
   "  record kotoba-log-append-result {\n"
   "    sequence: s64,\n"
   "  }\n"
   "}\n\n"
   "interface log {\n"
   "  use types.{kotoba-log-append-request, kotoba-log-append-result};\n"
   "  append: func(request: kotoba-log-append-request) -> kotoba-log-append-result;\n"
   "}\n\n"
   "world driver {\n"
   "  import log;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- log-append-sequence-driver-wat
  "Two appends with empty field sets; return (seq2 - seq1). Canonical ABI for
  record→record with flat result [:i64] is 8 i32 params + i64 return
  (or retptr — component-new will reject if wrong)."
  []
  (let [mod "cm32p2|kotoba:application/log@1"
        export-run "cm32p2||run"
        level-bytes (vec (.getBytes "info" "UTF-8"))
        event-bytes (vec (.getBytes "boot" "UTF-8"))
        msg1-bytes (vec (.getBytes "one" "UTF-8"))
        msg2-bytes (vec (.getBytes "two" "UTF-8"))
        level-ptr 8
        event-ptr (+ level-ptr (count level-bytes))
        msg1-ptr (+ event-ptr (count event-bytes))
        msg2-ptr (+ msg1-ptr (count msg1-bytes))
        push-append
        (fn [msg-ptr msg-len]
          (str
           "    i32.const " level-ptr "\n"
           "    i32.const " (count level-bytes) "\n"
           "    i32.const " event-ptr "\n"
           "    i32.const " (count event-bytes) "\n"
           "    i32.const " msg-ptr "\n"
           "    i32.const " msg-len "\n"
           "    i32.const 0\n"   ;; fields ptr
           "    i32.const 0\n"   ;; fields len
           "    call $append\n"))]
    (str
     "(module\n"
     "  (import \"" mod "\" \"append\""
     " (func $append (param i32 i32 i32 i32 i32 i32 i32 i32) (result i64)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz if (result i32) i32.const 256 else local.get $old end)\n"
     "  (func (export \"" export-run "\") (result i64)\n"
     "    (local $s1 i64) (local $s2 i64)\n"
     (push-append msg1-ptr (count msg1-bytes))
     "    local.set $s1\n"
     (push-append msg2-ptr (count msg2-bytes))
     "    local.set $s2\n"
     "    local.get $s2 local.get $s1 i64.sub)\n"
     "  (func (export \"" export-run "_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     "  (data (i32.const " level-ptr ") \"" (wat-data level-bytes) "\")\n"
     "  (data (i32.const " event-ptr ") \"" (wat-data event-bytes) "\")\n"
     "  (data (i32.const " msg1-ptr ") \"" (wat-data msg1-bytes) "\")\n"
     "  (data (i32.const " msg2-ptr ") \"" (wat-data msg2-bytes) "\")\n"
     ")\n")))

(defn- package-log-append-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-log-append-sequence-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (log-append-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (log-append-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:log/append]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest log-append-sequence-driver-closes-and-wasmtime-advances-sequence
  (let [d (log-v1-descriptors)
        provider (composition/package-log-provider
                  (:append-req d) (:append-res d)
                  (:read-req d) (:read-res d)
                  (:schemas d))
        driver (package-log-append-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-log-append-sequence-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (is (= [:log/append] (:application-imports closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "1" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))

(defn- log-append-read-sequence-driver-wit
  "Application WIT importing log.append + log.read; multi-step ring walk."
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-log-field {\n"
   "    key: string,\n"
   "    value: string,\n"
   "  }\n"
   "  record kotoba-log-append-request {\n"
   "    level: string,\n"
   "    event: string,\n"
   "    message: string,\n"
   "    fields: list<kotoba-log-field>,\n"
   "  }\n"
   "  record kotoba-log-append-result {\n"
   "    sequence: s64,\n"
   "  }\n"
   "  record kotoba-log-read-request {\n"
   "    after-sequence: s64,\n"
   "    limit: s64,\n"
   "  }\n"
   "  record kotoba-log-entry {\n"
   "    sequence: s64,\n"
   "    level: string,\n"
   "    event: string,\n"
   "    message: string,\n"
   "    fields: list<kotoba-log-field>,\n"
   "  }\n"
   "  record kotoba-log-read-result {\n"
   "    oldest-sequence: s64,\n"
   "    latest-sequence: s64,\n"
   "    truncated: bool,\n"
   "    entries: list<kotoba-log-entry>,\n"
   "  }\n"
   "}\n\n"
   "interface log {\n"
   "  use types.{kotoba-log-append-request, kotoba-log-append-result,\n"
   "             kotoba-log-read-request, kotoba-log-read-result};\n"
   "  append: func(request: kotoba-log-append-request) -> kotoba-log-append-result;\n"
   "  read: func(request: kotoba-log-read-request) -> kotoba-log-read-result;\n"
   "}\n\n"
   "world driver {\n"
   "  import log;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- log-append-read-sequence-driver-wat
  "One append then read after-sequence 0; return latest-sequence from read
  (should equal the append sequence, typically 1).
  append: flat i64 result; read: retptr for record (latest at retptr+8)."
  []
  (let [mod "cm32p2|kotoba:application/log@1"
        export-run "cm32p2||run"
        level-bytes (vec (.getBytes "info" "UTF-8"))
        event-bytes (vec (.getBytes "boot" "UTF-8"))
        msg-bytes (vec (.getBytes "hi" "UTF-8"))
        level-ptr 8
        event-ptr (+ level-ptr (count level-bytes))
        msg-ptr (+ event-ptr (count event-bytes))
        read-ret 128
        latest-offset 8]
    (str
     "(module\n"
     "  (import \"" mod "\" \"append\""
     " (func $append (param i32 i32 i32 i32 i32 i32 i32 i32) (result i64)))\n"
     "  (import \"" mod "\" \"read\""
     " (func $read (param i64 i64 i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz if (result i32) i32.const 256 else local.get $old end)\n"
     "  (func (export \"" export-run "\") (result i64)\n"
     "    (local $seq i64)\n"
     "    i32.const " level-ptr "\n"
     "    i32.const " (count level-bytes) "\n"
     "    i32.const " event-ptr "\n"
     "    i32.const " (count event-bytes) "\n"
     "    i32.const " msg-ptr "\n"
     "    i32.const " (count msg-bytes) "\n"
     "    i32.const 0\n"
     "    i32.const 0\n"
     "    call $append\n"
     "    local.set $seq\n"
     "    i64.const 0\n"                 ;; after-sequence
     "    i64.const 8\n"                 ;; limit
     "    i32.const " read-ret "\n"
     "    call $read\n"
     "    i32.const " read-ret " i64.load offset=" latest-offset ")\n"
     "  (func (export \"" export-run "_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     "  (data (i32.const " level-ptr ") \"" (wat-data level-bytes) "\")\n"
     "  (data (i32.const " event-ptr ") \"" (wat-data event-bytes) "\")\n"
     "  (data (i32.const " msg-ptr ") \"" (wat-data msg-bytes) "\")\n"
     ")\n")))

(defn- package-log-append-read-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-log-append-read-sequence-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (log-append-read-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (log-append-read-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:log/append :log/read]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest log-append-read-sequence-driver-closes-and-wasmtime-returns-latest
  "Multi-step deepen (ADR 0111): compose dual-export log provider with a
   driver that appends once then reads; Wasmtime returns latest-sequence 1.
   Requires compose-closed dual-export subset matching."
  (let [d (log-v1-descriptors)
        provider (composition/package-log-provider
                  (:append-req d) (:append-res d)
                  (:read-req d) (:read-res d)
                  (:schemas d))
        driver (package-log-append-read-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-log-append-read-sequence-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (is (= [:log/append :log/read] (:application-imports closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "1" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))
