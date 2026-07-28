(ns kotoba.object-get-stream-resource-provider-component-test
  "W5 deepen ADR 0134: object get-stream packaging with in-module linear
  resource table (i32 handles + poll/byte-count/drop) + multi-step Wasmtime."
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

(defn- resource-descriptors
  []
  (let [req-name :kotoba.object/get-stream-request
        req [:record req-name
             [[:binding :keyword] [:key :string]]]]
    {:req [:ref req-name]
     :res :i32
     :schemas {req-name req}}))

(deftest object-get-stream-resource-provider-rejects-wrong-shape
  (let [d (resource-descriptors)
        ;; i64 result is ADR 0130 shape, not resource handles
        bad-res :i64]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"binding\+key → i32"
         (composition/package-object-get-stream-resource-provider
          (:req d) bad-res (:schemas d)))))
  (let [d (resource-descriptors)
        bad-schemas (assoc (:schemas d)
                           :kotoba.object/get-stream-request
                           [:record :kotoba.object/get-stream-request
                            [[:key :string]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"binding\+key → i32"
         (composition/package-object-get-stream-resource-provider
          (:req d) (:res d) bad-schemas)))))

(deftest object-get-stream-resource-provider-packages-and-validates
  (let [d (resource-descriptors)
        provider (composition/package-object-get-stream-resource-provider
                  (:req d) (:res d) (:schemas d))
        path (Files/createTempFile "kotoba-object-get-stream-resource-validate-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-provider/v1 (:format provider)))
      (is (= :object/get-stream (:capability provider)))
      (is (= :linear-handles (:resource-table provider)))
      (is (= [:object/get-stream] (:capabilities provider)))
      (is (pos? (alength ^bytes (:bytes provider))))
      (Files/write path ^bytes (:bytes provider)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (finally
        (Files/deleteIfExists path)))))

(deftest object-get-stream-resource-provider-wat-has-table-ops
  (let [d (resource-descriptors)
        entry {:name :object/get-stream :interface "object-store" :function "get-stream"}
        wat (component-core/object-get-stream-resource-provider-wat
             entry (:req d) (:res d) (:schemas d))]
    (is (str/includes? wat "get-stream"))
    (is (str/includes? wat "task-poll"))
    (is (str/includes? wat "task-byte-count"))
    (is (str/includes? wat "task-drop"))
    (is (str/includes? wat "next-handle"))
    (is (str/includes? wat "i64.const 2"))))

(defn- resource-sequence-driver-wit
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-object-get-stream-request {\n"
   "    binding: string,\n"
   "    key: string,\n"
   "  }\n"
   "}\n\n"
   "interface object-store {\n"
   "  use types.{kotoba-object-get-stream-request};\n"
   "  get-stream: func(request: kotoba-object-get-stream-request) -> s32;\n"
   "  task-poll: func(task: s32) -> bool;\n"
   "  task-byte-count: func(task: s32) -> s64;\n"
   "  task-drop: func(task: s32);\n"
   "}\n\n"
   "world driver {\n"
   "  import object-store;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- resource-sequence-driver-wat
  "Two get-stream → poll → byte-count → drop walks; sum of body lengths (2+2=4)."
  []
  (let [binding (.getBytes "example/blocks" "UTF-8")
        key1 (.getBytes "k1" "UTF-8")
        key2 (.getBytes "k2" "UTF-8")
        b-ptr 256
        k1-ptr 384
        k2-ptr 400
        mod "cm32p2|kotoba:application/object-store@1"]
    (str
     "(module\n"
     "  (import \"" mod "\" \"get-stream\"\n"
     "    (func $get (param i32 i32 i32 i32) (result i32)))\n"
     "  (import \"" mod "\" \"task-poll\"\n"
     "    (func $poll (param i32) (result i32)))\n"
     "  (import \"" mod "\" \"task-byte-count\"\n"
     "    (func $count (param i32) (result i64)))\n"
     "  (import \"" mod "\" \"task-drop\"\n"
     "    (func $drop (param i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz\n"
     "    if (result i32) i32.const 512 else local.get $old end)\n"
     "  (func (export \"cm32p2||run\") (result i64)\n"
     "    (local $h1 i32) (local $h2 i32) (local $a i64) (local $b i64)\n"
     "    i32.const " b-ptr " i32.const " (alength binding)
     " i32.const " k1-ptr " i32.const " (alength key1)
     " call $get local.set $h1\n"
     "    i32.const " b-ptr " i32.const " (alength binding)
     " i32.const " k2-ptr " i32.const " (alength key2)
     " call $get local.set $h2\n"
     "    local.get $h1 call $poll drop\n"
     "    local.get $h2 call $poll drop\n"
     "    local.get $h1 call $count local.set $a\n"
     "    local.get $h2 call $count local.set $b\n"
     "    local.get $h1 call $drop\n"
     "    local.get $h2 call $drop\n"
     "    local.get $a local.get $b i64.add)\n"
     "  (func (export \"cm32p2||run_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     "  (data (i32.const " b-ptr ") \"" (wat-data binding) "\")\n"
     "  (data (i32.const " k1-ptr ") \"" (wat-data key1) "\")\n"
     "  (data (i32.const " k2-ptr ") \"" (wat-data key2) "\")\n"
     ")\n")))

(defn- package-resource-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-object-get-stream-resource-seq-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (resource-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (resource-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:object/get-stream]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest object-get-stream-resource-sequence-wasmtime-sum
  "Multi-step deepen (ADR 0134): get-stream → poll → byte-count → drop twice;
   Wasmtime returns 2+2=4."
  (let [d (resource-descriptors)
        provider (composition/package-object-get-stream-resource-provider
                  (:req d) (:res d) (:schemas d))
        driver (package-resource-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-object-get-stream-resource-seq-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "4" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))

(defn- use-after-drop-driver-wit
  []
  (resource-sequence-driver-wit))

(defn- use-after-drop-driver-wat
  "get-stream → drop → byte-count (must trap)."
  []
  (let [binding (.getBytes "example/blocks" "UTF-8")
        key1 (.getBytes "k1" "UTF-8")
        b-ptr 256
        k1-ptr 384
        mod "cm32p2|kotoba:application/object-store@1"]
    (str
     "(module\n"
     "  (import \"" mod "\" \"get-stream\"\n"
     "    (func $get (param i32 i32 i32 i32) (result i32)))\n"
     "  (import \"" mod "\" \"task-poll\"\n"
     "    (func $poll (param i32) (result i32)))\n"
     "  (import \"" mod "\" \"task-byte-count\"\n"
     "    (func $count (param i32) (result i64)))\n"
     "  (import \"" mod "\" \"task-drop\"\n"
     "    (func $drop (param i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz\n"
     "    if (result i32) i32.const 512 else local.get $old end)\n"
     "  (func (export \"cm32p2||run\") (result i64)\n"
     "    (local $h i32)\n"
     "    i32.const " b-ptr " i32.const " (alength binding)
     " i32.const " k1-ptr " i32.const " (alength key1)
     " call $get local.set $h\n"
     "    local.get $h call $drop\n"
     "    local.get $h call $count)\n"
     "  (func (export \"cm32p2||run_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     "  (data (i32.const " b-ptr ") \"" (wat-data binding) "\")\n"
     "  (data (i32.const " k1-ptr ") \"" (wat-data key1) "\")\n"
     ")\n")))

(defn- package-use-after-drop-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-object-get-stream-resource-uad-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (use-after-drop-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (use-after-drop-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:object/get-stream]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest object-get-stream-resource-use-after-drop-traps
  "Fail-closed: drop then byte-count must trap (non-zero wasmtime exit)."
  (let [d (resource-descriptors)
        provider (composition/package-object-get-stream-resource-provider
                  (:req d) (:res d) (:schemas d))
        driver (package-use-after-drop-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-object-get-stream-resource-uad-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (not (zero? (:exit run)))
            (str "expected trap, got exit 0 out=" (:out run) " err=" (:err run))))
      (finally
        (Files/deleteIfExists path)))))
