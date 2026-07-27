(ns kotoba.http-provider-component-test
  "W5 family-2 second slice: synthetic http-v1 wasm component provider.
   Multi-step Wasmtime post sequence (two fixed-ok posts) added as deepen slice."
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

(defn- ref-ify
  "Lift nested records/variants into :ref schemas (sets of records too)."
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
                (and (vector? d) (#{:set :list} (first d)))
                [(first d) (walk (second d))]
                :else d))]
      (let [root (walk descriptor)]
        {:descriptor root :schemas @schemas}))))

(defn- http-v1-descriptors
  []
  (let [header [:record :kotoba.http/header [[:name :keyword] [:value :string]]]
        request-raw
        [:record :kotoba.http/post-request
         [[:url :string] [:headers [:set header]] [:body :string] [:timeout-ms :i64]]]
        response
        [:record :kotoba.http/response
         [[:status :i64] [:headers [:set header]] [:body :string]]]
        error
        [:record :kotoba.http/error
         [[:code :keyword] [:message :string] [:retryable :bool]]]
        result-raw
        [:variant :kotoba.http/result
         [[:ok response] [:error error]]]
        req (ref-ify request-raw)
        res (ref-ify result-raw)]
    {:request (:descriptor req)
     :result (:descriptor res)
     :schemas (merge (:schemas req) (:schemas res))}))

(deftest http-provider-rejects-wrong-shape
  (let [d (http-v1-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.http/post-request
                   [:record :kotoba.http/post-request
                    [[:url :string] [:body :string]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"http-v1's own literal request/result shape"
         (composition/package-http-provider
          (:request d) (:result d) bad)))))

(deftest http-provider-packages-and-validates
  (let [d (http-v1-descriptors)
        provider (composition/package-http-provider
                  (:request d) (:result d) (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :http/post (:capability provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "http-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest http-provider-wat-exports-post-and-fixed-body
  (let [d (http-v1-descriptors)
        wat (component-core/http-provider-wat
             {:interface "http" :function "post"}
             (:request d) (:result d) (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/http@1\|post" wat))
    (is (re-find #"i64.const 200" wat))
    ;; 104 is UTF-8 \'h\', first byte of the https:// prefix guard
    (is (re-find #"i32.const 104" wat))))

(defn- http-post-sequence-driver-wit
  "Application WIT importing http-post.post; exports scalar multi-step run.
  Interface name is `http-post` (capability contract), not bare `http`."
  []
  (str
   "package kotoba:application@1.0.0;\n\n"
   "interface types {\n"
   "  record kotoba-http-header {\n"
   "    name: string,\n"
   "    value: string,\n"
   "  }\n"
   "  record kotoba-http-post-request {\n"
   "    url: string,\n"
   "    headers: list<kotoba-http-header>,\n"
   "    body: string,\n"
   "    timeout-ms: s64,\n"
   "  }\n"
   "  record kotoba-http-response {\n"
   "    status: s64,\n"
   "    headers: list<kotoba-http-header>,\n"
   "    body: string,\n"
   "  }\n"
   "  record kotoba-http-error {\n"
   "    code: string,\n"
   "    message: string,\n"
   "    retryable: bool,\n"
   "  }\n"
   "  variant kotoba-http-result {\n"
   "    ok(kotoba-http-response),\n"
   "    error(kotoba-http-error),\n"
   "  }\n"
   "}\n\n"
   "interface http-post {\n"
   "  use types.{kotoba-http-post-request, kotoba-http-result};\n"
   "  post: func(request: kotoba-http-post-request) -> kotoba-http-result;\n"
   "}\n\n"
   "world driver {\n"
   "  import http-post;\n"
   "  export run: func() -> s64;\n"
   "}\n"))

(defn- http-post-sequence-driver-wat
  "Two posts to https:// with empty headers/body; return (status1+status2)/200.
  Canonical ABI: MAX_FLAT_RESULTS=1 so variant result uses retptr as last
  param (url ptr/len, headers ptr/len, body ptr/len, timeout i64, retptr).
  ok status sits at retptr + 8 (disc at 0, payload aligned)."
  []
  (let [mod "cm32p2|kotoba:application/http-post@1"
        export-run "cm32p2||run"
        url-bytes (vec (.getBytes "https://" "UTF-8"))
        url-ptr 8
        r1-base 64
        r2-base 160
        status-offset 8
        push-post
        (fn [ret-base]
          (str
           "    i32.const " url-ptr "\n"
           "    i32.const " (count url-bytes) "\n"
           "    i32.const 0\n"   ;; headers ptr
           "    i32.const 0\n"   ;; headers len
           "    i32.const 0\n"   ;; body ptr
           "    i32.const 0\n"   ;; body len
           "    i64.const 1000\n" ;; timeout-ms
           "    i32.const " ret-base "\n"
           "    call $post\n"))]
    (str
     "(module\n"
     "  (import \"" mod "\" \"post\""
     " (func $post (param i32 i32 i32 i32 i32 i32 i64 i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2_realloc\")\n"
     "    (param $old i32) (param $old-size i32) (param $align i32) (param $new-size i32)\n"
     "    (result i32)\n"
     "    local.get $old i32.eqz if (result i32) i32.const 256 else local.get $old end)\n"
     "  (func (export \"" export-run "\") (result i64)\n"
     "    (local $s1 i64) (local $s2 i64)\n"
     (push-post r1-base)
     "    i32.const " r1-base " i64.load offset=" status-offset " local.set $s1\n"
     (push-post r2-base)
     "    i32.const " r2-base " i64.load offset=" status-offset " local.set $s2\n"
     "    local.get $s1 local.get $s2 i64.add i64.const 200 i64.div_s)\n"
     "  (func (export \"" export-run "_post\") (param i64))\n"
     "  (func (export \"cm32p2_initialize\"))\n"
     "  (data (i32.const " url-ptr ") \"" (wat-data url-bytes) "\")\n"
     ")\n")))

(defn- package-http-post-sequence-driver
  []
  (let [dir (Files/createTempDirectory "kotoba-http-post-sequence-driver-"
                                       (make-array FileAttribute 0))
        world (.resolve dir "driver.wit")
        core (.resolve dir "driver.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "driver.component.wasm")]
    (try
      (Files/writeString world (http-post-sequence-driver-wit)
                         (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (http-post-sequence-driver-wat))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command!
       ["wasm-tools" "component" "embed" (str world) (str core)
        "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command!
       ["wasm-tools" "component" "new" (str embedded)
        "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component/v1
       :imports [:http/post]
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]]
          (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(deftest http-post-sequence-driver-closes-and-wasmtime-returns-status-sum
  "Multi-step deepen (ADR 0105): compose real http provider with a driver that
   performs two posts; Wasmtime returns (200+200)/200 = 2."
  (let [d (http-v1-descriptors)
        provider (composition/package-http-provider
                  (:request d) (:result d) (:schemas d))
        driver (package-http-post-sequence-driver)
        closed (composition/compose-closed driver [provider])
        path (Files/createTempFile "kotoba-http-post-sequence-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-closed/v1 (:format closed)))
      (is (= [:http/post] (:application-imports closed)))
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [run (shell/sh wasmtime-binary "run" "--invoke" "run()" (str path))]
        (is (zero? (:exit run)) (str "wasmtime err: " (:err run)))
        (is (= "2" (str/trim (:out run)))))
      (finally
        (Files/deleteIfExists path)))))
