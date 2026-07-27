(ns kotoba.llm-provider-component-test
  "W5 remaining kit wasm packaging: synthetic llm-v1 component provider."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools]))

(defn- ref-ify
  "Lift nested records/variants into :ref schemas."
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

(defn- llm-v1-descriptors
  []
  (let [usage [:record :kotoba.llm/usage
               [[:input-tokens :i64] [:output-tokens :i64]]]
        completion [:record :kotoba.llm/completion
                    [[:text :string] [:finish-reason :keyword]
                     [:usage usage]]]
        error [:record :kotoba.llm/error
               [[:code :keyword] [:message :string] [:retryable :bool]]]
        request-raw
        [:record :kotoba.llm/generate-request
         [[:model :keyword] [:system :string] [:prompt :string]
          [:max-output-tokens :i64] [:temperature-milli :i64]]]
        result-raw
        [:variant :kotoba.llm/result
         [[:ok completion] [:error error]]]
        req (ref-ify request-raw)
        res (ref-ify result-raw)]
    {:request (:descriptor req)
     :result (:descriptor res)
     :schemas (merge (:schemas req) (:schemas res))}))

(deftest llm-provider-rejects-wrong-shape
  (let [d (llm-v1-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.llm/generate-request
                   [:record :kotoba.llm/generate-request
                    [[:model :keyword] [:prompt :string]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"llm-v1's own literal request/result shape"
         (composition/package-llm-provider
          (:request d) (:result d) bad)))))

(deftest llm-provider-packages-and-validates
  (let [d (llm-v1-descriptors)
        provider (composition/package-llm-provider
                  (:request d) (:result d) (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :llm/generate (:capability provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "llm-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest llm-provider-wat-exports-generate-and-fixed-ok
  (let [d (llm-v1-descriptors)
        wat (component-core/llm-provider-wat
             {:interface "llm" :function "generate"}
             (:request d) (:result d) (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/llm@1\|generate" wat))
    ;; fixed ok body text "ok" and finish-reason "stop"
    (is (re-find #"i32.const 2" wat)) ;; text length
    (is (re-find #"i32.const 4" wat)) ;; finish-reason length ("stop")
    ;; budget bounds: max-output-tokens 4096, temperature 2000
    (is (re-find #"i64.const 4096" wat))
    (is (re-find #"i64.const 2000" wat))))
