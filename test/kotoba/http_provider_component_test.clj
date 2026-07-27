(ns kotoba.http-provider-component-test
  "W5 family-2 second slice: synthetic http-v1 wasm component provider."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools]))

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
