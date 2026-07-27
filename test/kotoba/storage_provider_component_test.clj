(ns kotoba.storage-provider-component-test
  "W5 remaining kit wasm packaging: synthetic storage-v1 component provider."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools]))

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

(defn- storage-v1-descriptors
  []
  (let [get-r [:record :kotoba.storage/get [[:key :keyword]]]
        put-r [:record :kotoba.storage/put
               [[:key :keyword] [:value :string]
                [:expected-version [:option :i64]]]]
        del-r [:record :kotoba.storage/delete
               [[:key :keyword] [:expected-version [:option :i64]]]]
        req-raw [:variant :kotoba.storage/request
                 [[:get get-r] [:put put-r] [:delete del-r]]]
        entry [:record :kotoba.storage/entry
               [[:key :keyword] [:value :string] [:version :i64]]]
        conflict [:record :kotoba.storage/conflict
                  [[:key :keyword] [:current-version [:option :i64]]]]
        error [:record :kotoba.storage/error
               [[:code :keyword] [:message :string] [:retryable :bool]]]
        res-raw [:variant :kotoba.storage/result
                 [[:found entry] [:missing :bool] [:written entry]
                  [:deleted :bool] [:conflict conflict] [:error error]]]
        req (ref-ify req-raw)
        res (ref-ify res-raw)]
    {:request (:descriptor req)
     :result (:descriptor res)
     :schemas (merge (:schemas req) (:schemas res))}))

(deftest storage-provider-rejects-wrong-shape
  (let [d (storage-v1-descriptors)
        bad-req [:ref :demo/req]
        schemas (assoc (:schemas d)
                       :demo/req [:variant :demo/req [[:x :bool]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"storage-v1's own literal request/result shape"
         (composition/package-storage-provider
          bad-req (:result d) schemas)))))

(deftest storage-provider-packages-and-validates
  (let [d (storage-v1-descriptors)
        provider (composition/package-storage-provider
                  (:request d) (:result d) (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :storage/transact (:capability provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "storage-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest storage-provider-wat-exports-transact-missing
  (let [d (storage-v1-descriptors)
        wat (component-core/storage-provider-wat
             {:interface "storage" :function "transact"}
             (:request d) (:result d) (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/storage@1\|transact" wat))
    (is (re-find #"i32.const 1" wat)) ;; missing case disc
    (is (re-find #"i32.const 3 i32.ge_u" wat))))
