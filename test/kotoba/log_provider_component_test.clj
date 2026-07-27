(ns kotoba.log-provider-component-test
  "W5 third slice: real log-v1 dual-export wasm component provider."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools]))

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
