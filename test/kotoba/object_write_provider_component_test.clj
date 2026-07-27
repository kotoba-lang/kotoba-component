(ns kotoba.object-write-provider-component-test
  "W5 stream-object write-path wasm packaging: synthetic dual-export provider."
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

(defn- object-write-descriptors
  []
  (let [put-raw
        [:record :kotoba.object/put-block-request
         [[:binding :keyword] [:digest :string] [:bytes :string]]]
        cas-raw
        [:record :kotoba.object/compare-and-set-ref-request
         [[:binding :keyword] [:key :string]
          [:expected [:option :string]] [:next :string]]]
        put (ref-ify put-raw)
        cas (ref-ify cas-raw)]
    {:put-req (:descriptor put)
     :put-res :bool
     :cas-req (:descriptor cas)
     :cas-res :bool
     :schemas (merge (:schemas put) (:schemas cas))}))

(deftest object-write-provider-rejects-wrong-shape
  (let [d (object-write-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.object/put-block-request
                   [:record :kotoba.object/put-block-request
                    [[:binding :keyword]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"stream-object write-path shapes"
         (composition/package-object-write-provider
          (:put-req d) (:put-res d) (:cas-req d) (:cas-res d) bad)))))

(deftest object-write-provider-packages-and-validates
  (let [d (object-write-descriptors)
        provider (composition/package-object-write-provider
                  (:put-req d) (:put-res d)
                  (:cas-req d) (:cas-res d)
                  (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :object/put-block (:capability provider)))
    (is (= [:object/put-block :object/compare-and-set-ref]
           (:capabilities provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "object-write-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest object-write-provider-wat-exports-both-and-returns-true
  (let [d (object-write-descriptors)
        wat (component-core/object-write-provider-wat
             {:interface "object-store" :function "put-block"}
             {:interface "object-store" :function "compare-and-set-ref"}
             (:put-req d) (:put-res d)
             (:cas-req d) (:cas-res d)
             (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/object-store@1\|put-block" wat))
    (is (re-find #"cm32p2\|kotoba:application/object-store@1\|compare-and-set-ref" wat))
    (is (re-find #"i32.const 1\)" wat)) ;; always-true
    (is (re-find #"i32.const 2 i32.ge_u" wat)))) ;; option disc bound
