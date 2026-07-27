(ns kotoba.ui-provider-component-test
  "W5 remaining wasm packaging: synthetic ui-v1 dual-export component provider."
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
                (and (vector? d) (#{:set :list :option} (first d)))
                [(first d) (walk (second d))]
                :else d))]
      (let [root (walk descriptor)]
        {:descriptor root :schemas @schemas}))))

(defn- ui-v1-descriptors
  []
  (let [node [:record :kotoba.ui/node
              [[:id :keyword] [:parent [:option :keyword]]
               [:kind :keyword] [:text :string]]]
        commit-req-raw
        [:record :kotoba.ui/commit-request
         [[:base-revision :i64] [:nodes [:set node]]]]
        commit-res-raw
        [:record :kotoba.ui/commit-result
         [[:revision :i64] [:node-count :i64]]]
        event-req-raw
        [:record :kotoba.ui/event-request [[:after-revision :i64]]]
        event-raw
        [:record :kotoba.ui/event
         [[:revision :i64] [:target :keyword] [:kind :keyword] [:value :string]]]
        commit-req (ref-ify-record commit-req-raw)
        commit-res (ref-ify-record commit-res-raw)
        event-req (ref-ify-record event-req-raw)
        event (ref-ify-record event-raw)
        schemas (merge (:schemas commit-req) (:schemas commit-res)
                       (:schemas event-req) (:schemas event))]
    {:commit-req (:descriptor commit-req)
     :commit-res (:descriptor commit-res)
     :event-req (:descriptor event-req)
     :event-res [:option (:descriptor event)]
     :schemas schemas}))

(deftest ui-provider-rejects-wrong-shape
  (let [d (ui-v1-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.ui/commit-result
                   [:record :kotoba.ui/commit-result [[:revision :bool]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"ui-v1's own literal request/result shapes"
         (composition/package-ui-provider
          (:commit-req d) (:commit-res d)
          (:event-req d) (:event-res d)
          bad)))))

(deftest ui-provider-packages-and-validates
  (let [d (ui-v1-descriptors)
        provider (composition/package-ui-provider
                  (:commit-req d) (:commit-res d)
                  (:event-req d) (:event-res d)
                  (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :ui/commit (:capability provider)))
    (is (= [:ui/commit :ui/next-event] (:capabilities provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "ui-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest ui-provider-wat-exports-commit-and-event
  (let [d (ui-v1-descriptors)
        wat (component-core/ui-provider-wat
             {:interface "ui" :function "commit"}
             {:interface "ui" :function "next-event"}
             (:commit-req d) (:commit-res d)
             (:event-req d) (:event-res d)
             (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/ui@1\|commit" wat))
    (is (re-find #"cm32p2\|kotoba:application/ui@1\|next-event" wat))
    (is (re-find #"global \$rev" wat))))
