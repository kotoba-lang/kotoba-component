(ns kotoba.clock-provider-component-test
  "W5 second slice: real clock-v1 wasm component provider (composition + validate)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools]))

(defn- ref-ify
  [variant-descriptor]
  (let [[_ variant-name cases] variant-descriptor
        schemas (atom {})
        ref-cases (mapv (fn [[tag payload]]
                          (if (and (vector? payload) (= :record (first payload)))
                            (let [[_ record-name _fields] payload]
                              (swap! schemas assoc record-name payload)
                              [tag [:ref record-name]])
                            [tag payload]))
                        cases)
        ref-variant [:variant variant-name ref-cases]]
    (swap! schemas assoc variant-name ref-variant)
    {:descriptor [:ref variant-name] :schemas @schemas}))

(defn- clock-v1-descriptors
  []
  (let [res (io/resource "kotoba/lang/capability-kits/clock-v1.edn")
        kit (if res
              (edn/read-string (slurp res))
              {:request
               [:variant :kotoba.clock/request [[:wall :bool] [:monotonic :bool]]]
               :result
               [:variant :kotoba.clock/result
                [[:wall [:record :kotoba.clock/wall
                         [[:unix-millis :i64] [:observation-sequence :i64]]]]
                 [:monotonic [:record :kotoba.clock/monotonic
                              [[:nanos :i64] [:observation-sequence :i64]]]]
                 [:error [:record :kotoba.clock/error
                          [[:code :keyword] [:message :string]]]]]]})
        request (ref-ify (:request kit))
        result (ref-ify (:result kit))]
    {:descriptor (:descriptor request)
     :result-descriptor (:descriptor result)
     :schemas (merge (:schemas request) (:schemas result))}))

(deftest clock-provider-rejects-non-clock-shape
  ;; Same structural admission as ADR 0060's state negative: demo/* fixture
  ;; is close (same case COUNT, different field TYPES) so asymmetric-variant
  ;; admits it, then clock-provider-shape rejects before any WAT is emitted.
  (let [descriptor [:ref :demo/clock-request]
        result-descriptor [:ref :demo/clock-result]
        schemas {:demo/wall [:record :demo/wall
                             [[:unix-millis :i64] [:observation-sequence :bool]]]
                 :demo/mono [:record :demo/mono
                             [[:nanos :i64] [:observation-sequence :i64]]]
                 :demo/err [:record :demo/err
                            [[:code :keyword] [:message :string]]]
                 :demo/clock-request
                 [:variant :demo/clock-request [[:wall :bool] [:monotonic :bool]]]
                 :demo/clock-result
                 [:variant :demo/clock-result
                  [[:wall [:ref :demo/wall]]
                   [:monotonic [:ref :demo/mono]]
                   [:error [:ref :demo/err]]]]}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"clock-v1's own literal request/result shape"
         (composition/package-clock-provider
          :clock/now descriptor result-descriptor schemas)))))

(deftest clock-provider-packages-and-validates
  (let [{:keys [descriptor result-descriptor schemas]} (clock-v1-descriptors)
        provider (composition/package-clock-provider
                  :clock/now descriptor result-descriptor schemas)]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :clock/now (:capability provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "clock-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest clock-provider-wat-emits-synthetic-globals
  (let [{:keys [descriptor result-descriptor schemas]} (clock-v1-descriptors)
        entry {:interface "clock" :function "now"}
        wat (component-core/clock-provider-wat
             entry descriptor result-descriptor schemas)]
    (is (string? wat))
    (is (re-find #"global \$obs" wat))
    (is (re-find #"global \$wall" wat))
    (is (re-find #"global \$mono" wat))
    (is (re-find #"1700000000000" wat))
    (is (re-find #"cm32p2\|kotoba:application/clock@1\|now" wat))))
