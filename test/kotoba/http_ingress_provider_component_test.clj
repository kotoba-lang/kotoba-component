(ns kotoba.http-ingress-provider-component-test
  "W5 family-3 second slice: synthetic http-ingress dual-export wasm provider."
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

(defn- http-ingress-descriptors
  []
  (let [header [:record :kotoba.http/header [[:name :keyword] [:value :string]]]
        accept-raw [:record :kotoba.http/accept-request [[:slot :i64]]]
        incoming-raw
        [:record :kotoba.http/incoming-request
         [[:method :keyword] [:path :string]
          [:headers [:set header]] [:body :string]]]
        reply-raw
        [:record :kotoba.http/response
         [[:status :i64] [:headers [:set header]] [:body :string]]]
        accept (ref-ify accept-raw)
        incoming (ref-ify incoming-raw)
        reply (ref-ify reply-raw)
        schemas (merge (:schemas accept) (:schemas incoming) (:schemas reply))]
    {:accept-req (:descriptor accept)
     :accept-res [:option (:descriptor incoming)]
     :reply-req (:descriptor reply)
     :reply-res :bool
     :schemas schemas}))

(deftest http-ingress-provider-rejects-wrong-shape
  (let [d (http-ingress-descriptors)
        bad (assoc (:schemas d)
                   :kotoba.http/accept-request
                   [:record :kotoba.http/accept-request [[:x :bool]]])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"http-ingress-v1 shapes"
         (composition/package-http-ingress-provider
          (:accept-req d) (:accept-res d)
          (:reply-req d) (:reply-res d) bad)))))

(deftest http-ingress-provider-packages-and-validates
  (let [d (http-ingress-descriptors)
        provider (composition/package-http-ingress-provider
                  (:accept-req d) (:accept-res d)
                  (:reply-req d) (:reply-res d)
                  (:schemas d))]
    (is (= :wasm-component-provider/v1 (:format provider)))
    (is (= :http/accept (:capability provider)))
    (is (= [:http/accept :http/reply] (:capabilities provider)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes provider)))))
    (let [dir (java.nio.file.Files/createTempDirectory
               "http-ingress-provider-validate-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (.resolve dir "provider.component.wasm")]
      (try
        (java.nio.file.Files/write path (:bytes provider)
                                   (make-array java.nio.file.OpenOption 0))
        (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
        (finally
          (java.nio.file.Files/deleteIfExists path)
          (java.nio.file.Files/deleteIfExists dir))))))

(deftest http-ingress-provider-wat-exports-and-bounds
  (let [d (http-ingress-descriptors)
        wat (component-core/http-ingress-provider-wat
             {:interface "http-ingress" :function "accept"}
             {:interface "http-ingress" :function "reply"}
             (:accept-req d) (:accept-res d)
             (:reply-req d) (:reply-res d)
             (:schemas d))]
    (is (re-find #"cm32p2\|kotoba:application/http-ingress@1\|accept" wat))
    (is (re-find #"cm32p2\|kotoba:application/http-ingress@1\|reply" wat))
    (is (re-find #"i64.const 100" wat))
    (is (re-find #"i64.const 599" wat))
    (is (re-find #"i32.const 1\)" wat)))) ;; reply always true
