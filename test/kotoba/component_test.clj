(ns kotoba.component-test
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.component.core :as core]
            [kotoba.component.composition]
            [kotoba.component.admission]
            [kotoba.component.artifact :as artifact]
            [kotoba.component.wit :as wit])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Load gate: the split must not break namespace resolution. Each extracted
;; namespace must load standalone from this repo's own dependency closure.
(deftest every-extracted-namespace-loads
  (is (some? (find-ns 'kotoba.component.core)) "kotoba.component.core must load")
  (is (some? (find-ns 'kotoba.component.composition)) "kotoba.component.composition must load")
  (is (some? (find-ns 'kotoba.component.admission)) "kotoba.component.admission must load")
  (is (some? (find-ns 'kotoba.component.artifact)) "kotoba.component.artifact must load")
  (is (some? (find-ns 'kotoba.component.wit)) "kotoba.component.wit must load"))

(def multi-match-kir
  {:format :kotoba.kir/v4
   :exports ['choose-option 'choose-result 'negate]
   :schemas {}
   :effects #{}
   :functions
   [{:name 'twice :params ['value] :param-types [:i64] :result :i64
     :effects #{} :body '(* value 2)}
    {:name 'choose-option
     :params ['value 'fallback]
     :param-types [[:option :i64] :i64]
     :result :i64 :effects #{}
     :body '(option-match [:option :i64] value fallback item (twice item))}
    {:name 'choose-result
     :params ['value]
     :param-types [[:result :bool :f32]]
     :result :i64 :effects #{}
     :body '(result-match-of [:result :bool :f32] value
                            flag (if flag 1 0)
                            ratio (f32-to-bits ratio))}
    {:name 'negate :params ['flag] :param-types [:bool] :result :bool
     :effects #{} :body '(bool-not flag)}]})

(deftest multi-function-union-component-is-self-contained
  (let [world (wit/emit multi-match-kir)
        core-bytes (core/emit multi-match-kir :wasm32-wasi-kotoba-v1 {:fuel 2})
        component (artifact/package core-bytes multi-match-kir world)
        component-path (Files/createTempFile
                        "kotoba-component-package-multi-" ".wasm"
                        (make-array FileAttribute 0))
        core-path (Files/createTempFile
                   "kotoba-component-core-multi-" ".wasm"
                   (make-array FileAttribute 0))]
    (try
      (Files/write component-path ^bytes (:bytes component)
                   (make-array java.nio.file.OpenOption 0))
      (Files/write core-path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module (:canonical-lowering component)))
      (is (= ['choose-option 'choose-result 'negate] (:exports world)))
      (doseq [[invoke expected]
              [["choose-option(none, 9)" "9"]
               ["choose-option(some(7), 9)" "14"]
               ["choose-result(ok(true))" "1"]
               ["choose-result(err(-1.5))" "-1077936128"]
               ["negate(false)" "true"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke
                            (str component-path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (let [active-f32 (shell/sh "wasmtime" "run" "--invoke"
                                 "cm32p2||choose-result" (str core-path) "1" "2")
            active-bool (shell/sh "wasmtime" "run" "--invoke"
                                  "cm32p2||choose-result" (str core-path) "0" "2")
            ordinary-bool (shell/sh "wasmtime" "run" "--invoke"
                                    "cm32p2||negate" (str core-path) "2")]
        (is (zero? (:exit active-f32)) (:err active-f32))
        (is (= "2" (str/trim (:out active-f32))))
        (is (not (zero? (:exit active-bool))))
        (is (not (zero? (:exit ordinary-bool)))))
      (finally
        (Files/deleteIfExists component-path)
        (Files/deleteIfExists core-path)))))

(deftest wit-canonical-name-collisions-fail-before-packaging
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"export names collide"
       (wit/emit {:format :kotoba.kir/v4 :exports ['do-work 'do_work]
                  :schemas {}
                  :functions [{:name 'do-work :params [] :param-types []
                               :result :i64 :body 0}
                              {:name 'do_work :params [] :param-types []
                               :result :i64 :body 0}]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"parameter names collide"
       (wit/emit {:format :kotoba.kir/v4 :exports ['invoke] :schemas {}
                  :functions [{:name 'invoke
                               :params ['item-id 'item_id]
                               :param-types [:i64 :i64]
                               :result :i64 :body 'item-id}]}))))
