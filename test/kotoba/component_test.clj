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

(def aggregate-match-kir
  {:format :kotoba.kir/v4
   :exports ['choose-point 'point-x]
   :schemas
   {:demo/point
    [:record :demo/point [[:x :i64] [:state [:ref :demo/state]]]]
    :demo/state
    [:record :demo/state [[:visible :bool]]]}
   :effects #{}
   :functions
   [{:name 'choose-point
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/point]] value fallback point
             (if (record-get (record-get point :state) :visible)
               (+ (record-get point :x) 1)
               (record-get point :x)))}
    {:name 'point-x
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/point]] value fallback point
             (record-get point :x))}]})

(deftest aggregate-union-match-decodes-only-the-selected-record
  (let [world (wit/emit aggregate-match-kir)
        core-bytes (core/emit aggregate-match-kir :wasm32-wasi-kotoba-v1)
        component (artifact/package core-bytes aggregate-match-kir world)
        component-path (Files/createTempFile
                        "kotoba-component-aggregate-match-" ".wasm"
                        (make-array FileAttribute 0))
        core-path (Files/createTempFile
                   "kotoba-component-aggregate-match-core-" ".wasm"
                   (make-array FileAttribute 0))]
    (try
      (Files/write component-path ^bytes (:bytes component)
                   (make-array java.nio.file.OpenOption 0))
      (Files/write core-path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module (:canonical-lowering component)))
      (doseq [[invoke expected]
              [["choose-point(none, 9)" "9"]
               ["choose-point(some({x: 7, state: {visible: true}}), 9)" "8"]
               ["choose-point(some({x: 7, state: {visible: false}}), 9)" "7"]
               ["point-x(some({x: 11, state: {visible: true}}), 9)" "11"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke
                            (str component-path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      ;; A non-canonical bool bit pattern in an inactive option payload is
      ;; ignored. The same bits must trap for the selected record, even when
      ;; the branch does not read that bool field.
      (let [inactive (shell/sh "wasmtime" "run" "--invoke"
                               "cm32p2||point-x" (str core-path)
                               "0" "0" "2" "9")
            active (shell/sh "wasmtime" "run" "--invoke"
                             "cm32p2||point-x" (str core-path)
                             "1" "11" "2" "9")]
        (is (zero? (:exit inactive)) (:err inactive))
        (is (= "9" (str/trim (:out inactive))))
        (is (not (zero? (:exit active)))))
      (finally
        (Files/deleteIfExists component-path)
        (Files/deleteIfExists core-path)))))

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

(deftest structural-union-record-payloads-round-trip
  (let [point [:ref :demo/point]
        message [:ref :demo/message]
        outer [:ref :demo/outer]
        option-point [:option point]
        option-outer [:option outer]
        option-string [:option :string]
        option-list [:option :vector-i64]
        option-f64-list [:option :vector-f64]
        option-option [:option [:option :i64]]
        option-result [:option [:result :string :bool]]
        result-message [:result message :bool]
        schemas {:demo/point
                 [:record :demo/point [[:x :i64] [:visible :bool]]]
                 :demo/message
                 [:record :demo/message [[:topic :keyword] [:text :string]]]
                 :demo/inner
                 [:record :demo/inner [[:label :string] [:enabled :bool]]]
                 :demo/outer
                 [:record :demo/outer
                  [[:id :i64] [:inner [:ref :demo/inner]]]]}
        cases [{:descriptor option-point
                :calls [["echo(none)" "none"]
                        ["echo(some({x: 7, visible: true}))"
                         "some({x: 7, visible: true})"]]
                :core-check {:inactive ["0" "0" "2"]
                             :active ["1" "7" "2"]}}
               {:descriptor option-outer
                :calls [["echo(none)" "none"]
                        ["echo(some({id: 9, inner: {label: \"hi\", enabled: true}}))"
                         "some({id: 9, inner: {label: \"hi\", enabled: true}})"]]
                :core-check {:inactive ["0" "0" "0" "0" "2"]
                             :active ["1" "9" "0" "0" "2"]}}
               {:descriptor option-string
                :calls [["echo(none)" "none"]
                        ["echo(some(\"hello\"))" "some(\"hello\")"]]
                :core-check {:inactive ["0" "0" "65537"]
                             :active ["1" "0" "65537"]}}
               {:descriptor option-list
                :calls [["echo(none)" "none"]
                        ["echo(some([1, -2, 3]))" "some([1, -2, 3])"]]
                :core-check {:inactive ["0" "1" "16385"]
                             :active ["1" "1" "16385"]}}
               {:descriptor option-f64-list
                :calls [["echo(none)" "none"]
                        ["echo(some([1.5, -2.25]))"
                         "some([1.5, -2.25])"]]
                :core-check {:inactive ["0" "1" "16385"]
                             :active ["1" "1" "16385"]}}
               {:descriptor option-option
                :calls [["echo(none)" "none"]
                        ["echo(some(none))" "some(none)"]
                        ["echo(some(some(7)))" "some(some(7))"]]
                :core-check {:inactive ["0" "2" "0"]
                             :active ["1" "2" "0"]}}
               {:descriptor option-result
                :calls [["echo(none)" "none"]
                        ["echo(some(ok(\"hello\")))"
                         "some(ok(\"hello\"))"]
                        ["echo(some(err(true)))"
                         "some(err(true))"]]
                :core-check {:inactive ["0" "2" "0" "65537"]
                             :active ["1" "2" "0" "0"]}
                :extra-core-checks
                [{:args ["1" "0" "0" "65537"] :trap? true}
                 {:args ["1" "1" "2" "65537"] :trap? true}
                 {:args ["1" "1" "1" "65537"] :trap? false}]}
               {:descriptor result-message
                :calls [["echo(ok({topic: \"demo\", text: \"hello\"}))"
                         "ok({topic: \"demo\", text: \"hello\"})"]
                        ["echo(err(true))" "err(true)"]]}]]
    (doseq [{:keys [descriptor calls core-check extra-core-checks]} cases]
      (let [kir {:format :kotoba.kir/v4
                 :exports ['echo]
                 :schemas schemas
                 :effects #{}
                 :functions
                 [{:name 'echo :params ['value] :param-types [descriptor]
                   :result descriptor :effects #{} :body 'value}]}
            world (wit/emit kir)
            core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
            component (artifact/package core-bytes kir world)
            path (Files/createTempFile
                  "kotoba-component-union-record-" ".wasm"
                  (make-array FileAttribute 0))
            core-path (Files/createTempFile
                       "kotoba-component-union-record-core-" ".wasm"
                       (make-array FileAttribute 0))]
        (try
          (is (= :structural-union-identity
                 (:canonical-lowering component)))
          (Files/write path ^bytes (:bytes component)
                       (make-array java.nio.file.OpenOption 0))
          (Files/write core-path ^bytes core-bytes
                       (make-array java.nio.file.OpenOption 0))
          (doseq [[invoke expected] calls]
            (let [run (shell/sh "wasmtime" "run" "--invoke" invoke (str path))]
              (is (zero? (:exit run)) (:err run))
              (is (= expected (str/trim (:out run))) invoke)))
          (when core-check
            (let [inactive-malformed
                  (apply shell/sh "wasmtime" "run" "--invoke" "cm32p2||echo"
                         (str core-path) (:inactive core-check))
                  active-malformed
                  (apply shell/sh "wasmtime" "run" "--invoke" "cm32p2||echo"
                         (str core-path) (:active core-check))]
              (is (zero? (:exit inactive-malformed))
                  "inactive joined payload storage must not be inspected")
              (is (not (zero? (:exit active-malformed)))
                  "the selected case must validate its payload leaves")))
          (doseq [{:keys [args trap?]} extra-core-checks]
            (let [run (apply shell/sh "wasmtime" "run" "--invoke"
                             "cm32p2||echo" (str core-path) args)]
              (if trap?
                (is (not (zero? (:exit run)))
                    "the selected nested leaf must reject malformed input")
                (is (zero? (:exit run))
                    "an inactive nested joined slot must remain uninterpreted"))))
          (finally
            (Files/deleteIfExists path)
            (Files/deleteIfExists core-path)))))
    (doseq [[descriptor schemas]
            [[[:result [:vector :i64] :bool] {}]
             [[:option [:list :bool]] {}]
             [[:option [:list [:list :i64]]] {}]
             [[:option [:ref :demo/node]]
              {:demo/node
               [:record :demo/node [[:next [:ref :demo/node]]]]}]]]
      (let [kir {:format :kotoba.kir/v4 :exports ['echo]
                 :schemas schemas :effects #{}
                 :functions
                 [{:name 'echo :params ['value] :param-types [descriptor]
                   :result descriptor :effects #{} :body 'value}]}]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"no qualified Canonical lowering"
             (core/emit kir :wasm32-wasi-kotoba-v1)))))))

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
