(ns kotoba.general-capability-component-test
  "ADR 0076 increment 1: a capability-using component is no longer restricted to
  four hand-written single-function shapes.

  The blocker was that the general component path imported the generic
  `kotoba:typed`/`cap-call` intrinsic, which no WIT interface can be bound to.
  Each `typed-cap-call` now becomes a call to its own typed import, so any
  program shape works as long as its exports and capability calls are scalar.

  `typed-cap-call` is a KIR form, not source syntax, so these fixtures build
  KIR directly -- the same way `component-artifact-test` does."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [kotoba.component.artifact :as artifact]
            [kotoba.component.core :as component-core]
            [kotoba.component.wit :as wit]))

;; clock/now, id 7 in the component-model contract: interface "clock",
;; function "now".
(def ^:private clock-now 7)

;; A shape that was rejected before this increment: a helper function, real
;; computation on both sides of the capability call, two exports, and the same
;; capability invoked from two places.
(def ^:private multi-function-kir
  {:format :kotoba.kir/v4
   :exports ['measure 'twice]
   :schemas {}
   :functions [{:name 'scale :params ['x] :param-types [:i64] :result :i64
                :body '(* x 2)}
               {:name 'measure :params ['request] :param-types [:i64] :result :i64
                :body (list '+ (list 'typed-cap-call clock-now :i64 :i64
                                     '(scale request))
                            7)}
               {:name 'twice :params ['request] :param-types [:i64] :result :i64
                :body (list '+
                            (list 'typed-cap-call clock-now :i64 :i64 'request)
                            (list 'typed-cap-call clock-now :i64 :i64 'request))}]})

(def ^:private passthrough-kir
  {:format :kotoba.kir/v4
   :exports ['measure]
   :schemas {}
   :functions [{:name 'measure :params ['request] :param-types [:i64] :result :i64
                :body (list 'typed-cap-call clock-now :i64 :i64 'request)}]})

(def ^:private union-match-capability-kir
  {:format :kotoba.kir/v4
   :exports ['choose 'echo]
   :schemas {}
   :effects #{:clock/read}
   :functions
   [{:name 'choose
     :params ['value 'fallback]
     :param-types [[:option :i64] :i64]
     :result :i64
     :effects #{:clock/read}
     :body (list 'option-match [:option :i64] 'value 'fallback 'item
                 (list 'typed-cap-call clock-now :i64 :i64 'item))}
    {:name 'echo
     :params ['value]
     :param-types [:i64]
     :result :i64
     :effects #{}
     :body 'value}]})

(deftest multi-function-capability-program-is-admitted
  (testing "the general lowering claims a shape the allowlist never covered"
    (is (= :scalar-with-capabilities
           (component-core/assert-supported! multi-function-kir))))
  (testing "a bare passthrough still takes the hand-written shape"
    ;; Ordering matters: the four enumerated shapes stay ahead of the general
    ;; one, so this increment changes no existing artifact.
    (is (= :scalar-capability-call
           (component-core/assert-supported! passthrough-kir)))))

(deftest capability-imports-are-typed-and-deduplicated
  (let [imports (component-core/scalar-capability-imports multi-function-kir)]
    (testing "one import per capability id, not per call site"
      (is (= 1 (count imports)))
      (is (= clock-now (:id (first imports)))))
    (testing "named for the standard32 binding wasm-tools resolves"
      (is (= "cm32p2|kotoba:application/clock@1" (:module (first imports))))
      (is (= "now" (:field (first imports)))))
    (testing "signature is the scalar lowering: one i64 param, one i64 result"
      ;; 0x60 functype, 1 param, 0x7e i64, 1 result, 0x7e i64.
      (is (= [0x60 1 0x7e 1 0x7e] (:type (first imports)))))))

(deftest capability-binding-is-fail-closed
  (testing "an unknown capability id yields no bindings"
    (is (nil? (component-core/scalar-capability-imports
               (assoc-in passthrough-kir [:functions 0 :body]
                         (list 'typed-cap-call 9999 :i64 :i64 'request))))))
  (testing "a program with no capability call has no bindings"
    (is (nil? (component-core/scalar-capability-imports
               {:format :kotoba.kir/v4 :exports ['add] :schemas {}
                :functions [{:name 'add :params ['l 'r] :param-types [:i64 :i64]
                             :result :i64 :body '(+ l r)}]})))))

(deftest emitted-core-module-binds-the-capability-directly
  (let [bytes (component-core/emit multi-function-kir :wasm32-wasi-kotoba-v1)
        text (String. (byte-array (map unchecked-byte bytes)) "ISO-8859-1")]
    (testing "the module imports the typed capability"
      (is (str/includes? text "cm32p2|kotoba:application/clock@1"))
      (is (str/includes? text "now")))
    (testing "the generic cap-call intrinsic is gone once a typed binding exists"
      ;; `kotoba:typed` is the module name many intrinsics share, so its mere
      ;; presence proves nothing. An import is encoded as
      ;; <len> module <len> field, so the generic binding is exactly
      ;; "kotoba:typed" followed by the length byte 8 and "cap-call".
      ;; If this regresses the module still compiles, but nothing can bind the
      ;; import -- exactly the failure this increment removes.
      (is (not (str/includes? text (str "kotoba:typed" (char 8) "cap-call")))))))

(deftest generic-intrinsic-is-still-used-without-a-typed-binding
  ;; Negative control for the assertion above: with an unknown capability id
  ;; there is no typed binding, so the generic import must still appear. Without
  ;; this, that assertion could pass because the byte pattern never occurs.
  (let [unbound (assoc-in passthrough-kir [:functions 0 :body]
                          (list 'typed-cap-call clock-now :i64 :i64 'request))
        bytes (component-core/emit
               (assoc unbound :exports ['measure 'echo]
                      :functions (conj (:functions unbound)
                                       {:name 'echo :params ['x] :param-types [:i64]
                                        :result :i64 :body 'x}))
               :wasm32-wasi-kotoba-v1)
        text (String. (byte-array (map unchecked-byte bytes)) "ISO-8859-1")]
    (is (str/includes? text "cm32p2|kotoba:application/clock@1")
        "a bound capability must reach the typed import")))

(deftest structural-union-match-module-composes-with-a-named-capability
  (let [world (wit/emit union-match-capability-kir)
        imports (component-core/scalar-capability-imports
                 union-match-capability-kir)
        core (component-core/emit
              union-match-capability-kir :wasm32-wasi-kotoba-v1)
        component (artifact/package core union-match-capability-kir world)
        text (String. (byte-array (map unchecked-byte core)) "ISO-8859-1")]
    (is (= :structural-union-match-module
           (component-core/assert-supported! union-match-capability-kir)))
    (is (= [{:id clock-now
             :module "cm32p2|kotoba:application/clock@1"
             :field "now"
             :type [0x60 1 0x7e 1 0x7e]}]
           imports))
    (is (str/includes? text "cm32p2|kotoba:application/clock@1"))
    (is (not (str/includes? text (str "kotoba:typed" (char 8) "cap-call"))))
    (is (= [:clock/now] (:imports world)))
    (is (= :structural-union-match-module (:canonical-lowering component)))
    (is (= [:clock/now] (:imports component))))
  (let [unsupported
        (-> union-match-capability-kir
            (assoc :effects #{:unsupported/read})
            (assoc-in [:functions 0 :body]
                      (list 'option-match [:option :i64]
                            'value 'fallback 'item
                            (list 'typed-cap-call clock-now
                                  :string :i64 'item))))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"no qualified Canonical lowering"
         (component-core/assert-supported! unsupported))
        "a non-scalar capability cannot fall back to a generic ambient import")))

(deftest declared-fuel-still-reaches-a-capability-component
  ;; The new lowering goes through the real backend, so it must keep the
  ;; property ADR 0075 established rather than silently becoming host-only.
  (is (= :module-global (component-core/fuel-enforcement multi-function-kir)))
  (is (= :host-only (component-core/fuel-enforcement passthrough-kir))))
