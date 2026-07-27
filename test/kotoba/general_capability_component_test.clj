(ns kotoba.general-capability-component-test
  "ADR 0076 increment 1: a capability-using component is no longer restricted to
  four hand-written single-function shapes.

  The blocker was that the general component path imported the generic
  `kotoba:typed`/`cap-call` intrinsic, which no WIT interface can be bound to.
  Each `typed-cap-call` now becomes a call to its own typed import, so any
  program shape works as long as its exports and capability calls are scalar.

  `typed-cap-call` is a KIR form, not source syntax, so these fixtures build
  KIR directly -- the same way `component-artifact-test` does."
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [kotoba.component.artifact :as artifact]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.component.wit :as wit])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

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

(deftest structural-union-match-module-calls-an-aggregate-capability
  (let [descriptor [:option :vector-i64]
        kir {:format :kotoba.kir/v4
             :exports ['choose 'echo]
             :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'choose
               :params ['value 'fallback]
               :param-types [descriptor :i64]
               :result :i64
               :effects #{:clock/read}
               :body
               (list 'option-match descriptor 'value 'fallback 'items
                     (list 'option-match descriptor
                           (list 'typed-cap-call
                                 clock-now descriptor descriptor
                                 (list 'option-some-of descriptor 'items))
                           'fallback 'returned
                           (list 'vector-count 'returned)))}
              {:name 'echo
               :params ['value]
               :param-types [:i64]
               :result :i64
               :effects #{}
               :body 'value}]}
        world (wit/emit kir)
        core (component-core/emit kir :wasm32-wasi-kotoba-v1)
        application (artifact/package core kir world)
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-match-aggregate-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module
             (component-core/assert-supported! kir)))
      (is (= [:clock/now] (:imports application)))
      (doseq [[invoke expected]
              [["choose(none, 9)" "9"]
               ["choose(some([]), 9)" "0"]
               ["choose(some([1, -2, 3]), 9)" "3"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (let [maximum-values (str/join "," (repeat 16384 "0"))
            maximum (shell/sh
                     "wasmtime" "run" "--invoke"
                     (str "choose(some([" maximum-values "]), 9)")
                     (str path))
            over-values (str maximum-values ",0")
            over (shell/sh
                  "wasmtime" "run" "--invoke"
                  (str "choose(some([" over-values "]), 9)")
                  (str path))]
        (is (zero? (:exit maximum)) (:err maximum))
        (is (= "16384" (str/trim (:out maximum))))
        (is (not (zero? (:exit over)))
            "the Canonical boundary rejects 16,385 list elements"))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-match-module-calls-a-f64-list-capability
  (let [descriptor [:option :vector-f64]
        kir {:format :kotoba.kir/v4
             :exports ['choose 'echo]
             :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'choose
               :params ['value 'fallback]
               :param-types [descriptor :i64]
               :result :i64
               :effects #{:clock/read}
               :body
               (list 'option-match descriptor 'value 'fallback 'items
                     (list 'option-match descriptor
                           (list 'typed-cap-call
                                 clock-now descriptor descriptor
                                 (list 'option-some-of descriptor 'items))
                           'fallback 'returned
                           (list 'vector-f64-count 'returned)))}
              {:name 'echo
               :params ['value]
               :param-types [:i64]
               :result :i64
               :effects #{}
               :body 'value}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-match-f64-list-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module
             (component-core/assert-supported! kir)))
      (is (= [:clock/now] (:imports application)))
      (doseq [[invoke expected]
              [["choose(none, 9)" "9"]
               ["choose(some([]), 9)" "0"]
               ["choose(some([1.5, -2.25, 3.0]), 9)" "3"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-result-list-match-calls-a-named-capability
  (doseq [[descriptor count-op values]
          [[[:result :vector-i64 :vector-i64]
            'vector-count "[1, -2, 3]"]
           [[:result :vector-f64 :vector-f64]
            'vector-f64-count "[1.5, -2.25, 3.0]"]]]
    (let [inner
          (fn [constructor binder]
            (list 'result-match-of descriptor
                  (list 'typed-cap-call
                        clock-now descriptor descriptor
                        (list constructor descriptor binder))
                  'returned-ok (list count-op 'returned-ok)
                  'returned-err (list count-op 'returned-err)))
          kir {:format :kotoba.kir/v4
               :exports ['choose 'echo]
               :schemas {}
               :effects #{:clock/read}
               :functions
               [{:name 'choose
                 :params ['value]
                 :param-types [descriptor]
                 :result :i64
                 :effects #{:clock/read}
                 :body
                 (list 'result-match-of descriptor 'value
                       'ok-items (inner 'result-ok-of 'ok-items)
                       'err-items (inner 'result-err-of 'err-items))}
                {:name 'echo
                 :params ['value]
                 :param-types [:i64]
                 :result :i64
                 :effects #{}
                 :body 'value}]}
          application
          (artifact/package
           (component-core/emit kir :wasm32-wasi-kotoba-v1)
           kir (wit/emit kir))
          provider
          (composition/package-structural-union-identity-provider
           :clock/now descriptor)
          closed (composition/compose-closed application [provider])
          path (Files/createTempFile
                "kotoba-result-list-match-capability-" ".wasm"
                (make-array FileAttribute 0))]
      (try
        (Files/write path ^bytes (:bytes closed)
                     (make-array java.nio.file.OpenOption 0))
        (is (= :structural-union-match-module
               (component-core/assert-supported! kir)))
        (is (= [:clock/now] (:imports application)))
        (doseq [[invoke expected]
                [[(str "choose(ok(" values "))") "3"]
                 [(str "choose(err(" values "))") "3"]
                 ["echo(11)" "11"]]]
          (let [run (shell/sh "wasmtime" "run" "--invoke"
                              invoke (str path))]
            (is (zero? (:exit run)) (:err run))
            (is (= expected (str/trim (:out run))) invoke)))
        (finally
          (Files/deleteIfExists path))))))

(deftest structural-string-like-match-calls-a-named-capability
  (doseq [[descriptor body calls]
          (for [leaf [:string :keyword]
                kind [:option :result]
                :let [descriptor (if (= kind :option)
                                   [:option leaf]
                                   [:result leaf leaf])
                      inner
                      (fn [constructor binder]
                        (if (= kind :option)
                          (list 'option-match descriptor
                                (list 'typed-cap-call
                                      clock-now descriptor descriptor
                                      (list constructor descriptor binder))
                                9 'returned
                                (list 'string-byte-length 'returned))
                          (list 'result-match-of descriptor
                                (list 'typed-cap-call
                                      clock-now descriptor descriptor
                                      (list constructor descriptor binder))
                                'returned-ok
                                (list 'string-byte-length 'returned-ok)
                                'returned-err
                                (list 'string-byte-length 'returned-err))))
                      body (if (= kind :option)
                             (list 'option-match descriptor 'value 9 'selected
                                   (inner 'option-some-of 'selected))
                             (list 'result-match-of descriptor 'value
                                   'selected-ok
                                   (inner 'result-ok-of 'selected-ok)
                                   'selected-err
                                   (inner 'result-err-of 'selected-err)))
                      calls (if (= kind :option)
                              [["choose(none)" "9"]
                               ["choose(some(\"安全\"))" "6"]]
                              [["choose(ok(\"安全\"))" "6"]
                               ["choose(err(\"abc\"))" "3"]])]]
            [descriptor body calls])]
    (let [kir {:format :kotoba.kir/v4
               :exports ['choose 'echo]
               :schemas {}
               :effects #{:clock/read}
               :functions
               [{:name 'choose
                 :params ['value]
                 :param-types [descriptor]
                 :result :i64
                 :effects #{:clock/read}
                 :body body}
                {:name 'echo
                 :params ['value]
                 :param-types [:i64]
                 :result :i64
                 :effects #{}
                 :body 'value}]}
          application
          (artifact/package
           (component-core/emit kir :wasm32-wasi-kotoba-v1)
           kir (wit/emit kir))
          provider
          (composition/package-structural-union-identity-provider
           :clock/now descriptor)
          closed (composition/compose-closed application [provider])
          path (Files/createTempFile
                "kotoba-string-like-match-capability-" ".wasm"
                (make-array FileAttribute 0))]
      (try
        (Files/write path ^bytes (:bytes closed)
                     (make-array java.nio.file.OpenOption 0))
        (is (= :structural-union-match-module
               (component-core/assert-supported! kir)))
        (is (= [:clock/now] (:imports application)))
        (doseq [[invoke expected] calls]
          (let [run (shell/sh "wasmtime" "run" "--invoke"
                              invoke (str path))]
            (is (zero? (:exit run)) (:err run))
            (is (= expected (str/trim (:out run))) invoke)))
        (finally
          (Files/deleteIfExists path))))))

(deftest structural-option-record-match-calls-a-named-capability
  (let [record-type
        [:record :demo/cap-record
         [[:x :i64] [:enabled :bool] [:weight :f32]]]
        schemas {:demo/cap-record record-type}
        descriptor [:option [:ref :demo/cap-record]]
        kir {:format :kotoba.kir/v4
             :exports ['choose 'echo]
             :schemas schemas
             :effects #{:clock/read}
             :functions
             [{:name 'choose
               :params ['value 'fallback]
               :param-types [descriptor :i64]
               :result :i64
               :effects #{:clock/read}
               :body
               (list 'option-match descriptor 'value 'fallback 'selected
                     (list 'option-match descriptor
                           (list 'typed-cap-call
                                 clock-now descriptor descriptor
                                 (list 'option-some-of descriptor 'selected))
                           'fallback 'returned
                           (list 'record-get record-type
                                 'returned :x)))}
              {:name 'echo
               :params ['value]
               :param-types [:i64]
               :result :i64
               :effects #{}
               :body 'value}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor schemas)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-option-record-match-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module
             (component-core/assert-supported! kir)))
      (is (= [:clock/now] (:imports application)))
      (doseq [[invoke expected]
              [["choose(none, 9)" "9"]
               ["choose(some({x: 7, enabled: true, weight: 1.5}), 9)" "7"]
               ["echo(11)" "11"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-bounded-lists
  (let [descriptor [:option :vector-i64]
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo
               :params ['request]
               :param-types [descriptor]
               :result descriptor
               :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        world (wit/emit kir)
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir world)
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-list-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-capability-call
             (component-core/assert-supported! kir)))
      (is (= [:clock/now] (:imports application)))
      (doseq [[invoke expected]
              [["echo(none)" "none"]
               ["echo(some([]))" "some([])"]
               ["echo(some([1, -2, 3]))" "some([1, -2, 3])"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-nested-indirect-values
  (let [descriptor [:result [:option :vector-f64] :string]
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo
               :params ['request]
               :param-types [descriptor]
               :result descriptor
               :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        world (wit/emit kir)
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir world)
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-nested-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (doseq [[invoke expected]
              [["echo(ok(none))" "ok(none)"]
               ["echo(ok(some([1.5, -2.25])))" "ok(some([1.5, -2.25]))"]
               ["echo(err(\"denied\"))" "err(\"denied\")"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-list-of-records
  (let [descriptor [:option [:list [:ref :demo/point]]]
        schemas {:demo/point
                 [:record :demo/point
                  [[:x :i64]
                   [:visible :bool]]]}
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas schemas
             :effects #{:clock/read}
             :functions
             [{:name 'echo
               :params ['request]
               :param-types [descriptor]
               :result descriptor
               :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        world (wit/emit kir)
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir world)
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor schemas)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-record-list-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-capability-call
             (component-core/assert-supported! kir)))
      (is (= [:clock/now] (:imports application)))
      (doseq [[invoke expected]
              [["echo(none)" "none"]
               ["echo(some([]))" "some([])"]
               ["echo(some([{x:7,visible:true},{x:-2,visible:false}]))"
                "some([{x: 7, visible: true}, {x: -2, visible: false}])"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-list-of-strings
  (let [descriptor [:option [:list :string]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo
               :params ['request]
               :param-types [descriptor]
               :result descriptor
               :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-string-list-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-capability-call
             (component-core/assert-supported! kir)))
      (doseq [[invoke expected]
              [["echo(none)" "none"]
               ["echo(some([]))" "some([])"]
               ["echo(some([\"hello\",\"安全\"]))"
                "some([\"hello\", \"安全\"])"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-list-of-unions
  (let [descriptor [:option [:list [:option :string]]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo :params ['request] :param-types [descriptor]
               :result descriptor :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-union-list-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-capability-call
             (component-core/assert-supported! kir)))
      (doseq [[invoke expected]
              [["echo(none)" "none"]
               ["echo(some([]))" "some([])"]
               ["echo(some([none,some(\"hello\")]))"
                "some([none, some(\"hello\")])"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-nested-lists
  (let [descriptor [:option [:list [:list :i64]]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo :params ['request] :param-types [descriptor]
               :result descriptor :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-nested-list-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-capability-call
             (component-core/assert-supported! kir)))
      (doseq [[invoke expected]
              [["echo(none)" "none"]
               ["echo(some([]))" "some([])"]
               ["echo(some([[1,2],[],[-3]]))"
                "some([[1, 2], [], [-3]])"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke"
                            invoke (str path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-union-capability-transports-maximum-list
  (let [descriptor [:option :vector-i64]
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas {}
             :effects #{:clock/read}
             :functions
             [{:name 'echo
               :params ['request]
               :param-types [descriptor]
               :result descriptor
               :effects #{:clock/read}
               :body (list 'typed-cap-call clock-now
                           descriptor descriptor 'request)}]}
        application
        (artifact/package
         (component-core/emit kir :wasm32-wasi-kotoba-v1)
         kir (wit/emit kir))
        provider
        (composition/package-structural-union-identity-provider
         :clock/now descriptor)
        closed (composition/compose-closed application [provider])
        path (Files/createTempFile
              "kotoba-aggregate-capability-max-list-" ".wasm"
              (make-array FileAttribute 0))
        values (str/join "," (repeat 16384 "0"))
        invoke (str "echo(some([" values "]))")]
    (try
      (Files/write path ^bytes (:bytes closed)
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "wasmtime" "run" "--invoke" invoke (str path))]
        (is (zero? (:exit run)) (:err run))
        (is (= (str "some([" values "])")
               (str/replace (str/trim (:out run)) " " ""))))
      (finally
        (Files/deleteIfExists path)))))

(deftest declared-fuel-still-reaches-a-capability-component
  ;; The new lowering goes through the real backend, so it must keep the
  ;; property ADR 0075 established rather than silently becoming host-only.
  (is (= :module-global (component-core/fuel-enforcement multi-function-kir)))
  (is (= :host-only (component-core/fuel-enforcement passthrough-kir))))
