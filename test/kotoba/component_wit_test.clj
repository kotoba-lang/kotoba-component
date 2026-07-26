(ns kotoba.component-wit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell :as shell]
            [kotoba.component.wit :as wit])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def kir
  {:format :kotoba.kir/v4
   :exports ['invoke]
   :schemas {:app/request [:record :app/request [[:url :string]]]
             :app/result [:variant :app/result
                          [[:ok [:ref :app/request]] [:error :string]]]}
   :functions [{:name 'invoke :params ['request]
                :param-types [[:ref :app/request]] :result [:ref :app/result]
                :body '(typed-cap-call 4 [:ref :app/request]
                                        [:ref :app/result] request)}]})

(deftest emits-deterministic-closed-wit-world
  (let [a (wit/emit kir)
        b (wit/emit kir)]
    (is (= a b))
    (is (= "0.3.0" (:wasi-version a)))
    (is (= [:http/post] (:imports a)))
    (is (= ['invoke] (:exports a)))
    (is (re-find #"package kotoba:application@1.0.0" (:source a)))
    (is (re-find #"interface http-post" (:source a)))
    (is (re-find #"import http-post;" (:source a)))
    (is (re-find #"export invoke: func\(request: app-request\) -> app-result;" (:source a)))
    (is (not (re-find #"wasi:filesystem|wasi:sockets" (:source a))))))

(deftest emitted-package-is-accepted-by-the-official-wit-toolchain
  (let [path (Files/createTempFile "kotoba-component-" ".wit"
                                   (make-array FileAttribute 0))]
    (try
      (Files/writeString path (:source (wit/emit kir)) (make-array java.nio.file.OpenOption 0))
      (let [result (shell/sh "wasm-tools" "component" "embed" (str path) "--dummy" "-t")]
        (is (zero? (:exit result)) (:err result))
        (is (re-find #"component-type" (:out result))))
      (finally (Files/deleteIfExists path)))))

(deftest sealed-inline-record-exports-use-their-nominal-wit-type
  (let [record-type [:record :app/point [[:x :i64] [:visible :bool]]]
        value {:format :kotoba.kir/v4 :exports ['echo]
               :schemas {:app/point record-type}
               :functions [{:name 'echo :params ['point] :param-types [record-type]
                            :result record-type :body 'point}]}
        source (:source (wit/emit value))]
    (is (re-find #"export echo: func\(point: app-point\) -> app-point" source))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"differs from sealed schema identity"
                          (wit/emit (assoc-in value [:functions 0 :result]
                                              [:record :app/point [[:x :f64]]]))))))

(deftest rejects-unregistered-capabilities-and-name-collisions
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no WIT contract"
                        (wit/emit (assoc-in kir [:functions 0 :body]
                                           '(typed-cap-call 255 :i64 :i64 0)))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"collide"
                        (wit/emit (assoc kir :schemas
                                         {:app/a [:record :app/a [[:x :i64]]]
                                          :app.a [:record :app.a [[:x :i64]]]}))))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"export names collide"
       (wit/emit {:format :kotoba.kir/v4
                  :exports ['do-work 'do_work]
                  :schemas {}
                  :functions [{:name 'do-work :params [] :param-types []
                               :result :i64 :body 0}
                              {:name 'do_work :params [] :param-types []
                               :result :i64 :body 0}]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"parameter names collide"
       (wit/emit {:format :kotoba.kir/v4
                  :exports ['invoke]
                  :schemas {}
                  :functions [{:name 'invoke
                               :params ['item-id 'item_id]
                               :param-types [:i64 :i64]
                               :result :i64 :body 'item-id}]}))))
