(ns kotoba.object-get-stream-cm-resource-provider-component-test
  "W5 deepen ADR 0135: full Component Model `resource bytes-task` packaging
  for object get-stream (embed + component new + validate)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.component.composition :as composition]
            [kotoba.component.core :as component-core]
            [kotoba.wasm.tools :as wasm-tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- descriptors
  []
  (let [req-name :kotoba.object/get-stream-request
        req [:record req-name
             [[:binding :keyword] [:key :string]]]]
    {:req [:ref req-name]
     :schemas {req-name req}}))

(deftest cm-resource-provider-rejects-wrong-shape
  (let [req-name :kotoba.object/get-stream-request
        d (descriptors)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"binding\+key"
         (composition/package-object-get-stream-cm-resource-provider
          [:ref req-name]
          (assoc (:schemas d)
                 req-name
                 [:record req-name [[:key :string]]]))))))

(deftest cm-resource-provider-packages-and-validates
  (let [d (descriptors)
        provider (composition/package-object-get-stream-cm-resource-provider
                  (:req d) (:schemas d))
        path (Files/createTempFile "kotoba-cm-resource-validate-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (is (= :wasm-component-provider/v1 (:format provider)))
      (is (true? (:cm-resource-abi provider)))
      (is (= :own-bytes-task (:result-descriptor provider)))
      (is (= :object/get-stream (:capability provider)))
      (is (pos? (alength ^bytes (:bytes provider))))
      (Files/write path ^bytes (:bytes provider)
                   (make-array java.nio.file.OpenOption 0))
      (is (string? (wasm-tools/run-command! ["wasm-tools" "validate" (str path)])))
      (let [wit (wasm-tools/run-command! ["wasm-tools" "component" "wit" (str path)])]
        (is (str/includes? wit "resource bytes-task"))
        (is (str/includes? wit "poll-ready"))
        (is (str/includes? wit "body-len"))
        (is (str/includes? wit "get-stream")))
      (finally
        (Files/deleteIfExists path)))))

(deftest cm-resource-wat-exports-resource-ops
  (let [d (descriptors)
        entry {:name :object/get-stream :interface "object-store" :function "get-stream"}
        wat (component-core/object-get-stream-cm-resource-provider-wat
             entry (:req d) (:schemas d))]
    (is (str/includes? wat "[resource-new]bytes-task"))
    (is (str/includes? wat "[resource-rep]bytes-task"))
    (is (str/includes? wat "[resource-drop]bytes-task"))
    (is (str/includes? wat "[method]bytes-task.poll-ready"))
    (is (str/includes? wat "[method]bytes-task.body-len"))
    (is (str/includes? wat "get-stream"))))
