(ns kotoba.component-test
  (:require [clojure.test :refer [deftest is]]
            [kotoba.component.core]
            [kotoba.component.composition]
            [kotoba.component.admission]
            [kotoba.component.artifact]
            [kotoba.component.wit]))

;; Load gate: the split must not break namespace resolution. Each extracted
;; namespace must load standalone from this repo's own dependency closure.
(deftest every-extracted-namespace-loads
  (is (some? (find-ns 'kotoba.component.core)) "kotoba.component.core must load")
  (is (some? (find-ns 'kotoba.component.composition)) "kotoba.component.composition must load")
  (is (some? (find-ns 'kotoba.component.admission)) "kotoba.component.admission must load")
  (is (some? (find-ns 'kotoba.component.artifact)) "kotoba.component.artifact must load")
  (is (some? (find-ns 'kotoba.component.wit)) "kotoba.component.wit must load"))
