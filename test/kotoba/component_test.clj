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

(deftest typed-v03-clock-lowering-produces-a-standard-component
  (let [kir {:format :kotoba.kir/v4
             :exports ['main]
             :schemas {}
             :effects #{[:cap/call 7]}
             :functions [{:name 'main :params [] :param-types []
                          :result :i64 :effects #{[:cap/call 7]}
                          :body '(typed-cap-call 7 :i64 :i64 0)}]}
        opts {:typed-capability-v3? true}
        wit (wit/emit kir opts)
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1 opts)
        packaged (artifact/package core-bytes kir wit)]
    (is (= :wasm-component-kotoba-v2 (:target wit)))
    (is (= "aiueos:capability/application@0.3.0" (:world wit)))
    (is (= ["aiueos-clock-now"] (:imports wit)))
    (is (= [:poll :cancel]
           (get-in wit [:capability-transport :bytes-task :operations])))
    (is (false? (get-in wit [:capability-transport :ambient-executor])))
    (is (= :wasm-component-kotoba-v2 (:target packaged)))
    (is (= "aiueos:capability/application@0.3.0"
           (:component-world packaged)))
    (is (= [0 97 115 109 13 0 1 0]
           (mapv #(bit-and (int %) 0xff) (take 8 (:bytes packaged)))))))

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
             (if (record-get
                  [:record :demo/state [[:visible :bool]]]
                  (record-get
                   [:record :demo/point
                    [[:x :i64] [:state [:ref :demo/state]]]]
                   point :state)
                  :visible)
               (+ (record-get
                   [:record :demo/point
                    [[:x :i64] [:state [:ref :demo/state]]]]
                   point :x)
                  1)
               (record-get
                [:record :demo/point
                 [[:x :i64] [:state [:ref :demo/state]]]]
                point :x)))}
    {:name 'point-x
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/point]] value fallback point
             (record-get
              [:record :demo/point
               [[:x :i64] [:state [:ref :demo/state]]]]
              point :x))}]})

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

(def string-record-match-kir
  {:format :kotoba.kir/v4
   :exports ['label-size 'point-x]
   :schemas
   {:demo/labeled-point
    [:record :demo/labeled-point [[:label :string] [:x :i64]]]}
   :effects #{}
   :functions
   [{:name 'label-size
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/labeled-point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/labeled-point]] value fallback point
             (string-byte-length
              (record-get
               [:record :demo/labeled-point [[:label :string] [:x :i64]]]
               point :label)))}
    {:name 'point-x
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/labeled-point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/labeled-point]] value fallback point
             (record-get
              [:record :demo/labeled-point [[:label :string] [:x :i64]]]
              point :x))}]})

(deftest aggregate-match-consumes-a-selected-indirect-string-leaf
  (let [world (wit/emit string-record-match-kir)
        core-bytes (core/emit string-record-match-kir :wasm32-wasi-kotoba-v1)
        component (artifact/package core-bytes string-record-match-kir world)
        component-path (Files/createTempFile
                        "kotoba-component-indirect-string-match-" ".wasm"
                        (make-array FileAttribute 0))
        core-path (Files/createTempFile
                   "kotoba-component-indirect-string-match-core-" ".wasm"
                   (make-array FileAttribute 0))]
    (try
      (Files/write component-path ^bytes (:bytes component)
                   (make-array java.nio.file.OpenOption 0))
      (Files/write core-path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module (:canonical-lowering component)))
      (doseq [[invoke expected]
              [["label-size(none, 9)" "9"]
               ["label-size(some({label: \"安全\", x: 7}), 9)" "6"]
               ["point-x(some({label: \"ignored\", x: 11}), 9)" "11"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke
                            (str component-path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      ;; Core shape: disc, label.ptr, label.len, x, fallback. The inactive
      ;; option must not inspect malformed joined slots. Selecting the record
      ;; validates its string even when point-x never reads the label.
      (let [inactive (shell/sh "wasmtime" "run" "--invoke"
                               "cm32p2||point-x" (str core-path)
                               "0" "-1" "2" "0" "9")
            over-bound (shell/sh "wasmtime" "run" "--invoke"
                                 "cm32p2||point-x" (str core-path)
                                 "1" "0" "65537" "11" "9")
            wrapped (shell/sh "wasmtime" "run" "--invoke"
                              "cm32p2||point-x" (str core-path)
                              "1" "-1" "2" "11" "9")]
        (is (zero? (:exit inactive)) (:err inactive))
        (is (= "9" (str/trim (:out inactive))))
        (is (not (zero? (:exit over-bound))))
        (is (not (zero? (:exit wrapped)))))
      (finally
        (Files/deleteIfExists component-path)
        (Files/deleteIfExists core-path))))
  (let [escaped
        (assoc-in
         string-record-match-kir [:functions 0 :body]
         '(option-match
           [:option [:ref :demo/labeled-point]] value fallback point
           (record-get
            [:record :demo/labeled-point [[:label :string] [:x :i64]]]
            point :label)))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"no qualified Canonical lowering"
         (core/assert-supported! escaped))
        "an indirect leaf cannot escape through an unrelated scalar context")))

(def list-record-match-kir
  {:format :kotoba.kir/v4
   :exports ['item-count 'point-x 'f64-count 'item-at 'f64-at
             'item-get 'f64-get]
   :schemas
   {:demo/list-point
    [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
    :demo/f64-list
    [:record :demo/f64-list [[:items :vector-f64]]]}
   :effects #{}
   :functions
   [{:name 'item-count
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/list-point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/list-point]] value fallback point
             (vector-count
              (record-get
               [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
               point :items)))}
    {:name 'point-x
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/list-point]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/list-point]] value fallback point
             (record-get
              [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
              point :x))}
    {:name 'f64-count
     :params ['value 'fallback]
     :param-types [[:option [:ref :demo/f64-list]] :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/f64-list]] value fallback point
             (vector-f64-count
              (record-get
               [:record :demo/f64-list [[:items :vector-f64]]]
               point :items)))}
    {:name 'item-at
     :params ['value 'index 'fallback]
     :param-types [[:option [:ref :demo/list-point]] :i64 :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/list-point]] value fallback point
             (vector-at
              (record-get
               [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
               point :items)
              index))}
    {:name 'f64-at
     :params ['value 'index 'fallback]
     :param-types [[:option [:ref :demo/f64-list]] :i64 :f64]
     :result :f64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/f64-list]] value fallback point
             (vector-f64-at
              (record-get
               [:record :demo/f64-list [[:items :vector-f64]]]
               point :items)
              index))}
    {:name 'item-get
     :params ['value 'index 'missing 'none-value]
     :param-types [[:option [:ref :demo/list-point]] :i64 :i64 :i64]
     :result :i64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/list-point]] value none-value point
             (vector-get
              (record-get
               [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
               point :items)
              index missing))}
    {:name 'f64-get
     :params ['value 'index 'missing 'none-value]
     :param-types [[:option [:ref :demo/f64-list]] :i64 :f64 :f64]
     :result :f64 :effects #{}
     :body '(option-match
             [:option [:ref :demo/f64-list]] value none-value point
             (vector-f64-get
              (record-get
               [:record :demo/f64-list [[:items :vector-f64]]]
               point :items)
              index missing))}]})

(deftest aggregate-match-consumes-a-selected-indirect-list-leaf
  (let [world (wit/emit list-record-match-kir)
        core-bytes (core/emit list-record-match-kir :wasm32-wasi-kotoba-v1)
        component (artifact/package core-bytes list-record-match-kir world)
        component-path (Files/createTempFile
                        "kotoba-component-indirect-list-match-" ".wasm"
                        (make-array FileAttribute 0))
        core-path (Files/createTempFile
                   "kotoba-component-indirect-list-match-core-" ".wasm"
                   (make-array FileAttribute 0))]
    (try
      (Files/write component-path ^bytes (:bytes component)
                   (make-array java.nio.file.OpenOption 0))
      (Files/write core-path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (is (= :structural-union-match-module (:canonical-lowering component)))
      (doseq [[invoke expected]
              [["item-count(none, 9)" "9"]
               ["item-count(some({items: [1, 2, 3], x: 7}), 9)" "3"]
               ["point-x(some({items: [4, 5], x: 11}), 9)" "11"]
               ["f64-count(some({items: [1.5, 2.5]}), 9)" "2"]
               ["item-at(some({items: [10, 20, 30], x: 7}), 1, 9)" "20"]
               ["f64-at(some({items: [1.5, 2.5]}), 1, 9.0)" "2.5"]
               ["item-get(some({items: [10, 20], x: 7}), 1, 77, 9)" "20"]
               ["item-get(some({items: [10, 20], x: 7}), -1, 77, 9)" "77"]
               ["item-get(some({items: [10, 20], x: 7}), 2, 77, 9)" "77"]
               ["item-get(none, 0, 77, 9)" "9"]
               ["f64-get(some({items: [1.5, 2.5]}), 2, 3.5, 9.0)" "3.5"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke
                            (str component-path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      ;; Core shape: disc, items.ptr, items.count, x, fallback. The inactive
      ;; option is lazy; selecting it validates the list even when point-x
      ;; does not read items.
      (let [inactive (shell/sh "wasmtime" "run" "--invoke"
                               "cm32p2||point-x" (str core-path)
                               "0" "1" "-1" "0" "9")
            over-bound (shell/sh "wasmtime" "run" "--invoke"
                                 "cm32p2||point-x" (str core-path)
                                 "1" "8" "16385" "11" "9")
            unaligned (shell/sh "wasmtime" "run" "--invoke"
                                "cm32p2||point-x" (str core-path)
                                "1" "1" "1" "11" "9")
            wrapped (shell/sh "wasmtime" "run" "--invoke"
                              "cm32p2||point-x" (str core-path)
                              "1" "-8" "2" "11" "9")
            negative-index (shell/sh "wasmtime" "run" "--invoke"
                                     "cm32p2||item-at" (str core-path)
                                     "1" "8" "2" "0" "-1" "9")
            equal-index (shell/sh "wasmtime" "run" "--invoke"
                                  "cm32p2||item-at" (str core-path)
                                  "1" "8" "2" "0" "2" "9")
            invalid-list-before-fallback
            (shell/sh "wasmtime" "run" "--invoke"
                      "cm32p2||item-get" (str core-path)
                      "1" "1" "2" "0" "9" "77" "9")]
        (is (zero? (:exit inactive)) (:err inactive))
        (is (= "9" (str/trim (:out inactive))))
        (is (not (zero? (:exit over-bound))))
        (is (not (zero? (:exit unaligned))))
        (is (not (zero? (:exit wrapped))))
        (is (not (zero? (:exit negative-index))))
        (is (not (zero? (:exit equal-index))))
        (is (not (zero? (:exit invalid-list-before-fallback)))))
      (finally
        (Files/deleteIfExists component-path)
        (Files/deleteIfExists core-path))))
  (let [escaped
        (assoc-in
         list-record-match-kir [:functions 0 :body]
         '(option-match
           [:option [:ref :demo/list-point]] value fallback point
           (record-get
            [:record :demo/list-point [[:items :vector-i64] [:x :i64]]]
            point :items)))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"no qualified Canonical lowering"
         (core/assert-supported! escaped))
        "an indirect list cannot escape through an unrelated context")))

(deftest owned-vector-transforms-return-independent-canonical-lists
  (doseq [{:keys [name params param-types result body invoke expected]}
          [{:name 'drop
            :params ['items 'amount]
            :param-types [:vector-i64 :i64]
            :result :vector-i64
            :body '(vector-drop items amount)
            :invoke "drop([1, 2, 3], 1)"
            :expected "[2, 3]"}
           {:name 'assoc-item
            :params ['items 'index 'item]
            :param-types [:vector-i64 :i64 :i64]
            :result :vector-i64
            :body '(vector-assoc items index item)
            :invoke "assoc-item([1, 2, 3], 1, 9)"
            :expected "[1, 9, 3]"}
           {:name 'append
            :params ['items 'item]
            :param-types [:vector-i64 :i64]
            :result :vector-i64
            :body '(vector-conj items item)
            :invoke "append([1, 2], 3)"
            :expected "[1, 2, 3]"}
           {:name 'drop-f64
            :params ['items 'amount]
            :param-types [:vector-f64 :i64]
            :result :vector-f64
            :body '(vector-f64-drop items amount)
            :invoke "drop-f64([1.5, 2.5, 3.5], 1)"
            :expected "[2.5, 3.5]"}
           {:name 'assoc-f64
            :params ['items 'index 'item]
            :param-types [:vector-f64 :i64 :f64]
            :result :vector-f64
            :body '(vector-f64-assoc items index item)
            :invoke "assoc-f64([1.5, 2.5], 0, 9.5)"
            :expected "[9.5, 2.5]"}
           {:name 'append-f64
            :params ['items 'item]
            :param-types [:vector-f64 :f64]
            :result :vector-f64
            :body '(vector-f64-conj items item)
            :invoke "append-f64([1.5], 2.5)"
            :expected "[1.5, 2.5]"}]]
    (let [kir {:format :kotoba.kir/v4
               :exports [name] :schemas {} :effects #{}
               :functions [{:name name :params params :param-types param-types
                            :result result :effects #{} :body body}]}
          world (wit/emit kir)
          core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
          component (artifact/package core-bytes kir world)
          path (Files/createTempFile
                "kotoba-component-owned-vector-transform-" ".wasm"
                (make-array FileAttribute 0))]
      (try
        (Files/write path ^bytes (:bytes component)
                     (make-array java.nio.file.OpenOption 0))
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke (str path))]
          (is (= :owned-vector-transform (:canonical-lowering component)))
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke))
        (finally
          (Files/deleteIfExists path)))))
  ;; Direct core calls cannot populate input elements conveniently, but they
  ;; prove bounds are checked before copy/store and that maximum-size append
  ;; cannot exceed the public item ceiling.
  (doseq [{:keys [function args]}
          [{:function 'drop :args ["8" "3" "4"]}
           {:function 'assoc-item :args ["8" "3" "3" "9"]}
           {:function 'append :args ["8" "16384" "9"]}
           {:function 'append :args ["1" "1" "9"]}]]
    (let [kir (case function
                drop {:format :kotoba.kir/v4 :exports ['drop] :schemas {} :effects #{}
                      :functions [{:name 'drop :params ['items 'amount]
                                   :param-types [:vector-i64 :i64]
                                   :result :vector-i64
                                   :body '(vector-drop items amount)}]}
                assoc-item
                {:format :kotoba.kir/v4 :exports ['assoc-item] :schemas {} :effects #{}
                 :functions [{:name 'assoc-item :params ['items 'index 'item]
                              :param-types [:vector-i64 :i64 :i64]
                              :result :vector-i64
                              :body '(vector-assoc items index item)}]}
                append
                {:format :kotoba.kir/v4 :exports ['append] :schemas {} :effects #{}
                 :functions [{:name 'append :params ['items 'item]
                              :param-types [:vector-i64 :i64]
                              :result :vector-i64
                              :body '(vector-conj items item)}]})
          core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
          path (Files/createTempFile
                "kotoba-component-owned-vector-transform-core-" ".wasm"
                (make-array FileAttribute 0))]
      (try
        (Files/write path ^bytes core-bytes
                     (make-array java.nio.file.OpenOption 0))
        (let [run (apply shell/sh "wasmtime" "run" "--invoke"
                         (str "cm32p2||" (name function)) (str path) args)]
          (is (not (zero? (:exit run)))))
        (finally
          (Files/deleteIfExists path)))))
  (let [kir {:format :kotoba.kir/v4 :exports ['append] :schemas {} :effects #{}
             :functions [{:name 'append :params ['items 'item]
                          :param-types [:vector-i64 :i64]
                          :result :vector-i64
                          :body '(vector-conj items item)}]}
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-component-owned-vector-repeated-" ".wasm"
              (make-array FileAttribute 0))
        script
        (str
         "const fs=require('fs');"
         "WebAssembly.instantiate(fs.readFileSync(process.argv[1])).then(({instance})=>{"
         "const e=instance.exports,m=e.cm32p2_memory;"
         "e.cm32p2_initialize();"
         "for(let n=0;n<20000;n++){"
         "const p=e.cm32p2_realloc(0,0,8,16);"
         "const input=new BigInt64Array(m.buffer,p,2);input[0]=1n;input[1]=2n;"
         "const r=e['cm32p2||append'](p,2,3n);"
         "const area=new DataView(m.buffer);"
         "const q=area.getUint32(r,true),len=area.getUint32(r+4,true);"
         "const out=new BigInt64Array(m.buffer,q,len);"
         "if(len!==3||out[0]!==1n||out[1]!==2n||out[2]!==3n)throw Error('result');"
         "if(input[0]!==1n||input[1]!==2n)throw Error('aliased');"
         "e['cm32p2||append_post'](r);"
         "}"
         "}).catch(e=>{console.error(e);process.exit(1)})")]
    (try
      (Files/write path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "node" "-e" script (str path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists path)))))

(deftest aggregate-match-returns-fresh-owned-lists
  (let [payload [:ref :demo/items]
        descriptor [:option payload]
        schemas {:demo/items
                 [:record :demo/items
                  [[:values :vector-i64] [:enabled :bool]]]}
        kir {:format :kotoba.kir/v4
             :exports ['append-selected] :schemas schemas :effects #{}
             :functions
             [{:name 'append-selected
               :params ['value 'fallback 'item]
               :param-types [descriptor :vector-i64 :i64]
               :result :vector-i64 :effects #{}
               :body
               '(option-match
                 [:option [:ref :demo/items]] value fallback selected
                 (vector-conj
                  (record-get
                   [:record :demo/items
                    [[:values :vector-i64] [:enabled :bool]]]
                   selected :values)
                  item))}]}
        world (wit/emit kir)
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        component (artifact/package core-bytes kir world)
        component-path
        (Files/createTempFile "kotoba-component-owned-option-" ".wasm"
                              (make-array FileAttribute 0))
        core-path
        (Files/createTempFile "kotoba-component-owned-option-core-" ".wasm"
                              (make-array FileAttribute 0))]
    (try
      (Files/write component-path ^bytes (:bytes component)
                   (make-array java.nio.file.OpenOption 0))
      (Files/write core-path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (is (= :owned-vector-match (:canonical-lowering component)))
      (doseq [[invoke expected]
              [["append-selected(none, [8, 9], 4)" "[8, 9]"]
               ["append-selected(some({values: [1, 2], enabled: true}), [8], 3)"
                "[1, 2, 3]"]]]
        (let [run (shell/sh "wasmtime" "run" "--invoke" invoke
                            (str component-path))]
          (is (zero? (:exit run)) (:err run))
          (is (= expected (str/trim (:out run))) invoke)))
      ;; Joined payload slots are lazy for none, but every selected leaf is
      ;; checked for some even when the body only reads the list.
      (let [inactive
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||append-selected"
                      (str core-path) "0" "1" "16385" "2" "0" "0" "4")
            active-list
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||append-selected"
                      (str core-path) "1" "1" "16385" "1" "0" "0" "4")
            active-bool
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||append-selected"
                      (str core-path) "1" "8" "0" "2" "0" "0" "4")]
        (is (zero? (:exit inactive)) (:err inactive))
        (is (not (zero? (:exit active-list))))
        (is (not (zero? (:exit active-bool)))))
      (let [script
            (str
             "const fs=require('fs');"
             "WebAssembly.instantiate(fs.readFileSync(process.argv[1]))"
             ".then(({instance})=>{const e=instance.exports,m=e.cm32p2_memory;"
             "e.cm32p2_initialize();for(let n=0;n<20000;n++){"
             "const p=e.cm32p2_realloc(0,0,8,16);"
             "const input=new BigInt64Array(m.buffer,p,2);"
             "input[0]=1n;input[1]=2n;"
             "const r=e['cm32p2||append-selected'](1,p,2,1,0,0,3n);"
             "const area=new DataView(m.buffer);"
             "const q=area.getUint32(r,true),len=area.getUint32(r+4,true);"
             "const out=new BigInt64Array(m.buffer,q,len);"
             "if(len!==3||out[0]!==1n||out[1]!==2n||out[2]!==3n)"
             "throw Error('result');"
             "if(input[0]!==1n||input[1]!==2n)throw Error('aliased');"
             "e['cm32p2||append-selected_post'](r);}})"
             ".catch(e=>{console.error(e);process.exit(1)})")
            run (shell/sh "node" "-e" script (str core-path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists component-path)
        (Files/deleteIfExists core-path))))
  (let [descriptor [:result :vector-f64 :vector-f64]
        kir {:format :kotoba.kir/v4
             :exports ['adjust] :schemas {} :effects #{}
             :functions
             [{:name 'adjust
               :params ['value 'index 'item]
               :param-types [descriptor :i64 :f64]
               :result :vector-f64 :effects #{}
               :body
               '(result-match-of
                 [:result :vector-f64 :vector-f64] value
                 values (vector-f64-drop values index)
                 errors
                 (vector-f64-assoc
                  (vector-f64-new
                   (f64-from-bits 4616752568008179712)
                   (f64-from-bits 4617878467915022336))
                  index item))}]}
        path (Files/createTempFile "kotoba-component-owned-result-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (let [component (artifact/package
                       (core/emit kir :wasm32-wasi-kotoba-v1)
                       kir (wit/emit kir))]
        (Files/write path ^bytes (:bytes component)
                     (make-array java.nio.file.OpenOption 0))
        (doseq [[invoke expected]
                [["adjust(ok([1.5, 2.5, 3.5]), 1, 9.5)" "[2.5, 3.5]"]
                 ["adjust(err([1.5, 2.5]), 0, 9.5)" "[9.5, 5.5]"]]]
          (let [run (shell/sh "wasmtime" "run" "--invoke" invoke (str path))]
            (is (zero? (:exit run)) (:err run))
            (is (= expected (str/trim (:out run))) invoke))))
      (finally
        (Files/deleteIfExists path)))))

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
        option-bool-list [:option [:list :bool]]
        option-string-list [:option [:list :string]]
        option-option-string-list [:option [:list [:option :string]]]
        option-result-list [:option [:list [:result :string :bool]]]
        option-nested-list [:option [:list [:list :i64]]]
        option-point-list [:option [:list point]]
        option-message-list [:option [:list message]]
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
               {:descriptor option-bool-list
                :calls [["echo(none)" "none"]
                        ["echo(some([]))" "some([])"]
                        ["echo(some([true, false, true]))"
                         "some([true, false, true])"]]}
               {:descriptor option-string-list
                :calls [["echo(none)" "none"]
                        ["echo(some([]))" "some([])"]
                        ["echo(some([\"hello\", \"安全\"]))"
                         "some([\"hello\", \"安全\"])"]]}
               {:descriptor option-option-string-list
                :calls [["echo(some([none, some(\"hello\")]))"
                         "some([none, some(\"hello\")])"]]}
               {:descriptor option-result-list
                :calls [["echo(some([ok(\"hello\"), err(true)]))"
                         "some([ok(\"hello\"), err(true)])"]]}
               {:descriptor option-nested-list
                :calls [["echo(some([[1, 2], [], [-3]]))"
                         "some([[1, 2], [], [-3]])"]]}
               {:descriptor option-point-list
                :calls
                [["echo(none)" "none"]
                 ["echo(some([]))" "some([])"]
                 ["echo(some([{x: 7, visible: true}, {x: -2, visible: false}]))"
                  "some([{x: 7, visible: true}, {x: -2, visible: false}])"]]}
               {:descriptor option-message-list
                :calls
                [["echo(some([{topic: \"demo\", text: \"hello\"}]))"
                  "some([{topic: \"demo\", text: \"hello\"}])"]]}
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

(deftest structural-list-record-validates-every-active-item
  (let [point [:ref :demo/point]
        descriptor [:option [:list point]]
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas
             {:demo/point
              [:record :demo/point [[:x :i64] [:visible :bool]]]}
             :effects #{}
             :functions
             [{:name 'echo :params ['value] :param-types [descriptor]
               :result descriptor :effects #{} :body 'value}]}
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-component-list-record-validation-" ".wasm"
              (make-array FileAttribute 0))
        script
        "const fs=require('node:fs');
         WebAssembly.instantiate(fs.readFileSync(process.argv[1])).then(({instance})=>{
           const e=instance.exports;
           const p=e.cm32p2_realloc(0,0,8,16);
           new DataView(e.cm32p2_memory.buffer).setUint8(p+8,2);
           e['cm32p2||echo'](0,p,1);
           let trapped=false;
           try { e['cm32p2||echo'](1,p,1); } catch (_) { trapped=true; }
           if (!trapped) process.exit(2);
         }).catch(error=>{ console.error(error); process.exit(3); });"]
    (try
      (Files/write path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "node" "-e" script (str path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-nested-lists-share-one-total-item-budget
  (let [descriptor [:option [:list [:list :i64]]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {} :effects #{}
             :functions
             [{:name 'echo :params ['value] :param-types [descriptor]
               :result descriptor :effects #{} :body 'value}]}
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-component-nested-list-budget-" ".wasm"
              (make-array FileAttribute 0))
        script
        "const fs=require('node:fs');
         WebAssembly.instantiate(fs.readFileSync(process.argv[1])).then(({instance})=>{
           const e=instance.exports;
           const outer=e.cm32p2_realloc(0,0,4,16);
           const inner=e.cm32p2_realloc(0,0,8,8192*8);
           const view=new DataView(e.cm32p2_memory.buffer);
           for(let i=0;i<2;i++){
             view.setUint32(outer+i*8,inner,true);
             view.setUint32(outer+i*8+4,8192,true);
           }
           e['cm32p2||echo'](0,outer,2);
           let trapped=false;
           try { e['cm32p2||echo'](1,outer,2); } catch (_) { trapped=true; }
           if (!trapped) process.exit(2);
         }).catch(error=>{ console.error(error); process.exit(3); });"]
    (try
      (Files/write path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "node" "-e" script (str path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-list-string-enforces-one-aggregate-byte-budget
  (let [descriptor [:option [:list :string]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {} :effects #{}
             :functions
             [{:name 'echo :params ['value] :param-types [descriptor]
               :result descriptor :effects #{} :body 'value}]}
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-component-list-string-budget-" ".wasm"
              (make-array FileAttribute 0))
        script
        "const fs=require('node:fs');
         WebAssembly.instantiate(fs.readFileSync(process.argv[1])).then(({instance})=>{
           const e=instance.exports;
           const items=e.cm32p2_realloc(0,0,4,17*8);
           const bytes=e.cm32p2_realloc(0,0,1,65536);
           const view=new DataView(e.cm32p2_memory.buffer);
           for(let i=0;i<17;i++){
             view.setUint32(items+i*8,bytes,true);
             view.setUint32(items+i*8+4,65536,true);
           }
           e['cm32p2||echo'](0,items,17);
           let trapped=false;
           try { e['cm32p2||echo'](1,items,17); } catch (_) { trapped=true; }
           if (!trapped) process.exit(2);
           view.setUint32(items,bytes,true);view.setUint32(items+4,2,true);
           view.setUint8(bytes,0xc0);view.setUint8(bytes+1,0x80);
           trapped=false;
           try { e['cm32p2||echo'](1,items,1); } catch (_) { trapped=true; }
           if (!trapped) process.exit(4);
           view.setUint32(items+4,3,true);
           view.setUint8(bytes,0xe5);view.setUint8(bytes+1,0xae);
           view.setUint8(bytes+2,0x89);
           e['cm32p2||echo'](1,items,1);
         }).catch(error=>{ console.error(error); process.exit(3); });"]
    (try
      (Files/write path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "node" "-e" script (str path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists path)))))

(deftest structural-list-union-validates-only-each-active-item-case
  (let [descriptor [:option [:list [:option :string]]]
        kir {:format :kotoba.kir/v4
             :exports ['echo] :schemas {} :effects #{}
             :functions
             [{:name 'echo :params ['value] :param-types [descriptor]
               :result descriptor :effects #{} :body 'value}]}
        core-bytes (core/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-component-list-union-validation-" ".wasm"
              (make-array FileAttribute 0))
        script
        "const fs=require('node:fs');
         WebAssembly.instantiate(fs.readFileSync(process.argv[1])).then(({instance})=>{
           const e=instance.exports;
           const item=e.cm32p2_realloc(0,0,4,12);
           const view=new DataView(e.cm32p2_memory.buffer);
           view.setUint8(item,2);
           e['cm32p2||echo'](0,item,1);
           let trapped=false;
           try { e['cm32p2||echo'](1,item,1); } catch (_) { trapped=true; }
           if (!trapped) process.exit(2);
         }).catch(error=>{ console.error(error); process.exit(3); });"]
    (try
      (Files/write path ^bytes core-bytes
                   (make-array java.nio.file.OpenOption 0))
      (let [run (shell/sh "node" "-e" script (str path))]
        (is (zero? (:exit run)) (:err run)))
      (finally
        (Files/deleteIfExists path)))))

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
