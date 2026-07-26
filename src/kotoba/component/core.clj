(ns kotoba.component.core
  "Dedicated standard32 core emission for qualified Component Model slices."
  (:require [clojure.string :as str]
            [kotoba.wasm.core :as wasm]
            [kotoba.wasm.canonical-abi :as canonical]
            [kotoba.component.wit :as component-wit]
            [kotoba.kir.value :as value]
            [kotoba.wasm.tools :as wasm-tools]))

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :component-core))))

(defn- exported-functions [kir]
  (let [names (set (:exports kir))]
    (filterv #(contains? names (:name %)) (:functions kir))))

(defn- scalar-function? [{:keys [param-types result]}]
  (and (every? #{:i64 :f32 :f64} param-types)
       (contains? #{:i64 :f32 :f64} result)))

(defn- canonical-scalar-function? [{:keys [params param-types result]}]
  (and (= (count params) (count param-types))
       (every? #{:i64 :f32 :f64 :bool} param-types)
       (contains? #{:i64 :f32 :f64 :bool} result)))

(defn- string-leaves [form parameters]
  (cond
    (and (symbol? form) (contains? parameters form)) [{:kind :parameter :name form}]
    (string? form) [{:kind :literal :value form}]
    (and (seq? form) (= 'string-concat (first form)) (= 3 (count form)))
    (let [left (string-leaves (second form) parameters)
          right (string-leaves (nth form 2) parameters)]
      (when (and left right) (into left right)))
    :else nil))

(defn- string-expression-function? [{:keys [params param-types result body]}]
  (and (every? #{:string} param-types)
       (= (count params) (count param-types))
       (= :string result)
       (seq (string-leaves body (set params)))))

(defn- vector-i64-identity-function?
  [{:keys [params param-types result body]}]
  (and (= 1 (count params))
       (= [:vector-i64] param-types)
       (= :vector-i64 result)
       (= (first params) body)))

(defn- vector-i64-literal-function?
  [{:keys [params param-types result body]}]
  (and (empty? params)
       (empty? param-types)
       (= :vector-i64 result)
       (seq? body)
       (= 'vector-new (first body))
       (<= (count (rest body)) value/vector-item-limit)
       (every? integer? (rest body))))

(defn- uses-operation? [function operation]
  (boolean
   (some #(and (seq? %) (= operation (first %)))
         (tree-seq coll? seq (:body function)))))

(defn- sealed-scalar-record [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :record (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :record (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (comp #{:i64 :f32 :f64 :bool} second) (nth schema 2)))
      schema)))

(defn- nested-scalar-record-schema
  "Schema of `descriptor` when it is a sealed record whose fields are each
  either a Canonical scalar or exactly one level of nested sealed all-scalar
  record (a field type for which `sealed-scalar-record` itself succeeds, so a
  nested field's own fields must all be scalar). This admits at most one
  level of nesting: a field that is itself a nested-of-nested record fails
  `sealed-scalar-record` (its fields are not all scalar) and so is rejected
  here too, before component encoding is attempted."
  [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :record (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :record (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (fn [[_ field-type]]
                         (or (contains? #{:i64 :f32 :f64 :bool} field-type)
                             (sealed-scalar-record field-type schemas)))
                       (nth schema 2)))
      schema)))

(defn- string-keyword-scalar-field? [field-type]
  (contains? #{:i64 :f32 :f64 :bool :string :keyword} field-type))

(defn- string-field-record-schema
  "Schema of `descriptor` when it is a sealed record whose fields are each a
  Canonical scalar (`i64`/`f32`/`f64`/`bool`) or a bounded `string`/`keyword`
  leaf -- flat only, no nesting, no variant payloads. This is
  `sealed-scalar-record` widened by exactly the two leaf types ADR 0051's own
  'Remaining gaps' named ('strings or keywords inside a case's record
  payload, so `state-v1`'s actual `entry`/`error` records remain closed'):
  `state-v1`'s `entry` record (`key: keyword, value: string, version: i64`)
  is this shape exactly. It deliberately does not add `:string`/`:keyword`
  to `sealed-scalar-record` itself -- projection, construction, update, and
  scalar-capability-call's request/result admission all key off
  `sealed-scalar-record` alone, and their WAT emitters
  (`scalar-record-projection-wat`/`scalar-record-write-wat`/
  `record-capability-wat`) only know `wasm-value-type`/`wasm-store`, neither
  of which has a `:string`/`:keyword` case; widening `sealed-scalar-record`
  itself would make those predicates silently admit a shape their own
  emitters cannot correctly generate. This function and its own dedicated
  identity path were the only consumers of the wider field set as of ADR
  0053; ADR 0054 added a second, explicit consumer --
  `record-or-scalar-variant-case?`/`variant-case-schema`, whose matching
  `variant-case-body` emitter was extended in the same change to actually
  handle a string/keyword leaf, so that second consumer does not repeat the
  silent-admission hazard this docstring warns against."
  [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :record (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :record (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (comp string-keyword-scalar-field? second) (nth schema 2)))
      schema)))

(defn- record-or-scalar-variant-case?
  "True when `payload-type` is a shape ADR 0054 admits as one variant case's
  payload: a Canonical scalar, a sealed all-scalar record (the ADR 0052
  shape), or a sealed flat string/keyword-bearing record (the ADR 0053
  shape). A case payload that is itself an ADR 0051 one-level-nested record,
  or another variant, fails both `sealed-scalar-record` and
  `string-field-record-schema` and so is rejected here too -- this bounds
  every case to exactly the same 'flat record, no nesting, no variant
  payload' depth ADR 0052/0053 each proved on their own, now admitted
  per case in any mix within one variant."
  [payload-type schemas]
  (or (contains? #{:i64 :f32 :f64 :bool} payload-type)
      (boolean (sealed-scalar-record payload-type schemas))
      (boolean (string-field-record-schema payload-type schemas))))

(defn- variant-case-schema
  "Schema of `descriptor` when it is a sealed variant whose every case's
  payload independently satisfies `record-or-scalar-variant-case?`. This
  directly widens ADR 0052's `sealed-scalar-variant-schema` in place (the
  only caller was `variant-identity-function?`, and the codegen side --
  `variant-case-leaves`/`variant-case-body`/`variant-wat` -- is extended in
  the same change to actually emit correct WAT for the newly admitted
  string/keyword-bearing record case shape, so there is no window where this
  predicate admits a case shape its own emitter cannot yet correctly
  generate). Cases may freely mix all three kinds (scalar, all-scalar
  record, string/keyword-bearing record) within one variant -- there is no
  per-variant restriction that every record case be the same kind."
  [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :variant (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :variant (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (fn [[_ payload-type]]
                         (record-or-scalar-variant-case? payload-type schemas))
                       (nth schema 2)))
      schema)))

(defn- string-field-record-identity-function? [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        schema (string-field-record-schema descriptor schemas)]
    (and (= 1 (count params))
         (= 1 (count param-types))
         (= descriptor result)
         (= (first params) body)
         schema
         ;; Distinct from `scalar-record-identity-function?`: admitted only
         ;; when at least one field is a string or keyword leaf, so the two
         ;; predicates (and their dispatch cases) never overlap.
         (some (fn [[_ field-type]] (contains? #{:string :keyword} field-type))
               (nth schema 2))
         (canonical/layout descriptor schemas))))

(defn- scalar-record-identity-function? [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :record (first descriptor))) descriptor)]
    (and (= 1 (count params))
         (= 1 (count param-types))
         (= descriptor result)
         (= (first params) body)
         (and (vector? schema) (= :record (first schema)))
         (seq (nth schema 2))
         (every? (comp #{:i64 :f32 :f64 :bool} second) (nth schema 2))
         (canonical/layout descriptor schemas))))

(defn- nested-record-identity-function? [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        schema (nested-scalar-record-schema descriptor schemas)]
    (and (= 1 (count params))
         (= 1 (count param-types))
         (= descriptor result)
         (= (first params) body)
         schema
         ;; Distinct from `scalar-record-identity-function?`: admitted only
         ;; when at least one field is itself a nested sealed scalar record,
         ;; so the two predicates (and their dispatch cases) never overlap.
         (some (fn [[_ field-type]] (sealed-scalar-record field-type schemas))
               (nth schema 2))
         (canonical/layout descriptor schemas))))

(defn- variant-identity-function? [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        schema (variant-case-schema descriptor schemas)]
    (and (= 1 (count params))
         (= 1 (count param-types))
         (= descriptor result)
         (= (first params) body)
         schema
         (canonical/layout descriptor schemas))))

(defn- structural-union-identity-function?
  "An identity export over a structural Component Model option/result.
  These layouts have the same discriminant-plus-joined-payload shape as a
  sealed variant, so they deliberately share `variant-wat`. Payloads are
  admitted only when that emitter can recursively validate and store every
  active-case leaf: a Canonical scalar/string/keyword or a finite sealed record
  recursively containing those leaves, bounded `list<s64>`/`list<f64>`, or
  another finite structural option/result. Other list item types and recursive
  record identities remain fail-closed."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        supported-payload?
        (fn supported-payload? [payload]
          (letfn [(supported? [value seen]
                    (cond
                      (contains? #{:i64 :f32 :f64 :bool :string :keyword} value)
                      true

                      (contains? #{:vector-i64 :vector-f64} value)
                      true

                      (and (vector? value)
                           (= :option (first value))
                           (= 2 (count value)))
                      (supported? (second value) seen)

                      (and (vector? value)
                           (= :result (first value))
                           (= 3 (count value)))
                      (and (supported? (second value) seen)
                           (supported? (nth value 2) seen))

                      (and (vector? value)
                           (contains? #{:ref :record} (first value)))
                      (let [identity (second value)
                            schema (if (= :ref (first value))
                                     (get schemas identity)
                                     value)]
                        (and (keyword? identity)
                             (not (contains? seen identity))
                             (vector? schema)
                             (= :record (first schema))
                             (= identity (second schema))
                             (every? (fn [[_ field-type]]
                                       (supported? field-type (conj seen identity)))
                                     (nth schema 2))))

                      :else false))]
            (supported? payload #{})))
        payloads (when (vector? descriptor)
                   (case (first descriptor)
                     :option [(second descriptor)]
                     :result [(second descriptor) (nth descriptor 2)]
                     nil))]
    (and (= 1 (count params))
         (= 1 (count param-types))
         (= descriptor result)
         (= (first params) body)
         (seq payloads)
         (every? supported-payload? payloads)
         (canonical/layout descriptor schemas))))

(defn- structural-union-construction
  "Return the fixed case/payload plan for a scalar option/result constructor."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        [op descriptor payload] (when (seq? body) body)
        construction
        (case [op (when (vector? descriptor) (first descriptor))]
          [option-none-of :option] {:case-index 0 :payload-type nil}
          [option-some-of :option] {:case-index 1 :payload-type (second descriptor)}
          [result-ok-of :result] {:case-index 0 :payload-type (second descriptor)}
          [result-err-of :result] {:case-index 1 :payload-type (get descriptor 2)}
          nil)
        {:keys [case-index payload-type]} construction]
    (when (and construction
               (vector? descriptor)
               (= descriptor result)
               (contains? #{:option :result} (first descriptor))
               (= (if payload-type 1 0) (count params))
               (= (if payload-type [payload-type] []) param-types)
               (or (nil? payload-type)
                   (contains? #{:i64 :f32 :f64 :bool} payload-type))
               (or (nil? payload-type) (= payload (first params)))
               (canonical/layout descriptor schemas))
      (assoc construction :descriptor descriptor))))

(defn- structural-union-elimination
  "Return a plan for a scalar option/result predicate or payload projection.
  The admitted projection shape takes its fallback as a parameter, preserving
  an exact flat Component signature without compiling arbitrary branch
  expressions in this increment."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        [op descriptor value fallback] (when (seq? body) body)
        union-param (first params)
        union-type (first param-types)
        predicate-kind
        ({'option-some?-of :option 'result-ok?-of :result} op)
        predicate? (some? predicate-kind)
        projection
        (case op
          option-value-of {:selected-case 1 :payload-type (second descriptor)}
          result-value-of {:selected-case 0 :payload-type (second descriptor)}
          result-error-of {:selected-case 1 :payload-type (get descriptor 2)}
          nil)
        projection-kind
        ({'option-value-of :option
          'result-value-of :result
          'result-error-of :result} op)
        payload-type (:payload-type projection)]
    (when (and (vector? descriptor)
               (= descriptor union-type)
               (= value union-param)
               (contains? #{:option :result} (first descriptor))
               (or (nil? predicate-kind)
                   (= predicate-kind (first descriptor)))
               (or (nil? projection-kind)
                   (= projection-kind (first descriptor)))
               (canonical/layout descriptor schemas)
               (if predicate?
                 (and (= 1 (count params))
                      (= 1 (count param-types))
                      (= :bool result))
                 (and projection
                      (= 2 (count params))
                      (= [descriptor payload-type] param-types)
                      (= fallback (second params))
                      (= payload-type result)
                      (contains? #{:i64 :f32 :f64 :bool} payload-type))))
      (if predicate?
        {:descriptor descriptor :operation op :kind :predicate}
        (assoc projection :descriptor descriptor :operation op
               :kind :projection)))))

(defn- structural-union-match
  "Return an exhaustive host-free scalar option/result match plan. Branch
  bodies are compiled by the shared typed binary Wasm expression emitter, not
  reimplemented in this namespace. Heterogeneous result payloads use the same
  joined-flat coercions as the general Canonical variant codec."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        [op descriptor value & branches] (when (seq? body) body)
        union-type (first param-types)
        plan
        (case op
          option-match
          (let [[none-body some-name some-body] branches]
            {:operation op :descriptor descriptor
             :shape-valid? (and (= 3 (count branches)) (symbol? some-name))
             :case-0-body none-body
             :case-1-binder some-name :case-1-body some-body})
          result-match-of
          (let [[ok-name ok-body err-name err-body] branches]
            {:operation op :descriptor descriptor
             :shape-valid? (and (= 4 (count branches))
                                (symbol? ok-name) (symbol? err-name))
             :case-0-binder ok-name :case-0-body ok-body
             :case-1-binder err-name :case-1-body err-body})
          nil)
        expected-kind ({'option-match :option 'result-match-of :result} op)
        payloads (when (and plan (vector? descriptor))
                   (case (first descriptor)
                     :option [(second descriptor)]
                     :result [(second descriptor) (get descriptor 2)]
                     nil))
        layout (when (and payloads (every? #{:i64 :f32 :f64 :bool} payloads))
                 (canonical/layout descriptor schemas))
        joined-core-type (second (:flat layout))]
    (when (and plan
               (:shape-valid? plan)
               (= descriptor union-type)
               (= value (first params))
               (= expected-kind (first descriptor))
               (every? #{:i64 :f32 :f64 :bool} payloads)
               (contains? #{:i64 :f32 :f64 :bool} result)
               (every? #{:i64 :f32 :f64 :bool} (rest param-types))
               (contains? #{:i32 :i64 :f32 :f64} joined-core-type))
      (assoc plan
             :payload-types payloads
             :joined-core-type joined-core-type))))

(defn- joined-core-coercion
  "One authoritative classification of the Component Model's reachable
  joined-flat scalar coercions. Both binary match adapters and the general
  variant WAT codec render this classification into their own instruction
  representation."
  [have want]
  (cond
    (= have want) :identity
    (and (= have :i32) (= want :f32)) :i32-to-f32
    (and (= have :i64) (= want :i32)) :i64-to-i32
    (and (= have :i64) (= want :f32)) :i64-to-f32
    (and (= have :i64) (= want :f64)) :i64-to-f64
    :else (reject "variant flat join has no defined coercion"
                  {:have have :want want})))

(defn- structural-union-decode-form
  "Decode one joined Canonical payload slot for its selected scalar case.
  These are expression forms for the binary emitter equivalent of
  `variant-coerce-ops`; no conversion is allowed outside the Component
  Model's reachable join table."
  [payload joined-core-type payload-type]
  (let [wanted ({:i64 :i64 :f32 :f32 :f64 :f64 :bool :i32} payload-type)]
    (case (joined-core-coercion joined-core-type wanted)
      :identity
      (if (= :bool payload-type)
        (list 'component-assert-bool payload)
        payload)

      :i32-to-f32
      (list 'component-i32-to-f32 payload)

      :i64-to-i32
      (list 'component-assert-bool (list 'component-i64-to-i32 payload))

      :i64-to-f32
      (list 'component-i64-to-f32 payload)

      :i64-to-f64
      (list 'component-i64-to-f64 payload))))

(declare structural-union-match-adapter)

(defn- structural-union-match-core
  [function schemas plan target opts]
  (let [{:keys [function core-param-types unchecked-bool-params]}
        (structural-union-match-adapter function schemas plan)]
    (wasm/emit-component-core
     {:format :kotoba.kir/v4
      :exports [(:name function)]
      :schemas {}
      :effects #{}
      :functions [function]}
     target
     (assoc opts
            :component-canonical-scalars? true
            :component-unchecked-bool-params
            {(:name function) unchecked-bool-params}
            :core-param-types {(:name function) core-param-types}))))

(defn- structural-union-match-adapter
  [function _schemas plan]
  (let [used (set (:params function))
        fresh (fn [base]
                (first (remove used
                               (map #(symbol (str base (when (pos? %) %)))
                                    (range)))))
        disc32 (fresh "__component_disc")
        disc64 (fresh "__component_tag")
        payload (fresh "__component_payload")
        other-params (vec (rest (:params function)))
        payload-types (:payload-types plan)
        joined-core-type (:joined-core-type plan)
        joined-source-type ({:i32 :bool :i64 :i64 :f32 :f32 :f64 :f64}
                            joined-core-type)
        other-types (vec (rest (:param-types function)))
        case-0-payload (structural-union-decode-form
                        payload joined-core-type (first payload-types))
        case-1-payload (structural-union-decode-form
                        payload joined-core-type (if (= 1 (count payload-types))
                                                   (first payload-types)
                                                   (second payload-types)))
        case-0 (if-let [binder (:case-0-binder plan)]
                 (list 'let [binder case-0-payload] (:case-0-body plan))
                 (:case-0-body plan))
        case-1 (list 'let [(:case-1-binder plan) case-1-payload] (:case-1-body plan))
        branch (list 'if disc64 case-1 case-0)
        checked
        (list 'let [disc64 (list 'i64-extend-i32-u disc32)]
              (list 'if (list '< disc64 2)
                    branch
                    (list 'component-unreachable)))
        synthetic-function
        {:name (:name function)
         :params (vec (concat [disc32 payload] other-params))
         :param-types (vec (concat [:i64 joined-source-type] other-types))
         :result (:result function)
         :effects #{}
         :body checked}
        core-type {:i64 :i64 :f32 :f32 :f64 :f64 :bool :i32}
        core-types (vec (concat [:i32 joined-core-type]
                                (map core-type other-types)))
        wasm-types (mapv {:i32 0x7f :i64 0x7e :f32 0x7d :f64 0x7c} core-types)]
    {:function synthetic-function
     :core-param-types wasm-types
     :unchecked-bool-params (if (= :i32 joined-core-type) #{1} #{})}))

(defn- structural-union-match-module
  [kir]
  (let [functions (:functions kir)
        exports (exported-functions kir)
        plans (into {}
                    (keep (fn [function]
                            (when-let [plan (structural-union-match
                                             function (:schemas kir))]
                              [(:name function) plan])))
                    functions)]
    (when (and (or (> (count exports) 1)
                   (> (count functions) 1))
               (seq plans)
               (empty? (:effects kir))
               (every? (fn [function]
                         (or (contains? plans (:name function))
                             (canonical-scalar-function? function)))
                       functions))
      plans)))

(defn- structural-union-match-module-core
  [kir plans target opts]
  (let [adapters
        (mapv (fn [function]
                (if-let [plan (get plans (:name function))]
                  (structural-union-match-adapter function (:schemas kir) plan)
                  {:function function :unchecked-bool-params #{}}))
              (:functions kir))
        core-param-types
        (into {} (keep (fn [{:keys [function core-param-types]}]
                         (when core-param-types
                           [(:name function) core-param-types])))
              adapters)
        unchecked
        (into {} (map (fn [{:keys [function unchecked-bool-params]}]
                        [(:name function) unchecked-bool-params]))
              adapters)
        synthetic {:format :kotoba.kir/v4
                   :exports (:exports kir)
                   :schemas {}
                   :effects #{}
                   :functions (mapv :function adapters)}]
    (wasm/emit-component-core
     synthetic target
     (assoc opts
            :component-canonical-scalars? true
            :component-unchecked-bool-params unchecked
            :core-param-types core-param-types))))

(defn- scalar-record-projection [function schemas]
  (let [{:keys [params param-types result body]} function
        descriptor (first param-types)
        schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :record (first descriptor))) descriptor)
        [_ body-type value field] (when (seq? body) body)
        fields (when (and (vector? schema) (= :record (first schema))) (nth schema 2))
        field-index (first (keep-indexed (fn [index [name _]]
                                          (when (= name field) index))
                                        fields))
        field-type (when (some? field-index) (second (nth fields field-index)))]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'record-get (first body))
               (or (= body-type descriptor) (= body-type schema))
               (= value (first params))
               (some? field-index)
               (= field-type result)
               (contains? #{:i64 :f32 :f64 :bool} result)
               (every? (comp #{:i64 :f32 :f64 :bool} second) fields))
      {:descriptor descriptor :schema schema :field-index field-index})))

(defn- scalar-record-construction [function schemas]
  (let [{:keys [params param-types result body]} function
        schema (sealed-scalar-record result schemas)
        [_ body-type & values] (when (seq? body) body)
        field-types (mapv second (when schema (nth schema 2)))]
    (when (and schema
               (seq? body)
               (= 'record-new (first body))
               (= body-type schema)
               (= (vec values) (vec params))
               (= param-types field-types))
      {:descriptor result
       :input-types param-types
       :field-sources (vec (range (count field-types)))})))

(defn- scalar-record-update [function schemas]
  (let [{:keys [params param-types result body]} function
        record-descriptor (first param-types)
        replacement-type (second param-types)
        schema (sealed-scalar-record record-descriptor schemas)
        [_ body-type value field replacement] (when (seq? body) body)
        fields (when schema (nth schema 2))
        field-index (first (keep-indexed (fn [index [name _]]
                                          (when (= name field) index))
                                        fields))
        field-type (when (some? field-index) (second (nth fields field-index)))]
    (when (and schema
               (= 2 (count params))
               (= 2 (count param-types))
               (= result record-descriptor)
               (seq? body)
               (= 'record-assoc (first body))
               (or (= body-type record-descriptor) (= body-type schema))
               (= value (first params))
               (= replacement (second params))
               (some? field-index)
               (= replacement-type field-type))
      {:descriptor record-descriptor
       :input-types (conj (mapv second fields) replacement-type)
       :field-sources (assoc (vec (range (count fields)))
                             field-index (count fields))})))

(defn- scalar-capability-call [function]
  (let [{:keys [params param-types result body]} function
        [_ id request-type result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'typed-cap-call (first body))
               (= request (first params))
               (= request-type (first param-types))
               (= result-type result)
               (contains? #{:i64 :f32 :f64} request-type)
               (contains? #{:i64 :f32 :f64} result-type)
               capability)
      capability)))

(defn- scalar-literal-capability-call [function]
  (let [{:keys [params param-types result body]} function
        [_ id request-type result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))]
    (when (and (empty? params) (empty? param-types)
               (seq? body) (= 'typed-cap-call (first body))
               (= :i64 request-type) (= :i64 result-type) (= :i64 result)
               (integer? request) capability)
      {:capability capability :request request})))

(defn- record-capability-call [function schemas]
  (let [{:keys [params param-types result body]} function
        request-type (first param-types)
        [_ id body-request-type body-result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))
        request-schema (sealed-scalar-record request-type schemas)
        result-schema (sealed-scalar-record result schemas)]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'typed-cap-call (first body))
               (= request (first params))
               (= body-request-type request-type)
               (= body-result-type result)
               (= request-type result)
               request-schema result-schema capability)
      {:capability capability :request request-type :result result})))

(defn- variant-capability-case?
  "True when `payload-type` is a shape admitted as one variant case's
  payload *when that variant is used as a `typed-cap-call` request/result*:
  a bare Canonical scalar (`i64`/`f32`/`f64`/`bool`, ADR 0055's original
  scope), a sealed all-scalar record (the ADR 0052 shape, ADR 0056), or --
  new in ADR 0057 -- a sealed flat string/keyword-bearing record (the ADR
  0053 shape, via `string-field-record-schema`). This closes the exact gap
  both ADR 0055 and ADR 0056 named as remaining and unattempted: string/
  keyword data crossing a capability-call boundary at all. It does *not*
  admit a bare `:string`/`:keyword` case payload directly (a case whose own
  payload type is `:string`/`:keyword` with no record wrapper) -- only a
  record field carries string/keyword data across this boundary in this
  slice, matching `state-v1`'s own actual shape (every one of its non-bool
  cases wraps a record, never a bare string). `record-capability-call` (a
  bare record, not wrapped in a variant, as the whole request/result) is
  untouched and still admits scalar fields only -- widening that path is a
  separate, still-unattempted slice this ADR does not attempt, deliberately,
  because `state-v1`'s own request/result are both variants, never a bare
  record."
  [payload-type schemas]
  (or (contains? #{:i64 :f32 :f64 :bool} payload-type)
      (boolean (sealed-scalar-record payload-type schemas))
      (boolean (string-field-record-schema payload-type schemas))))

(defn- variant-capability-schema
  "Schema of `descriptor` when it is a sealed variant whose every case's
  payload independently satisfies `variant-capability-case?`. Structurally
  the same admission shape as `variant-case-schema` (ADR 0052/0054), renamed
  from ADR 0055's own `scalar-variant-capability-schema` now that the case
  set is no longer scalar-only. Kept as a separate function (not a
  parameterization of `variant-case-schema`) so the identity-export path's
  own admitted case set never silently narrows if this one changes, and vice
  versa -- `variant-case-schema` still additionally admits a string/keyword-
  bearing record case, which this one does not."
  [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :variant (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :variant (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (fn [[_ payload-type]] (variant-capability-case? payload-type schemas))
                       (nth schema 2)))
      schema)))

(defn- variant-capability-call
  "Admission for a direct `typed-cap-call` whose request and result are one
  sealed variant (`variant-capability-schema`), the same request/
  result identity, matching `record-capability-call`'s own same-type
  discipline (ADR 0048) rather than ADR 0046's scalar slice, which allows
  request and result to differ -- widening a *structured* request/result to
  different identities would require the provider to perform a real semantic
  mapping between two distinct shapes, out of scope for a wiring-only
  identity provider slice."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        request-type (first param-types)
        [_ id body-request-type body-result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))
        request-schema (variant-capability-schema request-type schemas)
        result-schema (variant-capability-schema result schemas)]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'typed-cap-call (first body))
               (= request (first params))
               (= body-request-type request-type)
               (= body-result-type result)
               (= request-type result)
               request-schema result-schema capability)
      {:capability capability :request request-type :result result})))

(defn- structural-union-capability-call
  "Admission for a direct named capability call that transports one scalar
  structural option/result type unchanged."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        request-type (first param-types)
        [_ id body-request-type body-result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))
        payloads (when (vector? request-type)
                   (case (first request-type)
                     :option [(second request-type)]
                     :result [(second request-type) (get request-type 2)]
                     nil))]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'typed-cap-call (first body))
               (= request (first params))
               (= body-request-type request-type)
               (= body-result-type result)
               (= request-type result)
               (seq payloads)
               (every? #{:i64 :f32 :f64 :bool} payloads)
               (canonical/layout request-type schemas)
               capability)
      {:capability capability :request request-type :result result})))

(defn- asymmetric-variant-capability-case?
  "True when `payload-type` is a shape admitted as one variant case's payload
  for the *different-identity* `typed-cap-call` crossing: a bare Canonical
  scalar (`i64`/`f32`/`f64`/`bool`), a sealed all-scalar record (the ADR
  0052 shape, ADR 0058), or -- new in ADR 0059 -- a sealed flat
  string/keyword-bearing record (the ADR 0053 shape, via
  `string-field-record-schema`), exactly `variant-capability-case?`'s own
  (same-identity) case-kind union. This closes the exact gap ADR 0058's own
  'Remaining gaps' named first and in these words: 'String/keyword-bearing
  cases crossing the different-identity boundary ... this ADR deliberately
  narrowed the different-identity crossing to exactly ADR 0055/0056's
  scalar-or-all-scalar-record case-kind union, leaving ADR 0057's
  string/keyword-bearing record case unattempted for this specific
  (asymmetric) boundary.' ADR 0058 itself named the two things that gap
  required before it could be closed: '(a) confirming the Canonical ABI's
  cross-instance string-copy glue composes correctly when request-layout
  and result-layout genuinely differ ... headroom is still deliberately
  computed from the request layout alone, correct only because neither side
  may carry a string leaf'; '(b) a provider that ... can allocate and write
  literal string/keyword byte data for a chosen result case.' Both are
  addressed by this ADR: (a) `variant-capability-wat`'s own memory-page
  sizing formula below now sums REQUEST-side and RESULT-side string
  headroom independently rather than assuming one side alone always
  suffices; (b) `asymmetric-variant-capability-provider-wat` gains
  `plan-result-string-data`, embedding one fixed compile-time literal per
  string/keyword RESULT leaf as a `(data ...)` segment (the provider's
  fixed-constant dispatch was already never derived from any request
  payload value, so a fixed literal string is the same 'wiring only, not
  semantic' framing ADR 0058's own numeric constants already used, merely
  widened to a leaf kind that cannot be a bare numeric immediate). This
  ADR does NOT widen the case-kind set further than `variant-capability-
  case?` already established for the same-identity path -- a case wrapping
  an ADR 0051 one-level-nested record, or a bare `:string`/`:keyword` case
  payload with no record wrapper, remain fail-closed here exactly as they
  do there."
  [payload-type schemas]
  (or (contains? #{:i64 :f32 :f64 :bool} payload-type)
      (boolean (sealed-scalar-record payload-type schemas))
      (boolean (string-field-record-schema payload-type schemas))))

(defn- asymmetric-variant-capability-schema
  "Schema of `descriptor` when it is a sealed variant whose every case's
  payload independently satisfies `asymmetric-variant-capability-case?` --
  the different-identity twin of `variant-capability-schema`, kept as a
  separate function for the same reason `variant-capability-schema` itself
  is kept separate from `variant-case-schema`: so the same-identity path's
  own admitted case set never silently narrows if this one changes, and
  vice versa."
  [descriptor schemas]
  (let [schema (cond
                 (and (vector? descriptor) (= :ref (first descriptor)))
                 (get schemas (second descriptor))
                 (and (vector? descriptor) (= :variant (first descriptor))) descriptor)]
    (when (and (vector? schema)
               (= :variant (first schema))
               (seq (nth schema 2))
               (= schema (get schemas (second schema)))
               (every? (fn [[_ payload-type]]
                         (asymmetric-variant-capability-case? payload-type schemas))
                       (nth schema 2)))
      schema)))

(defn- different-variant-capability-call
  "Admission for a direct `typed-cap-call` whose request and result are two
  INDEPENDENTLY admitted (`asymmetric-variant-capability-schema`) but
  DIFFERENT sealed variant identities (ADR 0058) -- widening
  `variant-capability-call`'s own same-identity discipline (ADR 0048/0055)
  along the one dimension every capability-call ADR through 0057 explicitly
  named as still unattempted. `state-v1`'s own request
  (`kotoba.state/request`) and result (`kotoba.state/result`) are exactly
  this shape: two different variant identities, never the same type twice.
  Structurally identical to `variant-capability-call` except (1) it checks
  `(not= request-type result)` instead of `(= request-type result)`, so the
  two admission functions are mutually exclusive by construction and never
  both admit the same function, and (2) each side is checked against the
  narrower `asymmetric-variant-capability-schema` (see that function's own
  docstring for why the case-kind set is narrower here)."
  [function schemas]
  (let [{:keys [params param-types result body]} function
        request-type (first param-types)
        [_ id body-request-type body-result-type request] (when (seq? body) body)
        capability (some #(when (= id (:id %)) %) (:capabilities component-wit/contract))
        request-schema (asymmetric-variant-capability-schema request-type schemas)
        result-schema (asymmetric-variant-capability-schema result schemas)]
    (when (and (= 1 (count params))
               (= 1 (count param-types))
               (seq? body)
               (= 'typed-cap-call (first body))
               (= request (first params))
               (= body-request-type request-type)
               (= body-result-type result)
               (not= request-type result)
               request-schema result-schema capability)
      {:capability capability :request request-type :result result})))

(def ^:private scalar-wasm-type-byte
  "Core value type for a scalar Canonical ABI leaf: i64/f32/f64."
  {:i64 0x7e :f32 0x7d :f64 0x7c})

(defn- typed-cap-calls
  "Every distinct (id, request-type, result-type) a KIR performs, in a stable
  order. Mirrors component-wit's own scan so the WIT and the core module's
  imports are derived from the same facts."
  [kir]
  (->> (:functions kir)
       (mapcat #(tree-seq coll? seq (:body %)))
       (keep (fn [form]
               (when (and (seq? form) (= 'typed-cap-call (first form)))
                 (let [[_ id request-type result-type] form]
                   {:id id :request-type request-type :result-type result-type}))))
       distinct
       (sort-by (juxt :id (comp pr-str :request-type) (comp pr-str :result-type)))
       vec))

(defn scalar-capability-imports
  "Per-capability core imports for a KIR whose capability calls are all scalar,
  or nil if any is not.

  ADR 0076 increment 1. The generic `kotoba:typed`/`cap-call` intrinsic cannot
  be bound to a WIT interface, so a capability-using component previously had
  to be one of four hand-written single-function shapes. A scalar request and
  result lower to themselves under the Canonical ABI -- no memory, no realloc --
  so the import is simply (param T) (result T) under the standard32 name the
  hand-written shapes already use."
  [kir]
  (let [calls (typed-cap-calls kir)
        by-id (into {} (map (juxt :id identity)) (:capabilities component-wit/contract))]
    (when (seq calls)
      (let [descriptors
            (reduce (fn [acc {:keys [id request-type result-type]}]
                      (let [entry (get by-id id)
                            param (scalar-wasm-type-byte request-type)
                            result (scalar-wasm-type-byte result-type)]
                        (if (and entry param result)
                          (conj acc {:id id
                                     :module (str "cm32p2|kotoba:application/"
                                                  (name (:interface entry)) "@1")
                                     :field (:function entry)
                                     :type [0x60 1 param 1 result]})
                          (reduced nil))))
                    [] calls)]
        ;; Distinct capability ids only: two calls to the same capability share
        ;; one import, and differing signatures for one id would be ambiguous.
        (when (and descriptors
                   (= (count descriptors)
                      (count (distinct (map :id descriptors)))))
          descriptors)))))

(defn assert-supported! [kir]
  (let [exports (exported-functions kir)
        match-module (structural-union-match-module kir)]
    (cond
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-literal-capability-call (first exports)))
      :scalar-literal-capability-call
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-capability-call (first exports))) :scalar-capability-call
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (record-capability-call (first exports) (:schemas kir))) :record-capability-call
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (variant-capability-call (first exports) (:schemas kir))) :variant-capability-call
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (structural-union-capability-call (first exports) (:schemas kir)))
      :structural-union-capability-call
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (different-variant-capability-call (first exports) (:schemas kir)))
      :different-variant-capability-call
      ;; ADR 0076 increment 1: any shape -- many functions, computation around
      ;; the call -- as long as every export and every capability call is
      ;; scalar. The four hand-written *-capability-call shapes above stay
      ;; ahead of this so their behaviour is unchanged.
      (and (every? scalar-function? exports)
           (some? (scalar-capability-imports kir)))
      :scalar-with-capabilities
      (every? scalar-function? exports) :scalar
      match-module :structural-union-match-module
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (string-expression-function? (first exports))
           (empty? (:effects kir))) :string-expression
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (vector-i64-identity-function? (first exports))
           (empty? (:effects kir))) :vector-i64-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (vector-i64-literal-function? (first exports))
           (empty? (:effects kir))) :vector-i64-literal
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-record-identity-function? (first exports) (:schemas kir))
           (empty? (:effects kir))) :scalar-record-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (nested-record-identity-function? (first exports) (:schemas kir))
           (empty? (:effects kir))) :nested-record-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (variant-identity-function? (first exports) (:schemas kir))
           (empty? (:effects kir))) :variant-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (structural-union-identity-function? (first exports) (:schemas kir))
           (empty? (:effects kir))) :structural-union-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (structural-union-construction (first exports) (:schemas kir))
           (empty? (:effects kir))) :structural-union-construction
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (structural-union-elimination (first exports) (:schemas kir))
           (empty? (:effects kir))) :structural-union-elimination
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (structural-union-match (first exports) (:schemas kir))
           (empty? (:effects kir))) :structural-union-match
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (string-field-record-identity-function? (first exports) (:schemas kir))
           (empty? (:effects kir))) :string-field-record-identity
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-record-projection (first exports) (:schemas kir))
           (empty? (:effects kir))) :scalar-record-projection
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-record-construction (first exports) (:schemas kir))
           (empty? (:effects kir))) :scalar-record-construction
      (and (= 1 (count (:functions kir)))
           (= 1 (count exports))
           (scalar-record-update (first exports) (:schemas kir))
           (empty? (:effects kir))) :scalar-record-update
      :else
      (if (some #(uses-operation? % 'vector-conj) exports)
        (reject "component vector-conj requires linearity analysis (ADR 0077)"
                {:operation 'vector-conj})
        (reject "component function body has no qualified Canonical lowering"
                {:exports (mapv #(select-keys % [:name :param-types :result :body])
                                exports)})))))

(defn- wit-name [symbol]
  (let [value (name symbol)]
    (when-not (re-matches #"[a-z][a-z0-9-]*" value)
      (reject "string component export has no direct WIT name" {:name symbol}))
    value))

(defn- align-up [value alignment]
  (* alignment (quot (+ value (dec alignment)) alignment)))

(defn- prepare-leaves [function]
  (let [parameter-indices (zipmap (:params function) (range))
        leaves (string-leaves (:body function) (set (:params function)))]
    (loop [remaining leaves offset 8 prepared []]
      (if-let [leaf (first remaining)]
        (if (= :parameter (:kind leaf))
          (recur (next remaining) offset
                 (conj prepared (assoc leaf :index (get parameter-indices (:name leaf)))))
          (let [bytes (vec (.getBytes ^String (:value leaf) "UTF-8"))]
            (recur (next remaining) (+ offset (count bytes))
                   (conj prepared (assoc leaf :pointer offset :bytes bytes
                                         :length (count bytes))))))
        {:leaves prepared :arena-base (align-up offset 8)}))))

(defn- wat-data [bytes]
  (apply str (map #(format "\\%02x" (bit-and (int %) 0xff)) bytes)))

(defn- leaf-pointer [leaf]
  (if (= :parameter (:kind leaf))
    (str "local.get $p" (:index leaf) "-ptr")
    (str "i32.const " (:pointer leaf))))

(defn- leaf-length [leaf]
  (if (= :parameter (:kind leaf))
    (str "local.get $p" (:index leaf) "-len")
    (str "i32.const " (:length leaf))))

(defn- string-expression-wat [function]
  (let [export (wit-name (:name function))
        {:keys [leaves arena-base]} (prepare-leaves function)
        parameter-count (count (:params function))
        required-bytes (+ arena-base (* (inc parameter-count) 65536) 8)
        pages (max 1 (quot (+ required-bytes 65535) 65536))
        capacity (* pages 65536)
        params (apply str
                      (mapcat (fn [index]
                                [(str " (param $p" index "-ptr i32)")
                                 (str " (param $p" index "-len i32)")])
                              (range parameter-count)))
        validate-parameters
        (apply str
               (map (fn [index]
                      (str
                       "    local.get $p" index "-len i32.const 65536 i32.gt_u if unreachable end\n"
                       "    local.get $p" index "-ptr local.get $p" index "-len i32.add\n"
                       "    local.tee $end local.get $p" index "-ptr i32.lt_u if unreachable end\n"
                       "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"))
                    (range parameter-count)))
        sum-lengths
        (apply str
               (map (fn [leaf]
                      (str "    local.get $total " (leaf-length leaf)
                           " i64.extend_i32_u i64.add local.tee $total\n"
                           "    i64.const 65536 i64.gt_u if unreachable end\n"))
                    leaves))
        copy-leaves
        (apply str
               (map (fn [leaf]
                      (let [length (leaf-length leaf)]
                        (str "    local.get $out local.get $cursor i32.add "
                             (leaf-pointer leaf) " " length " memory.copy\n"
                             "    local.get $cursor " length
                             " i32.add local.set $cursor\n")))
                    leaves))
        data-segments
        (apply str
               (keep (fn [leaf]
                       (when (= :literal (:kind leaf))
                         (str "  (data (i32.const " (:pointer leaf) ") \""
                              (wat-data (:bytes leaf)) "\")\n")))
                     leaves))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const " arena-base "))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $end i32) (local $out i32) (local $ret i32)\n"
     "    (local $cursor i32) (local $total i64)\n"
     validate-parameters
     "    i64.const 0 local.set $total\n"
     sum-lengths
     "    i32.const 0 i32.const 0 i32.const 1 local.get $total i32.wrap_i64\n"
     "    call $realloc local.set $out\n"
     "    i32.const 0 local.set $cursor\n"
     copy-leaves
     "    i32.const 0 i32.const 0 i32.const 4 i32.const 8 call $realloc local.tee $ret\n"
     "    local.get $out i32.store\n"
     "    local.get $ret local.get $total i32.wrap_i64 i32.store offset=4 local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const " arena-base " global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const " arena-base " global.set $next)\n"
     data-segments
     ")\n")))

(declare bounded-bump-realloc-wat)

(defn- vector-i64-identity-wat
  "Canonical `list<s64> -> list<s64>` identity.

  The Canonical adapter has already copied the input elements into this
  module's memory through `cm32p2_realloc`. Validate the complete `(ptr,len)`
  pair before returning it; the result record aliases that admitted input
  buffer and is consumed before `_post` resets the arena."
  [function]
  (let [export (wit-name (:name function))
        pages wasm/component-memory-pages
        capacity wasm/component-arena-capacity
        item-limit value/vector-item-limit]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     (bounded-bump-realloc-wat capacity)
     "  (func (export \"cm32p2||" export "\")"
     " (param $ptr i32) (param $len i32) (result i32)\n"
     "    (local $bytes i32) (local $end i32) (local $ret i32)\n"
     "    local.get $len i32.const " item-limit " i32.gt_u if unreachable end\n"
     "    local.get $len i32.const 3 i32.shl local.set $bytes\n"
     "    local.get $len i32.eqz if else\n"
     "      local.get $ptr i32.const 8 i32.lt_u if unreachable end\n"
     "      local.get $ptr i32.const 7 i32.and if unreachable end\n"
     "    end\n"
     "    local.get $ptr local.get $bytes i32.add local.tee $end\n"
     "    local.get $ptr i32.lt_u if unreachable end\n"
     "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"
     "    i32.const 0 i32.const 0 i32.const 4 i32.const 8\n"
     "    call $realloc local.tee $ret\n"
     "    local.get $ptr i32.store\n"
     "    local.get $ret local.get $len i32.store offset=4\n"
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next)\n"
     ")\n")))

(defn- vector-i64-literal-wat [function]
  (let [export (wit-name (:name function))
        items (vec (rest (:body function)))
        bytes (* 8 (count items))
        pages wasm/component-memory-pages
        capacity wasm/component-arena-capacity
        stores
        (apply str
               (map-indexed
                (fn [index item]
                  (str "    local.get $out i64.const " item
                       " i64.store offset=" (+ 8 (* index 8)) "\n"))
                items))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     (bounded-bump-realloc-wat capacity)
     "  (func (export \"cm32p2||" export "\") (result i32)\n"
     "    (local $out i32) (local $ret i32)\n"
     "    i32.const 0 i32.const 0 i32.const 8 i32.const " (+ 8 bytes)
     " call $realloc local.set $out\n"
     "    local.get $out i32.const " (count items) " i32.store\n"
     stores
     "    i32.const 0 i32.const 0 i32.const 4 i32.const 8\n"
     "    call $realloc local.tee $ret\n"
     "    local.get $out i32.const 8 i32.add i32.store\n"
     "    local.get $ret i32.const " (count items) " i32.store offset=4\n"
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next)\n"
     ")\n")))

(defn- wasm-value-type [descriptor]
  ({:i64 "i64" :f32 "f32" :f64 "f64" :bool "i32"} descriptor))

(defn- wasm-store [descriptor]
  ({:i64 "i64.store" :f32 "f32.store" :f64 "f64.store" :bool "i32.store8"}
   descriptor))

(defn- core-type-of
  "The joined component-flat core wasm type (`:i64`/`:f32`/`:f64`/`:i32`)
  that a leaf's own Canonical scalar `descriptor` flattens to -- the same
  mapping `canonical-abi/layout*` bakes into every leaf's own `:flat`, kept
  here too because variant codegen needs it both to build case payload
  values (`wasm-value-type`/`wasm-store`, keyed by `descriptor`) and to
  compare against the joined param's core type (keyed by core type)."
  [descriptor]
  ({:i64 :i64 :f32 :f32 :f64 :f64 :bool :i32} descriptor))

(defn- core-type-name [core-type]
  ({:i64 "i64" :f32 "f32" :f64 "f64" :i32 "i32"} core-type))

(defn- variant-disc-store [byte-size]
  ({1 "i32.store8" 2 "i32.store16" 4 "i32.store"} byte-size))

(defn- variant-coerce-ops
  "The WAT instruction(s) that turn a received joined-flat param value
  (core type `have`) back into the value a specific case's own leaf (core
  type `want`) needs to store, mirroring the Component Model spec's
  `lift_flat_variant` `CoerceValueIter` table exactly: identical types need
  nothing; `i32`-holding-a-bool next to an `f32` case at the same position
  reinterprets; anything joined up to `i64` either wraps down to `i32`,
  reinterprets down to `f64`, or does both to reach `f32`. No other
  `(have, want)` pair is reachable -- `join-core-type` only ever produces
  `i32` (self-join or the i32/f32 pair) or `i64` (every other mismatch), so
  this table is exhaustive over what this codebase's own join can produce,
  not a hand-picked subset of the spec's."
  [have want]
  (case (joined-core-coercion have want)
    :identity []
    :i32-to-f32 ["f32.reinterpret_i32"]
    :i64-to-i32 ["i32.wrap_i64"]
    :i64-to-f32 ["i32.wrap_i64" "f32.reinterpret_i32"]
    :i64-to-f64 ["f64.reinterpret_i64"]))

(defn- variant-case-leaves
  "Recursively return the ordered Canonical leaves of one active case.
  Relative offsets accumulate through nested record layouts and flat indices
  advance by each field layout's exact flattened width. String/keyword leaves
  retain their byte bound and pointer/length pair; list leaves retain their
  item bound/layout and pointer/length pair; scalar leaves retain one joined
  position. Admission excludes nested layouts with cases before this walker
  runs."
  [layout]
  (letfn [(walk [node base-offset base-flat-index]
            (cond
              (empty? (:flat node))
              []

              (:max-bytes node)
              [{:relative-offset base-offset
                :descriptor (:descriptor node)
                :flat-index base-flat-index
                :max-bytes (:max-bytes node)}]

              (:max-items node)
              [{:relative-offset base-offset
                :descriptor (:descriptor node)
                :flat-index base-flat-index
                :max-items (:max-items node)
                :item-layout (:item-layout node)}]

              (contains? node :fields)
              (loop [remaining (:fields node)
                     flat-index base-flat-index
                     leaves []]
                (if-let [{field-offset :offset field-layout :layout}
                         (first remaining)]
                  (recur (next remaining)
                         (+ flat-index (count (:flat field-layout)))
                         (into leaves
                               (walk field-layout
                                     (+ base-offset field-offset)
                                     flat-index)))
                  leaves))

              :else
              [{:relative-offset base-offset
                :descriptor (:descriptor node)
                :flat-index base-flat-index}]))]
    (walk layout 0 0)))

(defn- variant-flat-value-expr
  "The WAT expression that turns the joined-flat param at `flat-index`
  (core type `(nth joined-types flat-index)`) back into a value of core
  type `want`, via `variant-coerce-ops` -- the same un-join step
  `variant-payload-value-expr` already did for a scalar leaf's single
  position, factored out so a string/keyword leaf can call it twice (once
  for its pointer position, once for its length position, both always
  wanting `:i32`)."
  [joined-types flat-index want]
  (let [have (nth joined-types flat-index)]
    (apply str "local.get $p" flat-index
           (map #(str " " %) (variant-coerce-ops have want)))))

(defn- variant-payload-value-expr [joined-types leaf]
  (variant-flat-value-expr joined-types (:flat-index leaf) (core-type-of (:descriptor leaf))))

(defn- variant-indirect-leaf-value-exprs
  "The `[pointer-expr length-expr]` pair for one string/keyword/list leaf: its
  pointer sits at `flat-index`, its length at `flat-index`+1, both always
  joined as (or coerced back to) `:i32` -- the identical pointer+length
  linear-memory shape ADR 0040/0041/0053 already gave a bare string
  parameter and a string-field record leaf."
  [joined-types leaf]
  [(variant-flat-value-expr joined-types (:flat-index leaf) :i32)
   (variant-flat-value-expr joined-types (inc (:flat-index leaf)) :i32)])

(defn- variant-case-validation
  "Just the validation half of one active variant case's leaves (bool
  range-check, and, since ADR 0054, string/keyword length-against-
  `:max-bytes` plus pointer-range-against-`capacity` -- exactly
  `string-field-record-wat`'s `validate-parameters` shape), with no
  result-area store at all. Factored out of `variant-case-body` in ADR 0059
  so a second caller can run validation alone: `asymmetric-variant-
  capability-provider-wat`'s own REQUEST-side dispatch (new in ADR 0059)
  needs to validate the ACTIVE REQUEST case's own leaves against their
  Kotoba-declared bounds even though that provider never stores or reads
  their VALUES (it only ever writes a fixed, request-independent RESULT
  constant) -- without this, an oversized request string/keyword leaf would
  silently flow through the crossing unchecked simply because this
  provider has no other reason to touch it, breaking every prior ADR's
  fail-closed discipline for the byte bounds. `variant-case-body` itself is
  unchanged in observable behavior: it now calls this function for its own
  validation half rather than duplicating the logic inline (confirmed by
  the ADR 0055/0056/0057/0058 identity/same-identity fixtures' unchanged
  round trips after this refactor -- see Evidence)."
  [joined-types capacity leaves]
  (apply str
         (keep (fn [leaf]
                 (cond
                   (:max-bytes leaf)
                   (let [[pointer length] (variant-indirect-leaf-value-exprs joined-types leaf)]
                     (str
                      "      " length " i32.const " (:max-bytes leaf)
                      " i32.gt_u if unreachable end\n"
                      "      " pointer " " length " i32.add\n"
                      "      local.tee $end " pointer " i32.lt_u if unreachable end\n"
                      "      local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"))
                   (:max-items leaf)
                   (let [[pointer length]
                         (variant-indirect-leaf-value-exprs joined-types leaf)
                         item-layout (:item-layout leaf)
                         stride (align-up (:size item-layout)
                                          (:alignment item-layout))]
                     (str
                      "      " length " i32.const " (:max-items leaf)
                      " i32.gt_u if unreachable end\n"
                      "      " length " i32.eqz if else\n"
                      "        " pointer " i32.const " (:alignment item-layout)
                      " i32.const 1 i32.sub i32.and if unreachable end\n"
                      "        " pointer " i32.const 8 i32.lt_u if unreachable end\n"
                      "      end\n"
                      "      " pointer " " length " i32.const " stride
                      " i32.mul i32.add\n"
                      "      local.tee $end " pointer " i32.lt_u if unreachable end\n"
                      "      local.get $end i32.const " capacity
                      " i32.gt_u if unreachable end\n"))
                   (= :bool (:descriptor leaf))
                   (str "      " (variant-payload-value-expr joined-types leaf)
                        " i32.const 1 i32.gt_u if unreachable end\n")
                   :else nil))
               leaves)))

(defn- variant-case-body
  "The validation and result-area stores for one active variant case,
  covering three leaf shapes now: a plain scalar leaf (unchanged from ADR
  0052 -- range-checked if `:bool`, then stored via `wasm-store`), and a
  string/keyword leaf (new in ADR 0054 -- length checked against its own
  `:max-bytes` and pointer range checked against the module's own `capacity`
  exactly like `string-field-record-wat`'s `validate-parameters`, then
  stored as the pointer+length pair). Both validations are scoped inside
  this case's own branch only, matching the existing bool-validation
  precedent: validating a shared joined position unconditionally, before
  knowing which case is active, would wrongly reject a legitimate payload
  belonging to a different case occupying the same position. The
  validation half is `variant-case-validation` (ADR 0059 factors it out for
  reuse); this function adds the store half."
  [payload-offset joined-types capacity leaves]
  (let [validation (variant-case-validation joined-types capacity leaves)
        stores
        (apply str
               (map (fn [leaf]
                      (if (or (:max-bytes leaf) (:max-items leaf))
                        (let [[pointer length] (variant-indirect-leaf-value-exprs joined-types leaf)
                              offset (+ payload-offset (:relative-offset leaf))]
                          (str "      local.get $ret " pointer " i32.store offset=" offset "\n"
                               "      local.get $ret " length " i32.store offset=" (+ offset 4) "\n"))
                        (str "      local.get $ret "
                             (variant-payload-value-expr joined-types leaf)
                             " " (wasm-store (:descriptor leaf))
                             " offset=" (+ payload-offset (:relative-offset leaf)) "\n")))
                    leaves))]
    (str validation stores)))

(declare variant-layout-code)

(defn- nested-variant-case-chain
  "Dispatch one nested union using its flattened discriminant expression.
  Every payload starts at the union's shared in-memory payload offset and at
  the flat slot immediately after its discriminant. Range validation happens
  before this chain, so the final fallthrough is the final valid case."
  [cases discriminant-expr payload-base flat-base joined-types capacity store?]
  (letfn [(build [remaining index]
            (let [body (variant-layout-code
                        (:layout (first remaining))
                        payload-base flat-base joined-types capacity store?)]
              (if (= 1 (count remaining))
                body
                (str "      " discriminant-expr " i32.const " index " i32.eq\n"
                     "      if\n"
                     body
                     "      else\n"
                     (build (rest remaining) (inc index))
                     "      end\n"))))]
    (build cases 0)))

(defn- variant-layout-code
  "Recursively validate and optionally store one selected case layout.
  Products recurse by fixed field offsets. Nested unions validate and store
  their own discriminant, then recurse only into the selected inner case.
  Scalar and indirect leaves reuse the existing leaf validation/store code."
  [layout base-offset base-flat-index joined-types capacity store?]
  (cond
    (empty? (:flat layout))
    ""

    (contains? layout :fields)
    (loop [remaining (:fields layout)
           flat-index base-flat-index
           code ""]
      (if-let [{field-offset :offset field-layout :layout} (first remaining)]
        (recur (next remaining)
               (+ flat-index (count (:flat field-layout)))
               (str code
                    (variant-layout-code
                     field-layout (+ base-offset field-offset) flat-index
                     joined-types capacity store?)))
        code))

    (contains? layout :cases)
    (let [discriminant-expr
          (variant-flat-value-expr joined-types base-flat-index :i32)
          validation
          (str "      " discriminant-expr " i32.const "
               (count (:cases layout)) " i32.ge_u if unreachable end\n")
          store
          (when store?
            (str "      local.get $ret " discriminant-expr " "
                 (variant-disc-store (:discriminant-size layout))
                 " offset=" base-offset "\n"))
          cases
          (nested-variant-case-chain
           (:cases layout) discriminant-expr
           (+ base-offset (:payload-offset layout))
           (inc base-flat-index) joined-types capacity store?)]
      (str validation store cases))

    :else
    (let [leaf (cond-> {:relative-offset base-offset
                        :descriptor (:descriptor layout)
                        :flat-index base-flat-index}
                 (:max-bytes layout) (assoc :max-bytes (:max-bytes layout))
                 (:max-items layout) (assoc :max-items (:max-items layout)
                                            :item-layout (:item-layout layout)))]
      (if store?
        (variant-case-body 0 joined-types capacity [leaf])
        (variant-case-validation joined-types capacity [leaf])))))

(defn- variant-case-chain
  "The nested `if`/`else` chain that stores the active case's payload into
  the result area. Every case but the last is guarded by an explicit
  `local.get $disc i32.const <index> i32.eq`; the final case needs no guard
  because the discriminant is already range-checked (`i32.ge_u` against the
  case count) before this chain runs, so falling through every prior `else`
  leaves exactly the last case."
  [cases payload-offset joined-types capacity]
  (letfn [(build [remaining index]
            (let [body (variant-layout-code
                        (:layout (first remaining))
                        payload-offset 0 joined-types capacity true)]
              (if (= 1 (count remaining))
                body
                (str "    local.get $disc i32.const " index " i32.eq\n"
                     "    if\n"
                     body
                     "    else\n"
                     (build (rest remaining) (inc index))
                     "    end\n"))))]
    (build cases 0)))

(defn- variant-layout-indirect-headroom
  "Maximum indirect bytes reachable through one selected layout path."
  [layout]
  (cond
    (:max-bytes layout) (:max-bytes layout)
    (:max-items layout)
    (* (:max-items layout)
       (align-up (get-in layout [:item-layout :size])
                 (get-in layout [:item-layout :alignment])))
    (contains? layout :fields)
    (reduce + 0 (map #(variant-layout-indirect-headroom (:layout %))
                     (:fields layout)))
    (contains? layout :cases)
    (reduce max 0 (map #(variant-layout-indirect-headroom (:layout %))
                       (:cases layout)))
    :else 0))

(defn- variant-wat
  "Identity export for a sealed variant whose every case's payload is a
  Canonical scalar, a sealed all-scalar record (ADR 0052), or a sealed flat
  string/keyword-bearing record (ADR 0053, admitted as a case payload for
  the first time here in ADR 0054). The core function receives the variant
  already flattened by the caller's own `canon lower` -- an `i32`
  discriminant plus one core value per joined payload position
  (`canonical/layout`'s `:flat` on a variant descriptor, computed by
  `variant-flatten-payload`, which already folds a string/keyword field's
  own two-position `[:i32 :i32]` flat sequence into the join exactly like
  any other field's flat sequence -- no change was needed there) --
  range-checks the discriminant, allocates the variant's in-memory union
  result area (discriminant byte plus the widest case's payload, from the
  same layout's `:size`/`:alignment`), stores the discriminant, and then,
  in exactly the branch selected by the discriminant, un-joins and stores
  that case's own leaves (`variant-case-chain`/`variant-coerce-ops`,
  `variant-case-body`). This is the same realloc/result-area shape as
  `scalar-record-wat`/`nested-record-wat`/`string-field-record-wat`; the
  new work is the join/coercion table a variant's shared flat positions
  require (ADR 0052, unchanged) plus threading a string/keyword leaf's
  pointer+length pair and its own bounds validation through that same
  per-case branch (ADR 0054). Memory sizing follows
  `string-field-record-wat`'s generous-not-tight precedent, but keyed off
  the *widest single case* (`max-string-leaves-per-case`), not a sum across
  every case -- only one case's own payload is ever validated or stored per
  call, so only one case's own string-like leaf count needs headroom, and a
  variant with no string-like leaf in any case keeps the exact original
  one-page/65536-byte memory this function produced before ADR 0054 (no
  page-count regression for the ADR 0052 shapes already proven)."
  [function schemas]
  (let [export (wit-name (:name function))
        variant-layout (canonical/layout (first (:param-types function)) schemas)
        joined-types (vec (rest (:flat variant-layout)))
        indirect-headroom
        (reduce
         max 0
         (map #(variant-layout-indirect-headroom (:layout %))
              (:cases variant-layout)))
        needs-indirect-headroom? (pos? indirect-headroom)
        pages (max 1 (quot (+ 8 indirect-headroom
                              (:size variant-layout) 65535)
                           65536))
        capacity (* pages 65536)
        params (apply str
                      (map-indexed
                       (fn [index core-type]
                         (str " (param $p" index " " (core-type-name core-type) ")"))
                       joined-types))
        case-chain (variant-case-chain (:cases variant-layout)
                                       (:payload-offset variant-layout)
                                       joined-types capacity)]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\") (param $disc i32)" params " (result i32)\n"
     "    (local $ret i32)" (when needs-indirect-headroom? " (local $end i32)") "\n"
     "    local.get $disc i32.const " (count (:cases variant-layout)) " i32.ge_u if unreachable end\n"
     "    i32.const 0 i32.const 0 i32.const " (:alignment variant-layout)
     " i32.const " (:size variant-layout) " call $realloc local.set $ret\n"
     "    local.get $ret local.get $disc "
     (variant-disc-store (:discriminant-size variant-layout)) " offset=0\n"
     case-chain
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- scalar-record-wat [function schemas]
  (let [export (wit-name (:name function))
        record-layout (canonical/layout (first (:param-types function)) schemas)
        fields (:fields record-layout)
        params (apply str
                      (map-indexed
                       (fn [index {:keys [layout]}]
                         (str " (param $f" index " "
                              (wasm-value-type (:descriptor layout)) ")"))
                       fields))
        bool-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [layout]}]
                  (when (= :bool (:descriptor layout))
                    (str "    local.get $f" index
                         " i32.const 1 i32.gt_u if unreachable end\n")))
                fields))
        stores
        (apply str
               (map-indexed
                (fn [index {:keys [offset layout]}]
                  (str "    local.get $ret local.get $f" index " "
                       (wasm-store (:descriptor layout)) " offset=" offset "\n"))
                fields))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const 65536 i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32)\n"
     bool-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment record-layout)
     " i32.const " (:size record-layout) " call $realloc local.set $ret\n"
     stores
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- nested-record-wat
  "Identity export for a sealed record with exactly one level of nested
  record fields. Every core parameter and result-area store is planned from
  `canonical/layout-leaves`, which walks nested field layouts to absolute
  offsets in the same depth-first order as the Canonical ABI's own `:flat`
  vector; this is the only difference from `scalar-record-wat` (which is the
  degenerate zero-nesting case of the same flattening)."
  [function schemas]
  (let [export (wit-name (:name function))
        record-layout (canonical/layout (first (:param-types function)) schemas)
        leaves (canonical/layout-leaves record-layout)
        params (apply str
                      (map-indexed
                       (fn [index {:keys [descriptor]}]
                         (str " (param $f" index " " (wasm-value-type descriptor) ")"))
                       leaves))
        bool-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [descriptor]}]
                  (when (= :bool descriptor)
                    (str "    local.get $f" index
                         " i32.const 1 i32.gt_u if unreachable end\n")))
                leaves))
        stores
        (apply str
               (map-indexed
                (fn [index {:keys [offset descriptor]}]
                  (str "    local.get $ret local.get $f" index " "
                       (wasm-store descriptor) " offset=" offset "\n"))
                leaves))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const 65536 i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32)\n"
     bool-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment record-layout)
     " i32.const " (:size record-layout) " call $realloc local.set $ret\n"
     stores
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- string-field-record-wat
  "Identity export for a sealed flat record admitting bounded `string`/
  `keyword` leaves alongside Canonical scalars (`string-field-record-schema`
  -- no nesting, no variant payloads). Every leaf is planned from
  `canonical/layout-leaves`, exactly as `nested-record-wat` already does;
  the only new work is that a leaf carrying `:max-bytes` (a string or
  keyword field) takes two core wasm parameters (`$fN-ptr`/`$fN-len`)
  instead of one and is stored as that same pointer+length pair at its
  field offset -- the identical pointer+length linear-memory shape ADR
  0040/0041 already gave a bare string parameter/result, reused here
  unchanged rather than re-derived: the received pointer already refers to
  guest memory the caller populated (via this module's own `$realloc`)
  before invoking this export, so passthrough needs no byte copy, only the
  same bounds check `string-expression-wat`'s own `validate-parameters`
  already performs (length within the field's own bound, pointer range
  within the module's linear memory) before the pointer is trusted enough
  to store into the result record."
  [function schemas]
  (let [export (wit-name (:name function))
        record-layout (canonical/layout (first (:param-types function)) schemas)
        leaves (canonical/layout-leaves record-layout)
        string-like-count (count (filter :max-bytes leaves))
        required-bytes (+ 8 (* (inc string-like-count) 65536) (:size record-layout))
        pages (max 1 (quot (+ required-bytes 65535) 65536))
        capacity (* pages 65536)
        params (apply str
                      (map-indexed
                       (fn [index {:keys [descriptor max-bytes]}]
                         (if max-bytes
                           (str " (param $f" index "-ptr i32) (param $f" index "-len i32)")
                           (str " (param $f" index " " (wasm-value-type descriptor) ")")))
                       leaves))
        bool-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [descriptor max-bytes]}]
                  (when (and (not max-bytes) (= :bool descriptor))
                    (str "    local.get $f" index
                         " i32.const 1 i32.gt_u if unreachable end\n")))
                leaves))
        string-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [max-bytes]}]
                  (when max-bytes
                    (str
                     "    local.get $f" index "-len i32.const " max-bytes
                     " i32.gt_u if unreachable end\n"
                     "    local.get $f" index "-ptr local.get $f" index "-len i32.add\n"
                     "    local.tee $end local.get $f" index "-ptr i32.lt_u if unreachable end\n"
                     "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n")))
                leaves))
        stores
        (apply str
               (map-indexed
                (fn [index {:keys [offset descriptor max-bytes]}]
                  (if max-bytes
                    (str "    local.get $ret local.get $f" index "-ptr i32.store offset=" offset "\n"
                         "    local.get $ret local.get $f" index "-len i32.store offset="
                         (+ offset 4) "\n")
                    (str "    local.get $ret local.get $f" index " "
                         (wasm-store descriptor) " offset=" offset "\n")))
                leaves))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32) (local $end i32)\n"
     bool-validation
     string-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment record-layout)
     " i32.const " (:size record-layout) " call $realloc local.set $ret\n"
     stores
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- scalar-record-projection-wat [function schemas]
  (let [export (wit-name (:name function))
        {:keys [descriptor field-index]} (scalar-record-projection function schemas)
        record-layout (canonical/layout descriptor schemas)
        fields (:fields record-layout)
        result (:result function)
        params (apply str
                      (map-indexed
                       (fn [index {:keys [layout]}]
                         (str " (param $f" index " "
                              (wasm-value-type (:descriptor layout)) ")"))
                       fields))
        bool-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [layout]}]
                  (when (= :bool (:descriptor layout))
                    (str "    local.get $f" index
                         " i32.const 1 i32.gt_u if unreachable end\n")))
                fields))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const 65536 i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params
     " (result " (wasm-value-type result) ")\n"
     bool-validation
     "    local.get $f" field-index ")\n"
     "  (func (export \"cm32p2||" export "_post\") (param "
     (wasm-value-type result) "))\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- scalar-record-write-wat [function schemas plan]
  (let [export (wit-name (:name function))
        record-layout (canonical/layout (:descriptor plan) schemas)
        input-types (:input-types plan)
        params (apply str
                      (map-indexed (fn [index type]
                                     (str " (param $v" index " "
                                          (wasm-value-type type) ")"))
                                   input-types))
        bool-validation
        (apply str
               (keep-indexed (fn [index type]
                               (when (= :bool type)
                                 (str "    local.get $v" index
                                      " i32.const 1 i32.gt_u if unreachable end\n")))
                             input-types))
        stores
        (apply str
               (map (fn [{:keys [offset layout]} source]
                      (str "    local.get $ret local.get $v" source " "
                           (wasm-store (:descriptor layout)) " offset=" offset "\n"))
                    (:fields record-layout) (:field-sources plan)))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param $old-ptr i32) (param $old-size i32)\n"
     "    (param $align i32) (param $new-size i32) (result i32)\n"
     "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
     "    local.get $new-size i32.eqz if i32.const 0 return end\n"
     "    local.get $align i32.eqz if unreachable end\n"
     "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
     "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
     "    if unreachable end\n"
     "    local.get $end i32.const 65536 i32.gt_u if unreachable end\n"
     "    local.get $end global.set $next\n"
     "    local.get $old-ptr i32.eqz if else\n"
     "      local.get $old-size local.get $new-size i32.lt_u\n"
     "      if (result i32) local.get $old-size else local.get $new-size end\n"
     "      local.set $copy-size\n"
     "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
     "    end local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32)\n"
     bool-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment record-layout)
     " i32.const " (:size record-layout) " call $realloc local.set $ret\n"
     stores
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- scalar-capability-wat [function capability]
  (let [export (wit-name (:name function))
        request-type (first (:param-types function))
        result-type (:result function)
        interface (name (:interface capability))
        operation (:function capability)]
    (str
     "(module\n"
     "  (import \"cm32p2|kotoba:application/" interface "@1\" \"" operation
     "\" (func $provider (param " (wasm-value-type request-type)") (result "
     (wasm-value-type result-type) ")))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2||" export "\") (param $request "
     (wasm-value-type request-type) ") (result " (wasm-value-type result-type) ")\n"
     "    local.get $request call $provider)\n"
     "  (func (export \"cm32p2||" export "_post\") (param "
     (wasm-value-type result-type) "))\n"
     "  (func (export \"cm32p2_realloc\") (param i32 i32 i32 i32) (result i32)\n"
     "    i32.const 0)\n"
     "  (func (export \"cm32p2_initialize\")))\n")))

(defn- linear-resource-scalar-capability-wat [function capability]
  (let [export (wit-name (:name function))
        request-type (first (:param-types function))
        result-type (:result function)
        interface (name (:interface capability))
        operation (:function capability)
        module (str "cm32p2|kotoba:application/" interface "@1")]
    (str
     "(module\n"
     "  (import \"" module "\" \"issue-" operation
     "\" (func $issue (result i32)))\n"
     "  (import \"" module "\" \"execute-" operation
     "\" (func $execute (param i32 " (wasm-value-type request-type)
     ") (result " (wasm-value-type result-type) ")))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2||" export "\") (param $request "
     (wasm-value-type request-type) ") (result " (wasm-value-type result-type) ")\n"
     "    call $issue local.get $request call $execute)\n"
     "  (func (export \"cm32p2||" export "_post\") (param "
     (wasm-value-type result-type) "))\n"
     "  (func (export \"cm32p2_realloc\") (param i32 i32 i32 i32) (result i32)\n"
     "    i32.const 0)\n"
     "  (func (export \"cm32p2_initialize\")))\n")))

(defn- linear-resource-literal-capability-wat [function plan]
  (let [export (wit-name (:name function))
        capability (:capability plan)
        interface (name (:interface capability))
        operation (:function capability)
        module (str "cm32p2|kotoba:application/" interface "@1")]
    (str
     "(module\n"
     "  (import \"" module "\" \"issue-" operation
     "\" (func $issue (result i32)))\n"
     "  (import \"" module "\" \"execute-" operation
     "\" (func $execute (param i32 i64) (result i64)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (func (export \"cm32p2||" export "\") (result i64)\n"
     "    call $issue i64.const " (:request plan) " call $execute)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i64))\n"
     "  (func (export \"cm32p2_realloc\") (param i32 i32 i32 i32) (result i32)\n"
     "    i32.const 0)\n"
     "  (func (export \"cm32p2_initialize\")))\n")))

(defn- record-capability-wat [function schemas plan]
  (let [export (wit-name (:name function))
        capability (:capability plan)
        request-layout (canonical/layout (:request plan) schemas)
        result-layout (canonical/layout (:result plan) schemas)
        fields (:fields request-layout)
        params (apply str
                      (map-indexed
                       (fn [index {:keys [layout]}]
                         (str " (param $f" index " "
                              (wasm-value-type (:descriptor layout)) ")"))
                       fields))
        import-params (apply str
                             (map (fn [{:keys [layout]}]
                                    (str " (param " (wasm-value-type (:descriptor layout)) ")"))
                                  fields))
        bool-validation
        (apply str
               (keep-indexed
                (fn [index {:keys [layout]}]
                  (when (= :bool (:descriptor layout))
                    (str "    local.get $f" index
                         " i32.const 1 i32.gt_u if unreachable end\n")))
                fields))
        arguments (apply str (map #(str "    local.get $f" % "\n") (range (count fields))))]
    (str
     "(module\n"
     "  (import \"cm32p2|kotoba:application/" (:interface capability) "@1\" \""
     (:function capability) "\" (func $provider" import-params " (param i32)))\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     "  (func $realloc (export \"cm32p2_realloc\")\n"
     "    (param i32 i32) (param $align i32) (param $size i32) (result i32)\n"
     "    (local $ptr i32)\n"
     "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
     "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
     "    local.get $size i32.add global.set $next local.get $ptr)\n"
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32)\n"
     bool-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment result-layout)
     " i32.const " (:size result-layout) " call $realloc local.set $ret\n"
     arguments
     "    local.get $ret call $provider local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- max-string-leaves-per-case
  "The widest single case's own string/keyword-leaf count in `variant-
  layout` -- the shared measurement every capability-crossing variant's
  string-aware memory-sizing formula keys off (`variant-capability-string-
  headroom`, and, new in ADR 0059, `string-headroom-bytes`/`asymmetric-
  variant-capability-provider-wat`'s own independent per-side sizing),
  factored out here so both call sites compute it identically rather than
  re-deriving it."
  [variant-layout]
  (let [case-leaves (mapv (fn [case] (variant-case-leaves (:layout case))) (:cases variant-layout))]
    (reduce max 0 (map #(count (filter :max-bytes %)) case-leaves))))

(defn- variant-capability-string-headroom
  "`[pages capacity needs-string-headroom?]` for a capability-crossing
  variant layout, the capability-call twin of `variant-wat`'s own generous-
  not-tight sizing formula (same `max-string-leaves-per-case` computation,
  keyed off the *widest single case*, since only one case's own payload is
  ever active per call). New in ADR 0057: every capability-crossing variant
  WAT module before this ADR (`variant-capability-wat`/
  `variant-capability-provider-wat`) fixed its memory at exactly one page
  and never needed headroom, because `variant-capability-case?` admitted no
  string/keyword-bearing leaf. Now that it does, both the application module
  (which must hold a copy of the result's string bytes once the provider's
  result crosses back) and the provider module (which must hold a copy of
  the request's string bytes once they cross in, *and* leave room for its
  own result struct) need the same string-aware sizing `string-field-record-
  wat`/`variant-wat` already established, reused here rather than
  re-derived. Unchanged by ADR 0059 -- still used as-is by the SAME-identity
  path (`variant-capability-provider-wat`), where request-layout and
  result-layout are structurally identical so a single shared measurement is
  correct by construction; the DIFFERENT-identity path now uses
  `string-headroom-bytes` below instead, precisely because that coincidence
  no longer holds there."
  [variant-layout]
  (let [leaves-per-case (max-string-leaves-per-case variant-layout)
        needs-string-headroom? (pos? leaves-per-case)
        pages (if needs-string-headroom?
                (max 1 (quot (+ 8 (* (inc leaves-per-case) 65536)
                                (:size variant-layout) 65535)
                             65536))
                1)]
    [pages (* pages 65536) needs-string-headroom?]))

(defn- string-headroom-bytes
  "Generous-not-tight extra byte headroom one side (request OR result) of a
  capability-crossing variant needs for its OWN string/keyword leaves to be
  copied through the Canonical ABI's cross-instance string-lowering glue --
  `(inc (max-string-leaves-per-case layout)) * 65536` if that side ever
  carries a string/keyword leaf in any case, `0` otherwise. New in ADR 0059:
  factored out of `variant-capability-string-headroom`'s own combined
  `[pages capacity needs?]` formula so REQUEST-side and RESULT-side headroom
  can be computed independently. `variant-capability-wat` (application side)
  sums BOTH sides' amounts, because its own memory arena is shared, in one
  call, between whatever a caller pre-populates for the REQUEST's own
  string bytes (this module's `cm32p2_realloc` is the one a caller/harness
  must use to write them, exactly as every prior fixture in this ADR chain
  already required) and this module's OWN RESULT-string headroom (consumed
  afterward, when the crossing lowers the provider's result back into this
  module's memory) -- both draw from the same bump arena sequentially, so
  the declared capacity must cover both, not just the larger one.
  `asymmetric-variant-capability-provider-wat` (provider side) uses only the
  REQUEST-side amount from this function (for the glue's own copy-in), plus
  a *separate*, fixed (not realloc'd) allocation for its own literal RESULT
  string/keyword constants -- see `plan-result-string-data`'s own
  `:arena-base`, not this function, for that side."
  [layout]
  (let [n (max-string-leaves-per-case layout)]
    (if (pos? n) (* (inc n) 65536) 0)))

(defn- bounded-bump-realloc-wat
  "A real bounded bump allocator (`$next`-tracked, alignment-respecting,
  capacity-trapping, old-content-preserving on grow) as a standalone
  `cm32p2_realloc` export body -- exactly `variant-wat`'s own realloc,
  factored out so `variant-capability-wat` and
  `variant-capability-provider-wat` can each use it too (new in ADR 0057:
  neither previously needed more than one, single-purpose allocation per
  call, so a plain unbounded bump pointer, or in the provider's case a fixed
  constant address, was enough; a string/keyword-bearing case now means the
  Canonical ABI's own cross-instance string-copy machinery calls this
  export an *additional*, unpredictable number of times -- once per string-
  like leaf actually crossing, before this module's own body even runs --
  so both callers now need a real allocator that composes safely with those
  extra calls instead of colliding with them, and a capacity bound so an
  oversized string traps rather than silently corrupting memory past the
  module's declared page count)."
  [capacity]
  (str
   "  (func $realloc (export \"cm32p2_realloc\")\n"
   "    (param $old-ptr i32) (param $old-size i32)\n"
   "    (param $align i32) (param $new-size i32) (result i32)\n"
   "    (local $ptr i32) (local $end i32) (local $copy-size i32)\n"
   "    local.get $new-size i32.eqz if i32.const 0 return end\n"
   "    local.get $align i32.eqz if unreachable end\n"
   "    local.get $align i32.const 8 i32.gt_u if unreachable end\n"
   "    local.get $align local.get $align i32.const 1 i32.sub i32.and if unreachable end\n"
   "    global.get $next local.get $align i32.const 1 i32.sub i32.add\n"
   "    i32.const 0 local.get $align i32.sub i32.and local.tee $ptr\n"
   "    local.get $new-size i32.add local.tee $end local.get $ptr i32.lt_u\n"
   "    if unreachable end\n"
   "    local.get $end i32.const " capacity " i32.gt_u if unreachable end\n"
   "    local.get $end global.set $next\n"
   "    local.get $old-ptr i32.eqz if else\n"
   "      local.get $old-size local.get $new-size i32.lt_u\n"
   "      if (result i32) local.get $old-size else local.get $new-size end\n"
   "      local.set $copy-size\n"
   "      local.get $ptr local.get $old-ptr local.get $copy-size memory.copy\n"
   "    end local.get $ptr)\n"))

(defn- structural-union-construction-wat [function schemas plan]
  (let [export (wit-name (:name function))
        layout (canonical/layout (:descriptor plan) schemas)
        payload-type (:payload-type plan)
        payload-offset (:payload-offset layout)
        bool-validation (when (= :bool payload-type)
                          "    local.get $value i32.const 1 i32.gt_u if unreachable end\n")
        payload-store (when payload-type
                        (str "    local.get $ret local.get $value "
                             (wasm-store payload-type) " offset=" payload-offset "\n"))]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") 1 1)\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     (bounded-bump-realloc-wat 65536)
     "  (func (export \"cm32p2||" export "\")"
     (when payload-type (str " (param $value " (wasm-value-type payload-type) ")"))
     " (result i32)\n"
     "    (local $ret i32)\n"
     bool-validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment layout)
     " i32.const " (:size layout) " call $realloc local.set $ret\n"
     "    local.get $ret i32.const " (:case-index plan) " "
     (variant-disc-store (:discriminant-size layout)) " offset=0\n"
     payload-store
     "    local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- structural-union-elimination-wat [function schemas plan]
  (let [export (wit-name (:name function))
        layout (canonical/layout (:descriptor plan) schemas)
        joined-types (vec (rest (:flat layout)))
        joined-params
        (apply str
               (map-indexed
                (fn [index core-type]
                  (str " (param $p" index " " (core-type-name core-type) ")"))
                joined-types))
        predicate? (= :predicate (:kind plan))
        payload-type (:payload-type plan)
        result-type (if predicate? :bool payload-type)
        fallback-param (when-not predicate?
                         (str " (param $fallback "
                              (wasm-value-type payload-type) ")"))
        predicate-expression
        (case (:operation plan)
          option-some?-of "    local.get $disc\n"
          result-ok?-of "    local.get $disc i32.eqz\n"
          nil)
        payload-expression
        (when-not predicate?
          (variant-flat-value-expr joined-types 0 (core-type-of payload-type)))
        selected-body
        (when-not predicate?
          (if (= :bool payload-type)
            (str "      " payload-expression
                 " local.tee $selected i32.const 1 i32.gt_u if unreachable end\n"
                 "      local.get $selected\n")
            (str "      " payload-expression "\n")))
        projection-expression
        (when-not predicate?
          (str
           (when (= :bool payload-type)
             "    local.get $fallback i32.const 1 i32.gt_u if unreachable end\n")
           "    local.get $disc i32.const " (:selected-case plan) " i32.eq\n"
           "    if (result " (wasm-value-type payload-type) ")\n"
           selected-body
           "    else\n"
           "      local.get $fallback\n"
           "    end\n"))]
    (str
     "(module\n"
     "  (func (export \"cm32p2||" export "\") (param $disc i32)"
     joined-params fallback-param " (result " (wasm-value-type result-type) ")\n"
     (when (= :bool payload-type) "    (local $selected i32)\n")
     "    local.get $disc i32.const 2 i32.ge_u if unreachable end\n"
     (if predicate? predicate-expression projection-expression)
     "  )\n"
     "  (func (export \"cm32p2||" export "_post\") (param "
     (wasm-value-type result-type) "))\n"
     "  (func (export \"cm32p2_initialize\")))\n")))

(defn- variant-capability-wat
  "Application-side standard32 core module for a direct `typed-cap-call`
  whose request/result is one sealed variant admitted by
  `variant-capability-schema`. The joined component-flat signature (`$disc`
  plus one core param per joined payload position, exactly `variant-wat`'s
  own signature -- `canonical/layout`'s `:flat` on the variant descriptor)
  is unchanged from the identity-export case; this module never itself
  un-joins or stores a case's payload -- it allocates the variant's
  Canonical result area (`realloc` sized to the variant layout's own
  `:size`/`:alignment`, exactly as `variant-wat` does for its own
  self-allocated result), forwards the discriminant and every joined
  payload value plus that result pointer to the imported provider function
  unchanged, and returns the same pointer. This is `record-capability-wat`'s
  exact division of labor (forward flat values plus a caller-allocated
  result pointer to an import that has no core result, matching the
  Canonical ABI's own indirect-result convention for a >1-flat-value
  result) generalized from a record's flat field list to a variant's
  joined `:flat` case-value list. Unlike `record-capability-wat` (which
  validates bool fields on the *application* side before crossing),
  disc-range-checking and per-case validation for a variant can only be
  done correctly by whichever side actually knows which case is active,
  which is exactly the side that performs the case-dispatch store -- here,
  the provider (`variant-capability-provider-wat`), which reuses
  `variant-wat`'s own case-chain (disc range check plus in-branch bool and
  string/keyword-bounds validation) unmodified. The application module
  therefore performs no validation of its own; it is a thin pass-through,
  exactly mirroring `scalar-capability-wat`'s own no-validation precedent
  for a single scalar leaf, now applied to every joined position of a
  variant, string/keyword pointer+length pairs included -- new in ADR 0057
  is only that this module's own memory/`$realloc` must now tolerate the
  *additional* realloc calls the Canonical ABI's own cross-instance string
  lowering makes when copying a result string's bytes back into this
  module's memory (`variant-capability-string-headroom`/
  `bounded-bump-realloc-wat`, the same string-aware sizing and bounded
  allocator `variant-wat` already uses for its own single-module string
  leaves); the WAT emitted for a case with no string-like leaf at all is
  otherwise unchanged in shape from ADR 0055/0056 (still one page, still
  the same bump-pointer body, now just capacity-checked).

  ADR 0058 generalizes this function to a REQUEST layout and a (possibly
  different) RESULT layout, computed independently from `(:request plan)`
  and `(:result plan)`, rather than one shared `variant-layout` computed
  from the request alone and silently reused for the result area too. This
  is a genuine bug fix, not a cosmetic rename: the result area this module
  allocates and hands to the provider as an out-pointer must be sized to
  hold a RESULT-shaped value (the provider writes a result into it), which
  for every ADR 0055/0056/0057 fixture happened to be correct only because
  their request and result were, by construction, the identical schema --
  sizing it from the request layout was silently relying on that
  coincidence. For the different-identity case (ADR 0058) request and
  result layouts genuinely differ, so this fix is required for correctness,
  not merely for symmetry; for the same-identity case it is a no-op
  (`request-layout` and `result-layout` are structurally `=`, confirmed by
  rebuilding and re-running the ADR 0055/0056/0057 fixtures unchanged
  through this generalized function). The joined param signature is still
  sized from the REQUEST layout only, unchanged: this module forwards the
  request's own joined payload values to the import exactly as before.

  ADR 0059 fixes the remaining half of the same coincidence ADR 0058 left
  standing: memory-PAGE sizing (as opposed to the `$realloc` call's own
  struct SIZE, already fixed by ADR 0058) was still `(variant-capability-
  string-headroom request-layout)` alone -- correct for ADR 0055/0056/0057
  (request-layout = result-layout there) and for ADR 0058's own scope
  (neither side ever carried a string/keyword leaf), but wrong in general:
  this module's OWN declared memory is the ONLY memory a caller/harness can
  write a REQUEST string's bytes into (via this module's own exported
  `cm32p2_realloc`, exactly as every prior string-bearing fixture in this
  chain already required) *and* the memory this module's OWN `$realloc`
  must have room in when the crossing later lowers a RESULT string's bytes
  back into it (ADR 0057's own finding, restated in this function's own
  docstring above) -- two genuinely different consumers of the SAME shared
  arena within one call, now that a string/keyword leaf can appear on
  EITHER side independently. `string-headroom-bytes` is computed separately
  for `request-layout` and `result-layout` and SUMMED (not maxed): both
  amounts are real, sequential draws against the one arena within a single
  call (a caller populating a request string, THEN this module's own
  result-string headroom later), so the declared capacity must cover both
  at once, not merely the larger of the two. This is a strict widening,
  never a regression: when neither side carries a string/keyword leaf both
  summands are `0` and the formula reduces to exactly the prior one-page
  result (confirmed by rebuilding and re-running the ADR 0055/0056/0057/
  0058 no-string and scalar/record-only fixtures unchanged through this
  function -- see Evidence)."
  [function schemas plan]
  (let [export (wit-name (:name function))
        capability (:capability plan)
        request-layout (canonical/layout (:request plan) schemas)
        result-layout (canonical/layout (:result plan) schemas)
        joined-types (vec (rest (:flat request-layout)))
        request-headroom-bytes (string-headroom-bytes request-layout)
        result-headroom-bytes (string-headroom-bytes result-layout)
        needs-headroom? (or (pos? request-headroom-bytes) (pos? result-headroom-bytes))
        pages (if needs-headroom?
                (max 1 (quot (+ 8 request-headroom-bytes result-headroom-bytes
                                (:size result-layout) 65535)
                             65536))
                1)
        capacity (* pages 65536)
        payload-params (apply str
                              (map (fn [core-type] (str " (param " (core-type-name core-type) ")"))
                                   joined-types))
        import-params (str " (param i32)" payload-params)
        params (apply str
                      (cons " (param $disc i32)"
                            (map-indexed
                             (fn [index core-type]
                               (str " (param $p" index " " (core-type-name core-type) ")"))
                             joined-types)))
        arguments (apply str
                         (cons "    local.get $disc\n"
                               (map-indexed (fn [index _] (str "    local.get $p" index "\n"))
                                            joined-types)))]
    (str
     "(module\n"
     "  (import \"cm32p2|kotoba:application/" (:interface capability) "@1\" \""
     (:function capability) "\" (func $provider" import-params " (param i32)))\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     (bounded-bump-realloc-wat capacity)
     "  (func (export \"cm32p2||" export "\")" params " (result i32)\n"
     "    (local $ret i32) (local $end i32)\n"
     "    i32.const 0 i32.const 0 i32.const " (:alignment result-layout)
     " i32.const " (:size result-layout) " call $realloc local.set $ret\n"
     arguments
     "    local.get $ret call $provider local.get $ret)\n"
     "  (func (export \"cm32p2||" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn variant-capability-provider-wat
  "Wiring-only provider core module for one sealed variant admitted by
  `variant-capability-schema` (ADR 0055/0056/0057) -- public so
  `kotoba.component.composition` can build a provider artifact for
  it (mirroring `kotoba.component.composition/record-provider-wat`,
  which duplicates `record-capability-wat`'s own store shape locally rather
  than calling into this namespace; this function is exposed directly
  instead because the case-chain it reuses, `variant-case-chain`, is
  materially more involved than a flat record's own field loop and
  duplicating it would risk the two copies silently drifting). `entry` is a
  `{:interface :function}` capability map (the same shape
  `component-wit/contract`'s own `:capabilities` entries and
  `component-composition`'s local `capability` lookup already use).
  Reuses `variant-wat`'s exact case-chain (disc range check, then in-branch
  bool and, since ADR 0057 widened admission, string/keyword-bounds
  validation and store for the active case) unchanged.

  New in ADR 0057: the result pointer is no longer the fixed minimal
  address ADR 0055/0056's identity-only providers used (`i32.const 8`, no
  dynamic allocation) -- it is now allocated through this module's own
  `cm32p2_realloc` (`bounded-bump-realloc-wat`), exactly the way every
  other struct-producing WAT emitter in this namespace already allocates
  its own result area. This is not a cosmetic change: once a case's payload
  can carry a string/keyword leaf, the Canonical ABI's own cross-instance
  string-copy machinery calls this module's *exported* `cm32p2_realloc`
  itself -- once per string-like leaf in the active request case -- to copy
  each leaf's bytes into this module's memory *before* this module's own
  function body runs at all. A fixed constant-address realloc (correct only
  when it is ever the sole allocation in a call, true for every case shape
  ADR 0055/0056 admitted) would silently collide: the incoming string
  bytes and this function's own result struct would land at the identical
  address, corrupting whichever was written second. Routing the result
  struct through the same bump allocator the string copies already use
  keeps every allocation in one call sequential and non-overlapping,
  regardless of call order between this module's own body and the
  generated glue -- a plain consequence of a bump allocator's own
  construction, not case-specific logic. A case with no string-like leaf at
  all is unaffected in behavior: with no extra realloc call preceding it,
  the bump allocator's first call still returns the same fixed address 8
  ADR 0055/0056 hard-coded, for the same alignment reason `variant-wat`'s
  own realloc already establishes (8 is a multiple of every alignment this
  codebase's Canonical ABI layouts produce, so the bump math's first result
  is always exactly 8)."
  [entry descriptor schemas]
  (let [export (str "cm32p2|kotoba:application/" (:interface entry) "@1|" (:function entry))
        variant-layout (canonical/layout descriptor schemas)
        joined-types (vec (rest (:flat variant-layout)))
        [pages capacity needs-string-headroom?] (variant-capability-string-headroom variant-layout)
        params (apply str
                      (cons " (param $disc i32)"
                            (map-indexed
                             (fn [index core-type]
                               (str " (param $p" index " " (core-type-name core-type) ")"))
                             joined-types)))
        case-chain (variant-case-chain (:cases variant-layout)
                                       (:payload-offset variant-layout)
                                       joined-types capacity)]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const 8))\n"
     (bounded-bump-realloc-wat capacity)
     "  (func (export \"" export "\")" params " (result i32)\n"
     "    (local $ret i32)" (when needs-string-headroom? " (local $end i32)") "\n"
     "    i32.const 0 i32.const 0 i32.const " (:alignment variant-layout)
     " i32.const " (:size variant-layout) " call $realloc local.set $ret\n"
     "    local.get $disc i32.const " (count (:cases variant-layout)) " i32.ge_u if unreachable end\n"
     "    local.get $ret local.get $disc "
     (variant-disc-store (:discriminant-size variant-layout)) " offset=0\n"
     case-chain
     "    local.get $ret)\n"
     "  (func (export \"" export "_post\") (param i32)\n"
     "    i32.const 8 global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const 8 global.set $next))\n")))

(defn- constant-leaf-wat
  "A deterministic literal WAT push instruction for one Canonical SCALAR leaf
  of an asymmetric provider's fixed result payload (ADR 0058), distinct per
  `(case-index, leaf-index)` pair so a real round trip can tell two
  different chosen result cases -- and two different leaves within the same
  case -- apart, rather than every leaf silently sharing one constant (which
  would make the evidence weaker at distinguishing 'stored the right value
  at the right offset' from 'stored one lucky constant everywhere'). This
  value is never derived from any request payload leaf -- only from the
  chosen result case's own static index and the leaf's own position within
  it -- matching `asymmetric-variant-capability-provider-wat`'s own
  'wiring-only, not semantic' framing. Scalar-only, unchanged by ADR 0059: a
  string/keyword leaf cannot be a literal WAT push instruction at all (it is
  a pointer+length pair into linear memory, not a single core value) and
  uses `deterministic-constant-string`/`plan-result-string-data` instead --
  see `asymmetric-result-case-store`, the caller that dispatches between the
  two."
  [descriptor case-index leaf-index]
  (let [n (+ (* case-index 101) (* leaf-index 7) 3)]
    (case descriptor
      :bool (str "i32.const " (mod n 2))
      :i64 (str "i64.const " n)
      :f32 (str "f32.const " n)
      :f64 (str "f64.const " n))))

(defn- deterministic-constant-string
  "Deterministic literal UTF-8 content for one asymmetric provider's fixed
  string/keyword RESULT leaf (new in ADR 0059), the string/keyword-leaf
  counterpart to `constant-leaf-wat`'s own scalar-leaf constants and
  distinct per `(case-index, leaf-index)` pair for the identical reason:
  so a real round trip can tell two different chosen result cases, and two
  different leaves within the same case, apart. Deliberately small (a short
  fixed label, always well under either Kotoba byte bound --
  `keyword-value-byte-limit`/`string-value-byte-limit`) because this
  content is a compile-time literal this function itself generates and can
  never grow past what this ADR chooses to emit; unlike a REQUEST leaf's
  own bound, which a real caller controls and this ADR's new request-side
  validation (`asymmetric-request-validation-chain`) checks for real, a
  RESULT leaf's own bound is trivially satisfied by construction and is not
  independently exercised as a trap -- see this ADR's own Evidence/Remaining
  gaps for why that asymmetry is honest rather than an oversight."
  [case-index leaf-index]
  (str "kotoba/state-case-" case-index "-leaf-" leaf-index))

(defn- plan-result-string-data
  "`{:segments {[case-index leaf-index] {:pointer :length}} :data-wat
  <string> :arena-base <int>}` for every RESULT case's own string/keyword
  leaf (new in ADR 0059) -- walks `cases` (`(:cases result-layout)`) via
  `variant-case-leaves` (unmodified) exactly as `asymmetric-result-case-
  store` itself will, so `leaf-index` here and there always agree, and
  assigns each string/keyword leaf a FIXED, sequential, 8-byte-aligned
  data-segment address plus its own `deterministic-constant-string` bytes
  -- embedded exactly like `string-expression-wat`'s own literal-string data
  segments (a fixed address, since the content is a compile-time constant
  that needs no runtime allocation at all, unlike a REQUEST leaf's bytes,
  which genuinely must be realloc'd because they arrive from an unknown
  caller). `:arena-base` is where this module's own dynamic bump allocator
  ($next`'s initial value, and what `_post`/`cm32p2_initialize` reset it
  back to) must start, strictly after every literal segment -- exactly
  `prepare-leaves`'s own arena-base convention in `string-expression-wat`,
  reused here for the same reason. A case set with no string/keyword leaf
  anywhere (every ADR 0058 fixture) produces `{:segments {} :data-wat \"\"
  :arena-base 8}`, identical to the fixed `i32.const 8` this function's
  caller hard-coded before this ADR -- confirmed by rebuilding and
  re-running the ADR 0058 fixtures through the changed function (see
  Evidence)."
  [cases]
  (let [entries (for [[case-index case] (map-indexed vector cases)
                       [leaf-index leaf] (map-indexed vector (variant-case-leaves (:layout case)))
                       :when (:max-bytes leaf)]
                   [case-index leaf-index])]
    (loop [remaining entries offset 8 segments {} data-wat ""]
      (if-let [[case-index leaf-index] (first remaining)]
        (let [content (deterministic-constant-string case-index leaf-index)
              bytes (vec (.getBytes ^String content "UTF-8"))]
          (recur (next remaining) (+ offset (count bytes))
                 (assoc segments [case-index leaf-index]
                        {:pointer offset :length (count bytes)})
                 (str data-wat "  (data (i32.const " offset ") \"" (wat-data bytes) "\")\n")))
        {:segments segments :data-wat data-wat :arena-base (align-up offset 8)}))))

(defn- asymmetric-result-case-store
  "The stores for one RESULT case's own fixed constant payload, reusing
  `variant-case-leaves` (unmodified) to find each leaf's own relative
  offset, descriptor, and (new in ADR 0059) `:max-bytes` -- exactly
  `variant-case-body`'s store half, with `constant-leaf-wat` standing in
  for a scalar leaf's request-derived value and, new here, a FIXED
  pointer+length pair (from `string-segments`, `plan-result-string-data`'s
  own output) standing in for a string/keyword leaf's -- no validation at
  all for either kind, since a compile-time constant (scalar OR the fixed
  data-segment address of a compile-time literal string) is trivially
  in-bounds by construction."
  [result-payload-offset case-index layout string-segments]
  (let [leaves (variant-case-leaves layout)]
    (apply str
           (map-indexed
            (fn [leaf-index {:keys [relative-offset descriptor max-bytes]}]
              (let [offset (+ result-payload-offset relative-offset)]
                (if max-bytes
                  (let [{:keys [pointer length]} (get string-segments [case-index leaf-index])]
                    (str "      local.get $ret i32.const " pointer " i32.store offset=" offset "\n"
                         "      local.get $ret i32.const " length " i32.store offset="
                         (+ offset 4) "\n"))
                  (str "      local.get $ret " (constant-leaf-wat descriptor case-index leaf-index)
                       " " (wasm-store descriptor)
                       " offset=" offset "\n"))))
            leaves))))

(defn- asymmetric-provider-dispatch-chain
  "The nested `if`/`else` chain, dispatched on the REQUEST's own
  discriminant, that writes a FIXED result value into `$ret` for an
  asymmetric-identity provider (ADR 0058). One deterministic output case is
  chosen per request case via `(mod request-case-index (count
  result-cases))`, so every request case maps to some valid result case
  even when the two case counts differ (e.g. `state-v1`'s own 3 request
  cases vs. 5 result cases), and every possible request discriminant is
  provably distinguishable in a round trip (`constant-leaf-wat`/
  `deterministic-constant-string` each vary by the chosen output case's own
  index). This is wiring-only, not a semantic mapping: no request PAYLOAD
  leaf value is ever read here, only the request's own discriminant
  (already range-checked by the caller before this chain runs), to select
  purely which fixed result case gets written. `string-segments` (new in
  ADR 0059, `plan-result-string-data`'s own output) is threaded straight
  through to `asymmetric-result-case-store`, unused by this function
  itself."
  [request-case-count result-cases result-disc-size result-payload-offset string-segments]
  (let [result-case-count (count result-cases)]
    (letfn [(build [index]
              (let [output-index (mod index result-case-count)
                    output-layout (:layout (nth result-cases output-index))
                    body (str "      local.get $ret i32.const " output-index " "
                              (variant-disc-store result-disc-size) " offset=0\n"
                              (asymmetric-result-case-store result-payload-offset output-index
                                                            output-layout string-segments))]
                (if (= index (dec request-case-count))
                  body
                  (str "    local.get $disc i32.const " index " i32.eq\n"
                       "    if\n" body
                       "    else\n" (build (inc index))
                       "    end\n"))))]
      (build 0))))

(defn- asymmetric-request-validation-chain
  "The nested `if`/`else` chain, dispatched on the REQUEST's own
  discriminant, that VALIDATES (and never stores) the active REQUEST case's
  own leaves against their Kotoba-declared bounds (`variant-case-
  validation`) -- new in ADR 0059, needed for the first time now that a
  REQUEST case admitted by `asymmetric-variant-capability-case?` can carry
  a string/keyword leaf: `asymmetric-variant-capability-provider-wat` never
  reads or stores any request payload VALUE (see its own docstring, `it
  range-checks the request discriminant, never reads any request PAYLOAD
  leaf`), so without this chain an oversized request string/keyword leaf
  would cross the boundary entirely unchecked, breaking every prior ADR's
  fail-closed byte-bound discipline for no reason other than this
  provider's own dispatch having no other cause to touch the leaf's value.
  Mirrors `variant-case-chain`'s own dispatch shape exactly, substituting
  `variant-case-validation` for `variant-case-body` (validation only, no
  store, so no `$ret`/result-area dependency at all -- this chain can run
  entirely before `$ret` is even allocated)."
  [request-cases joined-types capacity]
  (letfn [(build [remaining index]
            (let [leaves (variant-case-leaves (:layout (first remaining)))
                  body (variant-case-validation joined-types capacity leaves)]
              (if (= 1 (count remaining))
                body
                (str "    local.get $disc i32.const " index " i32.eq\n"
                     "    if\n" body
                     "    else\n" (build (rest remaining) (inc index))
                     "    end\n"))))]
    (build request-cases 0)))

(defn asymmetric-variant-capability-provider-wat
  "Wiring-only provider core module for a `typed-cap-call` whose request and
  result are two genuinely DIFFERENT sealed variant identities (ADR 0058),
  each independently admitted by `asymmetric-variant-capability-schema`,
  which since ADR 0059 also admits ADR 0057's sealed flat string/keyword-
  bearing record case (`asymmetric-variant-capability-case?`'s own
  docstring records the reasoning). Unlike `variant-capability-provider-wat`
  (whose whole semantic IS echoing the active request case verbatim into a
  result area of the identical shape, only meaningful when request and
  result share one schema), this provider cannot echo a request case into a
  result case of a genuinely different, unrelated shape. It is a
  deliberately simple, explicitly non-semantic wiring fixture instead,
  matching the task's own framing exactly: it validates (ADR 0059) but
  never reads for its own purposes any request PAYLOAD leaf, and writes one
  of the result variant's own cases with a fixed compile-time-constant
  payload, chosen deterministically from the request's own discriminant
  alone (`asymmetric-provider-dispatch-chain`) -- this is NOT `state`'s
  real semantics (no request payload value ever informs the result's own
  value, only which case is chosen), exactly the same 'wiring only' framing
  every prior capability-call ADR's own identity provider already carried,
  now applied to a provider that (unlike an identity provider) cannot even
  in principle be semantically neutral, because request and result are
  different types.

  ADR 0059's own new engineering, precisely: (1) `plan-result-string-data`
  embeds one fixed `(data ...)` literal per string/keyword RESULT leaf
  (across every result case, not only the ones this dispatch's own `mod`
  arithmetic happens to reach, so this module stays correct regardless of
  case-count-ratio reachability), with the module's own dynamic bump
  allocator ($next`) starting strictly after that fixed region
  (`:arena-base`, not the old hard-coded `8`) -- a compile-time literal
  needs no runtime allocation, unlike a copied-in REQUEST string, so this is
  a FIXED allocation, not routed through `$realloc` at all. (2) This
  module's own memory/page count must still independently accommodate the
  Canonical ABI's own cross-instance string-copy glue calling this module's
  *exported* `cm32p2_realloc` once per string-like leaf in the ACTIVE
  REQUEST case, before this module's own body runs at all -- exactly ADR
  0057's own finding, now reached for the asymmetric path for the first
  time -- so total capacity is `arena-base` (fixed literal region) PLUS
  `(string-headroom-bytes request-layout)` (glue-driven copy-in headroom)
  PLUS the result struct's own `:size`, generalizing the flat one-page/
  65536-byte constant ADR 0058 used (a true no-op when neither `arena-base`
  exceeds 8 nor the request carries a string leaf -- confirmed by
  rebuilding and re-running the ADR 0058 fixtures unchanged through this
  function). (3) `asymmetric-request-validation-chain` validates (but never
  stores) the active REQUEST case's own leaves against their Kotoba bounds
  -- see that function's own docstring for why this is necessary now,
  unlike before ADR 0059 when no request case could ever carry a
  string/keyword leaf at all."
  [entry request-descriptor result-descriptor schemas]
  (let [export (str "cm32p2|kotoba:application/" (:interface entry) "@1|" (:function entry))
        request-layout (canonical/layout request-descriptor schemas)
        result-layout (canonical/layout result-descriptor schemas)
        joined-types (vec (rest (:flat request-layout)))
        params (apply str
                      (cons " (param $disc i32)"
                            (map-indexed
                             (fn [index core-type]
                               (str " (param $p" index " " (core-type-name core-type) ")"))
                             joined-types)))
        {:keys [segments data-wat arena-base]} (plan-result-string-data (:cases result-layout))
        request-headroom-bytes (string-headroom-bytes request-layout)
        needs-request-validation? (pos? request-headroom-bytes)
        required-bytes (+ arena-base request-headroom-bytes (:size result-layout))
        pages (max 1 (quot (+ required-bytes 65535) 65536))
        capacity (* pages 65536)
        validation (when needs-request-validation?
                     (asymmetric-request-validation-chain (:cases request-layout) joined-types capacity))
        dispatch (asymmetric-provider-dispatch-chain
                  (count (:cases request-layout)) (:cases result-layout)
                  (:discriminant-size result-layout) (:payload-offset result-layout)
                  segments)]
    (str
     "(module\n"
     "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
     "  (global $next (mut i32) (i32.const " arena-base "))\n"
     (bounded-bump-realloc-wat capacity)
     "  (func (export \"" export "\")" params " (result i32)\n"
     "    (local $ret i32)" (when needs-request-validation? " (local $end i32)") "\n"
     "    local.get $disc i32.const " (count (:cases request-layout)) " i32.ge_u if unreachable end\n"
     validation
     "    i32.const 0 i32.const 0 i32.const " (:alignment result-layout)
     " i32.const " (:size result-layout) " call $realloc local.set $ret\n"
     dispatch
     "    local.get $ret)\n"
     "  (func (export \"" export "_post\") (param i32)\n"
     "    i32.const " arena-base " global.set $next)\n"
     "  (func (export \"cm32p2_initialize\") i32.const " arena-base " global.set $next)\n"
     data-wat
     ")\n")))

(def state-provider-table-capacity
  "Slot count for `state-provider-wat`'s bounded in-memory key/value table --
  matches `provider.state/max-entries` (256, the pure-
  Clojure reference provider's own bound, itself equal to `state-v1.edn`'s
  own declared `:limits {:entries 256 ...}`) exactly (ADR 0061). ADR 0060
  deliberately narrowed this to `4` for its first real read/write/dispatch
  increment, on the stated premise that growing it back to the reference's
  real bound would be a mechanical, separate follow-up: 'the per-slot
  layout and unrolled scan generalize directly to any fixed compile-time
  slot count'. That premise held -- `state-scan-wat` already built its
  per-slot branches via `(map ... (range capacity))`, a Clojure-side
  generator over this constant, not hand-written cases, and
  `state-slot-layout`/`state-provider-wat`'s own memory-sizing math
  (`table-size`, `pages`) is already `capacity`-parametric -- so reaching
  256 needed exactly this one constant change plus proportionally more WAT
  text at emission time (a bigger `(module ...)`, not a different shape).
  `package-state-provider`/`state-provider-wat` both keep their own
  explicit-`capacity` arities so a test/evidence fixture can still request a
  SMALLER table (e.g. `4`, to re-run ADR 0060's own stateful-sequence
  fixture unchanged as a no-regression check) without touching this
  production default."
  256)

(defn- field-by-name
  "The `{:name :offset :layout}` entry of `record-layout`'s own `:fields`
  (`kotoba.wasm.canonical-abi/record-layout`'s output) whose `:name`
  matches `field-name` -- `variant-case-leaves` elsewhere in this namespace
  deliberately drops field names (it only needs relative offset/descriptor
  order for the wiring-only providers), but `state-provider-wat` needs to
  address `entry`'s `key`/`value`/`version` and `error`'s `code`/`message`
  fields BY NAME (the request/result shape is fixed and known, but which
  field is which is not assumed from position alone)."
  [record-layout field-name]
  (or (some #(when (= field-name (:name %)) %) (:fields record-layout))
      (reject "state provider record has no matching field" {:field field-name})))

(def ^:private bytes-equal-wat
  "One reusable `$bytes-equal` core function: real byte-CONTENT equality over
  two `(pointer, length)` linear-memory ranges, via an actual bounded WASM
  `loop` reading one byte at a time (`i32.load8_u`) and comparing it --
  distinct from every existing helper in this namespace, all of which only
  ever `memory.copy` bytes (moving them) or compare a SINGLE scalar leaf
  (`i32`/`i64`/`f32`/`f64`/bool), never walk and compare byte CONTENT of a
  runtime-length range. This is the genuinely new kind of Wasm logic
  `state-provider-wat`'s own key-lookup scan needs (matching a request's
  `key` against each occupied table slot's own stored key) that no prior ADR
  in this chain needed: every earlier provider was either a wiring-only
  identity/fixture (never inspecting payload bytes at all) or a fixed
  compile-time-literal writer (ADR 0058/0059's asymmetric provider, which
  reads only the request's own DISCRIMINANT, never a payload leaf's VALUE).
  Length is checked first (a cheap reject before touching a single byte); the
  loop itself walks both ranges in lockstep, using an early `return` the
  moment a byte mismatches (0) or the whole range has matched (1) -- the only
  function in this namespace that uses `return` mid-body rather than nested
  `if`/`else`, a deliberate, narrow exception: unlike every other WAT emitter
  here (whose top-level dispatch functions build a nested if/else chain over
  a small, fixed, compile-time case count), this loop's own trip count is a
  genuinely RUNTIME value (the key's byte length, up to
  `value/keyword-value-byte-limit`), which nested if/else cannot express at
  all -- `return` inside a `loop`/`if` is standard, well-defined WASM control
  flow (the same 'push a value then diverge' shape `if unreachable end`
  already uses pervasively in this namespace for traps, just diverging via
  `return` instead of `unreachable`)."
  (str
   "  (func $bytes-equal (param $a i32) (param $alen i32)"
   " (param $b i32) (param $blen i32) (result i32)\n"
   "    (local $i i32)\n"
   "    local.get $alen local.get $blen i32.ne if i32.const 0 return end\n"
   "    i32.const 0 local.set $i\n"
   "    loop $scan\n"
   "      local.get $i local.get $alen i32.ge_u if i32.const 1 return end\n"
   "      local.get $a local.get $i i32.add i32.load8_u\n"
   "      local.get $b local.get $i i32.add i32.load8_u\n"
   "      i32.ne if i32.const 0 return end\n"
   "      local.get $i i32.const 1 i32.add local.set $i\n"
   "      br $scan\n"
   "    end\n"
   "    i32.const 1)\n"))

(defn- state-slot-layout
  "Byte layout of one bounded key/value table slot for `state-provider-wat`,
  computed once and reused by every reader/writer of the table: `occupied`
  (i32 flag, 0/1), `version` (i64, matching
  `provider.state`'s own GLOBAL per-provider-instance
  monotonic counter -- not a per-key counter, see `state-provider-wat`'s own
  docstring for why this ADR preserves that exact, slightly surprising
  convention rather than inventing a more conventional per-key one), then
  `key-len`/`key-bytes` and `value-len`/`value-bytes`. Both byte buffers are
  sized to the FULL Kotoba-declared bound
  (`value/keyword-value-byte-limit`/`value/string-value-byte-limit`), not a
  narrower internal cap: a request already admitted by the reused
  `asymmetric-request-validation-chain` byte-bound check is therefore
  GUARANTEED to fit its own slot, with no second, narrower, provider-private
  fail-closed dimension the task did not ask for."
  [capacity]
  (let [occupied-offset 0
        version-offset (align-up (+ occupied-offset 4) 8)
        key-len-offset (+ version-offset 8)
        key-bytes-offset (align-up (+ key-len-offset 4) 8)
        key-bytes-size value/keyword-value-byte-limit
        value-len-offset (+ key-bytes-offset key-bytes-size)
        value-bytes-offset (align-up (+ value-len-offset 4) 8)
        value-bytes-size value/string-value-byte-limit
        slot-size (align-up (+ value-bytes-offset value-bytes-size) 8)]
    {:occupied-offset occupied-offset
     :version-offset version-offset
     :key-len-offset key-len-offset
     :key-bytes-offset key-bytes-offset
     :key-bytes-size key-bytes-size
     :value-len-offset value-len-offset
     :value-bytes-offset value-bytes-offset
     :value-bytes-size value-bytes-size
     :slot-size slot-size
     :table-base 8
     :table-size (* capacity slot-size)
     :capacity capacity}))

(defn- state-scan-wat
  "WAT computing `$match` (the occupied slot whose stored key byte-equals the
  active request's own key, via `$bytes-equal`, or -1) and `$free` (the
  lowest-indexed unoccupied slot, or -1). Unrolls one straight-line check per
  slot -- `capacity` is small and fixed at COMPILE time, so a plain sequence
  of `if`/`else` branches (matching every other dispatch chain in this
  namespace, e.g. `variant-case-chain`/`asymmetric-provider-dispatch-chain`)
  is simpler to hand-verify than a real indexed loop over the table itself;
  `$bytes-equal`'s own loop is the only place this provider needs
  RUNTIME-length iteration, since the table's own slot COUNT is always known
  at compile time. Assumes `$match`/`$free` locals are already declared and
  `$p0`/`$p1` hold the active request's own key pointer/length -- true for
  `get`/`put`/`delete` alike in `state-v1`'s own shape, since key is always
  the first joined position for every one of its three request cases."
  [{:keys [capacity table-base slot-size occupied-offset key-len-offset key-bytes-offset]}]
  (str
   "    i32.const -1 local.set $match\n"
   "    i32.const -1 local.set $free\n"
   (apply str
          (map (fn [index]
                 (let [slot-addr (+ table-base (* index slot-size))]
                   (str
                    "    i32.const " (+ slot-addr occupied-offset) " i32.load\n"
                    "    if\n"
                    "      local.get $p1 i32.const " (+ slot-addr key-len-offset)
                    " i32.load i32.eq\n"
                    "      if\n"
                    "        i32.const " (+ slot-addr key-bytes-offset)
                    " local.get $p1 local.get $p0 local.get $p1 call $bytes-equal\n"
                    "        if i32.const " index " local.set $match end\n"
                    "      end\n"
                    "    else\n"
                    "      local.get $free i32.const -1 i32.eq\n"
                    "      if i32.const " index " local.set $free end\n"
                    "    end\n")))
               (range capacity)))))

(defn- state-record-fields
  "`(nth schema 2)` (the `[[name type] ...]` field list) for `payload-type`
  when it is a sealed flat scalar-or-string/keyword record
  (`string-field-record-schema`), else nil -- `state-provider-shape`'s own
  field-shape check, reused for every one of `state-v1`'s five record
  shapes (`get`/`put`/`delete` request cases, `entry`/`error` result
  records)."
  [payload-type schemas]
  (when-let [schema (string-field-record-schema payload-type schemas)]
    (nth schema 2)))

(defn- state-provider-shape
  "True when `request-descriptor`/`result-descriptor` (against `schemas`) are
  `state-v1`'s own literal shape EXACTLY -- not merely 'a 3-case request
  crossing a 5-case result' structurally admitted by
  `asymmetric-variant-capability-schema`, but the SPECIFIC case tags and
  field names/types/order `resources/kotoba/lang/capability-kits/
  state-v1.edn` itself declares: request cases `get`/`put`/`delete` (`get`
  and `delete` each `{key: keyword}`, `put` `{key: keyword, value:
  string}`), result cases `found`/`missing`/`written`/`deleted`/`error`
  (`found` and `written` sharing ONE `entry` schema, `{key: keyword, value:
  string, version: i64}`; `missing`/`deleted` bare `bool`; `error` `{code:
  keyword, message: string}`). `state-provider-wat` is deliberately this
  narrow -- a real provider for `state-v1`'s own shape specifically, not a
  generic 'real provider for any asymmetric variant crossing' -- so this
  check is intentionally strict, matching the task's own framing that this
  is `state-v1`'s first real provider, not a new general capability."
  [request-descriptor result-descriptor schemas]
  (let [request-schema (get schemas (second request-descriptor))
        result-schema (get schemas (second result-descriptor))]
    (boolean
     (when (and (vector? request-descriptor) (= :ref (first request-descriptor))
                (vector? request-schema) (= :variant (first request-schema))
                (= (second request-descriptor) (second request-schema))
                (vector? result-descriptor) (= :ref (first result-descriptor))
                (vector? result-schema) (= :variant (first result-schema))
                (= (second result-descriptor) (second result-schema)))
       (let [request-cases (nth request-schema 2)
             result-cases (nth result-schema 2)]
         (when (and (= 3 (count request-cases)) (= 5 (count result-cases))
                    (= [:get :put :delete] (mapv first request-cases))
                    (= [:found :missing :written :deleted :error] (mapv first result-cases)))
           (let [[[_ get-payload] [_ put-payload] [_ delete-payload]] request-cases
                 [[_ found-payload] [_ missing-payload]
                  [_ written-payload] [_ deleted-payload] [_ error-payload]] result-cases
                 found-schema (string-field-record-schema found-payload schemas)
                 written-schema (string-field-record-schema written-payload schemas)]
             (and (= (state-record-fields get-payload schemas) [[:key :keyword]])
                  (= (state-record-fields put-payload schemas)
                     [[:key :keyword] [:value :string]])
                  (= (state-record-fields delete-payload schemas) [[:key :keyword]])
                  found-schema (= found-schema written-schema)
                  (= (nth found-schema 2)
                     [[:key :keyword] [:value :string] [:version :i64]])
                  (= missing-payload :bool)
                  (= deleted-payload :bool)
                  (= (state-record-fields error-payload schemas)
                     [[:code :keyword] [:message :string]])))))))))

(defn- state-string-field-store
  "The two i32 stores writing one string/keyword field of `entry`/`error`'s
  own in-memory shape at `ret`-relative `offset` (pointer then length, the
  same layout every other emitter in this namespace already uses for a
  string/keyword leaf) from `pointer-expr`/`length-expr`."
  [offset pointer-expr length-expr]
  (str "      local.get $ret " pointer-expr " i32.store offset=" offset "\n"
       "      local.get $ret " length-expr " i32.store offset=" (+ offset 4) "\n"))

(defn- state-get-body
  "WAT for the ACTIVE `get` request case: `found(entry)` copied (by
  reference, not by byte-copy -- see docstring below) from the matched
  slot's own persistent storage when `$match` is not -1, `missing(false)`
  otherwise (matching `provider.state`'s own `(result
  :missing false)` -- the reference always writes a literal `false`
  payload, never derives it, so this provider matches that exactly rather
  than inventing a different convention). The `found` case's `key`/`value`
  pointer+length pair points DIRECTLY at the slot's own persistent buffer
  (no extra `memory.copy` into scratch first): this is safe because the
  slot lives BELOW `arena-base` (this module's own transient bump allocator
  never touches it, by construction -- see `state-provider-wat`), so the
  bytes stay valid at least through the remainder of this call, which is
  all the Canonical ABI's own cross-instance result-copy glue needs (it
  reads them immediately after this function returns, before any later
  call could ever reuse the slot). The `found` case's own `key` LENGTH
  reuses `$p1` (the REQUEST's own key length) rather than reloading it from
  the slot: `state-scan-wat`'s own match condition already proved
  `$p1` equals the slot's stored key length exactly (length-then-content
  equality), so reloading would be a redundant, not a more correct, read."
  [{:keys [disc-store payload-offset key-field value-field version-field
           table-base slot-size key-bytes-offset value-bytes-offset value-len-offset
           version-offset]}]
  (str
   "      local.get $match i32.const -1 i32.ne\n"
   "      if\n"
   "        local.get $match i32.const " slot-size " i32.mul i32.const " table-base
   " i32.add local.set $slot-addr\n"
   "        local.get $ret i32.const 0 " disc-store " offset=0\n"
   (state-string-field-store (+ payload-offset (:offset key-field))
                             (str "local.get $slot-addr i32.const " key-bytes-offset " i32.add")
                             "local.get $p1")
   (state-string-field-store (+ payload-offset (:offset value-field))
                             (str "local.get $slot-addr i32.const " value-bytes-offset " i32.add")
                             (str "local.get $slot-addr i32.load offset=" value-len-offset))
   "      local.get $ret local.get $slot-addr i64.load offset=" version-offset
   " i64.store offset=" (+ payload-offset (:offset version-field)) "\n"
   "      else\n"
   "        local.get $ret i32.const 1 " disc-store " offset=0\n"
   "        local.get $ret i32.const 0 i32.store8 offset=" payload-offset "\n"
   "      end\n"))

(defn- state-put-body
  "WAT for the ACTIVE `put` request case, matching
  `provider.state`'s own `:put` branch exactly: an EXISTING
  key ($match not -1) is updated in place; a NEW key ($match is -1) is
  written into the lowest free slot ($free) UNLESS the table is already full
  ($free is also -1), in which case `error({code: \"state/capacity\",
  message: \"state entry limit reached\"})` is written and the version
  counter is deliberately left untouched (matching the reference's own
  early-return-before-`swap!` capacity check). `version` is a GLOBAL,
  per-provider-instance monotonic i64 (`$version`, a real WASM mutable
  global -- persists across calls within one instance exactly like the
  reference's own per-instance `atom`, and is untouched by both `_post`
  and this module's own bump-allocator reset), pre-incremented on every
  SUCCESSFUL write regardless of key -- reproducing
  `provider.state`'s own `(swap! next-version inc)`
  convention verbatim, including its slightly surprising 'first write in a
  fresh instance is version 2, not 1' consequence (the counter starts at 1
  and is incremented BEFORE use, matching the reference's own `(atom (inc
  (count initial)))` with an empty `initial`)."
  [{:keys [disc-store payload-offset key-field value-field version-field
           table-base slot-size occupied-offset key-len-offset key-bytes-offset
           value-len-offset value-bytes-offset version-offset
           error-code-pointer error-code-length error-message-pointer error-message-length
           code-field message-field]}]
  (str
   "        local.get $match i32.const -1 i32.ne\n"
   "        if\n"
   "          local.get $match local.set $slot\n"
   "          i32.const 0 local.set $full\n"
   "        else\n"
   "          local.get $free i32.const -1 i32.eq\n"
   "          if\n"
   "            i32.const 1 local.set $full\n"
   "          else\n"
   "            local.get $free local.set $slot\n"
   "            i32.const 0 local.set $full\n"
   "          end\n"
   "        end\n"
   "        local.get $full\n"
   "        if\n"
   "          local.get $ret i32.const 4 " disc-store " offset=0\n"
   (state-string-field-store (+ payload-offset (:offset code-field))
                             (str "i32.const " error-code-pointer)
                             (str "i32.const " error-code-length))
   (state-string-field-store (+ payload-offset (:offset message-field))
                             (str "i32.const " error-message-pointer)
                             (str "i32.const " error-message-length))
   "        else\n"
   "          local.get $slot i32.const " slot-size " i32.mul i32.const " table-base
   " i32.add local.set $slot-addr\n"
   "          local.get $slot-addr i32.const 1 i32.store offset=" occupied-offset "\n"
   "          local.get $slot-addr i32.const " key-bytes-offset " i32.add"
   " local.get $p0 local.get $p1 memory.copy\n"
   "          local.get $slot-addr local.get $p1 i32.store offset=" key-len-offset "\n"
   "          local.get $slot-addr i32.const " value-bytes-offset " i32.add"
   " local.get $p2 local.get $p3 memory.copy\n"
   "          local.get $slot-addr local.get $p3 i32.store offset=" value-len-offset "\n"
   "          global.get $version i64.const 1 i64.add global.set $version\n"
   "          local.get $slot-addr global.get $version i64.store offset=" version-offset "\n"
   "          local.get $ret i32.const 2 " disc-store " offset=0\n"
   (state-string-field-store (+ payload-offset (:offset key-field))
                             (str "local.get $slot-addr i32.const " key-bytes-offset " i32.add")
                             "local.get $p1")
   (state-string-field-store (+ payload-offset (:offset value-field))
                             (str "local.get $slot-addr i32.const " value-bytes-offset " i32.add")
                             "local.get $p3")
   "          local.get $ret global.get $version i64.store offset="
   (+ payload-offset (:offset version-field)) "\n"
   "        end\n"))

(defn- state-delete-body
  "WAT for the ACTIVE `delete` request case, matching
  `provider.state`'s own `:delete` branch exactly:
  `deleted(true)` and the slot marked free when the key was present,
  `deleted(false)` (never `missing`, never `error`) when it was already
  absent -- the reference never rejects or special-cases deleting an absent
  key, it always succeeds with a `deleted` result reporting whether there
  was anything to delete, and this provider reproduces that precisely
  (verified against `provider.state`'s own `(result
  :deleted present?)`, not assumed)."
  [{:keys [disc-store payload-offset table-base slot-size occupied-offset]}]
  (str
   "      local.get $match i32.const -1 i32.ne\n"
   "      if\n"
   "        local.get $match i32.const " slot-size " i32.mul i32.const " table-base
   " i32.add local.set $slot-addr\n"
   "        local.get $slot-addr i32.const 0 i32.store offset=" occupied-offset "\n"
   "        local.get $ret i32.const 3 " disc-store " offset=0\n"
   "        local.get $ret i32.const 1 i32.store8 offset=" payload-offset "\n"
   "      else\n"
   "        local.get $ret i32.const 3 " disc-store " offset=0\n"
   "        local.get $ret i32.const 0 i32.store8 offset=" payload-offset "\n"
   "      end\n"))

(defn state-provider-wat
  "The first REAL (non-wiring-only) provider core module in this ADR chain:
  a genuine, small, bounded, in-memory key/value store for `state-v1`'s own
  literal request/result shape (`state-provider-shape`), with real dispatch
  on the request's own discriminant, real reads of the request's own `key`
  (and, for `put`, `value`) payload leaves, real persistent mutable state
  across calls within one component instance (the bounded table plus the
  `$version` global, both untouched by the transient bump-allocator reset
  every prior provider in this chain already used for scratch memory), and a
  real byte-CONTENT equality check over linear memory (`$bytes-equal`) to
  match a request's key against each stored slot's own key -- a kind of Wasm
  logic no prior ADR in this chain needed (see `bytes-equal-wat`'s own
  docstring). Every EARLIER provider in this chain (ADR 0047 through 0059)
  either echoed its input unchanged or, since ADR 0058, wrote a fixed
  compile-time-literal constant chosen from the request's own DISCRIMINANT
  alone, never its payload VALUES -- this provider is the first to actually
  READ and ACT ON a request's own payload leaf content.

  Reuses, UNCHANGED, exactly the machinery every prior provider in this
  namespace already proved correct: `asymmetric-request-validation-chain`
  (REQUEST-side byte-bound/pointer-range validation -- unlike ADR 0058/
  0059's own asymmetric provider, which validates a leaf it otherwise never
  reads, THIS provider's own dispatch genuinely needs the key/value bytes
  to be in-bounds before `$bytes-equal`/`memory.copy` ever touch them, so
  the same validation is necessary here for a different, more direct
  reason), `string-headroom-bytes` (REQUEST-side memory-page sizing for the
  Canonical ABI's own cross-instance string-copy-in glue), and
  `bounded-bump-realloc-wat` (the transient scratch allocator, used here
  ONLY for this call's own `$ret` struct and whatever the glue's copy-in
  needs -- never for the persistent table, which lives in a FIXED region
  strictly below `arena-base` this allocator's own `$next` starts at and is
  reset back to by both `_post` and `cm32p2_initialize`, exactly the
  existing convention, just with the persistent region now excluded from
  what gets reset).

  The one genuinely new piece of memory-sizing math: this module's total
  capacity must additionally cover the FIXED, permanent table region
  (`state-slot-layout`'s own `:table-size`, `capacity` slots) and a small
  FIXED literal region for the one compile-time-constant string content
  this provider ever writes without deriving it from a request value (the
  `error`/`:state/capacity` code+message pair, embedded via `(data ...)`
  exactly like ADR 0059's own `plan-result-string-data`/
  `deterministic-constant-string` technique, narrowly reused here for just
  this one case) -- both placed below `arena-base`, ADDED to (not replacing)
  the existing REQUEST-headroom-plus-result-size sum ADR 0059's own
  `asymmetric-variant-capability-provider-wat` already established.

  What this provider does and does not do, matching the task's own framing:
  since ADR 0061 it DOES implement the reference's full 256-entry capacity
  (`state-provider-table-capacity` -- see that def's own docstring for the
  ADR 0060 -> ADR 0061 history); it caps
  stored key/value bytes at the same FULL Kotoba bound the reference itself
  enforces (512/65536), not a narrower internal limit, so no request that
  passes Kotoba's own byte-bound validation can ever be rejected by this
  provider's OWN storage for a reason a caller could not already predict
  from `state-v1.edn` itself; and it is not native-AOT, not JIT, and not
  reviewed for production/security hardening -- it is a real semantic
  reference implementation reachable through a real `typed-cap-call`
  boundary for the first time, not a production-hardened deployment
  artifact (see this ADR's own 'Remaining gaps')."
  ([entry request-descriptor result-descriptor schemas]
   (state-provider-wat entry request-descriptor result-descriptor schemas
                        state-provider-table-capacity))
  ([entry request-descriptor result-descriptor schemas capacity]
   (when-not (state-provider-shape request-descriptor result-descriptor schemas)
     (reject "state provider requires state-v1's own literal request/result shape"
             {:request request-descriptor :result result-descriptor}))
   (let [export (str "cm32p2|kotoba:application/" (:interface entry) "@1|" (:function entry))
         request-layout (canonical/layout request-descriptor schemas)
         result-layout (canonical/layout result-descriptor schemas)
         joined-types (vec (rest (:flat request-layout)))
         params (apply str
                       (cons " (param $disc i32)"
                             (map-indexed
                              (fn [index core-type]
                                (str " (param $p" index " " (core-type-name core-type) ")"))
                              joined-types)))
         slot (state-slot-layout capacity)
         {:keys [table-base table-size slot-size occupied-offset]} slot
         literal-base (align-up (+ table-base table-size) 8)
         error-code-bytes (vec (.getBytes "state/capacity" "UTF-8"))
         error-message-bytes (vec (.getBytes "state entry limit reached" "UTF-8"))
         error-code-pointer literal-base
         error-message-pointer (+ error-code-pointer (count error-code-bytes))
         arena-base (align-up (+ error-message-pointer (count error-message-bytes)) 8)
         request-headroom-bytes (string-headroom-bytes request-layout)
         result-size (:size result-layout)
         required-bytes (+ arena-base request-headroom-bytes result-size)
         pages (max 1 (quot (+ required-bytes 65535) 65536))
         capacity-bytes (* pages 65536)
         result-cases (:cases result-layout)
         found-layout (:layout (nth result-cases 0))
         error-layout (:layout (nth result-cases 4))
         payload-offset (:payload-offset result-layout)
         disc-store (variant-disc-store (:discriminant-size result-layout))
         ctx (merge slot
                    {:disc-store disc-store
                     :payload-offset payload-offset
                     :key-field (field-by-name found-layout :key)
                     :value-field (field-by-name found-layout :value)
                     :version-field (field-by-name found-layout :version)
                     :code-field (field-by-name error-layout :code)
                     :message-field (field-by-name error-layout :message)
                     :error-code-pointer error-code-pointer
                     :error-code-length (count error-code-bytes)
                     :error-message-pointer error-message-pointer
                     :error-message-length (count error-message-bytes)})
         scan (state-scan-wat slot)
         needs-request-validation? (pos? request-headroom-bytes)
         validation (when needs-request-validation?
                      (asymmetric-request-validation-chain
                       (:cases request-layout) joined-types capacity-bytes))]
     (str
      "(module\n"
      "  (memory (export \"cm32p2_memory\") " pages " " pages ")\n"
      "  (global $next (mut i32) (i32.const " arena-base "))\n"
      "  (global $version (mut i64) (i64.const 1))\n"
      (bounded-bump-realloc-wat capacity-bytes)
      bytes-equal-wat
      "  (func (export \"" export "\")" params " (result i32)\n"
      "    (local $ret i32) (local $match i32) (local $free i32)"
      " (local $slot i32) (local $slot-addr i32) (local $full i32)"
      (when needs-request-validation? " (local $end i32)") "\n"
      "    local.get $disc i32.const 3 i32.ge_u if unreachable end\n"
      validation
      scan
      "    i32.const 0 i32.const 0 i32.const " (:alignment result-layout)
      " i32.const " result-size " call $realloc local.set $ret\n"
      "    local.get $disc i32.const 0 i32.eq\n"
      "    if\n"
      (state-get-body ctx)
      "    else\n"
      "      local.get $disc i32.const 1 i32.eq\n"
      "      if\n"
      (state-put-body ctx)
      "      else\n"
      (state-delete-body ctx)
      "      end\n"
      "    end\n"
      "    local.get $ret)\n"
      "  (func (export \"" export "_post\") (param i32)\n"
      "    i32.const " arena-base " global.set $next)\n"
      "  (func (export \"cm32p2_initialize\") i32.const " arena-base " global.set $next)\n"
      "  (data (i32.const " error-code-pointer ") \"" (wat-data error-code-bytes) "\")\n"
      "  (data (i32.const " error-message-pointer ") \"" (wat-data error-message-bytes) "\")\n"
      ")\n"))))

(defn fuel-enforcement
  "Where a component's declared `:fuel` budget is actually enforced.

  `:module-global` -- the budget is compiled into the core module's fuel global
  and the guest traps by itself at exhaustion. `:host-only` -- this shape is
  emitted as hand-written Canonical ABI WAT (realloc arena, no fuel global), so
  only the host can enforce the budget. The admission envelope reports this so
  a declared budget never reads as guest-enforced when it is not."
  [kir]
  (if (contains? #{:scalar :scalar-with-capabilities :structural-union-match
                   :structural-union-match-module}
                 (assert-supported! kir))
    :module-global
    :host-only))

(defn emit
  ([kir target] (emit kir target {}))
  ([kir target opts]
  (case (assert-supported! kir)
    :scalar-literal-capability-call
    (let [function (first (exported-functions kir))]
      (if (= :linear-resource (:capability-mode opts))
        (wasm-tools/parse-wat
         (linear-resource-literal-capability-wat
          function (scalar-literal-capability-call function)))
        (wasm/emit-component-core
         kir target (assoc opts :capability-imports (scalar-capability-imports kir)))))
    :scalar (wasm/emit-component-core kir target opts)
    :scalar-with-capabilities
    (wasm/emit-component-core
     kir target (assoc opts :capability-imports (scalar-capability-imports kir)))
    :structural-union-match-module
    (structural-union-match-module-core
     kir (structural-union-match-module kir) target opts)
    :string-expression (wasm-tools/parse-wat
                        (string-expression-wat (first (exported-functions kir))))
    :vector-i64-identity
    (wasm-tools/parse-wat
     (vector-i64-identity-wat (first (exported-functions kir))))
    :vector-i64-literal
    (wasm-tools/parse-wat
     (vector-i64-literal-wat (first (exported-functions kir))))
    :scalar-record-identity
    (wasm-tools/parse-wat
     (scalar-record-wat (first (exported-functions kir)) (:schemas kir)))
    :nested-record-identity
    (wasm-tools/parse-wat
     (nested-record-wat (first (exported-functions kir)) (:schemas kir)))
    :variant-identity
    (wasm-tools/parse-wat
     (variant-wat (first (exported-functions kir)) (:schemas kir)))
    :structural-union-identity
    (wasm-tools/parse-wat
     (variant-wat (first (exported-functions kir)) (:schemas kir)))
    :structural-union-construction
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (structural-union-construction-wat
        function (:schemas kir)
        (structural-union-construction function (:schemas kir)))))
    :structural-union-elimination
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (structural-union-elimination-wat
        function (:schemas kir)
        (structural-union-elimination function (:schemas kir)))))
    :structural-union-match
    (let [function (first (exported-functions kir))
          plan (structural-union-match function (:schemas kir))]
      (structural-union-match-core function (:schemas kir) plan target opts))
    :string-field-record-identity
    (wasm-tools/parse-wat
     (string-field-record-wat (first (exported-functions kir)) (:schemas kir)))
    :scalar-record-projection
    (wasm-tools/parse-wat
     (scalar-record-projection-wat (first (exported-functions kir)) (:schemas kir)))
    :scalar-record-construction
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (scalar-record-write-wat function (:schemas kir)
                                (scalar-record-construction function (:schemas kir)))))
    :scalar-record-update
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (scalar-record-write-wat function (:schemas kir)
                                (scalar-record-update function (:schemas kir)))))
    :scalar-capability-call
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       ((if (= :linear-resource (:capability-mode opts))
          linear-resource-scalar-capability-wat
          scalar-capability-wat)
        function (scalar-capability-call function))))
    :record-capability-call
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (record-capability-wat function (:schemas kir)
                              (record-capability-call function (:schemas kir)))))
    :variant-capability-call
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (variant-capability-wat function (:schemas kir)
                               (variant-capability-call function (:schemas kir)))))
    :structural-union-capability-call
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (variant-capability-wat
        function (:schemas kir)
        (structural-union-capability-call function (:schemas kir)))))
    :different-variant-capability-call
    (let [function (first (exported-functions kir))]
      (wasm-tools/parse-wat
       (variant-capability-wat function (:schemas kir)
                               (different-variant-capability-call function (:schemas kir))))))))
