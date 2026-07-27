(ns kotoba.component.composition
  "Closed-world composition support for compiler-qualified Component artifacts."
  (:require [clojure.string :as str]
            [kotoba.wasm.canonical-abi :as canonical]
            [kotoba.component.core :as component-core]
            [kotoba.component.wit :as component-wit]
            [kotoba.wasm.tools :as wasm-tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :component-composition))))

(def wac-version
  "ADR 0056 bumps this from 0.9.0 (ADR 0047-0055's pin) to 0.10.1: 0.9.0
  fails encoding any capability-crossing variant whose case wraps a record
  with `type not valid to be used as import` (ADR 0055's own reproduced
  finding); 0.10.0 fixes exactly that failure mode
  (bytecodealliance/wac#205, 'Alias `use`'d types during composition
  instead of re-encoding them locally' -- the type-aliasing fix a `use`'d
  type crossing an import/export boundary needs), independently confirmed
  here against ADR 0055's own reproduction before this pin moved; 0.10.1 is
  the latest patch release on top of it (bytecodealliance/wac#207, only a
  release-process fix, no further behavior change relevant here). Narrow,
  deliberate bump made only because it directly and verifiably closes a
  reproduced defect this codebase hit -- not a routine or blind toolchain
  refresh."
  "0.10.1")

(defn- assert-wac-version! []
  (let [actual (.trim ^String (wasm-tools/run-command! ["wac" "--version"]))]
    (when-not (= (str "wac-cli " wac-version) actual)
      (reject "wac version is not pinned"
              {:expected wac-version :actual actual}))))

(defn- scalar-wasm-type [descriptor]
  (or ({:i64 "i64" :f32 "f32" :f64 "f64"} descriptor)
      (reject "provider identity requires a Canonical scalar" {:descriptor descriptor})))

(defn- capability [name]
  (or (some #(when (= name (:name %)) %) (:capabilities component-wit/contract))
      (reject "provider capability is not present in the pinned contract" {:capability name})))

(defn- wit-name [value]
  (-> (if (keyword? value) (subs (str value) 1) (str value))
      str/lower-case
      (str/replace #"[^a-z0-9-]+" "-")
      (str/replace #"-+" "-")
      (str/replace #"(^-|-$)" "")))

(defn- provider-wit [entry descriptor]
  (let [type-name (or ({:i64 "s64" :f32 "f32" :f64 "f64"} descriptor)
                      (reject "provider identity requires a Canonical scalar"
                              {:descriptor descriptor}))
        interface (:interface entry)
        function (:function entry)]
    (str "package kotoba:application@1.0.0;\n\n"
         "interface " interface " {\n"
         "  " function ": func(request: " type-name ") -> " type-name ";\n"
         "}\n\n"
         "world " interface "-provider {\n"
         "  export " interface ";\n"
         "}\n")))

(defn- provider-wat [entry descriptor]
  (let [wasm-type (scalar-wasm-type descriptor)
        export (str "cm32p2|kotoba:application/" (:interface entry) "@1|" (:function entry))]
    (str "(module\n"
         "  (memory (export \"cm32p2_memory\") 1 1)\n"
         "  (func (export \"" export "\") (param $request " wasm-type ") (result " wasm-type ")\n"
         "    local.get $request)\n"
         "  (func (export \"" export "_post\") (param " wasm-type "))\n"
         "  (func (export \"cm32p2_realloc\") (param i32 i32 i32 i32) (result i32) i32.const 0)\n"
         "  (func (export \"cm32p2_initialize\")))\n")))

(defn package-scalar-identity-provider
  "Build a validation-only provider that preserves one scalar value. This
  proves interface wiring; it is not semantic evidence for a production kit."
  [capability-name descriptor]
  (let [entry (capability capability-name)
        wit (provider-wit entry descriptor)
        dir (Files/createTempDirectory "kotoba-provider-" (make-array FileAttribute 0))
        world (.resolve dir "provider.wit")
        core (.resolve dir "provider.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "provider.component.wasm")]
    (try
      (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (provider-wat entry descriptor))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component-provider/v1
       :capability capability-name
       :descriptor descriptor
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(defn- record-wit [entry descriptor schemas]
  (let [schema (get schemas (second descriptor))
        [_ identity fields] schema
        wit-type {:i64 "s64" :f32 "f32" :f64 "f64" :bool "bool"}
        record-name (wit-name identity)
        interface (:interface entry)]
    (when-not (and (= :ref (first descriptor))
                   (= :record (first schema))
                   (= identity (second descriptor))
                   (seq fields)
                   (every? (comp wit-type second) fields))
      (reject "provider record requires one sealed scalar schema"
              {:descriptor descriptor :schema schema}))
    (str "package kotoba:application@1.0.0;\n\n"
         "interface types {\n"
         "  record " record-name " {\n"
         (apply str (map (fn [[field type]]
                           (str "    " (wit-name field) ": " (wit-type type) ",\n")) fields))
         "  }\n}\n\n"
         "interface " interface " {\n"
         "  use types.{" record-name "};\n"
         "  " (:function entry) ": func(request: " record-name ") -> " record-name ";\n"
         "}\n\n"
         "world " interface "-provider {\n  export " interface ";\n}\n")))

(defn- record-provider-wat [entry descriptor schemas]
  (let [layout (canonical/layout descriptor schemas)
        wasm-type {:i64 "i64" :f32 "f32" :f64 "f64" :bool "i32"}
        wasm-store {:i64 "i64.store" :f32 "f32.store" :f64 "f64.store" :bool "i32.store8"}
        fields (:fields layout)
        export (str "cm32p2|kotoba:application/" (:interface entry) "@1|" (:function entry))
        params (apply str (map-indexed
                           (fn [index field]
                             (str " (param $f" index " "
                                  (wasm-type (get-in field [:layout :descriptor])) ")")) fields))
        stores (apply str (map-indexed
                           (fn [index field]
                             (let [type (get-in field [:layout :descriptor])]
                               (str "    local.get $ret local.get $f" index " "
                                    (wasm-store type) " offset=" (:offset field) "\n"))) fields))]
    (str "(module\n"
         "  (memory (export \"cm32p2_memory\") 1 1)\n"
         "  (func (export \"" export "\")" params " (result i32)\n"
         "    (local $ret i32) i32.const 8 local.set $ret\n" stores "    local.get $ret)\n"
         "  (func (export \"" export "_post\") (param i32))\n"
         "  (func (export \"cm32p2_realloc\") (param i32 i32 i32 i32) (result i32) i32.const 8)\n"
         "  (func (export \"cm32p2_initialize\")))\n")))

(defn package-record-identity-provider
  "Build a wiring-only provider for one sealed scalar record identity."
  [capability-name descriptor schemas]
  (let [entry (capability capability-name)
        wit (record-wit entry descriptor schemas)
        dir (Files/createTempDirectory "kotoba-record-provider-" (make-array FileAttribute 0))
        world (.resolve dir "provider.wit") core (.resolve dir "provider.wasm")
        embedded (.resolve dir "embedded.wasm") component (.resolve dir "provider.component.wasm")]
    (try
      (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat (record-provider-wat entry descriptor schemas))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component-provider/v1 :capability capability-name
       :descriptor descriptor :schemas schemas :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(def ^:private variant-case-wit-type
  {:i64 "s64" :f32 "f32" :f64 "f64" :bool "bool"})

(def ^:private record-field-wit-type
  "WIT spelling for one record *field* type admitted inside a variant
  provider's referenced record -- `variant-case-wit-type` widened by
  `:string`/`:keyword`, both spelled WIT `string` (ADR 0057, mirroring
  `kotoba.component.wit/type-text`'s own long-standing `:string`/
  `:keyword` -> `string` mapping and `kotoba.component.core/
  string-field-record-schema`'s field-type set). Kept distinct from
  `variant-case-wit-type` because a *bare* case payload (the case itself,
  not a field inside a record case) still admits only a Canonical scalar in
  this slice -- no case kind's own payload type is directly `:string`/
  `:keyword`, only a record field nested inside a record case may be."
  (assoc variant-case-wit-type :string "string" :keyword "string"))

(defn- variant-record-case-schema
  "Schema of `payload-type` when it is `[:ref name]` to a sealed record whose
  fields are each a bare Canonical scalar (the ADR 0052 shape) or a bounded
  `string`/`keyword` leaf (the ADR 0053 shape, admitted as a capability-call
  variant case's record payload for the first time in ADR 0057) -- the
  provider-side admission twin of
  `kotoba.component.core/string-field-record-schema` (private
  there; duplicated here narrowly rather than reaching across the namespace
  boundary, matching this file's own established precedent --
  `record-provider-wat` already duplicates `record-capability-wat`'s store
  shape locally for the same reason)."
  [payload-type schemas]
  (when (and (vector? payload-type) (= :ref (first payload-type)))
    (let [schema (get schemas (second payload-type))]
      (when (and (vector? schema) (= :record (first schema))
                 (= (second payload-type) (second schema))
                 (seq (nth schema 2))
                 (every? (comp record-field-wit-type second) (nth schema 2)))
        schema))))

(defn- variant-referenced-record-schemas
  "Every distinct record schema `cases` reference, in stable sorted order (so
  the generated WIT text is deterministic)."
  [cases schemas]
  (->> cases
       (keep (fn [[_ payload-type]] (variant-record-case-schema payload-type schemas)))
       distinct
       (sort-by (comp str second))))

(defn- variant-case-payload-wit
  "WIT type text for one variant case's payload: a bare scalar's WIT spelling,
  or a sealed all-scalar or string/keyword-bearing record case's own WIT
  type name."
  [payload-type schemas]
  (or (get variant-case-wit-type payload-type)
      (when-let [schema (variant-record-case-schema payload-type schemas)]
        (wit-name (second schema)))
      (reject "provider variant case is not scalar or a sealed admitted record"
              {:payload-type payload-type})))

(defn- variant-wit
  "Deterministic WIT package/world text for one sealed variant provider whose
  every case's payload is a bare Canonical scalar (ADR 0055), a sealed
  all-scalar record (ADR 0056, the ADR 0052 record shape), or -- new in ADR
  0057 -- a sealed flat string/keyword-bearing record (the ADR 0053 shape)
  -- the provider-side counterpart to `record-wit`. ADR 0055 deliberately
  did not admit a case wrapping a record (unlike the identity-export
  variant path, ADR 0052/0054): a record-referencing-variant provider built
  this same way was tried against `wac plug` (pinned 0.9.0 at the time) and
  failed encoding with `type not valid to be used as import` for every
  shape tried, independent of case count, case mix, and `types`-interface
  declaration order. ADR 0056 confirmed `wac` 0.10.0
  (bytecodealliance/wac#205) fixes exactly that failure mode and widened
  this function to declare the referenced record type(s) inside the same
  `interface types {...}` block as the variant, mirroring `record-wit`'s
  own record-declaration style, rather than the single-type-only footprint
  ADR 0055 scoped down to. ADR 0056 still left a case wrapping a sealed
  *string/keyword-bearing* record (ADR 0053's shape) unadmitted, recording
  it as a separate, still-unattempted gap: string/keyword data crossing a
  capability-call boundary at all, independent of the `wac plug` defect ADR
  0056 fixed. ADR 0057 closes exactly that gap for a record *case*'s own
  field (not a bare case payload -- see `record-field-wit-type`'s
  docstring)."
  [entry descriptor schemas]
  (let [schema (get schemas (second descriptor))
        [_ identity cases] schema
        variant-name (wit-name identity)
        interface (:interface entry)
        record-schemas (variant-referenced-record-schemas cases schemas)]
    (when-not (and (= :ref (first descriptor))
                   (= :variant (first schema))
                   (= identity (second descriptor))
                   (seq cases)
                   (every? (fn [[_ payload-type]]
                             (or (contains? variant-case-wit-type payload-type)
                                 (variant-record-case-schema payload-type schemas)))
                           cases))
      (reject "provider variant requires scalar, sealed all-scalar record, or sealed string/keyword-bearing record cases"
              {:descriptor descriptor :schema schema}))
    (str "package kotoba:application@1.0.0;\n\n"
         "interface types {\n"
         (apply str
                (map (fn [[_ record-identity fields]]
                       (str "  record " (wit-name record-identity) " {\n"
                            (apply str
                                   (map (fn [[field type]]
                                          (str "    " (wit-name field) ": "
                                               (get record-field-wit-type type) ",\n"))
                                        fields))
                            "  }\n"))
                     record-schemas))
         "  variant " variant-name " {\n"
         (apply str
                (map (fn [[tag payload-type]]
                       (str "    " (wit-name tag) "("
                            (variant-case-payload-wit payload-type schemas) "),\n"))
                     cases))
         "  }\n}\n\n"
         "interface " interface " {\n"
         "  use types.{" variant-name "};\n"
         "  " (:function entry) ": func(request: " variant-name ") -> " variant-name ";\n"
         "}\n\n"
         "world " interface "-provider {\n  export " interface ";\n}\n")))

(defn package-variant-identity-provider
  "Build a wiring-only provider for one sealed variant identity whose cases
  are each a bare scalar or a sealed all-scalar record (ADR 0055/0056), the
  variant-crossing counterpart to `package-record-identity-provider`. The
  provider core module itself is
  `kotoba.component.core/variant-capability-provider-wat`, which
  reuses that namespace's own `variant-case-chain` (disc range check plus
  in-branch bool validation and store) rather than duplicating it here."
  [capability-name descriptor schemas]
  (let [entry (capability capability-name)
        wit (variant-wit entry descriptor schemas)
        dir (Files/createTempDirectory "kotoba-variant-provider-" (make-array FileAttribute 0))
        world (.resolve dir "provider.wit") core (.resolve dir "provider.wasm")
        embedded (.resolve dir "embedded.wasm") component (.resolve dir "provider.component.wasm")]
    (try
      (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat
                         (component-core/variant-capability-provider-wat entry descriptor schemas))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component-provider/v1 :capability capability-name
       :descriptor descriptor :schemas schemas :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(defn- structural-union-provider-wit [entry descriptor schemas]
  (letfn [(fixed-list-item? [value seen]
            (cond
              (contains? #{:i64 :f32 :f64 :bool :string :keyword} value)
              true

              (and (vector? value) (= :option (first value))
                   (= 2 (count value)))
              (fixed-list-item? (second value) seen)

              (and (vector? value) (= :result (first value))
                   (= 3 (count value)))
              (and (fixed-list-item? (second value) seen)
                   (fixed-list-item? (nth value 2) seen))

              (and (vector? value) (= :list (first value))
                   (= 2 (count value)))
              (fixed-list-item? (second value) seen)

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
                     (every? (fn [[_ field-type]]
                               (fixed-list-item?
                                field-type (conj seen identity)))
                             (nth schema 2))))

              :else false))
          (referenced-records [value seen]
            (cond
              (and (vector? value)
                   (= :option (first value)))
              (referenced-records (second value) seen)

              (and (vector? value)
                   (= :result (first value)))
              (concat
               (referenced-records (second value) seen)
               (referenced-records (nth value 2) seen))

              (and (vector? value)
                   (= :list (first value)))
              (referenced-records (second value) seen)

              (and (vector? value)
                   (contains? #{:ref :record} (first value)))
              (let [identity (second value)
                    schema (if (= :ref (first value))
                             (get schemas identity)
                             value)]
                (if (or (contains? seen identity)
                        (not= :record (first schema)))
                  []
                  (concat
                   (mapcat (fn [[_ field-type]]
                             (referenced-records
                              field-type (conj seen identity)))
                           (nth schema 2))
                   [schema])))

              :else []))]
    (let [payloads (when (vector? descriptor)
                   (case (first descriptor)
                     :option [(second descriptor)]
                     :result [(second descriptor) (get descriptor 2)]
                     nil))
        supported?
        (fn supported? [payload]
          (cond
            (contains? #{:i64 :f32 :f64 :bool :string :keyword
                         :vector-i64 :vector-f64} payload)
            true

            (and (vector? payload) (= :option (first payload))
                 (= 2 (count payload)))
            (supported? (second payload))

            (and (vector? payload) (= :result (first payload))
                 (= 3 (count payload)))
            (and (supported? (second payload))
                 (supported? (nth payload 2)))

            (and (vector? payload) (= :list (first payload))
                 (= 2 (count payload)))
            (fixed-list-item? (second payload) #{})

            (and (vector? payload)
                 (contains? #{:ref :record} (first payload)))
            (let [schema (if (= :ref (first payload))
                           (get schemas (second payload))
                           payload)]
              (and (vector? schema)
                   (= :record (first schema))
                   (every? (fn [[_ field-type]]
                             (contains? #{:i64 :f32 :f64 :bool}
                                        field-type))
                           (nth schema 2))))

            :else false))
        interface (:interface entry)
        record-schemas
        (->> payloads
             (mapcat #(referenced-records % #{}))
             distinct)
        declarations
        (apply str
               (map
                (fn [[_ identity fields]]
                  (str "  record " (wit-name identity) " {\n"
                       (apply str
                              (map (fn [[field-name field-type]]
                                     (str "    " (wit-name field-name) ": "
                                          (component-wit/type-text field-type)
                                          ",\n"))
                                   fields))
                       "  }\n"))
                record-schemas))]
    (when-not (and (seq payloads)
                   (every? supported? payloads))
      (reject "structural union provider requires bounded structural payloads"
              {:descriptor descriptor}))
    (let [type-name (component-wit/type-text descriptor)]
      (str "package kotoba:application@1.0.0;\n\n"
           "interface " interface " {\n"
           declarations
           "  " (:function entry) ": func(request: " type-name
           ") -> " type-name ";\n"
           "}\n\n"
           "world " interface "-provider {\n"
           "  export " interface ";\n"
           "}\n")))))

(defn package-structural-union-identity-provider
  "Build a named WIT provider that echoes one bounded option/result value."
  ([capability-name descriptor]
   (package-structural-union-identity-provider
    capability-name descriptor {}))
  ([capability-name descriptor schemas]
  (let [entry (capability capability-name)
        _ (canonical/layout descriptor schemas)
        wit (structural-union-provider-wit entry descriptor schemas)
        dir (Files/createTempDirectory
             "kotoba-structural-union-provider-" (make-array FileAttribute 0))
        world (.resolve dir "provider.wit")
        core (.resolve dir "provider.wasm")
        embedded (.resolve dir "embedded.wasm")
        component (.resolve dir "provider.component.wasm")]
    (try
      (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
      (Files/write core
                   (wasm-tools/parse-wat
                    (component-core/variant-capability-provider-wat
                     entry descriptor schemas))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component-provider/v1
       :capability capability-name
       :descriptor descriptor
       :schemas schemas
       :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir))))))

(defn- asymmetric-variant-record-case-schema
  "Schema of `payload-type` when it is `[:ref name]` to a sealed all-scalar
  record (the ADR 0052 shape) or a bounded `string`/`keyword` leaf (the ADR
  0053 shape, admitted for the different-identity crossing for the first
  time in ADR 0059) -- the provider-side admission twin of
  `kotoba.component.core/asymmetric-variant-capability-case?`.
  Through ADR 0058 this was deliberately narrower than `variant-record-
  case-schema` (which already admitted a string/keyword-bearing record
  field for the SAME-identity path since ADR 0057): the different-identity
  crossing did not yet admit a string/keyword leaf on either side. ADR 0059
  closes exactly that gap for this (provider-side) admission twin, mirroring
  `component-core/asymmetric-variant-capability-case?`'s own widening in
  the same ADR -- now identical in shape to `variant-record-case-schema`
  (reusing `record-field-wit-type`, not `variant-case-wit-type`, for its own
  field check), kept as a SEPARATE function rather than merged into it for
  the same reason every twin pair in this namespace stays separate: so the
  same-identity path's own admitted set never silently narrows if this one
  changes, and vice versa."
  [payload-type schemas]
  (when (and (vector? payload-type) (= :ref (first payload-type)))
    (let [schema (get schemas (second payload-type))]
      (when (and (vector? schema) (= :record (first schema))
                 (= (second payload-type) (second schema))
                 (seq (nth schema 2))
                 (every? (comp record-field-wit-type second) (nth schema 2)))
        schema))))

(defn- asymmetric-variant-case-payload-wit
  "WIT type text for one asymmetric-crossing variant case's payload: a bare
  scalar's WIT spelling, or a sealed all-scalar OR string/keyword-bearing
  record case's own WIT type name (ADR 0059 widens this from scalar-only,
  matching `variant-case-payload-wit`'s own admitted set) -- the
  provider-side twin of `component-core/asymmetric-variant-capability-
  case?`."
  [payload-type schemas]
  (or (get variant-case-wit-type payload-type)
      (when-let [schema (asymmetric-variant-record-case-schema payload-type schemas)]
        (wit-name (second schema)))
      (reject "provider variant case is not scalar or a sealed all-scalar or string/keyword-bearing record"
              {:payload-type payload-type})))

(defn- asymmetric-variant-schema-valid?
  "True when `descriptor` is `[:ref name]` to a sealed variant every one of
  whose cases independently satisfies `asymmetric-variant-case-payload-wit`'s
  own admitted shape -- the provider-side twin of
  `component-core/asymmetric-variant-capability-schema`."
  [descriptor schemas]
  (let [schema (get schemas (second descriptor))]
    (and (vector? descriptor) (= :ref (first descriptor))
         (vector? schema) (= :variant (first schema))
         (= (second descriptor) (second schema))
         (seq (nth schema 2))
         (every? (fn [[_ payload-type]]
                   (or (contains? variant-case-wit-type payload-type)
                       (asymmetric-variant-record-case-schema payload-type schemas)))
                 (nth schema 2)))))

(defn- asymmetric-variant-wit
  "Deterministic WIT package/world text for a provider whose capability
  crosses two DIFFERENT sealed variant identities (ADR 0058) -- the
  different-identity counterpart to `variant-wit`'s own same-identity
  `func(request: T) -> T`. Declares BOTH variant types (plus every record
  either one's own cases reference, deduplicated via
  `variant-referenced-record-schemas`, unmodified) inside the shared
  `types` interface, and the capability function as
  `func(request: RequestName) -> ResultName` -- `component-wit.clj`'s own
  generic `typed-cap-call` body walk already renders exactly this shape for
  the *application* side (confirmed by inspection, ADR 0055/0056/0057's own
  'no changes needed' finding extends here too); this is only the
  provider-side counterpart. ADR 0059 fixes a latent bug this function's own
  record-field WIT-rendering loop had (dormant until this ADR, since no
  fixture before it ever reached a string/keyword-bearing record on this
  path): it rendered every field type via `variant-case-wit-type`, the
  scalar-only map with no `:string`/`:keyword` entry, which would silently
  emit malformed WIT text (`key: ,`) for a string/keyword field rather than
  a clear error -- `variant-referenced-record-schemas` (this function's own
  record-collection call, unchanged since ADR 0058) already reused the
  SAME-identity path's `variant-record-case-schema` (which already admits a
  string/keyword-bearing record for the SAME-identity path since ADR 0057),
  so the collected `record-schemas` could already contain a string/keyword-
  bearing record even before ADR 0059's own admission widening, but nothing
  before this ADR ever fed one through this specific code path exercising
  it. Now uses `record-field-wit-type` (the same map `record-wit`/
  `variant-wit` already use), matching every other record-field WIT
  emission site in this namespace."
  [entry request-descriptor result-descriptor schemas]
  (when-not (and (asymmetric-variant-schema-valid? request-descriptor schemas)
                 (asymmetric-variant-schema-valid? result-descriptor schemas)
                 (not= (second request-descriptor) (second result-descriptor)))
    (reject "provider variant crossing requires two distinct admitted scalar-or-record variant identities"
            {:request request-descriptor :result result-descriptor}))
  (let [request-schema (get schemas (second request-descriptor))
        result-schema (get schemas (second result-descriptor))
        [_ request-identity request-cases] request-schema
        [_ result-identity result-cases] result-schema
        request-name (wit-name request-identity)
        result-name (wit-name result-identity)
        interface (:interface entry)
        record-schemas (variant-referenced-record-schemas
                         (concat request-cases result-cases) schemas)]
    (str "package kotoba:application@1.0.0;\n\n"
         "interface types {\n"
         (apply str
                (map (fn [[_ record-identity fields]]
                       (str "  record " (wit-name record-identity) " {\n"
                            (apply str
                                   (map (fn [[field type]]
                                          (str "    " (wit-name field) ": "
                                               (get record-field-wit-type type) ",\n"))
                                        fields))
                            "  }\n"))
                     record-schemas))
         "  variant " request-name " {\n"
         (apply str
                (map (fn [[tag payload-type]]
                       (str "    " (wit-name tag) "("
                            (asymmetric-variant-case-payload-wit payload-type schemas) "),\n"))
                     request-cases))
         "  }\n"
         "  variant " result-name " {\n"
         (apply str
                (map (fn [[tag payload-type]]
                       (str "    " (wit-name tag) "("
                            (asymmetric-variant-case-payload-wit payload-type schemas) "),\n"))
                     result-cases))
         "  }\n}\n\n"
         "interface " interface " {\n"
         "  use types.{" request-name ", " result-name "};\n"
         "  " (:function entry) ": func(request: " request-name ") -> " result-name ";\n"
         "}\n\n"
         "world " interface "-provider {\n  export " interface ";\n}\n")))

(defn package-variant-asymmetric-provider
  "Build a wiring-only provider for a `typed-cap-call` whose request and
  result are two DIFFERENT sealed variant identities (ADR 0058), each
  independently a scalar-or-sealed-all-scalar-record-cased variant -- the
  different-identity counterpart to `package-variant-identity-provider`.
  The provider core module itself is
  `kotoba.component.core/asymmetric-variant-capability-provider-wat`,
  which inspects only the request's own discriminant and writes a fixed,
  case-appropriate constant result -- it does not, and structurally cannot,
  echo the request the way every prior identity provider in this chain
  does, because request and result are unrelated shapes here."
  [capability-name request-descriptor result-descriptor schemas]
  (let [entry (capability capability-name)
        wit (asymmetric-variant-wit entry request-descriptor result-descriptor schemas)
        dir (Files/createTempDirectory "kotoba-variant-asym-provider-" (make-array FileAttribute 0))
        world (.resolve dir "provider.wit") core (.resolve dir "provider.wasm")
        embedded (.resolve dir "embedded.wasm") component (.resolve dir "provider.component.wasm")]
    (try
      (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
      (Files/write core (wasm-tools/parse-wat
                         (component-core/asymmetric-variant-capability-provider-wat
                          entry request-descriptor result-descriptor schemas))
                   (make-array java.nio.file.OpenOption 0))
      (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                "--encoding" "utf8" "-o" (str embedded)])
      (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                "--reject-legacy-names" "-o" (str component)])
      {:format :wasm-component-provider/v1 :capability capability-name
       :descriptor request-descriptor :result-descriptor result-descriptor
       :schemas schemas :bytes (Files/readAllBytes component)}
      (finally
        (doseq [path [component embedded core world]] (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))

(defn package-state-provider
  "Build the first REAL (non-wiring-only) provider artifact in this ADR
  chain, backed by `kotoba.component.core/state-provider-wat` -- the
  real-semantics counterpart to `package-variant-asymmetric-provider`, for
  `state-v1`'s own literal request/result shape specifically (checked by
  `state-provider-wat` itself, via `component-core/state-provider-shape`,
  before any WAT is generated at all). Reuses `asymmetric-variant-wit`
  UNCHANGED for the WIT text: the WIT SHAPE this provider exports is
  IDENTICAL to ADR 0058/0059's own wiring-only asymmetric provider's own
  (same package/world/interface, same `func(request: T) -> U` signature) --
  only the WASM BODY differs, real bounded-table dispatch/storage/byte-
  comparison logic in place of a fixed compile-time constant. `capacity`
  (default `component-core/state-provider-table-capacity`, `256` as of ADR
  0061, matching the pure-Clojure reference's own bound; ADR 0060 shipped
  this default as `4`) is exposed so a test/evidence fixture can build a
  SMALLER table (e.g. to reach the capacity-exhaustion fail-closed path in
  fewer `put`s, or to re-run ADR 0060's own 14-step stateful-sequence
  fixture unchanged as a no-regression check) without touching the
  production default."
  ([capability-name request-descriptor result-descriptor schemas]
   (package-state-provider capability-name request-descriptor result-descriptor
                            schemas component-core/state-provider-table-capacity))
  ([capability-name request-descriptor result-descriptor schemas capacity]
   (let [entry (capability capability-name)
         wit (asymmetric-variant-wit entry request-descriptor result-descriptor schemas)
         dir (Files/createTempDirectory "kotoba-state-provider-" (make-array FileAttribute 0))
         world (.resolve dir "provider.wit") core (.resolve dir "provider.wasm")
         embedded (.resolve dir "embedded.wasm") component (.resolve dir "provider.component.wasm")]
     (try
       (Files/writeString world wit (make-array java.nio.file.OpenOption 0))
       (Files/write core (wasm-tools/parse-wat
                          (component-core/state-provider-wat
                           entry request-descriptor result-descriptor schemas capacity))
                    (make-array java.nio.file.OpenOption 0))
       (wasm-tools/run-command! ["wasm-tools" "component" "embed" (str world) (str core)
                                 "--encoding" "utf8" "-o" (str embedded)])
       (wasm-tools/run-command! ["wasm-tools" "component" "new" (str embedded)
                                 "--reject-legacy-names" "-o" (str component)])
       {:format :wasm-component-provider/v1 :capability capability-name
        :descriptor request-descriptor :result-descriptor result-descriptor
        :schemas schemas :bytes (Files/readAllBytes component)}
       (finally
         (doseq [path [component embedded core world]] (Files/deleteIfExists path))
         (Files/deleteIfExists dir))))))

(defn compose-closed
  "Compose one application with provider definitions and reject any remaining
  instance import. `wasm-tools compose --no-imports` is the closure gate."
  [application providers]
  (when-not (= :wasm-component/v1 (:format application))
    (reject "composition requires a compiler component artifact"
            {:format (:format application)}))
  (let [required (frequencies (:imports application))
        supplied (frequencies (map :capability providers))]
    (when-not (= required supplied)
      (reject "provider definitions do not exactly close application imports"
              {:required required :supplied supplied})))
  (assert-wac-version!)
  (let [dir (Files/createTempDirectory "kotoba-compose-" (make-array FileAttribute 0))
        app (.resolve dir "application.wasm")
        output (.resolve dir "closed.wasm")
        definitions (mapv #(.resolve dir (str "provider-" % ".wasm"))
                          (range (count providers)))]
    (try
      (Files/write app ^bytes (:bytes application) (make-array java.nio.file.OpenOption 0))
      (doseq [[path provider] (map vector definitions providers)]
        (when-not (= :wasm-component-provider/v1 (:format provider))
          (reject "definition is not a compiler provider artifact" {:format (:format provider)}))
        (Files/write path ^bytes (:bytes provider) (make-array java.nio.file.OpenOption 0)))
      (wasm-tools/run-command!
       (into ["wac" "plug" (str app) "-o" (str output)]
             (mapcat #(vector "--plug" (str %)) definitions)))
      (wasm-tools/run-command! ["wasm-tools" "validate" (str output)])
      {:format :wasm-component-closed/v1
       :bytes (Files/readAllBytes output)
       :application-imports (:imports application)
       :providers (mapv #(select-keys % [:capability :descriptor]) providers)}
      (finally
        (doseq [path (concat [output app] definitions)] (Files/deleteIfExists path))
        (Files/deleteIfExists dir)))))
