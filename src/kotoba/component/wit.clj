(ns kotoba.component.wit
  "Deterministic WIT package/world generation from checked KIR."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [kotoba.abi.contract :as abi])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def contract
  (edn/read-string (slurp (io/resource "kotoba/lang/component-model-v1.edn"))))

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :component-wit))))

(defn- text-sha256 [text]
  (let [bytes (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes ^String text StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes))))

(defn- wit-name [value]
  (let [source (cond (keyword? value) (subs (str value) 1)
                     (symbol? value) (str value)
                     :else (str value))
        result (-> source str/lower-case
                   (str/replace #"[^a-z0-9-]+" "-")
                   (str/replace #"-+" "-")
                   (str/replace #"(^-|-$)" ""))]
    (when (or (empty? result) (not (re-matches #"[a-z][a-z0-9-]*" result)))
      (reject "value has no canonical WIT identifier" {:value value}))
    result))

(declare type-text)

(defn type-text
  "Return the canonical WIT spelling for an admitted Kotoba descriptor.
  Public so provider packaging derives its side of an interface from the
  same renderer as the application world."
  [descriptor]
  (cond
    (= descriptor :i64) "s64"
    (= descriptor :f32) "f32"
    (= descriptor :f64) "f64"
    (= descriptor :bool) "bool"
    (contains? #{:string :keyword} descriptor) "string"
    (= descriptor :vector-i64) "list<s64>"
    (= descriptor :vector-f64) "list<f64>"
    (and (vector? descriptor) (= :ref (first descriptor))) (wit-name (second descriptor))
    (and (vector? descriptor) (= :record (first descriptor))) (wit-name (second descriptor))
    (and (vector? descriptor) (= :option (first descriptor)))
    (str "option<" (type-text (second descriptor)) ">")
    (and (vector? descriptor) (= :result (first descriptor)))
    (str "result<" (type-text (second descriptor)) ", " (type-text (nth descriptor 2)) ">")
    (and (vector? descriptor) (= :list (first descriptor)))
    (str "list<" (type-text (second descriptor)) ">")
    (and (vector? descriptor) (= :vector (first descriptor)))
    (str "tuple<" (str/join ", " (map type-text (second descriptor))) ">")
    (and (vector? descriptor) (= :set (first descriptor)))
    (str "list<" (type-text (second descriptor)) ">")
    (and (vector? descriptor) (= :map (first descriptor)))
    (str "list<tuple<" (type-text (second descriptor)) ", "
         (type-text (nth descriptor 2)) ">>")
    :else (reject "descriptor is not representable as a WIT value type"
                  {:descriptor descriptor})))

(defn- schema-text [[schema-name descriptor]]
  (let [name (wit-name schema-name)]
    (case (first descriptor)
      :record
      (let [[_ nominal fields] descriptor]
        (when-not (= schema-name nominal)
          (reject "schema key and record identity differ" {:schema schema-name :identity nominal}))
        (str "  record " name " {\n"
             (apply str (map (fn [[field type]]
                               (str "    " (wit-name field) ": " (type-text type) ",\n")) fields))
             "  }\n"))
      :variant
      (let [[_ nominal cases] descriptor]
        (when-not (= schema-name nominal)
          (reject "schema key and variant identity differ" {:schema schema-name :identity nominal}))
        (str "  variant " name " {\n"
             (apply str (map (fn [[tag type]]
                               (str "    " (wit-name tag)
                                    "(" (type-text type) ")"
                                    ",\n")) cases))
             "  }\n"))
      (str "  type " name " = " (type-text descriptor) ";\n"))))

(defn- referenced-schema-names [descriptor]
  (cond
    (and (vector? descriptor) (= :ref (first descriptor))) #{(second descriptor)}
    (and (vector? descriptor) (= :record (first descriptor))) #{(second descriptor)}
    (coll? descriptor) (reduce into #{} (map referenced-schema-names descriptor))
    :else #{}))

(defn- type-uses [descriptors]
  (->> descriptors (mapcat referenced-schema-names) distinct (sort-by str) vec))

(defn- use-line [names]
  (when (seq names)
    (str "  use types.{" (str/join ", " (map wit-name names)) "};\n")))

(defn- capability-index []
  (into {} (map (juxt :id identity) (:capabilities contract))))

(defn- capability-contracts [kir]
  (->> (:functions kir)
       (mapcat #(tree-seq coll? seq (:body %)))
       (keep (fn [form]
               (when (and (seq? form) (= 'typed-cap-call (first form)))
                 (let [[_ id request-type result-type] form]
                   {:id id :request-type request-type :result-type result-type}))))
       distinct
       (sort-by (juxt :id (comp pr-str :request-type) (comp pr-str :result-type)))
       vec))

(defn- linear-resource-interface-text [[interface entries]]
  (let [descriptors (mapcat (juxt :request-type :result-type) entries)]
    (str "interface " interface " {\n"
         (use-line (type-uses descriptors))
         (apply str
                (map (fn [{:keys [function request-type result-type]}]
                       (let [resource (str function "-capability")]
                         (str "  resource " resource ";\n"
                              "  issue-" function ": func() -> own<" resource ">;\n"
                              "  execute-" function ": func(cap: own<" resource
                              ">, request: " (type-text request-type)
                              ") -> " (type-text result-type) ";\n")))
                     entries))
         "}\n\n")))

(defn- capability-interface-text [[interface entries]]
  (let [descriptors (mapcat (juxt :request-type :result-type) entries)]
    (str "interface " interface " {\n"
         (use-line (type-uses descriptors))
         (apply str
                (map (fn [{:keys [function request-type result-type]}]
                       (str "  " function ": func(request: " (type-text request-type)
                            ") -> " (type-text result-type) ";\n")) entries))
         "}\n\n")))

(defn- export-text [function]
  (str "  export " (wit-name (:name function)) ": func("
       (str/join ", " (map (fn [name type]
                              (str (wit-name name) ": " (type-text type)))
                            (:params function) (:param-types function)))
       ") -> " (type-text (:result function)) ";\n"))

(defn emit
  "Return a deterministic WIT v1 package and receipt for checked KIR."
  ([kir] (emit kir {}))
  ([kir {:keys [capability-mode typed-capability-v3?]
         :or {capability-mode :function}}]
  (when-not (contains? #{:kotoba.kir/v3 :kotoba.kir/v4} (:format kir))
    (reject "WIT generation requires checked KIR" {:format (:format kir)}))
  (if typed-capability-v3?
    (let [source (abi/typed-capability-wit-v3)]
      {:format :kotoba.wit-package/v1
       :target abi/component-target-v2
       :world abi/typed-capability-world-v3
       :wasi-version abi/wasi-version
       :source source
       :sha256 (text-sha256 source)
       :imports (mapv abi/capability-import-name
                      (sort (map :id (capability-contracts kir))))
       :capability-mode :linear-resource})
    (let [schemas (or (:schemas kir) {})
        schema-names (keys schemas)
        canonical-names (map wit-name schema-names)]
    (when-not (= (count canonical-names) (count (distinct canonical-names)))
      (reject "schema names collide after WIT canonicalization" {}))
    (let [by-id (capability-index)
          capabilities
          (mapv (fn [{:keys [id request-type result-type]}]
                  (let [entry (or (get by-id id)
                                  (reject "typed capability has no WIT contract" {:id id}))]
                    (assoc entry :request-type request-type :result-type result-type)))
                (capability-contracts kir))
          interfaces (->> capabilities (group-by :interface) (into (sorted-map)))
          exports (->> (:functions kir)
                       (filter (comp (set (:exports kir)) :name))
                       (sort-by (comp str :name)))
          canonical-export-names (mapv (comp wit-name :name) exports)
          _export-collision
          (when-not (= (count canonical-export-names)
                       (count (distinct canonical-export-names)))
            (reject "export names collide after WIT canonicalization"
                    {:exports (mapv :name exports)}))
          _parameter-collision
          (doseq [function exports
                  :let [parameter-names (mapv wit-name (:params function))]
                  :when (not= (count parameter-names)
                              (count (distinct parameter-names)))]
            (reject "parameter names collide after WIT canonicalization"
                    {:export (:name function) :parameters (:params function)}))
          export-types (mapcat (fn [f] (conj (vec (:param-types f)) (:result f))) exports)
          _ (doseq [descriptor (mapcat #(tree-seq coll? seq %) export-types)
                    :when (and (vector? descriptor) (= :record (first descriptor)))]
              (let [identity (second descriptor)]
                (when-not (= descriptor (get schemas identity))
                  (reject "inline record differs from sealed schema identity"
                          {:descriptor descriptor :schema (get schemas identity)}))))
          text (str "package kotoba:application@1.0.0;\n\n"
                    (when (seq schemas)
                      (str "interface types {\n"
                           (apply str (map schema-text (sort-by (comp str key) schemas)))
                           "}\n\n"))
                    (apply str (map (if (= capability-mode :linear-resource)
                                      linear-resource-interface-text
                                      capability-interface-text)
                                    interfaces))
                    "world application {\n"
                    (use-line (type-uses export-types))
                    (apply str (map #(str "  import " % ";\n") (keys interfaces)))
                    (apply str (map export-text exports))
                    "}\n")]
      {:format :kotoba.wit-package/v1
       :target :wasm-component-kotoba-v1
       :wasi-version "0.3.0"
       :source text
       :sha256 (text-sha256 text)
       :imports (mapv :name capabilities)
       :capability-mode capability-mode
       :exports (mapv :name exports)})))))
