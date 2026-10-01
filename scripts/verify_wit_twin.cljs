;; Differential: the Kotoba reading of kotoba.component.wit (emit / emit-with over
;; Form) on the KIR interpreter, linked as a project, against the host `emit`
;; (nbb). Both read the same checked KIR and the same embedded contract.
;;
;; Cases: the five frozen vectors of lang/wit-vectors.edn, then KIR of the shapes
;; the type mapping and the world assembly distinguish -- records and variants by
;; reference, every compound type, nested schemas, linear-resource mode, name
;; canonicalization, and the refusals (a recursive schema, colliding exports,
;; colliding parameters, an inline record that differs from its schema, a
;; non-checked KIR). For each, the Kotoba :source, :sha256, :imports and :exports
;; are compared with the host's; a refusal must be a refusal on both. A FAIL line
;; is a difference; the exit code is not set.
;;
;;   KROOTS=<source roots incl. kotoba-lang/lang/compat, colon separated> \
;;   node --stack-size=8192 nbb/cli.js --classpath <amu classpath> \
;;     scripts/verify_wit_twin.cljs
(ns verify-wit-twin
  (:require ["node:fs" :as fs]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [kotoba.sema :as sema]
            [kotoba.kir :as kir]
            [kotoba.compiler.project :as project]
            [kotoba.compiler.project-files :as pf]
            [kotoba.component.wit :as wit]))

(def env (fn [k d] (or (aget js/process.env k) d)))
(def roots (vec (str/split (env "KROOTS" "") #":")))
(def only (re-pattern (env "ONLY" ".")))
(def probe-root "/private/tmp/scratch/wit_probe.cljk")

(def form-schema
  "(:schemas {:form/r [:record :form/r [[:tag :i64] [:s :string] [:n :i64] [:k :keyword] [:kids [:list [:ref :form/r]]] [:span :i64] [:data :bytes]]]})")

(def probe-src
  (str "(ns probe.wit\n  {:kotoba/export [main emit-batch]}\n"
       "  (:require [kotoba.component.wit :as w] [kotoba.form :as form])\n  " form-schema ")\n"
       "(defn- join-leaves [xs [:ref :form/r] i :i64 acc :string] :string
          (if (>= i (form/count-of xs)) acc
            (let [x (form/nth-of xs i)
                  t (cond (form/is-string? x) (form/string-value x)
                      (form/is-symbol? x) (form/symbol-value x)
                      (form/is-keyword? x) (document-edn-print (document-keyword (form/keyword-value x)))
                      :else \"?\")]
              (join-leaves xs (+ i 1) (str acc (if (= i 0) \"\" \",\") t)))))\n"
       "(defn- get-k [m [:ref :form/r] k :keyword] [:ref :form/r] (form/form-get m (form/keyword-form k)))\n"
       "(defn- result-text [v [:ref :form/r]] :string
          (let [src (form/string-value (get-k v :source))
                sha (form/string-value (get-k v :sha256))
                imports (join-leaves (get-k v :imports) 0 \"\")
                exports (join-leaves (get-k v :exports) 0 \"\")]
            (str src \"\\u0001\" sha \"\\u0001\" imports \"\\u0001\" exports)))\n"
       "(defn- one-case [pair [:ref :form/r] ctr [:ref :form/r]] :string
          (let [r (w/emit-with-contract (form/nth-of pair 0) (form/nth-of pair 1) ctr)]
            (if (result-ok?-of [:result [:ref :form/r] :document] r)
              (result-text (result-value-of [:result [:ref :form/r] :document] r (form/nil-form)))
              \"ERR\")))\n"
       "(defn- batch-from [cases [:ref :form/r] ctr [:ref :form/r] i :i64 acc :string] :string
          (if (>= i (form/count-of cases)) acc
            (batch-from cases ctr (+ i 1) (str acc (if (= i 0) \"\" \"\\u0002\") (one-case (form/nth-of cases i) ctr)))))\n"
       ;; CASES: a vector of [kir opts] pairs; CTR the contract's EDN text (read once)
       "(defn emit-batch [cases :string ctr :string] :string
          (batch-from (form/edn-form cases) (form/edn-form ctr) 0 \"\"))\n"
       "(defn main [] :string \"ok\")\n"))

(.mkdirSync fs "/private/tmp/scratch" #js {:recursive true})
(.writeFileSync fs probe-root probe-src)
(def kir-mod
  (let [graph (pf/load-closed-graph probe-root roots)
        linked (project/link-source (:sources graph) (:root graph))
        hir (sema/analyze (:source linked) {:admit-linked-synthetics? true})]
    (kir/lower hir)))
(println "probe compiled")

(defn pr-flat [x] (binding [*print-namespace-maps* false] (pr-str x)))

(def contract-text
  (let [f (fn [rel] (try (.readFileSync fs rel "utf8") (catch :default _ nil)))]
    (or (f (env "CONTRACT" "resources/kotoba/lang/component-model-v1.edn")) "")))

(defn host-all [kir-data opts]
  (try (let [r (wit/emit kir-data opts)]
         (str (:source r) "\u0001" (:sha256 r) "\u0001" (str/join "," (map str (:imports r))) "\u0001"
              (str/join "," (map str (:exports r)))))
      (catch :default e "ERR")))

(def queue (atom []))
(defn compare-case [id kir-data opts]
  (when (re-find only (name id))
    (swap! queue conj {:id id :kir kir-data :opts opts})))

(def fails (atom 0))
(def total (atom 0))

(declare run-chunk!)
(defn run-queue! []
  (doseq [chunk (partition-all (js/parseInt (env "CHUNK" "12")) @queue)]
    (run-chunk! (vec chunk))))

(defn run-chunk! [cases]
  (println "chunk" (pr-str (mapv :id cases)))
  (let [
        text (pr-flat (mapv (fn [{:keys [kir opts]}] [kir opts]) cases))
        out (try (kir/execute kir-mod 'emit-batch [text contract-text]
                              {:fuel 30000000000 :frames 200000 :cells 2000000000 :bytes 2000000000
                               :typed-cap-call (fn [cap-id _ _ request]
                                                 (if (= 3 (js/Number cap-id))
                                                   (-> (.createHash (js/require "node:crypto") "sha256") (.update request "utf8") (.digest "hex"))
                                                   (throw (ex-info "no capability" {:cap cap-id}))))})
                 (catch :default e (str "TRAP:" (ex-message e))))
        results (if (str/starts-with? out "TRAP:") (vec (repeat (count cases) out)) (vec (str/split out "\u0002")))]
    (doseq [[{:keys [id kir opts]} k] (map vector cases results)]
      (let [h (host-all kir opts)
            k (if (str/starts-with? k "TRAP:") "ERR" k)
            [hs hh hi he] (str/split h "\u0001")
            [ks kh ki ke] (str/split k "\u0001")]
        (doseq [[field a b] [["source" hs ks] ["sha256" hh kh] ["imports" hi ki] ["exports" he ke]]]
          (swap! total inc)
          (when-not (= a b) (swap! fails inc))
          (println (if (= a b) "ok  " "FAIL") id field
                   (when-not (= a b) (pr-flat {:host (subs (str a) 0 (min 300 (count (str a)))) :kotoba (subs (str b) 0 (min 300 (count (str b))))}))))))))

;; ---- the frozen vectors ---------------------------------------------------------------------------
(let [table (edn/read-string (.readFileSync fs (env "VECTORS" "lang/wit-vectors.edn") "utf8"))]
  (doseq [{:keys [id kir]} (:vectors table)]
    (compare-case (keyword (str "vector-" (name id))) kir {})))

;; ---- more shapes ---------------------------------------------------------------------------------------
(defn f [name params types result body] {:name name :params params :param-types types :result result :body body})
(defn kir-of [schemas exports functions]
  {:format :kotoba.kir/v3 :schemas schemas :exports exports :functions functions})

(def pt [:record :t/pt [[:x :i64] [:y :string]]])
(def shape [:variant :t/shape [[:circle :f64] [:square [:ref :t/pt]] [:none :bool]]])
(def pair [:record :t/pair [[:a [:ref :t/pt]] [:b [:option :string]] [:c [:list :i64]]]])

(def cases
  {:record-by-ref (kir-of {:t/pt pt} '[main] [(f 'main '[p] [[:ref :t/pt]] :i64 '[1])])
   :record-inline (kir-of {:t/pt pt} '[main] [(f 'main '[p] [pt] :i64 '[1])])
   :record-inline-differs (kir-of {:t/pt pt} '[main] [(f 'main '[p] [[:record :t/pt [[:x :i64]]]] :i64 '[1])])
   :v-scalar (kir-of {:t/s [:variant :t/s [[:a :i64] [:b :bool]]]} '[main] [(f 'main '[s] [[:ref :t/s]] :i64 '[1])])
   :v-ref (kir-of {:t/s [:variant :t/s [[:a [:ref :t/pt]]]] :t/pt pt} '[main] [(f 'main '[s] [[:ref :t/s]] :i64 '[1])])
   :variant (kir-of {:t/shape shape :t/pt pt} '[main] [(f 'main '[s] [[:ref :t/shape]] :bool '[true])])
   :nested (kir-of {:t/pair pair :t/pt pt} '[main] [(f 'main '[p] [[:ref :t/pair]] [:ref :t/pair] '[1])])
   :compound-types (kir-of {} '[main]
                          [(f 'main '[a b c d e g h i]
                              [[:option :i64] [:result :string :i64] [:list :string] [:vector [:i64 :string :bool]]
                               [:set :keyword] [:map :string :i64] :vector-i64 :vector-f64]
                              [:option [:list :i64]] '[1])])
   :unused-schema-omitted (kir-of {:t/pt pt :t/other [:record :t/other [[:z :bool]]]} '[main]
                                  [(f 'main '[p] [[:ref :t/pt]] :i64 '[1])])
   ;; a schema whose descriptor is a bare keyword (`:t/n :i64`) is an EXPECTED difference: the
   ;; host's `(case (first descriptor) ...)` throws "Don't know how to create ISeq from:
   ;; Keyword" there, and the Kotoba reading emits `type t-n = s64;`. Only the vector alias is compared.
   :alias-schema (kir-of {:t/s [:list :string]} '[main] [(f 'main '[b] [[:ref :t/s]] :i64 '[1])])
   :two-exports-sorted (kir-of {} '[zeta alpha]
                              [(f 'zeta '[] [] :i64 '[1]) (f 'alpha '[x] [:i64] :i64 '[2]) (f 'hidden '[] [] :i64 '[3])])
   :name-canonicalization (kir-of {} '[Foo_Bar] [(f 'Foo_Bar '[A__b -x-] [:i64 :i64] :i64 '[1])])
   :name-digit-first (kir-of {} '[main] [(f 'main '[_1x] [:i64] :i64 '[1])])
   :exports-collide (kir-of {} '[a_b a-b] [(f 'a_b '[] [] :i64 '[1]) (f 'a-b '[] [] :i64 '[2])])
   :params-collide (kir-of {} '[main] [(f 'main '[a_b a-b] [:i64 :i64] :i64 '[1])])
   :schema-names-collide (kir-of {:t/a_b [:record :t/a_b [[:x :i64]]] :t/a-b [:record :t/a-b [[:x :i64]]]} '[main]
                                 [(f 'main '[] [] :i64 '[1])])
   :recursive-schema (kir-of {:t/n [:record :t/n [[:v :i64] [:next [:ref :t/n]]]]} '[main]
                             [(f 'main '[n] [[:ref :t/n]] :i64 '[1])])
   :mutually-recursive (kir-of {:t/a [:record :t/a [[:b [:ref :t/b]]]] :t/b [:record :t/b [[:a [:ref :t/a]]]]} '[main]
                               [(f 'main '[n] [[:ref :t/a]] :i64 '[1])])
   :record-key-differs (kir-of {:t/other pt} '[main] [(f 'main '[p] [[:ref :t/other]] :i64 '[1])])
   :unrepresentable (kir-of {} '[main] [(f 'main '[p] [:document] :i64 '[1])])
   :capability-with-types (kir-of {:t/pt pt} '[main]
                                  [(f 'main '[] [] :i64 '[(typed-cap-call 7 [:ref :t/pt] :string) (typed-cap-call 5 :i64 :i64)])])
   :capability-unknown (kir-of {} '[main] [(f 'main '[] [] :i64 '[(typed-cap-call 250 :i64 :i64)])])
   :capability-duplicate (kir-of {} '[main] [(f 'main '[] [] :i64 '[(typed-cap-call 7 :i64 :i64) (typed-cap-call 7 :i64 :i64)])])
   :capability-two-types (kir-of {} '[main] [(f 'main '[] [] :i64 '[(typed-cap-call 7 :i64 :i64) (typed-cap-call 7 :string :i64)])])
   :nested-cap-call (kir-of {} '[main] [(f 'main '[] [] :i64 '[(+ 1 (let [x (typed-cap-call 9 :i64 :i64)] x))])])
   :not-checked (assoc (kir-of {} '[main] [(f 'main '[] [] :i64 '[1])]) :format :kotoba.kir/v2)
   :no-schemas-key (dissoc (kir-of {} '[main] [(f 'main '[] [] :i64 '[1])]) :schemas)})

(doseq [[id k] cases]
  (compare-case id k {})
  (compare-case (keyword (str (name id) "-linear")) k {:capability-mode :linear-resource}))

(compare-case :v3-no-capabilities (kir-of {} '[main] [(f 'main '[] [] :i64 '[1])]) {:typed-capability-v3? true})
(compare-case :v3-capabilities (kir-of {} '[main] [(f 'main '[] [] :i64 '[(typed-cap-call 7 :i64 :i64)])]) {:typed-capability-v3? true})

(run-queue!)
(println (str "cases " @total " failures " @fails))
