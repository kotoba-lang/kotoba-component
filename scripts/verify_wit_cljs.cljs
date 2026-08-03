;; CI6 — the WIT layer, under a second implementation.
;;
;; This was the one layer CI6 could not compare at all: kotoba.component.wit
;; was .clj, so ClojureScript could not emit a world from checked KIR. That is
;; now a .cljc namespace, and this recomputes every frozen vector under nbb.
;;
;; What a disagreement would mean: the same checked KIR produced a different
;; WIT world on the two implementations — a different set of imports, a
;; different type mapping, or a different identifier canonicalization. Any of
;; those breaks the claim that a component's world is a property of its KIR
;; rather than of the toolchain that happened to build it.
;;
;; Run from the repository root:
;;
;;   nbb --classpath src scripts/verify_wit_cljs.cljs

(require '[kotoba.component.wit :as wit]
         '[cljs.reader :as reader]
         '["fs" :as fs])

(def table (reader/read-string (fs/readFileSync "lang/wit-vectors.edn" "utf8")))

(defn compare-vector [{:keys [id kir source sha256 imports exports]}]
  (let [actual (wit/emit kir)]
    (cond-> []
      (not= source (:source actual))
      (conj {:id id :field :source
             :detail (str "expected " (count source) " chars, got "
                          (count (:source actual)))})
      (not= sha256 (:sha256 actual))
      (conj {:id id :field :sha256 :expected sha256 :actual (:sha256 actual)})
      (not= imports (:imports actual))
      (conj {:id id :field :imports :expected imports :actual (:imports actual)})
      (not= exports (:exports actual))
      (conj {:id id :field :exports :expected exports :actual (:exports actual)}))))

(def failures (vec (mapcat compare-vector (:vectors table))))

(doseq [f failures]
  (println "FAIL" (:id f) (name (:field f)))
  (if (:detail f)
    (println "  " (:detail f))
    (do (println "   clojure:" (pr-str (:expected f)))
        (println "   cljs   :" (pr-str (:actual f))))))

(if (seq failures)
  (do (println "\n" (count failures) "WIT disagreement(s) across"
               (count (:vectors table)) "vectors")
      (js/process.exit 1))
  (println "ok:" (count (:vectors table))
           "WIT worlds emit identical text and digests under ClojureScript"))
