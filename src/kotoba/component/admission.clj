(ns kotoba.component.admission
  "Compiler-side half of kototama's component admission envelope.

  `kototama.component-platform/validate-world!` admits a component only when it
  is handed an envelope whose key set is exactly

    :target :wasi-version :profile :imports :exports :grants
    :provider-bindings :ambient-wasi :budgets :identity

  Two of those keys are NOT the compiler's to decide, and this namespace
  deliberately does not emit them:

  - `:grants` is the authority decision. `component-platform.edn` fixes
    `:authority {:imports :declared-and-granted-only}`, and ADR-2607252500 puts
    grant policy in the aiueos control plane with a native micro-TCB
    re-verifying it. A compiler that emitted its own grants would be asserting
    the very thing the admission check exists to test.
  - `:provider-bindings` is a composition decision (which provider component
    satisfies which import); `kotoba.component.composition` resolves
    it against real provider artifacts, and the contract requires the binding
    be `:exact`.

  So this namespace emits a REQUEST -- everything the compiler genuinely knows
  -- and names the two keys a composer must add. Requesting is not granting."
  (:require [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def format-tag :kotoba.component-admission-request/v1)

(def envelope-keys
  "The exact key set kototama's validate-world! requires of a full envelope."
  #{:target :wasi-version :profile :imports :exports :grants
    :provider-bindings :ambient-wasi :budgets :identity})

(def composer-supplied-keys
  "Envelope keys a composer/authority must add to a request. See ns docstring."
  #{:grants :provider-bindings})

(def request-keys (into #{} (remove composer-supplied-keys) envelope-keys))

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :component-admission))))

;; --- CIDv1 (raw, sha2-256, base32-lower, unpadded) --------------------------
;; kototama's `cid-looking?` gate only checks the multibase prefix, but a real
;; CID is cheap here and a prefix-shaped digest would be a lie that passes.

(def ^:private base32-alphabet "abcdefghijklmnopqrstuvwxyz234567")

(defn- base32-lower-nopad [^bytes bytes]
  (let [out (StringBuilder.)
        n (alength bytes)
        emit! (fn [chunk] (.append out (.charAt base32-alphabet (bit-and chunk 0x1f))))]
    (loop [i 0 acc 0 bits 0]
      (if (< i n)
        (let [acc (bit-or (bit-shift-left acc 8) (bit-and (aget bytes i) 0xff))
              [acc bits] (loop [acc acc bits (+ bits 8)]
                           (if (>= bits 5)
                             (do (emit! (unsigned-bit-shift-right acc (- bits 5)))
                                 (recur acc (- bits 5)))
                             [acc bits]))]
          (recur (inc i) acc bits))
        (do (when (pos? bits) (emit! (bit-shift-left acc (- 5 bits))))
            (.toString out))))))

(defn- sha256-bytes ^bytes [^bytes input]
  (.digest (MessageDigest/getInstance "SHA-256") input))

(defn cid
  "CIDv1 of `input` bytes: multibase 'b' + base32(0x01 0x55 0x12 0x20 digest).
  0x01 = CID version 1, 0x55 = raw codec, 0x12/0x20 = sha2-256 / 32 bytes."
  [^bytes input]
  (let [digest (sha256-bytes input)
        prefixed (byte-array (+ 4 (alength digest)))]
    (aset-byte prefixed 0 (unchecked-byte 0x01))
    (aset-byte prefixed 1 (unchecked-byte 0x55))
    (aset-byte prefixed 2 (unchecked-byte 0x12))
    (aset-byte prefixed 3 (unchecked-byte 0x20))
    (System/arraycopy digest 0 prefixed 4 (alength digest))
    (str "b" (base32-lower-nopad prefixed))))

(defn cid-of-text [^String text]
  (cid (.getBytes text StandardCharsets/UTF_8)))

;; --- request ----------------------------------------------------------------

(defn- bounded-set! [field values]
  (let [value-set (set values)]
    (when (> (count value-set) 256)
      (reject "component envelope field exceeds its bound"
              {:field field :count (count value-set) :max 256}))
    value-set))

(defn- budgets! [profile budgets]
  (when-not (map? budgets)
    (reject "component budgets must be a map" {:budgets budgets}))
  (let [required (case profile
                   :sync [:fuel :memory-pages]
                   :async [:fuel :memory-pages :deadline-ms :max-items :max-bytes]
                   (reject "component profile is unsupported" {:profile profile}))
        missing (remove #(let [n (get budgets %)] (and (integer? n) (pos? n))) required)]
    (when (seq missing)
      (reject "component requires positive resource bounds"
              {:profile profile :missing (vec missing) :budgets budgets}))
    (when (and (= :async profile) (not= true (:cancellation budgets)))
      (reject "async components require cancellation" {:budgets budgets})))
  budgets)

(defn request
  "Build the compiler's half of a component admission envelope.

  `component` is a `:wasm-component/v1` artifact, `wit` its WIT package. `opts`
  takes `:profile` (`:sync`/`:async`, default `:sync`), `:budgets`, and
  `:package-lock-cid`.

  When no package lock is supplied, `:identity` is omitted and
  `:identity-inputs-missing` names what is absent. That is deliberate: a
  fabricated package-lock CID would pass kototama's shape gate while binding
  the component to a supply chain that was never verified."
  [component wit {:keys [profile budgets package-lock-cid definition-sources]
                  :or {profile :sync}}]
  (when-not (= :wasm-component/v1 (:format component))
    (reject "admission request requires a packaged component"
            {:format (:format component)}))
  (let [imports (bounded-set! :imports (map name (:imports component)))
        exports (bounded-set! :exports (map name (:exports component)))
        definition-cids (into (sorted-set)
                              (map cid-of-text)
                              (or (seq definition-sources) [(:source wit)]))
        component-cid (cid (:bytes component))
        base {:target (:target component)
              :wasi-version (:wasi-version component)
              :profile profile
              :imports imports
              :exports exports
              :ambient-wasi false
              :budgets (budgets! profile budgets)}]
    (assoc (if package-lock-cid
             (assoc base :identity {:component-cid component-cid
                                    :package-lock-cid package-lock-cid
                                    :definition-cids definition-cids})
             (assoc base :identity-inputs-missing #{:package-lock-cid}
                         :component-cid component-cid
                         :definition-cids definition-cids))
           :format format-tag
           :composer-supplied-keys composer-supplied-keys)))

(defn complete
  "Compose a full kototama envelope from a `request` plus the two keys only an
  authority/composer may decide. Returns a map with exactly `envelope-keys`;
  `kototama.component-platform/validate-world!` is the acceptance test."
  [request grants provider-bindings]
  (when-not (= format-tag (:format request))
    (reject "envelope completion requires an admission request"
            {:format (:format request)}))
  (when-not (contains? request :identity)
    (reject "envelope completion requires a bound identity"
            {:missing (:identity-inputs-missing request)}))
  (let [grant-set (bounded-set! :grants grants)
        envelope (-> request
                     (select-keys request-keys)
                     (assoc :grants grant-set :provider-bindings provider-bindings))]
    (when-not (= (:imports envelope) (set (keys provider-bindings)))
      (reject "every declared import requires one exact provider binding"
              {:imports (:imports envelope) :bound (set (keys provider-bindings))}))
    (when-not (every? grant-set (:imports envelope))
      (reject "component import is not granted"
              {:ungranted (vec (remove grant-set (:imports envelope)))}))
    (when-not (= envelope-keys (set (keys envelope)))
      (reject "composed envelope key set is not exact"
              {:keys (set (keys envelope))}))
    envelope))
