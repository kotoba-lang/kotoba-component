(ns kotoba.component.doctor-test
  "The comparison logic and the contract/code drift check are pure, so they run
  on any machine — including one with none of the pinned binaries installed,
  which is exactly the machine that needs them."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.component.doctor :as doctor]))

(deftest pins-come-from-the-contract-not-from-this-namespace
  (testing "a preflight that restated the versions would be one more copy to
            drift; these must be read out of component-model-v1.edn"
    (is (= #{:wasm-tools :wac :wasmtime} (set (keys doctor/pins))))
    (is (re-matches #"\d+\.\d+\.\d+" (get-in doctor/pins [:wasm-tools :version])))
    (is (re-matches #"\d+\.\d+\.\d+" (get-in doctor/pins [:wac :version])))
    (is (pos-int? (get-in doctor/pins [:wasmtime :major])))))

(deftest the-contract-and-the-code-agree
  (testing "the declared pins are enforced by hardcoded constants in two other
            namespaces, one of them in another repository. Nothing compared
            them until now, so they could disagree silently and the contract
            would be documenting something untrue."
    (is (empty? (doctor/contract-drift))
        (pr-str (doctor/contract-drift)))))

(deftest version-parsing-tolerates-real-output
  (is (= "1.243.0" (doctor/parse-version "wasm-tools 1.243.0")))
  (testing "a build-hash suffix must not change the verdict"
    (is (= "1.243.0" (doctor/parse-version "wasm-tools 1.243.0 (abc1234)"))))
  (is (= "0.10.1" (doctor/parse-version "wac-cli 0.10.1")))
  (is (= "47.0.3" (doctor/parse-version "wasmtime 47.0.3")))
  (is (nil? (doctor/parse-version "command not found")))
  (is (nil? (doctor/parse-version nil))))

(deftest exact-pins-reject-a-near-miss
  (testing "the version that was actually installed on the machine this came
            from -- two releases ahead, and still wrong for a reproducibility
            gate"
    (let [r (doctor/evaluate :wasm-tools "wasm-tools 1.245.1")]
      (is (false? (:ok? r)))
      (is (= :version-mismatch (:reason r)))))
  (let [pinned (get-in doctor/pins [:wasm-tools :version])]
    (is (:ok? (doctor/evaluate :wasm-tools (str "wasm-tools " pinned))))))

(deftest wasmtime-is-a-minimum-not-an-exact-pin
  (testing "wasm-tools and wac produce artifacts whose bytes must reproduce;
            wasmtime only executes them, so the contract asks for a floor"
    (let [floor (get-in doctor/pins [:wasmtime :major])]
      (is (:ok? (doctor/evaluate :wasmtime (str "wasmtime " floor ".0.0"))))
      (is (:ok? (doctor/evaluate :wasmtime (str "wasmtime " (inc floor) ".0.3"))))
      (let [r (doctor/evaluate :wasmtime (str "wasmtime " (dec floor) ".9.9"))]
        (is (false? (:ok? r)))
        (is (= :below-minimum (:reason r)))))))

(deftest an-absent-binary-is-a-finding-not-an-exception
  (testing "45 raw IOExceptions is what absence looked like before"
    (let [r (doctor/evaluate :wasmtime nil)]
      (is (false? (:ok? r)))
      (is (= :absent (:reason r))))))

(deftest the-report-names-the-fix
  (testing "a preflight that only says FAIL leaves the reader where they were"
    (let [text (doctor/report [(doctor/evaluate :wasmtime nil)] [])]
      (is (re-find #"wasmtime" text))
      (is (re-find #"not installed" text))
      (is (re-find #"cargo install wasmtime-cli" text))))
  (testing "drift is reported distinctly from a missing tool"
    (let [text (doctor/report [] [{:pin :wac :contract "0.10.1"
                                   :enforced-by "x/y" :enforced "0.9.0"}])]
      (is (re-find #"CONTRACT DRIFT" text)))))
