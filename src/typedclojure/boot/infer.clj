(ns typedclojure.boot.infer
  (:require [clojure.test :as test]
            [clojure.set :as set]
            [clojure.core.typed :as t]))

(def infer-ns-config ".boot-typedclojure-infer-ns")

(defn pre-startup [opts]
  {:pre [(symbol? (:infer-ns opts))
         (#{:type :spec} (:infer-kind opts))
         ((some-fn nil? integer?) (:test-timeout-ms opts))
         ((some-fn nil? string?) (:load-infer-results opts))]}
  ;(prn "pre-startup")
  (spit infer-ns-config
        (binding [*print-dup* true]
          (pr-str opts))))

;; from https://stackoverflow.com/a/27550676
(defn exec-with-timeout [timeout-ms callback]
	(let [fut (future (callback))
				ret (deref fut timeout-ms ::timed-out)]
		(when (= ret ::timed-out)
			(println "Test timed out.")
			(future-cancel fut))
		ret))

(defn test-var-with-timeout [test-timeout-ms load-infer-results]
  {:pre [((some-fn nil? string?) load-infer-results)]}
  (if load-infer-results
    (fn [v])
    (fn [v]
      (when-let [t (:test (meta v))]
        (binding [test/*testing-vars* (conj test/*testing-vars* v)]
          (test/do-report {:type :begin-test-var, :var v})
          (test/inc-report-counter :test)
          (try (if-some [timeout test-timeout-ms]
                 (exec-with-timeout timeout t)
                 (t))
               (catch Throwable e
                 (test/do-report
                   {:type :error, :message "Uncaught exception, not in assertion."
                    :expected nil, :actual e})))
          (test/do-report {:type :end-test-var, :var v}))))))

(defn monkeypatch-test-var [test-timeout-ms load-infer-results]
  (alter-var-root #'test/test-var 
                  (constantly (fn [& args] 
                                (-> (test-var-with-timeout test-timeout-ms load-infer-results)
                                    (apply args))))))


(defn startup []
  ;(prn "startup")
  (let [{:keys [:infer-ns :infer-kind :test-timeout-ms :load-infer-results]} (read-string (slurp infer-ns-config))
        _ (assert (symbol? infer-ns))
        _ (assert (#{:type :spec} infer-kind))
        _ (assert ((some-fn nil? string?) load-infer-results))
        _ (if load-infer-results
            (println "Skipping instrumentation and test suite since --load-infer-results was provided")
            (t/prepare-infer-ns :ns infer-ns))
        _ (monkeypatch-test-var test-timeout-ms load-infer-results)]
    ;(prn "end startup")
    nil))

(defn shutdown []
  ;(prn "shutdown")
  (let [{:keys [:infer-ns :infer-kind :infer-opts :load-infer-results]} (read-string (slurp infer-ns-config))
        _ (assert (symbol? infer-ns))
        _ (assert (#{:type :spec} infer-kind))
        infer-fn (case infer-kind
                   :type t/runtime-infer
                   :spec t/spec-infer)]
    (apply infer-fn 
           :ns infer-ns 
           :out-dir (str "infer-" (name infer-kind)) 
           :load-infer-results load-infer-results
           (apply concat infer-opts))
    ;(prn "end shutdown")
    nil))
