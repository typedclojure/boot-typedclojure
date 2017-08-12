(ns typedclojure.boot
  {:boot/export-tasks true}
  (:require [boot.core :as core]
            [typedclojure.boot.infer :as infer]
            [clojure.java.io :as io]
            [clojure.tools.namespace.find :refer [find-namespaces-in-dir]]
            [adzerk.boot-test :as bt]))

(core/deftask spec-infer
  "Run clojure.test tests in a pod. Throws on test errors or failures.

  The --namespaces option specifies the namespaces to test. The default is to
  run tests in all namespaces found in the project.

  The --exclusions option specifies the namespaces to exclude from testing.

  The --filters option specifies Clojure expressions that are evaluated with %
  bound to a Var in a namespace under test. All must evaluate to true for a Var
  to be considered for testing by clojure.test/test-vars.

  The --junit-output-to option specifies the path to a directory relative to the
  target directory where a junit xml file for each test namespace will be
  generated by using the clojure.test.junit facility. When present it will make
  the target to be synced even when there are test errors or failures.

  The --infer-ns option specifies Clojure namespace that will be inferred for
  specs during unit testing.

  The --test-timeout-ms option specifies a timeout (in milliseconds) that will be
  applied to each individual unit test during execution.
  "
  [c clojure    VERSION   str    "the version of Clojure for testing."
   n namespaces NAMESPACE #{sym} "The set of namespace symbols to run tests in."
   e exclusions NAMESPACE #{sym} "The set of namespace symbols to be excluded from test."
   f filters    EXPR      #{edn} "The set of expressions to use to filter namespaces."
   X exclude    REGEX     regex  "the filter for excluded namespaces"
   I include    REGEX     regex  "the filter for included namespaces"
   r requires   REQUIRES  #{sym} "Extra namespaces to pre-load into the pool of test pods for speed."
   s shutdown   FN        #{sym} "functions to be called prior to pod shutdown"
   S startup    FN        #{sym} "functions to be called at pod startup"
   j junit-output-to JUNITOUT str "The directory where a junit formatted report will be generated for each ns"
   t infer-ns		INFER sym "The namespace symbol to run spec inference on"
   m test-timeout-ms  TIMEOUT int "Timeout for a single test (milliseconds). Default: No timeout."
   ]
  (let []
    (assert (symbol? infer-ns) "Must provide --infer-ns option")
    (comp
      (core/with-pre-wrap fileset
        (infer/pre-startup infer-ns :spec test-timeout-ms)
        fileset)
      (bt/test
        :requires (into #{'typedclojure.boot.infer} requires)
        :startup (into `#{infer/startup} startup)
        :shutdown (into `#{infer/shutdown} shutdown)
        :clojure clojure
        :namespaces namespaces
        :exclusions exclusions
        :filters filters
        :exclude exclude
        :include include
        :junit-output-to junit-output-to))))
