(ns codewars.clojure.test
  (:refer-clojure :exclude [time])
  (:require
   [clojure.test :refer :all :exclude [run-tests with-test-out]]
   [clojure.string]
   [clojure.stacktrace :as stack]))

(defmacro with-test-out [& body]
  `(->
    (with-out-str ~@body)
    (clojure.string/replace "\n" "<:LF:>")
    (println)))

(defn- print-context []
  (when (seq *testing-contexts*)
    (->> (testing-contexts-str) (str "<IT::>") println)))

(defn- print-with-message [status {:keys [:message]}]
  (if (string? message)
    (println (str status message))
    (println status)))

(defn- expr-str [expression]
  (if (instance? Throwable expression)
    (with-out-str
      (stack/print-cause-trace expression *stack-trace-depth*))
    (pr-str expression)))

(defn- print-expectations [{:keys [:expected :actual]}]
  (println "expected:" (pr-str expected) "- actual:" (expr-str actual)))

(defn- fail []
  (flush)
  (System/exit 1))

(defmulti codewars-report :type)

(defmethod codewars-report :pass [_]
  (with-test-out
    (inc-report-counter :pass)
    (print-context)
    (println "<PASSED::>Test Passed")))

(defmethod codewars-report :fail [m]
  (with-test-out
    (inc-report-counter :fail)
    (print-context)
    (print-with-message "<FAILED::>" m)
    (print-expectations m))
  (fail))

(defmethod codewars-report :error [m]
  (with-test-out
    (inc-report-counter :error)
    (print-context)
    (print-with-message "<ERROR::>" m)
    (print-expectations m))
  (fail))

(defmethod codewars-report :begin-test-ns [_])
(defmethod codewars-report :end-test-ns [_])

(defmethod codewars-report :begin-test-var [m]
  (with-test-out
    (print "<DESCRIBE::>")
    (-> m :var (. sym) println)))
(defmethod codewars-report :end-test-var [_])

(defmethod codewars-report :summary [_])

(defmacro time
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr]
     (println (str "<COMPLETEDIN::>"
                   (-> (System/nanoTime) (- start#) double (/ 1000000.0))
                   " ms<:LF:>"))
     ret#))

(defn run-tests [ & namespaces ]
  (binding [clojure.test/report
            codewars.clojure.test/codewars-report]
    (time (apply clojure.test/run-tests namespaces))))
