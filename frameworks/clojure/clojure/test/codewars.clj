(ns clojure.test.codewars
  (:require
   [clojure.test :refer :all]
   [clojure.stacktrace :as stack]))

(defmulti codewars-report :type)

(defmethod codewars-report :pass [m]
  (with-test-out 
    (when (seq *testing-contexts*)
      (->> (testing-contexts-str) (str "<IT::>") println))
    (-> "<PASSED::>Test Passed" println with-test-out)))

(defmethod codewars-report :fail [m]
  (with-test-out 
    (when (seq *testing-contexts*)
      (->> (testing-contexts-str) (str "<IT::>") println))
    (print "<FAILED::>")
    (when-let [message (:message m)] (print message "- "))
    (print "expected:" (pr-str (:expected m)))
    (println " actual:" (pr-str (:actual m)))))

(defmethod codewars-report :error [m]
  (with-test-out
    (when (seq *testing-contexts*)
      (->> (testing-contexts-str) (str "<IT::>") println))
    (print "<ERROR::>")
    (when-let [message (:message m)] (print message "- "))
    (print "expected:" (pr-str (:expected m)))
    (println " actual: ")
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (stack/print-cause-trace actual *stack-trace-depth*)
        (prn actual)))))

(defmethod codewars-report :begin-test-ns [_])
(defmethod codewars-report :end-test-ns [_])

(defmethod codewars-report :begin-test-var [m]
  (with-test-out 
    (print "<DESCRIBE::>")
    (-> m :var (. sym) println)))
(defmethod codewars-report :end-test-var [_])
(defmethod codewars-report :summary [_])
