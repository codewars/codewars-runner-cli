(ns codewars.warm-up-test
  (:require [clojure.test :refer :all]
            [codewars.warm-up :refer :all]))

(deftest warm-up-test
  (testing "java-code-warm-up should return 7 every time"
    (is (= 7 (java-code-warm-up))))
  (testing "clojure-code-warm-up should return :warm-up-payload every time"
    (is (= :warm-up-payload (clojure-code-warm-up))))
  (testing "clojure-test-warm-up should pass"
    (let [result (clojure-test-warm-up)]
      (is (.contains result "PASSED"))
      (is (.contains result ":in-test-payload-test"))))
 (testing "java-test-warm-up should pass"
    (let [result (java-test-warm-up)]
      (is (.contains result "PASSED")
          (.contains result "codeAndSetupAndFixture")))))
