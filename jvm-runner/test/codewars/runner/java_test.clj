(ns codewars.runner.java-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main]]
            [cheshire.core :as json]
            [codewars.runner :refer [run]]))

(deftest java-basic
  (testing "-main can handle a very basic java solution and fixture"
    (with-in-str
      (json/generate-string
       {:language "java"
        :solution "class Foo {}"
        :fixture "class Bar {}"})
      (is (= org.junit.runner.Result (class (-main)))))))

(deftest java-solution-only
  (testing "-main can handle a java solution without a fixture"
    (with-in-str
      (json/generate-string
       {:language "java"
        :solution "public class FooFighters {public static int main() {return 1;}}"})
      (is (= 1 (-main))))))
