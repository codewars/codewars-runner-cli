(ns codewars.runner.java-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main]]
            [cheshire.core :as json]
            [codewars.runner :refer [run]])
  (:import [org.apache.commons.io.output WriterOutputStream]
           [java.io PrintStream ByteArrayOutputStream]))

(defmacro with-java-out-str [& body]
  `(let [original-out# (PrintStream. (. System out))
         byte-out# (ByteArrayOutputStream.)]
     (-> byte-out# (PrintStream.) System/setOut)
     ~@body
     (System/setOut original-out#)
     (.toString byte-out#)))

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


(deftest java-solution-print
  (testing "-main can handle a java solution that prints to standard out"
    (with-in-str
      (json/generate-string
       {:language "java"
        :solution "public class Hello {public static void main() {System.out.print(\"Hellooo!\");}}"})
      (is (= "Hellooo!" (with-java-out-str (-main)))))))
