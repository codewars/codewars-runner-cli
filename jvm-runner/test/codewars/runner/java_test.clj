(ns codewars.runner.java-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main]]
            [codewars.test.utils :refer [with-java-out-str]]
            [cheshire.core :as json]))

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

(deftest java-test-fixture
  (testing "-main can handle a junit test"
    (with-in-str
      (json/generate-string
       {:language "java"
        :solution "public class Solution {
                   public Solution(){}
                   public int testthing(){return 3;}}",
        :fixture "import static org.junit.Assert.assertEquals;
                  import org.junit.Test;
                  import org.junit.runners.JUnit4;
                  public class TestFixture {
                     public TestFixture(){}
                     @Test public void myTestFunction(){
                        Solution s = new Solution();
                         assertEquals(\"wow\", 3, s.testthing());
                         System.out.println(\"test out\");}}"})
      (let [test-out-string (with-java-out-str (-main))]
        (is (.contains test-out-string "<DESCRIBE::>myTestFunction(TestFixture)"))
        (is (.contains test-out-string "test out"))
        (is (.contains test-out-string "<PASSED::>Test Passed<:LF:>"))))))
