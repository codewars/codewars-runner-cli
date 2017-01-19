(ns codewars.runners.java-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main] :as core]
            [codewars.utils :refer :all]
            [cheshire.core :as json]))

(deftest java-basic
  (testing "-main can handle a very basic java code and fixture"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "class Foo {}"
        :fixture "class Bar {}"})
      (is (= org.junit.runner.Result (class (-main)))))))

(deftest java-code-only
  (testing "-main can handle a java code without a fixture"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "class FooFighters { static int main(String [] args) {return 1;} }"})
      (is (= 1 (-main))))
   (with-in-str
      (json/generate-string
       {:language "java"
        :code "package bar.baz ; class FooFighters { static int main(String [] args) {return 7;} }"})
      (is (= 7 (-main))))))



(deftest java-code-print
  (testing "-main can handle a java code that prints to standard out"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "public class Hello {public static void main(String[] args) {System.out.print(\"Hellooo!\");}}"})
      (is (= "Hellooo!" (with-java-out-str (-main)))))))

(deftest java-setup-code
  (testing "-main can handle a java code and setup code"
    (with-in-str
      (json/generate-string
       {:language "java"
        :setup "import java.lang.String;
                public class Beatles {public static String sayHello() {
                  return \"Hello, hello!\";}}"
        :code "class HelloAgain {
                     static void main(String[] args) {
                         System.out.print(Beatles.sayHello());}}"})
      (is (= "Hello, hello!" (with-java-out-str (-main)))))))

(deftest java-test-fixture
  (testing "-main can handle a junit test"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "public class Code {
                   public Code(){}
                   public int testthing(){return 3;}}",
        :fixture "import static org.junit.Assert.assertEquals;
                  import org.junit.Test;
                  import org.junit.runners.JUnit4;
                  public class TestFixture {
                     public TestFixture(){}
                     @Test public void myTestFunction(){
                        Code s = new Code();
                         assertEquals(\"wow\", 3, s.testthing());
                         System.out.println(\"test out\");}}"})
      (let [test-out-string (with-java-out-str (-main))]
        (is (.contains test-out-string "<DESCRIBE::>myTestFunction(TestFixture)"))
        (is (.contains test-out-string "test out"))
        (is (.contains test-out-string "<PASSED::>Test Passed<:LF:>"))))))

(deftest java-sad-path
  (testing "-main can handle a failing junit test"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "public class Foo {
                     public Foo(){}
                     public int testthing(){return 3;}
                     public static void main(String[] args){System.out.println(\"42\");}}"
        :fixture "import static org.junit.Assert.assertEquals;
                  import org.junit.Test;
                  import org.junit.runners.JUnit4;
                  public class TestForFailure {
                    public TestForFailure(){}
                    @Test public void sadPath(){
                      Foo s = new Foo();
                      assertEquals(\"Failed Message\", 5, s.testthing());
                      System.out.println(\"Shouldn't get here\");}}"})
      (let [test-out-string (with-java-out-str (-main))]
        (is (.contains test-out-string "<DESCRIBE::>sadPath(TestForFailure)<:LF:>"))
        (is (.contains test-out-string "<FAILED::>Failed Message expected:"))
        (is (.contains test-out-string "<5> but was:<3>"))
        (is (not (.contains test-out-string "Shouldn't get here")))
        (is (not (.contains test-out-string "<PASSED::>Test Passed<:LF:>")))))))

(deftest java-bad-code
  (testing "-main fails when code can't compile"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "public class Code {
                     public static void main(String[] args){
                       notdefinedgonnafail(\"42\");}}"})
      (let [error-message
            (with-redefs [core/fail #(-> % .getMessage)] (-main))]
        (is (.contains error-message "error: cannot find symbol"))
        (is (.contains error-message "notdefinedgonnafail(\"42\");"))
        (is (.contains error-message "symbol:   method notdefinedgonnafail(String)"))
        (is (.contains error-message "location: class Code"))
        (is (.contains error-message "1 error"))))))

(deftest java-nine-yards
  (testing "-main can setup, code, and test fixture code for java"
    (with-in-str
      (json/generate-string
       {:language "java"
        :setup "public class Setupp { public static int three() {return 3;}}"
        :code "public class Sollution {
                   public Sollution(){}
                   public int testthingy(){return Setupp.three();}}",
        :fixture "import static org.junit.Assert.assertEquals;
                  import org.junit.Test;
                  import org.junit.runners.JUnit4;
                  public class NineYards {
                     public NineYards(){}
                     @Test public void codeAndSetupAndFixture(){
                         Sollution s = new Sollution();
                         assertEquals(\"wow\", 3, s.testthingy());
                         System.out.println(\"test out\");}}"})
      (let [test-out-string (with-java-out-str (-main))]
        (is (.contains test-out-string "<DESCRIBE::>codeAndSetupAndFixture(NineYards)<:LF:>"))
        (is (.contains test-out-string "test out"))
        (is (.contains test-out-string "<PASSED::>Test Passed<:LF:>"))))))


(deftest codewars-kumite-test
  (testing "Can handle a simple static export"
    (with-in-str
      (json/generate-string
       {:language "java"
        :code "public class Java {
                     public static int multiply(int x, int y) { return x * y; } }"
        :fixture "import org.junit.*;

                  public class JavaTest{
                    @Test
                    public final void testMultiply() {
                      Assert.assertEquals(\"The two values should multiply together\", 50, Java.multiply(10, 5));
                    }
                  }"})
      (let [test-out-string (with-java-out-str (-main))]
        (is (.contains test-out-string "<DESCRIBE::>testMultiply(JavaTest)<:LF:>"))
        (is (.contains test-out-string "<PASSED::>Test Passed<:LF:>"))))))
