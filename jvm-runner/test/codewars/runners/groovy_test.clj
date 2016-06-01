(ns codewars.runners.groovy-test
  (:require [clojure.test :refer :all]
            [cheshire.core :as json]
            [codewars.core :refer [-main] :as core]
            [codewars.utils :refer [with-java-out-str]]))

(deftest groovy-code-only
  (testing "-main can handle a groovy code with no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "1 + 1"})
      (is (= 2 (-main))))))

(deftest groovy-basic
  (testing "-main can handle a very basic groovy code and fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "class Greeter { static greet = { name -> 'Hello '+name } }"
        :fixture "class MyTestCase extends GroovyTestCase { void testGreet(){def result = Greeter.greet('Ruslan');assert result == 'Hello Ruslan'}}"
       }
      )
      (is (= org.junit.runner.Result (-main))))))

(deftest groovy-java-out
  (testing "-main can handle a groovy code with no setup code but no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "print 'Hello Groovy!'"})
      (is (= "Hello Groovy!" (with-java-out-str (-main)))))))
