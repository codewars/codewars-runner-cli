(ns codewars.runners.groovy-test
  (:require [clojure.test :refer :all]
            [clojure.test.junit :refer [with-junit-output]]
            [cheshire.core :as json]
            [codewars.core :refer [-main] :as core]
            [codewars.utils :refer [with-java-out-str]]
  )

(deftest groovy-code-only
  (testing "-main can handle a groovy code with no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "1 + 1"})
      (is (= 2 (-main)))
    )
  )
)

(deftest groovy-java-out
  (testing "-main can handle a groovy code with no setup code but no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "print 'Hello Groovy!'"}
      )
      (is (= "Hello Groovy!" (with-java-out-str (-main))) )
    )
  )
)

(deftest groovy-java-fixt
  (testing "-main can handle a groovy code with no setup code but no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "class Greeter { static greet = { name -> 'Hello '+name } }"
        :fixture "class MyTestCase extends GroovyTestCase { void testGreet(){def result = Greeter.greet('Ruslan');assert result == 'Hello Ruslan';println 'Lets sum'}}"}
      )
      ;(with-java-out-str (run-all-tests))
  
     (let [test-out-string (groovy-java-fixt (-main))]
        (is (.contains test-out-string "<DESCRIBE::>testGreet(MyTestCase)"))
        (is (.contains test-out-string "Lets sum"))
        (is (.contains test-out-string "<PASSED::>Test Passed<:LF:>"))
     )

    )
  )
)
