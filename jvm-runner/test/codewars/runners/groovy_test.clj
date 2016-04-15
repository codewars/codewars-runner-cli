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

(deftest groovy-java-out
  (testing "-main can handle a groovy code with no setup code but no fixture"
    (with-in-str
      (json/generate-string
       {:language "groovy"
        :code "print 'Hello Groovy!'"})
      (is (= "Hello Groovy!" (with-java-out-str (-main)))))))
