(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main]]
            [cheshire.core :as json]
            [codewars.runner :refer [run]]))

(deftest sanity-check
  (testing "-main is parsing JSONs from *in* and using handle to handle them"
    (with-in-str "1"
      (with-redefs [run identity]
        (is (= 1 (-main)))))))

(deftest handle-nonsense-language
  (testing "An illegal argument exception will be emitted by main if an invalid language is passed"
    (with-in-str "{\"language\": \"blorg\"}"
      (is (thrown? IllegalArgumentException (-main))))))

(deftest handle-clojure
  (testing "-main can handle a very basic clojure solution and fixture"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(ns foo)"
        :fixture "(ns bar)"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 0, :test 0}
             (-main))))))

(deftest handle-java
  (testing "-main can handle a very basic java solution and fixture"
    (with-in-str (json/generate-string
                  {:language "java"
                   :solution "class Foo {}"
                   :fixture "class Bar {}"})
      (is (= org.junit.runner.Result (class (-main)))))))
