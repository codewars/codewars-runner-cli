(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main]]
            [cheshire.core :as json]
            [codewars.clojure.test]
            [codewars.runner :refer [run]]))

(deftest sanity-check
  (testing "-main is parsing JSONs from *in* and using handle to handle them"
    (with-in-str "1"
      (with-redefs [run identity]
        (is (= 1 (-main)))))))

(deftest nonsense-language
  (testing "An illegal argument exception will be emitted by main if an invalid language is passed"
    (with-in-str "{\"language\": \"blorg\"}"
      (is (thrown? IllegalArgumentException (-main))))))

(deftest basic-clojure
  (testing "-main can handle a very basic clojure solution and fixture"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(ns foo)"
        :fixture "(ns bar)"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 0, :test 0}
             (-main))))))

(deftest basic-java
  (testing "-main can handle a very basic java solution and fixture"
    (with-in-str (json/generate-string
                  {:language "java"
                   :solution "class Foo {}"
                   :fixture "class Bar {}"})
      (is (= org.junit.runner.Result (class (-main)))))))

(deftest simple-clojure
  (testing "-main can handle a simple clojure solution and fixture"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(ns foo1)
                   (defn wizard [] :ok)"
        :fixture "(ns bar1
                    (:require [foo1]
                      [clojure.test :refer :all]))
                  (deftest ok (is (= :ok (foo1/wizard))))"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 1, :test 1}
             (-main))))))

(deftest clojure-sadpath
  (testing "-main can handle an erroneous test fixture"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(ns dio)
                   (defn holy-diver [] :ride-the-tiger)"
        :fixture "(ns race.for.the.morning
                    (:require [dio]
                      [clojure.test :refer :all]))
                  (deftest oh-we-will-pray-its-all-right
                      (is (= :gotta-get-away (dio/holy-diver))))"})
      (is (= {:type :summary, :fail 1, :error 0, :pass 0, :test 1}
             (with-redefs
               [codewars.clojure.test/fail (constantly nil)]
               (-main)))))))
