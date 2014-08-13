(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main] :as core]
            [cheshire.core :as json]
            [codewars.runners :refer [run]]))

(deftest sanity-check
  (testing "-main is parsing JSONs from *in* and using handle to handle them"
    (with-in-str "1"
      (with-redefs [run identity]
        (is (= 1 (-main)))))))

(deftest nonsense-language
  (testing "An illegal argument exception will be emitted by main if an invalid language is passed"
    (with-in-str "{\"language\": \"blorg\"}"
      (with-redefs [core/fail #(throw %)]
        (is (thrown? IllegalArgumentException (-main)))))))
