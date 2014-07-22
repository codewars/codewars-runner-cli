(ns codewars.runner.clojure-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer [-main] :as core]
            [codewars.test.utils :refer [with-out-str-not-thread-safe]]
            [cheshire.core :as json]
            [codewars.clojure.test])
  (:import [java.util.concurrent TimeoutException]))

(deftest basic-clojure
  (testing "-main can handle a very basic clojure solution and fixture"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(ns foo)"
        :fixture "(ns bar)"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 0, :test 0}
             (-main))))))

(deftest clojure-simple
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

(deftest clojure-solution-only
  (testing "-main will just run solution code if a fixture is not present"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(print \"Oh no, here it comes again\")"})
      (is (= "Oh no, here it comes again"
             (with-out-str-not-thread-safe (-main)))))))

(deftest clojure-solution-and-setup
  (testing "-main will just run solution code and read correctly from setup code"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :setup "(ns heaven.and.hell) (defn first-track [] (print \"So it's on and on and on, oh it's on and on and on\"))"
        :solution "(require 'heaven.and.hell) (heaven.and.hell/first-track)"})
      (is (= "So it's on and on and on, oh it's on and on and on"
             (with-out-str-not-thread-safe (-main)))))))

(deftest clojure-solution-fixture-and-setup
  (testing "-main can handle a solution, fixture, and setup code in clojure"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :setup "(ns fear.of.the.dark) (defn lyric [] \"I have a constant fear that someone's always near'\")"
        :solution "(ns maiden-greatest-hits (:require [fear.of.the.dark :refer [lyric]])) (defn fear-of-the-dark [] (lyric))"
        :fixture "(ns maiden-test (:require
                      [maiden-greatest-hits]
                      [fear.of.the.dark :refer [lyric]]
                      [clojure.test :refer :all]))
                  (deftest maiden-rocks (is (= (maiden-greatest-hits/fear-of-the-dark) (fear.of.the.dark/lyric))))"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 1, :test 1}
             (-main))))))

(deftest clojure-timeout
  (testing "-main will timeout if a kata solution takes too long"
    (with-in-str
      (json/generate-string
       {:language "clojure"
        :solution "(Thread/sleep 50000)"})
      (with-redefs [core/fail #(throw %)]
        (is (thrown? TimeoutException (-main)))))))
