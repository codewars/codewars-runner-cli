(ns codewars.runner.core-test
  (:require [clojure.test :refer :all]
            [codewars.runner.core :refer :all]
            [cheshire.core :as json]
            [codewars.runner.handler :refer [handle]]))

(deftest sanity-check
  (testing "-main is parsing JSONs from *in* and using handle to handle them"
    (with-in-str "1"
      (with-redefs [handle identity]
        (is (= 1 (-main)))))))

(deftest handle-multi-method
  (testing "-main is parsing JSONs from *in* and using handle to handle them"
    (with-in-str "{\"language\": \"blorg\"}"
      (is (thrown? IllegalArgumentException (-main))))))

(deftest handle-clojure
  (testing "-main can handle a clojure solution and fixture"
    (with-in-str (json/generate-string
                  {:language "clojure"
                   :solution "(ns foo)"
                   :fixture "(ns bar)"})
      (is (= {:type :summary, :fail 0, :error 0, :pass 0, :test 0}
             (-main))))))
