(ns codewars.warm-up
  (:require [codewars.runners :refer [run]]
            [codewars.runners.java]
            [codewars.runners.clojure]
            [codewars.utils :refer :all]
            [clojure.string :refer [split join capitalize]]))

(def replacements
  {\0 \G, \1 \H, \2 \I, \3 \J, \4 \K, \5 \L, \6 \M, \7 \N,
   \8 \O, \9 \P, \a \A, \b \B, \c \C, \d \D, \e \E, \f \F
   \- \-})

(defn- random-name-space []
  (let [ [class-name & package-path]
         (-> (java.util.UUID/randomUUID)
             str
             vec
             (->> (map replacements)
                  (apply str))
             (split #"-")
             (->> (map capitalize)))]
    {:class-name class-name
     :package (join "." package-path)}))

(defn clojure-code-warm-up
  []
  (run {:language "clojure"
        :code ":warm-up-payload"}))

(defn java-code-warm-up
  []
  (let [{:keys [:class-name :package]} (random-name-space)]
    (run {:language "java"
          :code (format "package %s ; class %s { static int main(String [] args) {return 7;} }" package class-name)})))

(defn clojure-test-warm-up
  []
  (let [setup-namespace (join "." (vals (random-name-space)))
        code-namespace (join "." (vals (random-name-space)))
        fixture-namespace (join "." (vals (random-name-space)))]
    (with-all-out-str
      (run {:language "clojure"
            :setup (format "(ns %s) (def x :test-payload)" setup-namespace)
            :code (format "(ns %s (:require [%s :as setup])) (def y setup/x)" code-namespace setup-namespace)
            :fixture (format "(ns %s (:require [%s :as code] [clojure.test :refer :all])) (deftest test-payload-test (prn :in-test-payload-test) (testing \"foooo\" (is (= :test-payload code/y))))" fixture-namespace code-namespace)}))))
(defn java-test-warm-up
  []
  (let [setup (random-name-space)
        code (random-name-space)
        fixture (random-name-space)]
    (with-all-out-str
      (run
        {:language "java"
         :setup (format  "package %s ; public class %s { public static String payload() {return \"test payload\";}}" (:package setup) (:class-name setup))
         :code (format "package %1$s ; public class %2$s { public %2$s(){} public String payloadForwarder(){return %3$s.%4$s.payload();}}" (:package code) (:class-name code) (:package setup) (:class-name setup)),
         :fixture (format  "package %1$s ; import static org.junit.Assert.assertEquals; import org.junit.Test; import org.junit.runners.JUnit4; public class %2$s { public %2$s (){} @Test public void codeAndSetupAndFixture(){ %3$s.%4$s s = new %3$s.%4$s(); assertEquals(\"Verifying test payload\", \"test payload\", s.payloadForwarder()); System.out.println(\"test success\");}}"
                           (:package fixture) (:class-name fixture)
                           (:package code) (:class-name code))}))))

(defn warm-up []
  (clojure-code-warm-up)
  (clojure-test-warm-up)
  (java-code-warm-up)
  (java-test-warm-up))
