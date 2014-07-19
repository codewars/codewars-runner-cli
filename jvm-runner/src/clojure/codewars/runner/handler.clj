(ns codewars.runner.handler
  (:require [codewars.runner.infer :as infer]
            [codewars.clojure.test]
            [clojure.java.io :as io]
            [cemerick.pomegranate :as pom])
  (:import [codewars.java TempDir]))

(defmulti handle
  (fn [{:keys [:language]}]
    (if (contains? (methods handle) language)
      language
      ::not-implemented)))

(defmethod handle ::not-implemented
  [{:keys [:language]}]
  (throw (IllegalArgumentException.
          (format "Language %s is not implemented"
                  (pr-str language)))))

(defn- write-code! [language dir code]
  (let [base-name (infer/file-name "clojure" code)
        file-name (io/file dir base-name)]
    (if (.exists file-name)
      (throw (UnsupportedOperationException.
              (format "Could not write to file %s, because that file already exists.  Perhaps it already contains the setup or test fixture code?\ncode:\n%s" (pr-str base-name) code)))
      (spit file-name code))))

(defmethod handle "clojure"
  [{:keys [:setup :solution :fixture]}]
  (let [dir (TempDir/create "clojure")
        fixture-ns (->> fixture (infer/class-name "clojure") symbol)]
    (when setup (write-code! "clojure" dir setup))
    (write-code! "clojure" dir fixture)
    (write-code! "clojure" dir solution)
    (pom/add-classpath dir)
    (require [fixture-ns])
    (codewars.clojure.test/run-tests fixture-ns)))
