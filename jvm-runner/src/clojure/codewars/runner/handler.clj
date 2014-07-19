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
  ;; TODO: Check that file DNE before writing
  (spit (io/file dir (infer/file-name "clojure" code))
        code))

(defmethod handle "clojure"
  [{:keys [:setup :solution :fixture]}]
  ;; TODO: TempDir should output a java.io.File
  (let [dir (str (TempDir/create "clojure"))
        fixture-ns (->> fixture (infer/class-name "clojure") symbol)]
    (when setup (write-code! "clojure" dir setup))
    (write-code! "clojure" dir solution)
    (write-code! "clojure" dir fixture)
    (pom/add-classpath dir)
    (require [fixture-ns])
    (codewars.clojure.test/run-tests fixture-ns)))
