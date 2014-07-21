(ns codewars.runner.clojure
  (:require [codewars.runner :refer [run]]
            [codewars.util :as util]
            [codewars.clojure.test]
            [cemerick.pomegranate :as pom])
  (:import [codewars.java TempDir]))

(defmethod run "clojure"
  [{:keys [:setup :solution :fixture] :as opts}]
  (let [dir (TempDir/create "clojure")
        {fixture-ns :class-name}
        (util/write-code! "clojure" dir fixture)]
    (when setup (util/write-code! "clojure" dir setup))
    (util/write-code! "clojure" dir solution)
    (pom/add-classpath dir)
    fixture-ns
    (require [fixture-ns])
    (codewars.clojure.test/run-tests fixture-ns)))
