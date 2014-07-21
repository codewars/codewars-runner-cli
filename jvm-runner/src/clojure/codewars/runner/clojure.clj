(ns codewars.runner.clojure
  (:require [codewars.runner :refer [run]]
            [codewars.util :as util]
            [codewars.clojure.test]
            [clojure.java.io :as io]
            [dynapath.util :as dp])
  (:import [codewars.java TempDir])
  (:refer-clojure :exclude (add-classpath)))

;; Extracted from cemerick.pomegranate
(defn- classloader-hierarchy
  "Returns a seq of classloaders, with the tip of the hierarchy first.
   Uses the current thread context ClassLoader as the tip ClassLoader if one is not provided."
  ([] (classloader-hierarchy (.. Thread currentThread getContextClassLoader)))
  ([tip]
    (->> tip
      (iterate #(.getParent %))
      (take-while boolean))))

(defn- add-classpath
  "A fixed version of the (deprecated) `add-classpath` in clojure.core."
  ([jar-or-dir classloader]
     (if-not (dp/add-classpath-url classloader (.toURL (.toURI (io/file jar-or-dir))))
       (throw (IllegalStateException. (str classloader " is not a modifiable classloader")))))
  ([jar-or-dir]
    (let [classloaders (classloader-hierarchy)]
      (if-let [cl (last (filter dp/addable-classpath? classloaders))]
        (add-classpath jar-or-dir cl)
        (throw (IllegalStateException. (str "Could not find a suitable classloader to modify from "
                                            classloaders)))))))


(defmethod run "clojure"
  [{:keys [:setup :solution :fixture] :as opts}]
  (let [dir (TempDir/create "clojure")
        {fixture-ns :class-name}
        (util/write-code! "clojure" dir fixture)]
    (when setup (util/write-code! "clojure" dir setup))
    (util/write-code! "clojure" dir solution)
    (add-classpath dir)
    (require fixture-ns)
    (codewars.clojure.test/run-tests fixture-ns)))
