(ns codewars.runner
  (:require [codewars.runner :refer [run]]
            [clojure.java.io :as io]
            [codewars.util :as util])
  (:import [codewars.java TempDir]
           [java.net URLClassLoader]
           [javax.tools ToolProvider]
           [org.junit.runner JUnitCore]
           [codewars.java CwRunListener]))

(defn- write-code-and-load! [class-loader dir code]
  (let [{:keys [:file-name :class-name]}
        (util/write-code! "java" dir code)]
    (-> (ToolProvider/getSystemJavaCompiler)
        (.run nil nil nil
              (into-array [(str file-name)])))
    (Class/forName (str class-name) true class-loader)))

(defmethod run "java"
  [{:keys [:setup :fixture :solution]}]
  (let [dir (TempDir/create "java")
        class-loader (URLClassLoader/newInstance
                      (into-array [(-> dir .toURI .toURL)]))
        fixture-class (write-code-and-load!
                       class-loader dir fixture)
        runner (JUnitCore.)]
    (when (not (nil? setup)) (write-code-and-load! class-loader dir setup))
    (write-code-and-load! class-loader dir solution)

    (.addListener runner (CwRunListener.))
    (.run runner (into-array [fixture-class]))))
