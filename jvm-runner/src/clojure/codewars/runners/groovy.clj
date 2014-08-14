(ns codewars.runners.groovy
  (:require [codewars.runners :refer [solution-only]])
  (:import [groovy.lang GroovyShell]))

(defmethod solution-only "groovy"
  [{:keys [:setup :solution]}]
    (when (not (nil? setup)) (throw (Exception. "Setup code is not supported")))
    (.evaluate (GroovyShell.) solution))
