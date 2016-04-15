(ns codewars.runners.groovy
  (:require [codewars.runners :refer [code-only]])
  (:import [groovy.lang GroovyShell]))

(defmethod code-only "groovy"
  [{:keys [:setup :code]}]
  (let [shell (GroovyShell.)]
    ;; TODO: Test setup code
    (when (not (nil? setup)) (.evaluate shell setup))
    (.evaluate shell code)))
