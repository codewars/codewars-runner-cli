(ns codewars.core
  (:require [cheshire.core :as json]
            [codewars.runner :refer [run]]
            [codewars.runner.java]
            [codewars.runner.clojure])
  (:gen-class))

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate runner"
  []
  (-> *in* (json/parse-stream true) run))
