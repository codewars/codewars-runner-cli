(ns codewars.runner.core
  (:require [cheshire.core :as json]
            [codewars.runner.handler :refer [handle]])
  (:gen-class))

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate handler"
  []
  (-> *in* (json/parse-stream true) handle))
