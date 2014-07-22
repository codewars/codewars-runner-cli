(ns codewars.core
  (:require [cheshire.core :as json]
            [codewars.runner :refer [run]]
            [codewars.kill-switch :refer [with-timeout]]
            [environ.core :refer [env]]
            [codewars.runner.java]
            [codewars.runner.clojure])
  (:import [java.util.concurrent TimeoutException])
  (:gen-class))

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate runner"
  []
  (let [t (-> env :timeout Integer/parseInt)
        input (json/parse-stream *in* true)]
    ;; TODO: handle exceptions and run System/exit
      (with-timeout t (run input))))
