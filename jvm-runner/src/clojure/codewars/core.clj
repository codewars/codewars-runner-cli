(ns codewars.core
  (:require [cheshire.core :as json]
            [codewars.runner :refer [run]]
            [codewars.kill-switch :refer [with-timeout]]
            [environ.core :refer [env]]
            [codewars.runner.java]
            [codewars.runner.clojure])
  (:import [java.util.concurrent TimeoutException])
  (:gen-class))

(defn- fail [e]
  (binding [*out* *err*]
    (println (str "<ERROR::>" (.getMessage e) "<:LF:>")))
  (System/exit 1))

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate runner"
  [& _]
  (let [ms (-> env :timeout Integer/parseInt)
        input (json/parse-stream *in* true)]
    (try
      (with-timeout ms (run input))
      (catch Exception e (fail e)))))
