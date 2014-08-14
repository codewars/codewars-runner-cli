(ns codewars.core
  (:require [cheshire.core :as json]
            [codewars.runners :refer [run]]
            [codewars.kill-switch :refer [with-timeout]]
            [environ.core :refer [env]]
            [codewars.runners.groovy]
            [codewars.runners.clojure]
            [codewars.runners.java])
  (:import [java.util.concurrent TimeoutException])
  (:gen-class))

(defn- flush-out [val]
  (flush)
  val)

(defn- fail [e]
  (binding [*out* *err*]
    (println (str "<ERROR::>" (.getMessage e) "<:LF:>")))
  ;(System/exit 1)
  )

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate runner"
  [& _]
  (let [ms ((fnil #(Integer/parseInt %)
                  "5000")
            (env :timeout))
        input (json/parse-stream *in* true)]
    (try
      (flush-out (with-timeout ms (run input)))
      (catch Exception e (fail e)))))
