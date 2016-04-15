(ns codewars.core
  (:require [cheshire.core :as json]
            [codewars.runners :refer [run]]
            [codewars.kill-switch :refer [with-timeout]]
            [environ.core :refer [env]]
            [codewars.warm-up :refer [warm-up]]
            [codewars.runners.groovy]
            [codewars.runners.clojure]
            [codewars.runners.java])
  (:gen-class))

(defn- flush-out [& [val]]
  (. *out* (flush))
  (. *err* (flush))
  (.flush (System/out))
  (.flush (System/err))
  val)

(defn- fail [e]
  (println (str "<ERROR::>" (.getMessage e) "<:LF:>"))
  (let [sw (java.io.StringWriter.)]
    (.printStackTrace e (java.io.PrintWriter. sw))
    (-> (str sw)
        (clojure.string/replace "\n" "<:LF:>")
        println))
  (flush-out)
  (System/exit 1))

(def timeout ((fnil #(Integer/parseInt %) "5000") (env :timeout)))

(defn -main
  "Listens to *in* for a JSON message, parses it and calls the appropriate runner"
  [& args]
  (when (not (empty? args))
    (warm-up)
    (apply println args)
    (flush))
  (let [input (json/parse-stream *in* true)]
    (try
      (flush-out (with-timeout timeout (run input)))
      (catch Exception e (fail e)))))
