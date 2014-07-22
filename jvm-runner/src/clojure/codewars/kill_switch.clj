(ns codewars.kill-switch
  (:import [java.util.concurrent TimeoutException ExecutionException TimeUnit FutureTask]))

;; Adapted from https://github.com/Raynes/clojail/blob/master/src/clojail/core.clj#L24
(defn thunk-timeout
  "Takes a function and an amount of time to wait for the function to finish
   executing."
  [thunk ms]
  (let [task (FutureTask. thunk)
        thr (Thread. task)]
    (try
      (.start thr)
      (.get task ms TimeUnit/MILLISECONDS)
      (catch TimeoutException e
        (.cancel task true)
        (.stop thr)
        (throw (TimeoutException. (str "Timeout: " ms " msecs"))))
      (catch ExecutionException e
        (.cancel task true)
        (.stop thr)
        (throw (.getCause e)))
      (catch Exception e
        (.cancel task true)
        (.stop thr)
        (throw e)))))


(defmacro with-timeout
  "Run a body of code for a given timeout"
  [ms & body]
    `(thunk-timeout (fn [] ~@body) ~ms))
