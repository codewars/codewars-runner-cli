(ns codewars.kill-switch
  (:import [java.util.concurrent TimeoutException ExecutionException TimeUnit FutureTask]))

(def ^{:doc "Create a map of pretty keywords to ugly TimeUnits"}
  uglify-time-unit
  (into {} (for [[enum aliases] {TimeUnit/NANOSECONDS [:ns :nanoseconds]
                                 TimeUnit/MICROSECONDS [:us :microseconds]
                                 TimeUnit/MILLISECONDS [:ms :milliseconds]
                                 TimeUnit/SECONDS [:s :sec :seconds]}
                 alias aliases]
             {alias enum})))

(defn thunk-timeout
  "Takes a function and an amount of time to wait for thse function to finish
   executing. The sandbox can do this for you. unit is any of :ns, :us, :ms,
   or :s which correspond to TimeUnit/NANOSECONDS, MICROSECONDS, MILLISECONDS,
   and SECONDS respectively."
  ([thunk ms]
     (thunk-timeout thunk ms :ms nil)) ; Default to milliseconds, because that's pretty common.
  ([thunk time unit]
     (thunk-timeout thunk time unit nil))
  ([thunk time unit tg]
     (let [task (FutureTask. thunk)
           thr (if tg (Thread. tg task) (Thread. task))]
       (try
         (.start thr)
         (.get task time (or (uglify-time-unit unit) unit))
         (catch TimeoutException e
           (.cancel task true)
           (.stop thr)
           (throw (TimeoutException. "Execution timed out.")))
         (catch ExecutionException e
           (.cancel task true)
           (.stop thr)
           (throw (.getCause e)))
         (catch Exception e
           (.cancel task true)
           (.stop thr)
           (throw e))
         (finally (when tg (.stop tg)))))))


(defmacro with-timeout
  "Run a body of code for a given timeout"
  [ms & body]
    `(thunk-timeout (fn [] ~@body) ~ms))
