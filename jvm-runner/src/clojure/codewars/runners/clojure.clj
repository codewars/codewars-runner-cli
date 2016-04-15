(ns codewars.runners.clojure
  (:require
   [codewars.runners :refer [code-only full-project]]
   [codewars.clojure.test]))

(defn name-space
  [code]
  (if-let [result (re-find #"^\(ns\s+([A-Z|a-z](?:[a-z|A-Z|0-9|-]|\.[A-Z|a-z])*)\W" code)]
    (-> result second symbol)
    (throw (java.text.ParseException. (str "Failed to parse clojure namespace in code:\n\n" code "\n\n") 0))))

(defmethod code-only "clojure"
  [{:keys [:setup :code]}]
  (when (not (empty? setup)) (load-string setup))
  (load-string code))

(defmethod full-project "clojure"
  [{:keys [:setup :code :fixture]}]
  (when (not (empty? setup)) (load-string setup))
  (load-string code)
  (load-string fixture)
  (codewars.clojure.test/run-tests (name-space fixture)))
