(ns codewars.runners)

(defmulti solution-only :language)
(defmulti full-project :language)
(defmethod solution-only :default
  [{:keys [:language]}]
  (throw (IllegalArgumentException.
          (format "Language %s is not implemented"
                  (pr-str language)))))

(defmethod full-project :default
  [{:keys [:language]}]
  (throw (IllegalArgumentException.
          (format "Language %s is not implemented"
                  (pr-str language)))))

(defn run
  "Run solution code or a test-fixture."
  [opts]
  (if (nil? (:fixture opts))
    (solution-only opts)
    (full-project opts)))
