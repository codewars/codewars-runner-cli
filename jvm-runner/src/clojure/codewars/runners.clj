(ns codewars.runners)

(defmulti code-only :language)
(defmulti full-project :language)
(defmethod code-only :default
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
  "Run code or a test-fixture."
  [opts]
  (if (nil? (:fixture opts))
    (code-only opts)
    (full-project opts)))
