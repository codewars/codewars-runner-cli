(ns codewars.runner)

(defmulti run
  (fn [{:keys [:language]}]
    (if (contains? (methods run) language)
      language
      ::not-implemented)))

(defmethod run ::not-implemented
  [{:keys [:language]}]
  (throw (IllegalArgumentException.
          (format "Language %s is not implemented"
                  (pr-str language)))))
