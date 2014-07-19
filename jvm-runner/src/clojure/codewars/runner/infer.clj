(ns codewars.runner.infer)

(def ^:private language-data
  {"clojure" {:pattern #"^\(ns\s+([A-Z|a-z](?:[a-z|A-Z|0-9|-]|\.[A-Z|a-z])*)\W"
              :suffix "clj"}})

(defn- lookup-language [language]
  (if-let [datum (get language-data language)]
    datum
    (throw
     (IllegalArgumentException.
      (format "Unrecognized language: %s" (pr-str language))))))

(defn class-name
  [language code]
  (let [{:keys [:pattern]} (lookup-language language)]
    (second (re-find pattern code))))

(defn file-name-from-class-name
  [language class-name]
  (let [{:keys [:suffix]} (lookup-language language)]
    (-> class-name
      (clojure.string/replace "." "/")
      (str "." suffix))))

(defn file-name
  "Infer the appropriate file name from code given a language"
  [language code]
  (if-let [name (class-name language code)]
    (file-name-from-class-name language name)
    (throw
     (IllegalArgumentException.
      (format "Could not infer file name for language %s from code:\n%s\n" (pr-str language) code)))))
