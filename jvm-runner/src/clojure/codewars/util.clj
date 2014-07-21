(ns codewars.util
  (:require [clojure.java.io :as io]))

(def ^:private
  language-data
  {"clojure" {:pattern #"^\(ns\s+([A-Z|a-z](?:[a-z|A-Z|0-9|-]|\.[A-Z|a-z])*)\W"
              :extension "clj"}
   "java" {:pattern #"\bclass\s+([A-Z][a-z|A-Z|0-9|_]*)\W"
           :extension "java"}})

(defn class-name
  "Infer the appropriate class or namespace name from code given a language"
  [language code]
  (let [{:keys [:pattern]} (get language-data language)]
    ;; TODO: Filter comments
    (if-let [name (->> code (re-find pattern) second symbol)]
      name
      (throw
       (IllegalArgumentException.
        (format "Could not infer class or namespace name for language %s from code:\n%s\n" (pr-str language) code))))))

(defn class-name-to-file-name
  "Create a file name for a class or namespace name given a language"
  [language class-name]
  (let [{:keys [:extension]} (get language-data language)]
    (-> class-name
        name
        ;; TODO: not platform independent...
        (clojure.string/replace "." "/")
        (str "." extension)
        io/file)))

(defn write-code!
  "Write code to an appropriate file in a specified directory given a language"
  [language dir code]
  (let [class-name (class-name language code)
        base-name (class-name-to-file-name language class-name)
        file-name (io/file dir base-name)]
    (if (.exists file-name)
      (throw
       (UnsupportedOperationException.
        (format "Could not write to file %s, because that file already exists.  Perhaps it already contains the setup or test fixture code?\ncode:\n%s" (pr-str base-name) code)))
      ;; else
      (do
        (spit file-name code)
        {:file-name file-name
         :class-name class-name}))))
