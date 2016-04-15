(ns codewars.utils
  "A small collection of utilities for testing"
  (:import [java.io PrintStream ByteArrayOutputStream StringWriter]))

(defmacro with-java-out-str
  "A version of clojure.core/with-out-str suitable for wrapping java's System.out"
  [& body]
  `(let [original-out# (PrintStream. System/out)
         byte-out# (ByteArrayOutputStream.)]
     (-> byte-out# (PrintStream.) System/setOut)
     ~@body
     (System/setOut original-out#)
     (.toString byte-out#)))

(defmacro with-out-str-not-thread-safe
  "A version of clojure.core/with-out-str that is not thread-safe"
  [& body]
  `(let [s# (StringWriter.)]
     (with-redefs [*out* s#]
       ~@body
       (str s#))))

(defmacro with-all-out-str
  "A version of clojure.core/with-out-str suitable for wrapping java's System.out"
  [& body]
  `(let [original-out# (PrintStream. System/out)
         byte-out# (ByteArrayOutputStream.)
         s# (StringWriter.)]
     (binding [*out* s#]
       (-> byte-out# (PrintStream.) System/setOut)
       ~@body
       (System/setOut original-out#)
       (str byte-out# s#))))

(defmacro with-err-str
  "A version of clojure.core/with-out-str for *err*"
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))
