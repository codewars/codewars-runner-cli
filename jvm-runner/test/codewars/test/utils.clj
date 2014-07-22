(ns codewars.test.utils
  "A small collection of utilities for testing"
  (:import [java.io PrintStream ByteArrayOutputStream StringWriter]))

(defmacro with-java-out-str
  "A version of clojure.core/with-out-str suitable for wrapping java's System.out"
  [& body]
  `(let [original-out# (PrintStream. (. System out))
         byte-out# (ByteArrayOutputStream.)]
     (-> byte-out# (PrintStream.) System/setOut)
     ~@body
     (System/setOut original-out#)
     (.toString byte-out#)))

(defmacro with-out-str-not-threadsafe
  "A version of clojure.core/with-out-str that is not threadsafe"
  [& body]
  `(let [s# (StringWriter.)]
     (with-redefs [*out* s#]
       ~@body
       (str s#))))
