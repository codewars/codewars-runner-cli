(ns codewars.runners.java
  (:require [codewars.runners :refer [code-only full-project]]
            [codewars.clojure.test])
  (:import
   [javax.tools ToolProvider SimpleJavaFileObject SimpleJavaFileObject ForwardingJavaFileManager]
   [java.net URI]
   [java.io PrintWriter ByteArrayOutputStream]
   [org.junit.runner JUnitCore]
   [codewars.java CwRunListener]))

(defn class-name
  "Determine the class name of some java code (include package)"
  [code]
  (let [package-name (second (re-find #"\bpackage\s+([A-Z|a-z](?:[a-z|A-Z|0-9|_]|\.[A-Z|a-z])*)\W" code))
        class-name (second (re-find #"\bclass\s+([A-Z][a-z|A-Z|0-9|_]*)\W" code))]
    (cond
     (nil? class-name) (throw (java.text.ParseException.
                               (str "Failed to parse java class in code:\n\n" code "\n\n") 0))
     (nil? package-name) class-name
     :else (str package-name "." class-name))))

(def ^:private source-kind javax.tools.JavaFileObject$Kind/SOURCE)
(def ^:private class-kind javax.tools.JavaFileObject$Kind/CLASS)

(defn- class-uri [class-name kind]
  (URI. (str "string:///"
             (.replace class-name "." "/")
             (.extension kind))))

(defn- source-code-file
  "In-memory file for storing source code"
  [code]
  (proxy
      [SimpleJavaFileObject]
      [(class-uri (class-name code) source-kind)
       source-kind]
    (getCharContent [_] code)))

(defn- byte-code-file
  "In-memory file for storing byte-code for class"
  [class-name kind]
  (let [out (ByteArrayOutputStream.)]
    (proxy
        [SimpleJavaFileObject]
        [(class-uri class-name kind) kind]
      (getBytes [] (.toByteArray out))
      (openOutputStream [] out))))

(defn- byte-code-manager
  "File-manager for in-memory class byte-codes"
  [standard-manager]
  (let [class-codes (atom {})
        class-loader
        ;; TODO: Use a lighter ClassLoader than this
        (proxy [clojure.lang.DynamicClassLoader] []
          (findClass [class-name]
            (if-let [byte-data (get @class-codes [class-name class-kind])]
              ;; TODO: Such hacks... :(
              (let [byte-code (.toByteArray (.openOutputStream byte-data))]
                (proxy-super defineClass class-name byte-code nil))
              (throw
               (java.lang.ClassNotFoundException.
                (str "Could not find "
                     [class-name class-kind]
                     " among possibilities: "
                     (keys @class-codes)))))))]
    (proxy
        [ForwardingJavaFileManager]
        [standard-manager]
      (getClassLoader [_] class-loader)
      (getJavaFileForOutput [_ class-name kind _]
        (get (swap! class-codes assoc [class-name kind] (byte-code-file class-name kind))
             [class-name kind])))))

(def ^:private compiler (ToolProvider/getSystemJavaCompiler))

(def ^:private file-manager (byte-code-manager (.getStandardFileManager compiler nil nil nil)))

(defn compile-and-load
  [class-name & codes]
  (let [files (map source-code-file codes)]
    (with-open [err-stream (ByteArrayOutputStream.)]
      (if(-> compiler (.getTask (PrintWriter. err-stream) file-manager nil nil nil files) .call)
        (-> file-manager (.getClassLoader nil) (.loadClass class-name))
        (throw (RuntimeException. (str err-stream)))))))

(defmethod code-only "java"
  [{:keys [:setup :code]}]
  (let [code-class-name (class-name code)]
    (-> (if (nil? setup)
          (compile-and-load code-class-name code)
          (compile-and-load code-class-name code setup))
        (.getDeclaredMethod "main" (into-array [(Class/forName "[Ljava.lang.String;")]))
        (doto (.setAccessible true))
        (.invoke nil (into-array [(into-array String [])])))))

(defn- run-junit-tests
  "Run a JUnit test using the Codewars Formatter for a given fixture-class"
  [fixture-class]
  (let [runner (JUnitCore.)]
    (.addListener runner (CwRunListener.))
    (codewars.clojure.test/time
     (.run runner (into-array [fixture-class])))))

(defmethod full-project "java"
  [{:keys [:fixture :setup :code]}]
  (let [fixture-class-name (class-name fixture)]
    (run-junit-tests
     (if (nil? setup)
       (compile-and-load fixture-class-name code fixture)
       (compile-and-load fixture-class-name code fixture setup)))))
