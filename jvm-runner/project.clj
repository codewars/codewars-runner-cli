(defproject jvm-runner "0.1.3"
  :description "JVM Runner for codewars"
  :url "http://www.codewars.com/"
  :javac-target "1.8"
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [cheshire "5.3.1"] ; JSON
   [junit/junit "4.11"] ; JUnit
   [org.codehaus.groovy/groovy-all "2.3.6"] ; Groovy
   [environ "0.5.0"] ; Environment Variables
   ]
  :plugins [[lein-environ "0.5.0"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :env {:timeout "10000"
        :zmq-socket "ipc:///tmp/codewars"}
  :main codewars.core)
