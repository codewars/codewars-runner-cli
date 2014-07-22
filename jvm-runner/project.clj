(defproject jvm-runner "0.1.0-SNAPSHOT"
  :description "JVM Runner for codewars"
  :url "http://www.codewars.com/"
  :javac-target "1.8"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cheshire "5.3.1"]
                 [org.tcrawley/dynapath "0.2.3"]
                 [junit/junit "4.11"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :main codewars.core)
