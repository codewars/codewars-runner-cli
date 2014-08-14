(defproject jvm-runner "0.1.1"
  :description "JVM Runner for codewars"
  :url "http://www.codewars.com/"
  :javac-target "1.8"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cheshire "5.3.1"]
                 [junit/junit "4.11"]
                 [org.codehaus.groovy/groovy-all "2.3.6"]
                 [environ "0.5.0"]]
  :plugins [[lein-environ "0.5.0"]]
  :source-paths      ["src/clojure"]
  :java-source-paths ["src/java"]
  :env {:timeout "2000"}
  :profiles {:uberjar {:aot :all
                       :omit-source true
                       :javac-options ["-g:none"]}}
  :main codewars.core)
