(defproject euler-solutions "0.1.0-SNAPSHOT"
  :description "An attempt to learn clojure while solving Euler problems"
  :url "https://projecteuler.net/about"
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :main ^:skip-aot euler-solutions.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
