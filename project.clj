(defproject vector "0.1.0-SNAPSHOT"
  :description "exploration of mathematics in clojure"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/tools.reader "0.8.12"]
                 [org.clojure/tools.analyzer.jvm "0.6.5"]
                 [org.clojure/test.check "0.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [net.mikera/core.matrix "0.33.2" :exclusions [org.clojure/clojure]]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [aysylu/loom "0.5.0"]
                 [expresso "0.2.0"]])
