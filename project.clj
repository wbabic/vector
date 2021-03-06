(defproject vector "0.1.0-SNAPSHOT"
  :description "exploration of mathematics in clojure"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/core.match "0.3.0-alpha4" :exclusions [org.clojure/tools.analyzer.jvm]]
                 [thi.ng/geom "0.0.908" :exclusions [org.clojure/tools.analyzer.jvm
                                                     org.clojure/clojurescript]]
                 [net.mikera/core.matrix "0.33.2" :exclusions [org.clojure/clojure]]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [com.gfredericks/exact "0.1.8" :exclusions [org.clojure/clojurescript]]
                 [aysylu/loom "0.5.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [funcool/cats "1.2.1"]])
