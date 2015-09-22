(defproject optimize "0.0.1"
  :description "Clojure library for optimization"
  :url "http://github.com/patapizza/optimize"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/tools.nrepl "0.2.5"]
                                  [org.clojure/test.check "0.8.0"]]}}
  :dependencies [[org.clojure/math.combinatorics "0.1.1"]]
  :resource-paths ["resources"])
