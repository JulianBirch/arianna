(defproject net.colourcoding/arianna "0.2.0"
  :description "YANIH validation library"
  :url "https://github.com/JulianBirch/arianna"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[spyscope "0.1.3"]
                 [org.clojure/core.typed "0.2.13"]
                 [net.colourcoding/poppea "0.1.6"]]
  :core.typed {:check [arianna]}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [org.clojure/tools.namespace "0.2.3"]]
                   :source-paths ["dev"]}
             :clj-1.5.1 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :clj-1.4.0 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :clj-1.3.0 {:dependencies [[org.clojure/clojure "1.3.0"]]}})
