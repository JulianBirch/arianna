(defproject net.colourcoding/arianna "0.2.0"
  :description "YANIH validation library"
  :url "https://github.com/JulianBirch/arianna"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[spyscope "0.1.3"]
                 [net.colourcoding/poppea "0.2.0"]
                 [potemkin "0.3.3"]
                 [stencil "0.3.2"]]
  :core.typed {:check [arianna]}
  :codox {:include [arianna]
          :writer codox-md.writer/write-docs
          :src-dir-uri "http://github.com/JulianBirch/arianna/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [codox-md "0.2.0"]]
                   :source-paths ["dev"]}
             :clj-1.5.1 {:dependencies [[org.clojure/clojure "1.5.1"]]}})
