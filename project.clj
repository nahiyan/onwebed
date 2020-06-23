(defproject onwebed-cli "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.2-alpha1"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/tools.cli "1.0.194"]]
  :plugins [[lein-cljsbuild "1.1.8"]]
  :main ^:skip-aot onwebed-cli.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :cljsbuild {:repl-listen-port 9000
              :repl-launch-commands
              {"chrome" ["chrome" "-jsconsole" "http://localhost"]}
              :builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {:main onwebed-cli.core
                                   :output-to "package/index.js"
                                   :target :nodejs
                                   :output-dir "target_dev"
                                   :optimizations :none
                                   :pretty-print false
                                   :foreign-libs [{:file "resources/src"
                                                   :module-type :es6}]
                                  ;  :npm-deps {:react "15.4.2"
                                  ;             :lodash "4.17.4"}
                                   :npm-deps {:left-pad "1.2.0"}
                                   :parallel-build true}}

                       {:id "prod"
                        :source-paths ["src"]
                        :compiler {:main onwebed-cli.core
                                   :output-to "package/index.js"
                                   :target :nodejs
                                   :output-dir "target"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :parallel-build true}}]})
