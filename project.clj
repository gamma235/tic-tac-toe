(defproject tic-tac-toe "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [expectations "2.0.4"]
                 [lein-autoexpect "1.2.2"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :main tic-tac-toe.core

  :cljsbuild {
    :builds [{:id "tic-tac-toe"
              :source-paths ["src"]
              :compiler {
                :output-to "tic_tac_toe.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
