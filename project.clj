(defproject tic-tac-toe "0.1.0-SNAPSHOT"
  :description "Stateless command line Tic Tac Toe game"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                 [org.clojure/core.typed "0.2.72"]
                 [expectations "2.0.9"]]

  :plugins [[lein-expectations "0.0.7"]]
  :source-paths ["src"]
  :main tic-tac-toe.core)
