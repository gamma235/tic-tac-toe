(ns tic-tac-toe.core
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.record :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer :all])
  (:import [tic_tac_toe.record StrategyImpl]))

(defn -main []
  "starts a game loop that goes, updates screen andgets user input til somebody wins and finishes"
  (let [strategy :- StrategyImpl, (StrategyImpl.)
        board-full? :- [-> Bool], (fn [] :- Bool (empty? (filter nil? (vals @board))))
        winning-strategy :- [-> (Option Any)]
        (fn [] :- (Option Any) (first (remove nil? [(can-win? strategy)
                                                    (can-block? strategy)
                                                    (can-fork? strategy)
                                                    (can-block-fork? strategy)
                                                    (can-take-center? strategy)
                                                    (can-take-opposite-corner? strategy)
                                                    (can-take-corner? strategy)
                                                    (can-take-side? strategy)])))

        has3? :- [Player -> Bool],
        (fn [player :- Player] :- Bool
          (not (empty? (filter (fn [a :- Kw] (= (count a) 3))
                               (for [triple :- Key-set, triplets] :- Key-set
                                 (s/intersection triple @player))))))

        print-board :- nil,
        (fn [] (doseq [ln :- (Seq (Vec (Option (U Kw Str)))), (partition 3 (sort @board))] :- nil
                 (println ln)))]

    (println "\nWelcome to Tic Tac Toe \nSadly, the computer always goes first :(")

    (loop []
      (if (board-full?) (println "Tie game \nThanks for playing!")
        (do
          ((winning-strategy) strategy)
          (if (has3? computer) (do (print-board) (println "I win!"))
            (if (board-full?) (println "Tie game \nThanks for playing!")
              (do
                (println "Your Turn")
                (print-board)
                (take-turn human {(keyword (read-line)) " x "})
                (print-board)
                (if (has3? human) (println "You win! \nCongratulations!")
                  (if (board-full?) (println "Tie game \nThanks for playing!")
                    (do
                      (println "human turn taken successfully")
                      (recur))))))))))))
