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
        board-full? :- [-> Bool], (fn [game-board] :- Bool (empty? (filter nil? (vals game-board))))
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
          (not (empty?
                (flatten (for [triple :- Key-set, triplets] :- (ASeq (Option Kw))
                  (remove nil? (some #{(s/intersection triple player)} triplets)))))))

               print-board :- nil,
               (fn [next-move] (doseq [ln :- (Seq (Vec (Option (U Kw Str)))), (partition 3 (sort (merge board next-move)))] :- nil
                                 (println ln)))]

               (println "\nWelcome to Tic Tac Toe \nSadly, the computer always goes first :(")

               (loop [board board
                      computer computer
                      human human]
                 (if (board-full? board) (println "Tie game \nThanks for playing!")
                   (let [next-comp ((winning-strategy board) strategy)]
                     (if (has3? (s/union computer #{next-comp})) (do (print-board (merge board {next-comp " o "})) (println "I win!"))
                       (if (board-full? (merge board {next-comp " o "})) (println "Tie game \nThanks for playing!")
                         (do
                           (println "Your Turn")
                           (print-board (merge board {next-comp " o "}))
                           (let [next-human (keyword (read-line))]
                             (print-board  (merge board {next-comp " o "} {next-human " x "}))
                             (if (has3? (s/union human #{next-human}))
                               (println "You win! \nCongratulations!")
                               (do
                                 (println "human turn taken successfully")
                                 (recur (merge board {next-comp " o "} {next-human " x "}) next-comp next-human))))))))))))
