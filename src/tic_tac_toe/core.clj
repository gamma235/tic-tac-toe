(ns tic-tac-toe.core
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.record :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer :all])
  (:import [tic_tac_toe.record StrategyImpl]))

(defn -main [] :- nil
  "starts a game loop that goes, updates screen andgets user input til somebody wins and finishes"
  (let [strategy :- StrategyImpl, (StrategyImpl.)
        board-full? :- [(Map Kw (Option Str)) -> Bool],
        (fn [game-board :- (Map Kw (Option Str))] :- Bool
          (empty? (filter nil? (vals game-board))))

        winning-strategy :- [Player Player (Map Kw (Option Str)) -> (Option Any)]
        (fn [computer :- Player, human :- Player, game-board :- (Map Kw (Option Str))] :- (Option Any)
          (first (remove nil? [(canWin strategy computer human game-board)
                               (canBlock strategy computer human game-board)
                               (canFork strategy computer human game-board)
                               (canBlockFork strategy computer human game-board)
                               (canTakeCenter strategy computer human game-board)
                               (canTakeOppositeCorner strategy computer human game-board)
                               (canTakeCorner strategy computer human game-board)
                               (canTakeSide strategy computer human game-board)])))

        has3? :- [Player Kw -> Bool]
        (fn [player :- Player, next-move :- Kw] :- Bool
          (let [doable-triples :- (ASeq (Option Key-set))
                (map (fn [a :- Key-set] :- (Option Key-set)
                       (conj a next-move)) (first-twos player))]
            (boolean (first (remove nil? (for [doable-triple :- Key-set, doable-triples
                                               triple :- Key-set, triplets] :- (Option Key-set)
                                           (if (= doable-triple triple) doable-triple)))))))

        print-board :- [(Map Kw (Option Str)) -> nil]
        (fn [game-board :- (Map Kw (Option Str))]
          (doseq [ln :- (ASeq (Vec Any)), (partition 3 (sort game-board))] :- nil
            (println ln)))]

    (println "\nWelcome to Tic Tac Toe \nSadly, the computer always goes first :(")

    (loop [computer :- Player, #{}
           human :- Player #{}
           board :- (Map Kw (Option Str)), {:a1 nil, :a2 nil, :a3 nil,
                                            :b1 nil, :b2 nil, :b3 nil,
                                            :c1 nil, :c2 nil, :c3 nil}]

      (if (board-full? board) (println "Tie game \nThanks for playing!")
        (let [next-comp :- Kw ((winning-strategy computer human board) strategy computer human board)]
          (if (has3? (s/union computer #{next-comp}) next-comp) (do (print-board (merge board {next-comp " o "})) (println "I win!"))
            (if (board-full? (merge board {next-comp " o "})) (println "Tie game \nThanks for playing!")
              (do
                (println "Your Turn")
                (print-board (merge board {next-comp " o "}))
                (let [next-human :- Kw (loop [nexto (keyword (read-line))]
                                         (if (and
                                              (some #{nexto} (keys board))
                                              (not (some #{nexto} (s/union human computer #{next-comp}))))
                                           nexto
                                           (do
                                             (println "Illegal move \nTry again!")
                                             (recur (keyword (read-line))))))]
                  (print-board  (merge board {next-comp " o "} {next-human " x "}))
                  (if (has3? (s/union human #{next-human}) next-human)
                    (println "You win! \nCongratulations!")
                    (do
                      (println "human turn taken successfully")
                      (recur (s/union computer #{next-comp}) (s/union human #{next-human}) (merge board {next-comp " o "} {next-human " x "})))))))))))))
