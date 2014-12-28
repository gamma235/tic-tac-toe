(ns tic-tac-toe.core
  (:refer-clojure :exclude [defrecord defprotocol let defn fn for loop defprotocol])
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.record :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer [U Set Vec Bool Kw ASeq Seq Map Str Option
                                        let defn fn for defalias loop defprotocol] :as t])
  (:import [tic_tac_toe.record StrategyImpl]))


(defn board-full?
  "tells if a board is full or not"
  [game-board :- (Map Kw (Option Str))] :- Bool
  (empty? (filter nil? (vals game-board))))


(defn winning-strategy
  "returns the best strategy for a given board"
  [strategy :- StrategyImpl, computer :- Key-set, human :- Key-set, game-board :- (Map Kw (Option Str))] :- Any
  (first (remove nil? [(canWin strategy computer human game-board)
                       (canBlock strategy computer human game-board)
                       (canFork strategy computer human game-board)
                       (canBlockFork strategy computer human game-board)
                       (canTakeCenter strategy computer human game-board)
                       (canTakeOppositeCorner strategy computer human game-board)
                       (canTakeCorner strategy computer human game-board)
                       (canTakeSide strategy computer human game-board)])))

(defn has3?
  "returns true if the player has three in a row"
  [player :- Key-set, next-move :- Kw] :- Bool
  (let [doable-triples :- (ASeq (Option Key-set))
        (map (fn [a :- Key-set] :- (Option Key-set)
               (conj a next-move)) (first-twos player))]
    (boolean (first (remove nil? (for [doable-triple :- Key-set, doable-triples
                                       triple :- Key-set, triplets] :- (Option Key-set)
                                   (if (= doable-triple triple)
                                     doable-triple)))))))

(defn print-board
  "prints the game board"
  [{:keys [a1 a2 a3, b1 b2 b3, c1 c2 c3]} :- (Map Kw (Option Str))] :- nil
  (let [a1 (if (string? a1) a1 " a1")
        a2 (if (string? a2) a2 " a2")
        a3 (if (string? a3) a3 " a3")
        b1 (if (string? b1) b1 " b1")
        b2 (if (string? b2) b2 " b2")
        b3 (if (string? b3) b3 " b3")
        c1 (if (string? c1) c1 " c1")
        c2 (if (string? c2) c2 " c2")
        c3 (if (string? c3) c3 " c3")]
    (println  a1 "|" a2"|" a3)
    (println "____|_____|____")
    (println "    |     |    ")
    (println  b1 "|" b2"|" b3)
    (println "____|_____|____")
    (println "    |     |    ")
    (println  c1 "|" c2"|" c3)))

(defn -main [] :- nil
  "starts a game loop that runs, updates the screen and gets user input until somebody wins and finishes"
  (let [strategy :- StrategyImpl, (StrategyImpl.)]
    (println "\nWelcome to Tic Tac Toe \n\nSadly, the computer always goes first (T_T)")
    (loop [computer :- Key-set, #{}
           human :- Key-set #{}
           board :- (Map Kw (Option Str)), {:a1 nil :a2 nil :a3 nil, :b1 nil :b2 nil :b3 nil, :c1 nil :c2 nil :c3 nil}]
      (if (board-full? board)
        (println "Tie game \nThanks for playing!")
        (let [next-comp :- Kw ((winning-strategy strategy computer human board) strategy computer human board)]
          (if (has3? (s/union computer #{next-comp}) next-comp)
            (print-board (merge board {next-comp " o "}))
            (if (board-full? (merge board {next-comp " o "}))
              (println "Tie game! Thanks for playing n(_ _)n")
              (do
                (println "Your Turn\n")
                (print-board (merge board {next-comp " o "}))
                (println "\ntype position and hit enter:")
                (let [next-human :- Kw (loop [nexto (keyword (read-line))]
                                         (if (and
                                              (some #{nexto} (keys board))
                                              (not (some #{nexto} (s/union human computer #{next-comp}))))
                                           nexto
                                           (do
                                             (println "\nIllegal move. Try again:")
                                             (recur (keyword (read-line))))))]
                      (println "\nhuman turn taken successfully")
                      (recur (s/union computer #{next-comp}) (s/union human #{next-human}) (merge board {next-comp " o "} {next-human " x "})))))))))))
