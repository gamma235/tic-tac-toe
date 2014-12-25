(ns tic-tac-toe.record
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer :all]))

;;;; Strategy protocol implmementation ;;;;
(defrecord StrategyImpl []

  Strategy
  (win [this computer human board]
       (println "winning")
       (third computer board))

  (block [this computer human board]
         (println "blocking")
         (third human board))

  (fork [this computer human board]
        (println "forking")
        (first (fork-seq computer computer human board)))

  (blockFork [this computer human board]
             (println "blocking your fork")
             (first (fork-seq human computer human board)))

  (takeCenter [this computer human board]
              (println "taking center")
              middle)

  (takeOppositeCorner [this computer human board]
                      (println "taking opposite corner")
                      (let [my-corners :- (Set (Option Kw)), (s/intersection corners computer)
                            candidate-corner :- (Option Kw), (first (map (fn [corner :- (Option Kw)] :- (Option Kw)
                                                                           (if-not (board corner)
                                                                             (complimentary-corners corner))) my-corners))]
                        candidate-corner))

  (takeCorner [this computer human board]
              (println "taking corner")
              (let [candidate-corner :- Kw, (first (map (fn [corner :- Kw] :- (Option Kw)
                                                          (if-not (board corner)
                                                            corner))  (available-corners board)))

                    next-candidate-corner :- Kw, (second (map (fn [corner :- Kw] :- (Option Kw)
                                                                (if-not (board corner)
                                                                  corner))  (available-corners board)))

                    opposite-corner :- Kw, (candidate-corner complimentary-corners)
                    corner-good? :- [-> Bool], (fn [] (if (or (not next-candidate-corner)
                                                              (not (board opposite-corner)))
                                                        true false))]
                (if (corner-good?)
                  (do (println "corner is good, taking it") candidate-corner)
                  (do (println "corner is whack, taking next" "\n" next-candidate-corner) next-candidate-corner))))

  (takeSide [this computer human board]
            (println "taking side")
            (let [my-sides :- (Set (Option Kw)), (s/intersection sides computer)
                  candidate-side :- (Option Kw), (first (map (fn [side :- (Option Kw)] :- (Option Kw)
                                                               (if-not (board side) side))  (available-sides board)))]
              candidate-side))

  ;; validators
  (canWin [this computer human board]
          (println "comp " computer " human " human)
          (if (and (has-two? computer) (third computer board) (not (board (third computer board))))
            win))

  (canBlock [this computer human board]
            (if (and (has-two? human) (third human board) (not (board (third human board))))
              block))

  (canFork [this computer human board]
           (if (not (empty? (fork-seq computer computer human board)))
             fork))

  (canBlockFork [this computer human board]
                (if (not (empty? (fork-seq human computer human board)))
                  blockFork))

  (canTakeCenter [this computer human board]
                 (if-not (middle board)
                   takeCenter))

  (canTakeOppositeCorner [this computer human board]
                         (let [complimentary-available? :- Bool,
                               (boolean (first
                                         (map (fn [corner :- Kw] :- (Option Kw)
                                                (if-not (corner board)
                                                  (complimentary-corners corner))) (s/intersection corners computer))))]
                           (if (and complimentary-available?
                                    (first (available-corners board))
                                    (first (candidate-opposite-corners computer))
                                    (not (empty? (candidate-opposite-corners computer))))
                             takeOppositeCorner)))

  (canTakeCorner [this computer human board]
                 (if (and (not (empty? (available-corners board))) (first (available-corners board)))
                   takeCorner))

  (canTakeSide [this computer human board]
               (if (and (not (empty? (available-sides board))) (first (available-sides board)))
                 takeSide)))
