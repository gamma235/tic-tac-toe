(ns tic-tac-toe.record
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer :all]))

;;;; Strategy protocol implmementation ;;;;
(defrecord ^:no-check StrategyImpl []

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
              (if-not (board middle) middle))

  (takeOppositeCorner [this computer human board]
                      (println "taking opposite corner")
                      (let [my-corners :- (Set (Option Kw)), (s/intersection corners computer)
                            candidate-corner :- (Option Kw), (first (map (fn [corner :- (Option Kw)] :- (Option Kw)
                                                                           (complimentary-corners corner)) my-corners))]
                        candidate-corner))

  (takeCorner [this computer human board]
              (println "taking corner")
              (first (map (fn [corner :- Kw] :- (Option Kw)
                            (if-not (board corner)
                              corner))  (available-corners board))))

  (takeSide [this computer human board]
            (println "taking side")
            (first (map (fn [side :- (Option Kw)] :- (Option Kw)
                          (if-not (board side) side))
                        (available-sides board))))

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
                         (if (and (first (available-corners board))
                                  (first (candidate-opposite-corners computer board)))
                           takeOppositeCorner))

  (canTakeCorner [this computer human board]
                 (if (and (not (empty? (available-corners board))) (first (available-corners board)))
                   takeCorner))

  (canTakeSide [this computer human board]
               (if (and (not (empty? (available-sides board))) (first (available-sides board)))
                 takeSide)))
