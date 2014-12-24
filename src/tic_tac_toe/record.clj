(ns tic-tac-toe.record
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.helpers :refer :all]
            [clojure.core.typed :refer :all]))

;;;; Strategy protocol implmementation ;;;;
(defrecord StrategyImpl []

  Strategy
  (win [this]
       (println "winning")
       (third computer))

  (block [this]
         (println "blocking")
         (third human))

  (fork [this]
        (println "forking")
        (first (fork-seq computer)))

  (block-fork [this]
              (println "blocking your fork")
              (first (fork-seq human)))

  (take-center [this]
               (println "taking center")
               middle)

  (take-opposite-corner [this]
                        (println "taking opposite corner")
                        (let [my-corners :- (Set (Option Kw)), (s/intersection corners @computer)
                              candidate-corner :- (Option Kw), (first (map (fn [corner :- (Option Kw)] :- (Option Kw)
                                                                             (if-not (board corner)
                                                                               (complimentary-corners corner))) my-corners))]
                          candidate-corner))

  (take-corner [this]
               (println "taking corner")
               (let [candidate-corner :- Kw, (first (map (fn [corner :- Kw] :- (Option Kw)
                                                           (if-not (board corner)
                                                             corner))  (available-corners)))

                     next-candidate-corner :- Kw, (second (map (fn [corner :- Kw] :- (Option Kw)
                                                                 (if-not (board corner)
                                                                   corner))  (available-corners)))

                     opposite-corner :- Kw, (candidate-corner complimentary-corners)
                     corner-good? :- [-> Bool], (fn [] (if (or (not next-candidate-corner)
                                                               (not (board opposite-corner)))
                                                         true false))]
                 (if (corner-good?)
                   (do (println "corner is good, taking it") (take-turn computer #{candidate-corner}))
                   (do (println "corner is whack, taking next" "\n" next-candidate-corner) next-candidate-corner))))

  (take-side [this]
             (println "taking side")
             (let [my-sides :- (Set (Option Kw)), (s/intersection sides computer)
                   candidate-side :- (Option Kw), (first (map (fn [side :- (Option Kw)] :- (Option Kw)
                                                                (if-not (board side) side))  (available-corners)))]
               candidate-side))

  ;; validators
  (can-win? [this]
            (if (and (has-two? computer) (third computer) (not (board (third computer))))
              win))

  (can-block? [this]
              (if (and (has-two? human) (third human) (not (board (third human))))
                block))

  (can-fork? [this]
             (if (not (empty? (fork-seq computer)))
               fork))

  (can-block-fork? [this]
                   (if (not (empty? (fork-seq human)))
                     block-fork))

  (can-take-center? [this board]
                    (if-not (middle board)
                      take-center))

  (can-take-opposite-corner? [this]
                             (let [complimentary-available? :- Bool,
                                   (boolean (first
                                             (map (fn [corner :- Kw] :- (Option Kw)
                                                    (if-not (corner board)
                                                      (complimentary-corners corner))) (s/intersection corners computer))))]
                               (if (and complimentary-available?
                                        (first (available-corners))
                                        (first (candidate-opposite-corners computer))
                                        (not (empty? (candidate-opposite-corners computer))))
                                 take-opposite-corner)))

  (can-take-corner? [this]
                    (if (and (not (empty? (available-corners))) (first (available-corners)))
                      take-corner))

  (can-take-side? [this]
                  (if (and (not (empty? (available-sides))) (first (available-sides)))
                    take-side)))
