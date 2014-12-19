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
       (take-turn computer {(third computer) " o "})
       (println "You lose!"))

  (block [this]
         (println "blocking")
         (take-turn computer {(third human) " o "})
         (println "Oh snap! Blocked!"))

  (fork [this]
        (println "forking")
        (take-turn computer {(first (fork-seq computer)) " o "})
        (println "fork?"))

  (block-fork [this]
              (println "blocking your fork")
              (take-turn computer {(first (fork-seq human)) " o "})
              (println "fork totally blocked!"))

  (take-center [this]
               (println "taking center")
               (take-turn computer {middle " o "})
               (println "got the middle!"))

  (take-opposite-corner [this]
                        (println "taking opposite corner")
                        (let [my-corners :- (Set (Option Kw)), (s/intersection corners @computer)
                              candidate-corner :- (Option Kw), (first (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                                                        (if-not (@board corner)
                                                                          (complimentary-corners corner))))]
                          (take-turn computer {candidate-corner " o "})
                          (println "Got the opposite corner")))

  (take-corner [this]
               (println "taking corner")
               (let [candidate-corner :- Kw, (first (for [corner :- Kw, (available-corners)] :- (Option Kw)
                                                      (if-not (@board corner)
                                                        corner)))

                     next-candidate-corner :- Kw, (second (for [corner :- Kw, (available-corners)] :- (Option Kw)
                                                            (if-not (@board corner)
                                                              corner)))

                     opposite-corner :- Kw, (candidate-corner complimentary-corners)
                     corner-good? :- [-> Bool], (fn []
                                                  (if (or (not next-candidate-corner) (not (@board opposite-corner)))
                                                    true false))]
                 (if (corner-good?)
                   (do (println "corner is good, taking it") (take-turn computer {candidate-corner " o "}))
                   (do (println "corner is whack, taking next" "\n" next-candidate-corner) (take-turn computer {next-candidate-corner " o "})))
                 (println "Nobody puts baby in the corner, cause that's where my 'o' goes")))

  (take-side [this]
             (println "taking side")
             (let [my-sides :- (Set (Option Kw)), (s/intersection sides @computer)
                   candidate-side :- (Option Kw), (first (for [side :- (Option Kw), (available-sides)] :- (Option Kw)
                                                           (if-not (@board side) side)))]
               (take-turn computer {candidate-side " o "}))
             (println "got the side!"))

  ;; validators
  (can-win? [this]
            (if (and (has-two? computer) (third computer) (not (@board (third computer))))
              win))

  (can-block? [this]
              (if (and (has-two? human) (third human) (not (@board (third human))))
                block))

  (can-fork? [this]
             (if (not (empty? (fork-seq computer)))
               fork))

  (can-block-fork? [this]
                   (if (not (empty? (fork-seq human)))
                     block-fork))

  (can-take-center? [this]
                    (if-not (middle @board)
                      take-center))

  (can-take-opposite-corner? [this]
                             (let [complimentary-available? :- Bool,
                                   (boolean (first
                                             (for [corner :- Kw, (s/intersection corners @computer)] :- (Option Kw)
                                               (if-not (corner @board)
                                                 (corner complimentary-corners)))))]
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
