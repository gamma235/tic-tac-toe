(ns tests.core-tests
  (:require [tic-tac-toe.core :refer :all]
            [clojure.set :as s]
            [expectations :refer :all]))

(def test-player (ref #{}))

(defn reset-test-player []
  (dosync
   (ref-set test-player #{})))


;; tests that has2? works as it should after a dosync guarantees a "true" return condition
(expect nil
        (let [player human]
          (println player)
          (assert (not (has2? player)))
          (dosync
           (alter human s/union #{:a1 :a2})
           (assert (has2? player)))
          (reset-test-player)))

;; tests that a player cannot fork in the init state
(expect false
        (can-fork? human))

;; tests that player can be tested for opposite corners amongst moves
(expect false
        (has-corners? human))

;; helpers
(defn bools [player coll]
  (for [pair coll] (= pair (s/intersection pair @player))))

(expect nil
        ())

(defn first2 [player]
  (first (filter set? (bools player adjacents))))

(defn has2? [player]
  (some true? (bools player adjacents)))

(defn third [player]
  (let [find-open-compliment (fn [] (first2 player))]))

;; strategies
(defn win []
  (let [computer-third (third computer)]
    (dosync
     (alter board merge {computer-third :x})
     (alter move inc)
     (alter computer s/union computer-third))
    (println "You lose!")))

(defn block []
  (let [human-third (third human)]
    (dosync
     (alter board merge {human-third :x})
     (alter move inc)
     (alter computer s/union human-third))
    (println "That has to suck")))

(defn fork [player]
  (if @player 1 2))

(defn block-fork [])

(defn take-center []
  (dosync
   (alter board merge {middle :x})
   (alter move inc)
   (alter computer s/union middle))
  (println "got the middle!"))

(defn take-opposite-corner [])
(defn take-corner [])
(defn take-side [])


(defn can-fork? [player] (if (< 0 (count (s/intersection @player corners))) true))
(can-fork? human)
