(ns tic-tac-toe.core
  (:require [clojure.set :as s]))

;; game state
(defonce board (ref {:a1 nil, :a2 nil, :a3 nil, :b1 nil, :b2 nil, :b3 nil, :c1 nil, :c2 nil, :c3 nil}))
(defonce move (ref 0))
(defonce human (ref #{}))
(defonce computer(ref #{}))

;; special positions
(def corners #{:a1 :a3 :c1 :c3})
(def middle :b2)
(def adjacents [#{:a1 :a2} #{:a1 :b1} #{:a1 :b2} #{:a2 :a3} #{:a2 :b1} #{:a2 :b2} #{:a2 :b3} #{:a3 :b2}#{:a3 :b3} #{:b1 :b2}
                #{:b1 :c1} #{:b1 :c2} #{:b2 :b3} #{:b2 :c2} #{:b2 :c3} #{:b3 :c2} #{:b3 :c3} #{:c1 :c2} #{:c2 :c3}])

;; helpers
(defn bools [player]
  (for [pair adjacents] (= pair (s/intersection pair @player))))

(defn first2 [player]
  (first (filter set? (bools player))))

(defn has2? [player]
  (some true? (bools player)))

(defn third [player]
  (let [find-open-compliment (fn [] (first2 player))]))

;; strategies
;; at some point try abstract out the dosync into a general function to avoid repetition
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

(defn fork [])
(defn block-fork [])

(defn take-center []
  (dosync
   (alter board merge {middle :x})
   (alter move inc)
   (alter computer x/union middle))
  (println "got the middle!"))

(defn take-opposite-corner)
(defn take-corner [])
(defn take-side [])

;; Hinky. Try using conds as a seq of bools and then just doing the first true
(defn go []
  (cond
   ;; these two are pretty solid
   (has2? computer) (win)
   (has2? human) (block)

   ;; not sure if both adjacent spaces need to be free or not
   (and (has-opposite-corners? computer) (corner-available?) (adjacent-spaces? free-corner)) (fork)

   ;; this has to take care of the "get 2 in a row to block human fork" and "take corner to block fork" scenarios
   (and (has-opposite-corners? human) (corner-available?) (adjacent-spaces? free-corner)) (block-fork)

   ;; to be continued
   () (take-center)
   () (take-corner)
   () (take-side)))

