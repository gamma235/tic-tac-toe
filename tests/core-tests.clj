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

;; makes sure that the computer will win when given the chance
(let [human #{:a1 :c1 :a2}
      computer #{:b2 :c3 :a3}
      board ])
