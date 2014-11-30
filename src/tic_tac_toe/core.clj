(ns tic-tac-toe.core
  (:require [clojure.set :as s]))

;; game state
;; todo: use recursion to generate some of these lists
(def board (ref {:a1 :x, :a2 nil, :a3 nil, :b1 nil, :b2 nil, :b3 nil, :c1 nil, :c2 nil, :c3 :x}))
(def move (ref 0))
(def human (ref #{:a1 :c3}))
(def computer(ref #{}))

;; special positions
;; some are redundant and will either be removed or moved to let bindings in functions that use them
(def corners #{:a1 :a3 :c1 :c3})
(def middle :b2)
(def adjacents [#{:a1 :a2} #{:a1 :b1} #{:a1 :b2} #{:a2 :a3} #{:a2 :b2} #{:a3 :b2} #{:a3 :b3} #{:b1 :b2} #{:b1 :c1} #{:b2 :b3} #{:b2 :c2} #{:b2 :c3} #{:b3 :c3} #{:c1 :c2} #{:c2 :c3}])
(def opposite-corners [#{:a1 :c3} #{:a3 :c1}])
(def same-side-corners [#{:a1 :a3} #{:a1 :c1} #{:a3 :c3} #{:c1 :c3}])
(def side-corner-pairs (concat opposite-corners same-side-corners))
(def complimentary-corners {:a1 :c3, :c3 :a1, :a3 :c1, :c1 :a3})
(def sides #{:a2 :b1 :b3 :c2})
(def opposite-sides [#{:a1 :a3} #{:a1 :c1} #{:a2 :c2} #{:a3 :c3} #{:b1 :b3} #{:c1 :c3}])
(def couples (concat adjacents opposite-corners opposite-sides))
(def thirds (let [adjacent-compliments [:a3 :c1 :c3 :a1 :c2 :c1 :c3 :b3 :a1 :b1 :a2 :a1 :a3 :c3 :c1]
                  adjacent-thirds (interleave adjacents adjacent-compliments)
                  opposite-corners-thirds [#{:a1 :c3} middle, #{:a3 :c1} middle]
                  opposite-sides-compliments [:a2 :b1 :b2 :b3 :b2 :c2]
                  opposite-sides-thirds (interleave opposite-sides opposite-sides-compliments)]
              (apply hash-map (concat adjacent-thirds opposite-corners-thirds opposite-sides-thirds))))

;; helpers
;; some are redundant and will either be removed or moved to let bindings in functions that use them
(defn first-two [player]
  (first (filter #(= (count %) 2)
                 (for [pair couples]
                   (s/intersection pair @player)))))

(defn has-two? [player]
  (boolean (first-two player)))

(defn has-corners? [player]
  (< 1 (count (remove #(= % nil) (for [corner corners]
                                   (@player corner))))))

(defn has-opposite-corners? [player]
  (not (empty? (filter #(= (count %) 2)
                       (for [pair opposite-corners]
                         (s/intersection pair @player))))))

(defn third [player]
  (thirds (first-two player)))

(defn take-turn [player position]
  (dosync
   (alter board merge position)
   (alter move inc)
   (alter player s/union #{(first (keys position))})))

(defn available-corners []
  (for [corner corners]
    (if-not (corner @board) corner)))

(defn candidate-opposite-corners [player]
  (let [my-corners (s/intersection @player corners)
        opposites (for [corner my-corners]
                    (corner complimentary-corners))]
    (for [corner opposites]
      (some #{corner} (available-corners)))))

(defn available-sides []
  (for [side sides]
    (if-not (side @board) side)))


;; strategies
(declare take-corner)

(defn win []
  (take-turn computer {(third computer) :o})
  (println "You lose!"))

(defn block []
  (take-turn computer {(third human) :o})
  (println "Oh snap! Blocked!"))


;; FINISH THIS!!!!
(defn fork []
  (let [my-complimentary-corners (first (filter #(= (count %) 2)
                                                (for [pair opposite-corners]
                                                  (s/intersection pair @player))))
        which-fork? (for [corner my-complimentary-corners]
                      ;; make sure that you go on the side where the edges aren't taken
                      ;; remember that there is also a middle-fork, think about pulling it out as it's own strategy
                      )])
  (take-corner)
  (println "forked!"))

(defn block-fork []
  (take-corner)
  (println "fork totally blocked!"))

(defn take-center []
  (take-turn computer {middle :o})
  (println "got the middle!"))

(defn take-opposite-corner []
  (let [my-corners (s/intersection corners @computer)
        candidate-corner (first (for [corner my-corners]
                                  (if-not (corner @board)
                                    (corner complimentary-corners))))]
    (if candidate-corner
      (take-turn computer {candidate-corner :o}))
    (println "Got the opposite corner")))

(defn take-corner []
  (take-turn computer {(first available-corners) :o})
  (println "Nobody puts baby in the corner, cause that's where my 'o' goes"))

(defn take-side []
  (let [candidate-side (first (for [side sides]
                                (if-not (side @board) side)))]
    (if candidate-side
      (take-turn computer {candidate-side :o})))
  (println "got the side!"))


(defn winning-strategy
  "Returns the function to be called in the subsequent 'go' function"
  []
  (let [;; passing tests return strategy functions themselves or nil if test fails
        can-win?                   (if (and (has-two? computer) (third computer) (not ((third computer) @board))) win)
        can-block?                 (if (and (has-two? human) (third human) (not ((third human) @board))) block)
        can-fork?                  (if (and (available-corners) (has-opposite-corners? computer)) fork)
        can-block-fork?            (if (and (available-corners) (has-opposite-corners? human)) block-fork)
        can-take-center?           (if-not (middle @board) take-center)
        can-take-opposite-corner?  (if-not (empty? (candidate-opposite-corners computer)) take-opposite-corner)
        can-take-corner?           (if-not (empty? (available-corners)) take-corner)
        can-take-side?             (if-not (empty? (available-sides)) take-side)

        ;; now we evaluate all of the tests and return a sequence of the results
        tests [can-win? can-block? can-fork? can-block-fork? can-take-center? can-take-opposite-corner? can-take-corner? can-take-side?]
        doables (pmap eval tests)]
    (first (remove #(= % nil) doables))))

;; This will be the last thing
(defn go
  "starts a game loop that goes, updates screen, gets user input, updates the screen, then goes again, til somebody wins and finishes"
  [])


;;; FOR THE REPL : TO BE DELETED
((winning-strategy))
(deref board)

;; TODO: GUI or browser UI, start screen, midis, refactoring, tighten strategies and tests for optimal behavior and performance
