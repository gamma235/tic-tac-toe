(ns tic-tac-toe.core
  (:require [clojure.set :as s]
            [tic-tac-toe.protocols :refer :all]))

;; game state
;; todo: use recursion to generate the board and special positions sequences
(def board (ref {:a1 nil, :a2 nil, :a3 nil, :b1 nil, :b2 nil, :b3 nil, :c1 nil, :c2 nil, :c3 nil}))
(def human (ref #{}))
(def computer(ref #{}))

;; special positions
(def corners #{:a1 :a3 :c1 :c3})
(def middle :b2)
(def adjacents [#{:a1 :a2} #{:a1 :b1} #{:a1 :b2} #{:a2 :a3} #{:a2 :b2} #{:a3 :b2} #{:a3 :b3} #{:b1 :b2} #{:b1 :c1} #{:b2 :b3} #{:b2 :c2} #{:b2 :c3} #{:b3 :c3} #{:c1 :c2} #{:c2 :c3}])
(def opposite-corners [#{:a1 :c3} #{:a3 :c1}])
(def same-side-corners [#{:a1 :a3} #{:a1 :c1} #{:a3 :c3} #{:c1 :c3}])
(def complimentary-corners {:a1 :c3, :c3 :a1, :a3 :c1, :c1 :a3})
(def sides #{:a2 :b1 :b3 :c2})
(def opposite-sides [#{:a1 :a3} #{:a1 :c1} #{:a2 :c2} #{:a3 :c3} #{:b1 :b3} #{:c1 :c3}])
(def triplets [#{:a1 :a2 :a3} #{:b1 :b2 :b3} #{:c1 :c2 :c3} #{:a1 :b1 :c1} #{:a2 :b2 :c2} #{:a3 :b3 :c3} #{:a1 :b2 :c3} #{:a3 :b2 :c1}])

;; helpers
(defn first-two [player]
  (let [couples (concat adjacents opposite-corners opposite-sides)]
    (first (filter #(= (count %) 2)
                   (for [pair couples]
                     (s/intersection pair @player))))))

(defn has-two? [player]
  (boolean (first-two player)))

(defn has-opposite-corners? [player]
  (not (empty? (filter #(= (count %) 2)
                       (for [pair opposite-corners]
                         (s/intersection pair @player))))))

(defn third [player]
  (let [adjacent-compliments [:a3 :c1 :c3 :a1 :c2 :c1 :c3 :b3 :a1 :b1 :a2 :a1 :a3 :c3 :c1]
        adjacent-thirds (interleave adjacents adjacent-compliments)
        opposite-corners-thirds [#{:a1 :c3} middle, #{:a3 :c1} middle]
        opposite-sides-compliments [:a2 :b1 :b2 :b3 :b2 :c2]
        opposite-sides-thirds (interleave opposite-sides opposite-sides-compliments)
        thirds (apply hash-map (concat adjacent-thirds opposite-corners-thirds opposite-sides-thirds))]
    (thirds (first-two player))))

(defn take-turn [player position]
  (dosync
   (alter board merge position)
   (alter player s/union #{(first (keys position))})))

(defn available-corners []
  (remove #(= nil %) (for [corner corners]
                       (if-not (corner @board) corner))))

(defn gets-me-a-corner []
  (let [candidate-corner (first (available-corners))]
    (if (and candidate-corner (not (candidate-corner @board)))
      candidate-corner)))

(defn candidate-opposite-corners [player]
  (let [my-corners (s/intersection @player corners)
        opposites (for [corner my-corners]
                    (corner complimentary-corners))]
    (for [corner opposites]
      (some #{corner} (available-corners)))))

(defn available-sides []
  (remove #(= nil %) (for [side sides]
                       (if-not (side @board) side))))

;;;; strategies ;;;;
(defrecord StrategyImpl []

  Strategy
  (win [_]
       (println "winning")
       (take-turn computer {(third computer) " o "})
       (println "You lose!"))

  (block [_]
         (println "blocking")
         (take-turn computer {(third human) " o "})
         (println "Oh snap! Blocked!"))

  (fork [_]
        (println "forking")
        (let [my-complimentary-corners (first (filter #(= (count %) 2)
                                                      (for [pair opposite-corners]
                                                        (s/intersection pair @computer))))
              ;; which-fork? (for [corner my-complimentary-corners] (if))
              ;; make sure that you go on the side where the edges aren't taken
              ;; remember that there is also a middle-fork, think about pulling it out as it's own strategy
              ])
        (gets-me-a-corner)
        (println "fork?"))

  (block-fork [_]
              (println "blocking your fork")
              (gets-me-a-corner)
              (println "fork totally blocked!"))

  (take-center [_]
               (println "taking center")
               (take-turn computer {middle " o "})
               (println "got the middle!"))

  (take-opposite-corner [_]
                        (println "taking opposite corner")
                        (let [my-corners (s/intersection corners @computer)
                              candidate-corner (first (for [corner my-corners]
                                                        (if-not (corner @board)
                                                          (corner complimentary-corners))))]
                          (take-turn computer {candidate-corner " o "})
                          (println "Got the opposite corner")))

  (take-corner [_]
               (println "taking corner")
               (let [candidate-corner (first (for [corner (available-corners)]
                                               (if-not (corner @board)
                                                 corner)))

                     next-candidate-corner (second (for [corner (available-corners)]
                                                     (if-not (corner @board)
                                                       corner)))

                     opposite-corner (candidate-corner complimentary-corners)

                     corner-good? (fn [] (if (or (not next-candidate-corner) (not (opposite-corner @board))) true false))]
                 (if (corner-good?)
                   (do (println "corner is good, taking it") (take-turn computer {candidate-corner " o "}))
                   (do (println "corner is whack, taking next" "\n" next-candidate-corner) (take-turn computer {next-candidate-corner " o "})))
                 (println "Nobody puts baby in the corner, cause that's where my 'o' goes")))

  (take-side [_]
             (println "taking side")
             (let [my-sides (s/intersection sides @computer)
                   candidate-side (first (for [side (available-sides)]
                                           (if-not (side @board) side)))]
               (take-turn computer {candidate-side " o "}))
             (println "got the side!"))

  ;; validation tests
  (can-win? [_]
            (if (and (has-two? computer) (third computer) (not ((third computer) @board)))
              win))

  (can-block? [_]
              (if (and (has-two? human) (third human) (not ((third human) @board)))
                block))

  (can-fork? [_]
             (if (and (available-corners) (has-opposite-corners? computer))
               fork))

  (can-block-fork? [_]
                   (if (and (first (available-corners)) (has-opposite-corners? human))
                     block-fork))

  (can-take-center? [_]
                    (if-not (middle @board) take-center))

  (can-take-opposite-corner? [_]
                             (let [complimentary-available? (boolean (first (for [corner (s/intersection corners @computer)]
                                                                              (if-not (corner @board)
                                                                                (corner complimentary-corners)))))]
                               (if (and complimentary-available?
                                        (first (available-corners))
                                        (first (candidate-opposite-corners computer))
                                        (not (empty? (candidate-opposite-corners computer))))
                                 take-opposite-corner)))

  (can-take-corner? [_]
                    (if (and (not (empty? (available-corners))) (first (available-corners))) take-corner))

  (can-take-side? [_]
                  (if (and (not (empty? (available-sides))) (first (available-sides))) take-side)))
;;;;;;;;;;;;;;;;;;;;

;; This will be the last thing
(defn -main []
  "starts a game loop that goes, updates screen, gets user input, updates the screen, then goes again, til somebody wins and finishes"
  (let [strategy          (StrategyImpl.)
        board-full?       (fn [] (empty? (filter #(= nil %) (vals @board))))
        winning-strategy  (fn [] (first (remove #(= % nil) [(can-win? strategy)
                                                            (can-block? strategy)
                                                            (can-fork? strategy)
                                                            (can-block-fork? strategy)
                                                            (can-take-center? strategy)
                                                            (can-take-opposite-corner? strategy)
                                                            (can-take-corner? strategy)
                                                            (can-take-side? strategy)])))
        has3?             (fn [player]
                            (not (empty? (filter #(= (count %) 3)
                                                 (for [triple triplets]
                                                   (s/intersection triple @player))))))
        print-board       (fn [] (doseq [ln (partition 3 (sort @board))] (println ln)))]

    (println "\nWelcome to Tic Tac Toe \nSadly, the computer always goes first :(")

    (loop [turn 1]
      (if (board-full?) (println "Tie game \nThanks for playing!")
        (do
          ((winning-strategy) strategy)
          (if (has3? computer) (println "I win! \nYou Lose!")
            (if (board-full?) (println "Tie game \nThanks for playing!")
              (do
                (println "Your Turn")
                (print-board)
                (take-turn human {(keyword (read-line)) " x "})
                (if (has3? human) (println "You win! \nCongratulations!")
                  (if (board-full?) (println "Tie game \nThanks for playing!")
                    (do
                      (println "human turn taken successfully")
                      (recur (inc turn)))))))))))))
