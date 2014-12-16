(ns tic-tac-toe.core
  (:require [clojure.set :as s]
            [tic-tac-toe.protocol :refer :all]
            [clojure.core.typed :refer :all]))

;; aliases for core.typed signatures
(defalias Key-set (Set (Option Kw)))
(defalias Player (Ref1 Key-set))
(defalias Set-vec (Vec (Set Kw)))
(defalias Move (U ':x ':o))

;; unannotated clojure.core functions
(ann clojure.core/hash-map [Key-set Kw -> Any])
(ann clojure.core/apply [[Key-set Kw -> Any] (Seq (U Key-set Kw)) -> (Map Key-set Kw)])

;; WON'T TYPE CHECK: players Don't pass type checker due to refs
(clojure.core.typed/def ^:no-check human :- Player (ref #{}))
(clojure.core.typed/def ^:no-check computer :- Player (ref #{}))

;; game state
;; todo: find a way to avoid hard coding (recursion?)
(clojure.core.typed/def board :- (Ref1 (HMap :mandatory {:a1 (Option Str), :a2 (Option Str), :a3 (Option Str), :b1 (Option Str), :b2 (Option Str), :b3 (Option Str), :c1 (Option Str), :c2 (Option Str), :c3 (Option Str)})),
  (ref {:a1 nil, :a2 nil, :a3 nil, :b1 nil, :b2 nil, :b3 nil, :c1 nil, :c2 nil, :c3 nil}))

(clojure.core.typed/def corners :- (Set Kw), #{:a1 :a3 :c1 :c3})
(clojure.core.typed/def middle :- Kw, :b2)
(clojure.core.typed/def adjacents :- Set-vec, [#{:a1 :a2} #{:a1 :b1} #{:a1 :b2} #{:a2 :a3} #{:a2 :b2} #{:a3 :b2} #{:a3 :b3} #{:b1 :b2} #{:b1 :c1} #{:b2 :b3} #{:b2 :c2} #{:b2 :c3} #{:b3 :c3} #{:c1 :c2} #{:c2 :c3}])
(clojure.core.typed/def opposite-corners :- Set-vec, [#{:a1 :c3} #{:a3 :c1}])
(clojure.core.typed/def same-side-corners :- Set-vec, [#{:a1 :a3} #{:a1 :c1} #{:a3 :c3} #{:c1 :c3}])
(clojure.core.typed/def complimentary-corners :- (Map Kw Kw), {:a1 :c3, :c3 :a1, :a3 :c1, :c1 :a3})
(clojure.core.typed/def sides :- (Set Kw), #{:a2 :b1 :b3 :c2})
(clojure.core.typed/def opposite-sides :- Set-vec, [#{:a1 :a3} #{:a1 :c1} #{:a2 :c2} #{:a3 :c3} #{:b1 :b3} #{:c1 :c3}])
(clojure.core.typed/def triplets :- Set-vec, [#{:a1 :a2 :a3} #{:b1 :b2 :b3} #{:c1 :c2 :c3} #{:a1 :b1 :c1} #{:a2 :b2 :c2} #{:a3 :b3 :c3} #{:a1 :b2 :c3} #{:a3 :b2 :c1}])

;; helpers
(defn first-twos [player :- Player] :- (ASeq (Option Key-set))
  (let [couples :- (Seqable Key-set), (concat adjacents opposite-corners opposite-sides)]
    (filter (fn [a :- (Option Key-set)] (= (count a) 2))
            (for [pair :- Key-set, couples] :- (Option Key-set)
              (s/intersection pair @player)))))

(defn has-two? [player :- Player] :- Bool
  (not (empty? (first-twos player))))


(defn has-opposite-corners? [player :- Player] :- Bool
  (not (empty? (filter (fn [a :- (Option Key-set)] (= (count a) 2))
                       (for [pair :- Key-set, opposite-corners] :- (Option Key-set)
                         (s/intersection pair @player))))))

(defn third [player :- Player] :- (Option Kw)
  (let [adjacent-compliments :- (Vec Kw), [:a3 :c1 :c3 :a1 :c2 :c1 :c3 :b3 :a1 :b1 :a2 :a1 :a3 :c3 :c1]
        adjacent-thirds :- (Seq (U Kw Key-set)), (interleave adjacents adjacent-compliments)
        opposite-corners-thirds :- (Vec (U Key-set Kw)), [#{:a1 :c3} middle, #{:a3 :c1} middle]
        opposite-sides-compliments :- (Vec Kw), [:a2 :b1 :b2 :b3 :b2 :c2]
        opposite-sides-thirds :- (Seq (U Kw Key-set)), (interleave opposite-sides opposite-sides-compliments)
        thirds-lookup :- (Map Key-set Kw), (apply hash-map (concat adjacent-thirds opposite-corners-thirds opposite-sides-thirds))
        pairs :- Set-vec, (first-twos player)
        potential-thirds :- (ASeq Kw),  (for [pair :- Key-set, pairs] :- Kw,
                                          (map
                                           (fn [triplet :- Key-set] :- Kw,
                                             (if (and
                                                  (= triplet (conj pair (thirds-lookup pair)))
                                                  (not (@human (thirds-lookup pair))))
                                               (thirds-lookup pair)))
                                             triplets))]
   (first (remove nil? (flatten potential-thirds)))))

;; WON'T TYPE CHECK: is this a core.typed bug dealing with refs?
(defn ^:no-check take-turn [player :- (Ref1 Key-set),  position :- (Map Kw String)] :- Key-set
  (dosync
   (alter board merge position)
   (alter player s/union #{(first (keys position))})))

(defn available-corners [] :- (Seq (Option Kw))
  (remove nil? (for [corner :- Kw, corners] :- (Option Kw)
                 (if-not (@board corner) corner))))


(defn gets-me-a-corner [] :- (Option Kw)
  (let [candidate-corner :- (Option Kw), (first (available-corners))]
    (if (and candidate-corner (not (@board candidate-corner)))
      candidate-corner)))

(defn candidate-opposite-corners [player :- Player] :- (Seq (Option Kw))
  (let [my-corners :- Key-set, (s/intersection @player corners)
        opposites :- (Seq (Option Kw)), (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                          (complimentary-corners corner))]
    (for [corner :- (Option Kw), opposites] :- (Option Kw)
      (some (fn [kw :- (Option Kw)] :- (Option Kw), (#{corner} kw)) (available-corners)))))

(defn available-sides [] :- (Seq (Option Kw))
  (remove nil? (for [side :- Kw, sides] :- (Option Kw)
                 (if-not (@board side) side))))

;;;; WON'T TYPE CHECK: Strategy protocol implmementation ;;;;
(ann-record StrategyImpl [])
(defrecord StrategyImpl []

  Strategy
  (win [_ ]
       (println "winning")
       (take-turn computer {(third computer) " o "})
       (println "You lose!"))

  (block [_ ]
         (println "blocking")
         (take-turn computer {(third human) " o "})
         (println "Oh snap! Blocked!"))

  ;; both fork and block fork will be updated, right now I'm, uhhh ... cutting corners (>_>)
  (fork [_ ]
        (println "forking")
        (let [my-complimentary-corners :- (Set (Option Kw)), (first (filter (fn [a :- Kw] (= (count a) 2))
                                                                            (for [pair :- (Set (Option Kw)), opposite-corners] :- (Set (Option Kw))
                                                                              (s/intersection pair @computer))))
              ;; which-fork? (for [corner my-complimentary-corners] (if...))
              ;; make sure that you go on the side where the edges aren't taken
              ;; remember that there is also a middle-fork, think about pulling it out as it's own strategy
              ])
        (take-turn computer {(gets-me-a-corner) " o "})
        (println "fork?"))

  (block-fork [_ ]
              (println "blocking your fork")
              (take-turn computer {(gets-me-a-corner) " o "})
              (println "fork totally blocked!"))

  (take-center [_ ]
               (println "taking center")
               (take-turn computer {middle " o "})
               (println "got the middle!"))

  (take-opposite-corner [_ ]
                        (println "taking opposite corner")
                        (let [my-corners :- (Set (Option Kw)), (s/intersection corners @computer)
                              candidate-corner :- (Option Kw), (first (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                                                        (if-not (@board corner)
                                                                          (complimentary-corners corner))))]
                          (take-turn computer {candidate-corner " o "})
                          (println "Got the opposite corner")))

  (take-corner [_ ]
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

  (take-side [_ ]
             (println "taking side")
             (let [my-sides :- (Set (Option Kw)), (s/intersection sides @computer)
                   candidate-side :- (Option Kw), (first (for [side :- (Option Kw), (available-sides)] :- (Option Kw)
                                                           (if-not (@board side) side)))]
               (take-turn computer {candidate-side " o "}))
             (println "got the side!"))

  ;; validation tests
  (can-win? [_ ]
            (if (and (has-two? computer) (third computer) (not (@board (third computer))))
              win))

  (can-block? [_ ]
              (if (and (has-two? human) (third human) (not (@board (third human))))
                block))

  (can-fork? [_ ]
             (if (and (available-corners) (has-opposite-corners? computer))
               fork))

  (can-block-fork? [_ ]
                   (if (and (first (available-corners)) (has-opposite-corners? human))
                     block-fork))

  (can-take-center? [_ ]
                    (if-not (middle @board) take-center))

  (can-take-opposite-corner? [_ ]
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

  (can-take-corner? [_ ]
                    (if (and (not (empty? (available-corners))) (first (available-corners))) take-corner))

  (can-take-side? [_ ]
                  (if (and (not (empty? (available-sides))) (first (available-sides))) take-side)))
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(ann -main (Fn [-> nil]))
(defn -main []
  "starts a game loop that goes, updates screen andgets user input til somebody wins and finishes"
  (let [strategy :- StrategyImpl, (StrategyImpl.)
        board-full? :- [-> Bool], (fn [] :- Bool (empty? (filter nil? (vals @board))))
        winning-strategy :- [-> (Option Any)]
        (fn [] :- (Option Any) (first (remove nil? [(can-win? strategy)
                                                    (can-block? strategy)
                                                    (can-fork? strategy)
                                                    (can-block-fork? strategy)
                                                    (can-take-center? strategy)
                                                    (can-take-opposite-corner? strategy)
                                                    (can-take-corner? strategy)
                                                    (can-take-side? strategy)])))

        has3? :- [Player -> Bool],
        (fn [player :- Player] :- Bool
          (not (empty? (filter (fn [a :- Kw] (= (count a) 3))
                               (for [triple :- Key-set, triplets] :- Key-set
                                 (s/intersection triple @player))))))

        print-board :- nil,
        (fn [] (doseq [ln :- (Seq (Vec (Option (U Kw Str)))), (partition 3 (sort @board))] :- nil
                 (println ln)))]

    (println "\nWelcome to Tic Tac Toe \nSadly, the computer always goes first :(")

    (loop []
      (if (board-full?) (println "Tie game \nThanks for playing!")
        (do
          (println (deref computer))
          ((winning-strategy) strategy)
          (if (has3? computer) (do (print-board) (println "I win!"))
            (if (board-full?) (println "Tie game \nThanks for playing!")
              (do
                (println "Your Turn")
                (print-board)
                (take-turn human {(keyword (read-line)) " x "})
                (print-board)
                (if (has3? human) (println "You win! \nCongratulations!")
                  (if (board-full?) (println "Tie game \nThanks for playing!")
                    (do
                      (println "human turn taken successfully")
                      (recur))))))))))))
