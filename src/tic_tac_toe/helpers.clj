(ns tic-tac-toe.helpers
  (:require [clojure.set :as s]
            [clojure.core.typed :refer :all]))

;; aliases for core.typed signatures
(defalias Key-set (Set (Option Kw)))
(defalias Player (Ref1 Key-set))
(defalias Set-vec (Vec (Set Kw)))
(defalias Move (U ':x ':o))

;; unannotated clojure.core functions
(ann clojure.core/hash-map [Key-set Kw -> Any])
(ann clojure.core/apply [[Key-set Kw -> Any] (Seq (U Key-set Kw)) -> (Map Key-set Kw)])

;; GAME STATE
;; :no-check due to core.typed's problem with refs
(clojure.core.typed/def ^:no-check human :- Player (ref #{}))
(clojure.core.typed/def ^:no-check computer :- Player (ref #{}))

;; board
(clojure.core.typed/def board :- (Ref1 (HMap :mandatory {:a1 (Option Str), :a2 (Option Str), :a3 (Option Str), :b1 (Option Str), :b2 (Option Str), :b3 (Option Str), :c1 (Option Str), :c2 (Option Str), :c3 (Option Str)})),
  (ref {:a1 nil, :a2 nil, :a3 nil, :b1 nil, :b2 nil, :b3 nil, :c1 nil, :c2 nil, :c3 nil}))

;; special positions
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

;; won't check due to core.typed bug dealing with refs
(defn ^:no-check take-turn [player :- (Ref1 Key-set),  position :- (Map Kw String)] :- Key-set
  (dosync
   (alter board merge position)
   (alter player s/union #{(first (keys position))})))

(defn available-corners [] :- (Seq (Option Kw))
  (remove nil? (for [corner :- Kw, corners] :- (Option Kw)
                 (if-not (@board corner) corner))))

(defn candidate-opposite-corners [player :- Player] :- (Seq (Option Kw))
  (let [my-corners :- Key-set, (s/intersection @player corners)
        opposites :- (Seq (Option Kw)), (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                          (complimentary-corners corner))]
    (for [corner :- (Option Kw), opposites] :- (Option Kw)
      (some (fn [kw :- (Option Kw)] :- (Option Kw), (#{corner} kw)) (available-corners)))))

(defn available-sides [] :- (Seq (Option Kw))
  (remove nil? (for [side :- Kw, sides] :- (Option Kw)
                 (if-not (@board side) side))))

(defn fork-seq [player :- Player] :- (ASeq (Option Kw))
  (let [current :- Key-set, @player

        doppel-twos :- [Kw -> (ASeq Player)]
        (fn [space :- Kw] :- (ASeq Player)
          (let [doppelganger :- Player, (ref (set (conj current space)))]
            (for [two :- Key-set, (first-twos doppelganger)] :- Player
              (ref (set two)))))

        get-squares :- [Kw -> (Aseq Kw)]
        (fn [space :- kw] :- (ASeq Kw)
          (let [doppelganger :- Player, (ref (conj current space))]
            (apply concat (first-twos doppelganger))))

        winnable? :- [Kw -> (Option Bool)]
        (fn [space :- Kw] :- (Option Bool)
          (let [doppelganger :- Player, (ref (set (conj current space)))]
            (if (and (has-two? doppelganger)
                     (third doppelganger)
                     (not (@board (third doppelganger))))
              true)))]
    (remove nil? (for [square :- Kw, (keys @board)] :- (Option Kw)
                   (if (and (not (@board square))
                            (<= 2 (count (remove nil? (map third (doppel-twos square)))))
                            (not (@human square))
                            (not (@computer square))
                            (< (count (set (get-squares square))) (count (get-squares square)))
                            (< 1 (count (first-twos (ref (conj current square)))))
                            (winnable? square))
                     square)))))
