(ns tic-tac-toe.helpers
  (:require [clojure.set :as s]
            [clojure.core.typed :refer :all]))

;; aliases for core.typed signatures
(defalias Key-set (Set (Option Kw)))
(defalias Player Key-set)
(defalias Set-vec (Vec (Set Kw)))

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
              (s/intersection pair player)))))

(defn has-two? [player :- Player] :- Bool
  (not (empty? (first-twos player))))

(defn has-opposite-corners? [player :- Player] :- Bool
  (not (empty? (filter (fn [a :- (Option Key-set)] (= (count a) 2))
                       (for [pair :- Key-set, opposite-corners] :- (Option Key-set)
                         (s/intersection pair player))))))

(defn third [player :- Player, game-board :- (Map Kw (Option Str))] :- (Option Kw)
  (let [twos :- (ASeq (Option Key-set)), (first-twos player)
        open-spaces :- (ASeq Kw), (filter keyword? (apply concat (filter (fn [[k v] :- (ASeq (U Kw Str nil))] :- Bool (nil? v)) game-board)))
        thirds :- (ASeq (Option Kw)), (for [two :- (Option Key-set), twos
                                            space :- Kw, open-spaces] :- (Option Kw)
                                        (if (some #{(conj two space)} triplets) space))]
    (first (filter keyword? thirds))))

(defn available-corners [game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (remove nil? (for [corner :- Kw, corners] :- (Option Kw)
                 (if-not (game-board corner) corner))))

(defn candidate-opposite-corners [player :- Player, game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (let [my-corners :- Key-set, (s/intersection player corners)
        opposites :- (clojure.lang.LazySeq (Option Kw)), (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                                           (complimentary-corners corner))]
    (for [corner :- (Option Kw), opposites] :- (Option Kw)
      (some (fn [Kw :- (Option Kw)] :- (Option Kw), (#{corner} Kw)) (available-corners game-board)))))

(defn available-sides [game-board :- (Map Kw (Option Str))] :- (Seq (Option Kw))
  (filter keyword? (map (fn [side :- Kw] (if-not (game-board side) side)) sides)))

(defn fork-seq [player :- Player, computer :- Player, human :- Player, game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (let [doppel-twos :- [Kw -> (ASeq Player)]
        (fn [space :- Kw] :- (ASeq Player)
          (let [doppelganger :- Player, (set (conj player space))]
            (first-twos doppelganger)))

        get-squares :- [Kw -> (ASeq Kw)]
        (fn [space :- Kw] :- (ASeq Kw)
          (let [doppelganger :- Player, (conj player space)]
            (apply concat (first-twos doppelganger))))

        winnable? :- [Kw -> (Option Bool)]
        (fn [space :- Kw] :- (Option Bool)
          (let [doppelganger :- Player, (set (conj player space))]
            (if (and (has-two? doppelganger)
                     (third doppelganger game-board)
                     (not (game-board (third doppelganger game-board))))
              true)))]
    (filter keyword? (map (fn [square :-　Kw] :- (Option Kw) (if (and (not (game-board square))
                                                      (<= 2 (count (remove nil? (map #(third % game-board) (doppel-twos square)))))
                                                      (not (human square))
                                                      (not (computer square))
                                                      (< (count (set (get-squares square))) (count (get-squares square)))
                                                      (< 1 (count (first-twos (conj player square))))
                                                      (winnable? square)) square))
                          (keys game-board)))))
