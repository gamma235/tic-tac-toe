(ns tic-tac-toe.helpers
  (:refer-clojure :exclude [defrecord defprotocol let defn fn for loop defprotocol])
  (:require [clojure.set :as s]
            [clojure.core.typed :refer [U Set Vec Bool Kw ASeq Seq Map Str Option
                                        let defn fn for defalias loop defprotocol] :as t]))

;; aliases for core.typed signatures
(defalias Key-set (Set (Option Kw)))
(defalias Set-vec (Vec (Set Kw)))

;; special positions
(t/def corners :- (Set Kw), #{:a1 :a3 :c1 :c3})
(t/def middle :- Kw, :b2)
(t/def adjacents :- Set-vec, [#{:a1 :a2} #{:a1 :b1} #{:a1 :b2} #{:a2 :a3} #{:a2 :b2} #{:a3 :b2} #{:a3 :b3} #{:b1 :b2} #{:b1 :c1} #{:b2 :b3} #{:b2 :c2} #{:b2 :c3} #{:b3 :c3} #{:c1 :c2} #{:c2 :c3}])
(t/def opposite-corners :- Set-vec, [#{:a1 :c3} #{:a3 :c1}])
(t/def same-side-corners :- Set-vec, [#{:a1 :a3} #{:a1 :c1} #{:a3 :c3} #{:c1 :c3}])
(t/def complimentary-corners :- (Map Kw Kw), {:a1 :c3, :c3 :a1, :a3 :c1, :c1 :a3})
(t/def sides :- (Set Kw), #{:a2 :b1 :b3 :c2})
(t/def opposite-sides :- Set-vec, [#{:a1 :a3} #{:a1 :c1} #{:a2 :c2} #{:a3 :c3} #{:b1 :b3} #{:c1 :c3}])
(t/def triplets :- Set-vec, [#{:a1 :a2 :a3} #{:b1 :b2 :b3} #{:c1 :c2 :c3} #{:a1 :b1 :c1} #{:a2 :b2 :c2} #{:a3 :b3 :c3} #{:a1 :b2 :c3} #{:a3 :b2 :c1}])

;; helpers
(defn first-twos
  "returns a seq of sets, each representing two taken squares, that could lead to a win"
  [player :- Key-set] :- (ASeq (Option Key-set))
  (let [couples :- (Seqable Key-set), (concat adjacents opposite-corners opposite-sides)]
    (filter (fn [a :- (Option Key-set)] (= (count a) 2))
            (for [pair :- Key-set, couples] :- (Option Key-set)
              (s/intersection pair player)))))

(defn has-two?
  "tells if provided has taken two spaces that can lead to a possible third"
  [player :- Key-set] :- Bool
  (not (empty? (first-twos player))))

(defn has-opposite-corners?
  "tells if provided player has two opposite corners"
  [player :- Key-set] :- Bool
  (not (empty? (filter (fn [a :- (Option Key-set)] (= (count a) 2))
                       (for [pair :- Key-set, opposite-corners] :- (Option Key-set)
                         (s/intersection pair player))))))

(defn third
  "gives the space that if taken will win the game for the provided player"
  [player :- Key-set, game-board :- (Map Kw (Option Str))] :- (Option Kw)
  (let [twos :- (ASeq (Option Key-set)), (first-twos player)
        open-spaces :- (ASeq Kw), (filter keyword? (apply concat (filter (fn [[k v] :- (ASeq (U Kw Str nil))] :- Bool (nil? v)) game-board)))
        thirds :- (ASeq (Option Kw)), (for [two :- (Option Key-set), twos
                                            space :- Kw, open-spaces] :- (Option Kw)
                                        (if (some #{(conj two space)} triplets) space))]
    (first (filter keyword? thirds))))

(defn available-corners
  "gives all available corners on the board"
  [game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (remove nil? (for [corner :- Kw, corners] :- (Option Kw)
                 (if-not (game-board corner) corner))))

(defn candidate-opposite-corners
  "tells which corner a player can take that is opposite one he/she already has"
  [player :- Key-set, game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (let [my-corners :- Key-set, (s/intersection player corners)
        opposites :- (clojure.lang.LazySeq (Option Kw)), (for [corner :- (Option Kw), my-corners] :- (Option Kw)
                                                           (complimentary-corners corner))]
    (for [corner :- (Option Kw), opposites] :- (Option Kw)
      (some (fn [Kw :- (Option Kw)] :- (Option Kw), (#{corner} Kw)) (available-corners game-board)))))

(defn available-sides
  "gives sides available on the board"
  [game-board :- (Map Kw (Option Str))] :- (Seq (Option Kw))
  (filter keyword? (map (fn [side :- Kw] (if-not (game-board side) side)) sides)))

(defn fork-seq
  "bad mamma jamma function that gives a seq of spaces that can lead to a fork"
  [player :- Key-set, computer :- Key-set, human :- Key-set, game-board :- (Map Kw (Option Str))] :- (ASeq (Option Kw))
  (let [;; bind three functions to the local scope

        ;; doppel-twos returns twos for the scenario in which the space provided is taken
        doppel-twos :- [Kw -> (ASeq Key-set)]
        (fn [space :- Kw] :- (ASeq Key-set)
          (let [doppelganger :- Key-set, (set (conj player space))]
            (first-twos doppelganger)))

        ;; get-squares provides a seq of squares for the case in which the space is taken
        get-squares :- [Kw -> (ASeq Kw)]
        (fn [space :- Kw] :- (ASeq Kw)
          (let [doppelganger :- Key-set, (conj player space)]
            (apply concat (first-twos doppelganger))))

        ;; winnable? tells whether taking the space will provide a scenario that can lead to a win
        winnable? :- [Kw -> (Option Bool)]
        (fn [space :- Kw] :- (Option Bool)
          (let [doppelganger :- Key-set, (set (conj player space))]
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
