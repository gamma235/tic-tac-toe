(ns tic-tac-toe.protocol
  (:refer-clojure :exclude [defrecord defprotocol let defn fn for loop defprotocol])
  (:require [clojure.core.typed :refer [U Set Vec Bool Kw ASeq Seq Map Str Option
                                        let defn fn for defalias loop defprotocol] :as t]))

(defprotocol Strategy
  "Strategy methods update the Tic-tac-toe board when they are called"

  ;; strategy methods
  (win [this computer human board] :- (Set (Option Kw)))
  (block [this computer human board] :- (Set (Option Kw)))
  (fork [this computer human board] :- (Set (Option Kw)))
  (blockFork [this computer human board] :- (Set (Option Kw)))
  (takeCenter [this computer human board] :- (Set (Option Kw)))
  (takeOppositeCorner[this computer human board] :- (Set (Option Kw)))
  (takeCorner [this computer human board] :- (Set (Option Kw)))
  (takeSide [this computer human board] :- (Set (Option Kw)))

  ;; validation methods
  (canWin [this computer human board] :- (Option [Strategy -> nil]))
  (canBlock [this computer human board] :- (Option [Strategy -> nil]))
  (canFork [this computer human board] :- (Option [Strategy -> nil]))
  (canBlockFork [this computer human board] :- (Option [Strategy -> nil]))
  (canTakeCenter [this computer human board] :- (Option [Strategy (Map Kw (Option Str)) -> nil]))
  (canTakeOppositeCorner [this computer human board] :- (Option [Strategy -> nil]))
  (canTakeCorner [this computer human board] :- (Option [Strategy -> nil]))
  (canTakeSide [this computer human board] :- (Option [Strategy -> nil])))
