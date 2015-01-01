(ns tic-tac-toe.protocol
  (:refer-clojure :exclude [defrecord defprotocol let defn fn for loop defprotocol])
  (:require [clojure.core.typed :refer [Set Kw Map Str Option defalias defprotocol]]))

(defalias Key-set (Set (Option Kw)))
(defalias Board (Map Kw (Option Str)))

(defprotocol Strategy
  "Strategy methods update the Tic-tac-toe board when they are called"

  ;; strategy methods
  (win [this computer human board] :- Key-set)
  (block [this computer human board] :- Key-set)
  (fork [this computer human board] :- Key-set)
  (blockFork [this computer human board] :- Key-set)
  (takeCenter [this computer human board] :- Key-set)
  (takeOppositeCorner[this computer human board] :- Key-set)
  (takeCorner [this computer human board] :- Key-set)
  (takeSide [this computer human board] :- Key-set)

  ;; validation methods
  (canWin [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canBlock [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canFork [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canBlockFork [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canTakeCenter [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canTakeOppositeCorner [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canTakeCorner [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil]))
  (canTakeSide [this computer human board] :- (Option [Strategy Key-set Key-set Board -> nil])))
