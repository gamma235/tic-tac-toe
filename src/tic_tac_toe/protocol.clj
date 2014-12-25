(ns tic-tac-toe.protocol
  (:require [clojure.core.typed :refer :all]))

;; Creating protocol for both the strategies and the tests that check to see if they are possible
(defprotocol Strategy
  "Strategy methods update the Tic-tac-toe board when they are called"

  ;; strategy methods : 'this' as a first argument is inferred so only the return values are made explicit
  (win [this computer human board] :- (Set (Option Kw))
   "wins the game by filling an open space to get 3 in a row")

  (block [this computer human board] :- (Set (Option Kw))
   "blocks an opponents win by filling an open space")

  (fork [this computer human board] :- (Set (Option Kw))
   "creates a two way win scenario guaranteeing victory")

  (blockFork [this computer human board] :- (Set (Option Kw))
   "prevents an opponent from forking")

  (takeCenter [this computer human board] :- (Set (Option Kw))
   "takes center")

  (takeOppositeCorner [this computer human board] :- (Set (Option Kw))
   "takes a corner opposite to one the computer already has")

  (takeCorner [this computer human board] :- (Set (Option Kw))
   "takes an avaiable corner")

  (takeSide [this computer human board] :- (Set (Option Kw))
   "takes an available side")

  ;; validation methods
  (canWin [this computer human board] :- (Option [Strategy -> nil]))

  (canBlock [this computer human board] :- (Option [Strategy -> nil]))

  (canFork [this computer human board] :- (Option [Strategy -> nil]))

  (canBlockFork [this computer human board] :- (Option [Strategy -> nil]))

  (canTakeCenter [this computer human board] :- (Option [Strategy (Map Kw (Option Str)) -> nil]))

  (canTakeOppositeCorner [this computer human board] :- (Option [Strategy -> nil]))

  (canTakeCorner [this computer human board] :- (Option [Strategy -> nil]))

  (canTakeSide [this computer human board] :- (Option [Strategy -> nil])))
