(ns tic-tac-toe.protocol
  (:require [clojure.core.typed :refer :all]))

;; Creating protocol for both the strategies and the tests that check to see if they are possible
;; Annotating the protocol for richer specification of type constraints on implementations
(defprotocol Strategy
  "Strategy methods update the Tic-tac-toe board when they are called"

  ;; strategy methods : 'this' as a first argument is inferred so only the return values are made explicit
  (win [this] :- (Set (Option Kw))
   "wins the game by filling an open space to get 3 in a row")

  (block [this] :- (Set (Option Kw))
   "blocks an opponents win by filling an open space")

  (fork [this] :- (Set (Option Kw))
   "creates a two way win scenario guaranteeing victory")

  (block-fork [this] :- (Set (Option Kw))
   "prevents an opponent from forking")

  (take-center [this] :- (Set (Option Kw))
   "takes center")

  (take-opposite-corner [this] :- (Set (Option Kw))
   "takes a corner opposite to one the computer already has")

  (take-corner [this] :- (Set (Option Kw))
   "takes an avaiable corner")

  (take-side [this] :- (Set (Option Kw))
   "takes an available side")

  ;; validation methods
  (can-win? [this] :- (Option [Strategy -> nil]))

  (can-block? [this] :- (Option [Strategy -> nil]))

  (can-fork? [this] :- (Option [Strategy -> nil]))

  (can-block-fork? [this] :- (Option [Strategy -> nil]))

  (can-take-center? [this] :- (Option [Strategy -> nil]))

  (can-take-opposite-corner? [this] :- (Option [Strategy -> nil]))

  (can-take-corner? [this] :- (Option [Strategy -> nil]))

  (can-take-side? [this] :- (Option [Strategy -> nil])))
