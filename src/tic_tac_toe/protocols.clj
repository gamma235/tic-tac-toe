(ns tic-tac-toe.protocols)


;; Creating protocol for both the strategies and the tests that check to see if they are possible
(defprotocol Strategy
  "Strategy methods update the Tic-tac-toe board when they are called"

  ;; strategies
  (win [_] "wins the game by filling an open space to get 3 in a row")
  (block [_] "blocks an opponents win by filling an open space")
  (fork [_] "creates a two way win scenario guaranteeing victory")
  (block-fork [_] "prevents an opponent from forking")
  (take-center [_] "takes center")
  (take-opposite-corner [_] "takes a corner opposite to one the computer already has")
  (take-corner [_] "takes an avaiable corner")
  (take-side [_] "takes an available side")

  ;; tests
  (can-win? [_])
  (can-block? [_])
  (can-fork? [_])
  (can-block-fork? [_])
  (can-take-center? [_])
  (can-take-opposite-corner? [_])
  (can-take-corner? [_])
  (can-take-side? [_]))
