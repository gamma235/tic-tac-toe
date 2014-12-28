(ns ttt-test.record-test
  (:require [tic-tac-toe.core :refer :all]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.record :refer :all]
            [clojure.set :as s]
            [tic-tac-toe.helpers :refer :all]
            [expectations :refer :all])
  (:import [tic_tac_toe.record StrategyImpl]))

(def tested (StrategyImpl.))

;; validation methods with empty inputs
(expect nil
        (canWin tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                :b1 nil, :b2 nil, :b3 nil
                                :c1 nil, :c2 nil, :c3 nil}))

(expect nil
        (canBlock tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                  :b1 nil, :b2 nil, :b3 nil
                                  :c1 nil, :c2 nil, :c3 nil}))

(expect nil
        (canFork tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                 :b1 nil, :b2 nil, :b3 nil
                                 :c1 nil, :c2 nil, :c3 nil}))

(expect nil
        (canBlockFork tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                      :b1 nil, :b2 nil, :b3 nil
                                      :c1 nil, :c2 nil, :c3 nil}))

(expect true
        (fn? (canTakeCenter tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                            :b1 nil, :b2 nil, :b3 nil
                                            :c1 nil, :c2 nil, :c3 nil})))

(expect nil
        (canTakeOppositeCorner tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                               :b1 nil, :b2 nil, :b3 nil
                                               :c1 nil, :c2 nil, :c3 nil}))

(expect true
        (fn? (canTakeCorner tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                            :b1 nil, :b2 nil, :b3 nil
                                            :c1 nil, :c2 nil, :c3 nil})))

(expect true
        (fn? (canTakeSide tested #{} #{} {:a1 nil, :a2 nil, :a3 nil
                                          :b1 nil, :b2 nil, :b3 nil
                                          :c1 nil, :c2 nil, :c3 nil})))


;; validation methods with non-empty inputs
(expect true
        (fn? (canWin tested #{:a1 :a2 :b2} #{:a3 :b1 :c1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                           :b1 " x ", :b2 " o ", :b3 nil
                                                           :c1 " x ", :c2 nil, :c3 nil})))

(expect true
        (fn? (canBlock tested #{:a1 :a2 :b2 :c1} #{:a3 :b1 :c2 :c3} {:a1 " o ", :a2 " o ", :a3 " x "
                                                                     :b1 " x ", :b2 " o ", :b3 nil
                                                                     :c1 " o ", :c2 " x ", :c3 " x "})))

(expect true
        (fn? (canFork tested #{:a1 :a2} #{:a3 :b1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                    :b1 " x ", :b2 nil, :b3 nil
                                                    :c1 nil, :c2 nil, :c3 nil})))

(expect true
        (fn? (canBlockFork tested #{:a1 :a2} #{:a3 :b1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                         :b1 " x ", :b2 nil, :b3 nil
                                                         :c1 nil, :c2 nil, :c3 nil})))

(expect nil
        (canTakeCenter tested #{:b2} #{:a1} {:a1 " x ", :a2 nil, :a3 nil
                                             :b1 nil, :b2 " o ", :b3 nil
                                             :c1 nil, :c2 nil, :c3 nil}))

(expect true
        (fn? (canTakeOppositeCorner tested  #{:a1} #{:a3} {:a1 " o ", :a2 nil, :a3 " x "
                                                           :b1 nil, :b2 nil, :b3 nil
                                                           :c1 nil, :c2 nil, :c3 nil})))

(expect nil
        (canTakeCorner tested #{:a1 :c3} #{:a3 :c1} {:a1 " o ", :a2 nil, :a3 " x "
                                                     :b1 nil, :b2 nil, :b3 nil
                                                     :c1 " x ", :c2 nil, :c3 " o "}))

(expect nil
        (canTakeSide tested #{:b1 :b3} #{:a2 :c2} {:a1 nil, :a2 " x ", :a3 nil
                                                   :b1 " o ", :b2 nil, :b3 " o "
                                                   :c1 nil, :c2 " x ", :c3 nil}))

;; Strategy method tests non-nil -> nil

;; win
(expect :b2
        (win tested #{:b1 :b3} #{:a2 :c2} {:a1 nil, :a2 " x ", :a3 nil
                                           :b1 " o ", :b2 nil, :b3 " o "
                                           :c1 nil, :c2 " x ", :c3 nil}))

(expect nil
        (win tested #{:a2 :b1} #{:c2 :b2} {:a1 nil, :a2 " o ", :a3 nil
                                           :b1 " o ", :b2 " x ", :b3 nil
                                           :c1 nil, :c2 " x ", :c3 nil}))
;; block
(expect :b2
        (block tested #{:b1 :b3} #{:a2 :c2} {:a1 nil, :a2 " x ", :a3 nil
                                             :b1 " o ", :b2 nil, :b3 " o "
                                             :c1 nil, :c2 " x ", :c3 nil}))

(expect nil
        (block tested #{:a2 :b1} #{:c2 :b2} {:a1 nil, :a2 " o ", :a3 nil
                                             :b1 " o ", :b2 " x ", :b3 nil
                                             :c1 nil, :c2 " x ", :c3 nil}))
;; fork
(expect :a1
        (fork tested #{:a2 :b1} #{:c2 :b2} {:a1 nil, :a2 " o ", :a3 nil
                                            :b1 " o ", :b2 " x ", :b3 nil
                                            :c1 nil, :c2 " x ", :c3 nil}))

(expect nil
        (fork tested #{:b2} #{:a3} {:a1 nil, :a2 nil, :a3 " x "
                                    :b1 nil, :b2 " o ", :b3 nil
                                    :c1 nil, :c2 nil, :c3 nil}))

;; blockFork
(expect :a1
        (blockFork tested #{:b2 :c3} #{:a3 :c1} {:a1 nil, :a2 nil, :a3 " x "
                                                 :b1 nil, :b2 " o ", :b3 nil
                                                 :c1 " x ", :c2 nil, :c3 " o "}))

(expect nil
        (blockFork tested #{:b1 :b2 :c3} #{:a3 :b3 :c1} {:a1 nil, :a2 nil, :a3 " x "
                                                         :b1 " o ", :b2 " o ", :b3 " x "
                                                         :c1 " x ", :c2 nil, :c3 " o "}))
;; takeCenter
(expect :b2
        (takeCenter tested #{:a2 :b1 :b3 :c2} #{:a1 :a3 :c1 :c3} {:a1 " x ", :a2 " o ", :a3 " x "
                                                                  :b1 " o ", :b2 nil, :b3 " o "
                                                                  :c1 " x ", :c2 " o ", :c3 " x "}))

(expect nil
        (takeCenter tested #{:b2} #{:a1} {:a1 " x ", :a2 nil, :a3 nil
                                          :b1 nil, :b2 " o ", :b3 nil
                                          :c1 nil, :c2 nil, :c3 nil}))

;; takeOppositeCorner
(expect :a3
        (takeOppositeCorner tested #{:b2 :c1} #{:a1} {:a1 " x ", :a2 nil, :a3 nil
                                                      :b1 nil, :b2 " o ", :b3 nil
                                                      :c1 " o ", :c2 nil, :c3 nil}))

(expect nil
        (takeOppositeCorner tested #{:b2} #{:a1} {:a1 " x ", :a2 nil, :a3 nil
                                                  :b1 nil, :b2 " o ", :b3 nil
                                                  :c1 nil, :c2 nil, :c3 nil}))

;; takeCorner
(expect :a1
        (takeCorner tested #{:a3 :b2 :c1} #{:c3 :b3 :b1} {:a1 nil, :a2 nil, :a3 " o "
                                                          :b1 nil, :b2 " o ", :b3 " x "
                                                          :c1 " o ", :c2 nil, :c3 " x "}))

(expect nil
        (takeCorner tested #{:a3 :b2 :c1} #{:c3 :b3 :b1} {:a1 " o ", :a2 " x ", :a3 " o "
                                                          :b1 nil, :b2 " o ", :b3 " x "
                                                          :c1 " o ", :c2 nil, :c3 " x "}))

;; takeSide
(expect :c2
        (takeSide tested #{:a1 :a3 :b2 :c1} #{:a2 :b1 :b3  :c3} {:a1 " o ", :a2 " x ", :a3 " o "
                                                                 :b1 " x ", :b2 " o ", :b3 " x "
                                                                 :c1 " o ", :c2 nil, :c3 " x "}))

(expect nil
        (takeSide tested #{:a1 :b2 :c1 :c2} #{:a2 :b1 :b3 :c3} {:a1 " o ", :a2 " x ", :a3 nil
                                                                :b1 " x ", :b2 " o ", :b3 " x "
                                                                :c1 " o ", :c2 " o ", :c3 " x "}))
