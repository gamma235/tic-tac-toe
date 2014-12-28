(ns ttt-test.helpers-test
  (:require [tic-tac-toe.core :refer :all]
            [tic-tac-toe.protocol :refer :all]
            [tic-tac-toe.record :refer :all]
            [clojure.set :as s]
            [tic-tac-toe.helpers :refer :all]
            [expectations :refer :all])
  (:import [tic_tac_toe.record StrategyImpl]))


;; first-twos
(expect '(#{:a1 :b1})
        (first-twos #{:a1 :b1}))

(expect '(#{:b2 :a1} #{:b2 :c2})
        (first-twos #{:a1 :c2 :b2}))

(expect '()
        (first-twos #{:a1 :c2}))

(expect '()
        (first-twos #{:a1}))

(expect '()
        (first-twos [:a1 :b1]))

(expect '()
        (first-twos #{}))

(expect '()
        (first-twos []))

(expect '()
        (first-twos nil))

(expect clojure.lang.ArityException
        (first-twos :a1 :b1))

(expect clojure.lang.ArityException
        (first-twos))

(expect UnsupportedOperationException
        (first-twos :a1))

(expect IllegalArgumentException
        (first-twos "hello"))


;; has-two?
(expect true
        (has-two? #{:a1 :a2}))

(expect true
        (has-two? #{:a1 :a2 :a3}))

(expect false
        (has-two? #{}))

(expect false
        (has-two? []))

(expect false
        (has-two? [:a1 :a2]))

(expect false
        (has-two? [:a1 :a2]))

(expect false
        (has-two? nil))

(expect clojure.lang.ArityException
        (has-two? :a1 :a2))

(expect clojure.lang.ArityException
        (has-two?))

(expect UnsupportedOperationException
        (has-two? :a1))

(expect IllegalArgumentException
        (first-twos "hello"))


;; has-opposite-corners?
(expect true
        (has-opposite-corners? #{:a1 :c3}))
(expect true
        (has-opposite-corners? #{:a1 :a3 :c1 :c3}))

(expect false
        (has-opposite-corners? #{}))

(expect false
        (has-opposite-corners? []))

(expect false
        (has-opposite-corners? [:a1 :c3]))

(expect false
        (has-opposite-corners? #{:a1 :a3}))

(expect false
        (has-opposite-corners? #{:a1 :a2}))

(expect false
        (has-opposite-corners? #{:a1 :b2}))

(expect false
        (has-opposite-corners? nil))

(expect clojure.lang.ArityException
        (has-opposite-corners?))

(expect clojure.lang.ArityException
        (has-opposite-corners? :a1 :a2))

(expect UnsupportedOperationException
        (has-opposite-corners? :a1))

(expect IllegalArgumentException
        (has-opposite-corners? "hello"))


;; third
(expect nil
        (third #{} {:a1 nil, :a2 nil, :a3 nil
                    :b1 nil, :b2 nil, :b3 nil
                    :c1 nil, :c2 nil, :c3 nil}))

(expect :a3
        (third #{:a1 :a2} {:a1 " o ", :a2 " o ", :a3 nil
                           :b1 nil, :b2 " x ", :b3 nil
                           :c1 " x ", :c2 nil, :c3 nil}))

(expect nil
        (third #{:a1 :a2 :a3} {:a1 " o ", :a2 " o ", :a3 " o "
                               :b1 nil, :b2 " x ", :b3 nil
                               :c1 " x ", :c2 nil, :c3 nil}))

(expect nil
        (third [:a1 :a2] {:a1 " o ", :a2 " o ", :a3 nil
                          :b1 nil, :b2 nil, :b3 " x "
                          :c1 nil, :c2 nil, :c3 " x "}))

(expect nil
        (third [] {:a1 nil, :a2 nil, :a3 nil
                   :b1 nil, :b2 nil, :b3 nil
                   :c1 nil, :c2 nil, :c3 nil}))

(expect nil
        (third #{:a1 :a2} nil))

(expect nil
        (third nil {:a1 " o ", :a2 nil, :a3 nil
                    :b1 nil, :b2 nil, :b3 nil
                    :c1 nil, :c2 nil, :c3 " x "}))

(expect nil
        (third #{:a1 :a2} nil))

(expect nil
        (third nil nil))

(expect clojure.lang.ArityException
        (third :a1 :a2 {:a1 " o ", :a2 " o ", :a3 nil
                        :b1 " x ", :b2 nil, :b3 " x "
                        :c1 nil, :c2 nil, :c3 nil}))

(expect clojure.lang.ArityException
        (third {:a1 nil, :a2 nil, :a3 nil
                :b1 nil, :b2 nil, :b3 nil
                :c1 nil, :c2 nil, :c3 nil}))

(expect UnsupportedOperationException
        (third :a1 {:a1 " o ", :a2 nil, :a3 nil
                    :b1 nil, :b2 nil, :b3 nil
                    :c1 nil, :c2 nil, :c3 " x "}))

(expect UnsupportedOperationException
        (third "hello" "world"))


;; available-corners --coerce to set to avoid failure related to order
(expect #{:a1 :a3 :c1 :c3}
        (set (available-corners {:a1 nil, :a2 nil, :a3 nil
                                 :b1 nil, :b2 nil, :b3 nil
                                 :c1 nil, :c2 nil, :c3 nil})))

(expect #{:a3 :c1 :c3}
        (set (available-corners {:a1 " o ", :a2 nil, :a3 nil
                                 :b1 nil, :b2 nil, :b3 nil
                                 :c1 nil, :c2 nil, :c3 nil})))

(expect #{:a3 :c1 :c3}
        (set (available-corners {:a1 " o ", :a2 nil, :a3 nil
                                 :b1 " x ", :b2 nil, :b3 nil
                                 :c1 nil, :c2 nil, :c3 nil})))

(expect '()
        (available-corners {:a1 " o ", :a2 nil, :a3 " x "
                            :b1 nil, :b2 nil, :b3 nil
                            :c1 " x ", :c2 nil, :c3 " o "}))

(expect #{:a1 :a3 :c1 :c3}
        (set (available-corners {}))) ;; unusual edge case

(expect #{:a1 :a3 :c1 :c3}
        (set (available-corners #{}))) ;; unusual edge case

(expect IllegalArgumentException
        (available-corners []))

(expect ClassCastException
        (available-corners "hello"))

(expect NullPointerException
        (available-corners nil))

;; candidate-opposite-corners
(expect '(:c3)
        (candidate-opposite-corners #{:a1} {:a1 " o ", :a2 nil, :a3 " x "
                                            :b1 nil, :b2 nil, :b3 nil
                                            :c1 nil, :c2 nil, :c3 nil}))

(expect '()
        (candidate-opposite-corners #{} {:a1 nil, :a2 nil, :a3 nil
                                         :b1 nil, :b2 nil, :b3 nil
                                         :c1 nil, :c2 nil, :c3 nil}))

(expect '()
        (candidate-opposite-corners #{} {:a1 " x ", :a2 nil, :a3 " o "
                                         :b1 nil, :b2 nil, :b3 nil
                                         :c1 " o ", :c2 nil, :c3 " x "}))

(expect '()
        (candidate-opposite-corners [] []))

(expect '()
        (candidate-opposite-corners #{} #{}))

(expect '()
        (candidate-opposite-corners {} {}))

(expect '()
        (candidate-opposite-corners [] {:a1 " x ", :a2 nil, :a3 " o "
                                        :b1 nil, :b2 nil, :b3 nil
                                        :c1 nil, :c2 nil, :c3 " x "}))

(expect '()
        (candidate-opposite-corners #{} nil))

(expect '()
        (candidate-opposite-corners nil {:a1 " x ", :a2 nil, :a3 " o "
                                         :b1 nil, :b2 nil, :b3 nil
                                         :c1 nil, :c2 nil, :c3 " x "}))

(expect '()
        (candidate-opposite-corners nil {:a1 nil, :a2 nil, :a3 nil
                                         :b1 nil, :b2 nil, :b3 nil
                                         :c1 nil, :c2 nil, :c3 nil}))
(expect '()
        (candidate-opposite-corners nil nil))

(expect NullPointerException
        (candidate-opposite-corners #{:c1} nil))

(expect clojure.lang.ArityException
        (candidate-opposite-corners #{:c1}))

(expect clojure.lang.ArityException
        (candidate-opposite-corners))

(expect IllegalArgumentException
        (candidate-opposite-corners "hello" "world"))

;; available-sides
(expect #{:a2 :b1 :b3 :c2}
        (set (available-sides {:a1 nil, :a2 nil, :a3 nil
                               :b1 nil, :b2 nil, :b3 nil
                               :c1 nil, :c2 nil, :c3 nil})))

(expect #{:a2 :b1 :b3 :c2}
        (set (available-sides {:a1 " o ", :a2 nil, :a3 " x "
                               :b1 nil, :b2 nil, :b3 nil
                               :c1 " x ", :c2 nil, :c3 " o "})))

(expect #{:a2 :b3 :c2}
        (set (available-sides {:a1 " o ", :a2 nil, :a3 nil
                               :b1 " x ", :b2 nil, :b3 nil
                               :c1 nil, :c2 nil, :c3 nil})))

(expect #{}
        (set (available-sides {:a1 nil, :a2 " x ", :a3 nil
                               :b1 " o ", :b2 nil, :b3 " o "
                               :c1 nil, :c2 " x ", :c3 nil})))

(expect #{:a2 :b1 :b3 :c2}
        (set (available-sides {}))) ;; unusual edge case

(expect #{:a2 :b1 :b3 :c2}
        (set (available-sides #{}))) ;; unusual edge case

(expect IllegalArgumentException
        (available-sides []))

(expect ClassCastException
        (available-sides "hello"))

(expect NullPointerException
        (available-sides nil))


;; fork-seq -- [tested-player computer human board]
(expect '(:b2)
        (fork-seq #{:a1 :a2} #{:a1 :a2} #{:a3 :c1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                    :b1 nil, :b2 nil, :b3 nil
                                                    :c1 " x ", :c2 nil, :c3 nil}))

(expect #{:b3 :c2 :c3}
        (set (fork-seq #{:a3 :c1} #{:a1 :a2} #{:a3 :c1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                         :b1 nil, :b2 nil, :b3 nil
                                                         :c1 " x ", :c2 nil, :c3 nil})))

(expect #{:c3}
        (set (fork-seq #{:a3 :c2} #{:a1 :a2} #{:a3 :c1} {:a1 " o ", :a2 " o ", :a3 " x "
                                                         :b1 nil, :b2 nil, :b3 nil
                                                         :c1 nil, :c2 " x ", :c3 nil})))

(expect '()
        (fork-seq #{} #{} #{} {:a1 nil, :a2 nil, :a3 nil
                               :b1 nil, :b2 nil, :b3 nil
                               :c1 nil, :c2 nil, :c3 nil}))

(expect '()
        (fork-seq [] [] [] {:a1 nil, :a2 nil, :a3 nil
                            :b1 nil, :b2 nil, :b3 nil
                            :c1 nil, :c2 nil, :c3 nil})) ;; unusual edge case

(expect '()
        (fork-seq nil #{} #{} {:a1 nil, :a2 nil, :a3 nil
                               :b1 nil, :b2 nil, :b3 nil
                               :c1 nil, :c2 nil, :c3 nil}))

(expect '()
        (fork-seq nil #{:a1 :a2} #{:a3 :c1} {:a1 " o ", :a2 " o ", :a3 " x "
                                             :b1 nil, :b2 nil, :b3 nil
                                             :c1 " x ", :c2 nil, :c3 nil}))

(expect '()
        (fork-seq #{} #{} #{} nil))

(expect '()
        (fork-seq nil nil nil {:a1 nil, :a2 nil, :a3 nil
                               :b1 nil, :b2 nil, :b3 nil
                               :c1 nil, :c2 nil, :c3 nil}))

(expect '()
        (fork-seq nil nil nil nil))

(expect NullPointerException
        (set (fork-seq #{:a3 :c1} #{:a1 :a2} nil {:a1 " o ", :a2 " o ", :a3 " x "
                                                  :b1 nil, :b2 nil, :b3 nil
                                                  :c1 " x ", :c2 nil, :c3 nil})))

(expect IllegalArgumentException
        (fork-seq [:a1 :a2] [:a1 :a2] [:a3 :b3] {:a1 " o ", :a2 " o ", :a3 " x "
                                                 :b1 nil, :b2 nil, :b3 " x "
                                                 :c1 nil, :c2 nil, :c3 nil}))

(expect IllegalArgumentException
        (fork-seq {} {} {} {:a1 nil, :a2 nil, :a3 nil
                            :b1 nil, :b2 nil, :b3 nil
                            :c1 nil, :c2 nil, :c3 nil}))

(expect ClassCastException
        (fork-seq "hello" "world" "!" {:a1 nil, :a2 nil, :a3 nil
                                       :b1 nil, :b2 nil, :b3 nil
                                       :c1 nil, :c2 nil, :c3 nil}))
