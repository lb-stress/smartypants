(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :as core]))

(deftest winner-detection
  (is (= :x (core/winner [:x :x :x nil nil nil nil nil nil])))
  (is (= :o (core/winner [:x nil nil :o :o :o nil nil nil])))
  (is (nil? (core/winner [nil nil nil nil nil nil nil nil nil]))))

(deftest best-move-wins
  (is (= 2 (core/best-move [:o :o nil :x :x nil nil nil nil])))
  (is (= 2 (core/best-move [:x :x nil nil :o nil nil nil nil]))))
