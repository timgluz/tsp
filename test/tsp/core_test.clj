(ns tsp.core-test
  (:require [midje.sweet :refer :all]
            [tsp.core :refer :all]))

(facts "calc-coord-diffs"
  (fact "it handles 2D points"
    (calc-coord-diffs [[0 0] [0 0]]) => [0 0]
    (calc-coord-diffs [[1 1] [0 0]]) => [1 1]
    (calc-coord-diffs [[-1 -1][0 0]]) => [-1 -1]
    (calc-coord-diffs [[-2 2][-2 0]]) => [0 2])
  (fact "it handles 3D coordinates"
    (calc-coord-diffs [[0 0 0][0 0 0]]) => [0 0 0]
    (calc-coord-diffs [[1 1 1][0 0 0]]) => [1 1 1]
    (calc-coord-diffs [[-1 -1 -1][0 0 0]]) => [-1 -1 -1]
    (calc-coord-diffs [[-2 2 0][-2 0 1]]) => [0 2 -1]))

(facts "square-sum"
  (fact "it returns sum of squared factors"
    (square-sum [0 0]) => 0
    (square-sum [1 1]) => 2
    (square-sum [0 0 0]) => 0
    (square-sum [1 1 1]) => 3
    (square-sum [2 3 4]) => 29
    (square-sum [-3 -3 -3]) => 27))

(facts "euclidean-distance"
  (fact "it calculates correct distances"
    (euclidean-distance [0 0] [0 0]) => 0.0
    (euclidean-distance [-3 4 0] [0 0 0]) => 5.0
    (euclidean-distance [-3 -4 0] [5 -2 3]) => (roughly 8.77 0.01)))

(facts "calc-path-lengths"
  (fact "it calculates distances between points"
    (calc-path-lengths [[0 0][0 1]]) => [1.0 1.0]
    (calc-path-lengths [[0 0][0 1][1 1][1 0]]) => [1.0 1.0 1.0 1.0]))

(facts "calc-total-length"
  (fact "it calculates total length of path"
    (calc-total-length [[0 0][0 1]]) => 2.0
    (calc-total-length [[0 0][0 1][1 1][1 0]]) => 4.0))

(facts "switch-items-at"
  (let [test-vec [1 2 3 4 5]]
    (fact "does nothing when pos1 and pos2 are equal or out of boundaries"
      (swap-items-at test-vec 1 1) => [1 2 3 4 5]
      (swap-items-at test-vec 5 5) => [1 2 3 4 5]
      (swap-items-at test-vec 0 5) => [1 2 3 4 5]
      (swap-items-at test-vec 1 100) => [1 2 3 4 5])
    (fact "swaps items correctly"
      (swap-items-at test-vec 1 2) => [2 1 3 4 5]
      (swap-items-at test-vec 1 3) => [3 2 1 4 5]
      (swap-items-at test-vec 1 5) => [5 2 3 4 1]
      (swap-items-at test-vec 2 4) => [1 4 3 2 5]
      (swap-items-at test-vec 2 5) => [1 5 3 4 2]
      (swap-items-at test-vec 3 4) => [1 2 4 3 5])))
