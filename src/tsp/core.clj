(ns tsp.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [tsp.canvas :as canvas])
  (:import [java.lang Math]))


(defn load-path
  "loads list of coordinates from the given file-uri
  It expect that every row as format <id> <coordX> <coordY>"
  [file-uri]
  (with-open [in-file (io/reader file-uri)]
    (vec
      (for [line (line-seq in-file)]
        (->>
          (string/split line #"\s")
          (map #(Float/parseFloat %1))
          rest
          vec)))))

(defn calc-coord-diffs
  "calculates difference between the coordinates of 2 points
  Input: the vector of points [[1 1][0 0]]
  Returns: the vector of differences [1 1]"
  [points]
  (vec
    (map
      (fn [coords] (reduce - coords))
      (partition 2 (apply interleave points)))))

(defn square-sum
  [factors]
  (reduce +
          (map #(* %1 %1) factors)))

(defn euclidean-distance
  "calculates euclidean distance between A-B
  Input: a vector of points, example [[0 0] [1 1]]"
  [pt1 pt2]
  (-> [pt1 pt2] calc-coord-diffs square-sum Math/sqrt))

(defn calc-path-lengths
  "calculates euclidean distances between closed path points
  for point vector [A B C] it calculates distances between cycle A-B-C-A"
  [path-points]
  (vec
    (map
      (fn [start end] (euclidean-distance start end))
      ;;iterate over pair of startpoints and endpoints
      path-points
      (vec (concat (rest path-points) [(first path-points)])))))

;;TODO: optimize, use only distances around swapped positions
(defn calc-total-length
  "Calculates total length of path"
  [path-points]
  (reduce + (calc-path-lengths path-points)))

(defn swap-items-at
  "moves item on pos1 to pos2"
  [items pos1 pos2]
  (let [[start-pos end-pos equal] ((juxt min max =) pos1 pos2)]
  (if (or
        (= true equal)
        (> 1 start-pos)
        (< (count items) end-pos))
    items ;; do nothing
    (let [[left, right] (split-at start-pos items)
          [beginning, item1] (split-at (dec start-pos) left)
          [split-area, end] (split-at (- end-pos start-pos) right)]
      (vec
        (concat beginning
                [(last split-area)]
                (take (dec (count split-area)) split-area)
                item1
                end))))))

(defn rand-pos [n-items]
  ((comp inc rand-int) n-items))


;; It's used by SimAN in book "Nature_inspired Algorithms", Xin-She Yang
(defn calc-transition-prob
  [old-length new-length current-temp]
  (Math/exp
    (/ (- (+ old-length Float/MIN_VALUE) new-length) current-temp)))


(defn mutate-path
  "mutates path current-temp times and may accept weaker results"
  [current-temp old-path]
  (let [ncities (count old-path)
        old-length (calc-total-length old-path)]
    (loop [step (int current-temp), current-path old-path, current-length old-length]
      (if (> 1 step)
        current-path
        (let [mutated-path (swap-items-at old-path (rand-pos ncities) (rand-pos ncities))
              mutated-length (calc-total-length mutated-path) ;; new total distance
              transition-probability (calc-transition-prob current-length
                                                           mutated-length
                                                           current-temp)
              ;; changes best-path iff it's better or passes transition test
              [new-path new-length] (if (or (< mutated-length current-length)
                                            (< (rand) transition-probability))
                                      [mutated-path mutated-length]
                                      [current-path current-length])]
              (recur (dec step) new-path new-length))))))

(defn show-debug
  [the-canvas current-temp current-step cooling-factor current-path]
  (println "#-------------------------\n"
           "Iteration:      " current-step "\n"
           "Current temp:   " current-temp "\n"
           "Cooling factor: " cooling-factor "\n"
           "Total length:   " (calc-total-length current-path))
  (canvas/update the-canvas current-path)
  (Thread/sleep 500))

(defn geometric-cooling
  [start-temp end-temp max-steps]
  (Math/exp (/
              (Math/log (/ end-temp start-temp))
              (- max-steps 1.0))))

(defn simulated-annealing
  "solves TSP with simulated annealing"
  [start-temp end-temp max-steps path]
  (let [start-temp (float start-temp)
        end-temp  (if (= 0 end-temp)
                    (+ (float end-temp) Float/MIN_VALUE)
                    (float end-temp))
        cooling-factor (geometric-cooling start-temp end-temp max-steps)
        the-canvas (canvas/initialize path)]
    (loop [current-temp start-temp, current-step 0, current-path path]
      (show-debug the-canvas current-temp current-step cooling-factor current-path)
      (if (or (< current-temp end-temp)
              (< max-steps current-step))
        current-path
          (recur (* current-temp cooling-factor)
                 (inc current-step)
                 (mutate-path current-temp current-path))))))

(defn genetic-algo
  "solves TSP with genetic algorithm"
  [crossover-prob mutation-prob max-steps path]
  (let [encoded-path (encode-path path)]
  (loop [current-step 0 population (initialize-population path size)]
    (if (<= max-steps current-step)
      (decode-path (get-best-item population))
      (recur (inc current-step)))))


