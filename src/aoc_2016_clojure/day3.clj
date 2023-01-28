(ns aoc-2016-clojure.day3 (:require [clojure.string :as str]))

(defn is-triangle? [sides]
  (let [sides (sort sides)]
    (> (reduce + (take 2 sides)) (last sides))))

(defn to-int [str-array]
  (mapv #(read-string %) str-array))

(defn clean-input [filename]
  (let [input (slurp filename)]
    (as-> input x
      (str/split x #"\n")
      (map str/trim x)
      (map #(str/split % #"\s\s*") x)
      (map to-int x))))

;; Part 1
(as-> (clean-input "resources/day3-input.txt") x
  (filter is-triangle? x)
  (count x))

;; Part 2
(defn count-triangles-in-grid [[r1 r2 r3]]
  (as-> (range 3) x
    (map (fn [x] [(get r1 x) (get r2 x) (get r3 x)]) x)
    (filter is-triangle? x)
    (count x)))

(as-> (clean-input "resources/day3-input.txt") x
  (partition 3 x)
  (map #(count-triangles-in-grid %) x)
  (reduce + x))