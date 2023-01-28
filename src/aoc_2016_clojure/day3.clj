(ns aoc-2016-clojure.day3 (:require [clojure.string :as str]))

(defn is-triangle? [sides]
  (let [sides (sort sides)]
    (> (reduce + (take 2 sides)) (last sides))))

(defn to-int [str-array]
  (mapv #(read-string %) str-array))

(to-int ["1" "2" "3"])

(def input (slurp "resources/day3-input.txt"))

(as-> input x
  (str/split x #"\n")
  (map str/trim x)
  (map #(str/split % #"\s\s*") x)
  (map to-int x)
  (filter is-triangle? x)
  (count x))
