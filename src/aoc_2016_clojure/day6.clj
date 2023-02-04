(ns aoc-2016-clojure.day6 (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/day6-input.txt") #"\n"))

(defn get-freq-at-index [input idx]
  (frequencies (map #(nth % idx) input)))

(defn sorted-freq-at-index [input idx]
  (let [freq (get-freq-at-index input idx)]
    (into (sorted-map-by (fn [k1 k2]
                           (compare [(get freq k2) k2]
                                    [(get freq k1) k1])))
          freq)))

;; part 1
(let [count (count (first input))]
  (apply str
         (keys (map (fn [x] (first (sorted-freq-at-index input x)))
                    (range count)))))

;; part 2
(let [count (count (first input))]
  (apply str
         (keys (map (fn [x] (last (sorted-freq-at-index input x)))
                    (range count)))))