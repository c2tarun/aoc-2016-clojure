(ns aoc-2016-clojure.day2 (:require [clojure.string :as str]))

(def keypad [[:e :e :e :e :e :e]
             [:e :e :e 1 :e :e :e]
             [:e :e  2 3 4  :e :e]
             [:e 5   6 7 8   9 :e]
             [:e :e \A \B \C :e :e]
             [:e :e :e \D :e :e :e]
             [:e :e :e :e :e :e :e]])

(defn get-code
  "Returns the keypad value of the given coordinate."
  [coord]
  (get-in keypad coord))

(def action {\U [-1 0]
             \D [1 0]
             \L [0 -1]
             \R [0 1]})

(defn shift [[r c] dir]
  (let [[ar ac] (get action dir)]
    [(+ r ar) (+ c ac)]))

(defn move [[r c] dir]
  (let [new-coord (shift [r c] dir)]
    (if (= :e (get-code new-coord))
      [r c]
      new-coord)))

(def input (slurp "resources/day2-input.txt"))

(defn find-button
  ([line] (find-button [1 1] line))
  ([start line]
   (reduce (fn [coord dir] (move coord dir)) start line)))

(defn clean-input [input]
  (as-> input x
    (str/split x #"\n")
    (map str/trim x)
    (map seq x)))

(loop [input (clean-input input) current [3 1] final-code []]
  (if (empty? input)
    final-code
    (let [next (find-button current (first input))]
      (recur (rest input) next (conj final-code (get-code next))))))