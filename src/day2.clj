(ns day2 (:require [clojure.string :as str]))

(def keypad [[1 2 3] [4 5 6] [7 8 9]])

(defn move
  "Method returns the coordinate based on current coordinates and move direction.
   If move step cannot be performed it'll return the same coordinate."
  ([[r c] dir]
   (move [r c] dir (count (get keypad 0))))
  ([[r c] dir size]
   (cond
     (= dir \U) [(max (dec r) 0) c]
     (= dir \D) [(min (inc r) (dec size)) c]
     (= dir \L) [r (max (dec c) 0)]
     (= dir \R) [r (min (inc c) (dec size))])))

(defn get-code
  "Returns the keypad value of the given coordinate."
  [coord]
  (get-in keypad coord))

(def input (slurp "resources/day2-input.txt"))

(defn find-button
  ([line] (find-button [1 1] line))
  ([start line]
   (reduce (fn [coord dir] (move coord dir)) start line)))

(find-button `(\U \L \L))

(defn clean-input [input]
  (as-> input x
    (str/split x #"\n")
    (map str/trim x)
    (map seq x)))

(loop [input (clean-input input) current [1 1] final-code []]
  (if (empty? input)
    final-code
    (let [next (find-button current (first input))]
      (recur (rest input) next (conj final-code (get-code next))))))