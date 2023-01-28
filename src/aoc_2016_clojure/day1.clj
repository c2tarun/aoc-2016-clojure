(ns aoc-2016-clojure.day1 (:require [clojure.string :as str]))

(def input (slurp "resources/day1-input.txt"))

(def move-data {:north {\L [0 -1] \R [0 1]}
                :south {\L [0 1] \R [0 -1]}
                :east {\L [1 0] \R [-1 0]}
                :west {\L [-1 0] \R [1 0]}})

(defn move [r c facing rorl steps]
  (let [op (get-in move-data [facing rorl])]
    [(+ r (* steps (get op 0))) (+ c (* steps (get op 1)))]))

(move 0 0 :north \R 15)

(defn get-track [r1 c1 r2 c2]
  (into #{}
        (remove #{[r1 c1]}
                (if (= r1 r2)
                  (into #{} (map (fn [x] [r1 x]) (range (min c1 c2) (inc (max c1 c2)))))
                  (into #{} (map (fn [x] [x c1]) (range (min r1 r2) (inc (max r1 r2)))))))))

(defn turn-direction [current rorl]
  (cond
    (= current :north) (if (= rorl \R) :east :west)
    (= current :south) (if (= rorl \R) :west :east)
    (= current :east) (if (= rorl \R) :south :north)
    (= current :west) (if (= rorl \R) :north :south)))

(defn direction [dir]
  [(first (seq dir)) (Integer/parseInt (apply str (rest (seq dir))))])

(defn parse-directions [input]
  (map #(direction %) (str/split input #", ")))

(defn final-cordinate [input]
  (loop [dirs (parse-directions input)
         r 0  ;; starts from origin
         c 0
         facing :north  ;; facing north
         visited #{}]
    (if (empty? dirs)
      [r c]
      (let [dir (first dirs)
            [nr nc] (move r c facing (get dir 0) (get dir 1))
            new-facing (turn-direction facing (dir 0))
            track (get-track r c nr nc)
            revisited (clojure.set/intersection visited track)]
        (if (not-empty revisited)
          revisited
          (recur
           (rest dirs)  ;; remaining instructions
           nr  ;; new row
           nc  ;; new col
           new-facing  ;; Now facing towards
           (into #{} (clojure.set/union visited (get-track r c nr nc)))  ;; visited coordinates
           ))))))

(final-cordinate "R8, R4, R4, R8")
(final-cordinate input)