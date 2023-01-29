(ns aoc-2016-clojure.day4 (:require [clojure.string :as str]))

(def alphabet-scores
  (into {} (map-indexed (fn [i v] {v (- 26 i)}) (seq "abcdefghijklmnopqrstuvwxyz"))))

(defn split-secid-n-checksum [s]
  (let [splits (str/split s #"\[")]
    {:secid (first splits) :checksum (first (str/split (last splits) #"\]"))}))

(split-secid-n-checksum "123[abcd]")

(defn shift-by-n [ch n]
  (let [current (- (int ch) (int \a)) new-index (+ (int \a) (mod (+ current n) 26))]
    (char new-index)))

(defn decrypt-name [name secid]
  (apply str (map (fn [x] (if (= x \-) \  (shift-by-n x (mod secid 26)))) (seq name))))


(defn parse-room [name]
  (let [splits (str/split name #"-") secid-n-checksum (split-secid-n-checksum (last splits))]
    {:e-name (apply str (take (dec (count splits)) splits))
     :str-name (str/join "-" (take (dec (count splits)) splits))
     :secid (:secid secid-n-checksum)
     :checksum (:checksum secid-n-checksum)
     :original name
     :decrypted-name (decrypt-name
                      (str/join "-" (take (dec (count splits)) splits))
                      (read-string (:secid secid-n-checksum)))}))

(defn priority-order [ch1 ch2 freq]
  (cond
    (> (get freq ch1) (get freq ch2)) -1
    (< (get freq ch1) (get freq ch2)) 1
    :else (compare (get alphabet-scores ch2) (get alphabet-scores ch1))))

(defn calculate-checksum [chars]
  (let [freq (frequencies chars)]
    (apply str (take 5 (sort #(priority-order %1 %2 freq) (keys freq))))))

(defn clean-input [filename]
  (str/split (slurp filename) #"\n"))

(defn is-real-room? [parsed-room-info]
  (= (calculate-checksum (:e-name parsed-room-info)) (:checksum parsed-room-info)))

;; Part 1
(as-> "resources/day4-input.txt" x
  (clean-input x)
  (map parse-room x)
  (filter is-real-room? x)
  (map (fn [s] (:secid s)) x)
  (map read-string x)
  (reduce + x))

;; Part 2
(as-> "resources/day4-input.txt" x
  (clean-input x)
  (map parse-room x)
  (filter (fn [s] (.contains (:decrypted-name s) "north")) x))