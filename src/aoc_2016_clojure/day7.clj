(ns aoc-2016-clojure.day7 (:require [clojure.string :as str]))

(defn is-abba-seq? [s]
  (let [fh (take 2 s) sh (take-last 2 s)]
    (if (and
         (= fh (reverse sh))
         (> (count (set fh)) 1)) true false)))

(defn is-aba-seq? [s]
  (let [fh (first s) sh (last s)]
    (if (and
         (not-empty s)
         (= fh sh)
         (> (count (set s)) 1)) true false)))

(defn does-have-abba? [s]
  (some true? (map is-abba-seq? (partition 4 1 s))))

(defn get-all-aba [s]
  (filter is-aba-seq? (partition 3 1 s)))

(get-all-aba "ababa")

(defn get-bab-for-aba [s]
  (let [fs (first s) sh (first (rest s))]
    (str sh fs sh)))

;; part 1
(defn check-ip-tls [ip]
  (let [splits (str/split ip #"(?=[\[\]])")
        supernet (filter #(not (str/starts-with? % "[")) splits)
        hypernet (filter #(str/starts-with? % "[") splits)]
    (and (some true? (map does-have-abba? supernet))
         (every? true? (map #(not (does-have-abba? %)) hypernet)))))

(defn is-any-bab-in-hypernet [babs hypernet]
  (loop [babs babs]
    (if (empty? babs)
      false
      (if (some true? (map #(str/includes? % (str (first babs))) hypernet))
        true
        (recur (rest babs))))))

;; part 2
(defn check-ip-ssl [ip]
  (let [splits (str/split ip #"(?=[\[\]])")
        supernet (filter #(not (str/starts-with? % "[")) splits) ;; supernet is outside of []
        hypernet (filter #(str/starts-with? % "[") splits)] ;; hypernet is within []
    (as-> supernet x
      (map get-all-aba x)
      (filter not-empty x)
      (apply concat x)
      (map get-bab-for-aba x)
      (is-any-bab-in-hypernet x hypernet))))

(def input (str/split (slurp "resources/day7-input.txt") #"\n"))

(count (filter true? (map check-ip-tls input)))
(count (filter true? (map check-ip-ssl input)))
