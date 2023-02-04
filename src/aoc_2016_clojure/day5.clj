(ns aoc-2016-clojure.day5
  (:require [clj-commons.digest :as digest]))

(defn md5 [input] (digest/md5 input))
(defn starts-with-5-0? [input]
  (= `(\0 \0 \0 \0 \0) (take 5 input)))

(defn find-next-index
  ([start input]
   (loop [start start]
     (if (starts-with-5-0? (md5 (str input start)))
       start
       (recur (inc start)))))
  ([input] (find-next-index 0 input)))

(defn find-password-part1 [word]
  (loop [word word next 0 pwd []]
    (if (= (count pwd) 8)
      pwd
      (let [next (find-next-index next word)
            ch (last (take 6 (md5 (str word next))))]
        (recur word (inc next) (conj pwd ch))))))

(defn find-password-part2 [word]
  (loop [word word next 0 pwd {}]
    (if (= (count pwd) 8)
      pwd
      (let [next (find-next-index next word)
            hash (md5 (str word next))
            idx (nth hash 5)
            ch (nth hash 6)]
        (if (and (Character/isDigit idx)
                 (<= (Character/digit idx 10) 7)
                 (= nil (pwd idx)))
          (recur word (inc next) (assoc pwd idx ch))
          (recur word (inc next) pwd))))))