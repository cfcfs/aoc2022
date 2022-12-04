(ns aoc2022.day04
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))

(defn parse-interval [line]
  (let [[v1 v2] (string/split line #"-")]
    {:min (Integer/parseInt v1) :max (Integer/parseInt v2)}))

(defn parse-intervals [line]
  (let [[& ints] (string/split line #",")]
    (map parse-interval ints)))

(defn interval-contains [a b]
  (and (<= (:min a) (:min b)) (>= (:max a) (:max b))))

(defn interval-intersects [a b]
  (not (or (< (:max a) (:min b)) (> (:min a) (:max b)))))

(defn line-match-1 [intervals]
  (let [[a b] intervals] (or (interval-contains a b) (interval-contains b a))))

(defn line-match-2 [intervals]
  (let [[a b] intervals]  (interval-intersects a b)))

(defn f1 [fname] 
  (->>
    (common/get-lines fname)
    (map parse-intervals)
    (map vec)
    (filter line-match-1)
    (count)))
    
(defn f2 [fname] 
  (->>
    (common/get-lines fname)
    (map parse-intervals)
    (map vec)
    (filter line-match-2)
    (count)))


(defn -main [& args]
  (println [(f1 "inputs/d04-1.txt") (f2 "inputs/d04-1.txt")]))
