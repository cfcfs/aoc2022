(ns aoc2022.day03 
  (:require [aoc2022.core :as common])
  (:require [clojure.set :as set])
  (:require [clojure.string :as string]))


(def upper-case-chars (set (map char (range 65 91))))

(defn item-prio [item] 
  (let [[c] (string/upper-case item)] 
    (+ 1
       (if (upper-case-chars item) 26 0)
       (- (int c) (int \A)))))

(defn group-prio [& args]
  (->>
    (vec args)
    (#(map set %1))
    (apply set/intersection)
    vec
    first
    item-prio))

(defn split-string [s] (let [half (/ (count s) 2)] (vec [(subs s 0 half) (subs s half)])))

(defn f1 [fname]
  ( ->>
    (common/get-lines fname)
    (map split-string)
    (map #(apply group-prio %1))
    (reduce +)))
  
(defn f2 [fname]
  ( ->>
    (common/get-lines fname)
    (partition 3)
    (map #(apply group-prio %1))
    (reduce +)))

(defn -main
  [& args]
  (println [(f1 "inputs/d03-1.txt") (f2 "inputs/d03-1.txt")]))
