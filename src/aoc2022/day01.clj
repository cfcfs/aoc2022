(ns aoc2022.day01 
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]) 
  (:gen-class))


(defn group-lines [lines] 
  (reduce 
    #(if (string/blank? %2)
       (cons '() %1)
       (cons (cons %2 (first %1)) (next %1)))
    '(()) lines))

(defn sum-groups [groups] (map #(reduce + 0 (map read-string %1)) groups))

(defn sum-top [top groups] (reduce + (take-last top (sort groups))))

(defn f1
  [fname]
  (->>
    (common/get-lines fname)
    group-lines
    sum-groups
    (apply max)))
    
(defn f2
  [fname]
  (->>
    (common/get-lines fname)
    group-lines
    sum-groups
    (sum-top 3)))
    

(def part1 (f1 "inputs/d01-1.txt"))
(def part2 (f2 "inputs/d01-1.txt"))

(defn -main
  [& args]
  (println [part1 part2]))
