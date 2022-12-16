(ns aoc2022.day13
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))


(defn parse-int [s] (Integer/parseInt s))

(defn parse-lines [lines] (if (empty? lines) nil (let [pair (take 2 lines) rst (drop 3 lines)] (cons (map load-string pair) (parse-lines rst)))))

(defn cmp-int [a b] (cond (< a b) -1 (> a b) 1 :else 0))

(declare cmp)
(defn cmp-vec [a b] 
  (cond 
    (and (empty? a) (empty? b)) 0
    (empty? a) -1
    (empty? b) 1
    :else (let [c (cmp (first a) (first b))] (if (= c 0) (recur (rest a) (rest b)) c))))

(defn cmp [a b] 
  (cond
    (and (int? a) (int? b)) (cmp-int a b)
    (int? a) (cmp-vec [a] b)
    (int? b) (cmp-vec a [b])
    :else (cmp-vec a b)))
         
(defn sum-right [results] 
  (let [ires (map-indexed vector results) valid (filter #(get %1 1) ires) idx (map #(inc (first %1)) valid)]
    (reduce + idx)))    

(defn f1 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    (map #(apply cmp %1))
    (map #(<= %1 0))
    sum-right))


(defn decoder-key [sorted-list] 
  (let [m (apply hash-map (apply concat (map-indexed (fn [i c] [c (inc i)]) sorted-list)))] 
    (* (get m [[2]]) (get m [[6]]))))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    (apply concat)
    (concat [[[2]] [[6]]])
    (sort cmp)
    decoder-key))


(defn -main [& args]
  (println [(f1 "inputs/d13-1.txt") (f2 "inputs/d13-1.txt")]))
