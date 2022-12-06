(ns aoc2022.day05
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))


(defn count-occurences [sub str] (count (filter (set sub) str)))

(defn get-chars [positions str] (map #(get str %1 ) positions))

(defn add-to-stack [stacks [idx c]] (assoc stacks idx (cons c (get stacks idx))))

(defn remove-from-stack [stacks idx] (assoc stacks idx (drop 1 (get stacks idx))))

(defn top-of-stack [stacks idx] (first (get stacks idx)))

(defn top-of-stacks [stacks] (map-indexed (fn [idx _] (top-of-stack stacks idx)) stacks))

(defn add-to-stack-n [stacks [idx cs]] (assoc stacks idx (concat cs (get stacks idx))))

(defn remove-from-stack-n [stacks idx cnt] (assoc stacks idx (drop cnt (get stacks idx))))

(defn top-of-stack-n [stacks idx cnt] (take cnt (get stacks idx)))

(defn build-stacks [nr-stacks ops]
  (let [stacks (vec (map (fn [_] nil) (range 0 nr-stacks)))]
    (reduce add-to-stack stacks ops)))

(defn parse-stacks [lines] 
  (let [max-size (apply max (map count lines))]
       (->>
         (map #(get-chars (range 1 max-size 4) %1) lines)
         ; (vec [(map (fn [_] nil) (range 0 (/ max-size 4)) (map #(map-indexed vector %1)))])
         (map #(map-indexed vector %1))
         flatten
         (partition 2)
         (filter #(let [[_ c] (vec %1)] (not (== (int c) (int \space)))))
         (build-stacks (/ max-size 4)))))

(defn parse-movements [lines] 
  (map #(let [[q f t] (re-seq #"\d+" %1)] {:qty (Integer/parseInt q) :from (Integer/parseInt f) :to (Integer/parseInt t)}) lines))

(defn parse-lines [lines]
  [ (parse-stacks (reverse (take-while #(string/includes? %1 "[") lines)))
    (parse-movements (drop-while #(not (string/includes? %1 "move")) lines))])

(defn apply-move [stacks move]
    (if (= 0 (:qty move)) 
        stacks 
        (let [v (top-of-stack stacks (dec (:from move)))]
          (apply-move 
            (add-to-stack (remove-from-stack stacks (dec (:from move))) [(dec (:to move)) v])
            (assoc move :qty (dec (:qty move)))))))
  

(defn apply-move-2 [stacks move]
  (let [v (top-of-stack-n stacks (dec (:from move)) (:qty move))]
      (add-to-stack-n (remove-from-stack-n stacks (dec (:from move)) (:qty move)) [(dec (:to move)) v])))

(def data (parse-lines (common/get-lines "inputs/d05-0.txt")))
(def stacks (get data 0))
(def moves (get data 1))

(defn f1 [fname] 
  (->>
    (common/get-lines fname)
    parse-lines
    (apply (fn [stacks moves] (reduce apply-move stacks moves)))
    top-of-stacks
    (string/join "")))

(defn f2 [fname] 
  (->>
    (common/get-lines fname)
    parse-lines
    (apply (fn [stacks moves] (reduce apply-move-2 stacks moves)))
    top-of-stacks
    (string/join "")))

;(f1 "inputs/d05-1.txt")
(f2 "inputs/d05-0.txt")

(defn -main [& args]
  (println [(f1 "inputs/d05-1.txt") (f2 "inputs/d05-1.txt")]))
