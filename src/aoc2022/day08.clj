(ns aoc2022.day08
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string])
  (:require [clojure.set :refer [union]]))

(defn parse-int [c] (Integer/parseInt (str c)))

(defn build-cells [lines] 
  (apply merge (flatten (map-indexed (fn [y line] (map-indexed (fn [x c] {{:x x :y y} (parse-int c)}) line)) lines))))

(defn build-board [lines] {:x (count (first lines)) :y (count lines) :cells (build-cells lines)})

(defn visible-cells 
  ([cells pos-seq] (visible-cells cells pos-seq (- 1)))
  ([cells pos-seq lower-bound]
   (if (empty? pos-seq) #{}
     (let [v (get cells (first pos-seq))] 
       (if (<= v lower-bound) 
         (visible-cells cells (rest pos-seq) lower-bound)
         (union #{(first pos-seq)} (visible-cells cells (rest pos-seq) v)))))))

(defn generate-seqs [X Y]
  (let [basex (map (fn [y] (map (fn [x] {:x x :y y}) (range X))) (range Y))
        basey (map (fn [x] (map (fn [y] {:x x :y y}) (range Y))) (range X))
        reversex (map reverse basex)
        reversey (map reverse basey)]
      (union basex basey reversex reversey)))

(defn all-visible [board] (apply union (map (fn [s] (visible-cells (:cells board) s)) (generate-seqs (:x board) (:y board)))))


(defn f1 [lines]
  (->>
    (common/get-lines lines)
    build-board
    all-visible
    count))
    
(defn block-distance [cells h coord-seq default] 
  (let [f (fn [c] (< (get cells c) h)) 
        dropped (count (take-while f coord-seq))
        blocker (first (drop dropped coord-seq))]
    (cond 
      (nil? blocker) default 
      :else (+ 1 dropped))))

(defn scenic-value [board cell]
  (let [cells (:cells board) x (:x cell) y (:y cell) v (get cells cell)  maxx (:x board) maxy (:y board)] 
    (*
     (block-distance cells v (map (fn [z] {:x z :y y}) (range (+ x 1) maxx)) (- maxx x 1))
     (block-distance cells v (map (fn [z] {:x z :y y}) (range (- x 1) -1 -1)) x)
     (block-distance cells v (map (fn [z] {:x x :y z}) (range (+ y 1) maxy)) (- maxy y 1))
     (block-distance cells v (map (fn [z] {:x x :y z}) (range (- y 1) -1 -1)) y))))

(defn best-scenic-value [board] (apply max (map (fn [c] (scenic-value board c)) (keys (:cells board)))))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    build-board
    best-scenic-value))

(defn -main [& args]
  (println [(f1 "inputs/d08-1.txt") (f2 "inputs/d08-1.txt")]))
