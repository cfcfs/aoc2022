(ns aoc2022.day09
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))


(defn parse-int [s] (Integer/parseInt s))

(def char-to-mov {"U" :up "D" :down "R" :right "L" :left})
(def mov-to-coord {:up {:x 0 :y (- 1)} :down {:x 0 :y 1} :left {:x (- 1) :y 0} :right {:x 1 :y 0}})
(def initial-board {:head {:x 0 :y 0} :tail {:x 0 :y 0} :track #{}})

(defn add-coords [p v] {:x (+ (:x p) (:x v)) :y (+ (:y p) (:y v))})
(defn sub-coord [a b] {:x (- (:x a) (:x b)) :y (- (:y a) (:y b))})
(defn norm-coord [p] (let [f (fn [x] (if (= x 0) 0 (/ x (abs x))))] {:x (f (:x p)) :y (f (:y p))}))

(defn man-dist [a b] (+ (abs (- (:x a) (:x b))) (abs (- (:y a) (:y b)))))
(defn aligned? [a b] (or (= (:x a) (:x b)) (= (:y a) (:y b))))

(defn parse-movements [lines]
  (->>
    (map #(string/split %1 #" ") lines)
    (map #(let [[c r] %1] (repeat (parse-int r) (char-to-mov c))))
    flatten))

(defn move-head [board move] (assoc board :head (add-coords (:head board) (mov-to-coord move))))
(defn move-tail [board] 
  (let [h (:head board) t (:tail board) aligned (aligned? h t) md (man-dist h t) mv (norm-coord (sub-coord h t))]
    (if (> md (if aligned 1 2)) (assoc board :tail (add-coords t mv)) board))) 

(defn move [board move] 
  (let [moved-board (move-tail (move-head board move)) 
        track (:track board)] 
    (assoc moved-board :track (conj track (:tail moved-board)))))

(defn f1 [lines]
  (->>
    (common/get-lines lines)
    (parse-movements)
    ; (take 10)
    (reduce move initial-board)
    :track
    count))
    
(defn build-initial-board [knots] {:knots (vec (repeat knots {:x 0 :y 0})) :track #{} :knot-count knots})

(defn move-head2 [board move] (assoc board :knots (assoc (:knots board) 0 (add-coords (first (:knots board)) (mov-to-coord move)))))
(defn move-tail2 [board hidx tidx] 
  (let [k (:knots board) h (get k hidx) t (get k tidx) aligned (aligned? h t) md (man-dist h t) mv (norm-coord (sub-coord h t))]
    (if (> md (if aligned 1 2)) (assoc board :knots (assoc k tidx (add-coords t mv))) board))) 

(defn move2 [board move] 
  (let [moved-head (move-head2 board move) 
        moved-board (reduce (fn [b x] (move-tail2 b x (+ x 1))) moved-head (range (- (:knot-count board) 1))) 
        track (:track board)] 
    (assoc moved-board :track (conj track (last (:knots moved-board))))))


(defn f2 [lines]
  (->>
    (common/get-lines lines)
    (parse-movements)
    (reduce move2 (build-initial-board 10))
    :track
    count))


(defn -main [& args]
  (println [(f1 "inputs/d09-1.txt") (f2 "inputs/d09-1.txt")]))
