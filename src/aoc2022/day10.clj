(ns aoc2022.day10
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))


(defn parse-int [s] (Integer/parseInt s))

(def str-to-op {"addx" :addx "noop" :noop})

(defn parse-line [line] 
  (let [[op arg & _] (string/split line #" ")]
    {:op (str-to-op op) :arg1 (if (nil? arg) nil (parse-int arg))}))

(defn apply-instr [states instr]
  (let [x (first states) op  (:op instr) arg1 (:arg1 instr)]
    (cond 
      (= op :noop) (cons x states)
      (= op :addx) (cons (+ x arg1) (cons x states)))))

(defn signal-strengths [states]
  (let [istates (map-indexed vector states)] 
    (map (fn [p] (let [[x y] p] (* (+ 1 x) y))) istates)))

(defn f1 [lines]
  (->>
    (common/get-lines lines)
    (map parse-line)
    (reduce apply-instr (list 1))
    (reverse)
    signal-strengths
    (drop 19)
    (take-nth 40)
    (reduce +)))
    

(defn build-pixel [pc rx] 
  (let [x (mod pc 40) y (quot pc 40)] 
    {{:x x :y y} (< (abs (- x rx)) 2)}))

(defn build-image [states] 
  (let [s (map-indexed vector states)]
    (apply merge (map (fn [p] (build-pixel (get p 0) (get p 1))) s))))

(defn draw-row [pixels row] 
  (let [bits (map (fn [x] (get pixels {:x x :y row})) (range 40))]
    (println (apply str (map {true "#" false " "} bits)))))

(defn draw-image [pixels] 
  (map (fn [y] (draw-row pixels y)) (range 6)))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    (map parse-line)
    (reduce apply-instr (list 1))
    (reverse)
    build-image
    draw-image))


(defn -main [& args]
  (println [(f1 "inputs/d10-1.txt")])
  (apply println (f2 "inputs/d10-1.txt"))
  (f2 "inputs/d10-1.txt"))
