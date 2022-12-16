(ns aoc2022.day11
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))

(defn parse-int [s] (Integer/parseInt s))
(defn parse-bigint [s] (bigdec (Long/parseLong s)))

(defn get-lines [pattern lines] (filter some? (map #(if (re-find pattern %1) %1 nil) lines)))

(defn get-starting [line] (map parse-int (re-seq #"\d+" line)))

(defn get-oper [line] 
  (let [items (drop 3 (string/split (string/replace (string/trim line) #"old" "%1")  #" ")) [v1 op v2] items] 
    (load-string (str "#(" op " " v1 " " v2 ")"))))

(defn get-test [line] 
  (let [divisor (parse-int (re-find #"\d+" line))] (fn [x] (= 0 (mod x divisor))))) 

(defn get-divisor [line] (parse-int (re-find #"\d+" line)))

(defn parse-lines [lines] 
  (let [nr (count (get-lines #"Monkey" lines))
        starting (get-lines #"Start" lines)
        oper (get-lines #"Operation" lines)
        tst (get-lines #"Test" lines)
        iftrue (get-lines #"true" lines)
        iffalse (get-lines #"false" lines)]
    {
     :nr nr
     :items (apply vector (map get-starting starting))
     :oper (apply vector (map get-oper oper))
     :tst (apply vector (map get-test tst))
     :iftrue (apply vector (map #(parse-int (re-find #"\d+" %1)) iftrue))
     :iffalse (apply vector (map #(parse-int (re-find #"\d+" %1)) iffalse))
     :tranq  (fn [i] (mod i (apply * (map get-divisor tst))))
     :count (apply vector (repeat nr 0))}))

(defn play-item [board monkey item] 
  (let [op ((:oper board) monkey) 
        tranq (:tranq board)
        worry-level (tranq (op item))
        test-fn (fn [i] (((:tst board) monkey) i))
        next-fn (fn [i] (if (test-fn i) ((:iftrue board) monkey) ((:iffalse board) monkey)))
        next-monkey (next-fn worry-level)
        new-board (assoc-in board [:count monkey] (inc ((:count board) monkey)))]
    (assoc-in new-board [:items next-monkey] (concat ((:items board) next-monkey) [worry-level]))))

(defn play-monkey [board monkey] 
  (let [items (get (:items board) monkey) new-board (assoc-in board [:items monkey] [])] 
    (reduce (fn [b i] (play-item b monkey i)) new-board items)))

(defn play-turn [board] (reduce play-monkey board (range (:nr board))))
(defn play-turns [turns board] (reduce (fn [b _] (play-turn b)) board (range turns)))

(defn set-board-tranq-f1 [board] (assoc board :tranq (fn [x] (quot x 3))))

(defn f1 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    set-board-tranq-f1
    (play-turns 20)
    :count
    sort
    reverse
    (take 2)
    (apply *)))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    (play-turns 10000)
    :count
    sort
    reverse
    (take 2)
    (apply *)))

(defn -main [& args]
  (println [(f1 "inputs/d11-1.txt") (f2 "inputs/d11-1.txt")]))
