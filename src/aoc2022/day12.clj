(ns aoc2022.day12
  (:require [aoc2022.core :as common]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(def moves {:up {:x 0 :y (- 1)} :down {:x 0 :y 1} :left {:x (- 1) :y 0} :right {:x 1 :y 0}})

(defn add-coords [p v] {:x (+ (:x p) (:x v)) :y (+ (:y p) (:y v))})

(defn parse-cell [board y idx_char] 
  (let [h (:heights board) [x c] idx_char] 
    (cond 
      (= c \S) (parse-cell (assoc board :start {:x x :y y}) y [x \a])
      (= c \E) (parse-cell (assoc board :end {:x x :y y}) y [x \z])
      :else (assoc board :heights (assoc h {:x x :y y} (int c))))))

(defn parse-line [board idx-line] 
  (let [[y line] idx-line idx-chars (map-indexed vector line)]
    (reduce (fn [b ic] (parse-cell b y ic)) board idx-chars)))

(defn parse-lines [lines] 
  (let [board {:start nil :end nil :heights {} :dist {}}]
    (reduce parse-line board (map-indexed vector lines))))

(defn neigh [board cell] 
  (filter #(get (:heights board) %1) (map (partial add-coords cell) (vals moves))))

(defn can-walk? [board c1 c2] (let [h (:heights board)] (>= (+ 1 (get h c1)) (get h c2))))

(defn set-distance [board cell dist] (let [d (:dist board)] (assoc board :dist (assoc d cell dist))))
(defn set-distances [board cells dist] (reduce (fn [b c] (set-distance b c dist)) board cells))

(defn build-distances 
  ([board] (build-distances (assoc-in board [:dist (:end board)] 0) (conj (queue) (:end board)))) 
  ([board q] (if (empty? q) board 
               (let [c (peek q) h (:heights board) d (:dist board)
                     cn (filter (fn [x] (and (can-walk? board x c) (nil? (get d x)))) (neigh board c))]
                 (recur (set-distances board cn (inc (get d c))) (into (pop q) cn))))))

(defn path-size [board] (get (:dist board) (:start board)))

(defn best-path-size [board] 
  (let [h (:heights board) d (:dist board) sp (filter (fn [x] (= 97 (h x))) (keys h)) dists (map #(d %1) sp)] 
    (apply min (filter some? dists))))

(defn f1 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    build-distances
    path-size))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    parse-lines
    build-distances
    best-path-size))

(defn -main [& args]
  (println [(f1 "inputs/d12-1.txt") (f2 "inputs/d12-1.txt")]))
