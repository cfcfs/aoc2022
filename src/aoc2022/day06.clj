(ns aoc2022.day06)


(defn first-marker
  ([s] (first-marker s 4))
  ([s n]
   (->>
    (partition n 1 s)
    (map-indexed vector)
    (filter #(let [[_ vals] %1] (= n (count (set vals)))))
    first
    first
    (+ n))))


(defn f1 [fname]
  (first-marker (slurp fname)))

(defn f2 [fname]
  (first-marker (slurp fname) 14))

(defn -main [& args]
  (println [(f1 "inputs/d06-1.txt") (f2 "inputs/d06-1.txt")]))

