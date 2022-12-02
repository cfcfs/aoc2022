(ns aoc2022.day02
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string :refer [split]]) 
  (:gen-class))


(def mapABC {"A" :rock, "B" :paper, "C" :scissor})
(def mapXYZ {"X" :rock, "Y" :paper, "Z" :scissor})

(def rps-outcome (hash-map
                  '(:paper :rock) 0
                  '(:paper :scissor) 6
                  '(:rock :paper) 6
                  '(:rock :scissor) 0
                  '(:scissor :paper) 0
                  '(:scissor :rock) 6))


(defn decode 
  ([line] (decode (merge mapABC mapXYZ) line))
  ([mapper line] (map #(mapper %1) (split line #" "))))

(defn score [values] 
  (let [[a b] values] 
    ( + 
      ({:rock 1, :paper 2, :scissor 3} b) 
      (rps-outcome  (list a b) 3))))


(defn f1
  [fname]
  (->>
    (common/get-lines fname)
    (map decode)
    (map score)
    (reduce +)))
    


(def mapXYZ2 {"X" :lose, "Y" :draw, "Z" :win})

(def rps-move (hash-map
               '(:paper :win) :scissor
               '(:paper :lose) :rock
               '(:rock :win) :paper
               '(:rock :lose) :scissor
               '(:scissor :win) :rock
               '(:scissor :lose) :paper))

(defn f2
  [fname]
  (->>
    (common/get-lines fname)
    (map #(decode (merge mapABC mapXYZ2) %1))
    (map #(let [[a b] %1] (list a (rps-move (list a b) a))))
    (map score)
    (reduce +)))
    

(defn -main
  [& args]
  (println [(f1 "inputs/d02-1.txt") (f2 "inputs/d02-1.txt")]))
