(ns aoc2022.core
  (:gen-class)
  (:require [clojure.string :as string]))
  

(defn get-lines [fname] (string/split-lines (slurp fname)))

