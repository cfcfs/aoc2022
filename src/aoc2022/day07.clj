(ns aoc2022.day07
  (:require [aoc2022.core :as common])
  (:require [clojure.string :as string]))

(defn parse-int [s] (Integer/parseInt s))

(defn is-command? [line] (string/starts-with? line "$"))
(defn get-outputs [lines] (take-while #(not (is-command? %1)) lines))

(defn group-instructions 
  ([lines] (group-instructions lines [])) 
  ([lines acc] 
   (if (empty? lines)
     (reverse acc)
     (group-instructions 
       (drop-while #(not (is-command? %1)) (rest lines))
       (cons {:cmd (first lines) :outputs (get-outputs (rest lines))} acc)))))

(defn parse-cmd [line] 
  (let [[t1 t2] (rest (string/split line #" "))] 
    (cond 
      (= t1 "ls") {:cmd :ls}
      (= t2 "/") {:cmd :root}
      (= t2 "..") {:cmd :parent}
      :else {:cmd :cd :arg t2})))

(defn parse-file [line] 
  (let [[t1 t2] (string/split line #" ")] 
    {:name t2, :size (if (= t1 "dir") 0 (parse-int t1)), :type (if (= t1 "dir") :dir :file)}))

(defn parse-instruction [cmd outputs] (merge (parse-cmd cmd) {:outputs (map parse-file outputs)}))

(defn set-files [fs stack files] 
  (if (empty? stack)
    (assoc fs :files (apply merge (map #(hash-map (:name %1) %1) files)))
    (let [[dir rst] [(last stack) (drop-last stack)]] 
      (assoc fs :files (assoc (:files fs) dir (set-files (get (:files fs) dir) rst files))))))

(defn apply-instruction [context inst]
  (let [[[stack root] [cmd arg files]] [context [(:cmd inst) (:arg inst) (:outputs inst)]]]
    (cond 
      (= cmd :parent) [(rest stack) root]
      (= cmd :root) [[] root]
      (= cmd :cd) [(cons arg stack) root]
      (= cmd :ls) [stack (set-files root stack files)])))

(defn build-fs [instructions] (reduce apply-instruction [[] {:name "/" :type :dir}] instructions))

(defn update-size [fs] 
  (if (= (:type fs) :file) fs 
    (let [updated-files (update-vals (:files fs) update-size)]
      (assoc fs :files updated-files :size (reduce + (map :size (vals updated-files)))))))
      
(defn filter-fs [f fs] (concat (filter f (list fs)) (flatten (map (partial filter-fs f) (vals (:files fs))))))


(defn f1 [lines]
  (->>
    (common/get-lines lines)
    group-instructions  ; -> [{:cmd "" :outputs: [""]} ...]
    (map #(parse-instruction (:cmd %1) (:outputs %1)))  ; -> [{:cmd :root | :cd | :cf | :ls  :output [{:name "" :type: file :size 0} ...]} ...]
    build-fs
    last
    update-size
    (filter-fs (fn [x] (and (= (:type x) :dir) (<= (:size x) 100000))))
    (map :size)
    (reduce +)))
    
(defn find-smallest [sorted-sizes] 
  (let [total (last sorted-sizes)]
    (first (filter (fn [x] (<= (- total x) 40000000)) sorted-sizes))))

(defn f2 [lines]
  (->>
    (common/get-lines lines)
    group-instructions  ; -> [{:cmd "" :outputs: [""]} ...]
    (map #(parse-instruction (:cmd %1) (:outputs %1)))  ; -> [{:cmd :cd-root | :cd-dir | :cf-parent | :ls  :output [{:name "" :type: file :size 0} ...]} ...]
    build-fs
    last
    update-size
    (filter-fs (fn [x] (= (:type x) :dir)))
    (map :size)
    sort
    find-smallest))


(defn -main [& args]
  (println [(f1 "inputs/d07-1.txt") (f2 "inputs/d07-1.txt")]))
