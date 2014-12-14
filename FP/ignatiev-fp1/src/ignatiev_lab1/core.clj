(ns ignatiev-lab1.core
  (:gen-class)
  (:use clojure.math.numeric-tower)
  (:use clojure.string)
  (import java.lang.Math)
)

(defn parse-data [string-source]
  (map (fn [line] (map (fn [str-point] (read-string str-point)) (split line #",")))
       (split string-source #"\n")))

(defn square-of-difference [a b]
  (* (- a b) (- a b)))

(defn distance [a b is-e]
  (if is-e
    (Math/sqrt (reduce + (map square-of-difference a b))) ; euclidean
    (reduce + (map (fn [a b] (if (= a b) 0 1)) a b))))    ; hamming

(defn two-point-potential [a-point b-point radius is-e]
  (expt
   2.71828182846
   (* -1 (/ 4 (* radius radius)) (distance a-point b-point is-e))))

(defn init-potential [point data radius is-e]
  (loop [result 0 point point data data radius radius is-e is-e]
    (if (seq data)
      (recur (+ result (two-point-potential point (first data) radius is-e)) point (rest data) radius is-e)
      result)))

(defn potential [core-entry point-entry bigger-radius is-e]
  (- (:potential point-entry)
     (* (:potential core-entry)
        (two-point-potential (:point core-entry) (:point point-entry) bigger-radius is-e))))

(defn update-points [points new-core bigger-radius is-e]
  (map (fn [el] {:point (:point el) :potential (potential new-core el bigger-radius is-e)}) points))

(defn medium-value [new-core cores radius greatest-potential is-e]
  (+ (/ (apply min (map (fn [el] (distance (:point new-core) el is-e)) cores)) radius)
     (/ (:potential new-core) greatest-potential)))

(defn reject-point [points new-core]
  (map (fn [el] {:point (:point el) :potential (if (= (:potential el) (:potential new-core)) 0 (:potential el))}) points))

(defn cores [data radius is-euclidean]
  (let [
        points (map (fn [el] {:point el :potential (init-potential el data radius is-euclidean)}) data)
        greatest-potential (apply max (map (fn [el] (:potential el)) points))
        high-border (* 0.5 greatest-potential)
        low-border (* 0.15 greatest-potential)
        bigger-radius (* radius 1.5)
       ]
  (loop [cores (list) points points is-e is-euclidean]
    (let [new-core (reduce (fn [a b] (if (> (:potential a) (:potential b)) a b)) points)]
      (if (> (:potential new-core) high-border)
        (recur (concat cores [(:point new-core)]) (update-points points new-core bigger-radius is-e) is-e)
        (if (< (:potential new-core) low-border)
          cores
          (if (>= (medium-value new-core cores radius greatest-potential is-e) 1)
            (recur (concat cores [(:point new-core)]) (update-points points new-core bigger-radius is-e) is-e)
            (recur cores (reject-point points new-core) is-e))))))))

(defn -main [& args]
  (let [
        source (slurp (nth args 0))
        radius (read-string (nth args 1))
        is-euclidean (read-string (nth args 2))
       ]
    (println (cores (parse-data source) radius (= is-euclidean 'e)))))
