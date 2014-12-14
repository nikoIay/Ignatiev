(ns ignatiev-lab1.core-test
  (:require [clojure.test :refer :all]
            [ignatiev-lab1.core :refer :all]))

(deftest distance-test
  (testing "Distance"
    (is (= (distance [0 -1 0] [1 1 2] true) 9)))
  (testing "Zero distance"
    (is (= (distance [1 -1 1 5][1 -1 1 5] true) 0))))

(deftest init-potential-test
  (testing "Potential distance"
    (is (<
         (init-potential [0 0] [[5 4][5 5][5 6]] 2 true)
         (init-potential [1 0] [[5 4][5 5][5 6]] 2 true)
         (init-potential [2 0] [[5 4][5 5][5 6]] 2 true)
         (init-potential [3 0] [[5 4][5 5][5 6]] 2 true)
         (init-potential [4 0] [[5 4][5 5][5 6]] 2) true)))
  (testing "Potential radius"
    (is (<
         (init-potential [0 0] [[5 4][5 5][5 6]] 2 true)
         (init-potential [0 0] [[5 4][5 5][5 6]] 3 true)
         (init-potential [0 0] [[5 4][5 5][5 6]] 4 true)))))

(deftest cores-test
  (testing "Clusters number"
    (is (= (count (cores [[-1 0][7 7][0 0][7 8][1 1][1 -1]] 3 true)) 2))))
