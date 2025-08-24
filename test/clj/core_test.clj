(ns clj.core-test
  (:require [clojure.test :refer :all]
            [clj.core :refer :all]))

(deftest test-add-nodes-path
  (let [empty-dir [(map->Node {:name "Root" :type :storage})]
        path [(map->Node {:name "B" :type :storage})
              (map->Node {:name "C" :type :storage})
              (map->Node {:name "Stream" :type :stream})]]
    (testing "One path"
      (let [expected [(map->Node {:name "Root" :type :storage :child 1})
                      (map->Node {:name "B" :type :storage :child 2})
                      (map->Node {:name "C" :type :storage :child 3})
                      (map->Node {:name "Stream" :type :stream :child nil})]
            [dir last-id] (add-nodes-path empty-dir path)]
        (is (= 3 last-id))
        (is (= expected dir))))
    (testing "Two path"
      (let [path2 [(map->Node {:name "B" :type :storage})
                   (map->Node {:name "D" :type :storage})
                   (map->Node {:name "AStream" :type :stream})]
            expected [(map->Node {:name "Root" :type :storage :child 1})
                      (map->Node {:name "B" :type :storage :child 2})
                      (map->Node {:name "C" :type :storage :child 3 :right 4})
                      (map->Node {:name "Stream" :type :stream :child nil})
                      (map->Node {:name "D" :type :storage :child 5})
                      (map->Node {:name "AStream" :type :stream :child nil})]
            [dir* _] (add-nodes-path empty-dir path)
            [dir last-id] (add-nodes-path dir* path2)]
        (is (= 5 last-id))
        (is (= expected dir))))
    (testing "Two path (left)"
      (let [path2 [(map->Node {:name "A" :type :stream})]
            expected [(map->Node {:name "Root" :type :storage :child 1})
                      (map->Node {:name "B" :type :storage :child 2 :left 4})
                      (map->Node {:name "C" :type :storage :child 3})
                      (map->Node {:name "Stream" :type :stream :child nil})
                      (map->Node {:name "A" :type :stream})]
            [dir* _] (add-nodes-path empty-dir path)
            [dir last-id] (add-nodes-path dir* path2)]
        (is (= 4 last-id))
        (is (= expected dir))))))

(deftest test-make-proto-fat
  (let [[starts fat] (make-proto-fat [1 2 3])]
    (is (= [0 1 3] starts))
    (is (= [ENDOFCHAIN 2 ENDOFCHAIN 4 5 ENDOFCHAIN] fat))))

(deftest test-calc-num-sector
  (is (= 1 (calc-num-sector (- SectorSize 1))))
  (is (= 1 (calc-num-sector SectorSize)))
  (is (= 2 (calc-num-sector (+ SectorSize 1)))))

(deftest test-make-fat
  (testing "Simple"
    (let [fat (make-fat (range 127))
          tail (last fat)]
      (is (= 128 (count fat)))
      (is (= FATSEC tail))))
  (testing "Expand"
    (let [fat (vec (make-fat (range 128)))
          padsec (subvec fat 128 254)
          fatsec (subvec fat 254)]
      (is (= 256 (count fat)))
      (is (= (vec (long-array 126 FREESEC)) padsec))
      (is (= (vec (long-array 2 FATSEC)) fatsec))))
  (testing "300"
    (let [fat (vec (make-fat (range 300)))
          padsec (subvec fat 300 381)
          fatsec (subvec fat 381)]
      (is (= 384 (count fat)))
      (is (= (vec (long-array 81 FREESEC)) padsec))
      (is (= (vec (long-array 3 FATSEC)) fatsec)))))
