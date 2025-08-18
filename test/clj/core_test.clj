(ns clj.core-test
  (:require [clojure.test :refer :all]
            [clj.core :refer :all]))

(deftest test-add-path
  (let [[_ last-id] (add-path edir ["a" "b" "c"])]
    (is (= 3 last-id))))
