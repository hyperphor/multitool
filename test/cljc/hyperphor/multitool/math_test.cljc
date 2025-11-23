(ns hyperphor.multitool.math-test
  (:require [hyperphor.multitool.math :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])))

(deftest tensorize-test
  (testing "scalars"
    (is (= 5 (sut/+T 2 3))))
  (testing "vectors"
    (is (= [4 6 8] (sut/+T [0 1 2] [4 5 6])))
    (is (thrown? Error
                 (sut/+T [0 1 2 3] [4 5 6]))))
  (testing "mixed"
    (is (= [3 4 5] (sut/+T [0 1 2] 3))))
  (testing "higher ranks"
    (is (= [[5 12] [21 32]]
           (sut/*T [[1 2] [3 4]] [[5 6] [7 8]])))
    (is (= [[10 20] [30 40]] (sut/*T '[[1 2] [3 4]] 10))))
  (testing "T"
    (is (= 5 ((sut/T +) 2 3)))
    (is (= [[6 7] [14 15]]
           ((sut/T +) [[2 3] [9 10]] [4 5])))))


(deftest interpolate-test
  (is (= 5.0 (sut/interpolate 0 10 0.5)))
  (is (= 1.0 (sut/interpolate 0 10 0.1))))

(deftest interpolated-test
  (is (= [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
         (sut/interpolated 0 10 11))))

(deftest rescale-test
  (is (= 12.0 (sut/rescale 0.2 0 1 10 20))))

(deftest primes-test
  (is (= '(2 3 5 7 11 13 17 19 23 29)
         (take 10 sut/primes))))

(deftest prime-factor-test
  (is (= '(9091 11) (sut/prime-factors 100001)))
  ;; Too slow in cljs
  #_
  (testing "big number"
    (is (= '(27961 3803 3607 3541 101 3 3)
           (sut/prime-factors 1234567890123456789))))
  (testing "random number roundtrip"
    (doseq [i (range 10)]
      (let [j (rand-int 1000000)]
        (is (= j (apply * (sut/prime-factors j))))))))

(deftest radian-degree-test
  (is (= 180.0 (sut/r2d Math/PI)))
  (is (= (/ Math/PI 2) (sut/d2r 90))))

(deftest mean-test
  (is (= 5 (sut/mean (range 11)))))


