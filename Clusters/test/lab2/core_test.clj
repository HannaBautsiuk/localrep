(ns lab2.core-test
  (:use clojure.test
        lab2.core))

(deftest getMethodTest
  (testing "getMethod function doesn't work"
    (is (= (lab2.core/getMethod "hamming") hamming))
    (is (= (lab2.core/getMethod "euclidian") euclidian))
    (is (nil? (lab2.core/getMethod "hammi")))))


(deftest findMaxTest
  (testing "find-max function doesn't work"
    (is (=   {:dst 4.5} (lab2.core/find-max `({:dst 1.3 } {:dst 4.5 } {:dst 4.1 } {:dst 2.3}))))))



(deftest hammingTest
  (testing "hamming function doesn't work"
    (is (=  2 (lab2.core/hamming `( 2 1 3 5) `( 2 5 6 5) )))))


(deftest euclidianTest
  (testing "euclidian function doesn't work"
    (is (=  25 (lab2.core/euclidian `( 2 1 3 5) `( 2 5 6 5) )))))