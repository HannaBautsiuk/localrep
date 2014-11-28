(ns lab1.core-test
  (:require [clj-http.client :as client])
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:use clojure.test
        lab1.core))

(deftest test404
  (testing "Returned http code should be 404 (return value bad)"
    (let [response (client/get "https://www.google.ru/img1hp?hl=ru&tab=wi&gws_rd=ssl" {:throw-exceptions false})
          stat (lab1.core/getStatusDesc response)]
      (is (= stat "bad")))))

(deftest testLinks
  (testing "Incorrect number of links"
     (let [html (first (line-seq (clojure.java.io/reader "forTest.html")))]
      (is (= 20 (count (lab1.core/get-all-links html "https://www.google.ru/")))))))