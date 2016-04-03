(ns proto-bayes.suite-tests
  (:use clojure.test)
  (:require [proto-bayes.suite :as suite]))

(deftest single-datum
  (let [cookie-suite (-> #{:bowl1 :bowl2}
                           (suite/make-suite)
                           (suite/add-prior {:bowl1 (/ 1 2) :bowl2 (/ 1 2)})
                           (suite/add-likelihood (fn [d h]
                                                   (get-in {:bowl1 {:vanilla (/ 3 4) :chocolate (/ 1 4)}
                                                            :bowl2 {:vanilla (/ 1 2) :chocolate (/ 1 2)}} [h d]))))]
    (testing "vanilla datum"
      (let [actual-posterior (suite/posterior cookie-suite :vanilla)]
        (are [x y] (= x (suite/probability actual-posterior y))
          (/ 3 5) :bowl1
          (/ 2 5) :bowl2)))
    (testing "chocolate datum"
      (let [actual-posterior (suite/posterior cookie-suite :chocolate)]
        (are [x y] (= x (suite/probability actual-posterior y))
          (/ 1 3) :bowl1
          (/ 2 3) :bowl2)))
    (testing "vanilla datum, vanilla datum"
      (let [actual-posterior (-> cookie-suite
                                 (suite/posterior :vanilla)
                                 (suite/posterior :vanilla))]
        (are [expected hypothesis] (= expected (suite/probability actual-posterior hypothesis))
          (/ 9 13) :bowl1
          (/ 4 13) :bowl2)))
    (testing "chocolate datum, vanilla datum, chocolate datum"
      (let [actual-posterior (-> cookie-suite
                                 (suite/posterior :chocolate)
                                 (suite/posterior :vanilla)
                                 (suite/posterior :chocolate))]
        (are [expected hypothesis] (= expected (suite/probability actual-posterior hypothesis))
            (/ 3 11) :bowl1
            (/ 8 11) :bowl2)))))

(deftest multiple-data
  (let [cookie-suite (-> #{:bowl1 :bowl2}
                           (suite/make-suite)
                           (suite/add-prior {:bowl1 (/ 1 2) :bowl2 (/ 1 2)})
                           (suite/add-likelihood (fn [d h]
                                                   (get-in {:bowl1 {:vanilla (/ 3 4) :chocolate (/ 1 4)}
                                                            :bowl2 {:vanilla (/ 1 2) :chocolate (/ 1 2)}} [h d]))))]
    (testing "no data"
      (let [actual-posterior-seq (suite/posterior-seq cookie-suite [])]
        (is (empty? (map suite/priors actual-posterior-seq)))))
    (testing "vanilla data"
      (let [actual-posterior-seq (suite/posterior-seq cookie-suite [:vanilla])]
        (is (= [{:bowl1 (/ 3 5) :bowl2 (/ 2 5)}]
               (map suite/priors actual-posterior-seq)))))
    (testing "chocolate data"
      (let [actual-posterior-seq (suite/posterior-seq cookie-suite [:chocolate])]
        (is (= [{:bowl1 (/ 1 3) :bowl2 (/ 2 3)}]
               (map suite/priors actual-posterior-seq)))))
    (testing "vanilla, vanilla data"
      (let [actual-posterior-seq (suite/posterior-seq cookie-suite [:vanilla :vanilla])]
        (is (= [{:bowl1 (/ 3 5) :bowl2 (/ 2 5)} {:bowl1 (/ 9 13) :bowl2 (/ 4 13)}]
               (map suite/priors actual-posterior-seq)))))
    (testing "chocolate, vanilla, chocolate data"
      (let [actual-posterior-seq (suite/posterior-seq cookie-suite [:chocolate :vanilla :chocolate])]
        (is (= [{:bowl1 (/ 1 3) :bowl2 (/ 2 3)} {:bowl1 (/ 3 7) :bowl2 (/ 4 7)} { :bowl1 (/ 3 11) :bowl2 (/ 8 11)}]
               (map suite/priors actual-posterior-seq)))))))

