(ns proto-bayes.suite-tests
  (:use clojure.test)
  (:require [proto-bayes.suite :as suite]))

(deftest single-datum
  (testing "vanilla datum"
    (let [cookie-suite (-> #{:bowl1 :bowl2}
                           (suite/make-suite)
                           (suite/add-prior {:bowl1 (/ 1 2) :bowl2 (/ 1 2)})
                           (suite/add-likelihood (fn [d h]
                                                   (get-in {:bowl1 {:vanilla (/ 3 4) :chocolate (/ 1 4)}
                                                            :bowl2 {:vanilla (/ 1 2) :chocolate (/ 1 2)}} [h d]))))
          actual-posterior (suite/posterior cookie-suite :vanilla)]
      (are [x y] (= x (suite/probability actual-posterior y))
        (/ 3 5) :bowl1
        (/ 2 5) :bowl2))))
