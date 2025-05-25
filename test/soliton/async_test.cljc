(ns soliton.async-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer [deftest is testing]]
            [net.r4s6.test-async :as ta :include-macros true]
            [ergo.async-utils :as utils]
            [soliton.lens :as lens]
            [soliton.core :as core]
            [soliton.async :as sut]))

(defn ainc [x] (a/go (inc x)))

(deftest basic-aover-test
  (ta/async
      done
      (a/go
        (is (= {:foo 2} (a/<! (sut/aover :foo ainc {:foo 1}))))
        (is (= {:foo {:bar 2}} (a/<! (sut/aover [:foo :bar] ainc {:foo {:bar 1}}))))
        (is (= [1 3 3] (a/<! (sut/aover [1] ainc [1 2 3]))))
        (done))))

(deftest lift-test
  (ta/async
      done
    (a/go
      (is (= {:foo [:a :b :c]}
             (a/<! (sut/lift [:foo 1] {:foo [:a (a/go :b) :c]}))))
      (is (= {:foo [:a :b :c]}
             (->> {:foo (a/go [:a (a/go :b) :c])}
                  (sut/lift :foo)
                  a/<!
                  (sut/lift [:foo 1])
                  a/<!)))
      (is (= {:foo {:bar 2}
              :baz [:a :b :c]}
             (a/<! (sut/multi-lift
                    [[:foo :bar]
                     [:baz 0]
                     [:baz 2]]
                    {:foo {:bar (a/go 2)}
                     :baz [(a/go :a) :b (a/go :c)]}))))
      (done))))

(defn a+ [& x] (a/go (apply + x)))

(deftest reflect-test
  (ta/async
      done
    (a/go
      (let [test-map {:bravo 1
                      :alpha {:bravo 2
                              :charlie 1}}
            expected {:bravo 1
                      :alpha {:bravo 2
                              :charlie 1}
                      :bravos 3}]
        (is (= expected
               (a/<! (sut/areflect [:bravo [:alpha :bravo] :bravos]
                                   a+
                                   test-map))))

        (let [reflector (core/reflector :bravo [:alpha :bravo] :bravos)]
          (is (= expected
                 (a/<! (sut/aover reflector a+ test-map))))
          (is (= {:foo expected}
                 (a/<! (sut/aover [:foo reflector] a+ {:foo test-map}))))))

      (is (= {:in 1 :out 2}
             (a/<! (sut/aover (core/reflector :in :out) ainc {:in 1}))))
      (is (= {:in 1 :out 2}
             (a/<! (sut/areflect [:in :out] ainc {:in 1}))))
      (done))))

(comment


  ,)
