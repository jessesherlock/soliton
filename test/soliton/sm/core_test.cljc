(ns soliton.sm.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [soliton.lens :as lens]
            [soliton.sm.core :as sut]))

(deftest single-test
  (is (= 42 (sut/focus :foo {:foo 42})))
  (is (= {:foo 42} (sut/put :foo 42 {:foo nil})))
  (is (= {:foo 43} (sut/over :foo inc {:foo 42}))))

(deftest compound-test
  (is (= :value
         (sut/focus [:foo 1] {:foo [:x :value]})))

  (is (= {:foo [:x :value :y]}
         (sut/put [:foo 1] :value {:foo [:x :x :y]})))

  (is (= {:foo [:x 43 :y]}
         (sut/over [:foo 1] inc {:foo [:x 42 :y]}))))

(deftest focus-steps-test
  (is (= [{:lenses [:foo 1], :state {:foo [:x :value]}}
          {:lenses [1], :state [:x :value]}
          {:state :value}]
         (sut/focus-steps [:foo 1] {:foo [:x :value]}))))

(deftest put-steps-test
  (is (= [{:lenses [:foo 1]
           :state {:foo [:x :x :y]}
           :stack [], :operand :value}
          {:lenses [1]
           :state [:x :x :y]
           :stack [[:foo {:foo [:x :x :y]}]]
           :operand :value}
          {:state [:x :value :y]
           :stack [[:foo {:foo [:x :x :y]}]]}
          {:state {:foo [:x :value :y]}
           :stack []}]
         (sut/put-steps [:foo 1] :value {:foo [:x :x :y]}))))

(deftest over-steps-test
  (is (= [{:lenses [:foo 1]
           :state {:foo [:x 42 :y]}
           :stack []
           :operand inc}
          {:lenses [1]
           :state [:x 42 :y]
           :stack [[:foo {:foo [:x 42 :y]}]]
           :operand inc}
          {:state [:x 43 :y]
           :stack [[:foo {:foo [:x 42 :y]}]]}
          {:state {:foo [:x 43 :y]}
           :stack []}]
         (sut/over-steps [:foo 1] inc {:foo [:x 42 :y]}))))
