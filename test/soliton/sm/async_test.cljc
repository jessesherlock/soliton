(ns soliton.sm.async-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as a]
            [net.r4s6.test-async :as ta]
            [soliton.core :as core]
            [soliton.lens :as lens]
            [soliton.sm.async :as sut]))

(defn ainc [x] (a/go (inc x)))

(defn async-nth-1
  ([s]
   (a/go (nth s 1)))
  ([s v]
   (a/go (assoc s 1 v))))

(def async-nth-2
  (lens/lens
   (fn [s] (a/go (nth s 2)))
   (fn [s v] (a/go (assoc s 2 v)))
   (fn [s f] (a/go (update s 2 f)))))

(deftest single-test
  (ta/async
      done
      (a/go
        (is (= 42 (a/<! (sut/focus :foo {:foo 42}))))
        (is (= 42 (a/<! (sut/focus :foo (a/go {:foo 42})))))
        (is (= 42 (a/<! (sut/focus async-nth-1 [1 42 3]))))
        (is (= 42 (a/<! (sut/focus async-nth-1 (a/go [1 42 3])))))
        (is (= 42 (a/<! (sut/focus async-nth-2 [1 2 42 4]))))
        (is (= 42 (a/<! (sut/focus async-nth-2 (a/go [1 2 42 4])))))

        (is (= {:foo 42} (a/<! (sut/put :foo 42 {:foo :x}))))
        (is (= {:foo 42} (a/<! (sut/put :foo (a/go 42) {:foo :x}))))
        (is (= {:foo 42} (a/<! (sut/put :foo 42 (a/go {:foo :x})))))
        (is (= {:foo 42} (a/<! (sut/put :foo (a/go 42) (a/go {:foo :x})))))

        (is (= {:foo 43} (a/<! (sut/over :foo inc {:foo 42}))))
        (is (= {:foo 43} (a/<! (sut/over :foo inc (a/go {:foo 42})))))
        (is (= {:foo 43} (a/<! (sut/over :foo ainc {:foo 42}))))
        (is (= {:foo 43} (a/<! (sut/over :foo ainc (a/go {:foo 42})))))
        (done))))

(deftest compound-test
  (ta/async
      done
    (a/go
      (is (= 42 (a/<! (sut/focus [:foo :bar] {:foo {:bar 42}}))))
      (is (= 42 (a/<! (sut/focus [:foo :bar] (a/go {:foo {:bar 42}})))))
      (is (= 42 (a/<! (sut/focus [:foo :bar] {:foo {:bar (a/go 42)}}))))

      ;; subvecs covered?
      (is (= 42 (a/<! (sut/focus (subvec [:foo :bar :baz] 0 2) {:foo {:bar 42}}))))

      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo 1] :value {:foo [:x :y :z]}))))
      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo 1] (a/go :value) {:foo [:x :y :z]}))))
      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo 1] :value (a/go {:foo [:x :y :z]})))))
      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo 1] (a/go :value) (a/go {:foo [:x :y :z]})))))
      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo async-nth-1] :value {:foo [:x :y :z]}))))
      (is (= {:foo [:x :value :z]}
             (a/<! (sut/put [:foo async-nth-1] :value (a/go {:foo [:x :y :z]})))))
      (is (= {:foo [:x :y :value]}
             (a/<! (sut/put [:foo async-nth-2] :value (a/go {:foo [:x :y :z]})))))
      (is (= {:foo [:x :y :value]}
             (a/<! (sut/put [:foo async-nth-2] :value (a/go {:foo [:x :y :z]})))))

      (is (= {:foo [:x :y 43]}
             (a/<! (sut/over [:foo 2] inc {:foo [:x :y 42]}))))
      (is (= {:foo [:x :y 43]}
             (a/<! (sut/over [:foo 2] ainc {:foo [:x :y 42]}))))
      (is (= {:foo [:x :y 43]}
             (a/<! (sut/over [:foo 2] ainc (a/go {:foo [:x :y 42]})))))
      (is (= {:foo [:x 43 :z]}
             (a/<! (sut/over [:foo async-nth-1] ainc {:foo [:x 42 :z]}))))
      (is (= {:foo [:x :y 43]}
             (a/<! (sut/over [:foo async-nth-2] ainc {:foo [:x :y 42]}))))
      (done))))

(deftest focus-steps-test
  (ta/async
      done
    (a/go
      (is (= [{:lenses [:foo 1], :state {:foo [:x :value]}}
              {:lenses [1], :state [:x :value]}
              {:state :value}]
             (a/<! (a/into [] (sut/focus-steps [:foo 1] {:foo [:x :value]})))
             (a/<! (a/into [] (sut/focus-steps [:foo 1] (a/go {:foo [:x :value]}))))))
      (done))))

(deftest put-steps-test
  (ta/async
      done
      (a/go 
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
               (a/<! (a/into [] (sut/put-steps [:foo 1] :value {:foo [:x :x :y]})))
               (a/<! (a/into [] (sut/put-steps [:foo 1] :value (a/go {:foo [:x :x :y]}))))
               (a/<! (a/into [] (sut/put-steps [:foo 1] (a/go :value) {:foo [:x :x :y]})))))
        (done))))

(deftest over-steps-test
  (ta/async
      done
    (a/go
      (let [result (->> (sut/over-steps [:foo 1] inc {:foo [:x 42 :y]})
                        (a/into [])
                        a/<!)]
        (is (fn? (-> result first :operand)))
        (is (fn? (-> result second :operand)))
        (let [result (mapv #(dissoc % :operand) result)]
          (is (= [{:lenses [:foo 1]
                   :state {:foo [:x 42 :y]}
                   :stack []}
                  {:lenses [1]
                   :state [:x 42 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state [:x 43 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state {:foo [:x 43 :y]}
                   :stack []}]
                 result))))
      (let [result (->> (sut/over-steps [:foo 1] ainc {:foo [:x 42 :y]})
                        (a/into [])
                        a/<!)]
        (is (fn? (-> result first :operand)))
        (is (fn? (-> result second :operand)))
        (let [result (mapv #(dissoc % :operand) result)]
          (is (= [{:lenses [:foo 1]
                   :state {:foo [:x 42 :y]}
                   :stack []}
                  {:lenses [1]
                   :state [:x 42 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state [:x 43 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state {:foo [:x 43 :y]}
                   :stack []}]
                 result))))
      (let [result
            (->> (sut/over-steps [:foo async-nth-1] ainc (a/go {:foo [:x 42 :y]}))
                 (a/into [])
                 a/<!)]
        (is (fn? (-> result first :operand)))
        (is (fn? (-> result second :operand)))
        (let [result (mapv #(dissoc % :operand) result)]
          (is (= [{:lenses [:foo async-nth-1]
                   :state {:foo [:x 42 :y]}
                   :stack []}
                  {:lenses [async-nth-1]
                   :state [:x 42 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state [:x 43 :y]
                   :stack [[:foo {:foo [:x 42 :y]}]]}
                  {:state {:foo [:x 43 :y]}
                   :stack []}]
                 result))))
      (done))))

(defn a+ [& xs] (a/go (apply + xs)))

(deftest reflector-test
  (ta/async
      done
    (a/go
      (let [test-map {:bravo 1
                      :alpha {:bravo 2
                              :charlie 1}}]
        (is (= {:bravo 1
                :alpha {:bravo 2
                        :charlie 1}
                :bravos 3}
               (a/<! (sut/reflect [:bravo [:alpha :bravo] :bravos]
                                  +
                                  test-map))
               (a/<! (sut/over (core/reflector :bravo [:alpha :bravo] :bravos)
                               a+
                               test-map))
               (a/<! (sut/over (core/reflector :bravo [:alpha :bravo] :bravos)
                               +
                               test-map))

               (a/<! (sut/over (core/reflector [:alpha :bravo] :bravos)
                               inc
                               test-map))
               (a/<! (sut/over (core/reflector [:alpha :bravo] :bravos)
                               ainc
                               test-map))))
        (is (= {:bravo 2
                :alpha {:bravo 2
                        :charlie 1}}
               (a/<! (sut/reflect [:bravo] ainc test-map))
               (a/<! (sut/over (core/reflector :bravo) ainc test-map)))))
      (done))))

(deftest <>-test
  (ta/async
      done
    (a/go
      (let [test-map {:foo 1, :bar 2, :baz {:one 1, :two 2}}]
        (is (= {:foo 1
                :bar 2
                :baz {:one 1
                      :two 2
                      :subtotal 3}
                :total 7}
               (a/<! (sut/-<> test-map
                              (a+ [:baz :one] [:baz :two] [:baz :subtotal])
                              (+ [:baz :subtotal] :foo :bar :total)
                              (ainc :total))))))
      (done))))


