(ns soliton.derived-atom-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as a]
            [soliton.core :refer [focus put over]]
            [soliton.derived-atom :as sut]))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn get-watches
  [a]
  #?(:clj
     (.getWatches ^clojure.lang.Atom a)
     :cljs
     (.-watches ^cljs.core.Atom a)))

(deftest derived-atom-test
  (let [parent (atom {:foo 1 :bar 2})
        derived (sut/derive :foo parent)]
    (is (= 1 (deref derived)))

    (swap! parent #(update % :foo inc))
    (is (= 2
           (:foo @parent)
           @derived))
    (swap! derived inc)
    (is (= 3
           (:foo @parent)
           @derived))

    (reset! parent {:foo 7 :bar 2})
    (is (= 7 @derived))
    (reset! derived 42)
    (is (= 42
           (:foo @parent)
           @derived))))

(deftest derived-ro-test
  (let [parent (atom {:foo 0 :bar 2})
        derived-ro (sut/derive-ro :foo parent)
        watch-outputs (atom [])]

    (is (= 0 @derived-ro))
    (swap! parent update :foo inc)
    (is (= 1 @derived-ro))

    (is (= 0 (count (get-watches parent))))
    (add-watch derived-ro :w1
               (fn [_ _ old new] (swap! watch-outputs conj {:w1 new})))
    (is (= 1 (count (get-watches parent))))

    (swap! parent update :foo inc)
    (is (= [{:w1 2}] @watch-outputs))
    (reset! watch-outputs [])

    (remove-watch derived-ro :w1)
    (swap! parent update :foo inc)
    (is (= [] @watch-outputs))
    (is (= 0 (count (get-watches parent))))))

(deftest derived-atom-watches
  (let [state-data {:foo {:alpha 1 :bravo 2} :bar 3}
        parent (atom state-data)
        derived (sut/derive [:foo :alpha] parent)
        watch-outputs (atom [])]
    (add-watch derived :dw1 (fn [_ _ old new]
                              (swap! watch-outputs conj {:watch-fn-dw1 new})))
    (swap! parent update :bar inc)
    (swap! parent update-in [:foo :bravo] inc)

    (testing "Didn't update focused part of parent data so no watches fire"
      (is (= [] @watch-outputs))
      (reset! parent state-data))

    (swap! parent update-in [:foo :alpha] inc)
    (swap! derived inc)

    (testing "Updated focused data in parent and derived so watch fn fired twice"
      (is (= [{:watch-fn-dw1 2}
              {:watch-fn-dw1 3}]
             @watch-outputs))
      (reset! parent state-data)
      (reset! watch-outputs []))
    
    (testing "Multiple watches all fire"
      (add-watch derived :dw2 (fn [_ _ old new]
                                (swap! watch-outputs conj {:watch-fn-dw2 new})))
      (is (= [] @watch-outputs))
      (swap! parent update-in [:foo :alpha] inc)
      (swap! derived inc)
      (is (= [{:watch-fn-dw1 2}
              {:watch-fn-dw2 2}
              {:watch-fn-dw1 3}
              {:watch-fn-dw2 3}]
             @watch-outputs))
      (reset! parent state-data)
      (reset! watch-outputs []))

    (testing "Watches can be removed"

      (is (not= {} (get-watches parent)))

      (remove-watch derived :dw1)
      (swap! derived inc)
      (is (= [{:watch-fn-dw2 2}] @watch-outputs))
      (reset! watch-outputs [])

      (remove-watch derived :dw2)
      (swap! derived inc)
      (is (= [] @watch-outputs))

      (is (= {} (get-watches parent))))))
