(ns soliton.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [soliton.lens :as lens]
            [soliton.core :as sut])
  #?(:cljs (:require-macros [soliton.core :as sut])))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(deftest focus-steps
  (is (= [{:foo {:bar [:x :y :z]}}
          {:bar [:x :y :z]}
          [:x :y :z]
          :z]
         (sut/focus-steps [:foo :bar 2] {:foo {:bar [:x :y :z]}}))))

(deftest keyword-lens-test
  (is (= :value
         (sut/focus :foo {:foo :value})))

  (is (= {:foo :value}
         (sut/put :foo :value {:foo :x})))

  (is (= {:foo 43}
         (sut/over :foo inc {:foo 42})))

  (is (= {:foo 42}
         (sut/put :foo 42 {}))))

(deftest fn-lens-test
  (let [tfn (fn tfn
              ([s] (:foo s))
              ([s v] (assoc s :foo v)))]
    (is (= :value
           (sut/focus tfn {:foo :value})))

    (is (= {:foo :value}
           (sut/put tfn :value {:foo :x})))

    (is (= {:foo 43}
           (sut/over tfn inc {:foo 42})))))

(deftest int-long-lens-test
  (is (= :value
         (sut/focus (long 2) [:x :x :value :y])
         (sut/focus (int 2) [:x :x :value :y])))

  (is (= [:x :x :value :y]
         (sut/put (long 2) :value [:x :x :x :y])
         (sut/put (int 2) :value [:x :x :x :y])))

  (is (= [nil nil :x]
         (sut/put (long 2) :x [])
         (sut/put (int 2) :x [])))

  (is (= [:x :x 43 :y]
         (sut/over (long 2) inc [:x :x 42 :y])
         (sut/over (int 2) inc [:x :x 42 :y])))

  (is (= [1]
         (sut/over (long 0) (fnil inc 0) [])
         (sut/over (int 0) (fnil inc 0) []))))

(deftest nil-lens-test
  (is (= nil (sut/focus nil {:foo 42})))
  (is (= {:foo 42} (sut/put nil 1 {:foo 42})))
  (is (= 41 (sut/over nil inc 41))))

;; compound lenses

(deftest vector-lens-test
  (is (= :value
         (sut/focus [:foo 1] {:foo [:x :value]})))
  
  (is (= {:foo [:x :value :y]}
         (sut/put [:foo 1] :value {:foo [:x :x :y]})))
  
  (is (= {:foo [:x 43 :y]}
         (sut/over [:foo 1] inc {:foo [:x 42 :y]})))
  
  (is (= {:foo {:bar :x}}
         (sut/put [:foo :bar] :x {})))

  (is (= {:foo [nil :x]}
         (sut/put [:foo 1] :x {})))

  ;; Have covered subvecs too?
  (is (= :value 
         (sut/focus (subvec [:foo 1 2] 0 2) {:foo [:x :value]}))))

(deftest map-compound-lens-test
  (let [test-map {:eu {:italy {:capital :rome, :lang :italian}
                       :france {:capital :paris, :lang :french}}
                  :australia {:capital :canberra, :lang :english}}]

    (is (= {:a :rome, :b :english}
           (sut/focus {:a [:eu :italy :capital]
                       :b [:australia :lang]}
                      test-map)))

    (is (= {:eu {:italy :replaced-italy
                 :france {:capital :paris :lang :french}}
            :australia :replaced-australia}
           (sut/put {:a [:eu :italy]
                     :b :australia}
                    {:a :replaced-italy
                     :b :replaced-australia}
                    test-map)))
    
    (is (= {:eu {:italy {:capital :rome, :lang :italian, :i-was-A true}
                 :france {:capital :paris, :lang :french}}
            :australia {:capital :canberra, :lang :english, :i-was-B true}}
           (sut/over {:a [:eu :italy]
                      :b :australia}
                     (fn [{:keys [a b]}]
                       {:a (assoc a :i-was-A true)
                        :b (assoc b :i-was-B true)})
                     test-map)
           (sut/put {:a [:eu :italy lens/merge]
                     :b [:australia lens/merge]}
                    {:a {:i-was-A true}
                     :b {:i-was-B true}}
                    test-map)))

    (is (= {:eu {:italy {:capital :rome :lang ":italian"}
                 :france {:capital :paris :lang 'french}}
            :australia {:capital :canberra :lang "english"}}
           (sut/over {str [:eu :italy :lang]
                      symbol [:eu :france :lang]
                      #(str (symbol %)) [:australia :lang]}
                     (fn [m] (into {} (map (fn [[k v]] [k (k v)])) m))
                     test-map)))))

(deftest sets-compound-lens-test
  (let [test-map {:eu {:italy {:capital :rome, :lang :italian}
                       :france {:capital :paris, :lang :french}}
                  :australia {:capital :canberra, :lang :english}}]

    (is (= #{:rome :canberra}
           (sut/focus #{[:eu :italy :capital]
                        [:australia :capital]}
                      test-map)))

    (is (= #{:italian, :french}
           (sut/focus [:eu #{[:italy :lang]
                             [:france :lang]}]
                      test-map)))
    ;; so sets end up kinda allowing very limited traversals
    ;; mostly by accident
    ;; only if the set lens is at the end (since focus doesn't know to join
    ;; results at all)
    ;; it's useful-ish and comes for free but don't expect real traversals
    ;; with focus/put/over lens functions
    (is (= {:eu {:italy {:capital :rome, :lang :italian, :tagged true}
                 :france {:capital :paris, :lang :french, :tagged true}}
            :australia {:capital :canberra, :lang :english}}
           (sut/put [:eu #{[:italy :tagged]
                           [:france :tagged]}]
                    true
                    test-map)))

    (is (= {:eu {:italy {:capital "rome", :lang :italian}
                 :france {:capital "paris", :lang :french}}
            :australia {:capital :canberra :lang :english}}
           (sut/over [:eu #{[:italy :capital]
                            [:france :capital]}]
                     name
                     test-map)))))

(deftest reflect-test
  (let [test-map {:bravo 1
                  :alpha {:bravo 2
                          :charlie 1}}]
    (testing "3 argument reflection, add :bravo to [:alpha :bravo] and put result in :bravos"
      (is (= {:bravo 1
              :alpha {:bravo 2
                      :charlie 1}
              :bravos 3}

             (sut/over (sut/reflector :bravo [:alpha :bravo] :bravos) + test-map)

             (sut/reflect [:bravo [:alpha :bravo] :bravos] + test-map)

             ((sut/<> + :bravo [:alpha :bravo] :bravos) test-map))))

    (testing "2 argument reflection, arg is [:alpha :charlie] and result in :charlie-incd"
        (is (= {:bravo 1
                :alpha {:bravo 2
                        :charlie 1}
                :charlie-incd 2}

               (sut/over (sut/reflector [:alpha :charlie] :charlie-incd) inc test-map)

               (sut/reflect [[:alpha :charlie] :charlie-incd] inc test-map)

               ((sut/<> inc [:alpha :charlie] :charlie-incd) test-map))))

    (testing "1 argument reflection, a normal non-reflection"
      (is (= {:bravo "1"
              :alpha {:bravo 2
                      :charlie 1}}

             (sut/over (sut/reflector :bravo) str test-map)

             (sut/reflect [:bravo] str test-map)
           
             ((sut/<> str :bravo) test-map))))))


(deftest -<>-test
  (let [test-map {:bravo 1
                  :alpha {:bravo 2
                          :charlie 1}}]
       (is (= {:bravo 1
               :alpha {:bravo 2
                       :charlie 1}
               :bravos 3}

              {:bravo 1
               :alpha {:bravo 2
                       :charlie 1}
               :bravos 3}

              ((sut/<> + :bravo [:alpha :bravo] :bravos) test-map)

              (->> test-map ((sut/<> + :bravo [:alpha :bravo] :bravos)))

              (sut/-<> test-map (+ :bravo [:alpha :bravo] :bravos))))

       (is (= {:bravo 1
               :alpha {:bravo 2
                       :charlie 1}
               :bravos 3
               :total 7
               :total-str "7"
               :total-plus-10 17}

              {:bravo 1
               :alpha {:bravo 2
                       :charlie 1}
               :bravos 3
               :total 7
               :total-str "7"
               :total-plus-10 17}

              (sut/-<> test-map
                       (+ :bravo [:alpha :bravo] :bravos)
                       (+ :bravo [:alpha :bravo] [:alpha :charlie] :bravos :total)
                       (+ :total (lens/const 10) :total-plus-10)
                       (str :total :total-str))))))
