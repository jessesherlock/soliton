(ns soliton.sm.async
  (:require [clojure.core.async :as a]
            [ergo.core :as ergo]
            [ergo.async]
            [ergo.async-utils :as utils]
            [soliton.sm.core]
            [soliton.core]
            [soliton.async]))

(defprotocol Focus
  (-focus [l s]))

(defprotocol Put
  (-put [l v s]))

(defprotocol Over
  (-over [l f s]))

(defn focus
  [l s]
  (if (vector? l)
    (-focus l s)
    (utils/pgo-safe (utils/?<? (soliton.core/focus l (utils/?<? s))))))

(defn put
  [l v s]
  (if (vector? l)
    (-put l v s)
    (utils/pgo-safe (utils/?<? (soliton.core/put l (utils/?<? v) (utils/?<? s))))))

(defn non-compound-async-over
  [l af s]
  (if (satisfies? soliton.async/Async-Over l)
    (soliton.async/-aover l af s)
    (utils/pgo-safe
      (let [target (utils/?<? (soliton.core/focus l s))
            value (utils/?<? (af target))]
        (utils/?<? (soliton.core/put l value s))))))

(defn over
  [l f s]
  (if (satisfies? Over l)
    (-over l f s)
    (utils/pgo-safe (a/<! (non-compound-async-over l f (utils/?<? s))))))

(defn map-op
  [l s operator operand rf init]
  (ergo.async/produce
   (comp (ergo.async/iterate (utils/pchan-returning
                              (soliton.sm.core/sm-step operator put)))
         ergo/til-nil)
   rf
   init
   (utils/->pchan
    s
    (a/promise-chan
     (map #(soliton.sm.core/->sm-context l operand %))))))

(defn async-sm-focus
  [l s]
  (utils/pgo-safe
    (:state (a/<! (map-op l s nil nil ergo/last nil)))))

(defn async-sm-put
  [l v s]
  (utils/pgo-safe
    (:state (a/<! (map-op l s put v ergo/last nil)))))

(defn async-sm-over
  [l f s]
  (utils/pgo-safe
    (:state (a/<! (map-op l s over f ergo/last nil)))))

(extend-type clojure.lang.PersistentVector
  Focus
  (-focus [l s] (async-sm-focus l s))
  Put
  (-put [l v s] (async-sm-put l v s))
  Over
  (-over [l f s] (async-sm-over l f s)))

(defn async-sm-focus-steps
  [l s]
  (let [res (a/chan 64)]
    (map-op l s nil nil ergo.async/put-rf! res)
    res))

(defn async-sm-put-steps
  [l v s]
  (let [res (a/chan 64)]
    (utils/pgo-safe (map-op l s put (utils/?<? v) ergo.async/put-rf! res))
    res))

(defn async-sm-over-steps
  [l f s]
  (let [res (a/chan 64)]
    (map-op l s over (utils/pchan-returning f) ergo.async/put-rf! res)
    res))

(defn focus-steps
  [l s]
  (-> (if (vector? l) l [l])
      (async-sm-focus-steps s)))

(defn put-steps
  [l v s]
  (-> (if (vector? l) l [l])
      (async-sm-put-steps v s)))

(defn over-steps
  [l f s]
  (-> (if (vector? l) l [l])
      (async-sm-over-steps f s)))

(defn reflector-async-over
  [reflector f s]
  (utils/pgo-safe
    (a/<! (put (:target reflector)
               (apply f (utils/?<? (focus reflector s)))
               s))))

(extend-type soliton.core.Reflector
  Over
  (-over [l f s] (reflector-async-over l f s)))

(defn reflect
  [lenses f s]
  (reflector-async-over
   (apply soliton.core/reflector lenses)
   f
   s))

(defn <>
  [f & ls]
  (fn [s] (reflect ls f s)))

(defmacro -<>
  [x & forms]
  (let [x ['->> x]]
    (loop [x x, forms forms]
      (if forms
        (let [form (first forms)
              wrapped (if (seq? form)
                        (conj x (list (cons `<> form)))
                        (conj x form))]
          (recur wrapped (next forms)))
        (seq x)))))

(comment

  (a/<!! (focus (soliton.core/reflector :foo :bar :baz) {:foo 1 :bar 2}))

  (a/<!! (reflect [:foo :bar :baz] + {:foo 1 :bar 2}))

  (a/<!! (reflector-async-over
          (soliton.core/reflector :foo)
          inc
          {:foo 1 :bar 2}))

  (a/<!! (reflector-async-over
          (soliton.core/reflector :foo)
          +
          {:foo 1 :bar 2}))

  (a/<!! (focus :foo (a/go {:foo {:bar 42}})))

  (a/<!! (focus [:foo :bar] (a/go {:foo {:bar 42}})))

  (a/<!! (put [:foo :bar] 42 {:foo {:bar :x}}))

  (a/<!! (put [:foo :bar] 42 (a/go {:foo {:bar :x}})))

  (a/<!! (put [:foo :bar] (a/go 42) (a/go {:foo {:bar :x}})))

  (defn ainc [x] (a/go (inc x)))

  (a/<!! (over :foo ainc {:foo 42}))

  (a/<!! (over [:foo :bar] ainc {:foo {:bar 42}}))

  (a/<!! (over [:foo :bar] inc {:foo {:bar 42}}))

  (a/<!! (a/into [] (focus-steps [:foo :bar :baz] {:foo {:bar {:baz 42}}})))


  (a/<!! (a/into [] (put-steps [:foo :bar :baz] 42 {:foo {:bar {:baz :x}}})))

  (a/<!! (a/into [] (put-steps [:foo :bar :baz] (a/go 42) (a/go {:foo {:bar {:baz :x}}}))))

  (a/<!! (a/into [] (over-steps [:foo :bar] ainc {:foo {:bar 42}})))

  (require '[soliton.lens :as lens])

  (a/<!! (over [:foo :bar] ainc {:foo {:bar 42}}))

  (a/<!! (over [:foo :bar] inc {:foo {:bar 42}}))

  (a/<!! (a/into [] (over-steps [:foo :bar] inc {:foo {:bar 42}})))

  (defn one
    ([x] (nth x 1))
    ([x v] (assoc x 1 v)))

  (a/<!! (put [:foo one] :x (a/go {:foo [0 1 2]})))
  (a/<!! (put [:foo one] (a/go :x) (a/go {:foo [0 1 2]})))

  (defn aone
    ([x] (utils/pgo (nth x 1)))
    ([x v] (utils/pgo (assoc x 1 v))))

  (a/<!! (focus [:foo aone] (a/go {:foo [0 1 2]})))

  (a/<!! (put [:foo aone] :x (a/go {:foo [0 1 2]})))

  (a/<!! (over [:foo aone] inc (a/go {:foo [0 1 2]})))

  (require '[soliton.lens :as lens])

  (a/<!! (over [:foo aone lens/id] ainc (a/go {:foo [0 1 2]})))

  (a/<!! (a/into [] (over-steps [:foo aone] ainc (a/go {:foo [0 1 2]}))))
   

  (a/<!! (a/into [] (over-steps [:foo aone lens/id] ainc (a/go {:foo [0 1 2]}))))
   

  (a/<!! (over [:foo aone 1] ainc (a/go {:foo [0 [10 11 12] 2]})))

  (a/<!! (over [:foo aone 1] inc (a/go {:foo [0 [10 11 12] 2]})))
  
  (a/<!! (over [:foo aone 1] inc {:foo [0 [10 11 12] 2]}))

  (a/<!! (put [:foo aone] 42 (a/go {:foo [1 2 3]})))

  (a/<!! (focus aone [1 2 3]))

  (a/<!! (put [:foo aone 1] 42 (a/go {:foo [1 [10 11 12] 3]})))


  , )
