(ns soliton.sm.async
  (:require [clojure.core.async :as a]
            [ergo.core :as ergo]
            [ergo.async]
            [ergo.async-mixed]
            [ergo.async-utils :as utils]
            [soliton.sm.core]
            [soliton.core]
            [soliton.async])
  #?(:cljs (:require-macros [soliton.sm.async :refer [-<>]])))

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
    (soliton.async/-async-over l af s)
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
  (ergo/produce
   (comp (ergo.async-mixed/iterate (soliton.sm.core/sm-step operator put))
         ergo/til-nil)
   rf
   init
   (if (utils/chan? s)
     (a/go (soliton.sm.core/->sm-context l operand (a/<! s)))
     (soliton.sm.core/->sm-context l operand s))))

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

(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core/PersistentVector)
  Focus
  (-focus [l s] (async-sm-focus l s))
  Put
  (-put [l v s] (async-sm-put l v s))
  Over
  (-over [l f s] (async-sm-over l f s)))

#?(:cljs
(extend-type cljs.core/Subvec
  Focus
  (-focus [l s] (async-sm-focus l s))
  Put
  (-put [l v s] (async-sm-put l v s))
  Over
  (-over [l f s] (async-sm-over l f s))))

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
  (cons '->> (cons x (soliton.core/-<>-form forms `<>))))
