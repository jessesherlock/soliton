(ns soliton.sm.core
  (:require [ergo.core :as ergo]
            [soliton.core :as core]
            [soliton.sm.protocols :as sm.p]))

(defn focus
  [l s]
  (if (vector? l)
    (sm.p/-focus l s)
    (core/focus l s)))

(defn put
  [l v s]
  (if (vector? l)
    (sm.p/-put l v s)
    (core/put l v s)))

(defn over
  [l f s]
  (if (vector? l)
    (sm.p/-over l f s)
    (core/over l f s)))

(defn focus-rf
  [state lens]
  (focus lens state))


;; map context

(defn ->sm-context
  [lenses operand state]
  ;; stacks for put or over, pass in an atomic put or over fn as operand
  (let [c {:lenses lenses
           :state state}]
    (if operand
      (-> (assoc c :stack [])
          (assoc :operand operand))
      c)))

(defn focus-step
  [c l ls state]
  (-> (if (:operand c)
      (update c :stack #(conj % [l state]))
      c)
      (assoc :lenses ls)
      (assoc :state (focus l state))))

(defn operate-step
  [c put-fn l operator state]
  (let [operand (:operand c)
        c (dissoc c :lenses :operand)]
    (if operand 
      ;; a put or over
      (put-fn :state (operator l operand state) c)
      ;; a focus
      (put-fn :state (focus l state) c))))

(defn stitch-step
  [c put-fn stack state]
  (if-let [frame (peek stack)]
    (->> (assoc c :stack (pop stack))
         (put-fn :state
                 (put-fn (first frame)
                         state
                         (second frame))))))

(defn sm-step
  [operator put-fn]
  (fn stepper
    [c]
    (let [[l & ls] (:lenses c)
          state (:state c)]
      (if ls
        ;; still focusing in
        (focus-step c l ls state)
        (if l
          ;; one lens left, so operate
          (operate-step c put-fn l operator state)
          ;; no lenses, stitch the stack if exists (put or over) or done (focus)
          (if-let [stack (:stack c)]
            (stitch-step c put-fn stack state)))))))

(defn map-op
  [l s operator operand rf init]
  (->> s
       (->sm-context l operand)
       (ergo/produce (comp (ergo/iterate (sm-step operator put))
                           ergo/til-nil)
                     rf
                     init)))

(defn sm-focus
  [l s]
  (:state (map-op l s nil nil ergo/last nil)))

(defn sm-focus-steps
  [l s]
  (map-op l s nil nil conj []))

(defn sm-put
  [l v s]
  (:state (map-op l s put v ergo/last nil)))

(defn sm-put-steps
  [l v s]
  (map-op l s put v conj []))

(defn sm-over
  [l f s]
  (:state (map-op l s over f ergo/last nil)))

(defn sm-over-steps
  [l f s]
  (map-op l s over f conj []))

(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core/PersistentVector)
  sm.p/Focus
  (-focus [l s] (sm-focus l s))
  sm.p/Put
  (-put [l v s] (sm-put l v s))
  sm.p/Over
  (-over [l f s] (sm-over l f s)))

#?(:cljs
(extend-type cljs.core/Subvec
  sm.p/Focus
  (-focus [l s] (sm-focus l s))
  sm.p/Put
  (-put [l v s] (sm-put l v s))
  sm.p/Over
  (-over [l f s] (sm-over l f s))))

(defn focus-steps
  [l s]
  (-> (if (vector? l) l [l])
      (sm-focus-steps s)))

(defn put-steps
  [l v s]
  (-> (if (vector? l) l [l])
      (sm-put-steps v s)))

(defn over-steps
  [l f s]
  (-> (if (vector? l) l [l])
      (sm-over-steps f s)))

;; ** fns for maps of lenses

(defn map-focus
  [lens-map s]
  (reduce-kv (fn [init k v]
               (assoc init k (sm.p/-focus v s)))
             (empty lens-map)
             lens-map))

(defn map-put
  [lens-map value-map s]
  (reduce-kv (fn [init k v]
               (sm.p/-put v (get value-map k) init))
             s
             lens-map))

(extend-type #?(:clj clojure.lang.IPersistentMap
                :cljs cljs.core/PersistentArrayMap)
  sm.p/Focus
  (-focus [l s] (map-focus l s))
  sm.p/Put
  (-put [l v s] (map-put l v s))
  sm.p/Over
  (-over [l f s] (sm.p/default-over l f s)))

#?(:cljs
(extend-type cljs.core/PersistentHashMap
  sm.p/Focus
  (-focus [l s] (map-focus l s))
  sm.p/Put
  (-put [l v s] (map-put l v s))
  sm.p/Over
  (-over [l f s] (sm.p/default-over l f s))))

#?(:cljs
(extend-type cljs.core/PersistentTreeMap
  sm.p/Focus
  (-focus [l s] (map-focus l s))
  sm.p/Put
  (-put [l v s] (map-put l v s))
  sm.p/Over
  (-over [l f s] (sm.p/default-over l f s)))) 


;; ** fns for sets of lenses

(defn set-focus
  [lens-set s]
  (into (empty lens-set) (map #(sm.p/-focus % s) lens-set)))

(defn set-put
  [lens-set v s]
  (let [[lens & lenses] lens-set]
    (if lenses
      (recur lenses v (sm.p/-put lens v s))
      (sm.p/-put lens v s))))

(defn set-over
  [lens-set f s]
  (let [[lens & lenses] lens-set]
    (if lenses
      (recur lenses f (sm.p/-over lens f s))
      (sm.p/-over lens f s))))

(extend-type #?(:clj clojure.lang.IPersistentSet
                :cljs cljs.core/PersistentHashSet)
  sm.p/Focus
  (-focus [l s] (set-focus l s))
  sm.p/Put
  (-put [l v s] (set-put l v s))
  sm.p/Over
  (-over [l f s] (set-over l f s)))

#?(:cljs
(extend-type cljs.core/PersistentTreeSet
  sm.p/Focus
  (-focus [l s] (set-focus l s))
  sm.p/Put
  (-put [l v s] (set-put l v s))
  sm.p/Over
  (-over [l f s] (set-over l f s))))


;; * Reflection

(extend-type soliton.core.Reflector
  sm.p/Focus
  (-focus [l s] (map #(focus % s) (:sources l)))
  sm.p/Put
  (-put [l v s] (put (:target l) v s))
  sm.p/Over
  (-over [l f s] (put (:target l)
                      (apply f (focus l s))
                      s)))
(defn reflect
  [lenses f s]
  (if (next lenses)
    (put (last lenses) (apply f (map #(focus % s) (butlast lenses))) s)
    (over (first lenses) f s)))

(defn <>
  [f & ls]
  (fn [s] (reflect ls f s)))

(defmacro -<>
  [x & forms]
  (cons '->> (cons x (core/-<>-form forms `<>))))
