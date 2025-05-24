(ns soliton.sm.core
  (:require [ergo.core :as ergo]
            [soliton.core :as core]
            [soliton.sm.protocols :as p]))

(defn focus
  [l s]
  (if (vector? l)
    (p/-focus l s)
    (core/focus l s)))

(defn put
  [l v s]
  (if (vector? l)
    (if (satisfies? p/Put l)
      (p/-put l v s)
      (p/default-put l v s))
    (core/put l v s)))

(defn over
  [l f s]
  (if (vector? l)
    (if (satisfies? p/Over l)
      (p/-over l f s)
      (p/default-over l f s))
    (core/over l f s)))

(defn focus-rf
  [state lens]
  (focus lens state))


;; map context

(defn ->map-context
  [lenses operand state]
  ;; stacks for put or over, pass in an atomic put or over fn as operand
  (let [c {:lenses lenses
           :state state}]
    (if operand
      (-> (assoc c :stack [])
          (assoc :operand operand))
      c)))

(defn map-context-focus-step
  [c l ls state]
  (-> (if (:operand c)
      (update c :stack #(conj % [l state]))
      c)
      (assoc :lenses ls)
      (assoc :state (focus l state))))

(defn map-context-operate-step
  [c put-fn l operator state]
  (let [operand (:operand c)
        c (dissoc c :lenses :operand)]
    (if operand 
      ;; a put or over
      (put-fn :state (operator l operand state) c)
      ;; a focus
      (put-fn :state (focus l state) c))))

(defn map-context-stitch-step
  [c put-fn stack state]
  (if-let [frame (peek stack)]
    (->> (assoc c :stack (pop stack))
         (put-fn :state
                 (put-fn (first frame)
                         state
                         (second frame))))))

(defn map-context-step
  [operator put-fn]
  (fn map-stepper
    [c]
    (let [[l & ls] (:lenses c)
          state (:state c)]
      (if ls
        ;; still focusing in
        (map-context-focus-step c l ls state)
        (if l
          ;; one lens left, so operate
          (map-context-operate-step c put-fn l operator state)
          ;; no lenses, stitch the stack if exists (put or over) or done (focus)
          (if-let [stack (:stack c)]
            (map-context-stitch-step c put-fn stack state)))))))

(defn map-op
  [l s operator operand rf init]
  (->> s
       (->map-context l operand)
       (ergo/produce (comp (ergo/iterate (map-context-step operator put))
                          ergo/til-nil)
                    rf
                    init)))

(defn context-focus
  [l s]
  (:state (map-op l s nil nil ergo/last nil)))

(defn context-focus-steps
  [l s]
  (map-op l s nil nil conj []))

(defn context-put
  [l v s]
  (:state (map-op l s put v ergo/last nil)))

(defn context-put-steps
  [l v s]
  (map-op l s put v conj []))

(defn context-over
  [l f s]
  (:state (map-op l s over f ergo/last nil)))

(defn context-over-steps
  [l f s]
  (map-op l s over f conj []))

(extend-type clojure.lang.PersistentVector
  p/Focus
  (p/-focus [l s] (context-focus l s))
  p/Put
  (p/-put [l v s] (context-put l v s))
  p/Over
  (p/-over [l f s] (context-over l f s)))

(defn focus-steps
  [l s]
  (-> (if (sequential? l) l [l])
      (context-focus-steps s)))

(defn put-steps
  [l v s]
  (-> (if (sequential? l) l [l])
      (context-put-steps v s)))

(defn over-steps
  [l f s]
  (-> (if (sequential? l) l [l])
      (context-over-steps f s)))

