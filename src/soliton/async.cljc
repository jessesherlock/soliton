(ns soliton.async
  (:require [clojure.core.async :as a]
            [soliton.core :as core]
            [soliton.lens :as lens]
            [soliton.protocols :as p]
            [ergo.async-utils :as autils]))

(defprotocol Async-Over
  (-async-over [l af s])
  (-async?-over [l f s]))

(defn stitch
  [lenses steps value]
  (let [value (core/put (peek lenses) value (peek steps))
        steps (pop steps)]
    (if (empty? steps)
      value
      (recur (pop lenses) steps value))))

(declare over ?over)

(defn single-async-over
  [l af s]
  (let [target (core/focus l s)]
    (autils/pgo-safe
      (let [value (a/<! (af target))]
        (core/put l value s)))))

(defn single-async?-over
  [l f s]
  (let [target (core/focus l s)]
    (autils/pgo-safe
      (let [value (autils/?<! (f target))]
        (core/put l value s)))))

(defn default-async-over
  [ls af s]
  (if (vector? ls)
    (if (= 1 (count ls))
      (single-async-over (ls 0) af s)
      (let [focus-steps (pop (core/focus-steps ls s))
            subject (peek focus-steps)
            steps (pop focus-steps)
            last-lens (last ls)
            result (over last-lens af subject)]
        (autils/pgo-safe
          (let [result (a/<! result)
                ls (pop ls)]
            (stitch ls steps result)))))
    (single-async-over ls af s)))

(defn default-async?-over
  [ls af s]
  (if (vector? ls)
    (if (= 1 (count ls))
      (single-async?-over (ls 0) af s)
      (let [focus-steps (pop (core/focus-steps ls s))
            subject (peek focus-steps)
            steps (pop focus-steps)
            last-lens (last ls)
            result (?over last-lens af subject)]
        (autils/pgo-safe
          (let [result (autils/?<! result)
                ls (pop ls)]
            (stitch ls steps result)))))
    (single-async?-over ls af s)))

(defn over
  [l af s]
  (if (satisfies? Async-Over l)
    (-async-over l af s)
    (default-async-over l af s)))

(defn ?over
  [l f s]
  (if (satisfies? Async-Over l)
    (-async?-over l f s)
    (default-async?-over l f s)))

(defn lift
  [l s]
  (over l lens/id s))

(defn multi-put
  [chan-map s]
  (a/go
    (loop [chan-map chan-map
           s s]
      (if-let [channels (keys chan-map)]
        (let [[val port] (a/alts! channels)
              s (core/put (chan-map port) val s)
              chan-map (dissoc chan-map port)]
          (recur chan-map s))
        s))))

(defn multi-lift
  [ls s]
  (let [targets (map (fn [lens] [(core/focus lens s) lens]) ls)]
    (multi-put (into {} targets) s)))

(defn reflector-async-over
  [reflector af s]
  (autils/pgo-safe
    (core/put (:target reflector)
              (a/<! (apply af (p/-focus reflector s)))
              s)))

(defn reflector-async?-over
  [reflector f s]
  (autils/pgo-safe
    (core/put (:target reflector)
              (autils/?<! (apply f (p/-focus reflector s)))
              s)))

(extend-type soliton.core.Reflector
  Async-Over
  (-async-over [l af s] (reflector-async-over l af s))
  (-async?-over [l f s] (reflector-async?-over l f s)))

(defn reflect
  [lenses af s]
  (if (next lenses)
    (autils/pgo-safe
      (core/put (last lenses)
                (a/<! (apply af (map #(core/focus % s) (butlast lenses))))
                s))
    (over (first lenses) af s)))

(defn ?reflect
  [lenses f s]
  (if (next lenses)
    (autils/pgo-safe
      (core/put (last lenses)
                (autils/?<! (apply f (map #(core/focus % s) (butlast lenses))))
                s))
    (?over (first lenses) f s)))

(defn <>
  [f & ls]
  (fn [s] (reflect ls f s)))

(defn ?<>
  [f & ls]
  (fn [s] (?reflect ls f s)))

(defmacro -<>
  [x & forms]
  (let [fn-forms (soliton.core/-<>-form forms `<>)
        fn-forms (interleave fn-forms (repeat `a/<!))]
    (list `autils/pgo-safe (cons '->> (cons x fn-forms)))))

(defmacro -?<>
  [x & forms]
  (let [fn-forms (soliton.core/-<>-form forms `?<>)
        fn-forms (interleave fn-forms (repeat `autils/?<!))]
    (list `autils/pgo-safe (cons '->> (cons x fn-forms)))))
