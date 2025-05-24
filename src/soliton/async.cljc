(ns soliton.async
  (:require [clojure.core.async :as a]
            [soliton.core :as core]
            [soliton.lens :as lens]
            [soliton.protocols :as p]
            [ergo.async-utils :as autils]))

(defprotocol Async-Over
  (-aover [l af s]))

(defn stitch
  [lenses steps value]
  (let [value (core/put (peek lenses) value (peek steps))
        steps (pop steps)]
    (if (empty? steps)
      value
      (recur (pop lenses) steps value))))

(declare aover)

(defn non-compound-async-over
  [l af s]
  (let [target (core/focus l s)]
    (autils/pgo-safe
      (let [value (a/<! (af target))]
        (core/put l value s)))))

(defn default-async-over
  [ls af s]
  (if (vector? ls)
    (if (= 1 (count ls))
      (non-compound-async-over (ls 0) af s)
      (let [focus-steps (pop (core/focus-steps ls s))
            subject (peek focus-steps)
            steps (pop focus-steps)
            last-lens (last ls)
            result (aover last-lens af subject)]
        (autils/pgo-safe
          (let [result (a/<! result)
                ls (pop ls)]
            (stitch ls steps result)))))
    (non-compound-async-over ls af s)))

(defn aover
  [l af s]
  (if (satisfies? Async-Over l)
    (-aover l af s)
    (default-async-over l af s)))


(defn lift
  [l s]
  (aover l lens/id s))

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

(extend-type soliton.core.Reflector
  Async-Over
  (-aover [l af s] (reflector-async-over l af s)))

(defn areflect
  [lenses af s]
  (if (next lenses)
    (autils/pgo-safe
      (core/put (last lenses)
                (a/<! (apply af (map #(core/focus % s) (butlast lenses))))
                s))
    (aover (first lenses) af s)))


 (comment

   (let [x {:foo (autils/->pchan 42)
            :bar {:baz 7 :bam (autils/->pchan 7)}
            :qux [0 1 (autils/->pchan 13)]}]
     (a/<!! (multi-lift [:foo [:bar :bam] [:qux 2]] x)))

   (defn a+ [& xs] (println "a+ args" xs) (autils/pgo-safe (apply + xs)))
   (defn ainc [x] (a/go (inc x)))

   (a/<!! (a+ 1 2 3))

   (a/<!! (aover [:foo :bar] ainc {:foo {:bar 7}}))


   ,)
  
