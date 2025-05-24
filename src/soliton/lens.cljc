(ns soliton.lens
  (:refer-clojure :exclude [dissoc first key merge next pop peek rest select-keys])
  (:require [clojure.core :as c]
            [soliton.protocols :as p])
  (:import [clojure.lang IDeref]))

(defn lens
  "create a lens from 2 or 3 separate functions"
  ([getter setter]
   (fn getter-setter-lens
     ([s] (getter s))
     ([s v] (setter s v))))
  ([getter setter updater]
   (reify
     p/Focus
     (-focus [_ s] (getter s))
     p/Put
     (-put [_ v s] (setter s v))
     p/Over
     (-over [_ f s] (updater s f)))))

(defn fmap
  "when you don't have a setter fn, you've got
  an updater/functor"
  [getter updater]
  (reify
    p/Focus
    (-focus [_ s] (getter s))
    p/Put
    (-put [_ v s] (updater s (fn [_] v)))
    p/Over
    (-over [_ f s] (updater s f))))

(defn iso
  "Isomorphism, takes isomorphic functions
  (2 inverse fns, a->b and b->a where (b->a (a->b x)) equals x)
  Useful for situations where you have a fn that works on data in one format
  and your data is in another format"
  [a->b b->a]
  (fn iso-optic
    ([s] (a->b s))
    ([_ v] (b->a v))))

;; * useful lenses

(defn id
  "Identity lens"
  {:inline (fn
             ([s] `~s)
             ([s v] `~v))
   :static true}
  ([s] s)
  ([s v] v))

(deftype Const [val]
  IDeref
  (deref [_] val)
  p/Focus
  (-focus [_ _] val))

(defn const
  "Read only lens that is always a constant value"
  [constant]
  (->Const constant))

(defn key
  "key into associative structures
  (keywords can be used on their own, other types of keys cannot)"
  [k]
  (fn key-lens
    ([s] (get s k))
    ([s v] (assoc s k v))))

(defn passes
  "a lens that only focuses if it passes the predicate
  not well behaved, inspired by lentes passes lens
  "
  [pred?]
  (fn
    ([s] (when (pred? s) s))
    ([s v] (if (pred? s) v s))))

;; ** map related

(defn select-keys
  [ks]
  (fn
    ([s] (c/select-keys s ks))
    ([s v]
     (c/merge (apply c/dissoc s ks)
              (c/select-keys v ks)))))

(defn merge
  ([s] s)
  ([s v] (c/merge s v)))

(defn- deep-merge-maps
  [a b]
  (if (map? a)
    (into a (for [[k v] b] [k (deep-merge-maps (a k) v)]))
    b))

(defn deep-merge
  ([s] s)
  ([s v] (deep-merge-maps s v)))

(defn dissoc
  ([k]
   (fn
     ([s] (c/dissoc s k))
     ([s v] (assoc v k (get s k)))))
  ([k & ks]
   (fn
     ([s] (apply c/dissoc s k ks))
     ([s v] (merge (c/select-keys s (conj ks k)) v)))))

;; ** vector related

;; idx for a lens that throws if the index is out of bounds
;; filled-idx for a lens that doesn't throw, but fills the vector with nils to expand accordingly

;; TODO cljs version
(defn idx
  [i]
  (fn idx-lens
    ([s] (.nth ^clojure.lang.Indexed s i))
    ([s v] (.assocN ^clojure.lang.IPersistentVector s i v))))

(defn fill
  [v i]
  (let [c (count v)]
    (if (> i c)
      (into v (repeat (- i c) nil))
      v)))

(defn filled-focus [i s] (if (< i (count s)) (nth s i)))

(defn filled-put [i v s] (assoc (fill (or s []) i) i v))

(defn filled-over [i f s] (update (fill (or s []) i) i f))

(defn filled-idx
  [i]
  (reify
    p/Focus
    (-focus [_ s] (filled-focus s))
    p/Put
    (-put [_ v s] (filled-put s v))
    p/Over
    (-over [_ f s] (filled-over s f))))


(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(defn- slice-focus
  [from to s n]
  (subvec s (bound 0 from n) (bound 0 to n)))

(defn slice
  [from to]
  (fn
    ([s]
     (let [n (count s)]
       (slice-focus from to s n)))
    ([s v]
     (let [n (count s)]
       (-> s
           (subvec 0 (bound 0 from n))
           (into v)
           (into (subvec s (bound 0 to n) n)))))))

(defn pop
  ([s] (c/pop s))
  ([s v] (if-let [p (c/peek s)] (conj v p) v)))

(defn peek
  ([s] (c/peek s))
  ([s v] (c/conj (or (c/pop s) []) v)))

(defn stack
  ([s] (c/peek s))
  ([s v] (c/conj s v)))

;; ** seq/list related

(defn first
  ([s] (c/first s))
  ([s v] (cons v (c/rest s))))

(defn next
  ([s] (c/next s))
  ([s v] (if s
           (cons (c/first s) v)
           (seq v))))

(defn rest
  ([s] (c/rest s))
  ([s v] (if (empty? s)
           (seq v)
           (cons (c/first s) v))))
