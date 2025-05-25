(ns soliton.core
  (:require [ergo.core :as ergo]
            [soliton.protocols :as p]
            [soliton.lens]))

(defn focus
  {:inline (fn [l s] `(soliton.protocols/-focus ~l ~s))}
  [l s]
  (p/-focus l s))

(defn put
  [l v s]
  (p/-put l v s))

(defn over
  [l f s]
  (p/-over l f s))

(defn focus-rf
  [state lens]
  (p/-focus lens state))

(defn focus-steps
  [lens state]
  (transduce (ergo/reductions focus-rf state)
             conj
             []
             (if (vector? lens) lens [lens])))

(extend clojure.lang.Keyword            ; cljs cljs.core/Keyword
  p/Focus
  {:-focus (fn [l s] (get s l))}
  p/Put
  {:-put (fn [l v s] (assoc s l v))}
  p/Over
  {:-over (fn [l f s] (update s l f))})

(extend clojure.lang.Fn                 ; cljs function
  p/Focus
  {:-focus (fn [l s] (l s))}
  p/Put
  {:-put (fn [l v s] (l s v))}
  p/Over
  {:-over p/default-over})

(extend java.lang.Long                  ; cljs number
  p/Focus
  {:-focus soliton.lens/filled-focus}
  p/Put
  {:-put soliton.lens/filled-put}
  p/Over
  {:-over soliton.lens/filled-over})

(extend nil
  p/Focus
  {:-focus (fn [l s] nil)}
  p/Put
  {:-put (fn [l v s] s)}
  p/Over
  {:-over (fn [l f s] s)})

;; reduce-focus, rec-put and rec-over are faster than other methods of
;; evaluating lens seqs but are opaque and
;; it's not tail recursive (but how big are your lenses anyway?)
;; clojure's assoc-in/update-in work this way

(defn reduce-focus
  [lens state]
  (if (sequential? lens)
    (reduce focus-rf state lens)
    (p/-focus lens state)))

(defn rec-put
  [[l & ls] v s]
  (if ls
    (put l (rec-put ls v (focus l s)) s)
    (put l v s)))

(defn rec-over
  [[l & ls] f s]
  (if ls
    (put l (rec-over ls f (focus l s)) s)
    (over l f s)))

(extend clojure.lang.Sequential
  p/Focus
  {:-focus reduce-focus}
  p/Put
  {:-put rec-put}
  p/Over
  {:-over rec-over})


;; ** fns for maps of lenses

(defn map-focus
  [lens-map s]
  (reduce-kv (fn [init k v]
               (assoc init k (p/-focus v s)))
             (empty lens-map)
             lens-map))

(defn map-put
  [lens-map value-map s]
  (reduce-kv (fn [init k v]
               (p/-put v (get value-map k) init))
             s
             lens-map))

(extend clojure.lang.APersistentMap
  p/Focus
  {:-focus map-focus}
  p/Put
  {:-put map-put}
  p/Over
  {:-over p/default-over})

;; ** fns for sets of lenses

(defn set-focus
  [lens-set s]
  (into #{} (map #(p/-focus % s) lens-set)))

(defn set-put
  [lens-set v s]
  (let [[lens & lenses] lens-set]
    (if lenses
      (recur lenses v (p/-put lens v s))
      (p/-put lens v s))))

(defn set-over
  [lens-set f s]
  (let [[lens & lenses] lens-set]
    (if lenses
      (recur lenses f (p/-over lens f s))
      (p/-over lens f s))))

(extend-type clojure.lang.PersistentHashSet
  p/Focus
  (p/-focus [l s] (set-focus l s))
  p/Put
  (p/-put [l v s] (set-put l v s))
  p/Over
  (p/-over [l f s] (set-over l f s)))

;; * Reflection

(defrecord Reflector [sources target])

(defn reflector
  [& ls]
  (if (next ls)
    (->Reflector (butlast ls) (last ls))
    (->Reflector ls (first ls))))

(extend-type soliton.core.Reflector
  p/Focus
  (p/-focus [l s] (map #(p/-focus % s) (:sources l)))
  p/Put
  (p/-put [l v s] (put (:target l) v s))
  p/Over
  (p/-over [l f s] (put (:target l)
                        (apply f (p/-focus l s))
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
  (let [x ['->> x]]
    (loop [x x, forms forms]
      (if forms
        (let [form (first forms)
              wrapped (if (seq? form)
                        (conj x (list (cons `<> form)))
                        (conj x form))]
          (recur wrapped (next forms)))
        (seq x)))))
