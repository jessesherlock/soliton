(ns soliton.core
  (:require [ergo.core :as ergo]
            [soliton.protocols :as p]
            [soliton.lens])
  #?(:cljs (:require-macros [soliton.core :refer [-<>]])))

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

(extend-type #?(:clj clojure.lang.Keyword :cljs cljs.core/Keyword)
  p/Focus
  (-focus [l s] (get s l))
  p/Put
  (-put [l v s] (assoc s l v))
  p/Over
  (-over [l f s] (update s l f)))

(extend-type #?(:clj clojure.lang.Fn :cljs function)
  p/Focus
  (-focus [l s] (l s))
  p/Put
  (-put [l v s] (l s v))
  p/Over
  (-over [l f s] (p/default-over l f s)))

(extend-type #?(:clj java.lang.Long :cljs number)
  p/Focus
  (-focus [l s] (soliton.lens/filled-focus l s))
  p/Put
  (-put [l v s] (soliton.lens/filled-put l v s))
  p/Over
  (-over [l f s] (soliton.lens/filled-over l f s)))

#?(:clj
(extend-type java.lang.Integer
  p/Focus
  (-focus [l s] (soliton.lens/filled-focus l s))
  p/Put
  (-put [l v s] (soliton.lens/filled-put l v s))
  p/Over
  (-over [l f s] (soliton.lens/filled-over l f s))))

(extend-type nil
  p/Focus
  (-focus [l s] nil)
  p/Put
  (-put [l v s] s)
  p/Over
  (-over [l f s] s))

;; reduce-focus, rec-put and rec-over are faster than other methods of
;; evaluating lens seqs but are opaque and
;; it's not tail recursive (but how big are your lenses anyway?)
;; clojure's assoc-in/update-in work this way

(defn reduce-focus
  [lens state]
  (if (vector? lens)
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

(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core/PersistentVector)
  p/Focus
  (-focus [l s] (reduce-focus l s))
  p/Put
  (-put [l v s] (rec-put l v s))
  p/Over
  (-over [l f s] (rec-over l f s)))

#?(:cljs
(extend-type cljs.core/Subvec
  p/Focus
  (-focus [l s] (reduce-focus l s))
  p/Put
  (-put [l v s] (rec-put l v s))
  p/Over
  (-over [l f s] (rec-over l f s))))

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

(extend-type #?(:clj clojure.lang.IPersistentMap
                :cljs cljs.core/PersistentArrayMap)
  p/Focus
  (-focus [l s] (map-focus l s))
  p/Put
  (-put [l v s] (map-put l v s))
  p/Over
  (-over [l f s] (p/default-over l f s)))

#?(:cljs
(extend-type cljs.core/PersistentHashMap
  p/Focus
  (-focus [l s] (map-focus l s))
  p/Put
  (-put [l v s] (map-put l v s))
  p/Over
  (-over [l f s] (p/default-over l f s))))

#?(:cljs
   (extend-type cljs.core/PersistentTreeMap
     p/Focus
     (-focus [l s] (map-focus l s))
     p/Put
     (-put [l v s] (map-put l v s))
     p/Over
     (-over [l f s] (p/default-over l f s)))) 


;; ** fns for sets of lenses

(defn set-focus
  [lens-set s]
  (into (empty lens-set) (map #(p/-focus % s) lens-set)))

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

(extend-type #?(:clj clojure.lang.IPersistentSet
                :cljs cljs.core/PersistentHashSet)
  p/Focus
  (-focus [l s] (set-focus l s))
  p/Put
  (-put [l v s] (set-put l v s))
  p/Over
  (-over [l f s] (set-over l f s)))

#?(:cljs
(extend-type cljs.core/PersistentTreeSet
  p/Focus
  (-focus [l s] (set-focus l s))
  p/Put
  (-put [l v s] (set-put l v s))
  p/Over
  (-over [l f s] (set-over l f s))))

;; * Reflection

(defrecord Reflector [sources target])

(defn reflector
  [& ls]
  (if (next ls)
    (->Reflector (butlast ls) (last ls))
    (->Reflector ls (first ls))))

(extend-type soliton.core.Reflector
  p/Focus
  (-focus [l s] (map #(p/-focus % s) (:sources l)))
  p/Put
  (-put [l v s] (put (:target l) v s))
  p/Over
  (-over [l f s] (put (:target l)
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

(defn -<>-form
  [forms <>-fn-sym]
  (loop [x [], forms forms]
    (if forms
      (let [form (first forms)
            wrapped (if (seq? form)
                      (conj x (list `(~<>-fn-sym ~@form)))
                      (conj x form))]
        (recur wrapped (next forms)))
      (seq x))))

(defmacro -<>
  [x & forms]
  (cons '->> (cons x (-<>-form forms `<>))))
