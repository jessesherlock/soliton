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

#?(:clj
   (extend-type clojure.lang.IPersistentMap
     p/Focus
     (-focus [l s] (map-focus l s))
     p/Put
     (-put [l v s] (map-put l v s))
     p/Over
     (-over [l f s] (p/default-over l f s))))

#?(:cljs
   (extend-type cljs.core/PersistentArrayMap
     p/Focus
     (-focus [l s] (map-focus l s))
     p/Put
     (-put [l v s] (map-put l v s))
     p/Over
     (-over [l f s] (p/default-over l f s))))

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

;; ** fns for lists of lenses

(defn list-put
  [lens-list vs s]
  (let [[lens & lenses] lens-list
        [v & vs] vs]
    (if lenses
      (recur lenses vs (p/-put lens v s))
      (p/-put lens v s))))

(defn list-over
  [lens-list f s]
  (p/-put lens-list (f (p/-focus lens-list s)) s))

(extend-type #?(:clj clojure.lang.IPersistentList
                :cljs cljs.core/List)
  p/Focus
  (-focus [l s] (set-focus (vec l) s))
  p/Put
  (-put [l v s] (list-put l v s))
  p/Over
  (-over [l f s] (list-over l f s)))


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

;; lens expansion
;; when using the odd lenses (maps/sets/lists) it can be useful to
;; get the set of all locations being lensed into

(defn normalize-lens
  [l]
  (cond
    (vector? l) (if (= 1 (count l)) (l 0) l)
    (fn? l) (if (= identity l) soliton.lens/id l)
    :else l))

(defn expand-lens*
  [l acc]
  (cond
    (nil? l) acc
    (vector? l) (let [[x & xs] l]
                  (expand-lens* (if xs (vec xs)) (expand-lens* x acc)))
    (map? l) (expand-lens* (set (vals l)) acc)
    (or (list? l)
        (set? l)) (let [ls (seq l)]
        (vec (mapcat #(expand-lens* % acc) ls)))
    :else (mapv #(conj % l) acc)))

(defn expand-lens
  [l]
  (mapv normalize-lens (expand-lens* l [[]])))

(def id-lens? #{soliton.lens/id identity})

(defn const-lens? [l] (= soliton.lens.Const (type l)))

(defn prefix?
  [a b]
  (or
   (id-lens? a)
   (when-not (or (nil? a) (nil? b) (const-lens? a) (const-lens? b))
     (or (and (id-lens? a) (not (id-lens? b)))
         (let [a (if (vector? a) a [a])
               b (if (vector? b) b [b])
               count-a (count a)]
           (when-not (>= count-a (count b))
             (= (seq a) (take count-a b))))))))

(defn prefix-lens?
  "a is a prefix-lens of b if the data focused by b would be inside the data focused by a."
  [a b]
  ;; if any lens in a is equal to or a prefix of any lens in b
  (some (fn [[a b]] (or (= a b) (prefix? a b)))
        (for [a (expand-lens a)
              b (expand-lens b)]
          [a b])))
