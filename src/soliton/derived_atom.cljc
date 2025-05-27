(ns soliton.derived-atom
  (:refer-clojure :exclude [derive])
  (:require [soliton.core :refer [focus put over]]))

;; Note: Lens' provided to derived atoms are used with soliton.core lens operators
;; Soliton.SM-specific functionality or soliton.async-specific functionality
;; won't work properly

#?(:clj  
   (deftype DerivedAtom [lens src
                         id eq-fn
                         ^:unsynchronized-mutable cache
                         ^:unsynchronized-mutable src-cache
                         ^:unsynchronized-mutable watchers]

     clojure.lang.IDeref
     (deref [self]
       (locking self
         (let [src-val (deref src)]
           (if (identical? (.-src-cache self) src-val)
             (.-cache self)
             (let [new-val (focus lens src-val)]
               (set! (.-cache self) new-val)
               (set! (.-src-cache self) src-val)
               new-val)))))

     clojure.lang.IAtom
     (reset [self new-val]
       (focus lens (swap! src (fn ri [s] (put lens new-val s)))))
     (swap [self f]
       (focus lens (swap! src (fn si [s] (over lens f s)))))
     (swap [self f x]
       (focus lens (swap! src (fn si2 [s] (over lens #(f % x) s)))))
     (swap [self f x y]
       (focus lens (swap! src (fn si3 [s] (over lens #(f % x y) s)))))
     (swap [self f x y more]
       (focus lens (swap! src (fn sim [s] (over lens #(apply f % x y more) s)))))

     clojure.lang.IAtom2
     (resetVals [self new-val]
       (mapv (fn rvf [s] (focus lens s))
             (swap-vals! src (fn rvi [s] (put lens new-val s)))))
     (swapVals [self f]
       (mapv (fn svf [s] (focus lens s))
             (swap-vals! src (fn svi [s] (over lens f s)))))
     (swapVals [self f x]
       (mapv (fn svf2 [s] (focus lens s))
             (swap-vals! src (fn svi2 [s] (over lens #(f % x) s)))))
     (swapVals [self f x y]
       (mapv (fn svf3 [s] (focus lens s))
             (swap-vals! src (fn svi3 [s] (over lens #(f % x y) s)))))
     (swapVals [self f x y more]
       (mapv (fn svfm [s] (focus lens s))
             (swap-vals! src (fn svim [s] (over lens #(apply f % x y more) s)))))

     clojure.lang.IRef
     (addWatch [self id-key cb-fn]
       (locking self
         (when-not watchers
           (set! (.-watchers self) {})
           (add-watch src id
                      (fn swfn [_ _ old-src-val new-src-val]
                        (when-not (identical? old-src-val new-src-val)
                          (let [old-val (focus lens old-src-val)
                                new-val (focus lens new-src-val)]
                            (set! (.-cache self) new-val)
                            (set! (.-src-cache self) new-src-val)
                            (when-not (eq-fn old-val new-val)
                              (run! (fn [[k wfn]]
                                      (wfn k self old-val new-val))
                                    (.-watchers self))))))))
         (set! (.-watchers self) (assoc watchers id-key cb-fn))
         self))
     (removeWatch [self id-key]
       (locking self
         (set! (.-watchers self) (dissoc watchers id-key))
         (when (empty? watchers)
           (set! (.-watchers self) nil)
           (remove-watch src id))))))

#?(:cljs
   (deftype DerivedAtom [lens src
                         id eq-fn
                         ^:mutable cache
                         ^:mutable src-cache
                         ^:mutable watchers]

     IAtom
     IDeref
     (-deref [self]
       (let [src-val (deref src)]
         (if (identical? (.-src-cache self) src-val)
           (.-cache self)
           (let [new-val (focus lens src-val)]
             (set! (.-cache self) new-val)
             (set! (.-src-cache self) src-val)
             new-val))))

     IReset
     (-reset! [self new-val]
       (swap! src (fn ri [s] (put lens new-val s)))
       (deref self))

     ISwap
     (-swap! [self f]
       (swap! src (fn si [s] (over lens f s)))
       (deref self))
     (-swap! [self f x]
       (swap! src (fn si2 [s] (over lens #(f % x) s)))
       (deref self))
     (-swap! [self f x y]
       (swap! src (fn si3 [s] (over lens #(f % x y) s)))
       (deref self))
     (-swap! [self f x y more]
       (swap! src (fn sim [s] (over lens #(apply f % x y more) s)))
       (deref self))

     IWatchable
     (-add-watch [self id-key cb-fn]
       (when-not watchers
         (set! (.-watchers self) {})
         (add-watch src id
                    (fn swfn [_ _ old-src-val new-src-val]
                      (when-not (identical? old-src-val new-src-val)
                        (let [old-val (focus lens old-src-val)
                              new-val (focus lens new-src-val)]
                          (set! (.-cache self) new-val)
                          (set! (.-src-cache self) new-src-val)
                          (when-not (eq-fn old-val new-val)
                            (run! (fn [[k wfn]]
                                    (wfn k self old-val new-val))
                                  (.-watchers self))))))))
       (set! (.-watchers self) (assoc watchers id-key cb-fn))
       self)
     (-remove-watch [self id-key]
       (set! (.-watchers self) (dissoc watchers id-key))
       (when (empty? watchers)
         (set! (.-watchers self) nil)
         (remove-watch src id)))))

#?(:clj 
   (deftype DerivedROAtom [lens src
                           id eq-fn
                           ^:unsynchronized-mutable cache
                           ^:unsynchronized-mutable src-cache
                           ^:unsynchronized-mutable watchers]

     clojure.lang.IDeref
     (deref [self]
       (locking self
         (let [src-val (deref src)]
           (if (identical? (.-src-cache self) src-val)
             (.-cache self)
             (let [new-val (focus lens src-val)]
               (set! (.-src-cache self) src-val)
               (set! (.-cache self) new-val)
               new-val)))))

     clojure.lang.IRef
     (addWatch [self id-key cb-fn]
       (locking self
         (when-not watchers
           (set! watchers {})
           (add-watch src id
                      (fn swfn [_ _ old-src-val new-src-val]
                        (when-not (identical? old-src-val new-src-val)
                          (let [old-val (focus lens old-src-val)
                                new-val (focus lens new-src-val)]
                            (set! (.-src-cache self) new-src-val)
                            (set! (.-cache self) new-val)
                            (when-not (eq-fn old-val new-val)
                              (run! (fn [[k wfn]]
                                      (wfn k self old-val new-val))
                                    (.-watchers self))))))))
         (set! watchers (assoc watchers id-key cb-fn))
         self))
     (removeWatch [self key]
       (locking self
         (set! watchers (dissoc watchers key))
         (when (empty? watchers)
           (set! watchers nil)
           (remove-watch src id))))))

#?(:cljs
   (deftype DerivedROAtom [lens src
                           id eq-fn
                           ^:mutable cache
                           ^:mutable src-cache
                           ^:mutable watchers]

     IAtom
     IDeref
     (-deref [self]
       (let [src-val (deref src)]
         (if (identical? (.-src-cache self) src-val)
           (.-cache self)
           (let [new-val (focus lens src-val)]
             (set! (.-cache self) new-val)
             (set! (.-src-cache self) src-val)
             new-val))))

     IWatchable
     (-add-watch [self id-key cb-fn]
       (when-not watchers
         (set! (.-watchers self) {})
         (add-watch src id
                    (fn swfn [_ _ old-src-val new-src-val]
                      (when-not (identical? old-src-val new-src-val)
                        (let [old-val (focus lens old-src-val)
                              new-val (focus lens new-src-val)]
                          (set! (.-cache self) new-val)
                          (set! (.-src-cache self) new-src-val)
                          (when-not (eq-fn old-val new-val)
                            (run! (fn [[k wfn]]
                                    (wfn k self old-val new-val))
                                  (.-watchers self))))))))
       (set! (.-watchers self) (assoc watchers id-key cb-fn))
       self)
     (-remove-watch [self id-key]
       (set! (.-watchers self) (dissoc watchers id-key))
       (when (empty? watchers)
         (set! (.-watchers self) nil)
         (remove-watch src id)))))


(defn derive
  ([lens src]
   (derive lens src identical?))
  ([lens src eq-fn]
   (let [id (gensym "soliton-DA-")]
     (DerivedAtom. lens src id eq-fn {} nil nil))))

(defn derive-ro
  ([lens src]
   (derive-ro lens src identical?))
  ([lens src eq-fn]
   (let [id (gensym "soliton-DROA-")]
     (DerivedROAtom. lens src id eq-fn {} nil nil))))


