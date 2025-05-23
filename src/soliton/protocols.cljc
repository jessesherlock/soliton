(ns soliton.protocols)

(defprotocol Focus
  (-focus [l s]))

(defprotocol Put
  (-put [l v s]))

(defprotocol Over
  (-over [l f s]))


;; default impl fns, if you only have a put or over impl then you may want to define
;; put in terms of over and vice versa

(defn default-put
  [l v s]
  (-over l (fn [_] v) s))

(defn default-over
  [l f s]
  (-put l (f (-focus l s)) s))

