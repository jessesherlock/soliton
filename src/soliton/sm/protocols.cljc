(ns soliton.sm.protocols)

(defprotocol Focus
  (-focus [l s]))

(defprotocol Put
  (-put [l v s]))

(defprotocol Over
  (-over [l f s]))

(defn default-put
  [l v s]
  (-over l (fn [_] v) s))

(defn default-over
  [l f s]
  (-put l (f (-focus l s)) s))
