(ns skynet.core)

; (def scores (sorted-map \a 1  \b 3 ...))

;  simple binary encoding  
; (def score-encoder (int-coder :bits 4))

; (encode score-coder 3)
; binary encoding of 3
; => <Vector4 [0 0 1 1]>
; (decode score-coder *1)
; => 3

; (def letter-coder
  ; (class-coder :values (keys scores)))

(def net
  (neural-network :inputs 26
                  :outputs 4
                  :hidden-sizes [6]))