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

; (def net
;   (neural-network :inputs 26
;                   :outputs 4
;                   :hidden-sizes [6]))


(defn default-decoder
"By default, do nothing but pass data through"
  [x]
  x)


(defn default-encoder
"By default, do nothing but pass data through"
  [x]
  x)


(defn default-trainer
"By default, do nothing but pass data through"
  [net]
  net)


(defn binary-encoder
  "Creates a binary encoder.
  Create a specific bit size encoder with:
    (def encoder-8bit (partial binary-encoder 8))"
  ([x]
    (binary-encoder 32 x))
  ([n x]
  (let [out (loop [input x coll []]
              (if (= input 0)
                coll
                (recur (bit-shift-right input 1) (concat [(bit-and 1 input)] coll))))]
    (into []
      (if (< (count out) n)
        (reduce
          (fn [o _]
            (concat [0] o))
          out
          (range (- n (count out))))
        out)))))


(defn binary-decoder
  "Takes a vector of binary bits and converts to an integer value"
  [coll]
  (loop [x 0 input coll]
    (if (= (count input) 0)
      x
      (recur (bit-or (bit-shift-left x 1) (first input)) (rest input)))))


(def default-network-opts
   {:network-configuration
       {:input-size   []
        :hidden-sizes []
        :output-size  []}
    :network
       {:input   []
        :output  []
        :hidden  []}
    :encoder default-encoder
    :decoder default-decoder
    :trainer default-trainer})


(defn init-network
  [net]
  (merge net default-network-opts))



(defn back-propogation
  [net]
  net)


(defprotocol Trainable
  (train [net])
  (run [net & args]))


(deftype NeuralNetwork [input output hidden encoder decoder trainer]
  Trainable
  (train
    [net]
    (println "train this network"))
  (run
    [net & args]
    (println "run this network")))


(defn train-network
  "Given a network which reached an epoch, train the network with the specified training function"
  [net]
  (let [trainer (:trainer net)]
    (trainer net)))