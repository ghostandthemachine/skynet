(ns skynet.util.core)

(defn exp
  [x]
  (Math/exp x))



(defn sigmoid
  [x]
  (/ 1
    (+ 1 (exp (- x)))))




(defn nan?
  [v]
  (Double/isNaN v))


(defn vec?
  [v]
  (=
    clojure.lang.PersistentVector
    (class v)))

(defn log
  ([v]
    (Math/log v))
  ([base v]
  (/
    (Math/log v)
    (Math/log base))))


(defn sum
  [v]
  (apply + v))


(defn vapply
  [f & vecs]
  (map (partial apply f)
    (partition (count vecs) (apply interleave vecs))))


(defn clone
  [v n]
  (reduce
    (fn [r _]
      (conj r v))
    []
    (range n)))


(defn find-w+
  [x y]
  (let [n       (count x)
        xy-sum  (sum (vapply * x y))
        x-sum   (sum x)
        y-sum   (sum y)
        xx-sum  (sum (vapply * x x))]
    (/
      (-
        (* n xy-sum)
        (* x-sum y-sum))
      (-
        (* n xx-sum)
        (* x-sum x-sum)))))


(defn find-w0
  [x y]
  (/
    (-
      (sum y)
      (*
        (find-w+ x y)
        (sum x)))
    (count x)))


(defn find-w
  [n x y]
  (if (= n 0)
    (find-w0 x y)
    (find-w+ x y)))


(defn linear-regression
  [X Y]
  (println)
  (println "problem 1")
  (println)
  (let [w0  (find-w 0 X Y)
        w1  (find-w 1 X Y)
        h   (+
              (* (find-w 1 X Y) (- 2014 2005))
              (find-w 0 X Y))]
  (println "X:                  " X)
  (println "Y:                  " Y)
  (println "summation of (Xi):  " (sum X))
  (println "summation of (Yi):  " (sum Y))
  (println "summation of (XiYi):" (sum (vapply * X Y)))
  (println "summation of (Xi^2):" (sum (vapply * X X)))
  (println "w1:                 " w1)
  (println "w0:                 " w0)
  (println (str "h for X = :" (- 2014 2005) "       " h))
  (println)
  nil))




;; Problem 4

(defn entropy
  [p]
  (let [size (sum p)
        eq   (fn [v]
                (let [v (/ v size)]
                  (*
                    v
                    (log 2 v))))
        out  (- (apply + (map eq p)))]
    (if (nan? out)
      0
      out)))


(defn probability
  [p c]
  (/ (apply + c) (apply + p)))


(defn expected
  [p & cs]
  (apply +
    (map
      (fn [c] (* (entropy c) (probability p c)))
      (partition 2 (flatten [cs])))))


(defn gain
  [p & cs]
  (- (entropy p) (expected p cs)))


(defn test-problem4
  []
  (println "For parent [9 5] with children [2 3] [4 0] [3 2]:"
    (gain [9 5] [2 3] [4 0] [3 2]))
  

  (println "For parent [5 4] with children [2 1] [1 2] [2 1]:"
    (gain [5 4] [2 1] [1 2] [2 1]))
  

  (println "For parent [5 4] with children [1 0] [1 4] [3 0]:"
    (gain [9 5] [1 0] [1 4] [3 0])))
  



;; problem 5
(defn str->binary
  [s]
  (Integer/parseInt s 2))

(defn int->binarystr
  [x]
  (Integer/toString x 2))

(defn hammond
  [x y]
   (count
    (re-seq #"1"
      (Integer/toString
        (bit-xor
          (Integer/parseInt x 2)
          (Integer/parseInt y 2))
        2))))

; (hammond "000111100010" "000110000101")
; (hammond "111011001100" "000110000101")


;; problem 6

(defn after
  [n coll]
  (loop [n n coll coll]
    (if (= n 0)
      coll
      (recur (dec n) (rest coll)))))


(defn cross-over
  [n x y]
  [(concat (take n x) (after n y))
   (concat (take n y) (after n x))])

; (cross-over 3 [0 1 2 3 4] [9 8 7 6 5])

