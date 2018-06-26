(ns alphabet-cipher.coder)

(defn keyword-repeated [keyword message]
  (take (count message) (-> keyword
                            (sequence)
                            (repeat)
                            (flatten))))

; implement as transducer? z = r^n(f(x,y)) ...eigentlich kein xf
(defn rotate-coll [coll]
  (conj (vec (rest coll)) (first coll)))

(def alphabet (take 26 (map char (iterate inc 97))))

(def substitution-chart
  ;((apply comp (repeat (rand-int 26) rotate-coll))
   (vec (take 26 (iterate rotate-coll (vec alphabet)))))

(defn index-of-char [c]
  (-> c
      (clojure.string/lower-case)
      (first)
      (char)
      (int)
      (- 97)))

(defn get-z [x y]
  (char
    (+ 97
      (let [x (index-of-char x)
            y (index-of-char y)]
        (mod  (+ x y) 26)))))

(defn get-x [z y]
  (char
    (+ 97
      (let [z (index-of-char z)
            y (index-of-char y)]
        (mod (- (+ 26 z) y) 26)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encode [kw msg]
  (apply str
    (map #(get-in substitution-chart (map index-of-char %))
      (partition 2
        (interleave
          (keyword-repeated kw msg)
          msg)))))

(defn infinite-kw [kw] (-> kw
                         (sequence)
                         (repeat)
                         (flatten)))

(defn decode [kw msg]
  (apply str (mapv get-x (infinite-kw kw) msg)))

(defn decipher [cipher message]
  "decypherme")


;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defn rotate-string [s]
    (str (apply str (rest s))
         (first s))))
