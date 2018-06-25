(ns alphabet-cipher.coder)

(defn keyword-repeated [keyword message]
  (take (count message) (-> keyword
                            (sequence)
                            (repeat)
                            (flatten))))

(defn rotate-coll [coll]
  (conj (vec (rest coll)) (first coll)))

(def alphabet (take 26 (map char (iterate inc 97))))

(def substitution-chart
  ((apply comp (repeat (rand-int 26) rotate-coll)
    (take 26 (iterate rotate-coll alphabet)))))


(defn index-of-char [c]
  (-> c
      (clojure.string/lower-case)
      (first)
      (char)
      (int)
      (- 97)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encode [keyword message]
  #_(map #(get-in substitution-chart (map index-of-char %)))
    (partition 2
      (interleave
        (keyword-repeated keyword message)
        message)))


(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")




;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (defn rotate-string [s]
    (str (apply str (rest s))
         (first s))))
