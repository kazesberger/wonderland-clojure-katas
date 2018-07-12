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

;(defn get-x1 [z y]
;  (char
;    (+ 97
;      (let [znum (index-of-char z)
;            ynum (index-of-char y)]
;        (mod (- (+ 26 znum) ynum) 26)))))

(defn get-x [z y]
  (char (+ 97
          (let [znum (index-of-char z)
                ynum (index-of-char y)]
            (-> znum
              (+ 26)
              (- ynum)
              (mod 26))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encode [kw msg]
  (apply str
    (map #(get-in substitution-chart (map index-of-char %))
      (partition 2
        (interleave
          (keyword-repeated kw msg)
          msg)))))

(defn infinite-kw [kw] (->> kw
                         (sequence)
                         (repeat)
                         (flatten)))

(defn decode [kw cipher]
  (let [x-seq (infinite-kw kw)
        z-seq cipher]
    (apply str (mapv get-x cipher x-seq))))

(defn get-x-coll [cipher message]
  (let [x-seq message
        z-seq cipher]
    (map get-x cipher message)))

(defn repeating-phrase? [kw-seq n]
  (= (seq kw-seq)
     (take (count kw-seq)
           (->> kw-seq
             (partition n)
             (first)
             (infinite-kw)))))

(defn decipher [cipher message]
  (let [x-coll (get-x-coll cipher message)]
    (apply str (take
                 (apply min
                   (filter (partial repeating-phrase? x-coll)
                         (map inc (range (count message)))))
                 x-coll))))

(comment
  (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")
  (apply min (filter (partial repeating-phrase? "vigilancev") (map inc (range (count message)))))
  (repeating-phrase? "vigilancev" 2)

  (def kw-seq "vigilancev")
  (take (count kw-seq)
        (->> kw-seq
             (partition 2)
             (first)
             (infinite-kw))))

;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def kw "foo")
  (def msg "irbaboon")
  (defn rotate-string [s]
    (str (apply str (rest s))
         (first s))))
