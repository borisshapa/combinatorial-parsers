; Scalars
(defn scalars? [scalars]
  (every? number? scalars))

; Vectors
(defn equalsVec? [vecs]
  (every? (let [first-count (count (first vecs))]
            #(== % first-count))
          (map count vecs)))

(defn vectors? [vecs]
  (and (every? vector? vecs)
       (every? scalars? vecs)))

; Matrices
(defn equalsMat? [matrices]
  (and (equalsVec? matrices)
       (equalsVec? (map first matrices))))

(defn matrices? [matrices]
  (and (every? vector? matrices)
       (every? vectors? matrices)
       (every? #(equalsVec? %) matrices)))

(defn mulMat? [& matrices]
  (let [mat (first matrices)
        seqMat (rest matrices)]
    (if (empty? seqMat)
      true
      (and
        (== (count (first mat)) (count (first seqMat)))
        (mulMat? (rest matrices))))))

; Tensors
(defn shape [tensor]
  (if (number? tensor)
    ()
    (cons (count tensor) (shape (first tensor)))))

(defn tensor? [tensor]
  (if (or (number? tensor) (every? number? tensor))
    true
    (if (and (every? vector? tensor) (equalsVec? tensor))
      (tensor? ((fn [t] (reduce (partial reduce conj) t)) tensor)))))

(defn broadcastable? [& tensors]
  (let [shapes (map shape tensors)
        first (first shapes)]
    (every? #(or (every? true? (map == % (take-last (count %) first)))
                 (every? true? (map == first (take-last (count first) %))))
            shapes)))

