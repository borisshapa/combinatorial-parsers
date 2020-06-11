(load-file "precondition.clj")

(defn vectorFunc [f]
  (fn [& vectors]
    {:pre [(and (vectors? vectors) (equalsVec? vectors))]}
    (apply mapv f vectors)))

(def v+ (vectorFunc +))
(def v- (vectorFunc -))
(def v* (vectorFunc *))

(defn v*s [vec & scalars]
  {:pre [(vectors? [vec]) (scalars? scalars)]}
  (mapv (fn [x] (* x (apply * scalars))) vec))

(defn scalar [& vectors]
  {:pre [(vectors? vectors) (equalsVec? vectors)]}
  (apply + (apply mapv * vectors)))

(defn vect [& vectors]
  {:pre [(vectors? vectors) (equalsVec? vectors)]}
  (letfn [(coordSub [x y xs ys]
            (- (* (nth x xs) (nth y ys))
               (* (nth y xs) (nth x ys))))]
    (reduce (fn [x y]
              (vector (coordSub x y 1 2)
                      (coordSub x y 2 0)
                      (coordSub x y 0 1)))
            vectors)))

(defn matrixFunc [vFunc]
  (fn [& matrices]
    {:pre [(matrices? matrices) (equalsMat? matrices)]}
    (apply mapv vFunc matrices)))

(def m+ (matrixFunc v+))
(def m- (matrixFunc v-))
(def m* (matrixFunc v*))

(defn m*s [m & scalars]
  {:pre [(matrices? (vector m)) (scalars? scalars)]}
  (mapv #(v*s % (apply * scalars)) m))

(defn transpose [m]
  {:pre [(matrices? (vector m))]}
  (apply mapv vector m))

(defn m*v [m vec]
  {:pre [(matrices? (vector m)) (vectors? (vector vec)) (mulMat? m vec)]}
  (mapv #(scalar vec %) m))

(defn m*m [& matrices]
  {:pre [(matrices? matrices) (mulMat? matrices)]}
  (reduce (fn [mx my]
            (mapv #(m*v (transpose my) %) mx))
          matrices))

(defn broadcast [tensor newShape]
  (reduce
    (fn [tensor rep] (vec (repeat rep tensor)))
    tensor
    (reverse (drop-last (count (shape tensor)) newShape))))

(defn maxShape [tensors]
  (reduce (fn [a b]
            (if (> (count a) (count b)) a b))
          (mapv #(shape %) tensors)))

(defn tensorFunc [f tensors]
  (if (every? number? tensors)
    (apply f tensors)
    (mapv #(tensorFunc f %) (apply map vector tensors))))

(defn broadcastFunc [f]
  (fn [& tensors]
  {:pre [(every? tensor? tensors) (broadcastable? tensors)]}
  (let [shape (maxShape tensors)]
    (tensorFunc f (mapv #(broadcast % shape) tensors)))))

(def b+ (broadcastFunc +))
(def b- (broadcastFunc -))
(def b* (broadcastFunc *))