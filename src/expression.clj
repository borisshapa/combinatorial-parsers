(load-file "src/parserLibrary.clj")

(defn getField [this key]
  (if (contains? this key)
    (this key)
    (getField (:prototype this) key)))

(defn field [key]
  (fn [this] (getField this key)))

(defn func [key]
  (fn [this & args]
    (apply (fn [this key & args]
             (apply (getField this key) this args)) this key args)))

(def evaluate (func :evaluate))
(def toString (func :toString))
(def diff (func :diff))
(def toStringInfix (func :toStringInfix))

(declare ZERO)
(declare ONE)

(def ConstantPrototype
  (let [cnst (field :value)]
    {:evaluate      (fn [this _] (cnst this))
     :toString      (fn [this] (format "%.1f" (cnst this)))
     :toStringInfix (fn [this] (format "%.1f" (cnst this)))
     :diff          (fn [_ _] ZERO)}))

(defn Constant [number]
  {:prototype ConstantPrototype
   :value     number})

(def ZERO (Constant 0))
(def ONE (Constant 1))

(def VariablePrototype
  (let [variable (field :value)]
    {:evaluate      (fn [this vars] (vars (variable this)))
     :toString      (fn [this] (variable this))
     :toStringInfix (fn [this] (variable this))
     :diff          (fn [this var] (if (= (variable this) var) ONE ZERO))}))
(defn Variable
  [var]
  {:prototype VariablePrototype
   :value     var})

(let [calc (field :calc)
      name (field :name)
      derivative (field :derivative)
      args (field :args)]

  (def OperationPrototype
    {:evaluate (fn [this vars] (apply (calc this) (mapv (fn [x] (evaluate x vars)) (args this))))
     :toString #(str "(" (name %) " " (clojure.string/join " " (map toString (args %))) ")")
     :diff     (fn [this var] (apply (derivative this) (concat (args this) (map (fn [x] (diff x var)) (args this)))))})

  (def UnOp
    {:prototype     OperationPrototype
     :toStringInfix (fn [this]
                      (str (name this) "(" (toStringInfix (first (args this))) ")"))})
  (def BinOp
    {:prototype     OperationPrototype
     :toStringInfix (fn [this]
                      (str "(" (toStringInfix (first (args this))) " " (name this) " " (toStringInfix (second (args this))) ")"))})
  )

(defn Op [calc name derivative type]
  (let [proto (if (== type 0) UnOp BinOp)
        OpProto {:prototype  proto
                 :calc       calc
                 :name       name
                 :derivative derivative}]
    (fn [& operands]
      {:prototype OpProto
       :args      (vec operands)})))

(def Add (Op
           +
           "+"
           (fn [x y dx dy] (Add dx dy)) 1))

(def Subtract (Op
                -
                "-"
                (fn [x y dx dy] (Subtract dx dy)) 1))

(def Multiply (Op
                *
                "*"
                (fn [x y dx dy] (Add (Multiply dx y) (Multiply x dy))) 1))

(def Divide (Op
              #(/ (double %1) %2)
              "/"
              (fn [x y dx dy] (Divide (Subtract (Multiply dx y) (Multiply x dy)) (Multiply y y))) 1))

(def Negate (Op
              -
              "negate"
              (fn [x dx] (Negate dx)) 0))

(def Abs (Op
           (fn [x] (Math/abs x))
           "abs"
           (fn [x dx] (Divide (Multiply dx x) (Abs x))) 0))

(def Square (Op
              (fn [x] (* x x))
              "square"
              (fn [x dx] (Multiply (Constant 2) x dx)) 0))

(def Sqrt (Op
            (fn [x] (Math/sqrt (Math/abs x)))
            "sqrt"
            (fn [x dx] (Divide (Multiply x dx) (Multiply (Constant 2) (Abs x) (Sqrt (Abs x))))) 0))

(def Log (Op
           (fn [x y] (/ (Math/log (Math/abs y)) (Math/log (Math/abs x))))
           "//"
           (fn [x y dx dy] (Subtract
                             (Divide dx (Multiply x (Log y (Constant Math/E))))
                             (Divide dx (Multiply (Square (Log y x)) x (Log y (Constant Math/E)))))) 1))

(def Pow (Op
           (fn [x y] (Math/pow x y))
           "**"
           (fn [x y dx dy] (Add (Multiply (Divide y (Constant 1)) (Pow x y) dx) (Multiply (Pow x y) (Log x (Constant Math/E)) dy))) 1))

; ===== Parser combinator =====
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))

(def *digit (+char "0123456789"))
(def *pos-integer (+str (+plus *digit)))
(def *number (+map read-string (+str (+seqf concat (+seq (+opt (+char "-")) *pos-integer) (+opt (+seq (+char ".") *pos-integer))))))
(def *variable (+map str (+char "xyz")))

(def *arg (+or (+map Constant *number) (+map Variable *variable)))
(defn *str-op [s] (+ignore (apply +seq (map #(+char (str %)) (seq s)))))

(defn *operation [d]
  (apply +or (map (fn [[key val]] (+seqf (constantly val) (*str-op key))) d)))

(defn *multioperation [*next-priority ops fold]
  (+map fold (+seqf cons *next-priority (+star (+seq *ws (*operation ops) *ws *next-priority)))))

(defn *rfun [a & b] (if (empty? b) a
                      (let [[op w] (first b)] (op a (apply *rfun w (rest b))))))
(def *fold-right (partial apply *rfun))

(def *fold-left (partial reduce (fn [a [op b]] (op a b))))

(declare *add-sub)

(def *primary-brackets (+or *arg
                   (+seqf (fn [op a] (op a)) (*operation {"negate" Negate}) *ws (delay *primary-brackets))
                   (+seqn 1 (+char "(") *ws (delay *add-sub) *ws (+char ")"))))

(def *pow-log (*multioperation
                *primary-brackets
                {"**" Pow, "//" Log}
                *fold-right))
(def *mul-div (*multioperation
                *pow-log
                {"*" Multiply, "/" Divide}
                *fold-left))
(def *add-sub (*multioperation
                *mul-div
                {"+" Add, "-" Subtract}
                *fold-left))

(def parseObjectInfix (+parser (+seqn 0 *ws *add-sub *ws)))