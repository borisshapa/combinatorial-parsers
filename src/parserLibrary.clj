(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(def pos (atom 1))
(defn ex [expected actual p]
  (str "Expected: " expected ", found unexpected symbol: '" actual "' at position: " p))
(defn str-or [p]
  (clojure.string/join "/" (map str p)))

(defn _show [result]
  (if (-valid? result) (do (reset! pos 1) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result)))))
                       "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" input (_show (parser input)))) inputs))

(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (try (if (and c (p c))
      (do (swap! pos inc) (-return c cs))
      (let [[expected] (cond
                         (= p #{\u0000}) ["EOF"]
                         (= p #{" " "\t" "\n" "\r"}) ["whitespace"]
                         :else [(str-or p)])]
        (throw (ex-info
               (ex expected c @pos)
               {:ex-str expected}))))
         (catch Exception e (let [expected (:ex-str (ex-data e))]
                              (throw (ex-info
                                     (ex expected, c, @pos)
                                     {:ex-str expected})))))))

(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))

(defn _combine [f a b]
  (fn [str]
    (try (let [ar ((force a) str)
               br ((force b) (-tail ar))]
           ((_map (partial f (-value ar))) br))
         (catch Exception e (let [expected (:ex-str (ex-data e))]
                              (throw (ex-info
                                     (ex expected, (get str (- @pos 1)), @pos)
                                     {:ex-str expected})))))))
(defn _either [a b]
  (let [start-pos @pos]
    (fn [str]
    (try ((force a) str)
         (catch Exception e1 (do (reset! pos start-pos) (try ((force b) str)
                     (catch Exception e2
                       (let [expected (clojure.string/join " or " (list (:ex-str (ex-data e1)) (:ex-str (ex-data e2))))]
                                           (throw (ex-info
                                                    (ex expected, (get str (- start-pos 1)), start-pos)
                                                  {:ex-str expected})))))))))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))

(defn +map [f parser] (comp (_map f) parser))
(def +parser _parser)

(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce (partial _either) p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))