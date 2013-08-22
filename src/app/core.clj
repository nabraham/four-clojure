(ns app.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World War R!"))

;;#56
(defn count-occ [s]
  (reduce #(assoc {} (first %) (count (second %))) (group-by identity s)))

(defn calc [v1 op v2 & r]
  (if r
    (apply calc (op v1 v2) r)
    (op v1 v2)))

(defn pascal [p]
  (let [fact #(reduce * (range 1 (inc %)))
        nchoose (fn [n] (fn [k] (/ (fact n) (* (fact k)(fact (- n k))))))
        nchoosek (nchoose (dec p))]
    (map nchoosek (range p))))

(defn isbi [n]
  (if (and (sequential? n) (= (count n) 3))
    (let [[v l r] n]
      (cond
        (and (nil? l) (nil? r)) true
        (nil? l) (isbi r)
        (nil? r) (isbi l)
        :else (and (isbi r) (isbi l))))
    false))

(defn mymap [f [h & t]]
  (lazy-seq (cons (f h) (if t (mymap f t)))))

(defn ssq [s]
  (let [f (fn [x] (reduce + (map #(* % %) (map #(- (int %) 48) (seq (str x))))))
        g (fn [x] (if (< x (f x)) 1 0))]
    (reduce + (map g s))))

(defn card [cs]
  (let [suits {"C" :club "D" :diamond "H" :heart "S" :spade}
        ranks (zipmap (concat (range 2 11) ["J" "Q" "K" "A"]) (range 0 13))
        S (.substring cs 0 1)
        R (.substring cs 1 2)]
    {:suit (suits S) :rank (ranks R)}))

(defn is [s] (reverse (into [] (zipmap s (range (count s))))))

(defn lcm [s m]
  (if (every? #(= 0 (rem m %)) s)
    m
    (lcm s (+ m (first s)))))

(defn sym [n]
  (let [[v l r] n
        flip (fn f [m]
               (let [[v l r] m]
                 (if (nil? v) v
                   (list v (f r) (f l)))))]
    (= (flatten l) (flatten (flip r)))))

(defn mapwalk [M]
  (into {}
        (for [[k v] M
              [k2 v2] v]
          [[k k2] v2])))

(defn dj [S]
  (let [c (reduce + (map count S))
        c2 (count (reduce into S))]
    (= c c2)))


(defn fcomp [& fns]
  (fn [& args]
    (let [[f & fns] (reverse fns)]
      (reduce #(%2 %1) (apply f args) fns))))

(defn ws [s]
  (sort-by #(.toLowerCase %) (map str (.split (.replaceAll s "[^A-Za-z ]", "") " "))))

(defn primes [n]
  (letfn [(is-prime? [n]
            (zero? (count (filter #(zero? (rem n %)) (range 3 n 2)))))]
    (take n (lazy-cat '(2 3) (filter #(is-prime? %) (take-nth 2 (iterate inc 5)))))))

(defn only-squares [s]
  (let [introot (fn [n] (int (Math/floor (Math/sqrt n))))
        rs (fn [n] (* (introot n) (introot n)))
        is-square? (fn [n] (= (rs n) n))]
    (apply str (interpose "," (filter is-square? (map #(Integer. (str %)) (.split s ",")))))))

(defn black-box [q]
  (cond
    (associative? q) (let [s2 (+ (count q) 2)
                           sab (count (conj q [:a :b] [:a :b]))]
                       (if (= s2 sab)
                         :vector
                         :map))
    :else (let [s2 (+ (count q) 2)
                saa (count (conj q :a :a))]
            (if (= saa s2)
              :list
              :set))))

(defn perfect [n]
  (= n (reduce + (filter #(= 0 (rem n %)) (range 1 (+ (/ n 2) 1))))))

(defn anagram [v]
  (set (map set (filter #(> (count %) 1) (vals (group-by #(set (seq %)) v))))))

(defn ana2 [v]
  (->> v
    (group-by #(set (seq %)))
    vals
    (filter #(> (count %) 1))
    (map set)
    set))

(defn redu
  ([op args] (redu op (first args) (rest args)))
  ([op seed args]
   (if (not (empty? args))
     (let [v (op seed (first args))]
       (lazy-cat [seed] (redu op v (rest args))))
     [seed])))

(defn mw [op & maps]
  (let [kvs (group-by first (apply concat (map vec maps)))
        ks (keys kvs)
        vs (map #(map second %) (vals kvs))
        rs (map #(reduce op %) vs)]
    (zipmap ks rs)))

(use '[clojure.string :only (split capitalize join)])
(defn cc [s]
  (let [[h & t] (clojure.string/split s #"-")
        cap-first (fn [f & r] (str (clojure.string/capitalize f) r))]
    (apply str (conj (map cap-first t) h))))

(defn happy
  ([n] (happy n []))
  ([n seen]
   (cond
     (some #{n} seen) false
     (= n 1) true
     :else (let [ss (fn [x] (reduce + (map #(* % %) (map #(- (int %) 48) (seq (str x))))))]
             (happy (ss n) (conj seen n))))))

(use '[clojure.set :only (intersection)])
(defn et [n]
  (if (= n 1)
    1
    (let [r (fn [x] (range 1 (inc x)))
          fs (fn [x] (filter #(zero? (mod x %)) (r x)))
          cp (fn [x y]
               (let [i (clojure.set/intersection (set (fs x)) (set (fs y)))]
                 (and (= (count i) 1) (= (first i) 1))))]
      (count (filter #(cp % n) (range 1 n))))))

(defn tramp [f & args]
  (let [r (apply f args)]
    (if (fn? r)
      (tramp r (rest args))
      r)))

(defn eq [f D]
  (set (map set (vals (group-by f D)))))

(defn ps-old [S]
  (letfn [(psvec [S]
            (flatten
              (for [x S]
                (let [t (disj S x)]
                  [S t (psvec t)]))))]
    (conj (set (psvec S)) #{})))

(defn ps [S]
  (letfn [(combine [acc x]
            (conj (into acc (map #(conj % x) acc)) #{x}))]
    (conj (reduce combine #{} S) #{})))

(defn balanced? [N]
  (let [nums (map #(- (int %) 48) (seq (str N)))
        [L R] (split-at (/ (count nums) 2) nums)]
    (if (> (count L) (count R))
      (= (reduce + (butlast L)) (reduce + R))
      (= (reduce + L) (reduce + R)))))

(defn vtm-old [v]
  (let [p (partition-by keyword? v)
        ak (fn [s] (if (every? keyword? s)
                     (interpose [] s)
                     s))
        gt1 (fn [s] (> (count s) 1))
        f (fn [x] (if (gt1 x) (ak x) x))
        v2 (apply concat (map f p))
        pairs (partition-by keyword? v2)
        hm (apply hash-map pairs)
        ks (keys hm)
        vs (vals hm)]
    (zipmap (map first ks) (map #(vec (flatten (vec %))) vs))))

(defn vtm [v]
  (if (not (empty? v))
    (merge
      {(first v) (take-while (complement keyword?) (rest v))}
      (vtm (drop-while (complement keyword?) (rest v))))
    {}))

(defn base-recur [n r]
  (if (= n 0) 0
    (let [maxd (int (/ (Math/log n) (Math/log r)))
          base (fn rbase [n10 radix pow]
                 (if (>= pow 0)
                   (let [p (int (Math/pow radix pow))
                         N (quot n10 p)
                         s (- n10 (* N p))]
                     (concat [N] (rbase s radix (dec pow))))
                   '()))]
      (base n r maxd))))

(defn proseq [s]
  (let [step (vec (mapcat #(vector (count %) (first %)) (partition-by identity s)))]
    (cons step (lazy-seq (proseq step))))) 

(defn ls [& ss]
  (if (apply = (map first ss)) (first (first ss))
    (let [mx (apply max (map first ss))
          sd (map (fn [x] (drop-while #(< % mx) x)) ss)]
      (apply ls sd))))

(defn pf [x]
  (let [b #(every? sequential? %)]
    (filter (complement b)
            (tree-seq b seq x))))

(defn oscilrate [v & fs]
  (let [rotfs (concat (rest fs) [(first fs)])
        newv ((first fs) v)]
    (lazy-seq (cons v (apply oscilrate (concat [newv] rotfs))))))

(defn fcomp [& fns]
  (fn [& args]
    (let [[f & fns] (reverse fns)]
      (reduce #(%2 %1) (apply f args) fns))))

(defn gtw
  ([n p s] (butlast (gtw n p s [])))
  ([n p s r]
   (if (or (= n 0) (empty? s)) 
     r
     (if (p (first s))
       (gtw (dec n) p (rest s) (conj r (first s)))
       (gtw n p (rest s) (conj r (first s)))))))

(defn i2
  ([p v s] (if (empty? s) [] (i2 p v (rest s) [(first s)])))
  ([p v s r]
   (if (empty? s)
     r
     (if (p (last r) (first s))
       (i2 p v (rest s) (conj r v (first s)))
       (i2 p v (rest s) (conj r (first s)))))))

(defn interpose-predicated [pred in coll]
  (if (next coll)
    (->> coll
      (partition 2 1)
      (mapcat (comp next #(if (apply pred %) (interpose in %) %)))
      (cons (first coll)))
    coll))

(defn roman2dec [s]
  (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        rs (reverse (map #(m %) (seq s)))
        convert (fn conv [[h & t]  mx] 
                  (lazy-seq (cons (if (>= h mx) h (- h)) (if t (conv t (max h mx))))))]
    (reduce + (convert rs 0))))

(defn dec2roman [d] 
  (let [v (map vector [1000 900 500 400 100 90 50 40 10 9 5 4 1]
               ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])
        convert (fn conv [x smap]
                  (if (= 0 x) 
                    []
                    (if (>= x (ffirst smap))
                      (lazy-seq (cons (second (first smap)) (conv (- x (ffirst smap)) smap)))
                      (conv x (rest smap)))))]
    (apply str (convert d v))))


(defn decurry [f]
  (fn [& args]
    (reduce #(% %2) f args)))

(defn kcomb [m C]
  (let [ss (fn kc [n S]
             (if (> (count S) n) (for [x S] (kc n (disj S x))) S))
        set-vals (range (count C))
        set-keys (vec C)
        smap (zipmap set-keys set-vals)
        rsmap (zipmap set-vals set-keys)]
    (cond (> m (count C)) #{}
          (> (count C) m) (set (map set (map #(replace rsmap %) (set (flatten (ss m (set (replace smap C))))))))
          :else #{C})))

(defn trick [ts]
  (fn [cards]
    (let [ls ((first cards) :suit)
          tc (filter #(= (% :suit) ts) cards)
          lc (filter #(= (% :suit) ls) cards)
          sc (fn [x] (last (sort-by :rank x)))]
      (if (empty? tc) (sc lc) (sc tc)))))

(defn intv [s]
  (if (empty? s) []
    (let [ss (sort (distinct s))
          sss (cons (dec (first ss)) (butlast ss))
          vss (map vector ss sss)
          eq1 #(= 1 (reduce - %))
          gbd (fn gb [x r]
                (if (empty? x) r
                  (let [f (cons (first x) (take-while eq1 (rest x)))]
                    (gb (drop-while eq1 (rest x)) (conj r f)))))
          grp (map (fn [x] (map first x)) (gbd vss []))]
      (map vector (map first grp) (map last grp)))))

(defn brack-match [s]
  (let [jb (fn [x] (clojure.string/replace x #"[^\(\)\{\}\[\]]" ""))
        cb (fn [x] (clojure.string/replace x #"\[\]|\(\)|\{\}" ""))
        bm (fn m [s] 
             (cond
               (empty? s) true 
               (= (cb s) s) false
               :else (m (cb s))))]
    (bm (jb s))))

(defn tt [b]
  (let [di #(= 1 (count (distinct %)))
        ne #(not (= (first %) :e))
        uni #(and (di %) (ne %))
        br (apply map vector b)
        di [[(ffirst b) (second (second b)) (last (last b))]
            [(first (last b)) (second (second b)) (last (first b))]]
        bind (fn [x] (map vector (map uni x) (map first x))) 
        [horz vert diag] (map bind [b br di])]
    (cond 
      (some first horz) (second (first (filter first horz)))
      (some first vert) (second (first (filter first vert)))
      (some first diag) (second (first (filter first diag)))
      :else nil)))

(defn tmp [[f s & r]]
  (if (and f s)
    (let [m (map min (rest f) (butlast f))
          v (concat [(first f)] m [(last f)])]
      (tmp (cons (map + v s) r)))
    (apply min f)))

(defn leven [s t]
  (cond
    (empty? s) (count t)
    (empty? t) (count s)
    :else (let [cost (if (= (last s) (last t)) 0 1)
                ss (butlast s)
                tt (butlast t)]
            (min (inc (leven ss t)) (inc (leven s tt)) (+ cost (leven ss tt))))))

(defn leven-fast
  [a b]
  (let [m (count a)
        n (count b)
        init (apply merge-with (fn [a b] b)
                    (concat 
                      ;;deletion
                      (for [i (range 0 (+ 1 m))]
                        {i {0 i}})
                      ;;insertion
                      (for [j (range 0 (+ 1 n))]
                        {0 {j j}})))
        table (reduce
                (fn [d [i j]]
                  (merge-with 
                    (fn [a b] b) 
                    d 
                    {i {j (if (= (nth a (- i 1))
                                 (nth b (- j 1)))
                            ((d (- i 1)) (- j 1))
                            (min 
                              (+ ((d (- i 1)) 
                                    j) 1) ;;deletion
                              (+ ((d i) 
                                    (- j 1)) 1) ;;insertion
                              (+ ((d (- i 1)) 
                                    (- j 1)) 1))) ;;substitution))
                        }}))
                init
                (for [j (range 1 (+ 1 n))
                      i (range 1 (+ 1 m))] [i j]))]

    ((table m) n)))

