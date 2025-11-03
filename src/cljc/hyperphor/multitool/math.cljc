(ns hyperphor.multitool.math
  (:require [hyperphor.multitool.core :as u]))

;;; See clojure.math.combinatorics, clojure.math.numeric-tower (gcd, lcm)

;;; ⩇⩆⩇ Tensor/vector arithmetic  ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See https://github.com/cguenthner/tensure or https://github.com/mikera/core.matrix for more industrial-strength versions
(defn tensorize
  "Vectorize but recursive, duh"
  [f]
  (letfn [(tensorized [& args]
            (if-let [l (some #(and (sequential? %) (count %)) args)]
              (let [vargs (map #(if (sequential? %) (vec %) %) args)] 
                (assert (apply = (map count (filter vector? vargs))) "vectors must be same size")
                (mapv (fn [i]
                        (apply tensorized (map (fn [arg] (if (vector? arg) (get arg i) arg)) vargs)))
                      (range l)))
              (apply f args))
            )] 
    tensorized))

;;; TODO (T fn) should work, use memoization. 

;; Basic
(def +T (tensorize +))
(def *T (tensorize *))
(def -T (tensorize -))
(def divT (tensorize /))
(def powT (tensorize Math/pow))

;; Slightly less basic
(def divxT (tensorize (fn [a b] (if (zero? b) nil (/ a b)))))
(def squareT (tensorize (fn [x] (Math/pow x 2))))
(def absT Math/abs)
(def diffT (tensorize (fn [a b] (Math/abs (- a b)))))



;;; ⩇⩆⩇ Scaling and interpolation ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn interpolate
  "Return a number between a and b, s is in interval [0, 1]"
  [a b s]
  (+ (* a (- 1 s)) (* b s)))

(def interpolateT (tensorize interpolate))

(defn interpolated
  "Return a sequence of n equally-spaced numbers between a and b"
  [a b n]
  (interpolateT a b (divT (range n) (float (dec n)))))

(defn rescale
  "Val is a number from interval [from-lower, from-upper], scale it to [to-lower, to-upper]"
  [val from-lower from-upper to-lower to-upper]
  (+ to-lower
     (* (- val from-lower)
        (/ (- to-upper to-lower)
           (- from-upper from-lower)))))

;;; ⩇⩆⩇ Sequence extrapolation ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn extrapolate
  "Extrapolate a vector of numbers by one element, using method of finite differences"
  [v]
  (if (every? zero? v)
    (conj v 0)
    (conj v (+ (last v) (last (extrapolate (mapv - (rest v) v)))))))


;;; ⩇⩆⩇ Number theory ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; An infinite sequence of primes. 
(def primes
  (cons 2
        (lazy-seq
         (filter
          (fn [n] (not (some (fn [i] (= 0 (rem n i)))
                             (take-while #(<= % (Math/sqrt n)) primes))))
          (iterate #(+ % 2) 3)
          ))))

(defn divides?
  [a b]
  (= 0 (rem a b)))

(defn prime?
  [n]
  (let [max-f (int (Math/sqrt n))]
    (not (some #(divides? n %)
               (take-while #(<= % max-f) primes)))))

;;; An infinite sequence of factorials. Not that useful, it blows out at around 30.
(def factorials
  (map * (rest (range)) (cons 1 (lazy-seq factorials))))

(defn prime-factors
  "Prime factors of n"
  [n]
  (loop [nn n
         [prime & rest-primes :as primes] primes
         acc ()]
    (cond (= nn 1) acc
          (zero? (mod nn prime))
          (recur (/ nn prime) primes (cons prime acc))
          (prime? nn)
          (cons nn acc)
          :else
          (recur nn rest-primes acc))))

;;; ⩇⩆⩇ Geometry ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn euclidean-distance
  "Euclidean distance between points. Either 2D points supplied as separate arguments, or between 2 n-dimensional points with vector coordinates"
  ([x0 y0 x1 y1]
   (euclidean-distance [x0 y0] [x1 y1]))
  ([p0 p1]
   (Math/sqrt (reduce + (squareT (-T p0 p1))))))

(defn manhattan-distance
  "Manhattan distance between points. Either 2D points supplied as separate arguments, or between 2 n-dimensional points with vector coordinates"
  ([x0 y0 x1 y1]
   (manhattan-distance [x0 y0] [x1 y1]))
  ([p0 p1]
   (reduce + (diffT p0 p1))))

(defn r2d
  "Convert radians to degrees"
  [r]
  (* r (/ 180 Math/PI)))

(defn d2r
  "Convert degrees to radians"
  [d]
  (* d (/ Math/PI 180)))

;;; ⩇⩆⩇ Naive statistics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; These are for convenience / learning and are not particularly efficient or scalable.
;;; Use a real statistics package if you have anything serious to do.
;;; eg: incanter.stats or see https://scicloj.github.io/docs/resources/libs/

(defn mean
  "Return the arithmetic mean of the elements of `seq`"
  [seq]
  (/ (reduce + seq)
     (count seq)))

(defn median
  "Return the median of the elements of `seq`"
  [seq]
  (let [sorted (sort seq)
        count (count seq)]
    (if (even? count)
      (mean [(nth sorted (/ count 2))
             (nth sorted (dec (/ count 2)))])
      (nth sorted (/ (dec count) 2)))))


(defn standard-deviation
  "Return standard deviation of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (Math/sqrt
     (/ (reduce + (squareT (-T seq mean0)))
        (count seq)))))

;;; https://en.wikipedia.org/wiki/Bessel%27s_correction
(defn standard-deviation-sample
  "Return standard deviation with Bessel correction of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (Math/sqrt
     (/ (reduce + (squareT (-T seq mean0)))
        (- (count seq) 1)))))

(defn coefficent-of-variation
  "Return coefficent of variation of the elements of `seq`"
  [seq]
  (/ (standard-deviation seq)
     (mean seq)))

(defn geometric-mean
  "Return the geometric mean of the elements of `seq`"
  [seq]
  (Math/pow (reduce * (map double seq))
            (/ 1 (count seq))))

#_
(defn covariance
  "Return the covariance between seq1 and seq2, which should be of equal size"  
  [seq1 seq2]
  (let [m1 (mean seq1)
        m2 (mean seq2)]
    (/ (reduce + (map #(* (- m1 %1) (- m2 %2)) seq1 seq2))
       (- (count seq1) 1))))

(defn covariance
  "Return the covariance between seq1 and seq2, which should be of equal size"  
  [seq1 seq2]
  (let [m1 (mean seq1)
        m2 (mean seq2)]
    (/ (reduce + (*T (-T m1 seq1) (-T m2 seq2)))
       (- (count seq1) 1))))

(defn pearson-correlation-coefficient
  "Return the correlation coefficient between seq1 and seq2, which should be of equal size. Value is in range [-1, 1]. "  
  [seq1 seq2]
  (/ (covariance seq1 seq2)
     (* (standard-deviation seq1) (standard-deviation seq2))))

(defn score-b
  "Return a list of [elt score] pairs, in descending score order."
  [keyfn seq]
  (reverse
   (sort-by second
            (map (fn [elt] [elt (keyfn elt)]) seq))))

(defn outliers-by
  "Return elements of `seq` on whom `scorefn` is more than `factor` standard-deviations away from the mean."
  [scorefn seq factor]
  (let [scores (map scorefn seq)
        mean (double (mean scores))
        threshold (* factor (standard-deviation scores))]
    (remove nil? (map (fn [elt score]
                        (when (> (Math/abs (- score mean)) threshold)
                          elt))
                      seq scores))))

(defn iles
  "Return the boundaries of deciles (n-iles) of seq"
  [seq n]
  (map first (partition (/ (count seq) n) (sort seq))))
    

