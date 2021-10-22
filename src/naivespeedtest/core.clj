(ns naivespeedtest.core
  (:require [criterium.core :as c]
            [net.cgrand.xforms :as x]
            [injest.path :refer [+> +>> x>> =>>]]
            [injest.state :as i.s]))

(def times-v (into [] (take 1e6) (iterate #(+ % (rand-int 1000)) 0)))

;;changed from doall to count, since retaining them ends up being
;;an exercise in GC pressure.
(defn test-it [f]
  (c/quick-bench (count (f times-v))))


(defn smt-8 [times]
  (->> times
       (partition 8 1)
       (map (juxt identity
                  (comp (partial apply -)
                        (juxt last first))))
       (filter (comp (partial > 1000) second))))

;;Execution time mean : 2.178325 sec

(defn fast-last [^clojure.lang.ISeq xs]
  (if-let [nxt  (and xs (.next xs))]
    (recur nxt)
    (.first xs)))

(defn smt-8-2 [times]
  (->> times
       (partition 8 1)
       (map (fn [x] [x (- (fast-last x) (first x))]))
       (filter (fn [x] (> 1000 (nth x 1))))))

;;Execution time mean : 1.565139 sec

;;implement a queue on a vector with subvec and meta...
(defn qpop
  ([^clojure.lang.APersistentVector v n]
   (let [k (.count v)]
     (cond (> k 1)
           (if-not (== k n)
             (let [^long depth (or (when-let [m (.meta ^clojure.lang.IMeta v)]
                                     (m :queue/depth))
                             0)]
               (if (< depth 32)
                 (.withMeta ^clojure.lang.IObj (subvec v n k)
                            {:queue/depth (unchecked-inc depth)})
                 (into [] (subvec v n k))))
              [])
           (pos? k) []
           :else  (throw (ex-info "Can't pop an empty queue vector..." {:in v})))))
  ([v] (qpop v 1)))

(defn qfirst  [v] (v 0))
(defn qlast   [v] (.peek ^clojure.lang.IPersistentStack v))

;;we just maintain an initial partition, and step it over by step-size.
;;no need for repeated O(n) counts or seq realizations...

(defn faster-partition-step
  ([window n step coll]
   (let [n-r
         (loop [idx step
                acc (qpop window step)
                ^clojure.lang.ISeq coll coll]
           (if (zero? idx)
             [acc coll]
             (when-let [v (and coll (.first coll))]
               (recur (unchecked-dec idx)
                      (.cons ^clojure.lang.IPersistentVector acc v)
                      (.next coll)))))]
     (when n-r
       (let [new-window (n-r 0)
             remaining (n-r 1)]
         (lazy-seq
          (cons new-window
                (faster-partition-step new-window n step remaining)))))))
  ([n step coll]
   (let [^clojure.lang.Counted window (vec (take n coll))]
     (when (= (.count window) n)
       (lazy-seq
        (cons window (faster-partition-step window n step (drop n coll))))))))

(defn smt-8-3 [times]
  (->> times
       (faster-partition-step 8 1)
       (map (juxt identity
                  (comp (partial apply -)
                        (juxt last first))))
       (filter (comp (partial > 1000) second))))
;;Execution time mean : 1.176623 sec

(defn smt-8-4 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (peek v) (v 0))]))
       (filter (fn [v] (>  1000 (v 1))))))

;;Execution time mean : 508.943866 ms

(defn smt-8-5 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (qlast v)
                              (qfirst v))]))
       (filter (fn [v] (>  1000 (v 1))))))
;;Execution time mean : 497.392516 ms

(defn smt-8-6 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (>  1000 (long (v 1)))))))
;;Execution time mean : 525.358682 ms

(defn smt-8-6 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (>  1000 (long (v 1)))))))
;;Execution time mean : 499.423699 ms

(i.s/reg-xf! x/reduce)
(i.s/reg-xf! x/count)
(i.s/reg-xf! x/partition)
(i.s/reg-xf! x/last)

(defn smt-8-7 [times]
  (x>> times
       (x/partition  8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (>  1000 (long (v 1)))))))
;;Execution time mean : 791.383949 ms ;;Unexpected!

(defn ^java.util.ArrayDeque mpop!  [^java.util.ArrayDeque q n]
  (dotimes [i n]
    (.removeFirst q))
  q)

(defn ^java.util.ArrayDeque mpush! [^java.util.ArrayDeque q xs]
  (doseq [x xs]
    (.add q x))
  q)

(defn ^java.util.ArrayDeque ->dq [^long n]
  (java.util.ArrayDeque. n))

(defn qclone [^java.util.ArrayDeque dq]
  (vec (.toArray dq)))


(defn mut-partition-step
  ([window n step coll]
   (let [n-r
         (loop [idx step
                ^java.util.ArrayDeque acc (mpop! window step)
                ^clojure.lang.ISeq coll coll]
           (if (zero? idx)
             [(qclone acc) coll]
             (when-let [v (and coll (.first coll))]
               (recur (unchecked-dec idx)
                      (doto  acc (.add v))
                      (.next coll)))))]
     (when n-r
       (let [new-part (n-r 0)
             remaining  (n-r 1)]
         (lazy-seq
          (cons new-part
                (mut-partition-step window n step remaining)))))))
  ([n step coll]
   (let [window (mpush! (->dq n) (take n coll))]
     (when (= (.size window) n)
       (lazy-seq
        (cons (qclone window) (mut-partition-step window n step (drop n coll))))))))

(defn smt-8-8 [times]
  (->> times
       (mut-partition-step  8 1)
       (map    (fn [v] [v (-  ^long (peek v)
                              ^long (v 0))]))
       (filter (fn [v] (> 1000  ^long (v 1))))))

;;Execution time mean : 397.179632 ms

(defn smt-8-9 [times]
  (->> times
       (mut-partition-step  8 1)
       (keep    (fn [v]
                  (let [n (-  ^long (peek v)
                              ^long (v 0))]
                    (when (>  1000 n)
                      [v n]))))))

;;Execution time mean : 303.931532 ms


;;from https://bsless.github.io/fast-and-elegant-clojure/

(defn sliding
  ([^long n]
   (sliding n 1))
  ([^long n ^long step]
   (fn [rf]
     (let [a (java.util.ArrayDeque. n)] ;; Queue here
       (fn
         ([] (rf))
         ([result] (rf result)) ;; don't need leftovers
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (vec (.toArray a))]
              ;; Remove `step` elements instead of clear
              (dotimes [_ step] (.removeFirst a))
              (rf result v))
            result)))))))

(def baseline-xf
  (comp
   (sliding 8 1)
   (map (juxt identity
              (comp (partial apply -)
                    (juxt last first))))
   (filter (comp (partial > 1000) second))))

(defn smt-8-10 [times]
  (sequence baseline-xf times))

;;Execution time mean : 782.675432 ms

(def decomposed-xf
  (comp
   (sliding 8 1)
   (map (fn [v] [v (- (last v) (first v))]))
   (filter (fn [[_ t]] (> 1000 t)))))

(defn smt-8-11 [times]
  (sequence decomposed-xf times))

;;Execution time mean : 688.363749 ms

(def vector-xf
  (comp
   (sliding 8 1)
   (map (fn [v] [v (- (peek v) (nth v 0))]))
   (filter (fn [[_ t]] (> 1000 t)))))

(defn smt-8-12 [times]
  (sequence vector-xf times))

;;Execution time mean : 293.541132 ms
(def keep-xf
  (comp
   (sliding 8 1)
   (keep (fn [v]
           (when (> 1000 (- (peek v) (nth v 0)))
             v)))))

(defn smt-8-13 [times]
  (sequence keep-xf times))

;;Execution time mean : 239.912566 ms


(set! *unchecked-math* true)
(def unchecked-xf
  (comp
   (sliding 8 1)
   (keep (fn [v]
           (when (> 1000 (unchecked-subtract (long (peek v)) (long (nth v 0))))
             v)))))
(set! *unchecked-math* false)

(defn smt-8-14 [times]
  (sequence unchecked-xf times))

;;Execution time mean : 226.269099 ms


(defn sliding-array
  ([^long n ^long step]
   (fn [rf]
     (let [a (java.util.ArrayDeque. n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (.toArray a)]
              ;; Remove `step` elements instead of clear
              (dotimes [_ step] (.removeFirst a))
              (rf result v))
            result)))))))

(set! *unchecked-math* true)
(def array-xf
  (comp
   (sliding-array 8 1)
   (keep (fn [^objects arr]
           (when (> 1000 (unchecked-subtract
                          (long (aget arr 7))
                          (long (aget arr 0))))
             arr)))))
(set! *unchecked-math* false)

(defn smt-8-15 [times]
  (sequence array-xf times))

;;Execution time mean : 42.582204 ms

(set! *unchecked-math* true)
(defn unrolled
  [^longs arr]
  (let [l (unchecked-subtract (alength arr) 7)]
    (loop [idx (int 0) agg ()]
      (if (< idx l)
        (let [idx (int idx)]
          (recur
           (unchecked-inc-int idx)
           (if (> 1000 (unchecked-subtract (aget arr (unchecked-add-int idx 7)) (aget arr idx)))
             (.cons agg idx)
             agg)))
        agg))))
(set! *unchecked-math* false)

(defn smt-8-16 [times]
  (let [arr (long-array times-v)]
    (unrolled arr)))
;;Execution time mean : 12.766361 ms
