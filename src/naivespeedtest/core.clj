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
       (filter (fn [v] (> (v 1) 1000)))))

;;Execution time mean : 570.269916 ms

(defn smt-8-5 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (qlast v)
                              (qfirst v))]))
       (filter (fn [v] (> (v 1) 1000)))))
;;Execution time mean : 545.621116 ms

(defn smt-8-6 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (> (long (v 1)) 1000)))))
;;Execution time mean : 525.358682 ms

(defn smt-8-6 [times]
  (->> times
       (faster-partition-step 8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (> (long (v 1)) 1000)))))

(i.s/reg-xf! x/reduce)
(i.s/reg-xf! x/count)
(i.s/reg-xf! x/partition)
(i.s/reg-xf! x/last)

(defn smt-8-7 [times]
  (x>> times
       (x/partition  8 1)
       (map    (fn [v] [v (-  (long (qlast v))
                              (long (qfirst v)))]))
       (filter (fn [v] (> (long (v 1)) 1000)))))
;;"Elapsed time: 862.0588 msecs" ;;Unexpected!

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
       (filter (fn [v] (> ^long (v 1) 1000)))))

;;Execution time mean : 442.740149 ms

(defn smt-8-9 [times]
  (->> times
       (mut-partition-step  8 1)
       (keep    (fn [v]
                  (let [n (-  ^long (peek v)
                              ^long (v 0))]
                    (when (> n 1000)
                      [v n]))))))

;;Execution time mean : 350.026532 ms

