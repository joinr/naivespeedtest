(ns naivespeedtest.mutableidiot
  (:require [criterium.core :as c]))

;; const smt = input => {
;;                       const partition = [], result = []
;;                       for (let i = 0; i < input.length; i++) partition[i] = input.slice(i, i + 8)
;;                                partition.reduce((x, y) => {
;;                                                            const s = y[y.length - 1] - y[0]
;;                                                            if (s < 1000) result.push([y, s])
;;                                                            }, result)
;;                                return result
;;                                }

(def times-v (into [] (take 1e6) (iterate #(+ % (rand-int 1000)) 0)))

(defn smt [input]
  (->>  (range (- (count input) 8))
        (reduce (fn [acc idx]
                  (let [y (subvec input idx (+ idx 8))
                        s (- (peek y) (y 0))]
                    (if (< s 1000)
                      (conj acc [y s])
                      acc))) [])))

(set! *unchecked-math* true)

(defn smtchecked [input]
  (->>  (range (- (count input) 8))
        (reduce (fn [acc idx]
                  (let [y (subvec input idx (+ idx 8))
                        s (- (peek y) (y 0))]
                    (if (< s 1000)
                      (conj acc [y s])
                      acc))) [])))
(set! *unchecked-math* false)

(defn smttr [input]
  (->>  (range (- (count input) 8))
        (reduce (fn [acc idx]
                  (let [y (subvec input idx (+ idx 8))
                        s (- (peek y) (y 0))]
                    (if (< s 1000)
                      (conj! acc [y s])
                      acc))) (transient []))
        (persistent!)))

(set! *unchecked-math* true)
(defn smtfly [input]
  (->>  (range (- (count input) 8))
        (reduce (fn [acc idx]
                  (let [l (input idx)
                        r (input (+ idx 8))
                        s (- r l)]
                    (if (< s 1000)
                      (conj acc [(subvec input idx (+ idx 8)) s])
                      acc))) [])))
(set! *unchecked-math* false)

(set! *unchecked-math* true)
(defn smtfly2 [input]
  (let [arr (long-array input)]
    (->>  (range (- (count input) 8))
          (reduce (fn [acc ^long idx]
                    (let [l (aget arr idx)
                          r (aget arr (+ idx 8))
                          s (- r l)]
                      (if (< s 1000)
                        (conj acc [(subvec input idx (+ idx 8)) s])
                        acc))) []))))
(set! *unchecked-math* false)

(set! *unchecked-math* true)
(defn smtfly3 [input]
  (let [arr (long-array input)]
    (->>  (range (- (count input) 8))
          (reduce (fn [^java.util.List acc ^long idx]
                    (let [l (aget arr idx)
                          r (aget arr (+ idx 8))
                          s (- r l)]
                      (if (< s 1000)
                        (doto  acc (.add [(subvec input idx (+ idx 8)) s]))
                        acc))) (java.util.ArrayList.)))))
(set! *unchecked-math* false)


;;u/didbus implementations
(set! *unchecked-math* true)
(defn smt-8ii [times-vec]
  (loop [res (transient [])
         pointer-1 0
         pointer-2 7]
    (if-let [end-element (get times-vec pointer-2)]
      (let [start-element (get times-vec pointer-1)
            time-diff (- end-element start-element)]
        (recur (if (< time-diff 1000)
                 (conj! res [(subvec times-vec pointer-1 (inc pointer-2))
                             time-diff])
                 res)
               (inc pointer-1) (inc pointer-2)))
      (persistent! res))))
;;Execution time mean : 43.059826 ms

;;'With performance tweaks: Execution time mean : 23.567174 ms'

(defn smt-8iii [times-vec]
  (binding [*unchecked-math* true]
    (loop [res (transient [])
           pointer-1 (int 0)
           pointer-2 (int 7)]
      (if-let [end-element (get times-vec pointer-2)]
        (let [start-element (get times-vec pointer-1)
              time-diff (- end-element start-element)]
          (recur (if (< time-diff 1000)
                   (conj! res [(subvec times-vec pointer-1 (inc pointer-2))
                               time-diff])
                   res)
                 (inc pointer-1)
                 (inc pointer-2)))
        (persistent! res)))))

;;Execution time mean : 42.147598 ms

;;'Less idiomatic as I'm using an array: Execution time mean : 1.678226 ms'

(defn smt-8iv [^"[J" times-arr]
  (binding [*unchecked-math* true]
    (loop [res (transient [])
           pointer-1 (int 0)
           pointer-2 (int 7)]
      (if (< pointer-2 (alength times-arr))
        (let [start-element (aget times-arr pointer-1)
              end-element   (aget times-arr pointer-2)
              time-diff     (- end-element start-element)]
          (recur (if (< time-diff 1000)
                   (conj! res [(mapv #(aget times-arr (+ pointer-1 %))
                                     (range 8)) time-diff])
                   res)
                 (inc pointer-1)
                 (inc pointer-2)))
        (persistent! res)))))

(set! *unchecked-math* false)
;;'All my solutions produce exactly the same result where it includes both the list of elements and their time difference.'
