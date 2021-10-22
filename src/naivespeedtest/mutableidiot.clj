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
