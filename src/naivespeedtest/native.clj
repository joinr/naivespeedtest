(ns naivespeedtest.native
  (:require [uncomplicate.clojurecl.core :refer :all]
            [uncomplicate.clojurecl.info :refer :all])
  (:require [uncomplicate.commons.core :refer :all]))

;;platforms - I have intel and nvidia
;;naivespeedtest.native> (map name-info (platforms))
;;("NVIDIA CUDA" "Intel(R) OpenCL")
;;naivespeedtest.native>


(def cuda-platform  (first (platforms)))
(def intel-platform (second (platforms)))
#_#_#_
(def cuda-gpu  (first (devices cuda-platform)))
(def intel-gpu (first (devices intel-platform)))
(def intel-cpu (second (devices intel-platform)))

(def ctx (context [cuda-gpu]))

(def times-v (into [] (take 1e6) (iterate #(+ % (rand-int 1000)) 0)))

(defn smt-8-base [times]
  (->> times
       (partition 8 1)
       (map-indexed (fn [idx xs]
                      [xs (- (last xs) (first xs)) idx]))
       (filter (comp (partial > 1000) second))))

;;we want a buffer size of 4*n for ints.

(def smt-source
  "__kernel void smt (__global int* acc,
                        __global int* x) {

    uint id = get_global_id(0);
    uint n = x[id + 7] - x[id];
    acc[id] = n < 1000;
    acc[id] = acc[id]*n;
   }")

(set! *unchecked-math* true)
(let [program-source smt-source]
  (defn smt-8 [in]
    (let [x          (int-array in) ;;paying for array conversion...
          n          (alength x)
          acc        (int-array n)
          work-sizes (work-size [n])]
      (with-release [dev      (first (devices cuda-platform))
                     ctx      (context [dev])
                     cqueue   (command-queue ctx dev)
                     cl-x     (cl-buffer ctx (* n Integer/BYTES) :read-only)
                     cl-acc   (cl-buffer ctx (* n Integer/BYTES) :read-write)
                     prog     (build-program! (program-with-source ctx [program-source]) [dev] nil nil nil)
                     smt      (kernel prog "smt")]
        (set-args! smt cl-acc cl-x )
        (enq-write!  cqueue cl-x x)
        (enq-kernel! cqueue smt work-sizes)
        (enq-read!   cqueue cl-acc acc)
        (areduce acc idx res []
           (let [v (aget acc idx)]
             (if (zero? v)
               res
               (conj res [idx (+ idx 8) v]))))))))
(set! *unchecked-math* false)

(set! *unchecked-math* true)
(defn smt-8-cached [in dev ctx cqueue smt-prog]
  (let [x          (int-array in) ;;paying for array conversion...
        n          (alength x)
        acc        (int-array n)
        work-sizes (work-size [n])]
    (with-release [cl-x     (cl-buffer ctx (* n Integer/BYTES) :read-only)
                   cl-acc   (cl-buffer ctx (* n Integer/BYTES) :read-write)]
      (let [x          (int-array in) ;;paying for array conversion...
            n          (alength x)
            acc        (int-array n)
            work-sizes (work-size [n])]
        (set-args! smt-prog cl-acc cl-x )
        (enq-write!  cqueue cl-x x)
        (enq-kernel! cqueue smt-prog work-sizes)
        (enq-read!   cqueue cl-acc acc)
        (areduce acc idx res []
                 (let [v (aget acc idx)]
                   (if (zero? v)
                     res
                     (conj res [idx (+ idx 8) v]))))))))
(set! *unchecked-math* false)

;;avoid creating contexts and programs all over.
(defn test-cached []
  (with-release [dev      (first (devices cuda-platform))
                 ctx      (context [dev])
                 cqueue   (command-queue ctx dev)
                 prog     (build-program! (program-with-source ctx [smt-source]) [dev] nil nil nil)
                 smt      (kernel prog "smt")]
    (c/quick-bench (smt-8-cached times-v  dev ctx cqueue smt))))
