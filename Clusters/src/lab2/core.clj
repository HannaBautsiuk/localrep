(ns lab2.core)

(def Ra 3)
(def Rb (* 1.5 Ra))
(def FourRa (/ 4 (* Ra Ra)))
(def FourRb (/ 4 (* Rb Rb)))

(defn calc-potential [distance]
  (Math/exp (- (* FourRa distance))))

(defn calc-revised-potential [distance]
  (Math/exp (- (* FourRb distance))))

(defn sqr [x]
  (* x x))

(defn getDst[pointWithDst]
  (:dst pointWithDst))

(defn euclidian [c1 c2]
  (->> (map - c1 c2) 
       (map sqr) 
       (reduce +)))

(defn hamming [c1 c2]
  (->> (map not= c1 c2)
       (filter true?)
       (count)))

(defn create-coorninates-from-string [str]
  (->> (clojure.string/split str #",")
       (pmap clojure.string/trim)
       (butlast)
       (pmap read-string)
       (hash-map :crds)
       (vector)))

(defn read-coordinates [fileName]
  (->> (clojure.java.io/reader fileName)
       (line-seq)
       (mapcat create-coorninates-from-string)))


(defn calc-point-potential [point points mthd]
  (->> (pmap #(mthd (:crds point) (:crds %)) points)
       (pmap calc-potential) 
       (reduce + 0)
       (assoc point :dst)))

(defn calc-potentials [points mthd]
  (pmap #(calc-point-potential % points mthd) points))

(defn find-max [points]
  (->> (sort-by getDst points)
       (first)))

(defn revise-potential [point kernel mthd]
  (->> (mthd (:crds point) (:crds kernel))
       (calc-revised-potential)
       (* (:dst kernel))
       (- (:dst point))
       (assoc point :dst)))

(defn revise-point-potentials [points kernel mthd]
  (->> (pmap #(revise-potential % kernel mthd) points)
       (sort-by getDst)
       (reverse)))

(defn calc-min-dist [point points mthd]
  (->> (pmap #(mthd (:crds point) (:crds %)) points)
       (apply min)))

(defn calculate [points mthd]
  (let [initPotentials (->> (calc-potentials points mthd)
                            (sort-by getDst)
                            (reverse))
        initKernel (first initPotentials)]
      (loop [kernels [initKernel] 
             elements (rest initPotentials)]
        (let [revisedPoints (revise-point-potentials elements (first kernels) mthd)
              nextKernel (first revisedPoints)]
            (cond
              (< (:dst nextKernel) (* 0.15 (:dst initKernel))) (->> (sort-by getDst kernels)
                                                                    (reverse))
              (> (:dst nextKernel) (* 0.5 (:dst initKernel))) (recur (cons nextKernel kernels) (rest revisedPoints))
              (>= (+ (/ (calc-min-dist nextKernel kernels mthd) Ra) (/ (:dst nextKernel) (:dst initKernel))) 1) (recur (cons nextKernel kernels) (rest revisedPoints))
              :else (recur kernels (cons (assoc nextKernel :dst 0) (rest revisedPoints))))))))

(defn- getMethod [mthd]
  (cond
    (= mthd "hamming") hamming
    (= mthd "euclidian") euclidian
    :else nil?))

(defn- input-params-incorrect[file-name mthd]
  (or
    (nil? file-name)
    (not (.exists (clojure.java.io/as-file file-name)))
    (nil? (getMethod mthd))))

(defn -main [file-name mthd]
  (if (input-params-incorrect file-name mthd)
    (println "Incorrect start params")
    (calculate (read-coordinates file-name) (getMethod mthd))))
