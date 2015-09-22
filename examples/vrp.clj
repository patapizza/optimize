(ns examples.vrp
  (:use [optimize.core])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defrecord Customer [i x y q])

(defrecord VRP [m n max-load vertices c]
  LocalSearch
  (init
   [this]
   (let [[depot & customers] vertices
         mk-route #(->> %
                        (reduce (fn [route customer]
                                  (let [w (->> route (map :q) (apply +))]
                                    (if (>= max-load (+ (:q customer) w))
                                      (cons customer route)
                                      route)))
                                (list depot))
                        (cons depot)
                        (map :i))
         mk-routes (fn [customers routes]
                     (let [route (mk-route customers)
                           customers' (drop (- (count route) 2) customers)
                           routes' (conj routes route)]
                       (if (empty? customers')
                         routes'
                         (recur customers' routes'))))
         routes (mk-routes customers [])]
     (concat routes (repeat (- m (count routes)) [0 0]))))
  (objective
   [this s]
   (let [route-n (map count s)
         route-cost (fn [route]
                      (reduce (fn [cost k]
                                (if (= 0 k)
                                  cost
                                  (+ cost (get-in c [(nth route (dec k))
                                                     (nth route k)]))))
                              0
                              (range (count route))))]
     (->> s (map route-cost) (apply +) -)))
  (neighborhood
   [this s]
   ;; move one customer within a route or to another route
   ;; FIXME: This is prohibitive. Build a smaller, smarter neighborhood.
   (let [route-perm (fn [route]
                      (->> (combo/permutations route)
                           (filter #(and (not= 0 (first %))
                                         (not= 0 (last %))))))]
     (->> (map route-perm s)
          (apply combo/cartesian-product)))))

(defn extract-data
  [line]
  "From data line, parse numbers."
  (-> line string/trim (string/split #"[ \t]+") (->> (map read-string))))

(defn mk-customer
  [line]
  "From data line, creates a Customer instance."
  (let [[i x y _ q & _] (extract-data line)]
    (->Customer i x y q)))

(defn dist
  [[x y] [x' y']]
  "Computes Euclidian distance between two points."
  (let [delta #(Math/pow (- %1 %2) 2)]
    (Math/sqrt (+ (delta x x') (delta y y')))))

(defn mk-vrp
  [f]
  "From resource file, creates a Vehicle Routing Problem instance."
  (let [lines (-> f io/resource slurp (string/split #"\n"))
        [_ m n _] (-> lines first extract-data)
        [_ max-load] (-> lines second extract-data)
        [_ x y & _] (-> lines (nth 3) extract-data)
        depot (->Customer 0 x y 0)
        customers (->> lines (drop 3) (map mk-customer))
        vertices (cons depot customers)
        c (mapv (fn [{:keys [i x y]}]
                 (mapv (fn [{j :i x' :x y' :y}]
                        (if (= i j)
                          0.0
                          (dist [x y] [x' y'])))
                      vertices))
               vertices)]
    (->VRP m n max-load vertices c)))

(let [vrp-instance (mk-vrp "vrp/p01")
      {:keys [s* score*]} (hill-climbing vrp-instance)]
  (prn (format "Best solution found: %s (score: %s)"
               (pr-str s*) score*)))
