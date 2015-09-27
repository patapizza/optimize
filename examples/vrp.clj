(ns examples.vrp
  (:use [optimize.core])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

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
                        (mapv :i))
         mk-routes (fn [customers routes]
                     (let [route (mk-route customers)
                           customers' (filter #(= -1 (.indexOf route (:i %))) customers)
                           routes' (conj routes route)]
                       (if (empty? customers')
                         routes'
                         (recur customers' routes'))))
         routes (mk-routes customers [])]
     (->> (repeat (->> routes count (- m)) [0 0])
          (concat routes)
          vec)))
  (objective
   [this s]
   (let [route-cost (fn [route]
                      (reduce (fn [cost k]
                                (if (= 0 k)
                                  cost
                                  (+ cost (get-in c [(nth route (dec k))
                                                     (nth route k)]))))
                              0
                              (-> route count range)))]
     (->> s (map route-cost) (apply +) -)))
  (neighborhood
   [this s]
   ;; swap two customers within a route or between routes
   (let [n (count s)
         stops #(-> % count (- 2))
         rand-int+ #(-> % rand-int inc)
         in-swap (fn [route]
                   (let [n (stops route)
                         [i j] (repeatedly 2 #(rand-int+ n))]
                     (assoc route i (nth route j) j (nth route i))))
         in-swaps (reduce (fn [routes i]
                            (->> (in-swap (nth s i))
                                 (assoc s i)
                                 (conj routes)))
                          []
                          (range n))
         out-swap (fn [x y]
                    (let [[i j] (map #(-> % stops rand-int+) [x y])]
                      [(assoc x i (nth y j)) (assoc y j (nth x i))]))
         out-swaps (reduce (fn [routes i]
                             (reduce (fn [routes' j]
                                       (->> s
                                            (keep-indexed #(when (and (not= i %1) (not= j %1)) %2))
                                            (concat (out-swap (nth s i) (nth s j)))
                                            vec
                                            (conj routes')))
                                     routes
                                     (range (inc i) n)))
                           []
                           (range n))
         valid? (fn [route]
                  (->> (map #(-> vertices (nth %) :q) route)
                       (apply +)
                       (>= max-load)))]
     (->> (filter #(every? valid? %) out-swaps)
          (concat in-swaps)
          vec))))

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

(let [vrp-instance (mk-vrp "vrp/p01")]
  (doseq [f [hill-climbing simulated-annealing]]
    (let [{:keys [s* score*]} (f vrp-instance)]
      (prn (format "Best solution found (%s): %s (score: %s)"
                   f (pr-str s*) (- score*))))))
