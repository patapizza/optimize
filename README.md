optimize
=====

Optimization library for Clojure.

```clojure
(require '[optimize.core :as o])

(defprotocol LocalSearch
  "Local search optimization"
  (init [] "Returns initial solution")
  (objective [s] "Objective function to minimize")
  (neighborhood [s] "Neighborhood function"))

(deftype KnapsackProblem [max-weight items]
  o/LocalSearch
  (init [this]
    (let [[name {:keys [weight]}] (rand-nth items)]
      {name (int (/ max-weight weight))}))
  (objective [this s]
    (reduce (fn [v [name n]]
              (+ v (* n (get-in items [name :value]))))
            0
            s))
  (neighborhood [this s]
    ;; add or remove one item from s 
    (let [rm (reduce (fn [rm [name n]]
                       (cond-> (update-in rm [name] dec)
                         (= 1 n) (dissoc name)))
                     s
                     s)
          add (->> items
                   (map (fn [[name {:keys [weight]}]]
                          [name (inc (get s name 0))]))
                   (into {}))]
      (->> (concat rm add)
           (filter #(>= max-weight (objective this %)))))))

(defrecord KnapsackItem [weight value])

(def kp (KnapsackProblem. 8 {:item1 (->KnapsackItem 4 3)
                             :item2 (->KnapsackItem 2 1)
                             :item3 (->KnapsackItem 3 4)}))

(o/tabu-search kp)
(o/hill-climbing kp)
(o/simulated-annealing kp)
```
