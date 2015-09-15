(ns examples.knapsack
  (:require [clojure.set :as set]
            [optimize.core :as o]))

(defrecord KnapsackItem [weight value])

(deftype KnapsackProblem [max-weight items]
  o/LocalSearch
  (init
   [this]
   (let [[name {:keys [weight]}] (rand-nth (seq items))]
     {name (int (/ max-weight weight))}))
  (objective
   [this s]
   (reduce (fn [v [name n]]
             (+ v (* n (get-in items [name :value]))))
           0
           s))
  (neighborhood
   [this s]
   ;; add or remove one item from s
   (let [rm (reduce (fn [rm [name n]]
                      (->> (cond-> (update-in s [name] dec)
                             (= 1 n) (dissoc name))
                           (conj rm)))
                    []
                    s)
         add (reduce (fn [add [name _]]
                       (->> #(-> % (or 0) inc)
                            (update-in s [name])
                            (conj add)))
                     []
                     items)
         weight (fn [s]
                  (->> s
                       (map #(* (get-in items [(first %) :weight])
                                (second %)))
                       (apply +)))]
     (->> (concat rm add)
          (filter #(>= max-weight (weight %)))))))

(extend-type KnapsackProblem
  o/TabuSearch
  (legal?
   [this t s]
   (let [t-active (-> (map second t) set)
         features (-> (o/extract-features this s) set)]
     (-> (set/intersection t-active features) empty?)))
  (extract-features
   [this s]
   (keys s)))

(let [kp (->KnapsackProblem 47 {:item1 (->KnapsackItem 2 3)
                                :item2 (->KnapsackItem 4 12)
                                :item3 (->KnapsackItem 3 9)
                                :item4 (->KnapsackItem 5 11)
                                :item5 (->KnapsackItem 6 13)})]
  (doseq [f [o/hill-climbing o/tabu-search]]
    (let [{:keys [s* score*]} (f kp)]
      (prn (format "Best solution found (%s): %s (score: %s)"
                   f (pr-str s*) score*)))))
