(ns optimize.core)

(defprotocol LocalSearch
  "Local search optimization"
  (init [pb] "Returns the initial solution")
  (objective [pb s] "Objective function to maximize")
  (neighborhood [pb s] "Neighborhood function"))

(def hill-climbing-opts {:max-iterations 1000})

(defn hill-climbing
  "Hill climbing method"
  ([pb] (hill-climbing pb {}))
  ([pb opts]
   {:pre [(satisfies? LocalSearch pb)]}
   (let [{:keys [max-iterations]} (merge hill-climbing-opts opts)]
     (reduce (fn [s* i]
               (let [s (->> (neighborhood pb s*)
                            (sort-by (comp - (partial objective pb)))
                            first)]
                 (if (>= (objective pb s) (objective pb s*))
                   s
                   s*)))
             (init pb)
             (range max-iterations)))))
