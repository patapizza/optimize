(ns optimize.core)

(def default-opts {:max-iterations 1000})

;; -----------------------------------------------------------------------------
;; Protocols

(defprotocol LocalSearch
  "Local search optimization"
  (init [pb] "Returns the initial solution")
  (objective [pb s] "Objective function to maximize")
  (neighborhood [pb s] "Neighborhood function"))

(defprotocol TabuSearch
  "Tabu search optimization"
  (legal? [pb t s] "Restricts the neighborhood by forbidding s tabu-active.")
  (extract-features [pb s] "Determines the tabu-active elements."))

;; -----------------------------------------------------------------------------
;; LS functions

(defn select-best-n
  "Picks randomly a neighbor among the best n."
  ([candidates objective-fn] (select-best-n candidates objective-fn 1))
  ([candidates objective-fn n]
   (->> candidates
        (sort-by (comp - objective-fn))
        (take n)
        rand-nth)))

(defn select-first-improvement
  "Picks the first neighbor improving the current solution.
   If none, same as select-best-n, with n=1."
  [candidates objective-fn score*]
  (-> (some #(when (> (objective-fn %) score*) %) candidates)
      (or (select-best-n candidates objective-fn 1))))

;; -----------------------------------------------------------------------------
;; Hill climbing

(def hill-climbing-opts (merge default-opts {:steepest-ascent? true}))

(defn hill-climbing
  "Hill climbing method"
  ([pb] (hill-climbing pb {}))
  ([pb opts]
   {:pre [(satisfies? LocalSearch pb)]}
   (let [{:keys [max-iterations steepest-ascent?]}
         (merge hill-climbing-opts opts)
         s0 (init pb)]
     (reduce (fn [{:keys [s* score*] :as best} _]
               (let [objective-fn (partial objective pb)
                     select-fn (if steepest-ascent?
                                 #(select-best-n % objective-fn 1)
                                 #(select-first-improvement % objective-fn score*))
                     s' (select-fn (neighborhood pb s*))
                     score' (objective pb s')]
                 (if (<= score* score')
                   {:s* s' :score* score'}
                   best)))
             {:s* s0 :score* (objective pb s0)}
             (range max-iterations)))))

;; -----------------------------------------------------------------------------
;; Tabu search

(def tabu-search-opts (merge default-opts {:tenure 1}))

(defn expire-features
  "Expires the elements that are no longer tabu-active."
  [t tenure step]
  (drop-while #(<= tenure (- step (first %))) t))

(defn legal-candidates
  "Returns a non-empty list of legal candidates, with resulting tabu list."
  [candidates legal-fn tenure step t]
  (let [legal (filter (partial legal-fn t) candidates)]
    (if (empty? legal)
      (->> (expire-features t tenure step)
           (recur candidates legal-fn tenure step))
      [legal t])))

(defn make-tabu
  "Appends features to the tabu list."
  [features t step]
  (let [active (map second t)]
    (reduce (fn [t feature]
              (if (some #{feature} active)
                t
                (conj t [step feature])))
            (vec t)
            features)))

(defn tabu-search
  "Tabu search"
  ([pb] (tabu-search pb {}))
  ([pb opts]
   {:pre [(and (satisfies? LocalSearch pb) (satisfies? TabuSearch pb))]}
   (let [{:keys [max-iterations tenure]} (merge tabu-search-opts opts)
         s0 (init pb)]
     (reduce (fn [{:keys [s* score* t] :as best} i]
               (let [[candidates t] (-> (neighborhood pb s*)
                                        (legal-candidates (partial legal? pb) tenure i t))
                     s' (select-first-improvement candidates (partial objective pb) score*)
                     score' (objective pb s')
                     t' (expire-features t tenure i)]
                 (if (<= score* score')
                   {:s* s'
                    :score* score'
                    :t (-> (extract-features pb s') (make-tabu t' i))}
                   (assoc best :t (vec t')))))
             {:s* s0 :score* (objective pb s0) :t []}
             (range max-iterations)))))

;; -----------------------------------------------------------------------------
;; Simulated annealing

(def simulated-annealing-opts default-opts)

(defn simulated-annealing
  "Simulated annealing"
  ([pb] (simulated-annealing pb {}))
  ([pb opts]
   {:pre [(satisfies? LocalSearch pb)]}
   (let [{:keys [max-iterations]} (merge simulated-annealing-opts opts)
         s0 (init pb)]
     (reduce (fn [{:keys [s* score*] :as best} i]
               (let [t (- max-iterations i)
                     s' (-> (neighborhood pb s*) rand-nth)
                     score' (objective pb s')
                     p #(if (< %2 %1) 1 (-> %2 (- %1) - (/ %3) Math/exp))]
                 (if (>= (p score' score* t) (rand))
                   {:s* s' :score* score'}
                   best)))
             {:s* s0 :score* (objective pb s0)}
             (range max-iterations)))))
