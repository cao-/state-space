(ns state-space.traversal-features-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [state-space.core :as ss]))

(defn- visit-snapshots [events]
  (mapv (fn [{:keys [phase visit]}] (assoc visit :phase phase))
        (filter #(= :visit (:event %)) events)))

(defn- event-phases [events]
  (mapv (fn [{:keys [phase visit]}] [(:state visit) phase])
        (filter #(= :visit (:event %)) events)))

(deftest event-output-uses-consistent-envelopes
  (let [graph {:root [:leaf] :leaf []}
        search-events (for [algorithm [ss/dfs ss/simple-paths ss/bfs
                                       ss/postorder ss/best-first]]
                        (vec (algorithm graph [:root] {:emit :events})))
        progress-events (vec (ss/propagate graph {:root 1}
                                           {:bottom 0 :join + :emit :events}))
        assert-envelope
        (fn [event]
          (is (map? event))
          (case (:event event)
            :candidate
            (is (every? #(contains? event %)
                        [:origin :visit :disposition]))

            :visit
            (is (every? #(contains? event %) [:phase :visit]))

            :progress
            (is (contains? event :record))

            (is false (str "Unexpected event type: " (:event event)))))]
    (doseq [event (concat (mapcat identity search-events) progress-events)]
      (assert-envelope event))
    (is (every? #(= #{:candidate :visit} (set (map :event %)))
                search-events))
    (is (every? #(= :progress (:event %)) progress-events))))

(deftest frontier-events-distinguish-entry-expansion-and-dfs-return
  (let [graph {:a [:b :c] :b [:d]}
        phases (fn [algorithm]
                 (event-phases
                       (algorithm graph [:a] {:emit :events})))]
    (is (= [[:a :enter] [:b :enter] [:c :enter] [:a :expand]
            [:d :enter] [:b :expand] [:c :expand] [:d :expand]]
           (phases ss/bfs)))
    (is (= [[:a :enter] [:a :expand] [:b :enter] [:b :expand]
            [:c :enter] [:c :expand] [:d :enter] [:d :expand]]
           (phases ss/best-first)))
    (is (= [[:a :enter] [:b :enter] [:d :enter] [:d :exit]
            [:b :exit] [:c :enter] [:c :exit] [:a :exit]]
           (phases ss/dfs)))))

(deftest frontier-events-preserve-visits-and-phase-free-callbacks
  (doseq [algorithm [ss/bfs ss/best-first]]
    (let [graph {:root [nil false :shared] nil [:root :shared]
                 false [:shared] :shared []}
          calls (atom [])
          record (fn [kind visit]
                   (is (not (contains? visit :phase)))
                   (swap! calls conj kind))
          options {:emit :events
                   :project (fn [visit] (record :project visit) (:state visit))
                   :expand? (fn [visit] (record :expand visit) true)
                   :visit-key (fn [visit] (record :key visit) (:state visit))
                   :extensions
                   [ss/path-extension
                    {:start (fn [visit] (record :start visit) {})
                     :next (fn [parent _ child]
                             (record :parent parent)
                             (record :child child) {})}]}
          events (algorithm graph [:root :root] options)]
      (is (empty? @calls))
      (let [all (vec events)
            entries (filter #(= :enter (:phase %)) (visit-snapshots all))
            expanded (filter #(= :expand (:phase %)) (visit-snapshots all))
            default-visits (algorithm graph [:root :root]
                                      {:extensions [ss/path-extension]})]
        (is (= (vec default-visits) (mapv #(dissoc % :phase) entries)))
        (is (= (set (map #(dissoc % :phase) entries))
               (set (map #(dissoc % :phase) expanded))))
        (is (= 8 (count (filter #(= :candidate (:event %)) all))))
        (is (= {:start 2 :child 6 :parent 6 :project 4 :expand 4}
               (dissoc (frequencies @calls) :key)))
        (let [previous @calls]
          (is (= all (vec events)))
          (is (= previous @calls)))))))

(deftest bfs-events-without-deduplication-and-context-sensitive-identity
  (doseq [options [{:visit-key nil}
                   {:visit-key (juxt :state :depth)
                    :extensions [ss/depth-extension]}]]
    (let [graph {:root [:shared :a] :a [:shared]}
          events (ss/bfs graph [:root] (assoc options :emit :events))]
      (is (= (vec (ss/bfs graph [:root] options))
             (mapv #(dissoc % :phase)
                   (filter #(= :enter (:phase %)) (visit-snapshots events)))))
      (is (= 2 (count (filter #(and (= :shared (:state %))
                                   (= :expand (:phase %)))
                              (visit-snapshots events))))))))

(deftest bfs-candidate-events-precede-their-dequeue-outcome
  (let [events (ss/bfs {:root [:left :right]
                        :left [:goal]
                        :right [:goal]
                        :goal []}
                       [:root]
                       {:emit :events})
        summary (mapv (fn [event]
                        (if (= :candidate (:event event))
                          [:candidate (get-in event [:visit :state])
                           (:disposition event)]
                          [:visit (get-in event [:visit :state])
                           (:phase event) (:reason event)]))
                      events)]
    (is (= [[:candidate :root :scheduled]
            [:visit :root :enter nil]
            [:candidate :left :scheduled]
            [:visit :left :enter nil]
            [:candidate :right :scheduled]
            [:visit :right :enter nil]
            [:visit :root :expand nil]
            [:candidate :goal :scheduled]
            [:visit :goal :enter nil]
            [:visit :left :expand nil]
            [:candidate :goal :scheduled]
            [:visit :goal :skip :already-visited]
            [:visit :right :expand nil]
            [:visit :goal :expand nil]]
           summary))))

(deftest frontier-event-entries-do-not-expand-the-selected-visit
  (doseq [algorithm [ss/bfs ss/best-first]]
    (let [expanded (atom [])
          decoded (atom [])
          events (algorithm
                  (fn [state] (swap! expanded conj state)
                    (when (= state :root) [:child :later]))
                  [:root]
                  {:emit :events
                   :decode-edge (fn [state] (swap! decoded conj state) {:state state})})]
      (is (= :candidate (:event (first events))))
      (is (empty? @expanded))
      (is (empty? @decoded))
      (if (= algorithm ss/bfs)
        (do
          (is (= [:child :enter]
                 ((juxt :state :phase) (nth (visit-snapshots (take 4 events)) 1))))
          (is (= [:child] @decoded))
          (is (= [:later :scheduled]
                 ((juxt (comp :state :visit) :disposition) (nth events 4))))
          (is (= [:root :expand]
                 ((juxt :state :phase) (nth (visit-snapshots (take 7 events)) 3)))))
        (do
          (is (= [:root :enter]
                 ((juxt :state :phase) (first (visit-snapshots (take 2 events))))))
          (is (empty? @decoded))
          (is (= [:child :scheduled]
                 ((juxt (comp :state :visit) :disposition) (nth events 2))))
          (is (= [:child] @decoded))
          (is (= :candidate (:event (nth events 3))))
          (is (= [:child :later] @decoded))))
      (is (= [:root] @expanded)))))

(deftest bfs-events-preserve-infinite-starts-and-successor-streams
  (testing "an infinite start chunk delays every expansion"
    (let [expanded (atom [])
          initialized (atom [])
          events (ss/bfs
                  (fn [state] (swap! expanded conj state) []) (range)
                  {:emit :events
                   :extensions [{:start (fn [{:keys [state]}]
                                          (swap! initialized conj state) {})}]})]
      (is (= (mapv #(vector % :enter) (range 10))
             (event-phases (take 20 events))))
      (is (empty? @expanded))
      (is (= (vec (range 10)) @initialized))))
  (testing "an infinite successor chunk produces entries, not a false completion"
    (let [decoded (atom [])
          expanded (atom [])
          events (ss/bfs
                  (fn [state] (swap! expanded conj state)
                    (when (= state :root) (range)))
                  [:root]
                  {:emit :events
                   :decode-edge (fn [state]
                                  (swap! decoded conj state) {:state state})})]
      (is (= (into [[:root :enter]] (map #(vector % :enter) (range 9)))
             (event-phases (take 20 events))))
      (is (= [:root] @expanded))
      (is (= (vec (range 9)) @decoded)))))

(deftest best-first-events-report-only-selected-candidates
  (let [graph {:root [[:target 10] [:via 1] [:other 1]]
               :via [[:target 2]]}
        events (ss/best-first graph [:root]
                              {:emit :events
                               :extensions [ss/path-extension]
                               :decode-edge (fn [[state weight]]
                                              {:state state :weight weight})})]
    (is (= [[:root :enter 0] [:root :expand 0]
            [:via :enter 1] [:via :expand 1]
            [:other :enter 1] [:other :expand 1]
            [:target :enter 3] [:target :expand 3]]
           (mapv (juxt :state :phase :distance) (visit-snapshots events))))
    (is (= [:root :via :target] (:path (last (visit-snapshots events))))))
  (testing "custom priorities can decrease; expansion does not mean globally sorted"
    (let [options {:priority :state :emit :events}
          events (ss/best-first {5 [1]} [5] options)]
      (is (= [[5 :enter] [5 :expand] [1 :enter] [1 :expand]]
             (event-phases events))))))

(deftest frontier-events-handle-pruning-empty-input-and-expansion-errors
  (doseq [algorithm [ss/bfs ss/best-first]]
    (is (empty? (algorithm {} [] {:emit :events})))
    (let [events (algorithm
                  (fn [_] (throw (Exception. "Pruned space must not be called")))
                  [:root] {:emit :events :expand? (constantly false)})]
      (is (= [[:root :enter] [:root :expand]]
             (event-phases events))))
    (let [error (ex-info "Expansion failed" {})
          events (algorithm (fn [_] (throw error)) [:root] {:emit :events})]
      (is (= :candidate (:event (first events))))
      (is (identical? error (try (nth events 2)
                                (catch clojure.lang.ExceptionInfo actual actual)))))))

(deftest frontier-events-are-stack-safe-on-deep-chains
  (doseq [algorithm [ss/bfs ss/best-first]]
    (let [events (algorithm (fn [n] (when (< n 20000) [(inc n)])) [0]
                            {:emit :events})]
      (is (= 60003 (count events)))
      (is (= [20000 :expand]
             ((juxt :state :phase) (last (visit-snapshots events))))))))

(deftest frontier-events-preserve-order-on-every-three-state-graph
  (doseq [mask (range 512)
          algorithm [ss/bfs ss/best-first]]
    (let [graph (into {} (for [from (range 3)]
                           [from (vec (for [to (range 3)
                                            :when (bit-test mask (+ (* 3 from) to))]
                                        to))]))
          options {:extensions [ss/path-extension]}
          events (vec (algorithm graph [0 1 2 0] (assoc options :emit :events)))
          entries (filter #(= :enter (:phase %)) (visit-snapshots events))
          expansions (filter #(= :expand (:phase %)) (visit-snapshots events))]
      (is (= (vec (algorithm graph [0 1 2 0] options))
             (mapv #(dissoc % :phase) entries)))
      (is (= (mapv :state entries) (mapv :state expansions)))
      (is (= (+ (count (filter #(= :visit (:event %)) events))
                (count (filter #(= :candidate (:event %)) events)))
             (count events))))))

(deftest best-first-event-callbacks-and-fifo-ties-remain-unchanged
  (let [calls (atom [])
        events (ss/best-first
                {} [:b :a :b :c]
                {:emit :events
                 :priority (fn [visit]
                             (is (not (contains? visit :phase)))
                             (swap! calls conj (:state visit)) 0)})]
    (is (empty? @calls))
    (is (= [[:b :enter] [:b :expand] [:a :enter] [:a :expand]
            [:c :enter] [:c :expand]]
           (event-phases events)))
    (is (= [:b :a :b :c] @calls))))

(deftest candidate-events-report-start-edge-and-rejection-decisions
  (let [dfs-events (vec (ss/dfs {:root [:leaf :leaf]} [:root :root]
                                {:emit :events}))
        bfs-events (vec (ss/bfs {:root [:leaf :leaf]} [:root]
                                {:emit :events}))
        path-events (vec (ss/simple-paths {:root [:root]} [:root]
                                          {:emit :events}))
        discarded (filter #(= :discarded (:disposition %))
                          (filter #(= :candidate (:event %)) dfs-events))
        skipped (filter #(and (= :visit (:event %)) (= :skip (:phase %)))
                        bfs-events)]
    (is (= [:already-visited :already-visited] (mapv :reason discarded)))
    (is (= [[:leaf :already-visited]]
           (mapv (fn [event]
                   [(get-in event [:visit :state]) (:reason event)])
                 skipped)))
    (is (= :on-current-path
           (:reason (last (filter #(= :candidate (:event %)) path-events)))))))

(deftest best-first-candidate-events-describe-pending-improvements-and-settled-keys
  (let [events (vec
                (ss/best-first
                 {:root [[:target 10] [:via 1] [:other 1]]
                  :via [[:target 2] [:root 1]]}
                 [:root]
                 {:emit :events
                  :extensions [ss/path-extension]
                  :decode-edge (fn [[state weight]]
                                 {:state state :weight weight})}))
        candidates (filter #(= :candidate (:event %)) events)
        target-events (filter #(= :target (get-in % [:visit :state])) candidates)]
    (is (= [[:scheduled :inserted] [:scheduled :improved-pending]]
           (mapv (juxt :disposition :effect) target-events)))
    (is (= :already-settled
           (:reason (last (filter #(= :root (get-in % [:visit :state]))
                                  candidates)))))
    (is (= [:root :via :target]
           (:path (:visit (first (filter #(and (= :visit (:event %))
                                                (= :enter (:phase %))
                                                (= :target (get-in % [:visit :state])))
                                         events))))))))

(deftest propagation-events-wrap-each-progress-record
  (let [space {:root [[:leaf #{:new}]]}
        initial {:root #{:old}}
        options {:bottom #{} :join set/union}
        progress (vec (ss/propagate space initial options))
        events (vec (ss/propagate space initial (assoc options :emit :events)))]
    (is (= progress (mapv :record events)))
    (is (every? #(= :progress (:event %)) events))))

(deftest postorder-event-output-matches-the-shared-dfs-trace
  (let [graph {:root [:left :right] :left [:shared] :right [:shared]}
        starts [:root :root]
        dfs-events (vec (ss/dfs graph starts {:emit :events}))
        postorder-events (vec (ss/postorder graph starts {:emit :events}))]
    (is (= dfs-events postorder-events))
    (is (= (vec (ss/postorder graph starts))
           (mapv #(dissoc % :phase)
                 (filter #(= :exit (:phase %)) (visit-snapshots postorder-events)))))))

(deftest fold-descent-sees-updated-siblings-before-identity-and-cached-lookups
  (let [calls (atom [])
        options
        {:extensions
         [{:start (fn [_] {:budget 10})
           :next (fn [parent transition _]
                   (swap! calls conj [:next (:budget parent) (:state transition)])
                   {:budget -1})}]
         :visit-key (fn [{:keys [state budget]}]
                      (swap! calls conj [:key state budget])
                      [state budget])
         :fold
         {:init (fn [{:keys [budget]}] budget)
          :descend (fn [budget transition child]
                     (swap! calls conj [:descend budget (:state transition) (:budget child)])
                     {:budget budget})
          :step (fn [budget _] (dec budget))}}
        visits (ss/postorder {:root [:a :b :a :a]} [:root] options)]
    (is (empty? @calls))
    (is (= [[:a 10 10] [:b 9 9] [:a 8 8] [:a 7 7] [:root 10 6]]
           (mapv (juxt :state :budget :value) visits)))
    (is (= [[:descend 10 :a -1] [:descend 9 :b -1]
            [:descend 8 :a -1] [:descend 7 :a -1]]
           (filterv #(= :descend (first %)) @calls))))
  (testing "descent runs on cached edges, without reinitializing the shared child"
    (let [descended (atom [])
          initialized (atom [])
          visits (ss/postorder
                  {:root [:child :child]} [:root]
                  {:fold {:init (fn [{:keys [state]}]
                                  (swap! initialized conj state) 0)
                          :descend (fn [n _ child]
                                     (swap! descended conj [n (:state child)]) {})
                          :step (fn [n _] (inc n))}})]
      (is (= [0 2] (mapv :value visits)))
      (is (= [[0 :child] [1 :child]] @descended))
      (is (= [:root :child] @initialized)))))

(deftest fold-descent-is-lazy-and-suppressed-after-cutoff
  (let [calls (atom [])
        events (ss/dfs
                {:root [:a :unused]} [:root]
                {:emit :events
                 :fold {:init (constantly 0)
                        :descend (fn [n transition _]
                                   (swap! calls conj [:descend n (:state transition)]) {})
                        :step (fn [_ _] (reduced 1))}})]
    (is (= :candidate (:event (first events))))
    (is (empty? @calls))
    (is (= :candidate (:event (nth events 2))))
    (is (= [[:descend 0 :a]] @calls))
    (is (= [:root :a :a :root]
           (mapv :state (visit-snapshots events))))
    (is (= [[:descend 0 :a]] @calls)))
  (testing "reduced initialization skips descent entirely"
    (is (= [{:state :root :value false}]
           (vec (ss/postorder {:root [:unused]} [:root]
                              {:fold {:init (constantly (reduced false))
                                      :step (fn [_ _] (throw (Exception. "Unexpected step")))
                                      :descend (fn [_ _ _]
                                                 (throw (Exception. "Unexpected descent")))}}))))))

(deftest fold-descent-can-affect-expansion-and-propagates-errors
  (let [visits (ss/postorder
                (fn [{:keys [state remaining]}]
                  (when (pos? remaining) [(inc state)]))
                [0]
                {:project identity
                 :extensions [{:start (fn [_] {:remaining 3})}]
                 :visit-key (juxt :state :remaining)
                 :fold {:init :remaining
                        :descend (fn [n _ _] {:remaining (dec n)})
                        :step (fn [n _] n)}})]
    (is (= [[3 0] [2 1] [1 2] [0 3]]
           (mapv (juxt :state :value) visits))))
  (let [error (ex-info "Descent failed" {})
        events (ss/dfs {:root [:child]} [:root]
                       {:emit :events
                        :fold {:init (constantly 0) :step (fn [n _] n)
                               :descend (fn [_ _ _] (throw error))}})]
    (is (= :candidate (:event (first events))))
    (is (identical? error
                    (try (nth events 2)
                         (catch clojure.lang.ExceptionInfo actual actual))))))

(defn- alpha-beta-fold [scores]
  {:init (fn [{:keys [state depth alpha beta]}]
           (if (contains? scores state)
             (reduced {:score (scores state)})
             {:max? (even? depth) :alpha alpha :beta beta
              :score (if (even? depth) ##-Inf ##Inf)}))
   :descend (fn [window _ _] (select-keys window [:alpha :beta]))
   :step (fn [{:keys [max?] :as window} {:keys [value]}]
           (let [score ((if max? max min) (:score window) value)
                 window (-> window
                            (assoc :score score)
                            (update (if max? :alpha :beta) (if max? max min) score))]
             (if (>= (:alpha window) (:beta window)) (reduced window) window)))
   :finish :score})

(defn- alpha-beta-options [scores alpha beta]
  {:extensions [ss/depth-extension
                {:start (fn [_] {:alpha alpha :beta beta})}]
   :visit-key (juxt :state :depth :alpha :beta)
   :fold (alpha-beta-fold scores)})

(defn- reference-alpha-beta [graph scores state depth alpha beta]
  (if (contains? scores state)
    (scores state)
    (loop [children (seq (graph state))
           score (if (even? depth) ##-Inf ##Inf)
           alpha alpha, beta beta]
      (if (or (nil? children) (>= alpha beta))
        score
        (let [value (reference-alpha-beta graph scores (first children)
                                          (inc depth) alpha beta)
              score ((if (even? depth) max min) score value)]
          (recur (next children) score
                 (if (even? depth) (max alpha score) alpha)
                 (if (even? depth) beta (min beta score))))))))

(deftest alpha-beta-skips-losing-siblings-and-returns-the-minimax-value
  (let [graph {:root [:a :b :c]
               :a [:a1 :a2 :a3] :b [:b1 :b2 :b3] :c [:c1 :c2 :c3]}
        scores {:a1 3 :a2 12 :a3 8 :b1 2 :b2 4 :b3 6 :c1 14 :c2 5 :c3 2}
        decoded (atom [])
        visits (ss/postorder graph [:root]
                             (assoc (alpha-beta-options scores ##-Inf ##Inf)
                                    :decode-edge (fn [state]
                                                   (swap! decoded conj state)
                                                   {:state state})))
        all (ss/dfs graph [:root])]
    (is (= 3 (:value (last visits))))
    (is (= 11 (count visits)))
    (is (= 13 (count all)))
    (is (= [:a :a1 :a2 :a3 :b :b1 :c :c1 :c2 :c3] @decoded))
    (is (= 7 (count (filter #(contains? scores (:state %)) visits))))
    (is (= 3 (:alpha (first (filter #(= :b (:state %)) visits)))))))

(deftest alpha-beta-agrees-with-reference-on-generated-dags-and-search-windows
  (let [random (java.util.Random. 7221)]
    (dotimes [case-number 150]
      (let [graph (into {} (for [from (range 11)]
                             [from (vec (filter (fn [_] (< (.nextDouble random) 0.4))
                                                (range (inc from) 12)))]))
            scores (into {} (for [state (range 12) :when (empty? (graph state))]
                              [state (- (.nextInt random 21) 10)]))
            exact (reference-alpha-beta graph scores 0 0 ##-Inf ##Inf)]
        (doseq [[alpha beta] [[##-Inf ##Inf] [-3 3] [0 1] [-10 -5] [5 10]]]
          (let [options (alpha-beta-options scores alpha beta)
                result (:value (last (ss/postorder graph [0] options)))
                events (ss/dfs graph [0] (assoc options :emit :events))]
            (is (= (reference-alpha-beta graph scores 0 0 alpha beta) result)
                (str "DAG " case-number ", window " [alpha beta]))
            (is (= result (:value (last (visit-snapshots events)))))
            (is (cond (<= exact alpha) (<= result alpha)
                      (>= exact beta) (>= result beta)
                      :else (= exact result)))))))))

(deftest alpha-beta-distinguishes-cached-bounds-under-different-windows
  (let [graph {:root [:a :b] :a [:limit :shared] :b [:shared]
               :shared [:low :high]}
        scores {:limit 5 :low 6 :high 9}
        visits (ss/postorder graph [:root]
                             (alpha-beta-options scores ##-Inf ##Inf))
        shared (filter #(= :shared (:state %)) visits)]
    (is (= 9 (:value (last visits))))
    (is (= [[##-Inf 5 6] [5 ##Inf 9]]
           (mapv (juxt :alpha :beta :value) shared)))
    (testing "a state-only cache would incorrectly reuse the lower bound as exact"
      (is (= 6 (:value
                (last (ss/postorder graph [:root]
                                     (assoc (alpha-beta-options scores ##-Inf ##Inf)
                                            :visit-key :state)))))))))

(deftest fold-descent-remains-stack-safe
  (let [visits (ss/postorder
                (fn [n] (when (< n 20000) [(inc n)])) [0]
                {:extensions [{:start (fn [_] {:n 0})}]
                 :fold {:init :n
                        :descend (fn [n _ _] {:n (inc n)})
                        :step (fn [_ child] (:value child))}})]
    (is (= 20001 (count visits)))
    (is (= 20000 (:value (last visits))))))
