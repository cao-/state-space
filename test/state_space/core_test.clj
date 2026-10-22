(ns state-space.core-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [state-space.core :as search]))

(defn- visit-snapshots [events]
  (mapv (fn [{:keys [phase visit]}] (assoc visit :phase phase))
        (filter #(= :visit (:event %)) events)))

(defn- event-phases [events]
  (mapv (fn [{:keys [phase visit]}] [(:state visit) phase])
        (filter #(= :visit (:event %)) events)))

(deftest dfs-lifecycle-emits-nested-entry-and-exit-snapshots
  (let [graph {:a [:b :c] :b [:d]}
        options {:extensions [search/path-extension search/depth-extension]}
        events (vec (search/dfs graph [:a :other] (assoc options :emit :events)))
        visits-at (fn [phase]
                    (mapv #(dissoc % :phase)
                          (filter #(= phase (:phase %)) (visit-snapshots events))))]
    (is (= [[:a :enter] [:b :enter] [:d :enter] [:d :exit]
            [:b :exit] [:c :enter] [:c :exit] [:a :exit]
            [:other :enter] [:other :exit]]
           (event-phases events)))
    (is (= (vec (search/dfs graph [:a :other] options)) (visits-at :enter)))
    (is (= (vec (search/postorder graph [:a :other] options)) (visits-at :exit)))
    (is (= (visits-at :enter)
           (vec (search/dfs graph [:a :other] (assoc options :emit :enter))))))
  (is (empty? (search/dfs {} [] {:emit :events}))))

(deftest dfs-lifecycle-preserves-identity-sharing-and-cycles
  (let [graph {:a [:b :c] :b [:a :d] :c [:d] :d [:d]}
        events (vec (search/dfs graph [:a :a :d] {:emit :events}))]
    (is (= [[:a :enter] [:b :enter] [:d :enter] [:d :exit]
            [:b :exit] [:c :enter] [:c :exit] [:a :exit]]
           (event-phases events))))
  (testing "nil and false are ordinary visit keys"
    (is (= [[nil :enter] [false :enter] [false :exit] [nil :exit]]
           (event-phases
            (search/dfs {nil [false] false [nil]}
                        [nil false] {:emit :events})))))
  (testing "context-sensitive keys distinguish separate computations"
    (let [events (search/dfs {:a [:d :b] :b [:d]} [:a]
                            {:emit :events
                             :extensions [search/depth-extension]
                             :visit-key (juxt :state :depth)})]
      (is (= [[:d 1] [:d 2] [:b 1] [:a 0]]
             (mapv (juxt :state :depth)
                   (filter #(= :exit (:phase %)) (visit-snapshots events))))))))

(deftest dfs-lifecycle-exits-pruned-visits-without-generating-successors
  (let [expanded (atom [])
        events (search/dfs
                (fn [state]
                  (swap! expanded conj state)
                  (case state :root [:pruned :leaf] :pruned [:hidden] []))
                [:root]
                {:emit :events :expand? #(not= :pruned (:state %))})]
    (is (= [[:root :enter] [:pruned :enter] [:pruned :exit]
            [:leaf :enter] [:leaf :exit] [:root :exit]]
           (event-phases events)))
    (is (= [:root :leaf] @expanded))))

(deftest dfs-lifecycle-metadata-does-not-leak-into-route-callbacks
  (let [calls (atom [])
        record (fn [kind visit]
                 (swap! calls conj [kind visit])
                 (is (not (contains? visit :phase))))
        events (search/dfs
                (fn [visit]
                  (record :space visit)
                  (when (= :root (:state visit)) [:leaf :leaf]))
                [:root :root]
                {:emit :events
                 :project (fn [visit] (record :project visit) visit)
                 :visit-key (fn [visit] (record :key visit) (:state visit))
                 :expand? (fn [visit] (record :expand visit) true)
                 :extensions
                 [search/path-extension
                  {:start (fn [visit] (record :start visit) {})
                   :next (fn [parent _transition visit]
                           (record :parent parent)
                           (record :next visit)
                           {})}]})]
    (is (empty? @calls))
    (is (= 8 (count events)))
    (is (= {:start 2 :next 2 :parent 2 :key 4
            :space 2 :project 2 :expand 2}
           (frequencies (map first @calls))))
    (is (= [{:state :root :path [:root]}
            {:state :leaf :path [:root :leaf]}]
           (mapv #(dissoc % :phase)
                 (filter #(= :exit (:phase %))
                         (reverse (visit-snapshots events))))))))

(deftest dfs-lifecycle-defers-expansion-and-next-sibling-until-requested
  (let [calls (atom [])
        events (search/dfs
                (fn [state]
                  (swap! calls conj [:space state])
                  (when (= :root state) [:leaf :later]))
                [:root]
                {:emit :events
                 :decode-edge (fn [state]
                                (swap! calls conj [:decode state])
                                {:state state})})]
    (is (empty? @calls))
    (is (= {:event :candidate :origin :start :visit {:state :root}
            :disposition :scheduled}
           (first events)))
    (is (empty? @calls))
    (is (= {:event :visit :phase :enter :visit {:state :root}}
           (second events)))
    (is (= {:event :candidate :origin :edge
            :from {:state :root} :transition {:state :leaf}
            :visit {:state :leaf} :disposition :scheduled}
           (nth events 2)))
    (is (= [[:space :root] [:decode :leaf]] @calls))
    (is (= {:event :visit :phase :enter :visit {:state :leaf}}
           (nth events 3)))
    (is (= [[:space :root] [:decode :leaf]] @calls))
    (is (= {:event :visit :phase :exit :visit {:state :leaf}}
           (nth events 4)))
    (is (= [[:space :root] [:decode :leaf] [:space :leaf]] @calls))
    (is (= {:event :candidate :origin :edge
            :from {:state :root} :transition {:state :later}
            :visit {:state :later} :disposition :scheduled}
           (nth events 5)))
    (is (= {:event :visit :phase :enter :visit {:state :later}}
           (nth events 6)))
    (is (= [[:space :root] [:decode :leaf] [:space :leaf] [:decode :later]]
           @calls))))

(deftest dfs-lifecycle-supports-infinite-starts-successors-and-descent
  (testing "infinite starts are initialized only when their entry is requested"
    (let [initialized (atom [])
          events (search/dfs {} (range)
                             {:emit :events
                              :extensions
                              [{:start (fn [{:keys [state]}]
                                         (swap! initialized conj state) {})}]})]
      (is (= [[0 :enter] [0 :exit] [1 :enter] [1 :exit] [2 :enter]]
             (event-phases (take 8 events))))
      (is (= [0 1 2] @initialized))))
  (testing "an infinite sequence of leaves produces exits incrementally"
    (let [decoded (atom [])
          events (search/dfs
                  (fn [state] (when (= :root state) (range))) [:root]
                  {:emit :events
                   :decode-edge (fn [state]
                                  (swap! decoded conj state)
                                  {:state state})})]
      (is (= [[:root :enter] [0 :enter] [0 :exit] [1 :enter] [1 :exit]]
             (event-phases (take 8 events))))
      (is (= [0 1] @decoded))))
  (testing "an infinite branch can produce only entries"
    (is (= (mapv #(hash-map :state % :phase :enter) (range 100))
           (filterv #(= :enter (:phase %))
                    (visit-snapshots
                     (take 200 (search/dfs (fn [n] [(inc n)]) [0]
                                           {:emit :events :visit-key nil}))))))))

(deftest dfs-lifecycle-without-deduplication-pairs-each-occurrence
  (let [events (search/dfs {:root [:leaf :leaf]} [:root :root]
                          {:emit :events :visit-key nil})]
    (is (= (vec (mapcat identity
                       (repeat 2 [[:root :enter] [:leaf :enter] [:leaf :exit]
                                  [:leaf :enter] [:leaf :exit] [:root :exit]])))
           (event-phases events)))))

(deftest simple-path-lifecycle-pairs-paths-and-rejects-cycles
  (let [graph {:a [:b :c] :b [:a :c] :c [:a]}
        options {:extensions [search/path-extension]}
        events (vec (search/simple-paths graph [:a] (assoc options :emit :events)))]
    (is (= [[[:a] :enter] [[:a :b] :enter] [[:a :b :c] :enter]
            [[:a :b :c] :exit] [[:a :b] :exit] [[:a :c] :enter]
            [[:a :c] :exit] [[:a] :exit]]
           (mapv (fn [{:keys [phase visit]}] [(:path visit) phase])
                 (filter #(= :visit (:event %)) events))))
    (is (= (vec (search/simple-paths graph [:a] options))
           (mapv #(dissoc % :phase)
                 (filter #(= :enter (:phase %)) (visit-snapshots events)))))))

(deftest dfs-lifecycle-and-its-views-are-stack-safe
  (let [depth 20000
        space (fn [n] (when (< n depth) [(inc n)]))]
    (is (= (range (inc depth)) (map :state (search/dfs space [0]))))
    (is (= (range depth -1 -1) (map :state (search/postorder space [0]))))
    (let [events (search/dfs space [0] {:emit :events})]
      (is (= (* 3 (inc depth)) (count events)))
      (is (= (range (inc depth))
             (map :state (take (inc depth) (visit-snapshots events)))))
      (is (= (range depth -1 -1)
             (map :state
                  (filter #(= :exit (:phase %)) (visit-snapshots events))))))))

(deftest dfs-lifecycle-does-not-hide-expansion-errors-or-publish-a-false-exit
  (let [error (ex-info "Successor generation failed" {:state :broken})
        expanded (atom [])
        events (search/dfs
                (fn [state]
                  (swap! expanded conj state)
                  (case state :root [:leaf :broken] :broken (throw error) []))
                [:root] {:emit :events})]
    (is (= [[:root :enter] [:leaf :enter] [:leaf :exit] [:broken :enter]]
           (event-phases (take 7 events))))
    (is (= [:root :leaf] @expanded))
    (is (identical? error
                    (try
                      (nth events 7)
                      (catch clojure.lang.ExceptionInfo actual actual))))
    (is (= [:root :leaf :broken] @expanded))))

(deftest postorder-keeps-evaluation-separate-from-event-output
  (let [initialized (atom [])
        edges (atom [])
        visits (search/postorder
                {:root [:left :right] :left [:shared] :right [:shared]}
                [:root]
                {:fold {:init (fn [{:keys [state]}]
                                (swap! initialized conj state) 1)
                        :step (fn [n child]
                                (swap! edges conj (:state child))
                                (+ n (:value child)))}})]
    (is (= [{:state :shared :value 1}
            {:state :left :value 2}
            {:state :right :value 2}
            {:state :root :value 5}]
           (vec visits)))
    (is (= [:root :left :shared :right] @initialized))
    (is (= [:shared :left :shared :right] @edges))))

(deftest depth-first-output-selection-is-consistent
  (doseq [algorithm [search/dfs search/simple-paths]
          options [{} {:visit-key nil}
                   {:extensions [search/path-extension search/depth-extension]}]]
    (let [graph {:root [:a :b] :a [:shared] :b [:shared]}
          events (vec (algorithm graph [:root] (assoc options :emit :events)))]
      (doseq [phase [:enter :exit]]
        (is (= (mapv #(dissoc % :phase)
                     (filter #(= phase (:phase %)) (visit-snapshots events)))
               (vec (algorithm graph [:root] (assoc options :emit phase)))))))))

(deftest simple-path-exits-preserve-separate-route-occurrences
  (is (= [[:root :a :shared] [:root :a]
          [:root :b :shared] [:root :b] [:root]]
         (mapv :path
               (search/simple-paths
                {:root [:a :b] :a [:root :shared] :b [:shared]}
                [:root]
                {:emit :exit :extensions [search/path-extension]})))))

(deftest postorder-is-the-dfs-exit-view-including-without-deduplication
  (doseq [options [{} {:visit-key nil}
                   {:fold {:init (constantly 1)
                           :step (fn [n child] (+ n (:value child)))}}]]
    (let [graph {:root [:a :b] :a [:shared] :b [:shared]}]
      (is (= (vec (search/postorder graph [:root :root] options))
             (vec (search/dfs graph [:root :root] (assoc options :emit :exit))))))))

(deftest dfs-fold-events-expose-results-only-on-exit-and-defer-computation
  (let [calls (atom [])
        events (search/dfs
                {:root [:a :b] :a [:shared] :b [:shared]} [:root]
                {:emit :events
                 :fold {:init (fn [{:keys [state]}]
                                (swap! calls conj [:init state]) 1)
                        :step (fn [n child]
                                (is (not (contains? child :phase)))
                                (swap! calls conj [:step (:state child)])
                                (+ n (:value child)))
                        :finish (fn [n] (swap! calls conj [:finish n]) n)}})]
    (is (empty? @calls))
    (is (= :candidate (:event (first events))))
    (is (empty? @calls))
    (is (= {:event :visit :phase :enter :visit {:state :root}}
           (second events)))
    (is (empty? @calls))
    (is (= {:event :candidate :origin :edge
            :from {:state :root} :transition {:state :a}
            :visit {:state :a} :disposition :scheduled}
           (nth events 2)))
    (is (= [[:init :root]] @calls))
    (is (= {:event :visit :phase :enter :visit {:state :a}}
           (nth events 3)))
    (let [all (vec events)]
      (is (= [[:init :root] [:init :a] [:init :shared] [:finish 1]]
             (take 4 @calls)))
    (is (= [[:shared 1] [:a 2] [:b 2] [:root 5]]
           (mapv (juxt :state :value)
                 (filter #(= :exit (:phase %)) (visit-snapshots all)))))
    (is (every? #(not (contains? % :value))
                (filter #(= :enter (:phase %)) (visit-snapshots all))))
    (is (= 4 (count (filter #(= :init (first %)) @calls))))
    (is (= 4 (count (filter #(= :step (first %)) @calls)))))))

(deftest dfs-fold-events-support-cycles-early-stopping-and-false-values
  (testing "active back edges are phase-free and have no computed value"
    (let [views (atom [])
          events (search/dfs
                  {:root [:root :leaf]} [:root]
                  {:emit :events
                   :fold {:init (constantly 0)
                          :step (fn [n child] (swap! views conj child) (inc n))}})]
      (is (= [[:root :enter] [:leaf :enter] [:leaf :exit] [:root :exit]]
             (event-phases events)))
      (is (= [true false] (mapv :back-edge @views)))
      (is (= [false true] (mapv #(contains? % :value) @views)))))
  (doseq [value [nil false]]
    (let [calls (atom [])
          events (search/dfs
                  (fn [_] (throw (Exception. "Unexpected expansion"))) [:root]
                  {:emit :events
                   :fold {:init (fn [_] (swap! calls conj :init) (reduced value))
                          :step (fn [_ _] (throw (Exception. "Unexpected step")))
                          :finish (fn [v] (swap! calls conj :finish) v)}})]
      (is (= :candidate (:event (first events))))
      (is (empty? @calls))
      (is (= [{:state :root :phase :enter}
              {:state :root :phase :exit :value value}]
             (visit-snapshots events)))
      (is (= [:init :finish] @calls))))
  (testing "a reduced step suppresses later successor decoding"
    (let [decoded (atom [])
          events (search/dfs
                  {:root [:leaf :unused]} [:root]
                  {:emit :events
                   :decode-edge (fn [state] (swap! decoded conj state) {:state state})
                   :fold {:init (constantly nil)
                          :step (fn [_ _] (reduced :done))}})]
      (is (= [[:root :enter] [:leaf :enter] [:leaf :exit] [:root :exit]]
             (event-phases events)))
      (is (= [:leaf] @decoded))
      (is (= :done (:value (last (visit-snapshots events))))))))

(deftest unsupported-execution-modes-fail-explicitly
  (doseq [[algorithm options expected-key]
          [[search/dfs {:emit :unknown} :emit]
           [search/simple-paths {:emit nil} :emit]
           [search/simple-paths {:emit :unknown} :emit]
           [search/dfs {:fold {} :emit :enter} :emit]
           [search/dfs {:fold {}} :emit]
           [search/dfs {:fold {} :emit :exit :visit-key nil} :visit-key]
           [search/dfs {:fold {} :emit :events :visit-key nil} :visit-key]
           [search/bfs {:emit :exit} :emit]
           [search/bfs {:emit nil} :emit]
           [search/best-first {:emit :exit} :emit]
           [search/best-first {:emit nil} :emit]
           [search/postorder {:emit :enter} :emit]
           [search/postorder {:emit :unknown} :emit]
           [search/postorder {:emit nil} :emit]
           [search/best-first {:visit-key nil} :visit-key]
           [search/postorder {:fold {} :visit-key nil} :visit-key]]]
    (let [error (try
                  (algorithm {} [:root] options)
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (= expected-key (:option (ex-data error)))))))

(deftest unused-options-are-ignored-by-every-operation
  (let [graph {:root [:leaf]}
        unrelated {:events? true :unknown-option :unused}
        fold {:init (constantly 1)
              :step (fn [n child] (+ n (:value child)))}
        cases [[search/dfs {} {:priority nil :bottom nil :join nil}]
               [search/dfs {:emit :events} {:priority nil :bottom nil :join nil}]
               [search/dfs {:emit :exit :fold fold} {:priority nil :bottom nil :join nil}]
               [search/simple-paths {} {:fold fold :priority nil :bottom nil :join nil}]
               [search/simple-paths {:emit :events}
                {:fold fold :priority nil :bottom nil :join nil}]
               [search/bfs {} {:fold fold :priority nil :bottom nil :join nil}]
               [search/bfs {:emit :events}
                {:fold fold :priority nil :bottom nil :join nil}]
               [search/best-first {} {:fold fold :bottom nil :join nil}]
               [search/best-first {:emit :events} {:fold fold :bottom nil :join nil}]
               [search/postorder {} {:priority nil :bottom nil :join nil}]
               [search/postorder {:fold fold}
                {:priority nil :bottom nil :join nil}]]]
    (doseq [[algorithm options unused] cases]
      (is (= (vec (algorithm graph [:root] options))
             (vec (algorithm graph [:root] (merge options unused unrelated)))))))
  (let [graph {:root [:leaf]}
        options {:bottom #{} :join set/union}]
    (is (= (vec (search/propagate graph {:root #{:fact}} options))
           (vec (search/propagate
                 graph {:root #{:fact}}
                 (merge options {:emit :enter :fold {} :priority nil
                                 :visit-key nil :extensions [{}]
                                 :events? true :unknown-option :unused})))))))

(deftest execution-mode-checks-run-before-traversal-callbacks
  (doseq [[algorithm options]
          [[search/dfs {:emit :unknown}]
           [search/simple-paths {:emit :unknown}]
           [search/bfs {:emit :exit}]
           [search/best-first {:emit :exit}]
           [search/postorder {:emit :enter}]
           [search/best-first {:visit-key nil}]
           [search/dfs {:fold {}}]
           [search/dfs {:emit :events :fold {} :visit-key nil}]
           [search/postorder {:fold {} :visit-key nil}]]]
    (let [calls (atom [])
          options (assoc options :extensions
                         [{:start (fn [_]
                                    (swap! calls conj :start)
                                    {})}])]
      (is (thrown? clojure.lang.ExceptionInfo
                   (algorithm (fn [_] (swap! calls conj :space) []) [:root] options)))
      (is (empty? @calls)))))

(deftest propagate-validates-required-options-before-processing
  (doseq [[options expected-option]
          [[{:bottom 0} :join]
           [{:join max} :bottom]
           [{:bottom 0 :join nil} :join]]]
    (let [calls (atom [])
          error (try
                  (search/propagate (fn [_] (swap! calls conj :space) [])
                                    {:root 1}
                                    options)
                  nil
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= expected-option (:option (ex-data error))))
      (is (= :propagate (:algorithm (ex-data error))))
      (is (empty? @calls))))
  (testing "unsupported progress output fails explicitly"
    (let [error (try
                  (search/propagate {} {} {:bottom 0 :join max :emit :unknown})
                  (catch clojure.lang.ExceptionInfo error error))]
      (is (= :emit (:option (ex-data error))))
      (is (= :propagate (:algorithm (ex-data error))))))
  (testing "nil is a valid bottom value when the option is present"
    (let [progress (search/propagate
                    {:root [{:state :leaf :value :fact}]}
                    {:root :initial}
                    {:decode-edge identity
                     :bottom nil
                     :join (fn [current contribution]
                             (or current contribution))})]
      (is (= {:root :initial :leaf :fact} (:values (last progress)))))))

(deftest dfs-lifecycle-matches-a-reference-on-every-three-state-graph
  (letfn [(reference [graph starts]
            (let [seen (atom #{})]
              (letfn [(visit [state]
                        (when-not (contains? @seen state)
                          (swap! seen conj state)
                          (into [[state :enter]]
                                (concat (mapcat visit (graph state))
                                        [[state :exit]]))))]
                (vec (mapcat visit starts)))))]
    (doseq [edges (range 512)]
      (let [graph (into {} (for [from (range 3)]
                             [from (vec (for [to (range 3)
                                              :when (bit-test edges (+ (* from 3) to))]
                                          to))]))
            expected (reference graph [0 1 2 0])
            events (vec (search/dfs graph [0 1 2 0] {:emit :events}))]
        (is (= expected (event-phases events))
            (str "Lifecycle for graph " graph))
        (is (= (mapv first (filter #(= :enter (second %)) expected))
               (mapv :state (search/dfs graph [0 1 2 0]))))
        (is (= (mapv first (filter #(= :exit (second %)) expected))
               (mapv :state (search/postorder graph [0 1 2 0]))))))))

(deftest postorder-fold-reports-back-edges-without-a-result
  (let [observed (atom [])
        visits (vec (search/postorder {:root [:root :leaf]}
                                     [:root]
                                     {:fold
                                      {:init (constantly 0)
                                       :step (fn [n child]
                                               (swap! observed conj
                                                      [(:state child) (:back-edge child)
                                                       (contains? child :value)])
                                               (inc n))}}))]
    (is (= [:leaf :root] (mapv :state visits)))
    (is (= [0 2] (mapv :value visits)))
    (is (= [[:root true false] [:leaf false true]] @observed))))

(deftest searches-defer-user-callbacks-until-consumption
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]]
    (let [calls (atom [])
          record (fn [kind value] (swap! calls conj kind) value)
          visits (algorithm
                  (fn [_] (record :space [])) [:root]
                  {:project (fn [visit] (record :project (:state visit)))
                   :expand? (fn [_] (record :expand true))
                   :visit-key (fn [visit] (record :key (:state visit)))
                   :extensions [{:start (fn [_] (record :start {}))}]})]
      (is (empty? @calls))
      (is (= :root (:state (first visits))))
      (is (seq @calls)))))

(deftest preorder-searches-do-not-expand-the-last-requested-visit
  (doseq [algorithm [search/dfs search/bfs search/simple-paths search/best-first]]
    (let [expanded (atom [])
          visits (algorithm
                  (fn [state]
                    (swap! expanded conj state)
                    [(inc state)])
                  [0])]
      (is (= (range 20) (mapv :state (take 20 visits))))
      (is (= (range 19) @expanded)))))

(deftest searches-without-global-priority-consume-infinite-starts-incrementally
  (doseq [algorithm [search/dfs search/bfs search/simple-paths search/postorder]]
    (let [initialized (atom [])
          visits (algorithm
                  {} (range)
                  {:extensions [{:start (fn [{:keys [state]}]
                                          (swap! initialized conj state)
                                          {})}]})]
      (is (= (range 20) (mapv :state (take 20 visits))))
      (is (= (range 20) @initialized)))))

(deftest depth-first-searches-consume-infinite-successors-incrementally
  (doseq [algorithm [search/dfs search/simple-paths]]
    (let [decoded (atom [])
          visits (algorithm
                  (fn [state] (when (= :root state) (iterate inc 0)))
                  [:root]
                  {:decode-edge (fn [state]
                                  (swap! decoded conj state)
                                  {:state state})})]
      (is (= (into [:root] (range 19))
             (mapv :state (take 20 visits))))
      (is (= (range 19) @decoded)))))

(deftest bfs-consumes-an-infinite-level-without-expanding-it
  (let [expanded (atom [])
        visits (search/bfs
                (fn [state]
                  (swap! expanded conj state)
                  (if (= :root state) (range) [:deeper]))
                [:root])]
    (is (= (into [:root] (range 19))
           (mapv :state (take 20 visits))))
    (is (= [:root] @expanded))))

(deftest bfs-defers-expansion-while-earlier-starts-remain
  (let [expanded (atom [])
        visits (search/bfs
                (fn [state] (swap! expanded conj state) [:child])
                (range))]
    (is (= (range 20) (mapv :state (take 20 visits))))
    (is (empty? @expanded))))

(deftest postorder-does-not-request-successors-when-pruned-immediately
  (let [calls (atom [])
        visits (search/postorder
                (fn [_] (swap! calls conj :space) (range))
                [:root]
                {:expand? (fn [_] (swap! calls conj :expand) true)
                 :project (fn [visit]
                            (swap! calls conj :project)
                            (:state visit))
                 :fold {:init (fn [_] (reduced :ready))
                        :step (fn [_ _] (throw (Exception. "Unexpected step")))}})]
    (is (= [{:state :root :value :ready}] (vec visits)))
    (is (empty? @calls))))

(deftest best-first-inspects-the-whole-local-frontier-before-selecting
  (let [decoded (atom [])
        expanded (atom [])
        visits (search/best-first
                (fn [state]
                  (swap! expanded conj state)
                  (when (= :root state)
                    [{:state :expensive :weight 9}
                     {:state :cheap :weight 1}]))
                [:root]
                {:decode-edge (fn [transition]
                                (swap! decoded conj (:state transition))
                                transition)})]
    (is (= :root (:state (first visits))))
    (is (empty? @expanded))
    (is (empty? @decoded))
    (is (= :cheap (:state (second visits))))
    (is (= [:root] @expanded))
    (is (= [:expensive :cheap] @decoded))))

(deftest propagation-consumes-one-whole-expansion-per-progress-record
  (let [expanded (atom [])
        decoded (atom [])
        visits (search/propagate
                (fn [state]
                  (swap! expanded conj state)
                  (when (= :root state) [:a :b]))
                {:root 1}
                {:bottom 0 :join max
                 :decode-edge (fn [state]
                                (swap! decoded conj state)
                                {:state state})})]
    (is (= {:values {:root 1} :changes []} (first visits)))
    (is (empty? @expanded))
    (is (empty? @decoded))
    (is (= {:values {:root 1 :a 1 :b 1}
            :processed :root
            :changes [{:state :a :value 1} {:state :b :value 1}]}
           (second visits)))
    (is (= [:root] @expanded))
    (is (= [:a :b] @decoded))))

(deftest propagation-defers-initial-worklist-construction
  (let [initial-values {:root 1}
        enumerated (atom 0)
        original-keys keys]
    (with-redefs [clojure.core/keys
                  (fn [m]
                    (when (identical? initial-values m)
                      (swap! enumerated inc))
                    (original-keys m))]
      (let [visits (search/propagate {} initial-values {:bottom 0 :join max})]
        (is (zero? @enumerated))
        (is (= {:values initial-values :changes []} (first visits)))
        (is (zero? @enumerated))
        (is (= :root (:processed (second visits))))
        (is (= 1 @enumerated))
        (is (= 2 (count visits)))
        (is (= 1 @enumerated))))))

(deftest propagation-distinguishes-missing-values-from-nil-and-false
  (let [initial-values (array-map :root :seed :known-nil nil :known-false false)
        visits (search/propagate
                {:root [{:state :known-nil :value nil}
                        {:state :known-false :value false}
                        {:state :new :value nil}]}
                initial-values
                {:decode-edge identity
                 :bottom nil
                 :join (fn [_ contribution] contribution)})]
    (is (= {:values (assoc initial-values :new nil)
            :processed :root
            :changes [{:state :new :value nil}]}
           (second visits)))
    (is (= [:root :known-nil :known-false :new]
           (mapv :processed (rest visits))))))

(deftest searches-accept-nil-options-and-nil-extensions
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]]
    (let [graph {:a [:b] :b []}
          expected (vec (algorithm graph [:a]))]
      (is (= expected (vec (algorithm graph [:a] nil))))
      (is (= expected (vec (algorithm graph [:a] {:extensions nil})))))))

(deftest extension-callbacks-merge-different-map-representations
  (doseq [context [(array-map :score 3)
                   (hash-map :score 3 nil false false nil)
                   (sorted-map :score 3)
                   (into {} (map (fn [n] [n n]) (range 20)))
                   {}]]
    (let [extension {:start (constantly context)
                     :next (fn [_ _ _] context)}
          options {:extensions [extension]}]
      (is (= [(merge {:state :root} context)
              (merge {:state :leaf} context)]
             (vec (search/dfs {:root [:leaf]} [:root] options))))
      (is (= [(merge {:state :leaf} context)
              (merge {:state :root} context)]
             (vec (search/postorder {:root [:leaf]} [:root] options)))))))

(deftest searches-preserve-nil-and-false-state-identities
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]]
    (is (= #{nil false :root}
           (set (map :state (algorithm {:root [nil false]} [:root])))))))

(deftest state-only-and-attributed-decoding-produce-the-same-visits
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]
          extensions [nil [] [search/depth-extension]]]
    (let [map-state {:state :embedded :label :state-data}
          graph {:root [nil false map-state] nil [:leaf] false [:leaf]}
          options (cond-> {:extensions extensions}
                    (= algorithm search/best-first) (assoc :priority (constantly 0)))
          expected (vec (algorithm graph [:root] options))
          decoded (vec (algorithm graph [:root]
                                  (assoc options :decode-edge
                                         (fn [state]
                                           {:state state :label :edge
                                            :weight 3 :depth 999}))))]
      (is (= expected decoded))
      (is (= #{:root nil false map-state :leaf} (set (map :state expected))))
      (is (every? #(not-any? (partial contains? %) [:label :weight]) decoded)))))

(deftest ordered-extensions-see-only-visit-context-and-the-original-transition
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]
          attributed? [false true]]
    (let [observed (atom [])
          transition {:state :leaf :label :edge :depth 999}
          visits (vec
                  (algorithm
                   {:root [(if attributed? transition :leaf)]} [:root]
                   (cond-> {:extensions
                            [search/depth-extension
                             {:next (fn [parent edge candidate]
                                      (swap! observed conj [parent edge candidate])
                                     {:from (:state parent)})}]}
                     attributed? (assoc :decode-edge identity)
                     (= algorithm search/best-first) (assoc :priority (constantly 0)))))]
      (is (= [[{:state :root :depth 0}
               (if attributed? transition {:state :leaf})
               {:state :leaf :depth 1}]]
             @observed))
      (is (= {:state :leaf :depth 1 :from :root}
             (first (filter #(= :leaf (:state %)) visits)))))))

(deftest traversal-results-can-be-consumed-again-after-a-prefix
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]]
    (let [expanded (atom [])
          visits (algorithm (fn [state]
                              (swap! expanded conj state)
                              (get {:root [:left :right] :left [:leaf]} state))
                            [:root])
          prefix (vec (take 2 visits))
          complete (vec visits)
          calls @expanded]
      (is (= prefix (subvec complete 0 2)))
      (is (= complete (vec visits)))
      (is (= calls @expanded))
      (is (= 4 (count complete))))))

(deftest postorder-is-stack-safe-on-a-deep-chain
  (let [depth 15000
        visits (search/postorder (fn [n] (when (< n depth) [(inc n)])) [0])]
    (is (= (range depth -1 -1) (map :state visits)))))

(deftest postorder-consumes-infinite-starts-one-at-a-time
  (let [initialized (atom [])
        visits (search/postorder
                {} (range)
                {:extensions [{:start (fn [{:keys [state]}]
                                        (swap! initialized conj state)
                                        {})}]})]
    (is (= (range 20) (mapv :state (take 20 visits))))
    (is (= (range 20) @initialized))))

(deftest postorder-consumes-infinite-successors-one-at-a-time
  (let [decoded (atom [])
        expanded (atom [])
        visits (search/postorder
                (fn [state]
                  (swap! expanded conj state)
                  (when (= :root state) (range)))
                [:root]
                {:decode-edge (fn [state]
                                (swap! decoded conj state)
                                {:state state})})]
    (is (= (range 20) (mapv :state (take 20 visits))))
    (is (= (range 20) @decoded))
    (is (= (into [:root] (range 20)) @expanded))))

(deftest postorder-can-prune-an-infinite-successor-sequence
  (let [decoded (atom [])
        visits (search/postorder
                (fn [state] (when (= :root state) (range)))
                [:root]
                {:decode-edge (fn [state]
                                (swap! decoded conj state)
                                {:state state})
                 :fold {:init (constantly [])
                        :step (fn [children child]
                                (let [children (conj children (:state child))]
                                  (if (= 3 (count children))
                                    (reduced children)
                                    children)))}})]
    (is (= [{:state 0 :value []}
            {:state 1 :value []}
            {:state 2 :value []}
            {:state :root :value [0 1 2]}]
           (vec visits)))
    (is (= [0 1 2] @decoded))))

(deftest postorder-agrees-with-a-recursive-reference-on-cyclic-graphs
  (let [random (java.util.Random. 271828)]
    (dotimes [_ 60]
      (let [graph (into {}
                        (for [source (range 12)]
                          [source (vec (filter (fn [_] (zero? (.nextInt random 4)))
                                               (range 12)))]))
            known (atom #{})
            expected (atom [])]
        (letfn [(visit [state]
                  (when-not (contains? @known state)
                    (swap! known conj state)
                    (doseq [child (graph state)]
                      (visit child))
                    (swap! expected conj state)))]
          (doseq [start (range 12)]
            (visit start)))
        (is (= @expected
               (mapv :state (search/postorder graph (range 12)))))))))

(deftest best-first-replaces-improvements-without-changing-fifo-ties
  (let [graph {:root [[:goal 9] [:a 1] [:b 2] [:c 3]]
               :a [[:goal 5]]
               :b [[:goal 1]]
               :c [[:goal 0]]
               :goal [[:root 0]]}
        visits (vec (search/best-first
                     graph [:root :root]
                     {:decode-edge (fn [[state weight]]
                                     {:state state :weight weight})
                      :extensions [search/path-extension]}))]
    (is (= [:root :a :b :c :goal] (mapv :state visits)))
    (is (= [0 1 2 3 3] (mapv :distance visits)))
    (is (= [:root :b :goal] (:path (peek visits))))))

(deftest best-first-keeps-only-one-queued-candidate-per-key
  (let [enqueue @#'search/enqueue
        peak (atom 0)
        graph {:root (mapv (fn [weight] {:state :goal :weight weight})
                          (range 1000 0 -1))}
        visits
        (with-redefs-fn
          {#'search/enqueue
           (fn [& args]
             (let [state (apply enqueue args)]
               (swap! peak max (count (second state)))
               state))}
          #(vec (search/best-first graph [:root] {:decode-edge identity})))]
    (is (= [[:root 0] [:goal 1]]
           (mapv (juxt :state :distance) visits)))
    (is (= 1 @peak))))

(deftest best-first-agrees-with-reference-relaxation
  (let [random (java.util.Random. 481516)]
    (dotimes [_ 60]
      (let [size 12
            graph (into {}
                        (for [source (range size)]
                          [source
                           (vec (for [destination (range size)
                                      :when (zero? (.nextInt random 4))]
                                  {:state destination
                                   :weight (.nextInt random 10)}))]))
            reference
            (nth (iterate
                  (fn [distances]
                    (reduce-kv
                     (fn [result source distance]
                       (reduce (fn [result {:keys [state weight]}]
                                 (update result state
                                         #(if (nil? %)
                                            (+ distance weight)
                                            (min % (+ distance weight)))))
                               result
                               (graph source)))
                     distances
                     distances))
                  {0 0})
                 (dec size))
            visits (vec (search/best-first graph [0] {:decode-edge identity}))]
        (is (= reference (into {} (map (juxt :state :distance)) visits)))
        (is (= (count reference) (count visits)))))))

(deftest dfs-walks-depth-first-and-reports-each-state-once
  (is (= [:a :b :d :c]
         (mapv :state
               (search/dfs {:a [:b :c]
                            :b [:d]
                            :c [:d]
                            :d []}
                           [:a])))))

(deftest bfs-walks-in-level-order
  (is (= [:a :b :c :d :e]
         (mapv :state
               (search/bfs {:a [:b :c]
                            :b [:d]
                            :c [:e]
                            :d []
                            :e []}
                           [:a])))))

(deftest bfs-visits-all-starts-before-their-successors
  (is (= [:a :b :a-child :b-child]
         (mapv :state
               (search/bfs {:a [:a-child]
                            :a-child []
                            :b [:b-child]
                            :b-child []}
                           [:a :b])))))

(deftest bfs-first-arrival-has-minimum-edge-count
  (let [graph {:start [:detour :near]
               :detour [:middle]
               :middle [:goal]
               :near [:goal]
               :goal []}
        visits (search/bfs graph [:start]
                           {:extensions [search/path-extension
                                         search/depth-extension]})
        goal (some #(when (= :goal (:state %)) %)
                   visits)]
    (is (= 2 (:depth goal)))
    (is (= [:start :near :goal] (:path goal)))
    (is (= 1 (count (filter #(= :goal (:state %)) visits))))))

(deftest propagation-yields-initial-values-before-expanding
  (let [expanded (atom [])
        progress (search/propagate (fn [state]
                                     (swap! expanded conj state)
                                     [:next])
                                   {:start #{:fact}}
                                   {:bottom #{}
                                    :join set/union})]
    (is (= {:values {:start #{:fact}}
            :changes []}
           (first progress)))
    (is (not (contains? (first progress) :processed)))
    (is (empty? @expanded))
    (is (= {:state :next :value #{:fact}}
           (first (:changes (second progress)))))
    (is (= [:start] @expanded))))

(deftest propagation-processes-nil-and-false-states
  (doseq [state [nil false]]
    (let [processed (atom [])
          progress (search/propagate (fn [actual]
                                       (swap! processed conj actual)
                                       [])
                                     {state #{:fact}}
                                     {:bottom #{}
                                      :join set/union})]
      (is (= [state] (mapv :processed (rest progress))))
      (is (not (contains? (first progress) :processed)))
      (is (contains? (second progress) :processed))
      (is (not= (first progress) (second progress)))
      (is (= [state] @processed)))))

(deftest propagation-joins-and-reprocesses-through-a-cycle
  (let [cfg {:entry {:successors [:header]
                     :gen #{[:x :initial]}
                     :kill #{}}
             :header {:successors [:body :exit]
                      :gen #{}
                      :kill #{}}
             :body {:successors [:header]
                    :gen #{[:x :loop]}
                    :kill #{[:x :initial] [:x :loop]}}
             :exit {:successors []
                    :gen #{}
                    :kill #{}}}
        transfer (fn [{:keys [state value]}]
                   (let [{:keys [successors gen kill]} (cfg state)
                         outgoing (into gen (remove kill value))]
                     (map (fn [target]
                            {:state target :value outgoing})
                          successors)))
        progress (search/propagate transfer
                                   {:entry #{}}
                                   {:project identity
                                    :decode-edge identity
                                    :bottom #{}
                                    :join set/union})
        result (:values (last progress))]
    (is (= #{[:x :initial] [:x :loop]} (:header result)))
    (is (= #{[:x :initial] [:x :loop]} (:body result)))
    (is (= #{[:x :initial] [:x :loop]} (:exit result)))
    (is (= 2 (count (filter #(= :header (:processed %)) progress))))
    (is (= [{:state :header :value #{[:x :initial] [:x :loop]}}]
           (:changes (some #(when (and (= :body (:processed %))
                                       (some (fn [change]
                                               (= :header (:state change)))
                                             (:changes %)))
                             %)
                           progress))))))

(deftest propagation-supports-meet-based-constraint-pruning
  (let [domains {:a #{1 2 3}
                 :b #{1 2 3}
                 :c #{1 2 3}}
        relations {:a [[:b <]]
                   :b [[:a >] [:c <]]
                   :c [[:b >]]}
        revise (fn [{:keys [state value]}]
                 (for [[neighbour relation] (relations state)]
                   {:state neighbour
                    :value (into #{}
                                 (filter (fn [candidate]
                                           (some #(relation % candidate) value)))
                                 (domains neighbour))}))
        result (:values (last (search/propagate revise
                                                domains
                                                {:project identity
                                                 :decode-edge identity
                                                 :bottom nil
                                                 :join set/intersection})))]
    (is (= {:a #{1} :b #{2} :c #{3}}
           result))))

(deftest propagation-requeues-a-state-only-when-its-value-changes
  (let [processed (atom [])
        progress (search/propagate (fn [{:keys [state]}]
                                     (swap! processed conj state)
                                     [{:state state :value #{:fact}}])
                                   {:node #{:fact}}
                                   {:project identity
                                    :decode-edge identity
                                    :bottom #{}
                                    :join set/union})]
    (is (= [:node] (mapv :processed (rest progress))))
    (is (not (contains? (first progress) :processed)))
    (is (= [:node] @processed))
    (is (= [[]] (mapv :changes (rest progress))))))

(deftest propagation-records-successive-updates-to-the-same-destination
  (let [progress (vec (search/propagate
                       {:start [{:state :next :value #{:first}}
                                {:state :next :value #{:second}}
                                {:state :next :value #{:first}}]}
                       {:start #{}}
                       {:decode-edge identity
                        :bottom #{}
                        :join set/union}))]
    (is (= [{:state :next :value #{:first}}
            {:state :next :value #{:first :second}}]
           (:changes (second progress))))
    (is (= {:start #{} :next #{:first :second}}
           (:values (second progress))))
    (is (= [:start :next] (mapv :processed (rest progress))))
    (is (= [] (:changes (last progress))))
    (is (= (:values (second progress)) (:values (last progress))))))

(deftest propagation-schedules-a-new-state-even-at-bottom
  (let [progress (search/propagate {:start [:empty]
                                    :empty [:done]
                                    :done []}
                                   {:start #{}}
                                   {:bottom #{}
                                    :join set/union})]
    (is (= #{:start :empty :done}
           (set (keys (:values (last progress))))))
    (is (= [{:state :empty :value #{}}]
           (:changes (second progress))))
    (is (= [:start :empty :done]
           (mapv :processed (rest progress))))))

(deftest propagation-can-disable-expansion-with-the-common-guard
  (let [expanded (atom [])
        progress (search/propagate (fn [state]
                                     (swap! expanded conj state)
                                     [:child])
                                   {:root #{:fact}}
                                   {:bottom #{}
                                    :join set/union
                                    :expand? (constantly false)})]
    (is (= 2 (count progress)))
    (is (= {:values {:root #{:fact}}
            :processed :root
            :changes []}
           (second progress)))
    (is (empty? @expanded))))

(deftest bfs-extensions-add-depth-and-path
  (is (= [{:state :a :depth 0 :path [:a]}
          {:state :b :depth 1 :path [:a :b]}
          {:state :c :depth 1 :path [:a :c]}]
         (vec (search/bfs {:a [:b :c] :b [] :c []}
                          [:a]
                          {:extensions [search/depth-extension
                                        search/path-extension]})))))

(deftest bfs-is-lazy-after-each-visit
  (let [expanded (atom [])]
    (is (= [:start :goal]
           (mapv :state
                 (take 2
                       (search/bfs (fn [state]
                                     (swap! expanded conj state)
                                     (case state
                                       :start [:goal :later]
                                       []))
                                   [:start])))))
    (is (= [:start] @expanded))))

(deftest simple-paths-enumerates-distinct-routes
  (let [paths {:a [:b :c]
               :b [:c]
               :c [:a]}]
    (is (= [:a :b :c :c]
           (mapv :state (search/simple-paths paths [:a]))))
    (is (= [[:a] [:a :b] [:a :b :c] [:a :c]]
           (mapv :path
                 (search/simple-paths paths [:a]
                                      {:extensions [search/path-extension]}))))
    (is (= [0 1 2 3]
           (mapv :state
                 (take 4 (search/simple-paths (fn [n] [(inc n)]) [0])))))))

(deftest postorder-returns-successors-before-parents
  (is (= [:parse :compile :package]
         (mapv :state
               (search/postorder {:package [:compile]
                                    :compile [:parse]
                                    :parse []}
                                   [:package])))))

(deftest postorder-is-lazy-between-completed-visits
  (let [expanded (atom [])
        dependencies (fn [task]
                       (swap! expanded conj task)
                       (case task
                         :root [:left :right]
                         []))
        first-visit (first (search/postorder dependencies [:root]))]
    (is (= :left (:state first-visit)))
    (is (= [:root :left] @expanded))))

(deftest postorder-fold-can-aggregate-a-subtree
  (let [subtree-size
        {:init (constantly 1)
         :step (fn [size child] (+ size (:value child)))}]
    (is (= [[:base 1] [:lib 2] [:config 1] [:app 4]]
           (mapv (juxt :state :value)
                 (search/postorder {:app [:lib :config]
                                      :lib [:base]
                                      :base []
                                      :config []}
                                     [:app]
                                     {:fold subtree-size}))))))

(deftest postorder-can-stop-after-a-cached-result
  (let [fold {:init (fn [{:keys [state]}] (when (= state :cached) state))
              :step (fn [selected child]
                      (if-let [source (:value child)]
                        (reduced source)
                        selected))}]
    (is (= [{:state :cached :value :cached}
            {:state :root :value :cached}]
           (vec (search/postorder {:root [:cached :build]}
                                 [:cached :root] {:fold fold}))))))

(deftest postorder-reduced-checks-before-decoding-the-next-successor
  (let [decoded (atom [])
        fold {:init (fn [{:keys [state]}] (= state :cached))
              :step (fn [_ child] (if (:value child) (reduced true) false))}
        visits (search/postorder {:root [:cached :fallback]
                                    :cached []
                                    :fallback []}
                                   [:root]
                                   {:fold fold
                                    :decode-edge (fn [state]
                                                   (swap! decoded conj state)
                                                   {:state state})})]
    (is (= [:cached :root] (mapv :state visits)))
    (is (= [:cached] @decoded))))

(deftest postorder-keeps-accumulators-out-of-route-context
  (let [parents (atom [])
        visits (search/postorder
                {:root [:left :right]} [:root]
                {:extensions [{:next (fn [parent _ _]
                                       (swap! parents conj parent)
                                       {})}]
                 :fold {:init (constantly 1)
                        :step (fn [size child] (+ size (:value child)))}})]
    (is (= [1 1 3] (mapv :value visits)))
    (is (= [{:state :root} {:state :root}] @parents))))

(deftest postorder-fold-reports-cycle-edges
  (let [edge-report
        {:init (constantly [])
         :step (fn [edges {:keys [state via back-edge]}]
                 (conj edges [state (:kind via) back-edge]))}]
    (is (= [[:b [[:a :depends-on true]]]
            [:a [[:b :depends-on false]]]]
           (mapv (juxt :state :value)
                 (search/postorder {:a [[:b :depends-on]]
                                      :b [[:a :depends-on]]}
                                     [:a]
                                     {:decode-edge (fn [[state kind]]
                                                     {:state state :kind kind})
                                      :fold edge-report}))))))

(deftest postorder-folds-shared-results-with-parent-local-aggregates
  (let [sum {:init (fn [{:keys [state]}]
                    (get {:ten 10 :shared 1 :other 2} state 0))
             :step (fn [total child] (+ total (:value child)))}]
    (is (= [[:ten 10] [:shared 1] [:warmup 11] [:other 2] [:root 3]]
           (mapv (juxt :state :value)
                 (search/postorder {:warmup [:ten :shared]
                                    :root [:shared :other]}
                                   [:warmup :root]
                                   {:fold sum}))))))

(deftest postorder-folds-each-edge-with-current-edge-data
  (let [fold {:init (constantly [])
              :step (fn [edges child]
                      (conj edges [(:state child) (:weight (:via child))
                                   (:back-edge child)]))}]
    (is (= [[:child [[:root 3 true]]]
            [:root [[:child 1 false] [:child 2 false]]]]
           (mapv (juxt :state :value)
                 (search/postorder {:root [[:child 1] [:child 2]]
                                    :child [[:root 3]]}
                                   [:root]
                                   {:decode-edge (fn [[state weight]]
                                                   {:state state :weight weight})
                                    :fold fold}))))))

(deftest postorder-cached-edge-fast-path-preserves-candidate-and-stop-semantics
  (let [decoded (atom [])
        prepared (atom [])
        initialized (atom [])
        visits (search/postorder
                {:root [:shared :shared :unrequested]} [:shared :root]
                {:decode-edge (fn [state]
                                (swap! decoded conj state)
                                {:state state :label :edge})
                 :extensions [{:next (fn [_ transition _]
                                       (swap! prepared conj transition)
                                       {})}]
                 :fold {:init (fn [{:keys [state]}]
                                (swap! initialized conj state)
                                (if (= state :shared) false []))
                        :step (fn [children child]
                                (let [children (conj children
                                                     [(:state child) (:value child)
                                                      (:back-edge child)])]
                                  (if (= 2 (count children))
                                    (reduced children)
                                    children)))}})]
    (is (= [{:state :shared :value false}
            {:state :root :value [[:shared false false] [:shared false false]]}]
           (vec visits)))
    (is (= [:shared :root] @initialized))
    (is (= [:shared :shared] @decoded))
    (is (= [{:state :shared :label :edge}
            {:state :shared :label :edge}]
           @prepared))))

(deftest postorder-decoded-edge-fields-do-not-leak-into-visits
  (doseq [extensions [nil [] [search/depth-extension]]]
    (let [visits (vec (search/postorder
                      {:root [{:state :child :label :edge :weight 3}]}
                      [:root]
                      {:decode-edge identity
                       :extensions extensions
                       :fold {:init (constantly [])
                              :step (fn [edges child]
                                      (conj edges (:via child)))}}))]
      (is (= [{:state :child :label :edge :weight 3}]
             (:value (last visits))))
      (is (every? #(not-any? (partial contains? %) [:label :weight :via :back-edge])
                  visits)))))

(deftest postorder-fold-agrees-with-reference-on-every-three-node-directed-graph
  (let [edges (vec (for [from (range 3), to (range 3)] [from to]))]
    (doseq [mask (range 512)]
      (let [graph (reduce (fn [graph [i [from to]]]
                            (if (bit-test mask i)
                              (update graph from conj to)
                              graph))
                          (zipmap (range 3) (repeat []))
                          (map-indexed vector edges))
            entered (atom #{})
            finished (atom {})
            expected (atom [])]
        (letfn [(visit [state]
                  (when-not (contains? @entered state)
                    (swap! entered conj state)
                    (let [value (mapv (fn [child]
                                        (visit child)
                                        [child (not (contains? @finished child))
                                         (get @finished child)])
                                      (graph state))]
                      (swap! finished assoc state value)
                      (swap! expected conj {:state state :value value}))))]
          (doseq [state (range 3)] (visit state)))
        (let [fold {:init (constantly [])
                    :step (fn [children child]
                            (conj children [(:state child) (:back-edge child)
                                            (:value child)]))}
              events (search/dfs graph (range 3) {:emit :events :fold fold})]
          (is (= @expected
                 (vec (search/postorder graph (range 3) {:fold fold})))
              (str "Postorder graph mask " mask))
          (is (= @expected
                 (vec (search/dfs graph (range 3) {:emit :exit :fold fold})))
              (str "DFS exit graph mask " mask))
          (is (= @expected
                 (mapv #(dissoc % :phase)
                       (filter #(= :exit (:phase %))
                               (visit-snapshots events)))))
          (is (= (+ 9 (reduce + (map count (vals graph))))
                 (count events))))))))

(deftest postorder-finishes-each-key-once-including-early-stops
  (let [finished (atom [])
        visits (search/postorder
                {:root [:a :b]} [:root :a]
                {:fold {:init (fn [visit] [(:state visit) 0])
                        :step (fn [[state total] child]
                                (reduced [state (+ total (:value child))]))
                        :finish (fn [[state total]]
                                  (swap! finished conj state)
                                  (inc total))}})]
    (is (= [[:a 1] [:root 2]] (mapv (juxt :state :value) visits)))
    (is (= [:a :root] @finished))))

(deftest postorder-fold-can-prune-without-retaining-successors
  (let [decoded (atom [])
        fold {:init (constantly nil)
              :step (fn [_ child] (reduced (:state child)))}
        visits (search/postorder {:root (range)} [:root]
                                 {:decode-edge (fn [state]
                                                 (swap! decoded conj state)
                                                 {:state state})
                                  :fold fold})]
    (is (= [{:state 0 :value nil} {:state :root :value 0}] (vec visits)))
    (is (= [0] @decoded))))

(deftest postorder-fold-handles-wide-fanout-with-a-scalar-accumulator
  (let [handled (atom 0)
        fold {:init (constantly 1)
              :step (fn [size child]
                      (swap! handled inc)
                      (+ size (:value child)))}]
    (is (= 20001
           (:value (last (search/postorder {::root (range 20000)} [::root]
                                          {:fold fold})))))
    (is (= 20000 @handled))))

(deftest postorder-fold-is-stack-safe-and-lazy
  (let [handled (atom 0)
        fold {:init (constantly 1)
              :step (fn [size child]
                      (swap! handled inc)
                      (+ size (:value child)))}
        visits (search/postorder #(when (< % 20000) [(inc %)]) [0]
                                 {:fold fold})]
    (is (= 20000 (:state (first visits))))
    (is (zero? @handled))
    (is (= 20001 (:value (last visits))))
    (is (= 20000 @handled))))

(deftest postorder-fold-preserves-nil-false-and-structured-values
  (doseq [value [nil false 0 [] {:size 1}]]
    (is (= [value value]
           (mapv :value
                 (search/postorder {:root [:child]} [:root]
                                   {:fold {:init (constantly value)
                                           :step (fn [_ child] (:value child))}}))))))

(deftest postorder-fold-initialization-sees-composed-route-context
  (let [initialized (atom [])
        visits (search/postorder
                {:root [:child]} [:root]
                {:extensions
                 [search/depth-extension
                  {:start (fn [visit] {::score (inc (:depth visit))})
                   :next (fn [_ _ visit] {::score (inc (:depth visit))})}]
                 :fold {:init (fn [visit]
                                (swap! initialized conj (:state visit))
                                {::score (::score visit) :nodes 1})
                        :step (fn [value child]
                                (-> value
                                    (update ::score + (::score (:value child)))
                                    (update :nodes + (:nodes (:value child)))))}})]
    (is (empty? @initialized))
    (is (= [[1 {::score 2 :nodes 1}]
            [0 {::score 3 :nodes 2}]]
           (mapv (juxt :depth :value) visits)))
    (is (= [:root :child] @initialized))))

(deftest postorder-fold-completion-handles-reduced-false-and-nil
  (doseq [value [nil false]]
    (let [completed (atom [])
          visits (search/postorder
                  (fn [_] (throw (Exception. "Unexpected expansion")))
                  [:a :a :b]
                  {:fold {:init (fn [_] (reduced value))
                          :step (fn [_ _] (throw (Exception. "Unexpected step")))
                          :finish (fn [result]
                                    (swap! completed conj result)
                                    result)}})]
      (is (empty? @completed))
      (is (= [{:state :a :value value} {:state :b :value value}]
             (vec visits)))
      (is (= [value value] @completed)))))

(deftest postorder-fold-handles-current-edge-weights-and-unfinished-back-edges
  (let [sum {:init (constantly 1)
             :step (fn [total child]
                     (if (:back-edge child)
                       total
                       (+ total (* (:weight (:via child)) (:value child)))))}]
    (is (= [[:b 1] [:a 6]]
           (mapv (juxt :state :value)
                 (search/postorder {:a [[:b 2] [:b 3]] :b [[:a 9]]}
                                   [:a]
                                   {:decode-edge (fn [[state weight]]
                                                   {:state state :weight weight})
                                    :fold sum}))))))

(deftest postorder-fold-agrees-with-direct-dag-dynamic-programming
  (let [edges (vec (for [from (range 4), to (range (inc from) 4)] [from to]))]
    (doseq [mask (range 64)]
      (let [graph (reduce (fn [graph [i [from to]]]
                            (if (bit-test mask i)
                              (update graph from conj to)
                              graph))
                          (zipmap (range 4) (repeat []))
                          (map-indexed vector edges))
            init (fn [state] (if (empty? (graph state)) 1N 0N))
            expected (reduce (fn [values state]
                               (assoc values state
                                      (reduce +' (init state)
                                              (map values (graph state)))))
                             {} (reverse (range 4)))
            sum {:init (fn [visit] (init (:state visit)))
                 :step (fn [ways child] (+' ways (:value child)))}]
        (is (= expected
               (into {} (map (juxt :state :value))
                     (search/postorder graph (range 4)
                                       {:fold sum}))))))))

(deftest best-first-orders-visits-by-cumulative-distance
  (let [routes {:home [[:slow 5] [:quick 1]]
                :quick [[:goal 2]]
                :slow [[:goal 1]]
                :goal []}
        decode (fn [[state weight]] {:state state :weight weight})]
    (is (= [[:home 0] [:slow 1] [:quick 1] [:goal 2]]
           (mapv (juxt :state :distance)
                 (search/best-first {:home [:slow :quick]
                                     :slow [:goal]
                                     :quick [:goal]
                                     :goal []}
                                    [:home]))))
    (is (= [[:home 0] [:quick 1] [:goal 3] [:slow 5]]
           (mapv (juxt :state :distance)
                 (search/best-first routes [:home]
                                    {:decode-edge decode}))))
    (is (= [0 1 3]
           (mapv :state
                 (search/best-first {0 [3 1] 1 [] 3 []}
                                    [0]
                                    {:priority :state}))))
    (is (= [:z :a :b]
           (mapv :state
                 (search/best-first {:z [:b :a] :a [] :b []}
                                    [:z]
                                    {:priority :state}))))
    (let [without-distance
          (first (search/best-first {0 []} [0] {:priority :state}))
          with-distance
          (first (search/best-first {0 []} [0]
                                    {:priority :state
                                     :extensions
                                     [search/distance-extension]}))]
      (is (not (contains? without-distance :distance)))
      (is (= 0 (:distance with-distance))))))

(deftest best-first-selects-the-lowest-pending-not-globally-increasing-priority
  (let [roads {:a [{:state :b :weight 2} {:state :c :weight 5}]
               :b [{:state :a :weight 2} {:state :c :weight 1}]
               :c [{:state :a :weight 5} {:state :b :weight 1}]}
        joining-edge {:start (fn [_] {:joining-edge 0})
                      :next (fn [_ transition _]
                              {:joining-edge (:weight transition)})}]
    (is (= [[:a 0] [:b 2] [:c 1]]
           (mapv (juxt :state :joining-edge)
                 (search/best-first roads [:a]
                                    {:decode-edge identity
                                     :extensions [joining-edge]
                                     :priority :joining-edge}))))))

(deftest best-first-breaks-equal-priorities-in-enqueue-order
  (is (= [:start :first :second :third]
         (mapv :state
               (search/best-first {:start [:first :second :third]}
                                  [:start])))))

(deftest best-first-is-lazy-over-an-infinite-chain
  (let [expanded (atom [])]
    (is (= [0 1 2 3]
           (mapv :state
                 (take 4
                       (search/best-first (fn [n]
                                            (swap! expanded conj n)
                                            [(inc n)])
                                          [0])))))
    (is (= [0 1 2] @expanded))))

(deftest best-first-defers-starts-until-consumption-and-queues-them-all
  (let [consumed (atom [])
        initialized (atom [])
        expanded (atom [])
        starts (map (fn [state]
                      (swap! consumed conj state)
                      state)
                    [5 1 3])
        visits (search/best-first
                (fn [state] (swap! expanded conj state) [])
                starts
                {:extensions [{:start (fn [visit]
                                        (swap! initialized conj (:state visit))
                                        {})}]
                 :priority :state})]
    (is (empty? @consumed))
    (is (empty? @initialized))
    (is (empty? @expanded))
    (is (= {:state 1} (first visits)))
    (is (= [5 1 3] @consumed))
    (is (= [5 1 3] @initialized))
    (is (empty? @expanded))
    (is (= [1 3 5] (mapv :state visits)))
    (is (= [5 1 3] @initialized))
    (is (= [1 3 5] @expanded))))

(deftest best-first-defers-start-callback-errors-until-consumption
  (let [error (ex-info "Start initialization failed" {})
        visits (search/best-first
                {} [:start]
                {:extensions [{:start (fn [_] (throw error))}]})]
    (is (identical? error
                    (try
                      (first visits)
                      (catch clojure.lang.ExceptionInfo actual
                        actual))))))

(deftest best-first-can-use-a-consistent-heuristic
  (let [roads {:home [[:hill 1] [:river 2]]
               :hill [[:goal 5]]
               :river [[:goal 2]]
               :goal []}
        estimate {:home 3 :hill 4 :river 2 :goal 0}
        route (first
               (filter #(= :goal (:state %))
                       (search/best-first
                        roads [:home]
                        {:decode-edge (fn [[state weight]]
                                        {:state state :weight weight})
                        :extensions [search/path-extension
                                     search/distance-extension]
                         :priority (fn [{:keys [state distance]}]
                                     (+ distance (estimate state)))})))]
    (is (= {:state :goal
            :distance 4
            :path [:home :river :goal]}
           route))))

(deftest best-first-can-revisit-a-state-with-a-distinct-distance-key
  (let [routes {:start [[:a 3] [:b 1]]
                :a [[:goal 1]]
                :b [[:a 1]]
                :goal []}
        options {:decode-edge (fn [[state weight]]
                                {:state state :weight weight})
                 :extensions [search/distance-extension]
                 :priority (fn [{:keys [state distance]}]
                             (+ distance
                                ({:start 0 :a 0 :b 2 :goal 0} state)))}
        goal (fn [options]
               (first (filter #(= :goal (:state %))
                              (search/best-first routes [:start] options))))]
    (testing "settling each state once can return a suboptimal route"
      (is (= 4 (:distance (goal options)))))
    (testing "including distance in the key lets A* reconsider the state"
      (is (= 3 (:distance
                (goal (assoc options
                             :visit-key (juxt :state :distance)))))))))

(deftest first-filter-stops-at-the-first-match
  (let [expanded (atom [])]
    (is (= :goal
           (:state
            (first
             (filter #(= :goal (:state %))
                     (search/dfs (fn [state]
                                   (swap! expanded conj state)
                                   (case state
                                     :start [:goal :later]
                                     []))
                                 [:start]))))))
    (is (= [:start] @expanded))))

(deftest accumulate-can-track-remaining-fuel
  (let [fuel-extension
        (search/accumulate :fuel-left (constantly 10)
                           (fn [fuel transition]
                             (- fuel (:fuel-used transition))))]
    (is (= [[:trailhead 10] [:ridge 7]]
           (mapv (juxt :state :fuel-left)
                 (search/dfs {:trailhead [[:ridge 3]]
                              :ridge []}
                             [:trailhead]
                             {:decode-edge (fn [[state fuel-used]]
                                             {:state state
                                              :fuel-used fuel-used})
                              :extensions [fuel-extension]}))))))

(deftest maps-and-state-functions-can-describe-spaces
  (testing "a missing map entry is a leaf"
    (is (= [:alone]
           (mapv :state (search/dfs {} [:alone])))))
  (testing "a function can generate successors"
    (is (= [0 1 2 3]
           (mapv :state
                 (search/dfs (fn [n] (when (< n 3) [(inc n)]))
                             [0])))))
  (testing "an infinite space can be consumed a finite prefix at a time"
    (is (= [0 1 2 3]
           (mapv :state
                 (take 4 (search/dfs (fn [n] [(inc n)]) [0])))))))

(deftest map-valued-states-are-not-treated-as-transition-maps
  (let [start {:board :start}
        goal {:board :goal}]
    (is (= [start goal]
           (mapv :state (search/dfs {start [goal] goal []} [start]))))))

(deftest starts-are-explored-in-order
  (is (= [:a :a-child :b :b-child]
         (mapv :state
               (search/dfs {:a [:a-child]
                            :a-child []
                            :b [:b-child]
                            :b-child []}
                           [:a :b])))))

(deftest starting-visit-extensions-are-realized-on-demand
  (doseq [algorithm [search/dfs search/bfs search/postorder]]
    (let [initialized (atom [])
          first-visit (first (algorithm {}
                                         (range 20)
                                         {:extensions
                                          [{:start (fn [visit]
                                                     (swap! initialized conj
                                                            (:state visit))
                                                     {})}]}))]
      (is (= 0 (:state first-visit)))
      (is (= [0] @initialized)))))

(deftest postorder-decodes-only-the-next-successor-before-yielding
  (let [decoded (atom [])
        first-visit (first (search/postorder
                            {:root (range 20)}
                            [:root]
                            {:decode-edge (fn [state]
                                            (swap! decoded conj state)
                                            {:state state})}))]
    (is (= 0 (:state first-visit)))
    (is (= [0] @decoded))))

(deftest visit-key-controls-which-visits-are-distinct
  (let [graph {:start [:left :right]
               :left [:goal]
               :right [:goal]
               :goal []}]
    (testing "the default state key keeps the first arrival"
      (is (= [:start :left :goal :right]
             (mapv :state (search/dfs graph [:start])))))
    (testing "nil preserves visits reached by distinct routes"
      (is (= [:start :left :goal :right :goal]
             (mapv :state
                   (search/dfs graph [:start] {:visit-key nil})))))))

(deftest visit-key-can-distinguish-the-same-state-by-context
  (is (= [[:start 0] [:goal 1] [:middle 1] [:goal 2]]
         (mapv (juxt :state :depth)
               (search/dfs {:start [:goal :middle]
                            :goal []
                            :middle [:goal]}
                           [:start]
                           {:extensions [search/depth-extension]
                            :visit-key (juxt :state :depth)})))))

(deftest extensions-carry-route-context
  (is (= [{:state :a :depth 0 :path [:a]}
          {:state :b :depth 1 :path [:a :b]}
          {:state :c :depth 2 :path [:a :b :c]}]
         (vec (search/dfs {:a [:b] :b [:c] :c []}
                          [:a]
                          {:extensions [search/depth-extension
                                        search/path-extension]})))))

(deftest custom-extensions-use-start-and-next-callbacks
  (let [steps-extension
        {:start (fn [_visit] {:steps 0})
         :next (fn [parent _transition _visit]
                 {:steps (inc (:steps parent))})}]
    (is (= [{:state :a :steps 0}
            {:state :b :steps 1}]
           (vec (search/dfs {:a [:b] :b []}
                            [:a]
                            {:extensions [steps-extension]}))))))

(deftest modified-built-in-extensions-use-their-replaced-callbacks
  (let [depth-extension
        (assoc search/depth-extension
               :next (fn [parent _transition _visit]
                       {:depth (+ 10 (:depth parent))}))]
    (is (= [{:state :a :depth 0}
            {:state :b :depth 10}]
           (vec (search/dfs {:a [:b] :b []}
                            [:a]
                            {:extensions [depth-extension]})))))
  (is (= [{:state :a :depth 0}
          {:state :b :depth 1}]
         (vec (search/dfs {:a [:b] :b []}
                          [:a]
                          {:decode-edge (fn [state] {:state state})
                           :extensions [search/depth-extension]})))))

(deftest start-callbacks-run-before-duplicate-start-rejection
  (doseq [algorithm [search/dfs search/bfs search/postorder search/best-first]]
    (let [initialized (atom [])
          visits (vec (algorithm {} [:root :root]
                                 {:extensions
                                  [{:start (fn [visit]
                                             (swap! initialized conj (:state visit))
                                             {})}]}))]
      (is (= [:root] (mapv :state visits)))
      (is (= [:root :root] @initialized)))))

(deftest next-callbacks-run-before-duplicate-or-path-repeat-rejection
  (doseq [algorithm [search/dfs search/bfs search/simple-paths
                     search/postorder search/best-first]]
    (let [prepared (atom [])
          visits (vec (algorithm
                       {:root [:child] :child [:root]} [:root]
                       {:extensions
                        [{:next (fn [parent transition visit]
                                  (swap! prepared conj
                                         [(:state parent)
                                          (:state transition)
                                          (:state visit)])
                                  {})}]}))]
      (is (= #{:root :child} (set (map :state visits))))
      (is (= 2 (count visits)))
      (is (= [[:root :child :child] [:child :root :root]]
             @prepared)))))

(deftest extensions-compose-in-order-and-can-replace-custom-fields
  (let [initial {:start (fn [_] {::value 1})
                 :next (fn [parent _ _]
                         {::value (inc (::value parent))})}
        adjust {:start (fn [visit]
                          {::value (* 10 (::value visit))})
                :next (fn [_ _ visit]
                        {::value (* 10 (::value visit))})}]
    (doseq [algorithm [search/dfs search/bfs search/simple-paths
                       search/postorder search/best-first]]
      (let [visits (algorithm {:a [:b]} [:a]
                             {:extensions [initial adjust]})]
        (is (= {:a 10 :b 110}
               (into {} (map (juxt :state ::value)) visits)))))))

(deftest postorder-fold-composes-with-stable-custom-identity-and-shared-results
  (let [identity-extension {:start (fn [visit] {::id (:state visit)})
                            :next (fn [_ _ visit] {::id (:state visit)})}
        fold {:init (constantly 1)
              :step (fn [size child] (+ size (:value child)))
              :finish #(* 10 %)}
        visits (vec (search/postorder
                     {:root [:left :right]
                      :left [:shared]
                      :right [:shared]}
                     [:root]
                     {:visit-key ::id
                      :extensions [identity-extension]
                      :fold fold}))]
    (is (= [:shared :left :right :root] (mapv :state visits)))
    (is (= (mapv :state visits) (mapv ::id visits)))
    (is (= [10 110 110 2210] (mapv :value visits)))))

(deftest postorder-context-key-keeps-depth-pruned-results-separate
  (doseq [pruning [:expand? :init]]
    (let [subtree-size
          {:init (fn [visit]
                   (if (and (= pruning :init) (>= (:depth visit) 2))
                     (reduced 1)
                     1))
           :step (fn [size child] (+ size (:value child)))}
          options
          (cond-> {:visit-key (juxt :state :depth)
                   :extensions [search/depth-extension]
                   :fold subtree-size}
            (= pruning :expand?)
            (assoc :expand? #(< (:depth %) 2)))
          visits (vec (search/postorder
                       {:root [:detour :shared]
                        :detour [:shared]
                        :shared [:leaf]}
                       [:root]
                       options))]
      (is (= [[2 1] [1 2]]
             (mapv (juxt :depth :value)
                   (filter #(= :shared (:state %)) visits))))
      (is (= [:leaf]
             (mapv :state (filter #(= :leaf (:state %)) visits))))
      (is (= 5 (:value (last visits)))))))

(deftest projection-can-give-a-space-the-whole-visit
  (let [successors (fn [{:keys [state depth]}]
                     (if (< depth 2) [(inc state)] []))]
    (is (= [{:state 10 :depth 0}
            {:state 11 :depth 1}
            {:state 12 :depth 2}]
           (vec (search/dfs successors
                            [10]
                            {:project identity
                             :extensions [search/depth-extension]}))))))

(deftest expand-predicate-prunes-successors
  (is (= [{:state 0 :depth 0}
          {:state 1 :depth 1}
          {:state 2 :depth 2}]
         (vec (search/dfs {0 [1]
                           1 [2]
                           2 [3]
                           3 []}
                          [0]
                          {:extensions [search/depth-extension]
                           :expand? #(< (:depth %) 2)})))))

(deftest decoder-and-trail-preserve-edge-data
  (let [graph {:start [[:next 7]]
               :next []}]
    (is (= [{:state :start
             :trail [{:state :start}]}
            {:state :next
             :trail [{:state :start} {:state :next :weight 7}]}]
           (vec (search/dfs graph
                            [:start]
                            {:decode-edge (fn [[state weight]]
                                            {:state state :weight weight})
                             :extensions [search/trail-extension]}))))))

(deftest search-is-lazy-after-each-visit
  (let [expanded (atom [])]
    (is (= :goal
           (:state
            (some (fn [visit]
                    (when (= :goal (:state visit)) visit))
                  (search/dfs (fn [state]
                                (swap! expanded conj state)
                                (case state
                                  :start [:goal :later]
                                  []))
                              [:start])))))
    (is (= [:start] @expanded))))
