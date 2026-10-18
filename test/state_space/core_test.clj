(ns state-space.core-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [state-space.core :as search]))

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

(deftest propagation-schedules-a-new-state-even-at-bottom
  (let [progress (search/propagate {:start [:empty]
                                    :empty [:done]
                                    :done []}
                                   {:start #{}}
                                   {:bottom #{}
                                    :join set/union})]
    (is (= #{:start :empty :done}
           (set (keys (:values (last progress))))))
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

(deftest postorder-exit-can-aggregate-a-subtree
  (let [subtree-size
        {:exit (fn [_visit successors]
                 {:size (inc (reduce + (map :size successors)))})}]
    (is (= [[:base 1] [:lib 2] [:config 1] [:app 4]]
           (mapv (juxt :state :size)
                 (search/postorder {:app [:lib :config]
                                      :lib [:base]
                                      :base []
                                      :config []}
                                     [:app]
                                     {:extensions [subtree-size]}))))))

(deftest postorder-can-stop-after-a-cached-result
  (let [extension
        {:more? (fn [{:keys [state]} previous]
                  (or (not= state :root)
                      (not-any? :ready? previous)))
         :exit (fn [{:keys [state]} successors]
                 (if (= state :root)
                   {:selected (or (some #(when (:ready? %) (:state %))
                                        successors)
                                  :build)}
                   {:ready? (= state :cached)}))}]
    (is (= [{:state :cached :ready? true}
            {:state :root :selected :cached}]
           (mapv #(select-keys % [:state :ready? :selected])
                 (search/postorder {:root [:cached :build]
                                      :cached []
                                      :build []}
                                     [:root]
                                     {:extensions [extension]}))))))

(deftest postorder-more-checks-before-decoding-the-next-successor
  (let [decoded (atom [])
        extension {:more? (fn [{:keys [state]} previous]
                            (or (not= state :root)
                                (not-any? :available? previous)))
                   :exit (fn [{:keys [state]} _successors]
                           {:available? (= state :cached)})}
        visits (search/postorder {:root [:cached :fallback]
                                    :cached []
                                    :fallback []}
                                   [:root]
                                   {:extensions [extension]
                                    :decode-edge (fn [state]
                                                   (swap! decoded conj state)
                                                   {:state state})})]
    (is (= [:cached :root] (mapv :state visits)))
    (is (= [:cached] @decoded))))

(deftest postorder-next-callback-sees-finished-siblings
  (let [extension {:next (fn [parent _transition _visit]
                           {:finished (mapv :state (:previous parent))})}
        right (some #(when (= :right (:state %)) %)
                    (search/postorder {:root [:left :right]
                                         :left []
                                         :right []}
                                        [:root]
                                        {:extensions [extension]}))]
    (is (= [:left] (:finished right)))))

(deftest postorder-exit-reports-cycle-edges
  (let [edge-report
        {:exit (fn [_visit successors]
                 {:edges (mapv (fn [{:keys [state via back-edge]}]
                                 [state (:kind via) back-edge])
                               successors)})}]
    (is (= [[:b [[:a :depends-on true]]]
            [:a [[:b :depends-on false]]]]
           (mapv (juxt :state :edges)
                 (search/postorder {:a [[:b :depends-on]]
                                      :b [[:a :depends-on]]}
                                     [:a]
                                     {:decode-edge (fn [[state kind]]
                                                     {:state state :kind kind})
                                      :extensions [edge-report]}))))))

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

(deftest postorder-exit-composes-with-stable-custom-identity-and-shared-results
  (let [identity-extension {:start (fn [visit] {::id (:state visit)})
                            :next (fn [_ _ visit] {::id (:state visit)})}
        count-extension {:exit (fn [_ children]
                                 {::size (inc (reduce + 0 (map ::size children)))})}
        adjust-extension {:exit (fn [visit _]
                                  {::original-size (::size visit)
                                   ::size (* 10 (::size visit))})}
        visits (vec (search/postorder
                     {:root [:left :right]
                      :left [:shared]
                      :right [:shared]}
                     [:root]
                     {:visit-key ::id
                      :extensions [identity-extension
                                   count-extension
                                   adjust-extension]}))]
    (is (= [:shared :left :right :root] (mapv :state visits)))
    (is (= (mapv :state visits) (mapv ::id visits)))
    (is (= [1 11 11 221] (mapv ::original-size visits)))
    (is (= [10 110 110 2210] (mapv ::size visits)))))

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
