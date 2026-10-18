(ns state-space.core-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [state-space.core :as search]))

(deftest postorder-processed-successors-include-back-edges
  (let [observed (atom [])
        extension
        {:continue? (fn [{:keys [state]} processed]
                      (when (= state :root)
                        (swap! observed conj
                               (mapv (juxt :state :back-edge) processed)))
                      true)
         :next (fn [parent _transition _visit]
                 {:earlier (mapv (juxt :state :back-edge)
                                 (:processed-successors parent))})}
        visits (vec (search/postorder {:root [:root :leaf]}
                                       [:root]
                                       {:extensions [extension]}))]
    (is (= [:leaf :root] (mapv :state visits)))
    (is (= [[:root true]] (:earlier (first visits))))
    (is (= [[] [[:root true]] [[:root true] [:leaf false]]]
           @observed))))

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
                   :extensions [{:start (fn [_] (record :start {}))
                                 :exit (fn [_ _] (record :exit {}))}]})]
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
                 :extensions [{:continue? (fn [_ _] false)}]})]
    (is (= [{:state :root}] (vec visits)))
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
                     :next (fn [_ _ _] context)
                     :exit (fn [_ _] context)}
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
                 :extensions
                 [{:continue? (fn [_ processed] (< (count processed) 3))
                   :exit (fn [_ children]
                           {:children (mapv :state children)})}]})]
    (is (= [{:state 0 :children []}
            {:state 1 :children []}
            {:state 2 :children []}
            {:state :root :children [0 1 2]}]
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
        {:continue? (fn [{:keys [state]} processed]
                  (or (not= state :root)
                      (not-any? :ready? processed)))
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

(deftest postorder-continue-checks-before-decoding-the-next-successor
  (let [decoded (atom [])
        extension {:continue? (fn [{:keys [state]} processed]
                            (or (not= state :root)
                                (not-any? :available? processed)))
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
                           {:finished (mapv :state (:processed-successors parent))})}
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

(deftest postorder-context-key-keeps-depth-pruned-results-separate
  (doseq [pruning [:expand? :continue?]]
    (let [subtree-size
          (cond-> {:exit (fn [_ children]
                           {::size (inc (reduce + 0 (map ::size children)))})}
            (= pruning :continue?)
            (assoc :continue? (fn [visit _] (< (:depth visit) 2))))
          options
          (cond-> {:visit-key (juxt :state :depth)
                   :extensions [search/depth-extension subtree-size]}
            (= pruning :expand?)
            (assoc :expand? #(< (:depth %) 2)))
          visits (vec (search/postorder
                       {:root [:detour :shared]
                        :detour [:shared]
                        :shared [:leaf]}
                       [:root]
                       options))]
      (is (= [[2 1] [1 2]]
             (mapv (juxt :depth ::size)
                   (filter #(= :shared (:state %)) visits))))
      (is (= [:leaf]
             (mapv :state (filter #(= :leaf (:state %)) visits))))
      (is (= 5 (::size (last visits)))))))

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
