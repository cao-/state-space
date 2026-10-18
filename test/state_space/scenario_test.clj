(ns state-space.scenario-test
  "Scenarios showing compositions of traversal, route identity, custom priorities,
  bottom-up folds, and fixed-point propagation. Results are checked against small
  independent solvers, not callback timing or implementation details. Fixed seeds
  make generated cases reproducible. Exhaustive solvers are bounded test oracles,
  not alternatives intended for large inputs; scenario-specific restrictions are
  documented alongside the tests."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [state-space.core :as ss]))

(defn- reachable [graph start blocked]
  (loop [pending [start], seen #{}]
    (if (empty? pending)
      seen
      (let [node (peek pending)
            pending (pop pending)]
        (if (or (contains? blocked node) (contains? seen node))
          (recur pending seen)
          (recur (into pending (graph node)) (conj seen node)))))))

(defn- simple-routes [graph start goal]
  (letfn [(routes [node path cost]
            (if (= node goal)
              [{:path path :cost cost}]
              (mapcat (fn [[child weight]]
                        (when-not (some #{child} path)
                          (routes child (conj path child) (+ cost weight))))
                      (graph node))))]
    (routes start [start] 0)))

(defn- permutations [items]
  (if (empty? items)
    [[]]
    (mapcat (fn [item]
              (map #(cons item %) (permutations (remove #{item} items))))
            items)))

(defn- random-graph [random nodes]
  (into {} (for [from nodes]
             [from (vec (filter (fn [_] (zero? (.nextInt ^java.util.Random random 3)))
                                nodes))])))

(deftest route-identity-enumerates-ranked-alternatives
  (let [random (java.util.Random. 510)]
    (dotimes [_ 25]
      (let [graph (into {} (for [from (range 6)]
                             [from (vec (for [to (range (inc from) 6)
                                              :when (.nextBoolean random)]
                                          [to (.nextInt random 6)]))]))
            expected (sort (map :cost (simple-routes graph 0 5)))
            arrivals (filter #(= 5 (:state %))
                             (ss/best-first
                              graph [0]
                              {:decode-edge (fn [[state weight]]
                                              {:state state :weight weight})
                               :visit-key :path
                               :extensions [ss/path-extension]}))]
        (is (= expected (map :distance arrivals)) (str "Ranked routes in " graph))
        (is (= (take 3 expected) (map :distance (take 3 arrivals))))
        (is (= (set (map (juxt :path :cost) (simple-routes graph 0 5)))
               (set (map (juxt :path :distance) arrivals))))))))

(defn- bounded-route-cost [graph start goal budget]
  (letfn [(costs [node remaining cost]
            (concat (when (= node goal) [cost])
                    (when (pos? remaining)
                      (mapcat (fn [[child weight]]
                                (costs child (dec remaining) (+ cost weight)))
                              (graph node)))))]
    (when-let [costs (seq (costs start budget 0))]
      (apply min costs))))

(deftest edge-budget-is-independent-of-space-projection
  (let [graph {:start [[:junction 10] [:detour 1]]
               :detour [[:junction 1] [:start 0]]
               :junction [[:goal 1]]
               :goal []}
        search (fn [budget]
                 (ss/best-first
                  (fn [{:keys [state depth]}]
                    (when (< depth budget) (graph state)))
                  [:start]
                  {:project identity
                   :decode-edge (fn [[state weight]] {:state state :weight weight})
                   :extensions [ss/depth-extension ss/path-extension]
                   :visit-key (juxt :state :depth)}))]
    (doseq [budget (range 5)]
      (let [goal (first (filter #(= :goal (:state %)) (search budget)))]
        (is (= (bounded-route-cost graph :start :goal budget) (:distance goal)))
        (when goal
          (is (<= (:depth goal) budget))
          (is (= (inc (:depth goal)) (count (:path goal)))))))
    (is (= 11 (:distance (first (filter #(= :goal (:state %)) (search 2))))))
    (is (= 3 (:distance (first (filter #(= :goal (:state %)) (search 3))))))))

(deftest widest-route-uses-a-nonadditive-priority
  (let [random (java.util.Random. 511)
        bottleneck (ss/accumulate :capacity (constantly ##Inf)
                                  (fn [capacity edge] (min capacity (:weight edge))))]
    (dotimes [_ 25]
      (let [graph (into {} (for [from (range 5)]
                             [from (vec (for [to (range 5)
                                              :when (and (not= from to)
                                                         (.nextBoolean random))]
                                          [to (inc (.nextInt random 9))]))]))
            routes (simple-routes graph 0 4)
            route-capacity (fn [path]
                             (reduce min ##Inf
                                     (map (fn [[from to]] (get (into {} (graph from)) to))
                                          (partition 2 1 path))))
            expected (when (seq routes) (apply max (map (comp route-capacity :path) routes)))
            goal (first
                  (filter #(= 4 (:state %))
                          (ss/best-first
                           graph [0]
                           {:decode-edge (fn [[state weight]] {:state state :weight weight})
                            :extensions [bottleneck ss/path-extension]
                            :priority #(- (:capacity %))})))]
        (is (= expected (:capacity goal)) (str "Widest route in " graph))
        (when goal
          (is (= expected (route-capacity (:path goal))))
          (is (not (contains? goal :distance))))))))

(deftest minimum-assignment-shares-equivalent-partial-assignments
  ;; Search is exponential, unlike the specialized polynomial Hungarian algorithm.
  (let [random (java.util.Random. 512)]
    (doseq [n (range 1 6), _ (range 4)]
      (let [costs (vec (repeatedly n #(vec (repeatedly n (fn [] (.nextInt random 10))))))
            expected (apply min (for [assignment (permutations (range n))]
                                  (reduce + (map-indexed #(get-in costs [%1 %2]) assignment))))
            goal (first
                  (filter #(= n (first (:state %)))
                          (ss/best-first
                           (fn [[row used]]
                             (for [column (range n)
                                   :when (and (< row n) (not (contains? used column)))]
                               {:state [(inc row) (conj used column)]
                                :weight (get-in costs [row column])
                                :column column}))
                           [[0 #{}]]
                           {:decode-edge identity :extensions [ss/trail-extension]})))
            assignment (mapv :column (rest (:trail goal)))]
        (is (= expected (:distance goal)) (str "Assignment costs " costs))
        (is (= n (count assignment) (count (set assignment))))
        (is (= expected (reduce + (map-indexed #(get-in costs [%1 %2]) assignment))))))))

(deftest cheapest-city-tour-agrees-with-simple-path-enumeration
  (let [random (java.util.Random. 519)]
    (doseq [n (range 2 7)]
      (let [graph (into {} (for [from (range n)]
                             [from (vec (for [to (range n) :when (not= from to)]
                                          {:state to :weight (.nextInt random 10)}))]))
            expected (apply min
                            (for [order (permutations (range 1 n))]
                              (reduce + (map (fn [[from to]]
                                               (:weight (first (filter #(= to (:state %))
                                                                       (graph from)))))
                                             (partition 2 1 (cons 0 order))))))
            exhaustive (filter #(= n (count (:path %)))
                               (ss/simple-paths
                                graph [0]
                                {:decode-edge identity
                                 :extensions [ss/path-extension ss/distance-extension]}))
            goal (first
                  (filter #(= n (count (first (:state %))))
                          (ss/best-first
                           (fn [[visited node]]
                             (for [{to :state weight :weight} (graph node)
                                   :when (not (contains? visited to))]
                               {:state [(conj visited to) to] :weight weight}))
                           [[#{0} 0]]
                           {:decode-edge identity :extensions [ss/path-extension]})))
            route (mapv second (:path goal))]
        (is (= expected (apply min (map :distance exhaustive)) (:distance goal)))
        (is (= (set (range n)) (set route)))
        (is (= n (count route)))
        (is (= expected
               (reduce + (map (fn [[from to]]
                                (:weight (first (filter #(= to (:state %)) (graph from)))))
                              (partition 2 1 route)))))))))

(deftest subset-construction-preserves-automaton-language
  (let [delta {[0 \a] #{0 1} [0 \b] #{0} [1 \b] #{2} [2 \b] #{3}}
        transition (fn [states symbol] (into #{} (mapcat #(delta [% symbol])) states))
        states (set (map :state
                         (ss/bfs (fn [states] (map #(transition states %) [\a \b]))
                                 [#{0}])))
        table (into {} (for [state states, symbol [\a \b]]
                         [[state symbol] (transition state symbol)]))
        words (take 7 (iterate (fn [words] (for [word words, c [\a \b]] (str word c))) [""]))]
    (is (= #{#{0} #{0 1} #{0 2} #{0 3}} states))
    (is (every? states (vals table)))
    (doseq [word (mapcat identity words)]
      (let [final (reduce #(table [%1 %2]) #{0} word)]
        (is (= (.endsWith ^String word "abb") (contains? final 3)) (str "Input " word))))))

(deftest iterative-deepening-composes-lazy-depth-limited-searches
  (let [expanded (atom [])
        space (fn [n] (swap! expanded conj n) [(* 2 n) (inc (* 2 n))])
        visits (mapcat (fn [limit]
                         (ss/dfs space [1]
                                 {:visit-key nil
                                  :extensions [ss/depth-extension ss/path-extension]
                                  :expand? #(< (:depth %) limit)}))
                       (range 5))]
    (is (empty? @expanded))
    (let [goal (first (filter #(= 13 (:state %)) visits))]
      (is (= [1 3 6 13] (:path goal)))
      (is (= 3 (:depth goal)))
      (is (= (:depth goal)
             (:depth (first (filter #(= 13 (:state %))
                                    (ss/bfs (fn [n] [(* 2 n) (inc (* 2 n))]) [1]
                                            {:extensions [ss/depth-extension]}))))))
      (is (every? #(< % 8) @expanded)))))

(defn- matrix-chain-cost [dimensions]
  ;; Explicit split nodes group the two operands without buffering child pairs.
  (let [space (fn [[kind i j k]]
                (case kind
                  :range (when (> (- j i) 1)
                           (mapv #(vector :split i j %) (range (inc i) j)))
                  :split [[:range i k] [:range k j]]))
        fold {:init (fn [{[kind i j k] :state}]
                      (if (= kind :split)
                        (* (dimensions i) (dimensions k) (dimensions j))
                        (if (= (inc i) j) 0 ##Inf)))
              :step (fn [cost child]
                      ((if (= :range (first (:state child))) + min)
                       cost (:value child)))}]
    (:value (last (ss/postorder space [[:range 0 (dec (count dimensions))]]
                               {:fold fold})))))

(defn- recursive-matrix-cost [dimensions i j]
  (if (= (inc i) j)
    0
    (apply min (for [k (range (inc i) j)]
                 (+ (recursive-matrix-cost dimensions i k)
                    (recursive-matrix-cost dimensions k j)
                    (* (dimensions i) (dimensions k) (dimensions j)))))))

(deftest matrix-chain-fold-models-grouped-alternatives
  (let [random (java.util.Random. 513)]
    (is (= 4500 (matrix-chain-cost [10 30 5 60])))
    (doseq [n (range 1 7), _ (range 5)]
      (let [dimensions (vec (repeatedly (inc n) #(inc (.nextInt random 15))))]
        (is (= (recursive-matrix-cost dimensions 0 n) (matrix-chain-cost dimensions))
            (str "Matrix dimensions " dimensions))))))

(defn- schedule-profit [jobs]
  (let [n (count jobs)
        next-job (fn [i]
                   (or (first (filter #(>= (first (jobs %)) (second (jobs i)))
                                      (range (inc i) n)))
                       n))
        space (fn [i]
                (when (< i n)
                  [{:state (inc i) :profit 0}
                   {:state (next-job i) :profit (nth (jobs i) 2)}]))]
    (:value (last (ss/postorder
                   space [0]
                   {:decode-edge identity
                    :fold {:init (constantly 0)
                           :step (fn [profit child]
                                   (max profit (+ (:value child) (:profit (:via child)))))}})))))

(deftest weighted-scheduling-keeps-contributions-from-parallel-edges
  (is (= 0 (schedule-profit [])))
  (is (= 14 (schedule-profit [[1 3 5] [2 5 6] [4 6 5] [6 9 4]])))
  (is (= 7 (schedule-profit [[0 1 7]])))
  (let [random (java.util.Random. 514)]
    (dotimes [_ 25]
      (let [jobs (vec (sort-by first
                              (repeatedly 6 #(let [start (.nextInt random 9)]
                                               [start (+ start 1 (.nextInt random 4))
                                                (.nextInt random 12)]))))
            expected (apply max
                            (for [mask (range (bit-shift-left 1 (count jobs)))
                                  :let [selected (keep-indexed #(when (bit-test mask %1) %2) jobs)]
                                  :when (every? (fn [[a b]] (<= (second a) (first b)))
                                                (partition 2 1 selected))]
                              (reduce + (map #(nth % 2) selected))))]
        (is (= expected (schedule-profit jobs)) (str "Scheduled jobs " jobs))))))

(deftest tree-fold-publishes-multiple-metrics-with-bounded-accumulators
  (let [random (java.util.Random. 515)]
    (doseq [n (range 1 26)]
      (let [parents (into {} (for [child (range 1 n)] [child (.nextInt random child)]))
            tree (reduce-kv (fn [tree child parent] (update tree parent (fnil conj []) child))
                            {} parents)
            undirected (reduce-kv (fn [g child parent]
                                    (-> g
                                        (update parent (fnil conj []) child)
                                        (update child (fnil conj []) parent)))
                                  {} parents)
            distances (fn [start]
                        (loop [distance {start 0}, pending [start]]
                          (if (empty? pending)
                            distance
                            (let [node (first pending)
                                  children (remove #(contains? distance %) (undirected node))]
                              (recur (reduce #(assoc %1 %2 (inc (distance node))) distance children)
                                     (into (vec (rest pending)) children))))))
            root (last
                  (ss/postorder
                   tree [0]
                   {:fold
                    {:init (constantly {:height 0 :second-height 0 :diameter 0})
                     :step (fn [{:keys [height second-height diameter]} {:keys [value]}]
                             (let [child-height (inc (:height value))]
                               {:height (max height child-height)
                                :second-height (max second-height (min height child-height))
                                :diameter (max diameter (:diameter value))}))
                     :finish (fn [{:keys [height second-height diameter]}]
                               {:height height :diameter (max diameter (+ height second-height))})}}))]
        (is (= {:height (apply max (vals (distances 0)))
                :diameter (apply max (mapcat #(vals (distances %)) (range n)))}
               (:value root))
            (str "Tree " tree))))))

(defn- low-link-bridges [graph]
  (let [visits (ss/postorder
                graph (sort (keys graph))
                {:extensions [ss/depth-extension ss/path-extension]
                 :fold
                 {:init (fn [{:keys [depth path]}]
                          {:low depth :parent (when (> (count path) 1)
                                               (nth path (- (count path) 2)))})
                  :step (fn [{:keys [parent] :as acc} child]
                          (if (and (:back-edge child) (= parent (:state child)))
                            acc
                            (update acc :low min (if (:back-edge child)
                                                  (:depth child)
                                                  (:value child)))))
                  :finish :low}})]
    (set (for [{:keys [depth path value]} visits
               :when (and (pos? depth) (= depth value))]
           (set (take-last 2 path))))))

(deftest low-links-find-bridges-in-disconnected-simple-undirected-graphs
  ;; Parent identity suffices for simple graphs, not parallel undirected edges.
  (let [edges (vec (for [a (range 4), b (range (inc a) 4)] [a b]))]
    (doseq [mask (range 64)]
      (let [present (keep-indexed #(when (bit-test mask %1) %2) edges)
            graph (reduce (fn [g [a b]] (-> g (update a conj b) (update b conj a)))
                          (zipmap (range 4) (repeat [])) present)
            expected (set (for [[a b] present
                                :let [without (-> graph
                                                  (update a #(remove #{b} %))
                                                  (update b #(remove #{a} %)))]
                                :when (not (contains? (reachable without a #{}) b))]
                            #{a b}))]
        (is (= expected (low-link-bridges graph)) (str "Undirected graph " graph))))))

(defn- kosaraju-components [graph]
  (let [nodes (vec (distinct (concat (keys graph) (mapcat val graph))))
        transpose (reduce-kv (fn [g from children]
                               (reduce #(update %1 %2 (fnil conj []) from) g children))
                             {} graph)
        order (reverse (map :state (ss/postorder graph nodes)))]
    (:components
     (reduce (fn [{:keys [assigned] :as result} node]
               (if (contains? assigned node)
                 result
                 (let [component (set (map :state
                                          (ss/dfs #(remove (fn [x] (contains? assigned x))
                                                           (transpose %))
                                                  [node])))]
                   (-> result
                       (update :assigned into component)
                       (update :components conj component)))))
             {:assigned #{} :components #{}} order))))

(deftest strongly-connected-components-compose-two-traversals
  ;; Two passes avoid exposing mutable Tarjan stack state through callbacks.
  (let [random (java.util.Random. 516)]
    (doseq [graph (cons {nil [false] false [nil :leaf]}
                       (repeatedly 40 #(random-graph random (range 5))))]
      (let [nodes (set (concat (keys graph) (mapcat val graph)))
            closure (into {} (for [node nodes] [node (reachable graph node #{})]))
            expected (set (for [node nodes]
                            (set (filter #(and (contains? (closure node) %)
                                               (contains? (closure %) node))
                                         nodes))))]
        (is (= expected (kosaraju-components graph)) (str "Directed graph " graph))))))

(deftest dominators-converge-by-intersection-through-cycles
  (let [random (java.util.Random. 517)]
    (dotimes [_ 25]
      (let [nodes (set (range 6))
            graph (into {} (for [from (range 6)]
                             [from (vec (distinct
                                         (concat (when (< from 5) [(inc from)])
                                                 (filter (fn [_] (.nextBoolean random))
                                                         (range 1 6)))))]))
            expected (into {} (for [node nodes]
                                [node (set (filter #(not (contains?
                                                          (reachable graph 0 #{%}) node))
                                                   nodes))]))
            result (:values
                    (last (ss/propagate
                           (fn [{:keys [state value]}]
                             (mapv #(hash-map :state % :value (conj value %)) (graph state)))
                           (assoc (zipmap nodes (repeat nodes)) 0 #{0})
                           {:project identity :decode-edge identity
                            :bottom nodes :join set/intersection})))]
        (is (= expected result) (str "Control-flow graph " graph))))))

(deftest minimum-cost-propagation-allows-negative-edges-without-negative-cycles
  ;; Potential-adjusted nonnegative weights guarantee no negative cycle.
  (let [random (java.util.Random. 518)]
    (dotimes [_ 25]
      (let [n 6
            potentials (vec (repeatedly n #(- (.nextInt random 21) 10)))
            graph (into {} (for [from (range n)]
                             [from (vec (for [to (range n)
                                              :when (and (not= from to) (.nextBoolean random))]
                                          [to (+ (.nextInt random 4)
                                                 (- (potentials to) (potentials from)))]))]))
            expected (nth
                      (iterate
                       (fn [costs]
                         (reduce-kv
                          (fn [next from cost]
                            (reduce (fn [next [to weight]]
                                      (update next to #(min (or % ##Inf) (+ cost weight))))
                                    next (graph from)))
                          costs costs))
                       {0 0})
                      (dec n))
            result (:values
                    (last (ss/propagate
                           (fn [{:keys [state value]}]
                             (mapv (fn [[to weight]] {:state to :value (+ value weight)})
                                   (graph state)))
                           {0 0}
                           {:project identity :decode-edge identity
                            :bottom ##Inf :join min})))]
        (is (= expected result) (str "Weighted graph " graph))))))
