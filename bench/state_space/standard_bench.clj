;; Usage:
;;   clojure -M:standard-bench
;;   clojure -M:standard-bench --warmup N --samples N [--workload NAME]
;; Compares scaled workloads with direct Clojure implementations, checks
;; result equality before timing, and reports median time and thread allocation.

(ns state-space.standard-bench
  "Compare library operations with direct Clojure implementations."
  (:require [clojure.set :as set]
            [state-space.core :as search])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util Comparator PriorityQueue]
           [java.util Locale]))

(defn- grid-neighbors [side blocked-column [x y]]
  (keep (fn [[next-x next-y :as position]]
          (when (and (<= 0 next-x (dec side))
                     (<= 0 next-y (dec side))
                     (not (and (= next-x blocked-column)
                               (not= next-y (dec side)))))
            position))
        [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn- direct-grid-distance [neighbors start goal]
  (loop [pending (conj clojure.lang.PersistentQueue/EMPTY start)
         distances {start 0}]
    (when (seq pending)
      (let [position (peek pending)
            pending (pop pending)]
        (if (= position goal)
          (get distances position)
          (let [next-distance (inc (get distances position))
                [pending distances]
                (reduce (fn [[pending distances] next-position]
                          (if (contains? distances next-position)
                            [pending distances]
                            [(conj pending next-position)
                             (assoc distances next-position next-distance)]))
                        [pending distances]
                        (neighbors position))]
            (recur pending distances)))))))

(defn- grid-workload [side]
  (let [middle (quot side 2)
        start [0 middle]
        goal [(dec side) middle]
        neighbors #(grid-neighbors side middle %)
        expected (+ (dec side) (* 2 (- (dec side) middle)))]
    {:name (str "grid-shortest-path-" side)
     :label (format "Grid shortest path, %d x %d" side side)
     :scale (format "%d x %d" side side)
     :expected expected
     :library #(->> (search/bfs neighbors [start]
                                {:extensions [search/depth-extension]})
                    (filter (fn [visit] (= goal (:state visit))))
                    first
                    :depth)
     :direct #(direct-grid-distance neighbors start goal)}))

(defn- dfs-successors [size state]
  (cond-> []
    (< (inc state) size) (conj (inc state))
    (< (+ state 3) size) (conj (+ state 3))
    (= state (quot size 2)) (conj 0)))

(defn- direct-dfs [neighbors start expand?]
  (loop [pending [start], visited #{}, order []]
    (if (empty? pending)
      order
      (let [state (peek pending)
            pending (pop pending)]
        (if (contains? visited state)
          (recur pending visited order)
          (let [visited (conj visited state)
                successors (when (expand? state) (neighbors state))]
            (recur (into pending (rseq (or successors [])))
                   visited
                   (conj order state))))))))

(defn- dfs-workload [size]
  (let [cutoff (* 3 (quot size 4))
        neighbors #(dfs-successors size %)
        expand? #(< % cutoff)
        direct #(direct-dfs neighbors 0 expand?)
        expected (direct)]
    {:name (str "dfs-pruned-graph-" size)
     :label (format "Depth-first reachability with pruning (%d states)" size)
     :scale (format "%d states" size)
     :expected expected
     :library #(mapv :state
                     (search/dfs neighbors [0]
                                 {:expand? (fn [{:keys [state]}]
                                             (< state cutoff))}))
     :direct direct}))

(defn- tree-seq-workload [depth]
  (let [leaf-start (dec (bit-shift-left 1 depth))
        node-count (dec (bit-shift-left 1 (inc depth)))
        branch? #(< % leaf-start)
        children (fn [node] [(inc (* 2 node)) (+ 2 (* 2 node))])
        direct #(vec (tree-seq branch? children 0))
        expected (direct)]
    {:name (str "tree-seq-dfs-" depth)
     :label (format "Binary-tree traversal, tree-seq vs dfs (depth %d)" depth)
     :scale (format "%d nodes" node-count)
     :expected expected
     :library #(mapv :state
                     (search/dfs (fn [node]
                                   (when (branch? node) (children node)))
                                 [0]
                                 {:visit-key nil}))
     :direct direct}))

(defn- weighted-grid-neighbors [side blocked-column [x y]]
  (keep (fn [[next-x next-y :as position]]
          (when (and (<= 0 next-x (dec side))
                     (<= 0 next-y (dec side))
                     (not (and (= next-x blocked-column)
                               (not= next-y (dec side)))))
            [position
             (inc (mod (+ (* next-x 17) (* next-y 31))
                       9))]))
        [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn- direct-priority-distance [neighbors start goal heuristic]
  (let [queue (PriorityQueue.
               11
               (reify Comparator
                 (compare [_ left right]
                   (let [priority-order (compare (first left) (first right))]
                     (if (zero? priority-order)
                       (compare (second left) (second right))
                       priority-order)))))]
    (.add queue [(heuristic start) 0 0 start])
    (loop [distances {start 0}, order 1]
      (when-not (.isEmpty queue)
        (let [[_ _ distance state] (.poll queue)]
          (cond
            (not= distance (get distances state))
            (recur distances order)

            (= state goal)
            distance

            :else
            (let [[distances order]
                  (reduce (fn [[distances order] [next-state weight]]
                            (let [next-distance (+ distance weight)
                                  previous (get distances next-state)]
                              (if (and previous (<= previous next-distance))
                                [distances order]
                                (do
                                  (.add queue [(+ next-distance (heuristic next-state))
                                               order next-distance next-state])
                                  [(assoc distances next-state next-distance)
                                   (inc order)]))))
                          [distances order]
                          (neighbors state))]
              (recur distances order))))))))

(defn- direct-weighted-grid-distance [neighbors start goal]
  (direct-priority-distance neighbors start goal (constantly 0)))

(defn- weighted-grid-workload [side]
  (let [middle (quot side 2)
        start [0 middle]
        goal [(dec side) middle]
        neighbors #(weighted-grid-neighbors side middle %)
        decode-edge (fn [[state weight]] {:state state :weight weight})
        expected (direct-weighted-grid-distance neighbors start goal)]
    {:name (str "weighted-grid-shortest-path-" side)
     :label (format "Weighted grid least-cost route, %d x %d" side side)
     :scale (format "%d x %d" side side)
     :expected expected
     :library #(->> (search/best-first neighbors [start]
                                       {:decode-edge decode-edge})
                    (filter (fn [visit] (= goal (:state visit))))
                    first
                    :distance)
     :direct #(direct-weighted-grid-distance neighbors start goal)}))

(defn- manhattan-distance [[x y] [goal-x goal-y]]
  (+ (Math/abs (long (- goal-x x)))
     (Math/abs (long (- goal-y y)))))

(defn- heuristic-neighbors [side [x y]]
  (let [middle (quot side 2)]
    (keep (fn [[next-x next-y :as position]]
            (when (and (<= 0 next-x (dec side))
                       (<= 0 next-y (dec side))
                       (or (= next-y middle)
                           (zero? (mod next-x 4))))
              [position 1]))
          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn- a-star-workload [side]
  (let [middle (quot side 2)
        start [0 middle]
        goal [(dec side) middle]
        neighbors #(heuristic-neighbors side %)
        decode-edge (fn [[state weight]] {:state state :weight weight})
        expected (direct-weighted-grid-distance neighbors start goal)]
    {:name (str "a-star-comb-graph-" side)
     :label (format "A* comb graph (%d spine states)" side)
     :scale (format "%d spine states" side)
     :expected expected
     :library #(->> (search/best-first
                     neighbors [start]
                     {:decode-edge decode-edge
                      :extensions [search/distance-extension]
                      :priority (fn [{:keys [state distance]}]
                                  (+ distance (manhattan-distance state goal)))})
                    (filter (fn [visit] (= goal (:state visit))))
                    first
                    :distance)
     :direct #(direct-priority-distance
               neighbors start goal (fn [state] (manhattan-distance state goal)))}))

(def ^:private dependency-fold
  {:init (fn [{:keys [state]}] #{state})
   :step (fn [included child] (into included (:value child)))})

(defn- dependency-workload [size]
  (let [space (fn [task]
                (cond-> []
                  (pos? task) (conj (dec task))
                  (> task 1) (conj (- task 2))))
        direct (fn []
                 (into {}
                       (map-indexed (fn [task included]
                                      [task (count included)]))
                       (loop [task 0, values []]
                         (if (> task size)
                           values
                           (let [included
                                 (persistent!
                                  (reduce (fn [included dependency]
                                            (reduce conj! included
                                                    (nth values dependency)))
                                          (transient #{task})
                                          (space task)))]
                             (recur (inc task) (conj values included)))))))
        expected (into {} (map (fn [task] [task (inc task)])
                               (range (inc size))))]
    {:name (str "dependency-closure-" size)
     :label (format "Prerequisite closure, shared-dependency DAG (%d tasks)" (inc size))
     :scale (format "%d tasks" (inc size))
     :expected expected
     :library #(into {}
                     (map (fn [{:keys [state value]}] [state (count value)]))
                     (search/postorder space [size] {:fold dependency-fold}))
     :direct direct}))

(defn- direct-propagate [space initial]
  (let [initial-queue (into clojure.lang.PersistentQueue/EMPTY (keys initial))]
    (loop [values initial
           pending initial-queue
           queued (set initial-queue)]
      (if (empty? pending)
        values
        (let [state (peek pending)
              pending (pop pending)
              queued (disj queued state)
              contribution (get values state)
              [values pending queued]
              (reduce (fn [[values pending queued] destination]
                        (let [previous (get values destination #{})
                              value (into previous contribution)]
                          (if (= previous value)
                            [values pending queued]
                            [(assoc values destination value)
                             (if (contains? queued destination)
                               pending
                               (conj pending destination))
                             (conj queued destination)])))
                      [values pending queued]
                      (space state))]
          (recur values pending queued))))))

(defn- propagation-workload [size]
  (let [graph (into {} (map (fn [state] [state [(mod (inc state) size)]])
                            (range size)))
        initial (into {} (map (fn [state] [state #{state}]) (range size)))
        expected (zipmap (range size) (repeat (set (range size))))]
    {:name (str "fixed-point-propagation-" size)
     :label (format "Fixed-point propagation, cyclic ring (%d states)" size)
     :scale (format "%d states" size)
     :expected expected
     :library #(-> (search/propagate
                    (fn [{:keys [state value]}]
                      (when-let [destination (first (graph state))]
                        [{:state destination :value value}]))
                    initial
                    {:project identity :decode-edge identity
                     :bottom #{} :join set/union})
                   last
                   :values)
     :direct #(direct-propagate graph initial)}))

(defn- diamond-successors [state goal]
  (cond
    (= state goal) []
    (zero? (mod state 3)) [(inc state) (+ state 2)]
    (= 1 (mod state 3)) [(+ state 2)]
    :else [(inc state)]))

(defn- direct-diamond-paths [depth]
  (let [goal (* 3 depth)]
    (letfn [(paths-from [state path]
              (if (= state goal)
                [path]
                (mapcat (fn [successor]
                          (paths-from successor (conj path successor)))
                        (diamond-successors state goal))))]
      (into [] (paths-from 0 [0])))))

(defn- simple-paths-workload [depth]
  (let [goal (* 3 depth)
        space #(diamond-successors % goal)
        direct #(direct-diamond-paths depth)
        expected (direct)]
    {:name (str "simple-paths-diamond-" depth)
     :label (format "Simple routes through shared diamonds (%d stages, %d routes)"
                    depth (bit-shift-left 1 depth))
     :scale (format "%d stages / %d routes"
                    depth (bit-shift-left 1 depth))
     :expected expected
     :library #(->> (search/simple-paths space [0]
                                         {:extensions [search/path-extension]})
                    (filter (fn [{:keys [state]}] (= goal state)))
                    (mapv :path))
     :direct direct}))

(defn- control-flow-successors [size state]
  (cond-> []
    (< state (dec size)) (conj (inc state))
    (and (< state (dec size))
         (zero? (mod state 3)))
    (conj (min (dec size) (+ state 2)))
    (= state (dec size)) (conj (quot size 2))))

(defn- control-flow-facts [state]
  (if (zero? (mod state 8))
    #{(mod (quot state 8) 8)}
    #{}))

(defn- direct-control-flow [size]
  (loop [values {0 #{}}
         pending (conj clojure.lang.PersistentQueue/EMPTY 0)
         queued #{0}]
    (if (empty? pending)
      values
      (let [state (peek pending)
            pending (pop pending)
            queued (disj queued state)
            contribution (into (get values state) (control-flow-facts state))
            [values pending queued]
            (reduce (fn [[values pending queued] destination]
                      (let [previous (get values destination #{})
                            value (into previous contribution)]
                        (if (= previous value)
                          [values pending queued]
                          [(assoc values destination value)
                           (if (contains? queued destination)
                             pending
                             (conj pending destination))
                           (conj queued destination)])))
                    [values pending queued]
                    (control-flow-successors size state))]
        (recur values pending queued)))))

(defn- control-flow-workload [size]
  (let [space (fn [{:keys [state value]}]
                (let [contribution (into value (control-flow-facts state))]
                  (map (fn [destination]
                         {:state destination :value contribution})
                       (control-flow-successors size state))))
        direct #(direct-control-flow size)
        expected (direct)]
    {:name (str "control-flow-dataflow-" size)
     :label (format "Control-flow dataflow, branches and loop (%d blocks)" size)
     :scale (format "%d blocks" size)
     :expected expected
     :library #(-> (search/propagate
                    space {0 #{}}
                    {:project identity :decode-edge identity
                     :bottom #{} :join set/union})
                   last
                   :values)
     :direct direct}))

(def ^:private workloads
  [{:label "Depth-first reachability with pruning"
    :cases (mapv dfs-workload [2048 8192 32768 131072 393216])}
   {:label "Binary-tree traversal: tree-seq versus dfs without deduplication"
    :cases (mapv tree-seq-workload [10 12 14 16 18])}
   {:label "Grid shortest path"
    :cases (mapv grid-workload [32 64 128 256 512])}
   {:label "Weighted grid least-cost route"
    :cases (mapv weighted-grid-workload [32 64 128 256 448])}
   {:label "A* on comb graph versus direct A*"
    :cases (mapv a-star-workload [128 256 512 768 1024])}
   {:label "Prerequisite closure on shared-dependency DAG"
    :cases (mapv dependency-workload [256 512 1024 2048 3072])}
   {:label "Fixed-point propagation on cyclic ring"
    :cases (mapv propagation-workload [32 64 128 192 320])}
   {:label "Simple-route enumeration through diamonds"
    :cases (mapv simple-paths-workload [9 11 13 15 17])}
   {:label "Control-flow dataflow with branches and loop"
    :cases (mapv control-flow-workload [16384 32768 65536 131072 262144])}])

(def ^:private allocation-bean
  (let [bean (ManagementFactory/getThreadMXBean)]
    (when-not (instance? ThreadMXBean bean)
      (throw (ex-info "Thread allocation measurements are unavailable." {})))
    (let [bean (cast ThreadMXBean bean)]
      (when-not (.isThreadAllocatedMemorySupported bean)
        (throw (ex-info "Thread allocation measurements are unsupported." {})))
      (when-not (.isThreadAllocatedMemoryEnabled bean)
        (.setThreadAllocatedMemoryEnabled bean true))
      bean)))

(defn- allocated-bytes []
  (.getThreadAllocatedBytes allocation-bean
                            (.getId (Thread/currentThread))))

(defn- median [values]
  (let [sorted (vec (sort values))
        middle (quot (count sorted) 2)]
    (if (odd? (count sorted))
      (nth sorted middle)
      (/ (+ (nth sorted (dec middle)) (nth sorted middle)) 2.0))))

(defn- format-line [pattern & values]
  (String/format Locale/ROOT pattern (to-array values)))

(defn- measure [run]
  (let [before-bytes (allocated-bytes)
        start (System/nanoTime)]
    (run)
    {:nanos (- (System/nanoTime) start)
     :bytes (- (allocated-bytes) before-bytes)}))

(defn- run-workload [{:keys [name expected library direct scale]} warmup samples]
  (doseq [[implementation run] [[:library library] [:direct direct]]]
    (let [actual (run)]
      (when-not (= expected actual)
        (throw (ex-info "Comparison implementations returned different results."
                        {:workload name :implementation implementation
                         :expected expected :actual actual})))))
  (dotimes [_ warmup]
    (library)
    (direct))
  (let [[library-results direct-results]
        (reduce (fn [[library-results direct-results] sample]
                  (let [first-run (if (even? sample) library direct)
                        second-run (if (even? sample) direct library)
                        first-result (measure first-run)
                        second-result (measure second-run)
                        [library-result direct-result]
                        (if (even? sample)
                          [first-result second-result]
                          [second-result first-result])]
                    [(conj library-results library-result)
                     (conj direct-results direct-result)]))
                [[] []]
                (range samples))
        library-ms (/ (median (map :nanos library-results)) 1e6)
        direct-ms (/ (median (map :nanos direct-results)) 1e6)
        library-mib (/ (median (map :bytes library-results)) 1048576.0)
        direct-mib (/ (median (map :bytes direct-results)) 1048576.0)]
    {:scale scale
     :library-ms library-ms
     :direct-ms direct-ms
     :library-mib library-mib
     :direct-mib direct-mib
     :time-ratio (/ library-ms direct-ms)
     :allocation-ratio (/ library-mib direct-mib)}))

(defn- run-workload-group [{:keys [label cases]} warmup samples]
  (let [results (mapv #(run-workload % warmup samples) cases)]
    (println)
    (println (format-line "%s (%d scales, median of %d samples)"
                          label (count cases) samples))
    (println (format-line "%-28s %13s %9s %13s %9s %9s %9s"
                          "Scale" "library ms" "MiB" "direct ms" "MiB"
                          "time x" "alloc x"))
    (doseq [{:keys [scale library-ms direct-ms library-mib direct-mib
                    time-ratio allocation-ratio]} results]
      (println (format-line "%-28s %13.2f %9.2f %13.2f %9.2f %9.2f %9.2f"
                            scale library-ms library-mib direct-ms direct-mib
                            time-ratio allocation-ratio)))))

(defn- select-workloads [workload-name]
  (if workload-name
    (let [selected (into []
                         (keep (fn [{:keys [cases] :as workload}]
                                 (let [cases (filterv #(= workload-name (:name %)) cases)]
                                   (when (seq cases)
                                     (assoc workload :cases cases)))))
                         workloads)]
      (when (empty? selected)
        (throw (ex-info (str "Unknown benchmark workload: " workload-name)
                        {:available (mapv :name (mapcat :cases workloads))})))
      selected)
    workloads))

(defn- parse-options [args]
  (loop [args args, options {:warmup 5 :samples 10 :workload nil}]
    (if-let [argument (first args)]
      (case argument
        "--warmup" (if-let [value (second args)]
                     (recur (nnext args) (assoc options :warmup (Long/parseLong value)))
                     (throw (ex-info "--warmup requires a number." {})))
        "--samples" (if-let [value (second args)]
                      (recur (nnext args) (assoc options :samples (Long/parseLong value)))
                      (throw (ex-info "--samples requires a number." {})))
        "--workload" (if-let [value (second args)]
                       (recur (nnext args) (assoc options :workload value))
                       (throw (ex-info "--workload requires a name." {})))
        "--help" (assoc options :help? true)
        (throw (ex-info (str "Unknown benchmark option: " argument) {})))
      (do
        (doseq [key [:warmup :samples]]
          (when-not (pos? (get options key))
            (throw (ex-info (str "--" (name key) " must be positive.")
                            {key (get options key)}))))
        options))))

(defn -main [& args]
  (let [{:keys [help? warmup samples workload]} (parse-options args)]
    (if help?
      (println "Run direct comparisons with clojure -M:standard-bench [--warmup N] [--samples N] [--workload NAME].")
      (do
        (println (format-line
                  "State-space vs direct Clojure comparisons (%d warmups, %d samples)"
                  warmup samples))
        (doseq [workload (select-workloads workload)]
          (run-workload-group workload warmup samples))))))
