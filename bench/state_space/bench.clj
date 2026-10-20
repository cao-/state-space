;; Usage:
;;   clojure -M:bench
;;   clojure -M:bench --compare [--baseline origin/main]
;; Options: --runs N --warmup N --samples N --workload NAME
;; Workloads: dfs-enter dfs-exit dfs-events bfs-enter bfs-events best-first
;;            postorder-fold simple-paths propagate

(ns state-space.bench
  "Benchmark public APIs, or compare the working tree with a Git ref."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [state-space.core :as search])
  (:import [com.sun.management ThreadMXBean]
           [java.io File]
           [java.lang.management ManagementFactory]
           [java.nio.file CopyOption Files Path StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util Locale]))

(def ^:private node-count 32767)
(def ^:private path-node-count 19)

(def ^:private graph
  (into {}
        (map (fn [node]
               [node (cond-> []
                       (< (inc (* 2 node)) node-count) (conj (inc (* 2 node)))
                       (< (+ 2 (* 2 node)) node-count) (conj (+ 2 (* 2 node))))])
             (range node-count))))

(def ^:private weighted-graph
  (into {}
        (map (fn [[node successors]]
               [node (mapv #(vector % 1) successors)])
             graph)))

(def ^:private path-graph
  (into {}
        (map (fn [node]
               [node (range (inc node) path-node-count)])
             (range path-node-count))))

(def ^:private tree-fold
  {:init (constantly 1)
   :step (fn [size child] (+ size (:value child)))})

(def ^:private workloads
  [{:name "dfs-enter" :label "DFS :enter, balanced tree"
    :expected node-count
    :run #(count (search/dfs graph [0]))}
   {:name "dfs-exit" :label "DFS :exit, balanced tree"
    :expected node-count
    :run #(count (search/dfs graph [0] {:emit :exit}))}
   {:name "dfs-events" :label "DFS :events, balanced tree"
    :expected (* 2 node-count)
    :run #(count (search/dfs graph [0] {:emit :events}))}
   {:name "bfs-enter" :label "BFS :enter, balanced tree"
    :expected node-count
    :run #(count (search/bfs graph [0]))}
   {:name "bfs-events" :label "BFS :events, balanced tree"
    :expected (* 2 node-count)
    :run #(count (search/bfs graph [0] {:emit :events}))}
   {:name "best-first" :label "Best-first, weighted balanced tree"
    :expected node-count
    :run #(count (search/best-first
                  weighted-graph [0]
                  {:decode-edge (fn [[state weight]]
                                  {:state state :weight weight})}))}
   {:name "postorder-fold" :label "Postorder fold, balanced tree"
    :expected node-count
    :run #(count (search/postorder graph [0] {:fold tree-fold}))}
   {:name "simple-paths" :label "Simple paths, dense DAG (19 states)"
    :expected (bit-shift-left 1 (dec path-node-count))
    :run #(count (search/simple-paths path-graph [0]))}
   {:name "propagate" :label "Propagate, balanced tree"
    :expected node-count
    :run #(count (:values
                  (last (search/propagate graph {0 1}
                                          {:bottom 0 :join max}))))}])

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

(defn- run-workload [{:keys [name expected run]}]
  (let [actual (run)]
    (when-not (= expected actual)
      (throw (ex-info "Benchmark workload returned an unexpected result."
                      {:workload name :expected expected :actual actual})))
    actual))

(defn- measure [workload]
  (let [before-bytes (allocated-bytes)
        start (System/nanoTime)]
    (run-workload workload)
    {:nanos (- (System/nanoTime) start)
     :bytes (- (allocated-bytes) before-bytes)}))

(defn- median [values]
  (let [sorted (vec (sort values))
        middle (quot (count sorted) 2)]
    (if (odd? (count sorted))
      (nth sorted middle)
      (/ (+ (nth sorted (dec middle)) (nth sorted middle)) 2.0))))

(defn- format-line [pattern & values]
  (String/format Locale/ROOT pattern (to-array values)))

(def ^:private color?*
  (delay (and (System/console)
              (not (System/getenv "NO_COLOR"))
              (not= "dumb" (System/getenv "TERM")))))

(defn- colorize [color text]
  (if @color?*
    (str "\u001b[" color "m" text "\u001b[0m")
    text))

(defn- change-color [change]
  (cond
    (neg? change) "32"
    (pos? change) "31"
    :else nil))

(defn- format-change [change]
  (let [text (format-line "%+.1f%%" change)]
    (if-let [color (change-color change)]
      (colorize color text)
      text)))

(defn- measure-workload [{:keys [name label] :as workload} {:keys [warmup samples]}]
  (dotimes [_ warmup]
    (run-workload workload))
  (let [measurements (repeatedly samples #(measure workload))
        result {:milliseconds (/ (median (map :nanos measurements)) 1e6)
                :mebibytes (/ (median (map :bytes measurements)) 1048576.0)}]
    (println (format-line "%s: %.2f ms/traversal; %.2f MiB allocated/traversal"
                          label (:milliseconds result) (:mebibytes result)))
    (println (format-line "RESULT\t%s\t%.6f\t%.6f"
                          name (:milliseconds result) (:mebibytes result)))
    result))

(defn- select-workloads [workload-name]
  (if workload-name
    (let [selected (filterv #(= workload-name (:name %)) workloads)]
      (when (empty? selected)
        (throw (ex-info (str "Unknown benchmark workload: " workload-name)
                        {:available (mapv :name workloads)})))
      selected)
    workloads))

(defn- run-benchmark [{:keys [warmup samples workload]}]
  (let [selected-workloads (select-workloads workload)]
    (println (format-line "Public API benchmarks (%d warmups, %d samples each)"
                          warmup samples))
    (into {}
          (map (fn [workload]
                 [(:name workload) (measure-workload workload {:warmup warmup
                                                                :samples samples})])
               selected-workloads))))

(defn- run-process [directory command]
  (let [builder (ProcessBuilder. ^java.util.List command)]
    (.directory builder (if (instance? Path directory)
                          (.toFile ^Path directory)
                          (io/file directory)))
    (.redirectErrorStream builder true)
    (let [process (.start builder)
          output (with-open [reader (io/reader (.getInputStream process))]
                   (slurp reader))
          exit-code (.waitFor process)]
      (when-not (zero? exit-code)
        (throw (ex-info (str "Command failed: " (str/join " " command))
                        {:command command
                         :directory (str directory)
                         :exit-code exit-code
                         :output output})))
      output)))

(defn- git! [repository & arguments]
  (run-process repository (into ["git"] arguments)))

(defn- parse-benchmark-output [output selected-workloads]
  (let [results (into {}
                      (map (fn [[_ name milliseconds mebibytes]]
                             [name {:milliseconds (Double/parseDouble milliseconds)
                                    :mebibytes (Double/parseDouble mebibytes)}])
                           (re-seq #"(?m)^RESULT\t([^\t]+)\t([0-9]+\.[0-9]+)\t([0-9]+\.[0-9]+)$"
                                   output)))]
    (when-not (= (set (map (comp name :name) selected-workloads))
                 (set (keys results)))
      (throw (ex-info "Could not parse all benchmark results."
                      {:expected (mapv :name selected-workloads)
                       :actual (keys results)
                       :output output})))
    results))

(defn- benchmark-process [directory harness-mode warmup samples workload]
  (let [common-arguments [(str "-J-Dbench.warmup=" warmup)
                          (str "-J-Dbench.samples=" samples)]
        arguments (case harness-mode
                    :working (into ["clojure"]
                                   (concat common-arguments
                                           ["-M:bench" "--workload" workload]))
                    :baseline (into ["clojure"] (concat common-arguments
                                                        ["-M" "-m" "state-space.bench"
                                                         "--workload" workload])))]
    (parse-benchmark-output (run-process directory arguments)
                            (select-workloads workload))))

(defn- median-field [results workload field]
  (median (map field (get results workload))))

(defn- report-workload [workload results runs warmup samples]
  (let [baseline-ms (median-field results :baseline :milliseconds)
        working-ms (median-field results :working :milliseconds)
        baseline-mib (median-field results :baseline :mebibytes)
        working-mib (median-field results :working :mebibytes)
        time-change (* 100.0 (/ (- working-ms baseline-ms) baseline-ms))
        allocation-change (* 100.0 (/ (- working-mib baseline-mib) baseline-mib))]
    (println)
    (println (format-line "%s (median of %d process runs; %d warmups, %d samples)"
                          (:label workload) runs warmup samples))
    (println (format-line "  baseline: %.2f ms, %.2f MiB" baseline-ms baseline-mib))
    (println (format-line "  working:  %.2f ms, %.2f MiB" working-ms working-mib))
    (println (str "  change:   " (format-change time-change) " time, "
                  (format-change allocation-change) " allocation"
                  (when-not @color?*
                    " (negative means improvement)")))))

(defn- compare-revisions [{:keys [baseline runs warmup samples workload]}]
  (let [repository (str/trim (git! "." "rev-parse" "--show-toplevel"))
        baseline-commit (str/trim (git! repository "rev-parse" "--verify"
                                         (str baseline "^{commit}")))
        temp-root (Files/createTempDirectory "state-space-bench-"
                                             (make-array FileAttribute 0))
        worktree (.resolve temp-root "baseline")
        worktree-added? (atom false)]
    (try
      (git! repository "worktree" "add" "--detach" (str worktree) baseline-commit)
      (reset! worktree-added? true)
      (let [current-harness
            (-> (File. repository
                       (str "bench" File/separator "state_space"
                            File/separator "bench.clj"))
                .toPath)
            baseline-harness (-> worktree
                                 (.resolve "src")
                                 (.resolve "state_space")
                                 (.resolve "bench.clj"))]
        (Files/copy current-harness baseline-harness
                    (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))
      (println (format-line "Benchmark comparison: %s vs working tree" baseline))
      (doseq [{:keys [name] :as workload} (select-workloads workload)]
        (let [results
              (loop [run 0, baseline-results [], working-results []]
                (if (< run runs)
                  (do
                    (println (format-line "%s: run %d/%d"
                                          (:label workload) (inc run) runs))
                    (if (even? run)
                      (recur (inc run)
                             (conj baseline-results
                                   (get (benchmark-process worktree :baseline
                                                           warmup samples name)
                                        name))
                             (conj working-results
                                   (get (benchmark-process repository :working
                                                           warmup samples name)
                                        name)))
                      (let [working-result
                            (get (benchmark-process repository :working
                                                    warmup samples name)
                                 name)
                            baseline-result
                            (get (benchmark-process worktree :baseline
                                                    warmup samples name)
                                 name)]
                        (recur (inc run)
                               (conj baseline-results baseline-result)
                               (conj working-results working-result)))))
                  {:baseline baseline-results
                   :working working-results}))]
          (report-workload workload results runs warmup samples)))
      (finally
        (when @worktree-added?
          (git! repository "worktree" "remove" "--force" (str worktree)))
        (Files/deleteIfExists temp-root)))))

(defn- parse-options [args]
  (loop [args args
         options {:mode :run
                  :baseline "origin/main"
                  :runs 3
                   :warmup (Long/parseLong (System/getProperty "bench.warmup" "10"))
                   :samples (Long/parseLong (System/getProperty "bench.samples" "15"))
                   :workload nil}]
    (if-let [argument (first args)]
      (case argument
        "--compare" (recur (rest args) (assoc options :mode :compare))
        "--baseline" (if-let [value (second args)]
                       (recur (nnext args) (assoc options :baseline value))
                       (throw (ex-info "--baseline requires a Git ref." {})))
        "--runs" (if-let [value (second args)]
                   (recur (nnext args) (assoc options :runs (Long/parseLong value)))
                   (throw (ex-info "--runs requires a number." {})))
        "--warmup" (if-let [value (second args)]
                     (recur (nnext args) (assoc options :warmup (Long/parseLong value)))
                     (throw (ex-info "--warmup requires a number." {})))
        "--samples" (if-let [value (second args)]
                      (recur (nnext args) (assoc options :samples (Long/parseLong value)))
                      (throw (ex-info "--samples requires a number." {})))
        "--workload" (if-let [value (second args)]
                       (recur (nnext args) (assoc options :workload value))
                       (throw (ex-info "--workload requires a workload name." {})))
        "--help" (assoc options :mode :help)
        (throw (ex-info (str "Unknown benchmark option: " argument) {})))
      (do
        (doseq [key [:runs :warmup :samples]]
          (when-not (pos? (get options key))
            (throw (ex-info (str "--" (name key) " must be positive.")
                            {key (get options key)}))))
        options))))

(defn- print-usage []
  (println "Run:       clojure -M:bench")
  (println "Compare:   clojure -M:bench --compare [--baseline origin/main]")
  (println "Options:   --runs N --warmup N --samples N")
  (println "Workloads: dfs-enter dfs-exit dfs-events bfs-enter bfs-events")
  (println "           best-first postorder-fold simple-paths propagate"))

(defn -main [& args]
  (let [{:keys [mode] :as options} (parse-options args)]
    (case mode
      :run (run-benchmark options)
      :compare (compare-revisions options)
      :help (print-usage))))
