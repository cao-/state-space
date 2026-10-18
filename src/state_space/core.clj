(ns state-space.core
  (:require [state-space.internal.frontier :as frontier]))

(defn- initialize-visit [extensions visit]
  (reduce (fn [visit {:keys [start]}]
            (if start (reduce-kv assoc visit (start visit)) visit))
          visit
          extensions))

(defn- extend-visit [extensions parent transition visit]
  (reduce (fn [visit {:keys [next]}]
            (if next (reduce-kv assoc visit (next parent transition visit)) visit))
          visit
          extensions))

(defn- successor-visit [extensions parent transition]
  (extend-visit extensions parent transition {:state (:state transition)}))

(defn- finish-visit
  "Bottom-up counterpart of extend-visit: extensions with an :exit callback
  add values computed from the visit and its already finished successors."
  [extensions visit children]
  (reduce (fn [visit extension]
            (if-let [exit (:exit extension)]
              (reduce-kv assoc visit (exit visit children))
              visit))
          visit
          extensions))

(defn accumulate
  "Builds an extension that carries one value along every route under key k.

  init is called with the start visit and returns the value at the start;
  step is called with the parent's value and the decoded transition and
  returns the value of the successor. For example, depth is
  (accumulate :depth (constantly 0) (fn [depth _] (inc depth)))."
  [k init step]
  {:start (fn [visit] {k (init visit)})
   :next (fn [parent transition _visit]
           {k (step (get parent k) transition)})})

(def path-extension
  "An extension that adds the root-to-current state vector as :path context."
  (accumulate :path
              (fn [{:keys [state]}] [state])
              (fn [path {:keys [state]}] (conj path state))))

(def depth-extension
  "An extension that adds the number of transitions from the start as :depth."
  (accumulate :depth (constantly 0) (fn [depth _] (inc depth))))

(def trail-extension
  "An extension that adds :trail, the decoded transition maps from the root to
  the current visit. Unlike :path it keeps edge data such as labels and
  weights; the start is represented as {:state start}."
  (accumulate :trail
              (fn [{:keys [state]}] [{:state state}])
              conj))

(def distance-extension
  "An extension that adds cumulative transition weight (the path cost,
  default one) as :distance."
  (accumulate :distance
              (constantly 0)
              (fn [distance transition] (+ distance (get transition :weight 1)))))

(defn- with-required
  "Prepends extensions an algorithm needs, dropping duplicates the user passed."
  [options & required]
  (update options :extensions
          (fn [extensions]
            (into (vec required) (remove (set required)) extensions))))

(defn- successors-of
  "Raw successors of a visit; a nil result from the space is a leaf, and so
  is any visit that :expand? rejects."
  [space {:keys [project expand?]} visit]
  (when (expand? visit)
    (space (project visit))))

(defn- default-decode-edge [successor]
  {:state successor})

(defn- search-options [options]
  {:project (get options :project :state)
   :decode-edge (get options :decode-edge default-decode-edge)
   :extensions (vec (get options :extensions []))
   :visit-key (get options :visit-key :state)
   :expand? (get options :expand? (constantly true))})

(defn- map-lazy
  "Unlike chunked map, invokes f only for the requested item."
  [f items]
  (lazy-seq
   (when-let [items (seq items)]
     (cons (f (first items))
           (map-lazy f (rest items))))))

(defn- start-visits [{:keys [extensions]} starts]
  (map-lazy #(initialize-visit extensions {:state %}) starts))

(defn- explore
  "Shares lazy traversal mechanics; frontier operations define visit order.
  An optional refine function maps (parent candidate) to a candidate or nil."
  [space {:keys [decode-edge extensions visit-key] :as opts} refine
   visited pending pop-item push-chunk]
  (lazy-seq
   (loop [visited visited
          pending pending]
     (when-let [{:keys [item frontier]} (pop-item pending)]
       (let [visit item
             key (when visit-key (visit-key visit))]
         (if (and visit-key (contains? visited key))
           (recur visited frontier)
           (let [next-visited (when visit-key (conj visited key))]
             (cons visit
                   (let [children
                         (lazy-seq
                          (keep (fn [successor]
                                  (let [transition (decode-edge successor)
                                        candidate (successor-visit
                                                   extensions visit transition)]
                                    (if refine
                                      (refine visit candidate)
                                      candidate)))
                                (successors-of space opts visit)))]
                     (explore space opts refine
                              next-visited
                              (push-chunk frontier children)
                              pop-item
                              push-chunk))))))))))

(defn- depth-first [space starts options refine]
  (let [{:keys [visit-key] :as opts} options]
    (explore space opts refine
             (when visit-key #{})
             (frontier/stack-push (frontier/stack) (start-visits opts starts))
             frontier/stack-pop
             frontier/stack-push)))

(defn dfs
  "Lazily depth-first explores from an ordered collection of starting states.

  Space is an invokable value called with the result of :project, which
  defaults to :state. Use :project identity for a function of the full visit
  map containing :state and any values supplied by :extensions.
  Maps and state-only functions work directly. Nil successor results are
  treated as leaves. Visits sharing the selected :visit-key are reported and
  expanded only on their first arrival. Each yielded visit contains :state.

  Options may contain :project and :visit-key, both functions of the full visit
  map. :visit-key defaults to :state; set it to nil to disable deduplication.
  :decode-edge optionally maps each raw successor to a transition map
  containing :state; DFS uses that destination. Ordered :extensions add
  derived visit values and can inspect all decoded transition fields.
  Extensions are maps with :start and :next callbacks returning visit-context
  maps. Callbacks must preserve :state. Custom context fields merge in
  extension order, with later fields replacing earlier ones; use namespaced
  keys for independently reusable extensions to avoid accidental collisions.
  Postorder reserves :processed-successors, :via, and :back-edge for its callback views.
  :start and :next prepare candidates before duplicate or path-repeat
  rejection, so they may run for visits that are never emitted. Prefer pure
  callbacks; use result consumption for actions on emitted visits.
  Inputs are the caller's responsibility and are not validated; violating
  these expectations may produce unexpected results."
  ([space starts]
   (dfs space starts {}))
  ([space starts options]
   (depth-first space starts
                (search-options options)
                nil)))

(defn- reject-repeated-keys
  "Refinement for simple paths: a candidate whose identity already occurs on
  its path is dropped. Path identities are hidden visit metadata, so they are
  neither exposed nor part of visit equality."
  [visit-key]
  (fn [parent candidate]
    (let [on-path (or (::path-keys (meta parent)) #{(visit-key parent)})
          key (visit-key candidate)]
      (when-not (contains? on-path key)
        (vary-meta candidate assoc ::path-keys (conj on-path key))))))

(defn simple-paths
  "Lazily enumerates every visit reached by a simple path, depth-first.

  Behaves like dfs without global deduplication, but never extends a path to
  a node whose :visit-key already occurs on that path, so paths may share
  nodes yet never revisit one. :visit-key defaults to :state; nil disables
  the check. Like dfs it only defines how the graph is traversed: which
  values appear in each visit is chosen with :extensions, e.g. path-extension
  for the route of states or trail-extension for the route with edge data.
  Visits are yielded for every path prefix; select goals with ordinary
  sequence functions, and return nil from the space to end a path at a goal.
  Enumeration is depth-first, so infinite searches need not be fair."
  ([space starts]
   (simple-paths space starts {}))
  ([space starts options]
   (let [{:keys [visit-key] :as opts} (search-options options)]
     (depth-first space starts (assoc opts :visit-key nil)
                  (when visit-key
                    (reject-repeated-keys visit-key))))))

(defn bfs
  "Lazily breadth-first explores from an ordered collection of starting states.

  Space and options are the same as in dfs. In a static space, each equal
  state is reported once at its minimum edge-count depth. Add depth-extension
  to include edge-count depth. Successor generation is deferred until its
  frontier chunk is needed. An infinite current level can produce visits
  indefinitely but prevents deeper levels from being reached."
  ([space starts]
   (bfs space starts {}))
  ([space starts options]
   (let [{:keys [visit-key] :as opts} (search-options options)]
     (explore space opts nil
              (when visit-key #{})
              (frontier/queue-push (frontier/queue) (start-visits opts starts))
              frontier/queue-pop
              frontier/queue-push))))

(defn postorder
  "Lazily returns visits in depth-first post-order.

  Each visit is returned after all of its successors, so when edges point
  from a task to its prerequisites, prerequisites appear before the task.
  A node is marked known when first entered, so cyclic graphs terminate: a
  back edge is simply ignored, and the result is then the plain DFS
  post-order (a valid topological order exactly when the reachable graph is
  a DAG). Options are as in dfs, but :visit-key must not be nil. Its result
  must remain stable from entry through completion: :exit callbacks must not
  change fields in a way that changes the visit key.

  Extensions may also define :exit, a function of a visit and the visits of
  its successors returning a map merged into the visit as it is emitted: a
  bottom-up computation over the successors' own results. Each successor
  appears as the visit that first reached that node: finished, with its :exit
  values, or, for a back edge to a node still in progress, with top-down
  values only. Equal visit keys must identify interchangeable computations:
  include relevant route context in the key when expansion, :continue? pruning,
  or computed results depend on it. A cached result from one budget or
  pruning window is not automatically valid under another; later arrivals
  do not recompute it. These are caller expectations, not runtime checks.
  Successors are reported in the order the space produced them,
  and each carries two keys describing the edge it was reached by in this
  particular parent: :via, the decoded transition (so edge weights and labels
  are available even when the node was first reached through another edge),
  and :back-edge, true when the successor was still in progress. Extensions
  must not define :processed-successors, :via, or :back-edge as their own context fields;
  these keys are reserved for postorder's callback views, not guaranteed
  fields on emitted visits. These expectations are not checked at runtime.

  Successors are consumed and decoded one at a time. The :next callback
  receives the parent with :processed-successors, a vector of successor
  views handled so far, including back edges (in the same form as in :exit).
  An extension's :continue? callback, a function of the visit and that vector,
  is checked before consuming and decoding the next successor,
  including the first. Rejecting the first successor also avoids calling
  :expand?, :project, and the space for that visit.
  If any callback returns false, no next successor is
  decoded or visited and the visit is finished with what it has. A chunked
  successor collection may realize more raw values when one is consumed. The
  check also happens once when a visit has no successors, so callbacks should
  not assume another successor exists. Together these hooks let information
  flow from earlier siblings to later ones, as in alpha-beta pruning or
  short-circuit evaluation."
  ([space starts]
   (postorder space starts {}))
  ([space starts options]
   (let [{:keys [decode-edge extensions visit-key] :as opts} (search-options options)
         continue? (keep :continue? extensions)]
     (letfn [(finish [known done pending visit processed]
               (let [finished (finish-visit extensions visit processed)
                     key (visit-key visit)]
                 (cons finished
                       (walk (assoc known key finished)
                             (conj done key)
                             pending))))
             (continue [known done pending visit processed remaining awaiting]
               (let [processed (if awaiting
                                (let [[key transition] awaiting]
                                  (conj processed
                                        (assoc (known key)
                                               :via transition
                                               :back-edge (not (contains? done key)))))
                                processed)]
                 (if-let [remaining (when (every? #(% visit processed) continue?)
                                      (seq remaining))]
                   (let [transition (decode-edge (first remaining))
                         child (successor-visit extensions
                                                (assoc visit :processed-successors processed)
                                                transition)
                         resume-parent {:kind :continue
                                        :visit visit
                                        :processed-successors processed
                                        :remaining (rest remaining)
                                        :awaiting [(visit-key child) transition]}]
                     (walk known done
                           (cons {:kind :enter :visit child}
                                 (cons resume-parent pending))))
                   (finish known done pending visit processed))))
             (enter [known done pending visit]
               (let [key (visit-key visit)]
                 (if (contains? known key)
                   (walk known done pending)
                   (let [resume-visit {:kind :continue
                                      :visit visit
                                      :processed-successors []
                                      :remaining
                                      (lazy-seq
                                       (successors-of space opts visit))}]
                     (walk (assoc known key visit)
                           done
                           (cons resume-visit pending))))))
             (walk [known done pending]
               (lazy-seq
                (when-let [pending (seq pending)]
                  (let [item (first pending)
                        pending (rest pending)]
                    (case (:kind item)
                      :enter
                      (enter known done pending (:visit item))

                      :continue
                      (continue known done pending
                                (:visit item)
                                (:processed-successors item)
                                (:remaining item)
                                (:awaiting item)))))))]
       (walk {} #{}
             (map-lazy (fn [state]
                         {:kind :enter
                          :visit (initialize-visit extensions {:state state})})
                       starts))))))

(defn propagate
  "Lazily propagates and joins values through a state space.

  initial-values maps starting states to their initial values; map keys are
  state identities. The space is called with :project (default :state); use
  :project identity to let its transitions depend on the full visit
  {:state state :value value}. Each decoded transition must contain :state and
  may contain :value, the contribution to join into that state's accumulated
  value. If :value is absent, the current value is propagated unchanged.
  Required option :join combines the current value with a contribution, and
  required option :bottom supplies the value for a state with no prior
  contribution. :bottom should be an identity for :join. :expand? can suppress
  expansion of a visit.

  The first result is {:values initial-values :changes []}; it has no
  :processed key because no state has been processed yet. Each later record
  contains the updated :values map, its :processed state, and :changes, a
  vector of {:state state :value value} maps recording destination updates in
  transition order, including newly discovered states even at :bottom.
  A destination may occur more than once, with its accumulated value after
  each update; contributions leaving a known value unchanged add no entry.
  The :values map contains the values after all transitions for that record.
  Initial worklist construction is deferred until a processing record is
  requested.
  States whose values change are scheduled for processing
  again; a pending state is queued only once. The result is finite exactly
  when the worklist becomes empty. Exhaust it or take a prefix to observe
  progress; (last ...) returns the final values if propagation terminates.

  For order-independent results, :join should be associative, commutative,
  and idempotent, and the contribution-generating behavior should be monotone
  in the accumulated information. Termination depends on the state space and
  whether values stabilize. Every expanded state must produce finitely many
  transitions. These are caller assumptions, not runtime checks. Input map
  iteration and successor order determine the order of intermediate results."
  [space initial-values {:keys [bottom join] :as options}]
  (let [opts {:project (get options :project :state)
              :decode-edge (get options :decode-edge default-decode-edge)
              :expand? (get options :expand? (constantly true))}]
    (letfn [(run [values queue pending]
              (lazy-seq
               (when (seq queue)
                 (let [state (peek queue)
                       queue (pop queue)
                       pending (disj pending state)
                       visit {:state state :value (get values state)}
                       result
                       (reduce
                        (fn [{:keys [values queue pending changes] :as result}
                             successor]
                          (let [transition ((:decode-edge opts) successor)
                                state (:state transition)
                                contribution (if (contains? transition :value)
                                               (:value transition)
                                               (:value visit))
                                previous (get values state bottom)
                                value (join previous contribution)]
                            (if (and (= previous value) (contains? values state))
                              result
                              {:values (assoc values state value)
                               :queue (if (contains? pending state)
                                        queue
                                        (conj queue state))
                               :pending (conj pending state)
                               :changes (conj changes
                                              {:state state :value value})})))
                        {:values values
                         :queue queue
                         :pending pending
                         :changes []}
                        (successors-of space opts visit))]
                   (cons {:values (:values result)
                          :processed state
                          :changes (:changes result)}
                         (run (:values result)
                              (:queue result)
                              (:pending result)))))))]
      (cons {:values initial-values
             :changes []}
            (lazy-seq
             (let [initial-queue (into clojure.lang.PersistentQueue/EMPTY
                                       (keys initial-values))]
               (run initial-values initial-queue (set initial-queue))))))))

(defn- enqueue
  "Queues a visit unless its key is settled or already has a priority at
  least as good."
  [{:keys [visit-key priority]} visit settled [queued queue order :as state]]
  (let [key (visit-key visit)
        p (priority visit)
        previous (get queued key)]
    (if (or (contains? settled key)
            (and previous
                 (not (neg? (compare p (first previous))))))
      state
      (let [queue-key [p order]]
        [(assoc queued key queue-key)
         (assoc (if previous (dissoc queue previous) queue) queue-key visit)
         (inc order)]))))

(defn- relax
  "Queues improved candidates for the successors of a settled visit."
  [space {:keys [decode-edge extensions] :as opts} visit settled state]
  (reduce
   (fn [state successor]
     (let [transition (decode-edge successor)]
       (enqueue opts
                (successor-visit extensions visit transition)
                settled
                state)))
   state
   (successors-of space opts visit)))

(defn best-first
  "Lazily selects the lowest-priority pending visit, best-first.

  By default the priority is :distance, giving Dijkstra's shortest-path
  order. This default automatically enables distance-extension. Each
  successor is reached at unit cost unless :decode-edge decodes attributed
  successors to transition maps containing :state and an optional :weight.
  Weights must be finite and non-negative for Dijkstra's guarantees. Options
  are as in dfs, plus :priority, a function of the visit map (default
  :distance) that orders the search, e.g. distance plus a heuristic for A*,
  or an extension-provided edge weight for Prim's algorithm. Each visit key is
  settled at most once, at the best priority offered for that key before
  settlement, so the priority must suit the problem (e.g. a consistent
  heuristic for A*); that is the caller's responsibility. Newly discovered
  candidates may have lower priorities than previously emitted visits, so
  custom priorities do not generally produce globally sorted results.
  A custom priority does not enable distance-extension; include it explicitly
  when the priority or results need cumulative distance. Equal priorities are emitted FIFO in
  enqueue order; this order depends on the iteration order of starts and
  successors. On infinite spaces, finite non-negative weights alone do not
  guarantee that a positive-cost result will eventually be emitted: an
  infinite region of equal or lower priority can keep it pending. Standard A*
  guarantees require additional assumptions, including bounded branching
  and edge costs bounded below by a positive value. :project and :visit-key
  receive a visit map containing :state and extension values, and :visit-key
  must not be nil.
  Starts and each expanded state's transitions must be finite. Each result
  contains :state and extension values. Starting states are consumed and
  initialized only when the result is consumed; all starts are queued before
  the first visit is selected."
  ([space starts]
   (best-first space starts {}))
  ([space starts options]
   (let [priority (get options :priority :distance)
         options (if (= :distance priority)
                   (with-required options distance-extension)
                   options)
         {:keys [extensions visit-key] :as opts}
         (assoc (search-options options) :priority priority)]
     (letfn [(search [queued settled queue order]
               (lazy-seq
                (when (seq queue)
                  (let [[queue-key visit] (first queue)
                        queue (dissoc queue queue-key)
                        key (visit-key visit)
                        queued (dissoc queued key)
                        settled (conj settled key)]
                    (cons visit
                          (lazy-seq
                           (let [[queued queue order]
                                 (relax space opts visit settled
                                        [queued queue order])]
                             (search queued settled queue order))))))))]
       (lazy-seq
        (let [[queued queue order]
              (reduce (fn [state start]
                        (enqueue opts (initialize-visit extensions {:state start})
                                 #{} state))
                      [{} (sorted-map) 0]
                      starts)]
          (search queued #{} queue order)))))))
