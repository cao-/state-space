(ns state-space.core
  (:require [state-space.internal.frontier :as frontier]))

(defn- initialize-visit [start-callbacks visit]
  (reduce (fn [visit start]
            (reduce-kv assoc visit (start visit)))
          visit
          start-callbacks))

(defn- extend-visit [next-callbacks parent transition visit]
  (reduce (fn [visit next]
            (reduce-kv assoc visit (next parent transition visit)))
          visit
          next-callbacks))

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
  (let [extensions (vec (get options :extensions []))
        opts {:project (get options :project :state)
              :decode-edge (get options :decode-edge default-decode-edge)
              :visit-key (get options :visit-key :state)
              :expand? (get options :expand? (constantly true))}
        start-callbacks (into [] (keep :start) extensions)
        next-callbacks (into [] (keep :next) extensions)]
    (assoc opts
           :start-callbacks start-callbacks
           :prepare-visit (fn [parent transition]
                            (extend-visit next-callbacks parent transition
                                          {:state (:state transition)})))))

(defn- search-output [algorithm options allowed]
  (let [output (get options :emit :enter)]
    (when-not (some #{output} allowed)
      (throw (ex-info (str algorithm " supports :emit " (pr-str allowed) ".")
                      {:algorithm algorithm :option :emit :value output
                       :allowed allowed})))
    output))

(defn- check-fold [algorithm options output]
  (when (:fold options)
    (when (nil? (get options :visit-key :state))
      (throw (ex-info "Memoized folds require a non-nil :visit-key."
                      {:algorithm algorithm :option :visit-key})))
    (when (= output :enter)
      (throw (ex-info "A fold requires :emit :exit or :emit :events."
                      {:algorithm algorithm :option :emit :value output})))))

(defn- check-present-option [algorithm options option]
  (when-not (contains? options option)
    (throw (ex-info (str (name algorithm) " requires "
                         (pr-str option) " to be present.")
                    {:algorithm algorithm :option option}))))

(defn- check-non-nil-option [algorithm option value]
  (when (nil? value)
    (throw (ex-info (str (name algorithm) " requires a non-nil "
                         (pr-str option) ".")
                    {:algorithm algorithm :option option :value value}))))

(defn- map-lazy
  "Unlike chunked map, invokes f only for the requested item."
  [f items]
  (lazy-seq
   (when-let [items (seq items)]
     (cons (f (first items))
           (map-lazy f (rest items))))))

(defn- start-visits [{:keys [start-callbacks]} starts]
  (map-lazy #(initialize-visit start-callbacks {:state %}) starts))

(defn- explore
  "Preorder traversal over lazy frontier chunks; stack or queue defines order."
  [space {:keys [decode-edge prepare-visit visit-key] :as opts} refine
   visited pending pop-item push-chunk]
  (lazy-seq
   (loop [visited visited, pending pending]
     (when-let [[item frontier] (pop-item pending)]
       (let [visit item
             key (when visit-key (visit-key visit))]
         (if (and visit-key (contains? visited key))
           (recur visited frontier)
           (let [next-visited (when visit-key (conj visited key))]
             (cons visit
                   (let [children
                         (lazy-seq
                          (keep (fn [successor]
                                  (let [candidate (prepare-visit visit (decode-edge successor))]
                                    (if refine (refine visit candidate) candidate)))
                                (successors-of space opts visit)))]
                     (explore space opts refine
                              next-visited (push-chunk frontier children)
                              pop-item push-chunk))))))))))

(defn- breadth-first-events
  "BFS expansion markers live at the end of their lazy successor chunks."
  [space {:keys [decode-edge prepare-visit visit-key] :as opts} visited pending]
  (lazy-seq
   (loop [visited visited
          pending pending]
     (when-let [[item frontier] (frontier/queue-pop pending)]
       (let [[kind visit] item
             key (when (and visit-key (= kind :enter)) (visit-key visit))]
         (cond
           (= kind :expand)
           (cons (assoc visit :phase :expand)
                 (breadth-first-events space opts visited frontier))

           (and visit-key (contains? visited key))
           (recur visited frontier)

           :else
           (let [next-visited (when visit-key (conj visited key))]
             (cons (assoc visit :phase :enter)
                   (let [children
                         (lazy-seq
                          (concat
                           (map-lazy (fn [successor]
                                       [:enter (prepare-visit visit (decode-edge successor))])
                                     (successors-of space opts visit))
                           [[:expand visit]]))]
                     (breadth-first-events space opts
                                           next-visited
                                           (frontier/queue-push frontier children)))))))))))

(defn- depth-first-complete
  "DFS return tracking for exit/event output and optional bottom-up evaluation.
  Route visits stay phase-free; only emitted event snapshots carry :phase.

  The active visit is held in loop locals; suspended ancestors are stacked in
  frames as [visit key accumulator remaining-successors transition-to-child].
  Unconsumed starting states are kept apart and entered once frames are empty."
  [space starts {:keys [decode-edge prepare-visit visit-key] :as opts}
   refine output fold]
  (let [{:keys [init step descend] complete :finish
         :or {complete identity}} fold
        events? (= output :events)]
    (letfn [(remember [known key visit]
              (cond fold (assoc known key visit)
                    visit-key (conj known key)
                    :else known))
            (fold-child [accumulator child transition]
              (step accumulator
                    (-> child
                        (assoc :via transition)
                        (assoc :back-edge (not (contains? child :value))))))
            (open [known visit key frames starts]
              (walk known visit key
                    (when fold (init visit))
                    (lazy-seq (successors-of space opts visit))
                    frames starts))
            (enter [known visit key frames starts]
              (let [known (remember known key visit)]
                (if events?
                  (cons (assoc visit :phase :enter)
                        (lazy-seq (open known visit key frames starts)))
                  (open known visit key frames starts))))
            (next-start [known starts]
              (loop [starts starts]
                (when-let [starts (seq starts)]
                  (let [visit (initialize-visit (:start-callbacks opts)
                                                {:state (first starts)})
                        key (when visit-key (visit-key visit))]
                    (if (and visit-key (contains? known key))
                      (recur (rest starts))
                      (enter known visit key () (rest starts)))))))
            (resume [known finished frames starts]
              (if-let [[visit key accumulator remaining transition] (peek frames)]
                (walk known visit key
                      (if fold (fold-child accumulator finished transition) accumulator)
                      remaining (pop frames) starts)
                (next-start known starts)))
            (walk [known visit key accumulator remaining frames starts]
              (loop [known known, visit visit, key key
                     accumulator accumulator, remaining remaining, frames frames]
                (if-let [remaining (when-not (reduced? accumulator) (seq remaining))]
                  (let [transition (decode-edge (first remaining))
                        candidate (prepare-visit visit transition)
                        candidate (if descend
                                    (reduce-kv assoc candidate
                                               (descend accumulator transition candidate))
                                    candidate)
                        child (if refine (refine visit candidate) candidate)
                        child-key (when (and child visit-key) (visit-key child))]
                    (cond
                      (nil? child)
                      (recur known visit key accumulator (rest remaining) frames)

                      (and visit-key (contains? known child-key))
                      (recur known visit key
                             (if fold
                               (fold-child accumulator (known child-key) transition)
                               accumulator)
                             (rest remaining) frames)

                      :else
                      (let [frames (conj frames
                                         [visit key accumulator (rest remaining) transition])]
                        (if events?
                          (enter known child child-key frames starts)
                          (recur (remember known child-key child) child child-key
                                 (when fold (init child))
                                 (lazy-seq (successors-of space opts child))
                                 frames)))))
                  (let [finished (if fold
                                   (assoc visit :value (complete (unreduced accumulator)))
                                   visit)
                        known (if fold (assoc known key finished) known)]
                    (cons (if events? (assoc finished :phase :exit) finished)
                          (lazy-seq (resume known finished frames starts)))))))]
      (lazy-seq (next-start (if fold {} (when visit-key #{})) starts)))))

(defn- depth-first [space starts {:keys [visit-key] :as opts} refine output fold]
  (if (= output :enter)
    (explore space opts refine
             (when visit-key #{})
             (frontier/stack-push (frontier/stack) (start-visits opts starts))
             frontier/stack-pop frontier/stack-push)
    (depth-first-complete space starts opts refine output fold)))

(defn dfs
  "Lazily depth-first explores from an ordered collection of starting states.

  Space is an invokable value called with the result of :project, which
  defaults to :state. Use :project identity for a function of the full visit
  map containing :state and any values supplied by :extensions.
  Maps and state-only functions work directly. Nil successor results are
  treated as leaves. Visits sharing the selected :visit-key are reported and
  expanded only on their first arrival. Each yielded visit contains :state.

  :emit selects :enter (default), :exit, or :events. Entry and exit modes
  return untagged visits in pre-order and post-order respectively.
  With :emit :events, emits two snapshots per accepted visit, tagged with
  :phase :enter and :phase :exit. Entry precedes successor generation; exit
  follows handling the successors under deduplication and pruning rules.
  Back edges to active visits are skipped, so exit does not imply that every
  successor has exited on a cyclic graph. Pruned visits also enter and exit.
  Filter :enter for pre-order or :exit for post-order.
  :phase is reserved in event output. It is not passed to extensions, the
  space, :expand?, or :visit-key; route context and identity are unchanged.
  An infinite descending branch may produce entries forever without exits.
  Optional :fold performs memoized bottom-up evaluation as in postorder.
  It requires a non-nil :visit-key and :emit :exit or :emit :events.
  Entry events have no computed :value; exits carry the finished :value.
  Fold initialization is deferred until consumption resumes after entry.

  Options may contain :project and :visit-key, both functions of the full visit
  map. :visit-key defaults to :state; set it to nil to disable deduplication.
  :decode-edge optionally maps each raw successor to a transition map
  containing :state; DFS uses that destination. Ordered :extensions add
  derived visit values and can inspect all decoded transition fields.
  Extensions are maps with :start and :next callbacks returning visit-context
  maps. Callbacks must preserve :state. Custom context fields merge in
  extension order, with later fields replacing earlier ones; use namespaced
  keys for independently reusable extensions to avoid accidental collisions.
  Folds reserve :value for results and :via and :back-edge for
  successor views.
  :start and :next prepare candidates before duplicate or path-repeat
  rejection, so they may run for visits that are never emitted. Prefer pure
  callbacks; use result consumption for actions on emitted visits.
  Invalid output modes and unsupported fold combinations throw ExceptionInfo
  when called. Unused options are ignored. Data and callback contracts are
  the caller's responsibility and are not validated."
  ([space starts]
   (dfs space starts {}))
  ([space starts options]
   (let [output (search-output :dfs options [:enter :exit :events])]
     (check-fold :dfs options output)
     (depth-first space starts (search-options options) nil output (:fold options)))))

(defn- reject-repeated-keys
  "Refinement for simple paths: a candidate whose identity already occurs on
  its path is dropped. Path identities are hidden visit metadata, so they are
  neither exposed nor part of visit equality."
  [visit-key]
  (fn [parent candidate]
    (let [on-path (or (::path-keys (meta parent)) #{(visit-key parent)})
          key (visit-key candidate)]
      (when-not (contains? on-path key)
        (with-meta candidate
          (assoc (meta candidate) ::path-keys (conj on-path key)))))))

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
  :emit selects :enter (default), :exit, or :events, as in dfs, for each
  accepted path occurrence rather than each globally distinct key.
  Exit mode gives post-order over path occurrences. :fold is ignored:
  shared-key memoization is different from occurrence-based evaluation.
  Output-mode checks and unused-option handling are as in dfs.
  Enumeration is depth-first, so infinite searches need not be fair."
  ([space starts]
   (simple-paths space starts {}))
  ([space starts options]
   (let [output (search-output :simple-paths options [:enter :exit :events])
         {:keys [visit-key] :as opts} (search-options options)]
     (depth-first space starts (assoc opts :visit-key nil)
                  (when visit-key
                    (reject-repeated-keys visit-key))
                  output
                  nil))))

(defn bfs
  "Lazily breadth-first explores from an ordered collection of starting states.

  Space and route options are the same as in dfs. :emit is :enter (default)
  or :events; exit-only output throws ExceptionInfo when called.
  Unused options, including :fold, are ignored.
  Events carry :phase :enter when a visit is accepted, and :phase :expand
  after all of its outgoing transitions have been handled. Expansion does
  not mean descendants have finished and does not give post-order.
  Successor entries can precede their parent's expansion event. An infinite
  successor collection can produce entries forever without that event.
  A pruned visit also gets an expansion event, without calling the space.
  Phases are output-only, not callback inputs or visit identity.
  In a static space, each equal state is reported once at its minimum
  edge-count depth. Add depth-extension
  to include edge-count depth. Successor generation is deferred until its
  frontier chunk is needed. An infinite current level can produce visits
  indefinitely but prevents deeper levels from being reached."
  ([space starts]
   (bfs space starts {}))
  ([space starts options]
   (let [output (search-output :bfs options [:enter :events])
         {:keys [visit-key] :as opts} (search-options options)
         starts (start-visits opts starts)]
     (if (= output :events)
       (breadth-first-events
        space opts (when visit-key #{})
        (frontier/queue-push (frontier/queue) (map-lazy #(vector :enter %) starts)))
       (explore space opts nil (when visit-key #{})
                (frontier/queue-push (frontier/queue) starts)
                frontier/queue-pop frontier/queue-push)))))

(defn postorder
  "Lazily returns visits in depth-first post-order.

  Successors precede their parents; shared keys are evaluated once. With a
  visit key that identifies repeated states, back edges are not followed, so
  finite graphs terminate even with cycles. Setting :visit-key nil disables
  this cycle check; traversal of a cycle may then not terminate.
  The order is topological only for a DAG. Route options are as in dfs.

  This is the exit view of the shared DFS lifecycle, without :phase tags;
  equivalent to dfs with :emit :exit, including optional :fold evaluation.
  :emit is ignored here; use dfs to select another output.
  Without a fold, :visit-key nil visits each occurrence, as in dfs.

  Optional :fold computes a :value on each completed visit. It is a map:
    :init   (visit -> accumulator), required
    :step   (accumulator successor-view -> accumulator), required
    :finish (accumulator -> value), defaults to identity.
    :descend (accumulator transition child -> context-map), optional.
  Each newly entered key gets its own accumulator. :step runs once per
  examined edge in successor order, including edges to cached visits. A
  successor view has :state, route context, its completed :value, :via (the
  transition from this parent), and :back-edge. An unfinished back-edge view
  has no :value; the step must handle it explicitly. Use propagate for cyclic
  fixed points.

  :descend adds or replaces child context after route extensions and before
  computing its visit key. It sees the parent's current accumulator, including
  earlier siblings' results, and runs once per examined edge, even for cached
  children. It must preserve :state and the reserved fields. It does not run
  for starts; initialize their context with a :start extension. Descended
  context affecting results must be included in :visit-key, just like route
  context. Alpha-beta cutoffs produce bounds, not necessarily exact values;
  do not reuse them as exact results under another window.

  Return (reduced accumulator) from :init or :step to finish this visit
  without requesting another successor. :finish receives the unwrapped
  accumulator and is called exactly once per completed key, including leaves
  and early stops. Reducing a parent happens when consumption resumes after
  its child is yielded. Chunked inputs may realize extra raw successors,
  but no further edge is decoded after reduction stops.

  Accumulators are private to parent frames: they are not route context and
  are not passed to :next or the space. Only :descend explicitly exposes
  selected fields to a child. Children are not collected; accumulate
  them explicitly if needed. The cache still retains completed visits.
  Extensions define only :start and :next, as in the other searches.
  :value, :via, and :back-edge are reserved and must not be extension fields.
  Keys must be stable and identify interchangeable computations, including
  any route context affecting expansion or the fold. Cached visits are not
  recomputed for a later arrival with the same key. These input contracts
  are the caller's responsibility and are not validated. A fold with
  :visit-key nil throws ExceptionInfo when called; unused options are ignored."
  ([space starts]
   (postorder space starts {}))
  ([space starts options]
   (check-fold :postorder options :exit)
   (depth-first space starts (search-options options) nil :exit (:fold options))))

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
  iteration and successor order determine the order of intermediate results.
  Unused options, including :emit and :fold, are ignored. A missing :bottom
  or nil :join throws ExceptionInfo when called."
  [space initial-values {:keys [bottom join] :as options}]
  (check-non-nil-option :propagate :join join)
  (check-present-option :propagate options :bottom)
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
                       [values queue pending changes]
                       (reduce
                        (fn [[values queue pending changes :as result]
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
                              [(assoc values state value)
                               (if (contains? pending state)
                                 queue
                                 (conj queue state))
                               (conj pending state)
                               (conj changes {:state state :value value})])))
                        [values queue pending []]
                        (successors-of space opts visit))]
                   (cons {:values values
                          :processed state
                          :changes changes}
                         (run values queue pending))))))]
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
  [space {:keys [decode-edge prepare-visit] :as opts} visit settled state]
  (reduce
   (fn [state successor]
     (let [transition (decode-edge successor)]
       (enqueue opts
                (prepare-visit visit transition)
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
  Weights must be finite and non-negative for Dijkstra's guarantees. Route
  options are as in dfs, plus :priority, a function of the visit map (default
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
  :emit is :enter (default) or :events. Event mode adds :phase :enter when
  a visit is selected and settled, then :phase :expand after its outgoing
  transitions have been handled, before selecting the next visit. An
  expansion is local, not completion of descendants or a post-order result.
  Improved pending candidates do not emit entries until selected. Phases
  never enter callbacks or identity functions. Invalid output modes throw
  ExceptionInfo when called. Unused options, including :fold, are ignored.
  Starts and each expanded state's transitions must be finite. Each result
  contains :state and extension values. A nil :visit-key throws ExceptionInfo
  when called. Starting states are consumed and
  initialized only when the result is consumed; all starts are queued before
  the first visit is selected."
  ([space starts]
   (best-first space starts {}))
  ([space starts options]
   (let [output (search-output :best-first options [:enter :events])
         _ (check-non-nil-option :best-first :visit-key
                                 (get options :visit-key :state))
         priority (get options :priority :distance)
         options (if (= :distance priority)
                   (with-required options distance-extension)
                   options)
         {:keys [start-callbacks visit-key] :as opts}
         (assoc (search-options options) :priority priority :emit output)]
     (letfn [(search [queued settled queue order]
               (lazy-seq
                (when (seq queue)
                  (let [[queue-key visit] (first queue)
                        queue (dissoc queue queue-key)
                        key (visit-key visit)
                        queued (dissoc queued key)
                        settled (conj settled key)]
                    (cons (if (= (:emit opts) :events) (assoc visit :phase :enter) visit)
                          (lazy-seq
                           (let [[queued queue order]
                                 (relax space opts visit settled
                                        [queued queue order])]
                             (if (= (:emit opts) :events)
                               (cons (assoc visit :phase :expand)
                                     (search queued settled queue order))
                               (search queued settled queue order)))))))))]
       (lazy-seq
        (let [[queued queue order]
              (reduce (fn [state start]
                        (enqueue opts (initialize-visit start-callbacks {:state start})
                                 #{} state))
                      [{} (sorted-map) 0]
                      starts)]
          (search queued #{} queue order)))))))
