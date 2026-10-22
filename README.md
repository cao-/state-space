# State-space

State-space exploration for Clojure, with depth-first, breadth-first,
simple-path, post-order traversal, and best-first search plus worklist-based
state propagation over explicit or generated state spaces.

Searches return visits as lazy sequences, so you can inspect intermediate
results and use ordinary Clojure sequence operations to select, transform,
or consume only a prefix instead of requesting only a final answer.

The [State-space documentation PDF](https://github.com/cao-/state-space/releases/latest/download/state-space.pdf)
describes the algorithms and API. The companion
[Worked examples PDF](https://github.com/cao-/state-space/releases/latest/download/worked-examples.pdf)
applies them to a range of problems.

Both PDFs are built from the documentation sources in this repository:
[state-space.tex](state-space.tex) and
[worked-examples.tex](worked-examples.tex).

## When to use it

A small, finite tree walk may be clearer as a recursive function. A
dynamic program with an obvious evaluation order may be clearer as a
`reduce`. You do not need this library just to iterate over values.

State-space is useful when you want to describe the successors and the
problem-specific computation without also implementing the traversal
machinery:

- Search generated states for a goal, with deduplication and optional
  route information, consuming only as much of the search as you need.
- Compute shared subproblems bottom-up with memoization and stack safety,
  discovering their evaluation order from the successor relation.
- Join information from different routes and revisit affected states until
  it stabilizes, including through cycles.

## Installation

The library requires Java 21 or newer. CI tests on Java 21 and Java 25.

Add the library to your `deps.edn` dependencies:

```clojure
{:deps
 {org.clojars.cao/state-space {:mvn/version "0.3.0"}}}
```

The examples below describe the source in this checkout. To use it from
another project, replace `{:mvn/version "0.3.0"}` with
`{:local/root "../state-space"}`, adjusting the path to your checkout.
Use the documentation attached to a release when using its published artifact.

## Quick start

Pass an adjacency map and a collection of starting states to a search. Visits
are maps containing `:state`:

```clojure
(require '[state-space.core :as ss])

(def graph
  {:start [:left :right]
   :left  [:goal]
   :right [:goal]
   :goal  []})

(mapv :state (ss/bfs graph [:start]))
;; => [:start :left :right :goal]
```

Use ordinary sequence functions to select results. Add an extension when
you need route information—for example, the path to a goal:

```clojure
(first (filter #(= :goal (:state %))
               (ss/bfs graph [:start]
                       {:extensions [ss/path-extension]})))
;; => {:state :goal, :path [:start :left :goal]}
```

`bfs` reports `:goal` once, on its first arrival. To list every route that
does not repeat a state, use `simple-paths`:

```clojure
(->> (ss/simple-paths graph [:start] {:extensions [ss/path-extension]})
     (filter #(= :goal (:state %)))
     (mapv :path))
;; => [[:start :left :goal] [:start :right :goal]]
```

When available moves depend on more than location, include that information
in the state. For example, `[:junction 3]` could represent a vehicle at a
junction with three units of fuel remaining: reaching the same junction
with less fuel may allow different onward moves. Extensions carry route
information such as paths and distances; they are not required for every
search.

Choose an algorithm based on the result you need:

- `dfs` explores depth-first; it does not minimize route length.
- `bfs` finds states in the fewest transitions when all transitions have
  equal cost.
- `best-first` orders visits by cumulative cost by default (Dijkstra's
  algorithm); edge weights must be non-negative for its shortest-path
  guarantee.
- `simple-paths` enumerates routes without repeating a state on a route.
- `postorder` visits successors before their parents, useful for dependency
  ordering and memoized bottom-up computations. Its optional `:fold` combines
  child results incrementally and can skip remaining children once a result
  is known.
- `propagate` repeatedly joins information arriving at states until the
  worklist (the queue of states still to process) is empty.

See the [State-space documentation](https://github.com/cao-/state-space/releases/latest/download/state-space.pdf)
for the full API, options, and algorithm assumptions. The
[Worked examples](https://github.com/cao-/state-space/releases/latest/download/worked-examples.pdf)
show these operations applied to concrete problems.

## Least-cost routes

When transitions have different costs, use `best-first` to find the cheapest
route rather than the one with the fewest steps. Decode each edge into a
transition map with its destination and weight:

```clojure
(def weighted-graph
  {:start [[:goal 10] [:via 2]]
   :via   [[:goal 3]]})

(first (filter #(= :goal (:state %))
               (ss/best-first weighted-graph [:start]
                              {:decode-edge (fn [[state weight]]
                                              {:state state :weight weight})
                               :extensions [ss/path-extension]})))
;; => {:state :goal, :distance 5, :path [:start :via :goal]}
```

By default, `best-first` adds cumulative `:distance` and uses it as the
priority (Dijkstra's algorithm). The two-step route costs 5, less than the
direct edge's 10. Weights must be finite and non-negative for this
shortest-path guarantee. User extensions run afterward and can replace
`:distance`, changing the default ordering.

For a custom `:priority`, return values that are mutually comparable so the
search can order pending visits.

## Bottom-up computations

A fold computes a parent's result from its children's results. Initialize
an accumulator (the running result) for each state, then update it with each
child's computed `:value`. The traversal reuses results for shared states
(memoization) and handles deep searches without growing the JVM call stack:

```clojure
(ss/postorder {:root [:left :right]} [:root]
              {:fold {:init (constantly 1)
                      :step (fn [size child] (+ size (:value child)))}})
;; => ({:state :left :value 1} {:state :right :value 1}
;;     {:state :root :value 3})
```

Here each state counts itself, then adds its children's counts. An optional
`:finish` callback converts the final accumulator to the visit's `:value`;
without it, the accumulator itself is the value.

To stop processing a visit's remaining children early, return the accumulator
from `:step` wrapped with Clojure's `reduced` function—for example,
`(reduced true)` once a match is found. Returning it from `:init` skips all
children of that visit.
The fold unwraps the value before applying `:finish`. This completes only
the current visit, not the whole traversal. Child results are passed to
`:step` one at a time, not collected into a vector unless the fold builds one.

Projection and identity are intentionally independent: `:project` chooses
the input to the successor function, while `:visit-key` defines which
arrivals the algorithm treats as equivalent. Both receive the full visit.
A key can include relevant context or ignore fields that only record a
witness, such as a path. For folds, a non-nil key identifies computations
whose results can be shared, independently of what the space needs as input.
Include route context in that key when it affects the computed result.
Shared children are evaluated once but contribute to each incoming edge;
use sets rather than sums to count distinct descendants. On a cycle, a
child passed to `:step` can refer back to a visit still being explored.
That child has `:back-edge true` and no computed `:value`.
Handle that case explicitly, or use propagation for cyclic fixed points.

## Combining information through cycles

Suppose capabilities flow from a database to an API, and between the API
and a worker that depend on each other. Each service must learn all
capabilities reaching it, not just those on its first arrival:

```clojure
(require '[clojure.set :as set])

(def capability-flow
  {:database [:api]
   :api      [:worker]
   :worker   [:api]})

(:values
 (last (ss/propagate capability-flow
                     {:database #{:sql}
                      :worker #{:jobs}}
                     {:bottom #{}
                      :join set/union})))
;; => {:database #{:sql}
;;     :api #{:sql :jobs}
;;     :worker #{:sql :jobs}}
```

A visit-once traversal can miss information arriving later. `propagate`
joins incoming sets and schedules a state again when its value changes.
The `:join` option is the combining function, and `:bottom` is the initial value
for a newly reached state before any contributions are combined.
Here, propagation terminates because sets only grow within a finite set of
capabilities on a finite graph; arbitrary joins and state spaces need not
terminate. Each progress record exposes the current values and changes,
so you can inspect intermediate results instead of taking only the last.

## Advanced: traversal output and tracing

Ordinary searches need no output options. When you need depth-first return
order (children before their parent) or want to observe traversal decisions,
select an output. `:events` returns a lazy stream of tagged event maps:

| Operation | Default output | `:emit :events` |
|-----------|---------------|-----------------|
| `dfs`, `simple-paths` | Pre-order visits (`:emit :enter`) | Candidate decisions and visit entry/return events |
| `bfs`, `best-first` | Ordinary visits | Candidate decisions and visit entry/expansion events |
| `postorder` | Completed visits | The shared DFS event stream |
| `propagate` | Bare progress records | Progress records wrapped in event envelopes |

```clojure
(mapv (fn [{:keys [phase visit]}] [(:state visit) phase])
      (filter #(= :visit (:event %))
              (ss/dfs {:root [:left :right]} [:root] {:emit :events})))
;; => [[:root :enter] [:left :enter] [:left :exit]
;;     [:right :enter] [:right :exit] [:root :exit]]
```

Every event has an `:event` key. `:visit` records contain `:phase` and a
nested `:visit` map; `:candidate` records describe starts or decoded edges,
including `:disposition` (`:scheduled` or `:discarded`). Edge candidates also
include `:from` and `:transition`. Rejections have a `:reason`; best-first
scheduled candidates include an `:effect` such as `:inserted` or
`:improved-pending`. A propagation event wraps its existing record under
`:record`.

Visit phases are output-only and never enter traversal callbacks:

- `:enter`: a visit is accepted for processing; in BFS and best-first it is
  emitted when selected, not when first queued.
- `:exit`: depth-first exploration returns under cycle and pruning rules.
- `:expand`: BFS or best-first has handled the immediate outgoing transitions,
  **not** completed descendants. BFS can accept children before this marker;
  best-first emits it before selecting another visit.
- `:skip`: a queued BFS candidate is rejected at dequeue time, for example
  because an equivalent visit was already accepted.

In DFS, immediate duplicate/path-repeat rejection appears as a discarded
candidate; the traversal does not emit a separate `:skip` visit event. BFS
emits a `:scheduled` candidate when it is queued, then may emit a later
`:skip` visit if the candidate is a duplicate when dequeued. Best-first emits
the candidate when it decides whether to enqueue, improve, or discard it.
Thus a candidate event reports the decision made at that point; later visit
events report what happens when a queued candidate is selected. Filter `:visit`
events by `:phase` to recover lifecycle snapshots. Event records keep route
visits unchanged and do not add event fields to callback inputs. A fold can
cut off siblings and therefore change which candidates and visits occur. Lazy
consumption can leave visits without completion events; infinite branches or
successor streams may never finish. Events are observations, not cleanup
callbacks.

## Advanced: sibling-dependent pruning

An optional fold callback, `:descend`, receives
`[accumulator transition child]` and returns context fields to merge into the
child after route extensions, before computing its key. It sees the parent's
updated accumulator after earlier siblings contribute. Combined with
`:step` and `reduced`, this enables alpha-beta pruning without mutable
feedback from event consumers.

Initialize root context with a `:start` extension. Include descended fields
affecting results in the visit key. In alpha-beta search, a cutoff may
establish only a lower or upper bound on the score, not its exact value;
that bound must not be reused as an exact score under different search
bounds. The worked examples contain a complete
alpha-beta problem and explain this distinction. A recursive solution may be
shorter for a small tree; folds supply stack safety and contextual memoization.

## Option reference

The graph searches share `:project`, `:expand?`, `:decode-edge`, `:visit-key`,
and `:extensions`. DFS and simple-paths also accept `:emit :exit`; DFS and
post-order accept `:fold`; best-first accepts `:priority`. Propagation shares
only `:project`, `:expand?`, and `:decode-edge`, with required `:bottom` and
`:join`, and accepts `:emit :events`.

Invalid `:emit` values on operations with selectable output, invalid
propagation output, folds with `:visit-key nil`, and DFS folds with entry-only
output throw `ExceptionInfo` when called. `postorder` defaults to `:emit :exit`
and also accepts `:emit :events`. Unused options and `:fold` on operations
without folding are ignored. Traversal callbacks remain lazy; data and
callback contracts are the caller's responsibility, not validated.

## Benchmarks

Run `clojure -M:bench` for library benchmarks or
`clojure -M:standard-bench` to compare with direct Clojure solutions. The
benchmark files document commands and workload details.
