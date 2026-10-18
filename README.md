# State-space

State-space exploration for Clojure, with depth-first, breadth-first,
simple-path, post-order traversal, and best-first search plus worklist-based
state propagation over explicit or generated state spaces.

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
  child results incrementally, using `reduced` for early stopping.
- `propagate` repeatedly joins information arriving at states until the
  worklist is empty.

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
shortest-path guarantee.

## Bottom-up computations

Initialize an accumulator for each state, then incorporate each child's
computed `:value`. The traversal supplies stack safety and memoization:

```clojure
(ss/postorder {:root [:left :right]} [:root]
              {:fold {:init (constantly 1)
                      :step (fn [size child] (+ size (:value child)))}})
;; => ({:state :left :value 1} {:state :right :value 1}
;;     {:state :root :value 3})
```

An optional `:finish` in the fold converts the accumulator to the published
value. Return `(reduced accumulator)` from `:init` or `:step` to finish a
visit without requesting more children. Child vectors are not collected
unless the fold explicitly builds one.

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
Here, propagation terminates because sets only grow within a finite set of
capabilities on a finite graph; arbitrary joins and state spaces need not
terminate. Each progress record exposes the current values and changes,
so you can inspect intermediate results instead of taking only the last.
