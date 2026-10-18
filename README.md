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
