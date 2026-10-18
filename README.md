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
 {org.clojars.cao/state-space {:mvn/version "0.2.0"}}}
```
