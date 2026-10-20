(ns state-space.internal.frontier)

(defn stack
  "Creates an empty lazy-chunk stack."
  []
  [nil '()])

(defn stack-push
  "Pushes a lazy chunk onto a stack, ahead of its remaining items."
  [[current chunks] chunk]
  [chunk (conj chunks current)])

(defn- pop-item [current chunks next-chunk]
  (loop [current current, chunks chunks]
    (if-let [remaining (seq current)]
      [(first remaining) [(rest remaining) chunks]]
      (when-let [[chunk chunks] (next-chunk chunks)]
        (recur chunk chunks)))))

(defn- stack-next-chunk [chunks]
  (when (seq chunks)
    [(first chunks) (rest chunks)]))

(defn stack-pop
  "Returns [value updated-stack], or nil when empty."
  [[current chunks]]
  (pop-item current chunks stack-next-chunk))

(defn queue
  "Creates an empty lazy-chunk queue."
  []
  [nil clojure.lang.PersistentQueue/EMPTY])

(defn queue-push
  "Appends a lazy chunk to a queue, behind its remaining items and chunks."
  [[current chunks] chunk]
  [current (conj chunks chunk)])

(defn- queue-next-chunk [chunks]
  (when (seq chunks)
    [(peek chunks) (pop chunks)]))

(defn queue-pop
  "Returns [value updated-queue], or nil when empty."
  [[current chunks]]
  (pop-item current chunks queue-next-chunk))
