(ns state-space.internal.frontier)

(defn stack
  "Creates an empty lazy-chunk stack."
  []
  {:current nil
   :chunks '()})

(defn stack-push
  "Pushes a lazy chunk onto a stack, ahead of its remaining items."
  [frontier chunk]
  (-> frontier
      (update :chunks conj (:current frontier))
      (assoc :current chunk)))

(defn- pop-item [frontier next-chunk remaining-chunks]
  (loop [current (:current frontier)
         chunks (:chunks frontier)]
    (if-let [remaining (seq current)]
      {:item (first remaining)
       :frontier (assoc frontier :current (rest remaining) :chunks chunks)}
      (when (seq chunks)
        (recur (next-chunk chunks) (remaining-chunks chunks))))))

(defn stack-pop
  "Returns {:item value :frontier updated-stack}, or nil when empty."
  [frontier]
  (pop-item frontier first rest))

(defn queue
  "Creates an empty lazy-chunk queue."
  []
  {:current nil
   :chunks clojure.lang.PersistentQueue/EMPTY})

(defn queue-push
  "Appends a lazy chunk to a queue, behind its remaining items and chunks."
  [frontier chunk]
  (update frontier :chunks conj chunk))

(defn queue-pop
  "Returns {:item value :frontier updated-queue}, or nil when empty."
  [frontier]
  (pop-item frontier peek pop))
