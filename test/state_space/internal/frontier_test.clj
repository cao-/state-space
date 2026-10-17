(ns state-space.internal.frontier-test
  (:require [clojure.test :refer [deftest is]]
            [state-space.internal.frontier :as frontier]))

(defn- drain [pop-item initial-frontier]
  (loop [pending initial-frontier
         items []]
    (if-let [{:keys [item frontier]} (pop-item pending)]
      (recur frontier (conj items item))
      items)))

(defn- observed-seq [items realized]
  (lazy-seq
   (when-let [items (seq items)]
     (let [item (first items)]
       (swap! realized conj item)
       (cons item (observed-seq (rest items) realized))))))

(deftest stack-pops-new-chunks-before-older-chunks
  (let [stack (-> (frontier/stack)
                  (frontier/stack-push [1 2])
                  (frontier/stack-push [])
                  (frontier/stack-push [3 4]))]
    (is (= [3 4 1 2]
           (drain frontier/stack-pop stack)))))

(deftest queue-pops-chunks-in-insertion-order
  (let [queue (-> (frontier/queue)
                  (frontier/queue-push [1 2])
                  (frontier/queue-push [])
                  (frontier/queue-push [3 4]))]
    (is (= [1 2 3 4]
           (drain frontier/queue-pop queue)))))

(deftest popping-a-chunk-realizes-only-the-next-item
  (doseq [[push pop-item empty-frontier]
          [[frontier/stack-push frontier/stack-pop (frontier/stack)]
           [frontier/queue-push frontier/queue-pop (frontier/queue)]]]
    (let [realized (atom [])
          chunk (observed-seq [1 2] realized)
          first-pop (pop-item (push empty-frontier chunk))]
      (is (= 1 (:item first-pop)))
      (is (= [1] @realized))
      (let [second-pop (pop-item (:frontier first-pop))]
        (is (= 2 (:item second-pop)))
        (is (= [1 2] @realized))))))
