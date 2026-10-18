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

(deftest frontiers-match-a-reference-under-interleaved-operations
  (doseq [[push pop-item empty-frontier prepend?]
          [[frontier/stack-push frontier/stack-pop (frontier/stack) true]
           [frontier/queue-push frontier/queue-pop (frontier/queue) false]]]
    (let [random (java.util.Random. 1729)]
      (loop [pending empty-frontier
             expected []
             step 0]
        (if (= step 1000)
          (is (= expected (drain pop-item pending)))
          (if (< (.nextInt random 3) 2)
            (let [chunk (vec (take (.nextInt random 5) [nil false step :item]))
                  expected (if prepend?
                             (into chunk expected)
                             (into expected chunk))]
              (recur (push pending chunk) expected (inc step)))
            (let [result (pop-item pending)]
              (is (= (boolean (seq expected)) (some? result)))
              (when (seq expected)
                (is (= (first expected) (:item result))))
              (recur (if result (:frontier result) pending)
                     (vec (rest expected))
                     (inc step)))))))))

(deftest frontiers-skip-many-empty-chunks
  (doseq [[push pop-item empty-frontier]
          [[frontier/stack-push frontier/stack-pop (frontier/stack)]
           [frontier/queue-push frontier/queue-pop (frontier/queue)]]]
    (let [pending (reduce push empty-frontier (repeat 20000 []))]
      (is (nil? (pop-item pending)))
      (is (= [nil false] (drain pop-item (push pending [nil false])))))))

(deftest pushing-does-not-realize-a-chunk
  (doseq [[push empty-frontier]
          [[frontier/stack-push (frontier/stack)]
           [frontier/queue-push (frontier/queue)]]]
    (let [realized (atom [])
          chunk (observed-seq [1 2] realized)]
      (push empty-frontier chunk)
      (is (empty? @realized)))))
