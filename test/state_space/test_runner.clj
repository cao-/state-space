(ns state-space.test-runner
  (:require [clojure.test :as test]
            [state-space.core-test]
            [state-space.documentation-examples-test]
            [state-space.internal.frontier-test]))

(defn -main [& _]
  (let [result (test/run-tests 'state-space.core-test
                               'state-space.documentation-examples-test
                               'state-space.internal.frontier-test)]
    (when (pos? (+ (:fail result) (:error result)))
      (System/exit 1))))
