(ns state-space.documentation-examples-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- clojure-documentation-examples [source]
  (->> (re-seq #"(?s)\\begtt \\hisyntax\{clojure\}\r?\n(.*?)\\endtt"
               (slurp source))
       (map second)))

(defn- evaluate-documentation-code [example-ns code]
  (binding [*ns* example-ns]
    (load-string code)))

(defn- documentation-results [example]
  (loop [lines (seq (str/split-lines example))
         code []
         results []]
    (if-let [line (first lines)]
      (if-let [[_ first-output-line]
               (re-matches #"^\s*;; =>\s*(.*)$" line)]
        (let [[output-lines remaining]
              (loop [lines (next lines)
                     output-lines [first-output-line]]
                (if-let [line (first lines)]
                  (if (re-matches #"^\s*;;(?!\s*=>).*" line)
                    (recur (next lines)
                           (conj output-lines
                                 (str/replace-first line #"^\s*;; ?" "")))
                    [output-lines lines])
                  [output-lines nil]))]
          (recur remaining []
                 (conj results
                       {:code (str/join "\n" code)
                        :expected (edn/read-string
                                   (str/join "\n" output-lines))})))
        (recur (next lines) (conj code line) results))
      (cond-> results
        (some (complement str/blank?) code)
        (conj {:code (str/join "\n" code)})))))

(deftest published-clojure-examples-match-their-displayed-results
  (doseq [source ["state-space.tex" "worked-examples.tex"]]
    (let [examples (clojure-documentation-examples source)
          example-ns (create-ns (gensym "state-space.documentation.examples"))]
      (is (seq examples) (str "Expected Clojure examples in " source))
      (try
        (binding [*ns* example-ns]
          (clojure.core/refer 'clojure.core)
          (doseq [[example-index example] (map-indexed vector examples)]
            (doseq [[output-index {:keys [code expected] :as result}]
                    (map-indexed vector (documentation-results example))]
              (let [actual (try
                             (evaluate-documentation-code example-ns code)
                             (catch Exception error
                               (throw (ex-info
                                       (str source ": Clojure documentation example "
                                            (inc example-index) " failed")
                                       {:source source
                                        :example-index (inc example-index)
                                        :output-index (inc output-index)}
                                       error))))]
                (when (contains? result :expected)
                  (is (= expected actual)
                      (str source ": documentation example " (inc example-index)
                           ", output " (inc output-index))))))))
        (finally
          (remove-ns (ns-name example-ns)))))))
