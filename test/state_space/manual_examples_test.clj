(ns state-space.manual-examples-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [state-space.core :as search]))

(deftest propagate-value-through-a-simple-space
  (let [space {:start [:next]
               :next []}]
    (is (= {:start #{:ready}
            :next #{:ready}}
           (:values (last (search/propagate space
                                           {:start #{:ready}}
                                           {:bottom #{}
                                            :join set/union})))))))

(deftest find-capabilities-required-by-a-service
  (let [service-dependencies {:catalog-db [:catalog-api]
                              :identity-provider [:account-api]
                              :catalog-api [:storefront]
                              :account-api [:storefront]
                              :storefront []}
        values (:values
                (last (search/propagate service-dependencies
                                        {:catalog-db #{:sql}
                                         :identity-provider #{:oauth}}
                                        {:bottom #{}
                                         :join set/union})))]
    (is (= {:catalog-db #{:sql}
            :identity-provider #{:oauth}
            :catalog-api #{:sql}
            :account-api #{:oauth}
            :storefront #{:sql :oauth}}
           values))))

(deftest depth-first-finds-first-route-in-successor-order
  (let [building {:lobby [:stairs :gallery]
                  :stairs [:landing]
                  :landing [:exit]
                  :gallery [:exit]
                  :exit []}]
    (is (= {:state :exit
            :path [:lobby :stairs :landing :exit]}
           (first (filter #(= :exit (:state %))
                          (search/dfs building [:lobby]
                                      {:extensions [search/path-extension]})))))))

(deftest disabling-visit-key-preserves-equal-tree-occurrences
  (let [  repeated-branch {:value :branch
                   :children [{:value :leaf}]}
  tree {:value :root
        :children [repeated-branch repeated-branch]}
  values (fn [nodes] (mapv :value nodes))
  default-dfs (values (map :state (search/dfs :children [tree])))
  undeduplicated-dfs
  (values (map :state
               (search/dfs :children [tree] {:visit-key nil})))
  tree-seq-values
  (values (tree-seq (comp seq :children) :children tree))]
    (is (= [:root :branch :leaf] default-dfs))
    (is (= [:root :branch :leaf :branch :leaf] undeduplicated-dfs))
    (is (= tree-seq-values undeduplicated-dfs))))

(deftest generate-every-subset-by-dfs
  (let [items [:a :b :c :d]
        subset-decisions (fn [[i chosen]]
                           (when (< i (count items))
                             [[(inc i) chosen]
                              [(inc i) (conj chosen (items i))]]))
        subsets (->> (search/dfs subset-decisions [[0 []]])
                     (map :state)
                     (filter #(= (count items) (first %)))
                     (map second)
                     vec)]
    (is (= [[] [:d] [:c] [:c :d]
            [:b] [:b :d] [:b :c] [:b :c :d]
            [:a] [:a :d] [:a :c] [:a :c :d]
            [:a :b] [:a :b :d] [:a :b :c] [:a :b :c :d]]
           subsets))))

(deftest find-eight-queens-solutions-by-pruned-dfs
  (let [safe-column?
        (fn [placed column]
          (let [row (count placed)]
            (every? (fn [[previous-row previous-column]]
                      (and (not= column previous-column)
                           (not= (- row previous-row)
                                 (abs (- column previous-column)))))
                    (map-indexed vector placed))))
        placements
        (fn [placed]
          (when (< (count placed) 8)
            (for [column (range 8)
                  :when (safe-column? placed column)]
              (conj placed column))))
        solutions (filter #(= 8 (count (:state %)))
                          (search/dfs placements [[]]))]
    (is (= 92 (count solutions)))
    (is (every? (fn [{:keys [state]}]
                  (= 8 (count (set state))))
                solutions))))

(deftest fewest-moves-around-a-blocked-square
  (let [blocked-squares #{[2 0] [2 1] [2 2] [2 3]}
        grid-moves (fn [[x y]]
                     (filter (fn [[nx ny]]
                               (and (<= 0 nx 4)
                                    (<= 0 ny 4)
                                    (not (contains? blocked-squares [nx ny]))))
                             [[(inc x) y] [(dec x) y]
                              [x (inc y)] [x (dec y)]]))]
    (is (= 8
           (:depth (first (filter
                           #(= [4 2] (:state %))
                           (search/bfs grid-moves [[0 2]]
                                       {:extensions [search/depth-extension]}))))))))

(deftest find-each-cells-distance-to-the-nearest-zero
  (let [grid ["000" "010" "111"]
        starts (for [row (range (count grid))
                     col (range (count (first grid)))
                     :when (= \0 (get-in grid [row col]))]
                 [row col])
        neighbours (fn [[row col]]
                     (filter #(get-in grid %)
                             [[(dec row) col] [(inc row) col]
                              [row (dec col)] [row (inc col)]]))
        distance (into {} (map (juxt :state :depth))
                       (search/bfs neighbours starts
                                   {:extensions [search/depth-extension]}))]
    (is (= [[0 0 0] [0 1 0] [1 2 1]]
           (mapv (fn [row]
                   (mapv #(distance [row %]) (range (count (first grid)))))
                 (range (count grid)))))))

(deftest check-whether-every-component-is-bipartite
  (let [nodes #{:a :b :c :d :x :y :z}
        graph {:a [:b :d]
               :b [:a :c]
               :c [:b :d]
               :d [:a :c]
               :x [:y :z]
               :y [:x :z]
               :z [:x :y]}
        adjacency (reduce (fn [adjacent [from to]]
                            (-> adjacent
                                (update from conj to)
                                (update to conj from)))
                          (zipmap nodes (repeat []))
                          (for [[from tos] graph, to tos] [from to]))
        bipartite?
        (loop [uncoloured nodes]
          (if-let [root (first uncoloured)]
            (let [component (into {}
                                  (map (fn [{:keys [state depth]}]
                                         [state (mod depth 2)]))
                                  (search/bfs adjacency [root]
                                              {:extensions [search/depth-extension]}))]
              (if (some (fn [[state colour]]
                          (some #(= colour (component %)) (adjacency state)))
                        component)
                false
                (recur (reduce disj uncoloured (keys component)))))
            true))]
    (is (false? bipartite?))))

(deftest browse-pages-only-up-to-two-clicks-away
  (let [website {:home [:about :shop]
                 :about [:team :shop]
                 :shop [:about :team :item]
                 :team [:home :reviews]
                 :item [:reviews]
                 :reviews []}
        website-links (fn [{:keys [state depth]}]
                        (when (< depth 2)
                          (website state)))]
    (is (= [[:home 0] [:about 1] [:shop 1] [:team 2] [:item 2]]
           (mapv (juxt :state :depth)
                 (search/bfs website-links [:home]
                             {:project identity
                              :extensions [search/depth-extension]}))))
    (is (= [[:home 0] [:about 1] [:shop 1] [:team 2] [:item 2]]
           (mapv (juxt :state :depth)
                 (search/bfs website [:home]
                             {:extensions [search/depth-extension]
                              :expand? #(< (:depth %) 2)}))))))

(defn- knight-moves [[x y]]
  (for [[dx dy] [[1 2] [2 1] [-1 2] [-2 1]
                 [1 -2] [2 -1] [-1 -2] [-2 -1]]
        :let [next [(+ x dx) (+ y dy)]]
        :when (every? #(<= 0 % 7) next)]
    next))

(deftest chessboard-knight-route
  (is (= 6
         (:depth (first (filter
                         #(= [7 7] (:state %))
                         (search/bfs knight-moves [[0 0]]
                                     {:extensions [search/depth-extension]})))))))

(defn- jug-moves [[a b]]
  (let [capacity-a 3 capacity-b 5
        pour-ab (min a (- capacity-b b))
        pour-ba (min b (- capacity-a a))]
    [[capacity-a b] [a capacity-b] [0 b] [a 0]
     [(- a pour-ab) (+ b pour-ab)]
     [(+ a pour-ba) (- b pour-ba)]]))

(deftest four-litres-in-two-jugs
  (let [route (first (filter
                      (fn [{[_ b] :state}] (= 4 b))
                      (search/bfs jug-moves [[0 0]]
                                  {:extensions [search/path-extension]})))]
    (is (= 7 (count (:path route))))))

(deftest missionaries-and-cannibals-cross-in-eleven-trips
  (let [safe-bank? (fn [missionaries cannibals]
                     (or (zero? missionaries)
                         (>= missionaries cannibals)))
        safe-state? (fn [[left-m left-c _]]
                      (and (<= 0 left-m 3)
                           (<= 0 left-c 3)
                           (safe-bank? left-m left-c)
                           (safe-bank? (- 3 left-m) (- 3 left-c))))
        crossings (fn [[missionaries cannibals boat]]
                    (let [direction (if (zero? boat) -1 1)]
                      (for [[move-m move-c] [[1 0] [2 0] [0 1] [0 2] [1 1]]
                            :let [next [(+ missionaries (* direction move-m))
                                        (+ cannibals (* direction move-c))
                                        (- 1 boat)]]
                            :when (safe-state? next)]
                        next)))
        goal (first (filter #(= [0 0 1] (:state %))
                            (search/bfs crossings [[3 3 0]]
                                        {:extensions [search/depth-extension]})))]
    (is (= 11 (:depth goal)))))

(deftest fewest-perfect-square-subtractions
  (let [square-subtractions
        (fn [remaining]
          (for [root (range 1 (inc (long (Math/sqrt remaining))))]
            (- remaining (* root root))))
        route (first (filter #(zero? (:state %))
                             (search/bfs square-subtractions [12]
                                         {:extensions [search/path-extension]})))]
    (is (= [12 8 4 0] (:path route)))))

(deftest open-a-combination-lock-in-fewest-turns
  (let [blocked-codes #{"0201" "0101" "0102" "1212" "2002"}
        lock-turns (fn [code]
                     (for [wheel (range 4)
                           direction [-1 1]
                           :let [digit (Character/digit ^char (nth code wheel) 10)
                                 next-digit (mod (+ digit direction) 10)
                                 next-code (str (subs code 0 wheel)
                                                next-digit
                                                (subs code (inc wheel)))]
                           :when (not (blocked-codes next-code))]
                       next-code))
        turns (first (filter #(= "0202" (:state %))
                             (search/bfs lock-turns ["0000"]
                                         {:extensions [search/depth-extension]})))]
    (is (= 6 (:depth turns)))))

(deftest solve-a-sliding-tile-puzzle-with-bfs
  (let [slide-blank (fn [tiles]
                      (let [blank (.indexOf ^java.util.List tiles 0)
                            row (quot blank 3)
                            col (rem blank 3)]
                        (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
                              :let [r (+ row dr)
                                    c (+ col dc)]
                              :when (and (<= 0 r 1)
                                         (<= 0 c 2))]
                          (assoc tiles blank
                                 (tiles (+ (* r 3) c))
                                 (+ (* r 3) c) 0))))
        route (first (filter
                      #(= [1 2 3 4 5 0] (:state %))
                      (search/bfs slide-blank [[4 1 3 0 2 5]]
                                 {:extensions [search/path-extension]})))]
    (is (= [[4 1 3 0 2 5]
            [0 1 3 4 2 5]
            [1 0 3 4 2 5]
            [1 2 3 4 0 5]
            [1 2 3 4 5 0]]
           (:path route)))))

(deftest solve-one-box-sokoban-with-bfs
  (let [grid ["#######"
              "#@    #"
              "# $ . #"
              "#     #"
              "#######"]
        wall? (fn [[row column]]
                (= \# (get-in grid [row column])))
        moves (fn [[player box]]
                (for [direction [[1 0] [-1 0] [0 1] [0 -1]]
                      :let [next-player (mapv + player direction)]
                      :when (not (wall? next-player))
                      :let [next-box (if (= next-player box)
                                       (mapv + box direction)
                                       box)]
                      :when (not (wall? next-box))]
                  [next-player next-box]))
        goal (first (filter #(= [2 4] (second (:state %)))
                            (search/bfs moves [[[1 1] [2 2]]]
                                        {:extensions [search/depth-extension]})))]
    (is (= 3 (:depth goal)))))

(deftest plan-around-the-sussman-anomaly
  (let [moves (fn [stacks]
                (for [from stacks
                      :let [block (peek from)
                            without (cond-> (disj stacks from)
                                      (seq (pop from)) (conj (pop from)))]
                      target (cons nil (disj stacks from))]
                  (if target
                    (-> without (disj target) (conj (conj target block)))
                    (conj without [block]))))
        plan (first (filter #(= #{[:c :b :a]} (:state %))
                            (search/bfs moves [#{[:a :c] [:b]}]
                                        {:extensions [search/depth-extension
                                                      search/path-extension]})))]
    (is (= 3 (:depth plan)))
    (is (= [[[:a :c] [:b]] [[:a] [:b] [:c]]
            [[:a] [:c :b]] [[:c :b :a]]]
           (mapv #(vec (sort-by first %))
                 (:path plan))))))

(def ^:private words
  #{"cold" "cord" "card" "ward" "warm" "worm" "word" "corm"})

(defn- one-letter-apart? [a b]
  (= 1 (count (filter true? (map not= a b)))))

(defn- ladder-neighbours [word]
  (filter #(one-letter-apart? word %) words))

(deftest word-ladder-shortest-route
  (let [route (first (filter
                      #(= "warm" (:state %))
                      (search/bfs ladder-neighbours ["cold"]
                                  {:extensions [search/path-extension]})))]
    (is (= 5 (count (:path route))))))

(deftest enumerate-simple-routes-through-a-cyclic-map
  (let [city-map {:home [:park :market]
                  :park [:market :museum]
                  :market [:park :museum]
                  :museum []}
        routes (->> (search/simple-paths city-map [:home]
                                         {:extensions [search/path-extension]})
                    (filter #(= :museum (:state %)))
                    (map :path)
                    vec)]
    (is (= [[:home :park :market :museum]
            [:home :park :museum]
            [:home :market :park :museum]
            [:home :market :museum]]
           routes))))

(deftest enumerate-walks-of-exactly-three-steps
  (let [walk-graph {:a [:b]
                    :b [:a :c]
                    :c [:a]}
        walks (->> (search/bfs walk-graph [:a]
                               {:visit-key nil
                                :expand? #(< (:depth %) 3)
                                :extensions [search/depth-extension
                                             search/path-extension]})
                   (filter #(= 3 (:depth %)))
                   (map :path)
                   set)]
    (is (= #{[:a :b :a :b] [:a :b :c :a]}
           walks))))

(deftest visit-every-node-in-the-fewest-steps
  (let [graph [[1 2 3] [0] [0] [0]]
        full (set (range (count graph)))
        moves (fn [[node visited]]
                (for [next (graph node)]
                  [next (conj visited next)]))
        starts (map (fn [node] [node #{node}]) (range (count graph)))
        goal (first (filter #(= full (second (:state %)))
                            (search/bfs moves starts
                                        {:extensions [search/depth-extension
                                                      search/path-extension]})))]
    (is (= 4 (:depth goal)))
    (is (= full (second (:state goal))))))

(deftest model-check-mutual-exclusion-and-find-counterexample
  (let [start {:pc [0 0] :flag [false false] :turn 0}
        peterson
        (fn [wait?]
          (fn [{:keys [turn] :as state}]
            (for [i [0 1]
                  :let [j (- 1 i)]]
              (case (get-in state [:pc i])
                0 (-> state (assoc-in [:flag i] true) (assoc-in [:pc i] 1))
                1 (-> state (assoc :turn j) (assoc-in [:pc i] 2))
                2 (if (and wait? (get-in state [:flag j]) (= turn j))
                    state
                    (assoc-in state [:pc i] 3))
                3 (assoc-in state [:pc i] 4)
                4 (-> state (assoc-in [:flag i] false)
                      (assoc-in [:pc i] 0))))))
        violation? #(= [3 3] (get-in % [:state :pc]))
        violating-visit
        (fn [wait?]
          (first (filter violation?
                         (search/bfs (peterson wait?) [start]
                                     {:extensions [search/depth-extension
                                                   search/path-extension]}))))
        safe-visits (search/bfs (peterson true) [start])
        counterexample (violating-visit false)]
    (is (nil? (first (filter violation? safe-visits))))
    (is (= 6 (:depth counterexample)))
    (is (= [[0 0] [1 0] [2 0] [3 0] [3 1] [3 2] [3 3]]
           (mapv #(get-in % [:pc]) (:path counterexample))))))

(deftest choose-least-cost-route
  (let [travel-times {:home [[:museum 11] [:village 4] [:park 1]]
                      :village [[:museum 7] [:hill 2]]
                      :park [[:museum 8] [:village 1]]
                      :hill [[:museum 2]]
                      :museum []}
        route (first (filter
                      #(= :museum (:state %))
                      (search/best-first travel-times [:home]
                                         {:decode-edge
                                          (fn [[state weight]]
                                            {:state state :weight weight})
                                          :extensions [search/path-extension]})))]
    (is (= {:path [:home :park :village :hill :museum]
            :distance 6}
           (select-keys route [:path :distance])))))

(deftest build-a-minimum-spanning-tree-with-best-first
  (let [graph {:a [{:state :b :weight 1} {:state :c :weight 4}]
               :b [{:state :a :weight 1} {:state :c :weight 2}
                   {:state :d :weight 5}]
               :c [{:state :a :weight 4} {:state :b :weight 2}
                   {:state :d :weight 1}]
               :d [{:state :b :weight 5} {:state :c :weight 1}]}
        joining-edge {:start (fn [_] {:joining-edge 0})
                      :next (fn [parent transition _]
                              {:joining-edge (:weight transition)
                               :road [(:state parent)
                                      (:state transition)
                                      (:weight transition)]})}
        tree (search/best-first graph [:a]
                                {:decode-edge identity
                                 :extensions [joining-edge]
                                 :priority :joining-edge})]
    (is (= [:a :b :c :d] (map :state tree)))
    (is (= [[:a :b 1] [:b :c 2] [:c :d 1]]
           (map :road (rest tree))))
    (is (= 4 (transduce (map :joining-edge) + tree)))))

(deftest choose-cheapest-route-through-a-negative-edge-in-a-dag
  (let [graph {:s [{:state :a :weight 4} {:state :b :weight 2}]
               :a [{:state :t :weight -3}]
               :b [{:state :a :weight -1} {:state :t :weight 5}]
               :t []}
        cost {:exit (fn [{:keys [state]} successors]
                      {:cost (if (= state :t)
                               0
                               (apply min
                                      (map #(+ (:weight (:via %)) (:cost %))
                                           successors)))})}
        result (last (search/postorder graph [:s]
                                         {:decode-edge identity
                                          :extensions [cost]}))]
    (is (= -2 (:cost result)))))

(deftest relax-negative-edge-weights-and-detect-a-negative-cycle
  (let [nodes #{:s :a :b :t}
        edges [[:s :a 4] [:s :b 3] [:a :b -2]
               [:a :t 5] [:b :t 1]]
        relax-edge (fn [distances [from to weight]]
                     (if-some [from-distance (distances from)]
                       (update distances to
                               #(if (some? %)
                                  (min % (+ from-distance weight))
                                  (+ from-distance weight)))
                       distances))
        bellman-ford
        (fn [vertices edges]
          (let [relax (fn [distances]
                        (reduce relax-edge distances edges))
                distances (nth (iterate relax {:s 0})
                               (dec (count vertices)))
                negative-cycle?
                (some (fn [[from to weight]]
                        (when-some [from-distance (distances from)]
                          (when-some [to-distance (distances to)]
                            (< (+ from-distance weight) to-distance))))
                      edges)]
            (if negative-cycle? :negative-cycle distances)))]
    (is (= {:s 0 :a 4 :b 2 :t 3} (bellman-ford nodes edges)))
    (is (= :negative-cycle
           (bellman-ford #{:s :a :b}
                         [[:s :a 1] [:a :b -2] [:b :a 1]])))))

(deftest minimize-the-largest-climb-on-a-route
  (let [heights [[1 2 2] [3 8 2] [5 3 5]]
        grid-climbs (fn [[row col]]
                      (for [[next-row next-col]
                            [[(dec row) col] [(inc row) col]
                             [row (dec col)] [row (inc col)]]
                            :let [here (get-in heights [row col])
                                  there (get-in heights [next-row next-col])]
                            :when there]
                        {:state [next-row next-col]
                         :climb (abs (- there here))}))
        effort-extension
        (search/accumulate :effort (constantly 0)
                           (fn [effort transition]
                             (max effort (:climb transition))))
        route (first (filter #(= [2 2] (:state %))
                             (search/best-first grid-climbs [[0 0]]
                                                {:decode-edge identity
                                                 :extensions [effort-extension]
                                                 :priority :effort})))]
    (is (= {:state [2 2] :effort 2}
           (select-keys route [:state :effort])))))

(deftest find-a-route-matching-a-label-pattern
  (let [labelled-graph
        {:start [{:to :middle :label "a"}
                 {:to :wrong :label "b"}]
         :middle [{:to :target :label "b"}]
         :target [{:to :middle :label "a"}]
         :wrong [{:to :target :label "a"}]}
        matching-edges
        (fn [[node phase]]
          (for [{:keys [to label]} (labelled-graph node)
                :let [next-phase
                      (case [phase label]
                        [:need-a "a"] :need-b
                        [:need-b "b"] :accept
                        [:accept "a"] :need-b
                        nil)]
                :when next-phase]
            {:state [to next-phase] :label label}))
        route (first (filter #(= [:target :accept] (:state %))
                             (search/bfs matching-edges [[:start :need-a]]
                                         {:decode-edge identity
                                          :extensions [search/trail-extension]})))]
    (is (= ["a" "b"] (mapv :label (rest (:trail route)))))))

(deftest two-travelers-meet-in-minimum-elapsed-time
  (let [meeting-roads {:west [[:ridge 2]]
                       :ridge [[:west 2] [:town 2] [:bridge 5]]
                       :east [[:market 1]]
                       :market [[:east 1] [:town 2] [:bridge 4]]
                       :town [[:ridge 2] [:market 2]]
                       :bridge [[:ridge 5] [:market 4]]}
        simultaneous-moves
        (fn [[city-a city-b]]
          (for [[next-a time-a] (meeting-roads city-a)
                [next-b time-b] (meeting-roads city-b)]
            {:state [next-a next-b]
             :weight (max time-a time-b)}))
        result (first (filter (fn [{[city-a city-b] :state}]
                                (= city-a city-b))
                              (search/best-first
                               simultaneous-moves
                               [[:west :east]]
                               {:decode-edge identity
                                :extensions [search/path-extension]})))]
    (is (= {:path [[:west :east] [:ridge :market] [:town :town]]
            :distance 4}
           (select-keys result [:path :distance])))))

(deftest choose-shortest-topic-covering-presentation
  (let [required-topics #{:welcome :robots :navigation}
        clips [{:id :short-welcome :duration 2 :topics #{:welcome}}
               {:id :robot-navigation :duration 5
                :topics #{:robots :navigation}}
               {:id :robot-basics :duration 4 :topics #{:robots}}
               {:id :navigation-demo :duration 2 :topics #{:navigation}}
               {:id :full-introduction :duration 8
                :topics #{:welcome :robots}}]
        add-clip (fn [{:keys [remaining selected]}]
                   (when-let [topic (first (sort remaining))]
                     (for [{:keys [id duration topics]} clips
                           :when (contains? topics topic)]
                       {:state {:remaining (reduce disj remaining topics)
                                :selected (conj selected id)}
                        :weight duration})))
        presentation (first (filter #(empty? (get-in % [:state :remaining]))
                                    (search/best-first
                                     add-clip
                                     [{:remaining required-topics :selected []}]
                                     {:decode-edge identity})))]
    (is (= 7 (:distance presentation)))
    (is (= #{:short-welcome :robot-navigation}
           (set (get-in presentation [:state :selected]))))))

(deftest catch-scheduled-transit-using-current-arrival-time
  (let [timetable {:home [{:to :junction :departs 0 :arrives 10}
                          {:to :museum :departs 0 :arrives 25}]
                   :junction [{:to :museum :departs 15 :arrives 20}]
                   :museum []}
        catchable-trips
        (fn [{:keys [state distance]}]
          (for [{:keys [to departs arrives]} (timetable state)
                :when (<= distance departs)]
            [to (- arrives distance)]))
        route (first (filter
                      #(= :museum (:state %))
                      (search/best-first
                       catchable-trips [:home]
                       {:project identity
                        :decode-edge (fn [[state weight]]
                                       {:state state :weight weight})
                        :extensions [search/path-extension]})))]
    (is (= {:path [:home :junction :museum] :distance 20}
           (select-keys route [:path :distance])))))

(deftest retain-labelled-transitions-along-a-route
  (let [labelled-travel {:home [[:museum 11 :express]
                                [:village 4 :ferry]
                                [:park 1 :tram]]
                         :village [[:museum 7 :bus]
                                   [:hill 2 :coach]]
                         :park [[:museum 8 :taxi]
                                [:village 1 :foot]]
                         :hill [[:museum 2 :rail]]
                         :museum []}
        route (first (filter
                      #(= :museum (:state %))
                      (search/best-first
                       labelled-travel [:home]
                       {:decode-edge (fn [[state weight mode]]
                                       {:state state :weight weight :mode mode})
                        :extensions [search/trail-extension]})))]
    (is (= {:trail [{:state :home}
                    {:state :park :weight 1 :mode :tram}
                    {:state :village :weight 1 :mode :foot}
                    {:state :hill :weight 2 :mode :coach}
                    {:state :museum :weight 2 :mode :rail}]
            :distance 6}
           (select-keys route [:trail :distance])))))

(deftest best-first-can-use-a-consistent-a-star-estimate
  (let [delivery-routes {:home [[:hill 1] [:river 2] [:market 4]]
                         :hill [[:goal 5]]
                         :river [[:goal 2]]
                         :market [[:goal 2]]
                         :goal []}
        remaining-time {:home 3 :hill 4 :river 2 :market 2 :goal 0}
        route (first (filter
                      #(= :goal (:state %))
                      (search/best-first
                       delivery-routes [:home]
                       {:decode-edge (fn [[state weight]]
                                       {:state state :weight weight})
                        :extensions [search/path-extension
                                     search/distance-extension]
                        :priority (fn [{:keys [state distance]}]
                                    (+ distance (remaining-time state)))})))]
    (is (= {:path [:home :river :goal]
            :distance 4}
           (select-keys route [:path :distance])))))

(deftest reach-goal-with-context-dependent-fuel
  (let [fuel-routes {:home [[:junction 4] [:detour 1]]
                     :detour [[:junction 1]]
                     :junction [[:goal 3]]
                     :goal []}
        fuel-left (search/accumulate :fuel-left (constantly 5)
                                     (fn [fuel transition]
                                       (- fuel (:fuel-used transition))))
        affordable-routes (fn [{:keys [state fuel-left]}]
                            (filter (fn [[_ fuel-used]]
                                      (<= fuel-used fuel-left))
                                    (fuel-routes state)))
        visit (first (filter
                      #(= :goal (:state %))
                      (search/bfs affordable-routes [:home]
                                 {:project identity
                                  :visit-key (juxt :state :fuel-left)
                                  :decode-edge
                                  (fn [[state fuel-used]]
                                    {:state state :fuel-used fuel-used})
                                  :extensions [fuel-left
                                               search/path-extension]})))]
    (is (= {:path [:home :detour :junction :goal]
            :fuel-left 0}
           (select-keys visit [:path :fuel-left])))))

(deftest collect-samples-and-return-with-limited-fuel
  (let [sample-locations {[2 0] :amber, [2 2] :cobalt}
        station [1 1]
        base [0 0]
        capacity 4
        successors (fn [{:keys [position fuel collected]}]
                     (let [moves
                           (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                 :let [next [(+ (first position) dx)
                                             (+ (second position) dy)]]
                                 :when (and (every? #(<= 0 % 2) next)
                                            (pos? fuel))]
                             {:state {:position next
                                      :fuel (dec fuel)
                                      :collected
                                      (cond-> collected
                                        (sample-locations next)
                                        (conj (sample-locations next)))}})]
                       (if (and (= position station) (< fuel capacity))
                         (conj (vec moves)
                               {:state {:position position
                                        :fuel capacity
                                        :collected collected}})
                         moves)))
        initial {:position base :fuel capacity :collected #{}}
        result (first (filter #(and (= base (get-in % [:state :position]))
                                    (= #{:amber :cobalt}
                                       (get-in % [:state :collected])))
                              (search/bfs successors [initial]
                                          {:decode-edge identity
                                           :extensions
                                           [search/depth-extension]})))]
    (is (= 12 (:depth result)))))

(deftest order-build-tasks-after-prerequisites
  (let [task-prerequisites {:app [:lib :config]
                            :lib [:base]
                            :config [:base]
                            :base []}]
    (is (= [:base :lib :config :app]
           (mapv :state
                 (search/postorder task-prerequisites [:app]))))))

(deftest schedule-ordered-preparation-with-postorder-next
  (let [prep-minutes {:chop-vegetables 4
                      :mix-dressing 3
                      :plate-food 2}
        prep-schedule
        {:next (fn [parent _transition _visit]
                 {:starts-at (reduce + 0
                                     (map #(prep-minutes (:state %))
                                          (:previous parent)))})}
        graph {:lunch [:chop-vegetables :mix-dressing :plate-food]
               :chop-vegetables []
               :mix-dressing []
               :plate-food []}]
    (is (= [[:chop-vegetables 0] [:mix-dressing 4] [:plate-food 7]]
           (->> (search/postorder graph [:lunch]
                                    {:extensions [prep-schedule]})
                (filter #(contains? % :starts-at))
                (mapv (juxt :state :starts-at)))))))

(deftest restore-from-first-available-copy-with-postorder-more
  (let [available-copies #{:local-copy}
        restore-extension
        {:more? (fn [{:keys [state]} previous]
                  (or (not= state :restore)
                      (not-any? :available? previous)))
         :exit (fn [{:keys [state]} successors]
                 (if (= state :restore)
                   {:source (:state (some #(when (:available? %) %) successors))}
                   {:available? (contains? available-copies state)}))}
        graph {:restore [:local-copy :network-copy]
               :local-copy []
               :network-copy []}]
    (is (= [{:state :local-copy :available? true}
            {:state :restore :source :local-copy}]
           (mapv #(select-keys % [:state :available? :source])
                 (search/postorder graph [:restore]
                                     {:extensions [restore-extension]}))))))

(deftest count-prerequisites-bottom-up-with-postorder-exit
  (let [task-count {:exit (fn [{:keys [state]} prerequisites]
                            (let [included
                                  (into #{}
                                        (mapcat #(conj (:included %) (:state %)))
                                        prerequisites)]
                              {:included (conj included state)
                               :size (inc (count included))}))}
        task-prerequisites {:app [:compile :docs]
                            :compile [:parse]
                            :docs [:parse]
                            :parse []}]
    (is (= [[:parse 1] [:compile 2] [:docs 2] [:app 4]]
           (mapv (juxt :state :size)
                 (search/postorder task-prerequisites [:app]
                                     {:extensions [task-count]}))))))

(deftest count-shortest-routes-by-composing-bfs-and-postorder
  (let [route-graph {:a [:b :c :f]
                     :b [:d]
                     :c [:d]
                     :d [:e]
                     :f [:g]
                     :g [:h]
                     :h [:e]
                     :e []}
        depth (into {} (map (juxt :state :depth))
                    (search/bfs route-graph [:a]
                                {:extensions [search/depth-extension]}))
        shortest-edges (fn [state]
                         (filter #(= (inc (depth state)) (depth %))
                                 (route-graph state)))
        count-routes
        {:exit (fn [{:keys [state]} successors]
                 {:count (if (= state :e)
                           1
                           (transduce (map :count) + 0 successors))})}
        visits (search/postorder shortest-edges [:a]
                                   {:extensions [count-routes]})]
    (is (= 2 (:count (last visits))))))

(deftest count-ways-to-break-a-word-into-dictionary-words
  (let [text "catsanddog"
        words #{"cat" "cats" "and" "sand" "dog"}
        next-cuts (fn [start]
                    (for [end (range (inc start) (inc (count text)))
                          :when (contains? words (subs text start end))]
                      end))
        count-ways
        {:exit (fn [{start :state} suffixes]
                 {:ways (if (= start (count text))
                          1
                          (transduce (map :ways) + 0 suffixes))})}
        result (last (search/postorder next-cuts [0]
                                         {:extensions [count-ways]}))]
    (is (= 2 (:ways result)))))

(deftest find-tic-tac-toe-value-under-perfect-play
  (let [winning-lines [[0 1 2] [3 4 5] [6 7 8]
                       [0 3 6] [1 4 7] [2 5 8]
                       [0 4 8] [2 4 6]]
        board-winner
        (fn [board]
          (some (fn [[a b c]]
                  (when (and (board a) (= (board a) (board b) (board c)))
                    (board a)))
                winning-lines))
        legal-moves
        (fn [board]
          (when-not (board-winner board)
            (let [mark (if (= (count (filter #{:x} board))
                              (count (filter #{:o} board)))
                         :x :o)]
              (for [i (range 9) :when (nil? (board i))]
                (assoc board i mark)))))
        game-value
        {:exit (fn [{board :state} children]
                 (let [winner (board-winner board)]
                   {:value (cond
                             (= :x winner) 1
                             (= :o winner) -1
                             (empty? children) 0
                             (even? (count (remove nil? board)))
                             (apply max (map :value children))
                             :else (apply min (map :value children)))}))}
        positions (search/postorder legal-moves
                                      [(vec (repeat 9 nil))]
                                      {:extensions [game-value]})]
    (is (= 5478 (count positions)))
    (is (= 0 (:value (last positions))))))

(deftest report-the-edge-that-closes-a-dependency-cycle
  (let [edge-report
        {:exit (fn [_visit successors]
                 {:edges (mapv (fn [{:keys [state via back-edge]}]
                                 [state (:kind via) back-edge])
                               successors)})}]
    (is (= [[:database [[:api :requires true]]]
            [:api [[:database :requires false]]]]
           (mapv (juxt :state :edges)
                 (search/postorder {:api [[:database :requires]]
                                      :database [[:api :requires]]}
                                     [:api]
                                     {:decode-edge (fn [[state kind]]
                                                     {:state state :kind kind})
                                      :extensions [edge-report]}))))))

(deftest reaching-definitions-propagate-through-a-loop
  (let [blocks
        {:entry {:successors [:header]
                 :gen #{[:x :initial]}
                 :kill #{}}
         :header {:successors [:body :exit]
                  :gen #{}
                  :kill #{}}
         :body {:successors [:header]
                :gen #{[:x :loop]}
                :kill #{[:x :initial] [:x :loop]}}
         :exit {:successors []
                :gen #{}
                :kill #{}}}
        transfer (fn [{:keys [state value]}]
                   (let [{:keys [successors gen kill]} (blocks state)
                         outgoing (into gen (remove kill value))]
                     (map (fn [target]
                            {:state target :value outgoing})
                          successors)))
        progress (search/propagate transfer
                                   {:entry #{}}
                                   {:project identity
                                    :decode-edge identity
                                    :bottom #{}
                                    :join set/union})]
    (is (= {:entry #{}
            :header #{[:x :initial] [:x :loop]}
            :body #{[:x :initial] [:x :loop]}
            :exit #{[:x :initial] [:x :loop]}}
           (:values (last progress))))
    (is (= 2 (count (filter #(= :header (:processed %)) progress))))))
