(ns dobutsu-shogi.core
  (:gen-class)
  (:use [clojure.repl]))

(defn height [board]
  (count board))         ; 4

(defn width [board]
  (count (nth board 0))) ; 3

(defn drop-last-first [col]
  (drop 1 (drop-last col)))

(defn nnth
  ([col i j]   (nth (nth col i) j))
  ([col i j k] (nth (nnth col i j) k)))

(defn update-nnth
  ([col i j x]
   (let [p (nth col i)]
    (assoc col i (assoc p j x)))))

(defn toggle[col]
  (map
    (fn [x] (update-in  x [0] #(- 0 %)))
    col))

(defn fmap [m f]
  (into {} (for [[k v] m] [k (f v)])))

(def animals
  "movable direction [i,j]"
  (let [b {:lion [[-1 -1] [+1 +1] [-1 +1]
                  [+1 -1] [-1 0] [+1 0] [0 -1] [0 +1]]
           :gir  [[+1 0] [-1 0] [0 +1] [0 -1]]
           :ele  [[+1 +1] [-1 -1] [+1 -1] [-1 +1]]
           :chi  [[-1 0]]
           :for  [[-1 0] [-1 -1] [-1 +1] [0 -1] [0 +1] [+1 0]]}]
    {:a (fmap b toggle),
     :b b}))

(defn out-of-board? [i j board]
  (or (< i 0)
      (< j 0)
      (>= i (height board))
      (>= j (width board))))

(defn empty-cell? [board i j]
  (nil? (nnth board i j)))

(defn can-move? [board i j turn]
  (and (not (out-of-board? i j board))
       (not (= (nnth board  i j 1) turn))))

(defn movable [board oldi oldj turn]
  "return movable positions, return keyword if cannot move"
  (cond (out-of-board? oldi oldj board)
        :out-of-board
        (empty-cell? board oldi oldj)
        :no-aminal
        (not (= (nnth board oldi oldj 1) turn))
        :not-yours
        :else
        (filter #(can-move? board (first %) (second %) turn)
                (map #(map + [oldi oldj] %)
                     ((animals turn)
                      (nnth board oldi oldj 0))))))

(defn move [board oldi oldj newi newj turn]
  "return board, and get animal, return false if cannot move"
  (let [movables (movable board oldi oldj turn)
        s        (nnth board oldi oldj)
        selected (if (and (= (first s) :chi)
                          (or (and (= (second s) :b) (= newi 0) (= oldi 1))
                              (and (= (second s) :a) (= newi 3) (= oldi 2))))
                   [:for (second s)]
                   s)]
    (if (and (not (keyword? movables))
             (some #(= % [newi newj]) movables))
      {:board (update-nnth (update-nnth board newi newj selected) oldi oldj nil)
       :get (nnth board newi newj 0)}
      false)))

(defn put [board i j animal turn]
  "return board, return false if cannot put"
  (cond (and (not (out-of-board? i j board))
             (empty-cell? board i j))
        (update-nnth board i j [animal turn])
        :else
        false))

(defn random-pick [col]
  (if (empty? col)
    []
    (nth col (int (rand (count (vec col)))))))

(defn ai-random [board turn]
  "return randam hand [[oldi oldj] [newi newj]]"
  (random-pick (filter #(and (not-empty %)
                             (not-empty (second %)))
                       (for [i (range 4) j (range 3)]
                         (let [p (nnth board i j)]
                           (if (and (not (nil? p))
                                    (= (second p) turn))
                             [[i j] (random-pick (movable board i j turn))]
                             []
                             ))))))

(def test-board [[[:gir :a] [:lion :a] [:ele :a]]
                 [nil       [:chi  :a] nil      ]
                 [nil       [:for  :b] nil      ]
                 [[:ele :b] [:lion :b] [:gir :b]]
                 ])

(ai-random test-board :a)

(random-pick (movable test-board 2 1 :b))
(nnth test-board 3 2)
(nnth test-board 3 2 1)
(move test-board 3 1 2 2 :b)
(put  test-board 1 0 :lion :a)

(println "莟")
(println animals)
(println (movable test-board 3 2 :b))
(println (movable test-board 4 1 :b))
(println (movable test-board 4 2 :b))
(println (movable test-board 4 3 :b))
(println (movable test-board 2 1 :b))
;(println (map #(map + [1 2] %) [[2 3][4 5]]))

;(clojure.repl/find-doc "doc")
;(clojure.repl/doc ns)
;(doc nth)
;(find-doc "in")
