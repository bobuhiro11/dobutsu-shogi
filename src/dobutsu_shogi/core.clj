(ns dobutsu-shogi.core
  (:gen-class)
  (:require
            [dobutsu-shogi.analysis :as da])
  (:use [clojure.repl]
        [clojure.tools.trace]))

(def ^:const height 4)
(def ^:const width  3)
(def ^:const negamax-depth  4)

(def ^:const bin-init-board 189874330207042)
(def ^:const bin-init-hands 2r0)

(def ^:const chick    2r001)
(def ^:const giraffe  2r010)
(def ^:const elephant 2r011)
(def ^:const lion     2r100)
(def ^:const fowl     2r101)

(def ^:const turn-a 2r0000)
(def ^:const turn-b 2r1000)

(def bin-animals
  "movable direction [i,j]"
  (let [b {(bit-or turn-b lion)     [[-1 -1] [+1 +1] [-1 +1]
                                     [+1 -1] [-1 0] [+1 0] [0 -1] [0 +1]]
           (bit-or turn-b giraffe)  [[+1 0] [-1 0] [0 +1] [0 -1]]
           (bit-or turn-b elephant) [[+1 +1] [-1 -1] [+1 -1] [-1 +1]]
           (bit-or turn-b chick)    [[-1 0]]
           (bit-or turn-b fowl)     [[-1 0] [-1 -1] [-1 +1] [0 -1] [0 +1] [+1 0]]}]
    (into {} (concat
               b
               (map (fn [[k v]]
                      [(bit-xor k 2r1000)
                       (map (fn [x] [(- (first x)) (second x)]) v)])
                    b)))))

(defn random-pick [col]
  "get random element from col"
  (if (empty? col)
    []
    (nth col (int (rand (count (vec col)))))))

(defn out-of-board? [^long i ^long j]
  "return true if (i,j) is out of board, false othewise."
  (or (< i 0)
      (< j 0)
      (>= i height)
      (>= j width)))

(defn bin-get-cell [^long board ^long i ^long j]
  "get a cell whose position is (i,j)."
  (bit-and 2r1111
           (bit-shift-right board (* 4 (+ (* width i) j)))))

(defn bin-set-cell [^long board ^long i ^long j ^long cell]
  "set a cell whose position is (i,j), and return new borad."
  (let [idx ^long (* 4 (+ (* width i) j))]
    (bit-or (bit-and (bit-xor (bit-shift-left 2r1111 idx)
                              16rFFFFFFFFFFFF)
                     board)
            (bit-shift-left cell idx))))

(defn bin-set-hands [^long hands ^long idx ^long cell]
  "set a hand whose index is idx, and return new hands."
  (bit-or (bit-and (bit-xor (bit-shift-left 2r111 idx)
                            16rFFFFFFFFFFFFFFF)
                   hands)
          (bit-shift-left cell idx)))

(defn bin-get-hands [^long hands ^long idx]
  "get a hand whose index is idx."
  (bit-and 2r111 (bit-shift-right hands idx)))

(defn bin-add-hands [^long hands ^long cell ^long turn]
  "add cell into hands. a input cell is 3bit. a input turn is 2r0000 or 21000."
  (let [mask-cell ^long (and 2r111 cell)
        c         ^long (if (= mask-cell fowl) chick mask-cell)]
    (loop [idx    ^long (if (= turn-b (bit-and 2r1000 turn)) 21 0)]
      (if (= (bin-get-hands hands idx) 2r000)
        (bin-set-hands hands idx c)
        (recur (+ idx 3))))))

(defn count-hands [^long hands ^long turn]
  "count hands whose turn is a input turn."
  (count (filter #(not (zero? %))
                 (for [i (range 7)]
                   (bin-get-hands
                     hands
                     (if (= turn turn-b)
                       (+ 21 (* 3 i))
                       (* 3 i)))))))

(defn bin-can-move? [^long board i j ^long turn]
  "return true if *turn* player can put animal at (i,j)."
  (and (not (out-of-board? i j))
       (let [cell (bin-get-cell board i j)]
         (or (= cell 2r0000)
             (not= (bit-and cell 2r1000) turn)))))

(defn bin-movable [^long board oldi oldj ^long turn]
  "return movable positions, return keyword if cannot move"
  (if (out-of-board? oldi oldj)
    :out-of-board
    (let [cell (bin-get-cell board oldi oldj)]
      (cond
        (= 2r0000 cell)
        :no-aminal
        (not= (bit-and cell 2r1000) turn)
        :not-yours
        :else
        (filter (fn [[i j]] (bin-can-move? board i j turn))
                (map (fn [[i j]] [(+ i oldi) (+ j oldj)]) (bin-animals (bin-get-cell board oldi oldj))))))))

(defn cell->binary ^long [cell]
  (if cell
    (bit-or (condp = (first cell)
              :chi  chick
              :gir  giraffe
              :ele  elephant
              :lion lion
              :for  fowl)
            (condp = (second cell)
              :a turn-a
              :b turn-b))
    2r0000))

(defn board->binary ^long [board]
  (reduce bit-or
          (for [i (range height) j (range width)]
            (bit-shift-left
              (let [cell (get-in board [i j])]
                (cell->binary cell))
              (* 4 (+ (* width i) j))))))

(defn binary->cell [^long bin]
  (if (= bin 2r0000)
    nil
    [(condp = (bit-and bin 2r111)
       chick :chi
       giraffe :gir
       elephant :ele
       lion :lion
       fowl :for)
     (condp =  (bit-and bin 2r1000)
       turn-a :a
       turn-b :b)]))

(defn binary->board [^long board]
  (for [i (range height) j (range width)]
    (let [b (bit-and 2r1111 (bit-shift-right board (* 4  (+ (* width i) j))))]
      (binary->cell b))))

(defn try? [^long board i j ^long turn]
  "lion i j is try success or not"
  (if (not= (bin-get-cell board i j) (+ lion turn))
    false
    (let [near-cells
          (filter (fn [[i j cell]] (and (> cell 2r0000)
                                        (not= (bit-and cell 2r1000) turn)))
                  [[(+ i  0) (+ j +1) (bin-get-cell board (+ i  0) (+ j +1))]
                   [(+ i  0) (+ j -1) (bin-get-cell board (+ i  0) (+ j -1))]
                   [(+ i +1) (+ j  0) (bin-get-cell board (+ i +1) (+ j  0))]
                   [(+ i +1) (+ j +1) (bin-get-cell board (+ i +1) (+ j +1))]
                   [(+ i +1) (+ j -1) (bin-get-cell board (+ i +1) (+ j -1))]
                   [(+ i -1) (+ j  0) (bin-get-cell board (+ i -1) (+ j  0))]
                   [(+ i -1) (+ j +1) (bin-get-cell board (+ i -1) (+ j +1))]
                   [(+ i -1) (+ j -1) (bin-get-cell board (+ i -1) (+ j -1))]])
          ]
      (every?
        (fn [[ni nj]]
          (or (not= i ni) (not= j nj)))
        (apply concat (filter #(not (keyword %)) (map (fn [[i j]] (bin-movable board i j (bit-xor turn 2r1000))) near-cells)))))))

(mapcat (fn [x] [[1 2]]) (range 10))

(defn bin-winner [^long board ^long hands]
  "return 2rx000 if end game, -1 othewise."
  (cond
    ;; get lion
    (some #(= % 2r100) (for [i (range 7)] (bin-get-hands hands (* 3 i))))
    turn-a
    (some #(= % 2r100) (for [i (range 7)] (bin-get-hands hands (+ 21 (* 3 i)))))
    turn-b
    ;; try
    (or
      (try? board 3 0 turn-a)
      (try? board 3 1 turn-a)
      (try? board 3 2 turn-a))
    turn-a
    (or
      (try? board 0 0 turn-b)
      (try? board 0 1 turn-b)
      (try? board 0 2 turn-b))
    turn-b
    :else
    -1))

(defn bin-show-hands [^long hands]
  (let [r
        {turn-a (for [i (range 7)]
                  (bin-get-hands hands (* 3 i)))
         turn-b (for [i (range 7)]
                  (bin-get-hands hands (+ 21 (* 3 i))))}]
    (doall (map (fn [[k v]]
                  (print k ":")
                  (doall (map (fn [^long x]
                                (print (condp = x
                                         2r000    " "
                                         chick    "C"
                                         giraffe  "G"
                                         elephant "E"
                                         lion     "L"
                                         fowl     "F")))
                              v))
                  (println)
                  ) r))
    (flush)))

(defn bin-show-board [^long board]
  (println "board=" board)
  (doall (for [^long i (range height)]
           (do (doall (for [^long j (range width)]
                        (let [cell ^long (bin-get-cell board i j)]
                          (if (= 2r0000 cell)
                            (print "  ")
                            (do
                              (print (bit-and 2r1000 cell))
                              (print (condp = (bit-and 2r111 cell)
                                       chick    "C"
                                       giraffe  "G"
                                       elephant "E"
                                       lion     "L"
                                       fowl     "F"))
                              ))
                          (print " ")
                          (flush))))
               (println)
               (flush)))))

(defn bin-hand-indexs [^long board ^long hands ^long turn]
  "return indexs of turn's hand"
  (let [bias (if (= turn 2r1000) 21 0)]
    (filter #(not= % nil)
            (for [i (range 7)]
              (if (not= (bin-get-hands hands (+ bias (* 3 i))) 0)
                i
                nil)))))

(defn bin-move [^long board [oldi oldj] [newi newj] ^long turn]
  "return board, and get animal, return false if cannot move"
  (let [movables (bin-movable  board oldi oldj turn)
        s        (bin-get-cell board oldi oldj)
        selected (if (and (= (bit-and s 2r111) chick)
                          (or (and (= (bit-and s 2r1000) turn-b) (= newi 0))
                              (and (= (bit-and s 2r1000) turn-a) (= newi 3))))
                   (+ s 2r100)
                   s)]
    (if (and (not (keyword? movables))
             (some #(= % [newi newj]) movables))
      {:board (bin-set-cell (bin-set-cell board newi newj selected) oldi oldj 2r0000)
       :get (bit-and 2r111 (bin-get-cell board newi newj))}
      false)))

(defn bin-ai-random [^long board ^long hands ^long turn]
  "return randam hand [:move [oldi oldj] [newi newj] value]"
  (let [result (filter #(and (not-empty %)
                             (not-empty (nth % 2)))
                       (for [i (range height) j (range width)]
                         (let [p (bin-get-cell board i j)]
                           (if (and (not= p 2r0000)
                                    (= (bit-and p 2r1000) turn))
                             [:move [i j] (random-pick (bin-movable board i j turn)) 0]
                             []
                             ))))
        all-hand-indexs
        (bin-hand-indexs board hands turn)
        puts-result
        (apply concat
               (for [i (range height) j (range width)]
                 (if (= 2r0000 (bin-get-cell board i j))
                   (map (fn [index]
                          [:put index [i j] 0]
                          )
                        all-hand-indexs))))
        ]
    (if (empty? puts-result)
      (random-pick result)
      (random-pick puts-result))))

(defn evaluate [^long board ^long hands]
  "return plus if turn 2r1000 is win."
  (let [winner ^long (bin-winner board hands)]
    (cond (= winner turn-b)
          2000
          (= winner turn-a)
          -2000
          :else
          (let [
                board-value ^long
                (reduce +
                        (map (fn [^long cell ^long i]
                               (if (= (bit-and cell 2r1000) turn-b)
                                 (condp = (bit-and cell 2r111)
                                   lion     (+ 50 (- 3 i))
                                   fowl     4
                                   giraffe  3
                                   elephant 3
                                   chick    1
                                   0)
                                 (condp = (bit-and cell 2r111)
                                   lion     (- -50 i)
                                   fowl     -4
                                   giraffe  -3
                                   elephant -3
                                   chick    -1
                                   0)))
                             (for [i (range height) j (range width)]
                               (bin-get-cell board i j))
                             (for [i (range height) j (range width)]
                               i)))
                myhands ^long
                (reduce +
                        (map (fn [^long cell]
                               (condp = cell
                                 giraffe  3
                                 elephant 3
                                 chick    1
                                 0))
                             (for [i (range 7)]
                               (bin-get-hands hands (+ 21 (* 3 i))))))
                yourhands ^long
                (reduce +
                        (map (fn [^long cell]
                               (condp = cell
                                 giraffe  -3
                                 elephant -3
                                 chick    -1
                                 0))
                             (for [i (range 7)]
                               (bin-get-hands hands (* 3 i)))))]
            (+ board-value myhands yourhands)))))

(defn evaluate2 [^long board ^long hands]
  "return plus if turn 2r1000 is win."
  (let [winner ^long (bin-winner board hands)
        our-movable (filter
                      (fn [[i j]]
                        (not= 2r0000 (bin-get-cell board i j)))
                     (apply concat
                           (filter
                             (fn [x] (and
                                       (not (keyword? x))))
                             (for [i (range height) j (range width)]
                               (bin-movable board i j turn-b)))))
        ]
    our-movable))

(defn bin-all-moves [^long board ^long hands ^long turn]
  "return {[2 1] ([1 1]), [3 1] ([2 2] [2 0]), [3 2] ([2 2]), [oldi oldj] ([ni nj] [ni nj])}
  key: [oldi oldj]
  value: ( [newi newj] [newi newj] ... )"
  (into {} (filter #(not (nil? %))
                   (for [i (range 4) j (range 3)]
                     (let [cell (bin-get-cell board i j)]
                       (cond (= 2r0000 cell)
                             nil ; cell not exist
                             :else
                             (let [movables (bin-movable board i j turn)] ; [[2 2] [2 0]]
                               (cond (or (keyword? movables) (empty? movables))
                                     nil
                                     :else
                                     {[i j] movables}))))))))

(defn find-children [^long hands ^long turn ^long board]
  (let [all-moves (bin-all-moves board hands turn)
        all-hand-indexs (bin-hand-indexs board hands turn)
        moves-result
        (->> all-moves
             (mapcat (fn [[old v]]
                       (->> v
                            (map (fn [new]
                                   (let [result (bin-move board old new turn)]
                                     (if (= result false)
                                       nil
                                       (if (= (:get result) 0)
                                         [(:board result) hands]
                                         [(:board result) (bin-add-hands hands (:get result) turn)])))))
                            (filter (fn [x] (not (nil? x))))))))
        puts-result
        (apply concat
               (for [i (range 4) j (range 3)]
                 (if (= 2r0000 (bin-get-cell board i j))
                   (map (fn [index]
                          [(bin-set-cell board i j (bin-get-hands hands (+ (* 3 index) (if (= 2r1000 turn) 21 0))))
                           (bin-set-hands hands (+ (* 3 index) (if (= 2r1000 turn) 21 0)) 2r000)
                           ]
                          )
                        all-hand-indexs))))
        ]
    (concat moves-result
            puts-result)))

(defn negamax2 [^long board ^long hands ^long color ^long depth]
  (cond (= 1 depth)
        (* color (evaluate board hands))
        (not= (bin-winner board hands) -1)
        (* color (evaluate board hands))
        :else
        (apply max
               (map #(- (negamax2 (first %) (second %) (- color) (dec depth)))
                    (find-children hands (if (= color 1) turn-b turn-a) board)))))

(defn negamax [board hands alpha beta color depth]
  (cond (or (= 1 depth) (not= (bin-winner board hands) -1))
        (* color (evaluate board hands))
        :else
        (->> board
             (find-children hands (if (= color 1) turn-b turn-a))
             (map #(- (negamax (first %) (second %) (- beta) (- alpha) (- color) (dec depth))))
             (take-while #(< % beta))
             (cons alpha)
             (apply max))))

(defn negamax3 [^long board ^long hands ^long alpha ^long beta]
  (let [color ^long (if (= (bit-and 2r1 (bit-shift-right board 48)) 1) 1 -1)
        depth ^long (bit-and 2r111 (bit-shift-right board 49))
        next-color ^long (if (= color 1) 2r0 2r1)
        b ^long (bit-and 16rFFFFFFFFFFFF board)
        ]
    (cond (or (= 1 depth) (not= (bin-winner board hands) -1))
          (* color (evaluate board hands))
          :else
          (->> board
               (find-children hands (if (= color 1) turn-b turn-a))
               (map #(- (negamax3 (bit-or (first %) 
                                          (bit-shift-left next-color 48)
                                          (bit-shift-left (dec depth) 49))
                                  (second %)
                                  (- beta)
                                  (- alpha))))
               (take-while #(< % beta))
               (cons alpha)
               (apply max)))))


(defn bin-ai-negamx [^long board ^long hands ^long turn]
  (let [all-moves (bin-all-moves board hands turn)
        all-hand-indexs (bin-hand-indexs board hands turn)
        color ^long (if (= turn turn-b) -1 +1)
        result
        (mapcat (fn [[old v]]
                  (map (fn [new]
                         (let [result (bin-move board old new turn)]
                           (if (= result false)
                             nil
                             (if (= (:get result) 0)
                               [:move old new (- (negamax (:board result) hands                                    -10000 10000 color negamax-depth))]
                               [:move old new (- (negamax (:board result) (bin-add-hands hands (:get result) turn) -10000 10000 color negamax-depth))]
                               ))))
                       v)
                  ) all-moves)
        puts-result
        (apply concat
               (for [i (range height) j (range width)]
                 (if (= 2r0000 (bin-get-cell board i j))
                   (map (fn [index]
                          [:put index [i j]
                           (- (negamax (bin-set-cell board i j (bin-get-hands hands (+ (* 3 index) (if (= turn-b turn) 21 0))))
                                       (bin-set-hands hands (+ (* 3 index) (if (= turn-b turn) 21 0)) 2r000)
                                       -10000 10000 color negamax-depth))])
                        all-hand-indexs))))
        ]
    (last (sort-by (fn [[op old new value]] value)
                   (concat puts-result result)))))

(defn game [^long human-turn]
  (letfn [(game1 [^long board
                  ^long hands
                  ^long turn
                  ^long n ]
            (println "--------------------------------------")
            (println "                     TURN: " (bit-and turn 2r1000) "(" n ")")
            (bin-show-board board)
            (println)
            (bin-show-hands hands)
            (cond (> n 50)
                  (println "                     END")
                  (not= -1 (bin-winner board hands))
                  (println "                     WINNER:" (bin-winner board hands))
                  :else
                  (let [ai-result (if (= human-turn turn)
                                    (da/bin-ai-victory board hands turn)
                                    (bin-ai-random board hands turn)
                                    ;(bin-ai-negamx board hands turn)
                                    )]
                    (println "ai-result: " ai-result)
                    (if (= (first ai-result) :move)
                      ;;;; move
                      (let [
                            move-pos (rest ai-result)
                            result (bin-move board (first move-pos) (second move-pos) turn)
                            new-board (long (:board result))
                            new-hands (long (if (:get result)
                                              (bin-add-hands hands (:get result) turn)
                                              hands))]
                        (println "                     move from" (first move-pos) "to" (second move-pos) " evaluate value:" (second (rest move-pos)))
                        (println)
                        (recur new-board new-hands (bit-xor turn turn-b) (inc n)))
                      ;;;; put
                      (let [
                            put-pos (rest ai-result)
                            index (first put-pos)
                            [i j]  (second put-pos)
                            value (second (rest put-pos))
                            new-board (long (bin-set-cell board i j (bin-get-hands hands (+ (* 3 index) (if (= turn-b turn) 21 0)))))
                            new-hands (long (bin-set-hands hands (+ (* 3 index) (if (= turn-b turn) 21 0)) 2r000))
                            ]
                        (println "                     put from" index "to" [i j] " evaluate value:" value)
                        (println)
                        (recur new-board new-hands (bit-xor turn turn-b) (inc n)))
                      )
                    )))]
    (game1 bin-init-board 2r0 2r0000 0)))

(defn bin-get-reverse [^long board ^long hands]
  {:board
  (reduce bit-or
          (for [i (range height) j (range width)]
            (bit-shift-left
              (let [cell (bin-get-cell board (- height i 1) (- width j 1))]
                (if (= cell 0)
                  cell
                  (bit-xor cell 2r1000)))
              (* 4 (+ (* width i) j)))))
   :hands
   (bit-or
     (bit-and (bit-shift-right hands 21) 0x1FFFFF)
     (bit-and (bit-shift-left  hands 21) 0x2FFFE0000))})

(comment
  (time (bin-movable bin-init-board 3 1 2r1000)) ; [2 2] [2 0]
  (time (game 2r0000))
  (bin-show-hands 0)
  (bin-show-board 1)
  (bin-show-board bin-init-board)
  (println (bin-ai-random (board->binary test-board) 2r0 2r1000))
  (board->binary test-board)
  (binary->board (:board (bin-move   (board->binary test-board) [3 1] [2 2] 2r1000)))
  (bin-move      (board->binary test-board) [3 1] [2 0] 2r1000)
  (bin-can-move? (board->binary test-board) 0 0 2r1000) ;true
  (bin-can-move? (board->binary test-board) 0 1 2r1000) ;true
  (bin-can-move? (board->binary test-board) 0 2 2r1000) ;true
  (bin-can-move? (board->binary test-board) 2 2 2r1000) ;true
  (bin-can-move? (board->binary test-board) 2 1 2r1000) ;false
  (binary->board (board->binary test-board))
  (println (try?
             (board->binary [[[:gir :b] [:lion :a] [:ele :a]]
                             [nil       [:chi  :a] nil      ]
                             [nil       [:chi  :b] nil      ]
                             [[:ele :b] [:lion  :a] [:gir :a]]
                             ])
             3 1 2r0000))
  (println (try?
             (board->binary [[[:gir :a] [:lion :a]   [:ele :a]]
                             [nil       nil          nil]
                             [[:chi :a] [:lion  :b]  nil      ]
                             [[:ele :b] nil          [:gir :b]]
                             ])
             3 2 2r0000))
  (println (bin-winner
             (board->binary [[[:gir :a] [:lion :b] [:ele :a]]
                             [nil       [:chi  :a] nil      ]
                             [nil       [:chi  :b] nil      ]
                             [[:ele :b] [:lion  :a] [:gir :a]]
                             ])
             2r000))
  (count-hands
    (-> 2r000
        (bin-add-hands chick 2r1000)
        (bin-add-hands fowl 2r0000)
        (bin-add-hands chick 2r1000)
        (bin-add-hands elephant 2r0000)
        )
    2r1000)
  (println (bin-ai-random (board->binary test-board) (->> 2r0
                                                          (bin-add-hands fowl 2r1000)
                                                          (bin-add-hands elephant 2r1000)) 2r1000))
  (println (bin-ai-random (board->binary test-board) 2r0 2r1000))
  (println (bin-all-moves 799070036736  (bin-add-hands bin-init-hands fowl 2r1000) 0))
  (bin-show-board 799070036736 )
  (time (negamax bin-init-board bin-init-hands -10000 10001 1 negamax-depth))
  (time (negamax bin-init-board bin-init-hands -10000 10001 -1 negamax-depth))
  (time (negamax3
          (bit-or bin-init-board
                  (bit-shift-left 1 48)
                  (bit-shift-left negamax-depth 49))
          bin-init-hands -10000 10001))
  (time (bin-ai-negamx bin-init-board 2r0 2r1000)) ; 50-70 ms
  (time (bin-ai-negamx bin-init-board 2r0 2r0000)) ; 50-70 ms
  (time (da/bin-ai-victory bin-init-board 2r0 2r0000)) ;
  (time (evaluate bin-init-board bin-init-hands))
  (time (evaluate2 bin-init-board bin-init-hands))
  (println (bin-ai-random (board->binary test-board) 2r0 2r1000))
  (println (bin-ai-negamx (board->binary test-board)
                          (->> 2r0
                               (bin-add-hands fowl 2r1000)
                               (bin-add-hands elephant 2r1000))
                          2r1000))
  (println (bin-get-cell bin-init-board 2 1))
  (bin-all-moves bin-init-board bin-init-hands 2r1000)
  (time (println (find-children bin-init-hands 2r1000 bin-init-board))) ; 1 ms
  (println (find-children
             (-> 2r000
                 (bin-add-hands chick 2r1000)
                 (bin-add-hands fowl 2r0000)
                 (bin-add-hands elephant 2r1000)
                 (bin-add-hands elephant 2r0000)
                 )
             2r1000
             bin-init-board
             ))
(let [init-board
      (bin-set-cell bin-init-board 1 2 (bit-or 2r1000 giraffe))
      init-hand
      (-> 2r000
          (bin-add-hands chick 2r1000)
          (bin-add-hands fowl 2r0000)
          (bin-add-hands elephant 2r1000)
          (bin-add-hands giraffe  2r1000)
          (bin-add-hands giraffe  2r1000)
          (bin-add-hands elephant 2r0000)
          )
      trans-board
      (:board (bin-get-reverse init-board init-hand))
      trans-hand
      (:hands (bin-get-reverse init-board init-hand))
      ]
  (bin-show-board init-board)
  (bin-show-hands init-hand)
  (println "-----")
  (bin-show-board trans-board)
  (bin-show-hands trans-hand)
  )
  )
