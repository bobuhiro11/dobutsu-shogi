(ns dobutsu-shogi.analysis
  (:gen-class)
  (:use     [clojure.repl])
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.timer :as st]
            [clojure.stacktrace :as cs]
            [dobutsu-shogi.core :as dc])
  (:import  [javax.swing ImageIcon JLabel]
           [javax.swing ImageIcon JButton]
           [java.awt Image BasicStroke Color]
           [java.awt.geom AffineTransform]
           [java.io RandomAccessFile]))

(defn abin-add-hand [^long abin ^long animal ^long belong]
  "解析局面 abin にどうぶつ animal を belong の持コマとして追加する
  abin, animal, belong は解析ソフトと統一
  belongは 1が手番, 2が手番ではない
  "
  (let [offset
        (condp = (bit-and animal 2r111)
          dc/chick    48
          dc/giraffe  52
          dc/elephant 56
          -1)]
    (if (= 2r00 (bit-and (bit-shift-right abin offset)
                         2r11))
      (bit-or abin
              (bit-shift-left (bit-and belong 2r11)
                              offset))
      (bit-or abin
              (bit-shift-left (bit-and belong 2r11)
                              (+ offset 2))))))

(defn bin->abin [^long board ^long hands ^long turn]
  "core.clj で定義された局面 board と持コマ hands ，そして turn から
  解析局面 abin に変換（正規化済）
  "
  (let [
        your-offset (if (= turn 2r1000) 0 21)
        my-offset   (if (= turn 2r1000) 21 0)
        your-hands (for [i (range 7)]
                     (dc/bin-get-hands hands (+ your-offset (* 3 i))))
        my-hands (for [i (range 7)]
                   (dc/bin-get-hands hands (+ my-offset (* 3 i))))
        my-kirin-num    (count (filter #(= % dc/giraffe)   my-hands))
        my-hiyoko-num   (count (filter #(= % dc/chick)     my-hands))
        my-zou-num      (count (filter #(= % dc/elephant)  my-hands))
        your-kirin-num  (count (filter #(= % dc/giraffe)   your-hands))
        your-hiyoko-num (count (filter #(= % dc/chick)     your-hands))
        your-zou-num    (count (filter #(= % dc/elephant)  your-hands))
        add-animal
        (fn [abin num animal belong]
          (cond (= num 2)
                (abin-add-hand (abin-add-hand abin animal belong) animal belong)
                (= num 1)
                (abin-add-hand abin animal belong)
                :else
                abin))
        board
        (reduce bit-or
                (for [i (range dc/height) j (range dc/width)]
                  (bit-shift-left
                    (let [cell (dc/bin-get-cell board i j)]
                      (if (= cell 2r0000)
                        cell
                        (bit-xor cell turn)))
                    (* 4 (+ (* dc/width (- dc/height i 1)) (- dc/width j 1))))))
        reverse-board
        (reduce bit-or
                (for [i (range dc/height) j (range dc/width)]
                  (bit-shift-left
                    (let [cell (dc/bin-get-cell board i (- dc/width j 1))]
                      cell)
                    (* 4 (+ (* dc/width (- dc/height i 1)) (- dc/width j 1))))))
        ]
    (->
      ;; baord
      (min board reverse-board)
      ; hand
      (add-animal your-hiyoko-num dc/chick    0x2)
      (add-animal your-kirin-num  dc/giraffe  0x2)
      (add-animal your-zou-num    dc/elephant 0x2)
      (add-animal my-hiyoko-num   dc/chick    0x1)
      (add-animal my-kirin-num    dc/giraffe  0x1)
      (add-animal my-zou-num      dc/elephant 0x1)
      )))

(defn abin-index [i j]
  (* 4 (+ (* dc/width (- dc/height i 1))
          (- dc/width j 1 ))))

(defn abin-get-cell [^long abin i j]
  (bit-and 2r1111
           (bit-shift-right abin (abin-index i j))))

(defn abin->bin [^long abin]
  {:board
   (reduce bit-or
           (for [i (range dc/height) j (range dc/width)]
             (bit-shift-left
               (let [cell (abin-get-cell abin i j)]
                 (if (= cell 2r0000)
                   cell
                   (bit-xor cell 2r1000)))
               (* 4 (+ (* dc/width i) j)))))
   :hands
   (let [
         my-hiyiko-num (get-hand-num abin dc/chick 1)
         my-kirin-num  (get-hand-num abin dc/giraffe 1)
         my-zou-num    (get-hand-num abin dc/elephant 1)
         your-hiyiko-num (get-hand-num abin dc/chick 2)
         your-kirin-num  (get-hand-num abin dc/giraffe 2)
         your-zou-num    (get-hand-num abin dc/elephant 2)
         f
         (fn [hand animal turn num]
           (cond (= num 2)
                 (dc/bin-add-hands (dc/bin-add-hands hand animal turn) animal turn)
                 (= num 1)
                 (dc/bin-add-hands hand  animal turn)
                 :else
                 hand))
         ]
     (-> 0
         (f dc/chick    2r1000 my-hiyiko-num)
         (f dc/giraffe  2r1000 my-kirin-num)
         (f dc/elephant 2r1000 my-zou-num)
         (f dc/chick    2r0000 your-hiyiko-num)
         (f dc/giraffe  2r0000 your-kirin-num)
         (f dc/elephant 2r0000 your-zou-num)
         ))
   })

(defn get-value [^RandomAccessFile file idx]
  (reduce +
          (map-indexed
            (fn [i x]
              (bit-shift-left (bit-and x 0xff) (* 8 i)))
            (let [arr (byte-array 8)
                  offset (* idx 8)]
              (.seek file offset)
              (.readFully file arr 0 8)
              arr))))

(defn get-index [^long abin]
  "解析局面 abin のファイル内でのインデックスを求める
  見つからなければ，-1を返す
  "
  (with-open [file (RandomAccessFile. "/Users/bobuhiro11/all-state_sorted.dat" "r")]
    ;(println "abin=" abin)
    (loop [_min 0 _max 246803166]
      (if (> _min _max)
        -1 ;; not found
        (let [mid (quot (+ _min _max) 2)]
          (let [x (get-value file mid)]
            ;(println "min=" _min ", max=" _max ", mid=" mid, ", x=" x)
            (cond (= abin x)
                  mid
                  (> abin x)
                  (recur (+ mid 1) _max)
                  (< abin x)
                  (recur _min (- mid 1)))))))))

(defn get-next-abin [^long abin]
  "次の解析局面 abin を返す
  見つからなければ，-1を返す"
  (let [index (get-index abin)]
    (if (= index -1)
      -1
      (let [v (with-open [file (RandomAccessFile. "/Users/bobuhiro11/next_state.dat" "r")]
               (get-value file index))]
        (if (= v 0)
          -1
          v)))))

(defn get-hand-num [^long abin animal belong]
  (let [offset (condp = animal
                 dc/chick    48
                 dc/giraffe  52
                 dc/elephant 56
                 -1)]
    (cond (and (= (bit-and 2r11 (bit-shift-right abin offset)) belong)
               (= (bit-and 2r11 (bit-shift-right abin (+ 2 offset))) belong))
          2
          (= (bit-and 2r11 (bit-shift-right abin offset)) belong)
          1
          (= (bit-and 2r11 (bit-shift-right abin (+ 2 offset))) belong)
          1
          :else
          0)))

(defn show-abin [^long abin]
  (let [r (abin->bin abin)]
    (dc/bin-show-board (:board r))
    (dc/bin-show-hands (:hands r))))

(defn show-binmap [_map]
    (dc/bin-show-board (:board _map))
    (dc/bin-show-hands (:hands _map)))

(defn bin-get-next [^long board ^long hands ^long turn]
  "次の局面 board と hands を返す．
  ただし，データがない場合は-1を返す（最後の一手などは自明なため）"
  (let [abin (if (= turn 2r0000)
               (bin->abin board hands)
               (let [r (dc/bin-get-reverse board hands)]
                 (bin->abin (:board r) (:hands r))))
        abin  (bin->abin board hands)
        na (get-next-abin abin)]
    (if (= na -1)
      -1
      (if (= turn 2r0000)
        (abin->bin na)
        (dc/bin-get-reverse (:board (abin->bin na))
                            (:hands (abin->bin na))
                            )))))

(show-binmap (bin-get-next dc/bin-init-board 2r0 2r1000))
(show-binmap (bin-get-next dc/bin-init-board 2r0 2r0000))

(defn bin-ai-victory [^long board ^long hands ^long turn]
  "bin-ai-randomやbin-ai-negamx同様の機能を持ち，
  必勝パターンを返す．
  ただし，データがない場合は bin-ai-negamax を使う．"
  (let [n (bin-get-next board hands turn)]
    (if (= n -1)
      (dc/bin-ai-negamx board hands turn)
      (let [
            all-moves (dc/bin-all-moves board hands turn)
            all-hand-indexs (dc/bin-hand-indexs board hands turn)
            next-board (:board n)
            next-hands (:hands n)
            next-abin (bin->abin next-board next-hands)
            move-result
            (mapcat (fn [[old v]]
                      (map (fn [new]
                             (let [result (dc/bin-move board old new turn)]
                               (if (= (:get result) 0)
                                 [:move old new 1000 (bin->abin (:board result) hands)]
                                 [:move old new 1000 (bin->abin (:board result) (dc/bin-add-hands hands (:get result) turn))]
                                 )
                               ))
                           v))
                    all-moves)
            put-result
            (apply concat
                   (for [i (range dc/height) j (range dc/width)]
                     (if (= 2r0000 (dc/bin-get-cell board i j))
                       (map (fn [index]
                              [:put index [i j]
                               1000
                               (dc/bin-set-cell
                                 board i j
                                 (bin->abin
                                 (dc/bin-get-hands hands
                                                   (+ (* 3 index)
                                                      (if (= turn dc/turn-b) 21 0))))
                               (dc/bin-set-hands hands
                                                 (+ (* 3 index)
                                                    (if (= turn dc/turn-b) 21 0))
                                                 2r000))
                               ]
                              )
                            all-hand-indexs))))
            move-candidacy
            (filter (fn [v]
                      (and (= (nth v 4) next-abin)))
                    move-result)
            put-candidacy
            (filter (fn [v]
                      (and (= (nth v 3) next-abin)))
                    put-result)
            ]
        (println "next-abin:")
        (show-abin next-abin)
        (println "move-result" move-result)
        (println "put-result" put-result)
        (println "move-candidacy:" move-candidacy)
        (println "put-candidacy:" put-candidacy)
        (if (>= (count move-candidacy) 1)
          (drop-last 1 (first move-candidacy))
          (drop-last 1 (first put-candidacy))
          )))))

(def test-board 801481229122)
(dc/bin-show-board test-board)
(dc/bin-show-board (:board (bin-get-next test-board 2r0 2r0000)))
(bin-ai-victory test-board 2r0 2r0000)
(bin-ai-victory dc/bin-init-board 2r0 2r0000)

(abin-index 1 1)
(println dc/bin-init-board)
(dc/bin-show-board dc/bin-init-board)
(dc/bin-show-board (:board (abin->bin
                             (bin->abin dc/bin-init-board 2r000))))
(let [init-board dc/bin-init-board
      init-hand
      (-> 2r000
          (dc/bin-add-hands dc/chick 2r1000)
          (dc/bin-add-hands dc/fowl 2r0000)
          (dc/bin-add-hands dc/elephant 2r1000)
          (dc/bin-add-hands dc/giraffe  2r1000)
          (dc/bin-add-hands dc/giraffe  2r1000)
          (dc/bin-add-hands dc/elephant 2r0000)
          )
      trans-board
      (:board (abin->bin (bin->abin init-board init-hand)))
      trans-hand
      (:hands  (abin->bin (bin->abin init-board init-hand)))
      ]
  (dc/bin-show-board init-board)
  (dc/bin-show-hands init-hand)
  (println "-----")
  (dc/bin-show-board trans-board)
  (dc/bin-show-hands trans-hand)
  )

(get-index (bin->abin dc/bin-init-board 2r000))
(with-open [file (RandomAccessFile. "/Users/bobuhiro11/all-state_sorted.dat" "r")]
  (get-value file 246803166))
(dc/bin-show-board (:board (abin->bin (get-next-abin (get-next-abin (bin->abin dc/bin-init-board 2r000))))))
