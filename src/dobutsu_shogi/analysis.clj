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

(defn test-bin->abin []
  (and
    (= 0x0000acb090010342       ;; 初期局面
       (bin->abin dc/bin-init-board 2r0 2r1000)
       (bin->abin dc/bin-init-board 2r0 2r0000))
    (= 0x0000acb090410302       ;; 初期局面からライオンがゾウの前に出た局面
       (bin->abin 189874334401282 2r0 2r0000))
    (= 13232964219907
       (bin->abin 211907572146240 0 2r0000))
    ))

(test-bin->abin)

(dc/bin-show-board 189874334401282)
(bin->abin 189118969807618 2097152 2r1000)

(defn bin->abin [^long board ^long hands ^long turn]
  "core.clj で定義された局面 board と持コマ hands ，そして turn から
  解析局面 abin に変換（正規化済み）
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
        abin
        (reduce bit-or
                (for [^long i (range dc/height) ^long j (range dc/width)]
                  (bit-shift-left
                    (let [
                          ci (if (= turn 2r1000) i (- dc/height i 1))
                          cj (if (= turn 2r1000) j (- dc/width j 1))
                          cell ^long (dc/bin-get-cell board ci cj)]
                      (if (= cell 2r0000)
                        2r0000
                        (if (= turn 2r0000) cell (bit-xor 2r1000 cell))))
                    (* 4 (+ (* dc/width (- dc/height i 1)) (- dc/width j 1))))))
        rev-abin
        (reduce bit-or
                (for [^long i (range dc/height) ^long j (range dc/width)]
                  (bit-shift-left
                    (let [
                          ci (if (= turn 2r1000) i (- dc/height i 1))
                          cj (if (= turn 2r1000) (- dc/width j 1) j)
                          cell ^long (dc/bin-get-cell board ci cj)]
                      (if (= cell 2r0000)
                        2r0000
                        (if (= turn 2r0000) cell (bit-xor 2r1000 cell))))
                    (* 4 (+ (* dc/width (- dc/height i 1)) (- dc/width j 1))))))
        ]
    (->
      ;; baord
      (min abin rev-abin)
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

(defn get-next-abin [^long abin]
  "（テスト済）次の解析局面 abin を返す
  見つからなければ，-1を返す"
  (let [
        get-value (fn [^RandomAccessFile file idx]
                    "fileを 64bit データの集まりとみなし，
                    その idx 番目のデータを取得する"
                    (reduce +
                            (map-indexed
                              (fn [i x]
                                (bit-shift-left (bit-and x 0xff) (* 8 i)))
                              (let [arr (byte-array 8)
                                    offset (* idx 8)]
                                (.seek file offset)
                                (.readFully file arr 0 8)
                                arr))))

        get-index (fn [^long abin]
                    "解析局面 abin のファイル内でのインデックスを求める
                    見つからなければ，-1を返す
                    "
                    (with-open [file (RandomAccessFile. "/Users/bobuhiro11/all-state_sorted.dat" "r")]
                      (loop [_min 0 _max 246803166]
                        (if (> _min _max)
                          -1 ;; not found
                          (let [mid (quot (+ _min _max) 2)]
                            (let [x (get-value file mid)]
                              (cond (= abin x)
                                    mid
                                    (> abin x)
                                    (recur (+ mid 1) _max)
                                    (< abin x)
                                    (recur _min (- mid 1)))))))))
        index (get-index abin)]
    (if (= index -1)
      -1
      (let [v (with-open [file (RandomAccessFile. "/Users/bobuhiro11/next_state.dat" "r")]
                (get-value file index))]
        (if (= v 0)
          -1
          v)))))

(defn test-get-next-abin []
  (and
    (= (get-next-abin 0x0000acb090010342) 0x0000a0b09c010342)
    (= (get-next-abin 0x01060ca0a0430000) 0x0006000bbc020240)
    (= (get-next-abin 0x01060ca0a0431110) -1)
       ))

(test-get-next-abin)

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

(defn tes-bin-ai-vicotry []
  (and (not (empty? (bin-ai-victory dc/bin-init-board 2r0 2r0000)))
       ))

(tes-bin-ai-vicotry)
;(bin-ai-victory 189118969807618 2097152 2r1000)
(bin-ai-victory 189187672245058  2097152 2r1000)

(dc/game 2r1000 bin-ai-victory dc/bin-ai-negamx)

(defn bin-ai-victory [^long board ^long hands ^long turn]
  "bin-ai-randomやbin-ai-negamx同様の機能を持ち，
  必勝パターンを返す．
  ただし，データがない場合は bin-ai-negamax を使う．"
  (let [abin (bin->abin board hands turn)
        next-abin (get-next-abin abin)
        ]
    (if (= next-abin -1)
      (do (println "abin=" abin ",next-abin=-1") (dc/bin-ai-negamx board hands turn))
      (let [
            all-moves (dc/bin-all-moves board hands turn)
            all-hand-indexs (dc/bin-hand-indexs board hands turn)
            move-result
            (mapcat (fn [[old v]]
                      (map (fn [new]
                             (let [result (dc/bin-move board old new turn)]
                               (if (= (:get result) 0)
                                 [:move old new 1000 (:board result) hands]
                                 [:move old new 1000 (:board result) (dc/bin-add-hands hands (:get result) turn)]
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
                                 (bit-or turn
                                         (dc/bin-get-hands hands
                                                           (+ (* 3 index)
                                                              (if (= turn dc/turn-b) 21 0)))))
                               (dc/bin-set-hands hands
                                                 (+ (* 3 index)
                                                    (if (= turn dc/turn-b) 21 0))
                                                 2r000)
                               ]
                              )
                            all-hand-indexs))))
            move-candidacy
            (filter (fn [v]
                      (let [nv (bin->abin (nth v 4)  (nth v 5) (bit-xor turn 2r1000))]
                        (do (println "next-abin候補:" nv) (= nv next-abin))))
                    move-result)
            put-candidacy
            (filter (fn [v]
                      (let [nv (bin->abin (nth v 4)  (nth v 5) (bit-xor turn 2r1000))]
                        (do (println "next-abin候補:" nv) (= nv next-abin))))
                    put-result)
            ]
        (println "board" board)
        (println "hands" hands)
        (println "turn" turn)
        (println "abin:" abin)
        (println "next-abin:" next-abin)
        (println "move-result" move-result)
        (println "put-result" put-result)
        ;(println "move-candidacy:" move-candidacy)
        ;(println "put-candidacy:" put-candidacy)
        (if (>= (count move-candidacy) 1)
          (drop-last 2 (first move-candidacy))
          (drop-last 2 (first put-candidacy))
          )))))
