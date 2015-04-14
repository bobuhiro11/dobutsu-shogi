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
           [java.awt.geom AffineTransform]))

(defn abin-add-hand [^long abin ^long animal ^long belong]
  "解析局面 abin にどうぶつ animal を belong の持コマとして追加する
  abin, animal, belong は解析ソフトと統一"
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

(defn bin->abin [^long board ^long hands]
  "core.clj で定義された局面 board と持コマ hands から
  解析局面 abin に変換（正規化済）
  "
  (let [your-hands (for [i (range 7)]
                     (dc/bin-get-hands hands (* 3 i)))
        my-hands (for [i (range 7)]
                   (dc/bin-get-hands hands (+ 21 (* 3 i))))
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
                        (bit-xor cell 2r1000)))
                    (* 4 (+ (* dc/width (- dc/height i 1)) (- dc/width j 1))))))
        reverse-board
        (reduce bit-or
                (for [i (range dc/height) j (range dc/width)]
                  (bit-shift-left
                    (let [cell (dc/bin-get-cell board i (- dc/width j 1))]
                      (if (= cell 2r0000)
                        cell
                        (bit-xor cell 2r1000)))
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

(Long/toHexString (bin->abin dc/bin-init-board
                             (-> 2r000
                                 (dc/bin-add-hands dc/chick 2r1000)
                                 (dc/bin-add-hands dc/fowl 2r0000)
                                 (dc/bin-add-hands dc/elephant 2r1000)
                                 (dc/bin-add-hands dc/giraffe  2r1000)
                                 (dc/bin-add-hands dc/giraffe  2r1000)
                                 (dc/bin-add-hands dc/elephant 2r0000)
                                 )))
(abin->bin (bin->abin dc/bin-init-board
                      (-> 2r000
                          (dc/bin-add-hands dc/chick 2r1000)
                          (dc/bin-add-hands dc/fowl 2r0000)
                          (dc/bin-add-hands dc/elephant 2r1000)
                          (dc/bin-add-hands dc/giraffe  2r1000)
                          (dc/bin-add-hands dc/giraffe  2r1000)
                          (dc/bin-add-hands dc/elephant 2r0000)
                          )))

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
