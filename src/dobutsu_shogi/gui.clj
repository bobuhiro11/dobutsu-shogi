(ns dobutsu-shogi.gui
  (:gen-class)
  (:use     [clojure.repl])
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.timer :as st]
            [dobutsu-shogi.core :as dc])
  (:import  [javax.swing ImageIcon JLabel]
            [javax.swing ImageIcon JButton]
            [java.awt Image BasicStroke Color]
            [java.awt.geom AffineTransform]))

;; mutable
(def selected-cell  (ref nil))
(def selected-hands (ref nil))
(def turn           (ref :b))
(def comp-hands     (ref []))
(def play-hands     (ref []))
(def board
  (ref
    [[[:gir :a] [:lion :a] [:ele :a]]
     [nil       [:chi  :a] nil      ]
     [nil       [:chi  :b] nil      ]
     [[:ele :b] [:lion :b] [:gir :b]]]))

;; immutable

(defn get-bias [can]
  (/ (sc/height can) 8))

(defn get-board-width [can]
  (- (sc/width can) (* 2 (get-bias can))))

(defn get-block-height [can]
  (/ (- (sc/height can) 5) 4))

(defn get-block-width [can]
  (/ (- (get-board-width  can) 4) 3))

(defn mouse2cell [can x y]
   (let [cy (quot y                    (+ (get-block-height can) 1))
         cx (quot (- x (get-bias can)) (+ (get-block-width  can) 1))]
     (if (or (< cx 0) (< cy 0) (> cx 2) (> cy 3))
       nil
       [cy cx])))

(defn mouse2playhands [can x y]
  (let [bbh (quot (get-block-height can) 2)
        sx (+ (get-bias can) 1 (* (+ (get-block-width can) 1) 3))
        i (- 7 (quot y (+ bbh 1)))]
    (if (or (<= x sx) (>= i (count @play-hands)))
      :bad
      i)))

(defn get-image [animal]
  (let [prefix (if (= (second animal) :a) "rot_" "")]
    (.getImage
      (ImageIcon.
        (clojure.java.io/resource
          (str prefix (apply str (rest (str (first animal)))) ".png"))))))


(defn draw-animal! [can g bw bh i j animal]
  (if (vector? animal)
    (let [img-size 283
          sx (+ (get-bias can) 1 (* (+ bw 1) j))
          sy (+                1 (* (+ bh 1) i))
          ex (+ (get-bias can) 1 (* (+ bw 1) j) bw)
          ey (+                1 (* (+ bh 1) i) bh)
          img (get-image animal)]
      (.drawImage g img sx sy ex ey 0 0 img-size img-size can))))

(defn draw-play-hands! [can g bw bh bbw bbh i animal]
  (let [img-size 283
        sx (+ (get-bias can) 1 (* (+ bw 1) 3))
        ex (+ sx (get-bias can))
        sy (+ (* (+ bbh 1) (- 7 i)) 1)
        ey (+ sy bbh)
        img (get-image [animal :b])]
    (.drawImage g img sx sy ex ey 0 0 img-size img-size can)))

(defn draw-comp-hands! [can g bw bh bbw bbh i animal]
  (let [img-size 283
        sx 0
        ex (get-bias can)
        sy (+ (* (+ bbh 1) i) 1)
        ey (+ sy bbh)
        img (get-image [animal :a])]
    (.drawImage g img sx sy ex ey 0 0 img-size img-size can)))

(defn draw-animals! [can g bw bh bbw bbh]
  (doall (for [i (range 4) j (range 3)]
           (draw-animal! can g bw bh i j (dc/nnth @board i j))))
  (doall (map (fn [ani i]
                (draw-play-hands! can g bw bh bbw bbh i ani))
              @play-hands (range 0 (count @play-hands))))
  (doall (map (fn [ani i]
                (draw-comp-hands! can g bw bh bbw bbh i ani))
              @comp-hands (range 0 (count @comp-hands)))))

(defn draw-rect! [can g bw bh i j status]
  (let [lw 3 ; line-width
        old-stroke (.getStroke g)
        old-color (.getColor g)
        sx (+ 1 (get-bias can)  (* (+ bw 1) i))
        sy (+ 1                 (* (+ bh 1) j))
        ex (+ 1 (get-bias can)  (* (+ bw 1) i) bw)
        ey (+ 1                 (* (+ bh 1) j) bh)
        c (cond (= status :none)     java.awt.Color/black
                (= status :selected) java.awt.Color/red
                (= status :movable)  java.awt.Color/pink)]
    (.setColor g c)
    (.setStroke g (BasicStroke. (* 2 lw)))
    (.drawLine  g (+ sx lw -1) (+ sy lw)    (+ sx lw -1) (- ey lw))
    (.drawLine  g (- ex lw)    (+ sy lw)    (- ex lw)    (- ey lw))
    (.drawLine  g (+ sx lw)    (+ sy lw -1) (- ex lw)    (+ sy lw -1))
    (.drawLine  g (+ sx lw)    (- ey lw)    (- ex lw)    (- ey lw))
    (.setColor  g old-color)
    (.setStroke g old-stroke)))

(defn draw-hand-rect! [can g bw bbh i]
  (let [lw 3 ; line-width
        old-stroke (.getStroke g)
        old-color (.getColor g)
        sx (+ 1 (get-bias can)  (* (+ bw 1) 3))
        sy (+ 1                 (* (+ bbh 1) (- 7 i)))
        ex (+ sx (get-bias can))
        ey (+ sy bbh)
        c java.awt.Color/red]
    (.setColor g c)
    (.setStroke g (BasicStroke. (* 2 lw)))
    (.drawLine  g (+ sx lw -1) (+ sy lw)    (+ sx lw -1) (- ey lw))
    (.drawLine  g (- ex lw)    (+ sy lw)    (- ex lw)    (- ey lw))
    (.drawLine  g (+ sx lw)    (+ sy lw -1) (- ex lw)    (+ sy lw -1))
    (.drawLine  g (+ sx lw)    (- ey lw)    (- ex lw)    (- ey lw))
    (.setColor  g old-color)
    (.setStroke g old-stroke)))

(defn draw-frame! [can g bw bh]
  (if @selected-cell
    (let [movables (dc/movable @board (first @selected-cell) (second @selected-cell) :b)]
      (draw-rect!
        can g bw bh
        (second @selected-cell)
        (first @selected-cell)
        :selected)
      (if (seq? movables)
        (doall (map (fn [e]
                      (draw-rect!
                        can g bw bh
                        (second e)
                        (first e)
                        :movable))
                    movables)))))
      (if (not (nil? @selected-hands))
        (doall (map (fn [e]
                      (draw-rect!
                        can g bw bh
                        (second e)
                        (first e)
                        :movable))
                    (filter #(not (dc/nnth @board (first %) (second %))) (for [i (range 4) j (range 3)] [i j]))))))


(defn paint-event! [can g]
  (let [w (get-board-width  can)
        h  (sc/height can)
        bw  (get-block-width can)
        bbh (quot (get-block-height can) 2)
        bh  (+ (* 2 bbh) 1)
        bbw (get-bias can)
        b  (get-bias can)]
    ; draw vertical line
    (doall (for [x (range 0 w (+ bw 1))] (.drawLine g (+ x b) 0 (+ x b) h)))
    ; draw horizontal line
    (doall (for [y (range 0 h (+ bh 1))] (.drawLine g b y (+ b w -1) y)))
    ; draw horizontal line (for side)
    (.setColor g java.awt.Color/lightGray)
    (doall (for [y (range 0 h (+ bbh 1))] (.drawLine g 0 y (+ b -1) y)))
    (doall (for [y (range 0 h (+ bbh 1))]
             (.drawLine g (+ b (* 3 (+ 1 bw))) y (+ b (* 4 (+ 1 bw))) y)))
    (.setColor g java.awt.Color/black)
    ; draw animals
    (draw-animals! can g bw bh bbw bbh)
    ; test
    (if @selected-hands
      (draw-hand-rect! can g bw bbh @selected-hands))
    ; draw frame
    (draw-frame! can g bw bh)))

(defn lazy-contains? [col key]
  (some #{key} col))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn canvas-clicked! [e]
  (let [pos   (mouse2cell (.getSource e) (.getX e) (.getY e))
        hands (mouse2playhands (.getSource e) (.getX e) (.getY e))]
    (cond
      ; out of board
      (and (nil? pos) (nil? hands)) (dosync (ref-set selected-cell nil))
      ; unselect hand
      (= hands @selected-hands) (dosync (ref-set selected-hands nil))
      ; select hand
      (not (= :bad hands)) (dosync (ref-set selected-cell nil)
                                 (ref-set selected-hands hands))
      ; unselect pivot cell
      (= pos @selected-cell) (dosync (ref-set selected-cell nil))
      ; put animal
      (and @selected-hands pos (nil? (dc/nnth @board (first pos) (second pos))))
      (let [newb (dc/put @board (first pos) (second pos) (nth @play-hands @selected-hands)
                         :b)]
        (dosync (ref-set board newb)
                (ref-set play-hands (vec-remove @play-hands @selected-hands))
                (ref-set selected-hands nil)
                (ref-set turn :a)
        ))
      ; move animal
      (lazy-contains?
        (if (nil? @selected-cell)
          []
          (dc/movable @board (first @selected-cell) (second @selected-cell) :b))
        pos)
      (let [move-result
            (dc/move @board
                     (first @selected-cell) (second @selected-cell)
                     (first pos)            (second pos) :b)]
        (dosync
          (ref-set board (:board move-result))
          (ref-set play-hands
                   (if (:get move-result)
                     (into [(if (= :for (:get move-result)) :chi (:get move-result))] @play-hands)
                     @play-hands))
          (ref-set turn :a)
          (ref-set selected-cell nil)))
      ; select pivot cell
      (let [cell (dc/nnth @board (first pos) (second pos))]
        (and (not (nil? cell))
             (= (second cell) :b)))
      (dosync (ref-set selected-cell pos)
              (ref-set selected-hands nil))
      :else
      (dosync (ref-set selected-cell nil)
            (ref-set selected-hands nil)))
    ; com turn
    (if (= @turn :a)
      (let [mov (dc/ai-random @board :a)
            move-result
            (dc/move @board
                     (first (first mov)) (second (first mov))
                     (first (second mov)) (second (second mov)) :a)]
        (dosync
          (ref-set board (:board move-result))
          (ref-set comp-hands
                   (if (:get move-result)
                     (into [(if (= :for (:get move-result)) :chi (:get move-result))] @comp-hands)
                     @comp-hands))
          (ref-set turn :b))
          ))))

(defn newgame-clicked! [e]
  (dosync
    (ref-set selected-cell nil)
    (ref-set play-hands [])
    (ref-set comp-hands [])
    (ref-set turn :b)
    (ref-set board
             [[[:gir :a] [:lion :a] [:ele :a]]
              [nil       [:chi  :a] nil      ]
              [nil       [:chi  :b] nil      ]
              [[:ele :b] [:lion :b] [:gir :b]]])))

(def canvas
  (ref (let [c (sc/canvas :paint paint-event!)]
    (sc/listen c :mouse-clicked canvas-clicked!)
    c)))

(def frame
  (ref (sc/frame
    :title   "どうぶつしょうぎ",
    :width    400
    :height   396
    :content  @canvas
    :on-close :hide
    :menubar
    (sc/menubar
      :items
      [(sc/menu
         :text  "File"
         :items [(sc/action :name "New Game" :key "menu N" :handler newgame-clicked!)])
       (sc/menu
         :text "Edit"
         :items [])]))))

(defn show-frame! []
  (st/timer (fn [_] (.repaint @frame))
            :start? true
            :initial-delay 1000
            :delay 10
            :repeats? true)
  (sc/invoke-later
    (sc/show! @frame)))

(defn -main []
  (show-frame!))

;(show-frame!)
