(ns dobutsu-shogi.analysis-test
  (:require [clojure.test :refer :all]
            [dobutsu-shogi.core :as dc]
            [dobutsu-shogi.analysis :refer :all]))

(deftest bin->abin-test []
  (is (= 0x0000acb090010342       ;; initial state
         (bin->abin dc/bin-init-board 2r0 2r1000)
         (bin->abin dc/bin-init-board 2r0 2r0000)))
  (is (= 0x0000acb090410302       ;; lion exists in front of elephant (from initial state)
         (bin->abin 189874334401282 2r0 2r0000))))

(deftest get-next-abin-test []
    (is (= (get-next-abin 0x0000acb090010342) 0x0000a0b09c010342))
    (is (= (get-next-abin 0x01060ca0a0430000) 0x0006000bbc020240))
    (is (= (get-next-abin 0x01060ca0a0431110) -1)))

(deftest bin-ai-vicotry-test []
  (is (not (empty? (bin-ai-victory dc/bin-init-board 2r0 2r0000)))))
