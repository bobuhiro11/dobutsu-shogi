(defproject dobutsu-shogi "0.1.0-SNAPSHOT"
  :description "dobutsu-shogi"
  :url "http://bobuhiro11.net"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main dobutsu-shogi.gui
  :aot [dobutsu-shogi.gui dobutsu-shogi.core]
  :resource-paths ["resources"]
  :profiles {:dev {:global-vars {*warn-on-reflection* true}}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.5"]
                 [org.clojure/tools.trace "0.7.8"]])
