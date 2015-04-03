(ns tsp.canvas
  (:require [tikkba.swing :as swing]
            [tikkba.dom :as dom]
            [analemma.svg :as svg]
            [analemma.xml :as xml]
            [analemma.charts :as charts])
  (:import [java.lang Math]
           [javax.swing JFrame SwingUtilities]))


(defn create-svg-map
  "creates a SVG presentation of paths"
  [path]
  (let [x (map first path)
        [xmin xmax] (apply (juxt min max) x)
        y (map second path)
        [ymin ymax] (apply (juxt min max) y)
        scale (fn [value min_val max_val] (* 800
                                             (/ (- max_val value)
                                                (- max_val min_val))))
        scaled-vals (map
                      (fn [[x y]] [(scale x xmin xmax) (scale y ymin ymax)])
                      path)]
    (svg/svg
      (apply svg/group
        (for [[x y] (seq scaled-vals)]
          (svg/circle x y 5 :fill "#00F")))
      (apply svg/group
        (map
          (fn [[x1 y1] [x2 y2]]
            (svg/line x1 y1 x2 y2 :stroke "#0F0"))
          scaled-vals
          (vec (concat (rest scaled-vals) [(first scaled-vals)]))))
      )))

(defn create-frame
  [canvas]
  (let [frame (JFrame.)]
    (.add (.getContentPane frame) canvas)
    (.setSize frame 1024 800)
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (SwingUtilities/invokeAndWait
      (fn [] (.setVisible frame true)))))

(defn initialize
  "creates GUI for 2D map"
  [path]
  (let [svg-doc (dom/svg-doc (create-svg-map path))
        canvas (swing/jsvgcanvas)]
    (swing/set-document canvas svg-doc)
    (create-frame canvas)
    canvas))

(defn update
  "refreshes content of existing map"
  [canvas path]
  (let [svg-doc (dom/svg-doc (create-svg-map path))]
    (swing/set-document canvas svg-doc)
    canvas))
