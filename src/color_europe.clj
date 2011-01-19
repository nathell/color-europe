(ns color-europe
  (:use [clojure.contrib.io :only [with-out-writer]])
  (:require [clojure.xml :as xml]
            [clojure.contrib.math :as math]
            [clojure.zip :as zip]))

;; The recursive tree mapping function, needed to get the actual 
;; colorizer working.

(defn map-zipper
  ([f z] (map-zipper f (constantly true) z))
  ([f pred z]
     (if (zip/end? z)
       (zip/root z)
       (recur f pred (-> z (zip/edit #(if (pred %) (f %) %)) zip/next)))))

;; The colorizer proper.

(defn htmlize-color
  [[r g b]]
  (format "#%02x%02x%02x" r g b))

(defn color-state
  [{:keys [tag attrs] :as element} colorize-fn]
  (let [state (:id attrs)]
    (if-let [color (colorize-fn state)]
      (assoc element :attrs (assoc attrs :style (str "fill:" (htmlize-color color))))
      element)
    element))

(defn make-colorizer
  [dataset ranges]
  (let [minv (apply min (vals dataset))
        maxv (apply max (vals dataset))
        progress (map (fn [[min-col max-col]] (/ (- max-col min-col) (- maxv minv))) ranges)]
    (into {}
          (map (fn [[k v]] [(.toLowerCase k) (map (fn [progress [min-color _]] (int (+ min-color (* (- v minv) progress)))) progress ranges)])
               dataset))))

(defn save-color-map
  [svg colorize-fn outfile]
  (let [colored-map (map-zipper #(color-state % colorize-fn) (fn [x] (#{:g :path} (:tag x))) (zip/xml-zip svg))]
    (with-out-writer outfile
      (xml/emit colored-map))))

;; Other sample colorizers -- have fun!

(defn in-range [a x b]
  (and (>= x a) (< x b)))

(defn hsv->rgb [h s v]
  (let [c (* v s)
        h1 (* h 6)
        x (* c (- 1 (math/abs (- (mod h1 2) 1))))
        [r g b] (cond
                     (in-range 0 h1 1) [c x 0]
                     (in-range 1 h1 2) [x c 0]
                     (in-range 2 h1 3) [0 c x]
                     (in-range 3 h1 4) [0 x c]
                     (in-range 4 h1 5) [x 0 c]
                     (in-range 5 h1 6) [c 0 x])
        m (- v c)]
    [(int (* 255 (+ r m))) 
     (int (* 255 (+ g m)))
     (int (* 255 (+ b m)))]))

(defn ranges-color
  [dataset state]
  (let [v (dataset state)]
    (cond (not v) nil
          (<= v 9) [0xb3 0xd8 0xf3]
          (<= 10 v 16) [0x78 0xc5 0xef]
          (<= 17 v 23) [0x4e 0xa4 0xdf]
          (<= 24 v 30) [0x34 0x6f 0xab]
          (> v 30) [0x2a 0x17 0x73])))