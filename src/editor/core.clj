(ns editor.core
  (:require [clojure.string :as str]
            [lanterna.terminal :as t]))

;;

(defmacro assoc-self [map key func]
  `(assoc ~map ~key (~func ~map)))
;;

(def string (slurp "./resources/lorum"))

(def lines (str/split string #"\n"))

;;

(defn lines-needed [lines l w]
  "Lines needed to fit a screen of _w_ characters wide."
  (int (Math/ceil (/ (count (nth lines l)) w))))

(defn get-line-y [lines l w]
  "The _y_ position of a line if a screen is _w_ characters wide."
  (loop [l l r 0]
    (if (zero? l) r
        (recur (dec l)
               (+ r
                  1 ;; new line
                  (lines-needed lines l w))))))

(defn split-line [lines l w]
  "Split the line in the amount of parts needed to fit a screen of width _w_"
  (let [q (lines-needed lines l w)
        s (nth lines l)
        c (count s)]
    (->> (range q)
         (map (fn [x] [(* x w)
                       (min c (* (inc x) w))]))
         (map (fn [[start end]]
                (subs s start end))))))

;;

(defn generate-buffer [lines window]
  (let [[w _] (:size window)]
    (->> (range (count lines))
         (map #(split-line lines % w))
         flatten
         (into []))))

(defn make-window [lines [w h]]
  (assoc-self {:from 0
               :size     [w h]}
              :buffer #(generate-buffer lines %)))

(defn line-down [window]
  (assoc window :from (inc (:from window))))

(defn line-up [window]
  (assoc window :from (dec (:from window))))

;;

(defonce term (let [term (t/get-terminal :swing)]
                (t/start term)
                term))

(defn next-liner [term x y]
  (let [y (atom y)]
    (t/move-cursor term x @y)
    (fn []
      (swap! y inc)
      (t/move-cursor term x @y))))

(defn draw-buffer [term window]
  (t/clear term)
  (let [next-line    (next-liner term 0 0)
        {buffer :buffer
         [_ h]  :size
         from   :from} window]
    (for [s (subvec buffer from (+ from h))]
      (do (t/put-string term s)
          (next-line)))))
