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

;;;;;;;;;;;;;;;;;;;;
;; Virtual DOM
;;;;;;;;;;;;;;;;;;;;

;; Compiling

(defn element
  [args children id]
  (let [el
        (assoc
         (apply (fn
                  ([tag options string]
                   {:tag tag :options options :string string})
                  ([tag extra]
                   {:tag tag
                    (if (string? extra)
                      :string :options) extra}) 
                  ([tag]
                   {:tag tag})) args)
         ;; new hash needed when arguments, postion and number of children
         :hash (hash [args id (count children)]))] 
    (if (empty? children) el (assoc el :children children))))

(defn coll-not-map?
  [x]
  (and (coll? x)
       (not (map? x))))

(defn -make-vector-optional
  "[[a] [b] [[c] [d]] [e]] -> [[a] [b] [c] [d] [e]]"
  [v]
  (loop [v v r []]
    (let [current (first v)]
      (if current
        (recur (next v)
               (if (coll-not-map? (first current))
                 (apply conj r current)
                 (conj r current)))
        r))))

(defn compile-form
  ([v c id]
   (loop [v  v
          id id
          r  []]
     (let [f (first v)]
       (cond
         ;; form ends with a multiple or a vector of child forms
         (coll-not-map? f)
         ;; let [a] [b] be the same as [[a] [b]]
         (element r (map-indexed #(compile-form %2 (inc c) (str id "." c ":" %1)) (-make-vector-optional v)) id)
         ;; form ends with zero children
         (empty? v)
         (element r [] id)
         ;; else keep gathering arguments
         :else
         (recur (rest v) id (conj r f))))))
  ([v] (compile-form v 0 "id0")))

;; Diffing

(defn -elements-1
  [cform parent]
  (let [pair {:k (:hash cform) :v (assoc cform
                                         :children (map :hash (:children cform))
                                         :parent parent)}]
    [pair (map #(-elements-1 %  (:hash cform)) (:children cform))]))

(defn elements
  [cform]
  "Returns a map of elements grouped by there hashes."
  (into {}
        (map (fn [{k :k v :v}] [k v])
             (flatten  (-elements-1 cform :root)))))

(defn diff
  [new-elements old-elements]
  (let [new (remove #(get old-elements %) (keys new-elements))
        old (remove #(get new-elements %) (keys old-elements))]
    {:new new
     :old old
     :dirty (into #{} (remove nil? (map #(:parent (get old-elements %)) (apply conj new old))))}))
