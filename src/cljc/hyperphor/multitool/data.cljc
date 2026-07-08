(ns hyperphor.multitool.data
  (:require [hyperphor.multitool.core :as u])
  )

;;; Note: these are for casual use, you probably want tablecloth if doing a lot of data operations

;;; ⩇⩆⩇ Mapseqs ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; A "mapseq" is what I'm calling a seq of maps, each of which has (about) the same fields, eg what you would get back
;;; from a SQL query. Odd that this extremely common construct has no good name. Semantically the same as Python/R/Tablecloth dataframe

;;; TODO make names and args more consistent (or just use tablecloth)

(defn ms-range
  "Returns [min, max, count-distinct] using generic comapre"
  [field ms]
  (let [vals (map field ms)]
    [(u/min* vals) (u/max* vals) (count (distinct vals))]))

(defn ms-fields
  [ms]
  (keys (first ms)))                    ;TODO assuming all keys in first rec

(defn ms-fields+
  [ms]
  (apply u/lunion (map keys ms)))

(defn ms-describe
  [ms]
  (let [fields (ms-fields ms)]
    (zipmap fields
            (map #(u/ignore-errors (ms-range % ms)) fields))))

(defn ms-find
  "Find values that match re, return seq of [k v rec]"
  [ms re]
  (u/collecting
   (fn [collect]
     (doseq [m ms]
       (doseq [[k v] m]
         (when (and (string? v) (re-find re v))
           (collect [k v m])             ;Add in m to get the actual row
           ))))))

(defn ms-filter
  "Filter ms based on field and pred"
  [ms field pred]
  (filter #(pred (field %)) ms))

(comment
;;; deprecated, use ms-filter (or reerite using it)
  (defn filter=
    [field v mapseq]
    (filter #(= (field %) v) mapseq))

;;; Similarly for all comparators
  (defn filter<
    [field v mapseq]
    (filter #(u/<* (field %) v) mapseq))

  (defn filter<=
    [field v mapseq]
    (filter #(u/<=* (field %) v) mapseq)))

(defn group-by*
  "Multilevel group-by"
  [[key1 & key*] mapseq]
  (if (empty? key*)
    (group-by key1 mapseq)
    (u/map-values #(group-by* key* %)
                  (group-by key1 mapseq))))

(defn add-deltas
  [field delta-field ms]
  (u/for* [e0 ms
           e1 (rest ms)]
    (assoc e1 delta-field (- (field e1) (field e0)))))

(defn add-field
  [field f ms]
  (map #(assoc % field (f %)) ms))

(defn ms-join
  [ms1 ms2 f1 f2]
  (let [i2 (u/index-by f2 ms2)]
    (map (fn [e1] (merge e1 (get i2 (f1 e1)))) ms1)))

(defn rename-field
  [ms from to]
  (map #(u/rename-key % from to) ms))

;;; Conditional prob
;;; P(value-up|xbi-up)
(defn cond-prob
  [ms pl pr]
  (let [prs (filter pr ms)
        pls (filter pl prs)]
    [(count pls) (count prs) (float (/ (count pls) (count prs)))]))

(defn reshape-fat
  "Reshape a mapseq from skinny to fat"
  [mapseq id-col prop-col val-col]
  (map (fn [[id cols]]
         (assoc (zipmap (map prop-col cols)
                        (map val-col cols))
                id-col id))
       (group-by id-col mapseq)))

;;; TODO reshape-skinny

(defn compare-ms
  [ms1 ms2 index]
  (let [i1 (u/index-by index ms1)
        i2 (u/index-by index ms2)]
    (prn :counts (count ms1) (count ms2))
    (prn :top-diff (u/map-diff i1 i2))))

