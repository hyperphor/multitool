(ns hyperphor.multitool.csv
  "Read and write CSV/TSV. Portable: the parser and writer are plain Clojure operating on
   strings and vectors, so they work identically in Clojure and ClojureScript. Clojure also
   gets convenience fns for reading from files, classpath resources, and urls (anything
   clojure.java.io/reader accepts), and for writing to files.

   Two shapes in and out:
    - raw: a seq of rows, each row a vector of string fields (first row usually a header).
      Always plain strings, untouched.
    - ms (mapseq): a seq of maps, keyed from the header row -- see hyperphor.multitool.data.
      Values are lightly coerced (empty field -> nil, otherwise numeric strings -> numbers)
      since that's almost always what you want out of a mapseq; pass :strings? true to keep
      everything as strings.

   Synthesized from the various copies of this floating around (voracious, traverse):
   numeric coercion, :__id for a blank header, and BOM-stripping on file reads all came from
   there. Their :quote? auto-quote-numeric-looking-strings-on-write trick was dropped -- it
   was marked broken in both sources and rows->str's ordinary auto-quoting covers real needs."
  (:require [clojure.string :as str]
            [hyperphor.multitool.core :as core]
            #?(:clj [clojure.java.io :as io])))

;;; ⩇⩆⩇ Parsing ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; A small hand-rolled state machine rather than a wrapped library, so behavior (quoting,
;;; escaping, line endings) is identical on both platforms. Relaxed like clojure.data.csv:
;;; a field only starts a quoted run if the quote is its very first character; anything
;;; between a closing quote and the next separator/newline is just appended to the field.

(defn parse-rows
  "Parse CSV/TSV text `s` into a seq of rows, each row a vector of string fields.
   Handles quoted fields (which may contain separators or newlines) and doubled quotes
   as an escaped quote. Recognizes both \\n and \\r\\n line endings.
   Options:
    :separator - field separator character (default \\,)
    :quote     - quote character (default \\\")"
  [s & {:keys [separator]
        quote-char :quote
        :or {separator \, quote-char \"}}]
  (loop [chars (seq s)
         field []
         row []
         rows []
         in-quotes? false]
    (if (empty? chars)
      (cond-> rows
        (or (seq field) (seq row))
        (conj (conj row (apply str field))))
      (let [c (first chars)
            more (rest chars)]
        (cond
          in-quotes?
          (if (= c quote-char)
            (if (= (first more) quote-char)
              (recur (rest more) (conj field quote-char) row rows true)
              (recur more field row rows false))
            (recur more (conj field c) row rows true))

          (and (= c quote-char) (empty? field))
          (recur more field row rows true)

          (= c separator)
          (recur more [] (conj row (apply str field)) rows false)

          (= c \newline)
          (recur more [] [] (conj rows (conj row (apply str field))) false)

          (= c \return)
          (recur more field row rows false)

          :else
          (recur more (conj field c) row rows false))))))

(defn parse-tsv-rows
  "Like parse-rows, but defaults :separator to tab."
  [s & opts]
  (apply parse-rows s :separator \tab opts))

;;; ⩇⩆⩇ Writing ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(def ^:private newlines {:lf "\n" :cr+lf "\r\n"})

(defn- needs-quote? [^String s separator quote-char nl]
  (or (str/includes? s (str separator))
      (str/includes? s (str quote-char))
      (str/includes? s nl)
      (str/includes? s "\n")
      (str/includes? s "\r")))

(defn- quote-field [s quote-char]
  (str quote-char (str/replace s (str quote-char) (str quote-char quote-char)) quote-char))

(defn rows->str
  "Write `rows` (a seq of seqs of values, stringified with str) as CSV/TSV text.
   Options:
    :separator - field separator character (default \\,)
    :quote     - quote character (default \\\")
    :newline   - line separator, :lf (default) or :cr+lf
    :quote?    - quote every field if true, or a predicate of the (stringified) field to
                 quote just that field; default nil, meaning auto-quote as needed"
  [rows & {:keys [separator newline quote?]
           quote-char :quote
           :or {separator \, quote-char \" newline :lf}}]
  (let [nl (or (newlines newline)
               (throw (ex-info "newline must be :lf or :cr+lf" {:newline newline})))
        force-quote? (cond (fn? quote?) quote?
                            quote? (constantly true)
                            :else (constantly false))
        field->str (fn [v]
                     (let [s (str v)]
                       (if (or (force-quote? s) (needs-quote? s separator quote-char nl))
                         (quote-field s quote-char)
                         s)))]
    (str/join nl (map (fn [row] (str/join separator (map field->str row))) rows))))

(defn rows->tsv-str
  "Like rows->str, but defaults :separator to tab."
  [rows & opts]
  (apply rows->str rows :separator \tab opts))

;;; ⩇⩆⩇ Mapseq conversion ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See hyperphor.multitool.data for more on mapseqs

(defn header->key
  "Default header field -> map key: a blank header (eg an unlabeled index column) becomes
   :__id, otherwise punctuation is replaced to make a valid keyword -- see core/keyword-safe."
  [s]
  (if (empty? s) :__id (core/keyword-safe s)))

(defn- coerce-field [strings? v]
  (let [v (if (= v "") nil v)]
    (if (or strings? (nil? v)) v (core/coerce-numeric v))))

(defn rows->ms
  "Convert `rows` into a mapseq: a seq of maps from header field to value.
   Options:
    :key-fn    - header field -> map key (default header->key)
    :headers   - explicit header row, for data with no header row of its own;
                 default is to take the header from the first row of `rows`
    :strings?  - keep all values as strings; default false, which coerces an empty field
                 to nil and otherwise leaves numeric-looking strings as numbers"
  [rows & {:keys [key-fn headers strings?] :or {key-fn header->key}}]
  (let [[header data] (if headers [headers rows] [(first rows) (rest rows)])
        ks (map key-fn header)]
    (map (fn [row] (core/clean-map (zipmap ks (map (partial coerce-field strings?) row))))
         data)))

(defn ms->rows
  "Convert mapseq `ms` into rows with a header row. `:columns` (optional) gives explicit
   field order; defaults to the union of keys across all maps, in first-seen order."
  [ms & {:keys [columns]}]
  (let [cols (or columns (distinct (mapcat keys ms)))]
    (cons (map name cols)
          (map (fn [m] (map #(get m %) cols)) ms))))

;;; ⩇⩆⩇ String-level read/write ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn read-csv
  "Parse CSV text `s` into raw rows. See parse-rows for options."
  [s & opts]
  (apply parse-rows s opts))

(defn read-tsv
  "Parse TSV text `s` into raw rows. See parse-rows for options."
  [s & opts]
  (apply parse-tsv-rows s opts))

(defn read-csv-ms
  "Parse CSV text `s` into a mapseq, using the first row as the header."
  [s & opts]
  (apply rows->ms (apply parse-rows s opts) opts))

(defn read-tsv-ms
  "Parse TSV text `s` into a mapseq, using the first row as the header."
  [s & opts]
  (apply rows->ms (apply parse-tsv-rows s opts) opts))

(defn write-csv
  "Write `rows` to CSV text. See rows->str for options."
  [rows & opts]
  (apply rows->str rows opts))

(defn write-tsv
  "Write `rows` to TSV text. See rows->str for options."
  [rows & opts]
  (apply rows->tsv-str rows opts))

(defn write-csv-ms
  "Write mapseq `ms` to CSV text, header row first. See ms->rows and rows->str for options."
  [ms & opts]
  (apply rows->str (apply ms->rows ms opts) opts))

(defn write-tsv-ms
  "Write mapseq `ms` to TSV text, header row first. See ms->rows and rows->str for options."
  [ms & opts]
  (apply rows->tsv-str (apply ms->rows ms opts) opts))

;;; ⩇⩆⩇ File / resource / url I/O (Clojure only) ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

#?(:clj
   (do

     (defn slurp-source
       "Slurp text from anything clojure.java.io/reader accepts: a file, a filename, a
        classpath resource (io/resource), a URL (as a string or java.net.URL), etc.
        Strips a leading UTF-8 byte-order-mark if present (common in Excel exports)."
       [source]
       (let [s (slurp (io/reader source))]
         (cond-> s
           (str/starts-with? s "﻿") (subs 1))))

     (defn read-csv-file
       "Read CSV rows from a file, resource, or url. See parse-rows for options."
       [source & opts]
       (apply read-csv (slurp-source source) opts))

     (defn read-tsv-file
       "Read TSV rows from a file, resource, or url. See parse-rows for options."
       [source & opts]
       (apply read-tsv (slurp-source source) opts))

     (defn read-csv-ms-file
       "Read a CSV mapseq from a file, resource, or url. See parse-rows for options."
       [source & opts]
       (apply read-csv-ms (slurp-source source) opts))

     (defn read-tsv-ms-file
       "Read a TSV mapseq from a file, resource, or url. See parse-rows for options."
       [source & opts]
       (apply read-tsv-ms (slurp-source source) opts))

     (defn write-csv-file
       "Write `rows` as CSV to `file`. See rows->str for options."
       [file rows & opts]
       (spit file (apply write-csv rows opts)))

     (defn write-tsv-file
       "Write `rows` as TSV to `file`. See rows->str for options."
       [file rows & opts]
       (spit file (apply write-tsv rows opts)))

     (defn write-csv-ms-file
       "Write mapseq `ms` as CSV to `file`, header row first. See ms->rows and rows->str for options."
       [file ms & opts]
       (spit file (apply write-csv-ms ms opts)))

     (defn write-tsv-ms-file
       "Write mapseq `ms` as TSV to `file`, header row first. See ms->rows and rows->str for options."
       [file ms & opts]
       (spit file (apply write-tsv-ms ms opts)))))
