(ns hyperphor.multitool.csv-test
  (:require [hyperphor.multitool.csv :as sut]
            #?(:clj [hyperphor.multitool.cljcore :refer [temp-file-path]])
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])))

(deftest parse-rows-test
  (is (= [["a" "b" "c"] ["1" "2" "3"]]
         (sut/parse-rows "a,b,c\n1,2,3")))
  (is (= [["a" "b" "c"] ["1" "2" "3"]]
         (sut/parse-rows "a,b,c\n1,2,3\n")))
  (testing "quoted fields with embedded separator and newline"
    (is (= [["a" "b,c" "d\ne"]]
           (sut/parse-rows "a,\"b,c\",\"d\ne\""))))
  (testing "escaped (doubled) quote"
    (is (= [["say \"hi\""]]
           (sut/parse-rows "\"say \"\"hi\"\"\""))))
  (testing "crlf line endings"
    (is (= [["a" "b"] ["1" "2"]]
           (sut/parse-rows "a,b\r\n1,2\r\n"))))
  (testing "empty field / empty line"
    (is (= [["a" "" "c"]] (sut/parse-rows "a,,c")))
    (is (= [[""]] (sut/parse-rows "\n")))
    (is (= [] (sut/parse-rows "")))))

(deftest parse-tsv-rows-test
  (is (= [["a" "b"] ["1" "2"]]
         (sut/parse-tsv-rows "a\tb\n1\t2"))))

(deftest rows->str-test
  (is (= "a,b,c\n1,2,3"
         (sut/rows->str [["a" "b" "c"] ["1" "2" "3"]])))
  (testing "auto-quotes fields needing it"
    (is (= "a,\"b,c\",\"d\"\"e\""
           (sut/rows->str [["a" "b,c" "d\"e"]]))))
  (testing "quote? true forces quoting"
    (is (= "\"a\",\"b\""
           (sut/rows->str [["a" "b"]] :quote? true))))
  (testing "roundtrip through parse-rows"
    (let [rows [["name" "note"] ["Bob" "has, a comma"] ["Sue" "has a \"quote\""]]]
      (is (= rows (sut/parse-rows (sut/rows->str rows)))))))

(deftest ms-roundtrip-test
  (let [ms [{:a "1" :b "2"} {:a "3" :b "4"}]
        coerced [{:a 1 :b 2} {:a 3 :b 4}]]
    (testing "default coerces numeric-looking strings"
      (is (= coerced (sut/rows->ms (sut/ms->rows ms))))
      (is (= coerced (sut/read-csv-ms (sut/write-csv-ms ms))))
      (is (= coerced (sut/read-tsv-ms (sut/write-tsv-ms ms)))))
    (testing ":strings? true keeps them as strings"
      (is (= ms (sut/rows->ms (sut/ms->rows ms) :strings? true)))
      (is (= ms (sut/read-csv-ms (sut/write-csv-ms ms) :strings? true))))))

(deftest rows->ms-test
  (testing "empty field becomes nil, and is dropped from the map"
    (is (= [{:a "x"}] (sut/rows->ms [["a" "b"] ["x" ""]] :strings? true))))
  (testing "blank header becomes :__id"
    (is (= [{:__id "1" :name "Bob"}]
           (sut/rows->ms [["" "name"] ["1" "Bob"]] :strings? true))))
  (testing ":headers option supplies a header for headerless data"
    (is (= [{:a "1" :b "2"}]
           (sut/rows->ms [["1" "2"]] :headers ["a" "b"] :strings? true)))))

#?(:clj
   (deftest file-io-test
     (let [rows [["a" "b"] ["1" "2"]]
           ms [{:a "1" :b "2"}]
           file (temp-file-path)]
       (sut/write-csv-file file rows)
       (is (= rows (sut/read-csv-file file)))
       (sut/write-csv-ms-file file ms)
       (is (= [{:a 1 :b 2}] (sut/read-csv-ms-file file))))))

#?(:clj
   (deftest bom-stripping-test
     (let [file (temp-file-path)]
       (spit file "﻿a,b\n1,2\n")
       (is (= [["a" "b"] ["1" "2"]] (sut/read-csv-file file))))))
