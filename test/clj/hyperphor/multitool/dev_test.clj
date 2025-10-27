(ns hyperphor.multitool.dev-test
  (:use clojure.test)
  (:use hyperphor.multitool.dev)
  (:require [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.clj-testing :as testing]
            ))

(deftest capture-to-file-test
  (let [file (ju/temp-file)]
    (capture-to-file
     #(do (prn :x) (prn :y))
     file)
    (is (testing/file-contents? file ":x\n:y\n"))))
  
