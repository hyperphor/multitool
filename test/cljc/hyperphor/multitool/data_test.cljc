(ns hyperphor.multitool.data-test
  (:require [hyperphor.multitool.data :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])))


(deftest group-by*-test
  (let [base
        [{:name "Fred" :country "US" :sex :male :status :fictional}
         {:name "Barney" :country "US" :sex :male :status :fictional}
         {:name "Wilma" :country "US" :sex :female :status :real}
         {:name "Amelie" :country "France" :sex :female :status :fictional}
         ]]
    (is
     (= {"US"
        {:male
         {:fictional
          [{:name "Fred", :country "US", :sex :male, :status :fictional}
           {:name "Barney", :country "US", :sex :male, :status :fictional}]},
         :female {:real [{:name "Wilma", :country "US", :sex :female, :status :real}]}},
        "France"
        {:female {:fictional [{:name "Amelie", :country "France", :sex :female, :status :fictional}]}}}
        (sut/group-by* [:country :sex :status] base)))))
