(ns hyperphor.multitool.data-test
  (:require [hyperphor.multitool.data :as sut]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])))

(def reshape-fat-data
  [{:id 1 :prop :name :value "Bob" }
   {:id 1 :prop :slack :value :high }
   {:id 2 :prop :name :value "Antibob" }
   {:id 2 :prop :slack :value :low }
   {:id 2 :prop :pink :value true }])

(deftest reshape-fat-test
  (is (= (set [{:name "Bob", :slack :high, :id 1}
               {:name "Antibob", :slack :low, :pink true, :id 2}])
         (set (sut/reshape-fat reshape-fat-data :id :prop :value)))))

(deftest ms-fields+-test
  (is (= #{} (set (sut/ms-fields+ []))))
  (is (= #{:a :b} (set (sut/ms-fields+ [{:a 1 :b 2}]))))
  (is (= #{:a :b :c} (set (sut/ms-fields+ [{:a 1 :b 2} {:a 3 :c 4}]))))
  ;; unlike ms-fields, picks up fields that only appear in later records
  (is (= #{:a :b} (set (sut/ms-fields+ [{:a 1} {:a 2 :b 3}])))))

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
