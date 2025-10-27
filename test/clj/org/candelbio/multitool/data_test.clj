(ns org.candelbio.multitool.data-test
  (:use clojure.test)
  (:use org.candelbio.multitool.data)
  )

(def reshape-fat-data
  [{:id 1 :prop :name :value "Bob" }
   {:id 1 :prop :slack :value :high }
   {:id 2 :prop :name :value "Antibob" }
   {:id 2 :prop :slack :value :low }
   {:id 2 :prop :pink :value true }])


(deftest reshape-fat-test
  (is (= (set [{:name "Bob", :slack :high, :id 1}
               {:name "Antibob", :slack :low, :pink true, :id 2}])
         (set (reshape-fat reshape-fat-data :id :prop :value)))))
