(ns hyperphor.multitool.nlp-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [hyperphor.multitool.nlp :as sut])
  )

(deftest token-test                     ; in more ways than one!
  (is (= ["foo" "bar"] (sut/tokens "foo bar"))))

