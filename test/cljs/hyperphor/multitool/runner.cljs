(ns hyperphor.multitool.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [hyperphor.multitool.core-test]
            [hyperphor.multitool.nlp-test]
            [hyperphor.multitool.math-test]
            [hyperphor.multitool.csv-test]
            ))

;;; This is the running for clojurescript tests (and maybe should be a cljs file, but works here)

(doo-tests 'hyperphor.multitool.core-test
           'hyperphor.multitool.nlp-test
           'hyperphor.multitool.math-test
           'hyperphor.multitool.csv-test
           #_ 'your-project.util-test
           )
