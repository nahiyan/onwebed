(ns onwebed-cli.compiler.document-test
  (:require [cljs.test :refer (deftest is)]
            [onwebed-cli.compiler.document :as document]))

(deftest test-document-filename-checker
  (is (= false (document/public-document? "test.txt")))
  (is (= true (document/public-document? "test.od")))
  (is (= false (document/public-document? "_test.od"))))
