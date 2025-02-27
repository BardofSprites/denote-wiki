(defpackage denote-wiki/tests/main
  (:use :cl
        :denote-wiki
        :rove))
(in-package :denote-wiki/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :denote-wiki)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
