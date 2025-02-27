(defsystem "denote-wiki"
  :version "0.0.1"
  :author "Daniel Pinkston"
  :license ""
  ;; (ql:quickload '(:ningle :clack :alexandria :cl-who :cl-ppcre))
  :depends-on ("ningle"
               "clack"
               "alexandria"
               "cl-who"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "note")
                 (:file "server"))))
  :description ""
  :in-order-to ((test-op (test-op "denote-wiki/tests"))))

(defsystem "denote-wiki/tests"
  :author "Daniel Pinkston"
  :license ""
  :depends-on ("denote-wiki"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for denote-wiki"
  :perform (test-op (op c) (symbol-call :rove :run c)))
