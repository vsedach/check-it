;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10; indent-tabs-mode: nil -*-

(defsystem "check-it"
  :name "check-it"
  :serial t
  :author "Kyle Littler"
  :license "LGPL-3.0-or-later"
  :description "A randomized property-based testing tool for Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/DalekBaldwin/check-it"
  :version "0.1.0"
  :components
  ((:static-file "check-it.asd")
   (:module :src
            :components ((:file "package")
                         (:file "util")
                         (:file "generators")
                         (:file "regenerate")
                         (:file "shrink")
                         (:file "check-it"))
            :serial t))
  :depends-on ("alexandria" "closer-mop" "optima")
  :in-order-to ((test-op (test-op "check-it/test"))))

(defsystem "check-it/test"
  :serial t
  :author "Kyle Littler"
  :license "LGPL-3.0-or-later"
  :description "Tests for check-it."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "check-it-test")
                         (:file "deterministic-tests")
                         (:file "randomized-tests")
                         (:file "destructive-tests")
                         (:file "for-travis"))))
  :depends-on (:check-it :stefil)
  :perform (test-op (o c) (symbol-call :check-it-test '#:run-all-tests)))
