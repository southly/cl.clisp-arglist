;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :clisp-arglist
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "clisp-arglist"))
  :depends-on (#+nil :cl-ppcre))
