;;; -*- mode: lisp -*-
;;;
;;; Copyright (c) 2010 NANRI <southly@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;
(in-package :make-arglist)

(defvar *hyperspec-root* "/usr/share/doc/hyperspec/")
(defvar *hyperspec-symbol-file* (merge-pathnames "Data/Map_Sym.txt" *hyperspec-root*))

(defun read-symbol-name-list ()
  (with-open-file (in *hyperspec-symbol-file*
                      :direction :input
                      :external-format :default)
    (loop :for line = (read-line in nil nil)
       :while line
       :collect line
       :do (read-line in nil nil))))

(defun map-to-function-list (symbol-names)
  (mapcan (lambda (s)
            (multiple-value-bind (symbol status)
                (find-symbol s :cl)
              (when (and status
                         (fboundp symbol))
                (list symbol))))
          symbol-names))

(defun %%arglist (fname)
  #+sbcl (sb-introspect:function-lambda-list fname)
  #+sbcl-deprecated  (sb-introspect:function-arglist f)
  #+clisp (ignore-errors (ext:arglist fname))
  #+cmu (swank-backend:arglist fname))
  )

(defun map-tree (function tree)
  (if (consp tree)
      (cons (map-tree function (car tree))
            (map-tree function (cdr tree)))
      (funcall function tree)))

(defun map-to-arglists (function-list)
  (let ((package-list (list (find-package :cl) (find-package :keyword))))
    (mapcar (lambda (f)
              (map-tree (lambda (x)
                          (cond ((not (symbolp x))
                                 x)
                                ((member (symbol-package x) package-list)
                                 x)
                                (t
                                 (make-symbol (symbol-name x)))))
                        (cons f (%%arglist f))))
            function-list)))

(defun dump-arglist (file)
  (with-open-file (out file
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create
                       :external-format :default)
    (format out ";;;; Arglist of ~A~%~%" (lisp-implementation-type))
    (format out "~S~%~%" '(cl:in-package :clisp-arglist))
    #+cmu (setf *print-pretty* nil)
    (format out "(~S ~S~%(~S ~S))~%" 'cl:defparameter '*arglist-list* 'cl:quote
            (map-to-arglists (map-to-function-list (read-symbol-name-list)))))
  (values))
