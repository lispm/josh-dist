;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)

;;; This seems to cause trouble in Allegro 10.0
(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (debug 3) (safety 3) (speed 1) (space 1))))

(require :climxm)

(load "~/josh-dist/driver.lisp")

(handler-bind ((fasl-casemode-mismatch #'(lambda (c) (invoke-restart 'excl::fasl-casemode-mismatch-continue)))) 
  (build-it)
  
  ;; on pascali I have this in a directory under my home
  ;; but on my MAC (and in its parallels linux) its under research projects
  (if (eql 'PASCALI (let ((stream (run-shell-command "hostname" :wait nil :output :stream))) (read stream)))
      (progn (format t "~%Loading PA code from /natural-software")
	     (load "~/natural-software/code/defsystem.lisp"))
    (load "~/Research-projects/natural-software/code/defsystem.lisp"))

    (load-system 'natsoft)

    (push "~/my-logical-pathnames.lisp"
	  (logical-pathname-translations-database-pathnames))
    )

(setq *read-init-files* t)

(required-top-level-binding *package* (find-package :natsoft))
(required-top-level-binding *readtable* ji::*joshua-readtable*)

(setq *restart-init-function* 'clim-env:start-clim-environment)

(dumplisp :name "sys:PA.dxl")




		      