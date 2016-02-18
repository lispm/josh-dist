;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: joshua-internals -*-
;;;

(in-package :ji)

(defmethod lep::definition-undefining-form (fspec (type (eql 'joshua:defrule)))
  `(undefrule ',fspec))

(defmethod lep::definition-undefining-form (fspec (type (eql ':type)))
  `(setf (find-class ',fspec) nil))



 