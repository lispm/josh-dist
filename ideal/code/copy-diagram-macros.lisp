;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************


;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



; Exporting these two will make sure that when saving diagrams all
; symbols will be printable without package qualifiers since they will
; all be in the using package. (I think).

(export '(NODE-REF NODE-REF-NAME))

;--------------------------------------------------------
; Actually, this macro was used when dealing with the multi-dimensional
; arrays that were used before for distributions. It has not been
; removed coz it is needed when converting diagrams of the old format to
; the new format.(18 Apr 89).
      
(defmacro for-each-location ((loc  array) &body body)
  (let ((it-var (gentemp "it-var"))(dimensions (gentemp "dimensions"))
	(new-array (gentemp "array")))
    `(let ((,new-array ,array))
       (labels ((create-nested-loops (,dimensions &optional ,loc)
		  (cond
		    ((null ,dimensions) ,@body)
		    (t (do ((,it-var 0 (1+ ,it-var)))
			   ((= ,it-var (car ,dimensions)) ,new-array)
			 (create-nested-loops (cdr ,dimensions) (cons ,it-var ,loc)))))))
	 (create-nested-loops (nreverse (array-dimensions ,new-array)))))))


;--------------------------------------------------------------------------------------------

; Used to maintain cross pointers when making non circular copies for writing to file.

(defstruct node-ref name)
