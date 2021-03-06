;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;
      
; SETTING UP THE PACKAGE THAT IDEAL IS LOADED IN:
;---------------------------------------------------

; If necessary, edit the defvar below to change the package in which
; IDEAL is loaded.

; The defvar below only ensures that the package name for IDEAL is
; accessible from within the IDEAL code (some functions need the package
; name). It is your responsibiliy to ensure that the IDEAL code is
; actually loaded into the package you have named below --- you can
; ensure this using the system definition utilities of your LISP or by
; using the Portable System Utilities distributed with IDEAL.

; If you plan to use SYM-IDEAL (the symbolics user interface for IDEAL)
; you must either have both SYM-IDEAL and IDEAL in the same package or
; have the IDEAL package inheriting from the SYMBOLICS-COMMON-LISP
; package (Robert Goldman, rpg@cs.brown.edu for details).


(defvar cl-user::*ideal-package-name-string*
	:ideal ; <- Customize this for all other implementations
	"Name string of the IDEAL package")
