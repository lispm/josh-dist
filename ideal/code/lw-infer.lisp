;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)


;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;



(export '(LW-INFER))

;--------------------------------------------------------

(defun lw-infer (diagram)
  (multiple-value-bind (evidence-case cutset-nodes sorted-nodes reverse-sorted-nodes)
      (set-up-and-initialize-for-lw-infer diagram)
    (absorb-case&set-local-evidence evidence-case)
    (for-all-cond-cases (cutset-case cutset-nodes)
      (absorb-case&set-local-evidence cutset-case)
      (propagate-thru-belief-net sorted-nodes reverse-sorted-nodes)
      (increment-beliefs diagram))
    (normalize-beliefs diagram)))

(defun absorb-case&set-local-evidence (node-cases)
  (let (node state)
    (dolist (n.s node-cases)
      (setq node (car n.s) state (cdr n.s))
	; When the new state is different from the previous local evidence ...
      (when (not (eq state (local-evidence node)))
	(setf (activated-p node) t)
	(setf (local-evidence node) state)
	(absorb-node-case node state)
	(dolist (s (node-successors node))
	  (setf (activated-p s) t))))))

(defun absorb-node-case (node state)
  (let (value)
	; Actually we need to absorb only into those successors which
	; are NOT connected to the node in the span tree but it is simpler
	; to absorb into all.
    (for-all-cond-cases (node-case node)
      (setq value (if (eq (state-in node-case) state) 1 0))
      (dolist (s (node-successors node))
	(setf (pi-msg-of s node-case) value)))))

;------- Propagation ---------------------------------------------------------

(defun propagate-thru-belief-net (sorted-nodes reverse-sorted-nodes)
  (dolist (n sorted-nodes)
    (update-ordered-parents-msg n))
  (dolist (n reverse-sorted-nodes)
    (update-ordered-childrens-msgs n)))

; The step that updates the overall lambda of the node before updating
; the lambda msg to the order parent or the overall pi of the node
; before updating the pi msg of the order parent is purely an
; implementation detail. The order parent's lambda msg does NOT depend
; purely on the updated lambda and incoming lambda messages.  It also
; depends on the incoming pi messages. Similarly the order parent's pi
; msg depends on the incoming lambda messages.

(defun update-ordered-parents-msg (n)
  (when (activated-p n)
    (let ((order-parent (node-order-parent n)))
	; When node is not root node ...
      (when (not (null order-parent))
	(cond
	; Order parent is a predecessor.
	  ((member order-parent (node-predecessors n))
	   (lw-update-overall-lambda n)
	   (lw-update-lambda-msg-to order-parent n))
	; Order parent is a successor
	  ((member order-parent (node-successors n))
	   (lw-update-overall-pi n)
	   (lw-update-pi-msg-to order-parent n))
	; Error condition
	  (t (error "The parent node ~A is neither a predecessor nor successor of ~A"
		    order-parent n)))
	; Activate parent
	(setf (activated-p order-parent) t)))
	; Deactivate self
    (setf (activated-p n) nil)))
  
(defun update-ordered-childrens-msgs (n)
  (lw-update-overall-lambda n)
  (lw-update-overall-pi n)
  (dolist (c (node-order-children n))
    (cond
      ((member c (node-predecessors n))
       (lw-update-lambda-msg-to c n))
      ((member c (node-successors n))
       (lw-update-pi-msg-to c n))
      (t (error "The ordered-child ~A of node ~A is neither a predecessor nor
               successor of ~A" c n n)))))

;----------- Belief operations --------------------------------------------------

(defun increment-beliefs (diagram)
  (dolist (n diagram)
    (for-all-cond-cases (node-case n)
      (incf (belief-of node-case)(* (pi-of node-case)(lambda-of node-case))))))

;-------- Initialization -------------------------------------------------------

(defun set-up-and-initialize-for-lw-infer (belief-net)
  ; Unlink all dummy nodes.
  (unlink-all-dummy-nodes belief-net)
  ; Mark as initialized for likelihood weighting.
  (set-diagram-initialization belief-net :algorithm-type :LW)
  ; Set up conditioning data structure.
  (dolist (n belief-net)(set-up-cond-ds n))
  ; Find and mark cutset and evidence nodes
  (let* ((cutset-members (get-cycle-cutset belief-net))
	 (evidence-nodes (get-evidence-nodes belief-net)))
    (ideal-debug-msg "~% Cutset nodes are ~A" cutset-members)
    (dolist (c cutset-members)
      (setf (is-cs-node-p c) t))
    (ideal-debug-msg "~% Evidence nodes are ~A" evidence-nodes)
    (dolist (e evidence-nodes)
      (setf (is-ev-node-p e) t))
    ; Intialize message data structures
    (dolist (n (order belief-net))(initialize-node-msg-data-structures n))
    ; Find node order in poly tree that results from absorption of evidence nodes.
    (let ((sorted-nodes (find-span-tree belief-net)))
      ; Return ...
      (values
	; evidence case
	(get-evidence-nodes-case evidence-nodes)
	; non instantiated cutset nodes 
	(set-difference cutset-members evidence-nodes)
	; ordered nodes in polytree ("leaf" first)
	sorted-nodes
	; Reverse of the ordered nodes
	(reverse sorted-nodes)))))


(defun get-evidence-nodes-case (e-nodes)
  (mapcar #'(lambda (n)(cons n (node-state n))) e-nodes))

(defun initialize-node-msg-data-structures (node)
  ; Set up overall lambda and lambda msgs to be vectors of 1's.
  (setf (node-lambda-msg node)
	(mapcar #'(lambda (pred)
		    (cons pred (make-vanilla-msg-array pred :initial-element 1.0)))
		(cons node (node-predecessors node))))
  ; Set up belief array
  (setf (node-bel node)(make-probability-array node))
  ; Calc belief by marginalizing from parents 
  (set-up-beliefs-of node)
  ; Set overall pi and pi msgs to copies of belief vector
  (setf (node-pi-msg node)
	(mapcar #'(lambda (succ)(cons succ (copy-seq (node-bel node))))
		(cons node (node-successors node))))
  ; Initialize belief vector to 0 to act as a belief accumulator during propagation
  (fill (node-bel node) 0.0)
  (values))

(defun set-up-beliefs-of (node)
  (for-all-cond-cases (node-case node)
    (setf (belief-of node-case)(marginalize-over-parents-for node-case))))

(defun marginalize-over-parents-for (node-case)
  (let ((total 0))
    (for-all-cond-cases (pred-case (node-predecessors (node-in node-case)))
      (incf total
	    (* (prob-of node-case pred-case)
	       (product-over (n.s pred-case)(pi-msg-of (node-in node-case) (list n.s))))))
    (values total)))

