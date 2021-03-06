;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: Ideal;  Base: 10 -*-

(in-package :ideal)

;;;;********************************************************
;;;;  Copyright (c) 1989, 1992 Rockwell International -- All rights reserved.
;;;;  Rockwell International Science Center Palo Alto Lab
;;;;********************************************************

;;;;;;;;;;;;;;;;;;;;;;;; Sampath ;;;;;;;;;;;;;;;;;;;;


(export '())

;--------------------------------------------------------


;------ Message processing for lw-infer -----------------

;--- Lambdas ---

(defun lw-update-overall-lambda (node)
  	; This is a Pearl style update
  (update-overall-lambda node)
	; This is inclusion of local evidence into the overall lambda
  (multiply-local-evidence-into-overall-lambda node))

(defun multiply-local-evidence-into-overall-lambda (node)
  (for-all-cond-cases (node-case node)
	; When there *is* local evidence and it does not match the node-case ..
    (if (and (local-evidence node)
	     (not (eq (local-evidence node)(state-in node-case))))
	(setf (lambda-of node-case) 0))))

(defun lw-update-lambda-msg-to (parent-node node)
  (update-lambda-msg-to parent-node node))

;---- Pies ----

(defun lw-update-pi-msg-to (succ node)
	; This is a Pearl style update with no normalization
  (update-pi-msg-to succ node :normalize :NO)
	; This step multiplies in the local evidence
  (multiply-local-evidence-into-pi-msg succ node))

(defun multiply-local-evidence-into-pi-msg (succ node)
  (for-all-cond-cases (node-case node)
	; When there *is* local evidence and it does not match the node-case ..
    (when (and (local-evidence node)
	       (not (eq (local-evidence node)(state-in node-case))))
      (setf (pi-msg-of succ node-case) 0))))

(defun lw-update-overall-pi (node)
  (update-overall-pi node))

;-------------------------------------------------------------------------------------
; Finding Span Tree of the belief net

; The node-order field contains nil intially and the layer number of the
; node finally. It is used initially to check whether the node has been
; ordered at all (when finding the root node) and is later used to sort
; the diagram.

; Each step thru the loop spans one connected portion of the graph.

(defun find-span-tree (diagram)
  (let ((root-node nil) (index 0)(component-number 0))
    (loop
      (ideal-debug-msg "~% Working on component number: ~A" (incf component-number))
      (if (null (setq root-node (find-if-not #'node-order diagram)))
	  (return))
      (setq index (find-span-tree-1 diagram root-node index)))
    (values (sort (copy-list diagram) #'> :key #'node-order))))

; This fn spans a connected portion of the graph.

(defun find-span-tree-1 (diagram rt-node index &optional (parent nil))
  (let ((max-index 0)(section-number 0))
    (loop
      (ideal-debug-msg "~%   Working on section number: ~A" (incf section-number))
	; Keep track of the max index generated
      (setq max-index (max max-index
	; Order from the root-node while ignoring arcs from
	; instantiated nodes to their successors.
			   (setq index (number-from rt-node (+ 1 index) parent))))
	; Choose a node at the other (un-ordered) end of one of these ignored
	; arcs as a new root-node.
      (multiple-value-setq (rt-node parent)(find-unordered-node diagram))
	; If there is no such node the entire connected graph is done.
      (if (null parent)(return max-index))
	; If you found a root node record that it is a child of the
	; parent. (Number-from would not know about this and would not
	; have recorded it)
      (push rt-node (node-order-children parent))
	; The index of the parent is the current index.
      (setq index (node-order parent)))))

; Returns two values:  The second value is a node that has already been
; ordered and the first value is an un-ordered neighbour of this node.
; Considering what number-from does --- order all nodes connected to the
; root except that it does not recognize existence of arcs from
; instantiated nodes to their kids, the two values this fn finds will be
; nodes at the ends of precisely such an arc.

(defun find-unordered-node (diagram)
  (let* ((rt-node nil)
	 (parent (find-if #'(lambda (n)
			      (and (node-order n)
				   (setq rt-node
					 (or (find-if-not #'node-order
							  (node-successors n))
					     (find-if-not #'node-order
							  (node-predecessors n))))))
			  diagram)))
    (values rt-node parent)))

(defun number-from (n i &optional (parent nil))
	; Error check
  (when (node-order n)
    (error "Attempt to number node ~A twice. Algorithm error." n))
  (ideal-debug-msg "~% Node: ~A Index:~A" (node-name n) i)
  (setf (node-order n) i
	(node-order-parent n) parent
	(node-order-children n) (delete parent (node-relevant-neighbours n)))
  (let ((max i) temp)
    (incf i)
    (dolist (c (node-order-children n))
      (if (> (setq temp (number-from c i n)) max) (setq max temp)))
    (values max)))

; Basically all neighbours except that links from evidence and cutset
; nodes to their kids are ignored.

(defun node-relevant-neighbours (n)
  (nconc
    (delete-if #'state-is-known-p (copy-list (node-predecessors n)))
    (if (not (state-is-known-p n)) (copy-list (node-successors n)) nil)))

(defun state-is-known-p (n)
  (or (is-ev-node-p n)(is-cs-node-p n)))

