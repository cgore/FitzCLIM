;;;; Copyright (c) 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(defpackage :FitzCLIM/sheet
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry)
  (:export :adopt-sheet
           :basic-sheet
           :bury-sheet
           :disown-sheet
           :graft
           :graft-p
           :leaf-sheet
           :map-over-sheets
           :raise-sheet
           :reorder-sheets
           :sheet
           :sheet-adopt-child
           :sheet-ancestor-p
           :sheet-children
           :sheet-disown-child
           :sheet-enabled-children
           :sheet-enabled-p
           :sheet-graft
           :sheet-identity-transformation-mixin
           :sheet-leaf-mixin
           :sheet-multiple-child-mixin
           :sheet-parent
           :sheet-parent-mixin
           :sheet-region
           :sheet-single-child-mixin
           :sheet-transformation
           :sheet-transformation-mixin
           :sheet-viewable-p
           :sheetp
           :single-child-sheet))
(in-package :FitzCLIM/sheet)

(defclass sheet ()
  ((region
    :initarg :region
    :initform +everywhere+
    :accessor sheet-region
    :documentation "The region, in the sheet's own coordinate system.")
   (enabled
    :initarg :enabled
    :initform t
    :accessor sheet-enabled-p
    :documentation "True when the sheet is willing to be shown."))
  (:documentation
   "A node in the window tree.  Its region is in its own coordinates.
   Its transformation maps those coordinates into its parent's."))

(defun sheetp (object)
  "Return true if OBJECT is a sheet."
  (typep object 'sheet))

(defmethod (setf sheet-region) :before (region (sheet sheet))
  (check-type region region))

(defmethod (setf sheet-enabled-p) :before (enabled (sheet sheet))
  (check-type enabled boolean))

(defclass sheet-parent-mixin (sheet)
  ((parent
    :initform nil
    :accessor %parent
    :documentation "The sheet that adopted this one, or NIL."))
  (:documentation "A sheet that can be adopted."))

(defgeneric sheet-parent (sheet)
  (:documentation "Return the sheet that adopted SHEET, or NIL."))

(defmethod sheet-parent ((sheet sheet))
  nil)

(defmethod sheet-parent ((sheet sheet-parent-mixin))
  (%parent sheet))

(defclass sheet-leaf-mixin (sheet)
  ()
  (:documentation "A sheet that cannot adopt children."))

(defclass sheet-single-child-mixin (sheet)
  ((children
    :initform nil
    :accessor %children))
  (:documentation "A sheet that adopts at most one child."))

(defclass sheet-multiple-child-mixin (sheet)
  ((children
    :initform nil
    :accessor %children))
  (:documentation
   "A sheet that adopts any number of children.  The child list runs
   from bottommost to topmost."))

(defgeneric sheet-children (sheet)
  (:documentation
   "Return the children of SHEET, from bottommost to topmost.  The
   result is a fresh list."))

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (copy-list (%children sheet)))

(defmethod sheet-children ((sheet sheet-multiple-child-mixin))
  (copy-list (%children sheet)))

(defclass sheet-identity-transformation-mixin (sheet)
  ()
  (:documentation "A sheet whose transformation is always the identity."))

(defclass sheet-transformation-mixin (sheet)
  ((transformation
    :initarg :transformation
    :initform +identity-transformation+
    :accessor %transformation))
  (:documentation "A sheet with a settable transformation."))

(defun identity-transformation? (transformation)
  "Return true if TRANSFORMATION leaves every position where it is."
  (and (transformationp transformation)
       (multiple-value-bind (x y) (transform-position transformation 0 0)
         (and (= x 0) (= y 0)))
       (multiple-value-bind (x y) (transform-position transformation 1 0)
         (and (= x 1) (= y 0)))
       (multiple-value-bind (x y) (transform-position transformation 0 1)
         (and (= x 0) (= y 1)))))

(defgeneric sheet-transformation (sheet)
  (:documentation
   "Return the transformation from SHEET's coordinates to its parent's."))

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defmethod sheet-transformation ((sheet sheet-transformation-mixin))
  (%transformation sheet))

(defgeneric (setf sheet-transformation) (transformation sheet)
  (:documentation "Set the transformation of SHEET to TRANSFORMATION."))

(defmethod (setf sheet-transformation)
    (transformation (sheet sheet-identity-transformation-mixin))
  (unless (identity-transformation? transformation)
    (error "This sheet keeps the identity transformation."))
  +identity-transformation+)

(defmethod (setf sheet-transformation)
    (transformation (sheet sheet-transformation-mixin))
  (check-type transformation transformation)
  (setf (%transformation sheet) transformation))

(defclass basic-sheet (sheet-parent-mixin
                       sheet-multiple-child-mixin
                       sheet-transformation-mixin)
  ()
  (:documentation
   "A sheet with a parent, any number of children, and a transformation."))

(defclass leaf-sheet (sheet-parent-mixin
                      sheet-leaf-mixin
                      sheet-transformation-mixin)
  ()
  (:documentation "A sheet that can be adopted and cannot adopt children."))

(defclass single-child-sheet (sheet-parent-mixin
                              sheet-single-child-mixin
                              sheet-transformation-mixin)
  ()
  (:documentation "A sheet that adopts at most one child."))

(defclass graft (sheet-parent-mixin
                 sheet-multiple-child-mixin
                 sheet-identity-transformation-mixin)
  ()
  (:documentation
   "A root sheet.  It stands for a display, and it is never adopted."))

(defun graft-p (object)
  "Return true if OBJECT is a graft."
  (typep object 'graft))

(defun sheet-ancestor-p (sheet putative-ancestor)
  "Return true if PUTATIVE-ANCESTOR adopted SHEET, directly or not."
  (loop for parent = (sheet-parent sheet) then (sheet-parent parent)
        while parent
        thereis (eq parent putative-ancestor)))

(defun link-child (sheet child)
  "Adopt CHILD under SHEET.  CHILD becomes the topmost child."
  (when (graft-p child)
    (error "A graft is a root sheet."))
  (unless (typep child 'sheet-parent-mixin)
    (error "The child cannot be adopted."))
  (when (sheet-parent child)
    (error "The sheet already has a parent."))
  (when (or (eq sheet child) (sheet-ancestor-p sheet child))
    (error "Adopting that sheet would make a cycle."))
  (setf (%parent child) sheet)
  (setf (%children sheet) (append (%children sheet) (list child)))
  child)

(defgeneric sheet-adopt-child (sheet child)
  (:documentation
   "Adopt CHILD under SHEET.  CHILD becomes the topmost child."))

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (declare (ignore child))
  (error "A leaf sheet cannot adopt a child."))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin) (child sheet))
  (when (sheet-children sheet)
    (error "A single-child sheet already has a child."))
  (link-child sheet child))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin) (child sheet))
  (link-child sheet child))

(defun adopt-sheet (sheet child)
  "Adopt CHILD under SHEET.  CHILD becomes the topmost child."
  (sheet-adopt-child sheet child))

(defgeneric sheet-disown-child (sheet child)
  (:documentation "Remove CHILD from SHEET."))

(defmethod sheet-disown-child ((sheet sheet-leaf-mixin) (child sheet))
  (declare (ignore child))
  (error "A leaf sheet has no children."))

(defmethod sheet-disown-child ((sheet sheet) (child sheet))
  (unless (eq (sheet-parent child) sheet)
    (error "The sheet is not that child's parent."))
  (setf (%parent child) nil)
  (setf (%children sheet) (remove child (%children sheet)))
  child)

(defun disown-sheet (child)
  "Remove CHILD from its parent."
  (let ((parent (sheet-parent child)))
    (unless parent
      (error "The sheet has no parent."))
    (sheet-disown-child parent child)))

(defun raise-sheet (sheet)
  "Make SHEET the topmost child of its parent."
  (let ((parent (sheet-parent sheet)))
    (unless parent
      (error "The sheet has no parent."))
    (setf (%children parent)
          (append (remove sheet (%children parent)) (list sheet)))
    sheet))

(defun bury-sheet (sheet)
  "Make SHEET the bottommost child of its parent."
  (let ((parent (sheet-parent sheet)))
    (unless parent
      (error "The sheet has no parent."))
    (setf (%children parent)
          (cons sheet (remove sheet (%children parent))))
    sheet))

(defun reorder-sheets (sheet new-children)
  "Replace the children of SHEET with NEW-CHILDREN, bottommost first.
NEW-CHILDREN must be a permutation of the current children."
  (let ((old (sheet-children sheet)))
    (unless (and (= (length new-children) (length old))
                 (= (length (remove-duplicates new-children :test #'eq))
                    (length new-children))
                 (every (lambda (child) (member child old :test #'eq))
                        new-children))
      (error "The new ordering must be a permutation of the children."))
    (setf (%children sheet) (copy-list new-children))
    sheet))

(defun sheet-enabled-children (sheet)
  "Return the enabled children of SHEET, from bottommost to topmost."
  (remove-if-not #'sheet-enabled-p (sheet-children sheet)))

(defun map-over-sheets (function sheet)
  "Call FUNCTION on SHEET and then on each descendant.
Children are visited from bottommost to topmost."
  (funcall function sheet)
  (map nil
       (lambda (child) (map-over-sheets function child))
       (sheet-children sheet))
  nil)

(defun sheet-graft (sheet)
  "Return the graft at the root of SHEET, or NIL if it has none."
  (cond
    ((graft-p sheet) sheet)
    ((sheet-parent sheet) (sheet-graft (sheet-parent sheet)))
    (t nil)))

(defun sheet-viewable-p (sheet)
  "Return true if SHEET is on a graft and every ancestor is enabled."
  (and (sheet-graft sheet)
       (loop for current = sheet then (sheet-parent current)
             while current
             always (sheet-enabled-p current))))

(behavior 'adopt-sheet
  (let ((parent (make-instance 'basic-sheet))
        (bottom (make-instance 'basic-sheet))
        (top (make-instance 'leaf-sheet)))
    (should-be-true (sheetp parent))
    (should-be-null (sheet-parent parent))
    (should-be-null (sheet-children parent))
    (should-be-true (sheet-enabled-p parent))
    (should-be-true (region-equal +everywhere+ (sheet-region parent)))
    (should-be-true (identity-transformation? (sheet-transformation parent)))
    (adopt-sheet parent bottom)
    (adopt-sheet parent top)
    (should-eq parent (sheet-parent bottom))
    (should-equal (list bottom top) (sheet-children parent))
    (should-be-true (sheet-ancestor-p top parent))
    (should-be-false (sheet-ancestor-p parent top))
    (raise-sheet bottom)
    (should-equal (list top bottom) (sheet-children parent))
    (bury-sheet bottom)
    (should-equal (list bottom top) (sheet-children parent))
    (reorder-sheets parent (list top bottom))
    (should-equal (list top bottom) (sheet-children parent))
    (setf (sheet-enabled-p top) nil)
    (should-equal (list bottom) (sheet-enabled-children parent))
    (disown-sheet top)
    (should-be-null (sheet-parent top))
    (should-equal (list bottom) (sheet-children parent)))
  (let ((leaf (make-instance 'leaf-sheet))
        (child (make-instance 'leaf-sheet))
        (only (make-instance 'single-child-sheet)))
    (should-be-true
     (handler-case
         (progn (adopt-sheet leaf child) nil)
       (error () t)))
    (adopt-sheet only child)
    (should-be-true
     (handler-case
         (progn (adopt-sheet only (make-instance 'leaf-sheet)) nil)
       (error () t)))
    (should-eq child (first (sheet-children only)))))

(behavior 'sheet-graft
  (let ((graft (make-instance 'graft :region (make-rectangle* 0 0 80 40)))
        (child (make-instance 'basic-sheet))
        (loose (make-instance 'basic-sheet)))
    (should-be-true (graft-p graft))
    (should-be-true (sheet-viewable-p graft))
    (should-be-false (sheet-viewable-p loose))
    (should-be-true
     (handler-case
         (progn (adopt-sheet child graft) nil)
       (error () t)))
    (adopt-sheet graft child)
    (should-eq graft (sheet-graft child))
    (should-be-true (sheet-viewable-p child))
    (setf (sheet-enabled-p graft) nil)
    (should-be-false (sheet-viewable-p graft))
    (should-be-false (sheet-viewable-p child))
    (should-be-true
     (handler-case
         (progn
           (setf (sheet-transformation graft)
                 (make-translation-transformation 1 2))
           nil)
       (error () t)))))

(behavior 'sheet-transformation
  (let ((sheet (make-instance 'basic-sheet))
        (shift (make-translation-transformation 10 4))
        (box (make-rectangle* 1 2 3 4)))
    (setf (sheet-transformation sheet) shift)
    (should-eq shift (sheet-transformation sheet))
    (setf (sheet-region sheet) box)
    (should-be-true (region-equal box (sheet-region sheet)))
    (let ((seen nil))
      (map-over-sheets (lambda (item) (push item seen)) sheet)
      (should-equal (list sheet) seen))
    (let ((parent (make-instance 'basic-sheet))
          (child (make-instance 'basic-sheet)))
      (adopt-sheet sheet parent)
      (should-be-true
       (handler-case
           (progn (adopt-sheet parent sheet) nil)
         (error () t)))
      (adopt-sheet parent child)
      (let ((seen nil))
        (map-over-sheets (lambda (item) (push item seen)) parent)
        (should-equal (list child parent) seen)))))
