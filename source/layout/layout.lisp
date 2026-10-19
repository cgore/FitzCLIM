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

(defpackage :FitzCLIM/layout
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/sheet)
  (:export :+fill+
           :allocate-box-space
           :allocate-space
           :box-pane
           :compose-box-space
           :compose-space
           :horizontally
           :make-pane
           :make-space-requirement
           :merge-user-space
           :outlining
           :outlining-pane
           :register-pane-class
           :space-requirement
           :space-requirement-combine
           :space-requirement-height
           :space-requirement-max-height
           :space-requirement-max-width
           :space-requirement-min-height
           :space-requirement-min-width
           :space-requirement-p
           :space-requirement-width
           :spacing
           :spacing-pane
           :text-span
           :vertically))
(in-package :FitzCLIM/layout)

(defparameter +fill+ most-positive-fixnum
  "A requested size that takes whatever room the layout can give.")

(defclass space-requirement ()
  ((width :initarg :width :reader space-requirement-width)
   (min-width :initarg :min-width :reader space-requirement-min-width)
   (max-width :initarg :max-width :reader space-requirement-max-width)
   (height :initarg :height :reader space-requirement-height)
   (min-height :initarg :min-height :reader space-requirement-min-height)
   (max-height :initarg :max-height :reader space-requirement-max-height))
  (:documentation
   "The space a pane wants, and the least and most it can take."))

(defun space-requirement-p (object)
  "Return true if OBJECT is a space requirement."
  (typep object 'space-requirement))

(defun normalize-span (preferred minimum maximum)
  "Return PREFERRED clamped into the range from MINIMUM to MAXIMUM."
  (unless (<= minimum maximum)
    (error "The minimum ~S is greater than the maximum ~S." minimum maximum))
  (values (max minimum (min preferred maximum)) minimum maximum))

(defun make-space-requirement (&key (width 0) (min-width width) (max-width width)
                                    (height 0) (min-height height) (max-height height))
  "Return a space requirement.  Each preferred size lies between its
minimum and its maximum."
  (multiple-value-bind (width min-width max-width)
      (normalize-span width min-width max-width)
    (multiple-value-bind (height min-height max-height)
        (normalize-span height min-height max-height)
      (make-instance 'space-requirement
                     :width width
                     :min-width min-width
                     :max-width max-width
                     :height height
                     :min-height min-height
                     :max-height max-height))))

(defun space-requirement-combine (function requirement &rest requirements)
  "Combine REQUIREMENT and REQUIREMENTS, one component at a time.
FUNCTION is applied to each component, as + or MAX would be."
  (let ((all (cons requirement requirements)))
    (flet ((each (reader)
             (reduce function (mapcar reader all))))
      (make-space-requirement
       :width (each #'space-requirement-width)
       :min-width (each #'space-requirement-min-width)
       :max-width (each #'space-requirement-max-width)
       :height (each #'space-requirement-height)
       :min-height (each #'space-requirement-min-height)
       :max-height (each #'space-requirement-max-height)))))

(behavior 'space-requirement-combine
  (let ((result (space-requirement-combine
                 #'max
                 (make-space-requirement :width 2 :height 3)
                 (make-space-requirement :width 5 :height 1))))
    (should= 5 (space-requirement-width result))
    (should= 3 (space-requirement-height result))
    (should-be-true
     (handler-case
         (progn (make-space-requirement :width 1 :min-width 4 :max-width 2) nil)
       (error () t)))))

(defun span-values (preferred default-min default-max hint min-hint max-hint)
  "Apply one axis of user hints to a composed size."
  (let* ((preferred (or hint preferred))
         (minimum (or min-hint (if hint preferred default-min)))
         (maximum (or max-hint (if hint preferred default-max))))
    (normalize-span preferred minimum maximum)))

(defun merge-user-space (pane requirement)
  "Return REQUIREMENT with PANE's size hints applied."
  (unless (typep pane 'sheet-layout-mixin)
    (return-from merge-user-space requirement))
  (multiple-value-bind (width min-width max-width)
      (span-values (space-requirement-width requirement)
                   (space-requirement-min-width requirement)
                   (space-requirement-max-width requirement)
                   (pane-width-hint pane)
                   (pane-min-width-hint pane)
                   (pane-max-width-hint pane))
    (multiple-value-bind (height min-height max-height)
        (span-values (space-requirement-height requirement)
                     (space-requirement-min-height requirement)
                     (space-requirement-max-height requirement)
                     (pane-height-hint pane)
                     (pane-min-height-hint pane)
                     (pane-max-height-hint pane))
      (make-instance 'space-requirement
                     :width width
                     :min-width min-width
                     :max-width max-width
                     :height height
                     :min-height min-height
                     :max-height max-height))))

(defgeneric compose-space (pane &key width height)
  (:documentation
   "Return the space PANE wants.  WIDTH and HEIGHT, when given, are
the space a parent is willing to offer."))

(defmethod compose-space ((pane sheet) &key width height)
  (declare (ignore pane width height))
  (make-space-requirement))

(defun text-span (text &key (pad 0) (height 16) (min-width 8) (max-width nil))
  "Return a space requirement for TEXT.  Each character is eight wide."
  (let ((width (max min-width (+ pad (* 8 (length (or text "")))))))
    (make-space-requirement :width width
                            :min-width width
                            :max-width (or max-width width)
                            :height height
                            :min-height height
                            :max-height height)))

(defun sum-spans (numbers)
  "Return the sum of NUMBERS, or +FILL+ when any of them is +FILL+."
  (let ((total 0))
    (dolist (number numbers total)
      (if (>= number +fill+)
          (return-from sum-spans +fill+)
          (setf total (min +fill+ (+ total number)))))))

(defun add-span (size pad)
  "Return SIZE plus PAD, capped at +FILL+."
  (if (or (>= size +fill+) (>= pad +fill+))
      +fill+
      (min +fill+ (+ size pad))))

(defun box-major (requirements preferred-reader min-reader max-reader spacing)
  "Return the preferred, minimum, and maximum size along a box's stack."
  (let* ((count (length requirements))
         (gaps (if (plusp count) (* spacing (1- count)) 0)))
    (values (add-span (sum-spans (mapcar preferred-reader requirements)) gaps)
            (add-span (sum-spans (mapcar min-reader requirements)) gaps)
            (add-span (sum-spans (mapcar max-reader requirements)) gaps))))

(defun box-minor (requirements preferred-reader min-reader max-reader)
  "Return the preferred, minimum, and maximum size across a box's stack."
  (if (null requirements)
      (values 0 0 0)
      (let ((preferred (reduce #'max requirements :key preferred-reader))
            (minimum (reduce #'max requirements :key min-reader))
            (maximum (reduce #'min requirements :key max-reader)))
        (when (< maximum minimum)
          (setf maximum minimum))
        (normalize-span preferred minimum maximum))))

(defun compose-box-space (pane children orientation spacing)
  "Return the space a stack of CHILDREN needs."
  (let ((requirements (mapcar #'compose-space children)))
    (multiple-value-bind (width min-width max-width height min-height max-height)
        (if (eq orientation :vertical)
            (multiple-value-bind (width min-width max-width)
                (box-minor requirements
                           #'space-requirement-width
                           #'space-requirement-min-width
                           #'space-requirement-max-width)
              (multiple-value-bind (height min-height max-height)
                  (box-major requirements
                             #'space-requirement-height
                             #'space-requirement-min-height
                             #'space-requirement-max-height
                             spacing)
                (values width min-width max-width height min-height max-height)))
            (multiple-value-bind (width min-width max-width)
                (box-major requirements
                           #'space-requirement-width
                           #'space-requirement-min-width
                           #'space-requirement-max-width
                           spacing)
              (multiple-value-bind (height min-height max-height)
                  (box-minor requirements
                             #'space-requirement-height
                             #'space-requirement-min-height
                             #'space-requirement-max-height)
                (values width min-width max-width height min-height max-height))))
      (merge-user-space pane
                        (make-instance 'space-requirement
                                       :width width
                                       :min-width min-width
                                       :max-width max-width
                                       :height height
                                       :min-height min-height
                                       :max-height max-height)))))

(defun grow-sizes (sizes maximums slack)
  "Give SLACK extra room to SIZES, stopping at MAXIMUMS."
  (loop while (plusp slack)
        for indexes = (loop for index below (length sizes)
                            when (< (aref sizes index) (aref maximums index))
                            collect index)
        while indexes
        do (let ((share (max 1 (floor slack (length indexes)))))
             (dolist (index indexes)
               (when (plusp slack)
                 (let ((delta (min share slack
                                   (- (aref maximums index) (aref sizes index)))))
                   (incf (aref sizes index) delta)
                   (decf slack delta))))))
  sizes)

(defun shrink-sizes (sizes minimums excess)
  "Take EXCESS away from SIZES, stopping at MINIMUMS."
  (loop while (plusp excess)
        for indexes = (loop for index below (length sizes)
                            when (> (aref sizes index) (aref minimums index))
                            collect index)
        while indexes
        do (let ((share (max 1 (floor excess (length indexes)))))
             (dolist (index indexes)
               (when (plusp excess)
                 (let ((delta (min share excess
                                   (- (aref sizes index) (aref minimums index)))))
                   (decf (aref sizes index) delta)
                   (decf excess delta))))))
  sizes)

(defun axis-sizes (available spacing requirements preferred-reader min-reader max-reader)
  "Return a size along one axis for each requirement."
  (let* ((count (length requirements))
         (preferred (mapcar preferred-reader requirements))
         (minimum (mapcar min-reader requirements))
         (maximum (mapcar max-reader requirements))
         (sizes (make-array count
                            :initial-contents
                            (mapcar (lambda (preferred minimum maximum)
                                      (max minimum (min preferred maximum)))
                                    preferred minimum maximum)))
         (gaps (if (plusp count) (* spacing (1- count)) 0))
         (slack (- available gaps (reduce #'+ sizes))))
    (cond ((plusp slack)
           (grow-sizes sizes (make-array count :initial-contents maximum) slack))
          ((minusp slack)
           (shrink-sizes sizes (make-array count :initial-contents minimum) (- slack))))
    (coerce sizes 'list)))

(defgeneric allocate-space (pane width height)
  (:documentation
   "Give PANE a region WIDTH by HEIGHT and lay out its children."))

(defmethod allocate-space ((pane sheet) width height)
  (setf (sheet-region pane) (make-rectangle* 0 0 width height))
  (let ((children (sheet-children pane)))
    (when (and children (null (rest children)))
      (setf (sheet-transformation (first children)) +identity-transformation+)
      (allocate-space (first children) width height))))

(defun allocate-box-space (pane width height orientation spacing)
  "Lay CHILDREN of PANE out in a row or a column and recurse."
  (setf (sheet-region pane) (make-rectangle* 0 0 width height))
  (let* ((children (sheet-children pane))
         (requirements (mapcar #'compose-space children))
         (vertical (eq orientation :vertical))
         (sizes (axis-sizes (if vertical height width)
                            spacing
                            requirements
                            (if vertical
                                #'space-requirement-height
                                #'space-requirement-width)
                            (if vertical
                                #'space-requirement-min-height
                                #'space-requirement-min-width)
                            (if vertical
                                #'space-requirement-max-height
                                #'space-requirement-max-width)))
         (position 0))
    (loop for child in children
          for requirement in requirements
          for size in sizes
          do (let* ((minor-min (if vertical
                                   (space-requirement-min-width requirement)
                                   (space-requirement-min-height requirement)))
                    (minor-max (if vertical
                                   (space-requirement-max-width requirement)
                                   (space-requirement-max-height requirement)))
                    (offered (if vertical width height))
                    (minor (max minor-min (min offered minor-max)))
                    (child-width (if vertical minor size))
                    (child-height (if vertical size minor)))
               (setf (sheet-transformation child)
                     (make-translation-transformation (if vertical 0 position)
                                                      (if vertical position 0)))
               (allocate-space child child-width child-height)
               (incf position (+ size spacing))))))

(defclass box-pane (basic-sheet sheet-layout-mixin)
  ((orientation
    :initarg :orientation
    :reader box-orientation
    :documentation "Either :VERTICAL or :HORIZONTAL.")
   (spacing
    :initarg :spacing
    :initform 0
    :accessor box-spacing
    :documentation "The gap between children."))
  (:documentation "A pane that stacks its children in one direction."))

(defmethod initialize-instance :after ((pane box-pane) &key contents orientation)
  (unless (member orientation '(:vertical :horizontal))
    (error "The box orientation ~S is not :VERTICAL or :HORIZONTAL." orientation))
  (dolist (child contents)
    (adopt-sheet pane child)))

(defmethod compose-space ((pane box-pane) &key width height)
  (declare (ignore width height))
  (compose-box-space pane (sheet-children pane)
                     (box-orientation pane) (box-spacing pane)))

(defmethod allocate-space ((pane box-pane) width height)
  (allocate-box-space pane width height (box-orientation pane) (box-spacing pane)))

(defmacro vertically (options &body contents)
  "Stack CONTENTS from top to bottom.  OPTIONS is a plist such as (:SPACING 4)."
  `(make-instance 'box-pane :orientation :vertical ,@options
                  :contents (list ,@contents)))

(defmacro horizontally (options &body contents)
  "Place CONTENTS from left to right.  OPTIONS is a plist such as (:SPACING 4)."
  `(make-instance 'box-pane :orientation :horizontal ,@options
                  :contents (list ,@contents)))

(defclass spacing-pane (single-child-sheet sheet-layout-mixin)
  ((thickness
    :initarg :thickness
    :initform 1
    :accessor spacing-thickness
    :documentation "The margin on each side of the child."))
  (:documentation "A pane that leaves a margin around its one child."))

(defclass outlining-pane (spacing-pane)
  ()
  (:documentation
   "A pane that leaves room for a border around its one child."))

(defmethod initialize-instance :after ((pane spacing-pane) &key contents)
  (when contents
    (unless (null (rest contents))
      (error "A spacing pane adopts one child."))
    (adopt-sheet pane (first contents))))

(defmethod compose-space ((pane spacing-pane) &key width height)
  (declare (ignore width height))
  (let* ((child (first (sheet-children pane)))
         (requirement (if child (compose-space child) (make-space-requirement)))
         (pad (* 2 (spacing-thickness pane))))
    (merge-user-space
     pane
     (make-space-requirement
      :width (add-span (space-requirement-width requirement) pad)
      :min-width (add-span (space-requirement-min-width requirement) pad)
      :max-width (add-span (space-requirement-max-width requirement) pad)
      :height (add-span (space-requirement-height requirement) pad)
      :min-height (add-span (space-requirement-min-height requirement) pad)
      :max-height (add-span (space-requirement-max-height requirement) pad)))))

(defmethod allocate-space ((pane spacing-pane) width height)
  (setf (sheet-region pane) (make-rectangle* 0 0 width height))
  (let ((child (first (sheet-children pane)))
        (pad (spacing-thickness pane)))
    (when child
      (setf (sheet-transformation child)
            (make-translation-transformation pad pad))
      (allocate-space child
                      (max 0 (- width (* 2 pad)))
                      (max 0 (- height (* 2 pad)))))))

(defmacro spacing (options &body contents)
  "Leave a margin around the one pane in CONTENTS.  OPTIONS is a plist."
  (unless (= (length contents) 1)
    (error "SPACING takes one pane."))
  `(make-instance 'spacing-pane ,@options :contents (list ,@contents)))

(defmacro outlining (options &body contents)
  "Leave room for a border around the one pane in CONTENTS."
  (unless (= (length contents) 1)
    (error "OUTLINING takes one pane."))
  `(make-instance 'outlining-pane ,@options :contents (list ,@contents)))

(defvar *pane-aliases* (make-hash-table :test #'eq)
  "Pane keywords registered by the modules that own the classes.")

(defun register-pane-class (alias class)
  "Teach MAKE-PANE that ALIAS means CLASS."
  (setf (gethash alias *pane-aliases*) class)
  alias)

(defun pane-class (type)
  "Return the class that implements the pane type TYPE."
  (or (gethash type *pane-aliases*)
      (and (symbolp type) (find-class type nil) type)
      (error "~S is not a pane type." type)))

(defun make-pane (type &rest initargs &key &allow-other-keys)
  "Return a pane of TYPE.  TYPE is a pane keyword or a class name."
  (apply #'make-instance (pane-class type) initargs))

(defclass fixed-pane (basic-sheet sheet-layout-mixin)
  ()
  (:documentation "A test pane whose size comes from its hints."))

(defmethod compose-space ((pane fixed-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 10 :height 10)))

(register-pane-class :fixed 'fixed-pane)

(behavior 'compose-space
  (let* ((top (make-instance 'fixed-pane :width 10 :height 20))
         (bottom (make-instance 'fixed-pane :width 30 :height 40))
         (box (vertically (:spacing 2) top bottom))
         (requirement (compose-space box)))
    (should= 30 (space-requirement-width requirement))
    (should= 62 (space-requirement-height requirement))
    (allocate-space box 30 62)
    (multiple-value-bind (x y) (transform-position (sheet-transformation bottom) 0 0)
      (should= 0 x)
      (should= 22 y))
    (should= 40 (bounding-rectangle-height (sheet-region bottom)))
    (should= 20 (bounding-rectangle-height (sheet-region top))))
  (let* ((left (make-instance 'fixed-pane :width 10 :height 8))
         (right (make-instance 'fixed-pane :width 12 :height 8))
         (box (horizontally (:spacing 1) left right)))
    (allocate-space box 23 8)
    (multiple-value-bind (x y) (transform-position (sheet-transformation right) 0 0)
      (should= 11 x)
      (should= 0 y)))
  (let* ((child (make-instance 'fixed-pane :width 10 :height 10
                                         :min-height 10 :max-height +fill+))
         (box (vertically () child)))
    (allocate-space box 10 40)
    (should= 40 (bounding-rectangle-height (sheet-region child))))
  (let* ((child (make-instance 'fixed-pane :width 10
                                      :height 50 :min-height 10 :max-height 50))
         (box (vertically () child)))
    (allocate-space box 10 10)
    (should= 10 (bounding-rectangle-height (sheet-region child)))))

(behavior 'spacing
  (let* ((child (make-instance 'fixed-pane :width 10 :height 10))
         (pane (spacing (:thickness 3) child))
         (requirement (compose-space pane)))
    (should= 16 (space-requirement-width requirement))
    (should= 16 (space-requirement-height requirement))
    (allocate-space pane 20 20)
    (multiple-value-bind (x y) (transform-position (sheet-transformation child) 0 0)
      (should= 3 x)
      (should= 3 y))
    (should= 14 (bounding-rectangle-width (sheet-region child)))
    (let ((border (outlining (:thickness 2) (make-instance 'fixed-pane :width 4 :height 4))))
      (should-be-true (typep border 'outlining-pane))
      (allocate-space border 10 10)
      (multiple-value-bind (x y)
          (transform-position (sheet-transformation (first (sheet-children border))) 0 0)
        (should= 2 x)
        (should= 2 y)))))

(behavior 'make-pane
  (let ((pane (make-pane :fixed :width 4 :height 5)))
    (should-be-true (typep pane 'fixed-pane))
    (should= 4 (space-requirement-width (compose-space pane)))
    (should= 5 (space-requirement-height (compose-space pane))))
  (should-be-true
   (handler-case
       (progn (make-pane :missing) nil)
     (error () t))))
