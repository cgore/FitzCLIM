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

(defpackage :FitzCLIM/graphics/line-style
  (:use :common-lisp
        :sigma/behave)
  (:export :contrasting-dash-patterns
           :contrasting-dash-patterns-limit
           :line-style
           :line-style-cap-shape
           :line-style-components
           :line-style-dashes
           :line-style-joint-shape
           :line-style-p
           :line-style-thickness
           :line-style-unit
           :make-line-style
           :standard-line-style))
(in-package :FitzCLIM/graphics/line-style)

(defclass line-style ()
  ()
  (:documentation "The thickness, dashes, joints, and caps of a stroke."))

(defclass standard-line-style (line-style)
  ((unit
    :initarg :unit
    :reader line-style-unit
    :documentation "The unit of the thickness.  :NORMAL, :POINT, or a
    positive real.")
   (thickness
    :initarg :thickness
    :reader line-style-thickness
    :type real
    :documentation "The thickness, in units of the line style.")
   (dashes
    :initarg :dashes
    :reader line-style-dashes
    :documentation "NIL for a solid stroke, T for a default dash, or a
    list of positive dash and gap lengths.")
   (joint-shape
    :initarg :joint-shape
    :reader line-style-joint-shape
    :type keyword
    :documentation "One of :MITER, :BEVEL, :ROUND, or :NONE.")
   (cap-shape
    :initarg :cap-shape
    :reader line-style-cap-shape
    :type keyword
    :documentation "One of :BUTT, :SQUARE, :ROUND, or :NO-END-POINT."))
  (:documentation "A line style stored as its five components."))

(defun line-style-p (object)
  "Return true if OBJECT is a line style."
  (typep object 'line-style))

(defun line-unit? (unit)
  "Return true if UNIT is a line-style unit."
  (or (member unit '(:normal :point))
      (and (realp unit) (plusp unit))))

(defun line-joint? (joint)
  "Return true if JOINT is a line joint shape."
  (member joint '(:miter :bevel :round :none)))

(defun line-cap? (cap)
  "Return true if CAP is a line cap shape."
  (member cap '(:butt :square :round :no-end-point)))

(defun canonicalize-dashes (dashes)
  "Return a stored dash pattern for DASHES."
  (cond
    ((null dashes) nil)
    ((eql dashes t) t)
    ((and (realp dashes) (plusp dashes))
     (list dashes dashes))
    ((typep dashes 'sequence)
     (let ((items (coerce dashes 'list)))
       (cond
         ((null items) nil)
         ((every (lambda (item)
                   (and (realp item) (plusp item)))
                 items)
          items)
         (t (error "Dash lengths must be positive reals.")))))
    (t (error "Dashes must be NIL, T, a positive real, or a sequence of positive reals."))))

(defun make-line-style (&key (unit :normal)
                             (thickness 1)
                             (dashes nil)
                             (joint-shape :miter)
                             (cap-shape :butt))
  "Return a line style.  UNIT is :NORMAL, :POINT, or a positive real.
THICKNESS is a non-negative real in that unit.  DASHES is NIL, T, or
a sequence of positive lengths.  JOINT-SHAPE is :MITER, :BEVEL, :ROUND,
or :NONE.  CAP-SHAPE is :BUTT, :SQUARE, :ROUND, or :NO-END-POINT."
  (check-type unit (satisfies line-unit?))
  (check-type thickness (real 0))
  (check-type joint-shape (satisfies line-joint?))
  (check-type cap-shape (satisfies line-cap?))
  (make-instance 'standard-line-style
                 :unit unit
                 :thickness thickness
                 :dashes (canonicalize-dashes dashes)
                 :joint-shape joint-shape
                 :cap-shape cap-shape))

(defun line-style-components (line-style)
  "Return the unit, thickness, dashes, joint shape, and cap shape."
  (values (line-style-unit line-style)
          (line-style-thickness line-style)
          (line-style-dashes line-style)
          (line-style-joint-shape line-style)
          (line-style-cap-shape line-style)))

(defvar *dash-patterns*
  '((2 2)
    (4 4)
    (6 2)
    (2 4)
    (8 2)
    (4 2 2 2)
    (6 2 2 2)
    (2 2 6 2))
  "Dash patterns returned by CONTRASTING-DASH-PATTERNS, in order.")

(defun contrasting-dash-patterns-limit ()
  "Return how many contrasting dash patterns are in the palette."
  (length *dash-patterns*))

(defun contrasting-dash-patterns (n &optional (k 0))
  "Return a list of N contrasting dash patterns, starting at index K."
  (check-type n (integer 1))
  (check-type k (integer 0))
  (loop for index from k below (+ k n)
        collect (nth (mod index (contrasting-dash-patterns-limit))
                     *dash-patterns*)))

(behavior 'make-line-style
  (let ((style (make-line-style :thickness 2
                                :dashes #(4 2)
                                :joint-shape :round
                                :cap-shape :square
                                :unit :point)))
    (should-be-true (line-style-p style))
    (multiple-value-bind (unit thickness dashes joint cap)
        (line-style-components style)
      (should-eq :point unit)
      (should= 2 thickness)
      (should-equal '(4 2) dashes)
      (should-eq :round joint)
      (should-eq :square cap)))
  (let ((style (make-line-style)))
    (should-eq :normal (line-style-unit style))
    (should= 1 (line-style-thickness style))
    (should-be-null (line-style-dashes style))
    (should-eq :miter (line-style-joint-shape style))
    (should-eq :butt (line-style-cap-shape style)))
  (should-equal '(3 3) (line-style-dashes (make-line-style :dashes 3)))
  (should-be-true
   (handler-case
       (progn (make-line-style :joint-shape :zigzag) nil)
     (error () t))))

(behavior 'contrasting-dash-patterns
  (should= 8 (contrasting-dash-patterns-limit))
  (should-equal '(2 2) (first (contrasting-dash-patterns 1)))
  (should-equal '(2 2) (second (contrasting-dash-patterns 2 7))))
