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

(defpackage :FitzCLIM/geometry
  (:use :common-lisp
        :sigma/behave)
  (:export :+everywhere+
           :+nowhere+
           :everywhere-p
           :nowhere-p
           :region
           :region-contains-position-p
           :region-contains-region-p
           :region-difference
           :region-equal
           :region-intersection
           :region-intersects-region-p
           :region-set
           :region-set-p
           :region-set-regions
           :region-union
           :regionp
           :standard-region-difference
           :standard-region-set))
(in-package :FitzCLIM/geometry)

(defclass region ()
  ()
  (:documentation
   "A set of points in the plane.  Points, rectangles, polygons, and
   region sets are regions."))

(defun regionp (object)
  "Return true if OBJECT is a region."
  (typep object 'region))

(defclass nowhere (region)
  ()
  (:documentation "The empty region.  It contains no positions."))

(defclass everywhere (region)
  ()
  (:documentation "The whole plane."))

(defvar +nowhere+ (make-instance 'nowhere)
  "The empty region.")

(defvar +everywhere+ (make-instance 'everywhere)
  "The region that contains every position.")

(defun nowhere-p (object)
  "Return true if OBJECT is the empty region."
  (typep object 'nowhere))

(defun everywhere-p (object)
  "Return true if OBJECT is the whole plane."
  (typep object 'everywhere))

(defclass region-set (region)
  ()
  (:documentation
   "A region made of member regions.  A position is inside the set when
   it is inside some member."))

(defclass standard-region-set (region-set)
  ((regions
    :initarg :regions
    :reader region-set-regions
    :type list
    :documentation "The member regions, in no particular order."))
  (:documentation "A region set stored as a list of member regions."))

(defun region-set-p (object)
  "Return true if OBJECT is a region set."
  (typep object 'region-set))

(defclass standard-region-difference (region)
  ((minuend
    :initarg :minuend
    :reader region-difference-minuend
    :documentation "The region being subtracted from.")
   (subtrahend
    :initarg :subtrahend
    :reader region-difference-subtrahend
    :documentation "The region being removed."))
  (:documentation
   "The points that lie in MINUEND and not in SUBTRAHEND.  Rectangle
   differences that split into rectangles are stored as those rectangles
   instead."))

(defgeneric region-contains-position-p (region x y)
  (:documentation
   "Return true when the position (X, Y) lies in REGION.  Boundaries
   count as inside."))

(defgeneric region-contains-region-p (region1 region2)
  (:documentation
   "Return true when every point of REGION2 lies in REGION1."))

(defgeneric region-equal (region1 region2)
  (:documentation
   "Return true when REGION1 and REGION2 contain the same points."))

(defgeneric region-intersects-region-p (region1 region2)
  (:documentation
   "Return true when REGION1 and REGION2 have at least one point in
   common."))

(defgeneric region-union (region &rest regions)
  (:documentation
   "Return the region that contains a point exactly when REGION or one
   of REGIONS contains it."))

(defgeneric region-intersection (region &rest regions)
  (:documentation
   "Return the region of points that lie in REGION and in every region
   in REGIONS."))

(defgeneric region-difference (region1 region2)
  (:documentation
   "Return the points of REGION1 that do not lie in REGION2."))

(defmethod region-contains-position-p ((region nowhere) x y)
  (declare (ignore x y))
  nil)

(defmethod region-contains-position-p ((region everywhere) x y)
  (declare (ignore x y))
  t)

(defmethod region-contains-position-p ((region standard-region-set) x y)
  (some (lambda (member)
          (region-contains-position-p member x y))
        (region-set-regions region)))

(defmethod region-contains-position-p ((region standard-region-difference) x y)
  (and (region-contains-position-p (region-difference-minuend region) x y)
       (not (region-contains-position-p (region-difference-subtrahend region) x y))))

(defmethod region-contains-region-p ((region1 everywhere) (region2 region))
  (declare (ignore region1 region2))
  t)

(defmethod region-contains-region-p ((region1 region) (region2 nowhere))
  (declare (ignore region1 region2))
  t)

(defmethod region-contains-region-p ((region1 nowhere) (region2 region))
  (declare (ignore region1))
  (nowhere-p region2))

(defmethod region-equal ((region1 region) (region2 region))
  (or (eq region1 region2)
      (and (region-contains-region-p region1 region2)
           (region-contains-region-p region2 region1))))

(defmethod region-intersects-region-p ((region1 region) (region2 region))
  (not (nowhere-p (region-intersection region1 region2))))

(defmethod region-union ((region nowhere) &rest regions)
  (if regions
      (apply #'region-union regions)
      +nowhere+))

(defmethod region-union ((region everywhere) &rest regions)
  (declare (ignore region regions))
  +everywhere+)

(defmethod region-intersection ((region everywhere) &rest regions)
  (if regions
      (apply #'region-intersection regions)
      +everywhere+))

(defmethod region-intersection ((region nowhere) &rest regions)
  (declare (ignore region regions))
  +nowhere+)

(defmethod region-difference ((region1 region) (region2 nowhere))
  (declare (ignore region2))
  region1)

(defmethod region-difference ((region1 nowhere) (region2 region))
  (declare (ignore region1 region2))
  +nowhere+)

(defmethod region-difference ((region1 region) (region2 everywhere))
  (declare (ignore region1 region2))
  +nowhere+)

(behavior '+nowhere+
  (should-be-true (nowhere-p +nowhere+))
  (should-be-true (regionp +nowhere+))
  (should-be-false (region-contains-position-p +nowhere+ 0 0))
  (should-be-true (region-contains-region-p +nowhere+ +nowhere+))
  (should-be-false (region-contains-region-p +nowhere+ +everywhere+))
  (should-be-true (region-equal +nowhere+ (region-intersection +nowhere+ +everywhere+)))
  (should-be-true (region-equal +everywhere+ (region-union +nowhere+ +everywhere+))))

(behavior '+everywhere+
  (should-be-true (everywhere-p +everywhere+))
  (should-be-true (region-contains-position-p +everywhere+ -40 12))
  (should-be-true (region-contains-region-p +everywhere+ +nowhere+))
  (should-be-true (region-equal +nowhere+ (region-difference +everywhere+ +everywhere+)))
  (should-be-false (region-intersects-region-p +everywhere+ +nowhere+)))
