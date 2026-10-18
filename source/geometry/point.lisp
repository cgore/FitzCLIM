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

(in-package :FitzCLIM/geometry)

(defclass point (region)
  ()
  (:documentation "A single position in the plane."))

(defclass standard-point (point)
  ((x
    :initarg :x
    :reader point-x
    :type real
    :documentation "The x coordinate.")
   (y
    :initarg :y
    :reader point-y
    :type real
    :documentation "The y coordinate."))
  (:documentation "A point stored as two real coordinates."))

(defun pointp (object)
  "Return true if OBJECT is a point."
  (typep object 'point))

(defun make-point (x y)
  "Return a point at the coordinates X and Y."
  (check-type x real)
  (check-type y real)
  (make-instance 'standard-point :x x :y y))

(defun point-position (point)
  "Return the x and y coordinates of POINT as two values."
  (values (point-x point) (point-y point)))

(defmethod region-contains-position-p ((region point) x y)
  (and (= x (point-x region))
       (= y (point-y region))))

(defmethod region-contains-region-p ((region1 region) (region2 point))
  (region-contains-position-p region1 (point-x region2) (point-y region2)))

(defmethod region-contains-region-p ((region1 point) (region2 point))
  (and (= (point-x region1) (point-x region2))
       (= (point-y region1) (point-y region2))))

(defmethod region-equal ((region1 point) (region2 point))
  (and (= (point-x region1) (point-x region2))
       (= (point-y region1) (point-y region2))))

(defmethod region-intersection ((region point) &rest regions)
  (if (every (lambda (other)
               (region-contains-position-p other (point-x region) (point-y region)))
             regions)
      region
      +nowhere+))

(defmethod region-difference ((region1 point) (region2 region))
  (if (region-contains-position-p region2 (point-x region1) (point-y region1))
      +nowhere+
      region1))

(export '(make-point
          point
          point-position
          point-x
          point-y
          pointp
          standard-point)
        :FitzCLIM/geometry)

(behavior 'make-point
  (let ((point (make-point 3 4)))
    (should-be-true (pointp point))
    (should-be-true (regionp point))
    (should= 3 (point-x point))
    (should= 4 (point-y point))
    (multiple-value-bind (x y) (point-position point)
      (should= 3 x)
      (should= 4 y))
    (should-be-true (region-contains-position-p point 3 4))
    (should-be-false (region-contains-position-p point 3 5))
    (should-be-true (region-equal point (make-point 3 4)))
    (should-be-false (region-equal point (make-point 3 5)))
    (should-be-true (region-equal +nowhere+
                                  (region-intersection point +nowhere+)))
    (should-be-true (region-equal point
                                  (region-intersection point +everywhere+)))
    (should-be-true (region-equal +nowhere+
                                  (region-difference point +everywhere+)))
    (should-be-true (region-equal point
                                  (region-difference point +nowhere+)))))
