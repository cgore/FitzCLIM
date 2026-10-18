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

(defclass polygon (region)
  ()
  (:documentation "A filled polygon.  The boundary is inside."))

(defclass standard-polygon (polygon)
  ((points
    :initarg :points
    :reader polygon-points
    :type list
    :documentation "The vertices, in order.  The first is not repeated.")
   (convex-p
    :initarg :convex-p
    :reader polygon-convex-p
    :initform nil
    :documentation
    "True when the polygon is convex.  Containment of another region
    uses the corners only in that case."))
  (:documentation "A polygon stored as a list of points."))

(defun polygonp (object)
  "Return true if OBJECT is a polygon."
  (typep object 'polygon))

(defun make-polygon (points &key convex-p)
  "Return a polygon through POINTS, which must hold at least three points."
  (let ((point-list (coerce points 'list)))
    (assert (>= (length point-list) 3) (points)
            "A polygon needs at least three points.")
    (assert (every #'pointp point-list) (points)
            "Every vertex of a polygon must be a point.")
    (make-instance 'standard-polygon
                   :points point-list
                   :convex-p convex-p)))

(defun make-polygon* (&rest coordinates)
  "Return a polygon from the flat coordinate list COORDINATES."
  (assert (evenp (length coordinates)) (coordinates)
          "A polygon coordinate list must pair x and y.")
  (make-polygon (loop for (x y) on coordinates by #'cddr
                      collect (make-point x y))))

(defun map-polygon-edges (polygon function)
  "Call FUNCTION on each edge of POLYGON as four coordinates, x1 y1 x2 y2."
  (let ((points (polygon-points polygon)))
    (loop for (start next) on points
          for end = (or next (first points))
          do (funcall function
                      (point-x start) (point-y start)
                      (point-x end) (point-y end)))))

(defun position-on-segment-p (x y x1 y1 x2 y2)
  "Return true when (X, Y) lies on the closed segment from (X1, Y1) to
(X2, Y2)."
  (and (<= (min x1 x2) x (max x1 x2))
       (<= (min y1 y2) y (max y1 y2))
       (= (* (- x2 x1) (- y y1))
          (* (- y2 y1) (- x x1)))))

(defun position-on-polygon-boundary-p (polygon x y)
  "Return true when (X, Y) lies on an edge of POLYGON."
  (block search
    (map-polygon-edges polygon
                       (lambda (x1 y1 x2 y2)
                         (when (position-on-segment-p x y x1 y1 x2 y2)
                           (return-from search t))))
    nil))

(defun position-in-polygon-p (polygon x y)
  "Return true when (X, Y) is inside POLYGON or on its boundary.
The interior test is the even-odd rule."
  (if (position-on-polygon-boundary-p polygon x y)
      t
      (let ((inside nil))
        (map-polygon-edges polygon
                           (lambda (x1 y1 x2 y2)
                             (when (and (not (eq (> y1 y) (> y2 y)))
                                        (< x (+ x1
                                                (/ (* (- x2 x1) (- y y1))
                                                   (- y2 y1)))))
                               (setf inside (not inside)))))
        inside)))

(defmethod region-contains-position-p ((region standard-polygon) x y)
  (position-in-polygon-p region x y))

(defmethod region-contains-region-p ((region1 standard-polygon) (region2 point))
  (region-contains-position-p region1 (point-x region2) (point-y region2)))

(defmethod region-contains-region-p ((region1 standard-polygon)
                                     (region2 standard-rectangle))
  (and (polygon-convex-p region1)
       (region-contains-position-p region1
                                   (rectangle-min-x region2)
                                   (rectangle-min-y region2))
       (region-contains-position-p region1
                                   (rectangle-min-x region2)
                                   (rectangle-max-y region2))
       (region-contains-position-p region1
                                   (rectangle-max-x region2)
                                   (rectangle-min-y region2))
       (region-contains-position-p region1
                                   (rectangle-max-x region2)
                                   (rectangle-max-y region2))))

(export '(make-polygon
          make-polygon*
          map-polygon-edges
          polygon
          polygon-convex-p
          polygon-points
          polygonp
          standard-polygon)
        :FitzCLIM/geometry)

(behavior 'make-polygon*
  (let ((square (make-polygon* 0 0 4 0 4 3 0 3)))
    (should-be-true (polygonp square))
    (should= 4 (length (polygon-points square)))
    (should-be-true (region-contains-position-p square 1 1))
    (should-be-true (region-contains-position-p square 0 0))
    (should-be-true (region-contains-position-p square 4 3))
    (should-be-false (region-contains-position-p square 5 1))
    (should-be-false (region-contains-position-p square -0.1 1))
    (should-be-true (region-contains-region-p square (make-point 2 2)))
    (should-be-false (region-contains-region-p square (make-point 6 0)))))
