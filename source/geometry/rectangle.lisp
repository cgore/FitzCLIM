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

(defclass rectangle (region)
  ()
  (:documentation
   "An axis-aligned rectangle.  Top is the smaller y coordinate, and
   bottom is the larger one.  The boundary is inside the rectangle."))

(defclass standard-rectangle (rectangle)
  ((left
    :initarg :left
    :reader rectangle-min-x
    :type real)
   (top
    :initarg :top
    :reader rectangle-min-y
    :type real)
   (right
    :initarg :right
    :reader rectangle-max-x
    :type real)
   (bottom
    :initarg :bottom
    :reader rectangle-max-y
    :type real))
  (:documentation
   "A rectangle stored by its left, top, right, and bottom edges.
   LEFT is at most RIGHT, and TOP is at most BOTTOM."))

(defun rectanglep (object)
  "Return true if OBJECT is a rectangle."
  (typep object 'rectangle))

(defun make-rectangle* (x1 y1 x2 y2)
  "Return the axis-aligned rectangle with corners (X1, Y1) and (X2, Y2)."
  (check-type x1 real)
  (check-type y1 real)
  (check-type x2 real)
  (check-type y2 real)
  (make-instance 'standard-rectangle
                 :left (min x1 x2)
                 :top (min y1 y2)
                 :right (max x1 x2)
                 :bottom (max y1 y2)))

(defun make-rectangle (point-1 point-2)
  "Return the axis-aligned rectangle with opposite corners POINT-1 and
POINT-2."
  (make-rectangle* (point-x point-1) (point-y point-1)
                   (point-x point-2) (point-y point-2)))

(defun rectangle-edges* (rectangle)
  "Return LEFT, TOP, RIGHT, and BOTTOM of RECTANGLE as four values."
  (values (rectangle-min-x rectangle)
          (rectangle-min-y rectangle)
          (rectangle-max-x rectangle)
          (rectangle-max-y rectangle)))

(defun rectangle-width (rectangle)
  "Return the width of RECTANGLE."
  (- (rectangle-max-x rectangle) (rectangle-min-x rectangle)))

(defun rectangle-height (rectangle)
  "Return the height of RECTANGLE."
  (- (rectangle-max-y rectangle) (rectangle-min-y rectangle)))

(defun rectangle-size (rectangle)
  "Return the width and height of RECTANGLE as two values."
  (values (rectangle-width rectangle) (rectangle-height rectangle)))

(defmethod region-contains-position-p ((region standard-rectangle) x y)
  (and (<= (rectangle-min-x region) x (rectangle-max-x region))
       (<= (rectangle-min-y region) y (rectangle-max-y region))))

(defmethod region-contains-region-p ((region1 standard-rectangle)
                                     (region2 standard-rectangle))
  (and (<= (rectangle-min-x region1) (rectangle-min-x region2))
       (<= (rectangle-min-y region1) (rectangle-min-y region2))
       (>= (rectangle-max-x region1) (rectangle-max-x region2))
       (>= (rectangle-max-y region1) (rectangle-max-y region2))))

(defmethod region-contains-region-p ((region1 standard-rectangle) (region2 point))
  (region-contains-position-p region1 (point-x region2) (point-y region2)))

(defmethod region-equal ((region1 standard-rectangle) (region2 standard-rectangle))
  (and (= (rectangle-min-x region1) (rectangle-min-x region2))
       (= (rectangle-min-y region1) (rectangle-min-y region2))
       (= (rectangle-max-x region1) (rectangle-max-x region2))
       (= (rectangle-max-y region1) (rectangle-max-y region2))))

(defun rectangle-intersection-or-nil (rectangle-1 rectangle-2)
  "Return the intersection rectangle, or NIL when the rectangles miss."
  (let ((left (max (rectangle-min-x rectangle-1) (rectangle-min-x rectangle-2)))
        (top (max (rectangle-min-y rectangle-1) (rectangle-min-y rectangle-2)))
        (right (min (rectangle-max-x rectangle-1) (rectangle-max-x rectangle-2)))
        (bottom (min (rectangle-max-y rectangle-1) (rectangle-max-y rectangle-2))))
    (when (and (<= left right) (<= top bottom))
      (make-rectangle* left top right bottom))))

(defun rectangle-difference-list (rectangle-1 rectangle-2)
  "Return the parts of RECTANGLE-1 that lie outside RECTANGLE-2, as a
list of rectangles.  An empty list means RECTANGLE-2 covers RECTANGLE-1."
  (let ((overlap (rectangle-intersection-or-nil rectangle-1 rectangle-2)))
    (cond ((null overlap)
           (list rectangle-1))
          ((region-equal overlap rectangle-1)
           nil)
          (t
           (let ((left (rectangle-min-x rectangle-1))
                 (top (rectangle-min-y rectangle-1))
                 (right (rectangle-max-x rectangle-1))
                 (bottom (rectangle-max-y rectangle-1))
                 (ileft (rectangle-min-x overlap))
                 (itop (rectangle-min-y overlap))
                 (iright (rectangle-max-x overlap))
                 (ibottom (rectangle-max-y overlap))
                 (pieces nil))
             (when (< top itop)
               (push (make-rectangle* left top right itop) pieces))
             (when (< ibottom bottom)
               (push (make-rectangle* left ibottom right bottom) pieces))
             (when (< left ileft)
               (push (make-rectangle* left itop ileft ibottom) pieces))
             (when (< iright right)
               (push (make-rectangle* iright itop right ibottom) pieces))
             pieces)))))

(defun rectangles-touch-horizontally-p (rectangle-1 rectangle-2)
  "Return true when the rectangles share a vertical edge and the same
vertical span, so their union is one rectangle."
  (and (= (rectangle-min-y rectangle-1) (rectangle-min-y rectangle-2))
       (= (rectangle-max-y rectangle-1) (rectangle-max-y rectangle-2))
       (or (= (rectangle-max-x rectangle-1) (rectangle-min-x rectangle-2))
           (= (rectangle-max-x rectangle-2) (rectangle-min-x rectangle-1)))))

(defun rectangles-touch-vertically-p (rectangle-1 rectangle-2)
  "Return true when the rectangles share a horizontal edge and the same
horizontal span, so their union is one rectangle."
  (and (= (rectangle-min-x rectangle-1) (rectangle-min-x rectangle-2))
       (= (rectangle-max-x rectangle-1) (rectangle-max-x rectangle-2))
       (or (= (rectangle-max-y rectangle-1) (rectangle-min-y rectangle-2))
           (= (rectangle-max-y rectangle-2) (rectangle-min-y rectangle-1)))))

(defun join-two-rectangles (rectangle-1 rectangle-2)
  "Return one rectangle covering RECTANGLE-1 and RECTANGLE-2.  The caller
has already decided that the union is itself a rectangle."
  (make-rectangle* (min (rectangle-min-x rectangle-1) (rectangle-min-x rectangle-2))
                   (min (rectangle-min-y rectangle-1) (rectangle-min-y rectangle-2))
                   (max (rectangle-max-x rectangle-1) (rectangle-max-x rectangle-2))
                   (max (rectangle-max-y rectangle-1) (rectangle-max-y rectangle-2))))

(export '(join-two-rectangles
          make-rectangle
          make-rectangle*
          rectangle
          rectangle-difference-list
          rectangle-edges*
          rectangle-height
          rectangle-intersection-or-nil
          rectangle-max-x
          rectangle-max-y
          rectangle-min-x
          rectangle-min-y
          rectangle-size
          rectangle-width
          rectanglep
          rectangles-touch-horizontally-p
          rectangles-touch-vertically-p
          standard-rectangle)
        :FitzCLIM/geometry)

(behavior 'make-rectangle*
  (let ((rectangle (make-rectangle* 4 1 0 3)))
    (should-be-true (rectanglep rectangle))
    (should= 0 (rectangle-min-x rectangle))
    (should= 1 (rectangle-min-y rectangle))
    (should= 4 (rectangle-max-x rectangle))
    (should= 3 (rectangle-max-y rectangle))
    (should= 4 (rectangle-width rectangle))
    (should= 2 (rectangle-height rectangle))
    (multiple-value-bind (left top right bottom) (rectangle-edges* rectangle)
      (should= 0 left)
      (should= 1 top)
      (should= 4 right)
      (should= 3 bottom))
    (should-be-true (region-contains-position-p rectangle 0 1))
    (should-be-true (region-contains-position-p rectangle 4 3))
    (should-be-true (region-contains-position-p rectangle 2 2))
    (should-be-false (region-contains-position-p rectangle -1 2))
    (should-be-true (region-equal rectangle (make-rectangle (make-point 0 3)
                                                            (make-point 4 1))))
    (should-be-true (region-contains-region-p rectangle (make-point 2 2)))
    (should-be-false (region-contains-region-p rectangle (make-point 5 2)))))

(behavior 'rectangle-difference-list
  (let* ((whole (make-rectangle* 0 0 10 10))
         (hole (make-rectangle* 2 2 8 8))
         (pieces (rectangle-difference-list whole hole)))
    (should= 4 (length pieces))
    (should-be-false (region-contains-position-p hole 1 1))
    (should-be-true (every (lambda (piece)
                             (region-contains-position-p whole
                                                         (rectangle-min-x piece)
                                                         (rectangle-min-y piece)))
                           pieces))
    (should-be-null (rectangle-difference-list hole whole))
    (should= 1 (length (rectangle-difference-list whole (make-rectangle* 20 20 30 30))))))
