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

(define-condition singular-transformation (error)
  ()
  (:documentation
   "The transformation has no inverse.  Its linear part collapses the
   plane onto a line or a point.")
  (:report "The transformation is singular and has no inverse."))

(defclass transformation ()
  ()
  (:documentation
   "A mapping of positions to positions.  Applying a composed
   transformation applies the second transformation first."))

(defclass standard-transformation (transformation)
  ((mxx :initarg :mxx :reader transformation-mxx :initform 1)
   (mxy :initarg :mxy :reader transformation-mxy :initform 0)
   (myx :initarg :myx :reader transformation-myx :initform 0)
   (myy :initarg :myy :reader transformation-myy :initform 1)
   (tx :initarg :tx :reader transformation-tx :initform 0)
   (ty :initarg :ty :reader transformation-ty :initform 0))
  (:documentation
   "An affine transformation.  x' = MXX x + MXY y + TX and
   y' = MYX x + MYY y + TY."))

(defun transformationp (object)
  "Return true if OBJECT is a transformation."
  (typep object 'transformation))

(defun make-transformation (mxx mxy myx myy tx ty)
  "Return the affine transformation with the given matrix and translation."
  (check-type mxx real)
  (check-type mxy real)
  (check-type myx real)
  (check-type myy real)
  (check-type tx real)
  (check-type ty real)
  (make-instance 'standard-transformation
                 :mxx mxx :mxy mxy :myx myx :myy myy :tx tx :ty ty))

(defvar +identity-transformation+
  (make-transformation 1 0 0 1 0 0)
  "The transformation that leaves every position where it is.")

(defun make-translation-transformation (dx dy)
  "Return a transformation that adds DX and DY to a position."
  (make-transformation 1 0 0 1 dx dy))

(defun make-scaling-transformation (sx sy &optional (origin nil))
  "Return a transformation that scales by SX and SY about ORIGIN.
ORIGIN defaults to (0, 0)."
  (if origin
      (compose-transformations
       (make-translation-transformation (point-x origin) (point-y origin))
       (compose-transformations
        (make-transformation sx 0 0 sy 0 0)
        (make-translation-transformation (- (point-x origin))
                                         (- (point-y origin)))))
      (make-transformation sx 0 0 sy 0 0)))

(defun make-rotation-transformation (angle &optional (origin nil))
  "Return a transformation that rotates counterclockwise by ANGLE
radians about ORIGIN.  ORIGIN defaults to (0, 0)."
  (let* ((cosine (cos angle))
         (sine (sin angle))
         (rotation (make-transformation cosine (- sine) sine cosine 0 0)))
    (if origin
        (compose-transformations
         (make-translation-transformation (point-x origin) (point-y origin))
         (compose-transformations
          rotation
          (make-translation-transformation (- (point-x origin))
                                           (- (point-y origin)))))
        rotation)))

(defun compose-transformations (transformation-1 transformation-2)
  "Return the transformation that applies TRANSFORMATION-2 first and
TRANSFORMATION-1 second."
  (let ((mxx1 (transformation-mxx transformation-1))
        (mxy1 (transformation-mxy transformation-1))
        (myx1 (transformation-myx transformation-1))
        (myy1 (transformation-myy transformation-1))
        (tx1 (transformation-tx transformation-1))
        (ty1 (transformation-ty transformation-1))
        (mxx2 (transformation-mxx transformation-2))
        (mxy2 (transformation-mxy transformation-2))
        (myx2 (transformation-myx transformation-2))
        (myy2 (transformation-myy transformation-2))
        (tx2 (transformation-tx transformation-2))
        (ty2 (transformation-ty transformation-2)))
    (make-transformation (+ (* mxx1 mxx2) (* mxy1 myx2))
                         (+ (* mxx1 mxy2) (* mxy1 myy2))
                         (+ (* myx1 mxx2) (* myy1 myx2))
                         (+ (* myx1 mxy2) (* myy1 myy2))
                         (+ (* mxx1 tx2) (* mxy1 ty2) tx1)
                         (+ (* myx1 tx2) (* myy1 ty2) ty1))))

(defun transformation-determinant (transformation)
  "Return the determinant of the linear part of TRANSFORMATION."
  (- (* (transformation-mxx transformation) (transformation-myy transformation))
     (* (transformation-mxy transformation) (transformation-myx transformation))))

(defun transformation-singular-p (transformation)
  "Return true when TRANSFORMATION has no inverse."
  (zerop (transformation-determinant transformation)))

(defun invert-transformation (transformation)
  "Return the inverse of TRANSFORMATION."
  (let ((determinant (transformation-determinant transformation)))
    (when (zerop determinant)
      (error 'singular-transformation))
    (let* ((mxx (/ (transformation-myy transformation) determinant))
           (mxy (/ (- (transformation-mxy transformation)) determinant))
           (myx (/ (- (transformation-myx transformation)) determinant))
           (myy (/ (transformation-mxx transformation) determinant))
           (tx (transformation-tx transformation))
           (ty (transformation-ty transformation)))
      (make-transformation mxx mxy myx myy
                           (- (+ (* mxx tx) (* mxy ty)))
                           (- (+ (* myx tx) (* myy ty)))))))

(defun transform-position (transformation x y)
  "Return the image of (X, Y) under TRANSFORMATION, as two values."
  (values (+ (* (transformation-mxx transformation) x)
             (* (transformation-mxy transformation) y)
             (transformation-tx transformation))
          (+ (* (transformation-myx transformation) x)
             (* (transformation-myy transformation) y)
             (transformation-ty transformation))))

(defun untransform-position (transformation x y)
  "Return the position that TRANSFORMATION maps to (X, Y)."
  (transform-position (invert-transformation transformation) x y))

(defun transformation-rectilinear-p (transformation)
  "Return true when TRANSFORMATION maps axis-aligned rectangles to
axis-aligned rectangles."
  (or (and (zerop (transformation-mxy transformation))
           (zerop (transformation-myx transformation)))
      (and (zerop (transformation-mxx transformation))
           (zerop (transformation-myy transformation)))))

(export '(+identity-transformation+
          compose-transformations
          invert-transformation
          make-rotation-transformation
          make-scaling-transformation
          make-transformation
          make-translation-transformation
          singular-transformation
          standard-transformation
          transform-position
          transformation
          transformation-rectilinear-p
          transformationp
          untransform-position)
        :FitzCLIM/geometry)

(defun coordinate-near-p (left right)
  "Return true when LEFT and RIGHT are the same coordinate, allowing a
tiny float error from trigonometric rotation."
  (< (abs (- left right)) 1d-9))

(behavior 'transform-position
  (multiple-value-bind (x y)
      (transform-position +identity-transformation+ 3 4)
    (should= 3 x)
    (should= 4 y))
  (multiple-value-bind (x y)
      (transform-position (make-translation-transformation 10 -2) 1 1)
    (should= 11 x)
    (should= -1 y))
  (multiple-value-bind (x y)
      (transform-position (make-scaling-transformation 2 3) 4 5)
    (should= 8 x)
    (should= 15 y))
  (multiple-value-bind (x y)
      (transform-position (make-scaling-transformation 2 2 (make-point 1 1))
                          2 1)
    (should= 3 x)
    (should= 1 y))
  (multiple-value-bind (x y)
      (transform-position (make-transformation 0 -1 1 0 0 0) 1 0)
    (should= 0 x)
    (should= 1 y))
  (let ((turn (make-rotation-transformation (/ pi 2) (make-point 1 1))))
    (multiple-value-bind (x y) (transform-position turn 1 1)
      (should-be-true (coordinate-near-p x 1))
      (should-be-true (coordinate-near-p y 1)))
    (multiple-value-bind (x y) (transform-position turn 2 1)
      (should-be-true (coordinate-near-p x 1))
      (should-be-true (coordinate-near-p y 2))))
  (let ((shift (make-translation-transformation 5 7)))
    (multiple-value-bind (x y) (untransform-position shift 8 9)
      (should= 3 x)
      (should= 2 y)))
  (should-be-true (transformation-rectilinear-p +identity-transformation+))
  (should-be-true (transformation-rectilinear-p
                   (make-transformation 0 -1 1 0 0 0)))
  (should-be-false (transformation-rectilinear-p
                    (make-transformation 1 -1 1 1 0 0)))
  (block caught
    (handler-case (invert-transformation (make-transformation 1 0 1 0 0 0))
      (singular-transformation () (return-from caught t)))
    (should-be-true nil)))
