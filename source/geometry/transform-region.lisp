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

(defgeneric transform-region (transformation region)
  (:documentation
   "Return the image of REGION under TRANSFORMATION."))

(defun transform-point (transformation point)
  "Return the image of POINT under TRANSFORMATION."
  (multiple-value-bind (x y)
      (transform-position transformation (point-x point) (point-y point))
    (make-point x y)))

(defmethod transform-region ((transformation transformation) (region point))
  (transform-point transformation region))

(defmethod transform-region ((transformation transformation) (region nowhere))
  (declare (ignore transformation region))
  +nowhere+)

(defmethod transform-region ((transformation transformation) (region everywhere))
  (if (transformation-singular-p transformation)
      (error 'singular-transformation)
      +everywhere+))

(defmethod transform-region ((transformation transformation)
                             (region standard-rectangle))
  (if (transformation-rectilinear-p transformation)
      (multiple-value-bind (x1 y1)
          (transform-position transformation
                              (rectangle-min-x region)
                              (rectangle-min-y region))
        (multiple-value-bind (x2 y2)
            (transform-position transformation
                                (rectangle-max-x region)
                                (rectangle-max-y region))
          (make-rectangle* x1 y1 x2 y2)))
      (make-polygon (list (transform-point transformation
                                           (make-point (rectangle-min-x region)
                                                       (rectangle-min-y region)))
                          (transform-point transformation
                                           (make-point (rectangle-max-x region)
                                                       (rectangle-min-y region)))
                          (transform-point transformation
                                           (make-point (rectangle-max-x region)
                                                       (rectangle-max-y region)))
                          (transform-point transformation
                                           (make-point (rectangle-min-x region)
                                                       (rectangle-max-y region))))
                    :convex-p t)))

(defmethod transform-region ((transformation transformation)
                             (region standard-polygon))
  (make-polygon (mapcar (lambda (point) (transform-point transformation point))
                        (polygon-points region))
                :convex-p (polygon-convex-p region)))

(defmethod transform-region ((transformation transformation)
                             (region standard-region-set))
  (make-region-from-members
   (mapcar (lambda (member) (transform-region transformation member))
           (region-set-regions region))))

(export '(transform-region) :FitzCLIM/geometry)

(behavior 'transform-region
  (let ((shift (make-translation-transformation 10 0)))
    (should-be-true (region-equal (make-point 12 4)
                                  (transform-region shift (make-point 2 4))))
    (should-be-true (region-equal (make-rectangle* 10 0 14 3)
                                  (transform-region shift
                                                    (make-rectangle* 0 0 4 3)))))
  (let ((turn (make-transformation 0 -1 1 0 0 0))
        (box (make-rectangle* 0 0 2 1)))
    (should-be-true (region-equal (make-rectangle* -1 0 0 2)
                                  (transform-region turn box))))
  (let* ((shear (make-transformation 1 0.5 0 1 0 0))
         (image (transform-region shear (make-rectangle* 0 0 2 2))))
    (should-be-true (polygonp image))
    (should-be-true (polygon-convex-p image))
    (should-be-true (region-contains-position-p image 1 1))))
