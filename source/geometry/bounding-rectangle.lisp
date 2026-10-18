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

(define-condition bounding-rectangle-of-nowhere (error)
  ()
  (:documentation
   "A bounding rectangle was requested for the empty region, which has
   no edges.")
  (:report "The empty region has no bounding rectangle."))

(defgeneric bounding-rectangle* (region)
  (:documentation
   "Return LEFT, TOP, RIGHT, and BOTTOM of the smallest axis-aligned
   rectangle that covers REGION."))

(defmethod bounding-rectangle* ((region nowhere))
  (error 'bounding-rectangle-of-nowhere))

(defmethod bounding-rectangle* ((region everywhere))
  (error 'bounding-rectangle-of-nowhere))

(defmethod bounding-rectangle* ((region point))
  (values (point-x region) (point-y region) (point-x region) (point-y region)))

(defmethod bounding-rectangle* ((region standard-rectangle))
  (rectangle-edges* region))

(defmethod bounding-rectangle* ((region standard-polygon))
  (let ((xs (mapcar #'point-x (polygon-points region)))
        (ys (mapcar #'point-y (polygon-points region))))
    (values (reduce #'min xs) (reduce #'min ys)
            (reduce #'max xs) (reduce #'max ys))))

(defmethod bounding-rectangle* ((region standard-region-set))
  (let ((boxes (mapcar (lambda (member)
                         (multiple-value-list (bounding-rectangle* member)))
                       (remove-if #'nowhere-p (region-set-regions region)))))
    (when (null boxes)
      (error 'bounding-rectangle-of-nowhere))
    (values (reduce #'min boxes :key #'first)
            (reduce #'min boxes :key #'second)
            (reduce #'max boxes :key #'third)
            (reduce #'max boxes :key #'fourth))))

(defun bounding-rectangle (region)
  "Return the smallest axis-aligned rectangle that covers REGION.
The empty region and the whole plane have none, and this signals
BOUNDING-RECTANGLE-OF-NOWHERE for them."
  (multiple-value-bind (left top right bottom) (bounding-rectangle* region)
    (make-rectangle* left top right bottom)))

(defun bounding-rectangle-min-x (region)
  "Return the left edge of the bounding rectangle of REGION."
  (nth-value 0 (bounding-rectangle* region)))

(defun bounding-rectangle-min-y (region)
  "Return the top edge of the bounding rectangle of REGION."
  (nth-value 1 (bounding-rectangle* region)))

(defun bounding-rectangle-max-x (region)
  "Return the right edge of the bounding rectangle of REGION."
  (nth-value 2 (bounding-rectangle* region)))

(defun bounding-rectangle-max-y (region)
  "Return the bottom edge of the bounding rectangle of REGION."
  (nth-value 3 (bounding-rectangle* region)))

(defun bounding-rectangle-width (region)
  "Return the width of the bounding rectangle of REGION."
  (- (bounding-rectangle-max-x region) (bounding-rectangle-min-x region)))

(defun bounding-rectangle-height (region)
  "Return the height of the bounding rectangle of REGION."
  (- (bounding-rectangle-max-y region) (bounding-rectangle-min-y region)))

(defun bounding-rectangle-size (region)
  "Return the width and height of the bounding rectangle of REGION."
  (values (bounding-rectangle-width region)
          (bounding-rectangle-height region)))

(export '(bounding-rectangle
          bounding-rectangle*
          bounding-rectangle-height
          bounding-rectangle-max-x
          bounding-rectangle-max-y
          bounding-rectangle-min-x
          bounding-rectangle-min-y
          bounding-rectangle-of-nowhere
          bounding-rectangle-size
          bounding-rectangle-width)
        :FitzCLIM/geometry)

(behavior 'bounding-rectangle
  (let ((box (bounding-rectangle (make-polygon* 0 1 3 4 1 2))))
    (should= 0 (rectangle-min-x box))
    (should= 1 (rectangle-min-y box))
    (should= 3 (rectangle-max-x box))
    (should= 4 (rectangle-max-y box)))
  (should= 0 (bounding-rectangle-width (make-point 8 9)))
  (should= 0 (bounding-rectangle-height (make-point 8 9)))
  (let ((pair (region-union (make-rectangle* 0 0 1 1)
                            (make-rectangle* 4 2 6 5))))
    (should= 6 (bounding-rectangle-width pair))
    (should= 5 (bounding-rectangle-height pair)))
  (block caught
    (handler-case (bounding-rectangle +nowhere+)
      (bounding-rectangle-of-nowhere () (return-from caught t)))
    (should-be-true nil)))
