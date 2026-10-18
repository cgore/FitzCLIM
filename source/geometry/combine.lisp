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

(defun rectangles-span-union-p (rectangle-1 rectangle-2)
  "Return true when the union of the two rectangles is itself a rectangle
because they share a full span on one axis and meet on the other."
  (or (and (= (rectangle-min-y rectangle-1) (rectangle-min-y rectangle-2))
           (= (rectangle-max-y rectangle-1) (rectangle-max-y rectangle-2))
           (not (or (< (rectangle-max-x rectangle-1) (rectangle-min-x rectangle-2))
                    (< (rectangle-max-x rectangle-2) (rectangle-min-x rectangle-1)))))
      (and (= (rectangle-min-x rectangle-1) (rectangle-min-x rectangle-2))
           (= (rectangle-max-x rectangle-1) (rectangle-max-x rectangle-2))
           (not (or (< (rectangle-max-y rectangle-1) (rectangle-min-y rectangle-2))
                    (< (rectangle-max-y rectangle-2) (rectangle-min-y rectangle-1)))))))

(defun try-join-rectangles (rectangle-1 rectangle-2)
  "Return one rectangle covering both arguments when that union is a
rectangle, and NIL otherwise."
  (when (or (region-contains-region-p rectangle-1 rectangle-2)
            (region-contains-region-p rectangle-2 rectangle-1)
            (rectangles-span-union-p rectangle-1 rectangle-2))
    (join-two-rectangles rectangle-1 rectangle-2)))

(defun merge-rectangle-list (rectangles)
  "Join rectangles whose union is a single rectangle, and return the list."
  (let ((pending (copy-list rectangles))
        (changed t))
    (loop while changed
          do (setf changed nil)
             (loop named scan
                   for tail on pending
                   for left = (first tail)
                   do (loop for right in (rest tail)
                            for joined = (try-join-rectangles left right)
                            when joined
                              do (setf pending
                                       (cons joined
                                             (remove right
                                                     (remove left pending
                                                             :count 1)
                                                     :count 1))
                                       changed t)
                                 (return-from scan nil))))
    pending))

(defun point-covered-p (members point)
  "Return true when some member region contains POINT."
  (some (lambda (member)
          (region-contains-position-p member (point-x point) (point-y point)))
        members))

(defun make-region-from-members (members)
  "Return one region with the same points as the union of MEMBERS.
Empty input is +NOWHERE+.  A single member is returned as itself.
Rectangles that form a larger rectangle are joined."
  (let ((flat nil))
    (dolist (member members)
      (typecase member
        (everywhere (return-from make-region-from-members +everywhere+))
        (nowhere nil)
        (standard-region-set
         (setf flat (append (region-set-regions member) flat)))
        (t (push member flat))))
    (let* ((rectangles (merge-rectangle-list (remove-if-not #'rectanglep flat)))
           (others (remove-if #'rectanglep flat))
           (kept (remove-if (lambda (point)
                              (point-covered-p rectangles point))
                            (remove-if-not #'pointp others)))
           (rest (remove-if #'pointp others))
           (all (append rectangles kept rest)))
      (cond ((null all) +nowhere+)
            ((null (rest all)) (first all))
            (t (make-instance 'standard-region-set :regions all))))))

(defmethod region-union ((region region) &rest regions)
  (make-region-from-members (cons region regions)))

(defgeneric region-intersection-2 (region-1 region-2)
  (:documentation
   "Return the points that lie in both REGION-1 and REGION-2."))

(defmethod region-intersection-2 ((region-1 everywhere) (region-2 region))
  region-2)

(defmethod region-intersection-2 ((region-1 region) (region-2 everywhere))
  region-1)

(defmethod region-intersection-2 ((region-1 nowhere) (region-2 region))
  (declare (ignore region-2))
  +nowhere+)

(defmethod region-intersection-2 ((region-1 region) (region-2 nowhere))
  (declare (ignore region-1))
  +nowhere+)

(defmethod region-intersection-2 ((region-1 standard-rectangle)
                                  (region-2 standard-rectangle))
  (or (rectangle-intersection-or-nil region-1 region-2)
      +nowhere+))

(defmethod region-intersection-2 ((region-1 point) (region-2 region))
  (if (region-contains-position-p region-2 (point-x region-1) (point-y region-1))
      region-1
      +nowhere+))

(defmethod region-intersection-2 ((region-1 region) (region-2 point))
  (region-intersection-2 region-2 region-1))

(defmethod region-intersection-2 ((region-1 standard-region-set) (region-2 region))
  (make-region-from-members
   (mapcar (lambda (member) (region-intersection-2 member region-2))
           (region-set-regions region-1))))

(defmethod region-intersection-2 ((region-1 region) (region-2 standard-region-set))
  (region-intersection-2 region-2 region-1))

(defmethod region-intersection-2 ((region-1 standard-polygon) (region-2 point))
  (region-intersection-2 region-2 region-1))

(defmethod region-intersection ((region region) &rest regions)
  (reduce #'region-intersection-2 regions :initial-value region))

(defmethod region-difference ((region1 everywhere) (region2 nowhere))
  (declare (ignore region1 region2))
  +everywhere+)

(defmethod region-difference ((region1 everywhere) (region2 everywhere))
  (declare (ignore region1 region2))
  +nowhere+)

(defmethod region-difference ((region1 everywhere) (region2 region))
  (make-instance 'standard-region-difference
                 :minuend region1
                 :subtrahend region2))

(defmethod region-difference ((region1 standard-rectangle)
                              (region2 standard-rectangle))
  (make-region-from-members (rectangle-difference-list region1 region2)))

(defmethod region-difference ((region1 standard-region-set) (region2 region))
  (make-region-from-members
   (mapcar (lambda (member) (region-difference member region2))
           (region-set-regions region1))))

(defmethod region-difference ((region1 standard-rectangle)
                              (region2 standard-region-set))
  (reduce #'region-difference
          (region-set-regions region2)
          :initial-value region1))

(defmethod region-contains-region-p ((region1 standard-region-set)
                                     (region2 standard-rectangle))
  (null (reduce (lambda (pieces member)
                  (mapcan (lambda (piece)
                            (typecase member
                              (standard-rectangle
                               (rectangle-difference-list piece member))
                              (everywhere nil)
                              (t (if (region-contains-region-p member piece)
                                     nil
                                     (list piece)))))
                          pieces))
                (region-set-regions region1)
                :initial-value (list region2))))

(defmethod region-contains-region-p ((region1 region)
                                     (region2 standard-region-set))
  (every (lambda (member)
           (region-contains-region-p region1 member))
         (region-set-regions region2)))

(behavior 'region-union
  (let* ((left (make-rectangle* 0 0 2 2))
         (right (make-rectangle* 2 0 5 2))
         (joined (region-union left right))
         (apart (region-union left (make-rectangle* 10 10 12 12))))
    (should-be-true (rectanglep joined))
    (should= 5 (rectangle-width joined))
    (should-be-true (region-set-p apart))
    (should= 2 (length (region-set-regions apart)))
    (should-be-true (region-intersects-region-p left right))
    (should-be-false (region-intersects-region-p
                      left (make-rectangle* 10 10 12 12)))
    (let ((overlap (region-intersection (make-rectangle* 0 0 4 4)
                                        (make-rectangle* 2 2 6 6))))
      (should-be-true (region-equal overlap (make-rectangle* 2 2 4 4))))
    (let ((rest (region-difference (make-rectangle* 0 0 4 2)
                                   (make-rectangle* 1 0 2 2))))
      (should-be-true (region-set-p rest))
      (should-be-false (region-contains-position-p rest 1.5 1))
      (should-be-true (region-contains-position-p rest 0.5 1))
      (should-be-true (region-contains-position-p rest 3 1)))
    (let* ((hole (make-rectangle* 1 1 2 2))
           (plane (region-difference +everywhere+ hole)))
      (should-be-true (region-contains-position-p plane 0 0))
      (should-be-false (region-contains-position-p plane 1.5 1.5)))
    (let ((points (region-union (make-point 1 1) (make-point 2 2))))
      (should-be-true (region-set-p points))
      (should-be-true (region-contains-position-p points 1 1))
      (should-be-false (region-contains-position-p points 0 0)))))
