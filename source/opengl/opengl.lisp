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

(defpackage :FitzCLIM/opengl
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/sheet
        :FitzCLIM/layout
        :FitzCLIM/presentation
        :FitzCLIM/event)
  (:export :add-scene-box
           :opengl-clear-blue
           :opengl-clear-green
           :opengl-clear-red
           :opengl-pane
           :opengl-presentation-at-position
           :opengl-scene
           :pane-ray
           :pick-scene))
(in-package :FitzCLIM/opengl)

(defclass scene-box ()
  ((min-x :initarg :min-x :reader scene-box-min-x)
   (min-y :initarg :min-y :reader scene-box-min-y)
   (min-z :initarg :min-z :reader scene-box-min-z)
   (max-x :initarg :max-x :reader scene-box-max-x)
   (max-y :initarg :max-y :reader scene-box-max-y)
   (max-z :initarg :max-z :reader scene-box-max-z)
   (object :initarg :object :reader scene-box-object)
   (type :initarg :type :reader scene-box-type))
  (:documentation
   "An axis-aligned box in the pane's world.  Y increases upward and
the camera looks down the negative Z axis.  OBJECT is what a hit presents."))

(defclass opengl-pane (leaf-sheet sheet-layout-mixin)
  ((scene
    :initform nil
    :accessor opengl-scene
    :documentation "The scene boxes, in any order.  Depth decides a hit.")
   (clear-red
    :initarg :clear-red
    :initform 0
    :accessor opengl-clear-red)
   (clear-green
    :initarg :clear-green
    :initform 0
    :accessor opengl-clear-green)
   (clear-blue
    :initarg :clear-blue
    :initform 0
    :accessor opengl-clear-blue))
  (:documentation
   "A pane whose picture is a 3D scene.  The port draws it.  A pointer
position becomes a ray, and a hit is a presentation.  The pane does not
record 2D output and it is not covered by another sheet."))

(defun add-scene-box (pane object type min-x min-y min-z max-x max-y max-z)
  "Add a box to PANE that presents OBJECT as TYPE."
  (push (make-instance 'scene-box
                       :min-x min-x :min-y min-y :min-z min-z
                       :max-x max-x :max-y max-y :max-z max-z
                       :object object :type type)
        (opengl-scene pane))
  pane)

(defun opengl-pane-size (pane)
  "Return the pixel width and height used to aim a ray."
  (let ((region (sheet-region pane)))
    (if (rectanglep region)
        (values (max 1 (round (rectangle-width region)))
                (max 1 (round (rectangle-height region))))
        (values (max 1 (round (or (pane-width-hint pane) 100)))
                (max 1 (round (or (pane-height-hint pane) 100)))))))

(defun pane-ray (pane x y)
  "Return the origin and direction of the ray through pixel X, Y.
The origin is on the Z=0 plane.  The direction is (0, 0, -1).  X and Y
are pane pixels with Y increasing downward.  World X runs from -1 on
the left to 1 on the right, and world Y runs from 1 at the top to -1
at the bottom."
  (multiple-value-bind (width height) (opengl-pane-size pane)
    (values (- (* 2 (/ x width)) 1)
            (- 1 (* 2 (/ y height)))
            0
            0 0 -1)))

(defun ray-hits-slab? (origin direction lower upper tmin tmax)
  "Narrow TMIN and TMAX to the slab from LOWER to UPPER.
Return the new bounds, or NIL when the ray misses the slab."
  (if (< (abs direction) 1d-12)
      (when (<= lower origin upper)
        (values tmin tmax))
      (let* ((inverse (/ 1d0 direction))
             (first (* (- lower origin) inverse))
             (second (* (- upper origin) inverse))
             (near (max tmin (min first second)))
             (far (min tmax (max first second))))
        (when (<= near far)
          (values near far)))))

(defun ray-box-distance (origin-x origin-y origin-z direction-x direction-y direction-z
                         min-x min-y min-z max-x max-y max-z)
  "Return the nearest non-negative distance at which the ray hits the box.
NIL means the ray misses."
  (multiple-value-bind (tmin tmax)
      (ray-hits-slab? origin-x direction-x min-x max-x 0d0 most-positive-double-float)
    (when tmin
      (multiple-value-bind (tmin tmax)
          (ray-hits-slab? origin-y direction-y min-y max-y tmin tmax)
        (when tmin
          (multiple-value-bind (tmin tmax)
              (ray-hits-slab? origin-z direction-z min-z max-z tmin tmax)
            (when (and tmin (>= tmax 0d0))
              (max tmin 0d0))))))))

(defun pick-scene (pane x y)
  "Return a presentation for the nearest box under pixel X, Y, or NIL."
  (multiple-value-bind (origin-x origin-y origin-z direction-x direction-y direction-z)
      (pane-ray pane x y)
    (let ((best nil)
          (best-distance nil))
      (dolist (box (opengl-scene pane))
        (let ((distance (ray-box-distance origin-x origin-y origin-z
                                          direction-x direction-y direction-z
                                          (scene-box-min-x box)
                                          (scene-box-min-y box)
                                          (scene-box-min-z box)
                                          (scene-box-max-x box)
                                          (scene-box-max-y box)
                                          (scene-box-max-z box))))
          (when (and distance
                     (or (null best-distance) (< distance best-distance)))
            (setf best box
                  best-distance distance))))
      (when best
        (make-presentation (scene-box-object best) (scene-box-type best))))))

(defun opengl-presentation-at-position (pane x y)
  "Return the presentation under pixel X, Y of PANE, or NIL."
  (pick-scene pane x y))

(behavior 'pick-scene
  (let ((pane (make-instance 'opengl-pane :width 100 :height 100)))
    (add-scene-box pane "near" 'string -1/2 -1/2 -2 1/2 1/2 -1)
    (add-scene-box pane "far" 'string -1/2 -1/2 -6 1/2 1/2 -4)
    (multiple-value-bind (x y z dx dy dz) (pane-ray pane 50 50)
      (should= 0 x)
      (should= 0 y)
      (should= 0 z)
      (should= 0 dx)
      (should= 0 dy)
      (should= -1 dz))
    (let ((hit (pick-scene pane 50 50)))
      (should-equal "near" (presentation-object hit))
      (should-eq 'string (presentation-type hit)))
    (should-be-null (pick-scene pane 0 0))
    (let ((event (make-instance 'pointer-button-press-event
                                :sheet pane :button :left :x 50 :y 50)))
      (should-be-true
       (find-applicable-translators (pick-scene pane 50 50) t nil
                                    :event event :window pane :x 50 :y 50)))))

(defmethod compose-space ((pane opengl-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 320 :min-width 20 :max-width +fill+
                                                 :height 240 :min-height 20 :max-height +fill+)))

(register-pane-class :opengl 'opengl-pane)

(behavior 'opengl-pane-space
  (let ((pane (make-pane :opengl :width 64 :height 48)))
    (should-be-true (typep pane 'opengl-pane))
    (should= 64 (space-requirement-width (compose-space pane)))
    (should= 48 (space-requirement-height (compose-space pane)))))
