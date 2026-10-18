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

(defpackage :FitzCLIM/graphics/compose
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/opacity)
  (:export :compose-in
           :compose-out
           :compose-over
           :composite-design
           :composite-design-ink-1
           :composite-design-ink-2
           :composite-design-operator
           :composite-design?
           :rgba-design
           :rgba-design?))
(in-package :FitzCLIM/graphics/compose)

(defclass rgba-design (colored-design)
  ((red
    :initarg :red
    :reader rgba-red
    :type real)
   (green
    :initarg :green
    :reader rgba-green
    :type real)
   (blue
    :initarg :blue
    :reader rgba-blue
    :type real)
   (opacity
    :initarg :opacity
    :reader rgba-opacity
    :type real))
  (:documentation
   "A uniform color that is neither fully opaque nor fully transparent."))

(defun rgba-design? (object)
  "Return true if OBJECT is a translucent uniform color."
  (typep object 'rgba-design))

(defmethod design-rgba ((design rgba-design))
  (values (rgba-red design)
          (rgba-green design)
          (rgba-blue design)
          (rgba-opacity design)))

(defclass composite-design (design)
  ((operator
    :initarg :operator
    :reader composite-design-operator
    :type keyword
    :documentation "One of :OVER, :IN, or :OUT.")
   (ink-1
    :initarg :ink-1
    :reader composite-design-ink-1
    :documentation "The first design passed to the operator.")
   (ink-2
    :initarg :ink-2
    :reader composite-design-ink-2
    :documentation "The second design passed to the operator."))
  (:documentation
   "A design recorded as an operator and two designs.  FitzCLIM uses
   this when the two designs are not both uniform colors."))

(defun composite-design? (object)
  "Return true if OBJECT records a compositing operator."
  (typep object 'composite-design))

(defun make-composite (operator ink-1 ink-2)
  "Return a composite design of OPERATOR applied to INK-1 and INK-2."
  (make-instance 'composite-design
                 :operator operator
                 :ink-1 ink-1
                 :ink-2 ink-2))

(defun make-rgba (red green blue opacity)
  "Return a uniform design for the four channels.
Opacity 0 is +TRANSPARENT-INK+.  Opacity 1 is an opaque color."
  (cond
    ((zerop opacity) +transparent-ink+)
    ((= opacity 1) (make-rgb-color red green blue))
    (t (make-instance 'rgba-design
                      :red red
                      :green green
                      :blue blue
                      :opacity opacity))))

(defun porter-duff-over (red-1 green-1 blue-1 opacity-1
                         red-2 green-2 blue-2 opacity-2)
  "Return the Porter-Duff over of two straight-alpha colors."
  (let ((opacity (+ opacity-1 (* opacity-2 (- 1 opacity-1)))))
    (if (zerop opacity)
        (values 0 0 0 0)
        (values (/ (+ (* red-1 opacity-1)
                      (* red-2 opacity-2 (- 1 opacity-1)))
                   opacity)
                (/ (+ (* green-1 opacity-1)
                      (* green-2 opacity-2 (- 1 opacity-1)))
                   opacity)
                (/ (+ (* blue-1 opacity-1)
                      (* blue-2 opacity-2 (- 1 opacity-1)))
                   opacity)
                opacity))))

(defgeneric compose-over (design-1 design-2)
  (:documentation
   "Return DESIGN-1 painted over DESIGN-2.  A fully opaque DESIGN-1
   hides DESIGN-2.  A fully transparent DESIGN-1 leaves DESIGN-2."))

(defgeneric compose-in (design-1 design-2)
  (:documentation
   "Return DESIGN-1 masked by the opacity of DESIGN-2.  The color is
   the color of DESIGN-1."))

(defgeneric compose-out (design-1 design-2)
  (:documentation
   "Return DESIGN-1 masked by the transparency of DESIGN-2."))

(defmethod compose-over ((design-1 colored-design) (design-2 colored-design))
  (multiple-value-bind (red-1 green-1 blue-1 opacity-1) (design-rgba design-1)
    (cond
      ((= opacity-1 1) design-1)
      ((zerop opacity-1) design-2)
      (t
       (multiple-value-bind (red-2 green-2 blue-2 opacity-2) (design-rgba design-2)
         (multiple-value-call #'make-rgba
           (porter-duff-over red-1 green-1 blue-1 opacity-1
                             red-2 green-2 blue-2 opacity-2)))))))

(defmethod compose-over ((design-1 opacity) (design-2 design))
  (if (zerop (opacity-value design-1))
      design-2
      (make-composite :over design-1 design-2)))

(defmethod compose-over ((design-1 colored-design) (design-2 opacity))
  (if (zerop (opacity-value design-2))
      design-1
      (make-composite :over design-1 design-2)))

(defmethod compose-over ((design-1 design) (design-2 design))
  (make-composite :over design-1 design-2))

(defmethod compose-in ((design-1 colored-design) (design-2 colored-design))
  (multiple-value-bind (red green blue opacity-1) (design-rgba design-1)
    (multiple-value-bind (red-2 green-2 blue-2 opacity-2) (design-rgba design-2)
      (declare (ignore red-2 green-2 blue-2))
      (if (= opacity-2 1)
          design-1
          (make-rgba red green blue (* opacity-1 opacity-2))))))

(defmethod compose-in ((design-1 colored-design) (design-2 opacity))
  (multiple-value-bind (red green blue opacity) (design-rgba design-1)
    (make-rgba red green blue (* opacity (opacity-value design-2)))))

(defmethod compose-in ((design-1 opacity) (design-2 design))
  (if (zerop (opacity-value design-1))
      +transparent-ink+
      (make-composite :in design-1 design-2)))

(defmethod compose-in ((design-1 design) (design-2 design))
  (make-composite :in design-1 design-2))

(defmethod compose-out ((design-1 colored-design) (design-2 colored-design))
  (multiple-value-bind (red green blue opacity-1) (design-rgba design-1)
    (multiple-value-bind (red-2 green-2 blue-2 opacity-2) (design-rgba design-2)
      (declare (ignore red-2 green-2 blue-2))
      (make-rgba red green blue (* opacity-1 (- 1 opacity-2))))))

(defmethod compose-out ((design-1 colored-design) (design-2 opacity))
  (let ((factor (- 1 (opacity-value design-2))))
    (if (= factor 1)
        design-1
        (multiple-value-bind (red green blue opacity) (design-rgba design-1)
          (make-rgba red green blue (* opacity factor))))))

(defmethod compose-out ((design-1 opacity) (design-2 design))
  (if (zerop (opacity-value design-1))
      +transparent-ink+
      (make-composite :out design-1 design-2)))

(defmethod compose-out ((design-1 design) (design-2 design))
  (make-composite :out design-1 design-2))

(behavior 'compose-over
  (should-eq +red+ (compose-over +red+ +blue+))
  (should-eq +blue+ (compose-over +transparent-ink+ +blue+))
  (should-eq +red+ (compose-over +red+ +transparent-ink+))
  (let ((ink (compose-over (compose-in +red+ (make-opacity 1/2)) +blue+)))
    (should-be-true (colorp ink))
    (multiple-value-bind (red green blue) (color-rgb ink)
      (should= 1/2 red)
      (should= 0 green)
      (should= 1/2 blue)))
  (let ((ink (compose-over +foreground-ink+ +red+)))
    (should-be-true (composite-design? ink))
    (should-eq :over (composite-design-operator ink))
    (should-eq +foreground-ink+ (composite-design-ink-1 ink))
    (should-eq +red+ (composite-design-ink-2 ink))))

(behavior 'compose-in
  (let ((ink (compose-in +red+ (make-opacity 1/2))))
    (should-be-true (rgba-design? ink))
    (multiple-value-bind (red green blue opacity) (design-rgba ink)
      (should= 1 red)
      (should= 0 green)
      (should= 0 blue)
      (should= 1/2 opacity)))
  (should-eq +red+ (compose-in +red+ +blue+))
  (should-eq +transparent-ink+ (compose-in +red+ +transparent-ink+))
  (should-eq +transparent-ink+ (compose-in +transparent-ink+ +red+)))

(behavior 'compose-out
  (let ((ink (compose-out +red+ (make-opacity 1/4))))
    (multiple-value-bind (red green blue opacity) (design-rgba ink)
      (should= 1 red)
      (should= 0 green)
      (should= 0 blue)
      (should= 3/4 opacity)))
  (should-eq +transparent-ink+ (compose-out +red+ +blue+))
  (should-eq +red+ (compose-out +red+ +transparent-ink+)))
