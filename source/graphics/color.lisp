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

(defpackage :FitzCLIM/graphics/color
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/graphics/design)
  (:export :+black+
           :+blue+
           :+cyan+
           :+green+
           :+magenta+
           :+red+
           :+white+
           :+yellow+
           :color
           :color-ihs
           :color-rgb
           :colorp
           :contrasting-inks
           :contrasting-inks-limit
           :make-gray-color
           :make-ihs-color
           :make-rgb-color
           :standard-color))
(in-package :FitzCLIM/graphics/color)

(defclass color (colored-design)
  ()
  (:documentation "An opaque color."))

(defclass standard-color (color)
  ((red
    :initarg :red
    :reader color-red
    :type real
    :documentation "The red channel, from 0 to 1.")
   (green
    :initarg :green
    :reader color-green
    :type real
    :documentation "The green channel, from 0 to 1.")
   (blue
    :initarg :blue
    :reader color-blue
    :type real
    :documentation "The blue channel, from 0 to 1."))
  (:documentation "A color stored as red, green, and blue."))

(defun colorp (object)
  "Return true if OBJECT is a color."
  (typep object 'color))

(defun make-rgb-color (red green blue)
  "Return an opaque color with the channels RED, GREEN, and BLUE.
Each channel is a real from 0 to 1."
  (check-type red (satisfies unit-interval?))
  (check-type green (satisfies unit-interval?))
  (check-type blue (satisfies unit-interval?))
  (make-instance 'standard-color :red red :green green :blue blue))

(defun color-rgb (color)
  "Return the red, green, and blue channels of COLOR."
  (values (color-red color) (color-green color) (color-blue color)))

(defmethod design-rgba ((design standard-color))
  (values (color-red design) (color-green design) (color-blue design) 1))

(defun make-gray-color (luminance)
  "Return a gray whose red, green, and blue channels are LUMINANCE."
  (make-rgb-color luminance luminance luminance))

(defun ihs-to-rgb (intensity hue saturation)
  "Return red, green, and blue for an IHS color.
Intensity is the average of the three channels.  Saturation is 1 minus
the minimum channel divided by the intensity, or 0 when the intensity
is 0.  Hue is a fraction of a turn: 0 is red, 1/6 is yellow, 1/3 is
green, 1/2 is cyan, 2/3 is blue, and 5/6 is magenta."
  (when (or (zerop saturation) (zerop intensity))
    (return-from ihs-to-rgb (values intensity intensity intensity)))
  (let ((minimum (* intensity (- 1 saturation))))
    (multiple-value-bind (sector fraction) (floor (* (mod hue 1) 6))
      (let* ((chroma (/ (* 3 intensity saturation)
                        (if (evenp sector)
                            (+ 1 fraction)
                            (- 2 fraction))))
             (maximum (+ minimum chroma))
             (middle (+ minimum
                        (* chroma
                           (if (evenp sector)
                               fraction
                               (- 1 fraction))))))
        (case sector
          (0 (values maximum middle minimum))
          (1 (values middle maximum minimum))
          (2 (values minimum maximum middle))
          (3 (values minimum middle maximum))
          (4 (values middle minimum maximum))
          (otherwise (values maximum minimum middle)))))))

(defun rgb-channel-in-cube? (red green blue)
  "Return true if the channels lie in the RGB cube, aside from a tiny
rounding error."
  (flet ((near? (channel)
           (<= -1e-8 channel (+ 1 1e-8))))
    (and (near? red) (near? green) (near? blue))))

(defun clamp-channel (channel)
  "Return CHANNEL clamped to the closed interval from 0 to 1."
  (cond
    ((< channel 0) 0)
    ((> channel 1) 1)
    (t channel)))

(defun make-ihs-color (intensity hue saturation)
  "Return the opaque color described by INTENSITY, HUE, and SATURATION.
See IHS-TO-RGB for the meaning of the three coordinates.  A color that
falls outside the RGB cube signals an error."
  (check-type intensity (satisfies unit-interval?))
  (check-type hue real)
  (check-type saturation (satisfies unit-interval?))
  (multiple-value-bind (red green blue) (ihs-to-rgb intensity hue saturation)
    (unless (rgb-channel-in-cube? red green blue)
      (error "The IHS color ~S falls outside the RGB cube."
             (list intensity hue saturation)))
    (make-rgb-color (clamp-channel red)
                    (clamp-channel green)
                    (clamp-channel blue))))

(defun rgb-to-ihs (red green blue)
  "Return intensity, hue, and saturation for the channels RED, GREEN,
and BLUE.  A gray has hue 0 and saturation 0."
  (let* ((intensity (/ (+ red green blue) 3))
         (minimum (min red green blue))
         (maximum (max red green blue))
         (chroma (- maximum minimum)))
    (if (or (zerop intensity) (zerop chroma))
        (values intensity 0 0)
        (values intensity
                (/ (cond
                     ((= maximum red)
                      (mod (/ (- green blue) chroma) 6))
                     ((= maximum green)
                      (+ (/ (- blue red) chroma) 2))
                     (t
                      (+ (/ (- red green) chroma) 4)))
                   6)
                (- 1 (/ minimum intensity))))))

(defun color-ihs (color)
  "Return the intensity, hue, and saturation of COLOR."
  (multiple-value-bind (red green blue) (color-rgb color)
    (rgb-to-ihs red green blue)))

(defvar +black+ (make-rgb-color 0 0 0)
  "Opaque black.")

(defvar +white+ (make-rgb-color 1 1 1)
  "Opaque white.")

(defvar +red+ (make-rgb-color 1 0 0)
  "Opaque red.")

(defvar +green+ (make-rgb-color 0 1 0)
  "Opaque green.")

(defvar +blue+ (make-rgb-color 0 0 1)
  "Opaque blue.")

(defvar +yellow+ (make-rgb-color 1 1 0)
  "Opaque yellow.")

(defvar +cyan+ (make-rgb-color 0 1 1)
  "Opaque cyan.")

(defvar +magenta+ (make-rgb-color 1 0 1)
  "Opaque magenta.")

(defvar *contrasting-inks*
  (list +black+ +white+ +red+ +green+ +blue+ +yellow+ +cyan+ +magenta+)
  "Inks returned by CONTRASTING-INKS, in order.")

(defun contrasting-inks-limit ()
  "Return how many mutually contrasting inks are in the palette.
Asking CONTRASTING-INKS for more than this cycles through the palette."
  (length *contrasting-inks*))

(defun contrasting-inks (n &optional (k 0))
  "Return a list of N contrasting inks, starting at palette index K."
  (check-type n (integer 1))
  (check-type k (integer 0))
  (loop for index from k below (+ k n)
        collect (nth (mod index (contrasting-inks-limit))
                     *contrasting-inks*)))

(behavior 'make-rgb-color
  (let ((color (make-rgb-color 1/5 2/5 3/5)))
    (should-be-true (colorp color))
    (should-be-true (designp color))
    (should-be-true (colored-design? color))
    (multiple-value-bind (red green blue) (color-rgb color)
      (should= 1/5 red)
      (should= 2/5 green)
      (should= 3/5 blue))
    (multiple-value-bind (red green blue opacity) (design-rgba color)
      (should= 1/5 red)
      (should= 2/5 green)
      (should= 3/5 blue)
      (should= 1 opacity)))
  (let ((gray (make-gray-color 1/4)))
    (multiple-value-bind (red green blue) (color-rgb gray)
      (should= 1/4 red)
      (should= 1/4 green)
      (should= 1/4 blue))
    (multiple-value-bind (intensity hue saturation) (color-ihs gray)
      (should= 1/4 intensity)
      (should= 0 hue)
      (should= 0 saturation)))
  (should-be-false (colorp +foreground-ink+))
  (should-be-false (colorp +flipping-ink+))
  (should-be-true
   (handler-case
       (progn (make-rgb-color 2 0 0) nil)
     (error () t))))

(behavior 'make-ihs-color
  (flet ((channels (intensity hue saturation)
           (color-rgb (make-ihs-color intensity hue saturation))))
    (multiple-value-bind (intensity hue saturation) (color-ihs +red+)
      (should= 1/3 intensity)
      (should= 0 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +green+)
      (should= 1/3 intensity)
      (should= 1/3 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +blue+)
      (should= 1/3 intensity)
      (should= 2/3 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +yellow+)
      (should= 2/3 intensity)
      (should= 1/6 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +cyan+)
      (should= 2/3 intensity)
      (should= 1/2 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +magenta+)
      (should= 2/3 intensity)
      (should= 5/6 hue)
      (should= 1 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +white+)
      (should= 1 intensity)
      (should= 0 hue)
      (should= 0 saturation))
    (multiple-value-bind (intensity hue saturation) (color-ihs +black+)
      (should= 0 intensity)
      (should= 0 hue)
      (should= 0 saturation))
    (multiple-value-bind (red green blue) (channels 1/2 1/4 1)
      (should= 1/2 red)
      (should= 1 green)
      (should= 0 blue))
    (multiple-value-bind (red-1 green-1 blue-1) (channels 1/3 -1/12 1)
      (multiple-value-bind (red-2 green-2 blue-2) (channels 1/3 11/12 1)
        (should= red-1 red-2)
        (should= green-1 green-2)
        (should= blue-1 blue-2))))
  (should-be-true
   (handler-case
       (progn (make-ihs-color 1 0 1) nil)
     (error () t))))

(behavior 'contrasting-inks
  (should= 8 (contrasting-inks-limit))
  (let ((inks (contrasting-inks 3)))
    (should= 3 (length inks))
    (should-eq +black+ (first inks))
    (should-eq +white+ (second inks))
    (should-eq +red+ (third inks)))
  (should-eq +green+ (first (contrasting-inks 1 3)))
  (should-eq +black+ (second (contrasting-inks 2 7))))
