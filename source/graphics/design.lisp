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

(defpackage :FitzCLIM/graphics/design
  (:use :common-lisp
        :sigma/behave)
  (:export :+background-ink+
           :+flipping-ink+
           :+foreground-ink+
           :colored-design
           :colored-design?
           :design
           :design-rgba
           :designp
           :unit-interval?))
(in-package :FitzCLIM/graphics/design)

(defclass design ()
  ()
  (:documentation
   "Something that can be drawn.  Colors, opacities, and indirect inks
   are designs."))

(defun designp (object)
  "Return true if OBJECT is a design."
  (typep object 'design))

(defun unit-interval? (value)
  "Return true if VALUE is a real from 0 to 1, inclusive."
  (and (realp value) (<= 0 value 1)))

(defclass colored-design (design)
  ()
  (:documentation
   "A uniform design with red, green, blue, and an opacity.  A color is
   a colored design whose opacity is 1."))

(defun colored-design? (object)
  "Return true if OBJECT has red, green, blue, and opacity channels."
  (typep object 'colored-design))

(defgeneric design-rgba (design)
  (:documentation
   "Return the red, green, blue, and opacity of DESIGN.  Each value is
   a real from 0 to 1."))

(defclass indirect-ink (design)
  ()
  (:documentation
   "An ink whose color is taken from the medium when it is drawn."))

(defclass flipping-ink (design)
  ()
  (:documentation
   "An ink that reverses the pixels it draws on.  Drawing it twice
   restores the picture."))

(defvar +foreground-ink+ (make-instance 'indirect-ink)
  "The ink that draws in the medium's foreground color.")

(defvar +background-ink+ (make-instance 'indirect-ink)
  "The ink that draws in the medium's background color.")

(defvar +flipping-ink+ (make-instance 'flipping-ink)
  "The ink that reverses the pixels it is drawn over.")

(behavior '+foreground-ink+
  (should-be-true (designp +foreground-ink+))
  (should-be-true (designp +background-ink+))
  (should-be-true (designp +flipping-ink+))
  (should-be-false (colored-design? +foreground-ink+))
  (should-be-false (colored-design? +flipping-ink+))
  (should-be-true (eq +foreground-ink+ +foreground-ink+))
  (should-be-false (eq +foreground-ink+ +background-ink+))
  (should-be-false (eq +foreground-ink+ +flipping-ink+)))
