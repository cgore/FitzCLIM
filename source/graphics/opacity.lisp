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

(defpackage :FitzCLIM/graphics/opacity
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/graphics/design)
  (:export :+transparent-ink+
           :make-opacity
           :opacity
           :opacity-value
           :opacityp
           :standard-opacity))
(in-package :FitzCLIM/graphics/opacity)

(defclass opacity (design)
  ()
  (:documentation
   "A mask.  An opacity has no color of its own.  0 is fully
   transparent and 1 is fully opaque."))

(defclass standard-opacity (opacity)
  ((value
    :initarg :value
    :reader opacity-value
    :type real
    :documentation "The opacity, from 0 to 1."))
  (:documentation "An opacity stored as a single real."))

(defun opacityp (object)
  "Return true if OBJECT is an opacity."
  (typep object 'opacity))

(defun make-opacity (value)
  "Return an opacity of VALUE.  VALUE is a real from 0 to 1."
  (check-type value (satisfies unit-interval?))
  (make-instance 'standard-opacity :value value))

(defvar +transparent-ink+ (make-opacity 0)
  "A fully transparent ink.")

(behavior 'make-opacity
  (let ((opacity (make-opacity 1/2)))
    (should-be-true (opacityp opacity))
    (should-be-true (designp opacity))
    (should-be-false (colored-design? opacity))
    (should= 1/2 (opacity-value opacity)))
  (should-be-true (opacityp +transparent-ink+))
  (should= 0 (opacity-value +transparent-ink+))
  (should-be-true
   (handler-case
       (progn (make-opacity 3/2) nil)
     (error () t))))
