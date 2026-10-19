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

(defpackage :FitzCLIM/pane
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/sheet
        :FitzCLIM/event
        :FitzCLIM/frame
        :FitzCLIM/medium
        :FitzCLIM/output-record
        :FitzCLIM/presentation
        :FitzCLIM/layout)
  (:export :application-pane
           :find-innermost-applicable-presentation
           :presentation-at-position))
(in-package :FitzCLIM/pane)

(defclass application-pane (leaf-sheet output-recording-stream sheet-layout-mixin)
  ()
  (:documentation
   "A sheet that records output and holds presentations.  A pointer
position on the sheet names the presentation drawn there."))

(defmethod initialize-instance :after ((pane application-pane) &key)
  (unless (stream-medium pane)
    (setf (stream-medium pane) (make-instance 'basic-medium)))
  (let ((medium (stream-medium pane)))
    (setf (medium-sheet medium) pane
          (medium-ink medium) (stream-ink pane)
          (medium-line-style medium) (stream-line-style pane)
          (medium-text-style medium) (stream-text-style pane)
          (medium-transformation medium) (stream-transformation pane)
          (medium-clipping-region medium) (stream-clipping-region pane))))

(defun innermost-presentation (record x y)
  "Return the innermost presentation at X and Y in RECORD's coordinates."
  (dolist (child (reverse (output-record-children record)))
    (when (output-record-contains-position-p child x y)
      (multiple-value-bind (child-x child-y) (output-record-position child)
        (let ((found (innermost-presentation child
                                             (- x child-x)
                                             (- y child-y))))
          (when found
            (return-from innermost-presentation found))))
      (when (presentationp child)
        (return-from innermost-presentation child))))
  nil)

(defun presentation-at-position (pane x y)
  "Return the innermost presentation at X and Y on PANE, or NIL.
X and Y are in the pane's coordinates."
  (let ((history (stream-output-history pane)))
    (multiple-value-bind (origin-x origin-y) (output-record-position history)
      (innermost-presentation history (- x origin-x) (- y origin-y)))))

(defun find-innermost-applicable-presentation
    (pane context-type x y &key event frame)
  "Return the innermost presentation at X and Y that fits CONTEXT-TYPE.
NIL for CONTEXT-TYPE skips the translator check."
  (let ((presentation (presentation-at-position pane x y)))
    (when (and presentation
               (or (null context-type)
                   (find-applicable-translators
                    presentation context-type
                    (when frame (frame-command-table frame))
                    :event event
                    :frame frame
                    :window pane
                    :x x
                    :y y)))
      presentation)))

(behavior 'presentation-at-position
  (let ((pane (make-instance 'application-pane)))
    (present "Hi" :stream pane :type 'string)
    (let ((found (presentation-at-position pane 1 0)))
      (should-equal "Hi" (presentation-object found))
      (should-eq 'string (presentation-type found)))
    (should-be-null (presentation-at-position pane 100 100))
    (with-output-as-presentation (pane 5 'integer)
      (draw-rectangle* pane 10 10 20 20))
    (should= 5 (presentation-object (presentation-at-position pane 15 15)))
    (should-be-null (find-innermost-applicable-presentation pane 'integer 1 0))
    (should-equal "Hi"
                  (presentation-object
                   (find-innermost-applicable-presentation pane 'string 1 0)))
    (let ((press (make-instance 'pointer-button-press-event
                                :button :left :x 1 :y 0)))
      (should-equal "Hi"
                    (with-input-context ('string) (object)
                        (throw-highlighted-presentation
                         (presentation-at-position pane 1 0) nil press)
                      (string object))))
    (let ((x (stream-cursor-x pane)))
      (present "Secret" :stream pane :type 'string :sensitive nil)
      (should-be-null (presentation-at-position pane (+ x 1) 0)))
    (with-output-as-presentation (pane "outer" 'string
                                       :allow-sensitive-inferiors nil)
      (with-output-as-presentation (pane "inner" 'string)
        (draw-point* pane 40 40)))
    (should-equal "outer"
                  (presentation-object (presentation-at-position pane 40 40)))))

(defmethod compose-space ((pane application-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 200 :min-width 20 :max-width +fill+
                                                 :height 150 :min-height 20 :max-height +fill+)))

(register-pane-class :application 'application-pane)

(behavior 'application-pane-space
  (let ((pane (make-pane :application :width 30 :max-width +fill+)))
    (should= 30 (space-requirement-width (compose-space pane)))
    (should= 150 (space-requirement-height (compose-space pane)))
    (should= +fill+ (space-requirement-max-width (compose-space pane)))))
