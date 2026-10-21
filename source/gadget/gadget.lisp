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

(defpackage :FitzCLIM/gadget
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/sheet
        :FitzCLIM/geometry
        :FitzCLIM/layout)
  (:export :action-gadget
           :activate-gadget
           :basic-gadget
           :check-box
           :deactivate-gadget
           :gadget
           :gadget-activate-callback
           :gadget-active-p
           :gadget-background
           :gadget-client
           :gadget-foreground
           :gadget-id
           :gadget-items
           :gadget-label
           :gadget-max-value
           :gadget-min-value
           :gadget-mode
           :gadget-name-key
           :gadget-orientation
           :gadget-test
           :gadget-text-style
           :gadget-value
           :gadget-value-changed-callback
           :gadget-value-key
           :gadgetp
           :label-pane
           :list-pane
           :note-gadget-activated
           :note-gadget-deactivated
           :option-pane
           :push-button
           :radio-box
           :scroll-bar
           :slider
           :text-field
           :toggle-button
           :value-gadget))
(in-package :FitzCLIM/gadget)

(defun invoke-callback (callback gadget)
  "Call CALLBACK with GADGET.  NIL does nothing.  A list calls each."
  (cond
    ((null callback) nil)
    ((functionp callback) (funcall callback gadget))
    ((symbolp callback) (funcall callback gadget))
    ((listp callback)
     (dolist (function callback)
       (funcall function gadget)))
    (t (error "~S is not a gadget callback." callback))))

(defclass gadget (sheet-parent-mixin sheet-transformation-mixin sheet-layout-mixin)
  ((id
    :initarg :id
    :initform nil
    :accessor gadget-id
    :documentation "A name the client uses to tell this gadget from others.")
   (client
    :initarg :client
    :initform nil
    :accessor gadget-client
    :documentation "The pane or frame that owns this gadget.")
   (active
    :initarg :active
    :initform t
    :accessor gadget-active-p
    :documentation "True when the user can operate this gadget.")
   (text-style
    :initarg :text-style
    :initform nil
    :accessor gadget-text-style
    :documentation "The text style of this gadget.  NIL uses the port default.")
   (foreground
    :initarg :foreground
    :initform nil
    :accessor gadget-foreground
    :documentation "The text color of this gadget.  NIL uses the system color.")
   (background
    :initarg :background
    :initform nil
    :accessor gadget-background
    :documentation "The fill color of this gadget.  NIL uses the system color."))
  (:documentation
   "A sheet the frame manager realizes as a native control.  A native
control does not hold output records."))

(defclass basic-gadget (gadget sheet-leaf-mixin)
  ()
  (:documentation "A gadget with no CLIM children."))

(defun gadgetp (object)
  "Return true if OBJECT is a gadget."
  (typep object 'gadget))

(behavior 'gadget-text-style
  (let ((gadget (make-instance 'basic-gadget)))
    (should-be-null (gadget-text-style gadget))
    (setf (gadget-text-style gadget) :sample)
    (should-eq :sample (gadget-text-style gadget))))

(behavior 'gadget-foreground
  (let ((gadget (make-instance 'basic-gadget)))
    (should-be-null (gadget-foreground gadget))
    (should-be-null (gadget-background gadget))
    (setf (gadget-foreground gadget) :orange)
    (setf (gadget-background gadget) :black)
    (should-eq :orange (gadget-foreground gadget))
    (should-eq :black (gadget-background gadget))))

(defclass value-gadget (gadget)
  ((value
    :initarg :value
    :initform nil
    :reader gadget-value
    :documentation "The gadget's current value.")
   (value-changed-callback
    :initarg :value-changed-callback
    :initform nil
    :accessor gadget-value-changed-callback
    :documentation "Called with the gadget after the value changes."))
  (:documentation "A gadget that holds a value."))

(defclass action-gadget (gadget)
  ((activate-callback
    :initarg :activate-callback
    :initform nil
    :accessor gadget-activate-callback
    :documentation "Called with the gadget when it is activated."))
  (:documentation "A gadget that runs a callback when it is activated."))

(defclass labelled-gadget-mixin ()
  ((label
    :initarg :label
    :initform ""
    :accessor gadget-label
    :documentation "The text shown on the gadget."))
  (:documentation "A gadget that shows a label."))

(defclass oriented-gadget-mixin ()
  ((orientation
    :initarg :orientation
    :initform :horizontal
    :accessor gadget-orientation
    :documentation "Either :HORIZONTAL or :VERTICAL."))
  (:documentation "A gadget that has an orientation."))

(defclass range-gadget-mixin ()
  ((min-value
    :initarg :min-value
    :initform 0
    :accessor gadget-min-value)
   (max-value
    :initarg :max-value
    :initform 1
    :accessor gadget-max-value))
  (:documentation "A gadget whose value lies between a minimum and a maximum."))

(defun check-orientation (orientation)
  "Signal an error unless ORIENTATION is :HORIZONTAL or :VERTICAL."
  (unless (member orientation '(:horizontal :vertical))
    (error "The orientation ~S is not :HORIZONTAL or :VERTICAL." orientation)))

(defmethod initialize-instance :after ((gadget oriented-gadget-mixin) &key)
  (check-orientation (gadget-orientation gadget)))

(defmethod (setf gadget-orientation) :before (orientation (gadget oriented-gadget-mixin))
  (check-orientation orientation))

(defgeneric (setf gadget-value) (value gadget &key invoke-callback)
  (:documentation
   "Set GADGET's value.  INVOKE-CALLBACK true runs the value-changed
callback when the value is different."))

(defmethod (setf gadget-value)
    (value (gadget value-gadget) &key (invoke-callback t))
  (let ((changed (not (equal value (gadget-value gadget)))))
    (setf (slot-value gadget 'value) value)
    (when (and changed invoke-callback)
      (invoke-callback (gadget-value-changed-callback gadget) gadget)))
  value)

(defgeneric note-gadget-activated (client gadget)
  (:documentation "Notice that GADGET became active.")
  (:method (client gadget)
    (declare (ignore client gadget))
    nil))

(defgeneric note-gadget-deactivated (client gadget)
  (:documentation "Notice that GADGET became inactive.")
  (:method (client gadget)
    (declare (ignore client gadget))
    nil))

(defun activate-gadget (gadget)
  "Make GADGET active and tell its client."
  (setf (gadget-active-p gadget) t)
  (note-gadget-activated (gadget-client gadget) gadget)
  gadget)

(defun deactivate-gadget (gadget)
  "Make GADGET inactive and tell its client."
  (setf (gadget-active-p gadget) nil)
  (note-gadget-deactivated (gadget-client gadget) gadget)
  gadget)

(defun activate-callback (gadget)
  "Run GADGET's activate callback when GADGET is active."
  (when (gadget-active-p gadget)
    (invoke-callback (gadget-activate-callback gadget) gadget))
  gadget)

(defclass label-pane (basic-gadget labelled-gadget-mixin)
  ()
  (:documentation "A label.  It has no value and no action."))

(defclass push-button (basic-gadget action-gadget labelled-gadget-mixin)
  ()
  (:documentation "A button.  Activating it runs its callback."))

(defclass toggle-button (basic-gadget value-gadget labelled-gadget-mixin)
  ()
  (:default-initargs :value nil)
  (:documentation "A button whose value is true or false."))

(defclass slider (basic-gadget value-gadget range-gadget-mixin oriented-gadget-mixin)
  ()
  (:default-initargs :value 0)
  (:documentation "A slider.  Its value stays inside its range."))

(defclass scroll-bar (slider)
  ()
  (:documentation "A scroll bar.  It is a slider."))

(defun check-slider-value (gadget value)
  "Signal an error when VALUE is outside GADGET's range."
  (unless (<= (gadget-min-value gadget) value (gadget-max-value gadget))
    (error "The value ~S is outside ~S and ~S."
           value (gadget-min-value gadget) (gadget-max-value gadget))))

(defmethod initialize-instance :after ((gadget slider) &key)
  (check-slider-value gadget (gadget-value gadget)))

(defmethod (setf gadget-value) :before
    (value (gadget slider) &key invoke-callback)
  (declare (ignore invoke-callback))
  (check-slider-value gadget value))

(defclass text-field (basic-gadget value-gadget)
  ()
  (:default-initargs :value "")
  (:documentation "A single line of text."))

(defun check-text-field-value (value)
  "Signal an error when VALUE is not a string."
  (unless (stringp value)
    (error "A text field value must be a string, not ~S." value)))

(defmethod initialize-instance :after ((gadget text-field) &key)
  (check-text-field-value (gadget-value gadget)))

(defmethod (setf gadget-value) :before
    (value (gadget text-field) &key invoke-callback)
  (declare (ignore invoke-callback))
  (check-text-field-value value))

(defclass item-list-gadget (value-gadget)
  ((items
    :initarg :items
    :initform nil
    :accessor gadget-items
    :documentation "The items the gadget offers.")
   (name-key
    :initarg :name-key
    :initform #'identity
    :accessor gadget-name-key
    :documentation "The function from an item to the text shown for it.")
   (value-key
    :initarg :value-key
    :initform #'identity
    :accessor gadget-value-key
    :documentation "The function from an item to the value it selects.")
   (test
    :initarg :test
    :initform #'eql
    :accessor gadget-test
    :documentation "The test that compares values.")
   (mode
    :initarg :mode
    :initform :one-of
    :accessor gadget-mode
    :documentation ":ONE-OF selects one value.  :SOME-OF selects a list."))
  (:documentation "A gadget that chooses from a list of items."))

(defclass list-pane (basic-gadget item-list-gadget)
  ()
  (:documentation "A list of items.  The value is the selection."))

(defclass option-pane (basic-gadget item-list-gadget)
  ()
  (:documentation "A control that shows the current item and offers the rest."))

(defun item-value (gadget item)
  "Return the value ITEM contributes to GADGET."
  (funcall (gadget-value-key gadget) item))

(defun value-in-items? (gadget value)
  "Return true when VALUE is one of GADGET's item values."
  (find value (gadget-items gadget)
        :key (lambda (item) (item-value gadget item))
        :test (gadget-test gadget)))

(defun check-item-list-value (gadget value)
  "Signal an error when VALUE is not a legal selection for GADGET."
  (ecase (gadget-mode gadget)
    (:one-of
     (unless (or (null value) (value-in-items? gadget value))
       (error "~S is not one of the items." value)))
    (:some-of
     (unless (and (listp value)
                  (every (lambda (item) (value-in-items? gadget item)) value))
       (error "~S is not a selection from the items." value)))))

(defmethod initialize-instance :after ((gadget item-list-gadget) &key)
  (check-item-list-value gadget (gadget-value gadget)))

(defmethod (setf gadget-value) :before
    (value (gadget item-list-gadget) &key invoke-callback)
  (declare (ignore invoke-callback))
  (check-item-list-value gadget value))

(defclass radio-box (value-gadget sheet-multiple-child-mixin)
  ()
  (:default-initargs :value nil)
  (:documentation
   "A box of toggle buttons.  Its value is the button whose value is true.
Setting the value selects that button and clears the others."))

(defclass check-box (radio-box)
  ()
  (:default-initargs :value nil)
  (:documentation
   "A box of toggle buttons.  Its value is the list of buttons whose
value is true."))

(defun button-box-value (box)
  "Return the value implied by the buttons in BOX."
  (let ((selected (remove-if-not #'gadget-value (sheet-children box))))
    (if (typep box 'check-box)
        selected
        (first selected))))

(defun button-selected? (box button value)
  "Return true when VALUE selects BUTTON in BOX."
  (if (typep box 'check-box)
      (and (listp value) (member button value :test #'eq))
      (eq button value)))

(defun check-button-box-value (box value)
  "Signal an error when VALUE is not a selection of BOX's buttons."
  (let ((children (sheet-children box)))
    (if (typep box 'check-box)
        (unless (and (listp value)
                     (every (lambda (button) (member button children :test #'eq))
                            value))
          (error "~S is not a selection of the buttons." value))
        (unless (or (null value) (member value children :test #'eq))
          (error "~S is not one of the buttons." value)))))

(defvar *suppress-button-box-update* nil
  "True while a button box is writing its children's values itself.")

(defmethod (setf gadget-value)
    (value (gadget radio-box) &key (invoke-callback t))
  (check-button-box-value gadget value)
  (let ((old (gadget-value gadget))
        (*suppress-button-box-update* t))
    (dolist (button (sheet-children gadget))
      (setf (gadget-value button :invoke-callback nil)
            (button-selected? gadget button value)))
    ;; The toggles would have written this slot.  Put the previous value
    ;; back so the value-gadget method can see whether it changed.
    (setf (slot-value gadget 'value) old)
    (call-next-method (button-box-value gadget) gadget
                      :invoke-callback invoke-callback)))

(defmethod (setf gadget-value) :after
    (value (gadget toggle-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (let ((parent (sheet-parent gadget)))
    (when (and (typep parent 'radio-box)
               (not *suppress-button-box-update*))
      (let ((old (gadget-value parent)))
        (when (and value (not (typep parent 'check-box)))
          (let ((*suppress-button-box-update* t))
            (dolist (button (sheet-children parent))
              (unless (eq button gadget)
                (setf (gadget-value button :invoke-callback nil) nil)))))
        (let ((new (button-box-value parent)))
          (setf (slot-value parent 'value) new)
          (unless (equal new old)
            (invoke-callback (gadget-value-changed-callback parent) parent)))))))

(defmethod compose-space ((pane gadget) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 80 :height 24)))

(defmethod compose-space ((pane label-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (text-span (gadget-label pane)
                                    :height 16
                                    :max-width +fill+)))

(defmethod compose-space ((pane push-button) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (text-span (gadget-label pane)
                                    :pad 16
                                    :height 23
                                    :min-width 75)))

(defmethod compose-space ((pane toggle-button) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (text-span (gadget-label pane)
                                    :pad 24
                                    :height 23
                                    :min-width 75)))

(defmethod compose-space ((pane slider) &key width height)
  (declare (ignore width height))
  (merge-user-space
   pane
   (if (eq (gadget-orientation pane) :vertical)
       (make-space-requirement :width 20 :min-width 20 :max-width 20
                               :height 120 :min-height 20 :max-height +fill+)
       (make-space-requirement :width 120 :min-width 20 :max-width +fill+
                               :height 20 :min-height 20 :max-height 20))))

(defmethod compose-space ((pane text-field) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 120 :min-width 20
                                                 :max-width +fill+
                                                 :height 23 :min-height 23
                                                 :max-height 23)))

(defmethod compose-space ((pane list-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 120 :min-width 20
                                                 :max-width +fill+
                                                 :height 80 :min-height 20
                                                 :max-height +fill+)))

(defmethod compose-space ((pane option-pane) &key width height)
  (declare (ignore width height))
  (merge-user-space pane (make-space-requirement :width 120 :min-width 20
                                                 :max-width +fill+
                                                 :height 23 :min-height 23
                                                 :max-height 23)))

(defmethod compose-space ((pane radio-box) &key width height)
  (declare (ignore width height))
  (compose-box-space pane (sheet-children pane) :vertical 0))

(defmethod allocate-space ((pane radio-box) width height)
  (allocate-box-space pane width height :vertical 0))

(register-pane-class :check-box 'check-box)
(register-pane-class :label 'label-pane)
(register-pane-class :list 'list-pane)
(register-pane-class :list-pane 'list-pane)
(register-pane-class :option 'option-pane)
(register-pane-class :option-pane 'option-pane)
(register-pane-class :push-button 'push-button)
(register-pane-class :radio-box 'radio-box)
(register-pane-class :scroll-bar 'scroll-bar)
(register-pane-class :slider 'slider)
(register-pane-class :text-field 'text-field)
(register-pane-class :toggle-button 'toggle-button)

(behavior 'push-button
  (let* ((pressed nil)
         (button (make-instance 'push-button
                                :label "Go"
                                :id 'go
                                :activate-callback
                                (lambda (gadget)
                                  (setf pressed (gadget-label gadget))))))
    (should-be-true (gadgetp button))
    (should-be-true (sheetp button))
    (should-equal "Go" (gadget-label button))
    (activate-callback button)
    (should-equal "Go" pressed)
    (deactivate-gadget button)
    (setf pressed nil)
    (activate-callback button)
    (should-be-null pressed)
    (activate-gadget button)
    (should-be-true (gadget-active-p button))))

(behavior 'gadget-value
  (let* (seen
         (slider (make-instance 'slider :min-value 0 :max-value 10 :value 2
                                       :value-changed-callback
                                       (lambda (gadget)
                                         (setf seen (gadget-value gadget))))))
    (setf (gadget-value slider) 4)
    (should= 4 seen)
    (setf (gadget-value slider :invoke-callback nil) 5)
    (should= 4 seen)
    (should= 5 (gadget-value slider))
    (should-be-true
     (handler-case
         (progn (setf (gadget-value slider) 11) nil)
       (error () t)))
    (let ((field (make-instance 'text-field :value "a")))
      (setf (gadget-value field) "bc")
      (should-equal "bc" (gadget-value field)))
    (let ((list (make-instance 'list-pane
                               :items '("red" "green")
                               :value '("red")
                               :test #'equal
                               :mode :some-of)))
      (setf (gadget-value list) '("green" "red"))
      (should-equal '("green" "red") (gadget-value list)))))

(behavior 'radio-box
  (let* ((fires 0)
         (box (make-instance 'radio-box
                             :value-changed-callback
                             (lambda (gadget)
                               (declare (ignore gadget))
                               (incf fires))))
         (one (make-instance 'toggle-button :label "One" :value t))
         (two (make-instance 'toggle-button :label "Two")))
    (adopt-sheet box one)
    (adopt-sheet box two)
    (setf (slot-value box 'value) one)
    (setf (gadget-value two) t)
    (should-be-false (gadget-value one))
    (should-be-true (gadget-value two))
    (should-eq two (gadget-value box))
    (should= 1 fires)
    (setf (gadget-value box) one)
    (should-be-true (gadget-value one))
    (should-be-false (gadget-value two))
    (should-eq one (gadget-value box))
    (should= 2 fires)
    (should-be-true
     (handler-case
         (progn (setf (gadget-value box) (make-instance 'toggle-button)) nil)
       (error () t)))
    (let* ((yes (make-instance 'toggle-button :label "Yes"))
           (no (make-instance 'toggle-button :label "No"))
           (checks (make-instance 'check-box)))
      (adopt-sheet checks yes)
      (adopt-sheet checks no)
      (setf (gadget-value yes) t)
      (setf (gadget-value no) t)
      (should-equal (list yes no) (gadget-value checks))
      (setf (gadget-value checks) (list no))
      (should-be-false (gadget-value yes))
      (should-be-true (gadget-value no))
      (should-equal (list no) (gadget-value checks)))))

(behavior 'radio-box-layout
  (let* ((one (make-instance 'toggle-button :label "One" :width 40 :height 10))
         (two (make-instance 'toggle-button :label "Two" :width 40 :height 12))
         (box (make-instance 'radio-box)))
    (adopt-sheet box one)
    (adopt-sheet box two)
    (allocate-space box 40 22)
    (multiple-value-bind (x y) (transform-position (sheet-transformation two) 0 0)
      (should= 0 x)
      (should= 10 y))))
