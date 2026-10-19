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

(defpackage :FitzCLIM/event
  (:use :common-lisp
        :sigma/behave)
  (:export :+control-key+
           :+hyper-key+
           :+meta-key+
           :+shift-key+
           :+super-key+
           :add-gesture-name
           :delete-gesture-name
           :device-event
           :event
           :event-matches-gesture-name-p
           :event-modifier-state
           :event-sheet
           :event-timestamp
           :eventp
           :key-press-event
           :key-release-event
           :keyboard-event
           :keyboard-event-character
           :keyboard-event-key-name
           :make-modifier-state
           :modifier-state-matches-gesture-name-p
           :pointer-button-event
           :pointer-button-press-event
           :pointer-button-release-event
           :pointer-enter-event
           :pointer-event
           :pointer-event-button
           :pointer-event-pointer
           :pointer-event-x
           :pointer-event-y
           :pointer-exit-event
           :pointer-motion-event))
(in-package :FitzCLIM/event)

(defparameter +shift-key+ 1
  "The shift bit in an event's modifier state.")

(defparameter +control-key+ 2
  "The control bit in an event's modifier state.")

(defparameter +meta-key+ 4
  "The meta bit in an event's modifier state.")

(defparameter +super-key+ 8
  "The super bit in an event's modifier state.")

(defparameter +hyper-key+ 16
  "The hyper bit in an event's modifier state.")

(defun make-modifier-state (&rest modifiers)
  "Return the modifier bits named by MODIFIERS.
The names are :SHIFT, :CONTROL, :META, :SUPER, and :HYPER."
  (let ((bits 0))
    (dolist (modifier modifiers bits)
      (incf bits (ecase modifier
                   (:shift +shift-key+)
                   (:control +control-key+)
                   (:meta +meta-key+)
                   (:super +super-key+)
                   (:hyper +hyper-key+))))))

(behavior 'make-modifier-state
  (should= 0 (make-modifier-state))
  (should= +shift-key+ (make-modifier-state :shift))
  (should= (+ +control-key+ +meta-key+)
           (make-modifier-state :control :meta)))

(defclass event ()
  ((timestamp
    :initarg :timestamp
    :initform 0
    :reader event-timestamp
    :documentation "A number that orders this event among others.")
   (modifier-state
    :initarg :modifier-state
    :initform 0
    :reader event-modifier-state
    :documentation "The modifier bits held down for this event."))
  (:documentation "Something that happened to a sheet."))

(defun eventp (object)
  "Return true if OBJECT is an event."
  (typep object 'event))

(defclass device-event (event)
  ((sheet
    :initarg :sheet
    :initform nil
    :reader event-sheet
    :documentation "The sheet the device was aimed at, or NIL."))
  (:documentation "An event from a pointer or a keyboard."))

(defclass pointer-event (device-event)
  ((x
    :initarg :x
    :initform 0
    :reader pointer-event-x
    :documentation "The pointer x in the sheet.")
   (y
    :initarg :y
    :initform 0
    :reader pointer-event-y
    :documentation "The pointer y in the sheet.")
   (pointer
    :initarg :pointer
    :initform nil
    :reader pointer-event-pointer
    :documentation "The pointer that produced the event, or NIL."))
  (:documentation "A pointer moved or its button changed."))

(defclass pointer-button-event (pointer-event)
  ((button
    :initarg :button
    :initform nil
    :reader pointer-event-button
    :documentation "The button.  :LEFT, :MIDDLE, or :RIGHT."))
  (:documentation "A pointer button went down or up."))

(defclass pointer-button-press-event (pointer-button-event)
  ()
  (:documentation "A pointer button went down."))

(defclass pointer-button-release-event (pointer-button-event)
  ()
  (:documentation "A pointer button went up."))

(defclass pointer-motion-event (pointer-event)
  ()
  (:documentation "The pointer moved."))

(defclass pointer-enter-event (pointer-motion-event)
  ()
  (:documentation "The pointer entered a sheet."))

(defclass pointer-exit-event (pointer-motion-event)
  ()
  (:documentation "The pointer left a sheet."))

(defclass keyboard-event (device-event)
  ((key-name
    :initarg :key-name
    :initform nil
    :reader keyboard-event-key-name
    :documentation "The key name, such as :F1, or NIL.")
   (character
    :initarg :character
    :initform nil
    :reader keyboard-event-character
    :documentation "The character the key produced, or NIL."))
  (:documentation "A key went down or up."))

(defclass key-press-event (keyboard-event)
  ()
  (:documentation "A key went down."))

(defclass key-release-event (keyboard-event)
  ()
  (:documentation "A key went up."))

(behavior 'event
  (let ((press (make-instance 'pointer-button-press-event
                              :sheet :sheet
                              :x 3
                              :y 4
                              :button :left
                              :modifier-state +shift-key+
                              :timestamp 9)))
    (should-be-true (eventp press))
    (should-eq :sheet (event-sheet press))
    (should= 3 (pointer-event-x press))
    (should= 4 (pointer-event-y press))
    (should-eq :left (pointer-event-button press))
    (should= +shift-key+ (event-modifier-state press))
    (should= 9 (event-timestamp press)))
  (let ((key (make-instance 'key-press-event :character #\a :key-name :a)))
    (should-eq #\a (keyboard-event-character key))
    (should-eq :a (keyboard-event-key-name key))))

(defclass gesture ()
  ((name
    :initarg :name
    :reader gesture-name)
   (event-type
    :initarg :event-type
    :reader gesture-event-type
    :documentation "The kind of event this gesture matches.")
   (button
    :initarg :button
    :initform nil
    :reader gesture-button)
   (character
    :initarg :character
    :initform nil
    :reader gesture-character)
   (modifier-state
    :initarg :modifier-state
    :initform 0
    :reader gesture-modifier-state))
  (:documentation "A named pointer or keyboard gesture."))

(defvar *gestures* nil
  "The gestures known to EVENT-MATCHES-GESTURE-NAME-P, newest first.")

(defun pointer-button? (name)
  "Return true if NAME is :LEFT, :MIDDLE, or :RIGHT."
  (member name '(:left :middle :right)))

(defun modifier-keywords-to-state (modifiers)
  "Return the modifier bits for the keyword list MODIFIERS."
  (apply #'make-modifier-state modifiers))

(defun parse-gesture-spec (event-type spec)
  "Return the button, character, and modifier state of SPEC.
EVENT-TYPE is :POINTER-BUTTON-PRESS, :POINTER-BUTTON-RELEASE, or
:KEYBOARD.  A pointer spec is (button &rest modifiers).  A keyboard
spec is (character &rest modifiers)."
  (cond
    ((eq event-type :keyboard)
     (values nil (first spec) (modifier-keywords-to-state (rest spec))))
    ((pointer-button? (first spec))
     (values (first spec) nil (modifier-keywords-to-state (rest spec))))
    (t (error "~S is not a gesture spec for ~S." spec event-type))))

(defun gestures-named (name)
  "Return the gestures named NAME, newest first."
  (remove name *gestures* :test-not #'eq :key #'gesture-name))

(defun add-gesture-name (name event-type gesture-spec &key (unique t))
  "Name GESTURE-SPEC as NAME for events of EVENT-TYPE.
EVENT-TYPE is :POINTER-BUTTON-PRESS, :POINTER-BUTTON-RELEASE, or
:KEYBOARD.  A pointer spec is (button &rest modifiers).  A keyboard
spec is (character &rest modifiers).  With UNIQUE true, NAME replaces
any gesture that already has it.  Return NAME."
  (check-type name symbol)
  (unless (member event-type '(:pointer-button-press
                               :pointer-button-release
                               :keyboard))
    (error "~S is not a gesture event type." event-type))
  (multiple-value-bind (button character modifiers)
      (parse-gesture-spec event-type gesture-spec)
    (when unique
      (setf *gestures* (remove name *gestures* :key #'gesture-name)))
    (push (make-instance 'gesture
                         :name name
                         :event-type event-type
                         :button button
                         :character character
                         :modifier-state modifiers)
          *gestures*))
  name)

(defun delete-gesture-name (name)
  "Remove every gesture named NAME.  Return NAME."
  (setf *gestures* (remove name *gestures* :key #'gesture-name))
  name)

(defun gesture-entry-matches-event-p (gesture event)
  "Return true if EVENT is the pointer or key action GESTURE names."
  (and (= (gesture-modifier-state gesture) (event-modifier-state event))
       (ecase (gesture-event-type gesture)
         (:pointer-button-press
          (and (typep event 'pointer-button-press-event)
               (eq (gesture-button gesture) (pointer-event-button event))))
         (:pointer-button-release
          (and (typep event 'pointer-button-release-event)
               (eq (gesture-button gesture) (pointer-event-button event))))
         (:keyboard
          (and (typep event 'key-press-event)
               (eql (gesture-character gesture)
                    (keyboard-event-character event)))))))

(defun event-matches-gesture-spec-p (event spec)
  "Return true if EVENT matches the gesture spec SPEC.
SPEC is a gesture name, a character, (button &rest modifiers),
(:KEYBOARD character &rest modifiers), or (gesture-name &rest modifiers)."
  (cond
    ((symbolp spec)
     (some (lambda (gesture)
             (gesture-entry-matches-event-p gesture event))
           (gestures-named spec)))
    ((characterp spec)
     (and (typep event 'key-press-event)
          (eql spec (keyboard-event-character event))
          (zerop (event-modifier-state event))))
    ((consp spec)
     (cond
       ((pointer-button? (first spec))
        (and (typep event 'pointer-button-press-event)
             (eq (first spec) (pointer-event-button event))
             (= (modifier-keywords-to-state (rest spec))
                (event-modifier-state event))))
       ((gestures-named (first spec))
        (some (lambda (gesture)
                (and (eq (gesture-event-type gesture) :pointer-button-press)
                     (typep event 'pointer-button-press-event)
                     (eq (gesture-button gesture) (pointer-event-button event))
                     (= (logior (gesture-modifier-state gesture)
                                (modifier-keywords-to-state (rest spec)))
                        (event-modifier-state event))))
              (gestures-named (first spec))))
       (t nil)))
    (t nil)))

(defun event-matches-gesture-name-p (event gesture)
  "Return true if EVENT is the action GESTURE describes.
GESTURE is a gesture name, a character, or a gesture spec list."
  (cond
    ((and (consp gesture) (eq (first gesture) :keyboard))
     (and (typep event 'key-press-event)
          (eql (second gesture) (keyboard-event-character event))
          (= (modifier-keywords-to-state (cddr gesture))
             (event-modifier-state event))))
    (t (event-matches-gesture-spec-p event gesture))))

(defun modifier-state-matches-gesture-name-p (modifier-state gesture-name)
  "Return true if GESTURE-NAME has an entry with MODIFIER-STATE."
  (some (lambda (gesture)
          (= modifier-state (gesture-modifier-state gesture)))
        (gestures-named gesture-name)))

(add-gesture-name :select :pointer-button-press '(:left))

(add-gesture-name :describe :pointer-button-press '(:middle))

(add-gesture-name :menu :pointer-button-press '(:right))

(add-gesture-name :edit :pointer-button-press '(:left :meta))

(add-gesture-name :delete :pointer-button-press '(:middle :shift))

(behavior 'event-matches-gesture-name-p
  (let ((left (make-instance 'pointer-button-press-event :button :left))
        (shifted (make-instance 'pointer-button-press-event
                                :button :left
                                :modifier-state +shift-key+))
        (right (make-instance 'pointer-button-press-event :button :right))
        (meta (make-instance 'pointer-button-press-event
                             :button :left
                             :modifier-state +meta-key+))
        (key (make-instance 'key-press-event
                            :character #\c
                            :modifier-state +control-key+)))
    (should-be-true (event-matches-gesture-name-p left :select))
    (should-be-false (event-matches-gesture-name-p shifted :select))
    (should-be-true (event-matches-gesture-name-p shifted '(:select :shift)))
    (should-be-false (event-matches-gesture-name-p right :select))
    (should-be-true (event-matches-gesture-name-p right :menu))
    (should-be-true (event-matches-gesture-name-p meta :edit))
    (should-be-true (modifier-state-matches-gesture-name-p 0 :select))
    (should-be-false (modifier-state-matches-gesture-name-p +shift-key+ :select))
    (should-be-true (event-matches-gesture-name-p key '(:keyboard #\c :control)))
    (should-be-true (event-matches-gesture-name-p
                     (make-instance 'key-press-event :character #\x)
                     #\x))
    (add-gesture-name 'shift-select :pointer-button-press '(:left :shift))
    (should-be-true (event-matches-gesture-name-p shifted 'shift-select))
    (delete-gesture-name 'shift-select)
    (should-be-false (event-matches-gesture-name-p shifted 'shift-select))))
