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

(defpackage :FitzCLIM/presentation/translator
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/frame
        :FitzCLIM/event
        :FitzCLIM/output-record
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present)
  (:import-from :FitzCLIM/presentation/present
                :read-token)
  (:export :accept-frame-command
           :call-presentation-translator
           :command-from-presentation
           :define-presentation-action
           :define-drag-and-drop-translator
           :define-presentation-to-command-translator
           :define-presentation-translator
           :document-presentation-translator
           :pointer-documentation
           :presentation-translator-destination-type
           :execute-presentation-gesture
           :find-applicable-translators
           :presentation-translator
           :presentation-translator-documentation
           :presentation-translator-from-type
           :presentation-translator-gesture
           :presentation-translator-menu
           :presentation-translator-name
           :presentation-translator-priority
           :presentation-translator-to-type
           :throw-highlighted-presentation))
(in-package :FitzCLIM/presentation/translator)

(defclass presentation-translator ()
  ((name
    :initarg :name
    :reader presentation-translator-name)
   (from-type
    :initarg :from-type
    :reader presentation-translator-from-type)
   (to-type
    :initarg :to-type
    :reader presentation-translator-to-type)
   (command-table
    :initarg :command-table
    :initform nil
    :reader presentation-translator-command-table)
   (gesture
    :initarg :gesture
    :initform nil
    :reader presentation-translator-gesture)
   (tester
    :initarg :tester
    :initform nil
    :reader presentation-translator-tester)
   (documentation
    :initarg :documentation
    :initform nil
    :reader presentation-translator-documentation)
   (menu
    :initarg :menu
    :initform nil
    :reader presentation-translator-menu)
   (priority
    :initarg :priority
    :initform 0
    :reader presentation-translator-priority)
   (body
    :initarg :body
    :reader presentation-translator-body)
   (command-name
    :initarg :command-name
    :initform nil
    :reader presentation-translator-command-name)
   (match-context
    :initarg :match-context
    :initform nil
    :reader presentation-translator-match-context
    :documentation "True when the presentation type must match the input context.")
   (destination-type
    :initarg :destination-type
    :initform nil
    :reader presentation-translator-destination-type
    :documentation "The presentation type a drag must be dropped on, or NIL.
A translator with a destination type is a drag.  A click does not run it."))
  (:documentation
   "A way to turn a presentation into an object of another type.
   A command translator produces a command.  A drag translator also
   names the presentation type that can receive the drop."))

(defvar *presentation-translators* nil
  "Translators, newest first.")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun translator-lambda (arglist body)
  "Return a lambda form for a translator body or tester."
  (if (member '&key arglist)
      `(lambda ,arglist ,@body)
      `(lambda (,@arglist &key presentation context-type frame event window x y)
         (declare (ignorable ,@arglist presentation context-type frame event
                             window x y))
         ,@body)))

(defun tester-lambda (tester)
  "Return a tester form for TESTER, or NIL."
  (cond
    ((null tester) nil)
    ((symbolp tester) `#',tester)
    ((and (consp tester) (consp (first tester)))
     (translator-lambda (first tester) (rest tester)))
    (t `#',tester))))

(defmacro define-presentation-translator
    (name (from-type to-type command-table
           &key gesture tester documentation menu (priority 0)
             command match-context destination-type)
     arglist &body body)
  "Define a translator named NAME from FROM-TYPE to TO-TYPE.
COMMAND-TABLE is a command table name, or NIL for every table.
GESTURE is a gesture name or spec.  TESTER is NIL, a function, or
((arguments) body).  COMMAND, when supplied, is a command name: BODY
returns the command's argument list.  MATCH-CONTEXT true applies the
translator only when the presentation's type is a subtype of the input
context.  DESTINATION-TYPE makes NAME a drag translator: a click does
not run it, and a drop must land on that presentation type.
Redefining NAME replaces the old translator."
  `(progn
     ,@(when command-table
         `((find-command-table ',command-table)))
     (setf *presentation-translators*
           (cons (make-instance 'presentation-translator
                                :name ',name
                                :from-type ',from-type
                                :to-type ',to-type
                                :command-table ',command-table
                                :gesture ',gesture
                                :tester ,(tester-lambda tester)
                                :documentation ,documentation
                                :menu ',menu
                                :priority ,priority
                                :command-name ',command
                                :match-context ,match-context
                                :destination-type ',destination-type
                                :body ,(translator-lambda arglist body))
                 (remove ',name *presentation-translators*
                         :key #'presentation-translator-name)))
     ',name))

(defmacro define-presentation-to-command-translator
    (name (from-type command-name command-table
           &key gesture tester documentation menu (priority 0))
     arglist &body body)
  "Define a translator that produces the command COMMAND-NAME.
BODY returns the command's argument list."
  `(define-presentation-translator ,name
       (,from-type (command :command-table ,command-table) ,command-table
        :gesture ,gesture
        :tester ,tester
        :documentation ,documentation
        :menu ,menu
        :priority ,priority
        :command ,command-name)
     ,arglist
     ,@body))

(defmacro define-presentation-action
    (name (from-type to-type command-table
           &key gesture tester documentation menu (priority 0))
     arglist &body body)
  "Define a translator that runs BODY for its effect.
The value of the translator is NIL."
  `(define-presentation-translator ,name
       (,from-type ,to-type ,command-table
        :gesture ,gesture
        :tester ,tester
        :documentation ,documentation
        :menu ,menu
        :priority ,priority)
     ,arglist
     ,@body
     nil))

(defmacro define-drag-and-drop-translator
    (name (from-type destination-type command-name command-table
           &key (gesture :select) tester documentation (priority 0))
     arglist &body body)
  "Define a translator that drags FROM-TYPE onto DESTINATION-TYPE.
BODY receives the source object and the destination object and returns
the argument list of COMMAND-NAME.  GESTURE defaults to :SELECT and is
matched against the press that started the drag."
  `(define-presentation-translator ,name
       (,from-type (command :command-table ,command-table) ,command-table
        :gesture ,gesture
        :tester ,tester
        :documentation ,documentation
        :priority ,priority
        :command ,command-name
        :destination-type ,destination-type)
     ,arglist
     ,@body))

(defun command-table-ancestor? (table ancestor)
  "Return true if TABLE is ANCESTOR or inherits from ANCESTOR."
  (let ((table (if (command-table-p table) table (find-command-table table)))
        (ancestor (if (command-table-p ancestor)
                      ancestor
                      (find-command-table ancestor)))
        (seen (make-hash-table :test #'eq)))
    (labels ((walk (candidate)
               (unless (gethash candidate seen)
                 (setf (gethash candidate seen) t)
                 (or (eq candidate ancestor)
                     (some #'walk (command-table-inherit-from candidate))))))
      (and (walk table) t))))

(defun translator-available? (translator command-table)
  "Return true if TRANSLATOR is visible from COMMAND-TABLE.
A NIL command table on the translator is visible everywhere.  A NIL
COMMAND-TABLE argument does not filter."
  (let ((owner (presentation-translator-command-table translator)))
    (or (null owner)
        (null command-table)
        (command-table-ancestor? command-table owner))))

(defun translator-gesture-matches? (translator event)
  "Return true if EVENT matches TRANSLATOR's gesture.
A missing event matches every gesture.  A translator with no gesture
matches only a missing event."
  (let ((gesture (presentation-translator-gesture translator)))
    (cond
      ((null event) t)
      ((null gesture) nil)
      (t (event-matches-gesture-name-p event gesture)))))

(defun translator-type-matches? (translator presentation context-type)
  "Return true if TRANSLATOR can turn PRESENTATION into CONTEXT-TYPE."
  (and (presentation-subtypep (presentation-type presentation)
                              (presentation-translator-from-type translator))
       (cond
         ((presentation-translator-match-context translator)
          (presentation-subtypep (presentation-type presentation) context-type))
         ((null (presentation-translator-to-type translator))
          (or (null context-type) (eq context-type t)))
         (t
          (presentation-subtypep (presentation-translator-to-type translator)
                                 context-type)))))

(defun translator-tester-passes? (translator presentation context-type
                                   frame event window x y)
  "Return true if TRANSLATOR's tester accepts PRESENTATION."
  (let ((tester (presentation-translator-tester translator)))
    (or (null tester)
        (funcall tester
                 (presentation-object presentation)
                 :presentation presentation
                 :context-type context-type
                 :frame frame
                 :event event
                 :window window
                 :x x
                 :y y))))

(defun find-applicable-translators (presentation context-type command-table
                                     &key event frame window x y)
  "Return the translators that apply to PRESENTATION, highest priority first."
  (let ((matches nil))
    (dolist (translator *presentation-translators*)
      (when (and (null (presentation-translator-destination-type translator))
                 (translator-available? translator command-table)
                 (translator-gesture-matches? translator event)
                 (translator-type-matches? translator presentation context-type)
                 (translator-tester-passes? translator presentation context-type
                                            frame event window x y))
        (push translator matches)))
    (stable-sort matches #'> :key #'presentation-translator-priority)))

(defun call-presentation-translator (translator presentation context-type
                                      &key frame event window x y destination)
  "Run TRANSLATOR.  Return the object and its presentation type.
A command translator returns a list of the command name and the
arguments BODY produced.  DESTINATION, when supplied, is the
presentation dropped on.  BODY then receives that object's value after
the source object."
  (multiple-value-bind (object new-type)
      (if destination
          (funcall (presentation-translator-body translator)
                   (presentation-object presentation)
                   (presentation-object destination)
                   :presentation presentation
                   :context-type context-type
                   :frame frame
                   :event event
                   :window window
                   :x x
                   :y y)
          (funcall (presentation-translator-body translator)
                   (presentation-object presentation)
                   :presentation presentation
                   :context-type context-type
                   :frame frame
                   :event event
                   :window window
                   :x x
                   :y y))
    (let ((result-type
            (or new-type
                (if (presentation-translator-match-context translator)
                    (presentation-type presentation)
                    (presentation-translator-to-type translator)))))
      (if (presentation-translator-command-name translator)
          (values (cons (presentation-translator-command-name translator) object)
                  result-type)
          (values object result-type)))))

(defun find-drag-translators (presentation command-table event
                              &key destination frame window x y)
  "Return drag translators for PRESENTATION, highest priority first.
A drag translator names a destination type.  DESTINATION nil asks
whether a drag can start.  A presentation asks whether that
presentation can receive the drop.  EVENT is the press that started
the drag, because a release does not match a press gesture."
  (let ((matches nil))
    (dolist (translator *presentation-translators*)
      (when (and (presentation-translator-destination-type translator)
                 (translator-available? translator command-table)
                 (translator-gesture-matches? translator event)
                 (presentation-subtypep
                  (presentation-type presentation)
                  (presentation-translator-from-type translator))
                 (or (null destination)
                     (presentation-subtypep
                      (presentation-type destination)
                      (presentation-translator-destination-type translator)))
                 (translator-tester-passes? translator presentation
                                            'command frame event window x y))
        (push translator matches)))
    (stable-sort matches #'> :key #'presentation-translator-priority)))

(defun command-from-drag (source destination event command-table
                          &key frame window x y)
  "Return the command produced by dropping SOURCE on DESTINATION, or NIL.
EVENT is the press that started the drag."
  (let ((translators (find-drag-translators source command-table event
                                            :destination destination
                                            :frame frame
                                            :window window
                                            :x x
                                            :y y)))
    (when translators
      (call-presentation-translator (first translators) source 'command
                                    :destination destination
                                    :event event
                                    :frame frame
                                    :window window
                                    :x x
                                    :y y))))

(defun document-presentation-translator (translator presentation context-type
                                          &key frame event window x y
                                            (stream *standard-output*))
  "Write TRANSLATOR's documentation to STREAM."
  (let ((documentation (presentation-translator-documentation translator)))
    (cond
      ((null documentation) nil)
      ((stringp documentation) (write-string documentation stream))
      ((functionp documentation)
       (funcall documentation presentation context-type frame event window x y
                stream))))
  nil)

(defun pointer-documentation (&key presentation context-type frame event
                                window x y stream)
  "Return the documentation of the translator that applies here.
PRESENTATION is the object under the pointer.  CONTEXT-TYPE defaults
to COMMAND.  FRAME supplies the command table.  EVENT, WINDOW, X, and
Y are passed to the translator.  The string is written to STREAM when
one is given.  NIL means no translator applies.  Nothing is drawn."
  (let ((table (and frame (frame-command-table frame))))
    (when (and presentation table)
      (let* ((context (or context-type 'command))
             (translators (find-applicable-translators
                           presentation context table
                           :event event
                           :frame frame
                           :window window
                           :x x
                           :y y)))
        (when translators
          (let ((text (with-output-to-string (buffer)
                        (document-presentation-translator
                         (first translators) presentation context
                         :frame frame
                         :event event
                         :window window
                         :x x
                         :y y
                         :stream buffer))))
            (when (plusp (length text))
              (when stream
                (write-string text stream))
              text)))))))

(defun command-from-presentation (presentation event command-table
                                  &key frame window x y)
  "Return the command PRESENTATION and EVENT produce, or NIL.
FRAME, WINDOW, X, and Y are passed to the translator and its tester."
  (let* ((table (if (command-table-p command-table)
                    command-table
                    (find-command-table command-table)))
         (context 'command)
         (translators (find-applicable-translators presentation context table
                                                   :event event
                                                   :frame frame
                                                   :window window
                                                   :x x
                                                   :y y)))
    (when translators
      (call-presentation-translator (first translators) presentation context
                                    :event event
                                    :frame frame
                                    :window window
                                    :x x
                                    :y y))))

(defun throw-highlighted-presentation (presentation input-context event
                                        &key frame window x y)
  "Throw PRESENTATION into the input context, through a translator.
INPUT-CONTEXT is an entry of *INPUT-CONTEXT*.  NIL uses the innermost
context.  EVENT is the gesture, or NIL."
  (let ((context (or input-context (first *input-context*))))
    (unless context
      (error "There is no input context."))
    (destructuring-bind (context-type . tag) context
      (let ((translators
              (find-applicable-translators
               presentation context-type
               (when frame (frame-command-table frame))
               :event event
               :frame frame
               :window window
               :x x
               :y y)))
        (unless translators
          (error "No translator applies to ~S for ~S."
                 (presentation-type presentation) context-type))
        (multiple-value-bind (object type)
            (call-presentation-translator
             (first translators) presentation context-type
             :frame frame :event event :window window :x x :y y)
          (throw tag (values object type event nil)))))))

(defun execute-presentation-gesture (frame presentation event)
  "Run the command PRESENTATION and EVENT produce in FRAME.
Return NIL when no translator applies."
  (let ((command (command-from-presentation presentation event
                                            (frame-command-table frame))))
    (when command
      (execute-frame-command frame command))))

(defun accept-frame-command (frame string)
  "Parse STRING as a command in FRAME's command table."
  (accept-from-string
   `(command :command-table ,(command-table-name (frame-command-table frame)))
   string))

(define-presentation-translator identity
    (t t nil
     :gesture :select
     :priority -1
     :match-context t
     :documentation "Use this object")
    (object)
  object)

(define-presentation-type starship
  :inherit-from t
  :description "a starship")

(define-presentation-method presentation-typep (object (type starship))
  (stringp object))

(define-presentation-method present (object (type starship) stream view &key)
  (write-string object stream))

(define-presentation-method accept ((type starship) stream view &key)
  (values (read-token stream) type))

(define-application-frame presentation-frame ()
  ((selected
    :initform nil
    :accessor selected-starship))
  (:command-table (presentation-frame)))

(define-command (com-select-starship :command-table presentation-frame :name t)
    ((ship starship))
  "Remember SHIP on the current frame and return it."
  (setf (selected-starship *application-frame*) ship))

(define-presentation-to-command-translator select-starship
    (starship com-select-starship presentation-frame
     :gesture :select
     :documentation "Select a starship"
     :menu t
     :priority 1)
    (object)
  (list object))

(define-presentation-to-command-translator reject-empty-starship
    (starship com-select-starship presentation-frame
     :gesture :select
     :priority 5
     :tester ((object) (not (stringp object))))
    (object)
  (list "rejected"))

(define-command-table presentation-child :inherit-from (presentation-frame))

(behavior 'presentation-translator
  (let ((presentation (make-presentation 3 'integer))
        (press (make-instance 'pointer-button-press-event :button :left :x 8 :y 9)))
    (should= 3
             (with-input-context ('integer) (object)
                 (throw-highlighted-presentation presentation nil press)
               (integer object)))
    (should-be-true
     (handler-case
         (progn
           (with-input-context ('integer) ()
               (throw-highlighted-presentation
                presentation nil
                (make-instance 'pointer-button-press-event :button :right)))
           nil)
       (error () t))))
  (let* ((frame (make-application-frame 'presentation-frame))
         (presentation (make-presentation "Nostromo" 'starship))
         (press (make-instance 'pointer-button-press-event :button :left))
         (menu (make-instance 'pointer-button-press-event :button :right)))
    (should-equal "Nostromo"
                  (execute-presentation-gesture frame presentation press))
    (should-equal "Nostromo" (selected-starship frame))
    (should-be-null (command-from-presentation presentation menu
                                               (frame-command-table frame)))
    (let ((command (command-from-presentation presentation press
                                              'presentation-child)))
      (should-equal "Nostromo" (second command)))
    (let ((translator (first (find-applicable-translators
                              presentation 'command 'presentation-frame
                              :event press))))
      (should-eq 'select-starship (presentation-translator-name translator))
      (should-eq t (presentation-translator-menu translator))
      (should-equal "Select a starship"
                    (with-output-to-string (stream)
                      (document-presentation-translator
                       translator presentation 'command :stream stream)))
      (should-equal "Select a starship"
                    (pointer-documentation :presentation presentation
                                           :frame frame
                                           :event press))
      (should-equal "Select a starship"
                    (with-output-to-string (stream)
                      (pointer-documentation :presentation presentation
                                             :frame frame
                                             :event press
                                             :stream stream)))
      (should-be-null (pointer-documentation :presentation presentation
                                             :context-type 'integer
                                             :frame frame
                                             :event press))
      (should-be-null (pointer-documentation
                       :presentation (make-presentation 3 'integer)
                       :frame frame
                       :event press)))
    (setf (frame-pending-commands frame)
          (list (accept-frame-command frame "Select Starship Sulaco")))
    (should-eq frame (run-frame-top-level frame))
    (should-equal "Sulaco" (selected-starship frame))))

(setf *default-command-table* nil)
