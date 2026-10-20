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

(defpackage :FitzCLIM/frame
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/geometry
        :FitzCLIM/gadget
        :FitzCLIM/sheet
        :FitzCLIM/layout)
  (:export :*application-frame*
           :application-frame
           :default-frame-top-level
           :define-application-frame
           :execute-frame-command
           :find-pane-named
           :frame-command-table
           :frame-current-layout
           :frame-exit
           :frame-height
           :frame-layouts
           :frame-name
           :frame-named-panes
           :frame-panes
           :frame-pending-commands
           :frame-pretty-name
           :frame-top-level-sheet
           :frame-width
           :framep
           :*frame-event-reader*
           :*frame-input-waiter*
           :layout-frame
           :make-application-frame
           :read-frame-command
           :run-frame-top-level))
(in-package :FitzCLIM/frame)

(defvar *application-frame* nil
  "The application frame whose command is running.")

(defclass application-frame ()
  ((name
    :initarg :name
    :reader frame-name)
   (pretty-name
    :initarg :pretty-name
    :accessor frame-pretty-name)
   (command-table
    :initarg :command-table
    :accessor frame-command-table)
   (pending-commands
    :initform nil
    :accessor frame-pending-commands
    :documentation "Commands waiting for READ-FRAME-COMMAND.  Each
    entry is a command name or a list of a name and arguments.  When
    this queue is empty, *FRAME-EVENT-READER* may supply the next
    command.")
   (named-panes
    :initform nil
    :accessor frame-named-panes
    :documentation "An alist of pane names and panes.")
   (layouts
    :initform nil
    :accessor frame-layouts
    :documentation "An alist of layout names and functions that build them.")
   (panes
    :initform nil
    :accessor frame-panes
    :documentation "The sheet of the current layout.")
   (top-level-sheet
    :initform nil
    :accessor frame-top-level-sheet
    :documentation "The sheet that adopts the current layout.")
   (current-layout
    :initform nil
    :reader frame-current-layout
    :documentation "The name of the layout currently adopted.")
   (width
    :initarg :width
    :initform 400
    :accessor frame-width
    :documentation "The width offered to the top-level sheet.")
   (height
    :initarg :height
    :initform 300
    :accessor frame-height
    :documentation "The height offered to the top-level sheet."))
  (:documentation
   "An application.  Its command table supplies the commands the
   command loop runs."))

(defun framep (object)
  "Return true if OBJECT is an application frame."
  (typep object 'application-frame))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-frame-command-table (frame-name options)
  "Return the command table name and its inherited table names.
Each option is a list beginning with a keyword."
  (let ((option (assoc :command-table options)))
    (if (null option)
        (values frame-name nil)
        (let ((spec (second option)))
          (if (symbolp spec)
              (values spec nil)
              (values (first spec) (getf (rest spec) :inherit-from)))))))

(defun pane-name (spec)
  "Return the symbol that names the pane SPEC."
  (let ((name (first spec)))
    (unless (and (symbolp name) (not (keywordp name)) (not (null name)))
      (error "A pane name must be a symbol, not ~S." name))
    name))

(defun pane-maker-form (spec)
  "Return the form that makes the pane SPEC."
  (let ((body (rest spec)))
    (unless body
      (error "Pane ~S has no type." (first spec)))
    (if (listp (first body))
        (first body)
        `(make-pane ,@body))))

(defun layout-name (spec)
  "Return the symbol that names the layout SPEC."
  (let ((name (first spec)))
    (unless (and (symbolp name) (not (keywordp name)) (not (null name)))
      (error "A layout name must be a symbol, not ~S." name))
    name))

(defun layout-form (spec)
  "Return the form that builds the layout SPEC."
  (unless (and (rest spec) (listp (second spec)) (null (rest (rest spec))))
    (error "A layout is a name and one form: ~S." spec))
  (second spec)))

(defmacro define-application-frame (name superclasses slots &rest options)
  "Define an application frame class and its command table.
SUPERCLASSES default to APPLICATION-FRAME, which is added when it is
missing.  :COMMAND-TABLE is a name, or (name :inherit-from (tables)).
:PRETTY-NAME is the string shown for the frame.  :PANES and :LAYOUTS
build the frame's sheets.  :GEOMETRY is a width and a height.
:DOCUMENTATION is the class docstring.  The command table becomes
*DEFAULT-COMMAND-TABLE* for later DEFINE-COMMAND forms."
  (dolist (option options)
    (unless (member (first option)
                    '(:command-table :pretty-name :panes :layouts :geometry
                      :documentation))
      (error "The frame option ~S is not implemented yet." (first option))))
  (multiple-value-bind (table-name inherit-from)
      (parse-frame-command-table name options)
    (let* ((supers (or superclasses '(application-frame)))
           (pretty-option (assoc :pretty-name options))
           (pretty (if pretty-option
                       (second pretty-option)
                       (string-capitalize (symbol-name name))))
           (documentation (second (assoc :documentation options)))
           (panes (assoc :panes options))
           (layouts (assoc :layouts options))
           (geometry (assoc :geometry options)))
      (when (and documentation (not (stringp documentation)))
        (error ":DOCUMENTATION must be a string."))
      (when (or panes layouts)
        (unless (and panes layouts)
          (error "A frame with panes needs both :PANES and :LAYOUTS."))
        (unless (rest panes)
          (error ":PANES names at least one pane."))
        (unless (rest layouts)
          (error ":LAYOUTS names at least one layout.")))
      (when geometry
        (unless (and (= (length geometry) 3)
                     (realp (second geometry))
                     (realp (third geometry)))
          (error ":GEOMETRY takes a width and a height.")))
      (unless (member 'application-frame supers)
        (setf supers (append supers '(application-frame))))
      (setf *default-command-table* table-name)
      (let ((initargs `(:name ',name
                        :pretty-name ,pretty
                        :command-table (find-command-table ',table-name)
                        ,@(when geometry
                            `(:width ,(second geometry)
                              :height ,(third geometry)))))
            (pane-method
              (when panes
                `(defmethod initialize-instance :after ((frame ,name) &key)
                   (let* ,(mapcar (lambda (spec)
                                    `(,(pane-name spec) ,(pane-maker-form spec)))
                                  (rest panes))
                     (setf (frame-named-panes frame)
                           (list ,@(mapcar (lambda (spec)
                                             `(cons ',(pane-name spec)
                                                    ,(pane-name spec)))
                                           (rest panes))))
                     (setf (frame-layouts frame)
                           (list ,@(mapcar (lambda (spec)
                                             `(cons ',(layout-name spec)
                                                    (lambda () ,(layout-form spec))))
                                           (rest layouts))))
                     (adopt-frame-layout frame
                                         ',(layout-name (first (rest layouts))))
                     (layout-frame frame))))))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (setf *default-command-table* ',table-name))
           (define-command-table ,table-name :inherit-from ,inherit-from)
           (defclass ,name ,supers
             ,slots
             (:default-initargs ,@initargs)
             ,@(when documentation
                 `((:documentation ,documentation))))
           ,@(when pane-method (list pane-method))
           ',name)))))

(defun find-pane-named (frame name &key (errorp t))
  "Return the pane of FRAME named NAME."
  (let ((pair (assoc name (frame-named-panes frame) :test #'eq)))
    (cond (pair (cdr pair))
          (errorp (error "The frame has no pane named ~S." name))
          (t nil))))

(defun adopt-frame-layout (frame name)
  "Adopt the layout named NAME.  Named panes are detached first so a
new layout can adopt the same sheets."
  (let ((builder (cdr (assoc name (frame-layouts frame) :test #'eq))))
    (unless builder
      (error "The frame has no layout named ~S." name))
    (let ((top (or (frame-top-level-sheet frame)
                   (setf (frame-top-level-sheet frame)
                         (make-instance 'single-child-sheet)))))
      (when (sheet-children top)
        (disown-sheet (first (sheet-children top))))
      (dolist (pair (frame-named-panes frame))
        (when (sheet-parent (cdr pair))
          (disown-sheet (cdr pair))))
      (adopt-sheet top (funcall builder))
      (setf (slot-value frame 'current-layout) name)
      (setf (frame-panes frame) (first (sheet-children top)))
      (layout-frame frame)
      name)))

(defmethod (setf frame-current-layout) (name (frame application-frame))
  (adopt-frame-layout frame name))

(defun layout-frame (frame &key width height)
  "Offer FRAME's top-level sheet WIDTH and HEIGHT and lay out its panes.
Missing sizes use the frame's current width and height."
  (let ((width (or width (frame-width frame)))
        (height (or height (frame-height frame)))
        (top (frame-top-level-sheet frame)))
    (unless top
      (error "The frame has no panes."))
    (setf (frame-width frame) width
          (frame-height frame) height)
    (allocate-space top width height)
    frame))

(defun make-application-frame (name &rest initargs &key &allow-other-keys)
  "Return a new frame of class NAME."
  (apply #'make-instance name initargs))

(defun frame-exit (&optional (frame *application-frame*))
  "Leave the command loop of FRAME."
  (unless frame
    (error "There is no application frame."))
  (throw 'frame-exit frame))

(defvar *frame-event-reader* nil
  "A function of one frame, or NIL.
READ-FRAME-COMMAND calls it when the frame has no pending command.
The function returns a command, or NIL when none is waiting.  A frame
manager sets it.")

(defvar *frame-input-waiter* nil
  "A function of one frame, or NIL.
The command loop calls it when READ-FRAME-COMMAND finds no command.
Return true to read again.  Return false to leave the loop.  NIL
means an empty queue ends the loop, which is the windowless case.")

(defgeneric read-frame-command (frame)
  (:documentation
   "Return the next command for FRAME, or NIL when none is waiting.
   A command is a name, or a list of a name and its arguments.
   A pending command is returned first.  *FRAME-EVENT-READER* is used
   only when that queue is empty.  NIL does not by itself mean the
   loop should exit."))

(defmethod read-frame-command ((frame application-frame))
  (or (pop (frame-pending-commands frame))
      (when *frame-event-reader*
        (funcall *frame-event-reader* frame))))

(defgeneric execute-frame-command (frame command)
  (:documentation
   "Run COMMAND in FRAME.  COMMAND is a name, or a list of a name and
   its arguments.  The command function's values are returned."))

(defmethod execute-frame-command ((frame application-frame) command)
  (let* ((*application-frame* frame)
         (command-name (if (listp command) (first command) command))
         (arguments (if (listp command) (rest command) nil))
         (entry (find-command command-name (frame-command-table frame))))
    (apply (command-function entry) arguments)))

(defun default-frame-top-level (frame)
  "Read and run commands for FRAME until the input ends or FRAME-EXIT
runs.  A shown frame waits for the next event.  An unshown frame
returns when no command is waiting.  Return FRAME."
  (let ((*application-frame* frame))
    (catch 'frame-exit
      (loop
        (let ((command (read-frame-command frame)))
          (cond
            (command
             (execute-frame-command frame command))
            ((and *frame-input-waiter*
                  (funcall *frame-input-waiter* frame)))
            (t
             (return frame))))))))

(defgeneric run-frame-top-level (frame)
  (:documentation "Run FRAME's command loop."))

(defmethod run-frame-top-level ((frame application-frame))
  (default-frame-top-level frame))

(define-application-frame adder-frame ()
  ((total
    :initform 0
    :accessor adder-total)))

(define-command (com-plus :command-table adder-frame :name t)
    ((amount integer))
  "Add AMOUNT to the frame's total and return the new total."
  (incf (adder-total *application-frame*) amount))

(define-command (com-quit-adder :command-table adder-frame :name t) ()
  "Leave the adder frame."
  (frame-exit))

(define-application-frame adder-child-frame (adder-frame) ()
  (:command-table (adder-child-frame :inherit-from (adder-frame))))

(define-application-frame documented-frame ()
  ()
  (:documentation "A frame with a class note."))

(behavior 'define-application-frame
  (let ((frame (make-application-frame 'adder-frame)))
    (should-be-true (framep frame))
    (should-eq 'adder-frame (frame-name frame))
    (should-equal "Adder-Frame" (frame-pretty-name frame))
    (should-eq 'adder-frame (command-table-name (frame-command-table frame)))
    (should= 5 (execute-frame-command frame '(com-plus 5)))
    (should= 5 (adder-total frame))
    (setf (frame-pending-commands frame)
          '((com-plus 2) (com-plus 3) (com-quit-adder)))
    (should-eq frame (run-frame-top-level frame))
    (should= 10 (adder-total frame))
    (should-be-null (read-frame-command frame))
    (should-be-true
     (handler-case
         (progn (execute-frame-command frame '(com-plus "no")) nil)
       (error () t))))
  (let ((child (make-application-frame 'adder-child-frame)))
    (should= 4 (execute-frame-command child '(com-plus 4)))
    (should= 4 (adder-total child)))
  (should-be-null (frame-panes (make-application-frame 'adder-frame)))
  (should-equal "A frame with a class note."
                (documentation 'documented-frame 'type))
  (should-be-true
   (handler-case
       (progn
         (macroexpand-1 '(define-application-frame bad-doc-frame () ()
                          (:documentation 5)))
         nil)
     (error () t)))
  (should-be-true
   (handler-case
       (progn
         (macroexpand-1 '(define-application-frame bad-option-frame () ()
                          (:menu nil)))
         nil)
     (error () t)))
  (should-be-true
   (handler-case
       (progn
         (macroexpand-1 '(define-application-frame bad-panes-frame () ()
                          (:panes (ok :push-button))))
         nil)
     (error () t))))

(define-application-frame stacked-frame ()
  ()
  (:panes
   (banner :label :label "Title" :width 80 :height 16)
   (ok :push-button :label "OK" :width 80 :height 20))
  (:layouts
   (default (vertically (:spacing 4) banner ok))
   (side-by-side (horizontally (:spacing 4) banner ok)))
  (:geometry 200 100))

(behavior 'frame-panes
  (let ((frame (make-application-frame 'stacked-frame)))
    (should-eq 'default (frame-current-layout frame))
    (should= 200 (frame-width frame))
    (should= 100 (frame-height frame))
    (let ((ok (find-pane-named frame 'ok)))
      (should-equal "OK" (gadget-label ok))
      (multiple-value-bind (x y) (transform-position (sheet-transformation ok) 0 0)
        (should= 0 x)
        (should= 20 y))
      (setf (frame-current-layout frame) 'side-by-side)
      (should-eq ok (find-pane-named frame 'ok))
      (multiple-value-bind (x y) (transform-position (sheet-transformation ok) 0 0)
        (should= 84 x)
        (should= 0 y)))
    (should-be-true
     (handler-case
         (progn (setf (frame-current-layout frame) 'missing) nil)
       (error () t)))
    (should-be-true
     (handler-case
         (progn (find-pane-named frame 'missing) nil)
       (error () t)))))
