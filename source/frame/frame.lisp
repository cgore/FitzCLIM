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
           :command-enabled
           :default-frame-top-level
           :define-application-frame
           :execute-frame-command
           :find-pane-named
           :frame-all-layouts
           :frame-background
           :frame-calling-frame
           :frame-command-table
           :frame-current-layout
           :frame-current-panes
           :frame-disabled-commands
           :frame-exit
           :frame-height
           :frame-layouts
           :frame-manager
           :frame-menu-bar
           :frame-menu-table
           :frame-name
           :frame-named-panes
           :frame-panes
           :frame-pending-commands
           :frame-pretty-name
           :frame-properties
           :frame-state
           :frame-standard-input
           :frame-standard-output
           :frame-query-io
           :frame-error-output
           :frame-top-level-sheet
           :frame-width
           :framep
           :*frame-event-reader*
           :*frame-input-waiter*
           :*frame-redisplayer*
           :*partial-command-reader*
           :*command-line-reader*
           :layout-frame
           :make-application-frame
           :read-frame-command
           :run-frame-top-level
           :with-application-frame))
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
    :documentation "The height offered to the top-level sheet.")
   (background
    :initarg :background
    :initform nil
    :accessor frame-background
    :documentation "The color behind the frame's panes.  NIL uses the
    system color.")
   (disabled-commands
    :initarg :disabled-commands
    :initform nil
    :accessor frame-disabled-commands
    :documentation "Command names this frame will not run.")
   (menu-bar
    :initarg :menu-bar
    :initform t
    :accessor frame-menu-bar
    :documentation "The command table whose menu is the menu bar.
    T uses this frame's command table.  NIL means there is no menu
    bar.  A symbol names another command table.")
   (top-level
    :initarg :top-level
    :initform '(default-frame-top-level)
    :accessor frame-top-level
    :documentation "A list of the top-level function and its extra
    arguments.  The function receives the frame first.")
   (calling-frame
    :initarg :calling-frame
    :initform nil
    :accessor frame-calling-frame
    :documentation "The frame that invoked this one, or NIL.")
   (state
    :initarg :state
    :initform :disowned
    :accessor frame-state
    :documentation "One of :DISOWNED, :ENABLED, :DISABLED, or :SHRUNK.")
   (manager
    :initform nil
    :accessor frame-manager
    :documentation "The frame manager that has adopted this frame, or NIL.")
   (properties
    :initform nil
    :accessor %frame-properties
    :documentation "An alist of property indicators and values."))
  (:documentation
   "An application.  Its command table supplies the commands the
   command loop runs."))

(defun framep (object)
  "Return true if OBJECT is an application frame."
  (typep object 'application-frame))

(behavior 'frame-background
  (let ((frame (make-instance 'application-frame)))
    (should-be-null (frame-background frame))
    (setf (frame-background frame) :black)
    (should-eq :black (frame-background frame))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun frame-option (options keyword)
  "Return the option in OPTIONS that begins with KEYWORD, or NIL."
  (assoc keyword options))

(defun parse-frame-command-table (frame-name options)
  "Return the command table name and its inherited table names.
Each option is a list beginning with a keyword."
  (let ((option (frame-option options :command-table)))
    (if (null option)
        (values frame-name nil)
        (let ((spec (second option)))
          (if (symbolp spec)
              (values spec nil)
              (values (first spec) (getf (rest spec) :inherit-from)))))))

(defun command-definer-name (frame-name options)
  "Return the command-defining macro for this frame, or NIL.
T names DEFINE-FRAME-NAME-COMMAND.  NIL defines no macro.  Another
symbol is used as the macro name."
  (let ((option (frame-option options :command-definer)))
    (let ((value (if option (second option) t)))
      (cond
        ((null value) nil)
        ((eq value t)
         (intern (format nil "DEFINE-~A-COMMAND" frame-name)
                 (symbol-package frame-name)))
        ((symbolp value) value)
        (t (error ":COMMAND-DEFINER must be T, NIL, or a symbol."))))))

(defun frame-menu-bar-initform (options)
  "Return the form stored as the frame's menu bar.
An omitted option is T, the frame's own command table."
  (let ((option (frame-option options :menu-bar)))
    (if option
        (second option)
        t)))

(defun frame-top-level-initform (options)
  "Return the form stored as the frame's top-level function list."
  (let ((option (frame-option options :top-level)))
    (if option
        `',(rest option)
        ''(default-frame-top-level))))

(defun frame-disabled-initform (options)
  "Return the form stored as the frame's initially disabled commands."
  (let ((option (frame-option options :disabled-commands)))
    (if option
        (second option)
        nil)))

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
:COMMAND-DEFINER is T, NIL, or a symbol.  T defines
DEFINE-NAME-COMMAND, which adds a command to this frame's table.
:MENU-BAR is T, NIL, or a command table name.  :DISABLED-COMMANDS
names commands that start disabled.  :TOP-LEVEL is a function and
its extra arguments.  :PANE is one pane form.  :PANES and :LAYOUTS
build named sheets.  :GEOMETRY is a width and a height.
:DOCUMENTATION is the class docstring.  The command table becomes
*DEFAULT-COMMAND-TABLE* for later DEFINE-COMMAND forms."
  (dolist (option options)
    (unless (member (first option)
                    '(:command-table :command-definer :pretty-name
                      :menu-bar :disabled-commands :top-level
                      :pane :panes :layouts :geometry :documentation))
      (error "The frame option ~S is not implemented yet." (first option))))
  (multiple-value-bind (table-name inherit-from)
      (parse-frame-command-table name options)
    (let* ((supers (or superclasses '(application-frame)))
           (pretty-option (frame-option options :pretty-name))
           (pretty (if pretty-option
                       (second pretty-option)
                       (string-capitalize (symbol-name name))))
           (documentation (second (frame-option options :documentation)))
           (pane (frame-option options :pane))
           (panes (frame-option options :panes))
           (layouts (frame-option options :layouts))
           (geometry (frame-option options :geometry))
           (definer (command-definer-name name options)))
      (when (and documentation (not (stringp documentation)))
        (error ":DOCUMENTATION must be a string."))
      (when (and pane (or panes layouts))
        (error ":PANE cannot be combined with :PANES or :LAYOUTS."))
      (when (or panes layouts)
        (unless (and panes layouts)
          (error "A frame with panes needs both :PANES and :LAYOUTS."))
        (unless (rest panes)
          (error ":PANES names at least one pane."))
        (unless (rest layouts)
          (error ":LAYOUTS names at least one layout.")))
      (when pane
        (unless (and (rest pane) (null (rest (rest pane))))
          (error ":PANE takes one form.")))
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
                        :menu-bar ,(frame-menu-bar-initform options)
                        :disabled-commands ,(frame-disabled-initform options)
                        :top-level ,(frame-top-level-initform options)
                        ,@(when geometry
                            `(:width ,(second geometry)
                              :height ,(third geometry)))))
            (pane-method
              (cond
                (panes
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
                      (layout-frame frame))))
                (pane
                 `(defmethod initialize-instance :after ((frame ,name) &key)
                    (let ((pane ,(second pane)))
                      (setf (frame-named-panes frame) (list (cons nil pane)))
                      (setf (frame-layouts frame)
                            (list (cons 'default (lambda () pane))))
                      (adopt-frame-layout frame 'default)
                      (layout-frame frame)))))))
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
           ,@(when definer
               `((defmacro ,definer (name-and-options arguments &body body)
                   "Define a command in this frame's command table.
A symbol names the command.  A list may supply :NAME and :MENU.
:COMMAND-TABLE is not accepted here."
                   (when (and (consp name-and-options)
                              (getf (rest name-and-options) :command-table))
                     (error "A frame command definer already has a command table."))
                   (let ((spec (if (symbolp name-and-options)
                                   (list name-and-options :command-table ',table-name)
                                   (list* (first name-and-options)
                                          :command-table ',table-name
                                          (rest name-and-options)))))
                     `(define-command ,spec ,arguments ,@body)))))
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

(defmacro with-application-frame ((frame) &body body)
  "Bind FRAME to the application frame whose command is running."
  `(let ((,frame *application-frame*))
     ,@body))

(defun frame-all-layouts (frame)
  "Return the names of FRAME's layouts."
  (mapcar #'car (frame-layouts frame)))

(defun frame-current-panes (frame)
  "Return the named panes in FRAME's current layout.
A frame made with :PANE returns that one pane."
  (let ((root (frame-panes frame))
        (panes nil))
    (when root
      (dolist (pair (frame-named-panes frame))
        (when (or (eq (cdr pair) root)
                  (sheet-ancestor-p (cdr pair) root))
          (push (cdr pair) panes))))
    (nreverse panes)))

(defun pane-class-named? (pane class-name)
  "Return true if PANE's class, or one of its superclasses, is named CLASS-NAME.
The comparison is by symbol name, so the pane package and this package
may each have the name."
  (some (lambda (class)
          (let ((name (class-name class)))
            (and (symbolp name)
                 (string= (symbol-name name) (symbol-name class-name)))))
        (sb-mop:class-precedence-list (class-of pane))))

(defun pane-of-class (frame class-name)
  "Return the first current pane of FRAME whose class is named CLASS-NAME.
A subclass counts.  The name is compared so this module does not depend
on the pane module."
  (find-if (lambda (pane) (pane-class-named? pane class-name))
           (frame-current-panes frame)))

(defun frame-standard-output (frame)
  "Return the first visible application pane of FRAME, or NIL."
  (or (pane-of-class frame 'application-pane)
      (pane-of-class frame 'interactor-pane)))

(defun frame-standard-input (frame)
  "Return the first visible interactor of FRAME, or its standard output."
  (or (pane-of-class frame 'interactor-pane)
      (frame-standard-output frame)))

(defun frame-query-io (frame)
  "Return the stream FRAME uses for queries."
  (or (frame-standard-input frame) (frame-standard-output frame)))

(defun frame-error-output (frame)
  "Return the stream FRAME uses for errors."
  (frame-standard-output frame))

(defun frame-menu-table (frame)
  "Return the command table FRAME shows as its menu bar, or NIL."
  (let ((bar (frame-menu-bar frame)))
    (cond
      ((null bar) nil)
      ((eq bar t) (frame-command-table frame))
      ((command-table-p bar) bar)
      ((symbolp bar) (find-command-table bar))
      (t (error "~S is not a menu bar." bar)))))

(defgeneric command-enabled (command-name frame)
  (:documentation
   "Return true when COMMAND-NAME can run in FRAME.
A command that is not in the frame's command table is not enabled."))

(defmethod command-enabled (command-name (frame application-frame))
  (and (frame-command-table frame)
       (command-present-in-command-table-p command-name
                                           (frame-command-table frame))
       (not (member command-name (frame-disabled-commands frame)))))

(defmethod (setf command-enabled) (enabled command-name (frame application-frame))
  "Enable or disable COMMAND-NAME in FRAME.
A command outside the frame's command table is left alone."
  (when (and (frame-command-table frame)
             (command-present-in-command-table-p command-name
                                                 (frame-command-table frame)))
    (if enabled
        (setf (frame-disabled-commands frame)
              (remove command-name (frame-disabled-commands frame)))
        (pushnew command-name (frame-disabled-commands frame))))
  enabled)

(defun frame-properties (frame property)
  "Return the value of PROPERTY on FRAME, or NIL."
  (cdr (assoc property (%frame-properties frame))))

(defun (setf frame-properties) (value frame property)
  "Set PROPERTY of FRAME to VALUE."
  (let ((cell (assoc property (%frame-properties frame))))
    (if cell
        (setf (cdr cell) value)
        (push (cons property value) (%frame-properties frame))))
  value)

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

(defvar *frame-redisplayer* nil
  "A function of one frame, or NIL.
The command loop calls it after a command runs, so panes with a
display function can be drawn again.  The redisplay module sets it.")

(defvar *partial-command-reader* nil
  "A function of a frame and a partial command, or NIL.
EXECUTE-FRAME-COMMAND calls it to fill arguments that a gesture left
unsupplied.  The presentation module sets it.  NIL runs the command
as it stands.")

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
         (table (frame-command-table frame)))
    (unless (command-enabled command-name frame)
      (error "The command ~S is not enabled in this frame." command-name))
    (when (and (partial-command-p command) *partial-command-reader*)
      (setf command (funcall *partial-command-reader* frame command)
            command-name (first command)
            arguments (rest command)))
    (apply (command-function (find-command command-name table))
           arguments)))

(defvar *command-line-reader* nil
  "A function of a frame and a stream, or NIL.
The command loop calls it when the frame's interactor has a typed line
and no gesture produced a command.  It returns a command, or NIL at
the end of the input.  The presentation module sets it.")

(defun prompt-for-command (frame stream prompt)
  "Write PROMPT on STREAM.
PROMPT is a string, or a function of the stream and the frame.  NIL
writes Command: ."
  (etypecase prompt
    (null (write-string "Command: " stream))
    (string (write-string prompt stream))
    (function (funcall prompt stream frame)))
  (terpri stream))

(defun command-from-typed-line (frame prompt)
  "Return the next typed command from FRAME's interactor, or NIL.
NIL means the interactor has no input stream, or the line has ended."
  (let ((stream (frame-standard-input frame)))
    (when (and stream
               (pane-class-named? stream 'interactor-pane)
               (funcall (find-symbol "PANE-INPUT" :fitzclim/pane) stream)
               *command-line-reader*)
      (prompt-for-command frame stream prompt)
      (funcall *command-line-reader*
               frame
               (funcall (find-symbol "PANE-INPUT" :fitzclim/pane) stream)))))

(defun default-frame-top-level (frame &key (prompt "Command: "))
  "Read and run commands for FRAME until the input ends or FRAME-EXIT
runs.  A pending command or a pointer gesture runs first.  Otherwise
a typed line on the interactor is read.  A shown frame with neither
waits for the next event.  An unshown frame returns when no command
is waiting.  After a command, panes that have a display function are
redrawn.  PROMPT is a string or a function of a stream and the frame.
Return FRAME."
  (let ((*application-frame* frame))
    (catch 'frame-exit
      (loop
        (let ((command (or (read-frame-command frame)
                           (command-from-typed-line frame prompt))))
          (cond
            (command
             (execute-frame-command frame command)
             (when *frame-redisplayer*
               (funcall *frame-redisplayer* frame)))
            ((and *frame-input-waiter*
                  (funcall *frame-input-waiter* frame)))
            (t
             (return frame))))))))

(defgeneric run-frame-top-level (frame &key &allow-other-keys)
  (:documentation
   "Run FRAME's command loop.  The function comes from the frame's
:TOP-LEVEL option, and defaults to DEFAULT-FRAME-TOP-LEVEL.  Keyword
arguments are passed to that function."))

(defmethod run-frame-top-level ((frame application-frame) &rest keys &key &allow-other-keys)
  (let ((top-level (frame-top-level frame)))
    (apply (first top-level) frame (append (rest top-level) keys))))

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
                          (:icon nil)))
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
       (error () t)))
    (should-equal '(default side-by-side) (frame-all-layouts frame))
    (should= 2 (length (frame-current-panes frame)))))



(define-command-table other-menu)

(define-application-frame option-frame ()
  ((draws
    :initform 0
    :accessor option-draws))
  (:command-definer define-option-command)
  (:menu-bar 'other-menu)
  (:disabled-commands '(com-option-hold))
  (:top-level counted-top-level)
  (:pane (make-pane :label :label "One" :width 40 :height 12)))

(defun counted-top-level (frame &key prompt)
  "Run the default loop and remember that this frame used it."
  (setf (frame-properties frame 'prompt) prompt)
  (default-frame-top-level frame))

(define-option-command (com-option-draw :name t :menu "Draw") ()
  "Count one redraw of the option frame."
  (incf (option-draws *application-frame*)))

(define-option-command (com-option-hold :menu "Hold") ()
  "A command that starts disabled."
  nil)

(define-application-frame bare-frame ()
  ()
  (:command-definer nil)
  (:menu-bar nil))

(behavior 'frame-options
  (let ((frame (make-application-frame 'option-frame)))
    (should-eq (find-command-table 'other-menu) (frame-menu-table frame))
    (should-eq :disowned (frame-state frame))
    (should-be-null (frame-manager frame))
    (should-be-null (frame-calling-frame frame))
    (should-be-true (command-enabled 'com-option-draw frame))
    (should-be-false (command-enabled 'com-option-hold frame))
    (should-be-false (command-enabled 'com-missing frame))
    (setf (command-enabled 'com-option-hold frame) t)
    (should-be-true (command-enabled 'com-option-hold frame))
    (setf (command-enabled 'com-option-hold frame) nil)
    (should-be-false (command-enabled 'com-option-hold frame))
    (setf (command-enabled 'com-missing frame) t)
    (should-be-false (command-enabled 'com-missing frame))
    (let ((pane (frame-panes frame)))
      (should-equal "One" (gadget-label pane))
      (should-equal (list pane) (frame-current-panes frame)))
    (setf (frame-pending-commands frame) '(com-option-draw))
    (should-eq frame (run-frame-top-level frame))
    (should= 1 (option-draws frame))
    (should-equal nil (frame-properties frame 'prompt))
    (should-be-true
     (handler-case
         (progn (execute-frame-command frame 'com-option-hold) nil)
       (error () t))))
  (let ((bare (make-application-frame 'bare-frame)))
    (should-be-null (frame-menu-table bare))
    (should-be-null (frame-panes bare))
    (should-be-null (frame-current-panes bare)))
  (should-be-true
   (handler-case
       (progn
         (macroexpand-1 '(define-application-frame mixed-pane-frame () ()
                          (:pane (make-pane :label))
                          (:panes (ok :push-button))
                          (:layouts (default ok))))
         nil)
     (error () t))))
