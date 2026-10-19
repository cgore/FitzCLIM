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
        :FitzCLIM/command)
  (:export :*application-frame*
           :application-frame
           :default-frame-top-level
           :define-application-frame
           :execute-frame-command
           :frame-command-table
           :frame-exit
           :frame-name
           :frame-pending-commands
           :frame-pretty-name
           :framep
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
    entry is a command name or a list of a name and arguments.  A frame
    manager replaces this queue with input from the user."))
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
              (values (first spec) (getf (rest spec) :inherit-from))))))))

(defmacro define-application-frame (name superclasses slots &rest options)
  "Define an application frame class and its command table.
SUPERCLASSES default to APPLICATION-FRAME, which is added when it is
missing.  :COMMAND-TABLE is a name, or (name :inherit-from (tables)).
:PRETTY-NAME is the string shown for the frame.  The command table
becomes *DEFAULT-COMMAND-TABLE* for later DEFINE-COMMAND forms."
  (dolist (option options)
    (unless (member (first option) '(:command-table :pretty-name))
      (error "The frame option ~S is not implemented yet." (first option))))
  (multiple-value-bind (table-name inherit-from)
      (parse-frame-command-table name options)
    (let* ((supers (or superclasses '(application-frame)))
           (pretty-option (assoc :pretty-name options))
           (pretty (if pretty-option
                       (second pretty-option)
                       (string-capitalize (symbol-name name)))))
      (unless (member 'application-frame supers)
        (setf supers (append supers '(application-frame))))
      (setf *default-command-table* table-name)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf *default-command-table* ',table-name))
         (define-command-table ,table-name :inherit-from ,inherit-from)
         (defclass ,name ,supers
           ,slots
           (:default-initargs
            :name ',name
            :pretty-name ,pretty
            :command-table (find-command-table ',table-name)))
         ',name))))

(defun make-application-frame (name &rest initargs &key &allow-other-keys)
  "Return a new frame of class NAME."
  (apply #'make-instance name initargs))

(defun frame-exit (&optional (frame *application-frame*))
  "Leave the command loop of FRAME."
  (unless frame
    (error "There is no application frame."))
  (throw 'frame-exit frame))

(defgeneric read-frame-command (frame)
  (:documentation
   "Return the next command for FRAME, or NIL to leave the loop.
   A command is a name, or a list of a name and its arguments."))

(defmethod read-frame-command ((frame application-frame))
  (pop (frame-pending-commands frame)))

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
runs.  Return FRAME."
  (let ((*application-frame* frame))
    (catch 'frame-exit
      (loop
        (let ((command (read-frame-command frame)))
          (unless command
            (return frame))
          (execute-frame-command frame command))))))

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
    (should-be-true
     (handler-case
         (progn (execute-frame-command frame '(com-plus "no")) nil)
       (error () t))))
  (let ((child (make-application-frame 'adder-child-frame)))
    (should= 4 (execute-frame-command child '(com-plus 4)))
    (should= 4 (adder-total child))))
