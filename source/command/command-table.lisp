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

(defpackage :FitzCLIM/command
  (:use :common-lisp
        :sigma/behave)
  (:export :*default-command-table*
           :add-command-to-command-table
           :command
           :command-arguments
           :command-function
           :command-line-name
           :command-line-name-from-symbol
           :command-menu
           :command-name
           :command-present-in-command-table-p
           :command-table
           :command-table-inherit-from
           :command-table-menu
           :command-table-name
           :command-table-p
           :define-command
           :define-command-table
           :find-command
           :find-command-table
           :make-command-table
           :map-over-command-table-commands
           :remove-command-from-command-table
           :standard-command
           :standard-command-table))
(in-package :FitzCLIM/command)

(defclass command-table ()
  ()
  (:documentation
   "A named set of commands.  A table may inherit commands from other
   tables."))

(defclass standard-command-table (command-table)
  ((name
    :initarg :name
    :reader command-table-name)
   (inherit-from
    :initarg :inherit-from
    :initform nil
    :reader command-table-inherit-from
    :documentation "Command tables searched after this one.")
   (commands
    :initform nil
    :accessor %commands
    :documentation "An alist of command names and commands, in the
    order they were added."))
  (:documentation "A command table stored as an alist."))

(defun command-table-p (object)
  "Return true if OBJECT is a command table."
  (typep object 'command-table))

(defvar *command-tables* (make-hash-table :test #'eq)
  "Command tables keyed by name.")

(defvar *default-command-table* nil
  "The command table used when DEFINE-COMMAND does not name one.
DEFINE-APPLICATION-FRAME sets this to the frame's command table.")

(defun find-command-table (name &key (errorp t))
  "Return the command table named NAME.  With ERRORP false, an unknown
name returns NIL."
  (or (gethash name *command-tables*)
      (when errorp
        (error "There is no command table named ~S." name))))

(defun make-command-table (name &key inherit-from (errorp t))
  "Return a command table named NAME that inherits from INHERIT-FROM.
INHERIT-FROM is a list of command table names.  With ERRORP true, an
existing name signals an error.  With ERRORP false, the new table
replaces the old one."
  (check-type name symbol)
  (when (and errorp (gethash name *command-tables*))
    (error "The command table ~S already exists." name))
  (let ((table (make-instance 'standard-command-table
                              :name name
                              :inherit-from (mapcar #'find-command-table
                                                    inherit-from))))
    (setf (gethash name *command-tables*) table)))

(defmacro define-command-table (name &key inherit-from)
  "Define a command table named NAME.  INHERIT-FROM is a list of
command table names.  Redefining a table replaces it."
  `(make-command-table ',name :inherit-from ',inherit-from :errorp nil))

(defclass command ()
  ()
  (:documentation "A command that can be read and run."))

(defclass standard-command (command)
  ((name
    :initarg :name
    :reader command-name)
   (line-name
    :initarg :line-name
    :reader command-line-name
    :documentation "The text typed to invoke the command, or NIL.")
   (menu
    :initarg :menu
    :reader command-menu
    :documentation "The menu string, or NIL when the command is not
    on a menu.")
   (arguments
    :initarg :arguments
    :reader command-arguments
    :documentation "A list of (name type) pairs.")
   (function
    :initarg :function
    :reader command-function))
  (:documentation "A command stored with its function and its names."))

(defun command-line-name-from-symbol (symbol)
  "Return a command-line name for SYMBOL.
A leading COM- is dropped, and hyphens become spaces."
  (let* ((raw (symbol-name symbol))
         (stripped (if (and (> (length raw) 4)
                            (string= raw "COM-" :end1 4))
                       (subseq raw 4)
                       raw)))
    (format nil "~:(~A~)" (substitute #\Space #\- stripped))))

(defun line-name-for (name command-name)
  "Return the stored command-line name for NAME."
  (cond
    ((eq name t) (command-line-name-from-symbol command-name))
    ((null name) nil)
    ((stringp name) name)
    (t (error "A command-line name must be T, NIL, or a string."))))

(defun menu-for (menu line-name)
  "Return the stored menu string for MENU."
  (cond
    ((eq menu t) line-name)
    ((null menu) nil)
    ((stringp menu) menu)
    (t (error "A command menu must be T, NIL, or a string."))))

(defun command-cell (table name)
  "Return the alist cell for NAME in TABLE, or NIL."
  (assoc name (%commands table) :test #'eq))

(defun add-command-to-command-table (command-name command-table
                                     &key (name t) menu arguments (errorp t))
  "Add the function COMMAND-NAME to COMMAND-TABLE.
NAME is T, NIL, or the command-line string.  MENU is T, NIL, or the
menu string.  ARGUMENTS is a list of (name type) pairs.  With ERRORP
false, a command already in the table is replaced."
  (let ((table (if (command-table-p command-table)
                   command-table
                   (find-command-table command-table)))
        (line-name (line-name-for name command-name)))
    (when (and errorp (command-cell table command-name))
      (error "The command ~S is already in ~S."
             command-name (command-table-name table)))
    (let ((command (make-instance 'standard-command
                                  :name command-name
                                  :line-name line-name
                                  :menu (menu-for menu line-name)
                                  :arguments arguments
                                  :function (fdefinition command-name)))
          (cell (command-cell table command-name)))
      (if cell
          (setf (cdr cell) command)
          (setf (%commands table)
                (append (%commands table) (list (cons command-name command)))))
      command)))

(defun remove-command-from-command-table (command-name command-table
                                          &key (errorp t))
  "Remove COMMAND-NAME from COMMAND-TABLE.  Inherited commands stay."
  (let* ((table (if (command-table-p command-table)
                    command-table
                    (find-command-table command-table)))
         (cell (command-cell table command-name)))
    (unless cell
      (when errorp
        (error "The command ~S is not in ~S."
               command-name (command-table-name table)))
      (return-from remove-command-from-command-table nil))
    (setf (%commands table) (remove cell (%commands table) :test #'eq))
    command-name))

(defun find-command (name command-table &key (errorp t))
  "Return the command named NAME, searching inherited tables.
Each table is visited once.  With ERRORP false, a miss returns NIL."
  (let ((table (if (command-table-p command-table)
                   command-table
                   (find-command-table command-table)))
        (seen (make-hash-table :test #'eq)))
    (labels ((search-table (candidate)
               (unless (gethash candidate seen)
                 (setf (gethash candidate seen) t)
                 (or (cdr (command-cell candidate name))
                     (some #'search-table
                           (command-table-inherit-from candidate))))))
      (or (search-table table)
          (when errorp
            (error "The command ~S is not in ~S."
                   name (command-table-name table)))))))

(defun command-present-in-command-table-p (name command-table
                                            &key (inherited t))
  "Return true if NAME is a command in COMMAND-TABLE.
INHERITED false searches only that table."
  (let ((table (if (command-table-p command-table)
                   command-table
                   (find-command-table command-table))))
    (if inherited
        (not (null (find-command name table :errorp nil)))
        (not (null (command-cell table name))))))

(defun map-over-command-table-commands (function command-table
                                        &key (inherited nil))
  "Call FUNCTION on each command name and command.
The local commands come first, in the order they were added.  Inherited
commands follow when INHERITED is true.  A name already visited is
skipped."
  (let ((table (if (command-table-p command-table)
                   command-table
                   (find-command-table command-table)))
        (seen-tables (make-hash-table :test #'eq))
        (seen-names (make-hash-table :test #'eq)))
    (labels ((visit (candidate)
               (unless (gethash candidate seen-tables)
                 (setf (gethash candidate seen-tables) t)
                 (dolist (cell (%commands candidate))
                   (unless (gethash (car cell) seen-names)
                     (setf (gethash (car cell) seen-names) t)
                     (funcall function (car cell) (cdr cell))))
                 (when inherited
                   (map nil #'visit (command-table-inherit-from candidate))))))
      (visit table)))
  nil)

(defun command-table-menu (command-table)
  "Return the menu of COMMAND-TABLE as a list of (string command-name).
Inherited commands are included.  Only commands with a menu string
appear, in the order MAP-OVER-COMMAND-TABLE-COMMANDS visits them."
  (let ((menu nil))
    (map-over-command-table-commands
     (lambda (name command)
       (when (command-menu command)
         (push (list (command-menu command) name) menu)))
     command-table
     :inherited t)
    (nreverse menu)))

(define-command-table command-tests)

(defun com-double (number)
  "Return twice NUMBER.  The behavior below registers this function."
  (* 2 number))

(behavior 'make-command-table
  (let ((table (find-command-table 'command-tests)))
    (should-be-true (command-table-p table))
    (should-eq 'command-tests (command-table-name table))
    (add-command-to-command-table 'com-double table :name t :menu t
                                  :arguments '((number integer)))
    (should-be-true (command-present-in-command-table-p 'com-double table))
    (should-equal "Double" (command-line-name (find-command 'com-double table)))
    (should-equal '(("Double" com-double)) (command-table-menu table))
    (should= 8 (funcall (command-function (find-command 'com-double table)) 4))
    (remove-command-from-command-table 'com-double table)
    (should-be-false (command-present-in-command-table-p 'com-double table
                                                         :inherited nil)))
  (should-be-true
   (handler-case
       (progn (find-command-table 'missing-command-table) nil)
     (error () t))))
