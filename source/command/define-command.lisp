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

(in-package :FitzCLIM/command)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun command-type-specifier (type)
  "Return TYPE as a type specifier.  A quoted presentation type is
unquoted."
  (if (and (consp type)
           (eq (first type) 'quote)
           (null (cddr type)))
      (second type)
      type))

(defun parse-define-command-name (spec)
  "Return the command name, command table, line name, and menu of SPEC."
  (if (symbolp spec)
      (values spec *default-command-table* t nil)
      (let ((name (first spec))
            (table *default-command-table*)
            (line-name t)
            (menu nil))
        (loop for (key value) on (rest spec) by #'cddr
              do (ecase key
                   (:command-table (setf table value))
                   (:name (setf line-name value))
                   (:menu (setf menu value))))
        (values name table line-name menu)))))

(defmacro define-command (name-and-options arguments &body body)
  "Define a command and add it to a command table.
NAME-AND-OPTIONS is a symbol, or a list (name :command-table table
:name line-name :menu menu).  A symbol uses *DEFAULT-COMMAND-TABLE*.
Each argument is (name type) or (name type &rest options).  TYPE is a
Common Lisp type specifier or a quoted specifier.  The options are
kept for later and are not interpreted yet.  Redefining a command
replaces its table entry."
  (multiple-value-bind (name table line-name menu)
      (parse-define-command-name name-and-options)
    (unless table
      (error "DEFINE-COMMAND needs a command table for ~S." name))
    (let ((parameters nil)
          (checks nil)
          (stored nil))
      (dolist (argument arguments)
        (unless (and (consp argument) (symbolp (first argument)))
          (error "A command argument must be a list starting with a name."))
        (let ((parameter (first argument))
              (type (command-type-specifier (second argument))))
          (push parameter parameters)
          (push `(check-type ,parameter ,type) checks)
          (push (list parameter type) stored)))
      `(progn
         (defun ,name ,(nreverse parameters)
           ,@(nreverse checks)
           ,@body)
         (add-command-to-command-table ',name ',table
                                       :name ,line-name
                                       :menu ',menu
                                       :arguments ',(nreverse stored)
                                       :errorp nil)
         ',name))))

(define-command-table command-macro-tests)

(define-command (com-add :command-table command-macro-tests :name t :menu "Add")
    ((left integer) (right 'integer))
  "Return the sum of LEFT and RIGHT."
  (+ left right))

(define-command (com-hidden :command-table command-macro-tests :name nil)
    ((text string))
  "Return TEXT unchanged.  This command has no command-line name."
  text)

(define-command-table command-macro-child :inherit-from (command-macro-tests))

(behavior 'define-command
  (let ((table (find-command-table 'command-macro-tests))
        (child (find-command-table 'command-macro-child)))
    (should-equal "Add" (command-line-name (find-command 'com-add table)))
    (should-equal '(("Add" com-add)) (command-table-menu table))
    (should= 7 (funcall (command-function (find-command 'com-add table)) 3 4))
    (should-equal '((left integer) (right integer))
                  (command-arguments (find-command 'com-add table)))
    (should-be-null (command-line-name (find-command 'com-hidden table)))
    (should-be-true (command-present-in-command-table-p 'com-add child))
    (should-be-false (command-present-in-command-table-p 'com-add child
                                                         :inherited nil))
    (let ((names nil))
      (map-over-command-table-commands
       (lambda (name command)
         (declare (ignore command))
         (push name names))
       child
       :inherited t)
      (should-equal '(com-hidden com-add) names)))
  (should-be-true
   (handler-case
       (progn (com-add 1 "two") nil)
     (error () t))))
