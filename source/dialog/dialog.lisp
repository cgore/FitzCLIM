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

(defpackage :FitzCLIM/dialog
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present)
  (:export :accepting-values
           :menu-choose
           :menu-choose-from-command-table))
(in-package :FitzCLIM/dialog)

(defun menu-item-parts (item printer)
  "Return the display string and the value of ITEM.
A string or other atom is its own value.  A list (display value)
uses DISPLAY.  A list (display :value value) does the same."
  (cond
    ((and (consp item)
          (consp (rest item))
          (eq (second item) :value))
     (values (princ-to-string (first item)) (third item)))
    ((and (consp item)
          (consp (rest item))
          (null (cddr item)))
     (values (princ-to-string (first item)) (second item)))
    (t (values (funcall printer item) item))))

(defun menu-item-display (item printer)
  "Return the text shown for ITEM."
  (nth-value 0 (menu-item-parts item printer)))

(defun menu-item-value (item printer)
  "Return the value chosen when ITEM is selected."
  (nth-value 1 (menu-item-parts item printer)))

(defun digit-line? (line)
  "Return true if every character of LINE is a digit."
  (and (plusp (length line))
       (every #'digit-char-p line)))

(defun find-menu-item (line items printer)
  "Return the item named or numbered by LINE.
Names are compared without regard to case.  A number is 1-based."
  (or (find line items
            :test #'string-equal
            :key (lambda (item) (menu-item-display item printer)))
      (when (digit-line? line)
        (let ((index (parse-integer line)))
          (unless (<= 1 index (length items))
            (error "~D is outside this menu." index))
          (nth (1- index) items)))
      (error "~S is not a menu item." line)))

(defun menu-abort? (line)
  "Return true if LINE leaves the menu without a choice."
  (or (zerop (length line))
      (string-equal line "Cancel")
      (string-equal line "Abort")))

(defun invoke-menu-choose (items continuation-printer &key label stream input)
  "Write ITEMS and read one choice from INPUT.
Return the item's value and :RETURN.  An empty line, Cancel, or Abort
returns NIL and NIL."
  (let ((items (coerce items 'list))
        (printer (or continuation-printer #'princ-to-string)))
    (when stream
      (when label
        (format stream "~A~%" label))
      (loop for item in items
            for index from 1
            do (format stream "~D. ~A~%"
                       index (menu-item-display item printer))))
    (let ((line (string-trim '(#\Space #\Tab)
                             (or (read-line input nil "") ""))))
      (if (menu-abort? line)
          (values nil nil)
          (values (menu-item-value (find-menu-item line items printer) printer)
                  :return)))))

(defun menu-choose (items &key (printer #'princ-to-string) label
                            (stream *standard-output*)
                            (input *standard-input*)
                            default-item associated-window
                            cache unique-id id-test cache-value cache-test
                            n-columns x-spacing y-spacing)
  "Choose one of ITEMS.  Return the value and the gesture :RETURN.
An item is an object, a list (display value), or a list
(display :value value).  The menu is written to STREAM and the choice
is read from INPUT, one line.  A line may be the display text or a
1-based number.  An empty line, Cancel, or Abort returns NIL.
DEFAULT-ITEM, ASSOCIATED-WINDOW, and the cache and column keywords are
accepted for callers.  This menu is textual."
  (declare (ignore default-item associated-window cache unique-id id-test
                   cache-value cache-test n-columns x-spacing y-spacing))
  (invoke-menu-choose items printer :label label :stream stream :input input))

(defun menu-choose-from-command-table (command-table &rest keys)
  "Choose a command from COMMAND-TABLE.  Return its name.
The menu is COMMAND-TABLE-MENU.  KEYS are passed to MENU-CHOOSE."
  (apply #'menu-choose (command-table-menu command-table) keys))

(define-command-table dialog-menu-tests)

(define-command (com-dialog-add :command-table dialog-menu-tests
                                :name t :menu "Add")
    ((left integer) (right integer))
  "Return the sum of LEFT and RIGHT."
  (+ left right))

(behavior 'menu-choose
  (let ((text (make-string-output-stream)))
    (multiple-value-bind (value gesture)
        (menu-choose '("Ada" "Grace")
                     :label "Pilot"
                     :stream text
                     :input (make-string-input-stream "2"))
      (should-equal "Grace" value)
      (should-eq :return gesture))
    (should-be-true (search "Pilot" (get-output-stream-string text))))
  (with-input-from-string (input "Add")
    (let ((name (menu-choose-from-command-table
                 'dialog-menu-tests
                 :stream (make-broadcast-stream)
                 :input input)))
      (should-eq 'com-dialog-add name)
      (should= 7 (funcall (command-function
                           (find-command name 'dialog-menu-tests))
                          3 4))))
  (should-be-null (menu-choose '("Ada")
                               :stream (make-broadcast-stream)
                               :input (make-string-input-stream "Cancel"))))

(defclass av-query ()
  ((id :initarg :id :reader av-query-id)
   (type :initarg :type :reader av-query-type)
   (prompt :initarg :prompt :reader av-query-prompt)
   (value :initarg :value :accessor av-query-value))
  (:documentation "One field of an accepting-values dialog."))

(defclass accepting-values-state ()
  ((queries
    :initform nil
    :accessor av-queries
    :documentation "Fields recorded during the current pass, oldest last.")
   (values
    :initform (make-hash-table :test #'equal)
    :accessor av-values
    :documentation "Field values keyed by query identifier.")
   (next-id
    :initform 0
    :accessor av-next-id
    :documentation "The next identifier for a field that names none."))
  (:documentation "The fields of one accepting-values dialog."))

(defun query-label (prompt id)
  "Return the label of a field."
  (cond
    ((stringp prompt) prompt)
    (prompt (princ-to-string prompt))
    ((symbolp id)
     (string-capitalize (substitute #\Space #\- (symbol-name id))))
    (t "Value")))

(defun record-accepting-value (state type default prompt query-identifier)
  "Remember one ACCEPT and return its current value and type."
  (let* ((id (or query-identifier
                 (prog1 (av-next-id state)
                   (incf (av-next-id state)))))
         (label (query-label prompt id)))
    (multiple-value-bind (stored present) (gethash id (av-values state))
      (let ((value (if present stored default)))
        (setf (gethash id (av-values state)) value)
        (push (make-instance 'av-query
                             :id id
                             :type type
                             :prompt label
                             :value value)
              (av-queries state))
        (values value type)))))

(defun write-accepting-values (stream state label exit-boxes)
  "Write the dialog's label, fields, and exit boxes."
  (when label
    (format stream "~A~%" label))
  (dolist (query (reverse (av-queries state)))
    (format stream "~A: ~A~%"
            (av-query-prompt query)
            (present-to-string (av-query-value query)
                               :type (av-query-type query))))
  (dolist (box exit-boxes)
    (format stream "~A~%" (second box))))

(defun dialog-line (input)
  "Return the next trimmed line from INPUT, or NIL at the end."
  (let ((line (read-line input nil nil)))
    (when line
      (string-trim '(#\Space #\Tab) line))))

(defun exit-box-action (line exit-boxes)
  "Return :EXIT or :ABORT when LINE names an exit box."
  (let ((box (find line exit-boxes :key #'second :test #'string-equal)))
    (when box
      (let ((action (first box)))
        (unless (member action '(:exit :abort))
          (error "An exit box must be :EXIT or :ABORT."))
        action))))

(defun find-dialog-query (state token)
  "Return the field named TOKEN, matching its identifier or its label."
  (find-if (lambda (query)
             (or (string-equal token (av-query-prompt query))
                 (and (symbolp (av-query-id query))
                      (string-equal token (symbol-name (av-query-id query))))
                 (and (integerp (av-query-id query))
                      (string-equal token
                                    (princ-to-string (av-query-id query))))))
           (av-queries state)))

(defun apply-dialog-edit (state line)
  "Store the new value named by LINE.  Return true when it changed.
LINE is a field name, a space, and text ACCEPT can read for that field."
  (let* ((space (position #\Space line))
         (token (if space (subseq line 0 space) line))
         (text (if space (string-trim '(#\Space #\Tab) (subseq line (1+ space))) ""))
         (query (find-dialog-query state token)))
    (unless query
      (error "~S is not a field in this dialog." token))
    (let ((object (accept-from-string (av-query-type query) text)))
      (unless (equal object (gethash (av-query-id query) (av-values state)))
        (setf (gethash (av-query-id query) (av-values state)) object)
        t))))

(defun read-accepting-values (state input exit-boxes)
  "Read edits until an exit box or the end of INPUT.
Return :EXIT, :ABORT, or :RESCAN.  End of input confirms the dialog."
  (loop
    (let ((line (dialog-line input)))
      (cond
        ((null line) (return :exit))
        ((exit-box-action line exit-boxes)
         (return (exit-box-action line exit-boxes)))
        ((apply-dialog-edit state line) (return :rescan))))))

(defun invoke-accepting-values (stream continuation
                                &key input label
                                  (exit-boxes '((:exit "OK") (:abort "Cancel")))
                                  own-window)
  "Run CONTINUATION as a dialog and return true unless the user aborts.
Each ACCEPT in CONTINUATION displays a field and returns the field's
value.  INPUT is a stream or a string of edits.  A line is an exit box,
or a field name and a new value.  A changed value runs CONTINUATION
again, so bindings in the body see the new value.  OWN-WINDOW is
accepted for callers.  The dialog is written as text on STREAM."
  (declare (ignore own-window))
  (let ((state (make-instance 'accepting-values-state)))
    (flet ((run (input-stream)
             (loop
               (setf (av-queries state) nil
                     (av-next-id state) 0)
               (let ((*accepting-values-acceptor*
                       (lambda (type stream default default-type prompt
                                query-identifier)
                         (declare (ignore stream default-type))
                         (record-accepting-value state type default
                                                 prompt query-identifier))))
                 (funcall continuation))
               (when stream
                 (write-accepting-values stream state label exit-boxes))
               (unless input-stream
                 (return t))
               (ecase (read-accepting-values state input-stream exit-boxes)
                 (:exit (return t))
                 (:abort (return nil))
                 (:rescan)))))
      (cond
        ((null input) (run nil))
        ((stringp input)
         (with-input-from-string (input-stream input)
           (run input-stream)))
        (t (run input))))))

(defmacro accepting-values
    ((stream &key input label
              (exit-boxes ''((:exit "OK") (:abort "Cancel")))
              own-window)
     &body body)
  "Accept the values queried by BODY and return true unless aborted.
STREAM receives the textual dialog.  INPUT is a stream or a string of
edits.  LABEL is a heading.  EXIT-BOXES is a list of (:EXIT text) and
(:ABORT text).  OWN-WINDOW is accepted for callers."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-accepting-values
        ,stream-var
        (lambda () ,@body)
        :input ,input
        :label ,label
        :exit-boxes ,exit-boxes
        :own-window ,own-window))))

(behavior 'accepting-values
  (let ((count 1)
        (name "Ada")
        (text (make-string-output-stream)))
    (should-be-true
     (accepting-values (text :input "count 4
OK"
                             :label "Pilot")
       (setf count (accept 'integer :stream text
                           :query-identifier 'count
                           :default count
                           :prompt "Count"))
       (setf name (accept 'string :stream text
                          :query-identifier 'name
                          :default name
                          :prompt "Name"))))
    (should= 4 count)
    (should-equal "Ada" name)
    (let ((written (get-output-stream-string text)))
      (should-be-true (search "Pilot" written))
      (should-be-true (search "Count: 1" written))
      (should-be-true (search "Count: 4" written))
      (should-be-true (search "Name: Ada" written))))
  (let ((count 1))
    (should-be-null
     (accepting-values ((make-broadcast-stream) :input "Cancel")
       (setf count (accept 'integer
                           :query-identifier 'count
                           :default count
                           :prompt "Count"))))
    (should= 1 count))
  (should-be-true
   (handler-case
       (progn
         (accepting-values ((make-broadcast-stream) :input "nope 1
OK")
           (accept 'integer :query-identifier 'count :default 1 :prompt "Count"))
         nil)
     (error () t))))
