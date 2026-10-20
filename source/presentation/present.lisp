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

(defpackage :FitzCLIM/presentation/present
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/frame
        :FitzCLIM/event
        :FitzCLIM/output-record
        :FitzCLIM/presentation)
  (:import-from :FitzCLIM/presentation
                :expression
                :find-presentation-type-class
                :invoke-presentation-method
                :presentation-type-class-history
                :widget)
  (:export :*accept-input-editor*
           :*accepting-values-acceptor*
           :*completion-possibilities*
           :*input-context*
           :*pointer-documentation-view*
           :+gadget-view+
           :+pointer-documentation-view+
           :+textual-dialog-view+
           :+textual-view+
           :accept
           :accept-from-string
           :gadget-view
           :input-context-type
           :make-presentation
           :pointer-documentation-view
           :present
           :present-to-string
           :presentation
           :presentation-modifier
           :presentation-object
           :presentation-single-box
           :presentation-type
           :presentation-type-history
           :presentation-type-of
           :presentation-view
           :presentationp
           :standard-presentation
           :textual-dialog-view
           :textual-view
           :view
           :viewp
           :with-input-context
           :with-output-as-presentation))
(in-package :FitzCLIM/presentation/present)

(defclass view ()
  ()
  (:documentation "A way of presenting and accepting an object."))

(defun viewp (object)
  "Return true if OBJECT is a view."
  (typep object 'view))

(defclass textual-view (view)
  ()
  (:documentation "Present an object as text."))

(defclass textual-dialog-view (textual-view)
  ()
  (:documentation "Present an object as text inside a dialog."))

(defclass gadget-view (view)
  ()
  (:documentation "Present an object as a gadget."))

(defclass pointer-documentation-view (view)
  ()
  (:documentation "Present an object in the pointer documentation."))

(defvar +textual-view+ (make-instance 'textual-view)
  "The view that presents objects as text.")

(defvar +textual-dialog-view+ (make-instance 'textual-dialog-view)
  "The view that presents objects as text in a dialog.")

(defvar +gadget-view+ (make-instance 'gadget-view)
  "The view that presents objects as gadgets.")

(defvar +pointer-documentation-view+ (make-instance 'pointer-documentation-view)
  "The view used for pointer documentation.")

(defvar *pointer-documentation-view* +pointer-documentation-view+
  "The view used while writing pointer documentation.")

(behavior 'textual-view
  (should-be-true (viewp +textual-view+))
  (should-be-true (typep +textual-dialog-view+ 'textual-view))
  (should-be-true (viewp +gadget-view+))
  (should-eq +pointer-documentation-view+ *pointer-documentation-view*))

(defun presentation-type-of (object)
  "Return a presentation type specifier for OBJECT."
  (typecase object
    (null 'null)
    ((eql t) 'boolean)
    (integer 'integer)
    (ratio 'rational)
    (float 'float)
    (keyword 'keyword)
    (symbol 'symbol)
    (string 'string)
    (character 'character)
    (pathname 'pathname)
    (cons 'expression)
    (t
     (let ((name (class-name (class-of object))))
       (if (and name (find-presentation-type-class name :errorp nil))
           name
           t)))))

(defclass presentation ()
  ((object
    :initarg :object
    :reader presentation-object
    :documentation "The Lisp object this presentation stands for.")
   (type
    :initarg :type
    :reader presentation-type
    :documentation "The presentation type of the object.")
   (single-box
    :initarg :single-box
    :initform nil
    :accessor presentation-single-box
    :documentation "True when the presentation is one sensitive box.")
   (modifier
    :initarg :modifier
    :initform nil
    :accessor presentation-modifier
    :documentation "A value translators may use to tell copies apart.")
   (view
    :initarg :view
    :initform nil
    :accessor presentation-view
    :documentation "The view that produced this presentation, or NIL."))
  (:documentation "A Lisp object presented to the user."))

(defclass standard-presentation (presentation compound-output-record)
  ()
  (:documentation
   "A presentation stored with its object, its type, and its output."))

(defmethod output-record-refined-position-test
    ((record standard-presentation) x y)
  (if (presentation-single-box record)
      t
      (some (lambda (child)
              (output-record-contains-position-p child x y))
            (output-record-children record))))

(defun presentationp (object)
  "Return true if OBJECT is a presentation."
  (typep object 'presentation))

(defun make-presentation (object type &key single-box modifier view)
  "Return a presentation of OBJECT as TYPE."
  (unless (presentation-type-specifier-p type)
    (error "~S is not a presentation type." type))
  (make-instance 'standard-presentation
                 :object object
                 :type type
                 :single-box single-box
                 :modifier modifier
                 :view view))

(behavior 'make-presentation
  (let ((presentation (make-presentation 'gear 'widget :modifier 2)))
    (should-be-true (presentationp presentation))
    (should-eq 'gear (presentation-object presentation))
    (should-eq 'widget (presentation-type presentation))
    (should= 2 (presentation-modifier presentation))))

(defmacro with-output-as-presentation
    ((stream object type &key modifier single-box (allow-sensitive-inferiors t))
     &body body)
  "Run BODY and return a presentation of OBJECT as TYPE.
A recording STREAM saves BODY inside the presentation.  Any other
stream runs BODY and returns a presentation that has no children.
ALLOW-SENSITIVE-INFERIORS false keeps a nested presentation from
being recorded."
  (let ((stream-var (gensym "STREAM"))
        (presentation (gensym "PRESENTATION"))
        (allow (gensym "ALLOW")))
    `(let ((,stream-var ,stream)
           (,allow ,allow-sensitive-inferiors))
       (unless (presentation-type-specifier-p ,type)
         (error "~S is not a presentation type." ,type))
       (cond
         ((and ,stream-var
               (output-recording-stream-p ,stream-var)
               (stream-allows-sensitive-inferiors ,stream-var))
          (with-new-output-record
              (,stream-var 'standard-presentation ,presentation
               :object ,object
               :type ,type
               :modifier ,modifier
               :single-box ,single-box)
            (let ((old (stream-allows-sensitive-inferiors ,stream-var)))
              (setf (stream-allows-sensitive-inferiors ,stream-var) ,allow)
              (unwind-protect
                   (progn ,@body)
                (setf (stream-allows-sensitive-inferiors ,stream-var) old)))))
         ((and ,stream-var (output-recording-stream-p ,stream-var))
          ,@body
          nil)
         (t
          ,@body
          (make-presentation ,object ,type
                             :modifier ,modifier
                             :single-box ,single-box))))))

(behavior 'with-output-as-presentation
  (let ((presentation (with-output-as-presentation (nil 'gear 'widget)
                        1)))
    (should-eq 'gear (presentation-object presentation))
    (should-eq 'widget (presentation-type presentation))))

(defun present (object &key (stream *standard-output*)
                         (type (presentation-type-of object))
                         (view +textual-view+)
                         acceptably
                         (for-context-type type)
                         single-box
                         (sensitive t))
  "Write OBJECT of TYPE to STREAM in VIEW.  Return OBJECT.
On a recording stream, SENSITIVE true saves the output as a presentation.
SINGLE-BOX makes the whole bounding box sensitive."
  (unless (presentation-type-specifier-p type)
    (error "~S is not a presentation type." type))
  (flet ((draw ()
           (invoke-presentation-method 'present type object stream view
                                       :acceptably acceptably
                                       :for-context-type for-context-type)))
    (if (and sensitive stream (output-recording-stream-p stream))
        (with-output-as-presentation (stream object type :single-box single-box)
          (draw))
        (draw)))
  object)

(defun present-to-string (object &rest keys &key type view acceptably for-context-type)
  "Return the text PRESENT would write for OBJECT."
  (declare (ignore type view acceptably for-context-type))
  (with-output-to-string (stream)
    (apply #'present object :stream stream keys)))

(defun whitespace? (character)
  "Return true if CHARACTER is a blank that ends a token."
  (member character '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))

(defun read-token (stream)
  "Read one token from STREAM.
A token in double quotes may contain spaces.  The returned string does
not include the quotes.  A backslash inside quotes escapes the next
character.  Signal SIMPLE-PARSE-ERROR at the end of the stream."
  (let ((first (peek-char t stream nil nil)))
    (unless first
      (error 'simple-parse-error
             :format-control "End of input while reading a token."
             :format-arguments nil))
    (if (char= first #\")
        (progn
          (read-char stream)
          (with-output-to-string (out)
            (loop for character = (read-char stream nil nil)
                  do (cond
                       ((null character)
                        (error 'simple-parse-error
                               :format-control "A quoted token ran off the end of the input."
                               :format-arguments nil))
                       ((char= character #\\)
                        (let ((escaped (read-char stream nil nil)))
                          (unless escaped
                            (error 'simple-parse-error
                                   :format-control "A quoted token ran off the end of the input."
                                   :format-arguments nil))
                          (write-char escaped out)))
                       ((char= character #\")
                        (return))
                       (t (write-char character out))))))
        (with-output-to-string (out)
          (loop for character = (peek-char nil stream nil nil)
                while (and character (not (whitespace? character)))
                do (write-char (read-char stream) out))))))

(defvar *presentation-type-histories* (make-hash-table :test #'eq)
  "Accepted objects keyed by presentation type name.")

(defun presentation-type-history (type)
  "Return the objects accepted as TYPE, newest first."
  (gethash (presentation-type-name type) *presentation-type-histories*))

(defun remember-presentation (type object)
  "Push OBJECT onto the history of TYPE when that type records one."
  (when (presentation-type-class-history
         (find-presentation-type-class (presentation-type-name type)))
    (push object (gethash (presentation-type-name type)
                          *presentation-type-histories*))))

(defvar *accepting-values-acceptor* nil
  "When set, ACCEPT calls this function instead of reading.
The function receives the type, stream, default, default type, prompt,
and query identifier.  It returns the object and its type.
ACCEPTING-VALUES binds it.")

(defvar *accept-input-editor* nil
  "A function of a stream and a continuation, or NIL.
ACCEPT calls it around the presentation method.  The continuation
receives the stream to read.  NIL reads the original stream.  The input
editor sets it.")

(defvar *completion-possibilities* nil
  "Strings the input editor may complete, or NIL.
ACCEPT of COMPLETION binds this to the printed elements.  A tab in the
input editor extends the buffer from these strings.")

(defun accept (type &key (stream *standard-input*)
                      (view +textual-view+)
                      (default nil)
                      (default-type type)
                      (provide-default nil)
                      insert-default
                      prompt
                      (prompt-mode :normal)
                      display-default
                      query-identifier
                      activation-gestures
                      additional-activation-gestures
                      delimiter-gestures
                      additional-delimiter-gestures)
  "Read an object of TYPE from STREAM.  Return the object and its type.
End of input with PROVIDE-DEFAULT true returns DEFAULT and
DEFAULT-TYPE.  While ACCEPTING-VALUES is running, the query's stored
value is returned instead.  *ACCEPT-INPUT-EDITOR*, when set, supplies
the stream the presentation method reads.  The remaining keywords are
accepted for callers."
  (declare (ignore insert-default prompt-mode display-default
                   activation-gestures additional-activation-gestures
                   delimiter-gestures additional-delimiter-gestures))
  (unless (presentation-type-specifier-p type)
    (error "~S is not a presentation type." type))
  (when *accepting-values-acceptor*
    (multiple-value-bind (object object-type)
        (funcall *accepting-values-acceptor*
                 type stream default default-type prompt query-identifier)
      (let ((result-type (or object-type type)))
        (remember-presentation type object)
        (return-from accept (values object result-type)))))
  (when (and provide-default (null (peek-char t stream nil nil)))
    (return-from accept (values default default-type)))
  (flet ((read-object (input)
           (invoke-presentation-method 'accept type input view
                                       :default default
                                       :default-type default-type)))
    (multiple-value-bind (object object-type)
        (if *accept-input-editor*
            (funcall *accept-input-editor* stream #'read-object)
            (read-object stream))
      (let ((result-type (or object-type type)))
        (remember-presentation type object)
        (values object result-type)))))

(defun accept-from-string (type string &rest keys &key view default default-type provide-default)
  "Read an object of TYPE from STRING.  Return the object and its type."
  (declare (ignore view default default-type provide-default))
  (with-input-from-string (stream string)
    (apply #'accept type :stream stream keys)))

(behavior 'present
  (should-equal "GEAR" (present-to-string 'gear :type t))
  (should-eq 'symbol (presentation-type-of 'gear))
  (should-eq 'null (presentation-type-of nil))
  (should-eq t (presentation-type-of
                (make-instance 'standard-presentation :object 1 :type t))))

(defvar *input-context* nil
  "The active input contexts, innermost first.
Each entry is (presentation-type . catch-tag).")

(defun input-context-type ()
  "Return the presentation type of the innermost input context, or NIL."
  (car (first *input-context*)))

(defmacro with-input-context ((type &key override)
                              (&optional object-var type-var event-var options-var)
                              form
                              &body clauses)
  "Establish TYPE as an input context while evaluating FORM.
A gesture throws an object, its type, the event, and options to this
context.  Each clause is ((presentation-type) body) and runs when the
thrown type is a subtype of that presentation type.  A normal return
from FORM is the value of this form.  OVERRIDE true hides the contexts
already established."
  (let ((block (gensym "INPUT"))
        (tag (gensym "INPUT"))
        (object (or object-var (gensym "OBJECT")))
        (thrown-type (or type-var (gensym "TYPE")))
        (event (or event-var (gensym "EVENT")))
        (options (or options-var (gensym "OPTIONS"))))
    `(block ,block
       (multiple-value-bind (,object ,thrown-type ,event ,options)
           (catch ',tag
             (let ((*input-context*
                     ,(if override
                          `(list (cons ,type ',tag))
                          `(cons (cons ,type ',tag) *input-context*))))
               (return-from ,block ,form)))
         (declare (ignorable ,object ,thrown-type ,event ,options))
         (cond
           ,@(mapcar (lambda (clause)
                       `((presentation-subtypep ,thrown-type ',(first clause))
                         ,@(rest clause)))
                     clauses)
           (t (error "No input context clause matches ~S." ,thrown-type)))))))

(behavior 'with-input-context
  (should-equal 'gear
                (with-input-context ('widget) (object)
                    (throw (cdr (first *input-context*))
                      (values 'gear 'widget nil nil))
                  (widget object)))
  (should-eq 'widget
             (with-input-context ('widget) ()
                 (input-context-type))))
