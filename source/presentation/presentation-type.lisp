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

(defpackage :FitzCLIM/presentation
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/frame
        :FitzCLIM/event
        :FitzCLIM/output-record)
  (:export :*accept-input-editor*
           :*accepting-values-acceptor*
           :*input-context*
           :*pointer-documentation-view*
           :+gadget-view+
           :+pointer-documentation-view+
           :+textual-dialog-view+
           :+textual-view+
           :accept
           :accept-frame-command
           :accept-from-string
           :blank-area
           :call-presentation-translator
           :command-from-presentation
           :define-presentation-action
           :define-presentation-method
           :define-presentation-to-command-translator
           :define-presentation-translator
           :define-presentation-type
           :describe-presentation-type
           :document-presentation-translator
           :execute-presentation-gesture
           :expand-presentation-type-abbreviation
           :find-applicable-translators
           :gadget-view
           :input-context-type
           :make-presentation
           :map-over-presentation-type-supertypes
           :pointer-documentation-view
           :present
           :present-to-string
           :presentation
           :presentation-modifier
           :presentation-object
           :presentation-single-box
           :presentation-translator
           :presentation-translator-documentation
           :presentation-translator-from-type
           :presentation-translator-gesture
           :presentation-translator-menu
           :presentation-translator-name
           :presentation-translator-priority
           :presentation-translator-to-type
           :presentation-type
           :presentation-type-history
           :presentation-type-name
           :presentation-type-of
           :presentation-type-option
           :presentation-type-parameter
           :presentation-type-specifier-p
           :presentation-typep
           :presentation-subtypep
           :presentation-view
           :presentationp
           :simple-parse-error
           :standard-presentation
           :textual-dialog-view
           :textual-view
           :throw-highlighted-presentation
           :view
           :viewp
           :with-input-context
           :with-output-as-presentation))
(in-package :FitzCLIM/presentation)

(define-condition simple-parse-error (parse-error)
  ((format-control
    :initarg :format-control
    :reader simple-parse-error-format-control)
   (format-arguments
    :initarg :format-arguments
    :initform nil
    :reader simple-parse-error-format-arguments))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (simple-parse-error-format-control condition)
                    (simple-parse-error-format-arguments condition))))
  (:documentation "Input that does not match a presentation type."))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass presentation-type-class ()
  ((name
    :initarg :name
    :reader presentation-type-class-name)
   (parameter-lambda-list
    :initarg :parameter-lambda-list
    :reader presentation-type-class-parameter-lambda-list)
   (parameter-names
    :initarg :parameter-names
    :reader presentation-type-class-parameter-names)
   (options
    :initarg :options
    :reader presentation-type-class-options
    :documentation "A list of (name default-form).")
   (description
    :initarg :description
    :initform nil
    :reader presentation-type-class-description)
   (history
    :initarg :history
    :initform nil
    :reader presentation-type-class-history)
   (decoder
    :initarg :decoder
    :reader presentation-type-class-decoder
    :documentation "Function of the parameter arguments.  Returns an alist.")
   (supertype
    :initarg :supertype
    :reader presentation-type-class-supertype
    :documentation "Function of the parameter arguments.  Returns a type or NIL."))
  (:documentation "The definition of one presentation type."))

(defvar *presentation-type-classes* (make-hash-table :test #'eq)
  "Presentation type classes keyed by name.")

(defvar *presentation-methods* (make-hash-table :test #'equal)
  "Presentation methods keyed by (method-name . type-name).")

(defun find-presentation-type-class (name &key (errorp t))
  "Return the presentation type class named NAME."
  (or (gethash name *presentation-type-classes*)
      (when errorp
        (error "~S is not a presentation type." name))))

(defun lambda-list-variables (lambda-list)
  "Return the variables bound by LAMBDA-LIST."
  (let ((variables nil))
    (dolist (item lambda-list (nreverse variables))
      (unless (member item lambda-list-keywords)
        (push (if (consp item) (first item) item) variables)))))

(defun normalize-option (option)
  "Return (name default-form) for a presentation type option."
  (cond
    ((symbolp option) (list option nil))
    ((and (consp option) (symbolp (first option)))
     (list (first option) (second option)))
    (t (error "~S is not a presentation type option." option))))

(defun install-presentation-type (name lambda-list parameter-names options
                                  description history decoder supertype)
  "Store a presentation type class under NAME."
  (setf (gethash name *presentation-type-classes*)
        (make-instance 'presentation-type-class
                       :name name
                       :parameter-lambda-list lambda-list
                       :parameter-names parameter-names
                       :options options
                       :description description
                       :history history
                       :decoder decoder
                       :supertype supertype)))

(defun presentation-type-name (type)
  "Return the name symbol of the presentation type specifier TYPE."
  (if (consp type) (first type) type))

(defun split-specifier-arguments (lambda-list option-specs args)
  "Split ARGS into parameter arguments and an option alist.
A keyword starts the options only when it names one of OPTION-SPECS.
A lambda list that contains &KEY takes every argument as a parameter."
  (if (member '&key lambda-list)
      (values args nil)
      (let ((option-keys (mapcar (lambda (spec)
                                   (intern (symbol-name (first spec)) :keyword))
                                 option-specs)))
        (loop for rest on args
              for item = (first rest)
              when (and (keywordp item) (member item option-keys))
                do (return (values (ldiff args rest)
                                   (loop while rest
                                         unless (rest rest)
                                           do (error "The option ~S needs a value."
                                                     (first rest))
                                         collect (cons (pop rest) (pop rest)))))
              finally (return (values args nil))))))

(defun decode-presentation-type (type)
  "Return the class, parameter alist, and option alist of TYPE."
  (let ((class (find-presentation-type-class (presentation-type-name type))))
    (multiple-value-bind (parameters options)
        (split-specifier-arguments
         (presentation-type-class-parameter-lambda-list class)
         (presentation-type-class-options class)
         (if (consp type) (rest type) nil))
      (values class
              (funcall (presentation-type-class-decoder class) parameters)
              options))))

(defun alist-value (alist name)
  "Return the value named NAME in ALIST, matching by symbol name."
  (cdr (assoc (symbol-name name) alist
              :key #'symbol-name
              :test #'string=)))

(defun presentation-type-parameter (type name)
  "Return the parameter NAME of the presentation type specifier TYPE."
  (multiple-value-bind (class parameters options)
      (decode-presentation-type type)
    (declare (ignore class options))
    (alist-value parameters name)))

(defun presentation-type-parameters (type)
  "Return the parameter alist of TYPE."
  (multiple-value-bind (class parameters options)
      (decode-presentation-type type)
    (declare (ignore class options))
    parameters))

(defun presentation-type-option (type name)
  "Return the option NAME of TYPE, or its default."
  (multiple-value-bind (class parameters options)
      (decode-presentation-type type)
    (declare (ignore parameters))
    (let ((supplied (assoc (intern (symbol-name name) :keyword) options)))
      (if supplied
          (cdr supplied)
          (let ((spec (assoc (symbol-name name)
                             (presentation-type-class-options class)
                             :key #'symbol-name
                             :test #'string=)))
            (when spec
              (eval (second spec))))))))

(defun presentation-type-direct-supertype (type)
  "Return the presentation type TYPE inherits from, or NIL.
A supertype equal to TYPE is treated as no supertype, so a cycle stops."
  (let ((class (find-presentation-type-class (presentation-type-name type))))
    (multiple-value-bind (parameters options)
        (split-specifier-arguments
         (presentation-type-class-parameter-lambda-list class)
         (presentation-type-class-options class)
         (if (consp type) (rest type) nil))
      (declare (ignore options))
      (let ((super (funcall (presentation-type-class-supertype class)
                            parameters)))
        (when (and super
                   (not (and (presentation-type-specifier-p super)
                             (presentation-types-match? super type))))
          super)))))

(defun register-presentation-method (method-name type-name function)
  "Store FUNCTION as METHOD-NAME for the presentation type TYPE-NAME."
  (setf (gethash (cons method-name type-name) *presentation-methods*) function))

(defun find-presentation-method (method-name type-name)
  "Return the METHOD-NAME function for TYPE-NAME, or NIL."
  (gethash (cons method-name type-name) *presentation-methods*))

(defun method-bindings (type-var type-name body)
  "Wrap BODY in bindings for the parameters and options of TYPE-NAME."
  (let* ((class (find-presentation-type-class type-name))
         (parameters (presentation-type-class-parameter-names class))
         (options (mapcar #'first (presentation-type-class-options class)))
         (locals nil)
         (forms nil))
    (dolist (name parameters)
      (let ((local (intern (symbol-name name) *package*)))
        (push local locals)
        (push `(,local (presentation-type-parameter ,type-var ',name)) forms)))
    (dolist (name options)
      (let ((local (intern (symbol-name name) *package*)))
        (push local locals)
        (push `(,local (presentation-type-option ,type-var ',name)) forms)))
    `(let ,(nreverse forms)
       ,@(when locals `((declare (ignorable ,@(nreverse locals)))))
       ,@body)))

(defmacro define-presentation-type (name-and-parameters
                                    &key options (inherit-from t)
                                      description history)
  "Define a presentation type.
NAME-AND-PARAMETERS is a name, or (name &rest lambda-list).  The
lambda list binds the parameters of a specifier.  OPTIONS is a list of
names or (name default) pairs.  A keyword in a specifier is an option
only when it names one of those options.  INHERIT-FROM is a form,
evaluated with the parameters bound.  It returns a supertype specifier
or NIL.  DESCRIPTION is a singular noun phrase.  HISTORY true records
accepted objects."
  (let* ((name (if (consp name-and-parameters)
                   (first name-and-parameters)
                   name-and-parameters))
         (lambda-list (if (consp name-and-parameters)
                          (rest name-and-parameters)
                          nil))
         (variables (lambda-list-variables lambda-list))
         (option-specs (mapcar #'normalize-option options)))
    (when (and (member '&key lambda-list) option-specs)
      (error "A presentation type cannot have both &KEY parameters and options."))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (install-presentation-type
        ',name
        ',lambda-list
        ',variables
        ',option-specs
        ,description
        ,history
        (lambda (parameters)
          (destructuring-bind ,lambda-list parameters
            ,@(when variables `((declare (ignorable ,@variables))))
            (list ,@(mapcar (lambda (variable)
                              `(cons ',variable ,variable))
                            variables))))
        (lambda (parameters)
          (destructuring-bind ,lambda-list parameters
            ,@(when variables `((declare (ignorable ,@variables))))
            ,inherit-from))))))

(defmacro define-presentation-method (method-name arglist &body body)
  "Define a presentation method.
METHOD-NAME is PRESENTATION-TYPEP, PRESENT, ACCEPT,
PRESENTATION-SUBTYPEP, or DESCRIBE-PRESENTATION-TYPE.  One argument is
(variable type-name).  The type's parameters and options are bound in
BODY."
  (ecase method-name
    (presentation-typep
     (destructuring-bind (object (type-var type-name)) arglist
       `(register-presentation-method
         'presentation-typep ',type-name
         (lambda (,type-var ,object)
           (declare (ignorable ,type-var ,object))
           ,(method-bindings type-var type-name body)))))
    (present
     (destructuring-bind (object (type-var type-name) stream view &rest rest)
         arglist
       (unless (or (null rest) (eq (first rest) '&key))
         (error "Only &key may follow the view in a PRESENT method."))
       `(register-presentation-method
         'present ',type-name
         (lambda (,type-var ,object ,stream ,view
                  &key acceptably for-context-type &allow-other-keys)
           (declare (ignorable ,type-var ,object ,stream ,view
                               acceptably for-context-type))
           ,(method-bindings type-var type-name body)))))
    (accept
     (destructuring-bind ((type-var type-name) stream view &rest rest) arglist
       (unless (or (null rest) (eq (first rest) '&key))
         (error "Only &key may follow the view in an ACCEPT method."))
       `(register-presentation-method
         'accept ',type-name
         (lambda (,type-var ,stream ,view
                  &key default default-type &allow-other-keys)
           (declare (ignorable ,type-var ,stream ,view default default-type))
           ,(method-bindings type-var type-name body)))))
    (presentation-subtypep
     (destructuring-bind ((type-var type-name) putative) arglist
       `(register-presentation-method
         'presentation-subtypep ',type-name
         (lambda (,type-var ,putative)
           (declare (ignorable ,type-var ,putative))
           ,(method-bindings type-var type-name body)))))
    (describe-presentation-type
     (destructuring-bind ((type-var type-name) stream plural-count) arglist
       `(register-presentation-method
         'describe-presentation-type ',type-name
         (lambda (,type-var ,stream ,plural-count)
           (declare (ignorable ,type-var ,stream ,plural-count))
           ,(method-bindings type-var type-name body)))))))
) ;; eval-when

(defun presentation-type-specifier-p (object)
  "Return true if OBJECT is a known presentation type specifier."
  (and (or (symbolp object) (consp object))
       (let ((name (presentation-type-name object)))
         (and (symbolp name)
              (find-presentation-type-class name :errorp nil)
              (handler-case
                  (progn (decode-presentation-type object) t)
                (error () nil))))))

(defun range-contains? (outer-low outer-high inner-low inner-high)
  "Return true if INNER-LOW..INNER-HIGH lies inside OUTER-LOW..OUTER-HIGH.
A NIL endpoint is unbounded.  A bounded inner range does not lie inside
an outer range that is bounded on that side when the inner end is open."
  (and (or (null outer-low)
           (and inner-low (<= outer-low inner-low)))
       (or (null outer-high)
           (and inner-high (<= inner-high outer-high)))))

(defun presentation-types-match? (left right)
  "Return true if LEFT and RIGHT have the same name and parameters."
  (and (eq (presentation-type-name left) (presentation-type-name right))
       (equal (presentation-type-parameters left)
              (presentation-type-parameters right))))

(defun invoke-presentation-method (method-name type &rest arguments)
  "Call METHOD-NAME for TYPE, walking supertypes when TYPE has none.
The method is the most specific one found.  It receives TYPE, so its
parameters and options are the ones in that specifier."
  (labels ((walk (specifier)
             (let ((method (find-presentation-method
                            method-name
                            (presentation-type-name specifier))))
               (cond
                 (method (apply method type arguments))
                 (t
                  (let ((super (presentation-type-direct-supertype specifier)))
                    (if super
                        (walk super)
                        (error "There is no ~S method for ~S."
                               method-name type))))))))
    (walk type)))

(defun presentation-typep (object type)
  "Return true if OBJECT is of the presentation type TYPE."
  (unless (presentation-type-specifier-p type)
    (error "~S is not a presentation type." type))
  (let ((method (find-presentation-method 'presentation-typep
                                           (presentation-type-name type))))
    (if method
        (funcall method type object)
        (let ((super (presentation-type-direct-supertype type)))
          (when (and super (not (eq super t)))
            (presentation-typep object super))))))

(defun presentation-subtypep (type putative-supertype)
  "Return two values: whether TYPE is a subtype of PUTATIVE-SUPERTYPE,
and whether that relationship is known.
Options are ignored.  A type is a subtype of T.  A compound OR type is
a supertype of any of its disjuncts, and a subtype when every disjunct
is.  A compound AND type is a supertype only when the type is a subtype
of every conjunct, and a subtype when any conjunct is."
  (unless (and (presentation-type-specifier-p type)
               (presentation-type-specifier-p putative-supertype))
    (error "~S and ~S must be presentation types." type putative-supertype))
  (cond
    ((presentation-types-match? type putative-supertype)
     (values t t))
    ((eq (presentation-type-name putative-supertype) t)
     (values t t))
    ((eq (presentation-type-name putative-supertype) 'or)
     (dolist (one (presentation-type-parameter putative-supertype 'types)
                  (values nil t))
       (when (presentation-subtypep type one)
         (return (values t t)))))
    ((eq (presentation-type-name putative-supertype) 'and)
     (dolist (one (presentation-type-parameter putative-supertype 'types)
                  (values t t))
       (unless (presentation-subtypep type one)
         (return (values nil t)))))
    ((eq (presentation-type-name type) 'or)
     (dolist (one (presentation-type-parameter type 'types) (values t t))
       (unless (presentation-subtypep one putative-supertype)
         (return (values nil t)))))
    ((eq (presentation-type-name type) 'and)
     (dolist (one (presentation-type-parameter type 'types) (values nil t))
       (when (presentation-subtypep one putative-supertype)
         (return (values t t)))))
    (t
     (let ((method (find-presentation-method
                    'presentation-subtypep
                    (presentation-type-name type))))
       (if method
           (multiple-value-bind (yes known)
               (funcall method type putative-supertype)
             (if known
                 (values (not (null yes)) t)
                 (presentation-subtype-through-supertype type putative-supertype)))
           (presentation-subtype-through-supertype type putative-supertype))))))

(defun presentation-subtype-through-supertype (type putative-supertype)
  "Return the subtype relationship by walking TYPE's supertype."
  (let ((super (presentation-type-direct-supertype type)))
    (if (and super (not (presentation-types-match? super type)))
        (presentation-subtypep super putative-supertype)
        (values nil t))))

(defun describe-presentation-type (type &optional stream (plural-count 1))
  "Write a noun phrase for TYPE to STREAM.
With STREAM NIL, return the phrase.  PLURAL-COUNT 1 or NIL asks for the
singular phrase."
  (let* ((collecting (null stream))
         (out (or stream (make-string-output-stream))))
    (let ((method (find-presentation-method 'describe-presentation-type
                                             (presentation-type-name type))))
      (if method
          (funcall method type out plural-count)
          (write-string
           (or (presentation-type-class-description
                (find-presentation-type-class (presentation-type-name type)))
               (string-downcase (symbol-name (presentation-type-name type))))
           out)))
    (when collecting
      (get-output-stream-string out))))

(defun map-over-presentation-type-supertypes (function type)
  "Call FUNCTION on TYPE and then on each supertype, most specific first."
  (let ((seen nil))
    (labels ((walk (specifier)
               (unless (member specifier seen :test #'presentation-types-match?)
                 (push specifier seen)
                 (funcall function specifier)
                 (let ((super (presentation-type-direct-supertype specifier)))
                   (when super
                     (walk super))))))
      (walk type)))
  nil)

(defun expand-presentation-type-abbreviation (type &optional environment)
  "Return TYPE.  Abbreviation expansion is reserved for later."
  (declare (ignore environment))
  type)

(defun numeric-presentation-subtypep (type putative type-name)
  "Return the range relationship when PUTATIVE is TYPE-NAME.
The second value is NIL when PUTATIVE has another name, so the caller
can walk the supertype."
  (cond
    ((eq (presentation-type-name putative) type-name)
     (values (range-contains? (presentation-type-parameter putative 'low)
                              (presentation-type-parameter putative 'high)
                              (presentation-type-parameter type 'low)
                              (presentation-type-parameter type 'high))
             t))
    (t (values nil nil))))

(define-presentation-type t
  :inherit-from nil
  :description "an object")

(define-presentation-method presentation-typep (object (type t))
  (progn object t))

(define-presentation-method present (object (type t) stream view &key)
  (princ object stream))

(define-presentation-type gadget
  :inherit-from nil
  :description "a gadget")

(define-presentation-type (widget &optional kind)
  :inherit-from t
  :description "a widget")

(define-presentation-method presentation-typep (object (type widget))
  (and (symbolp object)
       (or (null kind) (eq object kind))))

(behavior 'define-presentation-type
  (should-be-true (presentation-type-specifier-p 'widget))
  (should-be-true (presentation-type-specifier-p '(widget gear)))
  (should-be-false (presentation-type-specifier-p 'not-a-presentation-type))
  (should-eq 'gear (presentation-type-parameter '(widget gear) 'kind))
  (should-equal "a widget" (describe-presentation-type 'widget nil))
  (should-be-true (presentation-typep 'gear '(widget gear)))
  (should-be-true (presentation-typep 'gear 'widget))
  (should-be-false (presentation-typep 'sprocket '(widget gear)))
  (should-be-false (presentation-typep 1 'widget))
  (should-be-true (presentation-subtypep 'widget t))
  (multiple-value-bind (yes known)
      (presentation-subtypep 'widget 'gadget)
    (should-be-false yes)
    (should-be-true known))
  (let ((names nil))
    (map-over-presentation-type-supertypes
     (lambda (specifier) (push (presentation-type-name specifier) names))
     'widget)
    (should-equal '(t widget) names))
  (should-be-true
   (handler-case
       (progn (presentation-typep 1 'not-a-presentation-type) nil)
     (error () t))))
