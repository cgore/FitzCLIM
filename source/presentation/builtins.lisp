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

(defpackage :FitzCLIM/presentation/builtins
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/frame
        :FitzCLIM/event
        :FitzCLIM/output-record
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present)
  (:import-from :FitzCLIM/presentation
                :expression
                :numeric-presentation-subtypep)
  (:import-from :FitzCLIM/presentation/present
                :*presentation-type-histories*
                :read-token
                :whitespace?))
(in-package :FitzCLIM/presentation/builtins)

(defun parse-failure (control &rest arguments)
  "Signal a simple parse error from CONTROL and ARGUMENTS."
  (error 'simple-parse-error
         :format-control control
         :format-arguments arguments))

(defun function-for (designator)
  "Return the function DESIGNATOR names."
  (etypecase designator
    (function designator)
    (symbol (fdefinition designator))))

(defun token-matches? (token object)
  "Return true if TOKEN is a printed form of OBJECT, ignoring case."
  (string-equal token (princ-to-string object)))

(defun find-command-by-line-name (line command-table)
  "Return the command in COMMAND-TABLE whose line name matches LINE.
The match ignores case.  Inherited commands are included."
  (let ((found nil))
    (map-over-command-table-commands
     (lambda (name command)
       (declare (ignore name))
       (when (and (command-line-name command)
                  (string-equal line (command-line-name command)))
         (setf found command)))
     command-table
     :inherited t)
    found))

(defun command-line-names (command-table)
  "Return the command-line names in COMMAND-TABLE, including inherited ones."
  (let ((names nil))
    (map-over-command-table-commands
     (lambda (name command)
       (declare (ignore name))
       (when (command-line-name command)
         (push (command-line-name command) names)))
     command-table
     :inherited t)
    names))

(defun command-phrase-extends? (phrase names)
  "Return true if some name is PHRASE followed by more words."
  (some (lambda (name)
          (and (> (length name) (length phrase))
               (string-equal name phrase :end1 (length phrase))
               (char= (char name (length phrase)) #\Space)))
        names))

(defun read-command-from-stream (stream command-table)
  "Read a command name from STREAM, allowing a name of several words.
The longest name that matches the tokens is chosen."
  (let ((names (command-line-names command-table))
        (words nil))
    (loop
      (push (read-token stream) words)
      (let* ((phrase (format nil "~{~A~^ ~}" (reverse words)))
             (exact (find-if (lambda (name) (string-equal name phrase)) names)))
        (when (and exact (not (command-phrase-extends? phrase names)))
          (return (find-command-by-line-name exact command-table)))
        (unless (or exact (command-phrase-extends? phrase names))
          (parse-failure "~S is not a command." phrase))))))

(define-presentation-type symbol
  :options ((package nil))
  :inherit-from t
  :description "a symbol")

(define-presentation-method presentation-typep (object (type symbol))
  (symbolp object))

(define-presentation-method present (object (type symbol) stream view &key)
  (prin1 object stream))

(define-presentation-method accept ((type symbol) stream view &key)
  (let ((object (handler-case (read-from-string (read-token stream))
                  (error ()
                    (parse-failure "The input is not a symbol.")))))
    (unless (symbolp object)
      (parse-failure "~S is not a symbol." object))
    (when package
      (setf object (intern (symbol-name object) package)))
    (values object type)))

(define-presentation-type boolean
  :inherit-from 'symbol
  :description "a boolean")

(define-presentation-method presentation-typep (object (type boolean))
  (or (eq object t) (eq object nil)))

(define-presentation-method present (object (type boolean) stream view &key)
  (write-string (if object "Yes" "No") stream))

(define-presentation-method accept ((type boolean) stream view &key)
  (let ((token (read-token stream)))
    (cond
      ((member token '("yes" "true" "t") :test #'string-equal)
       (values t type))
      ((member token '("no" "false" "nil") :test #'string-equal)
       (values nil type))
      (t (parse-failure "~S is not a boolean." token)))))

(define-presentation-type keyword
  :inherit-from 'symbol
  :description "a keyword")

(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))

(define-presentation-method present (object (type keyword) stream view &key)
  (prin1 object stream))

(define-presentation-method accept ((type keyword) stream view &key)
  (let ((token (read-token stream)))
    (when (and (plusp (length token)) (char= (char token 0) #\:))
      (setf token (subseq token 1)))
    (when (zerop (length token))
      (parse-failure "The input is not a keyword."))
    (values (intern (string-upcase token) :keyword) type)))

(define-presentation-type null
  :inherit-from 'symbol
  :description "nil")

(define-presentation-method presentation-typep (object (type null))
  (null object))

(define-presentation-method presentation-subtypep ((type null) putative)
  (if (member (presentation-type-name putative) '(null boolean symbol t))
      (values t t)
      (values nil nil)))

(define-presentation-method present (object (type null) stream view &key)
  (write-string "nil" stream))

(define-presentation-method accept ((type null) stream view &key)
  (let ((token (read-token stream)))
    (unless (member token '("nil" "()") :test #'string-equal)
      (parse-failure "~S is not nil." token))
    (values nil type)))

(define-presentation-type number
  :inherit-from t
  :description "a number")

(define-presentation-method presentation-typep (object (type number))
  (numberp object))

(define-presentation-type (real &optional low high)
  :inherit-from 'number
  :description "a real number")

(define-presentation-method presentation-typep (object (type real))
  (and (realp object)
       (or (null low) (<= low object))
       (or (null high) (<= object high))))

(define-presentation-method presentation-subtypep ((type real) putative)
  (numeric-presentation-subtypep type putative 'real))

(define-presentation-type (rational &optional low high)
  :inherit-from (list 'real low high)
  :description "a rational")

(define-presentation-method presentation-typep (object (type rational))
  (and (rationalp object)
       (or (null low) (<= low object))
       (or (null high) (<= object high))))

(define-presentation-method presentation-subtypep ((type rational) putative)
  (numeric-presentation-subtypep type putative 'rational))

(define-presentation-type (integer &optional low high)
  :options ((base 10) (radix nil))
  :inherit-from (list 'rational low high)
  :description "an integer")

(define-presentation-method presentation-typep (object (type integer))
  (and (integerp object)
       (or (null low) (<= low object))
       (or (null high) (<= object high))))

(define-presentation-method presentation-subtypep ((type integer) putative)
  (numeric-presentation-subtypep type putative 'integer))

(define-presentation-method describe-presentation-type
    ((type integer) stream plural-count)
  (write-string (if (or (null plural-count) (eql plural-count 1))
                    "an integer"
                    "integers")
                stream))

(define-presentation-method present (object (type integer) stream view &key)
  (let ((*print-base* base)
        (*print-radix* (not (null radix))))
    (prin1 object stream)))

(define-presentation-method accept ((type integer) stream view &key)
  (let ((value (handler-case (parse-integer (read-token stream) :radix base)
                 (error ()
                   (parse-failure "The input is not an integer.")))))
    (unless (presentation-typep value type)
      (parse-failure "~A is not ~A."
                     value
                     (describe-presentation-type type nil)))
    (values value type)))

(define-presentation-type (float &optional low high)
  :inherit-from (list 'real low high)
  :description "a float")

(define-presentation-method presentation-typep (object (type float))
  (and (floatp object)
       (or (null low) (<= low object))
       (or (null high) (<= object high))))

(define-presentation-method presentation-subtypep ((type float) putative)
  (numeric-presentation-subtypep type putative 'float))

(define-presentation-method present (object (type real) stream view &key)
  (prin1 object stream))

(define-presentation-method accept ((type real) stream view &key)
  (let ((value (handler-case (read-from-string (read-token stream))
                 (error ()
                   (parse-failure "The input is not ~A."
                                  (describe-presentation-type type nil))))))
    (unless (presentation-typep value type)
      (parse-failure "~A is not ~A."
                     value
                     (describe-presentation-type type nil)))
    (values value type)))

(define-presentation-type character
  :inherit-from t
  :description "a character")

(define-presentation-method presentation-typep (object (type character))
  (characterp object))

(define-presentation-method present (object (type character) stream view &key)
  (prin1 object stream))

(define-presentation-method accept ((type character) stream view &key)
  (let* ((token (read-token stream))
         (value (cond
                  ((and (plusp (length token)) (char= (char token 0) #\#))
                   (read-from-string token))
                  ((= (length token) 1) (char token 0))
                  (t (parse-failure "~S is not a character." token)))))
    (unless (characterp value)
      (parse-failure "~S is not a character." token))
    (values value type)))

(define-presentation-type string
  :inherit-from t
  :description "a string")

(define-presentation-method presentation-typep (object (type string))
  (stringp object))

(define-presentation-method present (object (type string) stream view &key)
  (if (or (zerop (length object))
          (find-if #'whitespace? object))
      (prin1 object stream)
      (write-string object stream)))

(define-presentation-method accept ((type string) stream view &key)
  (values (read-token stream) type))

(define-presentation-type pathname
  :inherit-from t
  :description "a pathname")

(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(define-presentation-method present (object (type pathname) stream view &key)
  (write-string (namestring object) stream))

(define-presentation-method accept ((type pathname) stream view &key)
  (values (pathname (read-token stream)) type))

(define-presentation-type expression
  :inherit-from t
  :history t
  :description "an expression")

(define-presentation-method presentation-typep (object (type expression))
  (progn object t))

(define-presentation-method present (object (type expression) stream view &key)
  (prin1 object stream))

(define-presentation-method accept ((type expression) stream view &key)
  (values (handler-case (read stream)
            (error ()
              (parse-failure "The input is not an expression.")))
          type))

(define-presentation-type form
  :inherit-from 'expression
  :description "a form")

(define-presentation-type (eql value)
  :inherit-from t
  :description "one value")

(define-presentation-method presentation-typep (object (type eql))
  (eql object value))

(define-presentation-method accept ((type eql) stream view &key)
  (let ((token (read-token stream)))
    (unless (token-matches? token value)
      (parse-failure "~S is not ~S." token value))
    (values value type)))

(define-presentation-type (member &rest elements)
  :inherit-from t
  :description "one of a set")

(define-presentation-method presentation-typep (object (type member))
  (and (not (null elements))
       (member object elements)))

(define-presentation-method presentation-subtypep ((type member) putative)
  (if (eq (presentation-type-name putative) 'member)
      (values (subsetp elements
                       (presentation-type-parameter putative 'elements)
                       :test #'equal)
              t)
      (values nil nil)))

(define-presentation-method present (object (type member) stream view &key)
  (princ object stream))

(define-presentation-method accept ((type member) stream view &key)
  (let* ((token (read-token stream))
         (found (find token elements :test #'token-matches?)))
    (unless found
      (parse-failure "~S is not one of ~S." token elements))
    (values found type)))

(define-presentation-type (completion sequence)
  :options ((test #'equalp) (value-key #'identity))
  :inherit-from t
  :description "a completion")

(define-presentation-method presentation-typep (object (type completion))
  (not (null (position object sequence
                       :test (function-for test)
                       :key (function-for value-key)))))

(define-presentation-method present (object (type completion) stream view &key)
  (princ (funcall (function-for value-key) object) stream))

(define-presentation-method accept ((type completion) stream view &key)
  (let* ((token (read-token stream))
         (index (position token sequence
                          :test (lambda (text element)
                                  (funcall (function-for test)
                                           text
                                           (princ-to-string
                                            (funcall (function-for value-key)
                                                     element)))))))
    (unless index
      (parse-failure "~S is not a completion of ~S." token sequence))
    (values (elt sequence index) type)))

(define-presentation-type (sequence element-type)
  :inherit-from t
  :description "a sequence")

(define-presentation-method presentation-typep (object (type sequence))
  (and (typep object 'sequence)
       (every (lambda (element)
                (presentation-typep element element-type))
              object)))

(define-presentation-method present (object (type sequence) stream view &key)
  (let ((first t))
    (map nil
         (lambda (element)
           (if first
               (setf first nil)
               (write-char #\Space stream))
           (present element :stream stream :type element-type :view view))
         object)))

(define-presentation-method accept ((type sequence) stream view &key)
  (values (loop while (peek-char t stream nil nil)
                collect (accept element-type :stream stream :view view))
          type))

(define-presentation-type (subset sequence)
  :options ((test #'equalp))
  :inherit-from t
  :description "a subset")

(define-presentation-method presentation-typep (object (type subset))
  (and (listp object)
       (every (lambda (element)
                (find element sequence :test (function-for test)))
              object)))

(define-presentation-method accept ((type subset) stream view &key)
  (values (loop while (peek-char t stream nil nil)
                collect (accept `(completion ,sequence :test ,test)
                                :stream stream
                                :view view))
          type))

(define-presentation-type (or &rest types)
  :inherit-from t
  :description "one of several types")

(define-presentation-method presentation-typep (object (type or))
  (some (lambda (one) (presentation-typep object one)) types))

(define-presentation-method accept ((type or) stream view &key)
  (block found
    (let ((start (file-position stream)))
      (dolist (one types)
        (handler-case
            (return-from found (accept one :stream stream :view view))
          (error ()
            (when start
              (file-position stream start)))))
      (parse-failure "The input is not ~A."
                     (describe-presentation-type type nil)))))

(define-presentation-type (and &rest types)
  :inherit-from t
  :description "several types at once")

(define-presentation-method presentation-typep (object (type and))
  (every (lambda (one) (presentation-typep object one)) types))

(define-presentation-method accept ((type and) stream view &key)
  (unless types
    (parse-failure "AND needs at least one presentation type."))
  (multiple-value-bind (object object-type)
      (accept (first types) :stream stream :view view)
    (dolist (one (rest types))
      (unless (presentation-typep object one)
        (parse-failure "~S is not ~A."
                       object
                       (describe-presentation-type one nil))))
    (values object (or object-type type))))

(define-presentation-type blank-area
  :inherit-from nil
  :description "blank area")

(define-presentation-method presentation-typep (object (type blank-area))
  (null object))

(define-presentation-type (command &key (command-table nil))
  :inherit-from t
  :description "a command")

(define-presentation-method presentation-typep (object (type command))
  (and (consp object)
       (symbolp (first object))
       (or (null command-table)
           (find-command (first object) command-table :errorp nil))))

(define-presentation-method presentation-subtypep ((type command) putative)
  (cond
    ((not (eq (presentation-type-name putative) 'command))
     (values nil nil))
    ((null (presentation-type-parameter putative 'command-table))
     (values t t))
    ((eql command-table (presentation-type-parameter putative 'command-table))
     (values t t))
    (t (values nil t))))

(define-presentation-method present (object (type command) stream view &key)
  (let* ((table (or command-table *default-command-table*))
         (command (find-command (first object) table)))
    (write-string (command-line-name command) stream)
    (loop for argument in (rest object)
          for (nil argument-type) in (command-arguments command)
          do (write-char #\Space stream)
             (present argument :stream stream :type argument-type :view view))))

(define-presentation-method accept ((type command) stream view &key)
  (let* ((table (or command-table *default-command-table*))
         (command (read-command-from-stream stream table)))
    (values (cons (command-name command)
                  (loop for (nil argument-type) in (command-arguments command)
                        collect (accept argument-type :stream stream :view view)))
            type)))

(define-presentation-type (command-name &key (command-table nil))
  :inherit-from 'symbol
  :description "a command name")

(define-presentation-method presentation-typep (object (type command-name))
  (and (symbolp object)
       (let ((table (or command-table *default-command-table*)))
         (and table
              (find-command object table :errorp nil)))))

(define-presentation-method accept ((type command-name) stream view &key)
  (let* ((table (or command-table *default-command-table*))
         (command (read-command-from-stream stream table)))
    (values (command-name command) type)))

(define-command-table presentation-command-tests)

(define-command (com-add-numbers :command-table presentation-command-tests
                                 :name "Add")
    ((left integer) (right integer))
  "Return the sum of LEFT and RIGHT."
  (+ left right))

(setf *command-argument-typep*
      (lambda (object type)
        (if (presentation-type-specifier-p type)
            (presentation-typep object type)
            (typep object type))))

(behavior 'builtins
  (should= 42 (accept-from-string 'integer "42"))
  (should= 16 (accept-from-string '(integer :base 16) "10"))
  (should-equal "42" (present-to-string 42 :type 'integer))
  (should-equal "10" (present-to-string 16 :type '(integer :base 16)))
  (should-be-true (presentation-typep 5 '(integer 0 10)))
  (should-be-false (presentation-typep 11 '(integer 0 10)))
  (should-be-true (presentation-subtypep '(integer 0 10) 'integer))
  (should-be-false (presentation-subtypep 'integer '(integer 0 10)))
  (should-be-true (presentation-subtypep '(integer 0 10) '(rational 0 10)))
  (should-be-false (presentation-subtypep '(integer 0 10) '(rational 0 5)))
  (should-be-true (presentation-subtypep 'integer 'number))
  (should-equal "an integer" (describe-presentation-type 'integer nil))
  (should-equal "integers" (describe-presentation-type 'integer nil 2))
  (should-be-true
   (handler-case
       (progn (accept-from-string '(integer 0 1) "5") nil)
     (error () t)))
  (should-equal "hello" (accept-from-string 'string "hello"))
  (should-equal "hello world" (accept-from-string 'string "\"hello world\""))
  (should-equal "\"hello world\"" (present-to-string "hello world" :type 'string))
  (should-eq t (accept-from-string 'boolean "yes"))
  (should-eq nil (accept-from-string 'boolean "No"))
  (should-equal "Yes" (present-to-string t :type 'boolean))
  (should-equal "No" (present-to-string nil :type 'boolean))
  (should-be-true (presentation-typep nil 'null))
  (should-be-true (presentation-typep nil 'boolean))
  (should-be-false (presentation-typep t 'null))
  (should-be-true (presentation-subtypep 'null 'boolean))
  (should-eq :foo (accept-from-string 'keyword "foo"))
  (should-eq :red (accept-from-string '(member :red :blue) "red"))
  (should-equal "green"
                (accept-from-string '(completion ("red" "green")) "GREEN"))
  (should-equal '(1 2 3) (accept-from-string '(sequence integer) "1 2 3"))
  (should-equal '(2 1) (accept-from-string '(subset (1 2 3)) "2 1"))
  (should= 4 (accept-from-string '(or integer string) "4"))
  (should-equal "nope" (accept-from-string '(or integer string) "nope"))
  (should-be-true (presentation-typep 3 '(and integer (integer 0 10))))
  (should-be-false (presentation-typep 30 '(and integer (integer 0 10))))
  (should-equal '(a b) (accept-from-string 'expression "(a b)"))
  (should-equal '(a) (accept-from-string 'form "(a)"))
  (should-eq #\Z (accept-from-string 'character "Z"))
  (should-equal "foo" (pathname-name (accept-from-string 'pathname "foo.txt")))
  (let ((table 'presentation-command-tests))
    (let ((command (accept-from-string `(command :command-table ,table) "Add 2 3")))
      (should-equal "Add"
                    (command-line-name (find-command (first command) table)))
      (should-equal '(2 3) (rest command))
      (should= 5 (apply (command-function (find-command (first command) table))
                        (rest command)))
      (should-equal "Add 2 3"
                    (present-to-string command
                                       :type `(command :command-table ,table)))))
  (setf (gethash 'expression *presentation-type-histories*) nil)
  (accept-from-string 'expression "(a b)")
  (should-equal '((a b)) (presentation-type-history 'expression)))
