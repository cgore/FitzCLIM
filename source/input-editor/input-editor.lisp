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

(defpackage :FitzCLIM/input-editor
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present)
  (:export :input-editing-stream
           :invoke-with-input-editing
           :with-input-editing))
(in-package :FitzCLIM/input-editor)

(defclass input-editing-stream (sb-gray:fundamental-character-input-stream)
  ((source
    :initarg :source
    :reader input-editor-source
    :documentation "The stream gestures come from.")
   (buffer
    :initform (make-array 16 :element-type 'character
                            :fill-pointer 0 :adjustable t)
    :reader input-editor-buffer
    :documentation "The characters the parser is scanning.")
   (scan
    :initform 0
    :accessor input-editor-scan
    :documentation "The index of the next character the parser reads."))
  (:documentation
   "A character stream that rescans its buffer after a rubout.
A newline or a return ends the line.  Backspace and rubout delete the
last buffered character and the parser starts again."))

(defun rubout-character? (character)
  "Return true if CHARACTER deletes the last buffered character."
  (member character '(#\Backspace #\Rubout)))

(defun activation-character? (character)
  "Return true if CHARACTER ends the edited line."
  (member character '(#\Newline #\Return)))

(defun take-editor-gesture (stream)
  "Return the next character from STREAM's buffer or its source.
:EOF means the line or the source has ended.  A rubout shortens the
buffer and throws to INPUT-EDITOR-RESCAN."
  (let ((buffer (input-editor-buffer stream)))
    (loop
      (when (< (input-editor-scan stream) (length buffer))
        (let ((character (char buffer (input-editor-scan stream))))
          (incf (input-editor-scan stream))
          (return character)))
      (let ((gesture (read-char (input-editor-source stream) nil :eof)))
        (cond
          ((eq gesture :eof)
           (return :eof))
          ((rubout-character? gesture)
           (when (plusp (length buffer))
             (decf (fill-pointer buffer))
             (throw 'input-editor-rescan nil)))
          ((activation-character? gesture)
           (return :eof))
          (t
           (vector-push-extend gesture buffer)
           (incf (input-editor-scan stream))
           (return gesture)))))))

(defmethod sb-gray:stream-read-char ((stream input-editing-stream))
  (take-editor-gesture stream))

(defmethod sb-gray:stream-unread-char ((stream input-editing-stream) character)
  (declare (ignore character))
  (when (zerop (input-editor-scan stream))
    (error "Nothing to unread."))
  (decf (input-editor-scan stream)))

(defmethod sb-gray:stream-peek-char ((stream input-editing-stream))
  (let ((character (take-editor-gesture stream)))
    (unless (eq character :eof)
      (decf (input-editor-scan stream)))
    character))

(defun invoke-with-input-editing (stream continuation)
  "Call CONTINUATION with an input-editing stream wrapped around STREAM.
CONTINUATION is a function of one stream.  An input-editing STREAM is
used as it is.  A rubout restarts CONTINUATION from the buffer."
  (if (typep stream 'input-editing-stream)
      (funcall continuation stream)
      (let ((editor (make-instance 'input-editing-stream :source stream)))
        (loop
          (catch 'input-editor-rescan
            (setf (input-editor-scan editor) 0)
            (return (funcall continuation editor)))))))

(defmacro with-input-editing ((stream) &body body)
  "Bind STREAM to an input-editing stream and run BODY.
STREAM is a variable whose current value is the source.  A rubout
restarts BODY."
  (let ((editor (gensym "EDITOR")))
    `(invoke-with-input-editing ,stream
       (lambda (,editor)
         (let ((,stream ,editor))
           ,@body)))))

(setf *accept-input-editor* #'invoke-with-input-editing)

(behavior 'input-editing
  (with-input-from-string (stream "42")
    (should= 42 (accept 'integer :stream stream)))
  (with-input-from-string (stream (format nil "4~C2" #\Backspace))
    (should= 2 (accept 'integer :stream stream)))
  (with-input-from-string (stream (format nil "123~C~C9" #\Backspace #\Backspace))
    (should= 19 (accept 'integer :stream stream)))
  (with-input-from-string (stream (format nil "~C12" #\Backspace))
    (should= 12 (accept 'integer :stream stream)))
  (with-input-from-string (stream (format nil "42~%99"))
    (should= 42 (accept 'integer :stream stream))
    (should= 99 (accept 'integer :stream stream)))
  (with-input-from-string (stream "9 8")
    (should= 9 (accept 'integer :stream stream))
    (should= 8 (accept 'integer :stream stream)))
  (with-input-from-string (stream "\"Ada Lovelace\"")
    (should-equal "Ada Lovelace" (accept 'string :stream stream)))
  (let ((source (make-string-input-stream "Ada")))
    (with-input-editing (source)
      (should-equal "Ada" (accept 'string :stream source))
      (should-be-true (typep source 'input-editing-stream)))))
