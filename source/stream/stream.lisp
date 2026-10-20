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

(defpackage :FitzCLIM/stream
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/input-editor)
  (:export :stream-read-gesture
           :stream-unread-gesture))
(in-package :FitzCLIM/stream)

(defun stream-read-gesture (stream &key (eof-error-p t) eof-value peek-p
                                     timeout input-wait-test
                                     input-wait-handler
                                     pointer-button-press-handler)
  "Return the next gesture from STREAM.
A character stream's gesture is a character.  PEEK-P true leaves the
gesture to be read again.  TIMEOUT, INPUT-WAIT-TEST,
INPUT-WAIT-HANDLER, and POINTER-BUTTON-PRESS-HANDLER are accepted and
ignored: this stream does not wait for a pointer."
  (declare (ignore timeout input-wait-test input-wait-handler
                   pointer-button-press-handler))
  (if peek-p
      (peek-char nil stream eof-error-p eof-value)
      (read-char stream eof-error-p eof-value)))

(defun stream-unread-gesture (stream gesture)
  "Put GESTURE back so the next read of STREAM returns it.  Return NIL."
  (unread-char gesture stream)
  nil)

(behavior 'stream-read-gesture
  (with-input-from-string (stream "ab")
    (should-eq #\a (stream-read-gesture stream))
    (should-be-null (stream-unread-gesture stream #\a))
    (should-eq #\a (stream-read-gesture stream :peek-p t))
    (should-eq #\a (stream-read-gesture stream))
    (should-eq #\b (stream-read-gesture stream))
    (should-be-null (stream-read-gesture stream :eof-error-p nil)))
  (let ((editor (make-instance 'input-editing-stream
                               :source (make-string-input-stream "xy"))))
    (should-eq #\x (stream-read-gesture editor))
    (stream-unread-gesture editor #\x)
    (should-eq #\x (stream-read-gesture editor :peek-p t))
    (should-eq #\x (stream-read-gesture editor))
    (should-eq #\y (stream-read-gesture editor))
    (should-be-null (stream-read-gesture editor :eof-error-p nil))))
