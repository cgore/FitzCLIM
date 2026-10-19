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

(defpackage :FitzCLIM/redisplay
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/output-record)
  (:export :redisplay
           :updating-output
           :updating-output-record))
(in-package :FitzCLIM/redisplay)

(defclass updating-output-record (standard-sequence-output-record)
  ((unique-id
    :initarg :unique-id
    :initform nil
    :reader updating-output-unique-id
    :documentation "The identity used to match this record, or NIL.")
   (cache-value
    :initform nil
    :accessor updating-output-cache-value
    :documentation "The value from the pass that produced this record.")
   (cache-test
    :initarg :cache-test
    :initform #'eql
    :reader updating-output-cache-test
    :documentation "The function that compares cache values.")
   (cursor-dx
    :initform 0
    :accessor updating-output-cursor-dx
    :documentation "How far the cursor's x moved while this was recorded.")
   (cursor-dy
    :initform 0
    :accessor updating-output-cursor-dy
    :documentation "How far the cursor's y moved while this was recorded.")
   (all-new
    :initarg :all-new
    :initform nil
    :reader updating-output-all-new
    :documentation "True when this record must be recomputed on every pass.")
   (consumed
    :initform nil
    :accessor updating-output-consumed-p
    :documentation "True when this redisplay has already matched the record."))
  (:documentation
   "Output that can be kept when its cache value is unchanged.
Children are in this record's coordinates.  The origin is the cursor
where the output began."))

(defvar *redisplay-p* nil
  "True while redisplay is running the output again.")

(defvar *redisplay-records* nil
  "Updating records saved from the previous pass, in tree order.")

(defun map-updating-records (function record)
  "Call FUNCTION on each updating record under RECORD, parent before child."
  (dolist (child (output-record-children record))
    (when (typep child 'updating-output-record)
      (funcall function child))
    (map-updating-records function child)))

(defun consume-updating-descendants (record)
  "Mark updating records inside RECORD as already matched."
  (map-updating-records
   (lambda (child) (setf (updating-output-consumed-p child) t))
   record))

(defun claim-updating-record (unique-id)
  "Return the next saved record for UNIQUE-ID, or the next anonymous one.
The record is marked consumed.  NIL when this is not a redisplay or
nothing matches."
  (unless *redisplay-p*
    (return-from claim-updating-record nil))
  (let ((found (find-if
                (lambda (record)
                  (and (not (updating-output-consumed-p record))
                       (not (updating-output-all-new record))
                       (if unique-id
                           (eql (updating-output-unique-id record) unique-id)
                           (null (updating-output-unique-id record)))))
                *redisplay-records*)))
    (when found
      (setf (updating-output-consumed-p found) t))
    found))

(defun place-updating-record (stream record)
  "Put RECORD at STREAM's cursor, in the current record's coordinates."
  (multiple-value-bind (origin-x origin-y)
      (output-record-sheet-origin (stream-current-output-record stream))
    (set-output-record-position record
                                (- (stream-cursor-x stream) origin-x)
                                (- (stream-cursor-y stream) origin-y))))

(defun reuse-updating-output (stream record cache-value)
  "Keep RECORD and move the cursor as the original output did."
  (place-updating-record stream record)
  (stream-add-output-record stream record)
  (setf (updating-output-cache-value record) cache-value)
  (consume-updating-descendants record)
  (setf (stream-cursor-position stream)
        (values (+ (stream-cursor-x stream) (updating-output-cursor-dx record))
                (+ (stream-cursor-y stream) (updating-output-cursor-dy record))))
  record)

(defun record-updating-output (stream continuation
                               &key cache-value cache-test unique-id all-new)
  "Record CONTINUATION in a new updating record and return that record."
  (let ((record (make-instance 'updating-output-record
                               :unique-id unique-id
                               :cache-test cache-test
                               :all-new all-new))
        (parent (stream-current-output-record stream))
        (start-x (stream-cursor-x stream))
        (start-y (stream-cursor-y stream)))
    (place-updating-record stream record)
    (setf (output-record-parent record) parent)
    (setf (stream-current-output-record stream) record)
    (unwind-protect
         (progn
           (funcall continuation)
           (setf (updating-output-cursor-dx record)
                 (- (stream-cursor-x stream) start-x))
           (setf (updating-output-cursor-dy record)
                 (- (stream-cursor-y stream) start-y))
           (setf (updating-output-cache-value record) cache-value))
      (setf (stream-current-output-record stream) parent))
    (add-output-record record parent)
    record))

(defun invoke-updating-output (stream continuation
                               &key unique-id (cache-value t)
                                 (cache-test #'eql) all-new)
  "Record CONTINUATION, or reuse the previous record when the cache matches.
ALL-NEW recomputes every time.  UNIQUE-ID matches a record from the
previous pass.  Without it, records match in the order they are made."
  (let ((existing (and (not all-new) (claim-updating-record unique-id))))
    (if (and existing
             (funcall cache-test
                      (updating-output-cache-value existing)
                      cache-value))
        (reuse-updating-output stream existing cache-value)
        (record-updating-output stream continuation
                                :cache-value cache-value
                                :cache-test cache-test
                                :unique-id unique-id
                                :all-new all-new))))

(defmacro updating-output
    ((stream &key unique-id (cache-value t) (cache-test '#'eql) all-new)
     &body body)
  "Record BODY so a later redisplay can keep it.
CACHE-VALUE is compared with CACHE-TEST, which defaults to EQL.
UNIQUE-ID matches the same output across passes.  ALL-NEW recomputes
BODY whenever redisplay runs."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-updating-output
        ,stream-var
        (lambda () ,@body)
        :unique-id ,unique-id
        :cache-value ,cache-value
        :cache-test ,cache-test
        :all-new ,all-new))))

(defun redisplay (stream generator)
  "Run GENERATOR again to rebuild STREAM's output history.
The cursor returns to the left margin first.  updating-output keeps a
record when its cache value is unchanged.  GENERATOR is a function of
no arguments and writes to STREAM."
  (let ((saved nil))
    (map-updating-records (lambda (record) (push record saved))
                          (stream-output-history stream))
    (dolist (record saved)
      (setf (updating-output-consumed-p record) nil))
    (clear-output-record (stream-output-history stream))
    (setf (stream-cursor-position stream)
          (values (stream-left-margin stream) 0))
    (let ((*redisplay-p* t)
          (*redisplay-records* (nreverse saved)))
      (funcall generator)))
  stream)

(defun recorded-text (record string)
  "Return the text record under RECORD whose string is STRING."
  (labels ((walk (node)
             (cond
               ((and (typep node 'text-displayed-output-record)
                     (equal (displayed-text node) string))
                node)
               (t (some #'walk (output-record-children node))))))
    (walk record)))

(behavior 'updating-output
  (let ((stream (make-instance 'output-recording-stream))
        (value "one")
        (inner 1)
        (inner-runs 0))
    (flet ((draw ()
             (write-string "L" stream)
             (updating-output (stream :cache-value value :unique-id 'word)
               (write-string value stream))))
      (draw)
      (let ((kept (recorded-text (stream-output-history stream) "one"))
            (label (recorded-text (stream-output-history stream) "L")))
        (should= (+ 36/5 108/5) (stream-cursor-x stream))
        (redisplay stream #'draw)
        (should-eq kept (recorded-text (stream-output-history stream) "one"))
        (should-be-false
         (eq label (recorded-text (stream-output-history stream) "L")))
        (should= (+ 36/5 108/5) (stream-cursor-x stream))
        (setf value "two")
        (redisplay stream #'draw)
        (should-equal "two"
                      (displayed-text
                       (recorded-text (stream-output-history stream) "two")))
        (should-be-null (recorded-text (stream-output-history stream) "one"))))
    (let ((outer 1))
      (flet ((draw-nested ()
               (updating-output (stream :cache-value outer :unique-id 'outer)
                 (updating-output (stream :cache-value inner :unique-id 'inner)
                   (incf inner-runs)
                   (write-string "X" stream)))))
        (draw-nested)
        (should= 1 inner-runs)
        (redisplay stream #'draw-nested)
        (should= 1 inner-runs)
        (setf outer 2)
        (redisplay stream #'draw-nested)
        (should= 1 inner-runs)
        (setf outer 3)
        (setf inner 2)
        (redisplay stream #'draw-nested)
        (should= 2 inner-runs)))))
