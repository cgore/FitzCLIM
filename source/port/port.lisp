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

(defpackage :FitzCLIM/port
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/sheet
        :FitzCLIM/geometry
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/frame
        :FitzCLIM/event
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present
        :FitzCLIM/presentation/translator
        :FitzCLIM/output-record
        :FitzCLIM/opengl
        :FitzCLIM/pane)
  (:import-from :FitzCLIM/presentation/translator
                :command-from-drag
                :find-drag-translators)
  (:export :*frame-manager-maker*
           :*mirror-silent*
           :adopt-frame
           :disable-frame
           :disown-frame
           :enable-frame
           :enqueue-event
           :forget-frame-port
           :forget-mirror
           :forget-mirror-frame
           :frame-manager
           :frame-manager-frames
           :frame-manager-port
           :frame-port
           :make-frame-manager
           :mirror-frame
           :mirror-root
           :mirror-sheet
           :port
           :port-next-event
           :process-next-event
           :read-port-frame-command
           :remember-mirror
           :tracking-pointer
           :sheet-in-root
           :sheet-mirror
           :sheet-pixel-size))
(in-package :FitzCLIM/port)

(defclass port ()
  ()
  (:documentation
   "A connection to one native window system.  A host backend subclasses
this.  The class itself has no windows."))

(defclass frame-manager ()
  ((port
    :initarg :port
    :reader frame-manager-port
    :documentation "The port that owns this manager's windows.")
   (frames
    :initform nil
    :accessor frame-manager-frames
    :documentation "The frames this manager is currently showing."))
  (:documentation
   "A frame manager realizes frames on a port.  A host backend
subclasses this and supplies the native windows."))

(defvar *frame-manager-maker* nil
  "A function of no arguments that returns this host's frame manager.
The host backend sets it when that backend loads.  NIL means this host
has no frame manager.  A later backend, such as Linux X11, registers
the same way.")

(defun make-frame-manager ()
  "Return a frame manager for this host."
  (unless *frame-manager-maker*
    (error "FitzCLIM has no frame manager for this host."))
  (funcall *frame-manager-maker*))

(behavior 'make-frame-manager
  (let ((*frame-manager-maker* nil))
    (handler-case
        (progn
          (make-frame-manager)
          (should-be-true nil))
      (error (condition)
        (should-equal "FitzCLIM has no frame manager for this host."
                      (princ-to-string condition)))))
  (let* ((port (make-instance 'port))
         (*frame-manager-maker*
           (lambda ()
             (make-instance 'frame-manager :port port))))
    (let ((manager (make-frame-manager)))
      (should-be-true (typep manager 'frame-manager))
      (should-eq port (frame-manager-port manager))
      (should-be-null (frame-manager-frames manager)))))

(defgeneric adopt-frame (frame-manager frame)
  (:documentation "Realize FRAME on FRAME-MANAGER."))

(defgeneric disown-frame (frame-manager frame)
  (:documentation "Destroy FRAME's native windows."))

(defgeneric enable-frame (frame-manager frame)
  (:documentation "Show FRAME."))

(defgeneric disable-frame (frame-manager frame)
  (:documentation "Hide FRAME."))

(defgeneric process-next-event (port &key wait-function timeout)
  (:documentation
   "Dispatch one queued native event for PORT and return true when one
was dispatched.  WAIT-FUNCTION and TIMEOUT are accepted.  A backend
that cannot wait ignores them."))

(defvar *sheet-mirrors* (make-hash-table :test #'eq)
  "A table from a sheet to its native mirror.
A mirror is a host integer, such as a window handle.")

(defvar *mirror-sheets* (make-hash-table :test #'eql)
  "A table from a native mirror integer to its sheet.")

(defvar *mirror-frames* (make-hash-table :test #'eql)
  "A table from a top-level mirror integer to its frame.")

(defvar *mirror-silent* nil
  "The sheet whose native control is already showing the Lisp value.
A notification from that control must not write the value back.")

(defvar *frame-ports* (make-hash-table :test #'eq)
  "A table from a frame to the port that adopted it.")

(defvar *port-events* (make-hash-table :test #'eq)
  "A table from a port to the CLIM events waiting to be read.")

(defun sheet-mirror (sheet)
  "Return the native mirror of SHEET, or NIL when it has none."
  (gethash sheet *sheet-mirrors*))

(defun mirror-sheet (mirror)
  "Return the sheet that owns MIRROR, or NIL."
  (gethash mirror *mirror-sheets*))

(defun remember-mirror (sheet mirror)
  "Record that SHEET is realized as MIRROR."
  (setf (gethash sheet *sheet-mirrors*) mirror)
  (setf (gethash mirror *mirror-sheets*) sheet)
  mirror)

(defun forget-mirror (sheet)
  "Drop SHEET's mirror.  The native window itself is left alone."
  (let ((mirror (sheet-mirror sheet)))
    (when mirror
      (remhash mirror *mirror-sheets*)
      (remhash sheet *sheet-mirrors*)))
  nil)

(behavior 'sheet-mirror
  (let ((sheet (make-instance 'basic-sheet)))
    (should-be-null (sheet-mirror sheet))
    (should-eq 11 (remember-mirror sheet 11))
    (should= 11 (sheet-mirror sheet))
    (should-eq sheet (mirror-sheet 11))
    (forget-mirror sheet)
    (should-be-null (sheet-mirror sheet))
    (should-be-null (mirror-sheet 11))))

(defun mirror-frame (mirror)
  "Return the frame realized as the top-level MIRROR, or NIL."
  (gethash mirror *mirror-frames*))

(defun (setf mirror-frame) (frame mirror)
  "Record that MIRROR is the top-level window of FRAME."
  (setf (gethash mirror *mirror-frames*) frame))

(defun forget-mirror-frame (mirror)
  "Drop the frame recorded for MIRROR."
  (remhash mirror *mirror-frames*))

(defun frame-port (frame)
  "Return the port that adopted FRAME, or NIL."
  (gethash frame *frame-ports*))

(defun (setf frame-port) (port frame)
  "Record that PORT adopted FRAME."
  (setf (gethash frame *frame-ports*) port))

(defun forget-frame-port (frame)
  "Drop the port recorded for FRAME."
  (remhash frame *frame-ports*))

(defun mirror-root (sheet)
  "Return the top-level sheet of SHEET when that sheet has a mirror."
  (loop for current = sheet then (sheet-parent current)
        while current
        do (when (and (null (sheet-parent current))
                      (sheet-mirror current))
             (return current))))

(defun sheet-in-root (sheet root)
  "Return the origin of SHEET in ROOT's coordinates."
  (let ((x 0)
        (y 0)
        (current sheet))
    (loop
      (when (or (null current) (eq current root))
        (return (values (round x) (round y))))
      (multiple-value-bind (next-x next-y)
          (transform-position (sheet-transformation current) x y)
        (setf x next-x
              y next-y
              current (sheet-parent current))))))

(defun sheet-pixel-size (sheet)
  "Return the width and height of SHEET's region, in pixels.
A zero edge becomes one pixel, so a native window can be created."
  (let ((region (sheet-region sheet)))
    (values (max 1 (round (bounding-rectangle-width region)))
            (max 1 (round (bounding-rectangle-height region))))))

(behavior 'sheet-in-root
  (let ((root (make-instance 'basic-sheet))
        (mid (make-instance 'basic-sheet))
        (child (make-instance 'basic-sheet)))
    (adopt-sheet root mid)
    (adopt-sheet mid child)
    (setf (sheet-transformation mid)
          (make-translation-transformation 10 4))
    (setf (sheet-transformation child)
          (make-translation-transformation 3 1))
    (multiple-value-bind (x y) (sheet-in-root child root)
      (should= 13 x)
      (should= 5 y))
    (multiple-value-bind (x y) (sheet-in-root root root)
      (should= 0 x)
      (should= 0 y))
    (should-be-null (mirror-root child))
    (remember-mirror root 21)
    (unwind-protect
         (should-eq root (mirror-root child))
      (forget-mirror root)))
  (let ((sheet (make-instance 'basic-sheet
                              :region (make-rectangle* 0 0 80 60))))
    (multiple-value-bind (width height) (sheet-pixel-size sheet)
      (should= 80 width)
      (should= 60 height)))
  (let ((sheet (make-instance 'basic-sheet
                              :region (make-rectangle* 0 0 1.6 0))))
    (multiple-value-bind (width height) (sheet-pixel-size sheet)
      (should= 2 width)
      (should= 1 height))))

(defun enqueue-event (sheet event)
  "Queue EVENT for the port that realized SHEET.
EVENT is returned.  A sheet with no realized root is not queued."
  (let* ((root (mirror-root sheet))
         (frame (and root (mirror-frame (sheet-mirror root))))
         (port (and frame (frame-port frame))))
    (when port
      (setf (gethash port *port-events*)
            (append (gethash port *port-events*) (list event))))
    event))

(defun port-next-event (port)
  "Return the next CLIM event queued for PORT, or NIL."
  (let ((queue (gethash port *port-events*)))
    (setf (gethash port *port-events*) (rest queue))
    (first queue)))

(behavior 'enqueue-event
  (let ((sheet (make-instance 'basic-sheet))
        (event (make-instance 'pointer-motion-event :x 0 :y 0)))
    (should-eq event (enqueue-event sheet event))
    (should-be-null (port-next-event (make-instance 'port))))
  (let* ((port (make-instance 'port))
         (frame (make-instance 'application-frame
                               :name 'queued
                               :pretty-name "Queued"
                               :command-table nil))
         (root (make-instance 'basic-sheet))
         (sheet (make-instance 'basic-sheet))
         (first (make-instance 'pointer-motion-event :sheet sheet :x 1 :y 2))
         (second (make-instance 'pointer-motion-event :sheet sheet :x 3 :y 4)))
    (adopt-sheet root sheet)
    (remember-mirror root 31)
    (setf (mirror-frame 31) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (should-eq first (enqueue-event sheet first))
           (enqueue-event sheet second)
           (should-eq first (port-next-event port))
           (should-eq second (port-next-event port))
           (should-be-null (port-next-event port)))
      (forget-mirror root)
      (forget-mirror-frame 31)
      (forget-frame-port frame))))

(defun presentation-under-pointer (event)
  "Return the presentation under a pointer press, or NIL.
An application pane is searched through its output history.  An OpenGL
pane is searched through its scene."
  (let ((sheet (event-sheet event))
        (x (pointer-event-x event))
        (y (pointer-event-y event)))
    (cond
      ((typep sheet 'application-pane)
       (presentation-at-position sheet x y))
      ((typep sheet 'opengl-pane)
       (opengl-presentation-at-position sheet x y)))))

(defun command-from-pointer-event (frame event)
  "Return the command a pointer press on FRAME produces, or NIL."
  (when (typep event 'pointer-button-press-event)
    (let ((presentation (presentation-under-pointer event))
          (sheet (event-sheet event)))
      (when presentation
        (command-from-presentation presentation event
                                   (frame-command-table frame)
                                   :frame frame
                                   :window sheet
                                   :x (pointer-event-x event)
                                   :y (pointer-event-y event))))))

(defclass pointer-drag ()
  ((presentation
    :initarg :presentation
    :reader pointer-drag-presentation
    :documentation "The presentation the pointer pressed.")
   (event
    :initarg :event
    :reader pointer-drag-event
    :documentation "The press that started the drag."))
  (:documentation "A drag that is waiting for its button to come up."))

(defvar *pointer-drags* (make-hash-table :test #'eq)
  "Drags keyed by the frame they started in.")

(defun unhighlight-pointer-drag (frame)
  "Remove the highlight from FRAME's armed drag, when it has one."
  (let ((drag (gethash frame *pointer-drags*)))
    (when drag
      (highlight-output-record (pointer-drag-presentation drag)
                               (event-sheet (pointer-drag-event drag))
                               :unhighlight))))

(defun command-from-port-event (frame event)
  "Return the command EVENT produces for FRAME, or NIL.
A press that names a command is returned at once and cancels a drag.
A press that names a drag translator arms that drag and highlights its
presentation.  A release of the same button clears that highlight.  On
another presentation it may return the drag's command.  Motion leaves
an armed drag in place.  A key press whose character is a menu
keystroke returns that command."
  (cond
    ((typep event 'pointer-button-press-event)
     (let ((command (command-from-pointer-event frame event)))
       (cond
         (command
          (unhighlight-pointer-drag frame)
          (remhash frame *pointer-drags*)
          command)
         (t
          (let ((presentation (presentation-under-pointer event)))
            (when (and presentation
                       (find-drag-translators
                        presentation (frame-command-table frame) event
                        :frame frame
                        :window (event-sheet event)
                        :x (pointer-event-x event)
                        :y (pointer-event-y event)))
              (unhighlight-pointer-drag frame)
              (highlight-output-record presentation (event-sheet event)
                                       :highlight)
              (setf (gethash frame *pointer-drags*)
                    (make-instance 'pointer-drag
                                   :presentation presentation
                                   :event event))))
          nil))))
    ((typep event 'pointer-button-release-event)
     (let ((drag (gethash frame *pointer-drags*)))
       (when (and drag
                  (eq (pointer-event-button event)
                      (pointer-event-button (pointer-drag-event drag))))
         (unhighlight-pointer-drag frame)
         (remhash frame *pointer-drags*)
         (let ((destination (presentation-under-pointer event))
               (source (pointer-drag-presentation drag)))
           (when (and destination (not (eq destination source)))
             (command-from-drag source destination
                                (pointer-drag-event drag)
                                (frame-command-table frame)
                                :frame frame
                                :window (event-sheet event)
                                :x (pointer-event-x event)
                                :y (pointer-event-y event)))))))
    ((and (typep event 'key-press-event)
          (keyboard-event-character event)
          (frame-command-table frame))
     (command-for-menu-keystroke (frame-command-table frame)
                                 (keyboard-event-character event)))
    (t nil)))

(defun read-port-frame-command (frame)
  "Return the next command a pointer gesture on FRAME produces, or NIL.
Queued native events are dispatched first.  A press that names a
command is returned at once.  A press that names a drag translator
arms the drag and highlights that presentation.  The matching release
clears the highlight and may return that command.  Motion and
unmatched events are skipped.  NIL means this frame's port queue is
empty, or FRAME has no port."
  (let ((port (frame-port frame)))
    (when port
      (loop repeat 32
            while (process-next-event port))
      (loop for event = (port-next-event port)
            while event
            for command = (command-from-port-event frame event)
            when command
              return command))))

(setf *frame-event-reader* #'read-port-frame-command)

(defclass event-probe-port (port)
  ((calls
    :initform 0
    :accessor event-probe-calls))
  (:documentation
   "A port that stops after counting one pump.  The tests of
READ-PORT-FRAME-COMMAND use it in place of a window system."))

(defmethod process-next-event ((port event-probe-port) &key wait-function timeout)
  "Count one pump and stop.  The test queues events itself."
  (declare (ignore wait-function timeout))
  (incf (event-probe-calls port))
  nil)

(defclass spinning-port (port)
  ((calls
    :initform 0
    :accessor spinning-port-calls))
  (:documentation
   "A port whose pump never runs dry.  It bounds the command reader."))

(defmethod process-next-event ((port spinning-port) &key wait-function timeout)
  "Count one pump and report that another event is waiting."
  (declare (ignore wait-function timeout))
  (incf (spinning-port-calls port))
  t)

(behavior 'read-port-frame-command
  (let ((frame (make-instance 'application-frame
                              :name 'unread
                              :pretty-name "Unread"
                              :command-table nil)))
    (should-be-null (read-port-frame-command frame))
    (should-eq #'read-port-frame-command *frame-event-reader*))
  (let* ((port (make-instance 'event-probe-port))
         (frame (make-instance 'application-frame
                               :name 'pumped
                               :pretty-name "Pumped"
                               :command-table nil))
         (pane (make-instance 'application-pane
                              :region (make-rectangle* 0 0 20 20)))
         (motion (make-instance 'pointer-motion-event
                                :sheet pane :x 1 :y 1))
         (press (make-instance 'pointer-button-press-event
                               :sheet pane :x 2 :y 3 :button :left)))
    (remember-mirror pane 41)
    (setf (mirror-frame 41) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (should-be-null (read-port-frame-command frame))
           (should= 1 (event-probe-calls port))
           (enqueue-event pane motion)
           (enqueue-event pane press)
           (should-be-null (read-port-frame-command frame))
           (should-be-null (port-next-event port))
           (should= 2 (event-probe-calls port)))
      (forget-mirror pane)
      (forget-mirror-frame 41)
      (forget-frame-port frame)))
  (let* ((port (make-instance 'spinning-port))
         (frame (make-instance 'application-frame
                               :name 'spinning
                               :pretty-name "Spinning"
                               :command-table nil)))
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (should-be-null (read-port-frame-command frame))
           (should= 32 (spinning-port-calls port)))
      (forget-frame-port frame))))

(define-command-table drag-tests)

(define-command (com-load-cargo :command-table drag-tests :name t)
    ((cargo string) (dock integer))
  "Return CARGO and DOCK."
  (list cargo dock))

(define-drag-and-drop-translator load-cargo
    (string integer com-load-cargo drag-tests :gesture :select)
    (cargo dock)
  (list cargo dock))

(define-command (com-note-dock :command-table drag-tests :name t)
    ((dock integer))
  "Return DOCK."
  dock)

(define-presentation-to-command-translator note-dock
    (integer com-note-dock drag-tests :gesture :select)
    (object)
  (list object))

(behavior 'drag-and-drop
  (let* ((port (make-instance 'event-probe-port))
         (frame (make-instance 'application-frame
                               :name 'drag
                               :pretty-name "Drag"
                               :command-table (find-command-table 'drag-tests)))
         (pane (make-instance 'application-pane))
         (table (frame-command-table frame)))
    (setf (stream-cursor-position pane) (values 0 0))
    (present "cargo" :stream pane :type 'string)
    (setf (stream-cursor-position pane) (values 50 0))
    (present 3 :stream pane :type 'integer)
    (setf (stream-cursor-position pane) (values 80 0))
    (present "other" :stream pane :type 'string)
    (remember-mirror pane 51)
    (setf (mirror-frame 51) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (let ((press (make-instance 'pointer-button-press-event
                                     :sheet pane :x 2 :y 0 :button :left))
               (cargo (presentation-at-position pane 2 0))
               (dock (presentation-at-position pane 52 0))
               (other (presentation-at-position pane 82 0)))
           (should-equal "cargo" (presentation-object cargo))
           (should= 3 (presentation-object dock))
           (should-be-null
            (find-applicable-translators cargo 'command table :event press))
           (enqueue-event pane press)
           (should-be-null (read-port-frame-command frame))
           (should-be-true (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-motion-event
                                              :sheet pane :x 40 :y 0))
           (should-be-null (read-port-frame-command frame))
           (should-be-true (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-release-event
                                              :sheet pane :x 52 :y 0
                                              :button :left))
           (should-equal '(com-load-cargo "cargo" 3)
                         (read-port-frame-command frame))
           (should-be-null (gethash frame *pointer-drags*))
           (should-be-false (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-true (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-release-event
                                              :sheet pane :x 200 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-null (gethash frame *pointer-drags*))
           (should-be-false (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (enqueue-event pane (make-instance 'pointer-button-release-event
                                              :sheet pane :x 4 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-false (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (enqueue-event pane (make-instance 'pointer-button-release-event
                                              :sheet pane :x 82 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-false (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 82 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-false (output-record-highlighted cargo))
           (should-be-true (output-record-highlighted other))
           (enqueue-event pane (make-instance 'pointer-button-release-event
                                              :sheet pane :x 200 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-false (output-record-highlighted other))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-be-null (read-port-frame-command frame))
           (should-be-true (output-record-highlighted cargo))
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 52 :y 0
                                              :button :left))
           (let ((release (make-instance 'pointer-button-release-event
                                         :sheet pane :x 52 :y 0
                                         :button :left)))
             (enqueue-event pane release)
             (should-equal '(com-note-dock 3)
                           (read-port-frame-command frame))
             (should-eq release (port-next-event port))
             (should-be-null (gethash frame *pointer-drags*))
             (should-be-false (output-record-highlighted dock))
             (should-be-false (output-record-highlighted cargo))))
      (remhash frame *pointer-drags*)
      (forget-mirror pane)
      (forget-mirror-frame 51)
      (forget-frame-port frame))))

(define-command-table keystroke-tests)

(define-command (com-keystroke-note :command-table keystroke-tests :name t)
    ()
  "A command reached from a menu keystroke."
  nil)

(behavior 'menu-keystroke
  (add-menu-item-to-command-table 'keystroke-tests "Note"
                                  :command 'com-keystroke-note
                                  :keystroke #\n
                                  :errorp nil)
  (let* ((port (make-instance 'event-probe-port))
         (frame (make-instance 'application-frame
                               :name 'keyed
                               :pretty-name "Keyed"
                               :command-table (find-command-table 'keystroke-tests)))
         (pane (make-instance 'application-pane)))
    (remember-mirror pane 71)
    (setf (mirror-frame 71) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (enqueue-event pane (make-instance 'key-press-event
                                              :sheet pane
                                              :character #\n))
           (should-eq 'com-keystroke-note (read-port-frame-command frame))
           (enqueue-event pane (make-instance 'key-press-event
                                              :sheet pane
                                              :character #\x))
           (should-be-null (read-port-frame-command frame))
           (enqueue-event pane (make-instance 'key-release-event
                                              :sheet pane
                                              :character #\n))
           (should-be-null (read-port-frame-command frame)))
      (forget-mirror pane)
      (forget-mirror-frame 71)
      (forget-frame-port frame))))

(defun sheet-port (sheet)
  "Return the port that realized SHEET, or NIL."
  (let* ((root (mirror-root sheet))
         (frame (and root (mirror-frame (sheet-mirror root)))))
    (and frame (frame-port frame))))

(defun tracked-sheet-event? (event sheet multiple-window)
  "Return true if EVENT should be seen while tracking SHEET.
MULTIPLE-WINDOW also accepts an event aimed at another sheet."
  (or multiple-window
      (let ((target (event-sheet event)))
        (or (eq target sheet)
            (and target (sheet-ancestor-p target sheet))))))

(defun tracking-clause-matches? (name event presentation)
  "Return true if the clause named NAME applies to EVENT."
  (case name
    (:pointer-motion
     (and (typep event 'pointer-motion-event)
          (not (typep event 'pointer-enter-event))
          (not (typep event 'pointer-exit-event))))
    (:pointer-enter (typep event 'pointer-enter-event))
    (:pointer-exit (typep event 'pointer-exit-event))
    (:pointer-button-press (typep event 'pointer-button-press-event))
    (:pointer-button-release (typep event 'pointer-button-release-event))
    (:keyboard (typep event 'keyboard-event))
    (:presentation
     (and presentation
          (typep event 'pointer-motion-event)
          (not (typep event 'pointer-enter-event))
          (not (typep event 'pointer-exit-event))))
    (:presentation-button-press
     (and presentation (typep event 'pointer-button-press-event)))
    (:presentation-button-release
     (and presentation (typep event 'pointer-button-release-event)))
    (t nil)))

(defun tracking-pointer-arguments (event presentation)
  "Return the keyword arguments a tracking clause receives."
  (list :event event
        :window (event-sheet event)
        :presentation presentation
        :x (when (typep event 'pointer-event) (pointer-event-x event))
        :y (when (typep event 'pointer-event) (pointer-event-y event))
        :button (when (typep event 'pointer-button-event)
                  (pointer-event-button event))
        :modifier-state (event-modifier-state event)
        :character (when (typep event 'keyboard-event)
                     (keyboard-event-character event))
        :key-name (when (typep event 'keyboard-event)
                    (keyboard-event-key-name event))))

(defmacro tracking-pointer ((sheet &key multiple-window &allow-other-keys)
                            &body clauses)
  "Read queued pointer events on SHEET and run the matching clauses.
An empty queue returns NIL.  The tracker does not wait for a new
event.  MULTIPLE-WINDOW, when true, also reads events aimed at other
sheets of the same port.  Other options are accepted and ignored.
RETURN from a clause exits the tracker.  Highlighting and pointer
documentation are not drawn."
  (labels ((clause-lambda-list (lambda-list)
             (if (or (member '&allow-other-keys lambda-list)
                     (member '&rest lambda-list))
                 lambda-list
                 (append lambda-list '(&allow-other-keys))))
           (expand-clause (clause event-var presentation-var)
             (unless (and (consp clause) (keywordp (first clause))
                          (consp (rest clause)))
               (error "A tracking-pointer clause must start with a keyword and a lambda list."))
             (destructuring-bind (name lambda-list &body body) clause
               `(when (tracking-clause-matches? ',name ,event-var ,presentation-var)
                  (apply (lambda ,(clause-lambda-list lambda-list)
                           ,@body)
                         (tracking-pointer-arguments ,event-var ,presentation-var))))))
    (let ((sheet-var (gensym "SHEET"))
          (port-var (gensym "PORT"))
          (event-var (gensym "EVENT"))
          (presentation-var (gensym "PRESENTATION"))
          (multiple-var (gensym "MULTIPLE")))
      `(let* ((,sheet-var ,sheet)
              (,multiple-var ,multiple-window)
              (,port-var (sheet-port ,sheet-var)))
         (block nil
           (unless ,port-var
             (return nil))
           (let ((,event-var nil))
             (tagbody
              next
                (setf ,event-var (port-next-event ,port-var))
                (unless ,event-var
                  (return nil))
                (when (tracked-sheet-event? ,event-var ,sheet-var ,multiple-var)
                  (let ((,presentation-var
                          (when (typep ,event-var 'pointer-event)
                            (presentation-under-pointer ,event-var))))
                    ,@(mapcar (lambda (clause)
                                (expand-clause clause event-var presentation-var))
                              clauses)))
                (go next))))))))

(behavior 'tracking-pointer
  (let ((sheet (make-instance 'basic-sheet)))
    (should-be-null
     (tracking-pointer (sheet)
       (:pointer-motion (&key x y)
         (declare (ignore x y))
         (return t)))))
  (let* ((port (make-instance 'event-probe-port))
         (frame (make-instance 'application-frame
                               :name 'tracked
                               :pretty-name "Tracked"
                               :command-table nil))
         (sheet (make-instance 'basic-sheet))
         (other (make-instance 'basic-sheet))
         (seen nil))
    (remember-mirror sheet 61)
    (setf (mirror-frame 61) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (enqueue-event sheet (make-instance 'pointer-motion-event
                                               :sheet sheet :x 3 :y 4))
           (enqueue-event sheet (make-instance 'pointer-button-press-event
                                               :sheet sheet :x 5 :y 6
                                               :button :left))
           (should-equal '(5 6)
                         (tracking-pointer (sheet)
                           (:pointer-motion (&key x y)
                             (push (list x y) seen))
                           (:pointer-button-press (&key x y)
                             (return (list x y)))))
           (should-equal '((3 4)) seen)
           (should-be-null (port-next-event port))
           (enqueue-event sheet (make-instance 'pointer-motion-event
                                               :sheet other :x 9 :y 8))
           (enqueue-event sheet (make-instance 'pointer-motion-event
                                               :sheet sheet :x 1 :y 2))
           (should-equal '(1 2)
                         (tracking-pointer (sheet)
                           (:pointer-motion (&key x y window)
                             (should-eq sheet window)
                             (return (list x y)))))
           (should-be-null
            (tracking-pointer (sheet)
              (:pointer-motion (&key x y)
                (declare (ignore x y))
                (return t))))
           (enqueue-event sheet (make-instance 'pointer-motion-event
                                               :sheet other :x 7 :y 8))
           (should-equal '(7 8)
                         (tracking-pointer (sheet :multiple-window t)
                           (:pointer-motion (&key x y)
                             (return (list x y)))))
           (enqueue-event sheet (make-instance 'key-press-event
                                               :sheet sheet
                                               :character #\a
                                               :key-name :a))
           (should-equal '(:a #\a)
                         (tracking-pointer (sheet)
                           (:keyboard (&key key-name character)
                             (return (list key-name character)))))
           (enqueue-event sheet (make-instance 'pointer-enter-event
                                               :sheet sheet :x 0 :y 1))
           (should-equal '(0 1)
                         (tracking-pointer (sheet)
                           (:pointer-enter (&key x y)
                             (return (list x y))))))
      (forget-mirror sheet)
      (forget-mirror-frame 61)
      (forget-frame-port frame)))
  (let* ((port (make-instance 'event-probe-port))
         (frame (make-instance 'application-frame
                               :name 'tracked-presentation
                               :pretty-name "Tracked Presentation"
                               :command-table nil))
         (pane (make-instance 'application-pane)))
    (setf (stream-cursor-position pane) (values 0 0))
    (present "cargo" :stream pane :type 'string)
    (remember-mirror pane 62)
    (setf (mirror-frame 62) frame)
    (setf (frame-port frame) port)
    (unwind-protect
         (progn
           (enqueue-event pane (make-instance 'pointer-button-press-event
                                              :sheet pane :x 2 :y 0
                                              :button :left))
           (should-equal "cargo"
                         (tracking-pointer (pane)
                           (:presentation-button-press (&key presentation)
                             (return (presentation-object presentation))))))
      (forget-mirror pane)
      (forget-mirror-frame 62)
      (forget-frame-port frame))))
