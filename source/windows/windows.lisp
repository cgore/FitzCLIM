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

(defpackage :FitzCLIM/windows
  (:use :common-lisp
        :sigma/behave
        :sb-alien
        :FitzCLIM/frame
        :FitzCLIM/pane
        :FitzCLIM/gadget
        :FitzCLIM/sheet
        :FitzCLIM/geometry
        :FitzCLIM/layout)
  (:export :adopt-frame
           :disable-frame
           :disown-frame
           :enable-frame
           :frame-manager-frames
           :frame-manager-port
           :make-windows-frame-manager
           :process-next-event
           :sheet-mirror
           :windows-frame-manager
           :windows-port))
(in-package :FitzCLIM/windows)

(defvar *windows-libraries-loaded* nil
  "True after user32.dll and kernel32.dll have been loaded.")

(defun load-windows-libraries ()
  "Load the Windows libraries this port calls."
  (unless *windows-libraries-loaded*
    (load-shared-object "kernel32.dll")
    (load-shared-object "user32.dll")
    (setf *windows-libraries-loaded* t)))

(load-windows-libraries)

(defparameter +ws-popup+ #x80000000)
(defparameter +ws-child+ #x40000000)
(defparameter +ws-visible+ #x10000000)
(defparameter +ws-disabled+ #x08000000)
(defparameter +ws-clipsiblings+ #x04000000)
(defparameter +ws-clipchildren+ #x02000000)
(defparameter +ws-group+ #x00020000)
(defparameter +ws-tabstop+ #x00010000)
(defparameter +ws-ex-toolwindow+ #x00000080)
(defparameter +ws-ex-noactivate+ #x08000000)
(defparameter +sw-hide+ 0)
(defparameter +sw-showna+ 8)
(defparameter +wm-paint+ #x000F)
(defparameter +wm-command+ #x0111)
(defparameter +wm-hscroll+ #x0114)
(defparameter +wm-vscroll+ #x0115)
(defparameter +bm-getcheck+ #x00F0)
(defparameter +bm-setcheck+ #x00F1)
(defparameter +bm-click+ #x00F5)
(defparameter +bst-checked+ 1)
(defparameter +bn-clicked+ 0)
(defparameter +en-change+ #x0300)
(defparameter +sbm-setpos+ #x00E0)
(defparameter +sbm-getpos+ #x00E1)
(defparameter +sbm-setrange+ #x00E2)
(defparameter +sb-thumbposition+ 4)
(defparameter +sb-thumbtrack+ 5)
(defparameter +lb-addstring+ #x0180)
(defparameter +lb-resetcontent+ #x0184)
(defparameter +lb-setsel+ #x0185)
(defparameter +lb-setcursel+ #x0186)
(defparameter +lb-getsel+ #x0187)
(defparameter +lb-getcursel+ #x0188)
(defparameter +lbn-selchange+ 1)
(defparameter +cb-addstring+ #x0143)
(defparameter +cb-resetcontent+ #x014B)
(defparameter +cb-setcursel+ #x014E)
(defparameter +cb-getcursel+ #x0147)
(defparameter +cbn-selchange+ 1)
(defparameter +pm-remove+ 1)
(defparameter +error-class-already-exists+ 1410)
(defparameter +scroll-extent+ 1000
  "The native scrollbar range.  A scrollbar position is a 16-bit integer,
so a Lisp range is scaled onto 0 through 1000.")

(define-alien-type wndclassexw
  (struct wndclassexw
    (cb-size (unsigned 32))
    (style (unsigned 32))
    (proc system-area-pointer)
    (cls-extra (signed 32))
    (wnd-extra (signed 32))
    (instance system-area-pointer)
    (icon system-area-pointer)
    (cursor system-area-pointer)
    (background system-area-pointer)
    (menu-name system-area-pointer)
    (class-name system-area-pointer)
    (icon-sm system-area-pointer)))

(define-alien-type win-rect
  (struct win-rect
    (left (signed 32))
    (top (signed 32))
    (right (signed 32))
    (bottom (signed 32))))

(define-alien-routine ("GetLastError" get-last-error) (unsigned 32))
(define-alien-routine ("GetModuleHandleW" get-module-handle-w) system-area-pointer
  (name system-area-pointer))
(define-alien-routine ("RegisterClassExW" register-class-ex-w) (unsigned 16)
  (class (* wndclassexw)))
(define-alien-routine ("CreateWindowExW" create-window-ex-w) system-area-pointer
  (ex-style (unsigned 32))
  (class-name system-area-pointer)
  (window-name system-area-pointer)
  (style (unsigned 32))
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32))
  (parent system-area-pointer)
  (menu system-area-pointer)
  (instance system-area-pointer)
  (param system-area-pointer))
(define-alien-routine ("DestroyWindow" destroy-window) (signed 32)
  (hwnd system-area-pointer))
(define-alien-routine ("DefWindowProcW" def-window-proc-w) (signed 64)
  (hwnd system-area-pointer)
  (message (unsigned 32))
  (wparam (unsigned 64))
  (lparam (signed 64)))
(define-alien-routine ("SendMessageW" send-message-w) (signed 64)
  (hwnd system-area-pointer)
  (message (unsigned 32))
  (wparam (signed 64))
  (lparam (signed 64)))
(define-alien-routine ("MoveWindow" move-window) (signed 32)
  (hwnd system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32))
  (repaint (signed 32)))
(define-alien-routine ("SetWindowPos" set-window-pos) (signed 32)
  (hwnd system-area-pointer)
  (insert-after system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32))
  (flags (unsigned 32)))
(define-alien-routine ("SetWindowTextW" set-window-text-w) (signed 32)
  (hwnd system-area-pointer)
  (text system-area-pointer))
(define-alien-routine ("GetWindowTextW" get-window-text-w) (signed 32)
  (hwnd system-area-pointer)
  (buffer system-area-pointer)
  (max-count (signed 32)))
(define-alien-routine ("GetWindowTextLengthW" get-window-text-length-w) (signed 32)
  (hwnd system-area-pointer))
(define-alien-routine ("ShowWindow" show-window) (signed 32)
  (hwnd system-area-pointer)
  (command (signed 32)))
(define-alien-routine ("IsWindow" is-window) (signed 32)
  (hwnd system-area-pointer))
(define-alien-routine ("IsWindowVisible" is-window-visible) (signed 32)
  (hwnd system-area-pointer))
(define-alien-routine ("EnableWindow" enable-window) (signed 32)
  (hwnd system-area-pointer)
  (enable (signed 32)))
(define-alien-routine ("GetParent" get-parent) system-area-pointer
  (hwnd system-area-pointer))
(define-alien-routine ("GetWindowRect" get-window-rect) (signed 32)
  (hwnd system-area-pointer)
  (rect (* win-rect)))
(define-alien-routine ("BeginPaint" begin-paint) system-area-pointer
  (hwnd system-area-pointer)
  (paint system-area-pointer))
(define-alien-routine ("EndPaint" end-paint) (signed 32)
  (hwnd system-area-pointer)
  (paint system-area-pointer))
(define-alien-routine ("LoadCursorW" load-cursor-w) system-area-pointer
  (instance system-area-pointer)
  (name system-area-pointer))
(define-alien-routine ("PostMessageW" post-message-w) (signed 32)
  (hwnd system-area-pointer)
  (message (unsigned 32))
  (wparam (unsigned 64))
  (lparam (signed 64)))
(define-alien-routine ("PeekMessageW" peek-message-w) (signed 32)
  (message system-area-pointer)
  (hwnd system-area-pointer)
  (filter-min (unsigned 32))
  (filter-max (unsigned 32))
  (remove (unsigned 32)))
(define-alien-routine ("TranslateMessage" translate-message) (signed 32)
  (message system-area-pointer))
(define-alien-routine ("DispatchMessageW" dispatch-message-w) (signed 64)
  (message system-area-pointer))

(defvar *module-instance* nil
  "The HINSTANCE of this process.")

(defun module-instance ()
  "Return the HINSTANCE of this process."
  (or *module-instance*
      (setf *module-instance* (get-module-handle-w (sb-sys:int-sap 0)))))

(defun hwnd-sap (hwnd)
  "Return a system-area pointer for the integer HWND."
  (sb-sys:int-sap (or hwnd 0)))

(defun string-to-utf16 (string)
  "Return a null-terminated UTF-16LE alien string.  The caller frees it
unless the string must outlive the call."
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-16le))
         (count (1+ (floor (length octets) 2)))
         (buffer (make-alien (unsigned 16) count)))
    (dotimes (index (1- count))
      (setf (deref buffer index)
            (logior (aref octets (* index 2))
                    (ash (aref octets (1+ (* index 2))) 8))))
    (setf (deref buffer (1- count)) 0)
    buffer))

(defmacro with-utf16 ((symbol string) &body body)
  "Bind SYMBOL to a UTF-16LE alien copy of STRING for BODY, then free it."
  `(let ((,symbol (string-to-utf16 ,string)))
     (unwind-protect
          (progn ,@body)
       (free-alien ,symbol))))

(defun utf16-to-string (buffer length)
  "Return LENGTH UTF-16LE code units from BUFFER as a Lisp string."
  (let ((octets (make-array (* length 2) :element-type '(unsigned-byte 8))))
    (dotimes (index length)
      (let ((unit (deref buffer index)))
        (setf (aref octets (* index 2)) (logand unit #xff)
              (aref octets (1+ (* index 2))) (ldb (byte 8 8) unit))))
    (sb-ext:octets-to-string octets :external-format :utf-16le)))

(defun window-text (hwnd)
  "Return the text of the window HWND."
  (let* ((sap (hwnd-sap hwnd))
         (length (max 0 (get-window-text-length-w sap)))
         (buffer (make-alien (unsigned 16) (1+ length))))
    (unwind-protect
         (progn
           (get-window-text-w sap (alien-sap buffer) (1+ length))
           (utf16-to-string buffer length))
      (free-alien buffer))))

(defun set-window-text (hwnd text)
  "Replace the text of the window HWND."
  (with-utf16 (buffer (or text ""))
    (set-window-text-w (hwnd-sap hwnd) (alien-sap buffer))))

(defun send-message (hwnd message wparam lparam)
  "Send MESSAGE to HWND and return the result."
  (send-message-w (hwnd-sap hwnd) message wparam lparam))

(defvar *sheet-mirrors* (make-hash-table :test #'eq)
  "A table from a sheet to its HWND integer.")

(defvar *mirror-sheets* (make-hash-table :test #'eql)
  "A table from an HWND integer to its sheet.")

(defvar *mirror-frames* (make-hash-table :test #'eql)
  "A table from a top-level HWND integer to its frame.")

(defvar *mirror-silent* nil
  "The sheet whose native control is already showing the Lisp value.
A notification from that control must not write the value back.")

(defvar *window-proc-error* nil
  "The last error caught in the window procedure, or NIL.")

(defvar *control-classes* nil
  "An alist of window-class keywords and their permanent UTF-16 names.")

(defvar *windows-classes-ready* nil
  "True after FitzCLIMFrame and FitzCLIMPane have been registered.")

(defun sheet-mirror (sheet)
  "Return the HWND integer of SHEET, or NIL when it has no native window."
  (gethash sheet *sheet-mirrors*))

(behavior 'sheet-mirror
  (should-be-null (sheet-mirror (make-instance 'push-button))))

(defun mirror-sheet (hwnd)
  "Return the sheet that owns HWND, or NIL."
  (gethash hwnd *mirror-sheets*))

(defun remember-mirror (sheet hwnd)
  "Record that SHEET is realized as HWND."
  (setf (gethash sheet *sheet-mirrors*) hwnd)
  (setf (gethash hwnd *mirror-sheets*) sheet)
  hwnd)

(defun forget-mirror (sheet)
  "Drop SHEET's HWND.  The window itself is left alone."
  (let ((hwnd (sheet-mirror sheet)))
    (when hwnd
      (remhash hwnd *mirror-sheets*)
      (remhash sheet *sheet-mirrors*)))
  nil)

(defun loword (value)
  "Return the low 16 bits of VALUE."
  (logand value #xffff))

(defun hiword (value)
  "Return bits 16 through 31 of VALUE."
  (logand (ash value -16) #xffff))

(behavior 'win32-words
  (should= 4 (loword (logior 4 (ash 500 16))))
  (should= 500 (hiword (logior 4 (ash 500 16))))
  (should= #x0300 (hiword (ash #x0300 16))))

(defun mirror-root (sheet)
  "Return the top-level sheet of SHEET when that sheet has an HWND."
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
  "Return the width and height of SHEET's region, in pixels."
  (let ((region (sheet-region sheet)))
    (values (max 1 (round (bounding-rectangle-width region)))
            (max 1 (round (bounding-rectangle-height region))))))

(defun window-screen-position (hwnd)
  "Return the screen coordinates of HWND's outer origin."
  (with-alien ((rect win-rect))
    (get-window-rect (hwnd-sap hwnd) (addr rect))
    (values (slot rect 'left) (slot rect 'top))))

(defun mirror-origin (sheet)
  "Return SHEET's origin in its top-level window."
  (let ((root (mirror-root sheet)))
    (multiple-value-bind (x y) (window-screen-position (sheet-mirror sheet))
      (multiple-value-bind (root-x root-y)
          (window-screen-position (sheet-mirror root))
        (values (- x root-x) (- y root-y))))))

(defun control-class-sap (kind)
  "Return the permanent UTF-16 class name for KIND."
  (alien-sap
   (or (cdr (assoc kind *control-classes*))
       (let ((alien (string-to-utf16
                     (ecase kind
                       (:button "BUTTON")
                       (:static "STATIC")
                       (:edit "EDIT")
                       (:listbox "LISTBOX")
                       (:combobox "COMBOBOX")
                       (:scrollbar "SCROLLBAR")
                       (:frame "FitzCLIMFrame")
                       (:pane "FitzCLIMPane")))))
         (push (cons kind alien) *control-classes*)
         alien))))

(defun mirror-kind (sheet)
  "Return the native window kind of SHEET, or NIL when it stays a layout."
  (typecase sheet
    (push-button :button)
    (toggle-button :button)
    (label-pane :static)
    (text-field :edit)
    (slider :scrollbar)
    (list-pane :listbox)
    (option-pane :combobox)
    (application-pane :pane)
    (t nil)))

(defun button-style (sheet)
  "Return the button style bits of SHEET."
  (typecase sheet
    (push-button +ws-tabstop+)
    (toggle-button
     (let ((parent (sheet-parent sheet)))
       (logior +ws-tabstop+
               (cond
                 ((typep parent 'check-box) 3)
                 ((typep parent 'radio-box)
                  (logior 9
                          (if (eq sheet (first (sheet-children parent)))
                              +ws-group+
                              0)))
                 (t 3)))))
    (t 0)))

(defun control-style (sheet)
  "Return the window style of SHEET's native control."
  (logior +ws-child+ +ws-visible+ +ws-clipsiblings+
          (typecase sheet
            ((or push-button toggle-button) (button-style sheet))
            (label-pane #x80)
            (text-field (logior +ws-tabstop+ #x80))
            (slider (if (eq (gadget-orientation sheet) :vertical) 1 0))
            (list-pane (logior +ws-tabstop+ #x01 #x40 #x100 #x200000
                               (if (eq (gadget-mode sheet) :some-of) #x08 0)))
            (option-pane (logior +ws-tabstop+ #x03 #x200 #x200000))
            (t 0))
          (if (and (gadgetp sheet) (not (gadget-active-p sheet)))
              +ws-disabled+
              0)))

(defun labelled-gadget-p (sheet)
  "Return true when SHEET shows a label."
  (or (typep sheet 'label-pane)
      (typep sheet 'push-button)
      (typep sheet 'toggle-button)))

(defun control-title (sheet)
  "Return the text a new control should show."
  (cond
    ((typep sheet 'text-field) (gadget-value sheet))
    ((labelled-gadget-p sheet) (or (gadget-label sheet) ""))
    (t "")))

(defvar *next-control-id* 100
  "The next child-window identifier.")

(defun next-control-id ()
  "Return a fresh child-window identifier."
  (incf *next-control-id*))

(defun create-window (class title style x y width height parent id
                      &key (ex-style 0))
  "Create a window and return its HWND.  Signal an error when Windows refuses."
  (with-utf16 (title-alien (or title ""))
    (let ((hwnd (create-window-ex-w ex-style class (alien-sap title-alien) style
                                    x y (max 1 width) (max 1 height)
                                    parent
                                    (sb-sys:int-sap id)
                                    (module-instance)
                                    (sb-sys:int-sap 0))))
      (when (zerop (sb-sys:sap-int hwnd))
        (error "CreateWindowExW failed (error ~D)." (get-last-error)))
      hwnd)))

(defun gadget-item-value (gadget item)
  "Return the value ITEM contributes to GADGET."
  (funcall (gadget-value-key gadget) item))

(defun gadget-item-name (gadget item)
  "Return the text GADGET shows for ITEM."
  (let ((name (funcall (gadget-name-key gadget) item)))
    (if (stringp name)
        name
        (princ-to-string name))))

(defun item-index (gadget value)
  "Return the index of VALUE in GADGET, or NIL."
  (position value (gadget-items gadget)
            :key (lambda (item) (gadget-item-value gadget item))
            :test (gadget-test gadget)))

(defun slider-position (gadget)
  "Return GADGET's value scaled onto the native scrollbar range."
  (let ((min (gadget-min-value gadget))
        (max (gadget-max-value gadget)))
    (if (= min max)
        0
        (round (* +scroll-extent+ (/ (- (gadget-value gadget) min) (- max min)))))))

(defun position-to-slider (gadget position)
  "Return the Lisp value of a native scrollbar POSITION."
  (let ((min (gadget-min-value gadget))
        (max (gadget-max-value gadget)))
    (if (= min max)
        min
        (+ min (* (- max min) (/ position +scroll-extent+))))))

(behavior 'slider-position
  (let ((slider (make-instance 'slider :min-value 0 :max-value 10 :value 4)))
    (should= 400 (slider-position slider))
    (should= 2 (position-to-slider slider 200))
    (should= 0 (slider-position
                (make-instance 'slider :min-value 3 :max-value 3 :value 3)))))

(defun write-toggle (gadget)
  "Make GADGET's check box match its Lisp value."
  (send-message (sheet-mirror gadget) +bm-setcheck+
                (if (gadget-value gadget) +bst-checked+ 0) 0))

(defun write-slider (gadget)
  "Make GADGET's scrollbar match its Lisp value."
  (let ((hwnd (sheet-mirror gadget)))
    (send-message hwnd +sbm-setrange+ 1 (ash +scroll-extent+ 16))
    (send-message hwnd +sbm-setpos+ (slider-position gadget) 1)))

(defun write-items (gadget)
  "Replace GADGET's native items and selection from Lisp."
  (let ((hwnd (sheet-mirror gadget))
        (option (typep gadget 'option-pane)))
    (send-message hwnd (if option +cb-resetcontent+ +lb-resetcontent+) 0 0)
    (dolist (item (gadget-items gadget))
      (with-utf16 (text (gadget-item-name gadget item))
        (send-message hwnd (if option +cb-addstring+ +lb-addstring+)
                      0 (sb-sys:sap-int (alien-sap text)))))
    (write-selection gadget)))

(defun write-selection (gadget)
  "Select GADGET's Lisp value in its native control."
  (let ((hwnd (sheet-mirror gadget))
        (value (gadget-value gadget)))
    (cond
      ((typep gadget 'option-pane)
       (send-message hwnd +cb-setcursel+ (or (item-index gadget value) -1) 0))
      ((eq (gadget-mode gadget) :some-of)
       (loop for item in (gadget-items gadget)
             for index from 0
             do (send-message hwnd +lb-setsel+
                              (if (member (gadget-item-value gadget item) value
                                          :test (gadget-test gadget))
                                  1
                                  0)
                              index)))
      (t
       (send-message hwnd +lb-setcursel+ (or (item-index gadget value) -1) 0)))))

(defun read-selection (gadget)
  "Return the value selected in GADGET's native control."
  (let ((hwnd (sheet-mirror gadget))
        (items (gadget-items gadget)))
    (cond
      ((typep gadget 'option-pane)
       (let ((index (send-message hwnd +cb-getcursel+ 0 0)))
         (when (and (not (minusp index)) (< index (length items)))
           (gadget-item-value gadget (nth index items)))))
      ((eq (gadget-mode gadget) :some-of)
       (loop for item in items
             for index from 0
             when (plusp (send-message hwnd +lb-getsel+ index 0))
               collect (gadget-item-value gadget item)))
      (t
       (let ((index (send-message hwnd +lb-getcursel+ 0 0)))
         (when (and (not (minusp index)) (< index (length items)))
           (gadget-item-value gadget (nth index items))))))))

(defun refresh-mirror (sheet)
  "Copy SHEET's Lisp state into its native control."
  (let ((*mirror-silent* sheet))
    (cond
      ((typep sheet 'text-field)
       (set-window-text (sheet-mirror sheet) (gadget-value sheet)))
      ((labelled-gadget-p sheet)
       (set-window-text (sheet-mirror sheet) (or (gadget-label sheet) ""))))
    (when (typep sheet 'toggle-button)
      (write-toggle sheet))
    (when (typep sheet 'slider)
      (write-slider sheet))
    (when (or (typep sheet 'list-pane) (typep sheet 'option-pane))
      (write-items sheet))))

(defun run-gadget-callback (gadget)
  "Run GADGET's activate callback when GADGET is active."
  (when (gadget-active-p gadget)
    (let ((callback (gadget-activate-callback gadget)))
      (cond
        ((null callback) nil)
        ((functionp callback) (funcall callback gadget))
        ((symbolp callback) (funcall callback gadget))
        ((listp callback)
         (dolist (function callback)
           (funcall function gadget)))
        (t (error "~S is not a gadget callback." callback))))))

(defun handle-command (wparam lparam)
  "Apply a WM_COMMAND notification from the control at LPARAM."
  (when (plusp lparam)
    (let ((sheet (mirror-sheet lparam))
          (code (hiword wparam)))
      (when (and sheet (not (eq *mirror-silent* sheet)))
        (cond
          ((and (typep sheet 'push-button) (= code +bn-clicked+))
           (run-gadget-callback sheet))
          ((and (typep sheet 'toggle-button) (= code +bn-clicked+))
           (let ((*mirror-silent* sheet))
             (setf (gadget-value sheet)
                   (= +bst-checked+
                      (logand #xffff
                              (send-message lparam +bm-getcheck+ 0 0))))))
          ((and (typep sheet 'text-field) (= code +en-change+))
           (let ((*mirror-silent* sheet))
             (setf (gadget-value sheet) (window-text lparam))))
          ((and (typep sheet 'list-pane) (= code +lbn-selchange+))
           (let ((*mirror-silent* sheet))
             (setf (gadget-value sheet) (read-selection sheet))))
          ((and (typep sheet 'option-pane) (= code +cbn-selchange+))
           (let ((*mirror-silent* sheet))
             (setf (gadget-value sheet) (read-selection sheet)))))))))

(defun scroll-position (gadget code wparam)
  "Return the scrollbar position GADGET should take for this notification."
  (cond
    ((member code (list +sb-thumbposition+ +sb-thumbtrack+))
     (hiword wparam))
    ((= code 0) (max 0 (- (slider-position gadget) 10)))
    ((= code 1) (min +scroll-extent+ (+ (slider-position gadget) 10)))
    ((= code 2) (max 0 (- (slider-position gadget) 100)))
    ((= code 3) (min +scroll-extent+ (+ (slider-position gadget) 100)))
    ((= code 6) 0)
    ((= code 7) +scroll-extent+)
    (t (slider-position gadget))))

(defun handle-scroll (lparam wparam)
  "Apply a scroll notification from the scrollbar at LPARAM."
  (let ((sheet (mirror-sheet lparam)))
    (when (and (typep sheet 'slider) (not (eq *mirror-silent* sheet)))
      (let ((position (scroll-position sheet (loword wparam) wparam)))
        (let ((*mirror-silent* sheet))
          (send-message lparam +sbm-setpos+ position 1)
          (setf (gadget-value sheet) (position-to-slider sheet position)))))))

(defun paint-pane (hwnd)
  "Validate HWND.  Drawing the output history comes later."
  (let* ((paint (make-alien (unsigned 8) 16))
         (sap (alien-sap paint)))
    (unwind-protect
         (progn
           (begin-paint (hwnd-sap hwnd) sap)
           (end-paint (hwnd-sap hwnd) sap))
      (free-alien paint))))

(defun handle-message (hwnd message wparam lparam)
  "Handle one message for HWND.  Return a result when this port handles it."
  (cond
    ((= message +wm-command+)
     (handle-command wparam lparam)
     (values t 0))
    ((or (= message +wm-hscroll+) (= message +wm-vscroll+))
     (handle-scroll lparam wparam)
     (values t 0))
    ((and (= message +wm-paint+)
          (typep (mirror-sheet hwnd) 'application-pane))
     (paint-pane hwnd)
     (values t 0))
    (t (values nil 0))))

(define-alien-callable fitzclim-window-proc (signed 64)
    ((hwnd system-area-pointer)
     (message (unsigned 32))
     (wparam (unsigned 64))
     (lparam (signed 64)))
  (let ((integer (sb-sys:sap-int hwnd)))
    (handler-case
        (multiple-value-bind (handled result)
            (handle-message integer message wparam lparam)
          (if handled
              result
              (def-window-proc-w hwnd message wparam lparam)))
      (error (condition)
        (setf *window-proc-error* condition)
        (def-window-proc-w hwnd message wparam lparam)))))

(defun register-window-class (kind background)
  "Register KIND when Windows does not already have that class."
  (let ((class (make-alien wndclassexw)))
    (unwind-protect
         (progn
           (setf (slot class 'cb-size) (alien-size wndclassexw :bytes)
                 (slot class 'style) 0
                 (slot class 'proc)
                 (alien-sap (alien-callable-function 'fitzclim-window-proc))
                 (slot class 'cls-extra) 0
                 (slot class 'wnd-extra) 0
                 (slot class 'instance) (module-instance)
                 (slot class 'icon) (sb-sys:int-sap 0)
                 (slot class 'cursor) (load-cursor-w (sb-sys:int-sap 0)
                                                     (sb-sys:int-sap 32512))
                 (slot class 'background) (sb-sys:int-sap background)
                 (slot class 'menu-name) (sb-sys:int-sap 0)
                 (slot class 'class-name) (control-class-sap kind)
                 (slot class 'icon-sm) (sb-sys:int-sap 0))
           (let ((atom (register-class-ex-w class)))
             (unless (or (plusp atom)
                         (= (get-last-error) +error-class-already-exists+))
               (error "RegisterClassExW failed (error ~D)." (get-last-error)))))
      (free-alien class))))

(defun ensure-window-classes ()
  "Register the frame and application-pane window classes."
  (unless *windows-classes-ready*
    (unless (= 80 (alien-size wndclassexw :bytes))
      (error "WNDCLASSEXW is ~D bytes, not 80."
             (alien-size wndclassexw :bytes)))
    (register-window-class :frame 16)
    (register-window-class :pane 6)
    (setf *windows-classes-ready* t)))

(defun place-mirror (sheet)
  "Move SHEET's window to its place in the top-level window."
  (let ((root (mirror-root sheet))
        (hwnd (sheet-mirror sheet)))
    (when (and root hwnd (not (eq sheet root)))
      (multiple-value-bind (x y) (sheet-in-root sheet root)
        (multiple-value-bind (width height) (sheet-pixel-size sheet)
          (move-window (hwnd-sap hwnd) x y width height 0))))))

(defun resize-root (sheet width height)
  "Give the top-level window of SHEET a new client size, keeping its origin."
  (when (sheet-mirror sheet)
    (set-window-pos (hwnd-sap (sheet-mirror sheet)) (sb-sys:int-sap 0)
                    0 0 (max 1 (round width)) (max 1 (round height))
                    (logior 2 4 16))))

(defmethod allocate-space :after ((pane sheet) width height)
  (when (sheet-mirror pane)
    (if (null (sheet-parent pane))
        (resize-root pane width height)
        (place-mirror pane))))

(defun realize-sheet (sheet root)
  "Create SHEET's native window when it is a control or an application pane."
  (let ((kind (mirror-kind sheet)))
    (when (and kind (null (sheet-mirror sheet)))
      (multiple-value-bind (width height) (sheet-pixel-size sheet)
        (let ((hwnd (create-window (control-class-sap kind)
                                   (control-title sheet)
                                   (control-style sheet)
                                   0 0 width height
                                   (hwnd-sap (sheet-mirror root))
                                   (next-control-id))))
          (remember-mirror sheet (sb-sys:sap-int hwnd))
          (refresh-mirror sheet))))))

(defun destroy-mirrors (sheet)
  "Destroy the native windows of SHEET and its descendants."
  (map-over-sheets
   (lambda (descendant)
     (let ((hwnd (sheet-mirror descendant)))
       (when hwnd
         (let ((sap (hwnd-sap hwnd)))
           (when (plusp (is-window sap))
             (destroy-window sap)))
         (forget-mirror descendant))))
   sheet))

(defmethod sheet-disown-child :after ((sheet sheet) (child sheet))
  (declare (ignore sheet))
  (destroy-mirrors child))

(defmethod sheet-adopt-child :after ((sheet sheet) (child sheet))
  (let ((root (mirror-root sheet)))
    (when root
      (map-over-sheets (lambda (descendant) (realize-sheet descendant root))
                       child)
      (map-over-sheets #'place-mirror child))))

(defun realize-frame-sheets (frame)
  "Create native windows for FRAME's controls and place them."
  (let ((root (frame-top-level-sheet frame)))
    (map-over-sheets (lambda (sheet) (realize-sheet sheet root)) root)
    (map-over-sheets #'place-mirror root)))

(defclass windows-port ()
  ()
  (:documentation "The Windows port.  Its message queue is the thread's."))

(defclass windows-frame-manager ()
  ((port
    :initarg :port
    :reader frame-manager-port
    :documentation "The port that owns this manager's windows.")
   (frames
    :initform nil
    :accessor frame-manager-frames
    :documentation "The frames this manager is currently showing."))
  (:documentation
   "A frame manager that realizes frames as Windows windows and gadgets
as native controls.  Layout panes have no window of their own.  Their
children are parented to the frame window."))

(defun make-windows-frame-manager ()
  "Return a frame manager that realizes frames on Windows."
  (make-instance 'windows-frame-manager :port (make-instance 'windows-port)))

(behavior 'make-windows-frame-manager
  (let ((manager (make-windows-frame-manager)))
    (should-be-true (typep manager 'windows-frame-manager))
    (should-be-true (typep (frame-manager-port manager) 'windows-port))
    (should-be-null (frame-manager-frames manager))))

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
   "Dispatch one queued Windows message for PORT.  Return true when a
message was dispatched.  This does not wait."))

(defmethod adopt-frame ((manager windows-frame-manager) (frame application-frame))
  (ensure-window-classes)
  (let ((top (or (frame-top-level-sheet frame)
                 (error "The frame has no panes."))))
    (when (sheet-mirror top)
      (error "The frame is already adopted."))
    (let ((hwnd (create-window (control-class-sap :frame)
                               (frame-pretty-name frame)
                               (logior +ws-popup+ +ws-clipchildren+)
                               -32000 -32000
                               (round (frame-width frame))
                               (round (frame-height frame))
                               (sb-sys:int-sap 0)
                               0
                               :ex-style (logior +ws-ex-toolwindow+
                                                 +ws-ex-noactivate+))))
      (remember-mirror top (sb-sys:sap-int hwnd))
      (setf (gethash (sb-sys:sap-int hwnd) *mirror-frames*) frame)
      (push frame (frame-manager-frames manager))
      (handler-case
          (realize-frame-sheets frame)
        (error (condition)
          (disown-frame manager frame)
          (error condition)))))
  frame)

(defmethod disown-frame ((manager windows-frame-manager) (frame application-frame))
  (let ((top (frame-top-level-sheet frame)))
    (when top
      (let ((hwnd (sheet-mirror top)))
        (when hwnd
          (let ((sap (hwnd-sap hwnd)))
            (when (plusp (is-window sap))
              (destroy-window sap)))
          (remhash hwnd *mirror-frames*)))
      (map-over-sheets #'forget-mirror top)))
  (setf (frame-manager-frames manager)
        (remove frame (frame-manager-frames manager) :test #'eq))
  frame)

(defun frame-hwnd (frame)
  "Return FRAME's top-level HWND integer."
  (let ((top (frame-top-level-sheet frame)))
    (unless (and top (sheet-mirror top))
      (error "The frame has no native window."))
    (sheet-mirror top)))

(defmethod enable-frame ((manager windows-frame-manager) (frame application-frame))
  (declare (ignore manager))
  (show-window (hwnd-sap (frame-hwnd frame)) +sw-showna+)
  frame)

(defmethod disable-frame ((manager windows-frame-manager) (frame application-frame))
  (declare (ignore manager))
  (show-window (hwnd-sap (frame-hwnd frame)) +sw-hide+)
  frame)

(defmethod process-next-event ((port windows-port) &key wait-function timeout)
  (declare (ignore port wait-function timeout))
  (let* ((message (make-alien (unsigned 8) 8))
         (sap (alien-sap message)))
    (unwind-protect
         (when (plusp (peek-message-w sap (sb-sys:int-sap 0) 0 0 +pm-remove+))
           (translate-message sap)
           (dispatch-message-w sap)
           t)
      (free-alien message))))

(defmethod (setf gadget-value) :after
    (value (gadget text-field) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (set-window-text (sheet-mirror gadget) (gadget-value gadget)))))

(defun mirror-label (gadget label)
  "Show LABEL on GADGET's native control."
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (set-window-text (sheet-mirror gadget) (or label "")))))

(defmethod (setf gadget-label) :after (label (gadget label-pane))
  (mirror-label gadget label))

(defmethod (setf gadget-label) :after (label (gadget push-button))
  (mirror-label gadget label))

(defmethod (setf gadget-label) :after (label (gadget toggle-button))
  (mirror-label gadget label))

(defmethod (setf gadget-value) :around
    (value (gadget toggle-button) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (let ((result (call-next-method)))
    (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
      (let ((*mirror-silent* gadget))
        (write-toggle gadget)))
    result))

(defmethod (setf gadget-value) :after
    (value (gadget slider) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (write-slider gadget))))

(defmethod (setf gadget-value) :after
    (value (gadget list-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (write-selection gadget))))

(defmethod (setf gadget-value) :after
    (value (gadget option-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (write-selection gadget))))

(defmethod (setf gadget-items) :after (items (gadget list-pane))
  (declare (ignore items))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (write-items gadget))))

(defmethod (setf gadget-items) :after (items (gadget option-pane))
  (declare (ignore items))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (let ((*mirror-silent* gadget))
      (write-items gadget))))

(defmethod note-gadget-activated :after (client (gadget gadget))
  (declare (ignore client))
  (when (sheet-mirror gadget)
    (enable-window (hwnd-sap (sheet-mirror gadget)) 1)))

(defmethod note-gadget-deactivated :after (client (gadget gadget))
  (declare (ignore client))
  (when (sheet-mirror gadget)
    (enable-window (hwnd-sap (sheet-mirror gadget)) 0)))

(define-application-frame windows-gadget-frame ()
  ()
  (:pretty-name "Windows Gadgets")
  (:panes
   (banner :label :label "Call sign")
   (name :text-field :value "Fitz")
   (go :push-button :label "Go")
   (speed :slider :min-value 0 :max-value 10 :value 4 :orientation :horizontal)
   (choice :list-pane :items '("one" "two") :value "one" :test #'equal)
   (many :list-pane :items '("a" "b") :value '("a") :test #'equal :mode :some-of)
   (pick :option-pane :items '("red" "blue") :value "red" :test #'equal)
   (canvas :application :width 40 :height 40 :max-height 40)
   (radio (let ((box (make-instance 'radio-box))
                (one (make-instance 'toggle-button :label "A"))
                (two (make-instance 'toggle-button :label "B")))
            (adopt-sheet box one)
            (adopt-sheet box two)
            (setf (gadget-value box) one)
            box))
   (checks (let ((box (make-instance 'check-box))
                 (one (make-instance 'toggle-button :label "C"))
                 (two (make-instance 'toggle-button :label "D")))
             (adopt-sheet box one)
             (adopt-sheet box two)
             (setf (gadget-value box) (list one))
             box))
   (scroller :scroll-bar :orientation :vertical :min-value 0 :max-value 10
             :value 0))
  (:layouts
   (default (vertically (:spacing 2)
              banner name go speed choice many pick canvas radio checks
              scroller)))
  (:geometry 320 640))

(behavior 'windows-gadgets
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-gadget-frame))
        (clicked nil))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (should-eq frame (first (frame-manager-frames manager)))
           (let ((top (frame-top-level-sheet frame))
                 (banner (find-pane-named frame 'banner))
                 (name (find-pane-named frame 'name))
                 (go (find-pane-named frame 'go))
                 (speed (find-pane-named frame 'speed))
                 (choice (find-pane-named frame 'choice))
                 (many (find-pane-named frame 'many))
                 (pick (find-pane-named frame 'pick))
                 (canvas (find-pane-named frame 'canvas))
                 (radio (find-pane-named frame 'radio))
                 (checks (find-pane-named frame 'checks))
                 (scroller (find-pane-named frame 'scroller)))
             (should-be-true (plusp (is-window (hwnd-sap (sheet-mirror top)))))
             (should-equal "Windows Gadgets" (window-text (sheet-mirror top)))
             (should-be-null (sheet-mirror (frame-panes frame)))
             (should-be-null (sheet-mirror radio))
             (should-equal "Call sign" (window-text (sheet-mirror banner)))
             (should-equal "Fitz" (window-text (sheet-mirror name)))
             (should-equal "Go" (window-text (sheet-mirror go)))
             (should= (sheet-mirror top)
                      (sb-sys:sap-int (get-parent (hwnd-sap (sheet-mirror go)))))
             (should-be-true (sheet-mirror canvas))
             (multiple-value-bind (x y) (mirror-origin name)
               (should= 0 x)
               (should= 18 y))
             (should= 400 (send-message (sheet-mirror speed) +sbm-getpos+ 0 0))
             (setf (gadget-value name) "Roy")
             (should-equal "Roy" (window-text (sheet-mirror name)))
             (set-window-text (sheet-mirror name) "Henry")
             (should-equal "Henry" (gadget-value name))
             (setf (gadget-activate-callback go)
                   (lambda (gadget)
                     (declare (ignore gadget))
                     (setf clicked t)))
             (deactivate-gadget go)
             (send-message (sheet-mirror go) +bm-click+ 0 0)
             (should-be-null clicked)
             (activate-gadget go)
             (send-message (sheet-mirror go) +bm-click+ 0 0)
             (should-be-true clicked)
             (send-message (sheet-mirror top) +wm-hscroll+
                           (logior +sb-thumbposition+ (ash 200 16))
                           (sheet-mirror speed))
             (should= 2 (gadget-value speed))
             (send-message (sheet-mirror choice) +lb-setcursel+ 1 0)
             (send-message (sheet-mirror top) +wm-command+
                           (ash +lbn-selchange+ 16)
                           (sheet-mirror choice))
             (should-equal "two" (gadget-value choice))
             (send-message (sheet-mirror many) +lb-setsel+ 1 1)
             (send-message (sheet-mirror top) +wm-command+
                           (ash +lbn-selchange+ 16)
                           (sheet-mirror many))
             (should-equal '("a" "b") (gadget-value many))
             (send-message (sheet-mirror pick) +cb-setcursel+ 1 0)
             (send-message (sheet-mirror top) +wm-command+
                           (ash +cbn-selchange+ 16)
                           (sheet-mirror pick))
             (should-equal "blue" (gadget-value pick))
             (let ((buttons (sheet-children radio)))
               (send-message (sheet-mirror (second buttons)) +bm-click+ 0 0)
               (should-eq (second buttons) (gadget-value radio))
               (should= 0 (send-message (sheet-mirror (first buttons))
                                        +bm-getcheck+ 0 0))
               (should= 1 (send-message (sheet-mirror (second buttons))
                                        +bm-getcheck+ 0 0)))
             (let ((buttons (sheet-children checks)))
               (send-message (sheet-mirror (second buttons)) +bm-click+ 0 0)
               (should-equal buttons (gadget-value checks)))
             (send-message (sheet-mirror top) +wm-vscroll+
                           (logior +sb-thumbposition+ (ash 500 16))
                           (sheet-mirror scroller))
             (should= 5 (gadget-value scroller))
             (let ((port (frame-manager-port manager)))
               (dotimes (i 20)
                 (unless (process-next-event port)
                   (return)))
               (post-message-w (hwnd-sap (sheet-mirror top)) 0 0 0)
               (should-be-true (process-next-event port)))
             (enable-frame manager frame)
             (should-be-true (plusp (is-window-visible (hwnd-sap (sheet-mirror top)))))
             (disable-frame manager frame)
             (should= 0 (is-window-visible (hwnd-sap (sheet-mirror top))))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (let ((hwnd (sheet-mirror top)))
               (disown-frame manager frame)
               (should= 0 (is-window (hwnd-sap hwnd)))
               (should-be-null (sheet-mirror top))
               (should-be-null (frame-manager-frames manager)))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))
