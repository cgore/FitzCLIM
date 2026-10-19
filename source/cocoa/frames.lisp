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

(defpackage :FitzCLIM/cocoa/frames
  (:use :common-lisp
        :sigma/behave
        :sb-alien
        :FitzCLIM/command
        :FitzCLIM/frame
        :FitzCLIM/pane
        :FitzCLIM/gadget
        :FitzCLIM/event
        :FitzCLIM/sheet
        :FitzCLIM/geometry
        :FitzCLIM/layout
        :FitzCLIM/medium
        :FitzCLIM/output-record
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/opacity
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style
        :FitzCLIM/opengl
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present
        :FitzCLIM/port)
  (:import-from :FitzCLIM/cocoa
                :*id-id*
                :*ulong-ulong*
                :*void-id*
                :*void-id-bool*
                :add-method-once
                :ensure-cocoa-application
                :ensure-cocoa-classes
                :fitz-make-view
                :fitz-make-window
                :fitz-poll-send
                :fitz-post-wakeup
                :fitz-set-content-size
                :fitz-set-frame
                :fitz-view-frame
                :live-sap?
                :mirror-sap
                :ns-string
                :ns-text
                :objc-get-bool
                :objc-get-class
                :objc-get-double
                :objc-get-id
                :objc-get-long
                :objc-id
                :objc-new
                :objc-set-bool
                :objc-set-double
                :objc-set-id
                :objc-set-long
                :objc-set-ulong
                :objc-void
                :selector
                :with-cocoa)
  (:export :cocoa-frame-manager
           :cocoa-port
           :make-cocoa-frame-manager))
(in-package :FitzCLIM/cocoa/frames)

(defvar *cocoa-error* nil
  "The last error from a Cocoa callback, or NIL.")

(defvar *cocoa-targets* (make-hash-table :test #'eql)
  "A table from a target pointer to the sheet it speaks for.")

(defvar *sheet-targets* (make-hash-table :test #'eq)
  "A table from a sheet to the target that speaks for it.")

(defun target-sheet (target)
  "Return the sheet TARGET speaks for, or NIL."
  (gethash (sb-sys:sap-int target) *cocoa-targets*))

(defun sheet-target (sheet)
  "Return the target that speaks for SHEET, creating it on the first call."
  (or (gethash sheet *sheet-targets*)
      (let ((target (objc-new "FitzCLIMTarget")))
        (setf (gethash (sb-sys:sap-int target) *cocoa-targets*) sheet)
        (setf (gethash sheet *sheet-targets*) target)
        target)))

(defun forget-target (sheet)
  "Drop and release SHEET's target."
  (let ((target (gethash sheet *sheet-targets*)))
    (when target
      (remhash (sb-sys:sap-int target) *cocoa-targets*)
      (remhash sheet *sheet-targets*)
      (objc-void target (selector "release"))))
  nil)

(defun item-value (gadget item)
  "Return the value ITEM contributes to GADGET."
  (funcall (gadget-value-key gadget) item))

(defun item-name (gadget item)
  "Return the text GADGET shows for ITEM."
  (let ((name (funcall (gadget-name-key gadget) item)))
    (if (stringp name)
        name
        (princ-to-string name))))

(defun item-index (gadget value)
  "Return the index of VALUE in GADGET, or NIL."
  (position value (gadget-items gadget)
            :key (lambda (item) (item-value gadget item))
            :test (gadget-test gadget)))

(defun item-selected? (gadget item value)
  "Return true when VALUE selects ITEM in GADGET."
  (if (eq (gadget-mode gadget) :some-of)
      (and (listp value)
           (member (item-value gadget item) value :test (gadget-test gadget)))
      (funcall (gadget-test gadget) (item-value gadget item) value)))

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

(defun accept-toggle (sheet)
  "Copy a button's native state into SHEET."
  (let ((*mirror-silent* sheet))
    (setf (gadget-value sheet)
          (plusp (objc-get-long (mirror-sap sheet) "state")))))

(defun accept-slider (sheet)
  "Copy a slider's native value into SHEET."
  (let ((*mirror-silent* sheet))
    (setf (gadget-value sheet)
          (objc-get-double (mirror-sap sheet) "doubleValue"))))

(defun accept-field (sheet)
  "Copy a text field's native string into SHEET."
  (let ((*mirror-silent* sheet))
    (setf (gadget-value sheet)
          (ns-text (objc-get-id (mirror-sap sheet) "stringValue")))))

(defun selected-row-indexes (table)
  "Return the selected row indexes of TABLE, in increasing order."
  (let ((indexes (objc-get-id table "selectedRowIndexes"))
        (count (objc-get-long table "numberOfRows")))
    (loop for index = (objc-get-long indexes "firstIndex")
            then (alien-funcall *ulong-ulong* indexes
                                (selector "indexGreaterThanIndex:")
                                index)
          while (< index count)
          collect index)))

(defun accept-table (sheet)
  "Copy a table's native selection into SHEET."
  (let* ((items (gadget-items sheet))
         (values (loop for index in (selected-row-indexes (mirror-sap sheet))
                       when (< index (length items))
                         collect (item-value sheet (nth index items))))
         (*mirror-silent* sheet))
    (setf (gadget-value sheet)
          (if (eq (gadget-mode sheet) :some-of)
              values
              (first values)))))

(defun accept-popup (sheet)
  "Copy a popup's native selection into SHEET."
  (let* ((index (objc-get-long (mirror-sap sheet) "indexOfSelectedItem"))
         (items (gadget-items sheet))
         (*mirror-silent* sheet))
    (when (and (>= index 0) (< index (length items)))
      (setf (gadget-value sheet) (item-value sheet (nth index items))))))

(defun call-cocoa-target (target function)
  "Call FUNCTION with TARGET's sheet, and remember an error instead of escaping."
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (let ((sheet (target-sheet target)))
          (when (and sheet (not (eq *mirror-silent* sheet)))
            (funcall function sheet)))
      (error (condition)
        (setf *cocoa-error* condition))))
  nil)

(define-alien-callable cocoa-clicked void
    ((self system-area-pointer)
     (command system-area-pointer)
     (sender system-area-pointer))
  (declare (ignore command sender))
  (call-cocoa-target self
                     (lambda (sheet)
                       (cond
                         ((typep sheet 'push-button)
                          (run-gadget-callback sheet))
                         ((typep sheet 'toggle-button)
                          (accept-toggle sheet))))))

(define-alien-callable cocoa-slider-changed void
    ((self system-area-pointer)
     (command system-area-pointer)
     (sender system-area-pointer))
  (declare (ignore command sender))
  (call-cocoa-target self #'accept-slider))

(define-alien-callable cocoa-popup-changed void
    ((self system-area-pointer)
     (command system-area-pointer)
     (sender system-area-pointer))
  (declare (ignore command sender))
  (call-cocoa-target self #'accept-popup))

(define-alien-callable cocoa-take-string void
    ((self system-area-pointer)
     (command system-area-pointer)
     (sender system-area-pointer))
  (declare (ignore command sender))
  (call-cocoa-target self #'accept-field))

(define-alien-callable cocoa-row-count long
    ((self system-area-pointer)
     (command system-area-pointer)
     (table system-area-pointer))
  (declare (ignore command table))
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (let ((sheet (target-sheet self)))
          (if (typep sheet 'list-pane)
              (length (gadget-items sheet))
              0))
      (error (condition)
        (setf *cocoa-error* condition)
        0))))

(define-alien-callable cocoa-cell-value system-area-pointer
    ((self system-area-pointer)
     (command system-area-pointer)
     (table system-area-pointer)
     (column system-area-pointer)
     (row long))
  (declare (ignore command table column))
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (let ((sheet (target-sheet self)))
          (if (and (typep sheet 'list-pane)
                   (<= 0 row (1- (length (gadget-items sheet)))))
              (ns-string (item-name sheet (nth row (gadget-items sheet))))
              (ns-string "")))
      (error (condition)
        (setf *cocoa-error* condition)
        (ns-string "")))))

(define-alien-callable cocoa-selection-changed void
    ((self system-area-pointer)
     (command system-area-pointer)
     (note system-area-pointer))
  (declare (ignore command note))
  (call-cocoa-target self #'accept-table))

(defun ensure-target-methods ()
  "Add the target callbacks once."
  (ensure-cocoa-classes)
  (let ((class (objc-get-class "FitzCLIMTarget")))
    (add-method-once class "clicked:" "v@:@" 'cocoa-clicked)
    (add-method-once class "sliderChanged:" "v@:@" 'cocoa-slider-changed)
    (add-method-once class "popupChanged:" "v@:@" 'cocoa-popup-changed)
    (add-method-once class "takeString:" "v@:@" 'cocoa-take-string)
    (add-method-once class "numberOfRowsInTableView:" "q@:@" 'cocoa-row-count)
    (add-method-once class "tableView:objectValueForTableColumn:row:"
                     "@@:@@q" 'cocoa-cell-value)
    (add-method-once class "tableViewSelectionDidChange:" "v@:@"
                     'cocoa-selection-changed)))

(defun set-control-action (view sheet selector-name)
  "Make SHEET's target receive SELECTOR-NAME from VIEW."
  (objc-set-id view "setTarget:" (sheet-target sheet))
  (objc-set-id view "setAction:" (selector selector-name)))

(defun button-type (sheet)
  "Return the NSButton type of SHEET."
  (let ((parent (sheet-parent sheet)))
    (cond
      ((typep sheet 'push-button) 7)
      ((typep parent 'check-box) 3)
      ((typep parent 'radio-box) 4)
      (t 3))))

(defun mirror-class (sheet)
  "Return the Cocoa class that realizes SHEET, or NIL when it stays a layout."
  (typecase sheet
    ((or push-button toggle-button) "NSButton")
    ((or label-pane text-field) "NSTextField")
    (slider "NSSlider")
    (list-pane "NSTableView")
    (option-pane "NSPopUpButton")
    ((or application-pane opengl-pane) "FitzCLIMView")
    (t nil)))

(defun install-table-column (view)
  "Give VIEW one text column."
  (let ((column (alien-funcall *id-id*
                               (objc-id (objc-get-class "NSTableColumn")
                                        (selector "alloc"))
                               (selector "initWithIdentifier:")
                               (ns-string "items"))))
    (objc-set-id view "addTableColumn:" column)
    (objc-void column (selector "release"))))

(defun configure-mirror (view sheet)
  "Set the native properties VIEW needs before it shows SHEET's value."
  (cond
    ((or (typep sheet 'push-button) (typep sheet 'toggle-button))
     (objc-set-ulong view "setButtonType:" (button-type sheet))
     (set-control-action view sheet "clicked:")
     (objc-set-bool view "setEnabled:" (gadget-active-p sheet)))
    ((typep sheet 'label-pane)
     (objc-set-bool view "setEditable:" nil)
     (objc-set-bool view "setSelectable:" nil))
    ((typep sheet 'text-field)
     (objc-set-bool view "setEditable:" (gadget-active-p sheet))
     (objc-set-bool view "setSelectable:" t)
     (set-control-action view sheet "takeString:"))
    ((typep sheet 'slider)
     (objc-set-bool view "setContinuous:" t)
     (objc-set-bool view "setEnabled:" (gadget-active-p sheet))
     (set-control-action view sheet "sliderChanged:"))
    ((typep sheet 'list-pane)
     (install-table-column view)
     (let ((target (sheet-target sheet)))
       (objc-set-id view "setDataSource:" target)
       (objc-set-id view "setDelegate:" target))
     (objc-set-bool view "setAllowsMultipleSelection:"
                    (eq (gadget-mode sheet) :some-of))
     (objc-set-bool view "setEnabled:" (gadget-active-p sheet)))
    ((typep sheet 'option-pane)
     (objc-set-bool view "setEnabled:" (gadget-active-p sheet))
     (set-control-action view sheet "popupChanged:"))))

(defun write-toggle (gadget)
  "Make GADGET's button match its Lisp value."
  (objc-set-long (mirror-sap gadget) "setState:"
                 (if (gadget-value gadget) 1 0)))

(defun write-slider (gadget)
  "Make GADGET's slider match its Lisp range and value."
  (let ((view (mirror-sap gadget)))
    (objc-set-double view "setMinValue:" (gadget-min-value gadget))
    (objc-set-double view "setMaxValue:" (gadget-max-value gadget))
    (objc-set-double view "setDoubleValue:" (gadget-value gadget))))

(defun write-button-title (gadget)
  "Show GADGET's label on its button."
  (objc-set-id (mirror-sap gadget) "setTitle:"
               (ns-string (or (gadget-label gadget) ""))))

(defun write-field-string (gadget text)
  "Show TEXT in GADGET's text field."
  (objc-set-id (mirror-sap gadget) "setStringValue:"
               (ns-string (or text ""))))

(defun write-table-selection (gadget)
  "Select GADGET's Lisp value in its table."
  (let ((view (mirror-sap gadget))
        (set (objc-get-id (objc-get-class "NSMutableIndexSet") "indexSet")))
    (loop for item in (gadget-items gadget)
          for index from 0
          when (item-selected? gadget item (gadget-value gadget))
            do (objc-set-long set "addIndex:" index))
    (alien-funcall *void-id-bool* view
                   (selector "selectRowIndexes:byExtendingSelection:")
                   set 0)))

(defun write-popup (gadget)
  "Fill GADGET's popup from its items and select its value."
  (let ((view (mirror-sap gadget)))
    (objc-void view (selector "removeAllItems"))
    (dolist (item (gadget-items gadget))
      (objc-set-id view "addItemWithTitle:" (ns-string (item-name gadget item))))
    (let ((index (item-index gadget (gadget-value gadget))))
      (when index
        (objc-set-long view "selectItemAtIndex:" index)))))

(defun refresh-mirror (sheet)
  "Copy SHEET's Lisp state into its native control."
  (let ((*mirror-silent* sheet))
    (cond
      ((typep sheet 'text-field)
       (write-field-string sheet (gadget-value sheet)))
      ((typep sheet 'label-pane)
       (write-field-string sheet (or (gadget-label sheet) "")))
      ((or (typep sheet 'push-button) (typep sheet 'toggle-button))
       (write-button-title sheet)))
    (when (typep sheet 'toggle-button)
      (write-toggle sheet))
    (when (typep sheet 'slider)
      (write-slider sheet))
    (when (typep sheet 'list-pane)
      (objc-void (mirror-sap sheet) (selector "reloadData"))
      (write-table-selection sheet))
    (when (typep sheet 'option-pane)
      (write-popup sheet))))

(defun place-mirror (sheet)
  "Move SHEET's view to its place in the top-level window."
  (let ((root (mirror-root sheet))
        (mirror (sheet-mirror sheet)))
    (when (and root mirror (not (eq sheet root)))
      (multiple-value-bind (x y) (sheet-in-root sheet root)
        (multiple-value-bind (width height) (sheet-pixel-size sheet)
          (fitz-set-frame (sb-sys:int-sap mirror)
                          (coerce x 'double-float)
                          (coerce y 'double-float)
                          (coerce width 'double-float)
                          (coerce height 'double-float)))))))

(defun realize-sheet (sheet root)
  "Create SHEET's native view when it is a control or an application pane."
  (let ((class (mirror-class sheet)))
    (when (and class (null (sheet-mirror sheet)))
      (multiple-value-bind (x y) (sheet-in-root sheet root)
        (multiple-value-bind (width height) (sheet-pixel-size sheet)
          (let ((view (fitz-make-view class
                                      (coerce x 'double-float)
                                      (coerce y 'double-float)
                                      (coerce width 'double-float)
                                      (coerce height 'double-float))))
            (unless (live-sap? view)
              (error "The view for ~S could not be created." sheet))
            (configure-mirror view sheet)
            (remember-mirror sheet (sb-sys:sap-int view))
            (objc-set-id (objc-get-id (mirror-sap root) "contentView")
                         "addSubview:" view)
            (when (and (typep sheet 'application-pane)
                       (fboundp 'ensure-cocoa-medium))
              (funcall 'ensure-cocoa-medium sheet))
            (when (and (typep sheet 'opengl-pane)
                       (fboundp 'realize-opengl-pane))
              (funcall 'realize-opengl-pane sheet))
            (refresh-mirror sheet)))))))

(defun realize-frame-sheets (frame)
  "Create native views for FRAME's controls and place them."
  (let ((root (frame-top-level-sheet frame)))
    (map-over-sheets (lambda (sheet) (realize-sheet sheet root)) root)
    (map-over-sheets #'place-mirror root)))

(defun release-child-mirrors (top)
  "Release the views under TOP.  TOP's window is left to close."
  (map-over-sheets
   (lambda (descendant)
     (let ((mirror (sheet-mirror descendant)))
       (when (and mirror (not (eq descendant top)))
         (objc-void (sb-sys:int-sap mirror) (selector "release"))))
     (forget-mirror descendant)
     (forget-target descendant))
   top))

(defun destroy-mirrors (sheet)
  "Remove the native views of SHEET and its descendants."
  (map-over-sheets
   (lambda (descendant)
     (when (and (typep descendant 'opengl-pane)
                (fboundp 'release-opengl-pane))
       (funcall 'release-opengl-pane descendant))
     (let ((mirror (sheet-mirror descendant)))
       (when mirror
         (when (sheet-parent descendant)
           (objc-void (sb-sys:int-sap mirror) (selector "removeFromSuperview"))
           (objc-void (sb-sys:int-sap mirror) (selector "release")))
         (forget-mirror descendant)
         (forget-target descendant))))
   sheet))

(defmethod allocate-space :after ((pane sheet) width height)
  (when (sheet-mirror pane)
    (if (null (sheet-parent pane))
        (fitz-set-content-size (mirror-sap pane)
                               (coerce (max 1 (round width)) 'double-float)
                               (coerce (max 1 (round height)) 'double-float))
        (place-mirror pane))))

(defmethod sheet-disown-child :after ((sheet sheet) (child sheet))
  (declare (ignore sheet))
  (with-cocoa ()
    (destroy-mirrors child)))

(defmethod sheet-adopt-child :after ((sheet sheet) (child sheet))
  (let ((root (mirror-root sheet)))
    (when root
      (with-cocoa ()
        (map-over-sheets (lambda (descendant)
                           (realize-sheet descendant root))
                         child)
        (map-over-sheets #'place-mirror child)))))

(defclass cocoa-port (port)
  ()
  (:documentation "The Cocoa port.  Its event queue is the application's."))

(defclass cocoa-frame-manager (frame-manager)
  ()
  (:documentation
   "A frame manager that realizes frames as Cocoa windows and gadgets
as native controls.  Layout panes have no view of their own.  Their
children are parented to the frame's content view."))

(defun make-cocoa-frame-manager ()
  "Return a frame manager that realizes frames on macOS."
  (ensure-cocoa-application)
  (ensure-target-methods)
  (make-instance 'cocoa-frame-manager :port (make-instance 'cocoa-port)))

(setf *frame-manager-maker* #'make-cocoa-frame-manager)

(defun install-content-view (window width height)
  "Give WINDOW a flipped content view of WIDTH by HEIGHT."
  (let ((content (fitz-make-view "FitzCLIMView" 0d0 0d0
                                 (coerce width 'double-float)
                                 (coerce height 'double-float))))
    (unless (live-sap? content)
      (error "The content view could not be created."))
    (objc-set-id window "setContentView:" content)
    content))

(defun cocoa-events-function (name)
  "Return the function named NAME in the Cocoa events package, or NIL."
  (let ((package (find-package :fitzclim/cocoa/events)))
    (when package
      (let ((symbol (find-symbol name package)))
        (when (and symbol (fboundp symbol))
          (symbol-function symbol))))))

(defmethod adopt-frame ((manager cocoa-frame-manager) (frame application-frame))
  (ensure-cocoa-application)
  (ensure-target-methods)
  (with-cocoa ()
    (let ((top (or (frame-top-level-sheet frame)
                   (error "The frame has no panes."))))
      (when (sheet-mirror top)
        (error "The frame is already adopted."))
      (let* ((width (round (frame-width frame)))
             (height (round (frame-height frame)))
             (window (fitz-make-window 40d0 40d0
                                       (coerce width 'double-float)
                                       (coerce height 'double-float)
                                       (frame-pretty-name frame))))
        (unless (live-sap? window)
          (error "The frame window could not be created."))
        (install-content-view window width height)
        (objc-set-id window "orderOut:" (sb-sys:int-sap 0))
        (remember-mirror top (sb-sys:sap-int window))
        (setf (mirror-frame (sb-sys:sap-int window)) frame)
        (setf (frame-port frame) (frame-manager-port manager))
        (push frame (frame-manager-frames manager))
        (handler-case
            (progn
              (realize-frame-sheets frame)
              (let ((install (cocoa-events-function "INSTALL-COCOA-MENU")))
                (when install
                  (funcall install frame))))
          (error (condition)
            (disown-frame manager frame)
            (error condition))))))
  frame)

(defmethod disown-frame ((manager cocoa-frame-manager) (frame application-frame))
  (let ((forget (cocoa-events-function "FORGET-COCOA-MENU")))
    (when forget
      (funcall forget frame)))
  (with-cocoa ()
    (let ((top (frame-top-level-sheet frame)))
      (when top
        (when (fboundp 'release-opengl-pane)
          (map-over-sheets
           (lambda (descendant)
             (when (typep descendant 'opengl-pane)
               (funcall 'release-opengl-pane descendant)))
           top))
        (let ((window (sheet-mirror top)))
          (when window
            (objc-void (sb-sys:int-sap window) (selector "close"))
            (forget-mirror-frame window)))
        (release-child-mirrors top)))
    (setf (frame-manager-frames manager)
          (remove frame (frame-manager-frames manager) :test #'eq))
    (forget-frame-port frame))
  frame)

(defmethod enable-frame ((manager cocoa-frame-manager) (frame application-frame))
  (declare (ignore manager))
  (with-cocoa ()
    (objc-void (mirror-sap (frame-top-level-sheet frame))
               (selector "orderFrontRegardless")))
  frame)

(defmethod disable-frame ((manager cocoa-frame-manager) (frame application-frame))
  (declare (ignore manager))
  (with-cocoa ()
    (objc-set-id (mirror-sap (frame-top-level-sheet frame))
                 "orderOut:" (sb-sys:int-sap 0)))
  frame)

(defmethod process-next-event ((port cocoa-port) &key wait-function timeout)
  "Dispatch one queued Cocoa event.  This does not wait."
  (declare (ignore port wait-function timeout))
  (with-cocoa ()
    (with-alien ((kind int))
      (plusp (fitz-poll-send (addr kind))))))

(defmethod (setf gadget-value) :after
    (value (gadget text-field) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (write-field-string gadget (gadget-value gadget))))))

(defun mirror-label (gadget label)
  "Show LABEL on GADGET's native control."
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (if (typep gadget 'label-pane)
            (write-field-string gadget (or label ""))
            (write-button-title gadget))))))

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
      (with-cocoa ()
        (let ((*mirror-silent* gadget))
          (write-toggle gadget))))
    result))

(defmethod (setf gadget-value) :after
    (value (gadget slider) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (write-slider gadget)))))

(defmethod (setf gadget-value) :after
    (value (gadget list-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (write-table-selection gadget)))))

(defmethod (setf gadget-value) :after
    (value (gadget option-pane) &key invoke-callback)
  (declare (ignore value invoke-callback))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (let ((index (item-index gadget (gadget-value gadget))))
          (when index
            (objc-set-long (mirror-sap gadget) "selectItemAtIndex:" index)))))))

(defmethod (setf gadget-items) :after (items (gadget list-pane))
  (declare (ignore items))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (objc-void (mirror-sap gadget) (selector "reloadData"))
        (write-table-selection gadget)))))

(defmethod (setf gadget-items) :after (items (gadget option-pane))
  (declare (ignore items))
  (when (and (sheet-mirror gadget) (not (eq *mirror-silent* gadget)))
    (with-cocoa ()
      (let ((*mirror-silent* gadget))
        (write-popup gadget)))))

(defmethod note-gadget-activated :after (client (gadget gadget))
  (declare (ignore client))
  (when (sheet-mirror gadget)
    (with-cocoa ()
      (objc-set-bool (mirror-sap gadget) "setEnabled:" t))))

(defmethod note-gadget-deactivated :after (client (gadget gadget))
  (declare (ignore client))
  (when (sheet-mirror gadget)
    (with-cocoa ()
      (objc-set-bool (mirror-sap gadget) "setEnabled:" nil))))

(defun view-title (sheet)
  "Return the title of SHEET's window or button."
  (with-cocoa ()
    (ns-text (objc-get-id (mirror-sap sheet) "title"))))

(defun view-string (sheet)
  "Return the string of SHEET's text field."
  (with-cocoa ()
    (ns-text (objc-get-id (mirror-sap sheet) "stringValue"))))

(defun view-double (sheet)
  "Return the numeric value of SHEET's slider."
  (with-cocoa ()
    (objc-get-double (mirror-sap sheet) "doubleValue")))

(defun view-state (sheet)
  "Return the checked state of SHEET's button."
  (with-cocoa ()
    (objc-get-long (mirror-sap sheet) "state")))

(defun view-origin (sheet)
  "Return the origin of SHEET's view in its superview."
  (with-cocoa ()
    (with-alien ((x double) (y double) (width double) (height double))
      (fitz-view-frame (mirror-sap sheet)
                       (addr x) (addr y) (addr width) (addr height))
      (values x y))))

(defun view-window-mirror (sheet)
  "Return the window pointer of SHEET's view, as an integer."
  (with-cocoa ()
    (sb-sys:sap-int (objc-get-id (mirror-sap sheet) "window"))))

(defun view-flipped? (sheet)
  "Return true when SHEET's view has a top-left origin."
  (with-cocoa ()
    (= 1 (objc-get-bool (mirror-sap sheet) "isFlipped"))))

(defun frame-visible-value (frame)
  "Return 1 when FRAME's window is on screen, otherwise 0."
  (with-cocoa ()
    (objc-get-bool (mirror-sap (frame-top-level-sheet frame)) "isVisible")))

(defun click-sheet (sheet)
  "Click SHEET's button."
  (with-cocoa ()
    (objc-set-id (mirror-sap sheet) "performClick:" (sb-sys:int-sap 0))))

(defun view-enabled? (sheet)
  "Return true when SHEET's control accepts input."
  (with-cocoa ()
    (= 1 (objc-get-bool (mirror-sap sheet) "isEnabled"))))

(defun send-control-action (sheet)
  "Send SHEET's action.  A disabled control still receives this call."
  (with-cocoa ()
    (alien-funcall *void-id* (sheet-target sheet)
                   (selector "clicked:")
                   (mirror-sap sheet))))

(defun replace-field-text (sheet text)
  "Put TEXT in SHEET's field and take that string as its value."
  (with-cocoa ()
    (write-field-string sheet text)
    (accept-field sheet)))

(defun choose-slider-value (sheet value)
  "Put VALUE in SHEET's slider and take that number as its value."
  (with-cocoa ()
    (objc-set-double (mirror-sap sheet) "setDoubleValue:" value)
    (accept-slider sheet)))

(defun choose-table-indexes (sheet indexes)
  "Select INDEXES in SHEET's table and take that selection as its value."
  (with-cocoa ()
    (let ((set (objc-get-id (objc-get-class "NSMutableIndexSet") "indexSet")))
      (dolist (index indexes)
        (objc-set-long set "addIndex:" index))
      (alien-funcall *void-id-bool* (mirror-sap sheet)
                     (selector "selectRowIndexes:byExtendingSelection:")
                     set 0))
    (accept-table sheet)))

(defun choose-popup-index (sheet index)
  "Select INDEX in SHEET's popup and take that item as its value."
  (with-cocoa ()
    (objc-set-long (mirror-sap sheet) "selectItemAtIndex:" index)
    (accept-popup sheet)))

(defun wakeup-frame (frame)
  "Post an application event so the next poll has something to dispatch."
  (with-cocoa ()
    (fitz-post-wakeup (mirror-sap (frame-top-level-sheet frame)))))

(behavior 'make-cocoa-frame-manager
  (let ((manager (make-cocoa-frame-manager)))
    (should-be-true (typep manager 'cocoa-frame-manager))
    (should-be-true (typep (frame-manager-port manager) 'cocoa-port))
    (should-be-true (typep (frame-manager-port manager) 'port))
    (should-be-null (frame-manager-frames manager))
    (should-eq #'make-cocoa-frame-manager *frame-manager-maker*)))

(define-application-frame cocoa-gadget-frame ()
  ()
  (:pretty-name "Cocoa Gadgets")
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

(behavior 'cocoa-gadgets
  (setf *cocoa-error* nil)
  (let ((manager (make-cocoa-frame-manager))
        (frame (make-application-frame 'cocoa-gadget-frame))
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
             (should-be-true (sheet-mirror top))
             (should-equal "Cocoa Gadgets" (view-title top))
             (should-be-null (sheet-mirror (frame-panes frame)))
             (should-be-null (sheet-mirror radio))
             (should-be-null (sheet-mirror checks))
             (should-equal "Call sign" (view-string banner))
             (should-equal "Fitz" (view-string name))
             (should-equal "Go" (view-title go))
             (should= (sheet-mirror top) (view-window-mirror go))
             (should-be-true (sheet-mirror canvas))
             (should-be-true (view-flipped? canvas))
             (multiple-value-bind (x y) (view-origin name)
               (should= 0 x)
               (should= 18 y))
             (should= 4 (view-double speed))
             (setf (gadget-value name) "Roy")
             (should-equal "Roy" (view-string name))
             (replace-field-text name "Henry")
             (should-equal "Henry" (gadget-value name))
             (setf (gadget-activate-callback go)
                   (lambda (gadget)
                     (declare (ignore gadget))
                     (setf clicked t)))
             (deactivate-gadget go)
             (should-be-false (view-enabled? go))
             (send-control-action go)
             (should-be-null clicked)
             (activate-gadget go)
             (should-be-true (view-enabled? go))
             (click-sheet go)
             (should-be-true clicked)
             (choose-slider-value speed 2)
             (should= 2 (gadget-value speed))
             (choose-table-indexes choice '(1))
             (should-equal "two" (gadget-value choice))
             (choose-table-indexes many '(0 1))
             (should-equal '("a" "b") (gadget-value many))
             (choose-popup-index pick 1)
             (should-equal "blue" (gadget-value pick))
             (let ((buttons (sheet-children radio)))
               (click-sheet (second buttons))
               (should-eq (second buttons) (gadget-value radio))
               (should= 0 (view-state (first buttons)))
               (should= 1 (view-state (second buttons))))
             (let ((buttons (sheet-children checks)))
               (click-sheet (second buttons))
               (should-equal buttons (gadget-value checks)))
             (choose-slider-value scroller 5)
             (should= 5 (gadget-value scroller))
             (let ((port (frame-manager-port manager)))
               (dotimes (i 20)
                 (unless (process-next-event port)
                   (return)))
               (wakeup-frame frame)
               (should-be-true (process-next-event port)))
             (should= 0 (frame-visible-value frame))
             (enable-frame manager frame)
             (should= 1 (frame-visible-value frame))
             (disable-frame manager frame)
             (should= 0 (frame-visible-value frame))
             (when *cocoa-error*
               (error "A Cocoa callback failed: ~A" *cocoa-error*))
             (let ((window (sheet-mirror top)))
               (disown-frame manager frame)
               (should-be-null (sheet-mirror top))
               (should-be-null (mirror-sheet window))
               (should-be-null (mirror-frame window))
               (should-be-null (frame-manager-frames manager)))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))
