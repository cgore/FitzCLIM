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

(in-package :FitzCLIM/cocoa)

(defvar *cocoa-modifiers* 0
  "The modifier bits held by keys that are still down.")

(defvar *menu-commands* (make-hash-table :test #'eql)
  "A table from a menu item tag to a frame and a command name.")

(defvar *frame-menu-ids* (make-hash-table :test #'eq)
  "A table from a frame to the tags of its menu items.")

(defvar *frame-menus* (make-hash-table :test #'eq)
  "A table from a frame to its menu bar.")

(defvar *frame-command-menus* (make-hash-table :test #'eq)
  "A table from a frame to the menu that holds its commands.")

(defvar *next-menu-tag* 0
  "The last menu item tag handed out.")

(defvar *menu-target* nil
  "The object that receives menu actions.  setTarget: does not retain it.")

(defvar *add-menu-item*
  (extern-alien "objc_msgSend"
                (function system-area-pointer
                          system-area-pointer
                          system-area-pointer
                          system-area-pointer
                          system-area-pointer
                          system-area-pointer))
  "objc_msgSend for addItemWithTitle:action:keyEquivalent:.")

(defvar *void-id-id*
  (extern-alien "objc_msgSend"
                (function void
                          system-area-pointer
                          system-area-pointer
                          system-area-pointer
                          system-area-pointer))
  "objc_msgSend for a message with two object arguments.")

(defun modifier-bits (flags)
  "Return the CLIM modifier bits for Cocoa modifier FLAGS."
  (logior *cocoa-modifiers*
          (if (plusp (logand flags (ash 1 17))) +shift-key+ 0)
          (if (plusp (logand flags (ash 1 18))) +control-key+ 0)
          (if (plusp (logand flags (ash 1 19))) +meta-key+ 0)
          (if (plusp (logand flags (ash 1 20))) +super-key+ 0)))

(defun cocoa-key-name (code)
  "Return the CLIM key name for a Mac key code, or NIL.
Code 0 is the A key."
  (case code
    (#x7b :left)
    (#x7c :right)
    (#x7d :down)
    (#x7e :up)
    (#x24 :return)
    (#x30 :tab)
    (#x33 :delete)
    (#x35 :escape)
    (#x37 :super)
    (#x38 :shift)
    (#x3a :meta)
    (#x3b :control)
    (#x00 :a)
    (#x0b :b)
    (t nil)))

(defun key-modifier-bit (key-name)
  "Return the modifier bit KEY-NAME holds down, or 0."
  (case key-name
    (:shift +shift-key+)
    (:control +control-key+)
    (:meta +meta-key+)
    (:super +super-key+)
    (t 0)))

(defun pointer-event-class (type)
  "Return the event class and button for a Cocoa mouse TYPE."
  (case type
    (1 (values 'pointer-button-press-event :left))
    (2 (values 'pointer-button-release-event :left))
    (3 (values 'pointer-button-press-event :right))
    (4 (values 'pointer-button-release-event :right))
    (5 (values 'pointer-motion-event nil))
    (25 (values 'pointer-button-press-event :middle))
    (26 (values 'pointer-button-release-event :middle))
    (t (values nil nil))))

(defun note-cocoa-key (sheet class code character)
  "Queue a keyboard event and keep the modifier bits up to date."
  ;; Key code 0 is ANSI A.  A character is reported instead when one is present.
  (let* ((named (or (plusp code)
                    (and (zerop character) (cocoa-key-name code))))
         (name (if named (cocoa-key-name code) nil))
         (char (if named nil (and (plusp character) (code-char character))))
         (bit (key-modifier-bit name))
         (state (logior *cocoa-modifiers* bit)))
    (enqueue-event sheet
                   (make-instance class
                                  :sheet sheet
                                  :key-name name
                                  :character char
                                  :modifier-state state))
    (cond
      ((and (plusp bit) (eq class 'key-press-event))
       (setf *cocoa-modifiers* state))
      ((and (plusp bit) (eq class 'key-release-event))
       (setf *cocoa-modifiers* (logandc2 *cocoa-modifiers* bit))))))

(defun queue-menu-command (tag)
  "Queue the command named by menu item TAG."
  (let ((entry (gethash tag *menu-commands*)))
    (when entry
      (destructuring-bind (frame . name) entry
        (setf (frame-pending-commands frame)
              (append (frame-pending-commands frame) (list name)))))))

(define-alien-callable cocoa-mouse void
    ((self system-area-pointer)
     (selector system-area-pointer)
     (event system-area-pointer))
  (declare (ignore selector))
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (let ((sheet (mirror-sheet (sb-sys:sap-int self))))
          (when sheet
            (with-alien ((x double) (y double))
              (fitz-event-point event self (addr x) (addr y))
              (multiple-value-bind (class button)
                  (pointer-event-class (fitz-event-type event))
                (when class
                  (let ((modifiers (modifier-bits (fitz-event-modifiers event))))
                    (enqueue-event sheet
                                   (if button
                                       (make-instance class
                                                      :sheet sheet
                                                      :x x
                                                      :y y
                                                      :modifier-state modifiers
                                                      :button button)
                                       (make-instance class
                                                      :sheet sheet
                                                      :x x
                                                      :y y
                                                      :modifier-state modifiers)))))))))
      (error (condition)
        (setf *cocoa-error* condition))))
  nil)

(define-alien-callable cocoa-key void
    ((self system-area-pointer)
     (selector system-area-pointer)
     (event system-area-pointer))
  (declare (ignore selector))
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (let ((sheet (mirror-sheet (sb-sys:sap-int self)))
              (type (fitz-event-type event)))
          (when sheet
            (note-cocoa-key sheet
                            (if (= type 11)
                                'key-release-event
                                'key-press-event)
                            (fitz-event-key-code event)
                            (fitz-event-char event))))
      (error (condition)
        (setf *cocoa-error* condition))))
  nil)

(define-alien-callable cocoa-menu-clicked void
    ((self system-area-pointer)
     (selector system-area-pointer)
     (sender system-area-pointer))
  (declare (ignore self selector))
  (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
    (handler-case
        (queue-menu-command (objc-get-long sender "tag"))
      (error (condition)
        (setf *cocoa-error* condition))))
  nil)

(defun ensure-view-events ()
  "Add pointer and keyboard methods to the view class."
  (ensure-cocoa-classes)
  (let ((class (objc-get-class "FitzCLIMView"))
        (target (objc-get-class "FitzCLIMTarget")))
    (dolist (name '("mouseDown:" "mouseUp:" "rightMouseDown:" "rightMouseUp:"
                    "otherMouseDown:" "otherMouseUp:" "mouseMoved:"))
      (add-method-once class name "v@:@" 'cocoa-mouse))
    (add-method-once class "keyDown:" "v@:@" 'cocoa-key)
    (add-method-once class "keyUp:" "v@:@" 'cocoa-key)
    (add-method-once target "menuClicked:" "v@:@" 'cocoa-menu-clicked)))

(defun menu-target ()
  "Return the shared menu target, creating it on the first call."
  (or *menu-target*
      (setf *menu-target* (objc-new "FitzCLIMTarget"))))

(defun show-remaining-frame-menu ()
  "Show one remaining frame menu, when the menu bar was just cleared."
  (maphash (lambda (candidate bar)
             (declare (ignore candidate))
             (when (live-sap? bar)
               (objc-set-id *cocoa-application* "setMainMenu:" bar)
               (return-from show-remaining-frame-menu)))
           *frame-menus*)
  nil)

(defun install-cocoa-menu (frame)
  "Give FRAME a menu bar of the commands that have a menu string.
The bar is installed as the application menu.  A frame with no such
commands keeps no menu."
  (let ((items (remove-if-not #'stringp
                              (command-table-menu (frame-command-table frame))
                              :key #'first)))
    (when items
      (with-cocoa ()
        (ensure-view-events)
        (let ((commands (alien-funcall *id-id*
                                       (objc-id (objc-get-class "NSMenu")
                                                (selector "alloc"))
                                       (selector "initWithTitle:")
                                       (ns-string (frame-pretty-name frame))))
              (bar (alien-funcall *id-id*
                                  (objc-id (objc-get-class "NSMenu")
                                           (selector "alloc"))
                                  (selector "initWithTitle:")
                                  (ns-string (frame-pretty-name frame))))
              (tags nil))
          (unless (and (live-sap? commands) (live-sap? bar))
            (error "The frame menu could not be created."))
          (setf (gethash frame *frame-command-menus*) commands)
          (setf (gethash frame *frame-menus*) bar)
          (objc-set-bool commands "setAutoenablesItems:" nil)
          (objc-set-bool bar "setAutoenablesItems:" nil)
          (dolist (item items)
            (let* ((tag (incf *next-menu-tag*))
                   (entry (alien-funcall *add-menu-item*
                                         commands
                                         (selector "addItemWithTitle:action:keyEquivalent:")
                                         (ns-string (first item))
                                         (selector "menuClicked:")
                                         (ns-string ""))))
              (unless (live-sap? entry)
                (error "The menu item ~A could not be created." (first item)))
              (objc-set-id entry "setTarget:" (menu-target))
              (objc-set-long entry "setTag:" tag)
              (setf (gethash tag *menu-commands*) (cons frame (second item)))
              (push tag tags)))
          (let ((top (alien-funcall *add-menu-item*
                                    bar
                                    (selector "addItemWithTitle:action:keyEquivalent:")
                                    (ns-string (frame-pretty-name frame))
                                    (sb-sys:int-sap 0)
                                    (ns-string ""))))
            (unless (live-sap? top)
              (error "The menu bar item could not be created."))
            (alien-funcall *void-id-id*
                           bar
                           (selector "setSubmenu:forItem:")
                           commands
                           top))
          (objc-set-id *cocoa-application* "setMainMenu:" bar)
          (setf (gethash frame *frame-menu-ids*) (nreverse tags)))))))

(defun forget-cocoa-menu (frame)
  "Drop FRAME's menu bar and its item tags.
When that bar is showing, another adopted frame's menu takes its place."
  (with-cocoa ()
    (let ((bar (gethash frame *frame-menus*))
          (commands (gethash frame *frame-command-menus*))
          (detached nil))
      (when (and (live-sap? bar)
                 (live-sap? *cocoa-application*))
        (let ((current (objc-get-id *cocoa-application* "mainMenu")))
          (when (and (live-sap? current)
                     (= (sb-sys:sap-int bar) (sb-sys:sap-int current)))
            (objc-set-id *cocoa-application* "setMainMenu:" (sb-sys:int-sap 0))
            (setf detached t))))
      (when (live-sap? bar)
        (remhash frame *frame-menus*)
        (objc-void bar (selector "release")))
      (when (live-sap? commands)
        (remhash frame *frame-command-menus*)
        (objc-void commands (selector "release")))
      (when detached
        (show-remaining-frame-menu))))
  (dolist (tag (gethash frame *frame-menu-ids*))
    (remhash tag *menu-commands*))
  (remhash frame *frame-menu-ids*)
  nil)

(ensure-view-events)

(defun send-mouse (sheet type x y modifiers)
  "Deliver a mouse event to SHEET.  TYPE is a Cocoa event type."
  (with-cocoa ()
    (fitz-send-mouse (mirror-sap sheet) type
                     (coerce x 'double-float)
                     (coerce y 'double-float)
                     modifiers)))

(defun send-key (sheet type code character modifiers)
  "Deliver a key event to SHEET.  TYPE 10 is down and type 11 is up."
  (with-cocoa ()
    (fitz-send-key (mirror-sap sheet) type code character modifiers)))

(defun perform-menu-item (frame index)
  "Run the command item at INDEX in FRAME's menu."
  (with-cocoa ()
    (objc-set-long (gethash frame *frame-command-menus*)
                   "performActionForItemAtIndex:"
                   index)))

(define-application-frame cocoa-event-frame ()
  ()
  (:pretty-name "Cocoa Events")
  (:panes
   (canvas :application :width 80 :height 60))
  (:layouts
   (default (progn canvas)))
  (:geometry 80 60))

(behavior 'cocoa-events
  (setf *cocoa-error* nil)
  (setf *cocoa-modifiers* 0)
  (let ((manager (make-cocoa-frame-manager))
        (frame (make-application-frame 'cocoa-event-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((canvas (find-pane-named frame 'canvas))
                 (port (frame-manager-port manager)))
             (send-mouse canvas 1 3 4 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-press-event))
               (should-eq canvas (event-sheet event))
               (should= 3 (pointer-event-x event))
               (should= 4 (pointer-event-y event))
               (should-eq :left (pointer-event-button event))
               (should= 0 (event-modifier-state event))
               (should-be-true (event-matches-gesture-name-p event :select)))
             (send-mouse canvas 1 1 2 (ash 1 17))
             (should= +shift-key+
                      (event-modifier-state (port-next-event port)))
             (send-mouse canvas 1 1 2 (ash 1 18))
             (should= +control-key+
                      (event-modifier-state (port-next-event port)))
             (send-mouse canvas 1 1 2 (ash 1 19))
             (should= +meta-key+
                      (event-modifier-state (port-next-event port)))
             (send-mouse canvas 1 1 2 (ash 1 20))
             (should= +super-key+
                      (event-modifier-state (port-next-event port)))
             (send-mouse canvas 4 5 6 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-release-event))
               (should-eq :right (pointer-event-button event))
               (should= 5 (pointer-event-x event)))
             (send-mouse canvas 2 0 0 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-release-event))
               (should-eq :left (pointer-event-button event)))
             (send-mouse canvas 3 9 9 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-press-event))
               (should-eq :right (pointer-event-button event)))
             (send-mouse canvas 25 0 0 0)
             (should-eq :middle
                        (pointer-event-button (port-next-event port)))
             (send-mouse canvas 26 0 0 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-release-event))
               (should-eq :middle (pointer-event-button event)))
             (send-mouse canvas 5 7 8 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-motion-event))
               (should= 7 (pointer-event-x event))
               (should= 8 (pointer-event-y event)))
             (send-key canvas 10 #x7b 0 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'key-press-event))
               (should-eq :left (keyboard-event-key-name event))
               (should-be-null (keyboard-event-character event))
               (should-eq canvas (event-sheet event)))
             (send-key canvas 10 0 65 0)
             (should-eq #\A
                        (keyboard-event-character (port-next-event port)))
             (send-key canvas 10 #x38 0 0)
             (send-key canvas 10 #x0b 0 0)
             (let ((shift (port-next-event port))
                   (letter (port-next-event port)))
               (should-eq :shift (keyboard-event-key-name shift))
               (should= +shift-key+ (event-modifier-state letter))
               (should-eq :b (keyboard-event-key-name letter)))
             (send-key canvas 11 #x38 0 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'key-release-event))
               (should-eq :shift (keyboard-event-key-name event))
               (should= 0 *cocoa-modifiers*))
             (send-key canvas 10 #x7c 0 0)
             (should-eq :right
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x7d 0 0)
             (should-eq :down
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x7e 0 0)
             (should-eq :up
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x24 0 0)
             (should-eq :return
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x30 0 0)
             (should-eq :tab
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x33 0 0)
             (should-eq :delete
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x35 0 0)
             (should-eq :escape
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x00 0 0)
             (should-eq :a
                       (keyboard-event-key-name (port-next-event port)))
             (send-key canvas 10 #x3b 0 0)
             (let ((event (port-next-event port)))
               (should-eq :control (keyboard-event-key-name event))
               (should= +control-key+ (event-modifier-state event)))
             (send-key canvas 11 #x3b 0 0)
             (port-next-event port)
             (should= 0 *cocoa-modifiers*)
             (send-key canvas 10 #x3a 0 0)
             (let ((event (port-next-event port)))
               (should-eq :meta (keyboard-event-key-name event))
               (should= +meta-key+ (event-modifier-state event)))
             (send-key canvas 11 #x3a 0 0)
             (port-next-event port)
             (should= 0 *cocoa-modifiers*)
             (send-key canvas 10 #x37 0 0)
             (let ((event (port-next-event port)))
               (should-eq :super (keyboard-event-key-name event))
               (should= +super-key+ (event-modifier-state event)))
             (send-key canvas 11 #x37 0 0)
             (port-next-event port)
             (should= 0 *cocoa-modifiers*)
             (should-be-null (port-next-event port))
             (when *cocoa-error*
               (error "A Cocoa callback failed: ~A" *cocoa-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame cocoa-pick-frame ()
  ((word
    :initform nil
    :accessor picked-word))
  (:pretty-name "Cocoa Pick")
  (:panes
   (canvas :application :width 120 :height 40))
  (:layouts
   (default (progn canvas)))
  (:geometry 120 40))

(define-command (com-pick-word :command-table cocoa-pick-frame :name t)
    ((word string))
  "Remember WORD on the current frame and return it."
  (setf (picked-word *application-frame*) word))

(define-presentation-to-command-translator pick-cocoa-word
    (string com-pick-word cocoa-pick-frame
     :gesture :select)
    (object)
  (list object))

(behavior 'cocoa-pointer-command
  (setf *cocoa-error* nil)
  (let ((manager (make-cocoa-frame-manager))
        (frame (make-application-frame 'cocoa-pick-frame)))
    (unwind-protect
         (progn
           (should-be-null (read-frame-command frame))
           (adopt-frame manager frame)
           (let ((canvas (find-pane-named frame 'canvas)))
             (setf (stream-cursor-position canvas) (values 2 10))
             (present "Nostromo" :stream canvas :type 'string)
             (send-mouse canvas 1 70 10 0)
             (should-be-null (read-frame-command frame))
             (setf (frame-pending-commands frame) '((com-pick-word "queued")))
             (send-mouse canvas 5 10 10 0)
             (send-mouse canvas 3 10 10 0)
             (send-mouse canvas 1 10 10 0)
             (should-equal "queued" (second (read-frame-command frame)))
             (let ((command (read-frame-command frame)))
               (should-eq 'com-pick-word (first command))
               (should-equal "Nostromo" (second command)))
             (send-mouse canvas 1 10 10 0)
             (should-eq frame (run-frame-top-level frame))
             (should-equal "Nostromo" (picked-word frame))
             (when *cocoa-error*
               (error "A Cocoa callback failed: ~A" *cocoa-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame cocoa-menu-frame ()
  ((total
    :initform 0
    :accessor menu-total))
  (:pretty-name "Cocoa Menu")
  (:panes
   (canvas :application :width 40 :height 20))
  (:layouts
   (default (progn canvas)))
  (:geometry 40 20))

(define-command (com-menu-add :command-table cocoa-menu-frame
                              :name t :menu "Add")
    ()
  "Add one to the frame total and return the new total."
  (incf (menu-total *application-frame*)))

(define-command (com-menu-hidden :command-table cocoa-menu-frame :name t)
    ()
  "Return the frame total.  This command is not on the menu."
  (menu-total *application-frame*))

(behavior 'cocoa-command-menu
  (setf *cocoa-error* nil)
  (let ((manager (make-cocoa-frame-manager))
        (frame (make-application-frame 'cocoa-menu-frame))
        (other (make-application-frame 'cocoa-menu-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((ids (gethash frame *frame-menu-ids*))
                 (bar (gethash frame *frame-menus*)))
             (should-be-true (live-sap? bar))
             (should= (sb-sys:sap-int bar)
                      (sb-sys:sap-int (objc-get-id *cocoa-application* "mainMenu")))
             (should= 1 (length ids))
             (let* ((top (alien-funcall *id-long*
                                        bar
                                        (selector "itemAtIndex:")
                                        0))
                    (commands (objc-get-id top "submenu"))
                    (item (alien-funcall *id-long*
                                         commands
                                         (selector "itemAtIndex:")
                                         0)))
               (should-equal "Cocoa Menu" (ns-text (objc-get-id top "title")))
               (should-equal "Add" (ns-text (objc-get-id item "title")))))
           (perform-menu-item frame 0)
           (should-eq frame (run-frame-top-level frame))
           (should= 1 (menu-total frame))
           (when *cocoa-error*
             (error "A Cocoa callback failed: ~A" *cocoa-error*))
           (adopt-frame manager other)
           (should= (sb-sys:sap-int (gethash other *frame-menus*))
                    (sb-sys:sap-int (objc-get-id *cocoa-application* "mainMenu")))
           (disown-frame manager other)
           (should= (sb-sys:sap-int (gethash frame *frame-menus*))
                    (sb-sys:sap-int (objc-get-id *cocoa-application* "mainMenu")))
           (should-be-null (gethash other *frame-menu-ids*))
           (disown-frame manager frame)
           (should-be-null (gethash frame *frame-menu-ids*))
           (should-be-false (live-sap? (objc-get-id *cocoa-application* "mainMenu"))))
      (when (member other (frame-manager-frames manager) :test #'eq)
        (disown-frame manager other))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))
