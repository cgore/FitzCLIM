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

(defpackage :FitzCLIM/cocoa
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
        :FitzCLIM/port)
  (:export :cocoa-frame-manager
           :cocoa-port
           :make-cocoa-frame-manager))
(in-package :FitzCLIM/cocoa)

(defvar *cocoa-libraries-loaded* nil
  "True after the Objective-C runtime and AppKit have been loaded.")

(defun load-cocoa-libraries ()
  "Load the Objective-C runtime, Foundation, and AppKit."
  (unless *cocoa-libraries-loaded*
    (load-shared-object "/usr/lib/libobjc.A.dylib")
    (load-shared-object "/System/Library/Frameworks/Foundation.framework/Foundation")
    (load-shared-object "/System/Library/Frameworks/AppKit.framework/AppKit")
    (setf *cocoa-libraries-loaded* t)))

(load-cocoa-libraries)

(define-alien-routine ("objc_getClass" objc-get-class) system-area-pointer
  (name c-string))
(define-alien-routine ("sel_registerName" sel-register-name) system-area-pointer
  (name c-string))
(define-alien-routine ("class_getInstanceMethod" class-get-instance-method)
    system-area-pointer
  (class system-area-pointer)
  (selector system-area-pointer))
(define-alien-routine ("objc_allocateClassPair" allocate-class-pair)
    system-area-pointer
  (superclass system-area-pointer)
  (name c-string)
  (extra long))
(define-alien-routine ("objc_registerClassPair" register-class-pair) void
  (class system-area-pointer))
(define-alien-routine ("class_addMethod" add-objc-method) int
  (class system-area-pointer)
  (selector system-area-pointer)
  (implementation system-area-pointer)
  (types c-string))
(define-alien-routine ("objc_msgSend" objc-id) system-area-pointer
  (self system-area-pointer)
  (selector system-area-pointer))
(define-alien-routine ("objc_msgSend" objc-void) void
  (self system-area-pointer)
  (selector system-area-pointer))
(define-alien-routine ("objc_msgSend" objc-long) long
  (self system-area-pointer)
  (selector system-area-pointer))
(define-alien-routine ("objc_msgSend" objc-double) double
  (self system-area-pointer)
  (selector system-area-pointer))
(define-alien-routine ("objc_msgSend" objc-text) c-string
  (self system-area-pointer)
  (selector system-area-pointer))

(defvar *void-id*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer
                          system-area-pointer)))
(defvar *void-long*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer long)))
(defvar *void-ulong*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer
                          unsigned-long)))
(defvar *void-bool*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer
                          (signed 8))))
(defvar *void-double*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer double)))
(defvar *bool-send*
  (extern-alien "objc_msgSend"
                (function (signed 8) system-area-pointer system-area-pointer)))
(defvar *id-text*
  (extern-alien "objc_msgSend"
                (function system-area-pointer system-area-pointer
                          system-area-pointer c-string)))
(defvar *id-long*
  (extern-alien "objc_msgSend"
                (function system-area-pointer system-area-pointer
                          system-area-pointer long)))
(defvar *id-id*
  (extern-alien "objc_msgSend"
                (function system-area-pointer system-area-pointer
                          system-area-pointer system-area-pointer)))
(defvar *void-id-bool*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer
                          system-area-pointer (signed 8))))
(defvar *void-id-sel-id*
  (extern-alien "objc_msgSend"
                (function void system-area-pointer system-area-pointer
                          system-area-pointer system-area-pointer
                          system-area-pointer)))
(defvar *ulong-ulong*
  (extern-alien "objc_msgSend"
                (function unsigned-long system-area-pointer system-area-pointer
                          unsigned-long)))

(defun cocoa-host-source ()
  "Return the path of the Objective-C host."
  (asdf:system-relative-pathname "fitzclim" "source/cocoa/host.m"))

(defun cocoa-host-library ()
  "Return the path of the compiled Cocoa host."
  (merge-pathnames "libfitzclim-cocoa.dylib"
                   (merge-pathnames ".cache/fitzclim/"
                                    (user-homedir-pathname))))

(defun cocoa-host-current? (source library)
  "Return true when LIBRARY is newer than SOURCE."
  (and (probe-file library)
       (>= (file-write-date library) (file-write-date source))))

(defun compile-cocoa-host (source library)
  "Compile SOURCE into LIBRARY.  Signal an error when clang fails."
  (ensure-directories-exist library)
  (let* ((output (make-string-output-stream))
         (process (sb-ext:run-program
                   "clang"
                   (list "-dynamiclib" "-fno-objc-arc"
                         "-framework" "AppKit"
                         "-framework" "Foundation"
                         "-o" (namestring library)
                         (namestring source))
                   :search t
                   :wait t
                   :output output
                   :error :output)))
    (unless (and (sb-ext:process-exit-code process)
                 (zerop (sb-ext:process-exit-code process)))
      (error "clang failed to build the Cocoa host:~%~A"
             (get-output-stream-string output)))))

(defvar *cocoa-host-loaded* nil
  "True after the compiled Cocoa host has been loaded.")

(defun ensure-cocoa-host ()
  "Compile the Cocoa host when it is stale, and load it."
  (let ((source (cocoa-host-source))
        (library (cocoa-host-library)))
    (unless (cocoa-host-current? source library)
      (compile-cocoa-host source library))
    (unless *cocoa-host-loaded*
      (load-shared-object (namestring library))
      (setf *cocoa-host-loaded* t))))

(defun selector (name)
  "Return the selector named NAME."
  (sel-register-name name))

(defun live-sap? (sap)
  "Return true when SAP is a non-null pointer."
  (and (sb-sys:system-area-pointer-p sap)
       (plusp (sb-sys:sap-int sap))))

(defun mirror-sap (sheet)
  "Return the pointer for SHEET's mirror."
  (sb-sys:int-sap (sheet-mirror sheet)))

(defun objc-new (class-name)
  "Return a new instance of CLASS-NAME."
  (objc-id (objc-id (objc-get-class class-name) (selector "alloc"))
           (selector "init")))

(defun ns-string (text)
  "Return an autoreleased string for TEXT."
  (alien-funcall *id-text* (objc-get-class "NSString")
                 (selector "stringWithUTF8String:")
                 (or text "")))

(defun ns-text (object)
  "Return the Lisp string of an NSString."
  (or (objc-text object (selector "UTF8String")) ""))

(defun objc-set-id (object selector-name argument)
  "Send SELECTOR-NAME to OBJECT with one object argument."
  (alien-funcall *void-id* object (selector selector-name) argument))

(defun objc-set-long (object selector-name argument)
  "Send SELECTOR-NAME to OBJECT with one integer argument."
  (alien-funcall *void-long* object (selector selector-name) argument))

(defun objc-set-ulong (object selector-name argument)
  "Send SELECTOR-NAME to OBJECT with one unsigned integer argument."
  (alien-funcall *void-ulong* object (selector selector-name) argument))

(defun objc-set-bool (object selector-name argument)
  "Send SELECTOR-NAME to OBJECT with one boolean argument."
  (alien-funcall *void-bool* object (selector selector-name)
                 (if argument 1 0)))

(defun objc-set-double (object selector-name argument)
  "Send SELECTOR-NAME to OBJECT with one double argument."
  (alien-funcall *void-double* object (selector selector-name)
                 (coerce argument 'double-float)))

(defun objc-get-id (object selector-name)
  "Return the object SELECTOR-NAME reads from OBJECT."
  (objc-id object (selector selector-name)))

(defun objc-get-long (object selector-name)
  "Return the integer SELECTOR-NAME reads from OBJECT."
  (objc-long object (selector selector-name)))

(defun objc-get-bool (object selector-name)
  "Return the boolean SELECTOR-NAME reads from OBJECT."
  (alien-funcall *bool-send* object (selector selector-name)))

(defun objc-get-double (object selector-name)
  "Return the double SELECTOR-NAME reads from OBJECT."
  (objc-double object (selector selector-name)))

(defmacro with-cocoa (&body body)
  "Run BODY with floating-point traps masked and an autorelease pool."
  (let ((pool (gensym "POOL")))
    `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero :inexact)
       (let ((,pool (objc-new "NSAutoreleasePool")))
         (unwind-protect
              (progn ,@body)
           (objc-void ,pool (selector "drain")))))))

(defun add-method-once (class name encoding callable)
  "Add CALLABLE as NAME on CLASS when that class has no such method.
An inherited method does not count.  The class keeps the first
implementation registered in this process."
  (add-objc-method class (selector name)
                   (alien-sap (alien-callable-function callable))
                   encoding))

(define-alien-callable cocoa-flipped (signed 8)
    ((self system-area-pointer)
     (selector system-area-pointer))
  (declare (ignore self selector))
  1)

(defvar *cocoa-classes-ready* nil
  "True after FitzCLIMView and FitzCLIMTarget have been registered.")

(defun ensure-cocoa-class (name superclass)
  "Return the class NAME, creating it as a subclass of SUPERCLASS."
  (let ((existing (objc-get-class name)))
    (if (live-sap? existing)
        existing
        (let ((class (allocate-class-pair (objc-get-class superclass) name 0)))
          (unless (live-sap? class)
            (error "objc_allocateClassPair failed for ~A." name))
          (when (string= name "FitzCLIMView")
            (add-method-once class "isFlipped" "c@:" 'cocoa-flipped))
          (register-class-pair class)
          class))))

(defun ensure-cocoa-classes ()
  "Register the view and target classes."
  (unless *cocoa-classes-ready*
    (ensure-cocoa-class "FitzCLIMView" "NSView")
    (ensure-cocoa-class "FitzCLIMTarget" "NSObject")
    (setf *cocoa-classes-ready* t)))

(defvar *cocoa-application* nil
  "The shared NSApplication, or NIL before it has been started.")

(defvar *cocoa-ready* nil
  "True after the host, the classes, and the application are ready.")

(defun ensure-cocoa-application ()
  "Start the shared application as an accessory, without stealing focus."
  (unless *cocoa-ready*
    (ensure-cocoa-host)
    (ensure-cocoa-classes)
    (let ((application (objc-get-id (objc-get-class "NSApplication")
                                    "sharedApplication")))
      (unless (live-sap? application)
        (error "NSApplication could not be started."))
      (objc-set-long application "setActivationPolicy:" 1)
      (setf *cocoa-application* application
            *cocoa-ready* t))))

(ensure-cocoa-host)

(define-alien-routine ("fitz_make_window" fitz-make-window) system-area-pointer
  (x double) (y double) (width double) (height double) (title c-string))
(define-alien-routine ("fitz_make_view" fitz-make-view) system-area-pointer
  (class-name c-string)
  (x double) (y double) (width double) (height double))
(define-alien-routine ("fitz_set_frame" fitz-set-frame) void
  (view system-area-pointer)
  (x double) (y double) (width double) (height double))
(define-alien-routine ("fitz_view_frame" fitz-view-frame) void
  (view system-area-pointer)
  (x (* double)) (y (* double)) (width (* double)) (height (* double)))
(define-alien-routine ("fitz_set_content_size" fitz-set-content-size) void
  (window system-area-pointer)
  (width double) (height double))
(define-alien-routine ("fitz_event_point" fitz-event-point) void
  (event system-area-pointer)
  (view system-area-pointer)
  (x (* double)) (y (* double)))
(define-alien-routine ("fitz_event_type" fitz-event-type) int
  (event system-area-pointer))
(define-alien-routine ("fitz_event_modifiers" fitz-event-modifiers) unsigned-long
  (event system-area-pointer))
(define-alien-routine ("fitz_event_key_code" fitz-event-key-code) unsigned-short
  (event system-area-pointer))
(define-alien-routine ("fitz_event_char" fitz-event-char) unsigned-short
  (event system-area-pointer))
(define-alien-routine ("fitz_send_mouse" fitz-send-mouse) void
  (view system-area-pointer)
  (type int)
  (x double) (y double)
  (modifiers unsigned-long))
(define-alien-routine ("fitz_send_key" fitz-send-key) void
  (view system-area-pointer)
  (type int)
  (code unsigned-short)
  (character unsigned-short)
  (modifiers unsigned-long))
(define-alien-routine ("fitz_post_wakeup" fitz-post-wakeup) void
  (window system-area-pointer))
(define-alien-routine ("fitz_poll_send" fitz-poll-send) int
  (type (* int)))
