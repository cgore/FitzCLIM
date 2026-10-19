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

(defpackage :FitzCLIM/windows/gl
  (:use :common-lisp
        :sigma/behave
        :sb-alien
        :FitzCLIM/command
        :FitzCLIM/command/define-command
        :FitzCLIM/frame
        :FitzCLIM/pane
        :FitzCLIM/gadget
        :FitzCLIM/event
        :FitzCLIM/sheet
        :FitzCLIM/geometry
        :FitzCLIM/layout
        :FitzCLIM/medium
        :FitzCLIM/medium/drawing
        :FitzCLIM/output-record
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/opacity
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style
        :FitzCLIM/opengl
        :FitzCLIM/presentation
        :FitzCLIM/presentation/present
        :FitzCLIM/presentation/translator
        :FitzCLIM/port
        :FitzCLIM/windows)
  (:import-from :FitzCLIM/windows
                :*window-proc-error*
                :+wm-lbuttondown+
                :+wm-paint+
                :hwnd-sap
                :send-message))
(in-package :FitzCLIM/windows/gl)

(defvar *opengl-library-loaded* nil
  "True after opengl32.dll has been loaded.")

(defun load-opengl-library ()
  "Load opengl32.dll."
  (unless *opengl-library-loaded*
    (load-shared-object "opengl32.dll")
    (setf *opengl-library-loaded* t)))

(load-opengl-library)

(defparameter +gl-color-buffer-bit+ #x00004000)
(defparameter +gl-depth-buffer-bit+ #x00000100)
(defparameter +gl-front+ #x0404)
(defparameter +gl-rgba+ #x1908)
(defparameter +gl-unsigned-byte+ #x1401)
(defparameter +pfd-draw-to-window+ 4)
(defparameter +pfd-support-opengl+ #x20)
(defparameter +pfd-doublebuffer+ 1)

(define-alien-type pixel-format-descriptor
  (struct pixel-format-descriptor
    (size (unsigned 16))
    (version (unsigned 16))
    (flags (unsigned 32))
    (pixel-type (unsigned 8))
    (color-bits (unsigned 8))
    (red-bits (unsigned 8))
    (red-shift (unsigned 8))
    (green-bits (unsigned 8))
    (green-shift (unsigned 8))
    (blue-bits (unsigned 8))
    (blue-shift (unsigned 8))
    (alpha-bits (unsigned 8))
    (alpha-shift (unsigned 8))
    (accum-bits (unsigned 8))
    (accum-red-bits (unsigned 8))
    (accum-green-bits (unsigned 8))
    (accum-blue-bits (unsigned 8))
    (accum-alpha-bits (unsigned 8))
    (depth-bits (unsigned 8))
    (stencil-bits (unsigned 8))
    (aux-buffers (unsigned 8))
    (layer-type (unsigned 8))
    (reserved (unsigned 8))
    (layer-mask (unsigned 32))
    (visible-mask (unsigned 32))
    (damage-mask (unsigned 32))))

(define-alien-routine ("ChoosePixelFormat" choose-pixel-format) (signed 32)
  (hdc system-area-pointer)
  (descriptor (* pixel-format-descriptor)))
(define-alien-routine ("SetPixelFormat" set-pixel-format) (signed 32)
  (hdc system-area-pointer)
  (format (signed 32))
  (descriptor (* pixel-format-descriptor)))
(define-alien-routine ("SwapBuffers" swap-buffers) (signed 32)
  (hdc system-area-pointer))
(define-alien-routine ("wglCreateContext" wgl-create-context) system-area-pointer
  (hdc system-area-pointer))
(define-alien-routine ("wglMakeCurrent" wgl-make-current) (signed 32)
  (hdc system-area-pointer)
  (context system-area-pointer))
(define-alien-routine ("wglDeleteContext" wgl-delete-context) (signed 32)
  (context system-area-pointer))
(define-alien-routine ("glClearColor" gl-clear-color) void
  (red single-float)
  (green single-float)
  (blue single-float)
  (alpha single-float))
(define-alien-routine ("glClear" gl-clear) void
  (mask (unsigned 32)))
(define-alien-routine ("glViewport" gl-viewport) void
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32)))
(define-alien-routine ("glFlush" gl-flush) void)
(define-alien-routine ("glReadBuffer" gl-read-buffer) void
  (mode (unsigned 32)))
(define-alien-routine ("glReadPixels" gl-read-pixels) void
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32))
  (format (unsigned 32))
  (type (unsigned 32))
  (pixels system-area-pointer))

(defvar *opengl-contexts* (make-hash-table :test #'eq)
  "A table from an OpenGL pane to its HGLRC integer.")

(defun release-opengl-pane (pane)
  "Delete PANE's OpenGL context."
  (let ((context (gethash pane *opengl-contexts*)))
    (when context
      (wgl-make-current (sb-sys:int-sap 0) (sb-sys:int-sap 0))
      (wgl-delete-context (hwnd-sap context))
      (remhash pane *opengl-contexts*)))
  nil)

(defun call-with-opengl (pane function)
  "Make PANE's context current and call FUNCTION with its device context."
  (let ((context (gethash pane *opengl-contexts*))
        (hwnd (sheet-mirror pane)))
    (unless (and context hwnd)
      (error "The pane has no OpenGL context."))
    (let ((dc (get-dc (hwnd-sap hwnd))))
      (unwind-protect
          (progn
            (unless (plusp (wgl-make-current dc (hwnd-sap context)))
              (error "wglMakeCurrent failed (error ~D)." (get-last-error)))
            (funcall function dc))
        (wgl-make-current (sb-sys:int-sap 0) (sb-sys:int-sap 0))
        (release-dc (hwnd-sap hwnd) dc)))))

(defun redraw-opengl-pane (pane)
  "Clear PANE to its clear color and show that picture."
  (call-with-opengl pane
    (lambda (dc)
      (multiple-value-bind (width height) (sheet-pixel-size pane)
        (gl-viewport 0 0 width height))
      (gl-clear-color (coerce (opengl-clear-red pane) 'single-float)
                      (coerce (opengl-clear-green pane) 'single-float)
                      (coerce (opengl-clear-blue pane) 'single-float)
                      1.0f0)
      (gl-clear (logior +gl-color-buffer-bit+ +gl-depth-buffer-bit+))
      (gl-flush)
      (swap-buffers dc))))

(defun paint-opengl-pane (hwnd)
  "Validate HWND and redraw its OpenGL pane."
  (let* ((paint (make-alien (unsigned 8) 128))
         (sap (alien-sap paint)))
    (unwind-protect
        (progn
          (begin-paint (hwnd-sap hwnd) sap)
          (end-paint (hwnd-sap hwnd) sap))
      (free-alien paint)))
  (let ((pane (mirror-sheet hwnd)))
    (when (typep pane 'opengl-pane)
      (redraw-opengl-pane pane))))

(defun fill-pixel-format (descriptor)
  "Fill DESCRIPTOR with the format used for an OpenGL pane."
  (setf (slot descriptor 'size) 40
        (slot descriptor 'version) 1
        (slot descriptor 'flags) (logior +pfd-draw-to-window+
                                         +pfd-support-opengl+
                                         +pfd-doublebuffer+)
        (slot descriptor 'pixel-type) 0
        (slot descriptor 'color-bits) 24
        (slot descriptor 'red-bits) 0
        (slot descriptor 'red-shift) 0
        (slot descriptor 'green-bits) 0
        (slot descriptor 'green-shift) 0
        (slot descriptor 'blue-bits) 0
        (slot descriptor 'blue-shift) 0
        (slot descriptor 'alpha-bits) 8
        (slot descriptor 'alpha-shift) 0
        (slot descriptor 'accum-bits) 0
        (slot descriptor 'accum-red-bits) 0
        (slot descriptor 'accum-green-bits) 0
        (slot descriptor 'accum-blue-bits) 0
        (slot descriptor 'accum-alpha-bits) 0
        (slot descriptor 'depth-bits) 16
        (slot descriptor 'stencil-bits) 0
        (slot descriptor 'aux-buffers) 0
        (slot descriptor 'layer-type) 0
        (slot descriptor 'reserved) 0
        (slot descriptor 'layer-mask) 0
        (slot descriptor 'visible-mask) 0
        (slot descriptor 'damage-mask) 0)
  descriptor)

(defun realize-opengl-pane (pane)
  "Create an OpenGL context for PANE's window and draw the clear color."
  (unless (= 40 (alien-size pixel-format-descriptor :bytes))
    (error "PIXELFORMATDESCRIPTOR is ~D bytes, not 40."
           (alien-size pixel-format-descriptor :bytes)))
  (let* ((hwnd (hwnd-sap (sheet-mirror pane)))
         (dc (get-dc hwnd)))
    (unwind-protect
        (with-alien ((descriptor pixel-format-descriptor))
          (fill-pixel-format descriptor)
          (let ((format (choose-pixel-format dc (addr descriptor))))
            (unless (plusp format)
              (error "ChoosePixelFormat failed (error ~D)." (get-last-error)))
            (unless (plusp (set-pixel-format dc format (addr descriptor)))
              (error "SetPixelFormat failed (error ~D)." (get-last-error)))
            (let ((context (wgl-create-context dc)))
              (unless (plusp (sb-sys:sap-int context))
                (error "wglCreateContext failed (error ~D)." (get-last-error)))
              (setf (gethash pane *opengl-contexts*) (sb-sys:sap-int context))
              (redraw-opengl-pane pane))))
      (release-dc hwnd dc))))

(defun opengl-pixel (pane x y)
  "Return the red, green, blue, and alpha bytes at X and Y.
OpenGL's origin is the bottom-left corner of the pane.  The sample is
taken from the front buffer, which holds the last redraw."
  (call-with-opengl pane
    (lambda (dc)
      (declare (ignore dc))
      (gl-read-buffer +gl-front+)
      (let ((buffer (make-alien (unsigned 8) 4)))
        (unwind-protect
            (progn
              (gl-read-pixels (round x) (round y) 1 1
                              +gl-rgba+ +gl-unsigned-byte+
                              (alien-sap buffer))
              (values (deref buffer 0) (deref buffer 1)
                      (deref buffer 2) (deref buffer 3)))
          (free-alien buffer))))))

(define-application-frame windows-opengl-frame ()
  ()
  (:pretty-name "Windows OpenGL")
  (:panes
   (view :opengl :width 64 :height 64 :clear-red 0 :clear-green 0 :clear-blue 1))
  (:layouts
   (default (progn view)))
  (:geometry 64 64))

(behavior 'windows-opengl
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-opengl-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((view (find-pane-named frame 'view))
                 (port (frame-manager-port manager)))
             (should-be-true (sheet-mirror view))
             (should-be-true (gethash view *opengl-contexts*))
             (add-scene-box view "near" 'string -1/2 -1/2 -2 1/2 1/2 -1)
             (add-scene-box view "far" 'string -1/2 -1/2 -6 1/2 1/2 -4)
             (send-message (sheet-mirror view) +wm-lbuttondown+
                           0 (logior 32 (ash 32 16)))
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-press-event))
               (should-eq view (event-sheet event))
               (should= 32 (pointer-event-x event))
               (should= 32 (pointer-event-y event))
               (should-be-true (event-matches-gesture-name-p event :select))
               (let ((hit (opengl-presentation-at-position view 32 32)))
                 (should-equal "near" (presentation-object hit))
                 (should-be-true
                  (find-applicable-translators hit t nil
                                               :event event :window view
                                               :x 32 :y 32))))
             (should-be-null (opengl-presentation-at-position view 0 0))
             (multiple-value-bind (red green blue alpha) (opengl-pixel view 0 0)
               (should= 0 red)
               (should= 0 green)
               (should= 255 blue)
               (should= 255 alpha))
             (send-message (sheet-mirror view) +wm-paint+ 0 0)
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)
             (should-be-null (gethash view *opengl-contexts*))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame windows-scene-frame ()
  ((object
    :initform nil
    :accessor picked-scene-object))
  (:pretty-name "Windows Scene")
  (:panes
   (view :opengl :width 64 :height 64))
  (:layouts
   (default (progn view)))
  (:geometry 64 64))

(define-command (com-pick-scene :command-table windows-scene-frame :name t)
    ((object string))
  "Remember OBJECT on the current frame and return it."
  (setf (picked-scene-object *application-frame*) object))

(define-presentation-to-command-translator pick-scene-object
    (string com-pick-scene windows-scene-frame
     :gesture :select)
    (object)
  (list object))

(behavior 'windows-scene-command
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-scene-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((view (find-pane-named frame 'view)))
             (add-scene-box view "near" 'string -1/2 -1/2 -2 1/2 1/2 -1)
             (add-scene-box view "far" 'string -1/2 -1/2 -6 1/2 1/2 -4)
             (send-message (sheet-mirror view) +wm-lbuttondown+
                           0 (logior 32 (ash 32 16)))
             (let ((command (read-frame-command frame)))
               (should-eq 'com-pick-scene (first command))
               (should-equal "near" (second command)))
             (send-message (sheet-mirror view) +wm-lbuttondown+ 0 0)
             (should-be-null (read-frame-command frame))
             (send-message (sheet-mirror view) +wm-lbuttondown+
                           0 (logior 32 (ash 32 16)))
             (should-eq frame (run-frame-top-level frame))
             (should-equal "near" (picked-scene-object frame))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)
             (should-be-null (gethash view *opengl-contexts*))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))
