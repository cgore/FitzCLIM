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
        :FitzCLIM/port)
  (:export :load-private-font
           :make-windows-frame-manager
           :windows-frame-manager
           :windows-port))
(in-package :FitzCLIM/windows)

(defun windows-gl-function (name)
  "Return the function named NAME in the Windows OpenGL package, or NIL."
  (let ((package (find-package :fitzclim/windows/gl)))
    (when package
      (let ((symbol (find-symbol name package)))
        (when (and symbol (fboundp symbol))
          (symbol-function symbol))))))

(defvar *windows-libraries-loaded* nil
  "True after user32.dll, gdi32.dll, and kernel32.dll have been loaded.")

(defun load-windows-libraries ()
  "Load the Windows libraries this port calls."
  (unless *windows-libraries-loaded*
    (load-shared-object "kernel32.dll")
    (load-shared-object "user32.dll")
    (load-shared-object "gdi32.dll")
    (load-shared-object "uxtheme.dll")
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
(defparameter +ws-overlappedwindow+ #x00CF0000)
(defparameter +ws-ex-appwindow+ #x00040000)
(defparameter +gwl-style+ -16)
(defparameter +gwl-exstyle+ -20)
(defparameter +swp-nosize+ #x0001)
(defparameter +swp-nomove+ #x0002)
(defparameter +swp-nozorder+ #x0004)
(defparameter +swp-framechanged+ #x0020)
(defparameter +swp-showwindow+ #x0040)
(defparameter +swp-hidewindow+ #x0080)
(defparameter +spi-getworkarea+ #x0030)
(defparameter +wm-size+ #x0005)
(defparameter +wm-close+ #x0010)
(defparameter +wm-paint+ #x000F)
(defparameter +wm-keydown+ #x0100)
(defparameter +wm-keyup+ #x0101)
(defparameter +wm-char+ #x0102)
(defparameter +wm-erasebkgnd+ #x0014)
(defparameter +wm-ctlcolorstatic+ #x0138)
(defparameter +wm-drawitem+ #x002B)
(defparameter +wm-command+ #x0111)
(defparameter +mf-string+ #x0000)
(defparameter +mf-popup+ #x0010)
(defparameter +mf-separator+ #x0800)
(defparameter +wm-hscroll+ #x0114)
(defparameter +wm-vscroll+ #x0115)
(defparameter +wm-mousemove+ #x0200)
(defparameter +wm-lbuttondown+ #x0201)
(defparameter +wm-lbuttonup+ #x0202)
(defparameter +wm-rbuttondown+ #x0204)
(defparameter +wm-rbuttonup+ #x0205)
(defparameter +wm-mbuttondown+ #x0207)
(defparameter +wm-mbuttonup+ #x0208)
(defparameter +wm-printclient+ #x0318)
(defparameter +cs-owndc+ #x0020)
(defparameter +ps-solid+ 0)
(defparameter +ps-dash+ 1)
(defparameter +null-brush+ 5)
(defparameter +transparent+ 1)
(defparameter +opaque+ 2)
(defparameter +fw-normal+ 400)
(defparameter +fw-bold+ 700)
(defparameter +nonantialiased-quality+ 3)
(defparameter +cleartype-quality+ 5)
(defparameter +wm-setfont+ #x0030)
(defparameter +wm-getfont+ #x0031)
(defparameter +fr-private+ #x10)
(defparameter +logpixelsy+ 90)
(defparameter +mk-shift+ 4)
(defparameter +mk-control+ 8)
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
  (wparam (unsigned 64))
  (lparam (signed 64)))
(define-alien-routine ("CreateMenu" create-menu) system-area-pointer)
(define-alien-routine ("AppendMenuW" append-menu-w) (signed 32)
  (menu system-area-pointer)
  (flags (unsigned 32))
  (id (unsigned 64))
  (text system-area-pointer))
(define-alien-routine ("SetMenu" set-menu) (signed 32)
  (hwnd system-area-pointer)
  (menu system-area-pointer))
(define-alien-routine ("GetMenu" get-menu) system-area-pointer
  (hwnd system-area-pointer))
(define-alien-routine ("CreatePopupMenu" create-popup-menu) system-area-pointer)
(define-alien-routine ("GetSubMenu" get-sub-menu) system-area-pointer
  (menu system-area-pointer)
  (position (signed 32)))
(define-alien-routine ("GetMenuItemCount" get-menu-item-count) (signed 32)
  (menu system-area-pointer))
(define-alien-routine ("GetMenuItemID" get-menu-item-id) (unsigned 32)
  (menu system-area-pointer)
  (position (signed 32)))
(define-alien-routine ("MoveWindow" move-window) (signed 32)
  (hwnd system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (width (signed 32))
  (height (signed 32))
  (repaint (signed 32)))
(define-alien-routine ("SetWindowLongPtrW" set-window-long-ptr-w) (signed 64)
  (hwnd system-area-pointer)
  (index (signed 32))
  (new-long (signed 64)))
(define-alien-routine ("AdjustWindowRectEx" adjust-window-rect-ex) (signed 32)
  (rect (* win-rect))
  (style (unsigned 32))
  (menu (signed 32))
  (ex-style (unsigned 32)))
(define-alien-routine ("SystemParametersInfoW" system-parameters-info-w) (signed 32)
  (action (unsigned 32))
  (param (unsigned 32))
  (pv-param system-area-pointer)
  (win-ini (unsigned 32)))
(define-alien-routine ("GetMessageW" get-message-w) (signed 32)
  (message system-area-pointer)
  (hwnd system-area-pointer)
  (filter-min (unsigned 32))
  (filter-max (unsigned 32)))
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
(define-alien-routine ("GetClientRect" get-client-rect) (signed 32)
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
(define-alien-routine ("InvalidateRect" invalidate-rect) (signed 32)
  (hwnd system-area-pointer)
  (rect system-area-pointer)
  (erase (signed 32)))
(define-alien-routine ("RedrawWindow" redraw-window) (signed 32)
  (hwnd system-area-pointer)
  (rect system-area-pointer)
  (region system-area-pointer)
  (flags (unsigned 32)))
(define-alien-routine ("GetDC" get-dc) system-area-pointer
  (hwnd system-area-pointer))
(define-alien-routine ("ReleaseDC" release-dc) (signed 32)
  (hwnd system-area-pointer)
  (hdc system-area-pointer))
(define-alien-routine ("FillRect" fill-rect) (signed 32)
  (hdc system-area-pointer)
  (rect (* win-rect))
  (brush system-area-pointer))
(define-alien-routine ("CreateCompatibleDC" create-compatible-dc) system-area-pointer
  (hdc system-area-pointer))
(define-alien-routine ("CreateCompatibleBitmap" create-compatible-bitmap)
    system-area-pointer
  (hdc system-area-pointer)
  (width (signed 32))
  (height (signed 32)))
(define-alien-routine ("DeleteDC" delete-dc) (signed 32)
  (hdc system-area-pointer))
(define-alien-routine ("CreateSolidBrush" create-solid-brush) system-area-pointer
  (color (unsigned 32)))
(define-alien-routine ("CreatePen" create-pen) system-area-pointer
  (style (signed 32))
  (width (signed 32))
  (color (unsigned 32)))
(define-alien-routine ("GetStockObject" get-stock-object) system-area-pointer
  (object (signed 32)))
(define-alien-routine ("SelectObject" select-object) system-area-pointer
  (hdc system-area-pointer)
  (object system-area-pointer))
(define-alien-routine ("DeleteObject" delete-object) (signed 32)
  (object system-area-pointer))
(define-alien-routine ("MoveToEx" move-to-ex) (signed 32)
  (hdc system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (point system-area-pointer))
(define-alien-routine ("LineTo" line-to) (signed 32)
  (hdc system-area-pointer)
  (x (signed 32))
  (y (signed 32)))
(define-alien-routine ("Rectangle" gdi-rectangle) (signed 32)
  (hdc system-area-pointer)
  (left (signed 32))
  (top (signed 32))
  (right (signed 32))
  (bottom (signed 32)))
(define-alien-routine ("Polygon" gdi-polygon) (signed 32)
  (hdc system-area-pointer)
  (points system-area-pointer)
  (count (signed 32)))
(define-alien-routine ("Polyline" gdi-polyline) (signed 32)
  (hdc system-area-pointer)
  (points system-area-pointer)
  (count (signed 32)))
(define-alien-routine ("SetPixel" set-pixel) (unsigned 32)
  (hdc system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (color (unsigned 32)))
(define-alien-routine ("GetPixel" get-pixel) (unsigned 32)
  (hdc system-area-pointer)
  (x (signed 32))
  (y (signed 32)))
(define-alien-routine ("SetTextColor" set-text-color) (unsigned 32)
  (hdc system-area-pointer)
  (color (unsigned 32)))
(define-alien-routine ("SetBkMode" set-bk-mode) (signed 32)
  (hdc system-area-pointer)
  (mode (signed 32)))
(define-alien-routine ("SetBkColor" set-bk-color) (unsigned 32)
  (hdc system-area-pointer)
  (color (unsigned 32)))
(define-alien-routine ("GetTextColor" get-text-color) (unsigned 32)
  (hdc system-area-pointer))
(define-alien-routine ("GetBkColor" get-bk-color) (unsigned 32)
  (hdc system-area-pointer))
(define-alien-routine ("SetWindowTheme" set-window-theme) (signed 32)
  (hwnd system-area-pointer)
  (sub-app system-area-pointer)
  (sub-id system-area-pointer))
(define-alien-routine ("TextOutW" text-out-w) (signed 32)
  (hdc system-area-pointer)
  (x (signed 32))
  (y (signed 32))
  (text system-area-pointer)
  (count (signed 32)))
(define-alien-routine ("CreateFontW" create-font-w) system-area-pointer
  (height (signed 32))
  (width (signed 32))
  (escapement (signed 32))
  (orientation (signed 32))
  (weight (signed 32))
  (italic (unsigned 32))
  (underline (unsigned 32))
  (strikeout (unsigned 32))
  (charset (unsigned 32))
  (out-precision (unsigned 32))
  (clip-precision (unsigned 32))
  (quality (unsigned 32))
  (pitch (unsigned 32))
  (face system-area-pointer))
(define-alien-routine ("GetDeviceCaps" get-device-caps) (signed 32)
  (hdc system-area-pointer)
  (index (signed 32)))
(define-alien-routine ("GetObjectW" get-object-w) (signed 32)
  (object system-area-pointer)
  (size (signed 32))
  (buffer system-area-pointer))
(define-alien-routine ("GetTextFaceW" get-text-face-w) (signed 32)
  (hdc system-area-pointer)
  (count (signed 32))
  (face system-area-pointer))
(define-alien-routine ("AddFontResourceExW" add-font-resource-ex-w) (signed 32)
  (file system-area-pointer)
  (flags (unsigned 32))
  (reserved system-area-pointer))
(define-alien-routine ("SaveDC" save-dc) (signed 32)
  (hdc system-area-pointer))
(define-alien-routine ("RestoreDC" restore-dc) (signed 32)
  (hdc system-area-pointer)
  (saved (signed 32)))
(define-alien-routine ("IntersectClipRect" intersect-clip-rect) (signed 32)
  (hdc system-area-pointer)
  (left (signed 32))
  (top (signed 32))
  (right (signed 32))
  (bottom (signed 32)))
(define-alien-routine ("SetROP2" set-rop2) (signed 32)
  (hdc system-area-pointer)
  (mode (signed 32)))
(define-alien-routine ("SetPolyFillMode" set-poly-fill-mode) (signed 32)
  (hdc system-area-pointer)
  (mode (signed 32)))

(define-alien-type gdi-point
  (struct gdi-point
    (x (signed 32))
    (y (signed 32))))

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

(defvar *window-proc-error* nil
  "The last error caught in the window procedure, or NIL.")

(defvar *windows-modifier-state* 0
  "Modifier bits held down, tracked from key messages.")

(defvar *control-classes* nil
  "An alist of window-class keywords and their permanent UTF-16 names.")

(defvar *windows-classes-ready* nil
  "True after FitzCLIMFrame and FitzCLIMPane have been registered.")

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
                       (:pane "FitzCLIMPane")
                       (:opengl "FitzCLIMOpenGL")))))
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
    (opengl-pane :opengl)
    (t nil)))

(defun button-style (sheet)
  "Return the button style bits of SHEET.
A push button with no label is owner-drawn, so its color is the
button itself rather than a system button face."
  (typecase sheet
    (push-button (logior +ws-tabstop+
                         (if (string= (or (gadget-label sheet) "") "")
                             #x000B
                             0)))
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
            (opengl-pane +ws-clipchildren+)
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

(defvar *menu-commands* (make-hash-table)
  "A table from a menu item id to a frame and a command name.")

(defvar *frame-menu-ids* (make-hash-table :test #'eq)
  "A table from a frame to the ids of its menu items.")

(defvar *next-menu-id* #xefff
  "The last menu item id handed out.  Ids stay in the high half of a
16-bit value so they do not collide with control ids.")

(defun queue-menu-command (id)
  "Queue the command named by menu item ID, when this port owns it."
  (let ((entry (gethash id *menu-commands*)))
    (when entry
      (destructuring-bind (frame . name) entry
        (setf (frame-pending-commands frame)
              (append (frame-pending-commands frame) (list name)))))))

(defun fill-menu (menu frame items)
  "Append ITEMS onto the Windows menu MENU for FRAME.
Return the command ids in order, including ids inside submenus.  A
divider has no id.  A submenu is a popup menu."
  (let ((ids nil))
    (dolist (item items)
      (ecase (command-menu-item-type item)
        (:command
         (let ((id (incf *next-menu-id*)))
           (with-utf16 (text (command-menu-item-name item))
             (unless (plusp (append-menu-w menu +mf-string+ id (alien-sap text)))
               (error "AppendMenuW failed (error ~D)." (get-last-error))))
           (setf (gethash id *menu-commands*)
                 (cons frame (command-menu-item-value item)))
           (setf ids (nconc ids (list id)))))
        (:divider
         (unless (plusp (append-menu-w menu +mf-separator+ 0 (sb-sys:int-sap 0)))
           (error "AppendMenuW failed (error ~D)." (get-last-error))))
        (:menu
         (let ((popup (create-popup-menu))
               (table (command-menu-item-value item)))
           (unless (plusp (sb-sys:sap-int popup))
             (error "CreatePopupMenu failed (error ~D)." (get-last-error)))
           (setf ids (nconc ids
                            (fill-menu popup frame (command-menu-items table))))
           (with-utf16 (text (command-menu-item-name item))
             (unless (plusp (append-menu-w menu +mf-popup+
                                            (ldb (byte 64 0) (sb-sys:sap-int popup))
                                            (alien-sap text)))
               (error "AppendMenuW failed (error ~D)." (get-last-error))))))))
    ids))

(defun install-frame-menu (frame)
  "Give FRAME a menu bar from the table its :MENU-BAR option chose.
Commands with a menu string are items.  An explicit submenu is a
popup.  A frame with no menu bar, or with no items, keeps no menu."
  (let* ((table (frame-menu-table frame))
         (items (when table (command-menu-items table))))
    (when items
      (let ((menu (create-menu)))
        (unless (plusp (sb-sys:sap-int menu))
          (error "CreateMenu failed (error ~D)." (get-last-error)))
        (let ((ids (fill-menu menu frame items)))
          (unless (plusp (set-menu (hwnd-sap (frame-hwnd frame)) menu))
            (error "SetMenu failed (error ~D)." (get-last-error)))
          (setf (gethash frame *frame-menu-ids*) ids))))))

(defun forget-frame-menu (frame)
  "Drop FRAME's menu item ids.  DestroyWindow destroys the menu bar."
  (dolist (id (gethash frame *frame-menu-ids*))
    (remhash id *menu-commands*))
  (remhash frame *frame-menu-ids*))

(defun handle-command (wparam lparam)
  "Apply a WM_COMMAND notification.
A zero LPARAM is a menu item.  A control sends its HWND as LPARAM."
  (cond
    ((and (zerop lparam) (zerop (hiword wparam)))
     (queue-menu-command (loword wparam)))
    ((plusp lparam)
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
             (setf (gadget-value sheet) (read-selection sheet))))))))))

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

(defvar *gdi-hdc* nil
  "The device context currently receiving GDI output, or NIL.")

(defclass gdi-medium (basic-medium)
  ()
  (:documentation
   "A medium that draws an application pane with GDI when a device
context is bound.  Outside of painting, a drawing call invalidates the
pane so the next paint replays the output history."))

(defun ensure-gdi-medium (pane)
  "Use a GDI medium for PANE, keeping the drawing state it already has."
  (let ((old (stream-medium pane)))
    (unless (typep old 'gdi-medium)
      (let ((medium (make-instance 'gdi-medium)))
        (when old
          (setf (medium-foreground medium) (medium-foreground old)
                (medium-background medium) (medium-background old)
                (medium-ink medium) (medium-ink old)
                (medium-line-style medium) (medium-line-style old)
                (medium-text-style medium) (medium-text-style old)
                (medium-transformation medium) (medium-transformation old)
                (medium-clipping-region medium) (medium-clipping-region old)))
        (setf (medium-sheet medium) pane
              (stream-medium pane) medium))))
  (stream-medium pane))

(defun rgb-colorref (red green blue)
  "Return a COLORREF for channels RED, GREEN, and BLUE, each from 0 to 1."
  (logior (round (* 255 red))
          (ash (round (* 255 green)) 8)
          (ash (round (* 255 blue)) 16)))

(defun color-colorref (color)
  "Return a COLORREF for COLOR, or NIL when it has no opaque color."
  (when (colored-design? color)
    (multiple-value-bind (red green blue opacity) (design-rgba color)
      (when (>= opacity 1/2)
        (rgb-colorref red green blue)))))

(defvar *color-brushes* (make-hash-table :test #'eql)
  "COLORREF values mapped to a brush kept for the life of the process.")

(defun color-brush (colorref)
  "Return a solid brush for COLORREF, creating it once."
  (or (gethash colorref *color-brushes*)
      (setf (gethash colorref *color-brushes*)
            (create-solid-brush colorref))))

(defun fill-color-rect (hdc color left top right bottom)
  "Fill the rectangle with COLOR.  Right and bottom are exclusive."
  (let ((colorref (color-colorref color)))
    (when (and colorref (< left right) (< top bottom))
      (with-alien ((rect win-rect))
        (setf (slot rect 'left) left
              (slot rect 'top) top
              (slot rect 'right) right
              (slot rect 'bottom) bottom)
        (fill-rect hdc (addr rect) (color-brush colorref))))))

(defun stroke-color-rect (hdc color left top right bottom)
  "Stroke the rectangle with a one-pixel pen of COLOR."
  (let ((colorref (color-colorref color)))
    (when (and colorref (< left right) (< top bottom))
      (call-with-pen hdc colorref 1 nil
        (lambda ()
          (call-with-brush hdc colorref nil
            (lambda ()
              (gdi-rectangle hdc left top right bottom))))))))

(defun ink-colorref (medium ink)
  "Return a COLORREF for INK, :FLIP for flipping ink, or NIL to skip it.
Foreground and background inks take the medium's colors.  A design
whose opacity is below one half is skipped."
  (cond
    ((and (eq ink +foreground-ink+)
          (not (eq (medium-foreground medium) ink)))
     (ink-colorref medium (medium-foreground medium)))
    ((and (eq ink +background-ink+)
          (not (eq (medium-background medium) ink)))
     (ink-colorref medium (medium-background medium)))
    ((eq ink +flipping-ink+) :flip)
    ((colored-design? ink)
     (multiple-value-bind (red green blue opacity) (design-rgba ink)
       (when (>= opacity 1/2)
         (rgb-colorref red green blue))))
    (t nil)))

(behavior 'ink-colorref
  (let ((medium (make-instance 'basic-medium)))
    (should= #x000000 (ink-colorref medium +black+))
    (should= #x0000ff (ink-colorref medium +red+))
    (should= #x00ff00 (ink-colorref medium +green+))
    (should= #xff0000 (ink-colorref medium +blue+))
    (should= #xffffff (ink-colorref medium +white+))
    (should= #x000000 (ink-colorref medium +foreground-ink+))
    (should= #xffffff (ink-colorref medium +background-ink+))
    (should-eq :flip (ink-colorref medium +flipping-ink+))
    (should-be-null (ink-colorref medium +transparent-ink+))))

(defun line-thickness-pixels (style)
  "Return the pixel thickness of STYLE.  A point is 96/72 of a pixel."
  (let* ((style (or style (make-line-style)))
         (thickness (line-style-thickness style))
         (unit (line-style-unit style)))
    (max 1 (round (cond
                    ((eq unit :point) (* thickness 96/72))
                    ((realp unit) (* thickness unit))
                    (t thickness))))))

(defun line-dashed? (style)
  "Return true when STYLE asks for a dashed stroke."
  (and style (line-style-dashes style)))

(defun invalidate-sheet (sheet)
  "Ask Windows to paint SHEET again."
  (let ((hwnd (and sheet (sheet-mirror sheet))))
    (when hwnd
      (invalidate-rect (hwnd-sap hwnd) (sb-sys:int-sap 0) 1)
         (redraw-window (hwnd-sap hwnd) (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                        (logior 1 4 256)))))

(defun call-with-ink (hdc color function)
  "Call FUNCTION with a COLORREF.  Flipping ink reverses the pixels."
  (cond
    ((eq color :flip)
     (let ((old (set-rop2 hdc 6)))
       (unwind-protect
           (funcall function #x000000)
         (set-rop2 hdc old))))
    ((integerp color)
     (funcall function color))))

(defun call-with-pen (hdc color thickness dashed function)
  "Select a pen of COLOR and call FUNCTION.  A dashed pen is one pixel wide."
  (let* ((width (max 1 (round thickness)))
         (pen (create-pen (if (and dashed (<= width 1)) +ps-dash+ +ps-solid+)
                          width color))
         (old (select-object hdc pen)))
    (unwind-protect
        (funcall function)
      (select-object hdc old)
      (delete-object pen))))

(defun call-with-brush (hdc color filled function)
  "Select a brush and call FUNCTION.  An unfilled shape uses a null brush."
  (let ((brush (if filled
                   (create-solid-brush color)
                   (get-stock-object +null-brush+))))
    (let ((old (select-object hdc brush)))
      (unwind-protect
          (funcall function)
        (select-object hdc old)
        (when filled
          (delete-object brush))))))

(defun gdi-fill-rectangle (hdc x1 y1 x2 y2 color thickness filled dashed)
  "Draw a rectangle.  GDI's right and bottom edges are exclusive."
  (let ((left (round (min x1 x2)))
        (top (round (min y1 y2)))
        (right (1+ (round (max x1 x2))))
        (bottom (1+ (round (max y1 y2)))))
    (call-with-pen hdc color (if filled 1 thickness) (and (not filled) dashed)
      (lambda ()
        (call-with-brush hdc color filled
          (lambda ()
            (gdi-rectangle hdc left top right bottom)))))))

(defun set-gdi-points (points coordinates)
  "Write COORDINATES, a flat list, into the GDI POINT array POINTS."
  (loop for index from 0
        for (x y) on coordinates by #'cddr
        do (setf (slot (deref points index) 'x) (round x)
                 (slot (deref points index) 'y) (round y))))

(defun call-with-points (coordinates function)
  "Call FUNCTION with a POINT array and a count for COORDINATES."
  (let* ((count (floor (length coordinates) 2))
         (points (make-alien gdi-point count)))
    (unwind-protect
        (progn
          (set-gdi-points points coordinates)
          (funcall function (alien-sap points) count))
      (free-alien points))))

(defun gdi-fill-polygon (hdc coordinates color thickness filled closed dashed)
  "Draw a polygon.  A filled polygon is closed.  An open stroke is a polyline."
  (when (>= (length coordinates) 4)
    (call-with-pen hdc color (if filled 1 thickness) (and (not filled) dashed)
      (lambda ()
        (call-with-points coordinates
          (lambda (points count)
            (if (or filled closed)
                (call-with-brush hdc color filled
                  (lambda ()
                    (gdi-polygon hdc points count)))
                (gdi-polyline hdc points count))))))))

(defun full-ellipse? (start end)
  "Return true when START and END describe a whole ellipse."
  (or (and (null start) (null end))
      (and start end (>= (abs (- end start)) (* 2 pi 99/100)))))

(defun ellipse-outline (center-x center-y radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy start end)
  "Return a flat coordinate list for the ellipse boundary.
A full ellipse is used when START and END are NIL.  Angles are radians
in the sheet's coordinate system."
  (let* ((start (or start 0))
         (end (or end (+ start (* 2 pi))))
         (steps 48))
    (loop for index from 0 to steps
          for angle = (+ start (* (- end start) (/ index steps)))
          collect (+ center-x
                     (* radius-1-dx (cos angle))
                     (* radius-2-dx (sin angle)))
          collect (+ center-y
                     (* radius-1-dy (cos angle))
                     (* radius-2-dy (sin angle))))))

(behavior 'ellipse-outline
  (let ((points (ellipse-outline 10 10 4 0 0 4 nil nil)))
    (should= 14 (first points))
    (should= 10 (second points))
    (should-be-true (> (length points) 16))
    (should-be-true (full-ellipse? nil nil))
    (should-be-false (full-ellipse? 0 (/ pi 2)))))

(defun gdi-ellipse (hdc center-x center-y radius-1-dx radius-1-dy
                    radius-2-dx radius-2-dy start end color thickness
                    filled dashed)
  "Draw an ellipse as a polygon, including a pie when the arc is partial."
  (let ((outline (ellipse-outline center-x center-y
                                  radius-1-dx radius-1-dy
                                  radius-2-dx radius-2-dy
                                  start end)))
    (gdi-fill-polygon hdc
                      (if (and filled (not (full-ellipse? start end)))
                          (append outline (list center-x center-y))
                          outline)
                      color thickness filled t dashed)))

(defun text-style-bold? (style)
  "Return true when STYLE is bold."
  (member (text-style-face (parse-text-style style)) '(:bold :bold-italic)))

(defun text-style-italic? (style)
  "Return true when STYLE is italic."
  (member (text-style-face (parse-text-style style)) '(:italic :bold-italic)))

(defun font-face-name (style)
  "Return a Windows face name for STYLE."
  (let ((family (text-style-family (parse-text-style style))))
    (cond
      ((stringp family) family)
      ((eq family :fix) "Consolas")
      ((eq family :serif) "Times New Roman")
      (t "Segoe UI"))))

(defun font-pitch (style)
  "Return the pitch and family bits for STYLE.
A named face leaves the pitch to the font.  A family keyword asks for
a matching family."
  (let ((family (text-style-family (parse-text-style style))))
    (cond
      ((stringp family) 0)
      ((eq family :fix) (logior 1 48))
      ((eq family :serif) (logior 2 16))
      (t (logior 2 32)))))

(defun font-pixel-height (point-size)
  "Return the negative character height of POINT-SIZE at the screen DPI."
  (let* ((desktop (sb-sys:int-sap 0))
         (hdc (get-dc desktop))
         (dpi (if (plusp (sb-sys:sap-int hdc))
                  (get-device-caps hdc +logpixelsy+)
                  0)))
    (when (plusp (sb-sys:sap-int hdc))
      (release-dc desktop hdc))
    (- (max 1 (round (* point-size (if (plusp dpi) dpi 96) 1/72))))))

(defun create-control-font (style)
  "Return an HFONT for STYLE.  NIL means the port's default style."
  (let ((style (parse-text-style (or style *default-text-style*))))
    (with-utf16 (face (font-face-name style))
      (let ((font (create-font-w (font-pixel-height (text-style-point-size style))
                                 0 0 0
                                 (if (text-style-bold? style)
                                     +fw-bold+
                                     +fw-normal+)
                                 (if (text-style-italic? style) 1 0)
                                 0 0 1 0 0 +cleartype-quality+
                                 (font-pitch style)
                                 (alien-sap face))))
        (when (plusp (sb-sys:sap-int font))
          font)))))

(defvar *control-fonts* (make-hash-table :test #'eql)
  "HWND integers mapped to the HFONT assigned to that control.")

(defun assign-control-font (sheet)
  "Give SHEET's native control the font for its text style.
The font stays alive until the control is destroyed.  A sheet with no
window is left alone."
  (when (and (gadgetp sheet) (sheet-mirror sheet))
    (let ((font (create-control-font (gadget-text-style sheet)))
          (hwnd (sheet-mirror sheet)))
      (when font
        (send-message hwnd +wm-setfont+ (sb-sys:sap-int font) 1)
        (let ((previous (gethash hwnd *control-fonts*)))
          (setf (gethash hwnd *control-fonts*) font)
          (when (and previous (not (sb-sys:sap= previous font)))
            (delete-object previous)))))))

(defun release-control-font (hwnd)
  "Delete the font assigned to HWND.  A second call does nothing."
  (let ((font (gethash hwnd *control-fonts*)))
    (remhash hwnd *control-fonts*)
    (when font
      (delete-object font)))
  nil)

(defun release-sheet-font (sheet)
  "Delete the font assigned to SHEET's control."
  (let ((hwnd (sheet-mirror sheet)))
    (when hwnd
      (release-control-font hwnd))))

(defun text-origin (string x y align-x align-y style)
  "Return the top-left pixel of STRING drawn at the alignment point X, Y."
  (multiple-value-bind (width ascent descent) (text-metrics style string)
    (let ((height (+ ascent descent)))
      (values (ecase align-x
                (:left x)
                (:center (- x (/ width 2)))
                (:right (- x width)))
              (ecase align-y
                (:top y)
                (:center (- y (/ height 2)))
                (:baseline (- y ascent))
                (:bottom (- y height)))))))

(defun gdi-text (hdc string x y align-x align-y style color)
  "Draw STRING.  The recording metrics decide the alignment."
  (multiple-value-bind (origin-x origin-y)
      (text-origin string x y align-x align-y style)
    (with-utf16 (face (font-face-name style))
      (let ((font (create-font-w (round (- (max 1 (text-style-point-size style))))
                                 0 0 0
                                 (if (text-style-bold? style) +fw-bold+ +fw-normal+)
                                 (if (text-style-italic? style) 1 0)
                                 0 0 1 0 0 +nonantialiased-quality+
                                 (font-pitch style)
                                 (alien-sap face))))
        (when (plusp (sb-sys:sap-int font))
          (let ((old (select-object hdc font))
                (old-color (set-text-color hdc color))
                (old-mode (set-bk-mode hdc +transparent+)))
            (unwind-protect
                (with-utf16 (buffer string)
                  (text-out-w hdc (round origin-x) (round origin-y)
                              (alien-sap buffer) (length string)))
              (set-bk-mode hdc old-mode)
              (set-text-color hdc old-color)
              (select-object hdc old)
              (delete-object font))))))))

(defun draw-on-medium (medium ink drawer)
  "Run DRAWER on the paint device context, or invalidate MEDIUM's sheet.
DRAWER is called with the device context and a COLORREF."
  (let ((hdc *gdi-hdc*))
    (if hdc
        (call-with-ink hdc (ink-colorref medium ink)
          (lambda (color)
            (funcall drawer hdc color)))
        (invalidate-sheet (medium-sheet medium)))))

(defmethod medium-draw-point* ((medium gdi-medium) x y &key ink line-style)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (let ((thickness (line-thickness-pixels line-style)))
        (if (<= thickness 1)
            (set-pixel hdc (round x) (round y) color)
            (gdi-ellipse hdc x y (/ thickness 2) 0 0 (/ thickness 2)
                         nil nil color 1 t nil))))))

(defmethod medium-draw-line* ((medium gdi-medium) x1 y1 x2 y2 &key ink line-style)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (call-with-pen hdc color (line-thickness-pixels line-style)
          (line-dashed? line-style)
        (lambda ()
          (move-to-ex hdc (round x1) (round y1) (sb-sys:int-sap 0))
          (line-to hdc (round x2) (round y2))
          (set-pixel hdc (round x2) (round y2) color))))))

(defmethod medium-draw-rectangle* ((medium gdi-medium) x1 y1 x2 y2
                                   &key filled ink line-style)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (gdi-fill-rectangle hdc x1 y1 x2 y2 color
                          (line-thickness-pixels line-style)
                          filled
                          (line-dashed? line-style)))))

(defmethod medium-draw-polygon* ((medium gdi-medium) coordinates
                                 &key closed filled ink line-style)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (gdi-fill-polygon hdc coordinates color
                        (line-thickness-pixels line-style)
                        filled closed
                        (line-dashed? line-style)))))

(defmethod medium-draw-ellipse* ((medium gdi-medium) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 &key filled start-angle end-angle ink line-style)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (gdi-ellipse hdc center-x center-y
                   radius-1-dx radius-1-dy
                   radius-2-dx radius-2-dy
                   start-angle end-angle color
                   (line-thickness-pixels line-style)
                   filled
                   (line-dashed? line-style)))))

(defmethod medium-draw-text* ((medium gdi-medium) string x y
                              &key start end align-x align-y text-style ink)
  (draw-on-medium medium ink
    (lambda (hdc color)
      (gdi-text hdc
                (subseq string (or start 0) (or end (length string)))
                x y
                (or align-x :left)
                (or align-y :baseline)
                (or text-style (medium-text-style medium))
                color))))

(defmethod replay-displayed-output-record :around
    ((record displayed-output-record) stream x-offset y-offset)
  (let ((hdc *gdi-hdc*)
        (clip (displayed-output-record-clipping-region record)))
    (cond
      ((or (null hdc) (null clip) (everywhere-p clip))
       (call-next-method))
      ((nowhere-p clip) nil)
      (t
       (multiple-value-bind (left top right bottom) (bounding-rectangle* clip)
         (save-dc hdc)
         (unwind-protect
             (progn
               (intersect-clip-rect hdc
                                    (floor left) (floor top)
                                    (1+ (floor right)) (1+ (floor bottom)))
               (call-next-method))
           (restore-dc hdc -1)))))))

(defun fill-pane-background (pane hdc)
  "Fill PANE with its medium's background color."
  (multiple-value-bind (width height) (sheet-pixel-size pane)
    (let ((color (ink-colorref (stream-medium pane)
                               (medium-background (stream-medium pane)))))
      (when (integerp color)
        (with-alien ((rect win-rect))
          (setf (slot rect 'left) 0
                (slot rect 'top) 0
                (slot rect 'right) width
                (slot rect 'bottom) height)
          (let ((brush (create-solid-brush color)))
            (unwind-protect
                (fill-rect hdc (addr rect) brush)
              (delete-object brush))))))))

(defun paint-sheet (pane hdc)
  "Fill PANE and replay its output history onto HDC."
  (let ((*gdi-hdc* hdc))
    (set-poly-fill-mode hdc 2)
    (fill-pane-background pane hdc)
    (replay (stream-output-history pane) pane)))

(defun paint-pane (hwnd)
  "Paint the application pane at HWND from its output history."
  (let* ((paint (make-alien (unsigned 8) 128))
         (sap (alien-sap paint)))
    (unwind-protect
        (let ((hdc (begin-paint (hwnd-sap hwnd) sap)))
          (unwind-protect
              (let ((pane (mirror-sheet hwnd)))
                (when (and pane (plusp (sb-sys:sap-int hdc)))
                  (paint-sheet pane hdc)))
            (end-paint (hwnd-sap hwnd) sap)))
      (free-alien paint))))

(defun with-pane-pixels (pane function)
  "Call FUNCTION with a memory device context holding a replay of PANE."
  (multiple-value-bind (width height) (sheet-pixel-size pane)
    (let* ((window (hwnd-sap (sheet-mirror pane)))
           (source (get-dc window))
           (memory (create-compatible-dc source))
           (bitmap (create-compatible-bitmap source (max 1 width) (max 1 height)))
           (old (select-object memory bitmap)))
      (unwind-protect
          (progn
            (send-message (sheet-mirror pane) +wm-printclient+
                          (sb-sys:sap-int memory) 0)
            (funcall function memory))
        (select-object memory old)
        (delete-object bitmap)
        (delete-dc memory)
        (release-dc window source)))))

(defun pane-pixel (pane x y)
  "Return the COLORREF of the pixel at X and Y on a replay of PANE."
  (with-pane-pixels pane
    (lambda (hdc)
      (let ((value (get-pixel hdc (round x) (round y))))
        (if (= value #xffffffff)
            (error "GetPixel failed at ~D, ~D." x y)
            (logand value #xffffff))))))

(defun pane-has-dark-pixel? (pane left top width height)
  "Return true when a replay of PANE has a near-black pixel in the box."
  (with-pane-pixels pane
    (lambda (hdc)
      (loop for y from (round top) below (+ (round top) (round height))
            thereis (loop for x from (round left) below (+ (round left) (round width))
                          for value = (get-pixel hdc x y)
                          thereis (and (/= value #xffffffff)
                                       (< (logand value #xff) 40)
                                       (< (logand (ash value -8) #xff) 40)
                                       (< (logand (ash value -16) #xff) 40)))))))

(defun pointer-modifier-state (wparam)
  "Return the modifier bits for a pointer message."
  (logior *windows-modifier-state*
          (if (plusp (logand wparam +mk-shift+)) +shift-key+ 0)
          (if (plusp (logand wparam +mk-control+)) +control-key+ 0)))

(defun enqueue-pointer (sheet class x y modifiers &optional button)
  "Queue a pointer event aimed at SHEET."
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
                                    :modifier-state modifiers))))

(defun handle-pointer (hwnd message wparam lparam)
  "Queue a pointer event for a mouse message."
  (let ((sheet (mirror-sheet hwnd)))
    (when sheet
      (let ((x (loword lparam))
            (y (hiword lparam))
            (modifiers (pointer-modifier-state wparam)))
        (cond
          ((= message +wm-mousemove+)
           (enqueue-pointer sheet 'pointer-motion-event x y modifiers))
          ((= message +wm-lbuttondown+)
           (enqueue-pointer sheet 'pointer-button-press-event x y modifiers :left))
          ((= message +wm-lbuttonup+)
           (enqueue-pointer sheet 'pointer-button-release-event x y modifiers :left))
          ((= message +wm-rbuttondown+)
           (enqueue-pointer sheet 'pointer-button-press-event x y modifiers :right))
          ((= message +wm-rbuttonup+)
           (enqueue-pointer sheet 'pointer-button-release-event x y modifiers :right))
          ((= message +wm-mbuttondown+)
           (enqueue-pointer sheet 'pointer-button-press-event x y modifiers :middle))
          ((= message +wm-mbuttonup+)
           (enqueue-pointer sheet 'pointer-button-release-event x y modifiers :middle)))))))

(defun virtual-key-name (code)
  "Return the CLIM key name for a Windows virtual-key code, or NIL."
  (cond
    ((= code #x08) :backspace)
    ((= code #x09) :tab)
    ((= code #x0d) :return)
    ((= code #x1b) :escape)
    ((= code #x10) :shift)
    ((= code #x11) :control)
    ((= code #x12) :meta)
    ((= code #x21) :page-up)
    ((= code #x22) :page-down)
    ((= code #x23) :end)
    ((= code #x24) :home)
    ((= code #x25) :left)
    ((= code #x26) :up)
    ((= code #x27) :right)
    ((= code #x28) :down)
    ((= code #x2e) :delete)
    ((<= #x70 code #x7b)
     (intern (format nil "F~D" (1+ (- code #x70))) :keyword))
    ((<= 65 code 90)
     (intern (string (code-char code)) :keyword))
    (t nil)))

(defun modifier-bit (key-name)
  "Return the modifier bit KEY-NAME holds down, or 0."
  (case key-name
    (:shift +shift-key+)
    (:control +control-key+)
    (:meta +meta-key+)
    (t 0)))

(defun note-key (sheet class code character)
  "Queue a keyboard event and keep the modifier bits up to date."
  (let* ((name (if character nil (virtual-key-name code)))
         (bit (modifier-bit name))
         (state (logior *windows-modifier-state* bit)))
    (enqueue-event sheet
                   (make-instance class
                                  :sheet sheet
                                  :key-name name
                                  :character character
                                  :modifier-state state))
    (cond
      ((and (plusp bit) (eq class 'key-press-event))
       (setf *windows-modifier-state* state))
      ((and (plusp bit) (eq class 'key-release-event))
       (setf *windows-modifier-state* (logandc2 *windows-modifier-state* bit))))))

(defun handle-key (hwnd message wparam)
  "Queue a keyboard event for a key message."
  (let ((sheet (mirror-sheet hwnd)))
    (when sheet
      (cond
        ((= message +wm-char+)
         (note-key sheet 'key-press-event nil (code-char wparam)))
        ((= message +wm-keydown+)
         (note-key sheet 'key-press-event wparam nil))
        ((= message +wm-keyup+)
         (note-key sheet 'key-release-event wparam nil))))))

(defun apply-control-color (sheet hdc)
  "Apply SHEET's colors to HDC.  Return a brush integer, or NIL."
  (when (and (gadgetp sheet)
             (plusp (sb-sys:sap-int hdc))
             (or (gadget-foreground sheet) (gadget-background sheet)))
    (let ((foreground (color-colorref (gadget-foreground sheet)))
          (background (color-colorref (gadget-background sheet))))
      (when foreground
        (set-text-color hdc foreground))
      (when background
        (set-bk-color hdc background)
        (set-bk-mode hdc +opaque+))
      (when background
        (sb-sys:sap-int (color-brush background))))))

(defun draw-frame-lights (frame hdc top)
  "Fill each square cockpit light of FRAME.
A push button with no label is a light.  Its background is the color
of the square.  The native button face is covered."
  (map-over-sheets
   (lambda (sheet)
     (when (and (typep sheet 'push-button)
                (string= (or (gadget-label sheet) "") "")
                (gadget-background sheet))
       (multiple-value-bind (x y) (sheet-in-root sheet top)
         (multiple-value-bind (width height) (sheet-pixel-size sheet)
           (fill-color-rect hdc (gadget-background sheet)
                            x y (+ x width) (+ y height))))))
   top))

(defun draw-frame-client (frame hdc)
  "Fill FRAME's background and stroke the borders of its outlining panes."
  (let ((top (frame-top-level-sheet frame)))
    (when (and top (frame-background frame))
      (multiple-value-bind (width height) (sheet-pixel-size top)
        (fill-color-rect hdc (frame-background frame) 0 0 width height)))
    (when top
      (map-over-sheets
       (lambda (sheet)
         (when (and (typep sheet 'outlining-pane) (outlining-ink sheet))
           (multiple-value-bind (x y) (sheet-in-root sheet top)
             (multiple-value-bind (width height) (sheet-pixel-size sheet)
               (stroke-color-rect hdc (outlining-ink sheet)
                                  (+ x 1) (+ y 1)
                                  (+ x width -1) (+ y height -1))))))
       top)
    (when top
      (draw-frame-lights frame hdc top)))))

(defun frame-client-painted? (frame)
  "Return true when FRAME paints its own background or a border."
  (when frame
    (or (frame-background frame)
        (and (frame-top-level-sheet frame)
             (block found
               (map-over-sheets
                (lambda (sheet)
                  (when (and (typep sheet 'outlining-pane)
                             (outlining-ink sheet))
                    (return-from found t)))
                (frame-top-level-sheet frame)))))))

(defun paint-frame-client (hwnd)
  "Paint the background and borders of the frame at HWND."
  (let* ((paint (make-alien (unsigned 8) 128))
         (sap (alien-sap paint))
         (frame (mirror-frame hwnd)))
    (unwind-protect
        (let ((hdc (begin-paint (hwnd-sap hwnd) sap)))
          (unwind-protect
              (when (and frame (plusp (sb-sys:sap-int hdc)))
                (draw-frame-client frame hdc))
            (end-paint (hwnd-sap hwnd) sap)))
      (free-alien paint))))

(defun frame-client-pixel (frame x y)
  "Return the COLORREF draw-frame-client would paint at X and Y."
  (let ((top (frame-top-level-sheet frame)))
    (multiple-value-bind (width height) (sheet-pixel-size top)
      (let* ((screen (get-dc (sb-sys:int-sap 0)))
             (memory (create-compatible-dc screen))
             (bitmap (create-compatible-bitmap screen (max 1 width) (max 1 height)))
             (old (select-object memory bitmap)))
        (unwind-protect
            (progn
              (fill-color-rect memory +white+ 0 0 width height)
              (draw-frame-client frame memory)
              (logand (get-pixel memory x y) #xffffff))
          (select-object memory old)
          (delete-object bitmap)
          (delete-dc memory)
          (release-dc (sb-sys:int-sap 0) screen))))))

(defun draw-owner-button (lparam)
  "Fill an owner-drawn button from the DRAWITEMSTRUCT at LPARAM.
The button's background is the fill.  A button without one stays a
system button."
  (when (plusp lparam)
    (let* ((sap (sb-sys:int-sap lparam))
           (sheet (mirror-sheet (sb-sys:sap-ref-word sap 16))))
      (when (and (typep sheet 'push-button) (gadget-background sheet))
        (fill-color-rect (sb-sys:int-sap (sb-sys:sap-ref-word sap 32))
                         (gadget-background sheet)
                         (sb-sys:signed-sap-ref-32 sap 40)
                         (sb-sys:signed-sap-ref-32 sap 44)
                         (sb-sys:signed-sap-ref-32 sap 48)
                         (sb-sys:signed-sap-ref-32 sap 52)))))
  t)

(defun handle-message (hwnd message wparam lparam)
  "Handle one message for HWND.  Return a result when this port handles it."
  (cond
    ((= message +wm-drawitem+)
     (draw-owner-button lparam)
     (values t 1))
    ((= message +wm-size+)
     (let ((frame (mirror-frame hwnd)))
       (when frame
         (layout-frame frame
                       :width (loword lparam)
                       :height (hiword lparam))
         (invalidate-rect (hwnd-sap hwnd) (sb-sys:int-sap 0) 1)
         (redraw-window (hwnd-sap hwnd) (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                        (logior 1 4 256))))
     (values t 0))
    ((= message +wm-erasebkgnd+)
     (let ((frame (mirror-frame hwnd)))
       (when (and frame (frame-background frame) (plusp wparam))
         (let ((top (frame-top-level-sheet frame)))
           (when top
             (multiple-value-bind (width height) (sheet-pixel-size top)
               (fill-color-rect (sb-sys:int-sap wparam)
                                (frame-background frame)
                                0 0 width height))))
         (values t 1))))
    ((= message +wm-ctlcolorstatic+)
     (let ((brush (apply-control-color
                   (mirror-sheet (ldb (byte 64 0) lparam))
                   (sb-sys:int-sap wparam))))
       (when brush
         (values t brush))))
    ((and (= message +wm-paint+)
          (frame-client-painted? (mirror-frame hwnd)))
     (paint-frame-client hwnd)
     (values t 0))
    ((= message +wm-command+)
     (handle-command wparam lparam)
     (values t 0))
    ((or (= message +wm-hscroll+) (= message +wm-vscroll+))
     (handle-scroll lparam wparam)
     (values t 0))
    ((or (= message +wm-mousemove+)
         (= message +wm-lbuttondown+) (= message +wm-lbuttonup+)
         (= message +wm-rbuttondown+) (= message +wm-rbuttonup+)
         (= message +wm-mbuttondown+) (= message +wm-mbuttonup+))
     (handle-pointer hwnd message wparam lparam)
     (values t 0))
    ((or (= message +wm-keydown+) (= message +wm-keyup+) (= message +wm-char+))
     (handle-key hwnd message wparam)
     (values t 0))
    ((= message +wm-printclient+)
     (let ((pane (mirror-sheet hwnd)))
       (when (and (plusp wparam) (typep pane 'application-pane))
         (paint-sheet pane (sb-sys:int-sap wparam))))
     (values t 0))
    ((and (= message +wm-paint+)
          (typep (mirror-sheet hwnd) 'application-pane))
     (paint-pane hwnd)
     (values t 0))
    ((and (= message +wm-paint+)
          (typep (mirror-sheet hwnd) 'opengl-pane))
     (let ((paint (windows-gl-function "PAINT-OPENGL-PANE")))
       (when paint
         (funcall paint hwnd)))
     (values t 0))
    (t (values nil 0))))

(defun signed-lresult (value)
  "Return VALUE as a signed 64-bit LRESULT.
A window procedure returns an LRESULT, and DefWindowProc takes a signed
lParam.  sap-int of a handle whose high bit is set is larger than a
signed 64-bit integer.  The low 64 bits are the value Windows uses.
NIL is 0."
  (let ((bits (ldb (byte 64 0) (or value 0))))
    (if (logbitp 63 bits)
        (- bits (ash 1 64))
        bits)))

(behavior 'signed-lresult
  (should= 1 (signed-lresult 1))
  (should= 0 (signed-lresult nil))
  (should= -1 (signed-lresult -1))
  (should= -1626332176 (signed-lresult 18446744072083219440))
  (should-be-true (typep (signed-lresult 18446744072083219440)
                         '(signed-byte 64))))

(define-alien-callable fitzclim-window-proc (signed 64)
    ((hwnd system-area-pointer)
     (message (unsigned 32))
     (wparam (unsigned 64))
     (lparam (unsigned 64)))
  (let ((integer (sb-sys:sap-int hwnd))
        (signed-param (signed-lresult lparam)))
    (handler-case
        (multiple-value-bind (handled result)
            (handle-message integer message wparam lparam)
          (if handled
              (signed-lresult result)
              (def-window-proc-w hwnd message wparam signed-param)))
      (error (condition)
        (setf *window-proc-error* condition)
        (def-window-proc-w hwnd message wparam signed-param)))))

(behavior 'window-proc-lresult
  (setf *window-proc-error* nil)
  (let ((result (alien-funcall
                 (cast (alien-callable-function 'fitzclim-window-proc)
                       (function (signed 64)
                                 system-area-pointer
                                 (unsigned 32)
                                 (unsigned 64)
                                 (unsigned 64)))
                 (sb-sys:int-sap 0)
                 +wm-keyup+
                 0
                 18446744072083219440)))
    (should= 0 result)
    (should-be-true (typep result '(signed-byte 64)))
    (should-be-null *window-proc-error*)))

(defun register-window-class (kind background &optional (style 0))
  "Register KIND when Windows does not already have that class."
  (let ((class (make-alien wndclassexw)))
    (unwind-protect
         (progn
           (setf (slot class 'cb-size) (alien-size wndclassexw :bytes)
                 (slot class 'style) style
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
    (register-window-class :opengl 0 +cs-owndc+)
    (setf *windows-classes-ready* t)))

(defun place-mirror (sheet)
  "Move SHEET's window to its place in the top-level window."
  (let ((root (mirror-root sheet))
        (hwnd (sheet-mirror sheet)))
    (when (and root hwnd (not (eq sheet root)))
      (multiple-value-bind (x y) (sheet-in-root sheet root)
        (multiple-value-bind (width height) (sheet-pixel-size sheet)
          (move-window (hwnd-sap hwnd) x y width height 1))))))

(defun resize-root (sheet width height)
  "Give the top-level window of SHEET a new client size, keeping its origin."
  (when (sheet-mirror sheet)
    (set-window-pos (hwnd-sap (sheet-mirror sheet)) (sb-sys:int-sap 0)
                    0 0 (max 1 (round width)) (max 1 (round height))
                    (logior 2 4 16))))

(defmethod allocate-space :after ((pane sheet) width height)
  (when (and (sheet-mirror pane) (sheet-parent pane))
    (place-mirror pane)))

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
          (assign-control-font sheet)
          (when (and (gadgetp sheet)
                     (or (gadget-foreground sheet) (gadget-background sheet)))
            (disable-window-theme (sheet-mirror sheet)))
          (when (typep sheet 'application-pane)
            (ensure-gdi-medium sheet))
          (let ((realize (windows-gl-function "REALIZE-OPENGL-PANE")))
            (when (and (typep sheet 'opengl-pane) realize)
              (funcall realize sheet)))
          (refresh-mirror sheet))))))

(defun destroy-mirrors (sheet)
  "Destroy the native windows of SHEET and its descendants."
  (map-over-sheets
   (lambda (descendant)
     (let ((release (windows-gl-function "RELEASE-OPENGL-PANE")))
       (when (and (typep descendant 'opengl-pane) release)
         (funcall release descendant)))
     (let ((hwnd (sheet-mirror descendant)))
       (when hwnd
         (let ((sap (hwnd-sap hwnd)))
           (when (plusp (is-window sap))
             (destroy-window sap)))
         (release-control-font hwnd)
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

(defclass windows-port (port)
  ()
  (:documentation "The Windows port.  Its message queue is the thread's."))

(defclass windows-frame-manager (frame-manager)
  ()
  (:documentation
   "A frame manager that realizes frames as Windows windows and gadgets
as native controls.  Layout panes have no window of their own.  Their
children are parented to the frame window."))

(defun make-windows-frame-manager ()
  "Return a frame manager that realizes frames on Windows."
  (make-instance 'windows-frame-manager :port (make-instance 'windows-port)))

(setf *frame-manager-maker* #'make-windows-frame-manager)

(behavior 'make-windows-frame-manager
  (let ((manager (make-windows-frame-manager)))
    (should-be-true (typep manager 'windows-frame-manager))
    (should-be-true (typep (frame-manager-port manager) 'windows-port))
    (should-be-true (typep (frame-manager-port manager) 'port))
    (should-be-null (frame-manager-frames manager))
    (should-eq #'make-windows-frame-manager *frame-manager-maker*)))

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
      (setf (mirror-frame (sb-sys:sap-int hwnd)) frame)
      (setf (frame-port frame) (frame-manager-port manager))
      (push frame (frame-manager-frames manager))
      (handler-case
          (progn
            (realize-frame-sheets frame)
            (install-frame-menu frame))
        (error (condition)
          (disown-frame manager frame)
          (error condition)))))
  frame)

(defmethod disown-frame ((manager windows-frame-manager) (frame application-frame))
  (forget-frame-menu frame)
  (let ((top (frame-top-level-sheet frame)))
    (when top
      (let ((release (windows-gl-function "RELEASE-OPENGL-PANE")))
        (when release
          (map-over-sheets
           (lambda (descendant)
             (when (typep descendant 'opengl-pane)
               (funcall release descendant)))
           top)))
      (let ((hwnd (sheet-mirror top)))
        (when hwnd
          (let ((sap (hwnd-sap hwnd)))
            (when (plusp (is-window sap))
              (destroy-window sap)))
          (forget-mirror-frame hwnd)))
      (map-over-sheets #'release-sheet-font top)
      (map-over-sheets #'forget-mirror top)))
  (setf (frame-manager-frames manager)
        (remove frame (frame-manager-frames manager) :test #'eq))
  (forget-frame-port frame)
  frame)

(defun frame-hwnd (frame)
  "Return FRAME's top-level HWND integer."
  (let ((top (frame-top-level-sheet frame)))
    (unless (and top (sheet-mirror top))
      (error "The frame has no native window."))
    (sheet-mirror top)))

(defun work-area-origin ()
  "Return the left and top of the desktop work area, in pixels."
  (with-alien ((rect win-rect))
    (if (plusp (system-parameters-info-w +spi-getworkarea+ 0
                                         (alien-sap (addr rect)) 0))
        (values (slot rect 'left) (slot rect 'top))
        (values 0 0))))

(defun frame-outer-size (frame menu?)
  "Return the window size whose client area is FRAME's width and height.
MENU? is true when the window has a menu bar."
  (with-alien ((rect win-rect))
    (setf (slot rect 'left) 0
          (slot rect 'top) 0
          (slot rect 'right) (round (frame-width frame))
          (slot rect 'bottom) (round (frame-height frame)))
    (adjust-window-rect-ex (addr rect)
                           (logior +ws-overlappedwindow+ +ws-clipchildren+)
                           (if menu? 1 0)
                           +ws-ex-appwindow+)
    (values (- (slot rect 'right) (slot rect 'left))
            (- (slot rect 'bottom) (slot rect 'top)))))

(defmethod enable-frame ((manager windows-frame-manager) (frame application-frame))
  "Show FRAME as a normal window on the work area.
The window can take focus.  Its title is the frame's pretty name.
SetWindowPos shows the window.  The first ShowWindow call in a process
uses the startup show command, and a hidden startup would hide FRAME."
  (declare (ignore manager))
  (let* ((hwnd (frame-hwnd frame))
         (sap (hwnd-sap hwnd))
         (style (logior +ws-overlappedwindow+ +ws-clipchildren+)))
    (set-window-long-ptr-w sap +gwl-style+ style)
    (set-window-long-ptr-w sap +gwl-exstyle+ +ws-ex-appwindow+)
    (set-window-text hwnd (frame-pretty-name frame))
    (multiple-value-bind (x y) (work-area-origin)
      (multiple-value-bind (width height)
          (frame-outer-size frame (plusp (sb-sys:sap-int (get-menu sap))))
        (set-window-pos sap (sb-sys:int-sap 0)
                        (+ x 40) (+ y 40) width height
                        (logior +swp-nozorder+
                                +swp-framechanged+
                                +swp-showwindow+)))))
  frame)

(defmethod disable-frame ((manager windows-frame-manager) (frame application-frame))
  "Hide FRAME without using ShowWindow.
The first ShowWindow call in a process uses the startup show command."
  (declare (ignore manager))
  (set-window-pos (hwnd-sap (frame-hwnd frame))
                  (sb-sys:int-sap 0)
                  0 0 0 0
                  (logior +swp-nosize+ +swp-nomove+ +swp-nozorder+
                          +swp-hidewindow+))
  frame)

(define-alien-type win-msg
  (struct win-msg
    (hwnd system-area-pointer)
    (message (unsigned 32))
    (padding1 (unsigned 32))
    (wparam (unsigned 64))
    (lparam (signed 64))
    (time (unsigned 32))
    (x (signed 32))
    (y (signed 32))
    (padding2 (unsigned 32))))

(defun dispatch-queued-message (&optional wait?)
  "Dispatch one Windows message.
WAIT? uses GetMessage and blocks until a message arrives.  The default
uses PeekMessage and returns false when the queue is empty."
  (with-alien ((message win-msg))
    (let ((sap (alien-sap (addr message))))
      (cond
        (wait?
         (when (plusp (get-message-w sap (sb-sys:int-sap 0) 0 0))
           (translate-message sap)
           (dispatch-message-w sap)
           t))
        ((plusp (peek-message-w sap (sb-sys:int-sap 0) 0 0 +pm-remove+))
         (dispatch-message-w sap)
         t)))))

(defmethod process-next-event ((port windows-port) &key wait-function timeout)
  "Dispatch one Windows message for PORT.
TIMEOUT T waits for the next message.  Any other TIMEOUT, including
NIL, takes a queued message and does not wait."
  (declare (ignore port wait-function))
  (dispatch-queued-message (eq timeout t)))

(defun windows-frame-shown? (frame)
  "Return true when FRAME has a visible Windows window."
  (let ((top (frame-top-level-sheet frame)))
    (when (and top (sheet-mirror top))
      (let ((sap (hwnd-sap (sheet-mirror top))))
        (and (plusp (is-window sap))
             (plusp (is-window-visible sap)))))))

(defun wait-for-windows-frame (frame)
  "Wait until FRAME receives one Windows message.
Return true when FRAME is still shown.  Return false when it has no
visible window, so an unshown frame does not block the command loop."
  (when (windows-frame-shown? frame)
    (let ((port (frame-port frame)))
      (when port
        (process-next-event port :timeout t)))
    (windows-frame-shown? frame)))

(setf *frame-input-waiter* #'wait-for-windows-frame)

(define-application-frame windows-show-frame ()
  ()
  (:pretty-name "Shown Frame")
  (:panes
   (name :label :label "Name" :width 80 :height 16))
  (:layouts
   (default (progn name)))
  (:geometry 200 80))

(behavior 'windows-shown-frame
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-show-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((sap (hwnd-sap (frame-hwnd frame))))
             (should-eq frame (run-frame-top-level frame))
             (with-alien ((rect win-rect))
               (get-window-rect sap (addr rect))
               (should= -32000 (slot rect 'left))
               (should= -32000 (slot rect 'top)))
             (enable-frame manager frame)
             (multiple-value-bind (work-x work-y) (work-area-origin)
               (with-alien ((rect win-rect))
                 (get-window-rect sap (addr rect))
                 (should= (+ work-x 40) (slot rect 'left))
                 (should= (+ work-y 40) (slot rect 'top))))
             (should-equal "Shown Frame" (window-text (frame-hwnd frame)))
             (should-be-true (plusp (is-window-visible sap)))
             (let* ((start (get-internal-real-time))
                    (timer (sb-ext:make-timer
                            (lambda ()
                              (post-message-w sap +wm-close+ 0 0))
                            :thread t)))
               (unwind-protect
                    (progn
                      (sb-ext:schedule-timer timer 0.3)
                      (should-eq frame (run-frame-top-level frame))
                      (should-be-true
                       (> (- (get-internal-real-time) start)
                          (* 0.15 internal-time-units-per-second))))
                 (sb-ext:unschedule-timer timer)))
             (should= 0 (is-window sap))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

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

(defmethod (setf gadget-text-style) :after (style (gadget gadget))
  (declare (ignore style))
  (assign-control-font gadget))

(defun disable-window-theme (hwnd)
  "Draw HWND with the classic control colors instead of the visual style."
  (with-utf16 (empty "")
    (set-window-theme (hwnd-sap hwnd) (alien-sap empty) (alien-sap empty))))

(defun note-control-color (gadget)
  "Ask GADGET to paint again when it has a window."
  (when (sheet-mirror gadget)
    (when (or (gadget-foreground gadget) (gadget-background gadget))
      (disable-window-theme (sheet-mirror gadget)))
    (invalidate-rect (hwnd-sap (sheet-mirror gadget)) (sb-sys:int-sap 0) 1)))

(defmethod (setf gadget-foreground) :after (color (gadget gadget))
  (declare (ignore color))
  (note-control-color gadget))

(defmethod (setf gadget-background) :after (color (gadget gadget))
  (declare (ignore color))
  (note-control-color gadget))

(defmethod (setf frame-background) :after (color (frame application-frame))
  (declare (ignore color))
  (let ((top (frame-top-level-sheet frame)))
    (when (and top (sheet-mirror top))
      (invalidate-rect (hwnd-sap (sheet-mirror top)) (sb-sys:int-sap 0) 1))))

(defun null-terminated-utf16 (buffer max)
  "Return the text in BUFFER, which holds at most MAX UTF-16 units."
  (let ((length 0))
    (loop for index from 0 below max
          until (zerop (deref buffer index))
          do (incf length))
    (utf16-to-string buffer length)))

(defun control-font-metrics (hwnd)
  "Return the face, weight, and height of the font HWND is using.
The face is the one GDI selected."
  (let* ((handle (ldb (byte 64 0) (send-message hwnd +wm-getfont+ 0 0)))
         (font (sb-sys:int-sap handle))
         (hdc (get-dc (hwnd-sap hwnd))))
    (unless (and (plusp handle) (plusp (sb-sys:sap-int hdc)))
      (when (plusp (sb-sys:sap-int hdc))
        (release-dc (hwnd-sap hwnd) hdc))
      (return-from control-font-metrics (values nil nil nil)))
    (unwind-protect
         (let ((old (select-object hdc font))
               (face-buffer (make-alien (unsigned 16) 64))
               (logfont (make-alien (unsigned 8) 92)))
           (unwind-protect
                (let ((copied (get-text-face-w hdc 64 (alien-sap face-buffer)))
                      (written (get-object-w font 92 (alien-sap logfont))))
                  (values (when (plusp copied)
                            (null-terminated-utf16 face-buffer 64))
                          (when (>= written 20)
                            (sb-sys:signed-sap-ref-32 (alien-sap logfont) 16))
                          (when (>= written 4)
                            (sb-sys:signed-sap-ref-32 (alien-sap logfont) 0))))
             (select-object hdc old)
             (free-alien face-buffer)
             (free-alien logfont)))
      (release-dc (hwnd-sap hwnd) hdc))))

(defun load-private-font (pathname)
  "Make the font file PATHNAME available to this process.
Other programs do not see the font.  Returns the number of faces
added.  Returns NIL when the file cannot be loaded."
  (let ((file (probe-file pathname)))
    (when file
      (with-utf16 (name (namestring file))
        (let ((count (add-font-resource-ex-w (alien-sap name)
                                             +fr-private+
                                             (sb-sys:int-sap 0))))
          (when (plusp count)
            count))))))

(define-application-frame windows-font-frame ()
  ()
  (:pretty-name "Windows Font")
  (:panes
   (plain :label :label "Plain")
   (styled :label :label "Styled"
           :text-style (make-text-style :fix :bold 16)))
  (:layouts
   (default (vertically () plain styled)))
  (:geometry 160 80))

(behavior 'load-private-font
  (should-be-null (load-private-font #p"C:/no/such/FitzCLIM-font.ttf"))
  (should= 0 (font-pitch (make-text-style "Named Face" :roman 12)))
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-font-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((plain (find-pane-named frame 'plain))
                 (styled (find-pane-named frame 'styled)))
             (multiple-value-bind (face weight height)
                 (control-font-metrics (sheet-mirror plain))
               (should-equal "Segoe UI" face)
               (should= +fw-normal+ weight)
               (should-be-true (minusp height)))
             (multiple-value-bind (face weight height)
                 (control-font-metrics (sheet-mirror styled))
               (should-equal "Consolas" face)
               (should= +fw-bold+ weight)
               (should-be-true (minusp height)))
             (setf (gadget-text-style plain)
                   (make-text-style :serif :roman 14))
             (multiple-value-bind (face weight height)
                 (control-font-metrics (sheet-mirror plain))
               (should-equal "Times New Roman" face)
               (should= +fw-normal+ weight)
               (should-be-true (minusp height)))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame windows-color-frame ()
  ()
  (:pretty-name "Windows Color")
  (:panes
   (plain :label :label "Plain")
   (colored :label :label "Colored"
            :foreground (make-rgb-color 1 0 0)
            :background +black+))
  (:layouts
   (default (outlining (:thickness 8 :ink (make-rgb-color 0 1 0))
              (vertically () plain colored))))
  (:geometry 80 40))

(behavior 'frame-client-pixel
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-color-frame :background +black+))
        (red #x0000ff)
        (green #x00ff00))
    (unwind-protect
         (progn
           (should= #x000000 (frame-client-pixel frame 0 0))
           (should= green (frame-client-pixel frame 1 10))
           (should= #x000000 (frame-client-pixel frame 6 6))
           (adopt-frame manager frame)
           (let ((top (frame-top-level-sheet frame))
                 (plain (find-pane-named frame 'plain))
                 (colored (find-pane-named frame 'colored))
                 (hdc (get-dc (sb-sys:int-sap 0))))
             (unwind-protect
                 (progn
                   (multiple-value-bind (handled brush)
                       (handle-message (sheet-mirror top) +wm-ctlcolorstatic+
                                       (sb-sys:sap-int hdc)
                                       (sheet-mirror plain))
                     (declare (ignore brush))
                     (should-be-null handled))
                   (multiple-value-bind (handled brush)
                       (handle-message (sheet-mirror top) +wm-ctlcolorstatic+
                                       (sb-sys:sap-int hdc)
                                       (sheet-mirror colored))
                     (should-be-true handled)
                     (should-be-true (plusp brush))
                     (should= red (logand (get-text-color hdc) #xffffff))
                     (should= #x000000 (logand (get-bk-color hdc) #xffffff))))
               (release-dc (sb-sys:int-sap 0) hdc))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(behavior 'colored-window-proc
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-color-frame
                                       :background +black+)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (enable-frame manager frame)
           (loop repeat 32
                 while (dispatch-queued-message))
           (should-be-null *window-proc-error*)
           (disable-frame manager frame))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

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

(define-application-frame windows-event-frame ()
  ()
  (:pretty-name "Windows Events")
  (:panes
   (canvas :application :width 80 :height 60))
  (:layouts
   (default (progn canvas)))
  (:geometry 80 60))

(behavior 'windows-events
  (setf *window-proc-error* nil)
  (setf *windows-modifier-state* 0)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-event-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((canvas (find-pane-named frame 'canvas))
                 (port (frame-manager-port manager)))
             (send-message (sheet-mirror (frame-top-level-sheet frame)) +wm-size+
                           0 (logior 90 (ash 70 16)))
             (should= 90 (frame-width frame))
             (should= 70 (frame-height frame))
             (should= 90 (bounding-rectangle-width
                          (sheet-region (frame-top-level-sheet frame))))
             (send-message (sheet-mirror canvas) +wm-lbuttondown+
                           0 (logior 3 (ash 4 16)))
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-press-event))
               (should-eq canvas (event-sheet event))
               (should= 3 (pointer-event-x event))
               (should= 4 (pointer-event-y event))
               (should-eq :left (pointer-event-button event))
               (should= 0 (event-modifier-state event))
               (should-be-true (event-matches-gesture-name-p event :select)))
             (send-message (sheet-mirror canvas) +wm-lbuttondown+
                           +mk-shift+ (logior 1 (ash 2 16)))
             (should= +shift-key+
                      (event-modifier-state (port-next-event port)))
             (send-message (sheet-mirror canvas) +wm-rbuttonup+
                           0 (logior 5 (ash 6 16)))
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-button-release-event))
               (should-eq :right (pointer-event-button event))
               (should= 5 (pointer-event-x event)))
             (send-message (sheet-mirror canvas) +wm-mbuttondown+ 0 0)
             (should-eq :middle
                        (pointer-event-button (port-next-event port)))
             (send-message (sheet-mirror canvas) +wm-mousemove+
                           0 (logior 7 (ash 8 16)))
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'pointer-motion-event))
               (should= 7 (pointer-event-x event))
               (should= 8 (pointer-event-y event)))
             (send-message (sheet-mirror canvas) +wm-keydown+ #x25 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'key-press-event))
               (should-eq :left (keyboard-event-key-name event))
               (should-be-null (keyboard-event-character event))
               (should-eq canvas (event-sheet event)))
             (send-message (sheet-mirror canvas) +wm-char+ 65 0)
             (should-eq #\A
                        (keyboard-event-character (port-next-event port)))
             (send-message (sheet-mirror canvas) +wm-keydown+ #x10 0)
             (send-message (sheet-mirror canvas) +wm-keydown+ 66 0)
             (let ((shift (port-next-event port))
                   (letter (port-next-event port)))
               (should-eq :shift (keyboard-event-key-name shift))
               (should= +shift-key+ (event-modifier-state letter))
               (should-eq :b (keyboard-event-key-name letter)))
             (send-message (sheet-mirror canvas) +wm-keyup+ #x10 0)
             (let ((event (port-next-event port)))
               (should-be-true (typep event 'key-release-event))
               (should-eq :shift (keyboard-event-key-name event))
               (should= 0 *windows-modifier-state*))
             (should-be-null (port-next-event port))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame windows-drawing-frame ()
  ()
  (:pretty-name "Windows Drawing")
  (:panes
   (canvas :application :width 80 :height 60))
  (:layouts
   (default (progn canvas)))
  (:geometry 80 60))

(behavior 'windows-drawing
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-drawing-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((canvas (find-pane-named frame 'canvas)))
             (multiple-value-bind (width height) (sheet-pixel-size canvas)
               (should= 80 width)
               (should= 60 height))
             (should-be-true (typep (stream-medium canvas) 'gdi-medium))
             (draw-rectangle* canvas 2 2 16 12 :ink +red+ :filled t)
             (draw-point* canvas 24 6 :ink +black+)
             (draw-line* canvas 0 18 30 18 :ink +blue+)
             (draw-circle* canvas 58 18 8 :ink +green+ :filled t)
             (draw-polygon* canvas '(40 40 70 40 40 55) :ink +red+ :filled t)
             (draw-text* canvas "Ag" 2 34 :align-y :top :ink +black+
                         :text-style (make-text-style :sans-serif :bold 16))
             (let ((count (output-record-count (stream-output-history canvas))))
               (should= 6 count)
               (send-message (sheet-mirror canvas) +wm-paint+ 0 0)
               (should= count
                        (output-record-count (stream-output-history canvas))))
             (should= #xffffff (pane-pixel canvas 0 0))
             (should= #x0000ff (pane-pixel canvas 6 6))
             (should= #xffffff (pane-pixel canvas 20 6))
             (should= #x000000 (pane-pixel canvas 24 6))
             (should= #xff0000 (pane-pixel canvas 10 18))
             (should= #xffffff (pane-pixel canvas 10 19))
             (should= #x00ff00 (pane-pixel canvas 58 18))
             (should= #xffffff (pane-pixel canvas 58 8))
             (should= #x0000ff (pane-pixel canvas 42 42))
             (should-be-true (pane-has-dark-pixel? canvas 2 34 24 16))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame windows-pick-frame ()
  ((word
    :initform nil
    :accessor picked-word))
  (:pretty-name "Windows Pick")
  (:panes
   (canvas :application :width 120 :height 40))
  (:layouts
   (default (progn canvas)))
  (:geometry 120 40))

(define-command (com-pick-word :command-table windows-pick-frame :name t)
    ((word string))
  "Remember WORD on the current frame and return it."
  (setf (picked-word *application-frame*) word))

(define-presentation-to-command-translator pick-word
    (string com-pick-word windows-pick-frame
     :gesture :select)
    (object)
  (list object))

(behavior 'windows-pointer-command
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-pick-frame)))
    (unwind-protect
         (progn
           (should-be-null (read-frame-command frame))
           (adopt-frame manager frame)
           (let ((canvas (find-pane-named frame 'canvas)))
             (setf (stream-cursor-position canvas) (values 2 10))
             (present "Nostromo" :stream canvas :type 'string)
             (send-message (sheet-mirror canvas) +wm-lbuttondown+
                           0 (logior 70 (ash 10 16)))
             (should-be-null (read-frame-command frame))
             (setf (frame-pending-commands frame) '((com-pick-word "queued")))
             (send-message (sheet-mirror canvas) +wm-mousemove+
                           0 (logior 10 (ash 10 16)))
             (send-message (sheet-mirror canvas) +wm-rbuttondown+
                           0 (logior 10 (ash 10 16)))
             (send-message (sheet-mirror canvas) +wm-lbuttondown+
                           0 (logior 10 (ash 10 16)))
             (should-equal "queued" (second (read-frame-command frame)))
             (let ((command (read-frame-command frame)))
               (should-eq 'com-pick-word (first command))
               (should-equal "Nostromo" (second command)))
             (send-message (sheet-mirror canvas) +wm-lbuttondown+
                           0 (logior 10 (ash 10 16)))
             (should-eq frame (run-frame-top-level frame))
             (should-equal "Nostromo" (picked-word frame))
             (when *window-proc-error*
               (error "The window procedure failed: ~A" *window-proc-error*))
             (disown-frame manager frame)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-application-frame windows-menu-frame ()
  ((total
    :initform 0
    :accessor menu-total))
  (:pretty-name "Windows Menu")
  (:panes
   (canvas :application :width 40 :height 20))
  (:layouts
   (default (progn canvas)))
  (:geometry 40 20))

(define-command (com-menu-add :command-table windows-menu-frame
                              :name t :menu "Add")
    ()
  "Add one to the frame total and return the new total."
  (incf (menu-total *application-frame*)))

(define-command (com-menu-hidden :command-table windows-menu-frame :name t)
    ()
  "Return the frame total.  This command is not on the menu."
  (menu-total *application-frame*))

(define-application-frame windows-no-menu-frame ()
  ()
  (:menu-bar nil)
  (:panes
   (banner :label :label "Quiet" :width 40 :height 16))
  (:layouts
   (default (vertically () banner)))
  (:geometry 40 20))

(behavior 'windows-command-menu
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (quiet (make-application-frame 'windows-no-menu-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager quiet)
           (should-be-null (frame-menu-table quiet))
           (should-be-null (gethash quiet *frame-menu-ids*))
           (disown-frame manager quiet))
      (when (member quiet (frame-manager-frames manager) :test #'eq)
        (disown-frame manager quiet))))
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-menu-frame)))
    (unwind-protect
         (progn
           (adopt-frame manager frame)
           (let ((menu (get-menu (hwnd-sap (frame-hwnd frame))))
                 (ids (gethash frame *frame-menu-ids*)))
             (should-be-true (plusp (sb-sys:sap-int menu)))
             (should= 1 (length ids))
             (send-message (frame-hwnd frame) +wm-command+ (first ids) 0))
           (should-eq frame (run-frame-top-level frame))
           (should= 1 (menu-total frame))
           (when *window-proc-error*
             (error "The window procedure failed: ~A" *window-proc-error*))
           (disown-frame manager frame)
           (should-be-null (gethash frame *frame-menu-ids*)))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))

(define-command-table windows-submenu-more)

(define-application-frame windows-submenu-frame ()
  ((total
    :initform 0
    :accessor submenu-total)
   (note
    :initform nil
    :accessor submenu-note))
  (:command-table (windows-submenu-frame :inherit-from (windows-submenu-more)))
  (:pretty-name "Windows Submenu")
  (:panes
   (canvas :application :width 40 :height 20))
  (:layouts
   (default (progn canvas)))
  (:geometry 40 20))

(define-command (com-submenu-add :command-table windows-submenu-frame
                                 :name t :menu "Add")
    ()
  "Add one to the frame total and return the new total."
  (incf (submenu-total *application-frame*)))

(define-command (com-submenu-note :command-table windows-submenu-more :name t)
    ()
  "Remember that the submenu command ran."
  (setf (submenu-note *application-frame*) 7))

(behavior 'windows-command-submenu
  (setf *window-proc-error* nil)
  (let ((manager (make-windows-frame-manager))
        (frame (make-application-frame 'windows-submenu-frame)))
    (unwind-protect
         (progn
           (add-menu-item-to-command-table 'windows-submenu-more
                                           "Note" :command 'com-submenu-note
                                           :errorp nil)
           (add-menu-item-to-command-table (frame-command-table frame)
                                           "More" :menu 'windows-submenu-more
                                           :documentation "Further commands."
                                           :errorp nil)
           (adopt-frame manager frame)
           (let ((menu (get-menu (hwnd-sap (frame-hwnd frame))))
                 (ids (gethash frame *frame-menu-ids*))
                 (popup nil))
             (should-be-true (plusp (sb-sys:sap-int menu)))
             (should= 2 (get-menu-item-count menu))
             (should= 2 (length ids))
             (should-be-false (plusp (sb-sys:sap-int (get-sub-menu menu 0))))
             (setf popup (get-sub-menu menu 1))
             (should-be-true (plusp (sb-sys:sap-int popup)))
             (should= 1 (get-menu-item-count popup))
             (should= (first ids) (get-menu-item-id menu 0))
             (should= #xffffffff (get-menu-item-id menu 1))
             (should= (second ids) (get-menu-item-id popup 0))
             (send-message (frame-hwnd frame) +wm-command+ (second ids) 0))
           (should-eq frame (run-frame-top-level frame))
           (should= 7 (submenu-note frame))
           (should= 0 (submenu-total frame))
           (when *window-proc-error*
             (error "The window procedure failed: ~A" *window-proc-error*))
           (let ((note-id (second (gethash frame *frame-menu-ids*))))
             (disown-frame manager frame)
             (should-be-null (gethash frame *frame-menu-ids*))
             (should-be-null (gethash note-id *menu-commands*))))
      (when (member frame (frame-manager-frames manager) :test #'eq)
        (disown-frame manager frame)))))
