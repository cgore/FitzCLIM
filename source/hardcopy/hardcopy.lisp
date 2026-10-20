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

(defpackage :FitzCLIM/hardcopy
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/opacity
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style
        :FitzCLIM/output-record
        :FitzCLIM/medium
        :FitzCLIM/medium/drawing)
  (:export :postscript-medium
           :postscript-stream
           :with-output-to-postscript-stream))
(in-package :FitzCLIM/hardcopy)

(defclass postscript-medium (basic-medium)
  ((page-width
    :initarg :page-width
    :initform 612
    :reader postscript-page-width
    :documentation "The page width, in points.")
   (page-height
    :initarg :page-height
    :initform 792
    :reader postscript-page-height
    :documentation "The page height, in points.  CLIM y grows downward,
so a CLIM y of 0 is this far up the page.")
   (output
    :reader postscript-output
    :documentation "The stream that receives PostScript operators."))
  (:documentation
   "A medium that writes PostScript.  The page is one string of
operators.  Y increases upward in that string."))

(defmethod initialize-instance :after ((medium postscript-medium) &key)
  (setf (slot-value medium 'output) (make-string-output-stream))
  (write-line "%!PS" (postscript-output medium)))

(defclass postscript-stream (output-recording-stream)
  ()
  (:documentation
   "A recording stream whose medium writes PostScript.  Drawing both
records output and appends operators."))

(defmethod initialize-instance :after
    ((stream postscript-stream) &key (page-width 612) (page-height 792))
  (setf (stream-medium stream)
        (make-instance 'postscript-medium
                       :page-width page-width
                       :page-height page-height
                       :sheet stream)))

(defun postscript-number (value)
  "Return VALUE as a PostScript number.  A whole number has no decimal."
  (let ((number (coerce value 'double-float)))
    (if (= number (fround number))
        (princ-to-string (round number))
        (format nil "~,3F" number))))

(defun write-postscript (medium &rest tokens)
  "Write TOKENS as one PostScript line.  A number is printed as a number."
  (format (postscript-output medium)
          "~{~A~^ ~}~%"
          (mapcar (lambda (token)
                    (if (realp token)
                        (postscript-number token)
                        token))
                  tokens)))

(defun postscript-y (medium y)
  "Return the PostScript y that corresponds to CLIM y on MEDIUM."
  (- (postscript-page-height medium) y))

(defun postscript-rgb (ink)
  "Return red, green, blue, and true when INK is drawn.
Foreground is black.  Background is white.  Transparent ink and an
opacity below one half are omitted."
  (cond
    ((or (null ink) (eq ink +foreground-ink+))
     (values 0 0 0 t))
    ((eq ink +background-ink+)
     (values 1 1 1 t))
    ((eq ink +transparent-ink+)
     (values nil nil nil nil))
    ((and (opacityp ink) (< (opacity-value ink) 1/2))
     (values nil nil nil nil))
    ((colorp ink)
     (multiple-value-bind (red green blue) (color-rgb ink)
       (values red green blue t)))
    ((colored-design? ink)
     (multiple-value-bind (red green blue alpha) (design-rgba ink)
       (if (< alpha 1/2)
           (values nil nil nil nil)
           (values red green blue t))))
    (t
     (values 0 0 0 t))))

(defun begin-postscript-ink (medium ink line-style)
  "Set MEDIUM's color and line width.  Return true when INK is drawn."
  (multiple-value-bind (red green blue draw?) (postscript-rgb ink)
    (when draw?
      (write-postscript medium
                        (if line-style (line-style-thickness line-style) 1)
                        "setlinewidth")
      (write-postscript medium red green blue "setrgbcolor"))
    draw?))

(defun postscript-string (string)
  "Return STRING with parentheses and backslashes escaped."
  (with-output-to-string (out)
    (loop for character across string
          do (case character
               (#\( (write-string "\\(" out))
               (#\) (write-string "\\)" out))
               (#\\ (write-string "\\\\" out))
               (otherwise (write-char character out))))))

(defmethod medium-draw-point* ((medium postscript-medium) x y &key ink line-style)
  (when (begin-postscript-ink medium ink line-style)
    (write-postscript medium x (postscript-y medium y) "moveto")
    (write-postscript medium 0 0 "rlineto")
    (write-postscript medium "stroke")))

(defmethod medium-draw-line* ((medium postscript-medium) x1 y1 x2 y2
                              &key ink line-style)
  (when (begin-postscript-ink medium ink line-style)
    (write-postscript medium x1 (postscript-y medium y1) "moveto")
    (write-postscript medium x2 (postscript-y medium y2) "lineto")
    (write-postscript medium "stroke")))

(defmethod medium-draw-rectangle* ((medium postscript-medium) x1 y1 x2 y2
                                   &key filled ink line-style)
  (when (begin-postscript-ink medium ink line-style)
    (let* ((left (min x1 x2))
           (right (max x1 x2))
           (lower (max y1 y2))
           (width (- right left))
           (height (abs (- y1 y2))))
      (write-postscript medium left (postscript-y medium lower) "moveto")
      (write-postscript medium width 0 "rlineto")
      (write-postscript medium 0 height "rlineto")
      (write-postscript medium (- width) 0 "rlineto")
      (write-postscript medium "closepath")
      (write-postscript medium (if filled "fill" "stroke")))))

(defmethod medium-draw-polygon* ((medium postscript-medium) coordinates
                                 &key closed filled ink line-style)
  (when (and coordinates (begin-postscript-ink medium ink line-style))
    (write-postscript medium
                      (first coordinates)
                      (postscript-y medium (second coordinates))
                      "moveto")
    (loop for (x y) on (cddr coordinates) by #'cddr
          do (write-postscript medium x (postscript-y medium y) "lineto"))
    (when closed
      (write-postscript medium "closepath"))
    (write-postscript medium (if filled "fill" "stroke"))))

(defmethod medium-draw-ellipse* ((medium postscript-medium) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 &key filled start-angle end-angle ink line-style)
  (when (begin-postscript-ink medium ink line-style)
    (let ((radius (max (abs radius-1-dx) (abs radius-1-dy)
                       (abs radius-2-dx) (abs radius-2-dy)))
          (start (if start-angle (round (* (- start-angle) (/ 180 pi))) 0))
          (end (if end-angle (round (* (- end-angle) (/ 180 pi))) 360)))
      (write-postscript medium
                        center-x (postscript-y medium center-y)
                        radius start end "arc")
      (when filled
        (write-postscript medium "closepath"))
      (write-postscript medium (if filled "fill" "stroke")))))

(defmethod medium-draw-text* ((medium postscript-medium) string x y
                              &key start end align-x align-y text-style ink)
  (declare (ignore start end))
  (multiple-value-bind (red green blue draw?) (postscript-rgb ink)
    (when draw?
      (let* ((style (or text-style *default-text-style*))
             (size (text-style-point-size style)))
        (multiple-value-bind (width ascent descent) (text-metrics style string)
          (let ((dx (ecase (or align-x :left)
                      (:left 0)
                      (:center (- (/ width 2)))
                      (:right (- width))))
                (dy (ecase (or align-y :baseline)
                      (:baseline 0)
                      (:top ascent)
                      (:bottom (- descent))
                      (:center (/ (- ascent descent) 2)))))
            (write-postscript medium red green blue "setrgbcolor")
            (write-postscript medium
                              "/Courier" "findfont" size "scalefont" "setfont")
            (write-postscript medium
                              (+ x dx)
                              (- (postscript-y medium y) dy)
                              "moveto")
            (write-postscript medium
                              (concatenate 'string "(" (postscript-string string) ")")
                              "show")))))))

(defun postscript-page (stream)
  "Return the PostScript written to STREAM so far."
  (get-output-stream-string (postscript-output (stream-medium stream))))

(defmacro with-output-to-postscript-stream
    ((stream &key (page-width 612) (page-height 792)) &body body)
  "Bind STREAM to a PostScript stream, run BODY, and return the page.
PAGE-WIDTH and PAGE-HEIGHT are in points.  The default page is
612 by 792.  CLIM y increases downward."
  `(let ((,stream (make-instance 'postscript-stream
                                 :page-width ,page-width
                                 :page-height ,page-height)))
     ,@body
     (postscript-page ,stream)))

(behavior 'with-output-to-postscript-stream
  (let ((page (with-output-to-postscript-stream (stream :page-height 100)
                (draw-line* stream 0 0 10 0)
                (draw-rectangle* stream 0 0 10 4 :ink +red+ :filled t)
                (draw-text* stream "A(b)" 0 0)
                (should= 3 (output-record-count (stream-output-history stream))))))
    (should-be-true (search "0 100 moveto" page))
    (should-be-true (search "10 100 lineto" page))
    (should-be-true (search "1 0 0 setrgbcolor" page))
    (should-be-true (search "0 96 moveto" page))
    (should-be-true (search "10 0 rlineto" page))
    (should-be-true (search "fill" page))
    (should-be-true (search "/Courier findfont 12 scalefont setfont" page))
    (should-be-true (search "(A\\(b\\)) show" page)))
  (let ((page (with-output-to-postscript-stream (stream :page-height 100)
                (draw-line* stream 0 0 4 0 :ink +transparent-ink+))))
    (should-be-false (search "lineto" page))))
