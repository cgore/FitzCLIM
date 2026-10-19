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

(defpackage :FitzCLIM/medium
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style
        :FitzCLIM/output-record)
  (:export :basic-medium
           :invoke-with-drawing-options
           :medium
           :medium-background
           :medium-clipping-region
           :medium-default-text-style
           :medium-foreground
           :medium-ink
           :medium-line-style
           :medium-merged-text-style
           :medium-sheet
           :medium-text-style
           :medium-transformation
           :medium-draw-ellipse*
           :medium-draw-line*
           :medium-draw-point*
           :medium-draw-polygon*
           :medium-draw-rectangle*
           :medium-draw-text*
           :mediump
           :with-drawing-options
           :with-identity-transformation
           :with-rotation
           :with-scaling
           :with-translation))
(in-package :FitzCLIM/medium)

(defclass medium ()
  ((sheet
    :initarg :sheet
    :initform nil
    :accessor medium-sheet
    :documentation "The sheet this medium draws on, or NIL.")
   (foreground
    :initarg :foreground
    :initform +black+
    :accessor medium-foreground)
   (background
    :initarg :background
    :initform +white+
    :accessor medium-background)
   (ink
    :initarg :ink
    :initform +foreground-ink+
    :accessor medium-ink)
   (transformation
    :initarg :transformation
    :initform +identity-transformation+
    :accessor medium-transformation)
   (clipping-region
    :initarg :clipping-region
    :initform +everywhere+
    :accessor medium-clipping-region)
   (line-style
    :initarg :line-style
    :initform (make-line-style)
    :accessor medium-line-style)
   (text-style
    :initarg :text-style
    :initform *default-text-style*
    :accessor medium-text-style)
   (default-text-style
    :initarg :default-text-style
    :initform *default-text-style*
    :accessor medium-default-text-style))
  (:documentation
   "The drawing state of a sheet.  A medium with no sheet can still
receive drawing calls.  A port turns those calls into pixels later."))

(defclass basic-medium (medium)
  ()
  (:documentation "A medium that remembers its drawing state."))

(defun mediump (object)
  "Return true if OBJECT is a medium."
  (typep object 'medium))

(defun medium-merged-text-style (medium)
  "Return the text style used for drawing on MEDIUM.
Each component comes from the medium's text style when that style
specifies it, and from the default text style otherwise."
  (merge-text-styles (medium-text-style medium)
                     (medium-default-text-style medium)))

(defgeneric medium-draw-point* (medium x y &key ink line-style)
  (:documentation "Draw a point on MEDIUM.  X and Y are sheet coordinates.")
  (:method ((medium basic-medium) x y &key ink line-style)
    (declare (ignore medium x y ink line-style))
    nil))

(defgeneric medium-draw-line* (medium x1 y1 x2 y2 &key ink line-style)
  (:documentation "Draw a line on MEDIUM.  The coordinates are sheet coordinates.")
  (:method ((medium basic-medium) x1 y1 x2 y2 &key ink line-style)
    (declare (ignore medium x1 y1 x2 y2 ink line-style))
    nil))

(defgeneric medium-draw-rectangle* (medium x1 y1 x2 y2 &key filled ink line-style)
  (:documentation "Draw a rectangle on MEDIUM.")
  (:method ((medium basic-medium) x1 y1 x2 y2 &key filled ink line-style)
    (declare (ignore medium x1 y1 x2 y2 filled ink line-style))
    nil))

(defgeneric medium-draw-polygon* (medium coordinates &key closed filled ink line-style)
  (:documentation "Draw a polygon on MEDIUM.  COORDINATES is a flat list.")
  (:method ((medium basic-medium) coordinates &key closed filled ink line-style)
    (declare (ignore medium coordinates closed filled ink line-style))
    nil))

(defgeneric medium-draw-ellipse* (medium center-x center-y
                                  radius-1-dx radius-1-dy
                                  radius-2-dx radius-2-dy
                                  &key filled start-angle end-angle ink line-style)
  (:documentation "Draw an ellipse on MEDIUM.")
  (:method ((medium basic-medium) center-x center-y
            radius-1-dx radius-1-dy radius-2-dx radius-2-dy
            &key filled start-angle end-angle ink line-style)
    (declare (ignore medium center-x center-y radius-1-dx radius-1-dy
                     radius-2-dx radius-2-dy filled start-angle end-angle
                     ink line-style))
    nil))

(defgeneric medium-draw-text* (medium string x y
                               &key start end align-x align-y text-style ink)
  (:documentation "Draw STRING on MEDIUM.  X and Y are the alignment point.")
  (:method ((medium basic-medium) string x y
            &key start end align-x align-y text-style ink)
    (declare (ignore medium string x y start end align-x align-y text-style ink))
    nil))

(defun copy-medium-from-stream (stream)
  "Copy STREAM's drawing state onto its medium, when it has one."
  (let ((medium (stream-medium stream)))
    (when medium
      (setf (medium-ink medium) (stream-ink stream)
            (medium-line-style medium) (stream-line-style stream)
            (medium-text-style medium) (stream-text-style stream)
            (medium-transformation medium) (stream-transformation stream)
            (medium-clipping-region medium) (stream-clipping-region stream)))
    medium))

(defun invoke-with-drawing-options (stream function &key
                                                   (ink nil inkp)
                                                   (line-style nil linep)
                                                   (text-style nil textp)
                                                   (clipping-region nil clipp)
                                                   (transformation nil transformp))
  "Bind drawing options around a call of FUNCTION.
A clipping region is intersected with the current clip.  It is
expressed in the user coordinates in effect before TRANSFORMATION.
TRANSFORMATION is applied before the current transformation."
  (let ((old-ink (stream-ink stream))
        (old-line (stream-line-style stream))
        (old-text (stream-text-style stream))
        (old-clip (stream-clipping-region stream))
        (old-transform (stream-transformation stream)))
    (when inkp
      (setf (stream-ink stream) ink))
    (when linep
      (setf (stream-line-style stream) line-style))
    (when textp
      (setf (stream-text-style stream) text-style))
    (when clipp
      (setf (stream-clipping-region stream)
            (region-intersection old-clip
                                 (transform-region old-transform clipping-region))))
    (when transformp
      (setf (stream-transformation stream)
            (compose-transformations old-transform transformation)))
    (copy-medium-from-stream stream)
    (unwind-protect
         (funcall function)
      (setf (stream-ink stream) old-ink
            (stream-line-style stream) old-line
            (stream-text-style stream) old-text
            (stream-clipping-region stream) old-clip
            (stream-transformation stream) old-transform)
      (copy-medium-from-stream stream))))

(defmacro with-drawing-options ((stream &rest options &key ink clipping-region
                                      transformation line-style text-style)
                                &body body)
  "Bind drawing options of STREAM around BODY."
  (declare (ignore ink clipping-region transformation line-style text-style))
  `(invoke-with-drawing-options ,stream (lambda () ,@body) ,@options))

(defmacro with-identity-transformation ((stream) &body body)
  "Run BODY with STREAM's transformation set to the identity."
  (let ((stream-var (gensym "STREAM"))
        (old (gensym "OLD")))
    `(let* ((,stream-var ,stream)
            (,old (stream-transformation ,stream-var)))
       (setf (stream-transformation ,stream-var) +identity-transformation+)
       (copy-medium-from-stream ,stream-var)
       (unwind-protect
            (progn ,@body)
         (setf (stream-transformation ,stream-var) ,old)
         (copy-medium-from-stream ,stream-var)))))

(defmacro with-translation ((stream dx dy) &body body)
  "Run BODY with STREAM's coordinates translated by DX and DY."
  `(invoke-with-drawing-options
    ,stream
    (lambda () ,@body)
    :transformation (make-translation-transformation ,dx ,dy)))

(defmacro with-scaling ((stream sx &optional (sy sx) origin) &body body)
  "Run BODY with STREAM's coordinates scaled by SX and SY."
  `(invoke-with-drawing-options
    ,stream
    (lambda () ,@body)
    :transformation (make-scaling-transformation ,sx ,sy ,origin)))

(defmacro with-rotation ((stream angle &optional origin) &body body)
  "Run BODY with STREAM's coordinates rotated by ANGLE radians."
  `(invoke-with-drawing-options
    ,stream
    (lambda () ,@body)
    :transformation (make-rotation-transformation ,angle ,origin)))

(behavior 'medium
  (let ((medium (make-instance 'basic-medium)))
    (should-be-true (mediump medium))
    (should-eq +black+ (medium-foreground medium))
    (should-eq +white+ (medium-background medium))
    (should-eq +foreground-ink+ (medium-ink medium))
    (should-eq :sans-serif
               (text-style-family (medium-merged-text-style medium)))))
