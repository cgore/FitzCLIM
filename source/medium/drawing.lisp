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

(in-package :FitzCLIM/medium)

(defun effective-line-style (stream thickness)
  "Return the line style to draw with.  THICKNESS overrides the stream."
  (let ((style (stream-line-style stream)))
    (if thickness
        (make-line-style :unit (line-style-unit style)
                         :thickness thickness
                         :dashes (line-style-dashes style)
                         :joint-shape (line-style-joint-shape style)
                         :cap-shape (line-style-cap-shape style))
        style)))

(defun stream-record-position (stream x y)
  "Return X and Y in the current record and in sheet coordinates."
  (multiple-value-bind (sheet-x sheet-y)
      (transform-position (stream-transformation stream) x y)
    (multiple-value-bind (origin-x origin-y)
        (output-record-sheet-origin (stream-current-output-record stream))
      (values (- sheet-x origin-x) (- sheet-y origin-y) sheet-x sheet-y))))

(defun graphic-visible? (stream record)
  "Return true when RECORD's box meets STREAM's clipping region.
RECORD's parent is set while the box is computed, and cleared again
when the record is not visible."
  (setf (output-record-parent record) (stream-current-output-record stream))
  (multiple-value-bind (left top right bottom) (output-record-sheet-box record)
    (let* ((clip (stream-clipping-region stream))
           (visible (or (null clip)
                        (everywhere-p clip)
                        (and (not (nowhere-p clip))
                             (region-intersects-region-p
                              clip
                              (make-rectangle* left top right bottom))))))
      (unless visible
        (setf (output-record-parent record) nil))
      visible)))

(defun finish-graphic (stream record drawer)
  "Record RECORD when recording is on, and call DRAWER when drawing.
Return NIL when RECORD is clipped away."
  (when (graphic-visible? stream record)
    (if (stream-recording-p stream)
        (stream-add-output-record stream record)
        (setf (output-record-parent record) nil))
    (when (and (stream-drawing-p stream) (stream-medium stream))
      (funcall drawer))
    record))

(defun draw-point-1 (stream x y thickness)
  "Record and draw one point."
  (multiple-value-bind (record-x record-y sheet-x sheet-y)
      (stream-record-position stream x y)
    (let* ((style (effective-line-style stream thickness))
           (record (make-point-output-record record-x record-y
                                             :ink (stream-ink stream)
                                             :line-style style
                                             :clipping-region (stream-clipping-region stream))))
      (finish-graphic stream record
                      (lambda ()
                        (medium-draw-point* (stream-medium stream)
                                            sheet-x sheet-y
                                            :ink (stream-ink stream)
                                            :line-style style))))))

(defun draw-line-1 (stream x1 y1 x2 y2 thickness)
  "Record and draw one line."
  (multiple-value-bind (rx1 ry1 sx1 sy1) (stream-record-position stream x1 y1)
    (multiple-value-bind (rx2 ry2 sx2 sy2) (stream-record-position stream x2 y2)
      (let* ((style (effective-line-style stream thickness))
             (record (make-line-output-record rx1 ry1 rx2 ry2
                                              :ink (stream-ink stream)
                                              :line-style style
                                              :clipping-region (stream-clipping-region stream))))
        (finish-graphic stream record
                        (lambda ()
                          (medium-draw-line* (stream-medium stream)
                                             sx1 sy1 sx2 sy2
                                             :ink (stream-ink stream)
                                             :line-style style)))))))

(defun draw-rectangle-1 (stream x1 y1 x2 y2 filled thickness)
  "Record and draw one rectangle."
  (multiple-value-bind (rx1 ry1 sx1 sy1) (stream-record-position stream x1 y1)
    (multiple-value-bind (rx2 ry2 sx2 sy2) (stream-record-position stream x2 y2)
      (let* ((style (effective-line-style stream thickness))
             (record (make-rectangle-output-record rx1 ry1 rx2 ry2
                                                   :filled filled
                                                   :ink (stream-ink stream)
                                                   :line-style style
                                                   :clipping-region
                                                   (stream-clipping-region stream))))
        (finish-graphic stream record
                        (lambda ()
                          (medium-draw-rectangle* (stream-medium stream)
                                                  sx1 sy1 sx2 sy2
                                                  :filled filled
                                                  :ink (stream-ink stream)
                                                  :line-style style)))))))

(defun flat-coordinates (sequence)
  "Return SEQUENCE as a list of coordinates.
A sequence of points becomes alternating x and y values."
  (let ((items (coerce sequence 'list)))
    (if (and items (pointp (first items)))
        (loop for point in items
              collect (point-x point)
              collect (point-y point))
        items)))

(defun draw-polygon-1 (stream coordinates closed filled thickness)
  "Record and draw one polygon."
  (let* ((source (flat-coordinates coordinates))
         (sheet nil)
         (local nil))
    (loop for (x y) on source by #'cddr
          do (multiple-value-bind (record-x record-y sheet-x sheet-y)
                 (stream-record-position stream x y)
               (push record-x local)
               (push record-y local)
               (push sheet-x sheet)
               (push sheet-y sheet)))
    (setf local (nreverse local)
          sheet (nreverse sheet))
    (let* ((style (effective-line-style stream thickness))
           (record (make-polygon-output-record local
                                               :closed closed
                                               :filled filled
                                               :ink (stream-ink stream)
                                               :line-style style
                                               :clipping-region
                                               (stream-clipping-region stream))))
      (finish-graphic stream record
                      (lambda ()
                        (medium-draw-polygon* (stream-medium stream) sheet
                                              :closed closed
                                              :filled filled
                                              :ink (stream-ink stream)
                                              :line-style style))))))

(defun draw-ellipse-1 (stream center-x center-y
                       radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                       filled start-angle end-angle thickness)
  "Record and draw one ellipse.  The radii are transformed as vectors."
  (multiple-value-bind (cx cy sx sy) (stream-record-position stream center-x center-y)
    (multiple-value-bind (r1x r1y)
        (transform-position (stream-transformation stream) radius-1-dx radius-1-dy)
      (multiple-value-bind (origin-x origin-y)
          (transform-position (stream-transformation stream) 0 0)
        (declare (ignore origin-x origin-y))
        (multiple-value-bind (r2x r2y)
            (transform-position (stream-transformation stream) radius-2-dx radius-2-dy)
          (let* ((style (effective-line-style stream thickness))
                 (vector-1-dx (- r1x (nth-value 0 (transform-position
                                                   (stream-transformation stream) 0 0))))
                 (vector-1-dy (- r1y (nth-value 1 (transform-position
                                                   (stream-transformation stream) 0 0))))
                 (vector-2-dx (- r2x (nth-value 0 (transform-position
                                                   (stream-transformation stream) 0 0))))
                 (vector-2-dy (- r2y (nth-value 1 (transform-position
                                                   (stream-transformation stream) 0 0))))
                 (record (make-ellipse-output-record
                          cx cy vector-1-dx vector-1-dy vector-2-dx vector-2-dy
                          :filled filled
                          :start-angle start-angle
                          :end-angle end-angle
                          :ink (stream-ink stream)
                          :line-style style
                          :clipping-region (stream-clipping-region stream))))
            (finish-graphic stream record
                            (lambda ()
                              (medium-draw-ellipse* (stream-medium stream)
                                                    sx sy
                                                    vector-1-dx vector-1-dy
                                                    vector-2-dx vector-2-dy
                                                    :filled filled
                                                    :start-angle start-angle
                                                    :end-angle end-angle
                                                    :ink (stream-ink stream)
                                                    :line-style style)))))))))

(defun draw-text-1 (stream string x y start end align-x align-y text-style)
  "Record and draw one run of text."
  (let ((text (subseq string start (or end (length string)))))
    (multiple-value-bind (record-x record-y sheet-x sheet-y)
        (stream-record-position stream x y)
      (let* ((style (or text-style (stream-text-style stream)))
             (record (make-text-output-record text record-x record-y
                                              :text-style style
                                              :align-x align-x
                                              :align-y align-y
                                              :ink (stream-ink stream)
                                              :clipping-region
                                              (stream-clipping-region stream))))
        (finish-graphic stream record
                        (lambda ()
                          (medium-draw-text* (stream-medium stream) text
                                             sheet-x sheet-y
                                             :align-x align-x
                                             :align-y align-y
                                             :text-style style
                                             :ink (stream-ink stream))))))))

(defun without-thickness (options)
  "Return OPTIONS without a :LINE-THICKNESS entry."
  (loop for (key value) on options by #'cddr
        unless (eq key :line-thickness)
          collect key
          and collect value))

(defun draw-point* (stream x y &rest options &key line-thickness &allow-other-keys)
  "Draw a point at X and Y."
  (apply #'invoke-with-drawing-options
         stream
         (lambda () (draw-point-1 stream x y line-thickness))
         (without-thickness options)))

(defun draw-points* (stream coordinates &rest options)
  "Draw a point for each coordinate pair in COORDINATES."
  (let ((coords (flat-coordinates coordinates)))
    (loop for (x y) on coords by #'cddr
          do (apply #'draw-point* stream x y options))))

(defun draw-point (stream point &rest options)
  "Draw POINT."
  (apply #'draw-point* stream (point-x point) (point-y point) options))

(defun draw-points (stream points &rest options)
  "Draw each point in POINTS."
  (apply #'draw-points* stream points options))

(defun draw-line* (stream x1 y1 x2 y2 &rest options &key line-thickness
                   &allow-other-keys)
  "Draw a line from (X1, Y1) to (X2, Y2)."
  (apply #'invoke-with-drawing-options
         stream
         (lambda () (draw-line-1 stream x1 y1 x2 y2 line-thickness))
         (without-thickness options)))

(defun draw-lines* (stream coordinates &rest options)
  "Draw a line for each group of four coordinates."
  (let ((coords (flat-coordinates coordinates)))
    (loop for (x1 y1 x2 y2) on coords by #'cddddr
          do (apply #'draw-line* stream x1 y1 x2 y2 options))))

(defun draw-line (stream point-1 point-2 &rest options)
  "Draw a line from POINT-1 to POINT-2."
  (apply #'draw-line* stream
         (point-x point-1) (point-y point-1)
         (point-x point-2) (point-y point-2)
         options))

(defun draw-lines (stream points &rest options)
  "Draw a line for each successive pair of POINTS."
  (apply #'draw-lines* stream points options))

(defun draw-rectangle* (stream x1 y1 x2 y2 &rest options &key (filled t) line-thickness
                        &allow-other-keys)
  "Draw the rectangle with corners (X1, Y1) and (X2, Y2)."
  (declare (ignore filled))
  (apply #'invoke-with-drawing-options
         stream
         (lambda ()
           (draw-rectangle-1 stream x1 y1 x2 y2
                             (getf options :filled t)
                             line-thickness))
         (without-thickness (loop for (key value) on options by #'cddr
                                  unless (eq key :filled)
                                    collect key and collect value))))

(defun draw-rectangles* (stream coordinates &rest options)
  "Draw a rectangle for each group of four coordinates."
  (let ((coords (flat-coordinates coordinates)))
    (loop for (x1 y1 x2 y2) on coords by #'cddddr
          do (apply #'draw-rectangle* stream x1 y1 x2 y2 options))))

(defun draw-rectangle (stream point-1 point-2 &rest options)
  "Draw the rectangle with opposite corners POINT-1 and POINT-2."
  (apply #'draw-rectangle* stream
         (point-x point-1) (point-y point-1)
         (point-x point-2) (point-y point-2)
         options))

(defun draw-rectangles (stream points &rest options)
  "Draw a rectangle for each successive pair of POINTS."
  (apply #'draw-rectangles* stream points options))

(defun draw-polygon* (stream coordinates &rest options &key (closed t) (filled t)
                      line-thickness &allow-other-keys)
  "Draw a polygon through the coordinate pairs in COORDINATES."
  (declare (ignore closed filled))
  (apply #'invoke-with-drawing-options
         stream
         (lambda ()
           (draw-polygon-1 stream coordinates
                           (getf options :closed t)
                           (getf options :filled t)
                           line-thickness))
         (without-thickness
          (loop for (key value) on options by #'cddr
                unless (or (eq key :closed) (eq key :filled))
                  collect key and collect value))))

(defun draw-polygon (stream points &rest options)
  "Draw a polygon through POINTS."
  (apply #'draw-polygon* stream points options))

(defun draw-ellipse* (stream center-x center-y
                      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                      &rest options &key (filled t) start-angle end-angle
                        line-thickness &allow-other-keys)
  "Draw an ellipse centered at (CENTER-X, CENTER-Y)."
  (declare (ignore filled start-angle end-angle))
  (apply #'invoke-with-drawing-options
         stream
         (lambda ()
           (draw-ellipse-1 stream center-x center-y
                           radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                           (getf options :filled t)
                           (getf options :start-angle)
                           (getf options :end-angle)
                           line-thickness))
         (without-thickness
          (loop for (key value) on options by #'cddr
                unless (member key '(:filled :start-angle :end-angle))
                  collect key and collect value))))

(defun draw-ellipse (stream center
                     radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                     &rest options)
  "Draw an ellipse centered at CENTER."
  (apply #'draw-ellipse* stream (point-x center) (point-y center)
         radius-1-dx radius-1-dy radius-2-dx radius-2-dy options))

(defun draw-circle* (stream center-x center-y radius &rest options)
  "Draw a circle centered at (CENTER-X, CENTER-Y)."
  (apply #'draw-ellipse* stream center-x center-y radius 0 0 radius options))

(defun draw-circle (stream center radius &rest options)
  "Draw a circle centered at CENTER."
  (apply #'draw-circle* stream (point-x center) (point-y center) radius options))

(defun draw-text* (stream string x y &rest options &key (start 0) end
                   (align-x :left) (align-y :baseline) text-style
                   &allow-other-keys)
  "Draw STRING at X and Y.  The text stays on a horizontal baseline."
  (declare (ignore start end align-x align-y text-style))
  (apply #'invoke-with-drawing-options
         stream
         (lambda ()
           (draw-text-1 stream string x y
                        (getf options :start 0)
                        (getf options :end)
                        (getf options :align-x :left)
                        (getf options :align-y :baseline)
                        (getf options :text-style)))
         (loop for (key value) on options by #'cddr
               unless (member key '(:start :end :align-x :align-y :text-style))
                 collect key and collect value)))

(defun draw-text (stream string point &rest options)
  "Draw STRING at POINT."
  (apply #'draw-text* stream string (point-x point) (point-y point) options))

(defmethod replay-displayed-output-record
    ((record point-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-point* medium (+ x x-offset) (+ y y-offset)
                            :ink (displayed-output-record-ink record)
                            :line-style (displayed-line-style record))))))

(defmethod replay-displayed-output-record
    ((record line-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-line* medium
                           (+ x (line-record-x1 record) x-offset)
                           (+ y (line-record-y1 record) y-offset)
                           (+ x (line-record-x2 record) x-offset)
                           (+ y (line-record-y2 record) y-offset)
                           :ink (displayed-output-record-ink record)
                           :line-style (displayed-line-style record))))))

(defmethod replay-displayed-output-record
    ((record rectangle-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-rectangle* medium
                                (+ x x-offset) (+ y y-offset)
                                (+ x (rectangle-record-width record) x-offset)
                                (+ y (rectangle-record-height record) y-offset)
                                :filled (rectangle-record-filled record)
                                :ink (displayed-output-record-ink record)
                                :line-style (displayed-line-style record))))))

(defmethod replay-displayed-output-record
    ((record polygon-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-polygon*
         medium
         (loop for (px py) in (polygon-record-points record)
               collect (+ x px x-offset)
               collect (+ y py y-offset))
         :closed (polygon-record-closed record)
         :filled (polygon-record-filled record)
         :ink (displayed-output-record-ink record)
         :line-style (displayed-line-style record))))))

(defmethod replay-displayed-output-record
    ((record ellipse-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-ellipse* medium
                              (+ x (ellipse-record-center-x record) x-offset)
                              (+ y (ellipse-record-center-y record) y-offset)
                              (ellipse-record-radius-1-dx record)
                              (ellipse-record-radius-1-dy record)
                              (ellipse-record-radius-2-dx record)
                              (ellipse-record-radius-2-dy record)
                              :filled (ellipse-record-filled record)
                              :start-angle (ellipse-record-start-angle record)
                              :end-angle (ellipse-record-end-angle record)
                              :ink (displayed-output-record-ink record)
                              :line-style (displayed-line-style record))))))

(defmethod replay-displayed-output-record
    ((record text-displayed-output-record) stream x-offset y-offset)
  (let ((medium (stream-medium stream)))
    (when medium
      (multiple-value-bind (x y) (output-record-sheet-origin record)
        (medium-draw-text* medium (displayed-text record)
                           (+ x x-offset) (+ y y-offset)
                           :align-x (displayed-text-align-x record)
                           :align-y (displayed-text-align-y record)
                           :text-style (displayed-text-style record)
                           :ink (displayed-output-record-ink record))))))

(defclass noted-medium (basic-medium)
  ((notes
    :initform nil
    :accessor medium-notes
    :documentation "Drawing calls recorded by the medium tests."))
  (:documentation "A medium that records drawing calls for tests."))

(defmethod medium-draw-point* ((medium noted-medium) x y &key ink line-style)
  (declare (ignore ink line-style))
  (push (list :point x y) (medium-notes medium)))

(defmethod medium-draw-line* ((medium noted-medium) x1 y1 x2 y2 &key ink line-style)
  (declare (ignore ink line-style))
  (push (list :line x1 y1 x2 y2) (medium-notes medium)))

(defmethod medium-draw-rectangle*
    ((medium noted-medium) x1 y1 x2 y2 &key filled ink line-style)
  (declare (ignore filled ink line-style))
  (push (list :rectangle x1 y1 x2 y2) (medium-notes medium)))

(defmethod medium-draw-text* ((medium noted-medium) string x y
                              &key start end align-x align-y text-style ink)
  (declare (ignore start end align-x align-y text-style ink))
  (push (list :text string x y) (medium-notes medium)))

(behavior 'draw-line*
  (let* ((medium (make-instance 'noted-medium))
         (stream (make-instance 'output-recording-stream :medium medium)))
    (draw-line* stream 0 0 10 0)
    (should-equal '(:line 0 0 10 0) (first (medium-notes medium)))
    (should-be-true (output-record-contains-position-p
                     (stream-output-history stream) 5 0))
    (with-translation (stream 5 6)
      (draw-line* stream 0 0 1 1))
    (should-equal '(:line 5 6 6 7) (first (medium-notes medium)))
    (multiple-value-bind (x y)
        (transform-position (stream-transformation stream) 3 4)
      (should= 3 x)
      (should= 4 y))
    (with-drawing-options (stream :clipping-region (make-rectangle* 0 0 2 2))
      (draw-rectangle* stream 10 10 12 12)
      (draw-rectangle* stream 0 0 1 1))
    (should-be-false (output-record-contains-position-p
                      (stream-output-history stream) 11 11))
    (should-be-true (output-record-contains-position-p
                     (stream-output-history stream) 1 1))
    (draw-polygon* stream '(0 0 4 0 0 4))
    (should-be-true (output-record-contains-position-p
                     (stream-output-history stream) 1 1))
    (draw-circle* stream 20 20 3)
    (should-be-true (output-record-contains-position-p
                     (stream-output-history stream) 20 20))
    (draw-text* stream "Go" 0 30)
    (should-be-true (some (lambda (record)
                            (and (typep record 'text-displayed-output-record)
                                 (equal (displayed-text record) "Go")))
                          (output-record-children (stream-output-history stream))))
    (with-output-recording-options (stream :record nil :draw t)
      (let ((before (output-record-count (stream-output-history stream))))
        (draw-point* stream 1 1)
        (should= before (output-record-count (stream-output-history stream)))
        (should-eq :point (first (first (medium-notes medium))))))
    (setf (medium-notes medium) nil)
    (replay (stream-output-history stream) stream)
    (should-be-true (find :line (medium-notes medium) :key #'first))))
