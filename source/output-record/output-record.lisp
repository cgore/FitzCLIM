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

(defpackage :FitzCLIM/output-record
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style)
  (:export :add-output-record
           :clear-output-record
           :compound-output-record
           :delete-output-record
           :displayed-output-record
           :displayed-output-record-clipping-region
           :displayed-line-style
           :displayed-output-record-ink
           :displayed-text
           :displayed-text-align-x
           :displayed-text-align-y
           :displayed-text-style
           :ellipse-displayed-output-record
           :ellipse-record-center-x
           :ellipse-record-center-y
           :ellipse-record-end-angle
           :ellipse-record-filled
           :ellipse-record-radius-1-dx
           :ellipse-record-radius-1-dy
           :ellipse-record-radius-2-dx
           :ellipse-record-radius-2-dy
           :ellipse-record-start-angle
           :erase-output-record
           :highlight-output-record
           :line-displayed-output-record
           :line-record-x1
           :line-record-x2
           :line-record-y1
           :line-record-y2
           :make-ellipse-output-record
           :make-line-output-record
           :make-point-output-record
           :make-polygon-output-record
           :make-rectangle-output-record
           :make-text-output-record
           :map-over-output-records
           :map-over-output-records-containing-position
           :map-over-output-records-overlapping-region
           :output-record
           :output-record-children
           :output-record-contains-position-p
           :output-record-count
           :output-record-highlighted
           :output-record-p
           :output-record-parent
           :output-record-position
           :output-record-refined-position-test
           :output-record-sheet-box
           :output-record-sheet-origin
           :output-recording-stream
           :output-recording-stream-p
           :point-displayed-output-record
           :polygon-displayed-output-record
           :polygon-record-closed
           :polygon-record-filled
           :polygon-record-points
           :rectangle-displayed-output-record
           :rectangle-record-filled
           :rectangle-record-height
           :rectangle-record-width
           :replay
           :replay-displayed-output-record
           :replay-output-record
           :set-output-record-position
           :standard-sequence-output-record
           :standard-tree-output-history
           :standard-tree-output-record
           :stream-add-output-record
           :stream-allows-sensitive-inferiors
           :stream-clipping-region
           :stream-current-output-record
           :stream-cursor-position
           :stream-cursor-x
           :stream-cursor-y
           :stream-drawing-p
           :stream-ink
           :stream-left-margin
           :stream-line-style
           :stream-medium
           :stream-output-history
           :stream-recording-p
           :stream-text-style
           :stream-transformation
           :text-displayed-output-record
           :text-line-height
           :text-metrics
           :text-style-point-size
           :with-new-output-record
           :with-output-recording-options))
(in-package :FitzCLIM/output-record)

(defclass output-record ()
  ((parent
    :initform nil
    :accessor output-record-parent
    :documentation "The record that contains this one, or NIL.")
   (x
    :initform 0
    :initarg :x
    :accessor output-record-x
    :documentation "The origin's x in the parent's coordinates.")
   (y
    :initform 0
    :initarg :y
    :accessor output-record-y
    :documentation "The origin's y in the parent's coordinates.")
   (left
    :initform 0
    :accessor %left
    :documentation "The left edge, in this record's own coordinates.")
   (top
    :initform 0
    :accessor %top
    :documentation "The top edge, in this record's own coordinates.")
   (right
    :initform 0
    :accessor %right
    :documentation "The right edge, in this record's own coordinates.")
   (bottom
    :initform 0
    :accessor %bottom
    :documentation "The bottom edge, in this record's own coordinates.")
   (highlighted
    :initform nil
    :accessor output-record-highlighted
    :documentation "True while the record is highlighted.")
   (start-x
    :initform 0
    :accessor %start-x)
   (start-y
    :initform 0
    :accessor %start-y)
   (end-x
    :initform 0
    :accessor %end-x)
   (end-y
    :initform 0
    :accessor %end-y))
  (:documentation
   "A saved piece of output.  Its position is its origin in its
parent's coordinates.  Its children, when it has them, use coordinates
whose origin is that position.  Y increases downward."))

(defun output-record-p (object)
  "Return true if OBJECT is an output record."
  (typep object 'output-record))

(defun output-record-position (record)
  "Return the X and Y origin of RECORD in its parent's coordinates."
  (values (output-record-x record) (output-record-y record)))

(defun set-output-record-position (record x y)
  "Move RECORD so its origin is X and Y in its parent's coordinates."
  (setf (output-record-x record) x
        (output-record-y record) y)
  (let ((parent (output-record-parent record)))
    (when parent
      (recompute-extent parent)))
  (values x y))

(define-setf-expander output-record-position (record &environment env)
  (multiple-value-bind (vars vals stores writer reader)
      (get-setf-expansion record env)
    (declare (ignore stores writer))
    (let ((x (gensym "X"))
          (y (gensym "Y")))
      (values vars
              vals
              (list x y)
              `(progn
                 (set-output-record-position ,reader ,x ,y)
                 (values ,x ,y))
              `(output-record-position ,reader)))))

(defun set-output-record-extent (record left top right bottom)
  "Set RECORD's edges in its own coordinates."
  (setf (%left record) left
        (%top record) top
        (%right record) right
        (%bottom record) bottom)
  (values left top right bottom))

(defmethod bounding-rectangle* ((record output-record))
  (values (+ (output-record-x record) (%left record))
          (+ (output-record-y record) (%top record))
          (+ (output-record-x record) (%right record))
          (+ (output-record-y record) (%bottom record))))

(defun output-record-start-cursor-position (record)
  "Return the cursor position saved when RECORD was opened."
  (values (%start-x record) (%start-y record)))

(defun set-output-record-start-cursor-position (record x y)
  "Remember that RECORD was opened at cursor X and Y."
  (setf (%start-x record) x
        (%start-y record) y)
  (values x y))

(defun output-record-end-cursor-position (record)
  "Return the cursor position saved when RECORD was closed."
  (values (%end-x record) (%end-y record)))

(defun set-output-record-end-cursor-position (record x y)
  "Remember that RECORD was closed at cursor X and Y."
  (setf (%end-x record) x
        (%end-y record) y)
  (values x y))

(defclass displayed-output-record (output-record)
  ((ink
    :initarg :ink
    :initform +foreground-ink+
    :reader displayed-output-record-ink
    :documentation "The ink this record draws with.")
   (clipping-region
    :initarg :clipping-region
    :initform +everywhere+
    :reader displayed-output-record-clipping-region
    :documentation "The clipping region in effect when this was drawn."))
  (:documentation "An output record that draws one graphic or one run of text."))

(defclass compound-output-record (output-record)
  ((children
    :initform nil
    :accessor %children
    :documentation "The children, from bottommost to topmost."))
  (:documentation "An output record that holds other output records."))

(defclass standard-sequence-output-record (compound-output-record)
  ()
  (:documentation "A compound record that keeps its children in order."))

(defclass standard-tree-output-record (compound-output-record)
  ()
  (:documentation
   "A compound record.  Children are kept in order.  A spatial index
may replace the walk later.  The order is the protocol."))

(defclass standard-tree-output-history (standard-tree-output-record)
  ()
  (:documentation "The output history of a recording stream."))

(defgeneric output-record-children (record)
  (:documentation
   "Return the children of RECORD, from bottommost to topmost.
The list is a fresh one."))

(defmethod output-record-children ((record output-record))
  nil)

(defmethod output-record-children ((record compound-output-record))
  (copy-list (%children record)))

(defun output-record-count (record)
  "Return how many children RECORD has."
  (length (output-record-children record)))

(defun recompute-extent (record)
  "Set RECORD's extent from its children, then update its ancestors.
A displayed record keeps the extent it was given."
  (when (typep record 'compound-output-record)
    (let ((children (%children record))
          (left nil)
          (top nil)
          (right nil)
          (bottom nil))
      (if (null children)
          (set-output-record-extent record 0 0 0 0)
          (progn
            (dolist (child children)
              (multiple-value-bind (child-left child-top child-right child-bottom)
                  (bounding-rectangle* child)
                (setf left (if left (min left child-left) child-left)
                      top (if top (min top child-top) child-top)
                      right (if right (max right child-right) child-right)
                      bottom (if bottom (max bottom child-bottom) child-bottom))))
            (set-output-record-extent record left top right bottom)))))
  (let ((parent (output-record-parent record)))
    (when parent
      (recompute-extent parent)))
  record)

(defun add-output-record (child record)
  "Add CHILD to RECORD as the topmost child.  Return CHILD."
  (unless (typep record 'compound-output-record)
    (error "~S cannot hold children." record))
  (when (and (output-record-parent child)
             (not (eq (output-record-parent child) record)))
    (delete-output-record child (output-record-parent child) nil))
  (setf (output-record-parent child) record)
  (unless (member child (%children record) :test #'eq)
    (setf (%children record) (append (%children record) (list child))))
  (recompute-extent record)
  child)

(defun delete-output-record (child record &optional (errorp t))
  "Remove CHILD from RECORD.  Return true when it was removed.
ERRORP true signals when CHILD is not there."
  (unless (member child (%children record) :test #'eq)
    (when errorp
      (error "~S is not a child of ~S." child record))
    (return-from delete-output-record nil))
  (setf (%children record) (remove child (%children record) :test #'eq))
  (when (eq (output-record-parent child) record)
    (setf (output-record-parent child) nil))
  (recompute-extent record)
  t)

(defun clear-output-record (record)
  "Remove every child of RECORD."
  (dolist (child (%children record))
    (setf (output-record-parent child) nil))
  (setf (%children record) nil)
  (set-output-record-extent record 0 0 0 0)
  (let ((parent (output-record-parent record)))
    (when parent
      (recompute-extent parent)))
  nil)

(defun erase-output-record (record stream &optional (errorp t))
  "Remove RECORD from its parent.  STREAM is the sheet that owns it.
The parent is what changes.  STREAM selects the error when RECORD has
no parent and STREAM's history is not that parent."
  (declare (ignore stream))
  (let ((parent (output-record-parent record)))
    (unless parent
      (when errorp
        (error "~S has no parent." record))
      (return-from erase-output-record nil))
    (delete-output-record record parent errorp)))

(defgeneric highlight-output-record (record stream state)
  (:documentation
   "Highlight RECORD on STREAM when STATE is :HIGHLIGHT, and remove
the highlight when STATE is :UNHIGHLIGHT."))

(defmethod highlight-output-record ((record output-record) stream state)
  (declare (ignore stream))
  (setf (output-record-highlighted record)
        (ecase state
          (:highlight t)
          (:unhighlight nil)))
  state)

(defun output-record-ancestor-origin (record)
  "Return the origin of RECORD's parent in the root record's coordinates."
  (let ((parent (output-record-parent record)))
    (if (null parent)
        (values 0 0)
        (multiple-value-bind (parent-x parent-y)
            (output-record-ancestor-origin parent)
          (values (+ parent-x (output-record-x parent))
                  (+ parent-y (output-record-y parent)))))))

(defun output-record-sheet-origin (record)
  "Return the origin of RECORD in the root record's coordinates."
  (multiple-value-bind (parent-x parent-y)
      (output-record-ancestor-origin record)
    (values (+ parent-x (output-record-x record))
            (+ parent-y (output-record-y record)))))

(defun output-record-sheet-box (record)
  "Return the bounding box of RECORD in the root record's coordinates."
  (multiple-value-bind (origin-x origin-y)
      (output-record-ancestor-origin record)
    (multiple-value-bind (left top right bottom) (bounding-rectangle* record)
      (values (+ origin-x left) (+ origin-y top)
              (+ origin-x right) (+ origin-y bottom)))))

(defun box-overlaps-box? (left-1 top-1 right-1 bottom-1
                          left-2 top-2 right-2 bottom-2)
  "Return true when the two closed boxes overlap."
  (and (<= left-1 right-2) (<= left-2 right-1)
       (<= top-1 bottom-2) (<= top-2 bottom-1)))

(defun box-overlaps-region? (region left top right bottom)
  "Return true when the closed box meets REGION.
NIL and +EVERYWHERE+ cover the box.  +NOWHERE+ misses it.  Any other
region is tested through its bounding box."
  (cond
    ((or (null region) (everywhere-p region)) t)
    ((nowhere-p region) nil)
    (t
     (multiple-value-bind (region-left region-top region-right region-bottom)
         (bounding-rectangle* region)
       (box-overlaps-box? left top right bottom
                          region-left region-top region-right region-bottom)))))

(defgeneric output-record-refined-position-test (record x y)
  (:documentation
   "Return true if RECORD contains X and Y in its own coordinates.
The bounding box has already been checked."))

(defmethod output-record-refined-position-test ((record output-record) x y)
  (declare (ignore record x y))
  t)

(defun output-record-contains-position-p (record x y)
  "Return true if RECORD contains X and Y in its parent's coordinates."
  (multiple-value-bind (left top right bottom) (bounding-rectangle* record)
    (and (<= left x right)
         (<= top y bottom)
         (output-record-refined-position-test
          record
          (- x (output-record-x record))
          (- y (output-record-y record))))))

(defun map-over-output-records (function record &optional (x-offset 0) (y-offset 0))
  "Call FUNCTION on each child of RECORD, from bottommost to topmost.
FUNCTION receives the child and its position plus the offsets."
  (dolist (child (output-record-children record))
    (multiple-value-bind (x y) (output-record-position child)
      (funcall function child (+ x x-offset) (+ y y-offset)))))

(defun map-over-output-records-containing-position
    (function record x y &optional (x-offset 0) (y-offset 0))
  "Call FUNCTION on each child of RECORD that contains X and Y.
X and Y are in RECORD's coordinates.  The offsets shift each child
before the test.  Children are visited from topmost to bottommost."
  (dolist (child (reverse (output-record-children record)))
    (when (output-record-contains-position-p child
                                             (- x x-offset)
                                             (- y y-offset))
      (funcall function child))))

(defun map-over-output-records-overlapping-region
    (function record region &optional (x-offset 0) (y-offset 0))
  "Call FUNCTION on each child of RECORD whose box meets REGION.
REGION is in RECORD's coordinates, shifted by the offsets.  Children
are visited from topmost to bottommost."
  (dolist (child (reverse (output-record-children record)))
    (multiple-value-bind (left top right bottom) (bounding-rectangle* child)
      (when (box-overlaps-region? region
                                  (+ left x-offset) (+ top y-offset)
                                  (+ right x-offset) (+ bottom y-offset))
        (funcall function child)))))

(defgeneric replay-displayed-output-record (record stream x-offset y-offset)
  (:documentation
   "Draw RECORD on STREAM.  Add X-OFFSET and Y-OFFSET to its sheet position.")
  (:method ((record displayed-output-record) stream x-offset y-offset)
    (declare (ignore record stream x-offset y-offset))
    nil))

(defgeneric replay-output-record (record stream &optional region x-offset y-offset)
  (:documentation
   "Replay RECORD onto STREAM.  REGION limits the replay.  NIL replays
everything.  X-OFFSET and Y-OFFSET are added to drawn positions."))

(defmethod replay-output-record
    ((record compound-output-record) stream &optional region (x-offset 0) (y-offset 0))
  (dolist (child (output-record-children record))
    (multiple-value-bind (left top right bottom) (output-record-sheet-box child)
      (when (box-overlaps-region? region left top right bottom)
        (replay-output-record child stream region x-offset y-offset)))))

(defmethod replay-output-record
    ((record displayed-output-record) stream &optional region (x-offset 0) (y-offset 0))
  (multiple-value-bind (left top right bottom) (output-record-sheet-box record)
    (when (box-overlaps-region? region left top right bottom)
      (replay-displayed-output-record record stream x-offset y-offset))))

(defun replay (record stream &optional region)
  "Replay RECORD onto STREAM, without recording the replay as new output."
  (let ((was-recording (and (output-recording-stream-p stream)
                            (stream-recording-p stream))))
    (when (output-recording-stream-p stream)
      (setf (stream-recording-p stream) nil))
    (unwind-protect
         (replay-output-record record stream region 0 0)
      (when (output-recording-stream-p stream)
        (setf (stream-recording-p stream) was-recording)))))

(defun line-pad (line-style)
  "Return half the thickness of LINE-STYLE, the stroke's padding."
  (/ (line-style-thickness (or line-style (make-line-style))) 2))

(defun within-segment? (x y x1 y1 x2 y2 pad)
  "Return true when (X, Y) is within PAD of the closed segment."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length-squared (+ (* dx dx) (* dy dy)))
         (pad-squared (* pad pad)))
    (if (zerop length-squared)
        (<= (+ (expt (- x x1) 2) (expt (- y y1) 2)) pad-squared)
        (let* ((raw (/ (+ (* (- x x1) dx) (* (- y y1) dy)) length-squared))
               (clamped (max 0 (min 1 raw)))
               (nearest-x (+ x1 (* clamped dx)))
               (nearest-y (+ y1 (* clamped dy))))
          (<= (+ (expt (- x nearest-x) 2) (expt (- y nearest-y) 2))
              pad-squared)))))

(defclass point-displayed-output-record (displayed-output-record)
  ((line-style
    :initarg :line-style
    :reader displayed-line-style))
  (:documentation "A recorded point."))

(defun make-point-output-record (x y &key (ink +foreground-ink+)
                                        (line-style (make-line-style))
                                        (clipping-region +everywhere+))
  "Return a point record at X and Y in its parent's coordinates."
  (let* ((pad (line-pad line-style))
         (record (make-instance 'point-displayed-output-record
                                :ink ink
                                :line-style line-style
                                :clipping-region clipping-region)))
    (set-output-record-position record x y)
    (set-output-record-extent record (- pad) (- pad) pad pad)
    record))

(defmethod output-record-refined-position-test
    ((record point-displayed-output-record) x y)
  (within-segment? x y 0 0 0 0 (line-pad (displayed-line-style record))))

(defclass line-displayed-output-record (displayed-output-record)
  ((x1 :initarg :x1 :reader line-record-x1)
   (y1 :initarg :y1 :reader line-record-y1)
   (x2 :initarg :x2 :reader line-record-x2)
   (y2 :initarg :y2 :reader line-record-y2)
   (line-style :initarg :line-style :reader displayed-line-style))
  (:documentation "A recorded line.  The endpoints are in its own coordinates."))

(defun make-line-output-record (x1 y1 x2 y2 &key (ink +foreground-ink+)
                                              (line-style (make-line-style))
                                              (clipping-region +everywhere+))
  "Return a line record.  The coordinates are in the parent's system."
  (let* ((pad (line-pad line-style))
         (left (min x1 x2))
         (top (min y1 y2))
         (record (make-instance 'line-displayed-output-record
                                :x1 (- x1 left)
                                :y1 (- y1 top)
                                :x2 (- x2 left)
                                :y2 (- y2 top)
                                :ink ink
                                :line-style line-style
                                :clipping-region clipping-region)))
    (set-output-record-position record left top)
    (set-output-record-extent record
                              (- pad) (- pad)
                              (+ (- (max x1 x2) left) pad)
                              (+ (- (max y1 y2) top) pad))
    record))

(defmethod output-record-refined-position-test
    ((record line-displayed-output-record) x y)
  (within-segment? x y
                   (line-record-x1 record) (line-record-y1 record)
                   (line-record-x2 record) (line-record-y2 record)
                   (line-pad (displayed-line-style record))))

(defun on-rectangle-stroke? (x y width height pad)
  "Return true when (X, Y) lies on the stroke of a rectangle."
  (and (<= (- pad) x (+ width pad))
       (<= (- pad) y (+ height pad))
       (or (<= x pad) (>= x (- width pad))
           (<= y pad) (>= y (- height pad)))))

(defclass rectangle-displayed-output-record (displayed-output-record)
  ((width :initarg :width :reader rectangle-record-width)
   (height :initarg :height :reader rectangle-record-height)
   (filled :initarg :filled :reader rectangle-record-filled)
   (line-style :initarg :line-style :reader displayed-line-style))
  (:documentation "A recorded rectangle.  Its origin is its top-left corner."))

(defun make-rectangle-output-record (x1 y1 x2 y2 &key (filled t)
                                                    (ink +foreground-ink+)
                                                    (line-style (make-line-style))
                                                    (clipping-region +everywhere+))
  "Return a rectangle record.  The corners are in the parent's system."
  (let* ((left (min x1 x2))
         (top (min y1 y2))
         (width (- (max x1 x2) left))
         (height (- (max y1 y2) top))
         (pad (if filled 0 (line-pad line-style)))
         (record (make-instance 'rectangle-displayed-output-record
                                :width width
                                :height height
                                :filled filled
                                :ink ink
                                :line-style line-style
                                :clipping-region clipping-region)))
    (set-output-record-position record left top)
    (set-output-record-extent record (- pad) (- pad) (+ width pad) (+ height pad))
    record))

(defmethod output-record-refined-position-test
    ((record rectangle-displayed-output-record) x y)
  (if (rectangle-record-filled record)
      t
      (on-rectangle-stroke? x y
                            (rectangle-record-width record)
                            (rectangle-record-height record)
                            (line-pad (displayed-line-style record)))))

(defclass polygon-displayed-output-record (displayed-output-record)
  ((points :initarg :points :reader polygon-record-points)
   (closed :initarg :closed :reader polygon-record-closed)
   (filled :initarg :filled :reader polygon-record-filled)
   (line-style :initarg :line-style :reader displayed-line-style))
  (:documentation
   "A recorded polygon.  POINTS are (X Y) lists in its own coordinates."))

(defun make-polygon-output-record (coordinates &key (closed t) (filled t)
                                                 (ink +foreground-ink+)
                                                 (line-style (make-line-style))
                                                 (clipping-region +everywhere+))
  "Return a polygon record.  COORDINATES is a flat list of parent coordinates."
  (unless (and coordinates (evenp (length coordinates)))
    (error "A polygon needs an even number of coordinates."))
  (let* ((pairs (loop for (x y) on coordinates by #'cddr
                      collect (list x y)))
         (left (reduce #'min pairs :key #'first))
         (top (reduce #'min pairs :key #'second))
         (right (reduce #'max pairs :key #'first))
         (bottom (reduce #'max pairs :key #'second))
         (local (mapcar (lambda (pair)
                          (list (- (first pair) left) (- (second pair) top)))
                        pairs))
         (pad (if filled 0 (line-pad line-style)))
         (record (make-instance 'polygon-displayed-output-record
                                :points local
                                :closed closed
                                :filled filled
                                :ink ink
                                :line-style line-style
                                :clipping-region clipping-region)))
    (set-output-record-position record left top)
    (set-output-record-extent record
                              (- pad) (- pad)
                              (+ (- right left) pad)
                              (+ (- bottom top) pad))
    record))

(defmethod output-record-refined-position-test
    ((record polygon-displayed-output-record) x y)
  (let ((points (polygon-record-points record))
        (pad (line-pad (displayed-line-style record))))
    (if (polygon-record-filled record)
        (region-contains-position-p
         (make-polygon (mapcar (lambda (pair) (apply #'make-point pair)) points))
         x y)
        (loop for (start next) on points
              for end = (or next
                            (and (polygon-record-closed record) (first points)))
              thereis (and end
                           (within-segment? x y
                                            (first start) (second start)
                                            (first end) (second end)
                                            pad))))))

(defun ellipse-radius-box (dx dy)
  "Return how far an ellipse radius reaches along one axis."
  (sqrt (+ (* dx dx) (* dy dy))))

(defclass ellipse-displayed-output-record (displayed-output-record)
  ((center-x :initarg :center-x :reader ellipse-record-center-x)
   (center-y :initarg :center-y :reader ellipse-record-center-y)
   (radius-1-dx :initarg :radius-1-dx :reader ellipse-record-radius-1-dx)
   (radius-1-dy :initarg :radius-1-dy :reader ellipse-record-radius-1-dy)
   (radius-2-dx :initarg :radius-2-dx :reader ellipse-record-radius-2-dx)
   (radius-2-dy :initarg :radius-2-dy :reader ellipse-record-radius-2-dy)
   (start-angle :initarg :start-angle :reader ellipse-record-start-angle)
   (end-angle :initarg :end-angle :reader ellipse-record-end-angle)
   (filled :initarg :filled :reader ellipse-record-filled)
   (line-style :initarg :line-style :reader displayed-line-style))
  (:documentation
   "A recorded ellipse.  The center is in its own coordinates.  A
partial arc is stored, and the whole ellipse is sensitive."))

(defun make-ellipse-output-record (center-x center-y
                                   radius-1-dx radius-1-dy
                                   radius-2-dx radius-2-dy
                                   &key (filled t)
                                     start-angle end-angle
                                     (ink +foreground-ink+)
                                     (line-style (make-line-style))
                                     (clipping-region +everywhere+))
  "Return an ellipse record.  The center is in the parent's coordinates."
  (let* ((half-width (ellipse-radius-box radius-1-dx radius-2-dx))
         (half-height (ellipse-radius-box radius-1-dy radius-2-dy))
         (pad (if filled 0 (line-pad line-style)))
         (record (make-instance 'ellipse-displayed-output-record
                                :center-x half-width
                                :center-y half-height
                                :radius-1-dx radius-1-dx
                                :radius-1-dy radius-1-dy
                                :radius-2-dx radius-2-dx
                                :radius-2-dy radius-2-dy
                                :start-angle start-angle
                                :end-angle end-angle
                                :filled filled
                                :ink ink
                                :line-style line-style
                                :clipping-region clipping-region)))
    (set-output-record-position record
                                (- center-x half-width)
                                (- center-y half-height))
    (set-output-record-extent record
                              (- pad) (- pad)
                              (+ (* 2 half-width) pad)
                              (+ (* 2 half-height) pad))
    record))

(defun ellipse-parameters (record x y)
  "Return the conjugate coordinates of (X, Y), or NIL when the radii collapse."
  (let* ((dx (- x (ellipse-record-center-x record)))
         (dy (- y (ellipse-record-center-y record)))
         (r1x (ellipse-record-radius-1-dx record))
         (r1y (ellipse-record-radius-1-dy record))
         (r2x (ellipse-record-radius-2-dx record))
         (r2y (ellipse-record-radius-2-dy record))
         (determinant (- (* r1x r2y) (* r2x r1y))))
    (unless (zerop determinant)
      (values (/ (- (* dx r2y) (* r2x dy)) determinant)
              (/ (- (* r1x dy) (* dx r1y)) determinant)))))

(defun ellipse-contains-position? (record x y)
  "Return true when (X, Y), in RECORD's coordinates, is inside the ellipse."
  (multiple-value-bind (a b) (ellipse-parameters record x y)
    (and a (<= (+ (* a a) (* b b)) 1))))

(defun ellipse-stroke-contains? (record x y)
  "Return true when (X, Y) lies within the stroke of RECORD."
  (multiple-value-bind (a b) (ellipse-parameters record x y)
    (when a
      (let* ((dx (- x (ellipse-record-center-x record)))
             (dy (- y (ellipse-record-center-y record)))
             (distance (sqrt (+ (* dx dx) (* dy dy))))
             (scale (sqrt (+ (* a a) (* b b)))))
        (and (plusp scale)
             (<= (abs (* distance (- 1 (/ 1 scale))))
                 (line-pad (displayed-line-style record))))))))

(defmethod output-record-refined-position-test
    ((record ellipse-displayed-output-record) x y)
  (if (ellipse-record-filled record)
      (ellipse-contains-position? record x y)
      (ellipse-stroke-contains? record x y)))

(defun text-style-point-size (text-style)
  "Return the point size of TEXT-STYLE.
A keyword size uses a fixed stand-in: tiny 6, very small 8, small 10,
normal 12, large 14, very large 18, and huge 24.  An unspecified size
is 12.  :SMALLER is 10 and :LARGER is 14."
  (let ((size (if text-style
                  (text-style-size (parse-text-style text-style))
                  nil)))
    (cond
      ((and (realp size) (plusp size)) size)
      ((eq size :tiny) 6)
      ((eq size :very-small) 8)
      ((eq size :small) 10)
      ((eq size :large) 14)
      ((eq size :very-large) 18)
      ((eq size :huge) 24)
      ((eq size :smaller) 10)
      ((eq size :larger) 14)
      (t 12))))

(defun text-metrics (text-style string)
  "Return the width, ascent, and descent of STRING in TEXT-STYLE.
Each character is three fifths of the point size wide, including a
space.  Ascent is four fifths of the size.  Descent is one fifth.
These are the recording metrics.  A frame manager may replace them
with a font's metrics."
  (let ((size (text-style-point-size text-style)))
    (values (* (length string) (* 3/5 size))
            (* 4/5 size)
            (* 1/5 size))))

(defun text-line-height (text-style)
  "Return the distance from one baseline to the next for TEXT-STYLE."
  (text-style-point-size text-style))

(defclass text-displayed-output-record (displayed-output-record)
  ((string :initarg :string :reader displayed-text)
   (text-style :initarg :text-style :reader displayed-text-style)
   (align-x :initarg :align-x :reader displayed-text-align-x)
   (align-y :initarg :align-y :reader displayed-text-align-y))
  (:documentation
   "A recorded run of text.  Its origin is the point the text was
drawn at, before alignment."))

(defun make-text-output-record (string x y &key (text-style *default-text-style*)
                                             (align-x :left) (align-y :baseline)
                                             (ink +foreground-ink+)
                                             (clipping-region +everywhere+))
  "Return a text record.  X and Y are the alignment point in the parent's coordinates."
  (multiple-value-bind (width ascent descent) (text-metrics text-style string)
    (let* ((height (+ ascent descent))
           (left (ecase align-x
                   (:left 0)
                   (:center (- (/ width 2)))
                   (:right (- width))))
           (top (ecase align-y
                  (:baseline (- ascent))
                  (:top 0)
                  (:center (- (/ height 2)))
                  (:bottom (- height))))
           (record (make-instance 'text-displayed-output-record
                                  :string string
                                  :text-style (parse-text-style text-style)
                                  :align-x align-x
                                  :align-y align-y
                                  :ink ink
                                  :clipping-region clipping-region)))
      (set-output-record-position record x y)
      (set-output-record-extent record left top (+ left width) (+ top height))
      record)))

(defclass output-recording-stream (sb-gray:fundamental-character-output-stream)
  ((history
    :accessor stream-output-history
    :documentation "The root output record.")
   (current
    :accessor stream-current-output-record
    :documentation "The record that receives new output.")
   (recording-p
    :initform t
    :accessor stream-recording-p
    :documentation "True when new output is saved.")
   (drawing-p
    :initform t
    :accessor stream-drawing-p
    :documentation "True when new output is drawn.")
   (sensitive-inferiors
    :initform t
    :accessor stream-allows-sensitive-inferiors
    :documentation "True when a nested presentation is sensitive.")
   (cursor-x
    :initform 0
    :accessor stream-cursor-x)
   (cursor-y
    :initform 0
    :accessor stream-cursor-y)
   (left-margin
    :initform 0
    :accessor stream-left-margin
    :documentation "The x terpri returns to.  Indenting output raises it.")
   (ink
    :initform +foreground-ink+
    :accessor stream-ink)
   (line-style
    :initform (make-line-style)
    :accessor stream-line-style)
   (text-style
    :initform *default-text-style*
    :accessor stream-text-style)
   (transformation
    :initform +identity-transformation+
    :accessor stream-transformation)
   (clipping-region
    :initform +everywhere+
    :accessor stream-clipping-region
    :documentation "The clip, in the root record's coordinates.")
   (medium
    :initform nil
    :initarg :medium
    :accessor stream-medium
    :documentation "The medium that draws this stream, or NIL."))
  (:documentation
   "A character stream that saves what it writes as output records.
The cursor is in the root record's coordinates."))

(defun output-recording-stream-p (object)
  "Return true if OBJECT records its output."
  (typep object 'output-recording-stream))

(defmethod initialize-instance :after ((stream output-recording-stream) &key)
  (let ((history (make-instance 'standard-tree-output-history)))
    (setf (stream-output-history stream) history)
    (setf (stream-current-output-record stream) history)))

(defun stream-cursor-position (stream)
  "Return the cursor's X and Y in the root record's coordinates."
  (values (stream-cursor-x stream) (stream-cursor-y stream)))

(defun set-stream-cursor-position (stream x y)
  "Move STREAM's cursor to X and Y in the root record's coordinates."
  (setf (stream-cursor-x stream) x
        (stream-cursor-y stream) y)
  (values x y))

(define-setf-expander stream-cursor-position (stream &environment env)
  (multiple-value-bind (vars vals stores writer reader)
      (get-setf-expansion stream env)
    (declare (ignore stores writer))
    (let ((x (gensym "X"))
          (y (gensym "Y")))
      (values vars
              vals
              (list x y)
              `(progn
                 (set-stream-cursor-position ,reader ,x ,y)
                 (values ,x ,y))
              `(stream-cursor-position ,reader)))))

(defun sheet-position-in-current-record (stream x y)
  "Return X and Y translated into the current record's coordinates."
  (multiple-value-bind (origin-x origin-y)
      (output-record-sheet-origin (stream-current-output-record stream))
    (values (- x origin-x) (- y origin-y))))

(defun record-text-line (stream text)
  "Record TEXT at STREAM's cursor and advance the cursor."
  (when (plusp (length text))
    (when (stream-recording-p stream)
      (multiple-value-bind (x y)
          (sheet-position-in-current-record stream
                                            (stream-cursor-x stream)
                                            (stream-cursor-y stream))
        (stream-add-output-record
         stream
         (make-text-output-record text x y
                                  :text-style (stream-text-style stream)
                                  :ink (stream-ink stream))))))
  (incf (stream-cursor-x stream)
        (nth-value 0 (text-metrics (stream-text-style stream) text))))

(defun stream-add-output-record (stream record)
  "Add RECORD to STREAM's current output record."
  (add-output-record record (stream-current-output-record stream)))

(defmethod sb-gray:stream-terpri ((stream output-recording-stream))
  (setf (stream-cursor-x stream) (stream-left-margin stream))
  (incf (stream-cursor-y stream) (text-line-height (stream-text-style stream)))
  nil)

(defmethod sb-gray:stream-write-char ((stream output-recording-stream) character)
  (if (char= character #\Newline)
      (sb-gray:stream-terpri stream)
      (record-text-line stream (string character)))
  character)

(defmethod sb-gray:stream-write-string
    ((stream output-recording-stream) string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (text (subseq string start end)))
    (loop for piece-start = 0 then (1+ newline)
          for newline = (position #\Newline text :start piece-start)
          do (record-text-line stream
                               (subseq text piece-start (or newline (length text))))
          while newline
          do (sb-gray:stream-terpri stream)))
  string)

(defmethod sb-gray:stream-line-column ((stream output-recording-stream))
  (stream-cursor-x stream))

(defmethod sb-gray:stream-finish-output ((stream output-recording-stream))
  nil)

(defmethod sb-gray:stream-force-output ((stream output-recording-stream))
  nil)

(defmacro with-new-output-record
    ((stream &optional (record-type ''standard-sequence-output-record) record
              &rest initargs)
     &body body)
  "Open a new output record on STREAM, run BODY, and add the record.
RECORD-TYPE defaults to STANDARD-SEQUENCE-OUTPUT-RECORD.  RECORD is a
variable name, bound to the new record.  INITARGS are passed to
MAKE-INSTANCE."
  (let ((stream-var (gensym "STREAM"))
        (record-var (or record (gensym "RECORD")))
        (parent-var (gensym "PARENT")))
    `(let* ((,stream-var ,stream)
            (,record-var (make-instance ,record-type ,@initargs))
            (,parent-var (stream-current-output-record ,stream-var)))
       (setf (output-record-parent ,record-var) ,parent-var)
       (set-output-record-start-cursor-position
        ,record-var
        (stream-cursor-x ,stream-var)
        (stream-cursor-y ,stream-var))
       (setf (stream-current-output-record ,stream-var) ,record-var)
       (unwind-protect
            (progn ,@body)
         (set-output-record-end-cursor-position
          ,record-var
          (stream-cursor-x ,stream-var)
          (stream-cursor-y ,stream-var))
         (setf (stream-current-output-record ,stream-var) ,parent-var)
         (stream-add-output-record ,stream-var ,record-var))
       ,record-var)))

(defmacro with-output-recording-options
    ((stream &key (record nil recordp) (draw nil drawp)) &body body)
  "Bind whether STREAM records and draws, then run BODY.
An omitted keyword leaves that flag as it is."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (let ,(append
              (when recordp
                `((old-record (stream-recording-p ,stream-var))))
              (when drawp
                `((old-draw (stream-drawing-p ,stream-var)))))
         ,@(when recordp
             `((setf (stream-recording-p ,stream-var) ,record)))
         ,@(when drawp
             `((setf (stream-drawing-p ,stream-var) ,draw)))
         (unwind-protect
              (progn ,@body)
           ,@(when recordp
               `((setf (stream-recording-p ,stream-var) old-record)))
           ,@(when drawp
               `((setf (stream-drawing-p ,stream-var) old-draw))))))))

(defclass noted-replay-record (displayed-output-record)
  ()
  (:documentation "A displayed record whose replay is noted for a test."))

(defvar *replay-notes* nil
  "The records replayed by the output-record tests.")

(defmethod replay-displayed-output-record
    ((record noted-replay-record) stream x-offset y-offset)
  (declare (ignore stream x-offset y-offset))
  (push record *replay-notes*))

(behavior 'output-record-position
  (let ((record (make-instance 'standard-sequence-output-record)))
    (should-be-true (output-record-p record))
    (setf (output-record-position record) (values 3 4))
    (should= 3 (nth-value 0 (output-record-position record)))
    (should= 4 (nth-value 1 (output-record-position record)))
    (should= 3 (bounding-rectangle-min-x record))
    (should= 4 (bounding-rectangle-min-y record))))

(behavior 'add-output-record
  (let ((parent (make-instance 'standard-tree-output-history))
        (line (make-line-output-record 0 0 10 0))
        (later (make-point-output-record 5 0)))
    (add-output-record line parent)
    (add-output-record later parent)
    (should= 2 (output-record-count parent))
    (should-eq parent (output-record-parent line))
    (should= -1/2 (bounding-rectangle-min-x parent))
    (should= 21/2 (bounding-rectangle-max-x parent))
    (should-be-true (output-record-contains-position-p line 5 0))
    (should-be-false (output-record-contains-position-p line 5 3))
    (should-be-true (output-record-contains-position-p later 5 0))
    (should-be-false (output-record-contains-position-p later 6 0))
    (let ((found nil))
      (map-over-output-records-containing-position
       (lambda (child) (setf found (append found (list child))))
       parent 5 0)
      (should-eq later (first found))
      (should-eq line (second found)))
    (delete-output-record later parent)
    (should= 1 (output-record-count parent))
    (should-be-null (output-record-parent later))
    (should-be-false (delete-output-record later parent nil))
    (clear-output-record parent)
    (should= 0 (output-record-count parent))
    (highlight-output-record line nil :highlight)
    (should-be-true (output-record-highlighted line))
    (highlight-output-record line nil :unhighlight)
    (should-be-false (output-record-highlighted line))))

(behavior 'make-rectangle-output-record
  (let ((filled (make-rectangle-output-record 0 0 10 8))
        (open (make-rectangle-output-record 0 0 10 8 :filled nil))
        (polygon (make-polygon-output-record '(0 0 10 0 0 10)))
        (ellipse (make-ellipse-output-record 0 0 5 0 0 5)))
    (should-be-true (output-record-contains-position-p filled 5 4))
    (should-be-false (output-record-contains-position-p filled 11 4))
    (should-be-true (output-record-contains-position-p open 0 4))
    (should-be-false (output-record-contains-position-p open 5 4))
    (should-be-true (output-record-contains-position-p polygon 1 1))
    (should-be-false (output-record-contains-position-p polygon 9 9))
    (should-be-true (output-record-contains-position-p ellipse 0 0))
    (should-be-false (output-record-contains-position-p ellipse 6 0))
    (should= 5 (bounding-rectangle-max-x ellipse))))

(behavior 'text-metrics
  (should= 12 (text-style-point-size *default-text-style*))
  (should= 72/5 (nth-value 0 (text-metrics *default-text-style* "Hi")))
  (let ((text (make-text-output-record "Hi" 10 20)))
    (should-equal "Hi" (displayed-text text))
    (should= 10 (output-record-x text))
    (should= 20 (output-record-y text))
    (should-be-true (output-record-contains-position-p text 11 20))
    (should-be-false (output-record-contains-position-p text 11 0))))

(behavior 'output-recording-stream
  (let ((stream (make-instance 'output-recording-stream)))
    (write-string "Hi" stream)
    (should= 72/5 (stream-cursor-x stream))
    (should= 0 (stream-cursor-y stream))
    (let ((records (output-record-children (stream-output-history stream))))
      (should= 1 (length records))
      (should-equal "Hi" (displayed-text (first records))))
    (terpri stream)
    (should= 0 (stream-cursor-x stream))
    (should= 12 (stream-cursor-y stream))
    (write-string (format nil "A~%B") stream)
    (should-equal "B" (displayed-text
                       (first (last (output-record-children
                                     (stream-output-history stream))))))
    (setf (stream-cursor-position stream) (values 2 3))
    (should= 2 (stream-cursor-x stream))
    (should= 3 (stream-cursor-y stream))
    (with-output-recording-options (stream :record nil)
      (write-string "Z" stream))
    (should-be-false (find "Z" (output-record-children
                                (stream-output-history stream))
                           :key #'displayed-text
                           :test #'equal))
    (let ((group (with-new-output-record (stream)
                   (write-string "Q" stream))))
      (should-eq group (first (last (output-record-children
                                     (stream-output-history stream)))))
      (should-equal "Q" (displayed-text
                         (first (output-record-children group))))
      (should= 1 (output-record-count group)))))

(behavior 'replay
  (let ((stream (make-instance 'output-recording-stream))
        (inside (make-instance 'noted-replay-record))
        (outside (make-instance 'noted-replay-record)))
    (set-output-record-extent inside 0 0 1 1)
    (set-output-record-position outside 20 20)
    (set-output-record-extent outside 0 0 1 1)
    (add-output-record inside (stream-output-history stream))
    (add-output-record outside (stream-output-history stream))
    (setf *replay-notes* nil)
    (replay (stream-output-history stream) stream (make-rectangle* 0 0 5 5))
    (should-eq inside (first *replay-notes*))
    (should= 1 (length *replay-notes*))
    (should-be-true (stream-recording-p stream))))
