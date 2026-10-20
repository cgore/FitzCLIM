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

(defpackage :FitzCLIM/formatted-output
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/output-record
        :FitzCLIM/medium
        :FitzCLIM/medium/drawing)
  (:export :cell-output-record
           :filling-output
           :formatting-cell
           :formatting-column
           :formatting-item-list
           :formatting-row
           :formatting-table
           :indenting-output
           :row-output-record
           :surrounding-output-with-border
           :table-output-record))
(in-package :FitzCLIM/formatted-output)

(defclass table-output-record (standard-sequence-output-record)
  ()
  (:documentation "A formatted table.  Its origin is the first baseline."))

(defclass row-output-record (standard-sequence-output-record)
  ()
  (:documentation "One row of a table.  Its origin is the row baseline."))

(defclass cell-output-record (standard-sequence-output-record)
  ((align-x
    :initarg :align-x
    :reader cell-align-x)
   (align-y
    :initarg :align-y
    :reader cell-align-y)
   (min-width
    :initarg :min-width
    :reader cell-min-width)
   (min-height
    :initarg :min-height
    :reader cell-min-height))
  (:documentation
   "One cell.  Its children are in the cell's own coordinates, and the
cell's origin is the baseline of that output before alignment."))

(defvar *in-table* nil
  "True while a formatting-table body is running.")

(defvar *table-rows* nil
  "Rows collected by the current formatting-table, last row first.")

(defvar *table-columns* nil
  "Columns collected by the current formatting-table, last column first.
Each column is a list of cells from top to bottom.")

(defvar *cell-collectors* nil
  "A stack of cell lists.  The car is the innermost row or item list.")

(defun collect-cell (cell)
  "Add CELL to the innermost row or item list."
  (unless *cell-collectors*
    (error "formatting-cell is outside a row or an item list."))
  (push cell (car *cell-collectors*))
  cell)

(defun cell-baseline-span (cell)
  "Return the ascent above CELL's origin and the descent below it.
MIN-HEIGHT extends the descent when the content is shorter."
  (multiple-value-bind (left top right bottom) (bounding-rectangle* cell)
    (declare (ignore left right))
    (let* ((ascent (max 0 (- top)))
           (descent (max 0 bottom))
           (extra (max 0 (- (cell-min-height cell) ascent descent))))
      (values ascent (+ descent extra)))))

(defun cell-content-width (cell)
  "Return the width CELL asks its column to be."
  (max (cell-min-width cell) (bounding-rectangle-width cell)))

(defun place-cell (cell column-x column-width row-ascent row-descent)
  "Move CELL into its column.  The row origin is the baseline."
  (multiple-value-bind (left top right bottom) (bounding-rectangle* cell)
    (let* ((width (- right left))
           (x (+ column-x
                 (ecase (cell-align-x cell)
                   (:left (- left))
                   (:right (- column-width right))
                   (:center (- (/ (- column-width width) 2) left)))))
           (y (ecase (cell-align-y cell)
                (:baseline 0)
                (:top (- (- row-ascent) top))
                (:bottom (- row-descent bottom))
                (:center
                 (let ((row-height (+ row-ascent row-descent))
                       (height (- bottom top)))
                   (- (+ (- row-ascent) (/ (- row-height height) 2)) top))))))
      (set-output-record-position cell x y))))

(defun column-widths (rows equalize)
  "Return a vector of column widths for ROWS."
  (let* ((count (reduce #'max rows :key #'length :initial-value 0))
         (widths (make-array count :initial-element 0)))
    (dolist (row rows)
      (loop for cell in row
            for column from 0
            do (setf (aref widths column)
                     (max (aref widths column) (cell-content-width cell)))))
    (when (and equalize (plusp count))
      (let ((widest (reduce #'max widths)))
        (loop for column from 0 below count
              do (setf (aref widths column) widest))))
    widths))

(defun layout-rows (stream rows &key (x-spacing 8) (y-spacing 2)
                                  (move-cursor t) equalize-column-widths)
  "Place ROWS on STREAM and return the table record.
The table origin is the cursor at entry, which is the first baseline.
Each later baseline is one row height plus Y-SPACING below the previous.
When MOVE-CURSOR is true, the cursor's x returns to where it started
and its y becomes the baseline just under the table."
  (let ((start-x (stream-cursor-x stream))
        (start-y (stream-cursor-y stream))
        (table (make-instance 'table-output-record))
        (widths (column-widths rows equalize-column-widths))
        (baseline 0))
    (dolist (row rows)
      (let ((row-ascent 0)
            (row-descent 0)
            (row-record (make-instance 'row-output-record)))
        (dolist (cell row)
          (multiple-value-bind (ascent descent) (cell-baseline-span cell)
            (setf row-ascent (max row-ascent ascent)
                  row-descent (max row-descent descent))))
        (set-output-record-position row-record 0 baseline)
        (loop for cell in row
              for column from 0
              for x = 0 then (+ x (aref widths (1- column)) x-spacing)
              do (place-cell cell x (aref widths column) row-ascent row-descent)
                 (add-output-record cell row-record))
        (add-output-record row-record table)
        (incf baseline (+ row-ascent row-descent
                          (if (eq row (first (last rows))) 0 y-spacing)))))
    (multiple-value-bind (origin-x origin-y)
        (output-record-sheet-origin (stream-current-output-record stream))
      (set-output-record-position table
                                  (- start-x origin-x)
                                  (- start-y origin-y)))
    (stream-add-output-record stream table)
    (when (and move-cursor rows)
      (setf (stream-cursor-position stream)
            (values start-x (+ start-y baseline))))
    table))

(defun empty-cell ()
  "Return a cell with no output, used to keep a short column aligned."
  (make-instance 'cell-output-record
                 :align-x :left
                 :align-y :baseline
                 :min-width 0
                 :min-height 0))

(defun columns-to-rows (columns)
  "Turn COLUMNS into rows.  COLUMNS run from left to right.
A short column is padded so a later cell stays in its own column."
  (let ((height (reduce #'max columns :key #'length :initial-value 0)))
    (loop for row-index from 0 below height
          collect (loop for column in columns
                        collect (or (nth row-index column)
                                    (empty-cell))))))

(defun invoke-formatting-table (stream continuation
                                &key (x-spacing 8) (y-spacing 2)
                                  (move-cursor t) equalize-column-widths)
  "Run CONTINUATION, which calls formatting-row or formatting-column.
A table uses one of those, and the rows are then laid out."
  (let ((*in-table* t)
        (*table-rows* nil)
        (*table-columns* nil))
    (funcall continuation)
    (when (and *table-rows* *table-columns*)
      (error "A table cannot mix formatting-row and formatting-column."))
    (layout-rows stream
                 (if *table-columns*
                     (columns-to-rows (nreverse *table-columns*))
                     (nreverse *table-rows*))
                 :x-spacing x-spacing
                 :y-spacing y-spacing
                 :move-cursor move-cursor
                 :equalize-column-widths equalize-column-widths)))

(defmacro formatting-table
    ((stream &key (x-spacing 8) (y-spacing 2) (move-cursor t)
              equalize-column-widths)
     &body body)
  "Lay out rows from BODY as a table on STREAM.
X-SPACING is the gap between columns.  Y-SPACING is the gap between
baselines beyond the row's own height.  The table's origin is the
cursor, and that origin is the first row's baseline."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-formatting-table
        ,stream-var
        (lambda () ,@body)
        :x-spacing ,x-spacing
        :y-spacing ,y-spacing
        :move-cursor ,move-cursor
        :equalize-column-widths ,equalize-column-widths))))

(defun invoke-formatting-row (continuation)
  "Collect the cells CONTINUATION writes into the current table."
  (unless *in-table*
    (error "formatting-row is outside formatting-table."))
  (let ((*cell-collectors* (cons nil *cell-collectors*)))
    (funcall continuation)
    (push (nreverse (car *cell-collectors*)) *table-rows*))
  nil)

(defmacro formatting-row ((stream) &body body)
  "Collect cells from BODY as the next row of the current table.
STREAM is accepted so the form matches the surrounding macros.  The
row is recorded on that table's stream."
  (declare (ignore stream))
  `(invoke-formatting-row (lambda () ,@body)))

(defun invoke-formatting-column (continuation)
  "Collect the cells CONTINUATION writes as the next column.
Cells run from top to bottom.  This must run inside a table."
  (unless *in-table*
    (error "formatting-column is outside formatting-table."))
  (let ((*cell-collectors* (cons nil *cell-collectors*)))
    (funcall continuation)
    (push (nreverse (car *cell-collectors*)) *table-columns*))
  nil)

(defmacro formatting-column
    ((stream &key (x-spacing 8) (y-spacing 2) (move-cursor t))
     &body body)
  "Collect cells from BODY as one column.
Inside formatting-table, columns sit side by side and the table's
spacing is used.  Outside a table, BODY is a table of one column.
X-SPACING, Y-SPACING, and MOVE-CURSOR apply only in that case."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (if *in-table*
           (invoke-formatting-column (lambda () ,@body))
           (invoke-formatting-table
            ,stream-var
            (lambda ()
              (invoke-formatting-column (lambda () ,@body)))
            :x-spacing ,x-spacing
            :y-spacing ,y-spacing
            :move-cursor ,move-cursor)))))

(defun invoke-formatting-cell (stream continuation
                               &key (align-x :left) (align-y :baseline)
                                 (min-width 0) (min-height 0))
  "Record CONTINUATION as one cell and return that cell.
Output inside the cell is recorded relative to the cell.  The stream
cursor is restored when the cell ends."
  (let ((cell (make-instance 'cell-output-record
                             :align-x align-x
                             :align-y align-y
                             :min-width min-width
                             :min-height min-height))
        (saved-x (stream-cursor-x stream))
        (saved-y (stream-cursor-y stream))
        (parent (stream-current-output-record stream)))
    (setf (output-record-parent cell) parent)
    (setf (stream-current-output-record stream) cell)
    (setf (stream-cursor-position stream) (output-record-sheet-origin cell))
    (unwind-protect
         (funcall continuation)
      (setf (stream-current-output-record stream) parent)
      (setf (output-record-parent cell) nil)
      (setf (stream-cursor-position stream) (values saved-x saved-y)))
    (collect-cell cell)))

(defmacro formatting-cell
    ((stream &key (align-x :left) (align-y :baseline)
              (min-width 0) (min-height 0))
     &body body)
  "Record BODY as one cell of the current row or item list.
ALIGN-X is :LEFT, :CENTER, or :RIGHT.  ALIGN-Y is :BASELINE, :TOP,
:BOTTOM, or :CENTER.  MIN-WIDTH and MIN-HEIGHT enlarge the cell."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-formatting-cell
        ,stream-var
        (lambda () ,@body)
        :align-x ,align-x
        :align-y ,align-y
        :min-width ,min-width
        :min-height ,min-height))))

(defun find-recorded-text (record string)
  "Return the text record under RECORD whose string is STRING."
  (labels ((walk (node)
             (cond
               ((and (typep node 'text-displayed-output-record)
                     (equal (displayed-text node) string))
                node)
               (t
                (some #'walk (output-record-children node))))))
    (walk record)))

(behavior 'formatting-table
  (let ((stream (make-instance 'output-recording-stream)))
    (setf (stream-cursor-position stream) (values 3 5))
    (formatting-table (stream :x-spacing 4 :y-spacing 2)
      (formatting-row (stream)
        (formatting-cell (stream) (write-string "A" stream))
        (formatting-cell (stream) (write-string "BBB" stream)))
      (formatting-row (stream)
        (formatting-cell (stream) (write-string "CC" stream))
        (formatting-cell (stream) (write-string "D" stream))))
    (let ((wide (find-recorded-text (stream-output-history stream) "BBB"))
          (below (find-recorded-text (stream-output-history stream) "D")))
      (should= (+ 3 72/5 4) (nth-value 0 (output-record-sheet-origin wide)))
      (should= 5 (nth-value 1 (output-record-sheet-origin wide)))
      (should= (+ 3 72/5 4) (nth-value 0 (output-record-sheet-origin below)))
      (should= (+ 5 14) (nth-value 1 (output-record-sheet-origin below))))
    (should= 3 (stream-cursor-x stream))
    (should= (+ 5 26) (stream-cursor-y stream))
    (setf (stream-cursor-position stream) (values 0 0))
    (formatting-table (stream :x-spacing 0 :y-spacing 0)
      (formatting-row (stream)
        (formatting-cell (stream) (write-string "BBB" stream))
        (formatting-cell (stream :align-x :right :min-width 20)
          (write-string "Z" stream))))
    (let ((right (find-recorded-text (stream-output-history stream) "Z")))
      (should= (+ 108/5 (- 20 36/5))
               (nth-value 0 (output-record-sheet-origin right))))))

(behavior 'formatting-column
  (let ((stream (make-instance 'output-recording-stream)))
    (formatting-table (stream :x-spacing 4 :y-spacing 2)
      (formatting-column (stream)
        (formatting-cell (stream) (write-string "A" stream))
        (formatting-cell (stream) (write-string "BB" stream)))
      (formatting-column (stream)
        (formatting-cell (stream) (write-string "C" stream))
        (formatting-cell (stream) (write-string "D" stream))))
    (let ((beside (find-recorded-text (stream-output-history stream) "C"))
          (below (find-recorded-text (stream-output-history stream) "BB"))
          (lower (find-recorded-text (stream-output-history stream) "D")))
      (should= (+ 72/5 4) (nth-value 0 (output-record-sheet-origin beside)))
      (should= 0 (nth-value 1 (output-record-sheet-origin beside)))
      (should= 0 (nth-value 0 (output-record-sheet-origin below)))
      (should= 14 (nth-value 1 (output-record-sheet-origin below)))
      (should= (+ 72/5 4) (nth-value 0 (output-record-sheet-origin lower)))
      (should= 14 (nth-value 1 (output-record-sheet-origin lower))))
    (should= 0 (stream-cursor-x stream))
    (should= 26 (stream-cursor-y stream)))
  (let ((stream (make-instance 'output-recording-stream)))
    (formatting-table (stream :x-spacing 4 :y-spacing 0)
      (formatting-column (stream)
        (formatting-cell (stream) (write-string "A" stream)))
      (formatting-column (stream)
        (formatting-cell (stream) (write-string "B" stream))
        (formatting-cell (stream) (write-string "C" stream))))
    (let ((lower (find-recorded-text (stream-output-history stream) "C")))
      (should= (+ 36/5 4) (nth-value 0 (output-record-sheet-origin lower)))
      (should= 12 (nth-value 1 (output-record-sheet-origin lower)))))
  (let ((stream (make-instance 'output-recording-stream)))
    (formatting-column (stream :y-spacing 2)
      (formatting-cell (stream) (write-string "A" stream))
      (formatting-cell (stream) (write-string "B" stream)))
    (let ((below (find-recorded-text (stream-output-history stream) "B")))
      (should= 0 (nth-value 0 (output-record-sheet-origin below)))
      (should= 14 (nth-value 1 (output-record-sheet-origin below)))))
  (let ((stream (make-instance 'output-recording-stream)))
    (should-be-true
     (handler-case
         (progn
           (formatting-table (stream)
             (formatting-row (stream)
               (formatting-cell (stream) (write-string "A" stream)))
             (formatting-column (stream)
               (formatting-cell (stream) (write-string "B" stream))))
           nil)
       (error () t)))))

(defun group-items (cells n-columns row-wise)
  "Split CELLS into rows of N-COLUMNS.
ROW-WISE fills across.  Otherwise the items fill down each column."
  (cond
    ((null cells) nil)
    (row-wise
     (loop for rest = cells then (nthcdr n-columns rest)
           while rest
           collect (subseq rest 0 (min n-columns (length rest)))))
    (t
     (let* ((rows (ceiling (length cells) n-columns))
            (grid (make-array (list rows n-columns) :initial-element nil)))
       (loop for cell in cells
             for index from 0
             do (setf (aref grid (mod index rows) (floor index rows)) cell))
       (loop for row from 0 below rows
             collect (loop for column from 0 below n-columns
                           for cell = (aref grid row column)
                           when cell collect cell))))))

(defun invoke-formatting-item-list (stream continuation
                                    &key (n-columns 1) (x-spacing 8)
                                      (y-spacing 2) (row-wise t)
                                      (move-cursor t))
  "Collect cells from CONTINUATION and lay them out in columns."
  (unless (and (integerp n-columns) (plusp n-columns))
    (error "n-columns must be a positive integer."))
  (let ((*cell-collectors* (cons nil *cell-collectors*)))
    (funcall continuation)
    (layout-rows stream
                 (group-items (nreverse (car *cell-collectors*))
                              n-columns row-wise)
                 :x-spacing x-spacing
                 :y-spacing y-spacing
                 :move-cursor move-cursor)))

(defmacro formatting-item-list
    ((stream &key (n-columns 1) (x-spacing 8) (y-spacing 2)
              (row-wise t) (move-cursor t))
     &body body)
  "Lay out the cells in BODY as an item list of N-COLUMNS.
ROW-WISE fills across and then down.  Otherwise each column fills
before the next column starts."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-formatting-item-list
        ,stream-var
        (lambda () ,@body)
        :n-columns ,n-columns
        :x-spacing ,x-spacing
        :y-spacing ,y-spacing
        :row-wise ,row-wise
        :move-cursor ,move-cursor))))

(behavior 'formatting-item-list
  (let ((stream (make-instance 'output-recording-stream)))
    (formatting-item-list (stream :n-columns 2 :x-spacing 4 :y-spacing 2
                                  :row-wise nil)
      (formatting-cell (stream) (write-string "A" stream))
      (formatting-cell (stream) (write-string "B" stream))
      (formatting-cell (stream) (write-string "C" stream)))
    (let ((down (find-recorded-text (stream-output-history stream) "B"))
          (beside (find-recorded-text (stream-output-history stream) "C")))
      (should= 0 (nth-value 0 (output-record-sheet-origin down)))
      (should= 14 (nth-value 1 (output-record-sheet-origin down)))
      (should= (+ 36/5 4) (nth-value 0 (output-record-sheet-origin beside)))
      (should= 0 (nth-value 1 (output-record-sheet-origin beside))))))

(defun indent-amount (stream indentation)
  "Return INDENTATION as a distance.  A string uses its text width."
  (etypecase indentation
    (real indentation)
    (string (nth-value 0 (text-metrics (stream-text-style stream) indentation)))))

(defun invoke-indenting-output (stream indentation continuation
                                &key (move-cursor t))
  "Indent CONTINUATION by INDENTATION, including text after a newline.
The left margin is restored afterwards.  When MOVE-CURSOR is false,
the cursor returns to where it started."
  (let ((amount (indent-amount stream indentation))
        (saved-x (stream-cursor-x stream))
        (saved-y (stream-cursor-y stream)))
    (incf (stream-left-margin stream) amount)
    (incf (stream-cursor-x stream) amount)
    (unwind-protect
         (funcall continuation)
      (decf (stream-left-margin stream) amount)
      (unless move-cursor
        (setf (stream-cursor-position stream) (values saved-x saved-y))))))

(defmacro indenting-output ((stream indentation &key (move-cursor t)) &body body)
  "Run BODY with STREAM's left margin increased by INDENTATION.
INDENTATION is a number, or a string whose width is the indent."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-indenting-output
        ,stream-var ,indentation
        (lambda () ,@body)
        :move-cursor ,move-cursor))))

(behavior 'indenting-output
  (let ((stream (make-instance 'output-recording-stream)))
    (indenting-output (stream 10)
      (write-string "A" stream)
      (terpri stream)
      (write-string "B" stream))
    (should= 0 (stream-left-margin stream))
    (let ((first (find-recorded-text (stream-output-history stream) "A"))
          (second (find-recorded-text (stream-output-history stream) "B")))
      (should= 10 (nth-value 0 (output-record-sheet-origin first)))
      (should= 0 (nth-value 1 (output-record-sheet-origin first)))
      (should= 10 (nth-value 0 (output-record-sheet-origin second)))
      (should= 12 (nth-value 1 (output-record-sheet-origin second))))
    (terpri stream)
    (should= 0 (stream-cursor-x stream))))

(defun invoke-surrounding-output-with-border
    (stream continuation &key (padding 1) (move-cursor t))
  "Draw a rectangle around the output of CONTINUATION, PADDING outside it.
When MOVE-CURSOR is true, the cursor's x returns to where it started
and its y is the baseline just under the border."
  (let ((start-x (stream-cursor-x stream))
        (start-y (stream-cursor-y stream)))
    (let ((inner (with-new-output-record (stream)
                   (funcall continuation))))
      (multiple-value-bind (left top right bottom)
          (output-record-sheet-box inner)
        (draw-rectangle* stream
                         (- left padding) (- top padding)
                         (+ right padding) (+ bottom padding)
                         :filled nil)
        (when move-cursor
          (setf (stream-cursor-position stream)
                (values start-x
                        (+ bottom padding
                           (nth-value 1 (text-metrics (stream-text-style stream)
                                                      " "))))))))
    (unless move-cursor
      (setf (stream-cursor-position stream) (values start-x start-y)))))

(defmacro surrounding-output-with-border
    ((stream &key (padding 1) (move-cursor t)) &body body)
  "Draw an unfilled rectangle around BODY.  PADDING is the gap."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-surrounding-output-with-border
        ,stream-var
        (lambda () ,@body)
        :padding ,padding
        :move-cursor ,move-cursor))))

(behavior 'surrounding-output-with-border
  (let ((stream (make-instance 'output-recording-stream)))
    (surrounding-output-with-border (stream :padding 1)
      (write-string "Hi" stream))
    (let ((border (find-if (lambda (child)
                             (typep child 'rectangle-displayed-output-record))
                           (output-record-children
                            (stream-output-history stream)))))
      (should-be-false (rectangle-record-filled border))
      (should= -1 (nth-value 0 (output-record-position border)))
      (should= (+ -1 -48/5) (nth-value 1 (output-record-position border)))
      (should= (+ 72/5 2) (rectangle-record-width border))
      (should= (+ 12 2) (rectangle-record-height border)))
    (should= 0 (stream-cursor-x stream))
    (should= 13 (stream-cursor-y stream))))

(defvar *filling-stream* nil
  "The stream whose text is being wrapped, or NIL.")

(defvar *filling-width* nil
  "The x at which *FILLING-STREAM* starts a new line.")

(defvar *filling-word* nil
  "The word being collected for *FILLING-STREAM*.")

(defvar *filling-break* nil
  "True when a space is waiting before *FILLING-WORD*.")

(defvar *filling-pass* nil
  "True while wrapped text is being written through to the stream.")

(defun filling-this-stream? (stream)
  "Return true if STREAM should wrap the text being written."
  (and *filling-stream*
       (eq stream *filling-stream*)
       (not *filling-pass*)))

(defun filling-finish-word (stream)
  "Write the collected word, breaking the line when it does not fit.
A space before the word becomes the line break.  A word that is wider
than the line is written anyway."
  (when (plusp (length *filling-word*))
    (let* ((word (copy-seq *filling-word*))
           (word-width (nth-value 0 (text-metrics (stream-text-style stream) word)))
           (space-width (if *filling-break*
                            (nth-value 0 (text-metrics (stream-text-style stream) " "))
                            0)))
      (when (and *filling-break*
                 (> (+ (stream-cursor-x stream) space-width word-width)
                    *filling-width*)
                 (> (stream-cursor-x stream) (stream-left-margin stream)))
        (terpri stream)
        (setf *filling-break* nil))
      (let ((*filling-pass* t))
        (when *filling-break*
          (write-char #\Space stream))
        (write-string word stream)))
    (setf (fill-pointer *filling-word*) 0
          *filling-break* nil)))

(defun fill-text (stream text)
  "Wrap TEXT onto STREAM.  A space or a newline can end a line."
  (loop for character across text
        do (cond
             ((char= character #\Newline)
              (filling-finish-word stream)
              (setf *filling-break* nil)
              (terpri stream))
             ((char= character #\Space)
              (filling-finish-word stream)
              (setf *filling-break* t))
             (t
              (vector-push-extend character *filling-word*)))))

(defmethod sb-gray:stream-write-char :around
    ((stream output-recording-stream) character)
  (if (filling-this-stream? stream)
      (fill-text stream (string character))
      (call-next-method)))

(defmethod sb-gray:stream-write-string :around
    ((stream output-recording-stream) string &optional (start 0) end)
  (if (filling-this-stream? stream)
      (fill-text stream (subseq string start (or end (length string))))
      (call-next-method)))

(defun invoke-filling-output (stream continuation &key fill-width)
  "Run CONTINUATION and wrap its text to FILL-WIDTH.
FILL-WIDTH defaults to the width of eighty n's.  A space ends a line
when the next word would pass FILL-WIDTH.  A space at the end of
CONTINUATION does not leave a trailing blank."
  (let ((*filling-stream* stream)
        (*filling-width* (or fill-width
                             (* 80 (nth-value 0
                                              (text-metrics
                                               (stream-text-style stream) "n")))))
        (*filling-word* (make-array 16 :element-type 'character
                                       :fill-pointer 0 :adjustable t))
        (*filling-break* nil)
        (*filling-pass* nil))
    (unwind-protect
         (funcall continuation)
      (filling-finish-word stream)
      (setf *filling-break* nil))))

(defmacro filling-output ((stream &key fill-width) &body body)
  "Run BODY and wrap its text to FILL-WIDTH."
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (invoke-filling-output
        ,stream-var
        (lambda () ,@body)
        :fill-width ,fill-width))))

(behavior 'filling-output
  (let ((stream (make-instance 'output-recording-stream)))
    (filling-output (stream :fill-width 252/5)
      (write-string "one two three" stream))
    (let ((two (find-recorded-text (stream-output-history stream) "two"))
          (three (find-recorded-text (stream-output-history stream) "three")))
      (should= 144/5 (nth-value 0 (output-record-sheet-origin two)))
      (should= 0 (nth-value 1 (output-record-sheet-origin two)))
      (should= 0 (nth-value 0 (output-record-sheet-origin three)))
      (should= 12 (nth-value 1 (output-record-sheet-origin three)))))
  (let ((stream (make-instance 'output-recording-stream)))
    (filling-output (stream :fill-width 400)
      (write-string "one two" stream))
    (let ((two (find-recorded-text (stream-output-history stream) "two")))
      (should= 0 (nth-value 1 (output-record-sheet-origin two))))))
