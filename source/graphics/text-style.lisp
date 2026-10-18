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

(defpackage :FitzCLIM/graphics/text-style
  (:use :common-lisp
        :sigma/behave)
  (:export :*default-text-style*
           :*undefined-text-style*
           :make-text-style
           :merge-text-styles
           :parse-text-style
           :standard-text-style
           :text-style
           :text-style-components
           :text-style-face
           :text-style-family
           :text-style-p
           :text-style-size))
(in-package :FitzCLIM/graphics/text-style)

(defclass text-style ()
  ()
  (:documentation "A family, a face, and a size for drawing text."))

(defclass standard-text-style (text-style)
  ((family
    :initarg :family
    :reader text-style-family
    :documentation "The font family.  :FIX, :SERIF, :SANS-SERIF, a
    string, or NIL when the family is unspecified.")
   (face
    :initarg :face
    :reader text-style-face
    :documentation "The face.  :ROMAN, :BOLD, :ITALIC, :BOLD-ITALIC, a
    list, or NIL when the face is unspecified.")
   (size
    :initarg :size
    :reader text-style-size
    :documentation "The size.  A size keyword, a positive real in
    points, or NIL when the size is unspecified."))
  (:documentation "A text style stored as family, face, and size."))

(defun text-style-p (object)
  "Return true if OBJECT is a text style."
  (typep object 'text-style))

(defun canonicalize-family (family)
  "Return a stored family for FAMILY."
  (cond
    ((null family) nil)
    ((member family '(:fix :serif :sans-serif)) family)
    ((and (stringp family) (plusp (length family))) family)
    (t (error "The family ~S is not a text style family." family))))

(defun face-keyword? (keyword)
  "Return true if KEYWORD is a single text-style face."
  (member keyword '(:roman :bold :italic :bold-italic
                    :condensed :extended)))

(defun canonicalize-face (face)
  "Return a stored face for FACE.
A list that is only bold and italic is stored as :BOLD-ITALIC.  :ROMAN
with no other face is stored as :ROMAN."
  (cond
    ((null face) nil)
    ((keywordp face)
     (unless (member face '(:roman :bold :italic :bold-italic))
       (error "The face ~S is not a text style face." face))
     face)
    ((listp face)
     (let ((items (remove-duplicates face :test #'eq)))
       (unless (and items (every #'face-keyword? items))
         (error "The face ~S is not a text style face." face))
       (when (and (member :condensed items) (member :extended items))
         (error "A face cannot be both condensed and extended."))
       (when (member :bold-italic items)
         (setf items (remove-duplicates
                      (append '(:bold :italic)
                              (remove :bold-italic items))
                      :test #'eq)))
       (setf items (remove :roman items))
       (let ((bold (member :bold items))
             (italic (member :italic items))
             (width (remove-if-not (lambda (item)
                                     (member item '(:condensed :extended)))
                                   items)))
         (cond
           ((and bold italic width)
            (append '(:bold :italic) width))
           ((and bold italic) :bold-italic)
           ((and bold width) (cons :bold width))
           (bold :bold)
           ((and italic width) (cons :italic width))
           (italic :italic)
           (width (if (= (length width) 1) (first width) width))
           (t :roman)))))
    (t (error "The face ~S is not a text style face." face))))

(defun canonicalize-size (size)
  "Return a stored size for SIZE."
  (cond
    ((null size) nil)
    ((member size '(:tiny :very-small :small :normal :large
                    :very-large :huge :smaller :larger))
     size)
    ((and (realp size) (plusp size)) size)
    (t (error "The size ~S is not a text style size." size))))

(defun make-text-style (family face size)
  "Return a text style.  NIL for any component leaves that component
unspecified.  FAMILY is :FIX, :SERIF, :SANS-SERIF, or a font name.
FACE is :ROMAN, :BOLD, :ITALIC, :BOLD-ITALIC, or a list of those.
SIZE is a size keyword or a positive real, in points."
  (make-instance 'standard-text-style
                 :family (canonicalize-family family)
                 :face (canonicalize-face face)
                 :size (canonicalize-size size)))

(defun text-style-components (text-style)
  "Return the family, face, and size of TEXT-STYLE."
  (values (text-style-family text-style)
          (text-style-face text-style)
          (text-style-size text-style)))

(defvar *undefined-text-style* (make-text-style nil nil nil)
  "The text style whose family, face, and size are all unspecified.")

(defvar *default-text-style* (make-text-style :sans-serif :roman :normal)
  "The text style FitzCLIM uses when a medium has not chosen one.")

(defun parse-text-style (style)
  "Return a text style.  STYLE is a text style, NIL, or a list of a
family, a face, and a size.  NIL is *UNDEFINED-TEXT-STYLE*."
  (etypecase style
    (null *undefined-text-style*)
    (text-style style)
    (cons
     (unless (= (length style) 3)
       (error "A text style list has a family, a face, and a size."))
     (apply #'make-text-style style))))

(defun merge-text-styles (style-1 style-2)
  "Return a text style.  Each component comes from STYLE-1 when STYLE-1
specifies it, and from STYLE-2 otherwise."
  (let ((left (parse-text-style style-1))
        (right (parse-text-style style-2)))
    (make-text-style (or (text-style-family left)
                         (text-style-family right))
                     (or (text-style-face left)
                         (text-style-face right))
                     (or (text-style-size left)
                         (text-style-size right)))))

(behavior 'make-text-style
  (let ((style (make-text-style :serif :bold 12)))
    (should-be-true (text-style-p style))
    (multiple-value-bind (family face size) (text-style-components style)
      (should-eq :serif family)
      (should-eq :bold face)
      (should= 12 size)))
  (should-eq :bold-italic
             (text-style-face (make-text-style :fix '(:italic :bold) :large)))
  (should-eq :roman (text-style-face (make-text-style nil '(:roman) nil)))
  (multiple-value-bind (family face size)
      (text-style-components *default-text-style*)
    (should-eq :sans-serif family)
    (should-eq :roman face)
    (should-eq :normal size))
  (multiple-value-bind (family face size)
      (text-style-components *undefined-text-style*)
    (should-be-null family)
    (should-be-null face)
    (should-be-null size))
  (should-be-true
   (handler-case
       (progn (make-text-style :script :roman :normal) nil)
     (error () t))))

(behavior 'merge-text-styles
  (let ((style (merge-text-styles (make-text-style :serif nil :large)
                                  *default-text-style*)))
    (multiple-value-bind (family face size) (text-style-components style)
      (should-eq :serif family)
      (should-eq :roman face)
      (should-eq :large size)))
  (let ((style (parse-text-style '(:fix :italic :small))))
    (should-eq :fix (text-style-family style))
    (should-eq :italic (text-style-face style))
    (should-eq :small (text-style-size style)))
  (should-eq *undefined-text-style* (parse-text-style nil))
  (should-eq :smaller
             (text-style-size
              (merge-text-styles '(:sans-serif nil :smaller)
                                 *default-text-style*))))
