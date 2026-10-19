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

(defpackage :FitzCLIM
  (:use :common-lisp
        :sigma/behave
        :FitzCLIM/geometry
        :FitzCLIM/graphics/design
        :FitzCLIM/graphics/color
        :FitzCLIM/graphics/opacity
        :FitzCLIM/graphics/compose
        :FitzCLIM/graphics/line-style
        :FitzCLIM/graphics/text-style
        :FitzCLIM/sheet
        :FitzCLIM/command
        :FitzCLIM/frame))
(in-package :FitzCLIM)

(dolist (package '(:FitzCLIM/geometry
                   :FitzCLIM/graphics/design
                   :FitzCLIM/graphics/color
                   :FitzCLIM/graphics/opacity
                   :FitzCLIM/graphics/compose
                   :FitzCLIM/graphics/line-style
                   :FitzCLIM/graphics/text-style
                   :FitzCLIM/sheet
                   :FitzCLIM/command
                   :FitzCLIM/frame))
  (do-external-symbols (symbol package)
    (export symbol)))

(behavior 'make-point
  (let ((point (make-point -2 5)))
    (should-be-true (pointp point))
    (should= -2 (point-x point))
    (should= 5 (point-y point))))

(behavior 'make-rgb-color
  (let ((color (make-rgb-color 0 0 1)))
    (should-be-true (colorp color))
    (should-eq :butt (line-style-cap-shape (make-line-style)))
    (should-eq :sans-serif (text-style-family *default-text-style*))))

(behavior 'adopt-sheet
  (let ((parent (make-instance 'basic-sheet))
        (child (make-instance 'basic-sheet)))
    (adopt-sheet parent child)
    (should-eq parent (sheet-parent child))))

(behavior 'make-command-table
  (let ((table (make-command-table 'fitzclim-reexport-tests :errorp nil)))
    (should-be-true (command-table-p table))
    (should-be-true
     (framep (make-instance 'application-frame
                            :name 'reexport
                            :pretty-name "Reexport"
                            :command-table table)))))
