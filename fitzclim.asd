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

(defsystem "fitzclim"
  :description "A Common Lisp implementation of the CLIM II protocols and the CLIM 2.2 revisions."
  :version "0.0.1"
  :author "Christopher Mark Gore <cgore@cgore.com>"
  :license "BSD-3-Clause"
  :homepage "https://github.com/cgore/FitzCLIM"
  :source-control (:git "https://github.com/cgore/FitzCLIM.git")
  :bug-tracker "https://github.com/cgore/FitzCLIM/issues"
  :depends-on ("sigma")
  :perform (test-op (operation system)
                    (declare (ignore operation))
                    (labels ((reload (component)
                               (typecase component
                                 (asdf:cl-source-file
                                  (load (asdf:component-pathname component)))
                                 (asdf:parent-component
                                  (map nil #'reload (asdf:component-children component))))))
                      (reload system)))
  :components
  ((:module "source"
    :components
    ((:module "geometry"
      :components
      ((:file "region")
       (:file "point"
        :depends-on ("region"))
       (:file "rectangle"
        :depends-on ("point"))
       (:file "polygon"
        :depends-on ("point"))
       (:file "transformation"
        :depends-on ("point"))
       (:file "combine"
        :depends-on ("rectangle" "polygon"))
       (:file "transform-region"
        :depends-on ("combine" "transformation"))
       (:file "bounding-rectangle"
        :depends-on ("polygon" "rectangle" "combine"))))
     (:module "graphics"
      :components
      ((:file "design")
       (:file "color"
        :depends-on ("design"))
       (:file "opacity"
        :depends-on ("design"))
       (:file "compose"
        :depends-on ("color" "opacity"))
       (:file "line-style")
       (:file "text-style")))
     (:module "sheet"
      :depends-on ("geometry")
      :components
      ((:file "sheet")))
     (:module "command"
      :components
      ((:file "command-table")
       (:file "define-command"
        :depends-on ("command-table"))))
     (:module "frame"
      :depends-on ("command" "geometry" "sheet" "gadget" "layout")
      :components
      ((:file "frame")))
     (:module "event"
      :components
      ((:file "event")))
     (:module "presentation"
      :depends-on ("command" "frame" "event" "output-record")
      :components
      ((:file "presentation-type")
       (:file "present"
        :depends-on ("presentation-type"))
       (:file "builtins"
        :depends-on ("present"))
       (:file "translator"
        :depends-on ("builtins"))))
     (:module "output-record"
      :depends-on ("geometry" "graphics")
      :components
      ((:file "output-record")))
     (:module "medium"
      :depends-on ("output-record")
      :components
      ((:file "medium")
       (:file "drawing"
        :depends-on ("medium"))))
     (:module "layout"
      :depends-on ("geometry" "sheet")
      :components
      ((:file "layout")))
     (:module "pane"
      :depends-on ("sheet" "medium" "presentation" "layout")
      :components
      ((:file "pane")))
     (:module "gadget"
      :depends-on ("geometry" "sheet" "layout")
      :components
      ((:file "gadget")))
     (:module "opengl"
      :depends-on ("sheet" "layout" "geometry" "presentation" "event")
      :components
      ((:file "opengl")))
     (:module "windows"
      :depends-on ("frame" "pane" "gadget" "event" "sheet" "geometry"
                   "medium" "output-record" "graphics" "opengl")
      :components
      ((:file "windows")
       (:file "gl"
        :depends-on ("windows"))))
     (:file "fitzclim"
      :depends-on ("geometry" "graphics" "sheet" "command" "frame"
                   "event" "presentation" "output-record" "medium"
                   "pane" "gadget" "layout" "opengl" "windows"))))))
