;;;  tiff4cl.asd --- system definition

;;;  Copyright (C) 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: TIFF4CL

#+cmu (ext:file-comment "$Module: tiff4cl.asd, Time-stamp: <2009-05-28 18:13:35 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

(in-package :cl-user)

(defpackage :tiff4cl-system
  (:use :common-lisp :asdf))

(in-package :tiff4cl-system)

(defsystem tiff4cl
    :name "TIFF4CL"
    :author "Walter C. Pelissero <walter@pelissero.de>"
    :maintainer "Walter C. Pelissero <walter@pelissero.de>"
    ;; :version "0.0"
    :description "TIFF access primitives"
    :long-description
    "A TIFF file parser that reads the tags leaving the actual image
data alone."
    :licence "LGPL"
    :depends-on (:ieee-floats)
    :components
    ((:file "package")
     (:file "util" :depends-on ("package"))
     (:file "tiff" :depends-on ("util"))))
