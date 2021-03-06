;;; package.lisp --- package definition

;;; Copyright (C) 2009 by Walter C. Pelissero

;;; Author: Walter C. Pelissero <walter@pelissero.de>
;;; Project: tiff4cl

#+cmu (ext:file-comment "$Module: package.lisp $")

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

(defpackage :tiff4cl
  (:nicknames :tiff)
  (:use :common-lisp)
  (:import-from #:ieee-floats
                #:decode-float32
                #:decode-float64)
  (:export #:parse-TIFF
	   #:print-TIFF-tags
	   #:map-TIFF-tags
	   #:TIFF-extract-tags
	   ;; accessors
	   #:ifd-tags
	   #:tag-id
	   #:tag-type
	   #:tag-value))
