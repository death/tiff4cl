 ;;; util.lisp --- various helper functions

 ;;; Copyright (C) 2009 by Walter C. Pelissero

 ;;; Author: Walter C. Pelissero <walter@pelissero.de>
 ;;; Project: tiff4cl

#+cmu (ext:file-comment "$Module: util.lisp $")

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

(in-package :tiff4cl)

(defmacro make-read-sequence (name buffer)
  `(defun ,name (stream length &key (eof-errorp t) eof-value)
     (let* ((buffer ,buffer)
	    (n (read-sequence buffer stream)))
       (cond ((= n length)
	      buffer)
	     (eof-errorp
	      (error "EOF."))
	     (t eof-value)))))

(make-read-sequence read-bytes (make-sequence '(vector (unsigned-byte 8)) length))
;; (make-read-sequence read-string (make-string length))

(defun decode-integer-BE (sequence &key (start 0) end)
  "Decode a big-endian sequence of bytes as an integer and return it."
  (loop
     with value = 0
     for i from start below (or end (length sequence))
     do (setf value (logior (ash value 8)
			    (elt sequence i)))
     finally (return value)))

(defun decode-integer-LE (sequence &key (start 0) end)
  "Decode a big-endian sequence of bytes as an integer and return it."
  (loop
     with value = 0
     for i from (1- (or end (length sequence))) downto start
     do (setf value (logior (ash value 8)
			    (elt sequence i)))
     finally (return value)))

(defun read-16bit-BE (stream)
  (decode-integer-BE (read-bytes stream 2)))

(defun read-16bit-LE (stream)
  (decode-integer-LE (read-bytes stream 2)))

(defun read-32bit-BE (stream)
  (decode-integer-BE (read-bytes stream 4)))

(defun read-32bit-LE (stream)
  (decode-integer-LE (read-bytes stream 4)))

