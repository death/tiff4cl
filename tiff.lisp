;;; tiff.lisp --- TIFF parsing

;;; Copyright (C) 2009-2015 by Walter C. Pelissero

;;; Author: Walter C. Pelissero <walter@pelissero.de>
;;; Project: tiff4cl

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


;; Lifted from SCLF
(defmacro defconst (name value &rest etc)
  #+sbcl (list* 'defvar name value etc)
  #-sbcl (list* 'defconstant name value etc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition TIFF-error (error) ())

(define-condition not-a-TIFF (TIFF-error) ())

(define-condition wrong-version (TIFF-error)
  ((version :initarg :version
	    :reader TIFF-error-version)))

(define-condition invalid-pointer (TIFF-error)
  ((pointer :initarg :pointer
	    :reader TIFF-error-pointer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TIFF-stream ()
  ((stream :initarg :stream
	   :reader TIFF-stream)
   (start :initarg :start
	  :initform 0
	  :type integer
	  :reader TIFF-start)
   (end :initarg :end
	:initform nil
	:type (or integer null)
	:reader TIFF-end)))

(defclass TIFF-little-endian-stream (TIFF-stream) ())
(defclass TIFF-big-endian-stream (TIFF-stream) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric decode-integer (tiff buffer &key start end))
(defmethod decode-integer ((tiff TIFF-little-endian-stream) buffer &key (start 0) end)
  (decode-integer-LE buffer
		     :start start
		     :end end))
(defmethod decode-integer ((tiff TIFF-big-endian-stream) buffer &key (start 0) end)
  (decode-integer-BE buffer
		     :start start
		     :end end))

(defgeneric read-16bit (tiff))
(defmethod read-16bit ((tiff TIFF-little-endian-stream))
  (read-16bit-le (TIFF-stream tiff)))
(defmethod read-16bit ((tiff TIFF-big-endian-stream))
  (read-16bit-be (TIFF-stream tiff)))

(defgeneric read-32bit (tiff))
(defmethod read-32bit ((tiff TIFF-little-endian-stream))
  (read-32bit-le (TIFF-stream tiff)))
(defmethod read-32bit ((tiff TIFF-big-endian-stream))
  (read-32bit-be (TIFF-stream tiff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +little-endian-signature+
  (map 'vector #'char-code "II"))

(defconst +big-endian-signature+
  (map 'vector #'char-code "MM"))

(defconst +TIFF-version+ 42
  "What was the question?")

(defun parse-endianness (stream)
  (let ((buffer (read-bytes stream 2)))
    (cond ((equalp +little-endian-signature+ buffer)
	   ;; Intel's little endian
	   :little-endian)
	  ((equalp +big-endian-signature+ buffer)
	   ;; Motorola's big endian
	   :big-endian)
	  (t
	   (error 'not-a-TIFF)))))

(defun parse-file-format-version (tiff)
  (read-16bit tiff))

(defun read-IFD-pointer (tiff)
  (let ((pointer (read-32bit tiff)))
    (when (or (and (< pointer 8)
		   ;; the last pointer is always zero
		   (not (zerop pointer)))
	      (not (zerop (mod pointer 2)))
	      (let ((end (TIFF-end tiff)))
		(when end
		  (> pointer (- end (TIFF-start tiff))))))
      (error 'invalid-pointer :pointer pointer))
    pointer))

(defun TIFF-position (tiff &optional position)
  (- (TIFF-start tiff)
     (if position
	 (progn
	   (assert
	    (file-position (TIFF-stream tiff)
			   (+ position (TIFF-start tiff))))
	   position)
	 (file-position (TIFF-stream tiff)))))

(defun read-IFD-tags-number (tiff)
  (read-16bit tiff))

(defconst +tag-types+
  '((1 :byte 1)
    (2 :ascii 1)
    (3 :short 2)
    (4 :long 4)
    (5 :rational 8)
    (6 :signed-byte 1)
    (7 :undefined 1)
    (8 :signed-short 2)
    (9 :signed-long 4)
    (10 :signed-rational 8)
    (11 :float 4)
    (12 :double 8))
  "List of supported tag types.  Every type is a triplet consisting
of: a numeric id, a keyword id, and a length in bytes.")

(defun tag-type-size (type)
  (caddr (find type +tag-types+ :key #'cadr)))

(defun fetch-tag-data (file address length)
  (TIFF-position file (decode-integer file address))
  (read-bytes (TIFF-stream file) length))

(defclass TIFF-tag ()
  ((id :initarg :id
       :reader tag-id)
   (type :initarg :type
	 :reader tag-type)
   (value :initarg :value
	  :reader tag-value)))

(defclass TIFF-IFD ()
  ((tags :initarg :tags
	 :type list
	 :reader ifd-tags)
   (next :initarg :next
	 :reader ifd-next)))

(defmethod print-object ((object TIFF-tag) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (object stream :type t)
	(format stream "~A" (tag-id object)))))

(defun get-tag-value (file type count data)
  (let ((item-size (tag-type-size type)))
    (if item-size
	(let ((max-positive (ash 1 (1- (* 8 item-size)))))
	  (labels ((maybe-negative (x)
		     (if (< x max-positive)
			 x
			 (lognot x)))
		   (parse-byte (buffer pos)
		     (aref buffer pos))
		   (parse-int (buffer start end)
		     (decode-integer file buffer :start start :end end))
		   (parse-data-item (buffer pos)
		     (case type
		       (:byte (parse-byte buffer pos))
		       (:ascii (code-char (aref buffer pos)))
		       ((:short :long)
			(parse-int buffer (* pos item-size) (* (1+ pos) item-size)))
		       (:rational
			(/ (parse-int buffer
					  (* pos item-size)
					  (+ (* pos item-size) (/ item-size 2)))
			   (parse-int buffer
					  (+ (* pos item-size) (/ item-size 2))
					  (* (1+ pos) item-size))))
		       (:signed-byte
			(maybe-negative (parse-byte buffer pos)))
		       (:undefined
			(aref buffer pos))
		       ((:signed-short :signed-long)
			(maybe-negative
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))
		       (:signed-rational
			(/ (maybe-negative
			    (parse-int buffer
					   (* pos item-size)
					   (+ (* pos item-size) (/ item-size 2))))
			   (maybe-negative
			    (parse-int buffer
					   (+ (* pos item-size) (/ item-size 2))
					   (* (1+ pos) item-size)))))
		       (:float
			(ieee-754:decode-ieee-float
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))
		       (:double
			(ieee-754:decode-ieee-double
			 (parse-int buffer
					(* pos item-size)
					(* (1+ pos) item-size))))))
		   (parse-tag-data (buffer)
		     (if (= 1 count)
			 (parse-data-item buffer 0)
			 (loop
			    with result = (make-array count)
			    for i from 0 below count
			    do (setf (aref result i) (parse-data-item buffer i))
			    finally (return (if (eq type :ascii)
						(coerce (subseq result 0
								;; get rid of the trailing null
								(1- (length result)))
							'string)
						result))))))
	    (cond ((> (* item-size count) 4)
		   (parse-tag-data
		    (fetch-tag-data file data (* item-size count))))
		  (t
		   (parse-tag-data data)))))
	;; the value is in the data field itself
	data)))


(defparameter *tag-ids*
  '((254 :new-subfile-type)
    (255 :subfile-type)
    (256 :image-width)
    (257 :image-length)
    (258 :bits-per-sample)
    (259 :compression ((1 nil)
		       (2 :CCITT-1d)
		       (3 :fax-group3)
		       (4 :fax-group4)
		       (5 :LZW)
		       (6 :JPEG)
		       (7 :JPEG2)	; ???
		       (8 :ZIP)		; Photoshop extension
		       (32773 :pack-bits)))
    (262 :photometric-interpretation ((0 :white-is-zero)
				      (1 :black-is-zero)
				      (2 :RGB)
				      (3 :RGB-palette)
				      (4 :transparency-mask)
				      (5 :CMYK)
				      (6 :YCbCr)
				      (8 :CIE-Lab)
				      (9 :ICC-Lab)))
    (263 :threshholding)
    (264 :cell-width)
    (265 :cell-length)
    (266 :fill-order)
    (269 :document-name)
    (270 :image-description)
    (271 :make)
    (272 :model)
    (273 :strip-offsets)
    (274 :orientation)
    (277 :samples-per-pixel)
    (278 :rows-per-strip)
    (279 :strip-byte-counts)
    (280 :min-sample-value)
    (281 :max-sample-value)
    (282 :x-resolution)
    (283 :y-resolution)
    (284 :planar-configuration)
    (285 :page-name)
    (286 :x-position)
    (287 :y-position)
    (288 :free-offsets)
    (289 :free-byte-counts)
    (290 :gray-response-unit)
    (291 :gray-response-curve)
    (292 :t4-options)
    (293 :t6-options)
    (296 :resolution-unit ((1 nil)
			   (2 :inch)
			   (3 :cm)))
    (297 :page-number)
    (301 :transfer-function)
    (305 :software)
    (306 :date-time)
    (315 :artist)
    (316 :host-computer)
    (317 :predictor)
    (318 :white-point)
    (319 :primary-chromaticities)
    (320 :color-map)
    (321 :halftone-hints)
    (322 :tile-width)
    (323 :tile-length)
    (324 :tile-offsets)
    (325 :tile-byte-counts)
    (330 :sub-ifds)			; PageMaker extension
    (332 :ink-set)
    (333 :ink-names)
    (334 :number-of-inks)
    (336 :dot-range)
    (337 :target-printer)
    (338 :extra-samples)
    (339 :sample-format)
    (340 :s-min-sample-value)
    (341 :s-max-sample-value)
    (342 :transfer-range)
    (343 :clip-path)			; PageMaker extension
    (344 :x-clip-path-units)		; PageMaker extension
    (345 :y-clip-path-units)		; PageMaker extension
    (346 :indexed)			; PageMaker extension
    (347 :JPEG-tables)			; Photoshop extension
    (351 :OPI-proxy)			; PageMaker extension
    (512 :JPEG-proc)
    (513 :JPEG-interchange-format)
    (514 :JPEG-interchange-format-length)
    (515 :JPEG-restart-interval)
    (517 :JPEG-lossless-predictors)
    (518 :JPEG-point-transforms)
    (519 :JPEG-q-tables)
    (520 :JPEG-dc-tables)
    (521 :JPEG-ac-tables)
    (529 :y-cb-cr-coefficients)
    (530 :y-cb-cr-sub-sampling)
    (531 :y-cb-cr-positioning ((1 :centered)
			       (2 :cosited)))
    (532 :reference-black-white)
    (32781 :image-id)			; PageMaker extension
    (33432 :copyright)
    (34665 :exif-IFD)			; Exif extension
    (34853 :GPS-IFD)			; Exif extension
    (37724 :image-source-data)		; Photoshop extension
    (40965 :interoperability-IFD)	; Exif extension
    (50341 :print-IM)			; Exif extension

    ;; Exif extensions
    (33434 :exposure-time)
    (33437 :F-number)
    (34850 :exposure-program ((0 nil)
			      (1 :manual)
			      (2 :normal)
			      (3 :aperture-priority)
			      (4 :shutter-priority)
			      (5 :creative)
			      (6 :action)
			      (7 :portrait)
			      (8 :landscape)))
    (34852 :spectral-sensitivity)
    (34855 :ISO-speed-ratings)
    (34856 :OECF)
    (36864 :exif-version)
    (36867 :date-time-original)
    (36868 :date-time-digitized)
    (37121 :components-configuration)
    (37122 :compressed-bits-per-pixel)
    (37377 :shutter-speed-value)
    (37378 :aperture-value)
    (37379 :brightness-value)
    (37380 :exposure-bias-value)
    (37381 :max-aperture-value)
    (37382 :subject-distance)
    (37383 :metering-mode ((0 nil)
			   (1 :average)
			   (2 :center-weighted-average)
			   (3 :spot)
			   (4 :multi-spot)
			   (5 :pattern)
			   (6 :partial)
			   (255 :other)))
    (37384 :light-source ((0 nil)
			  (1 :daylight)
			  (2 :fluorescent)
			  (3 :tungsten)
			  (4 :flash)
			  (9 :fine-weather)
			  (10 :cloudy-weather)
			  (11 :shade)
			  (12 :daylight-fluorescent) ; (D 5700 - 7100K)
			  (13 :day-white-fluorescent) ; (N 4600 - 5400K)
			  (14 :cool-white-fluorescent) ; (W 3900 - 4500K)
			  (15 :white-fluorescent) ; (WW 3200 - 3700K)
			  (17 :standard-light-A)
			  (18 :standard-light-B)
			  (19 :standard-light-C)
			  (20 :d55)
			  (21 :d65)
			  (22 :d75)
			  (23 :d50)
			  (24 :iso-studio-tungsten)
			  (255 :other-light-source)))
    (37385 :flash)
    (37386 :focal-length)
    (37396 :subject-area)
    (37500 :maker-note)
    (37510 :user-comment)
    (37520 :subsec-time)
    (37521 :subsec-time-original)
    (37522 :subsec-time-digitized)
    (40960 :flashpix-version)
    (40961 :color-space)
    (40962 :pixel-x-dimension)
    (40963 :pixel-y-dimension)
    (40964 :related-sound-file)
    (41483 :flash-energy)
    (41484 :spatial-frequency-response)
    (41486 :focal-plane-x-resolution)
    (41487 :focal-plane-y-resolution)
    (41488 :focal-plane-resolution-unit)
    (41492 :subject-location)
    (41493 :exposure-index)
    (41495 :sensing-method ((1 :undefined)
			    (2 :one-chip-color-area)
			    (3 :two-chip-color-area)
			    (4 :three-chip-color-area)
			    (5 :color-sequential-area)
			    (7 :trilinear)
			    (8 :color-sequential-linear)))
    (41728 :file-source ((3 :dsc)))
    (41729 :scene-type ((1 :direct)))
    (41730 :CFA-pattern)
    (41985 :custom-rendered ((0 nil)
			     (1 t)))
    (41986 :exposure-mode ((0 :auto)
			   (1 :manual)
			   (2 :auto-bracket)))
    (41987 :white-balance ((0 :auto)
			   (1 :manual)))
    (41988 :digital-zoom-ratio ((0 nil)))
    (41989 :focal-length-in-35mm-film ((0 nil)))
    (41990 :scene-capture-type ((0 :standard)
				(1 :landscape)
				(2 :portrait)
				(3 :night)))
    (41991 :gain-control ((0 nil)
			  (1 :low-gain-up)
			  (2 :high-gain-up)
			  (3 :low-gain-down)
			  (4 :high-gain-down)))
    (41992 :contrast ((0 :normal)
		      (1 :soft)
		      (2 :hard)))
    (41993 :saturation ((0 :normal)
			(1 :low)
			(2 :high)))
    (41994 :sharpness ((0 :normal)
		       (1 :soft)
		       (2 :hard)))
    (41995 :device-setting-description)
    (41996 :subject-distance-range)
    (42016 :image-unique-id)
    ))

(defun interpret-tag-value (file id value)
  (case id
    ((:exif-IFD :GPS-IFD :interoperability-IFD)
     (parse-IFD file value))
    ((:flashpix-version :exif-version)
     (let ((str (map 'string #'code-char value)))
       (cons (parse-integer str :start 0 :end 2)
	     (parse-integer str :start 2))))
    (:flash
     (let ((features '()))
       (macrolet ((bf (feature bit)
		    `(when (logbitp ,bit value)
		       (push ,feature features))))
	 (bf :fired 0)
	 (when (logbitp 2 value)
	   (push (if (logbitp 1 value)
		     :strobe-detected
		     :strobe-undetected)
		 features))
	 (case (ldb (byte 2 3) value)
	   (1 (push :flash-forced-on features))
	   (2 (push :flash-disabled features))
	   (3 (push :auto-mode features)))
	 (bf :flash-present 5)
	 (bf :red-eye-suppression 6))
       features))
    (t
     (let ((symbolic-mapping (assoc value (caddr (find id *tag-ids* :key #'cadr)))))
       (if symbolic-mapping
	   (cadr symbolic-mapping)
	   value)))))

(defun tag-id->symbol (n)
  (cadr (assoc n *tag-ids*)))

(defun read-tag-id (tiff)
  (let ((x (read-16bit tiff)))
    (or (tag-id->symbol x) x)))

(defun tag-type->symbol (n)
  (cadr (assoc n +tag-types+)))

(defun read-tag-type (tiff)
  (let ((x (read-16bit tiff)))
    (or (tag-type->symbol x)
	x)))

(defun read-tag-values-count (tiff)
  (read-32bit tiff))

(defun read-tag-data (tiff)
  (read-bytes (TIFF-stream tiff) 4))

(defun parse-tag (tiff)
  (let* ((id (read-tag-id tiff))
	 (type (read-tag-type tiff))
	 (n (read-tag-values-count tiff))
	 (data (read-tag-data tiff)))
    (list id type n data)))

(defun parse-IFD (tiff &optional position)
  (when position
    (TIFF-position tiff position))
  ;; First read the tags data and then create the objects, because
  ;; get-tag-value can seek through the file to fetch the data values,
  ;; thus disrupting the sequential read we do here.
  (let* ((tags (loop
		  for i from 0 below (read-IFD-tags-number tiff)
		  collect (parse-tag tiff)))
	 (next-ifd (read-IFD-pointer tiff))
	 (tag-objects (mapcar #'(lambda (tag)
				  (destructuring-bind (id type count data) tag
				    (let ((value (get-tag-value tiff type count data)))
				      (make-instance 'TIFF-tag
						     :id id
						     :type type
						     :value (interpret-tag-value tiff id value)))))
			      tags)))
    (make-instance 'TIFF-IFD :tags tag-objects :next next-ifd)))

(defun parse-TIFF-stream (stream &optional end)
  (let* ((start (file-position stream))
	 (endianness (parse-endianness stream))
	 (tiff (make-instance (if (eq endianness :little-endian)
				  'TIFF-little-endian-stream
				  'TIFF-big-endian-stream)
			      :start start
			      :end end
			      :stream stream))
	 (version (parse-file-format-version tiff)))
    (unless (= version +TIFF-version+)
      (error 'wrong-version :version version))
    (let ((ifds '())
	  (address (read-IFD-pointer tiff)))
      (loop
	 (when (zerop address)
	   (return))
	 (let ((ifd (parse-IFD tiff address)))
	   (push ifd ifds)
	   (setf address (ifd-next ifd))))
      ifds)))

(defun parse-TIFF (file &key start end)
  (if (streamp file)
      (progn
	(when start
	  (file-position file start))
	(parse-TIFF-stream file end))
      (with-open-file (stream file)
	(when start
	  (file-position stream start))
	(parse-TIFF-stream stream end))))

(defun map-IFD-tags (function ifd)
  (dolist (tag (ifd-tags ifd))
    (let ((value (tag-value tag)))
      (if (typep value 'tiff-ifd)
	  (map-IFD-tags function value)
	  (funcall function tag)))))

(defun map-TIFF-tags (function ifds)
  (dolist (ifd ifds)
    (map-IFD-tags function ifd)))

(defun TIFF-extract-tags (ifds tag-ids)
  (let ((result '()))
    (map-TIFF-tags #'(lambda (tag)
		       (when (or (eq tag-ids t)
				 (find (tag-id tag) tag-ids))
			 (push (cons (tag-id tag) (tag-value tag))
			       result)))
		   ifds)
    result))

(defun print-TIFF-tags (ifds &optional stream)
  (unless stream
    (setf stream *standard-output*))
  (labels ((print-IFD (ifd indent)
	     (loop
		for tag in (ifd-tags ifd)
		for value = (tag-value tag)
		if (typep value 'TIFF-IFD)
		do
		  (format stream "~vT~A:~%" indent (tag-id tag))
		  (print-IFD value (+ 2 indent))
		else do
		  (format stream "~vT~A = ~S~%" indent (tag-id tag) value))))
    (loop
       for ifd in ifds
       for i from 0
       do
	 (format stream "~&IFD ~A:~%" i)
	 (print-IFD ifd 2))))
