(in-package :mediabox-png)

(deftype png-dimension () '(unsigned-byte 32))

(defun get-channel-count ()
  (ecase (color-type *png-object*)
    (:truecolour 3)
    (:truecolour-alpha 4)
    (:indexed-colour 1)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun get-sample-bytes ()
  (max 1 (/ (bit-depth *png-object*) 8)))

(defun get-pixel-bytes ()
  (* (get-sample-bytes) (get-channel-count)))

(defun get-scanline-bytes ()
  (1+ (* (image-width *png-object*) (get-pixel-bytes))))

(defun get-filter-type-name (filter-type)
  (ecase filter-type
    (0 :none)
    (1 :sub)
    (2 :up)
    (3 :average)
    (4 :paeth)))

(defconstant +ft0-none+ 0)
(defconstant +ft0-sub+ 1)
(defconstant +ft0-up+ 2)
(defconstant +ft0-average+ 3)
(defconstant +ft0-paeth+ 4)

(defun allocate-image-data ()
  (with-slots (image-width image-height color-type bit-depth) *png-object*
    (make-array (case color-type
                  ((:truecolour :indexed-colour) `(,image-height ,image-width 3))
                  (:truecolour-alpha `(,image-height ,image-width 4))
                  (:greyscale-alpha `(,image-height ,image-width 2))
                  (:greyscale `(,image-height ,image-width)))
                :element-type (ecase bit-depth
                                ((1 2 4 8) '(unsigned-byte 8))
                                (16 '(unsigned-byte 16))))))

(defun get-scanlines ()
  (loop :with data = (image-data *png-object*)
        :with scanlines = (make-array (image-height *png-object*))
        :with size = (get-scanline-bytes)
        :for line :below (length scanlines)
        :for start = (* line size)
        :for end = (min (length data) (* (1+ line) size))
        :do (setf (aref scanlines line) (list data start (- end start)))
        :finally (return scanlines)))

(declaim (inline unfilter-sub unfilter-up unfilter-average unfilter-paeth
                 unfilter-byte))

(defun unfilter-sub (x data start pixel-bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type (unsigned-byte 8) pixel-bytes)
           (type png-dimension x)
           (fixnum start)
           (optimize speed))
  (if (>= x pixel-bytes)
      (aref data (+ start (- x pixel-bytes)))
      0))

(defun unfilter-up (x y data start-up)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type png-dimension x y)
           (fixnum start-up)
           (optimize speed))
  (if (zerop y)
      0
      (aref data (+ x start-up))))

(defun unfilter-average (x y data start start-up pixel-bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type png-dimension x y)
           (fixnum start start-up)
           (type (unsigned-byte 8) pixel-bytes)
           (optimize speed))
  (let ((a (unfilter-sub x data start pixel-bytes))
        (b (unfilter-up x y data start-up)))
    (declare (type (unsigned-byte 8) a b))
    (floor (+ a b)
           2)))

(defun unfilter-paeth (x y data start-left start-up pixel-bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type png-dimension x y)
           (fixnum start-left start-up)
           (type (unsigned-byte 8) pixel-bytes)
           (optimize speed))
  (let* ((a (unfilter-sub x data start-left pixel-bytes))
         (b (unfilter-up x y data start-up))
         (c (if (plusp y)
                (unfilter-sub x data start-up pixel-bytes)
                0))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(defun unfilter-byte (filter x y data start start-up pixel-bytes)
  (case filter
    (#.+ft0-none+ 0)
    (#.+ft0-sub+ (unfilter-sub x data start pixel-bytes))
    (#.+ft0-up+ (unfilter-up x y data start-up))
    (#.+ft0-average+ (unfilter-average x y data start start-up pixel-bytes))
    (#.+ft0-paeth+ (unfilter-paeth x y data start start-up pixel-bytes))))

(defun unfilter (scanlines)
  (declare (optimize speed))
  (loop :with pixel-bytes = (get-pixel-bytes)
        :with data :of-type (simple-array (unsigned-byte 8) (*))
          := (first (aref scanlines 0))
        :with row-bytes fixnum = (* pixel-bytes (image-width *png-object*))
        :with scanline-bytes = (1+ row-bytes)
        :for y :below (length scanlines)
        :for in-start from 0 by scanline-bytes
        :for left-start from 0 by row-bytes
        :for up-start from (- row-bytes) by row-bytes
        :for filter = (aref data in-start)
        :do (loop :for xs fixnum :from (1+ in-start)
                  :for xo fixnum :from left-start
                  :for x fixnum :from 0 :below row-bytes
                  :for sample = (aref data xs)
                  :for out = (unfilter-byte filter x y data
                                            left-start up-start pixel-bytes)
                  :do (setf (aref data xo)
                            (logand #xff (+ sample out))))))

(defun write-image ()
  (let* ((out (make-instance 'zpng:png
                             :color-type :truecolor
                             :width (image-width *png-object*)
                             :height (image-height *png-object*)))
         (image (zpng:data-array out)))
    (dotimes (x (image-width *png-object*))
      (dotimes (y (image-height *png-object*))
        (dotimes (c 3)
          (setf (aref image y x c)
                (aref (image-data *png-object*) y x c)))))
    (zpng:write-png out "/tmp/out.png")))

(defun decode ()
  (declare (optimize speed))
  (let ((scanlines (get-scanlines))
        (data (image-data *png-object*))
        (bit-depth (bit-depth *png-object*)))
    (declare (type (simple-array (unsigned-byte 8) (*)) data))
    (setf (image-data *png-object*) (allocate-image-data))
    (unfilter scanlines)

    (assert (and (typep bit-depth '(unsigned-byte 8))
                 (member bit-depth '(1 2 4 8 16))))
    (let ((image-data (image-data *png-object*)))
      (if (= bit-depth 16)
          (locally (declare (type (simple-array (unsigned-byte 16) (* * *))
                                  image-data))
            (loop :for d :below (array-total-size image-data)
                  :for s :below (array-total-size data) :by 2
                  :for v of-type (unsigned-byte 16)
                    := (progn ;locally (declare (optimize (safety 0)))
                         (dpb (aref data s) (byte 8 8)
                              (aref data (1+ s))))
                  :do (progn ;locally (declare (optimize (safety 0)))
                        (setf (row-major-aref image-data d) v))))
          (locally (declare (type (simple-array (unsigned-byte 8) (* * *))
                                  image-data))
            (loop :for d :below (array-total-size image-data)
                  :for s :below (array-total-size data)
                  :do (setf (row-major-aref image-data d)
                            (aref data s))))))
    ;; write test image to file
    ;(time (write-image))
    *png-object*))
