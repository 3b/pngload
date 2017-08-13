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
                  ((:truecolour :indexed-colour) `(,image-width ,image-height 3))
                  (:truecolour-alpha `(,image-width ,image-height 4))
                  (:greyscale-alpha `(,image-width ,image-height 2))
                  (:greyscale `(,image-width ,image-height)))
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
  (if (> x pixel-bytes)
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

(defun unfilter-paeth (x y data start start-up pixel-bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type png-dimension x y)
           (fixnum start start-up)
           (type (unsigned-byte 8) pixel-bytes)
           (optimize speed))
  (let* ((a (unfilter-sub x data start pixel-bytes))
         (b (unfilter-up x y data start-up))
         (c (if (plusp y)
                (unfilter-sub x data start-up pixel-bytes)
                0))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((<= pa pb pc) a)
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
        :for y :below (length scanlines)
        :for (nil start len) = (aref scanlines y)
        :for (nil start-up nil) = (when (plusp y) (aref scanlines (1- y)))
        :for filter = (aref data start)
        :do (loop :for xs fixnum :from start
                  :for x fixnum :from 0 :below len
                  :for sample = (aref data xs)
                  :for out = (unfilter-byte filter x y data start start-up pixel-bytes)
                  :do (setf (aref data xs) ;(mod (+ sample out) 256)
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
          (setf (aref image y x c) (aref (image-data *png-object*) x y c)))))
    (zpng:write-png out "/tmp/out.png")))

(defun decode ()
  (declare (optimize speed))
  (let ((scanlines (get-scanlines))
        (bit-depth (bit-depth *png-object*)))
    (setf (image-data *png-object*) (allocate-image-data))
    (unfilter scanlines)

    (assert (and (typep bit-depth '(unsigned-byte 8))
                 (member bit-depth '(1 2 4 8 16))))
    (loop :with image-data = (image-data *png-object*)
          :with bda :of-type (integer 0 8) = (get-sample-bytes)
          :with data :of-type (simple-array (unsigned-byte 8) (*))
            := (first (aref scanlines 0))
          :for (nil start len) :of-type (null fixnum fixnum) :across scanlines
          :for k :from 0
          :do (loop :for x fixnum :from (1+ start) :below (+ start len) :by bda
                    :for y fixnum :from 0
                    :for v fixnum
                      := (loop :for i :from (1- bda) :downto 0
                               :for j :from x :below (expt 2 20)
                               :sum (ash (aref data j) (* 8 i)) :into s
                               :finally (return s))
                    :do (if (= bit-depth 16)
                            (locally (declare (type (simple-array (unsigned-byte 16) (* * *))
                                                    image-data))
                              (setf (aref image-data (floor y (* bda 3)) k (mod y 3)) v))
                            (locally (declare (type (simple-array (unsigned-byte 8) (* * *))
                                                    image-data))
                              (setf (aref image-data (floor y (* bda 3)) k (mod y 3)) v)))))
    ;; write test image to file
    #++(write-image)
    *png-object*))
