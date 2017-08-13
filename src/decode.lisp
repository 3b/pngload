(in-package :mediabox-png)

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
  (with-slots (image-width image-height color-type) *png-object*
    (make-array (case color-type
                  ((:truecolour :indexed-colour) `(,image-width ,image-height 3))
                  (:truecolour-alpha `(,image-width ,image-height 4))
                  (:greyscale-alpha `(,image-width ,image-height 2))
                  (:greyscale `(,image-width ,image-height))))))

(defun get-scanlines ()
  (loop :with data = (image-data *png-object*)
        :with scanlines = (make-array (image-height *png-object*))
        :with size = (get-scanline-bytes)
        :for line :below (length scanlines)
        :for start = (* line size)
        :for end = (min (length data) (* (1+ line) size))
        :do (setf (aref scanlines line) (list data start (- end start)))
        :finally (return scanlines)))

(defun unfilter-sub (x y scanlines pixel-bytes)
  (destructuring-bind (data start end) (aref scanlines y)
    (assert (< x end))
    (if (> x pixel-bytes)
        (aref data (+ start (- x pixel-bytes)))
        0)))

(defun unfilter-up (x y scanlines)
  (if (zerop y)
      0
      (destructuring-bind (data start end) (aref scanlines (1- y))
        (assert (< x end))
        (aref data (+ x start)))))

(defun unfilter-average (x y scanlines pixel-bytes)
  (floor (+ (unfilter-sub x y scanlines pixel-bytes)
            (unfilter-up x y scanlines))
         2))

(defun unfilter-paeth (x y scanlines pixel-bytes)
  (let* ((a (unfilter-sub x y scanlines pixel-bytes))
         (b (unfilter-up x y scanlines))
         (c (if (plusp y)
                (unfilter-sub x (1- y) scanlines pixel-bytes)
                0))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((<= pa pb pc) a)
          ((<= pb pc) b)
          (t c))))

(defun unfilter-byte (filter x y scanlines pixel-bytes)
  (case filter
    (#.+ft0-none+ 0)
    (#.+ft0-sub+ (unfilter-sub x y scanlines pixel-bytes))
    (#.+ft0-up+ (unfilter-up x y scanlines))
    (#.+ft0-average+ (unfilter-average x y scanlines pixel-bytes))
    (#.+ft0-paeth+ (unfilter-paeth x y scanlines pixel-bytes))))

(defun unfilter (scanlines)
  (loop :with pixel-bytes = (get-pixel-bytes)
        :for y :below (length scanlines)
        :for (data start len) = (aref scanlines y)
        :for filter = (aref data start)
        :do (loop :for xs :from start
                  :for x :from 0 :below len
                  :for sample = (aref data xs)
                  :for out = (unfilter-byte filter x y scanlines pixel-bytes)
                  :do (setf (aref data xs) (mod (+ sample out) 256)))))

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
  (let ((scanlines (get-scanlines)))
    (setf (image-data *png-object*) (allocate-image-data))
    (unfilter scanlines)

    ;; write test image to file
    #++(write-image)
    *png-object*))
