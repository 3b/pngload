(in-package #:pngload)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +filter-type-none+ 0)
  (alexandria:define-constant +filter-type-sub+ 1)
  (alexandria:define-constant +filter-type-up+ 2)
  (alexandria:define-constant +filter-type-average+ 3)
  (alexandria:define-constant +filter-type-paeth+ 4))

(defmacro %row-major-aref (array index)
  `(row-major-aref ,array (the fixnum ,index)))

(defun get-image-bytes (png)
  (let ((width (width png))
        (height (height png)))
    (ecase (interlace-method png)
      (:null
       (+ height (* height (get-scanline-bytes png width))))
      (:adam7
       (loop :for (width height) :in (calculate-sub-image-dimensions png)
             :sum (* height (1+ (get-scanline-bytes png width))))))))

(defun get-image-raw-channels (png)
  (ecase (color-type png)
    ((:truecolour :indexed-colour) 3)
    (:truecolour-alpha 4)
    (:greyscale-alpha 2)
    (:greyscale 1)))

(defun get-image-channels (png)
  (let ((channels (get-image-raw-channels png)))
    (when (transparency png)
      (assert (member (color-type png)
                      '(:truecolour :indexed-colour :greyscale)))
      (incf channels))
    channels))

(defun allocate-image-data (png)
  (let* ((width (width png))
         (height (height png))
         (channels (get-image-channels png))
         (args (list (if (state-flatten (state png))
                         (* width height channels)
                         `(,height ,width ,@(when (> channels 1)
                                              (list channels))))
                     :element-type (ecase (bit-depth png)
                                     ((1 2 4 8) 'ub8)
                                     (16 'ub16))))
         (use-static-vector (state-use-static-vector (state png))))
    (when use-static-vector
      (assert (state-flatten (state png))))
    #-(or clisp abcl)
    (if use-static-vector
        (apply #'static-vectors:make-static-vector args)
        (apply #'make-array args))
    #+ (or clisp abcl)
    (apply #'make-array args)))

;;; Following the PNG sW3 spec, the pixels considered when performing filter
;;; Are described in this diagram:
;;;   +-------+
;;;   | c | b |
;;;   +---+---+
;;;   | a | x |
;;;   +---+---+
;;; Where x is the 'current' pixel

(defun unfilter-row-sub (y data row-start row-bytes pixel-bytes)
  (declare (type ub8a1d data)
           (type (and fixnum (integer 0)) y row-start)
           (type (and fixnum (integer 1)) row-bytes pixel-bytes)
           (ignore y)
           (optimize speed))
  (loop :for x :from (+ row-start pixel-bytes)
          :below (+ row-start (1- row-bytes))
        :for a fixnum = (- x pixel-bytes)
        :do (setf (aref data x)
                  (ldb (byte 8 0) (+ (aref data x) (aref data a))))))

(defun unfilter-row-up (y data row-start row-bytes pixel-bytes)
  (declare (type ub8a1d data)
           (type (and fixnum (integer 0)) y row-start)
           (type (and fixnum (integer 1)) row-bytes  pixel-bytes)
           (ignore pixel-bytes)
           (optimize speed))
  (when (plusp y)
    (loop :for x :from row-start :below (+ row-start (1- row-bytes))
          :for b = (- x row-bytes)
          :do (setf (aref data x)
                    (ldb (byte 8 0) (+ (aref data x) (aref data b)))))))

(defun unfilter-row-average (y data row-start row-bytes pixel-bytes)
  (declare (type ub8a1d data)
           (type (and fixnum (integer 0)) y row-start)
           (type (and fixnum (integer 1)) row-bytes  pixel-bytes)
           (optimize speed))
  (loop :for x fixnum :from row-start :below (+ row-start (1- row-bytes))
        :for a fixnum = (- x pixel-bytes)
        :for b fixnum = (- x row-bytes)
        :do (setf (aref data x)
                  (ldb (byte 8 0)
                       (+ (aref data x)
                          (floor (+ (if (>= a row-start) (aref data a) 0)
                                    (if (plusp y) (aref data b) 0))
                                 2))))))

(defun unfilter-row-paeth (y data row-start row-bytes pixel-bytes)
  (declare (type ub8a1d data)
           (type (and fixnum (integer 0)) y row-start)
           (type (and fixnum (integer 1)) row-bytes pixel-bytes)
           (optimize speed (safety 0)))
  (if (zerop y)
      ;; paeth on the first row is equivalent to a sub
      (unfilter-row-sub y data row-start row-bytes pixel-bytes)
      (let ((tmp1 (+ row-start pixel-bytes)))
        (declare (fixnum tmp1))
        ;; Handle the first column specifically so we don't have to worry
        ;; about it later
        (loop :for x fixnum :from row-start :below tmp1
              :do (setf (aref data x)
                        (ldb (byte 8 0)
                             (+ (aref data x) (aref data (- x row-bytes))))))
        (loop :for x fixnum :from tmp1 :below (+ row-start (1- row-bytes))
              :do (let* ((a (- x pixel-bytes))
                         (b (- x row-bytes))
                         (c (- b pixel-bytes))
                         (av (aref data a))
                         (bv (aref data b))
                         (cv (aref data c))
                         (p (- (+ av bv) cv))
                         (pa (abs (- p av)))
                         (pb (abs (- p bv)))
                         (pc (abs (- p cv)))
                         (tmp2 (cond
                                 ((and (<= pa pb) (<= pa pc)) av)
                                 ((<= pb pc) bv)
                                 (t cv))))
                    (declare (ub8 a b c av bv cv p pa pb pc))
                    (setf (aref data x)
                          (ldb (byte 8 0) (+ (aref data x) tmp2))))))))

(defun unfilter (png data width height start)
  (declare (ub32 width height)
           (fixnum start)
           (ub8a1d data))
  (loop :with pixel-bytes = (get-pixel-bytes png)
        :with scanline-bytes fixnum = (get-scanline-bytes png width)
        :with row-bytes = (1+ scanline-bytes)
        :for y fixnum :below height
        :for in-start fixnum :from start :by row-bytes
        :for row-start fixnum :from (1+ start) :by row-bytes
        :do (ecase (aref data in-start)
              (#.+filter-type-paeth+
               (unfilter-row-paeth y data row-start row-bytes pixel-bytes))
              (#.+filter-type-average+
               (unfilter-row-average y data row-start row-bytes pixel-bytes))
              (#.+filter-type-sub+
               (unfilter-row-sub y data row-start row-bytes pixel-bytes))
              (#.+filter-type-up+
               (unfilter-row-up y data row-start row-bytes pixel-bytes))
              (#.+filter-type-none+ ; nothing tbd
               nil))
        :finally
           ;; Now compact row data by removing the filter bytes
           (loop :for y :below height
                 :for dst-data :from start :by scanline-bytes
                 :for row-start fixnum :from (1+ start) :by row-bytes
                 :do (replace data
                              data
                              :start1 dst-data
                              :start2 row-start
                              :end2 (+ row-start scanline-bytes)))))

(defmacro maybe-flatten (png dims bit-depth)
  (let ((nd-fn-sym (intern (format nil "COPY/~dD/~d" dims bit-depth)))
        (1d-fn-sym (intern (format nil "COPY/1D/~d" bit-depth)))
        (copy-fn-sym (intern (format nil "COPY/~d" bit-depth)))
        (copy-flip-fn-sym (intern (format nil "COPY/~d/FLIP" bit-depth)))
        (nd-type-sym (intern (format nil "UB~dA~dD" bit-depth dims)))
        (1d-type-sym (intern (format nil "UB~dA1D" bit-depth))))
    ;; TODO: Figure out why there are two identical functions here. ~axion
    ;; 4/27/2020
    `(flet ((,nd-fn-sym ()
              (declare (,nd-type-sym data))
              (if (state-flip-y (state ,png))
                  (,copy-flip-fn-sym ,png)
                  (,copy-fn-sym ,png)))
            (,1d-fn-sym ()
              (declare (,1d-type-sym data))
              (if (state-flip-y (state ,png))
                  (,copy-flip-fn-sym ,png)
                  (,copy-fn-sym ,png))))
       (if (state-flatten (state ,png))
           (,1d-fn-sym)
           (,nd-fn-sym)))))

(defmacro copy/8 (png)
  (declare (ignore png))
  `(loop :for d fixnum :below (array-total-size data)
         :for s fixnum :below (array-total-size image-data)
         :do (locally (declare (optimize speed (safety 0)))
               (setf (%row-major-aref data d)
                     (aref image-data s)))))

(defmacro copy/8/flip (png)
  `(let* ((width (width ,png))
          (height (height ,png))
          (channels (get-image-raw-channels ,png))
          (stride (* channels width))
          (ssize (array-total-size image-data))
          (dsize (array-total-size data)))
     (declare (fixnum ssize dsize)
              (type (unsigned-byte 34) stride))
     (loop :for dy :below height
           :for sy :downfrom (1- height)
           :for d1 = (* dy stride)
           :for s1 = (* sy stride)
           :do (assert (<= 0 (+ d1 stride) dsize))
               (assert (<= 0 (+ s1 stride) ssize))
               (locally (declare (optimize speed))
                 (loop :for s fixnum :from s1 :below ssize
                       :for d fixnum :from d1 :below dsize
                       :repeat stride
                       :do (locally (declare (optimize speed (safety 0)))
                             (setf (%row-major-aref data d)
                                   (aref image-data s))))))))

(defmacro copy/16 (png)
  (declare (ignore png))
  `(progn
     (assert (zerop (mod (array-total-size image-data) 2)))
     (loop :for d :below (array-total-size data)
           :for s :below (array-total-size image-data) :by 2
           :do (locally (declare (optimize speed (safety 0)))
                 (setf (%row-major-aref data d)
                       (dpb (aref image-data s) (byte 8 8)
                            (aref image-data (1+ s))))))))

(defmacro copy/16/flip (png)
  `(let* ((width (width ,png))
          (height (height ,png))
          (channels (get-image-raw-channels ,png))
          (stride (* channels width))
          (ssize (array-total-size image-data))
          (dsize (array-total-size data)))
     (declare (fixnum ssize dsize)
              (type (unsigned-byte 34) stride))
     (loop :for dy :below height
           :for sy :downfrom (1- height)
           :for d1 = (* dy stride)
           :for s1 = (* sy stride 2)
           :do (assert (<= 0 (+ d1 stride) dsize))
               (assert (<= 0 (+ s1 stride stride) ssize))
               (locally (declare (optimize speed))
                 (loop :for s fixnum :from s1 :below ssize :by 2
                       :for d fixnum :from d1 :below dsize
                       :repeat stride
                       :do (locally (declare (optimize speed (safety 0)))
                             (setf (%row-major-aref data d)
                                   (dpb (aref image-data s) (byte 8 8)
                                        (aref image-data (1+ s))))))))))

(defun copy/pal/8 (png image-data)
  (let ((data (data png))
        (palette (palette png))
        (transparency (transparency png)))
    (macrolet ((copy ()
                 `(loop :with c = (get-image-channels png)
                        :for d :below (array-total-size data) :by c
                        :for s :across image-data
                        :do  (setf (%row-major-aref data (+ d 0))
                                   (aref palette s 0)
                                   (%row-major-aref data (+ d 1))
                                   (aref palette s 1)
                                   (%row-major-aref data (+ d 2))
                                   (aref palette s 2))
                             (when transparency
                               (setf (%row-major-aref data (+ d 3))
                                     (if (array-in-bounds-p transparency s)
                                         (aref transparency s)
                                         255))))))
      (if (state-flatten (state png))
          (locally (declare (ub8a1d data)) (copy))
          (locally (declare (ub8a3d data)) (copy))))))

(defun copy/pal/sub (png image-data)
  (loop :with width = (width png)
        :with bit-depth = (bit-depth png)
        :with palette = (palette png)
        :with transparency = (transparency png)
        :with scanline-bytes = (get-scanline-bytes png width)
        :with pixels-per-byte = (/ 8 bit-depth)
        :with channels = (get-image-channels png)
        :with dstride = (* width channels)
        :for y :below (height png)
        :for yb = (* y scanline-bytes)
        :do (flet (((setf %data) (v y x c)
                     (setf (%row-major-aref
                            (data png)
                            (+ (* y dstride) (* x channels) c))
                           v)))
              (loop :for x :below width
                    :do (multiple-value-bind (b p) (floor x pixels-per-byte)
                          (let ((i (ldb (byte bit-depth
                                              (- 8 (* p bit-depth) bit-depth))
                                        (aref image-data (+ yb b)))))
                            (setf (%data y x 0) (aref palette i 0)
                                  (%data y x 1) (aref palette i 1)
                                  (%data y x 2) (aref palette i 2))
                            (when transparency
                              (setf (%data y x 3)
                                    (if (array-in-bounds-p transparency i)
                                        (aref transparency i)
                                        255)))))))))

(defun copy/2d/sub (png image-data)
  (loop :with width = (width png)
        :with bit-depth = (bit-depth png)
        :with data :of-type ub8a = (data png)
        :with s = 0
        :with x = 0
        :with bx = 0
        :with p = 0
        :with b = 0
        :with scanline-bytes = (get-scanline-bytes png width)
        :with ssize = (array-total-size image-data)
        :for d :below (array-total-size data)
        :while (< (+ s bx) ssize)
        :when (zerop p)
          :do (setf b (aref image-data (+ s bx)))
        :do (setf (%row-major-aref data d)
                  (ldb (byte bit-depth (- 8 p bit-depth)) b))
            (incf p bit-depth)
            (incf x)
            (cond
              ((>= x width)
               (setf x 0
                     bx 0
                     p 0)
               (incf s scanline-bytes))
              ((>= p 8)
               (setf p 0)
               (incf bx 1)))))

(defmacro trns (png opaque)
  `(loop :with c = (get-image-channels ,png)
         :with key = (etypecase transparency
                       (ub16 (make-array 1 :element-type 'ub16
                                           :initial-element transparency))
                       (ub16a1d transparency))
         :for s :from (- (* width height (1- c)) (1- c)) :downto 0 :by (1- c)
         :for d :from (- (array-total-size data) c) :downto 0 :by c
         :do (loop :for i :below (1- c)
                   :for k :across key
                   :for v = (%row-major-aref data (+ s i))
                   :do (setf (%row-major-aref data (+ d i)) v)
                   :count (= v k) :into matches
                   ;; collect (list v k matches) :into foo
                   :finally (setf (%row-major-aref data (+ d (1- c)))
                                  (if (= matches (1- c)) 0 ,opaque)))))

(defun flip (png image)
  (let ((w (width png))
        (h (height png))
        (c (get-image-channels png)))
    (let ((stride (* w c))
          (end (array-total-size image)))
      (assert (plusp stride))
      (macrolet ((f (&key (opt t))
                   `(loop :for y1 :below (floor h 2)
                          :for y2 :downfrom (1- h) :above 0
                          :do (loop :for x1 :from (* y1 stride) :below end
                                    :for x2 :from (* y2 stride) :below end
                                    :repeat stride
                                    :do (,@(if opt
                                               '(locally
                                                 (declare
                                                  (optimize speed (safety 0))))
                                               '(progn))
                                         (rotatef (%row-major-aref image x1)
                                                  (%row-major-aref
                                                   image x2)))))))
        (typecase image
          (ub8a3d (f))
          (ub8a2d (f))
          (ub8a1d (f))
          (ub16a3d (f))
          (ub16a2d (f))
          (ub16a1d (f))
          (t (f :opt nil)))))))

(declaim (inline maybe-flip))
(defun maybe-flip (png data)
  (when (state-flip-y (state png))
    (flip png data)))

(defun decode (png)
  (let ((image-data (data png))
        (width (width png))
        (height (height png))
        (bit-depth (bit-depth png))
        (transparency (transparency png)))
    (declare (ub8a1d image-data))
    (if (eq (interlace-method png) :null)
        (unfilter png image-data width height 0)
        (setf image-data (deinterlace-adam7 png image-data)))
    (assert (and (typep bit-depth 'ub8)
                 (member bit-depth '(1 2 4 8 16))))
    (setf (data png) (allocate-image-data png))
    (let ((data (data png)))
      (ecase (color-type png)
        ((:truecolour :truecolour-alpha :greyscale-alpha)
         (ecase bit-depth
           (8 (maybe-flatten png 3 8))
           (16 (maybe-flatten png 3 16)))
         (when transparency
           (ecase bit-depth
             (8 (trns png #xff))
             (16 (trns png #xffff)))))
        (:greyscale
         (if transparency
             (ecase bit-depth
               (8 (maybe-flatten png 3 8) (trns png #xff))
               (16 (maybe-flatten png 3 16) (trns png #xffff))
               ((1 2 4)
                (copy/2d/sub png image-data)
                (trns png #xff)
                (maybe-flip png data)))
             (ecase bit-depth
               (8 (maybe-flatten png 2 8))
               (16 (maybe-flatten png 2 16))
               ((1 2 4)
                (copy/2d/sub png image-data)
                (maybe-flip png data)))))
        (:indexed-colour
         (ecase bit-depth
           (8 (copy/pal/8 png image-data))
           ((1 2 4) (copy/pal/sub png image-data)))
         (maybe-flip png data)))))
  png)
