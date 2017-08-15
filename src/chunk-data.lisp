(in-package :pngload)

(defmacro define-chunk-data ((name) slots &body body)
  (with-gensyms (chunk-name chunk-data)
    (let ((class-name (symbolicate 'chunk-data- name)))
      `(progn
         (defclass ,class-name ()
           ,(loop :for slot :in slots :collect slot))
         (defmethod parse-chunk-data ((,chunk-name (eql ,(make-keyword name))))
           (let ((,chunk-data (make-instance ',class-name)))
             (with-slots ,slots ,chunk-data
               (with-fast-input (*buffer* (read-bytes (chunk-size)))
                 ,@body))
             ,chunk-data))))))

(defgeneric parse-chunk-data (chunk-name))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (read-integer :bytes 4)
        height (read-integer :bytes 4)
        bit-depth (read-integer :bytes 1)
        colour-type (read-integer :bytes 1)
        compression-method (read-integer :bytes 1)
        filter-method (read-integer :bytes 1)
        interlace-method (read-integer :bytes 1))
  (setf (width *png-object*) width
        (height *png-object*) height
        (bit-depth *png-object*) bit-depth
        (color-type *png-object*) (colour-type-name colour-type)
        (interlace-method *png-object*) (interlace-method-name interlace-method)))

(define-chunk-data (plte) (palette-entries)
  (let ((entry-count (/ (chunk-size) 3)))
    (setf palette-entries (make-array `(,entry-count 3) :element-type 'ub8))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref palette-entries entry sample) (read-integer :bytes 1))))
    (setf (palette-count *png-object*) entry-count
          (palette *png-object*) palette-entries)))

(define-chunk-data (idat) (data)
  (when *decode-data*
    (setf data (read-bytes (chunk-size)))
    (push data (data *png-object*))))

(define-chunk-data (iend) ()
  (when *decode-data*
    (loop :with data = (reverse (data *png-object*))
          :with dstate = (chipz:make-dstate 'chipz:zlib)
          :with out = (make-array (get-image-bytes) :element-type 'ub8)
          :for part :in data
          :for start = 0 :then (+ start written)
          :for (read written) = (multiple-value-list
                                 (chipz:decompress out dstate part
                                                   :output-start start))
          :finally (setf (data *png-object*) out))))

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x
                                         green-y blue-x blue-y)
  (setf white-point-x (read-integer :bytes 4)
        white-point-y (read-integer :bytes 4)
        red-x (read-integer :bytes 4)
        red-y (read-integer :bytes 4)
        green-x (read-integer :bytes 4)
        green-y (read-integer :bytes 4)
        blue-x (read-integer :bytes 4)
        blue-y (read-integer :bytes 4)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (read-integer :bytes 4)))

(define-chunk-data (iccp) (profile-name compression-method compressed-profile)
  (setf profile-name (read-string :bytes 79 :nullp t)
        compression-method (read-integer :bytes 1)
        compressed-profile (read-bytes (chunk-offset) :deflatep t)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (color-type *png-object*)
    (:greyscale
     (setf greyscale (read-integer :bytes 1)))
    ((:truecolour :indexed-colour)
     (setf red (read-integer :bytes 1)
           green (read-integer :bytes 1)
           blue (read-integer :bytes 1)))
    (:greyscale-alpha
     (setf greyscale (read-integer :bytes 1)
           alpha (read-integer :bytes 1)))
    (:truecolour-alpha
     (setf red (read-integer :bytes 1)
           green (read-integer :bytes 1)
           blue (read-integer :bytes 1)
           alpha (read-integer :bytes 1)))))

(define-chunk-data (srgb) (rendering-intent)
  (setf rendering-intent (read-integer :bytes 1)))

(define-chunk-data (bkgd) (greyscale red green blue palette-index)
  (case (color-type *png-object*)
    ((:greyscale :greyscale-alpha)
     (setf greyscale (read-integer :bytes 2)))
    ((:truecolour :truecolour-alpha)
     (setf red (read-integer :bytes 2)
           green (read-integer :bytes 2)
           blue (read-integer :bytes 2)))
    (:indexed-colour
     (setf palette-index (read-integer :bytes 1)))))

(define-chunk-data (hist) (frequencies)
  (let ((count (palette-count *png-object*)))
    (setf frequencies (make-array count :element-type 'ub16))
    (dotimes (i count)
      (setf (aref frequencies i) (read-integer :bytes 2)))))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (color-type *png-object*)
    (:greyscale
     (setf grey (read-integer :bytes 2))
     (setf (transparency *png-object*) grey))
    (:truecolour
     (setf red (read-integer :bytes 2)
           blue (read-integer :bytes 2)
           green (read-integer :bytes 2))
     (setf (transparency *png-object*)
           (make-array 3 :element-type 'ub16
                         :initial-contents (list red green blue))))
    (:indexed-colour
     (let ((size (chunk-size)))
       (setf alpha-values (make-array size :element-type 'ub8))
       (dotimes (i size)
         (setf (aref alpha-values i) (read-integer :bytes 1)))
       (setf (transparency *png-object*) alpha-values)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (read-integer :bytes 4)
        pixels-per-unit-y (read-integer :bytes 4)
        unit-specifier (read-integer :bytes 1)))

(define-chunk-data (splt) (palette-name sample-depth palette-entries)
  (setf palette-name (read-string :bytes 79 :nullp t)
        sample-depth (read-integer :bytes 1))
  (let* ((entry-bytes (ecase sample-depth (8 6) (16 10)))
         (sample-bytes (/ sample-depth 8))
         (entry-count (/ (chunk-offset) entry-bytes)))
    (setf palette-entries (make-array `(,entry-count 5) :element-type 'ub16))
    (dotimes (entry entry-count)
      (dotimes (sample 4)
        (setf (aref palette-entries entry sample)
              (read-integer :bytes sample-bytes)))
      (setf (aref palette-entries entry 4)
            (read-integer :bytes 2)))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (read-integer :bytes 2)
        month (read-integer :bytes 1)
        day (read-integer :bytes 1)
        hour (read-integer :bytes 1)
        minute (read-integer :bytes 1)
        second (read-integer :bytes 1)))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  (setf keyword (read-string :bytes 79 :nullp t)
        compression-flag (read-integer :bytes 1)
        compression-method (read-integer :bytes 1)
        language-tag (read-string :nullp t)
        translated-keyword (read-string :nullp t :encoding :utf-8)
        text (read-string :encoding :utf-8
                          :deflatep (when (= compression-flag 1) t))))

(define-chunk-data (text) (keyword text-string)
  (setf keyword (read-string :bytes 79 :nullp t)
        text-string (read-string)))

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (setf keyword (read-string :bytes 79 :nullp t)
        compression-method (read-integer :bytes 1)
        compressed-text-datastream (read-string :deflatep t)))

(define-chunk-data (exif) ()
  (warn 'draft-chunk-detected :chunk-type :exif))

(define-chunk-data (unknown) ()
  (warn 'unknown-chunk-detected))
