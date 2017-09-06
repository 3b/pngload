(in-package :pngload)

(defmacro define-chunk-data ((name) slots &body body)
  (with-gensyms (chunk-name chunk-data)
    (let ((class-name (symbolicate 'chunk-data- name)))
      `(progn
         (defclass ,class-name () ,slots)
         (defmethod parse-chunk-data ((,chunk-name (eql ,(make-keyword name))))
           (let ((,chunk-data (make-instance ',class-name)))
             (with-slots ,slots ,chunk-data
               (with-fast-input (*byte-buffer* (read-bytes (chunk-size)))
                 ,@body))
             ,chunk-data))))))

(defgeneric parse-chunk-data (chunk-name))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (read-uint-be 4)
        height (read-uint-be 4)
        bit-depth (read-uint-be 1)
        colour-type (read-uint-be 1)
        compression-method (read-uint-be 1)
        filter-method (read-uint-be 1)
        interlace-method (read-uint-be 1))
  (setf (width *png-object*) width
        (height *png-object*) height
        (bit-depth *png-object*) bit-depth
        (color-type) colour-type
        (compression-method) compression-method
        (interlace-method) interlace-method
        (filter-method) filter-method))

(define-chunk-data (plte) (palette-entries)
  (let ((entry-count (/ (chunk-size) 3)))
    (setf palette-entries (make-array `(,entry-count 3) :element-type 'ub8))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref palette-entries entry sample) (read-uint-be 1))))
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
  (setf white-point-x (read-uint-be 4)
        white-point-y (read-uint-be 4)
        red-x (read-uint-be 4)
        red-y (read-uint-be 4)
        green-x (read-uint-be 4)
        green-y (read-uint-be 4)
        blue-x (read-uint-be 4)
        blue-y (read-uint-be 4)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (read-uint-be 4))
  (setf (gamma) image-gamma))

(define-chunk-data (iccp) (profile-name compression-method compressed-profile)
  (setf profile-name (read-string :bytes 79 :encoding :latin-1
                                  :null-terminated t)
        compression-method (read-uint-be 1)
        compressed-profile (read-bytes (chunk-offset)
                                       :processor #'uncompress-zlib)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (color-type *png-object*)
    (:greyscale
     (setf greyscale (read-uint-be 1)))
    ((:truecolour :indexed-colour)
     (setf red (read-uint-be 1)
           green (read-uint-be 1)
           blue (read-uint-be 1)))
    (:greyscale-alpha
     (setf greyscale (read-uint-be 1)
           alpha (read-uint-be 1)))
    (:truecolour-alpha
     (setf red (read-uint-be 1)
           green (read-uint-be 1)
           blue (read-uint-be 1)
           alpha (read-uint-be 1)))))

(define-chunk-data (srgb) (rendering-intent)
  (setf rendering-intent (read-uint-be 1)
        (rendering-intent) rendering-intent))

(define-chunk-data (bkgd) (greyscale red green blue palette-index)
  (case (color-type *png-object*)
    ((:greyscale :greyscale-alpha)
     (setf greyscale (read-uint-be 2)))
    ((:truecolour :truecolour-alpha)
     (setf red (read-uint-be 2)
           green (read-uint-be 2)
           blue (read-uint-be 2)))
    (:indexed-colour
     (setf palette-index (read-uint-be 1)))))

(define-chunk-data (hist) (frequencies)
  (let ((count (palette-count *png-object*)))
    (setf frequencies (make-array count :element-type 'ub16))
    (dotimes (i count)
      (setf (aref frequencies i) (read-uint-be 2)))))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (color-type *png-object*)
    (:greyscale
     (setf grey (read-uint-be 2))
     (setf (transparency *png-object*) grey))
    (:truecolour
     (setf red (read-uint-be 2)
           blue (read-uint-be 2)
           green (read-uint-be 2))
     (setf (transparency *png-object*)
           (make-array 3 :element-type 'ub16
                         :initial-contents (list red green blue))))
    (:indexed-colour
     (let ((size (chunk-size)))
       (setf alpha-values (make-array size :element-type 'ub8))
       (dotimes (i size)
         (setf (aref alpha-values i) (read-uint-be 1)))
       (setf (transparency *png-object*) alpha-values)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (read-uint-be 4)
        pixels-per-unit-y (read-uint-be 4)
        unit-specifier (read-uint-be 1)
        (pixel-size) (list pixels-per-unit-x pixels-per-unit-y unit-specifier)))

(define-chunk-data (splt) (palette-name sample-depth palette-entries)
  (setf palette-name (read-string :bytes 79 :encoding :latin-1
                                  :null-terminated t)
        sample-depth (read-uint-be 1))
  (let* ((entry-bytes (ecase sample-depth (8 6) (16 10)))
         (sample-bytes (/ sample-depth 8))
         (entry-count (/ (chunk-offset) entry-bytes)))
    (setf palette-entries (make-array `(,entry-count 5) :element-type 'ub16))
    (dotimes (entry entry-count)
      (dotimes (sample 4)
        (setf (aref palette-entries entry sample)
              (read-uint-be sample-bytes)))
      (setf (aref palette-entries entry 4)
            (read-uint-be 2)))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (read-uint-be 2)
        month (read-uint-be 1)
        day (read-uint-be 1)
        hour (read-uint-be 1)
        minute (read-uint-be 1)
        second (read-uint-be 1)
        (last-modified) (list year month day hour minute second)))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated t)
        compression-flag (read-uint-be 1)
        compression-method (read-uint-be 1)
        language-tag (read-string :encoding :latin-1 :null-terminated t)
        translated-keyword (read-string :encoding :utf-8 :null-terminated t))
  (if (= compression-flag 1)
      (setf text (read-string :encoding :utf-8 :processor #'uncompress-zlib))
      (setf text (read-string :encoding :utf-8)))
  (setf (text) (list keyword text language-tag translated-keyword)))

(define-chunk-data (text) (keyword text-string)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated t)
        text-string (read-string :encoding :latin-1)
        (text) (list keyword text-string)))

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated t)
        compression-method (read-uint-be 1)
        compressed-text-datastream (read-string :encoding :latin-1
                                                :processor #'uncompress-zlib)
        (text) (list keyword compressed-text-datastream)))

(define-chunk-data (unknown) ()
  (warn 'unknown-chunk-detected))
