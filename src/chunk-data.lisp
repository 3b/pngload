(in-package :mediabox-png)

(defmacro define-chunk-data ((name) slots &body body)
  (let ((constructor (symbolicate 'make- name '-data)))
    `(progn
       (defstruct (,(symbolicate 'chunk-data- name)
                   (:constructor ,constructor)
                   (:conc-name ,(symbolicate name '-))
                   (:copier nil)
                   (:predicate nil))
         ,@slots)
       (defmethod parse (parse-data (node (eql ,(make-keyword name)))
                         &key length)
         (let ((data (,constructor)))
           (with-slots ,slots data
             (with-fast-input (@ (read-bytes (buffer parse-data) :count length))
               (declare (ignorable @))
               ,@body))
           data)))))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (read-integer @ :bytes 4)
        height (read-integer @ :bytes 4)
        bit-depth (read-integer @ :bytes 1)
        colour-type (read-integer @ :bytes 1)
        compression-method (read-integer @ :bytes 1)
        filter-method (read-integer @ :bytes 1)
        interlace-method (read-integer @ :bytes 1))
  (setf (png-colour-type (data parse-data)) (colour-type-name colour-type)))

(define-chunk-data (plte) (palette-entries)
  (let ((entry-count (/ length 3)))
    (setf palette-entries (make-array `(,entry-count 3) :element-type 'octet)
          (png-palette-count (data parse-data)) entry-count)
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref palette-entries entry sample) (read-integer @ :bytes 1))))))

(defmethod parse (parse-data (node (eql :idat)) &key length)
  ;; TODO after metadata chunks
  (seek (buffer parse-data) length))

(define-chunk-data (iend) ())

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x
                                         green-y blue-x blue-y)
  (setf white-point-x (read-integer @ :bytes 4)
        white-point-y (read-integer @ :bytes 4)
        red-x (read-integer @ :bytes 4)
        red-y (read-integer @ :bytes 4)
        green-x (read-integer @ :bytes 4)
        green-y (read-integer @ :bytes 4)
        blue-x (read-integer @ :bytes 4)
        blue-y (read-integer @ :bytes 4)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (read-integer @ :bytes 4)))

(define-chunk-data (iccp) (profile-name compression-method compressed-profile)
  (setf profile-name (read-string @ :bytes 79 :nullp t)
        compression-method (read-octet @)
        compressed-profile (read-bytes @ :count (- length (buffer-position @))
                                         :deflatep t)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (png-colour-type (data parse-data))
    (:greyscale
     (setf greyscale (read-integer @ :bytes 1)))
    ((:truecolour :indexed-colour)
     (setf red (read-integer @ :bytes 1)
           green (read-integer @ :bytes 1)
           blue (read-integer @ :bytes 1)))
    (:greyscale-alpha
     (setf greyscale (read-integer @ :bytes 1)
           alpha (read-integer @ :bytes 1)))
    (:truecolour-alpha
     (setf red (read-integer @ :bytes 1)
           green (read-integer @ :bytes 1)
           blue (read-integer @ :bytes 1)
           alpha (read-integer @ :bytes 1)))))

(define-chunk-data (bkgd) (greyscale red green blue palette-index)
  (case (png-colour-type (data parse-data))
    ((:greyscale :greyscale-alpha)
     (setf greyscale (read-integer @ :bytes 2)))
    ((:truecolour :truecolour-alpha)
     (setf red (read-integer @ :bytes 2)
           green (read-integer @ :bytes 2)
           blue (read-integer @ :bytes 2)))
    (:indexed-colour
     (setf palette-index (read-integer @ :bytes 1)))))

(define-chunk-data (hist) (frequencies)
  (let ((frequency-count (png-palette-count (data parse-data))))
    (setf frequencies (make-array frequency-count
                                  :element-type '(unsigned-byte 16)))
    (dotimes (i frequency-count)
      (setf (aref frequencies i) (read-integer @ :bytes 2)))))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (png-colour-type (data parse-data))
    (:greyscale
     (setf grey (read-integer @ :bytes 2)))
    (:truecolour
     (setf red (read-integer @ :bytes 2)
           blue (read-integer @ :bytes 2)
           green (read-integer @ :bytes 2)))
    (:indexed-colour
     (setf alpha-values (make-array length :element-type 'octet))
     (dotimes (i length)
       (setf (aref alpha-values i) (read-integer @ :bytes 1))))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (read-integer @ :bytes 4)
        pixels-per-unit-y (read-integer @ :bytes 4)
        unit-specifier (read-integer @ :bytes 1)))

(define-chunk-data (splt) (palette-name sample-depth palette-entries)
  (setf palette-name (read-string @ :bytes 79 :nullp t)
        sample-depth (read-integer @ :bytes 1))
  (let* ((entry-bytes (ecase sample-depth (8 6) (16 10)))
         (sample-bytes (/ sample-depth 8))
         (entry-count (/ (- length (buffer-position @)) entry-bytes)))
    (setf palette-entries (make-array `(,entry-count 5)
                                      :element-type '(unsigned-byte 16)))
    (dotimes (entry entry-count)
      (dotimes (sample 4)
        (setf (aref palette-entries entry sample)
              (read-integer @ :bytes sample-bytes)))
      (setf (aref palette-entries entry 4)
            (read-integer @ :bytes 2)))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (read-integer @ :bytes 2)
        month (read-integer @ :bytes 1)
        day (read-integer @ :bytes 1)
        hour (read-integer @ :bytes 1)
        minute (read-integer @ :bytes 1)
        second (read-integer @ :bytes 1)))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  (setf keyword (read-string @ :bytes 79 :nullp t)
        compression-flag (read-octet @)
        compression-method (read-octet @)
        language-tag (read-string @ :nullp t)
        translated-keyword (read-string @ :nullp t :encoding :utf-8)
        text (read-string @ :encoding :utf-8
                            :deflatep (if (= compression-flag 1) t nil))))

(define-chunk-data (text) (keyword text-string)
  (setf keyword (read-string @ :bytes 79 :nullp t)
        text-string (read-string @)))

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (setf keyword (read-string @ :bytes 79 :nullp t)
        compression-method (read-integer @ :bytes 1)
        compressed-text-datastream (read-string @ :deflatep t)))

(defmethod parse (parse-data (node (eql :exif)) &key length)
  (seek (buffer parse-data) length)
  (warn 'draft-chunk-detected :parse-data parse-data :chunk-type node))

(defmethod parse (parse-data (node (eql :unknown-chunk)) &key length)
  (seek (buffer parse-data) length)
  (warn 'unknown-chunk-detected :parse-data parse-data))
