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
       (defmethod parse (parse-data (node (eql ,(make-keyword name))) &key length)
         (let ((data (,constructor)))
           (with-slots ,slots data
             (with-fast-input (@ (buffer-data (buffer parse-data) :bytes length))
               (declare (ignorable @))
               ,@body))
           data)))))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (read-bytes 4 @)
        height (read-bytes 4 @)
        bit-depth (read-bytes 1 @)
        colour-type (read-bytes 1 @)
        compression-method (read-bytes 1 @)
        filter-method (read-bytes 1 @)
        interlace-method (read-bytes 1 @))
  (setf (png-colour-type (data parse-data)) (colour-type-name colour-type)))

(define-chunk-data (plte) (pallete-entries)
  (let* ((entry-count (/ length 3)))
    (setf pallete-entries (make-array `(,entry-count 3) :element-type 'octet)
          (png-pallete-count (data parse-data)) (/ length 3))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref pallete-entries entry sample) (read-bytes 1 @))))))

(define-chunk-data (bkgd) (greyscale red green blue pallete-index)
  (case (png-colour-type (data parse-data))
    ((:greyscale :greyscale-alpha)
     (setf greyscale (read-bytes 2 @)))
    ((:truecolour :truecolour-alpha)
     (setf red (read-bytes 2 @)
           green (read-bytes 2 @)
           blue (read-bytes 2 @)))
    (:indexed-colour
     (read-bytes 1 @))))

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x
                                         green-y blue-x blue-y)
  (setf white-point-x (read-bytes 4 @)
        white-point-y (read-bytes 4 @)
        red-x (read-bytes 4 @)
        red-y (read-bytes 4 @)
        green-x (read-bytes 4 @)
        green-y (read-bytes 4 @)
        blue-x (read-bytes 4 @)
        blue-y (read-bytes 4 @)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (read-bytes 4 @)))

(define-chunk-data (hist) (frequencies)
  (let ((frequency-count (png-pallete-count (data parse-data))))
    (setf frequencies (make-array frequency-count
                                  :element-type '(unsigned-byte 16)))
    (dotimes (i frequency-count)
      (setf (aref frequencies i) (read-bytes 2 @)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (read-bytes 4 @)
        pixels-per-unit-y (read-bytes 4 @)
        unit-specifier (read-bytes 1 @)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (png-colour-type (data parse-data))
    (:greyscale
     (setf greyscale (read-bytes 1 @)))
    ((:truecolour :indexed-colour)
     (setf red (read-bytes 1 @)
           green (read-bytes 1 @)
           blue (read-bytes 1 @)))
    (:greyscale-alpha
     (setf greyscale (read-bytes 1 @)
           alpha (read-bytes 1 @)))
    (:truecolour-alpha
     (setf red (read-bytes 1 @)
           green (read-bytes 1 @)
           blue (read-bytes 1 @)
           alpha (read-bytes 1 @)))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (read-bytes 2 @)
        month (read-bytes 1 @)
        day (read-bytes 1 @)
        hour (read-bytes 1 @)
        minute (read-bytes 1 @)
        second (read-bytes 1 @)))

(define-chunk-data (text) (keyword text-string)
  (let* ((octets (get-vector @))
         (split (position 0 octets)))
    (setf keyword (read-string (subseq octets 0 split))
          text-string (read-string (subseq octets (1+ split))))))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  ;; TODO
  )

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (let* ((octets (get-vector @))
         (split (position 0 octets)))
    (setf keyword (read-string (subseq octets 0 split))
          compression-method (elt octets (1+ split))
          compressed-text-datastream (read-string
                                      (chipz:decompress
                                       nil
                                       :zlib (subseq octets (+ split 2)))))))

(define-chunk-data (splt) (pallete-name sample-depth pallete-entries)
  (let* ((octets (fast-io::input-buffer-vector @))
         (split (position 0 octets))
         (rest-bytes (- length (+ split 2))))
    (setf pallete-name (read-string (subseq octets 0 split))
          sample-depth (elt octets (1+ split)))
    ;; TODO make pallete entries
    #++(make-entries rest-bytes sample-depth)))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (png-colour-type (data parse-data))
    (:greyscale
     (setf grey (read-bytes 2 @)))
    (:truecolour
     (setf red (read-bytes 2 @)
           blue (read-bytes 2 @)
           green (read-bytes 2 @)))
    (:indexed-colour
     ;; TODO indexed colour
     )))

(defmethod parse (parse-data (node (eql :idat)) &key length)
  ;; TODO after metadata chunks
  (seek (buffer parse-data) length))

(define-chunk-data (iend) ())

(defmethod parse (parse-data (node (eql :unknown-chunk)) &key length)
  (seek (buffer parse-data) length)
  (warn 'unknown-chunk-detected :parse-data parse-data))
