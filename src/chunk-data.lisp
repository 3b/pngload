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
       (defmethod parse (file (node (eql ,(make-keyword name))) &key length)
         (let ((data (,constructor)))
           (with-slots ,slots data
             (with-fast-input (@ (read-octets file :count length))
               (declare (ignorable @))
               ,@body))
           data)))))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (readu32-be @)
        height (readu32-be @)
        bit-depth (readu8-be @)
        colour-type (readu8-be @)
        compression-method (readu8-be @)
        filter-method (readu8-be @)
        interlace-method (readu8-be @)))

(define-chunk-data (plte) (pallete-entries)
  (let* ((entry-count (/ length 3)))
    (setf pallete-entries (make-array `(,entry-count 3) :element-type 'octet))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref pallete-entries entry sample) (readu8-be @))))))

(define-chunk-data (bkgd) (greyscale red green blue pallete-index)
  (let* ((ihdr (find-chunk file :ihdr)))
    (case (colour-type->name ihdr)
      ((:greyscale :greyscale-alpha)
       (setf greyscale (readu16-be @)))
      ((:truecolour :truecolour-alpha)
       (setf red (readu16-be @)
             green (readu16-be @)
             blue (readu16-be @)))
      (:indexed-colour
       (readu8-be @)))))

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x
                                         green-y blue-x blue-y)
  (setf white-point-x (readu32-be @)
        white-point-y (readu32-be @)
        red-x (readu32-be @)
        red-y (readu32-be @)
        green-x (readu32-be @)
        green-y (readu32-be @)
        blue-x (readu32-be @)
        blue-y (readu32-be @)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (readu32-be @)))

(define-chunk-data (hist) (frequencies)
  (let* ((plte (find-chunk file :plte))
         (frequency-count (/ (chunk-length plte) 3)))
    (setf frequencies (make-array frequency-count
                                  :element-type '(unsigned-byte 16)))
    (dotimes (i frequency-count)
      (setf (aref frequencies i) (readu16-be @)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (readu32-be @)
        pixels-per-unit-y (readu32-be @)
        unit-specifier (readu8-be @)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (let* ((ihdr (find-chunk file :ihdr)))
    (case (colour-type->name ihdr)
      (:greyscale
       (setf greyscale (readu8-be @)))
      ((:truecolour :indexed-colour)
       (setf red (readu8-be @)
             green (readu8-be @)
             blue (readu8-be @)))
      (:greyscale-alpha
       (setf greyscale (readu8-be @)
             alpha (readu8-be @)))
      (:truecolour-alpha
       (setf red (readu8-be @)
             green (readu8-be @)
             blue (readu8-be @)
             alpha (readu8-be @))))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (readu16-be @)
        month (readu8-be @)
        day (readu8-be @)
        hour (readu8-be @)
        minute (readu8-be @)
        second (readu8-be @)))

(define-chunk-data (text) (keyword text-string)
  (let* ((octets (fast-io::input-buffer-vector @))
         (split (position 0 octets)))
    (setf keyword (read-string (subseq octets 0 split))
          text-string (read-string (subseq octets (1+ split))))))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  ;; TODO
  )

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (let* ((octets (fast-io::input-buffer-vector @))
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
  (let ((ihdr (find-chunk file :ihdr)))
    (ecase (colour-type->name ihdr)
      (:greyscale
       (setf grey (readu16-be @)))
      (:truecolour
       (setf red (readu16-be @)
             blue (readu16-be @)
             green (readu16-be @)))
      (:indexed-colour
       ;; TODO indexed colour
       ))))

(defmethod parse (file (node (eql :idat)) &key length)
  ;; TODO after metadata chunks
  (seek file length))

(define-chunk-data (iend) ())

(defmethod parse (file (node (eql :unknown-chunk)) &key length)
  ;; TODO signal condition
  (seek file length))
