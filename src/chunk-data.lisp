(in-package :pngload)

(defmacro define-chunk-data ((name &key (buffer t)) slots &body body)
  (alexandria:with-gensyms (chunk-name chunk-data )
    (let ((class-name (alexandria:symbolicate 'chunk-data- name)))
      `(progn
         (defclass ,class-name () ,slots)
         (defmethod parse-chunk-data ((,chunk-name (eql ,(alexandria:make-keyword name))))
           (let ((,chunk-data (make-instance ',class-name)))
             (with-slots ,slots ,chunk-data
               (with-source (*png-source* :end (+ (pos *png-source*)
                                                  (chunk-size))
                                          :buffer ,(cond
                                                     ((eql buffer t)
                                                      `(chunk-size))
                                                     (t ;; number or nil
                                                      buffer)))
                 ,@body))
             ,chunk-data))))))

(defgeneric parse-chunk-data (chunk-name))

(define-chunk-data (ihdr)
                   (width height bit-depth
                          colour-type compression-method filter-method
                          interlace-method)
  (setf width (ub32be)
        height (ub32be)
        bit-depth (ub8)
        colour-type (ub8)
        compression-method (ub8)
        filter-method (ub8)
        interlace-method (ub8))
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
        (setf (aref palette-entries entry sample) (ub8))))
    (setf (palette-count *png-object*) entry-count
          (palette *png-object*) palette-entries)))


(define-chunk-data (idat :buffer nil) (data)
  (if *decode-data*
      (progn
        (setf data (source-region (chunk-size)))
        (push data (data *png-object*)))
      (skip-bytes (chunk-size))))

#++
(define-chunk-data (iend) ()
  (when *decode-data*
    (loop :with data = (reverse (data *png-object*))
          :with dstate = (chipz:make-dstate 'chipz:zlib)
          :with out = (make-array (get-image-bytes) :element-type 'ub8)
          :for part :in data
          :for start = 0 :then (+ start written)
          :for (read written) = (multiple-value-list
                                 (chipz:decompress out dstate part :output-start start))
          :finally (setf (data *png-object*) out))))

(defun source->3bz-context (s)
  (etypecase s ;; shouldn't get stream-source here
    (octet-vector-source
     (3bz:make-octet-vector-context (data s)
                                    :start (pos s)
                                    :end (end s)
                                    :offset (pos s)))
    (octet-pointer-source
     (3bz:make-octet-pointer-context *mmap-pointer*
                                     :start (pos s)
                                     :end (end s)
                                     :offset (pos s)))
    (file-stream-source
     #++ ;; 3bz stream input is slow, so copy for now
     (3bz:make-octet-stream-context (data s)
                                    :start (pos s)
                                    :end (end s)
                                    :offset (pos s))
     (let ((buf (make-array (- (end s) (pos s))
                            :element-type 'ub8))
           (p (file-position (data s))))
       (file-position (data s) (pos s))
       (read-sequence buf (data s))
       (file-position (data s) p)
       (3bz:make-octet-vector-context buf)))))

(define-chunk-data (iend) ()
  (when *decode-data*
    (loop :with data = (reverse (data *png-object*))
          :with out = (make-array (get-image-bytes) :element-type 'ub8)
          :with dstate = (3bz:make-zlib-state :output-buffer out)
          :for part :in data
          :for read-context = (source->3bz-context part)
          :do (3bz:%resync-file-stream read-context)
              (3bz:decompress read-context dstate)
          :finally (progn
                     (assert (3bz:finished dstate))
                     (setf (data *png-object*) out)))))

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x green-y blue-x blue-y)
  (setf white-point-x (ub32be)
        white-point-y (ub32be)
        red-x (ub32be)
        red-y (ub32be)
        green-x (ub32be)
        green-y (ub32be)
        blue-x (ub32be)
        blue-y (ub32be)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (ub32be))
  (setf (gamma) image-gamma))

(define-chunk-data (iccp) (profile-name compression-method compressed-profile)
  (setf profile-name (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-method (ub8)
        compressed-profile (read-bytes (chunk-offset) :zlib t)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (color-type *png-object*)
    (:greyscale
     (setf greyscale (ub8)))
    ((:truecolour :indexed-colour)
     (setf red (ub8)
           green (ub8)
           blue (ub8)))
    (:greyscale-alpha
     (setf greyscale (ub8)
           alpha (ub8)))
    (:truecolour-alpha
     (setf red (ub8)
           green (ub8)
           blue (ub8)
           alpha (ub8)))))

(define-chunk-data (srgb) (rendering-intent)
  (setf rendering-intent (ub8)
        (rendering-intent) rendering-intent))

(define-chunk-data (bkgd) (greyscale red green blue palette-index)
  (case (color-type *png-object*)
    ((:greyscale :greyscale-alpha)
     (setf greyscale (ub16be)))
    ((:truecolour :truecolour-alpha)
     (setf red (ub16be)
           green (ub16be)
           blue (ub16be)))
    (:indexed-colour
     (setf palette-index (ub8)))))

(define-chunk-data (hist) (frequencies)
  (let ((count (palette-count *png-object*)))
    (setf frequencies (make-array count :element-type 'ub16))
    (dotimes (i count)
      (setf (aref frequencies i) (ub16be)))))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (color-type *png-object*)
    (:greyscale
     (setf grey (ub16be))
     (setf (transparency *png-object*) grey))
    (:truecolour
     (setf red (ub16be)
           blue (ub16be)
           green (ub16be))
     (setf (transparency *png-object*)
           (make-array 3 :element-type 'ub16 :initial-contents (list red green blue))))
    (:indexed-colour
     (let ((size (chunk-size)))
       (setf alpha-values (make-array size :element-type 'ub8))
       (dotimes (i size)
         (setf (aref alpha-values i) (ub8)))
       (setf (transparency *png-object*) alpha-values)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (ub32be)
        pixels-per-unit-y (ub32be)
        unit-specifier (ub8)
        (pixel-size) (list pixels-per-unit-x pixels-per-unit-y unit-specifier)))

(define-chunk-data (splt) (palette-name sample-depth palette-entries)
  (setf palette-name (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        sample-depth (ub8))
  (let* ((entry-bytes (ecase sample-depth (8 6) (16 10)))
         (sample-bytes (/ sample-depth 8))
         (entry-count (/ (chunk-offset)
                         entry-bytes)))
    (setf palette-entries (make-array `(,entry-count 5) :element-type 'ub16))
    (dotimes (entry entry-count)
      (dotimes (sample 4)
        (setf (aref palette-entries entry sample)
              (ecase sample-bytes
                (1 (ub8))
                (2 (ub16be))
                (4 (ub32be)))))
      (setf (aref palette-entries entry 4) (ub16be)))))

(define-chunk-data (time) (year month day hour minute second)
  (setf year (ub16be)
        month (ub8)
        day (ub8)
        hour (ub8)
        minute (ub8)
        second (ub8)
        (last-modified) (list year month day hour minute second)))

(define-chunk-data (itxt) (keyword compression-flag compression-method language-tag
                                   translated-keyword text)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-flag (ub8)
        compression-method (ub8)
        language-tag (read-string :encoding :latin-1 :null-terminated-p t)
        translated-keyword (read-string :encoding :utf-8 :null-terminated-p t))
  (if (= compression-flag 1)
      (setf text (read-string :encoding :utf-8 :zlib t))
      (setf text (read-string :encoding :utf-8)))
  (setf (text) (list keyword text language-tag translated-keyword)))

(define-chunk-data (text) (keyword text-string)
  (setf keyword (read-string :bytes 80 :encoding :latin-1 :null-terminated-p t)
        text-string (read-string :encoding :latin-1)
        (text) (list keyword text-string)))

(define-chunk-data (ztxt) (keyword compression-method compressed-text-datastream)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-method (ub8)
        compressed-text-datastream (read-string :encoding :latin-1
                                                :zlib t)
        (text) (list keyword compressed-text-datastream)))

(define-chunk-data (unknown) ()
  (warn 'unknown-chunk-detected))

