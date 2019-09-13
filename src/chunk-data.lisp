(in-package #:pngload)

(defmacro define-chunk-data ((type &key (buffer t)) slots &body body)
  (alexandria:with-gensyms (chunk chunk-type chunk-data)
    (let ((struct-name (alexandria:symbolicate 'chunk-data- type)))
      `(progn
         (defstruct ,struct-name ,@slots)
         (defmethod parse-chunk-data
             ((,chunk-type (eql ,(alexandria:make-keyword type))) ,chunk)
           (let ((,type ,chunk)
                 (,chunk-data (,(alexandria:symbolicate '#:make- struct-name))))
             (symbol-macrolet ,(mapcar
                                (lambda (x)
                                  (list x
                                        `(,(alexandria:symbolicate
                                            struct-name '#:- x)
                                          ,chunk-data)))
                                slots)
               (with-source (*png-source* :end (+ (pos *png-source*)
                                                  (chunk-length ,chunk))
                                          :buffer ,(cond
                                                     ((eql buffer t)
                                                      `(chunk-length ,chunk))
                                                     (t ;; number or nil
                                                      buffer)))
                 ,@body))
             ,chunk-data))))))

(defgeneric parse-chunk-data (chunk-type chunk))

(define-chunk-data (ihdr) (width height bit-depth colour-type compression-method
                                 filter-method interlace-method)
  (setf width (ub32be)
        height (ub32be)
        bit-depth (ub8)
        colour-type (ub8)
        compression-method (ub8)
        filter-method (ub8)
        interlace-method (ub8)
        (width *png*) width
        (height *png*) height
        (bit-depth *png*) bit-depth
        (color-type *png*) (ecase colour-type
                             (0 :greyscale)
                             (2 :truecolour)
                             (3 :indexed-colour)
                             (4 :greyscale-alpha)
                             (6 :truecolour-alpha))
        (compression-method *png*) (ecase compression-method
                                     (0 :zlib))
        (interlace-method *png*) (ecase interlace-method
                                   (0 :null)
                                   (1 :adam7))
        (filter-method *png*) (ecase filter-method
                                (0 :standard))))

(define-chunk-data (plte) (palette-entries)
  (let ((entry-count (/ (chunk-length plte) 3)))
    (setf palette-entries (make-array `(,entry-count 3) :element-type 'ub8))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref palette-entries entry sample) (ub8))))
    (setf (palette-count *png*) entry-count
          (palette *png*) palette-entries)))

(define-chunk-data (idat :buffer nil) (data)
  (if *decode-data*
      (progn
        (setf data (source-region (chunk-length idat)))
        (push data (data *png*)))
      (skip-bytes (chunk-length idat))))

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
    (loop :with out = (make-array (get-image-bytes) :element-type 'ub8)
          :with dstate = (3bz:make-zlib-state :output-buffer out)
          :for part :in (nreverse (data *png*))
          :for read-context = (source->3bz-context part)
          :do (3bz:%resync-file-stream read-context)
              (3bz:decompress read-context dstate)
          :finally (assert (3bz:finished dstate))
                   (setf (data *png*) out))))

(define-chunk-data (chrm) (white-point-x white-point-y red-x red-y green-x
                                         green-y blue-x blue-y)
  (setf white-point-x (ub32be)
        white-point-y (ub32be)
        red-x (ub32be)
        red-y (ub32be)
        green-x (ub32be)
        green-y (ub32be)
        blue-x (ub32be)
        blue-y (ub32be)))

(define-chunk-data (gama) (image-gamma)
  (setf image-gamma (ub32be)
        (gamma *png*) (float (/ image-gamma 100000))))

(define-chunk-data (iccp) (profile-name compression-method compressed-profile)
  (setf profile-name (read-string :bytes 79
                                  :encoding :latin-1
                                  :null-terminated-p t)
        compression-method (ub8)
        compressed-profile (read-bytes (chunk-offset) :zlib t)))

(define-chunk-data (sbit) (greyscale red green blue alpha)
  (case (color-type *png*)
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
        (rendering-intent *png*) (ecase rendering-intent
                                   (0 :perceptual)
                                   (1 :relative-colorimetric)
                                   (2 :saturation)
                                   (3 :absolute-colorimetric))))

(define-chunk-data (bkgd) (greyscale red green blue palette-index)
  (case (color-type *png*)
    ((:greyscale :greyscale-alpha)
     (setf greyscale (ub16be)))
    ((:truecolour :truecolour-alpha)
     (setf red (ub16be)
           green (ub16be)
           blue (ub16be)))
    (:indexed-colour
     (setf palette-index (ub8)))))

(define-chunk-data (hist) (frequencies)
  (let ((count (palette-count *png*)))
    (setf frequencies (make-array count :element-type 'ub16))
    (dotimes (i count)
      (setf (aref frequencies i) (ub16be)))))

(define-chunk-data (trns) (grey red blue green alpha-values)
  (ecase (color-type *png*)
    (:greyscale
     (setf grey (ub16be))
     (setf (transparency *png*) grey))
    (:truecolour
     (setf red (ub16be)
           blue (ub16be)
           green (ub16be))
     (setf (transparency *png*)
           (make-array 3 :element-type 'ub16
                         :initial-contents (list red green blue))))
    (:indexed-colour
     (let ((size (chunk-length trns)))
       (setf alpha-values (make-array size :element-type 'ub8))
       (dotimes (i size)
         (setf (aref alpha-values i) (ub8)))
       (setf (transparency *png*) alpha-values)))))

(define-chunk-data (phys) (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (ub32be)
        pixels-per-unit-y (ub32be)
        unit-specifier (ub8)
        (pixel-size *png*) (list :x pixels-per-unit-x
                                 :y pixels-per-unit-y
                                 :unit (ecase unit-specifier
                                         (0 :unknown)
                                         (1 :meter)))))

(define-chunk-data (splt) (palette-name sample-depth palette-entries)
  (setf palette-name (read-string :bytes 79
                                  :encoding :latin-1
                                  :null-terminated-p t)
        sample-depth (ub8))
  (let ((entry-count (/ (chunk-offset) (ecase sample-depth (8 6) (16 10)))))
    (setf palette-entries (make-array `(,entry-count 5) :element-type 'ub16))
    (dotimes (entry entry-count)
      (dotimes (sample 4)
        (setf (aref palette-entries entry sample)
              (ecase (/ sample-depth 8)
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
        (last-modified *png*) (encode-universal-time
                               second minute hour day month year)))

(define-chunk-data (itxt) (keyword compression-flag compression-method
                                   language-tag translated-keyword text)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-flag (ub8)
        compression-method (ub8)
        language-tag (read-string :encoding :latin-1 :null-terminated-p t)
        translated-keyword (read-string :encoding :utf-8 :null-terminated-p t))
  (if (= compression-flag 1)
      (setf text (read-string :encoding :utf-8 :zlib t))
      (setf text (read-string :encoding :utf-8)))
  (push (list language-tag keyword translated-keyword text) (text *png*)))

(define-chunk-data (text) (keyword text-string)
  (setf keyword (read-string :bytes 80 :encoding :latin-1 :null-terminated-p t)
        text-string (read-string :encoding :latin-1))
  (push (list keyword text-string) (text *png*)))

(define-chunk-data (ztxt) (keyword compression-method
                                   compressed-text-datastream)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-method (ub8)
        compressed-text-datastream (read-string :encoding :latin-1 :zlib t))
  (push (list keyword compressed-text-datastream) (text *png*)))

(define-chunk-data (unknown) ()
  (warn 'unknown-chunk-detected))
