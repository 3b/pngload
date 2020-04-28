(in-package #:pngload)

(defmacro define-chunk (type (png &key (buffer t)) &body body)
  (alexandria:with-gensyms (source chunk chunk-data)
    (let ((struct-name (alexandria:symbolicate 'chunk-data- type)))
      (destructuring-bind (slots . body) body
        `(progn
           (defstruct ,struct-name ,@slots)
           (defun ,(alexandria:symbolicate '#:parse-chunk/ type) (,chunk)
             (let* ((,png *png*)
                    (,type ,chunk)
                    (,chunk-data (,(alexandria:symbolicate
                                    '#:make- struct-name)))
                    (,source (state-source (state ,png))))
               (declare (ignorable ,type ,png))
               (symbol-macrolet ,(mapcar
                                  (lambda (x)
                                    (list x
                                          `(,(alexandria:symbolicate
                                              struct-name '#:- x)
                                            ,chunk-data)))
                                  slots)
                 (with-source (,source
                               :end (+ (pos ,source)
                                       (chunk-length ,chunk))
                               :buffer ,(cond
                                          ((eql buffer t)
                                           `(chunk-length ,chunk))
                                          (t ;; number or nil
                                           buffer)))
                   ,@body))
               ,chunk-data)))))))

(define-chunk idat (png :buffer nil)
  (data)
  (if (state-decode-data (state png))
      (progn
        (setf data (source-region (chunk-length idat)))
        (push data (data png)))
      (skip-bytes (chunk-length idat))))

(define-chunk ihdr (png)
  (width height bit-depth colour-type compression-method filter-method
         interlace-method)
  (setf width (ub32be)
        height (ub32be)
        bit-depth (ub8)
        colour-type (ub8)
        compression-method (ub8)
        filter-method (ub8)
        interlace-method (ub8)
        (width png) width
        (height png) height
        (bit-depth png) bit-depth
        (color-type png) (ecase colour-type
                           (0 :greyscale)
                           (2 :truecolour)
                           (3 :indexed-colour)
                           (4 :greyscale-alpha)
                           (6 :truecolour-alpha))
        (compression-method png) (ecase compression-method
                                   (0 :zlib))
        (interlace-method png) (ecase interlace-method
                                 (0 :null)
                                 (1 :adam7))
        (filter-method png) (ecase filter-method
                              (0 :standard))))

(define-chunk iend (png) ()
  (when (state-decode-data (state png))
    (loop :with out = (make-array (get-image-bytes) :element-type 'ub8)
          :with dstate = (3bz:make-zlib-state :output-buffer out)
          :for part :in (nreverse (data png))
          :for read-context = (source->3bz-context part)
          :do (3bz:%resync-file-stream read-context)
              (3bz:decompress read-context dstate)
          :finally (assert (3bz:finished dstate))
                   (setf (data png) out))))

(define-chunk plte (png)
  (palette-entries)
  (let ((entry-count (/ (chunk-length plte) 3)))
    (setf palette-entries (make-array `(,entry-count 3) :element-type 'ub8))
    (dotimes (entry entry-count)
      (dotimes (sample 3)
        (setf (aref palette-entries entry sample) (ub8))))
    (setf (palette-count png) entry-count
          (palette png) palette-entries)))

(define-chunk chrm (png)
  (white-point-x white-point-y red-x red-y green-x green-y blue-x blue-y)
  (setf white-point-x (ub32be)
        white-point-y (ub32be)
        red-x (ub32be)
        red-y (ub32be)
        green-x (ub32be)
        green-y (ub32be)
        blue-x (ub32be)
        blue-y (ub32be)))

(define-chunk gama (png) (image-gamma)
  (setf image-gamma (ub32be)
        (gamma png) (float (/ image-gamma 100000))))

(define-chunk iccp (png)
  (profile-name compression-method compressed-profile)
  (setf profile-name (read-string :bytes 79
                                  :encoding :latin-1
                                  :null-terminated-p t)
        compression-method (ub8)
        compressed-profile (read-bytes (chunk-offset) :zlib t)))

(define-chunk sbit (png)
  (greyscale red green blue alpha)
  (case (color-type png)
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

(define-chunk srgb (png)
  (rendering-intent)
  (setf rendering-intent (ub8)
        (rendering-intent png) (ecase rendering-intent
                                 (0 :perceptual)
                                 (1 :relative-colorimetric)
                                 (2 :saturation)
                                 (3 :absolute-colorimetric))))

(define-chunk bkgd (png)
  (greyscale red green blue palette-index)
  (case (color-type png)
    ((:greyscale :greyscale-alpha)
     (setf greyscale (ub16be)))
    ((:truecolour :truecolour-alpha)
     (setf red (ub16be)
           green (ub16be)
           blue (ub16be)))
    (:indexed-colour
     (setf palette-index (ub8)))))

(define-chunk hist (png)
  (frequencies)
  (let ((count (palette-count png)))
    (setf frequencies (make-array count :element-type 'ub16))
    (dotimes (i count)
      (setf (aref frequencies i) (ub16be)))))

(define-chunk trns (png)
  (grey red blue green alpha-values)
  (ecase (color-type png)
    (:greyscale
     (setf grey (ub16be))
     (setf (transparency png) grey))
    (:truecolour
     (setf red (ub16be)
           blue (ub16be)
           green (ub16be))
     (setf (transparency png)
           (make-array 3 :element-type 'ub16
                         :initial-contents (list red green blue))))
    (:indexed-colour
     (let ((size (chunk-length trns)))
       (setf alpha-values (make-array size :element-type 'ub8))
       (dotimes (i size)
         (setf (aref alpha-values i) (ub8)))
       (setf (transparency png) alpha-values)))))

(define-chunk phys (png)
  (pixels-per-unit-x pixels-per-unit-y unit-specifier)
  (setf pixels-per-unit-x (ub32be)
        pixels-per-unit-y (ub32be)
        unit-specifier (ub8)
        (pixel-size png) (list :x pixels-per-unit-x
                               :y pixels-per-unit-y
                               :unit (ecase unit-specifier
                                       (0 :unknown)
                                       (1 :meter)))))

(define-chunk splt (png)
  (palette-name sample-depth palette-entries)
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

(define-chunk time (png)
  (year month day hour minute second)
  (setf year (ub16be)
        month (ub8)
        day (ub8)
        hour (ub8)
        minute (ub8)
        second (ub8)
        (last-modified png) (encode-universal-time
                             second minute hour day month year)))

(define-chunk itxt (png)
  (keyword compression-flag compression-method language-tag translated-keyword
           text)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-flag (ub8)
        compression-method (ub8)
        language-tag (read-string :encoding :latin-1 :null-terminated-p t)
        translated-keyword (read-string :encoding :utf-8 :null-terminated-p t))
  (if (= compression-flag 1)
      (setf text (read-string :encoding :utf-8 :zlib t))
      (setf text (read-string :encoding :utf-8)))
  (push (list language-tag keyword translated-keyword text) (text png)))

(define-chunk text (png)
  (keyword text-string)
  (setf keyword (read-string :bytes 80 :encoding :latin-1 :null-terminated-p t)
        text-string (read-string :encoding :latin-1))
  (push (list keyword text-string) (text png)))

(define-chunk ztxt (png)
  (keyword compression-method compressed-text-datastream)
  (setf keyword (read-string :bytes 79 :encoding :latin-1 :null-terminated-p t)
        compression-method (ub8)
        compressed-text-datastream (read-string :encoding :latin-1 :zlib t))
  (push (list keyword compressed-text-datastream) (text png)))

;;; Extension chunks
;;; http://ftp-osl.osuosl.org/pub/libpng/documents/pngextensions.html#Chunks

(define-chunk offs (png)
  (x y unit-specifier)
  (setf x (sb32be)
        y (sb32be)
        unit-specifier (ub8))
  ;; TODO: Add user getter functions (remove below line when done)
  (warn 'chunk-not-implemented :chunk offs :path (get-path)))

(define-chunk pcal (png)
  (name original-zero original-max equation-type parameter-count unit-name)
  ;; TODO: Parse this chunk (remove below 2 lines when done)
  (skip-bytes (chunk-length pcal))
  (warn 'chunk-not-implemented :chunk pcal :path (get-path)))

(define-chunk scal (png)
  (unit-specifier pixel-width pixel-height)
  (setf unit-specifier (ub8)
        pixel-width (parse-float:parse-float
                     (read-string :encoding :ascii :null-terminated-p t))
        pixel-height (parse-float:parse-float
                      (read-string :encoding :ascii)))
  ;; TODO: Add user getter functions (remove below line when done)
  (warn 'chunk-not-implemented :chunk scal :path (get-path)))

(define-chunk gifg (png)
  (disposal-method user-input-flag delay-time)
  (setf disposal-method (ub8)
        user-input-flag (ub8)
        delay-time (ub16be))
  ;; TODO: Add user getter functions (remove below line when done)
  (warn 'chunk-not-implemented :chunk gifg :path (get-path)))

(define-chunk gifx (png)
  (application-identifier authentication-code application-data)
  (setf application-identifier (read-string :bytes 8 :encoding :ascii)
        authentication-code (read-bytes 3)
        application-data (read-bytes (chunk-offset)))
  ;; TODO: Add user getter functions (remove below line when done)
  (warn 'chunk-not-implemented :chunk gifx :path (get-path)))

(define-chunk ster (png)
  (mode)
  (setf mode (ub8))
  ;; TODO: Add user getter function (remove below line when done)
  (warn 'chunk-not-implemented :chunk ster :path (get-path)))

(define-chunk exif (png)
  (data)
  ;; TODO: Parse this chunk (remove below 2 lines when done)
  (setf data (alexandria:alist-plist
              (zpb-exif:exif-alist
               (zpb-exif:parse-exif-octets (read-bytes (chunk-offset)))
               :parsedp t)))
  ;; TODO: Add user getter function (remove below line when done)
  (warn 'chunk-not-implemented :chunk exif :path (get-path)))

(define-chunk unknown (png) ()
  (skip-bytes (chunk-length unknown))
  (warn 'unknown-chunk-detected :chunk unknown :path (get-path)))
