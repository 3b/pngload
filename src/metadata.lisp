(in-package #:pngload)

(defun find-chunks (png &rest chunk-types)
  (remove-if-not
   (lambda (x)
     (member (alexandria:make-keyword (string-upcase (get-chunk-type x)))
             chunk-types))
   (datastream-chunks (parse-tree png))))

(defgeneric get-metadata (png key)
  (:method (png key)
    (error "Unknown metadata ~s" key)))

;;; IHDR

(defmethod get-metadata (png (key (eql :width)))
  (width png))

(defmethod get-metadata (png (key (eql :height)))
  (height png))

(defmethod get-metadata (png (key (eql :bit-depth)))
  (bit-depth png))

(defmethod get-metadata (png (key (eql :color-type)))
  (color-type png))

(defmethod get-metadata (png (key (eql :compression-method)))
  (let ((chunk (first (find-chunks png :ihdr))))
    (case (chunk-data-ihdr-compression-method (chunk-data chunk))
      (0 :zlib)
      (t :unknown))))

(defmethod get-metadata (png (key (eql :interlace-method)))
  (let ((chunk (first (find-chunks png :ihdr))))
    (case (chunk-data-ihdr-interlace-method (chunk-data chunk))
      (0 :null)
      (1 :adam7)
      (t :unknown))))

(defmethod get-metadata (png (key (eql :filter-method)))
  (let ((chunk (first (find-chunks png :ihdr))))
    (case (chunk-data-ihdr-filter-method (chunk-data chunk))
      (0 :standard)
      (t :unknown))))

;;; PLTE

(defmethod get-metadata (png (key (eql :palette)))
  (alexandria:when-let ((chunk (first (find-chunks png :plte))))
    (chunk-data-plte-palette (chunk-data chunk))))

;;; cHRM

(defmethod get-metadata (png (key (eql :white-point)))
  (alexandria:when-let* ((chunk (first (find-chunks png :chrm)))
                         (data (chunk-data chunk)))
    (values (/ (chunk-data-chrm-white-point-x data) 1f6)
            (/ (chunk-data-chrm-white-point-y data) 1f6))))

(defmethod get-metadata (png (key (eql :chromaticity-red)))
  (alexandria:when-let* ((chunk (first (find-chunks png :chrm)))
                         (data (chunk-data chunk)))
    (values (/ (chunk-data-chrm-red-x data) 1f6)
            (/ (chunk-data-chrm-red-y data) 1f6))))

(defmethod get-metadata (png (key (eql :chromaticity-green)))
  (alexandria:when-let* ((chunk (first (find-chunks png :chrm)))
                         (data (chunk-data chunk)))
    (values (/ (chunk-data-chrm-green-x data) 1f6)
            (/ (chunk-data-chrm-green-y data) 1f6))))

(defmethod get-metadata (png (key (eql :chromaticity-blue)))
  (alexandria:when-let* ((chunk (first (find-chunks png :chrm)))
                         (data (chunk-data chunk)))
    (values (/ (chunk-data-chrm-blue-x data) 1f6)
            (/ (chunk-data-chrm-blue-y data) 1f6))))

;;; gAMA

(defmethod get-metadata (png (key (eql :gamma)))
  (alexandria:if-let ((chunk (first (find-chunks png :gama))))
    (let ((data (chunk-data chunk)))
      (/ (chunk-data-gama-image-gamma data) 1f6))
    1f0))

;;; iCCP

(defmethod get-metadata (png (key (eql :color-profile)))
  (alexandria:when-let* ((chunk (first (find-chunks png :iccp)))
                         (data (chunk-data chunk)))
    (chunk-data-iccp-compressed-profile data)))

;;; sBIT

(defmethod get-metadata (png (key (eql :significant-bits)))
  (alexandria:when-let* ((chunk (first (find-chunks png :sbit)))
                         (data (chunk-data chunk)))
    (ecase (color-type png)
      (:greyscale
       (chunk-data-sbit-greyscale data))
      ((:truecolour :indexed-colour)
       (values (chunk-data-sbit-red data)
               (chunk-data-sbit-green data)
               (chunk-data-sbit-blue data)))
      (:greyscale-alpha
       (values (chunk-data-sbit-greyscale data)
               (chunk-data-sbit-alpha data)))
      (:truecolour-alpha
       (values (chunk-data-sbit-red data)
               (chunk-data-sbit-green data)
               (chunk-data-sbit-blue data)
               (chunk-data-sbit-alpha data))))))

;;; sRGB

(defmethod get-metadata (png (key (eql :srgb-rendering-intent)))
  (alexandria:when-let* ((chunk (first (find-chunks png :srgb)))
                         (data (chunk-data chunk)))
    (ecase (chunk-data-srgb-rendering-intent data)
      (0 :perceptual)
      (1 :relative-colorimetric)
      (2 :saturation)
      (3 :absolute-colorimetric))))

;;; bKGD

(defmethod get-metadata (png (key (eql :background-color)))
  (alexandria:when-let* ((chunk (first (find-chunks png :bkgd)))
                         (data (chunk-data chunk)))
    (ecase (color-type png)
      ((:greyscale :greyscale-alpha)
       (let ((color (chunk-data-bkgd-greyscale data)))
         (values color color color)))
      ((:truecolour :truecolour-alpha)
       (values (chunk-data-bkgd-red data)
               (chunk-data-bkgd-green data)
               (chunk-data-bkgd-blue data)))
      (:indexed-colour
       (let ((palette (state-palette (state png)))
             (index (chunk-data-bkgd-palette-index data)))
         (values (aref palette index 0)
                 (aref palette index 1)
                 (aref palette index 2)))))))

;;; hIST

(defmethod get-metadata (png (key (eql :histogram)))
  (alexandria:when-let* ((chunk (first (find-chunks png :hist)))
                         (data (chunk-data chunk))
                         (palette (state-palette (state png))))
    (loop :for frequency :across (chunk-data-hist-frequencies data)
          :for i :from 0
          :collect (cons (vector (aref palette i 0)
                                 (aref palette i 1)
                                 (aref palette i 2))
                         frequency))))

;;; tRNS

(defmethod get-metadata (png (key (eql :transparency)))
  (alexandria:when-let* ((chunk (first (find-chunks png :trns)))
                         (data (chunk-data chunk))
                         (palette (state-palette (state png))))
    (ecase (color-type png)
      (:greyscale
       (chunk-data-trns-grey data))
      (:truecolour
       (values (chunk-data-trns-red data)
               (chunk-data-trns-green data)
               (chunk-data-trns-blue data)))
      (:indexed-colour
       (loop :for alpha :across (chunk-data-trns-alpha-values data)
             :for i :from 0
             :collect (cons (vector (aref palette i 0)
                                    (aref palette i 1)
                                    (aref palette i 2))
                            alpha))))))

;;; pHYS

(defmethod get-metadata (png (key (eql :pixel-dimensions)))
  (alexandria:if-let ((chunk (first (find-chunks png :phys))))
    (let ((data (chunk-data chunk)))
      (list :x (chunk-data-phys-pixels-per-unit-x data)
            :y (chunk-data-phys-pixels-per-unit-y data)
            :unit (ecase (chunk-data-phys-unit-specifier data)
                    (0 :unknown)
                    (1 :meter))))
    (list :x 1 :y 1 :unit :unknown)))

;;; sPLT

(defmethod get-metadata (png (key (eql :suggested-palettes)))
  (alexandria:if-let ((chunks (find-chunks png :splt)))
    (loop :for chunk :in chunks
          :for data = (chunk-data chunk)
          :for entries = (chunk-data-splt-palette-entries data)
          :collect (cons (chunk-data-splt-palette-name data)
                         (list :sample-depth (chunk-data-splt-sample-depth data)
                               :entries entries)))))

;;; tIME

(defmethod get-metadata (png (key (eql :last-modified)))
  (alexandria:when-let* ((chunk (first (find-chunks png :splt)))
                         (data (chunk-data chunk)))
    (encode-universal-time
     (chunk-data-time-second data)
     (chunk-data-time-minute data)
     (chunk-data-time-hour data)
     (chunk-data-time-day data)
     (chunk-data-time-month data)
     (chunk-data-time-year data))))

(defmethod get-metadata (png (key (eql :text)))
  (alexandria:when-let ((chunks (find-chunks png :itxt :text :ztxt)))
    (let (text)
      (dolist (chunk chunks)
        (let ((data (chunk-data chunk)))
          (etypecase data
            (chunk-data-text
             (push (list :language :unspecified
                         :keyword (chunk-data-text-keyword data)
                         :translated-keyword :unspecfied
                         :text (chunk-data-text-text-string data))
                   text))
            (chunk-data-ztxt
             (push (list :language :unspecified
                         :keyword (chunk-data-ztxt-keyword data)
                         :translated-keyword :unspecified
                         :text (chunk-data-ztxt-compressed-text-datastream
                                data))
                   text))
            (chunk-data-itxt
             (push (list :language (chunk-data-itxt-language-tag data)
                         :keyword (chunk-data-itxt-keyword data)
                         :translated-keyword (chunk-data-itxt-translated-keyword
                                              data)
                         :text (chunk-data-itxt-text data))
                   text)))))
      (nreverse text))))

;;; TODO: Extension chunks
