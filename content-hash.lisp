;; copied from tarhash.lisp from quicklisp-controller: https://github.com/quicklisp/quicklisp-controller
;; changed to use openssl to compute sha1 digest rather than depending on ironclad

(in-package #:ql-https)

(defconstant +block-octet-count+ 512)

(defun make-block-buffer ()
  (make-array +block-octet-count+
              :element-type '(unsigned-byte 8)
              :initial-element 0))

(defun read-header-block (buffer stream)
  "Read a tar header block from STREAM into BUFFER. Returns NIL when
at the terminating block of the end of input, BUFFER otherwise."
  (let ((size (read-sequence buffer stream)))
    (cond ((zerop size)
           nil)
          ((/= size 0 +block-octet-count+)
           (error "Short block (only ~D bytes)" size))
          ((every #'zerop buffer)
           nil)
          (t
           buffer))))

(defun ascii-subseq (vector start end)
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun block-asciiz-string (block start length)
  (let* ((end (+ start length))
         (eos (or (position 0 block :start start :end end)
                            end)))
    (ascii-subseq block start eos)))

(defun payload-size (header)
  (values (parse-integer (block-asciiz-string header 124 12) :radix 8)))

(defun file-payload-p (header)
  (member (aref header 156) '(0 48)))

(defparameter *ignored-path-substrings*
  '("/_darcs/" "/CVS/" "/.git/" "/CVS/" "/.hg/"))

(defun ignored-path-p (path)
  (dolist (substring *ignored-path-substrings*)
    (when (search substring path)
      (return t))))

(defun prefix (header)
  (when (plusp (aref header 345))
    (block-asciiz-string header 345 155)))

(defun name (header)
  (block-asciiz-string header 0 100))

(defun full-path (header)
  (let ((prefix (prefix header))
        (name (name header)))
    (if prefix
        (format nil "~A/~A" prefix name)
        name)))

(defun skip-n-octets-blocks (n stream)
  (let ((count (ceiling n +block-octet-count+))
        (block (make-block-buffer)))
    (dotimes (i count)
      (read-sequence block stream))))

(defun content-info (stream)
  "Return a list of file info for the POSIX tar stream STREAM. Each
element in the result is a list of a filename, the position of its
starting storage block in STREAM, and the total file size."
  (file-position stream :start)
  (let ((buffer (make-block-buffer))
        (result '()))
    (loop
      (let ((header (read-header-block buffer stream))
            (position (file-position stream)))
        (when (not header)
          (return result))
        (let ((size (payload-size header)))
          (when (file-payload-p header)
            (let ((path (full-path header)))
              (unless (ignored-path-p path)
                (push (list path
                            position
                            size)
                      result))))
          (skip-n-octets-blocks size stream))))))

(defun content-hash (tarfile)
  "Return a hash string of TARFILE. The hash is computed by creating
the digest of the files in TARFILE in order of their name."
  (uiop:with-temporary-file (:pathname temp)
    (setf tarfile (gunzip tarfile temp))
    (unwind-protect
         (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
           (let* ((openssl (uiop:launch-program "openssl dgst -sha1"
                                                :input :stream
                                                :output :stream))
                  (digest-stream (uiop:process-info-input openssl))
                  (buffer (make-block-buffer)))
             (flet ((add-file-content (position size)
                      (file-position stream position)
                      (multiple-value-bind (complete partial)
                          (truncate size +block-octet-count+)
                        (dotimes (i complete)
                          (read-sequence buffer stream)
                          (write-sequence buffer digest-stream))
                        (read-sequence buffer stream)
                        (write-sequence buffer digest-stream :end partial))))
               (let ((contents (content-info stream)))
                 (setf contents (sort contents #'string< :key #'first))
                 (dolist (info contents)
                   (destructuring-bind (position size)
                       (rest info)
                     (add-file-content position size))))
               (close (uiop:process-info-input openssl))
               (unless (zerop (uiop:wait-process openssl))
                 (error "openssl failed to calculate sha1"))
               (extract-openssl-digest (read-line (uiop:process-info-output openssl))))))
      (when (probe-file temp)
        (ignore-errors (delete-file temp))))))
