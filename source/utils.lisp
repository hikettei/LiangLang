
(in-package #:liang.compiler)



(defun read-file-sequence (filepath)
  (with-open-file (in filepath :direction :input)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      buf)))

(defun write-file-sequence (filepath sequence)
  (with-open-file (target filepath
                    :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)

      (format target sequence)))
