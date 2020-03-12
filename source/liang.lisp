
(in-package #:liang)


(defun liang-execute-code (code)
  (let* ((iseq (compile-to-lvm code))
        (vm (initvm iseq (hash-table-count *variable-names*))))

    (vmrun vm)))

