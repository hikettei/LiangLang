
(in-package #:liang)


(defun liang-makevm (code)
  (let* ((iseq (compile-to-lvm code))
        (vm (initvm iseq (hash-table-count *variable-names*))))

    vm))

(defun liang-makevm-byfile (path)
  (let* ((iseq (liang.compiler:compile-to-lvm-byfile path))
         (vm (initvm iseq (hash-table-count *variable-names*))))
    vm))

(defun liang-execute-code (code)
  (vmrun (liang-makevm code)))

(defun liang-execute-file (file)
  (vmrun (liang-makevm-byfile file)))

(defun liang-runvm (vm)
  (vmrun vm))

(defun liang-compilefile-write (basefile targetfile)
  (let ((iseq (liang.compiler:compile-to-lvm-byfile basefile)))
    (liang.lvm:write-lvm-iseq iseq (hash-table-count *variable-names*) targetfile)))
