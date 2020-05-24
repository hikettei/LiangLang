
(in-package #:liang)


(defun liang-makevm (code)
  (let* ((iseq (compile-to-lvm code))
        (vm (initvm iseq (hash-table-count *variable-names*) *static-heap*)))
    vm))

(defun liang-makevm-byfile (path)
  (let* ((iseq (liang.compiler:compile-to-lvm-byfile path))
         (vm (initvm iseq (hash-table-count *variable-names*) *static-heap*)))
    vm))

(defun liang-execute-code (code &optional args)
  (vmrun (liang-makevm code) args))

(defun liang-execute-file (file &optional args)
  (vmrun (liang-makevm-byfile file) args))

(defun liang-runvm (vm &optional args)
  (vmrun vm args))

(defun liang-compilefile-write (basefile targetfile)
  (let ((iseq (liang.compiler:compile-to-lvm-byfile basefile)))
    (liang.lvm:write-lvm-iseq iseq (hash-table-count *variable-names*) *static-heap*
                              targetfile)))

(defun main (&optional command &rest args)
  (cond
    ((equal command "scr")
     (liang-execute-file (car args)))
    ((equal command "compile")
     (liang-compilefile-write (first args) (second args)))
    ((equal command "run")
     (let ((vm (liang.lvm:load-lvm-structure (car args))))
       (liang-runvm vm)))
    ((equal command "eval")
     (liang-execute-code (car args) (cdr args)))))
