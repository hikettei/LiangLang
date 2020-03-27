
(in-package :liang.compiler)

(defun compile-to-lvm (source)
  (with-generate-iseq i
    (setq *using-files*
          (delete-duplicates *using-files* :test #'equal)) 
    (mapc #'(lambda (file) (append-liang-file i file))
          (coerce *using-files* 'list))
    (dolist (n (gentree source)) (generate-tree-to-iseq i n))
    (generate-iseq i :RETURN)))

(defun compile-to-lvm-byfile (path)
  (compile-to-lvm (read-file-sequence path)))
