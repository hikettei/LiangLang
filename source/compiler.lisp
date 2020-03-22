
(in-package :liang.compiler)

(defun compile-to-lvm (source &optional (use-library? T))
  (with-generate-iseq i
    (if use-library?
        (append-liang-file i "source/lib.liang"))
    (dolist (n (gentree source)) (generate-tree-to-iseq i n))
    (generate-iseq i :RETURN)))

(defun compile-to-lvm-byfile (path)
  (compile-to-lvm (read-file-sequence path)))
