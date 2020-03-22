
(in-package :liang.lvm)


(defparameter *lvmstructure* "NLL")

"""
LVMFile {
  N version
  L variable-field-size
  L body-size
}
"""

(defparameter *instruction* "NLLLLL")

"""
Instruction {
  N instruction
  L operand0
  L operand1
  L operand2
  L operand3
  L operand4
  L operand5
}
"""

(defun make-instruction (list)
  `(,(car list) ;instruction
    ,(or (elt list 1) 0)
    ,(or (elt list 2) 0)
    ,(or (elt list 3) 0)
    ,(or (elt list 4) 0)
    ,(or (elt list 5) 0)))

(defun make-form (size)
  (concatenate 'string *lvmstructure*
               (format nil "~v@{~A~:*~}" size *instruction*)))

(defun pack-lvmstructure (iseq &key version
                                 variable-size)
  (let ((body (mapcan #'make-instruction iseq)))
    (pack (make-form (length iseq))
          version
          variable-size
          (length iseq)
          body)))

(defun write-lvm-iseq (iseq variable-size path)

  (with-open-file (s path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let ((lvmstructure (pack-lvmstructure iseq :version 0
                                                :variable-size variable-size)))
      (dotimes (i (length lvmstructure))
        (write-char (aref lvmstructure i) s)))))

(defun load-lvm-structure (path)
  (with-open-file (s path :direction :input)
    (let ((lvmfile (make-string (file-length s))))
      (read-sequence lvmfile s)
      (multiple-value-bind (version variable-field-size body-size)
          (unpack *lvmstructure* lvmfile)
        (declare (ignore version))
        (let* ((lvmfilelist (multiple-value-list (unpack (make-form body-size) lvmfile)))
               (iseq (subseq lvmfilelist 3))
               (2diseq (loop for i from 0 to (1- body-size)
                             append `(,(subseq iseq (* i 6) (+ 6 (* i 6)) )))))
          (initvm 2diseq variable-field-size))))))

