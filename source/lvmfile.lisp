
(in-package :liang.lvm)


(defparameter *lvmstructure* "NLLL")

"""
LVMFile {
  N version
  L variable-field-size
  L body-size
  L static-heap-size
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

(defparameter *STRING-TAG* 1)
(defparameter *HEAPHEAD* "NL")

"""
String {
  N type
  L length
  A[(length string)] string
"""

(defun make-instruction (list)
  `(,(car list) ;instruction
    ,(or (elt list 1) 0)
    ,(or (elt list 2) 0)
    ,(or (elt list 3) 0)
    ,(or (elt list 4) 0)
    ,(or (elt list 5) 0)))

(defun make-string-structure (n)
  (format nil "~v@{~A~:*~}" n "A"))

(defmethod make-vmheap ((string string))
  `(,*STRING-TAG* ,(length string) ,string))

(defmethod make-vmheap-structure ((string string))
  "NLA*")

(defun make-form (size heap)
  (with-output-to-string (s)
    (format s *lvmstructure*)
    (mapc #'(lambda (x)
                     (format s (make-vmheap-structure x)))
         heap)
    (format s "~v@{~A~:*~}" size *instruction*)))

(defun pack/append (form &rest rest)
  (apply #'pack form (apply #'append rest)))

(defun pack-lvmstructure (iseq &key version
                                 static-heap
                                 variable-size)
  (let ((body (mapcan #'make-instruction iseq))
        (heap (mapcan #'make-vmheap (coerce static-heap 'list))))
    (pack/append (make-form (length iseq) (coerce static-heap 'list))
          `(,version
            ,variable-size
            ,(length iseq)
            ,(length static-heap))
          heap
          body)))

(defun write-lvm-iseq (iseq variable-size static-heap path)
  (with-open-file (s path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (let ((lvmstructure (pack-lvmstructure iseq :version 0
                                                :static-heap static-heap
                                                :variable-size variable-size)))
      (dotimes (i (length lvmstructure))
        (write-char (aref lvmstructure i) s)))))

(defun load-lvm-structure (path)
  (with-open-file (s path :direction :input)
    (let ((lvmfile (make-string (file-length s))))
      (read-sequence lvmfile s)
      (multiple-value-bind (version variable-field-size body-size heap-size)
          (unpack *lvmstructure* lvmfile)
        (declare (ignore version))
        (let ((iseq-at (length *lvmstructure*))
              (tmpform *lvmstructure*)
              (heapresult (make-array 0 :adjustable T :fill-pointer 0)))
          (flet ((appendform (str)
                   (setq tmpform (concatenate 'string tmpform str))))
            (dotimes (i heap-size)
              (appendform *HEAPHEAD*)
              (let ((heapform (last (multiple-value-list (unpack tmpform lvmfile)) 2)))
                (setq iseq-at (+ iseq-at (second heapform) 2))
                (case (car heapform)
                  (1 (appendform (make-string-structure (second heapform)))))
                (let ((heapvalue (last (multiple-value-list (unpack tmpform lvmfile))
                                       (second heapform))))
                  (vector-push-extend (with-output-to-string (s)
                                        (dolist (n heapvalue) (format s n)))
                                      heapresult))))
            (appendform (format nil "~v@{~A~:*-~}" body-size *instruction*))
            (let* ((lvmfile-complete (multiple-value-list (unpack tmpform lvmfile)))
                   (iseq (subseq lvmfile-complete iseq-at))
                   (2diseq (loop for i from 0 to (1- body-size)
                                 append `(,(subseq iseq (* i 6) (+ 6 (* i 6)))))))
              (initvm 2diseq variable-field-size heapresult))))))))
            

(defun x64-forward-pc (lvm)
  (let ((i (nth (LVM-pc lvm) (LVM-iseq lvm))))
    (case (car i)
      (2 ;pushstring
       (with-output-to-string (out)
	 (format out "push ")
	 (format out (x64-string (second i))))))))

(defmacro x64-string (n)
  `(concatenate 'string "str" (princ-to-string ,n)))

(defun x64-static-string (lvm)
  (let ((string-table (LVM-static-strings lvm))
	(i -1))
    (with-output-to-string (out)
     (mapcar (lambda (s)
	      (setq i (1+ i))
	      (format out (concatenate 'string
			   (x64-string i)
			   " db "
			   "'"
			   s
			   "'"
			   ", 0"
			   '(#\newline))))
	     (coerce string-table 'list)))))
