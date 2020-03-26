
(in-package :liang.lvm)


(defstruct LVMSimpleArray
  (content)
  (length NIL :type fixnum))

(defstruct LVMArrayAdjustable
  (content)
  (length NIL :type fixnum))


(defun init-lvm-array (element length)
  (make-LVMSimpleArray
   :content (make-array length :initial-contents element)
   :length length))

(defun init-lvm-array-adjustable (element length)
  (make-LVMArrayAdjustable
   :content (make-array length :initial-contents element :adjustable T)
   :length length))


(defmethod lvmarray-getof ((array LVMSimpleArray) n)
  (aref (LVMSimpleArray-content array) n))

(defmethod lvmarray-getof ((array LVMArrayAdjustable) n)
  (aref (LVMArrayAdjustable-content array) n))

(defmethod lvmarray-setof ((array LVMSimpleArray) n val)
  (setf (aref (LVMSimpleArray-content array) n) val))

(defmethod lvmarray-setof ((array LVMArrayAdjustable) n val)
  (setf (aref (LVMArrayAdjustable-content array) n) val))

(defmethod lvmarray-lengthof ((array LVMSimpleArray))
  (LVMSimpleArray-length array))

(defmethod lvmarray-lengthof ((array LVMArrayAdjustable))
  (LVMArrayAdjustable-length array))
