
(in-package #:liang.compiler)


(defvar *variable-names* (make-hash-table :test 'equal))
(defvar *compile-errors* (make-array 0 :adjustable T))

(defparameter *BuiltInMethods* `(= + - * / print value_if equals or))

(defun variable-names (&optional name)
  (gethash (write-to-string name) *variable-names*))

(defun (setf variable-names) (_ &optional name0)
  (declare (ignore _))
  (let* ((name (write-to-string name0))
         (val (gethash name *variable-names*)))
    (if val val
        (setf (gethash name *variable-names*) (hash-table-count *variable-names*)))))

(dolist (i *BuiltInMethods*)
  (setf (variable-names i) NIL))

(defmacro thirdcar (list)
  `(car (third ,list)))

(defmacro ssecond (list)
  `(second (second ,list)))

(defmacro thirdsec (list)
  `(second (third ,list)))

(defun iseq-builder (name &rest operand)
  `((,(mnemonic name) ,@operand)))

(defmacro append-iseq (target iseq)
  `(setq ,target (nconc ,target ,iseq)))

(defmacro generate-iseq (target name &rest operand)
  `(setq ,target (nconc ,target (iseq-builder ,name ,@operand))))

(defmacro generate-tree-to-iseq (target tree)
  `(setq ,target (nconc ,target (tree2iseq ,tree))))

(defmacro with-generate-iseq (i &body body)
  `(let ((,i NIL)) ,@body ,i))

(defun compile-body-to-lvm (tree)
  (with-generate-iseq i
    (dolist (n tree) (generate-tree-to-iseq i n))))

(defun gencode-push (name tree)
  (iseq-builder name (second tree)))

(defun gencode-exp (tree)
  (destructuring-bind (_ x y z) tree
    (declare (ignore _))
    (with-generate-iseq i
      (generate-tree-to-iseq i y)
      (generate-tree-to-iseq i z)
      (generate-iseq i :SENDEXP (variable-names (second x))))))

(defmacro compile-args (target args)
  `(dolist (argument ,args)
     (setf (variable-names (second argument)) NIL)
     (generate-tree-to-iseq ,target argument)))


(defun gencode-fn (opename tree)
  (destructuring-bind (_ name args) tree
    (declare (ignore _))
    (with-generate-iseq i
      (setf (variable-names (second name)) NIL)
      (compile-args i args)
      (generate-iseq i opename (variable-names (second name)) (length args)))))


(defun tree2iseq (tree)
  (case (first tree)
    (:NUMBER (gencode-push :PUSHNUMBER tree))
    (:STRING (gencode-push :PUSHSTRING tree))
    (:NAME (iseq-builder :PUSHNAME (variable-names (second tree))))
    (:SETQ
     (if (eq (thirdcar tree) :CALLDEF)
         (progn
           (setf (thirdcar tree) :PUSHDEF)
           (setf (variable-names (second (thirdsec tree))) NIL))
         (setf (variable-names (second (third tree))) NIL))
     (with-generate-iseq i
       (generate-tree-to-iseq i (third  tree))
       (generate-tree-to-iseq i (fourth tree))
       (generate-iseq i :SETQ)))
    (:EXP (gencode-exp tree))
    (:PUSHDEF (gencode-fn :PUSHDEF tree))
    (:CALLDEF (gencode-fn :SENDFN tree))
    (:LAMBDA (destructuring-bind (_ args body) tree
               (declare (ignore _))
               (let ((compiledbody (compile-body-to-lvm body)))
                 (with-generate-iseq i
                   (compile-args i args)
                   (generate-iseq i :PUSHLAMBDA (1+ (length compiledbody)) (length args))
                   (append-iseq i compiledbody)
                   (generate-iseq i :RETURN)))))
    (T (print tree) NIL)))

