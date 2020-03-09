
(in-package #:liang)


(defun liang-compile-file (path)
  (liang-compile-program
   (read-file-sequence (concatenate 'string path ".liang"))))


(defparameter *MNEMONIC*
  (alexandria:plist-hash-table
   
  `(:PUSHNUMBER 0
    :PUSHSTRING 1
    :PUSHNAME   2
    :PUSHDEF    3
    :PUSHLAMBDA 4

    :SENDEXP    10
    :SENDFN    11

    :RETURN 30)))

(defparameter *BuiltInMethods* `(= + - * / print value_if equals or))


(defmacro mnemonic (mnemonic)
  `(gethash ,mnemonic *MNEMONIC*))

