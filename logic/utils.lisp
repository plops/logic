(in-package :logic)
(defun reuse-cons (x y x-y)
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))