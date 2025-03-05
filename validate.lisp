;;; -*- Lisp -*-

(in-package "ADVENT2024")

(defun validate ()
  (do ((i 1 (+ i 1)))
      ((> i 25) t)
    (let ((package (find-package (format nil "ADVENT2024/DAY~d" i))))
      (when (and (boundp (intern "+SOLUTION-1+" package))
                 (boundp (intern "+SOLUTION-2+" package)))
          (funcall (intern "VALIDATE" package))))))

(validate)
