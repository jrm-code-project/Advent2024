;;; -*- Lisp -*-

(in-package "ADVENT2024")

(defvar *initialized* nil)

(unless *initialized*
  (setf *initialized* t)
  (iterate ((n (scan-range :from 1 :below 26)))
    (let* ((package (find-package (format nil "ADVENT2024/DAY~D" n)))
           (day (intern "+DAY+" package)))
      (eval `(progn
               (defparameter ,day ,(format nil "Day~d" n))
  
               (defun ,(intern "SOLUTION-DIRECTORY" package) ()
                 (advent-pathname (make-pathname :directory (list :relative ,day))))
  
               (defun ,(intern "INPUT-PATHNAME" package) (&optional (,(intern "N" package) 0))
                 (merge-pathnames
                  (make-pathname :name (format nil "input~[~:;-~:*~d~]" ,(intern "N" package))
                                 :type "txt")
                  (,(intern "SOLUTION-DIRECTORY" package))))
  
               (defun ,(intern "SAMPLE-INPUT-PATHNAME" package) (&optional (,(intern "N" package) 0))
                 (merge-pathnames
                  (make-pathname :name (format nil "sample-input~[~:;-~:*~d~]" ,(intern "N" package))
                                 :type "txt")
                  (,(intern "SOLUTION-DIRECTORY" package))))

               (defun ,(intern "VALIDATE-1" package) ()
                 (format t "~&  Part-1~%")
                 (assert (equal (,(intern "PART-1" package)) ,(intern "+SOLUTION-1+" package))))

               (defun ,(intern "VALIDATE-2" package) ()
                 (format t "~&  Part-2~%")
                 (assert (equal (,(intern "PART-2" package)) ,(intern "+SOLUTION-2+" package))))

               (defun ,(intern "VALIDATE" package) ()
                 (format t  "~&~A~%" ,day)
                 (,(intern "VALIDATE-1" package))
                 (,(intern "VALIDATE-2" package)))

               )))))

               

