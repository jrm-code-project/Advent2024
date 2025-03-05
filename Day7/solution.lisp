;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY7")

(defun can-satisfy? (target ops terms)
  (let recur ((accum (first terms))
              (terms (rest terms)))
       (cond ((and (> accum target)
                   (not (find-if #'zerop terms)))
              nil)
             ((consp terms)
              (find-if
               (lambda (op)
                 (recur (funcall op accum (first terms)) (rest terms)))
               ops))
             ((null terms) (= target accum))
             (t (error "Dotted list.")))))

(defun parse-integer-list (str)
  (map 'list #'parse-integer (str:split #\Space str :omit-nulls t)))

(defun parse-line (str)
  (let ((key+value (str:split #\: str)))
    (cons (parse-integer (first key+value))
          (parse-integer-list (second key+value)))))

(defun puzzle (ops)
  (collect-sum
    (let* ((equations
             (#Mparse-line
              (scan-file (input-pathname) #'read-line)))
           (satisfied (#M(lambda (equation)
                           (can-satisfy? (car equation) ops (cdr equation)))
                         equations)))
      (#Mcar (choose satisfied equations)))))

(defun part-1 ()
  (puzzle (list #'+ #'*)))

(defun concatenate-digits (left right)
  (+ (* left (expt 10 (1+ (integer-log right 10))))
     right))

(defun part-2 ()
  (puzzle (list #'+ #'* #'concatenate-digits)))

(defconstant +solution-1+ 3598800864292)
(defconstant +solution-2+ 340362529351427)
