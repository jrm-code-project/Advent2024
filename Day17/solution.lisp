;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY17")

(defstruct (machine
            (:type vector)
            (:conc-name machine/))
  (pc 0)
  a
  b
  c
  (program (vector) :read-only t))

(defun read-machine (filename)
  (apply #'make-machine
        (collect-append
          (choose
           (#m(lambda (line)
                (cond ((str:starts-with? "Register A:" line)
                       (list :a (parse-integer (subseq line 11))))
                      ((str:starts-with? "Register B:" line)
                       (list :b (parse-integer (subseq line 11))))
                      ((str:starts-with? "Register C:" line)
                       (list :c (parse-integer (subseq line 11))))
                      ((str:starts-with? "Program:" line)
                       `(:program ,(collect 'vector
                                     (choose-if
                                      #'numberp
                                      (#Mdigit-char-p
                                       (scan 'string (subseq line 9)))))))
                      (t nil)))
              (scan-file filename #'read-line))))))

(defun run-machine (machine)
  (symbol-macrolet ((a  (machine/a machine))
                    (b  (machine/b machine))
                    (c  (machine/c machine))
                    (pc (machine/pc machine))
                    (program (machine/program machine))
                    (immediate (svref program (1+ pc)))
                    (argument (ecase immediate
                                (0 0)
                                (1 1)
                                (2 2)
                                (3 3)
                                (4 a)
                                (5 b)
                                (6 c)))
                    (next-instruction (progn (incf pc 2)
                                             (iter))))

    (let ((output '()))
      (let iter ()
        (if (>= pc (length program))
            (reverse output)
            (ecase (svref program pc)
              (0 (setf a (truncate a (expt 2 argument))) next-instruction)
              (1 (setf b (logxor b immediate))           next-instruction)
              (2 (setf b (mod argument 8))               next-instruction)

              (3
               (if (zerop a)
                   next-instruction
                   (progn
                     (setf pc immediate)
                     (iter))))

              (4 (setf b (logxor b c))                   next-instruction)
              (5 (push (mod argument 8) output)          next-instruction)
              (6 (setf b (truncate a (expt 2 argument))) next-instruction)
              (7 (setf c (truncate a (expt 2 argument))) next-instruction)))))))

(defun part-1 ()
  (format nil "~{~d~^,~}" (run-machine (read-machine (input-pathname)))))

(defun get-machine-state (machine)
  (list (machine/pc machine)
        (machine/a machine)
        (machine/b machine)
        (machine/c machine)))

(defun set-machine-state! (machine state)
  (setf (machine/pc machine) (first state)
        (machine/a machine) (second state)
        (machine/b machine) (third state)
        (machine/c machine) (fourth state)))

(defun try-machine (machine state input-a)
  (set-machine-state! machine state)
  (setf (machine/a machine) input-a)
  (run-machine machine))

(defun pad-terms (terms size)
  (revappend (make-list (- size (length terms)) :initial-element 0) terms))

(defun from-octal (octal-digits)
  (fold-left (lambda (n digit) (+ (* n 8) digit)) 0 (reverse octal-digits)))

(defun part-2 ()
  (let* ((machine (read-machine (input-pathname)))
         (initial-state (get-machine-state machine))
         (target (machine/program machine)))
    (let term-loop ((terms '())
                    (index (1- (length target))))
      (if (= index -1)
          (from-octal terms)
          (let digit-loop ((digit 0))
            (if (> digit 7)
                (error "No solution")
                (let* ((padded (pad-terms (cons digit terms) (length target)))
                       (output (try-machine machine initial-state (from-octal padded))))
                  (if (and (= (length output) (length target))
                           (= (elt output index) (svref target index)))
                      (term-loop (cons digit terms) (1- index))
                      (digit-loop (1+ digit))))))))))

(alexandria:define-constant +solution-1+ "4,3,7,1,5,3,0,5,4" :test #'string=)
(defconstant +solution-2+ 190384615275535)
