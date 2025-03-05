;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY24")

(defun part-2 () "bpf,fdw,hcc,hqc,qcw,z05,z11,z35")

(defparameter +solution-1+ 58367545758258)
(defparameter +solution-2+ "bpf,fdw,hcc,hqc,qcw,z05,z11,z35")

(defun get-input (swaps input-pathname)
  (flet ((maybe-swap (symbol)
           (cond ((assoc symbol swaps) (cdr (assoc symbol swaps)))
                 ((rassoc symbol swaps) (car (rassoc symbol swaps)))
                 (t symbol))))

    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (set-syntax-from-char #\: #\;)
      (set-macro-character #\: (lambda (stream char) (declare (ignore stream char)) :colon))
      (set-macro-character #\newline (lambda (stream char) (declare (ignore stream char)) :newline))

      (with-open-file (stream input-pathname :direction :input)
        (let iter ((token (read stream nil :eof))
                   (line '())
                   (gates '())
                   (wires '())
                   (outputs '()))
        
          (multiple-value-bind (line* gates* wires* outputs*)
              (if (or (eq token :eof) (eq token :newline))
                  (if line
                      (if (member :colon line)
                          (values '()
                                  gates
                                  (cons `(,(third line) () ,(first line)) wires)
                                  outputs)
                          (values '()
                                  (cons `(,(maybe-swap (first line)) ()
                                          (,(ecase (fourth line)
                                              (XOR 'logxor)
                                              (OR 'logior)
                                              (AND 'logand))
                                           ,@(list (list (third line)) (list (fifth line)))))
                                        gates)
                                  wires
                                  (if (and (symbolp token)
                                           (char= (char (symbol-name token) 0) #\z))
                                      (cons `(list ,(list token)) outputs)
                                      outputs)
                                  ))
                      (values '() gates wires outputs))
                  (values (cons token line) gates wires (if (and (symbolp token)
                                                                 (char= (char (symbol-name token) 0) #\z))
                                                            (cons (list token) outputs)
                                                            outputs)))
            (if (eq token :eof)
                `(labels (,@wires*
                          ,@gates*)
                   (fold-left (lambda (acc bit)
                                (+ (* 2 acc) bit))
                              0  (list ,@(sort outputs* #'string-greaterp :key #'(lambda (term) (symbol-name (car term)))))))
                (iter (read stream nil :eof) line* gates* wires* outputs*))))))))

(defun part-1 ()
  (eval (get-input '() (input-pathname))))

#||
(defun apply-renames (renames gate)
  `(,(if (assoc (first gate) renames)
         (cdr (assoc (first gate) renames))
         (first gate))
    (,(first (second gate))
     ,@(sort (list (if (assoc (second (second gate)) renames)
                       (cdr (assoc (second (second gate)) renames))
                       (second (second gate)))
                   (if (assoc (third (second gate)) renames)
                       (cdr (assoc (third (second gate)) renames))
                       (third (second gate))))
             #'string-lessp
             :key (lambda (k)
                    (if (consp k)
                        (symbol-name (car k))
                        (symbol-name k)))))))

(defun apply-renames* (renames gates)
  (sort (map 'list (lambda (gate)
                     (apply-renames renames gate))
             gates)
        #'string-lessp
        :key (lambda (gate) (let ((x (second (second gate))))
                              (if (consp x)
                                        (symbol-name (car x))
                                        (symbol-name x))))))

(defun compute-renames (gates)
  (let ((renames '()))
    (dolist (gate gates)
      (when (and (char= (char (symbol-name (second (second gate))) 0) #\x)
                 (char= (char (symbol-name (third (second gate))) 0) #\y)
                 (not (char= (char (symbol-name (first gate)) 0) #\z)))
        (let* ((n (subseq (symbol-name (second (second gate))) 1))
               (rename (ecase (first (second gate))
                         (logxor (intern (format nil "^~a~a" n (first gate))))
                         (logand (intern (format nil "&~a~a" n (first gate)))))))
          (push `(,(first gate) . ,rename) renames))))
    (let ((gates* (apply-renames* renames gates)))
      ;; (dolist (gate gates*)
      ;;   (when (and (eql (first (second gate)) 'logior)
      ;;              (char= (char (symbol-name (second (second gate))) 0) #\&)
      ;;              (every #'alpha-char-p (symbol-name (first gate))))
      ;;     (let* ((n (subseq (symbol-name (second (second gate))) 1))
      ;;            (rename (intern (format nil "o~a" n (second (second gate))))))
      ;;       (push `(,(first gate) . ,rename) renames)))
      ;;   (when (and (eql (first (second gate)) 'logand)
      ;;              (char= (char (symbol-name (second (second gate))) 0) #\^)
      ;;              (every #'alpha-char-p (symbol-name (third (second gate))))
      ;;              (every #'alpha-char-p (symbol-name (first gate))))
      ;;     (let* ((n (subseq (symbol-name (second (second gate))) 1 3))
      ;;            (rename (intern (format nil "a~a~a" n (first gate)))))
      ;;       (push `(,(first gate) . ,rename) renames)))
      ;;   )
      ;;    (when (and (eql (first (second gate)) 'logior)
      ;;               (symbolp (third (second gate)))
      ;;               (every #'alpha-char-p (symbol-name (third (second gate)))))
      ;;      (let* ((n (subseq (symbol-name (second (second gate))) 1))
      ;;             (rename (intern (format nil "i~a" n))))
      ;;        (push `(,(third (second gate)) . ,rename) renames)))

    (dolist (gate (apply-renames* renames gates))
      (format t "~&~s~%" gate)))))

      (|gpg| (LOGAND |&24dfm| |^25hnf|))
      (|z25| (LOGXOR |&24dfm| |^25hnf|))

      (|hqc| (LOGIOR |^24qcw| |chn|))
          



(multiple-value-bind (wires gates outputs)
    (let ((swaps '((|z35| . |fdw|)
                   (|z05| . |bpf|)
                   (|z11| . |hcc|)
                   (|qcw| . |hqc|)
                   )))
      (get-input swaps (input-pathname)))
  (compute-renames gates))

(format nil "~{~a~^,~}"(sort '(|z35| |fdw| |z05| |bpf| |z11| |hcc| |qcw| |hqc|)
      #'string-lessp
      :key #'symbol-name)) ; => "bpf,fdw,hcc,hqc,qcw,z05,z11,z35"


    (labels ((parse-line (line)
               (let ((colon (position #\: line)))
               


  (scan-file (input-pathname) #'parse-line))

(defun parse-line (line)
  (let ((colon (position #\: line)))
    (if (numberp colon)
        `(def ,(intern (subseq line 0 colon) (find-package "ADVENT2024/DAY24"))
             ,(parse-integer (subseq line (1+ colon))))
        (let ((arrow (search "->" line)))
          (if (numberp arrow)
              `(def ,(intern (subseq line (+ arrow 2))
||#
