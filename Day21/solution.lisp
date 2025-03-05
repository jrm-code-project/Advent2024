;;; -*- Lisp -*-

(in-package "ADVENT2024/DAY21")

(defparameter *numeric-keypad* #2a(( 7  8  9)
                                   ( 4  5  6)
                                   ( 1  2  3)
                                   (nil 0  A)))

(defparameter *directional-keypad* #2a((nil |^| A)
                                       ( <  |v| >)))

(defun read-input (input-pathname)
  (collect 'list
    (#M(lambda (line)
         (collect 'list
           (#M(lambda (c)
                (or (digit-char-p c)
                    (intern (string c) (find-package "ADVENT2024/DAY21"))))
              (scan 'string line))))
       (scan-file input-pathname #'read-line))))

(defun key-coords (keypad key)
  (let ((coords (scan-grid-coords keypad)))
    (collect-first
     (choose
      (#Meql
       (#Mgrid-ref (series keypad) coords)
       (series key))
      coords))))

(defun jog-x (dx)
  (make-list (abs dx) :initial-element (if (minusp dx) '< '>)))

(defun jog-y (dy)
  (make-list (abs dy) :initial-element (if (minusp dy) '|^| '|v|)))

(defun valid-jog? (keypad from jog)
  (let iter ((current from)
             (jog jog))
    (cond ((null (grid-ref keypad current)) nil)
          ((null jog) t)
          (t (iter (ecase (car jog)
                     (|^| (coord-north current))
                     (|v| (coord-south current))
                     (>   (coord-east  current))
                     (<   (coord-west  current)))
               (cdr jog))))))

;; Returns a list of a list of directional keypresses that will transport you from
;; <from> to <to>.
(defun jog-xy (keypad from to)
  (let ((dx (jog-x (- (column to) (column from))))
        (dy (jog-y (- (row to) (row from)))))
    (cond ((null dx) (list dy))
          ((null dy) (list dx))
          (t (let ((column-first (append dx dy))
                   (row-first    (append dy dx)))
               (cond ((and (valid-jog? keypad from column-first)
                           (valid-jog? keypad from row-first))
                      (list column-first row-first))
                     ((valid-jog? keypad from column-first)
                      (list column-first))
                     (t (list row-first))))))))

#||
(jog-xy *numeric-keypad* (coord 0 0) (coord 0 2))
(jog-xy *numeric-keypad* (coord 0 0) (coord 2 0))
(jog-xy *numeric-keypad* (coord 0 0) (coord 2 2))
(jog-xy *numeric-keypad* (coord 0 0) (coord 2 3))
(jog-xy *numeric-keypad* (coord 1 3) (coord 0 2))
(jog-xy *numeric-keypad* (coord 0 2) (coord 1 3))
(jog-xy *directional-keypad* (coord 0 1) (coord 1 0))
(jog-xy *directional-keypad* (coord 1 0) (coord 0 1))

||#

(defun step-paths (keypad start-key end-key)
  (jog-xy keypad (key-coords keypad start-key) (key-coords keypad end-key)))

#||
(dolist (start '(0 1 2 3 4 5 6 7 8 9 a))
  (dolist (end '(0 1 2 3 4 5 6 7 8 9 a))
    (format t "~&~s, ~s: ~{~s~^, ~}~%" start end (step-paths *numeric-keypad* start end))))

(dolist (start '(a < > |v| |^|))
  (dolist (end '(a < > |v| |^|))
    (format t "~&~s, ~s: ~{~s~^, ~}~%" start end (step-paths *dirction-keypad* start end))))
||#

(defparameter seq-paths-cache (make-hash-table :test #'equal))

(defun preload-cache ()
  (clrhash seq-paths-cache)
  (setf 
   ;; Required preloads
   ;; Code does not work if these are not preloaded.
   (gethash '(|v| A)     seq-paths-cache) '(((< |v| A)   (^ > A)))
   (gethash '( <  A)     seq-paths-cache) '(((|v| < < A) (> > ^ A)))

   (gethash '(|^|  >  A) seq-paths-cache) '(((< A)     (|v| > A) (^ A)))
   (gethash '(|v|  >  A) seq-paths-cache) '(((< |v| A)     (> A) (^ A)))
   (gethash '( >  |^| A) seq-paths-cache) '(((|v| A)     (< ^ A) (> A)))
   (gethash '( <  |^| A) seq-paths-cache) '(((|v| < < A) (> ^ A) (> A)))
   (gethash '( <  |v| A) seq-paths-cache) '(((|v| < < A)   (> A) (^ > A)))

   (gethash '(|v|  <   <  A) seq-paths-cache) '(((< |v| A)     (< A)       (A) (> > ^ A)))
   (gethash '( >   >  |^| A) seq-paths-cache) '(((|v| A)         (A)   (< ^ A) (> A)))

   (gethash '( >  |v| |v| |v| A) seq-paths-cache) '(((|v| A)   (< A) (A)   (A) (^ > A)))
   (gethash '(|v| |v| |v|  >  A) seq-paths-cache) '(((< |v| A)   (A) (A) (> A) (^ A)))

   ;; Additional preloads.  Optional
   (gethash '(A)             seq-paths-cache) '(((A)))

   (gethash '(> A)           seq-paths-cache) '(((|v| A) (^ A)))
   (gethash '(^ A)           seq-paths-cache) '(((< A)   (> A)))

   (gethash '( ^   <  A)     seq-paths-cache) '(((< A)   (|v| < A) (> > ^ A)))
   (gethash '( >   >  A)     seq-paths-cache) '(((|v| A)       (A)     (^ A)))
   (gethash '( >  |v| A)     seq-paths-cache) '(((|v| A)     (< A)   (^ > A)))
   (gethash '(|v|  <  A)     seq-paths-cache) '(((< |v| A)   (< A) (> > ^ A)))
   (gethash '(|v| |v| A)     seq-paths-cache) '(((< |v| A)     (A)   (^ > A)))

   (gethash '(> ^ ^ A)       seq-paths-cache) '(((|v| A)     (< ^ A)       (A)     (> A)))
   (gethash '(< ^ ^ A)       seq-paths-cache) '(((|v| < < A) (> ^ A)       (A)     (> A)))
   (gethash '(^ ^ ^ A)       seq-paths-cache) '(((< A)           (A)       (A)     (> A)))
   (gethash '(^ ^ < A)       seq-paths-cache) '(((< A)           (A) (|v| < A) (> > ^ A)))
   (gethash '(^ ^ > A)       seq-paths-cache) '(((< A)           (A) (|v| > A)     (^ A)))
   (gethash '(^ < < A)       seq-paths-cache) '(((< A)     (|v| < A)       (A) (> > ^ A)))
   (gethash '(|v| |v| |v| A) seq-paths-cache) '(((< |v| A)       (A)       (A)   (^ > A)))

   (gethash '(^ ^ ^ < A)     seq-paths-cache) '(((< A)           (A)       (A) (|v| < A) (> > ^ A)))
   (gethash '(^ ^ < < A)     seq-paths-cache) '(((< A)           (A) (|v| < A)       (A) (> > ^ A)))
   (gethash '(^ ^ ^ < A)     seq-paths-cache) '(((< A)           (A)       (A) (|v| < A) (> > ^ A)))
   (gethash '(< ^ ^ ^ A)     seq-paths-cache) '(((|v| < < A) (> ^ A)       (A)       (A)     (> A)))
   ))
(preload-cache)

(defun seq-paths (keypad sequence)
  (if (eql keypad *numeric-keypad*)
      (seq-paths-1 keypad sequence)
      (let* ((key sequence)
             (probe (gethash sequence seq-paths-cache :not-found)))
        (if (eq probe :not-found)
            (let ((answer (seq-paths-1 keypad sequence)))
              (setf (gethash key seq-paths-cache answer) answer)
              answer)
            probe))))

(defun seq-paths-1 (keypad sequence)
  (cartesian-product-list
   (butlast (maplist (lambda (tail)
                       (cond ((null tail) nil)
                             ((null (cdr tail)) nil)
                             (t (revmap (lambda (jog)
                                          (append jog (list 'a)))
                                        (jog-xy keypad
                                                (key-coords keypad (first tail))
                                                (key-coords keypad (second tail)))))))
                     (cons 'a sequence)))))

(defun shortest-seq-tables (seq-tables)
  (let* ((sorted (sort seq-tables #'< :key #'seq-table-length))
         (shortest-length (seq-table-length (car sorted))))
    (remove-if-not (lambda (seq-table)
                     (= (seq-table-length seq-table) shortest-length))
                   sorted)))

(defun next-seq-tables (seq-table)
  (remove-duplicates (collapse-seq-tables (next-seq-tables-1 seq-table)) :test #'equal))

(defun collapse-seq-tables (seq-tables)
  (revmap #'collapse-seq-table seq-tables))

(defun symbol-lessp (left right)
  (string-lessp (symbol-name left) (symbol-name right)))

(defun term-lessp (left right)
  (or (and (null left) right)
      (and (null right) nil)
      (symbol-lessp (car left) (car right))
      (and (eql (car left) (car right))
           (term-lessp (cdr left) (cdr right)))))

(defun collapse-seq-table (seq-table)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry seq-table)
      (let ((key (car entry))
            (count (cdr entry)))
        (incf (gethash key table 0) count)))
    (sort (hash-table-alist table) #'term-lessp :key #'car)))

(defun next-seq-tables-1 (seq-table)
  (if (null seq-table)
      (list (list))
      (let ((tail-tables (next-seq-tables-1 (cdr seq-table))))
        (extend-seq-tables (car seq-table) tail-tables))))

(defun extend-seq-tables (entry tail-tables)
  (revmappend (lambda (tail-table)
             (extend-seq-table entry tail-table))
           tail-tables))

(defun extend-seq-table (entry tail-table)
  (revmap (lambda (path)
            (extend-with-path path (cdr entry) tail-table))
          (seq-paths *directional-keypad* (car entry))))

(defun extend-with-path (path count tail-table)
  (append (revmap (lambda (term) (cons term count)) path)
          tail-table))

(defun seq-table-length (seq-table)
  (reduce #'+ (map 'list (lambda (entry) (* (length (car entry)) (cdr entry))) seq-table)))

(defun initial-paths-table (numeric-seq)
  (map 'list (lambda (path)
                (let ((table (make-hash-table :test #'equal)))
                  (dolist (term path (alexandria:hash-table-alist table))
                    (incf (gethash term table 0)))))
       (seq-paths *numeric-keypad* numeric-seq)))

(defun generation-table (n numeric-seq)
  (if (zerop n)
      (initial-paths-table numeric-seq)
      (revmappend #'next-seq-tables (generation-table (1- n) numeric-seq))))

(defun shortest-table (sequence-tables)
  (car (sort sequence-tables #'< :key #'seq-table-length)))

(defun complexity (code n-generations)
    (* (seq-table-length (shortest-table (generation-table n-generations code)))
       (fold-left (lambda (acc digit)
                    (if (eql digit 'a)
                        acc
                        (+ (* acc 10) digit)))
                  0
                  code)))

(defun test-1 ()
  (reduce #'+ (map 'list (lambda (input) (complexity input 2)) (read-input (sample-input-pathname)))))

(defparameter +solution-1+ 188384)
(defparameter +solution-2+ 232389969568832)

(defun part-1 ()
  (reduce #'+ (map 'list (lambda (input) (complexity input 2)) (read-input (input-pathname)))))

(defun part-2 ()
  (reduce #'+ (map 'list (lambda (input) (complexity input 25)) (read-input (input-pathname)))))


#||

82050061710, 
72242026390, 
81251039228, 
80786362258, 
77985628636

85644066084,
86475783012,
87288844796,
93831469524,
90594397580,

||#
