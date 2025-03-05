;;; -*- Lisp -*-

(in-package "ADVENT2024")

(deftype grid-index ()
  `(integer ,(- array-dimension-limit) (,array-dimension-limit)))

(deftype coord ()
  '(cons (grid-index) (grid-index)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun coord (column row)
  (check-type column grid-index)
  (check-type row grid-index)
  (cons column row))
)
(define-compiler-macro coord (column row)
  `(cons ,column ,row))

(defun column (coord)
  (check-type coord coord)
  (car coord))

(define-compiler-macro column (coord)
  `(car ,coord ))

(defun row (coord)
  (check-type coord coord)
  (cdr coord))

(define-compiler-macro row (coord)
  `(cdr ,coord))

(defun scan-coords (row-count col-count)
  (declare (optimizable-series-function))
  (multiple-value-bind (rows cols)
      (map-fn '(values grid-index grid-index)
              #'floor
              (scan-range :below (* row-count col-count))
              (series col-count))
    (declare (type (series grid-index) rows cols))
    (map-fn 'coord #'coord cols rows)))

(deftype grid () `(array atom 2))

(defun make-grid (height width &rest keys)
  (apply #'make-array (list height width) keys))

(defun row-list->grid (row-list)
  (make-grid (length row-list) (length (first row-list)) :initial-contents row-list))

(defun grid-height (grid)
  (check-type grid grid)
  (array-dimension grid 0))

(define-compiler-macro grid-height (grid)
  `(array-dimension ,grid 0))

(defun grid-width (grid)
  (check-type grid grid)
  (array-dimension grid 1))

(define-compiler-macro grid-width (grid)
  `(array-dimension ,grid 1))

(defun on-grid? (grid coord)
  (and (>= (column coord) 0)
       (< (column coord) (grid-width grid))
       (>= (row coord) 0)
       (< (row coord) (grid-height grid))))

(defun grid-ref (grid coord)
  (check-type grid grid)
  (check-type coord coord)
  (aref grid (row coord) (column coord)))

(define-compiler-macro grid-ref (grid coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (aref ,grid (row ,coord-var) (column ,coord-var)))))

(defsetf grid-ref (grid coord) (value)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (setf (aref ,grid (row ,coord-var) (column ,coord-var)) ,value))))

(defun grid-row (grid row-number)
  (check-type grid grid)
  (make-array (list (grid-width grid))
              :displaced-to grid
              :displaced-index-offset (array-row-major-index grid row-number 0)))

(defun grid-column (grid columm-number)
  (check-type grid grid)
  (let ((answer (make-simple-vector (grid-height grid))))
    (dotimes (row (grid-height grid) answer)
      (setf (svref answer row)
            (grid-ref grid (coord columm-number row))))))

(defun grid-falling-diagonal (grid diagonal-number)
  (check-type grid grid)
  (let ((start-column (if (minusp diagonal-number)
                          0
                          diagonal-number))
        (start-row (if (minusp diagonal-number)
                       (- diagonal-number)
                       0)))
    (let ((limit (min (- (grid-width grid) start-column)
                      (- (grid-height grid) start-row))))
      (let ((answer (make-simple-vector limit)))
        (do ((row    start-row    (+ row 1))
             (column start-column (+ column 1))
             (index 0 (+ index 1)))
            ((>= index limit) answer)
          (setf (svref answer index)
                (grid-ref grid (coord column row))))))))

(defun grid-rising-diagonal (grid diagonal-number)
  (check-type grid grid)
  (let ((start-column (if (minusp diagonal-number)
                          (- diagonal-number)
                          0))
        (start-row (if (minusp diagonal-number)
                       (1- (grid-height grid))
                       (- (grid-height grid) diagonal-number 1))))
    (let ((limit (min (- (grid-width grid) start-column)
                      (1+ start-row))))
      (let ((answer (make-simple-vector limit)))
        (do ((row    start-row    (- row 1))
             (column start-column (+ column 1))
             (index 0 (+ index 1)))
            ((>= index limit) answer)
          (setf (svref answer index)
                (grid-ref grid (coord column row))))))))

(defun scan-rows (grid)
  (declare (optimizable-series-function))
  (map-fn 'vector #'grid-row (series grid) (scan-range :below (grid-height grid))))

(defun scan-columns (grid)
  (declare (optimizable-series-function))
  (map-fn 'vector #'grid-column (series grid) (scan-range :below (grid-width grid))))

(defun scan-falling-diagonals (grid)
  (declare (optimizable-series-function))
  (map-fn 'vector
          #'grid-falling-diagonal
          (series grid)
          (scan-range :from (1+ (- (grid-height grid))) :below (grid-width grid))))

(defun scan-rising-diagonals (grid)
  (declare (optimizable-series-function))
  (map-fn 'vector
          #'grid-rising-diagonal
          (series grid)
          (scan-range :from (- 1 (grid-width grid)) :below (grid-height grid))))

(deftype unit ()
  `(integer -1 1))

(deftype orientation ()
  '(cons (unit) (unit)))

(defun unit-vector (column row)
  (check-type column unit)
  (check-type row unit)
  (cons column row))

(defparameter +north+ (unit-vector 0 -1))
(defparameter +northeast+ (unit-vector 1 -1))
(defparameter +east+  (unit-vector 1 0))
(defparameter +southeast+ (unit-vector 1 1))
(defparameter +south+ (unit-vector 0 1))
(defparameter +southwest+ (unit-vector -1 1))
(defparameter +west+  (unit-vector -1 0))
(defparameter +northwest+ (unit-vector -1 -1))

(defun 2v- (left right)
  (check-type left coord)
  (check-type right coord)
  (coord (- (column left) (column right))
         (- (row left) (row right))))

(define-compiler-macro 2v- (left right)
  (let ((left-var (gensym "LEFT-"))
        (right-var (gensym "RIGHT-")))
    `(let ((,left-var ,left)
           (,right-var ,right))
       (coord (- (column ,left-var) (column ,right-var))
              (- (row ,left-var) (row ,right-var))))))

(defun 2v+ (left right)
  (check-type left coord)
  (check-type right coord)
  (coord (+ (column left) (column right))
         (+ (row left) (row right))))

(define-compiler-macro 2v+ (left right)
  (let ((left-var (gensym "LEFT-"))
        (right-var (gensym "RIGHT-")))
    `(let ((,left-var ,left)
           (,right-var ,right))
       (coord (+ (column ,left-var) (column ,right-var))
              (+ (row ,left-var) (row ,right-var))))))

(defun 2v* (scalar vector)
  (check-type scalar integer)
  (check-type vector coord)
  (coord (* scalar (column vector))
         (* scalar (row vector))))

(define-compiler-macro 2v* (scalar vector)
  (let ((left-var (gensym "SCALAR-"))
        (right-var (gensym "VECTOR-")))
    `(let ((,left-var ,scalar)
           (,right-var ,vector))
       (coord (* ,left-var (column ,right-var))
              (* ,left-var (row ,right-var))))))

(defun 2v-mod (vector modulus)
  (check-type vector coord)
  (check-type modulus coord)
  (coord (mod (column vector) (column modulus))
         (mod (row vector) (row modulus))))

(define-compiler-macro 2v-mod (vector modulus)
  (let ((left-var (gensym "VECTOR-"))
        (right-var (gensym "MODULUS-")))
    `(let ((,left-var ,vector)
           (,right-var ,modulus))
       (coord (mod (column ,left-var) (column ,right-var))
              (mod (row ,left-var) (row ,right-var))))))

(defun coord-northwest (coord)
  (check-type coord coord)
  (2v+ coord +northwest+))

(define-compiler-macro coord-northwest (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1- (column ,coord-var)) (1- (row ,coord-var))))))

(defun coord-north (coord)
  (check-type coord coord)
  (2v+ coord +north+))

(define-compiler-macro coord-north (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (column ,coord-var) (1- (row ,coord-var))))))

(defun coord-northeast (coord)
  (check-type coord coord)
  (2v+ coord +northeast+))

(define-compiler-macro coord-northeast (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1+ (column ,coord-var)) (1- (row ,coord-var))))))

(defun coord-east (coord)
  (check-type coord coord)
  (2v+ coord +east+))

(define-compiler-macro coord-east (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1+ (column ,coord-var)) (row ,coord-var)))))

(defun coord-southeast (coord)
  (check-type coord coord)
  (2v+ coord +southeast+))

(define-compiler-macro coord-southeast (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1+ (column ,coord-var)) (1+ (row ,coord-var))))))

(defun coord-south (coord)
  (check-type coord coord)
  (2v+ coord +south+))

(define-compiler-macro coord-south (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (column ,coord-var) (1+ (row ,coord-var))))))

(defun coord-southwest (coord)
  (check-type coord coord)
  (2v+ coord +southwest+))

(define-compiler-macro coord-southwest (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1- (column ,coord-var)) (1+ (row ,coord-var))))))

(defun coord-west (coord)
  (check-type coord coord)
  (2v+ coord +west+))

(define-compiler-macro coord-west (coord)
  (let ((coord-var (gensym "COORD-")))
    `(let ((,coord-var ,coord))
       (coord (1- (column ,coord-var)) (row ,coord-var)))))

(deftype ocoord ()
  '(cons coord orientation))

(defun ocoord (coord orientation)
  (check-type coord coord)
  (check-type orientation orientation)
  (cons coord orientation))

(define-compiler-macro ocoord (coord orientation)
  `(cons ,coord ,orientation))

(defun ocoord-coord (ocoord)
  (check-type ocoord ocoord)
  (car ocoord))

(define-compiler-macro ocoord-coord (ocoord)
  `(car ,ocoord))

(defun ocoord-orientation (ocoord)
  (check-type ocoord ocoord)
  (cdr ocoord))

(define-compiler-macro ocoord-orientation (ocoord)
  `(cdr ,ocoord))

(defun ocoord-advance (ocoord)
  (check-type ocoord ocoord)
  (ocoord (2v+ (ocoord-coord ocoord) (ocoord-orientation ocoord))
          (ocoord-orientation ocoord)))

(define-compiler-macro ocoord-advance (ocoord)
  (let ((ocoord-var (gensym "OCOORD-")))
    `(let ((,ocoord-var ,ocoord))
       (ocoord (2v+ (ocoord-coord ,ocoord-var) (ocoord-orientation ,ocoord-var))
               (ocoord-orientation ,ocoord-var)))))

(defun ocoord-cw (ocoord)
  (check-type ocoord ocoord)
  (ocoord (ocoord-coord ocoord)
          (cond ((equal (ocoord-orientation ocoord) +north+) +east+)
                ((equal (ocoord-orientation ocoord) +east+) +south+)
                ((equal (ocoord-orientation ocoord) +south+) +west+)
                ((equal (ocoord-orientation ocoord) +west+) +north+))))

(defun ocoord-ccw (ocoord)
  (check-type ocoord ocoord)
  (ocoord (ocoord-coord ocoord)
          (cond ((equal (ocoord-orientation ocoord) +north+) +west+)
                ((equal (ocoord-orientation ocoord) +east+) +north+)
                ((equal (ocoord-orientation ocoord) +south+) +east+)
                ((equal (ocoord-orientation ocoord) +west+) +south+))))

(defun scan-grid-coords (grid)
  (declare (optimizable-series-function))
  (scan-coords (grid-height grid) (grid-width grid)))

(defun scan-grid (grid)
  (declare (optimizable-series-function 2))
  (#2m(lambda (grid coord)
        (values coord (grid-ref grid coord)))
      (series grid)
      (scan-coords (grid-height grid) (grid-width grid))))

(defun invert-grid (grid &optional initial-value)
  (if initial-value
      (multiple-value-bind (coords vals) (scan-grid grid)
        (collect-hash-push-except vals coords (list initial-value)))
      (multiple-value-bind (coords vals) (scan-grid grid)
        (collect-hash-push vals coords))))

(defun char->decimal (char)
  (- (char-code char) (char-code #\0)))

(defun char-interner (char-folder package)
  (lambda (char)
    (if (digit-char-p char)
        (char->decimal char)
        (intern (string (funcall char-folder char)) package))))

(defun string-mapper (char-fn)
  "Returns a function that maps strings to lists."
  (lambda (line)
    (collect 'list
      (map-fn 't
        char-fn
        (scan 'string line)))))

(defun read-file-into-grid (char-fn filename)
  "Returns the contents of the file as a two-dimensional array."
  (row-list->grid
   (collect 'list
     (map-fn 'list
             (string-mapper char-fn)
             (scan-file filename #'read-line)))))

(defun advent-pathname (pathname)
  (merge-pathnames pathname
                   (asdf/system:system-source-directory "advent2024")))

(defun map-cons (car cdrs)
  (map 'list (lambda (cdr) (cons car cdr)) cdrs))

(defun cartesian-product (&rest lists)
  (cartesian-product-list lists))

(defun cartesian-product-list (lists)
  (fold-left (lambda (tails terms)
               (mappend (lambda (term)
                          (map-cons term tails))
                        terms))
             (list nil)
             (reverse lists)))

(defun integer-log (n base)
  "Returns two values, the integer-log of <n> in <base>, and the leftmost digit
in <base>."
  (if (< n base)
      (values 0 n)
      (multiple-value-bind (ilog l) (integer-log n (* base base))
        (if (< l base)
            (values (* ilog 2) l)
            (values (+ (* ilog 2) 1) (/ l base))))))

(defun make-simple-vector (length &rest keyargs &key (initial-element nil) (initial-contents nil))
  (declare (ignore initial-element initial-contents))
  (apply #'make-array (list length) keyargs))

(defun map-pairs (f list)
  (fold-left #'append nil
             (maplist (lambda (tail)
                        (map 'list
                             (lambda (second)
                               (funcall f (first tail) second))
                             (rest tail)))
                      list)))

(defun revmap (func list)
  (do ((tail list (cdr tail))
       (result nil (cons (funcall func (car tail)) result)))
      ((null tail) result)))

(defun revmap-cons (car cdrs)
  (fold-left (lambda (result cdr)
               (cons (cons car cdr) result))
             nil
             cdrs))

(defun revmappend (func list)
  (do ((tail list (cdr tail))
       (result nil (do ((subtail (funcall func (car tail)) (cdr subtail))
                        (result* result (cons (car subtail) result*)))
                       ((null subtail) result*))))
      ((null tail) result)))

(defun remove-one-element (list)
  (cond ((consp list) (cons (cdr list)
                            (map-cons (car list) (remove-one-element (cdr list)))))
        ((null list) nil)
        (t (error "Dotted tail."))))

(defun scan-suffixes (seq &optional (include-empty? t) (proper? nil))
  (declare (optimizable-series-function))
  (scan-fn-inclusive t
           (lambda () (if proper? (subseq seq 1) seq))
           (lambda (seq) (subseq seq 1))
           (lambda (seq) (length= (if include-empty? 0 1) seq))))

