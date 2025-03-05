;;; -*- Lisp -*-

(defpackage "ADVENT2024"
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "REMOVE-FROM-PLIST"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export
   "+NORTH+"
   "+SOUTH+"
   "+EAST+"
   "+WEST+"
   "2V*"
   "2V+"
   "2V-"
   "2V-MOD"
   "ADVENT-PATHNAME"
   "CARTESIAN-PRODUCT-LIST"
   "CHAR->DECIMAL"
   "CHAR-INTERNER"
   "COLUMN"
   "COORD"
   "COORD-EAST"
   "COORD-NORTH"
   "COORD-NORTHEAST"
   "COORD-NORTHWEST"
   "COORD-SOUTH"
   "COORD-SOUTHEAST"
   "COORD-SOUTHWEST"
   "COORD-WEST"
   "CRAMERS-RULE"
   "FLATMAP"
   "GRID-HEIGHT"
   "GRID-REF"
   "GRID-ROW"
   "GRID-WIDTH"
   "INTEGER-LOG"
   "INVERT-GRID"
   "MAKE-GRID"
   "MAKE-SIMPLE-VECTOR"
   "MAP-CONS"
   "MAP-PAIRS"
   "MULTI-MAP-CONS"
   "OCOORD"
   "OCOORD-ADVANCE"
   "OCOORD-CCW"
   "OCOORD-COORD"
   "OCOORD-CW"
   "OCOORD-ORIENTATION"
   "ON-GRID?"
   "READ-FILE-INTO-GRID"
   "REVMAP"
   "REMAP"
   "REMOVE-ONE-ELEMENT"
   "REVMAP-CONS"
   "REVMAPPEND"
   "ROW"
   "SCAN-2D-COORDS"
   "SCAN-COLUMNS"
   "SCAN-COORDS"
   "SCAN-FALLING-DIAGONALS"
   "SCAN-GRID"
   "SCAN-GRID-COORDS"
   "SCAN-RISING-DIAGONALS"
   "SCAN-ROWS"
   "SCAN-SUFFIXES"
   "VALIDATE")
  (:use "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY1"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY2"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY3"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY4"
  (:shadow "VALIDATE")
  (:shadow "GRID-ROW")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY5"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY6"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY7"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY8"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY9"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY10"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY11"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY12"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY13"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY14"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY15"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY16"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )
  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY17"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY18"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY19"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY20"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY21"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY22"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY23"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY24"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))

(defpackage "ADVENT2024/DAY25"
  (:shadow "VALIDATE")
  (:import-from "ALEXANDRIA"
                "FLATTEN"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-VALUES"
                "LENGTH="
                "MAPPEND"
                "MAP-PERMUTATIONS"
                "MAP-PRODUCT"
                "READ-FILE-INTO-STRING"
                "STARTS-WITH-SUBSEQ"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )

  (:export "PART-1" "PART-2" "+SOLUTION-1+" "+SOLUTION-2+" "VALIDATE")
  (:use "ADVENT2024" "COMMON-LISP" "FOLD" "FUNCTION" "NAMED-LET" "SERIES"))
