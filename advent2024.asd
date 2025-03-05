;;; -*- Lisp -*-

(defsystem "advent2024"
  :depends-on ("alexandria" "cl-ppcre" "fold" "function" "named-let" "series" "str" "wtree")
  :components ((:file "Day1/solution"    :depends-on ("macros" "package" "series-extra"))
               (:file "Day2/solution"    :depends-on ("macros" "package" "series-extra"))
               (:file "Day3/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day4/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day5/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day6/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day7/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day8/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day9/solution"    :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day10/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day11/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day12/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day13/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day14/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day15/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day16/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day17/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day18/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day19/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day20/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day21/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day22/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day23/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day24/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "Day25/solution"   :depends-on ("macros" "misc" "package" "series-extra"))
               (:file "initialize"       :depends-on ("package"))
               (:file "macros"           :depends-on ("package" "series-extra"))
               (:file "misc"             :depends-on ("macros" "package" "series-extra"))
               (:file "package")
               (:file "series-extra")
               (:file "validate"         :depends-on ("package"
                                                      "Day1/solution"
                                                      "Day2/solution"
                                                      "Day3/solution"
                                                      "Day4/solution"
                                                      "Day5/solution"
                                                      "Day6/solution"
                                                      "Day7/solution"
                                                      "Day8/solution"
                                                      "Day9/solution"
                                                      "Day10/solution"
                                                      "Day11/solution"
                                                      "Day12/solution"
                                                      "Day13/solution"
                                                      "Day14/solution"
                                                      "Day15/solution"
                                                      "Day16/solution"
                                                      "Day17/solution"
                                                      "Day18/solution"
                                                      "Day19/solution"
                                                      "Day20/solution"
                                                      "Day21/solution"
                                                      "Day22/solution"
                                                      "Day23/solution"
                                                      "Day24/solution"
                                                      "Day25/solution"
                                                      ))
               ))
