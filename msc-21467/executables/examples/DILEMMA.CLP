
;;;======================================================
;;;   Farmer's Dilemma Problem
;;;
;;;     Another classic AI problem (cannibals and the 
;;;     missionary) in agricultural terms. The point is
;;;     to get the farmer, the fox, the cabbage, and the
;;;     goat across a stream.
;;;     But the boat only holds 2 items. If left 
;;;     alone with the goat, the fox will eat it. If
;;;     left alone with the cabbage, the goat will eat
;;;     it.
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;;****************************
;;;* Farmer's Dilemma Problem *
;;;****************************

;;; The status facts hold the state information of the search tree.

(deftemplate status
   (field search-depth)
   (field id)
   (field parent)
   (field farmer-location)
   (field fox-location)
   (field goat-location)
   (field cabbage-location)
   (field previous-move))

;;; The moves facts hold the information of all the moves made to
;;; reach a given state.

;;;*****************
;;;* Initial State *
;;;*****************

(deffacts initial-positions
  (status (search-depth 1)
          (id initial-setup)
          (parent no-parent)
          (farmer-location shore-1) 
          (fox-location shore-1)
          (goat-location shore-1)
          (cabbage-location shore-1)
          (previous-move no-move)))

(deffacts opposites
  (opposite-of shore-1 shore-2)
  (opposite-of shore-2 shore-1))

;;;************************
;;;* Generate Paths Rules *
;;;************************

(defrule move-alone ""
  (status (search-depth ?num)
          (id ?name)
          (farmer-location ?fs)
          (fox-location ?xs)
          (goat-location ?gs)
          (cabbage-location ?cs))
  (opposite-of ?fs ?ns)
  =>
  (assert (status (search-depth =(+ 1 ?num))
                  (id =(gensym))
                  (parent ?name)
                  (farmer-location ?ns)
                  (fox-location ?xs)
                  (goat-location ?gs)
                  (cabbage-location ?cs)
                  (previous-move alone))))

(defrule move-with-fox ""
  (status (search-depth ?num)
          (id ?name)
          (farmer-location ?fs)
          (fox-location ?fs)
          (goat-location ?gs)
          (cabbage-location ?cs))
  (opposite-of ?fs ?ns)
  =>
  (assert (status (search-depth =(+ 1 ?num))
                  (id =(gensym))
                  (parent ?name)
                  (farmer-location ?ns)
                  (fox-location ?ns)
                  (goat-location ?gs)
                  (cabbage-location ?cs)
                  (previous-move fox))))

(defrule move-with-goat ""
  (status (search-depth ?num)
          (id ?name)
          (farmer-location ?fs)
          (fox-location ?xs)
          (goat-location ?fs)
          (cabbage-location ?cs))
  (opposite-of ?fs ?ns)
  =>
  (assert (status (search-depth =(+ 1 ?num))
                  (id =(gensym))
                  (parent ?name)
                  (farmer-location ?ns)
                  (fox-location ?xs)
                  (goat-location ?ns)
                  (cabbage-location ?cs)
                  (previous-move goat))))

(defrule move-with-cabbage ""
  (status (search-depth ?num)
          (id ?name)
          (farmer-location ?fs)
          (fox-location ?xs)
          (goat-location ?gs)
          (cabbage-location ?fs))
  (opposite-of ?fs ?ns)
  =>
  (assert (status (search-depth =(+ 1 ?num))
                  (id =(gensym))
                  (parent ?name)
                  (farmer-location ?ns)
                  (fox-location ?xs)
                  (goat-location ?gs)
                  (cabbage-location ?ns)
                  (previous-move cabbage))))

;;;******************************
;;;* Constraint Violation Rules *
;;;******************************

(defrule fox-eats-goat ""
  (declare (salience 10000))
  ?rm <- (status (farmer-location ?s1)
                 (fox-location ?s2&~?s1)
                 (goat-location ?s2))
  =>
  (retract ?rm))

(defrule goat-eats-cabbage ""
  (declare (salience 10000))
  ?rm <- (status (farmer-location ?s1)
                 (goat-location ?s2&~?s1)
                 (cabbage-location ?s2))
  =>
  (retract ?rm))

(defrule circular-path ""
  (declare (salience 10000))
  (status (search-depth ?nm)
          (farmer-location ?fs)
          (fox-location ?xs)
          (goat-location ?gs)
          (cabbage-location ?cs))
  ?rm <- (status (search-depth ?nm1&:(< ?nm ?nm1))
                 (farmer-location ?fs)
                 (fox-location ?xs)
                 (goat-location ?gs)
                 (cabbage-location ?cs))
  =>
  (retract ?rm))

;;;********************************
;;;* Find and Print Solution Rule *
;;;********************************

(defrule recognize-solution ""
  (declare (salience 5000))
  ?rm <- (status (parent ?parent)
                 (farmer-location shore-2)
                 (fox-location shore-2)
                 (goat-location shore-2)
                 (cabbage-location shore-2)
                 (previous-move ?move))
  =>
  (retract ?rm)
  (assert (moves ?parent ?move)))

(defrule further-solution ""
  (declare (salience 5000))
  ?mv <- (moves ?name $?rest)
  (status (id ?name)
          (parent ?parent)
          (previous-move ?move))
  =>
  (retract ?mv)
  (assert (moves ?parent ?move $?rest)))

(defrule print-solution ""
  (declare (salience 5000))
  ?mv <- (moves no-parent no-move $?m)
  =>
  (retract ?mv)
  (printout t t  "Solution found: " t t)
  (bind ?length (length $?m))
  (bind ?i 1)
  (bind ?shore shore-2)
  (while (<= ?i ?length)
     (bind ?thing (nth ?i $?m))
     (if (eq ?thing alone)
        then (printout t "Farmer moves alone to " ?shore "." t)
        else (printout t "Farmer moves with " ?thing " to " ?shore "." t))
     (if (eq ?shore shore-1)
        then (bind ?shore shore-2)
        else (bind ?shore shore-1))
     (bind ?i (+ 1 ?i))))
