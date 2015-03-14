
;;;======================================================
;;;   Tic Tac Toe Sample Problem
;;;
;;;     This is a rather simple tic tac toe expert
;;;     system. It will usually win if possible, but
;;;     won't always stop you from winning.
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

(deffacts initial-state
  (square 1 1 " " corner)
  (square 2 1 " " side)
  (square 3 1 " " corner)
  (square 1 2 " " side)
  (square 2 2 " " center)
  (square 3 2 " " side)
  (square 1 3 " " corner)
  (square 2 3 " " side)
  (square 3 3 " " corner))

;;;******************************************
;;; evaluate board for blocks, wins, and ties
;;;******************************************

(defrule evaluate-potential-win ""
  (evaluate board for ?)
  (square ?row1 ?col1 ?pl&~" " ?)
  (square ?row2 ?col2 " " ?)
  (test (or  (!= ?row1 ?row2) (!= ?col1 ?col2)))
  (square ?row3 ?col3 " " ?type)
  (test (and (or  (!= ?row1 ?row3) (!= ?col1 ?col3))
	            (or  (!= ?row2 ?row3) (!= ?col2 ?col3))))
  (test (or  (and (= ?row1 ?row2)
		                (= ?row1 ?row3))
	            (and (= ?col1 ?col2)
		                (= ?col1 ?col3))
	            (and (= (+ ?row1 ?col1) (+ ?row2 ?col2))
		                (= (+ ?row1 ?col1) (+ ?row3 ?col3)))
	            (and (= (- ?row1 ?col1) (- ?row2 ?col2))
		                (= (- ?row1 ?col1) (- ?row3 ?col3)))))
   =>
   (assert (potential-win ?row3 ?col3 ?type ?pl)))

(defrule evaluate-immediate-block-or-win ""
  (evaluate board for ?)
  (square ?row1 ?col1 ?pl&~" " ?)
  (square ?row2 ?col2 ?pl ?)
  (test (or (!= ?row1 ?row2) (!= ?col1 ?col2)))
  (square ?row3 ?col3 " " ?type)
  (test (and (or  (!= ?row1 ?row3) (!= ?col1 ?col3))
	            (or  (!= ?row2 ?row3) (!= ?col2 ?col3))))
  (test (or  (and (= ?row1 ?row2)
		                (= ?row1 ?row3))
	            (and (= ?col1 ?col2)
		                (= ?col1 ?col3))
	            (and (= (+ ?row1 ?col1) (+ ?row2 ?col2))
		                (= (+ ?row1 ?col1) (+ ?row3 ?col3)))
	            (and (= (- ?row1 ?col1) (- ?row2 ?col2))
		                (= (- ?row1 ?col1) (- ?row3 ?col3)))))
   =>
   (assert (immediate-block-or-win ?row3 ?col3 ?type ?pl)))

(defrule tie-game ""
  ?phase <- (evaluate board for ?)
  (not (square ? ? " " ?))
  =>
  (retract ?phase)
  (printout t "The game has ended in a tie." t))

;;;*****************************
;;; rules for the computers move
;;;*****************************

(defrule move-computer-to-win
   ?phase <- (computer move)
   (computer is ?cs)
   ?dummy <- (immediate-block-or-win ?row ?col ?type ?cs)
   ?square <- (square ?row ?col ? ?)
   =>
   (assert (display board))
   (retract ?dummy ?phase ?square)
   (assert (square ?row ?col ?cs ?type))
   (printout t "Computer moves to win!" t))

(defrule move-computer-to-block ""
  ?phase <- (computer move)
  (computer is ?cs)
  (not (immediate-block-or-win ? ? ? ?cs))
  (human is ?hs)
  ?dummy <- (immediate-block-or-win ?row ?col ?type ?hs)
  ?square <- (square ?row ?col ? ?)
  =>
  (retract ?dummy ?phase ?square)
  (assert (display board))
  (assert (square ?row ?col ?cs ?type))
  (assert (next-move human)))

(defrule move-to-center ""
   ?phase <- (computer move)
   (computer is ?cs)
   (not (immediate-block-or-win ? ? ? ?))
   ?square <- (square ? ? " " center)
   =>
   (retract ?phase ?square)
   (assert (display board))
   (assert (square 2 2 ?cs center))
   (assert (next-move human)))

(defrule move-to-corner ""
   (computer is ?cs)
   ?phase <- (computer move)
   (not (immediate-block-or-win ? ? ? ?)) 
   ?square <- (square ?row ?col " " corner)
   (not (square ? ? " " center))
   =>
   (retract ?phase ?square)
   (assert (display board))
   (assert (square ?row ?col ?cs corner))
   (assert (next-move human)))

(defrule move-to-side ""
   ?phase <- (computer move)
   (computer is ?cs)
   (not (immediate-block-or-win ? ? ? ?)) 
   ?square <- (square ?row ?col " " side) 
   (not (square ? ? " " corner))
   (not (square ? ? " " center))
   =>
   (retract ?phase ?square)
   (assert (display board))
   (assert (square ?row ?col ?cs side))
   (assert (next-move human)))

;;;*******************************************
;;; Get human move and determine validity
;;;*******************************************

(defrule human-input ""
  (human move)
  =>
  (printout t "Column? ")
  (bind ?col (read))
  (printout t "Row? ")
  (bind ?row (read))
  (printout t crlf)
  (assert (human-to ?col ?row)))

(defrule valid-human-move ""
  ?phase <- (human move)
  (human is ?hs)
  ?move  <- (human-to ?row ?col)
  ?square <- (square ?row ?col " " ?type)
  =>
  (assert (display board))
  (retract ?phase ?move ?square)
  (assert (square ?row ?col ?hs ?type))
  (assert (valid-move ?row ?col)))

(defrule human-winning-move ""
  ?move <- (valid-move ?row ?col)
  (human is ?hs)
  ?info <- (immediate-block-or-win ?row ?col ?type ?hs)
  =>
  (retract ?move ?info)
  (printout t "You have made the winning move!" t))

(defrule human-blocking-move ""
  ?move <- (valid-move ?row ?col)
  (computer is ?cs)
  ?info <- (immediate-block-or-win ? ? ?type ?cs)
  =>
  (retract ?info ?move)
  (assert (next-move computer)))

(defrule no-block-or-win-for-human ""
  ?move <- (valid-move ?row ?col)
  (not (immediate-block-or-win ? ? ? ?))
  =>
  (retract ?move)
  (assert (next-move computer)))

(defrule invalid-human-move ""
  ?phase <- (human move)
  ?move  <- (human-to ?row ?col)
  (square ?row ?col ~" " ?)
  =>
  (printout t "Invalid move. Try another." t)
  (retract ?phase ?move)
  (assert (human move)))

;;;******************************
;;; cleanup rules
;;;******************************

(defrule cleanup-1 ""
  (cleanup for ?)
  ?trash <- (immediate-block-or-win ? ? ? ?)
  =>
  (retract ?trash))

(defrule cleanup-2 ""
  (cleanup for ?)
  ?trash <- (potential-win ? ? ? ?)
  =>
  (retract ?trash))

;;;******************************
;;; phase control rules
;;;******************************

(defrule setup ""
  =>
  (printout t "Should the computer move first? ")
  (bind ?resp (read))
  (printout t t)
  (if (or (eq ?resp yes) (eq ?resp y))
      then
      (assert (computer is o))
      (assert (human is x))
      (assert (evaluate board for computer))
      else
      (assert (computer is x))
      (assert (human is o))
      (assert (evaluate board for human))))

(defrule cleanup-for-player ""
  (declare (salience -10))
  ?phase <- (next-move ?player)
  =>
  (retract ?phase)
  (assert (cleanup for ?player)))

(defrule switch-to-evaluate ""
  (declare (salience -10))
  ?phase <- (cleanup for ?player)
  =>
  (retract ?phase)
  (assert (evaluate board for ?player)))

(defrule switch-to-move ""
  (declare (salience -10))
  ?phase <- (evaluate board for ?player)
  =>
  (retract ?phase)
  (printout t "*********" t)
  (printout t ?player " move" t)
  (printout t "*********" t)
  (assert (?player move)))

(defrule board ""
  ?phase <- (display board)
  (square 1 1 ?p1-1 ?)
  (square 2 1 ?p2-1 ?)
  (square 3 1 ?p3-1 ?)
  (square 1 2 ?p1-2 ?)
  (square 2 2 ?p2-2 ?)
  (square 3 2 ?p3-2 ?)
  (square 1 3 ?p1-3 ?)
  (square 2 3 ?p2-3 ?)
  (square 3 3 ?p3-3 ?)
  =>
  (retract ?phase)
  (format t "%nBoard:%n%n")
  (format t " %s|%s|%s%n" ?p1-1 ?p2-1 ?p3-1)
  (format t " -----%n")
  (format t " %s|%s|%s%n" ?p1-2 ?p2-2 ?p3-2)
  (format t " -----%n")
  (format t " %s|%s|%s%n%n" ?p1-3 ?p2-3 ?p3-3))
