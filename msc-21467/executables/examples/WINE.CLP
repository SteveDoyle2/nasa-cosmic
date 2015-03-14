
;;;======================================================
;;;   Wine Expert Sample Problem
;;;
;;;     WINEX: The WINe EXpert system.
;;;     This example selects an appropriate wine
;;;     to drink with a meal.
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

(deffacts valid-combinations ""
  (combine best-body)
  (combine best-color)
  (combine best-sweetness)
  (combine recommended-body)
  (combine recommended-color)
  (combine recommended-sweetness)
  (combine wine))

(deffacts question-info ""
  (values-for main-component meat fish poultry unknown)
  (values-for has-turkey yes no unknown)
  (values-for has-veal yes no unknown)
  (values-for has-sauce yes no unknown)
  (values-for sauce spicy sweet cream tomato unknown)
  (values-for tastiness delicate average strong unknown)
  (values-for preferred-body light medium full unknown)
  (values-for preferred-sweetness dry medium sweet unknown)
  (values-for preferred-color red white unknown)
  (values-for preferred-body light medium full unknown))
  
;;*******************************
;;* CHOOSE WINE QUALITIES RULES *
;;*******************************

(defrule choose-body-for-spicy-sauce ""
  (choose-qualities)
  (has-sauce yes)
  (sauce spicy)
  =>
  (assert (best-body full 100 =(gensym))))

(defrule choose-body-for-delicate-taste "" 
  (choose-qualities)
  (tastiness delicate)
  =>
  (assert (best-body light 100 =(gensym))))

(defrule choose-body-for-average-taste ""
  (choose-qualities)
  (tastiness average)
  =>
  (assert (best-body light 30 =(gensym)))
  (assert (best-body medium 60 =(gensym)))
  (assert (best-body full 30 =(gensym))))

(defrule choose-body-for-strong-taste ""
  (choose-qualities)
  (tastiness strong)
  =>
  (assert (best-body medium 40 =(gensym)))
  (assert (best-body full 80 =(gensym))))

(defrule choose-body-for-cream-sauce ""
  (choose-qualities)
  (has-sauce yes)
  (sauce cream)
  =>
  (assert (best-body medium 40 =(gensym)))
  (assert (best-body full 60 =(gensym))))

(defrule choose-color-for-meat ""
  (choose-qualities)
  (main-component meat)
  (has-veal no)
  =>
  (assert (best-color red 90 =(gensym))))

(defrule choose-color-for-poultry ""
  (choose-qualities)
  (main-component poultry)
  (has-turkey no)
  =>
  (assert (best-color white 90 =(gensym)))
  (assert (best-color red 30 =(gensym))))

(defrule choose-color-for-fish ""
  (choose-qualities)
  (main-component fish)
  =>
  (assert (best-color white 100 =(gensym))))

(defrule choose-color-for-tomato-sauce ""
  (choose-qualities)
  (not (main-component fish))
  (has-sauce yes)
  (sauce tomato)
  =>
  (assert (best-color red 100 =(gensym))))

(defrule choose-color-for-turkey ""
  (choose-qualities)
  (main-component poultry)
  (has-turkey yes)
  =>
  (assert (best-color red 80 =(gensym)))
  (assert (best-color white 50 =(gensym))))

(defrule choose-color-for-cream-sauce ""
  (choose-qualities)
  (main-component unknown)
  (has-sauce yes)
  (sauce cream)
  =>
  (assert (best-color white 40 =(gensym))))

(defrule choose-color-for-sweet-sauce ""
  (choose-qualities)
  (has-sauce yes)
  (sauce sweet)
  =>
  (assert (best-sweetness sweet 90 =(gensym)))
  (assert (best-sweetness medium 40 =(gensym))))

(defrule spicy-sauce-is-spicy-feature ""
  (choose-qualities)
  (has-sauce yes)
  (sauce spicy)
  =>
  (assert (feature spiciness)))

(defrule best-body-always-recommended ""
  (recommend-qualities)
  (best-body ?body ?per ?)
  =>
  (assert (recommended-body ?body ?per =(gensym))))

(defrule preferred-body-may-be-recommended ""
  (recommend-qualities)
  (preferred-body ?body)
  (best-body ?body ?per ?)
  =>
  (assert (recommended-body ?body =(/ (* 20 ?per) 100) =(gensym))))

(defrule recommend-medium-body-1 ""
  (recommend-qualities)
  (preferred-body light)
  (best-body full ?per ?)
  =>
  (assert (recommended-body medium ?per =(gensym))))

(defrule recommend-medium-body-2 ""
  (recommend-qualities)
  (preferred-body full)
  (best-body light ?per ?)
  =>
  (assert (recommended-body medium ?per =(gensym))))

(defrule best-color-always-recommended ""
  (recommend-qualities)
  (best-color ?color ?per ?)
  =>
  (assert (recommended-color ?color ?per =(gensym))))

(defrule preferred-color-may-be-recommended
  (recommend-qualities)
  (preferred-color ?color)
  (best-color ?color ?per ?)
  =>
  (assert (recommended-color ?color =(/ (* 20 ?per) 100) =(gensym))))

(defrule preferred-color-is-unknown ""
  (recommend-qualities)
  (preferred-color unknown)
  =>
  (assert (recommended-color white 50 =(gensym)))
  (assert (recommended-color red 50 =(gensym))))

(defrule best-sweetness-always-recommended ""
  (recommend-qualities)
  (best-sweetness ?sweet ?per ?)
  =>
  (assert (recommended-sweetness ?sweet ?per =(gensym))))

(defrule preferred-sweetness-may-be-recommended ""
  (recommend-qualities)
  (best-sweetness ?sweet ?per ?)
  (preferred-sweetness ?sweet)
  =>
  (assert (recommended-sweetness ?sweet =(/ (* 20 ?per) 100) =(gensym))))

(defrule recommend-medium-sweetness-1 ""
  (recommend-qualities)
  (best-sweetness sweet ?per ?)
  (preferred-sweetness dry)
  =>
  (assert (recommended-sweetness medium ?per =(gensym))))

(defrule recommend-medium-sweetness-2 ""
  (recommend-qualities)
  (best-sweetness dry ?per ?)
  (preferred-sweetness sweet)
  =>
  (assert (recommended-sweetness medium ?per =(gensym))))

;;*************************************
;;* DEFAULT QUALITIES SELECTION RULES *
;;*************************************

(defrule use-prefered-body-if-no-best-body ""
  (default-qualities)
  (preferred-body ?body&~unknown)
  (not (best-body ? ? ?))
  =>
  (assert (recommended-body ?body 100 =(gensym))))    

(defrule use-medium-body-if-no-best-body ""
  (default-qualities)
  (not (best-body ? ? ?))
  =>
  (assert (recommended-body medium 100 =(gensym))))

(defrule use-preferred-color-if-no-best-color ""
  (default-qualities)
  (preferred-color ?color&~unknown)
  (not (best-color ? ? ?))
  =>
  (assert (recommended-color ?color 100 =(gensym))))

(defrule use-medium-sweetness-if-preference-unknown ""
  (default-qualities)
  (not (best-sweetness ? ? ?))
  (preferred-sweetness unknown)
  =>
  (assert (recommended-sweetness medium 100 =(gensym))))

(defrule use-preferred-sweetness-if-no-best-sweetness ""
  (default-qualities)
  (not (best-sweetness ? ? ?))
  (preferred-sweetness ?sweet&~unknown)
  =>
  (assert (recommended-sweetness ?sweet 100 =(gensym))))

;;************************
;;* WINE SELECTION RULES *
;;************************

(defrule recommend-gamay ""
  (select-wines)
  (recommended-color red ?per1 ?)
  (recommended-body medium ?per2 ?)
  (or (recommended-sweetness medium ?per3 ?)
      (recommended-sweetness sweet ?per3 ?))
  =>
  (assert (wine Gamay =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-chablis ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (recommended-body light ?per2 ?)
  (recommended-sweetness dry ?per3 ?)
  =>
  (assert (wine Chablis =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-sauvignon-blanc ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (recommended-body medium ?per2 ?)
  (recommended-sweetness dry ?per3 ?)
  =>
  (assert (wine Sauvignon-Blanc =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-chardonnay ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (or (recommended-body  medium ?per2 ?)
      (recommended-body  full ?per2 ?))
  (or (recommended-sweetness medium ?per3 ?)
      (recommended-sweetness dry ?per3 ?))
  =>
  (assert (wine Chardonnay =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-soave ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (recommended-body light ?per2 ?)
  (or (recommended-sweetness medium  ?per3 ?)
      (recommended-sweetness dry ?per3 ?))
  =>
  (assert (wine Soave =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-riesling ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (or (recommended-body light ?per2 ?)
      (recommended-body medium ?per2 ?))
  (or (recommended-sweetness medium ?per3 ?)
      (recommended-sweetness sweet ?per3 ?))
  =>
  (assert (wine Riesling =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-geverztraminer ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (recommended-body full ?per2 ?)
  (feature spiciness)
  =>
  (assert (wine Geverztraminer =(min ?per1 ?per2) =(gensym))))

(defrule recommend-chenin-blanc ""
  (select-wines)
  (recommended-color white ?per1 ?)
  (recommended-body light ?per2 ?)
  (or (recommended-sweetness medium ?per3 ?)
      (recommended-sweetness sweet ?per3 ?))
  =>
  (assert (wine Chenin-Blanc =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-valpolicella ""
  (select-wines)
  (recommended-color red ?per1 ?)
  (recommended-body light ?per2 ?)
  =>
  (assert (wine Valpolicella =(min ?per1 ?per2) =(gensym))))

(defrule recommend-zinfandel-and-cabernet-sauvignon ""
  (select-wines)
  (recommended-color red ?per1 ?)
  (or (recommended-sweetness medium ?per2 ?)
      (recommended-sweetness dry  ?per2 ?))
  =>
  (assert (wine Cabernet-Sauvignon =(min ?per1 ?per2) =(gensym)))
  (assert (wine Zinfandel =(min ?per1 ?per2) =(gensym))))

(defrule recommend-pinot-noir ""
  (select-wines)
  (recommended-color red ?per1 ?)
  (recommended-body medium ?per2 ?)
  (recommended-sweetness medium ?per3 ?)
  =>
  (assert (wine Pinot-Noir =(min ?per1 ?per2 ?per3) =(gensym))))

(defrule recommend-burgundy ""
  (select-wines)
  (recommended-color red ?per1 ?)
  (recommended-body full ?per2 ?)
  =>
  (assert (wine Burgundy =(min ?per1 ?per2) =(gensym))))

;;***************
;;* QUERY RULES *
;;***************

(defrule bad-value ""
  (declare (salience 10))
  (values-for ?variable $?list)
  ?f1 <- (?variable ?value)
  (test (! (member ?value $?list)))
  =>
  (retract ?f1))

(defrule question-1 ""
  ?rem <- (ask-question)
  (not (main-component ?))
  =>
  (retract ?rem)
  (printout t "Is the main component of the meal meat, fish, or poultry? ")
  (assert (main-component =(read))))

(defrule question-2 ""
  ?rem <- (ask-question)
  (main-component poultry)
  (not (has-turkey ?))
  =>
  (retract ?rem)
  (printout t "Does the meal have turkey in it? ")
  (assert (has-turkey =(read))))

(defrule question-3 ""
  ?rem <- (ask-question)
  (main-component meat)
  (not (has-veal ?))
  =>
  (retract ?rem)
  (printout t "Does the meal have veal in it? ")
  (assert (has-veal =(read))))

(defrule question-4 ""  
  ?rem <- (ask-question)
  (not (has-sauce ?))
  =>
  (retract ?rem)
  (printout t "Does the meal have a sauce on it? ")
  (assert (has-sauce =(read))))

(defrule question-5 ""
  ?rem <- (ask-question)
  (has-sauce yes)
  (not (sauce ?))
  =>
  (retract ?rem)
  (printout t "Is the sauce for the meal spicy, sweet, cream, or tomato? ")
  (assert (sauce =(read))))

(defrule question-6 ""
  ?rem <- (ask-question)
  (not (tastiness ?))
  =>
  (retract ?rem)
  (printout t "Is the flavor of the meal delicate, average, or strong? ")
  (assert (tastiness =(read))))

(defrule question-7 ""
  ?rem <- (ask-question)
  (not (preferred-body ?))
  =>
  (retract ?rem)
  (printout t "Do you generally prefer light, medium, or full bodied wines? ")
  (assert (preferred-body =(read))))

(defrule question-8 ""
  ?rem <- (ask-question)
  (not (preferred-color ?))
  =>
  (retract ?rem)
  (printout t "Do you generally prefer red or white wines? ")
  (assert (preferred-color =(read))))

(defrule question-9 ""
  ?rem <- (ask-question)
  (not (preferred-sweetness ?))
  =>
  (retract ?rem)
  (printout t "Do you generally prefer dry, medium, or sweet wines? ")
  (assert (preferred-sweetness =(read)))) 

(defrule ask-another-question ""
  (not (ask-question))
  =>
  (assert (ask-question)))

;;*****************************
;;* PRINT SELECTED WINE RULES *
;;*****************************

(defrule print-wine ""
  (print-wines)
  ?rem <- (wine ?name ?per ?)		  
  (not (wine ?name1 ?per1&:(> ?per1 ?per) ?))
  =>
  (retract ?rem)
  (format t " %-24s %2d%%%n" ?name ?per))

(defrule end-spaces ""
   (print-wines)
   (not (wine ? ? ?))
   =>
   (printout t t))

;;*******************************
;;* ELIMINATE POOR CHOICES RULE *
;;*******************************

(defrule remove-poor-wine-choices ""
  (remove-poor-choices)
  ?rem <- (wine ? ?per ?)
  (test (< ?per 20))
  =>
  (retract ?rem))

;;****************************
;;* COMBINE CERTAINTIES RULE *
;;****************************

(defrule combine-certainties ""
  (declare (salience 10000))
  (combine ?rel)
  ?rem1 <- (?rel ?val ?per1 ?sym1)
  ?rem2 <- (?rel ?val ?per2 ?sym2&~?sym1)
  =>
  (retract ?rem1 ?rem2)
  (assert (?rel ?val
		=(/ (- (* 100 (+ ?per1 ?per2)) (* ?per1 ?per2)) 100)
		=(gensym))))
		
;;**************************************
;;* PHASE CONTROL RULES                *
;;*   PHASE 0: Ask Questions           *
;;*   PHASE 1: Choose Best Qualities   *
;;*   PHASE 2: Choose Recommended      *
;;*            Qualities               *
;;*   PHASE 3: Check for Default       *
;;*            Recommended Qualities   *
;;*   PHASE 4: Select Wines based on   *
;;*            Recommended Qualities   *
;;*   PHASE 5: Remove Wine Selections  *
;;*            with Low Certainties    *
;;*   PHASE 6: Display Wine Selections *
;;**************************************

(defrule change-to-phase-1 ""
   (declare (salience -10))
   (initial-fact)
   =>
   (assert (choose-qualities)))

(defrule change-to-phase-2 ""
   (declare (salience -10))
   ?phase <- (choose-qualities)
   =>
   (retract ?phase)
   (assert (recommend-qualities)))

(defrule change-to-phase-3 ""
   (declare (salience -10))
   ?phase <- (recommend-qualities)
   =>
   (retract ?phase)
   (assert (default-qualities)))

(defrule change-to-phase-4 ""
   (declare (salience -10))
   ?phase <- (default-qualities)
   =>
   (retract ?phase)
   (assert (select-wines)))

(defrule change-to-phase-5 ""
   (declare (salience -10))
   ?phase <- (select-wines)
   =>
   (retract ?phase)
   (assert (remove-poor-choices)))

(defrule change-to-phase-6 ""
   (declare (salience -10))
   ?phase <- (remove-poor-choices)
   =>
   (retract ?phase)
   (printout t t)
   (printout t "        SELECTED WINES" t t)
   (printout t " WINE                  CERTAINTY" t)
   (printout t " -------------------------------" t)
   (assert (print-wines)))
