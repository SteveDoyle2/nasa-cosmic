
;;;****************************
;;;* AUTOMOTIVE EXPERT SYSTEM *
;;;****************************

;;;**********************
;;;* ENGINE STATE RULES *
;;;**********************

(defrule normal-engine-state-conclusions ""
   (working-state engine normal)
   =>
   (assert (spark-state engine normal))
   (assert (fuel-level gas-tank sufficient))
   (assert (charge-state battery charged))
   (assert (rotation-state engine rotates)))

(defrule unsatisfactory-engine-state-conclusions ""
   (working-state engine unsatisfactory)
   =>
   (assert (fuel-level gas-tank sufficient))
   (assert (charge-state battery charged))
   (assert (rotation-state engine rotates)))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-engine-state ""
   ?rem <- (query phase)
   (not (working-state engine ?))
   =>
   (retract ?rem)
   (printout t "What is the working state of the engine:" t)
   (printout t " (normal/unsatisfactory/does-not-start)? ")
   (bind ?response (read))
   (assert (working-state engine ?response)))

(defrule determine-rotation-state ""
   ?rem <- (query phase)
   (working-state engine does-not-start)
   (not (rotation-state engine ?))
   =>
   (retract ?rem)
   (printout t "Does the engine rotate (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (rotation-state engine rotates))
       (assert (spark-state engine irregular-spark))
       else
       (assert (rotation-state engine does-not-rotate))       
       (assert (spark-state engine does-not-spark))))

(defrule determine-sluggishness ""
   ?rem <- (query phase)
   (working-state engine unsatisfactory)
   (not (symptom engine sluggishness | not-sluggishness))
   =>
   (retract ?rem)
   (printout t "Is the engine sluggish (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (symptom engine sluggishness))
       else
       (assert (symptom engine not-sluggishness))))

(defrule determine-misfiring ""
   ?rem <- (query phase)
   (working-state engine unsatisfactory)
   (not (symptom engine misfiring | not-misfiring))
   =>
   (retract ?rem)
   (printout t "Does the engine misfire (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (symptom engine misfiring))       
       (assert (spark-state engine irregular-spark))
       else
       (assert (symptom engine not-misfiring))))

(defrule determine-knocking ""
   ?rem <- (query phase)
   (working-state engine unsatisfactory)
   (not (symptom engine knocking | not-knocking))
   =>
   (retract ?rem)
   (printout t "Does the engine knock (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (symptom engine knocking))
       else
       (assert (symptom engine not-knocking))))

(defrule determine-low-output ""
   ?rem <- (query phase)
   (working-state engine unsatisfactory)
   (not (symptom engine low-output | not-low-output))
   =>
   (retract ?rem)
   (printout t "Is the output of the engine low (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (symptom engine low-output))
       else
       (assert (symptom engine not-low-output))))

(defrule determine-gas-level ""
   ?rem <- (query phase)
   (working-state engine does-not-start)
   (rotation-state engine rotates)
   (not (fuel-level gas-tank ?))
   =>
   (retract ?rem)
   (printout t "Does the tank have any gas in it (yes/no)? ")
   (bind ?response (read))
   (if (eq ?response yes)
       then
       (assert (fuel-level gas-tank sufficient))
       else
       (assert (fuel-level gas-tank empty))))

(defrule determine-battery-state ""
   ?rem <- (query phase)
   (rotation-state engine does-not-rotate)
   (not (charge-state battery ?))
   =>
   (retract ?rem)
   (printout t "What is the state of the battery (charged/dead)? ")
   (bind ?response (read))
   (assert (charge-state battery ?response)))   

(defrule determine-point-surface-state ""
   ?rem <- (query phase)
   (or (and (working-state engine does-not-start)      
            (spark-state engine irregular-spark))
       (symptom engine low-output))
   (not (point-surface-state points ?))       
   =>
   (retract ?rem)
   (printout t "What is the surface state of the points " t)
   (printout t " (normal/burned/contaminated)? ")
   (bind ?response (read))
   (assert (point-surface-state points ?response)))

(defrule determine-conductivity-test ""
   ?rem <- (query phase)
   (working-state engine does-not-start)      
   (spark-state engine does-not-spark)
   (not (charge-state battery dead))
   (not (conductivity-test ignition-coil ?))       
   =>
   (retract ?rem)
   (printout t "What is conductivity test for the ignition coil" t)
   (printout t " (positive/negative)? ")
   (bind ?response (read))
   (assert (conductivity-test ignition-coil ?response)))

;;;***************************
;;;* REPAIR SUGGESTION RULES *
;;;***************************

(defrule no-repair-needed ""
   (working-state engine normal)    
   =>
   (assert (repair "No repair needed.")))

(defrule charge-battery-repair ""
   (rotation-state engine does-not-rotate) 
   (charge-state battery dead)             
   =>
   (assert (repair "Charge the battery.")))

(defrule timing-adjustment-repair ""
   (working-state engine unsatisfactory)  
   (symptom engine knocking)             
   =>
   (assert (repair "Timing adjustment.")))

(defrule replace-ignition-coil-repair ""
   (working-state engine does-not-start)      
   (spark-state engine does-not-spark)       
   (conductivity-test ignition-coil negative) 
   =>
   (assert (repair "Replace the ignition coil.")))

(defrule distributor-lead-wire-repair ""
   (working-state engine does-not-start)      
   (spark-state engine does-not-spark)       
   (conductivity-test ignition-coil positive) 
   =>
   (assert (repair "Repair the distributor lead wire.")))

(defrule point-gap-adjustment-repair ""
  (working-state engine unsatisfactory)   
  (symptom engine misfiring)             
  =>
  (assert (repair "Point gap adjustment.")))

(defrule replace-points-repair ""
   (working-state engine does-not-start)  
   (spark-state engine irregular-spark)   
   (point-surface-state points burned)
   =>
   (assert (repair "Replace the points.")))

(defrule clean-points-repair-1 ""
   (working-state engine does-not-start)  
   (spark-state engine irregular-spark)    
   (point-surface-state points contaminated)
   =>
   (assert (repair "Clean the points.")))

(defrule clean-points-repair-2 ""
   (working-state engine unsatisfactory)  
   (symptom engine low-output)           
   (point-surface-state points contaminated)
   =>
   (assert (repair "Clean the points.")))

(defrule clean-fuel-line-repair ""
   (symptom engine sluggishness) 
   =>
   (assert (repair "Clean the fuel line.")))

(defrule add-gas-repair ""
   (rotation-state engine rotates)  
   (fuel-level gas-tank empty)   
   =>
   (assert (repair "Add gas.")))

;;;************************
;;;* REPAIR LISTING RULES *
;;;************************

(defrule no-repairs ""
  (list-repairs)
  (not (repair ?))
  =>
  (assert (repair "Take your car to a mechanic.")))

(defrule print-repair ""
  (list-repairs)
  (repair ?item)
  =>
  (format t " %s%n" ?item))

(defrule spaces-at-end ""
  (declare (salience -10))
  (list-repairs)
  =>
  (printout t t t))

;;;***********************
;;;* PHASE CONTROL RULES *
;;;***********************

(defrule system-banner ""
  =>
  (printout t t t)
  (printout t "The Engine Diagnosis Expert System")
  (printout t t t))

(defrule initiate-query ""
  (declare (salience -10))
  (not (query phase))
  =>
  (assert (query phase)))

(defrule initiate-repair ""
  (declare (salience -10))
  (query phase)
  =>
  (printout t t t)
  (printout t "Suggested Repairs:")
  (printout t t t)
  (assert (list-repairs)))

