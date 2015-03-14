
;;;======================================================
;;;   Monkees and Bananas Sample Problem
;;;
;;;     This is an extended version of a
;;;     rather common AI planning problem.
;;;     The point is for the monkee to find
;;;     and eat some bananas.
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================
  
;;;*************
;;;* TEMPLATES *
;;;*************
           
(deftemplate monkey 
   (field location 
      (type WORD) 
      (default green-couch))
   (field on-top-of 
      (type WORD) 
      (default floor)) 
   (field holding 
      (type WORD) 
      (default nothing)))

(deftemplate object 
   (field name 
      (type WORD)
      (default ?NONE)) 
   (field location 
      (type WORD)
      (default ?NONE)) 
   (field on-top-of 
      (type WORD) 
      (default floor))
   (field weight 
      (default light)))
                    
(deftemplate chest 
   (field name 
      (type WORD)
      (default ?NONE)) 
   (field contents 
      (type WORD)
      (default ?NONE)) 
   (field unlocked-by 
      (type WORD)
      (default ?NONE)))
               
(deftemplate goal-is-to 
   (field action 
      (type WORD)
      (default ?NONE)) 
   (multi-field arguments 
      (type WORD)
      (default ?NONE)))
             
;;;*************************
;;;* chest unlocking rules *
;;;*************************

(defrule unlock-chest-to-hold-object ""
  (goal-is-to (action holds) (arguments ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (arguments ?chest)))
  =>
  (assert (goal-is-to (action unlock) (arguments ?chest))))

(defrule unlock-chest-to-move-object "" 
  (goal-is-to (action move) (arguments ?obj ?))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (arguments ?chest)))
  =>
  (assert (goal-is-to (action unlock) (arguments ?chest))))

(defrule hold-chest-to-put-on-floor "" 
  (goal-is-to (action unlock) (arguments ?chest))
  (object (name ?chest) (on-top-of ~floor) (weight light))
  (monkey (holding ~?chest))
  (not (goal-is-to (action holds) (arguments ?chest)))
  =>
  (assert (goal-is-to (action holds) (arguments ?chest))))

(defrule put-chest-on-floor "" 
  (goal-is-to (action unlock) (arguments ?chest))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding ?chest))
  ?object <- (object (name ?chest))
  =>
  (printout t "Monkey throws " ?chest " off " ?on " onto floor." t)
  (modify ?monkey (holding blank))
  (modify ?object (location ?place) (on-top-of floor)))

(defrule get-key-to-unlock "" 
  (goal-is-to (action unlock) (arguments ?obj))
  (object (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding ~?key))
  (not (goal-is-to (action holds) (arguments ?key)))
  =>
  (assert (goal-is-to (action holds) (arguments ?key))))

(defrule move-to-chest-with-key "" 
  (goal-is-to (action unlock) (arguments ?chest))
  (monkey (location ?mplace) (holding ?key))
  (object (name ?chest) (location ?cplace&~?mplace) (on-top-of floor))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (arguments ?cplace)))
  =>
  (assert (goal-is-to (action walk-to) (arguments ?cplace))))

(defrule unlock-chest-with-key "" 
  ?goal <- (goal-is-to (action unlock) (arguments ?name))
  ?chest <- (chest (name ?name) (contents ?contents) (unlocked-by ?key))
  (object (name ?name) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?on) (holding ?key))
  =>
  (printout t "Monkey opens " ?name " with " ?key " revealing " ?contents t)
  (modify ?chest (contents nothing))
  (assert (object (name ?contents) (location ?place) (on-top-of ?name)))
  (retract ?goal))

;;;***********************
;;;* process hold object * 
;;;***********************

(defrule use-ladder-to-hold ""
  (goal-is-to (action holds) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (not (object (name ladder) (location ?place)))
  (not (goal-is-to (action move) (arguments ladder ?place)))
  =>
  (assert (goal-is-to (action move) (arguments ladder ?place))))

(defrule climb-ladder-to-hold ""
  (goal-is-to (action holds) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (object (name ladder) (location ?place) (on-top-of floor))
  (monkey (on-top-of ~ladder))
  (not (goal-is-to (action on) (arguments ladder)))
  =>
  (assert (goal-is-to (action on) (arguments ladder))))

(defrule grab-object-from-ladder "" 
  ?goal <- (goal-is-to (action holds) (arguments ?name))
  ?object <- (object (name ?name) (location ?place) (on-top-of ceiling) (weight light))
  (object (name ladder) (location ?place))
  ?monkey <- (monkey (location ?place) (on-top-of ladder) (holding blank))
  =>
  (printout t "Monkey grabs the " ?name t)
  (modify ?object (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

(defrule climb-to-hold "" 
  (goal-is-to (action holds) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ?on&~ceiling) (weight light))
  (monkey (location ?place) (on-top-of ~?on))
  (not (goal-is-to (action on) (arguments ?on)))
  =>
  (assert (goal-is-to (action on) (arguments ?on))))

(defrule walk-to-hold ""
  (goal-is-to (action holds) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ~ceiling) (weight light))
  (monkey (location ~?place))
  (not (goal-is-to (action walk-to) (arguments ?place)))
  =>
  (assert (goal-is-to (action walk-to) (arguments ?place))))

(defrule drop-to-hold ""
  (goal-is-to (action holds) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ?on) (holding ~blank))
  (not (goal-is-to (action holds) (arguments blank)))
  =>
  (assert (goal-is-to (action holds) (arguments blank))))

(defrule grab-object "" 
  ?goal <- (goal-is-to (action holds) (arguments ?name))
  ?object <- (object (name ?name) (location ?place) (on-top-of ?on) (weight light))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey grabs the " ?name t)
  (modify ?object (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

;;;**************************
;;;* move object to a place * 
;;;**************************

(defrule hold-object-to-move ""  
  (goal-is-to (action move) (arguments ?obj ?place))
  (object (name ?obj) (location ~?place) (weight light))
  (monkey (holding ~?obj))
  (not (goal-is-to (action holds) (arguments ?obj)))
  =>
  (assert (goal-is-to (action holds) (arguments ?obj))))

(defrule move-object-to-place "" 
  (goal-is-to (action move) (arguments ?obj ?place))
  (monkey (location ~?place) (holding ?obj))
  (not (goal-is-to (action walk-to) (arguments ?place)))
  =>
  (assert (goal-is-to (action walk-to) (arguments ?place))))

(defrule drop-object-once-moved "" 
  ?goal <- (goal-is-to (action move) (arguments ?name ?place))
  ?monkey <- (monkey (location ?place) (holding ?obj))
  ?object <- (object (name ?name) (weight light))
  =>
  (printout t "Monkey drops the " ?name "." t)
  (modify ?monkey (holding blank))
  (modify ?object (location ?place) (on-top-of floor))
  (retract ?goal))

(defrule already-moved-object ""
  ?goal <- (goal-is-to (action move) (arguments ?obj ?place))
  (object (name ?obj) (location ?place))
  =>
  (retract ?goal))

;;;*************************
;;;* process walk-to place *
;;;*************************

(defrule already-at-place "" 
  ?goal <- (goal-is-to (action walk-to) (arguments ?place))
  (monkey (location ?place))
  =>
  (retract ?goal))

(defrule get-on-floor-to-walk ""
  (goal-is-to (action walk-to) (arguments ?place))
  (monkey (location ~?place) (on-top-of ~floor))
  (not (goal-is-to (action on) (arguments floor)))
  =>
  (assert (goal-is-to (action on) (arguments floor))))

(defrule walk-holding-nothing ""
  ?goal <- (goal-is-to (action walk-to) (arguments ?place))
  ?monkey <- (monkey (location ~?place) (on-top-of floor) (holding blank))
  =>
  (printout t "Monkey walks to " ?place t)
  (modify ?monkey (location ?place))
  (retract ?goal))

(defrule walk-holding-object ""
  ?goal <- (goal-is-to (action walk-to) (arguments ?place))
  ?monkey <- (monkey (location ~?place) (on-top-of floor) (holding ?obj&~blank))
  =>
  (printout t "Monkey walks to " ?place " holding " ?obj t)
  (modify ?monkey (location ?place))
  (retract ?goal))

(defrule drop-object ""  
  ?goal <- (goal-is-to (action holds) (arguments blank))
  ?monkey <- (monkey (location ?place) 
                     (on-top-of ?on) 
                     (holding ?name&~blank))
  ?object <- (object (name ?name))
  =>
  (printout t "Monkey drops " ?name t)
  (modify ?monkey (holding blank))
  (modify ?object (location ?place) (on-top-of ?on))
  (retract ?goal))

;;;*************************
;;;* process get on object * 
;;;*************************

(defrule jump-onto-floor "" 
  ?goal <- (goal-is-to (action on) (arguments floor))
  ?monkey <- (monkey (on-top-of ?on&~floor))
  =>
  (printout t "Monkey jumps off " ?on " onto the floor." t)
  (modify ?monkey (on-top-of floor))
  (retract ?goal))

(defrule walk-to-place-to-climb "" 
  (goal-is-to (action on) (arguments ?obj))
  (object (name ?obj) (location ?place))
  (monkey (location ~?place))
  (not (goal-is-to (action walk-to) (arguments ?place)))
  =>
  (assert (goal-is-to (action walk-to) (arguments ?place))))

(defrule drop-to-climb "" 
  (goal-is-to (action on) (arguments ?obj))
  (object (name ?obj) (location ?place))
  (monkey (location ?place) (holding ~blank))
  (not (goal-is-to (action holds) (arguments blank)))
  =>
  (assert (goal-is-to (action holds) (arguments blank))))

(defrule climb-indirectly "" 
  (goal-is-to (action on) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ~?on&~?obj) (holding blank))
  (not (goal-is-to (action on) (arguments ?on)))
  =>
  (assert (goal-is-to (action on) (arguments ?on))))

(defrule climb-directly ""  
  ?goal <- (goal-is-to (action on) (arguments ?obj))
  (object (name ?obj) (location ?place) (on-top-of ?on))
  ?monkey <- (monkey (location ?place) (on-top-of ?on) (holding blank))
  =>
  (printout t "Monkey climbs onto " ?obj t)
  (modify ?monkey (on-top-of ?obj))
  (retract ?goal))

(defrule already-on-object ""
  ?goal <- (goal-is-to (action on) (arguments ?obj))
  (monkey (on-top-of ?obj))
  =>
  (retract ?goal))

;;;**********************
;;;* process eat object * 
;;;**********************

(defrule hold-to-eat ""
  (goal-is-to (action eat) (arguments ?obj))
  (monkey (holding ~?obj))
  (not (goal-is-to (action holds) (arguments ?obj)))
  =>
  (assert (goal-is-to (action holds) (arguments ?obj))))

(defrule satisfy-hunger ""
  ?goal <- (goal-is-to (action eat) (arguments ?name))
  ?monkey <- (monkey (holding ?name))
  ?object <- (object (name ?name))
  =>
  (printout t "Monkey eats the " ?name "." t)
  (modify ?monkey (holding blank))
  (retract ?goal ?object))
 
;;;*****************
;;;* initial-state * 
;;;*****************

(defrule startup ""
  =>
  (assert (monkey (location t5-7) (on-top-of green-couch) (holding blank)))
  (assert (object (name green-couch) (location t5-7) (weight heavy)))
  (assert (object (name red-couch) (location t2-2) (weight heavy)))
  (assert (object (name big-pillow) (location t2-2) (on-top-of red-couch)))
  (assert (object (name red-chest) (location t2-2) (on-top-of big-pillow)))
  (assert (chest (name red-chest) (contents ladder) (unlocked-by red-key)))
  (assert (object (name blue-chest) (location t7-7) (on-top-of ceiling)))
  (assert (chest (name blue-chest) (contents bananas) (unlocked-by blue-key)))
  (assert (object (name blue-couch) (location t8-8) (weight heavy)))
  (assert (object (name green-chest) (location t8-8) (on-top-of ceiling)))
  (assert (chest (name green-chest) (contents blue-key) (unlocked-by red-key)))
  (assert (object (name red-key) (location t1-3)))
  (assert (goal-is-to (action eat) (arguments bananas))))
