/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    DRIVE MODULE                     */
   /*******************************************************/

#include <stdio.h>

#include "clips.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                     empty_drive();
   int                     pp_drive();
   int                     pn_drive();
   int                     clear_lower_beta_memory();
   int                     remove_beta_memory_id();
   
/***************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
   
   int                     drive();
   int                     join_compute();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct fbind    *newnid(); 
   extern struct fbind    *copy_binds();
   extern struct flink    *deletebinds();
   extern float            my_and();
   extern float            my_or();
   extern int              generic_compute();
   extern char            *symbol_string();
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct fbind      *gbl_lhs_binds;
   extern struct fbind      *gbl_rhs_binds;
   extern struct funtab     *PTR_NEQ_VARS;
   extern struct funtab     *PTR_EQ_VARS;

/*******************************************************************/
/* DRIVE: Drives a set of fact bindings through the join net.      */ 
/*   Input variables are join (the join in the join net being      */ 
/*   entered), binds (the set of fact bindings being passed to the */
/*   join), and enter_direction (the direction, left or right,     */ 
/*   that binds are entering the join.  Drive will alter and use   */ 
/*   binds, so if the calling routine does not want binds altered, */
/*   then it should make a copy of them before calling drive.      */
/*******************************************************************/
int drive(binds,join,enter_direction)
  struct flink *binds;
  struct internode *join;
  int enter_direction;
  {
   struct fbind *lhs_binds, *rhs_binds, *tracer;
   struct flink *compare_side, *linker, *clinker;
   char entry_logic, opp_logic;
   struct list *list_of_joins;
   int join_test;

   /*=======================================================*/
   /* If the link to next join is null, then the terminator */
   /* join has been reached and the rule is ready to fire.  */
   /* This join is connected to the list of actions for the */
   /* rule about to fire.  Add the rule to the agenda.      */
   /*=======================================================*/
    
   if (join->next == NULL)
     {
      add_activation(symbol_string(join->eval->val.hvalue),  /* Name     */
                     join->eval->next_arg->val.index,        /* Salience */
                     join->eval->next_arg->next_arg,         /* Rhs      */
                     binds);                                 /* Basis    */
      return(1);
     }

   /*=========================================================*/
   /* Store the bindings on the appropriate side of the join. */
   /*=========================================================*/

   if (enter_direction == RHS)
     {
      /*
      binds->next = join->entry_pat->alpha;
      join->entry_pat->alpha = binds;
      */
     }
   else if (enter_direction == LHS) 
     {
      binds->next = join->beta;
      join->beta = binds;
     }
   else
     {
      clips_system_error(301);
      cl_exit(5);
     }
  
   /*=====================================================*/
   /* Set up a group of variables to indicate which binds */
   /* the new binds are to be compared with and the logic */
   /* of each side of the join.                           */
   /*=====================================================*/

   if (enter_direction == RHS)
     { 
      compare_side = join->beta;
      opp_logic = join->lhs_log;
      entry_logic = join->rhs_log;
      rhs_binds = binds->binds;
     }
   else
     {
      compare_side = join->entry_pat->alpha;
      opp_logic = join->rhs_log;
      entry_logic = join->lhs_log;
      lhs_binds = binds->binds;
     }

   /*===================================================*/
   /* If the opposite side of the join that was entered */
   /* has positive logic, but has no binds, then no     */
   /* binds will be passed down to the next join.       */
   /*===================================================*/

   if ((opp_logic == '+') && (compare_side == NULL))
     { return(1); }

   /*===================================================*/
   /* If the opposite side of the join that was entered */
   /* is empty (no pattern or upper level join enters   */
   /* through that side), then perform the appropriate  */
   /* action for the rhs join logic.                    */
   /*===================================================*/

   if (opp_logic == 'e')
     {
      empty_drive(join,binds->binds);
      return(1);
     }

   /*===================================================*/
   /* Compare each set of binds on the opposite side of */
   /* the join with the set of binds that entered this  */
   /* join.  If the binds don't mismatch, then perform  */
   /* the appropriate action for the logic of the join. */
   /*===================================================*/

   while (compare_side != NULL)
     {
      join_test = FALSE;
      
      if (enter_direction == RHS)
        { lhs_binds = compare_side->binds ; }
      else
        { rhs_binds = compare_side->binds; } 

      /*======================================================*/
      /* Evaluate the join expression for this combination of */
      /* rhs and lhs bindings.                                */
      /*======================================================*/
     
      if (join->eval == NULL)
        { join_test = TRUE; }
      else
        { join_test = join_compute(join->eval,lhs_binds,rhs_binds); }

      /*====================================================*/
      /* If the join expression evaluated to true (i.e.     */ 
      /* there were no conflicts between variable bindings, */
      /* all tests were satisfied, etc.), then perform the  */
      /* appropriate action given the logic of this join.   */
      /*====================================================*/
 
      if (join_test != FALSE)
	   {
         if ((entry_logic == '+') && (opp_logic == '+'))
           { pp_drive(lhs_binds,rhs_binds,join); }
         else if (entry_logic == '-')
           { pn_drive(join,compare_side); }
         else if (opp_logic == '-')
           { binds->count++; }
        }

      compare_side = compare_side->next;
     } 

   /*======================================================*/
   /* If a join with a positive lhs and a negative rhs was */
   /* entered from the lhs side of the join, and the join  */
   /* test failed for all sets of matches for the new      */
   /* bindings on the lhs side (the counter on the lhs is  */
   /* set to zero), then the lhs bindings should be send   */
   /* down to the joins below along with an NID marker     */
   /* that represents the instance of the not pattern that */
   /* was satisfied.                                       */
   /*======================================================*/

   if ((opp_logic == '-') && (binds->count == 0))
     {
      if (join->not_eval != NULL)
        { 
         join_test = join_compute(join->not_eval,binds->binds,NULL);
         if (join_test == FALSE) return(1);
        }
         
      linker = get_struct(flink);
      linker->binds = copy_binds(binds->binds);
      linker->count = 0;
      linker->next = NULL;
      tracer = linker->binds;
      while (tracer->next != NULL)
        { tracer = tracer->next; }
      tracer->next = newnid();
      binds->count = tracer->next->whoset;

      /*============================================*/
      /* Send binding to all joins below this join. */
      /*============================================*/

      list_of_joins = join->next;
      while (list_of_joins->next != NULL)
        {
         clinker = get_struct(flink);
         clinker->next = NULL;
         clinker->count = 0;
         clinker->binds = copy_binds(linker->binds);
         drive(clinker,list_of_joins->path,LHS);
         list_of_joins = list_of_joins->next;
        }

      drive(linker,list_of_joins->path,LHS);
     }
   
   return(1); 
  }

/***************************************************************/
/* JOIN_COMPUTE: Performs a faster evaluation for join         */
/*   expressions than if generic_compute were used directly.   */
/*   This function evaluates calls to function eq_vars,        */
/*   function neq_vars, function and, and function or.  All    */
/*   other function calls are evaluated using generic_compute. */
/***************************************************************/
int join_compute(jn_test,lbinds,rbinds)
  struct test *jn_test;
  struct fbind *lbinds, *rbinds;
  {
   int pass,fail;
   int p1, e1, p2, e2;
   struct test *tptr;
   struct values vresult;
   struct element *elem_a, *elem_b;
   struct fbind *lhs_binds;
   int count;
   struct fact *fact_ptr1, *fact_ptr2;
   int a_extent, b_extent;
   struct fact_marker *marks1, *marks2; 

   /*==============================================================*/
   /* Evaluate expressions eq_vars and neq_vars.  Calls to these   */
   /* functions are expressed in the format:                       */
   /*   (eq_vars  <pattern m> <element n> <pattern p> <element r>) */
   /*   (neq_vars <pattern m> <element n> <pattern p> <element r>) */
   /* Function eq_vars returns true (1.0) if the nth element of    */
   /* the mth pattern is equal to the rth element of the pth       */
   /* pattern.  Function neq_vars acts similarly to eq_vars except */
   /* the return value is negated.                                 */
   /*==============================================================*/

   if ((jn_test->val.fun_ptr == PTR_EQ_VARS) ||
       (jn_test->val.fun_ptr == PTR_NEQ_VARS))
     {
      /*==========================================*/
      /* Determine the appropriate return values. */
      /*==========================================*/

      if (jn_test->val.fun_ptr == PTR_EQ_VARS)
        {
         pass = TRUE;
         fail = FALSE;
        }
      else
        {
         pass = FALSE;
         fail = TRUE;
        }

      /*==============================================*/
      /* Determine which elements are to be extracted */
      /* out of which patterns.                       */
      /*==============================================*/                  

      tptr = jn_test->arg_list;
      p1 = tptr->val.index;
      tptr = tptr->next_arg;
      e1 = tptr->val.index;
      tptr = tptr->next_arg;
      p2 = tptr->val.index;
      tptr = tptr->next_arg;
      e2 = tptr->val.index;

      /*==============================================*/
      /* Extract the fact pointer and list of segment */
      /* markers for the first pattern.               */
      /*==============================================*/

      if ((p1 > 1) || (lbinds == NULL))
        { 
         fact_ptr1 = rbinds->origin;
         marks1 = rbinds->marker; 
        }
      else
        {
         lhs_binds = lbinds;

         for (count = 1; count < p1; count++)
           { lhs_binds = lhs_binds->next; }

         fact_ptr1 = lhs_binds->origin;
         marks1 = lhs_binds->marker; 
        }

      /*==============================================*/
      /* Extract the fact pointer and list of segment */
      /* markers for the second pattern.              */
      /*==============================================*/

      if (p2 == p1)
        { 
         fact_ptr2 = fact_ptr1;
         marks2 = marks1;
        }
      else
        {
         lhs_binds = lbinds;

         for (count = 1; count < p2; count++)
           { lhs_binds = lhs_binds->next; }

         fact_ptr2 = lhs_binds->origin;
         marks2 = lhs_binds->marker; 
        }

      /*=============================================*/
      /* Determine the beginning point and extent of */
      /* the comparision for the first pattern.      */
      /*=============================================*/

      a_extent = 1;

      if (marks1 != NULL)
        { 
         if (marks1->element < e1)
           { 
            e1 = bump_elm_num(marks1,e1,&a_extent);
           }
         else if (marks1->element == e1)
           { a_extent = (marks1->end - marks1->start) + 1; }
        }

      /*=============================================*/
      /* Determine the beginning point and extent of */
      /* the comparision for the second pattern.     */
      /*=============================================*/

      b_extent =  1;

      if (marks2 != NULL)
        { 
         if (marks2->element < e2)
           { 
            e2 = bump_elm_num(marks2,e2,&b_extent);
           }
         else if (marks2->element == e2)
           { b_extent = (marks2->end - marks2->start) + 1; }
        }
  
      /*===============================================*/
      /* If the extents are not the same, then the     */
      /* comparision fails (e.g. two segment variables */
      /* of different length.                          */
      /*===============================================*/

      if (a_extent != b_extent)
        { return(fail); }

      /*=========================================*/
      /* If the extents are both zero, then the  */
      /* comparision succeeds (e.g. two segment  */
      /* variables of length zero).              */
      /*=========================================*/

      if (a_extent == 0)
        { return(pass); }

      /*==============================================*/
      /* Compare the two variables element by element */
      /* for the length of the extent.                */
      /*==============================================*/

      e1--;
      elem_a = &fact_ptr1->atoms[e1];
      e2--;
      elem_b = &fact_ptr2->atoms[e2];

      while (TRUE)
        {
         if (elem_a->type != elem_b->type)
           { return(fail); }

         if (elem_a->type == NUMBER)
           {
            if (elem_a->val.fvalue != elem_b->val.fvalue)
              { return(fail); }
           }
         else if (elem_a->val.hvalue != elem_b->val.hvalue)
           { return(fail); }

         a_extent--;

         if (a_extent == 0) return(pass);

         elem_a++;
         elem_b++;
        }   
     }

   /*=========================================================*/
   /* Evaluate or expressions expressed in the format:        */
   /*   (or <expression 1> <expression 2> ... <expression n>) */
   /* Returns true (1.0) if any of the expression are true,   */
   /* otherwise returns false (0.0).                          */
   /*=========================================================*/

   else if (jn_test->val.fun_ptr->ip == (int (*)()) my_or)
     {
      jn_test = jn_test->arg_list;
      while (jn_test != NULL)
        {
         if (join_compute(jn_test,lbinds,rbinds) == TRUE)
           { return(TRUE); } 
         jn_test = jn_test->next_arg;
        }
      return(FALSE);
     }

   /*==========================================================*/
   /* Evaluate and expressions expressed in the format:        */
   /*   (and <expression 1> <expression 2> ... <expression n>) */
   /* Returns false (0.0) if any of the expression are false,  */
   /* otherwise returns true (1.0).                            */
   /*==========================================================*/

   else if (jn_test->val.fun_ptr->ip == (int (*)()) my_and)
     {
      jn_test = jn_test->arg_list;
      while (jn_test != NULL)
        {
         if (join_compute(jn_test,lbinds,rbinds) == FALSE)
           { return(FALSE); } 
         jn_test = jn_test->next_arg;
        }
      return(TRUE);
     }

   /*=======================================================*/
   /* Evaluate all other expressions using generic_compute. */
   /*=======================================================*/

   else
     {
      struct fbind *old_lhs_binds = NULL;
      struct fbind *old_rhs_binds = NULL;
   
      old_lhs_binds = gbl_lhs_binds;
      old_rhs_binds = gbl_rhs_binds;  
      gbl_lhs_binds = lbinds;
      gbl_rhs_binds = rbinds;

      generic_compute(jn_test,&vresult);
      
      gbl_lhs_binds = old_lhs_binds;
      gbl_rhs_binds = old_rhs_binds;
      
      if (vresult.val.fvalue == 0.0)
        { return(FALSE); }
      else
        { return(TRUE); }
     }
  }

/*******************************************************************/
/* PP_DRIVE: Handles the entry of a set of fact bindings into a    */
/*   join which has a positive LHS entry and a positive RHS entry. */
/*******************************************************************/
static int pp_drive(lhs_binds,rhs_binds,join)
  struct fbind *lhs_binds, *rhs_binds;
  struct internode *join;
  {
   struct flink *linker, *clinker;
   struct fbind *bind_ptr;
   struct list *list_of_joins;

   /*========================*/
   /* Copy the LHS bindings. */
   /*========================*/

   linker = get_struct(flink);
   linker->count = 0;   
   linker->next = NULL;
   linker->binds = copy_binds(lhs_binds); 

   /*=========================================*/
   /* Find the last bind in the LHS bindings, */  
   /* and attach the RHS bindings to it.      */
   /*=========================================*/

   bind_ptr = linker->binds;
   while (bind_ptr->next != NULL)
     { bind_ptr = bind_ptr->next; }

   bind_ptr->next = copy_binds(rhs_binds);
   
   /*=========================================================*/
   /* Send the set of bindings down to the next set of joins. */
   /*=========================================================*/

   list_of_joins = join->next;
   while (list_of_joins->next != NULL)
     {
      clinker = get_struct(flink);
      clinker->next = NULL;
      clinker->count = 0;
      clinker->binds = copy_binds(linker->binds);
      drive(clinker,list_of_joins->path,LHS);
      list_of_joins = list_of_joins->next;
     }

   drive(linker,list_of_joins->path,LHS);

   return(1);
  }

/*******************************************************************/
/* PN_DRIVE: Handles the entry of a set of fact bindings into a    */
/*   join which has a positive LHS entry and a negative RHS entry. */
/*******************************************************************/
static int pn_drive(join,lhs_binds)
  struct internode *join;
  struct flink *lhs_binds;
  {
   FACT_ID not_id;
   struct list *list_of_joins;

   /*==============================================================*/ 
   /* The positive LHS entry bindings of a positive-negative join  */
   /* have a "counter" attached to them which indicates the number */
   /* of RHS entry bindings which are consistent with them. If     */
   /* no RHS bindings are consistent, then the not side has been   */
   /* satisfied, the counter will have a value of < 0 (which       */
   /* indicates the id associated with the not and that there are  */
   /* no consistent bindings), and the LHS bindings will have been */
   /* sent down to the next join.  This routine handles the case   */
   /* in which a set of bindings entered the negative RHS entry of */
   /* the join and were consistent with the LHS binds.  If the     */
   /* counter for the LHS binds is less than zero, then all binds  */
   /* with that "not" id value must be removed from lower joins.   */
   /* All agenda activations with that id are also removed. If the */
   /* counter for the binds is greater than zero, then the counter */
   /* needs only to be incremented.                                */
   /*==============================================================*/ 
   
   if (lhs_binds->count < 0)
     {
      not_id = lhs_binds->count;
      lhs_binds->count = 1;
      purge_agenda(not_id);

      list_of_joins = join->next;
      while (list_of_joins != NULL)
        {
         remove_beta_memory_id(not_id,list_of_joins->path);
         list_of_joins = list_of_joins->next; 
        }   
     }
   else
     { lhs_binds->count++; } 
   
  }

/*****************************************************************/
/* REMOVE_ID_FROM_LOWER_BETA_MEMORY:  Recursively deletes all    */
/*   partial matches which contain the fact id from beta memory. */
/*   Note: could be improved by using less recursion.            */
/*****************************************************************/
static int remove_beta_memory_id(not_id,node)
  FACT_ID not_id;
  struct internode *node;
  {
   struct list *list_of_joins;
   int found_id;
   
   node->beta = deletebinds(not_id,node->beta,&found_id);
   list_of_joins = node->next;
   while (list_of_joins != NULL)
     { 
      remove_beta_memory_id(not_id,list_of_joins->path); 
      list_of_joins = list_of_joins->next;
     }
  }

/***********************************************************/
/* EMPTY_DRIVE: Handles drive logic when the lhs side of a */
/*   join is empty (i.e. no patterns enter the join from   */
/*   that direction.                                       */
/***********************************************************/
static int empty_drive(join,rhs_binds)
  struct internode *join;
  struct fbind *rhs_binds;
  {
   char entry_logic;
   struct flink *linker, *clinker;
   struct list *list_of_joins;
   int join_test;

   /*=================================================*/
   /* Determine if the set of fact bindings satisfies */
   /* the join expression.                            */
   /*=================================================*/

   if (join->eval != NULL)
     {
      join_test = join_compute(join->eval,NULL,rhs_binds);
      if (join_test == FALSE) return;
     }
   
   /*===================================================*/
   /* If the entry logic is negative (i.e. join entered */
   /* from pattern with a not pattern operator), then   */
   /* remove any instantations from the agenda that     */
   /* contain the not_id associated with this join, and */
   /* remove all fact bindings from all joins below.    */
   /*===================================================*/

   entry_logic = join->rhs_log;
   if (entry_logic == '-')
     {
      if (join->id > 0)
        {
         join->id++;
         return;
        }

      purge_agenda(join->id);
      list_of_joins = join->next;
      while (list_of_joins != NULL)
        {
         clear_lower_beta_memory(list_of_joins->path);
         list_of_joins = list_of_joins->next;
        }

      join->id = 1;
      return;
     }

   /*==============================================*/
   /* Copy the bindings and send them to all joins */
   /* below this join.                             */
   /*==============================================*/

   linker = get_struct(flink);
   linker->next = NULL;
   linker->count = 0;
   linker->binds = copy_binds(rhs_binds);

   list_of_joins = join->next;
   while (list_of_joins->next != NULL)
     {
      clinker = get_struct(flink);
      clinker->next = NULL;
      clinker->binds = copy_binds(linker->binds);
      clinker->count = 0;
      drive(clinker,list_of_joins->path,LHS);
      list_of_joins = list_of_joins->next;
     }

   drive(linker,list_of_joins->path,LHS);

   return;
  }

/****************************************************************/
/* CLEAR_LOWER_BETA_MEMORY: Removes all fact bindings from the */
/*   beta memory of a join and recursively from the beta memory */
/*   of all joins below it.                                     */
/*   NOTE: Could be optimized to use less recursion?            */
/****************************************************************/
static int clear_lower_beta_memory(node)
  struct internode *node;
  {
   struct flink *not_binds, *temp_binds;
   struct list *list_of_joins;

   /*=====================================*/
   /* Remove lhs bindings from this join. */
   /*=====================================*/

   not_binds = node->beta;
   node->beta = NULL;
   while (not_binds != NULL)
     { 
      returnbinds(not_binds->binds);
      temp_binds = not_binds->next;
      rtn_struct(flink,not_binds);
      not_binds = temp_binds;
     }

   /*=======================================*/
   /* Remove lhs bindings from joins below. */
   /*=======================================*/

   list_of_joins = node->next;
   while (list_of_joins != NULL)
     {
      clear_lower_beta_memory(list_of_joins->path);
      list_of_joins = list_of_joins->next;
     }
  }
