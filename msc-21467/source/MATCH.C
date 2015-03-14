/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                     MATCH MODULE                    */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "clips.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                         pat_compute();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   int                         compare();
   int                         my_get_field();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern float                my_or();
   extern float                my_and();
   extern struct fact_marker  *copy_marks();
   extern char                *symbol_string();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct fact         *current_pattern_fact;
   static struct fact_marker  *current_pattern_marks;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct funtab       *PTR_CONSTANT;
   extern struct funtab       *PTR_NOTCONSTANT;
   extern struct funtab       *PTR_EQ_FIELD;
   extern struct funtab       *PTR_NEQ_FIELD;
   
/*****************************************************************/
/* COMPARE:  Compares the elements of a fact to the nodes in the */
/*   pattern network.  Arguments are:                            */
/*   fact_ptr:  Pointer to the fact being compared.              */
/*   elem_ptr:  Pointer to the list of atoms in the fact.        */
/*              Redundant argument because it can be derived     */
/*              from fact_ptr.                                   */
/*   pat_pos:   Current index into the pattern being matched.    */
/*   cur_elem:  The index of the current atom being pointed to   */
/*              in the fact.                                     */
/*   markers:   The list of segment markers generated during the */
/*              pattern match.                                   */  
/*****************************************************************/
compare(fact_ptr,elem_ptr,patn_ptr,pat_pos,cur_elem,markers,end_mark)
  struct fact *fact_ptr;
  struct element *elem_ptr;
  struct pat_node *patn_ptr;
  int cur_elem;
  struct fact_marker *markers, *end_mark;
  {
   struct list *drive_list;
   struct flink *var_list;
   struct match *list_of_matches;
   int finish_match, length, end_epos;
   struct fact_marker *new_mark, *old_mark;

   if (patn_ptr == NULL) return;
   
   current_pattern_fact = fact_ptr;
   current_pattern_marks = markers;
   length = fact_ptr->fact_length;   
   while (TRUE)
     {
      finish_match = FALSE;

      /*==========================================================*/
      /* If there are no elements in the fact left to match, and  */
      /* the pattern element being matched against is not an end  */
      /* of pattern marker or a multiple element binder, then the */
      /* pattern matching attempt for this pattern has failed.    */
      /*==========================================================*/

      if ((cur_elem == length) && 
          (patn_ptr->type != STOP) &&
          (patn_ptr->type != MULTIPLE))
        { finish_match = TRUE; }

      /*===========================================================*/
      /* If there are elements in the fact left to match, and the  */
      /* pattern element being matched against is an end of        */
      /* pattern marker, then the fact is too long for the pattern */
      /* and pattern matching attempt has failed.                  */
      /*===========================================================*/

      else if ((cur_elem < length) && (patn_ptr->type == STOP))
        { finish_match = TRUE; }

      /*===========================================================*/
      /* If there are no elements in the fact left to match, and   */
      /* the pattern element being matched against is an end of    */
      /* pattern marker, then the pattern has matched.             */
      /*===========================================================*/
    
      else if ((cur_elem == length) && (patn_ptr->type == STOP))
        {
         /*=======================================================*/
         /* Add the pattern to the list of matches for this fact. */
         /*=======================================================*/
        
         list_of_matches = fact_ptr->list;
         fact_ptr->list = get_struct(match);
         fact_ptr->list->next = list_of_matches;
         fact_ptr->list->slot = patn_ptr;

         /*============================================*/
         /* Get the fact bindings associated with this */
         /* pattern, and "drive" them to the joins     */
         /* connected to this pattern.                 */
         /*============================================*/
 
         var_list = get_struct(flink);
         var_list->next = NULL;
         var_list->count = 0;
         var_list->binds = get_struct(fbind);
         var_list->binds->whoset = fact_ptr->ID;
         var_list->binds->origin = fact_ptr;
         var_list->binds->next = NULL; 
         if (markers != NULL)
           { var_list->binds->marker = copy_marks(markers); }
         else
           { var_list->binds->marker = NULL; }
         
         var_list->next = patn_ptr->alpha;
         patn_ptr->alpha = var_list;
            
         drive_list = patn_ptr->path;
         while (drive_list != NULL)
           {   
            drive(var_list,drive_list->path,RHS);
            drive_list = drive_list->next;
           }
         finish_match = TRUE;
        }

      /*====================================================*/
      /* Compare the pattern element with the fact element. */
      /*====================================================*/
     
      if (finish_match == TRUE)
        {
         while ((patn_ptr->same_level == NULL) ||
                (patn_ptr->type == BLOCKED))
           {
            if (patn_ptr->type == BLOCKED) patn_ptr->type = SINGLE;
            patn_ptr = patn_ptr->last_level;
            if (patn_ptr == NULL) 
              { return; }
            else if (patn_ptr->type == MULTIPLE) 
              { return; }

            pat_pos--;
            cur_elem--;
           }
         patn_ptr = patn_ptr->same_level;
        }
      else if (patn_ptr->type == SINGLE)
        {
         /*==================================================*/
         /* Pattern element is a constant.  Check that the   */
         /* fact element is the same type and value, and if  */
         /* it is then proceed to the next level of compare. */
         /*==================================================*/
   
         if (patn_ptr->eval == NULL)
           {
            cur_elem++;
            pat_pos++;
            patn_ptr = patn_ptr->next_level;
           }                   
         else if (pat_compute(&elem_ptr[cur_elem],patn_ptr->eval) != FALSE)
           {
            if (patn_ptr->eval->val.fun_ptr == PTR_CONSTANT) 
              { patn_ptr->type = BLOCKED; }
            cur_elem++;
            pat_pos++;
            patn_ptr = patn_ptr->next_level; 
           }
         else
           {
            while ((patn_ptr->same_level == NULL) ||
                   (patn_ptr->type == BLOCKED))
              {
               if (patn_ptr->type == BLOCKED) patn_ptr->type = SINGLE;
               patn_ptr = patn_ptr->last_level;
               if (patn_ptr == NULL) 
                 { return; }
               else if (patn_ptr->type == MULTIPLE) 
                { return; }
               pat_pos--;
               cur_elem--;
              }
            patn_ptr = patn_ptr->same_level;
           }         
        }
      else if (patn_ptr->type == MULTIPLE)
        {
         /*======================================================*/
         /* Pattern element is a '$?'.  The type or value of the */ 
         /* fact element does not matter, however multiple paths */
         /* of comparison have to be followed since the '$?' can */
         /* bind to zero or more fact elements.                  */
         /*======================================================*/

         /* special case: next level is stop and no others at that level */
         /* $? binds to all elements.                                    */

         old_mark = markers;
         if ((patn_ptr->next_level->type == STOP) &&
             (patn_ptr->next_level->same_level == NULL))
           {
            new_mark = get_struct(fact_marker);
            new_mark->element = pat_pos;
            new_mark->start = cur_elem + 1;
            new_mark->end = length;
            new_mark->next = NULL;
            if (end_mark == NULL)
              { markers = new_mark; }
            else
              { end_mark->next = new_mark; }
              
            compare(fact_ptr,elem_ptr,patn_ptr->next_level,
                    pat_pos+1,length,markers,new_mark);
           }
         else
           {
            /* Bind to no elements */

            new_mark = get_struct(fact_marker);
            new_mark->element = pat_pos;
            new_mark->start = cur_elem + 1;
            new_mark->end = cur_elem;
            new_mark->next = NULL;
            if (end_mark == NULL)
              { markers = new_mark; }
            else
              { end_mark->next = new_mark; }
              
            compare(fact_ptr,elem_ptr,patn_ptr->next_level,
                    pat_pos+1,cur_elem,markers,new_mark);

            /* Consider rest of _bindings */
        
            end_epos = cur_elem;
            while (end_epos < length)
              {
               new_mark->element = pat_pos;
               new_mark->start = cur_elem + 1;
               new_mark->end = end_epos + 1;
               new_mark->next = NULL;
               end_epos++;
               compare(fact_ptr,elem_ptr,patn_ptr->next_level,
                       pat_pos+1,end_epos,markers,new_mark);
              }
           }

         rtn_struct(fact_marker,new_mark);
         if (end_mark != NULL) end_mark->next = NULL;
         markers = old_mark;
         current_pattern_marks = old_mark;

         while ((patn_ptr->same_level == NULL) ||
                (patn_ptr->type == BLOCKED))
           {
            if (patn_ptr->type == BLOCKED) patn_ptr->type = SINGLE;
            patn_ptr = patn_ptr->last_level;
            if (patn_ptr == NULL) 
              { return; }
            else if (patn_ptr->type == MULTIPLE) 
              { return; }

            pat_pos--;
            cur_elem--;
           }
         patn_ptr = patn_ptr->same_level;
        }
     }
  }

/**************************************************************/
/* PATTERN_COMPUTE: Performs a faster evaluation for pattern  */
/*   expressions than if generic_compute were used directly.  */
/*   This function evaluates calls to function constant,      */
/*   function notconstant, function and, and function or.  No */
/*   other function calls are made in pattern expressions.    */
/**************************************************************/
static int pat_compute(elem_ptr,pat_test)
  struct element *elem_ptr;
  struct test *pat_test;
  {
   int pass, fail;
   struct values vresult;
   
   int e1, e2;
   struct test *tptr;
   struct element *elem_a, *elem_b;
   struct fact *fact_ptr;
   int a_extent, b_extent;
   struct fact_marker *marks; 
   
   /*=============================================================*/
   /* Evaluate expressions constant and notconstant.  Calls to    */
   /* these functions are expressed in the format:                */
   /*   (constant <value>)                                        */
   /*   (notconstant <value>)                                     */
   /* Function constant returns true (1.0) if the element pointer */
   /* has the same type and value as the expression value.        */  
   /* Function notconstant acts similarly to constant except the  */
   /* return value is negated.                                    */
   /*=============================================================*/
   
   if ((pat_test->val.fun_ptr == PTR_CONSTANT) ||
       (pat_test->val.fun_ptr == PTR_NOTCONSTANT))
     {
      /*==========================================*/
      /* Determine the appropriate return values. */
      /*==========================================*/

      if (pat_test->val.fun_ptr != PTR_NOTCONSTANT)
        {
         pass = TRUE;
         fail = FALSE;
        }
      else
        {
         pass = FALSE;
         fail = TRUE;
        }

      /*========================================*/
      /* Compare the element to the test value. */
      /*========================================*/

      pat_test = pat_test->arg_list;
      if (pat_test->type != elem_ptr->type)
        { return(fail); }

      if (pat_test->type == NUMBER)
        {
         if (pat_test->val.fvalue != elem_ptr->val.fvalue)
           { return(fail); }
         else
           { return(pass); }
         }

      if (pat_test->val.hvalue != elem_ptr->val.hvalue)
        { return(fail); }

      return(pass);
     }
     
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

   else if ((pat_test->val.fun_ptr == PTR_EQ_FIELD) ||
            (pat_test->val.fun_ptr == PTR_NEQ_FIELD))
     {
      /*==========================================*/
      /* Determine the appropriate return values. */
      /*==========================================*/

      if (pat_test->val.fun_ptr == PTR_EQ_FIELD)
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

      tptr = pat_test->arg_list;
      e1 = tptr->val.index;
      tptr = tptr->next_arg;
      e2 = tptr->val.index;

      /*==============================================*/
      /* Extract the fact pointer and list of segment */
      /* markers for the first pattern.               */
      /*==============================================*/

      fact_ptr = current_pattern_fact;
      marks = current_pattern_marks;
      
      /*=============================================*/
      /* Determine the beginning point and extent of */
      /* the comparision for the first pattern.      */
      /*=============================================*/

      a_extent = b_extent = 1;
     
      if (marks != NULL)
        { 
         if (marks->element < e1)
           { 
            e1 = bump_elm_num(marks,e1,&a_extent);
           }
         else if (marks->element == e1)
           { a_extent = (marks->end - marks->start) + 1; }
           
         if (marks->element < e2)
           { 
            e2 = bump_elm_num(marks,e2,&b_extent);
           }
         else if (marks->element == e2)
           { b_extent = (marks->end - marks->start) + 1; }
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
      elem_a = &fact_ptr->atoms[e1];
      e2--;
      elem_b = &fact_ptr->atoms[e2];

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

   else if (pat_test->val.fun_ptr->ip == (int (*)()) my_or)
     {
      pat_test = pat_test->arg_list;
      while (pat_test != NULL)
        {
         if (pat_compute(elem_ptr,pat_test) == TRUE)
           { return(TRUE); } 
         pat_test = pat_test->next_arg;
        }
      return(FALSE);
     }

   /*==========================================================*/
   /* Evaluate and expressions expressed in the format:        */
   /*   (and <expression 1> <expression 2> ... <expression n>) */
   /* Returns false (0.0) if any of the expression are false,  */
   /* otherwise returns true (1.0).                            */
   /*==========================================================*/

   else if (pat_test->val.fun_ptr->ip == (int (*)()) my_and)
     {
      pat_test = pat_test->arg_list;
      while (pat_test != NULL)
        {
         if (pat_compute(elem_ptr,pat_test) == FALSE)
           { return(FALSE); } 
         pat_test = pat_test->next_arg;
        }
      return(TRUE);
     } 
     
   /*=======================================================*/
   /* Evaluate all other expressions using generic_compute. */
   /*=======================================================*/

   else
     {
      generic_compute(pat_test,&vresult);
      if (vresult.val.fvalue == 0.0)
        { return(FALSE); }
      else
        { return(TRUE); }
     }

  }
  
/******************************************************/
/* MY_GET_FIELD:  Extracts the mth pattern of a rule. */
/******************************************************/
int my_get_field(bound_var)
  VALUE_PTR bound_var;
  {
   int element;
   struct fact *fact_ptr;
   struct element *elem_ptr;
   struct fact_marker *marks;
   struct test *test_ptr;
   int extent;

   test_ptr = get_first_arg();
   element = get_test_index(test_ptr);
     
   fact_ptr = current_pattern_fact;
   marks = current_pattern_marks;

   extent = -1;

   if (marks != NULL)
     { 
      if (marks->element < element)
        { element = bump_elm_num(marks,element,&extent); }
      else if (marks->element == element)
        { extent = (marks->end - marks->start) + 1; }
     }

   if (extent != -1)
     {
      bound_var->type = MULTIPLE;
      bound_var->origin = fact_ptr;
      bound_var->begin = --element;
      bound_var->end = element + extent - 1;
      return;
     }
   else
     { extent = 1; }  

   element--;
   elem_ptr = &fact_ptr->atoms[element];

   bound_var->type = elem_ptr->type;
   if (bound_var->type == NUMBER)
     { bound_var->val.fvalue = elem_ptr->val.fvalue; }
   else
     { bound_var->val.hvalue = elem_ptr->val.hvalue; }
   return;
  }

#if ! RUN_TIME
  
/*****************************************/
/* SHOW_COMPARE_NET:                     */
/*****************************************/
show_compare_net(pat_ptr,level)
  struct pat_node *pat_ptr;
  int level;
  {
   int count = 1;
   
   while (pat_ptr != NULL)
     {
      cl_print("wdialog","At Level ");
      print_long_int("wdialog",(long int) level);
      cl_print("wdialog"," ");
      print_long_int("wdialog",(long int) count);
      cl_print("wdialog",": ");
         
      if (pat_ptr->type == STOP)
        { cl_print("wdialog","STOP\n"); }
      else
        {
        if (pat_ptr->type == SINGLE)
          { cl_print("wdialog"," ? "); }
        else
          { cl_print("wdialog","$? "); }
                            
         pp_test(pat_ptr->eval,"stdout");
         cl_print("wdialog","\n");

         show_compare_net(pat_ptr->next_level,level + 1);
        } 
      count++;
      pat_ptr = pat_ptr->same_level;
     }
  }
  
#endif
  
