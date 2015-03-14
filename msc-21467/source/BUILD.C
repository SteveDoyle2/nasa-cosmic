/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                     BUILD MODULE                    */
   /*******************************************************/
 
#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "constant.h"
#include "clipsmem.h"
#include "variable.h"
#include "rule.h"


/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   struct list              *reuse_join();
   int                       conn_pat_to_join();
   struct pat_node          *place_pattern();
   int                       same_thing();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                       add_pat_list();
   struct internode         *construct_joins();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct test       *get_join_expr();
   extern struct test       *get_not_expr();
   extern char               get_join_logic();
   extern struct test       *get_pn_expr();
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct funtab     *PTR_CONSTANT;
   extern int                LOAD_FLAG;

/********************************************************************/
/* construct_joins:  Takes the logical representation of a rule and */
/*   builds the set of linked joins which represents the left hand  */
/*   side of the rule.  Returns the last join constructed which     */
/*   functions as a link between the join net and the right hand    */
/*   side actions of the rule.                                      */
/********************************************************************/
struct internode *construct_joins(top_pat_node,rule_ptr)
  struct pat_node **top_pat_node;
  struct ruleinfo *rule_ptr;
  {
   struct internode *last_join, *join;
   struct pat_node *last_pat;
   int first_join = TRUE;
   char logic = '+';
   int try_to_reuse = TRUE;
   struct list *temp_list, *old_join_list, *list_of_joins;
   char lhs_log;
   int total_joins, join_number;
   struct test *join_test, *not_test;

   last_join = NULL;
   
   /*=======================================================*/
   /* Process each of the patterns and/or test constructs   */
   /* of the rule.  At this point, there should be no lower */
   /* level 'and' or 'or' logic structures.                 */
   /*=======================================================*/ 
   
   total_joins = count_joins();
   join_number = 1;
   
   while (join_number <= total_joins)
     {
      logic = get_join_logic(join_number);
      if (logic == '?') 
        {
         clips_system_error(201);
         cl_exit(4);
        }
     
      if (first_join == TRUE)
        { lhs_log = 'e'; }
      else
        { lhs_log = '+'; }

      last_pat = place_pattern(*top_pat_node,NULL,
                               join_number,1,get_elem_count(join_number),top_pat_node);

      if (first_join == TRUE)
        { list_of_joins = last_pat->path; }
      else
        { list_of_joins = last_join->next; }
        
      join_test = get_join_expr(join_number);
      not_test = get_not_expr(join_number);

      if ((try_to_reuse == TRUE) && 
          ((old_join_list = reuse_join(list_of_joins,lhs_log,logic,
                                  join_test,not_test,last_pat->path)) != NULL) ) 
        {
         add_pat_list(last_pat,old_join_list,rule_ptr);

         if ((get_compilations_watch() == TRUE) && (LOAD_FLAG == TRUE))
           { cl_print("wdialog","=j"); }
         last_join = old_join_list->path;
         returntests(join_test);
         returntests(not_test);
        }
      else
        {
         try_to_reuse = FALSE;

         if ((get_compilations_watch() == TRUE) && (LOAD_FLAG == TRUE))
           { cl_print("wdialog","+j"); }

         join = get_struct(internode);
         join->beta = NULL;
         join->eval = NULL;
         join->not_eval = NULL;
         join->join_above = NULL;
         join->entry_pat = NULL;
         join->next = NULL;
         join->lhs_log = lhs_log;
         join->id = 0;

         if (lhs_log == '+')
           {
            temp_list = last_join->next;      
	        last_join->next = get_struct(list);
            last_join->next->next = temp_list;
            last_join->next->path = join;        
           }

         conn_pat_to_join(join,last_pat,logic,rule_ptr);

         join->join_above = last_join;
         join->entry_pat = last_pat;
         last_join = join;


         join->eval = join_test;
         test_install(join_test);
         join->not_eval = not_test;
         test_install(not_test);
        }

      /*===============================================*/
      /* Point to the next pattern and the expressions */
      /* associated with the join for that pattern.    */
      /*===============================================*/
          
      join_number++;
      first_join = FALSE;
     }
   
   if ((get_compilations_watch() == TRUE) && (LOAD_FLAG == TRUE))
     { cl_print("wdialog","\n"); }

   return(last_join);
  }

/**********************************************************************/
/* REUSE_JOIN:                                                        */
/**********************************************************************/
static struct list *reuse_join(test_list,lhs_log,rhs_log,join_test,not_test,pat_list)
  struct list *test_list, *pat_list;
  char lhs_log, rhs_log;
  struct test *join_test, *not_test;
  {
   struct internode *old_join;
   struct list *list_ptr;

   while (test_list != NULL)
     {
      old_join = test_list->path;
      
      if ((old_join->lhs_log == lhs_log) &&
          (old_join->rhs_log == rhs_log) &&
          (same_thing(old_join->eval,join_test) == TRUE) &&
          (same_thing(old_join->not_eval,not_test) == TRUE) )
        {
         list_ptr = pat_list;
         while (list_ptr != NULL)
           {
            if (list_ptr->path == old_join) 
              { return (test_list); }
            list_ptr = list_ptr->next;
           }
        }
      test_list = test_list->next;
     }

   return (NULL);
  }


/******************************************************************/
/* conn_pat_to_join: Connects the end of a pattern in the pattern */
/*   net to its corresponding join in the join net.               */
/******************************************************************/
static int conn_pat_to_join(join,pattern,boolean,rule_ptr)
  struct internode *join;
  struct pat_node *pattern;
  char boolean;
  struct ruleinfo *rule_ptr;
  {
   struct list *pat_to_join;

   /*======================================================*/
   /* Connect the pattern net to the join net for a single */
   /* pattern of a rule.                                   */
   /*======================================================*/

   pat_to_join = get_struct(list);
   pat_to_join->next = NULL;
   pat_to_join->path = join;
   join->rhs_log = boolean;
    
   pat_to_join->next = pattern->path; 
   pattern->path = pat_to_join;

   /*============================================================*/
   /* Keep track for each rule how it is attached to the pattern */
   /* network.  This information will be used by other commands  */
   /* such as excise and clear.                                  */
   /*============================================================*/

   add_pat_list(pattern,pattern->path,rule_ptr);
  }

/*****************************************************************/
/* add_pat_list:                                                 */
/*****************************************************************/
int add_pat_list(pattern,join_list,rule_ptr)
  struct pat_node *pattern;
  struct list *join_list;
  struct ruleinfo *rule_ptr;
  {
   struct patptr *tmp_ptr;

   if (rule_ptr->pats == NULL)
     {
      rule_ptr->pats = get_struct(patptr);
      tmp_ptr = rule_ptr->pats;
     }
   else
     { 
      tmp_ptr = rule_ptr->pats; 
      while (tmp_ptr->next != NULL) 
        { tmp_ptr = tmp_ptr->next; }
      tmp_ptr->next = get_struct(patptr);
      tmp_ptr = tmp_ptr->next;
     }
   
   tmp_ptr->next = NULL;
   tmp_ptr->pptr = pattern;
   tmp_ptr->lptr = join_list;
  }   

/*****************************************/
/* place_pattern                         */
/*****************************************/
static struct pat_node *place_pattern(pattern_list,upper_level,
                                      pat,elem,last_elem,top_pat_node)
  struct pat_node *pattern_list;
  struct pat_node *upper_level;
  int pat, elem;
  struct pat_node **top_pat_node;
  {
   struct test *basic_list;
   struct pat_node *cur_elem, *new_elem;
   struct pat_node *last_look = NULL;
   int same_type, cn_test;
   int pn_type;

   /*=================================================*/
   /* Check for a match on this level of the pattern  */
   /* network.                                        */
   /*=================================================*/

   cur_elem = pattern_list;
   basic_list = get_pn_expr(pat,elem);
      
   while (cur_elem != NULL)
     {
      if ((cur_elem->type == STOP) && (elem > last_elem))
        { return(cur_elem); }
        
      pn_type = get_node_type(pat,elem);
      if (pn_type == KUNKNOWN)
        {
         clips_system_error(202);
         cl_exit(4);
        }

      if (cur_elem->type == pn_type)
        { same_type = TRUE; }
      else
        { same_type = FALSE; }
        
        
      if ((same_type == TRUE) &&
          (same_thing(cur_elem->eval,basic_list) == TRUE))
        {
         returntests(basic_list);   
         cur_elem = place_pattern(cur_elem->next_level,cur_elem,
                                  pat, elem+1,last_elem,top_pat_node);
         return(cur_elem);         
        }
      else
        {
         last_look = cur_elem; 
         cur_elem = cur_elem->same_level;
        }
     }
   
   cn_test = FALSE;
   if (basic_list != NULL)
     {
      if (basic_list->val.fun_ptr == PTR_CONSTANT)
        { cn_test = TRUE; }
     }

   /*==================================================*/
   /* Pattern node not found.  Need to add new pattern */
   /* node into the pattern network.                   */
   /*==================================================*/

   new_elem = get_struct(pat_node);
   new_elem->last_level = upper_level;
   new_elem->next_level = NULL;
   new_elem->same_level = NULL;
   new_elem->prev = NULL;
   new_elem->eval = NULL;
   new_elem->path = NULL;
   new_elem->alpha = NULL;

   if (last_look == NULL)
     {
      if (upper_level == NULL)
        { *top_pat_node = new_elem; }
      else
        { upper_level->next_level = new_elem; }
     }
   else
     {
      if (cn_test == TRUE)
        {
         last_look->same_level = new_elem;
         new_elem->prev = last_look;
        }
      else if (upper_level != NULL)
        {
         new_elem->same_level = upper_level->next_level;
         if (upper_level->next_level != NULL)
           { upper_level->next_level->prev = new_elem; }
         upper_level->next_level = new_elem;
        }
      else
        {
         new_elem->same_level = *top_pat_node;
         if (*top_pat_node != NULL)
           { (*top_pat_node)->prev = new_elem; }
         *top_pat_node = new_elem;
        }
     }

   /*=============================================================*/
   /* No test indicates that the end of pattern has been reached. */
   /* The pattern has now been installed in the pattern net.      */
   /*=============================================================*/

   if (elem > last_elem)
     {
      new_elem->type = STOP;
      new_elem->eval = NULL;
      return(new_elem);
     }

   /*=============================================================*/
   /* If this element of the pattern was a segment variable, then */
   /* the pattern node will perform a multiple bind when pattern  */
   /* matching.  Otherwise a single bind will occur.              */
   /*=============================================================*/  
   
    pn_type = get_node_type(pat,elem);
      if (pn_type == KUNKNOWN)
        {
         clips_system_error(203);
         cl_exit(4);
        }
        
    new_elem->type = pn_type;

   /*=============================================================*/
   /* If the test to be performed at the pattern node is a "nop"  */
   /* (i.e. no test), then set the evaluation slot to NULL rather */
   /* than storing the test.  Otherwise store the test.           */
   /*=============================================================*/
  
   new_elem->eval = basic_list;
   test_install(basic_list);
     

   /*===========================================================*/
   /* Place the next level of the pattern into the pattern net. */
   /*===========================================================*/
         
   new_elem = place_pattern(NULL,new_elem,pat,elem+1,last_elem,top_pat_node);
   return(new_elem);
  }
 
/*****************************************/
/* same_thing                            */
/*****************************************/
static int same_thing(check_elem,basic_list)
  struct test *check_elem;
  struct test *basic_list;
  {
   if ((check_elem == NULL) && (basic_list == NULL))
     { return(TRUE); }  

   if ((check_elem != NULL) && (basic_list == NULL))
     { return(FALSE); }

   if ((check_elem == NULL) && (basic_list != NULL))
     { return(FALSE); }

   if (check_elem->type != basic_list->type)
     { return(FALSE); }

   if (check_elem->type == NUMBER)
     {
      if (check_elem->val.fvalue != basic_list->val.fvalue)
        { return (FALSE); }
     }
   else if (check_elem->type == INDEX)
     {
      if (check_elem->val.index != basic_list->val.index)
        { return (FALSE); }
     }
   else
     {
      if (check_elem->val.hvalue != basic_list->val.hvalue)
        { return (FALSE); }
     }

   if (same_thing(check_elem->arg_list,basic_list->arg_list) == FALSE)
     { return(FALSE); }

   if (same_thing(check_elem->next_arg,basic_list->next_arg) == FALSE)
     { return(FALSE); }

   return(TRUE);
  }
  
#endif
