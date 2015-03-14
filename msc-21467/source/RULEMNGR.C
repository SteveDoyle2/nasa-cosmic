/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                 RULE MANAGER MODULE                 */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "rule.h"
#include "clipsmem.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   struct pat_node          *network_pointer();
   struct ruleinfo          *find_rule();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct pat_node   *patn_list = NULL;
   static struct ruleinfo   *list_of_rules;
   static int                watch_rules = OFF;
   static int                watch_compilations = ON;
   
/*************************************************************/
/* FIND_RULE:  Searches for a rule in the list of rules.     */
/*   Returns a pointer to the rule if found, otherwise NULL. */
/*************************************************************/
struct ruleinfo *find_rule(rule_name)
  char *rule_name;
  {
   struct ruleinfo *rule_ptr;

   rule_ptr = list_of_rules;
   while (rule_ptr != NULL)
     {
      if (strcmp(rule_ptr->name,rule_name) == 0)
        { return(rule_ptr); }
      
      rule_ptr = rule_ptr->next; 
     }

   return(NULL);
  }
  
#if (! RUN_TIME) 

/***********************************************************/
/* excise_rule: Remove a named rule from the database.     */
/*   Returns 1 if the rule was found and removed.  Returns */
/*   0 if the rule was not found.                         */
/***********************************************************/
excise_rule(rule_name)
  char *rule_name;
  {
#if (! BLOAD_ONLY)
   struct ruleinfo *rule_ptr, *rule_before;
   int rule_found;

   /*=================================================*/
   /* Search for the named rule in the list of rules. */
   /*=================================================*/
   
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (bloaded() == TRUE) return(0);
#endif

   rule_before = NULL;
   rule_found = FALSE;
   rule_ptr = list_of_rules;
   while ((rule_ptr != NULL) && (rule_found == FALSE))
     {
      if (strcmp(rule_ptr->name,rule_name) == 0)
        { rule_found = TRUE; }
      else
        {  
         rule_before = rule_ptr;
         rule_ptr = rule_ptr->next;
        } 
     }

   if (rule_found == FALSE)
     { return(FALSE); }

   if (get_compilations_watch() == TRUE)
     { 
      cl_print("wdialog","Excising rule: ");
      cl_print("wdialog",rule_name);
      cl_print("wdialog","\n");
     }

   /*============================================================*/
   /* Clear the agenda of all the activations added by the rule. */
   /*============================================================*/

   clear_rule_from_agenda(rule_name);

   /*=============================================================*/
   /* Remove all structures associated with each pattern on the   */
   /* lhs of a rule from both the pattern and join nets.  This    */
   /* will also remove the structures associated with the actions */
   /* of the rhs of the rule.                                     */
   /*=============================================================*/

   remove_rule_network(rule_ptr->pats);
   rule_ptr->pats = NULL;
   
   /*=========================================*/
   /* Remove the rule from the list of rules. */
   /*=========================================*/

   if (rule_before == NULL)
     {
      list_of_rules = rule_ptr->next;
      rtn_ruleinfo(rule_ptr);
     }
   else
     {
      rule_before->next = rule_ptr->next;
      rtn_ruleinfo(rule_ptr);
     }

   /*=====================*/
   /* System Error Check. */
   /*=====================*/

   if ((list_of_rules == NULL) && (patn_list != NULL))
     {
      clips_system_error(1601);
      cl_exit(5);
     }

   return(TRUE);
#else
   return(0);
#endif
  }

#if (! BLOAD_ONLY)
/***********************************************************/
/* REMOVE_RULE_NETWORK:                                    */
/***********************************************************/
remove_rule_network(pat_del)
   struct patptr *pat_del;
   {
    struct patptr *next_del;
    
    while (pat_del != NULL)
      {
       next_del = pat_del->next;

       if (pat_del->pptr == NULL)
         { purge_joins(pat_del->lptr->path); } 
  
       rtn_struct(patptr,pat_del);
       pat_del = next_del; 
      }
   }
   
/***********************************************************/
/* PURGE_JOINS:                                            */
/***********************************************************/
purge_joins(join)
  struct internode *join;
  {
   struct internode *prev_join;
   struct list *tracer, *last_list;   

   
   while (join != NULL)
     {
      
      /*======================================*/
      /* Remember the join "above" this join. */
      /*======================================*/

      prev_join = join->join_above;

      /*=================================================*/
      /* If the join was attached to a pattern, remove   */
      /* any structures associated with the pattern that */
      /* are no longer needed.                           */
      /*=================================================*/
      
      if (join->entry_pat != NULL) 
        { rm_jp(join); }

      /*=======================================*/
      /* Remove any bindings left in the join. */
      /*=======================================*/

      rmv_side(join->beta);
      join->beta = NULL;
      /*
      rmv_side(join->rhs);
      join->rhs = NULL;
      */
      
      /*=================================================*/
      /* Remove the expression associated with the join. */
      /* This may either be an cross pattern evaluation  */
      /* or the actions associated with the rules.       */
      /*=================================================*/

      test_deinstall(join->eval);
      returntests(join->eval);
      test_deinstall(join->not_eval);
      returntests(join->not_eval);
      rtn_struct(internode,join);

      if (prev_join == NULL) return;
      
      last_list = NULL;
      tracer = prev_join->next;
      while (tracer != NULL)
        {
         if (tracer->path == join)
           {
            if (last_list == NULL)
              { prev_join->next = tracer->next; }
            else
              { last_list->next = tracer->next; }

            rtn_struct(list,tracer);
            tracer = NULL;
           }
         else
           {
            last_list = tracer;
            tracer = tracer->next; 
           }
         }  

      if (prev_join->next == NULL) 
        { join = prev_join; }
      else
        { join = NULL; }
     }
  
  }

/**********************************************************/
/* rm_jp:  Removes the pattern structures associated with */
/*   a join.  This includes the list structure which      */
/*   connects the pattern to the join along with any      */
/*   parts of the pattern that are no longer needed if    */
/*   this is the last join to which the pattern points.   */
/**********************************************************/
rm_jp(join)
  struct internode *join;
  {
   struct pat_node *pat_ptr;
   struct list *list_ptr, *last_list;

   /**************************************************/
   /* Determine the pattern that enters this join.   */
   /* Determine the list of joins which this pattern */
   /* points to.                                     */
   /**************************************************/

   pat_ptr = join->entry_pat;
   list_ptr = pat_ptr->path;
   last_list = NULL;
   
   /***************************************************/
   /* Loop through the list of join pointers that the */
   /* the pattern feeds to until this join is found.  */
   /* Remove the list pointer from that pattern.      */
   /***************************************************/

   while (list_ptr != NULL)
     {
      if (list_ptr->path == join)
        {
         if (last_list == NULL)
           { pat_ptr->path = list_ptr->next; }
         else
           { last_list->next = list_ptr->next; }

         rtn_struct(list,list_ptr);
         list_ptr = NULL;
        }
      else
        {
         last_list = list_ptr;
         list_ptr = list_ptr->next; 
        }
     }

   /*****************************************************/
   /* If the terminal node of the pattern doesn't point */
   /* to any joins, then start removing the pattern.    */
   /*****************************************************/

   if (pat_ptr->path == NULL)
     {   
      fact_clear(pat_ptr);
      detach_pattern(pat_ptr); 
     }  
  }  


/*************************************************************/
/* detach_pattern:  Detaches a pattern from the pattern net. */
/*   The pattern net is a tree-like structure.               */
/*   Detachment of a pattern involves starting at the end    */ 
/*   of the pattern in the net (the terminating leaf) and    */ 
/*   removing the longest "limb" of the pattern.             */
/*   Example:                                                */
/*     Patterns (a b c d) and (a b e f) would be represented */
/*     by the pattern net shown on the left.  If (a b c d)   */
/*     was detached, the resultant pattern net would be the  */
/*     one shown on the right. The '=' represents an end of  */
/*     of pattern or STOP marker.                            */  
/*                                                           */
/*           a                  a                            */
/*           |                  |                            */
/*           b                  b                            */
/*           |                  |                            */
/*           c--e               e                            */
/*           |  |               |                            */
/*           d  f               f                            */
/*           |  |               |                            */
/*           =  =               =                            */
/*                                                           */
/*************************************************************/
detach_pattern(pat_ptr)
  struct pat_node *pat_ptr;
  {
   struct pat_node *upper_level;

   /*========================*/
   /* System Error Checking. */
   /*========================*/

   if (pat_ptr->next_level != NULL) 
     {
      clips_system_error(1602);
      cl_exit(5);
     }

   /*==============================================================*/
   /* Loop until all appropriate pattern nodes have been detached. */
   /*==============================================================*/

   upper_level = pat_ptr;
   while (upper_level != NULL)
     {
      if ((upper_level->prev == NULL) &&
          (upper_level->same_level == NULL))
        {
         /*===============================================*/
         /* Pattern node is the only node on this level.  */
         /* Remove it and continue detaching other nodes  */
         /* above this one, because no other patterns are */
         /* dependent upon this node.                     */
         /*===============================================*/
         
         pat_ptr = upper_level;
         upper_level = pat_ptr->last_level;
         if (upper_level == NULL) 
           { patn_list = NULL; }

         test_deinstall(pat_ptr->eval);
         returntests(pat_ptr->eval);
         rtn_struct(pat_node,pat_ptr);
        }
      else if (upper_level->prev != NULL)
        {
         /*====================================================*/
         /* Pattern node has another pattern node which must   */
         /* be checked preceding it.  Remove the pattern node, */
         /* but do not detach any nodes above this one.        */      
         /*====================================================*/

         pat_ptr = upper_level;
         upper_level->prev->same_level = upper_level->same_level;
         if (upper_level->same_level != NULL)
           { upper_level->same_level->prev = upper_level->prev; }

         test_deinstall(pat_ptr->eval);
         returntests(pat_ptr->eval);
         rtn_struct(pat_node,pat_ptr);
         upper_level = NULL;
        }
      else
        {
         /*====================================================*/
         /* Pattern node has no pattern node preceding it, but */
         /* does have one succeeding it.  Remove the pattern   */
         /* node, but do not detach any nodes above this one.  */      
         /*====================================================*/

         pat_ptr = upper_level;
         upper_level = upper_level->last_level;
         if (upper_level == NULL)
           { patn_list = pat_ptr->same_level; }
         else
           { upper_level->next_level = pat_ptr->same_level; }
         pat_ptr->same_level->prev = NULL;

         test_deinstall(pat_ptr->eval);
         returntests(pat_ptr->eval);
         rtn_struct(pat_node,pat_ptr);
         upper_level = NULL;
        }
     }
   return(TRUE);
  }
#endif
#endif
  
/*************************************************************/
/* GET_NEXT_RULE:                                            */
/*************************************************************/
struct ruleinfo *get_next_rule(rule_ptr)
  struct ruleinfo *rule_ptr;
  {
   if (rule_ptr == NULL)
     { return(list_of_rules); }
     
   return(rule_ptr->next);
  }
  
/*************************************************************/
/* GET_RULE_PPFORM:                                            */
/*************************************************************/
char *get_rule_ppform(rule_ptr)
  struct ruleinfo *rule_ptr;
  {
   if (rule_ptr == NULL)
     { return(NULL); }
     
   return(rule_ptr->pp_form);
  }
  
/*************************************************************/
/* GET_RULE_NAME:                                            */
/*************************************************************/
char *get_rule_name(rule_ptr)
  struct ruleinfo *rule_ptr;
  {
   if (rule_ptr == NULL)
     { return(NULL); }
     
   return(rule_ptr->name);
  }
  
/*************************************************************/
/* GET_RULE_NUM:                                            */
/*************************************************************/
struct ruleinfo *get_rule_num(count)
  int count;
  {
   struct ruleinfo *rule_ptr;
   int i = 1;
   
   rule_ptr = list_of_rules;
   while ((rule_ptr != NULL) && (i < count))
     { 
      rule_ptr = rule_ptr->next; 
      i++;
     }
   return(rule_ptr);
  }
  
/*********************************************************************/
/* SET_RULES_WATCH: */
/*********************************************************************/
set_rules_watch(value)
  int value;
  {
   watch_rules = value;
  }
  
/*********************************************************************/
/* GET_RULES_WATCH: */
/*********************************************************************/
get_rules_watch()
  { return(watch_rules); }
  
/*********************************************************************/
/* SET_COMPILATIONS_WATCH: */
/*********************************************************************/
set_compilations_watch(value)
  int value;
  {
   watch_compilations = value;
  }
  
/*********************************************************************/
/* GET_COMPILATIONS_WATCH: */
/*********************************************************************/
get_compilations_watch()
  { return(watch_compilations); }
  
/*********************************************************************/
/* NETWORK_POINTER: */
/*********************************************************************/
struct pat_node *network_pointer()
  {
   return(patn_list);
  }
  
/*********************************************************************/
/* SET_NETWORK_POINTER: */
/*********************************************************************/
set_network_pointer(value)
  struct pat_node *value;
  {
   patn_list = value;
  }
    
/*********************************************************************/
/* ADD_RULE: */
/*********************************************************************/
add_rule(temp_rule)
  struct ruleinfo *temp_rule;
  {
   struct ruleinfo *chase_rule;
   
   chase_rule = list_of_rules;

   if (chase_rule == NULL)
     { list_of_rules = temp_rule; }
   else
     {
      while (chase_rule->next != NULL)
        { chase_rule = chase_rule->next; }
      chase_rule->next = temp_rule;
     }
  }
