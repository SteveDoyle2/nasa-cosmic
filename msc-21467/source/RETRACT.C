/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   RETRACT MODULE                    */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "clipsmem.h"
#include "network.h"

/**************/
/* STRUCTURES */
/**************/

struct rdriveinfo
  {
   struct flink *link;
   struct list *jlist;
   struct rdriveinfo *next;
  };
  
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct rdriveinfo *d_r_list = NULL;
   
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                     p_retract();
   int                     enn_retract();
   int                     pnn_retract();
   int                     returnfms();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                     match_retract();
   struct flink           *deletebinds();
   int                     returnbinds();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct fbind    *newnid();
   extern struct fbind    *copy_binds();

/*************************************************************/
/* MATCH_RETRACT:  Retracts a fact from the fact list given a */
/*   pointer to the fact.                                    */
/*************************************************************/
match_retract(match_list,fact_num)
  struct match *match_list;
  FACT_ID fact_num;
  {
   struct match *temp_match;
   struct internode *join;
   struct list *join_list;
   int found_id;

   while (match_list != NULL)
     {
      /*================================================*/
      /* Loop through the list of all joins attached to */
      /* this pattern.                                  */
      /*================================================*/

      join_list = match_list->slot->path;
        
      while (join_list != NULL)
        {
	     join = join_list->path;
         
         /*========================================*/
         /* Handle retract based on logic of join. */
         /*========================================*/

	     if (join_list->path->rhs_log == '-')
	       {
            if (join->lhs_log == 'e')
              { enn_retract(fact_num,join); }
            else if (join->lhs_log == '+')
              { pnn_retract(join,fact_num); }
            else if (join->lhs_log == '-')
              {
               clips_system_error(601);
               cl_exit(5);
              }    
	       }
         else 
           { p_retract(RHS,join,fact_num); }

         join_list = join_list->next;
        }

      match_list->slot->alpha = deletebinds(fact_num,match_list->slot->alpha,&found_id);
      temp_match = match_list->next;
      rtn_struct(match,match_list); 
      match_list = temp_match;
     } 
  
   drive_retractions();
  }

/******************************************************/
/* p_retract:  Handles retract for join with positive */
/*   entry logic.                                     */
/******************************************************/
static int p_retract(dir,join,fact_num)
  FACT_ID fact_num;
  int dir;
  struct internode *join;
  {
   struct list *list_of_joins;   
   int found_id;
   
   while (join->next != NULL)
     {
      /*=========================================*/
      /* Remove the bindings from this join that */
      /* contain the fact to be retracted.       */
      /*=========================================*/

      if (dir == RHS)
        { found_id = TRUE; }
      else
        { 
         if (join->beta == NULL) return; /* optimize */
         join->beta = deletebinds(fact_num,join->beta,&found_id); 
        }

      /*===================================================*/
      /* If no facts were deleted at this join, then there */
      /* is no need to check joins at a lower level.       */
      /* Global Variable FOUND_ID is set by deletebinds.   */
      /*===================================================*/

      if (found_id == FALSE) return;

      /*==================================================*/
      /* If there is more than one join below this join,  */
      /* then recursively remove fact bindings from all   */
      /* but one of the lower joins.  Remove the bindings */
      /* from the other join through this loop.           */
      /*==================================================*/
      
      list_of_joins = join->next;
      while (list_of_joins->next != NULL)
        {
         p_retract(LHS,list_of_joins->path,fact_num);
         list_of_joins = list_of_joins->next;
        }
  
      dir = LHS;
      join = list_of_joins->path;      
     }
  }
  
/******************************************************************/
/* enn_retract:  Handles retract for join with entry direction    */
/*   from rhs, negative rhs logic, and no entry from lhs (empty). */ 
/******************************************************************/
static int enn_retract(fact_num,join)
  FACT_ID fact_num;
  struct internode *join;
  {
   struct flink *nlr;
   struct flink *rhs_ptr;
   int join_test;
   struct rdriveinfo *temp_dr;
   
   /*===========================*/
   /* Network Assumption Check. */
   /*===========================*/

   if (join->eval != NULL)
     {
      clips_system_error(602);
      cl_exit(5);
     }

   /*===============================================*/
   /* Delete all list of bindings from the rhs that */
   /* contain the fact to be retracted.             */
   /*===============================================*/

   rhs_ptr = join->entry_pat->alpha;
   while (rhs_ptr != NULL)
     {
      if (find_id(fact_num,rhs_ptr->binds) == TRUE)
        { join->id--; }
      rhs_ptr = rhs_ptr->next;
     }

   if (join->id != 0) return;
   
   /*=====================================================*/
   /* Check test associated with not. Note that since the */
   /* not was the first pattern in the rule, no variables */
   /* can be accessed by the expression.                  */
   /*=====================================================*/
   
   if (join->not_eval != NULL)
     {
      join_test = join_compute(join->not_eval,NULL,NULL);
      if (join_test == FALSE) return;
     }
     
   /*====================================================*/
   /* If the rhs is null, then the not pattern has been  */
   /* satisfied and a partial match needs to be created. */
   /*====================================================*/

   nlr = get_struct(flink);
   nlr->count = 0;
   nlr->next = NULL;                  
   nlr->binds = newnid();
   join->id = nlr->binds->whoset;

   temp_dr = get_struct(rdriveinfo);
   temp_dr->link = nlr;
   temp_dr->jlist = join->next;
   temp_dr->next = d_r_list;
   d_r_list = temp_dr;
  }

/***************************************************************/
/* PNN_RETRACT:  Handles retract for join with entry direction */
/*   from rhs, negative rhs logic, and positive lhs logic.     */
/***************************************************************/
static int pnn_retract(join,fact_num)
  struct internode *join;
  FACT_ID fact_num;
  { 
   struct flink *nlr, *rhs_ptr, *lhs_ptr;
   int join_test;
   struct fbind *temp;
   struct rdriveinfo *temp_dr;
         
   /*===============================================*/
   /* Loop through all rhs bindings looking for the */
   /* fact that is to be retracted in the bindings. */
   /*===============================================*/

   rhs_ptr = join->entry_pat->alpha;
   while (rhs_ptr != NULL)
     {
      if (find_id(fact_num,rhs_ptr->binds) == TRUE)
        {
         /*==================================================*/
         /* Fact was found in rhs bindings. Loop through all */ 
         /* lhs bindings checking for sets that satisfied    */
         /* the join expression.                             */
         /*==================================================*/

         lhs_ptr = join->beta;
         while (lhs_ptr != NULL)
           {
            /*================================================*/
            /* Evaluate join expression with this combination */
            /* of rhs and lhs fact bindings.                  */
            /*================================================*/

            if (join->eval == NULL)
              { join_test = TRUE; }
            else
              { join_test = join_compute(join->eval,lhs_ptr->binds,rhs_ptr->binds); }

            /*===========================*/
            /* Check for a system error. */
            /*===========================*/

            if ((join_test != FALSE) && (lhs_ptr->count <= 0))
              {
               clips_system_error(603);
               cl_exit(5);
              }

            /*================================================*/
            /* If the join expression evaluated to true, then */
            /* the lhs bindings will have one less set of     */
            /* of bindings that did not conflict with it.     */
            /*================================================*/

            if (join_test != FALSE)
              { lhs_ptr->count--; }

            /*==================================================*/
            /* If the lhs bindings now has no rhs bindings that */
            /* do not conflict with it, then it satisfies the   */
            /* the conditions of the rhs not pattern. Create a  */
            /* partial match and send it to the joins below.    */
            /*==================================================*/
            /* NOTE - Does not have to be performed unless count is zero. */
            if (lhs_ptr->count == 0)
            {
            if (join->not_eval == NULL)
              { join_test = TRUE; }
            else
              { join_test = join_compute(join->not_eval,lhs_ptr->binds,NULL); }
            }
                 
            if ((lhs_ptr->count == 0) && (join_test == TRUE))
              {
               nlr = get_struct(flink);
               nlr->count = 0;
               nlr->next = NULL;
               nlr->binds = copy_binds(lhs_ptr->binds);
               temp = nlr->binds;
               while (temp->next != NULL)
                 { temp = temp->next; }
               temp->next = newnid();
               lhs_ptr->count = temp->next->whoset;

               temp_dr = get_struct(rdriveinfo);
               temp_dr->link = nlr;
               temp_dr->jlist = join->next;
               temp_dr->next = d_r_list;
               d_r_list = temp_dr;
              }

            lhs_ptr = lhs_ptr->next;
           }
        }
      rhs_ptr = rhs_ptr->next;
     }
  }
  
/************************************************************/
/* DELETEBINDS: Searches through a set of lists of fact    */ 
/*   bindings and removes any list of bindings that contain */
/*   the fact id fact_num.                                  */
/************************************************************/
struct flink *deletebinds(fact_num,list_of_binds,found_id)
  FACT_ID fact_num;                 
  struct flink *list_of_binds;   
  int *found_id;
  {
   struct flink *head, *past_bind, *next_bind;

   past_bind = NULL;
   head = list_of_binds;

   *found_id = FALSE;
   while (list_of_binds != NULL)
     {
      if (find_id(fact_num,list_of_binds->binds) == TRUE)
        {
         *found_id = TRUE;
         if (list_of_binds == head)
           {
            /* Delete bind at beginning of list. */
	        next_bind = list_of_binds->next;
            returnbinds(list_of_binds->binds);
            rtn_struct(flink,list_of_binds);
	        list_of_binds = next_bind;
	        head = list_of_binds;
           }
         else
           {
            /* Delete bind after beginning of list. */
            past_bind->next = list_of_binds->next;
	        returnbinds(list_of_binds->binds);
            rtn_struct(flink,list_of_binds);
	        list_of_binds = past_bind->next;
           }
        }
      else
        {
         past_bind = list_of_binds;
         list_of_binds = list_of_binds->next;
        }
     }
   return(head);
  }
  

/*********************************************************/
/* returnbinds:  Returns a multiply linked list of fbind */
/*   structures to the list of free fbinds.              */
/*********************************************************/
returnbinds(waste)
  struct fbind *waste;
  {
   struct fbind *temp;

   while (waste != NULL)
     {
      temp = waste->next;
      
      if (waste->marker != NULL) returnfms(waste->marker);
      
      rtn_struct(fbind,waste);

      waste = temp;
     }
  }
  
/************************************************************/
/* returnfms                                                */
/************************************************************/
static int returnfms(waste)
  struct fact_marker *waste;
  {
   struct fact_marker *temp;

   while (waste != NULL)
     {
      temp = waste->next;
      rtn_struct(fact_marker,waste);
      waste = temp;
     }
  }

/***********************************/
/* drive_retractions:              */
/***********************************/
drive_retractions()
  {
   struct rdriveinfo *temp_dr;
   struct flink *clinker;
   struct list *list_of_joins;
   
   while (d_r_list != NULL)
     {
      list_of_joins = d_r_list->jlist;
      while (list_of_joins->next != NULL)
        {
         clinker = get_struct(flink);
         clinker->next = NULL;
         clinker->count = 0;
         clinker->binds = copy_binds(d_r_list->link->binds);
         drive(clinker,list_of_joins->path,LHS);
         list_of_joins = list_of_joins->next;
        }

      drive(d_r_list->link,list_of_joins->path,LHS);
      
      temp_dr = d_r_list->next;
      rtn_struct(rdriveinfo,d_r_list);
      d_r_list = temp_dr;
     }
  }
   
   
            

