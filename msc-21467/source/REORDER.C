/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   REORDER MODULE                    */
   /*******************************************************/
  
#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)
 
#include <stdio.h>

#include "constant.h"
#include "lhsparse.h"
#include "clipsmem.h"

/****************************************/
/* LOCAL INTERNAL FUNCTIONS DEFINITIONS */
/****************************************/

   struct node            *adjacent_reduction();
   struct node            *reverse_or();
   
/*****************************************/
/* GLOBAL INTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/

   struct node            *copy_nodes();
   struct node            *reorder_patterns();
   
/*****************************************/
/* GLOBAL EXTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/

   extern struct test     *copy_tests();
      
/**************************************************/
/* REORDER_PATTERNS                               */
/**************************************************/
struct node *reorder_patterns(ao_list)
  struct node *ao_list;
  {
   struct node *args;
   struct node *before, *save;
   int count;
   
   ao_list = adjacent_reduction(ao_list);

   if (ao_list->type == PAT_AND)
     {
      count = 1;
      args = ao_list->right;
      while (args != NULL)
        {
         if (args->type == PAT_OR)
           {
            ao_list = reverse_or(ao_list,args->right,count);
            args = NULL;
           }
         else
           {
            count++;
            args = args->bottom;
           }
        }
     }

   ao_list = adjacent_reduction(ao_list);

   before = NULL;
   args = ao_list->right;

   while (args != NULL)
     {
      save = args->bottom;
      if ((args->type == PAT_AND) || 
          (args->type == PAT_OR))
        {
         if (before == NULL)
           {
            args->bottom = NULL;
            ao_list->right = reorder_patterns(args);
            ao_list->right->bottom = save;
            before = ao_list->right;
           }
         else
           {
            args->bottom = NULL;
            before->bottom = reorder_patterns(args);
            before->bottom->bottom = save;
            before = before->bottom;
           }
        }
      args = save;
     }

   ao_list = adjacent_reduction(ao_list);

   return(ao_list);
  }

/**************************************************/
/* REVERSE_OR: Switches and/or constructs into    */
/*   or/and constructs.                           */ 
/*   For example:                                 */
/*     (and (or a b) (or c d))                    */
/*   would be converted to                        */
/*     (or (and a (or c d)) (and b (or c d))),    */
/*   if the or pointer were pointing at (or a b). */
/**************************************************/
static struct node *reverse_or(ao_list,or_ptr,or_num)
  struct node *ao_list;
  struct node *or_ptr;
  int or_num;
  {
   int count;
   struct node *my_list = NULL;
   struct node *last_add = NULL;
   struct node *new_list, *replace_slot;

   while (or_ptr != NULL)
     {
      new_list = copy_nodes(ao_list);
      count = 1;
      replace_slot = new_list->right;
      while (count != or_num)
        {
         replace_slot = replace_slot->bottom;
         count++;
        }
      returnnodes(replace_slot->right);
      replace_slot->type = or_ptr->type;
      replace_slot->state = or_ptr->state;
      replace_slot->fvalue = or_ptr->fvalue;
      replace_slot->svalue = or_ptr->svalue;
      replace_slot->right = copy_nodes(or_ptr->right);
      replace_slot->expression = copy_tests(or_ptr->expression);

      if (last_add == NULL)
        { 
         my_list = new_list;
         new_list->bottom = NULL;
         last_add = new_list;
        }
      else
        {
         last_add->bottom = new_list;
         new_list->bottom = NULL;
         last_add = new_list;
        }

      or_ptr = or_ptr->bottom;
     }

   returnnodes(ao_list); 
   
   new_list = get_struct(node);
   new_list->type = PAT_OR;
   new_list->svalue = NULL;
   new_list->expression = NULL;
   new_list->bottom = NULL;
   new_list->right = my_list;
   
   return(new_list);
  }
 
/***********************************************************/
/* ADJACENT_REDUCTION:  Given the represention of the lhs  */
/*   of a rule, this routine remove redundant information. */
/*   Logicial Pattern And's within and's and or's within   */
/*   or's are simplified.                                  */
/*                                                         */
/*   For example:                                          */
/*     (or (and (and (a) (b)) (and (c) (d))))              */
/*   would be converted to                                 */
/*     (or (and (a) (b)) (and (c) (d)))                    */
/*                                                         */
/*   The and's and or's to be simplified must be adjacent. */
/*   For example, the following lhs would not be           */
/*   simplified by this routine:                           */
/*     (or (and (or (a))))                                 */
/***********************************************************/
static struct node *adjacent_reduction(ao_list)
  struct node *ao_list;
  {
   struct node *node_ptr, *arg_ptr;
   struct node *last_arg, *next_arg, *temp_arg;

   if (ao_list == NULL) 
     {  return(NULL); }

   node_ptr = ao_list;
   while (node_ptr != NULL)
     {
      if ((node_ptr->type == PAT_AND) ||
          (node_ptr->type == PAT_OR))
        { node_ptr->right = adjacent_reduction(node_ptr->right); } 
      node_ptr = node_ptr->bottom;
     }

   node_ptr = ao_list;
   while (node_ptr != NULL)
     {

      arg_ptr = node_ptr->right;
      last_arg = NULL;

      while (arg_ptr != NULL)
        {
         next_arg = arg_ptr->bottom;

         if ((arg_ptr->type == node_ptr->type) && 
             ((node_ptr->type == PAT_AND) || 
              (node_ptr->type == PAT_OR)))
           {  
            temp_arg = arg_ptr->right;
            arg_ptr->right = NULL;
            arg_ptr->bottom = NULL;
            returnnodes(arg_ptr);

            if (last_arg == NULL)
              { node_ptr->right = temp_arg; }
            else
              { last_arg->bottom = temp_arg; }

            while (temp_arg->bottom != NULL)
              { temp_arg = temp_arg->bottom; }
            
            temp_arg->bottom = next_arg;
            last_arg = temp_arg;
           }
         else
           { last_arg = arg_ptr; }

         arg_ptr = next_arg;
        }

      node_ptr = node_ptr->bottom;
     }

   return(ao_list);
  }

/**************************************************/
/* COPY_NODES                                     */
/**************************************************/
struct node *copy_nodes(node_list)
  struct node *node_list;
  {
   struct node *head;

   if (node_list == NULL)
     { return(NULL); }

   head = get_struct(node);
   head->type = node_list->type;
   head->state = node_list->state;
   head->fvalue = node_list->fvalue;
   head->svalue = node_list->svalue;
   head->expression = copy_tests(node_list->expression);
   head->right = copy_nodes(node_list->right);
   head->bottom = copy_nodes(node_list->bottom);

   return(head);
  }
  
#endif

