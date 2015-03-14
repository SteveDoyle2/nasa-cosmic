#include <stdio.h>
#include "crsv.h"

/* ==================================================================== */
/*   This file contains functions that allow CRSV to store information  */
/*   about nested-if levels so that it may give intelligent error       */
/*   messages when multiple retracts are encountered.  The first few    */
/*   functions are general to nested if's, else's, and while's, and the */
/*   last few are specific to retracts. The stack (and its information!)*/
/*   can ONLY be accessed through these functions.                      */
/* ==================================================================== */


/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern void  memory_error();
extern char *gen_alloc();
extern void  gen_free();


/* ===========  Functions defined here for Global use  ================ */

void reset_stack();
void push();
void pop();
int  inside_if();
int  inside_while();
int  num_nest_levels();
int  describe_previous();
void mark_retraction();


/* ===========  Functions defined here for internal use  ============== */

STK_ELT *new_element();
int      find_elt_in_stack();


/* ===========  Variables defined here for internal use  ============== */

STK_PTR  TOP_OF_STACK = NULL;
int      newest_level = 0;

/* ==================================================================== */
/*                IF, ELSE, and WHILE STACK OPERATORS                   */
/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  reset_stack
 *
 *  PURPOSE:  This function initializes the stack by setting
 *            TOP_OF_STACK to NULL and resetting the global
 *            level-counter to zero. This function should be
 *            called each time a new rule is being processed.
 *
 *  INPUTS:   None.
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

void reset_stack()
{
   while(TOP_OF_STACK NEQ NULL) DO
      pop();
   END_WHILE

   newest_level = 0;
}

/*
 * -------------------------------------------------------------
 *  NAME   :  new_element
 *
 *  PURPOSE:  This function allocates the memory needed by an
 *            individual stack element. It does NOT initialize
 *            any of the new element's fields.
 *
 *  INPUTS:   None.
 *
 *  RETURNS:  a pointer to the new element.
 * -------------------------------------------------------------
 */

static STK_ELT *new_element()
{
   STK_ELT *new_elt;

   if ((new_elt = (STK_ELT *) gen_alloc(sizeof(STK_ELT),"new_element")) ==
      NULL)
      memory_error();

   return(new_elt);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  push
 *
 *  PURPOSE:  This function pushes a new element on the stack
 *            and assigns it a level and a type. The element
 *            then becomes TOP_OF_STACK.
 *
 *  INPUTS:   The element's type (if, else, or while).
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

void push(elt_type)
   int elt_type;
{
   STK_ELT *new_elt;

   new_elt = new_element();

   if (elt_type != _else_)
   {
      new_elt->level      = ++newest_level;
      new_elt->next_level = 0;
   }

   /*======================================================================*/
   /* NOTE: When an else is encountered, its companion-if is TOP_OF_STACK  */
   /*======================================================================*/

   else if (newest_level != 0)
   {
      new_elt->level      = TOP_OF_STACK->level;
      new_elt->next_level = newest_level + 1;
   }

   new_elt->type = elt_type;
   new_elt->next = TOP_OF_STACK;
   TOP_OF_STACK  = new_elt;
}

/*
 * -------------------------------------------------------------
 *  NAME   :  pop
 *
 *  PURPOSE:  This function pops an element off the stack. If
 *            the element (TOP_OF_STACK) is an else, it pops off
 *            the next element also, which is its companion if.
 *            It resets the TOP_OF_STACK accordingly.
 *
 *  INPUTS:   None.
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

void pop()
{
   STK_ELT *old_elt;

   if (TOP_OF_STACK != NULL)
   {
      if (TOP_OF_STACK->type == _else_)
      {
         /*==========================*/
         /* pop off an extra element */
         /*==========================*/

         old_elt = TOP_OF_STACK;
         TOP_OF_STACK = TOP_OF_STACK->next;
         gen_free((char *)old_elt, sizeof(STK_ELT));
      }
      old_elt = TOP_OF_STACK;
      TOP_OF_STACK = TOP_OF_STACK->next;
      gen_free((char *)old_elt, sizeof(STK_ELT));
   }
}

/* ==================================================================== */
/*                      STACK INFORMATION FUNCTIONS                     */
/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  inside_if()
 *
 *  PURPOSE:  This function detects whether the current level is
 *            an if block.
 *
 *  INPUTS:   None.
 *
 *  RETURNS:  YES if it is, NO if it isn't.
 * -------------------------------------------------------------
 */

int inside_if()
{
   if (TOP_OF_STACK == NULL)
      return(NO);

   if (TOP_OF_STACK->type == _if_)
      return(YES);
   else
      return(NO);
}

/*
 * --------------------------------------------------------------
 *  NAME   :  inside_while()
 *
 *  PURPOSE:  This function detects whether the current level is
 *            inside a while block.  NOTE: This is different from
 *            inside_if() in that it checks the whole stack, not
 *            just the current level!
 *
 *  INPUTS:   None.
 *
 *  RETURNS:  YES if it is, NO if it isn't.
 * --------------------------------------------------------------
 */

int inside_while()
{
   int curr_level;
   STK_PTR temp;

   IF(TOP_OF_STACK NEQ NULL) THEN
      curr_level = TOP_OF_STACK->level;
   ELSE
      curr_level = 0;
   END_IF

   temp = TOP_OF_STACK;

   while(temp != NULL)
   {
      if ((temp->type == _while_) && (curr_level >= temp->level))
         return(YES);
      temp = temp->next;
   }
   return(NO);
}

/*
 * --------------------------------------------------------------
 *  NAME   :  num_nest_levels()
 *
 *  PURPOSE:  This function counts how many blocks of type
 *            block_type have been nested..
 *
 *  INPUTS:   The block type (an integer).
 *
 *  RETURNS:  The number of nested levels.
 * --------------------------------------------------------------
 */

int num_nest_levels(block_type)
   int block_type;
{
   STK_PTR temp;
   int level_count = 0;

   for (temp = TOP_OF_STACK; temp != NULL; temp = temp->next)
      if (temp->type == block_type)
         ++level_count;

   return(level_count);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  find_elt_in_stack
 *
 *  PURPOSE:  This function searches the stack to determine
 *            whether it contains an element with the given
 *            level. It returns upon finding the first occurence.
 *
 *  INPUTS:   The level to be located.
 *
 *  RETURNS:  YES if it is there, NO if it isn't.
 * -------------------------------------------------------------
 */

static int find_elt_in_stack(elt_level)
   int elt_level;
{
   STK_PTR temp;

   if (TOP_OF_STACK == NULL)
      return(NO);

   temp = TOP_OF_STACK->next;

   while(temp != NULL)
   {
      if (temp->level == elt_level)
         return(YES);
      temp = temp->next;
   }
   return(NO);
}

/* ==================================================================== */
/*                      RETRACT-SPECIFIC FUNCTIONS                      */
/* ==================================================================== */

/*
 * ---------------------------------------------------------------
 *  NAME   :  describe_previous
 *
 *  PURPOSE:  This function classifies a previously encountered
 *            retraction variable as seen from the current level.
 *
 *  INPUTS:   A previously retracted retraction variable.
 *
 *  RETURNS:  An integer that classifies the retraction variable.
 * ---------------------------------------------------------------
 */

int describe_previous(prev_ret_var)
   VAR_PTR prev_ret_var;
{
   STK_ELT curr;
   int     prev_level = prev_ret_var->level_retracted;
   int     prev_type  = prev_ret_var->block_type;
   int     found;
   int     rtn = OK_RETRACT;

   if (TOP_OF_STACK == NULL)
   {
      curr.level = 0;
      curr.type  = NULL;
      found      = NO;
   }
   else
   {
      curr.level      = TOP_OF_STACK->level;
      curr.type       = TOP_OF_STACK->type;
      if (curr.type == _else_)
         curr.next_level = TOP_OF_STACK->next_level;
      found           = find_elt_in_stack(prev_level);
   }

   /*=====================================================================*/
   /*  If previous level was outside an if, current level doesn't matter  */
   /*=====================================================================*/

   if (prev_level == OUTSIDE_IF)
      rtn = OUTSIDE_IF;

   /*==================================================================*/
   /*  If the previous level was entered before the current level was  */
   /*==================================================================*/

   else if (prev_level < curr.level)
   {
      /*==============================================*/
      /*  If not on stack, must be separate if-block  */
      /*==============================================*/

      if (!found)
         rtn = SEPARATE;

      /*==========================================*/
      /*  Otherwise, must be superlevel if_block  */
      /*==========================================*/

      else
         rtn = SUPERLEVEL;
   }

   /*=================================================================*/
   /*  If the previous level was entered after the current level was  */
   /*=================================================================*/

   else if (prev_level > curr.level)
   {
      /*=======================================================*/
      /*  If the current type wasn't an else, must be sublevel */
      /*=======================================================*/

      if (curr.type != _else_)
         rtn = SUBLEVEL;

      /*===============================================================*/
      /*  Otherwise, if the previous level was within the else block,  */
      /*     it must be a sublevel                                     */
      /*===============================================================*/

      else if (prev_level >= curr.next_level)
         rtn = SUBLEVEL;

      /*=============================================================*/
      /*  Otherwise, it's a sublevel of companion-if and must be OK  */
      /*=============================================================*/

      else
         rtn = OK_RETRACT;
   }

   /*==========================================================*/
   /*  If the previous level is the same as the current level  */
   /*==========================================================*/

   else if (curr.level == prev_level)
   {
      /*=====================================================*/
      /*  If the types are the same, then must be same block */
      /*=====================================================*/

      if (curr.type == prev_type)
         rtn = SAME;

      /*===================================*/
      /*  Otherwise, must be companion-if  */
      /*===================================*/

      else
         rtn = COMPANION_IF;
   }

   return(rtn);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  mark_retraction
 *
 *  PURPOSE:  This function marks a retraction variable as
 *            retracted - sets it up for future encounters.
 *
 *  INPUTS:   A pointer to the retraction variable.
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

void mark_retraction(ret_var)
   VAR_PTR ret_var;
{
   if (TOP_OF_STACK == NULL)
      ret_var->level_retracted = OUTSIDE_IF;
   else
   {
      ret_var->level_retracted = TOP_OF_STACK->level;
      ret_var->block_type      = TOP_OF_STACK->type;
   }
}
