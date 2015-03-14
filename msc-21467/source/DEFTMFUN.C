/*   CLIPS Version 4.30   4/25/89 */

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*             DEFTEMPLATE FUNCTIONS MODULE            */
   /*******************************************************/

#include "setup.h"

#if DEFTEMPLATES && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "deftempl.h"
#include "constant.h"
#include "lhsparse.h"
#include "scanner.h"
#include "clipsmem.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   int                     UpdateModify();
   struct test            *prod_getend();
   struct slot            *ParseSlotLabel();
   struct test            *ParseAssertTemplate();
   struct sap             *ParseAssertSlotValues();
   struct test            *ReorderAssertSlotValues();
   struct test            *GetSlotAssertValues();
   struct test            *GetSingleSlotAssert();
   struct test            *GetMultiSlotAssert();
   int                     returnSAPs();
   struct node            *ReorderLhsSlotValues();
   struct slp             *GetLhsSlots();
   struct slp             *GetSingleLhsSlot();

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct draw     *GetRelationForPattern();
   extern struct funtab   *find_function();
   extern struct test     *prod_gtvar();
   extern struct test     *prod_gv_args();
   extern struct slp      *find_slot_item();
   extern struct slot     *FindSlot();
   extern struct draw     *add_symbol();
   extern struct test     *GetAssertArgument();
   extern struct dtmpl    *FindDeftemplate();
   extern struct node     *get_node();
   extern struct node     *restriction_parse();

/*####################################################*/
/*#                                                  #*/
/*# DEFRELATION MODIFICATIONS FOR THE ASSERT COMMAND #*/
/*#                                                  #*/
/*####################################################*/

/******************************************************************/
/* ParseAssertTemplate: Parses and builds the list of values that */
/*   are used for an assert of a fact with a deftemplate.         */
/******************************************************************/
struct test *ParseAssertTemplate(read_source,temp_tkn,multi,error,dname)
  char *read_source;
  struct token *temp_tkn;
  int *multi, *error;
  struct draw *dname;
  {
   struct dtmpl *dtmpl_ptr;
   struct sap *first_slot, *last_slot, *next_slot;
   struct test *first_arg;
   struct slot *slotPtr;
   
   first_slot = NULL;
   last_slot = NULL;
   
   /*===============================================*/
   /* Get a pointer to the deftemplate information. */
   /*===============================================*/
   
   dtmpl_ptr = FindDeftemplate(dname->contents);
   if (dtmpl_ptr == NULL)
     { clips_system_error(9931); }
     
   /*==============================================*/
   /* Parse each of the slot fields in the assert. */
   /*==============================================*/
  
   while ((slotPtr = ParseSlotLabel(read_source,temp_tkn,dtmpl_ptr,error)) != NULL)
     {
      next_slot = ParseAssertSlotValues(read_source,temp_tkn,
                                        slotPtr,multi,error);
      
      if (*error)
        { 
         returnSAPs(first_slot); 
         return(NULL);
        }

      if (last_slot == NULL)
        { first_slot = next_slot; }
      else
        { last_slot->next = next_slot; }
      last_slot = next_slot;
     }
     
   if (*error)
     {
      returnSAPs(first_slot); 
      return(NULL);
     }
   
   /*=================================================*/
   /* Check to see if any slot names were duplicated. */
   /*=================================================*/
    
   if (MultiplyDefinedAssertSlots(first_slot))
     {
      *error = TRUE;
      returnSAPs(first_slot);
      return(NULL);
     }
     
   /*=============================================================*/
   /* Reorder the arguments to the order used by the deftemplate. */
   /*=============================================================*/
 
   first_arg = ReorderAssertSlotValues(dtmpl_ptr->slot_list,first_slot);
   returnSAPs(first_slot);
   
   /*==============================*/
   /* Return the assert arguments. */
   /*==============================*/
   
   return(first_arg);			 
  }
  
/**************************************************************************/
/* ParseAssertSlotValues: Gets a single assert slot value for a template. */
/**************************************************************************/
struct sap *ParseAssertSlotValues(inputSource,tempToken,
                                                slotPtr,multi,error)
  char *inputSource;
  struct token *tempToken;
  struct slot *slotPtr;
  int *multi, *error;
  {
   struct sap *next_slot;
   struct test *new_test, *multi_test, *last_test;
   
   /*=============================*/
   /* Handle a single field slot. */
   /*=============================*/
   
   if (slotPtr->multi_slot == FALSE)
     {  
      save_pp_buffer(" ");
      *multi = FALSE;
      new_test = GetAssertArgument(inputSource,tempToken,multi,error); 
      if (*error)
        { return(NULL); }
        
      if (new_test == NULL)
       {
        *error = TRUE;
        cl_print("werror","\nSingle field slot ");
        cl_print("werror",slotPtr->slot_name->contents);
        cl_print("werror"," must contain a single field value\n");
        return(NULL);
       }
       
      if (*multi)
       {
        *error = TRUE;
        cl_print("werror","\nA multi-field value cannot be used ");
        cl_print("werror","in the single field slot ");
        cl_print("werror",slotPtr->slot_name->contents);
        cl_print("werror","\n");
        returntests(new_test);
        return(NULL);
       }
       
      gettoken(inputSource,tempToken);
     }
      
   /*============================*/
   /* Handle a multi field slot. */
   /*============================*/
   
   else
     {
      multi_test = GetAssertArgument(inputSource,tempToken,multi,error);
      if (*error) return(NULL);
      last_test = multi_test;
      
      while (tempToken->token != RPAREN)
        {
         pp_backup();  
         save_pp_buffer(" ");
         save_pp_buffer(tempToken->print_rep);
            
         new_test = GetAssertArgument(inputSource,tempToken,multi,error);
         if (*error) 
           { 
            returntests(multi_test);
            return(NULL); 
           }
             
         last_test->next_arg = new_test;
         last_test = new_test;
        }
        
      new_test = multi_test;
     }
   
   /*==========================================================*/
   /* Slot definition must be closed with a right parenthesis. */
   /*==========================================================*/
   
   if (tempToken->token != RPAREN)   
     {
      cl_print("werror","\nSingle field slot ");
      cl_print("werror",slotPtr->slot_name->contents);
      cl_print("werror"," must contain a single field value\n");
      *error = TRUE;
      returntests(new_test);
      return(NULL);
     }        
     
   /*=========================================================*/
   /* Build and return a structure describing the slot value. */
   /*=========================================================*/
   
   next_slot = get_struct(sap);
   next_slot->name = slotPtr->slot_name;
   next_slot->expr = new_test;
   next_slot->next = NULL;
   
   return(next_slot);
  }
  
/*************************************************************************/
/* ReorderAssertSlotValues: Rearranges the asserted values to correspond */
/*   to the order of the values described by the deftemplate.            */
/*************************************************************************/
struct test *ReorderAssertSlotValues(slotPtr,first_slot)
  struct slot *slotPtr;
  struct sap *first_slot;
  {
   struct test *first_arg = NULL;
   struct test *last_arg = NULL, *new_arg;
   
   while (slotPtr != NULL)
     {
      new_arg = GetSlotAssertValues(slotPtr,first_slot);
        
      if (new_arg != NULL)
        {
         if (last_arg == NULL)
           { first_arg = new_arg; }
         else
           { last_arg->next_arg = new_arg; }
           
         while (new_arg->next_arg != NULL)
           { new_arg = new_arg->next_arg; }
         last_arg = new_arg;
        }
        
      slotPtr = slotPtr->next;
     }
     
   return(first_arg);
  }
 
/***************************************************************/  
/* GetSlotAssertValues: Gets the assert value for a given slot */
/*   of a deftemplate. If the value was supplied by the user,  */
/*   it will be used. If not the default value or default      */
/*   default value will be used.                               */
/***************************************************************/  
struct test *GetSlotAssertValues(slotPtr,first_slot)
  struct slot *slotPtr;
  struct sap *first_slot;
  {
   struct sap *slotItem;
   struct test *new_arg;
   
   /*==================================================*/
   /* Determine if the slot is assigned in the assert. */
   /*==================================================*/
   
   slotItem = (struct sap *) find_slot_item(slotPtr,first_slot);
   
   /*==========================================*/
   /* If the slot is assigned, use that value. */
   /*==========================================*/
   
   if (slotItem != NULL)
     {
      new_arg = slotItem->expr;
      slotItem->expr = NULL; 
     }
     
   /*=================================*/
   /* Otherwise, use a default value. */
   /*=================================*/
   
   else
     {
      if (slotPtr->multi_slot == FALSE)
        { new_arg = GetSingleSlotAssert(slotPtr->item_list); }
      else
        { new_arg = GetMultiSlotAssert(slotPtr->item_list); }
     }
     
   /*==============================================*/
   /* Return the value to be asserted in the slot. */
   /*==============================================*/
   
   return(new_arg);
  }
  
/**************************************************************************/
/* GetSingleSlotAssert: Supplies a default value for a single value slot. */
/**************************************************************************/
struct test *GetSingleSlotAssert(default_list)
  struct slot_item *default_list;
  {
   struct test *new_arg;
   
   /*=============================================*/
   /* Get a structure to store the default value. */
   /*=============================================*/
   
   new_arg = get_struct(test);
   new_arg->next_arg = NULL;
   new_arg->arg_list = NULL;
   
   /*========================================*/
   /* If no default value is supplied by the */
   /* deftemplate, use the value nil.        */
   /*========================================*/
   
   if (default_list == NULL)
     { 
      new_arg->type = WORD;
      new_arg->val.hvalue = add_symbol("nil");
     }
     
   /*============================================*/
   /* Otherwise, use the default value supplied. */
   /*============================================*/
   
   else
     {
      new_arg->type = default_list->type;
      if (new_arg->type == NUMBER)
        { new_arg->val.fvalue = default_list->value.number; }
      else
        { new_arg->val.hvalue = default_list->value.string; }
     }
     
   /*===========================*/
   /* Return the default value. */
   /*===========================*/
   
   return(new_arg);
  }
        
/************************************************************************/
/* GetMultiSlotAssert: Supplies a default value for a multi value slot. */ 
/************************************************************************/
struct test *GetMultiSlotAssert(default_list)
  struct slot_item *default_list;
  {
   struct test *first_arg = NULL;
   struct test *last_arg = NULL, *new_arg;
   
   /*============================================================*/
   /* Copy the values in the default list for the default value. */
   /* Note that if no default was specified, then the default    */
   /* list will be null (which is the desired default default).  */
   /*============================================================*/
   
   while (default_list != NULL)
     {
      new_arg = get_struct(test);
      new_arg->next_arg = NULL;
      new_arg->arg_list = NULL;
      new_arg->type = default_list->type;
      if (new_arg->type == NUMBER)
        { new_arg->val.fvalue = default_list->value.number; }
      else
        { new_arg->val.hvalue = default_list->value.string; }
                 
      if (last_arg == NULL)
        { first_arg = new_arg; }
      else
        { last_arg->next_arg = new_arg; }
                 
      last_arg = new_arg;
      default_list = default_list->next;
     }
     
   /*===========================*/
   /* Return the default value. */
   /*===========================*/
   
   return(first_arg);
  }
  
/********************************************/
/* returnSAPs: Returns data structures used */ 
/*   for parsing deftemplate assert.        */
/********************************************/
returnSAPs(saps)
  struct sap *saps;
  {
   struct sap *tempSaps;

   while (saps != NULL)
     {
      tempSaps = saps->next;
      if (saps->expr != NULL) returntests(saps->expr);
      rtn_struct(sap,saps);
      saps = tempSaps;
     }
  }
  
/************************************************************/
/* MultiplyDefinedAssertSlots: Determines if any slots were */
/*   defined more that once for an assert statement.        */
/************************************************************/
MultiplyDefinedAssertSlots(first_slot)
  struct sap *first_slot;
  {
   struct sap *next_slot;
   int rv = FALSE;
   
   while (first_slot != NULL)
     {
      next_slot = first_slot->next;
      while (next_slot != NULL)
        {
         if (next_slot->name == first_slot->name)
           {
            cl_print("werror","\nMultiple occurences of slot ");
            cl_print("werror",first_slot->name->contents);
            cl_print("werror","\n");
            rv = TRUE;
           }
         next_slot = next_slot->next;
        }
      first_slot = first_slot->next;
     }
  
   return(rv);
  }
  
/*#############################################*/
/*#                                           #*/
/*# DEFRELATION MODIFICATIONS FOR LHS PARSING #*/
/*#                                           #*/
/*#############################################*/


/**********************************************/
/* dtmpl_lhs_parse: Parses a LHS pattern that */
/*   uses the deftemplate format.             */
/**********************************************/
struct node *dtmpl_lhs_parse(read_source,hname)
  char *read_source;
  struct draw *hname;
  {
   struct node *head;
   struct slp *first_slot;
   struct token temp_tkn;
   struct dtmpl *dtmpl_ptr;
   int error;
   
   head = NULL;
   
   /*===============================================*/
   /* Get a pointer to the deftemplate information. */
   /*===============================================*/
   
   dtmpl_ptr = FindDeftemplate(hname->contents);  
   if (dtmpl_ptr == NULL)
     { clips_system_error(9901); }

   /*===============================================================*/
   /* Make sure the deftemplate name is not connected to subfields. */
   /*===============================================================*/
   
   gettoken(read_source,&temp_tkn);
   if ((temp_tkn.token == LOR) || (temp_tkn.token == LAND))
     {
      cl_print("werror","\nDeftemplate name used with field connector.\n");
      return(NULL);
     }
   
   /*===============================================*/
   /* Create pattern node for the deftemplate name. */
   /*===============================================*/
       
   head = get_node();
   head->type = SINGLE;
   head->state = 'o';
   head->bottom = get_node();
   head->bottom->type = WORD;
   head->bottom->state = 'o';
   head->bottom->svalue = hname;

   /*======================================*/
   /* Get other fields in the deftemplate. */
   /*======================================*/
   
   error = FALSE;
   first_slot = GetLhsSlots(read_source,&temp_tkn,dtmpl_ptr,&error);
   if (error)
     {
      returnnodes(head);
      return(NULL);
     }

   /*==============================================================*/
   /* Determine if any of the slot names were used more than once. */
   /*==============================================================*/
   
   if (MultiplyDefinedLhsSlots(first_slot))
     {
      returnnodes(head);
      returnSLPs(first_slot);
      return(NULL);
     }
     
   /*===========================*/
   /* Reorder slot definitions. */
   /*===========================*/
     
   head = ReorderLhsSlotValues(dtmpl_ptr->slot_list,first_slot,head);
   returnSLPs(first_slot);
   
   /*=========================*/
   /* Return the LHS pattern. */
   /*=========================*/
   
   return(head);			   
  }

/******************************************/
/* GetLhsSlots: Retrieves all of the slot */
/*   values used in a LHS pattern.        */
/******************************************/
struct slp *GetLhsSlots(read_source,tempToken,dtmpl_ptr,error)
  char *read_source;
  struct token *tempToken;
  struct dtmpl *dtmpl_ptr;
  int *error;
  {
   struct slp *first_slot = NULL, *next_slot, *last_slot = NULL;
   struct slot *slotPtr;
   
   /*=======================================================*/
   /* Continue parsing slot definitions until the pattern's */
   /* closing right parenthesis is encountered.             */
   /*=======================================================*/
   
   while (tempToken->token != RPAREN)
     {
      pp_backup();
      save_pp_buffer(" ");
      save_pp_buffer(tempToken->print_rep);
      
      /*=================================================*/
      /* Slot definitions begin with a left parenthesis. */
      /*=================================================*/
      
      if (tempToken->token != LPAREN)
        {
         *error = TRUE;
         cl_print("werror","\nExpected a '(' to begin a slot definition\n");
         cl_print("werror","or a ')' to end slot definitions\n");
         return(NULL);
        }
        
      /*====================*/
      /* Get the slot name. */
      /*====================*/
      
      gettoken(read_source,tempToken);
      if (tempToken->token != WORD)
        {
         *error = TRUE;
         cl_print("werror","\nExpected slot name to be a word\n");
         return(NULL);
        }
        
      /*==========================================================*/
      /* Determine if the slot name is valid for the deftemplate. */
      /*==========================================================*/
      
      if ((slotPtr = FindSlot(dtmpl_ptr,tempToken->hashword)) == NULL)
        {
         *error = TRUE;
         cl_print("werror","\nInvalid slot ");
         cl_print("werror",tempToken->tknword);
         cl_print("werror"," not defined in corresponding deftemplate\n");
         returnSLPs(first_slot);
         return(NULL);
        }

      /*==============================================================*/
      /* Get the pattern matching values used in the slot definition. */
      /*==============================================================*/
      
      next_slot = GetSingleLhsSlot(read_source,tempToken,slotPtr,error);
      if (*error)
        {
         returnSLPs(first_slot);
         returnSLPs(next_slot);
         return(NULL);
        }
        
      /*=====================================*/
      /* Add the slot definition to the list */ 
      /* of slot definitions already parsed. */
      /*=====================================*/
      
      if (last_slot == NULL)
        { first_slot = next_slot; }
      else
        { last_slot->next = next_slot; }
      last_slot = next_slot;
         
      /*==============================*/
      /* Begin parsing the next slot. */
      /*==============================*/
      
      gettoken(read_source,tempToken);
     }
   
   /*===========================================================*/
   /* Return all the slot definitions found in the lhs pattern. */
   /*===========================================================*/
     
   return(first_slot);
  }
  
/*****************************************************/
/* GetSingleLhsSlot: Get the pattern matching values */
/*   to be associated with a slot name.              */
/*****************************************************/
struct slp *GetSingleLhsSlot(read_source,tempToken,slotPtr,error)
  char *read_source;
  struct token *tempToken;
  struct slot *slotPtr;
  int *error;
  {
   struct slp *next_slot;
   struct node *multi_node, *new_node, *last_node;
   
   next_slot = get_struct(slp);
   next_slot->name = tempToken->hashword;
   next_slot->expr = NULL;
   next_slot->next = NULL;

   save_pp_buffer(" ");
   gettoken(read_source,tempToken);

   /*====================================*/
   /* Get value for a single field slot. */
   /*====================================*/
   
   if (slotPtr->multi_slot == FALSE)
     {  
      /*=======================*/
      /* Get the single value. */
      /*=======================*/
      
      new_node = restriction_parse(read_source,tempToken); 
      if (new_node == NULL)
        {
         *error = TRUE;
         returnSLPs(next_slot);
         return(NULL);
        }
      
      /*======================================*/
      /* Multi field wildcards and variables  */
      /* not allowed in a single field slot.  */
      /*======================================*/
      
      if ((new_node->type == BWORDS) || 
          (new_node->type == MULTIPLE))
        {
         cl_print("werror","\nA multi-field value cannot be used ");
         cl_print("werror","in the single field slot ");
         cl_print("werror",slotPtr->slot_name->contents);
         cl_print("werror","\n");
         *error = TRUE;
         returnnodes(new_node);
         returnSLPs(next_slot);
         return(NULL);
        }
     }
     
   /*====================================*/
   /* Get values for a multi field slot. */
   /*====================================*/
   
   else
     {
      multi_node = NULL;
      last_node = NULL;
      
      /*===============================================*/
      /* Continue parsing the multi field values until */
      /* a right parenthesis is encountered.           */
      /*===============================================*/
      
      while (tempToken->token != RPAREN)
        {
         /*==============*/
         /* Get a value. */
         /*==============*/
         
         new_node = restriction_parse(read_source,tempToken); 
         if (new_node == NULL)
           {
            *error = TRUE;
            returnSLPs(next_slot);
            return(NULL);
           }
           
         /*====================================*/
         /* Fix pretty print representation to */
         /* include spaces between values.     */
         /*====================================*/
                       
         if (tempToken->token != RPAREN)
           {
            pp_backup();
            save_pp_buffer(" ");
            save_pp_buffer(tempToken->print_rep);
           }
           
         /*=====================================*/
         /* Add the value to the list of values */
         /* for this multi field slot.          */
         /*=====================================*/
         
         if (last_node == NULL)
           { multi_node = new_node; }
         else
           { last_node->right = new_node; }
         last_node = new_node;
        }
      new_node = multi_node;
     }
   
   /*========================================================*/
   /* The slot definition must end with a right parenthesis. */
   /*========================================================*/
   
   if (tempToken->token != RPAREN)   
     {
      cl_print("werror","\nExpected ')' to close slot definition.\n");
      *error = TRUE;
      returnSLPs(next_slot);
      return(NULL);
     }        

   /*=================================*/
   /* Add the slot values to the slot */
   /* structure and return it.        */
   /*=================================*/
   
   next_slot->expr = new_node;
   return(next_slot);
  }
  
/****************************************************/ 
/* ReorderLhsSlotValues: Reorders LHS pattern slots */
/*   to match the order used by the deftemplate.    */
/****************************************************/ 
struct node *ReorderLhsSlotValues(slotPtr,first_slot,head)
  struct slot *slotPtr;
  struct slp *first_slot;
  struct node *head;
  {
   struct node *last_node, *new_node;
   struct slp *slotItem;
   
   last_node = head;
   while (slotPtr != NULL)
     {
      /*==========================================================*/
      /* If the slot wasn't defined in the pattern, use a single  */
      /* field wildcard for a single field slot and a multi field */
      /* wildcard for a multi field slot.                         */
      /*==========================================================*/
      
      if ((slotItem = find_slot_item(slotPtr,first_slot)) == NULL)
        {
         new_node = get_node();
         new_node->state = 'o';
         if (slotPtr->multi_slot == FALSE)
           { new_node->type = SINGLE; }
         else
           { new_node->type = MULTIPLE; }
        }
      else
       
      /*==========================================*/
      /* Otherwise, use the field associated with */
      /* the slot in the LHS pattern.             */ 
      /*==========================================*/
                    
        { 
         new_node = slotItem->expr;
         slotItem->expr = NULL; 
        }
        
      /*=============================================*/
      /* Add the field to the new reordered pattern. */
      /*=============================================*/
      
      if (new_node != NULL)
        {
         last_node->right = new_node;
         last_node = new_node;
        }
        
      slotPtr = slotPtr->next;
     }
    
   /*======================================================*/
   /* Return the reordered pattern with unspecified fields */
   /* replaced with match anything (? or $?) values.       */ 
   /*======================================================*/
   
   return(head);
  }

/******************************************************/
/* MultiplyDefinedLhsSlots: Determines if a slot name */
/*   was used more than once in a LHS pattern.        */
/******************************************************/
MultiplyDefinedLhsSlots(first_slot)
  struct slp *first_slot;
  {
   struct slp *next_slot;
   int rv = FALSE;
   
   while (first_slot != NULL)
     {
      next_slot = first_slot->next;
      while (next_slot != NULL)
        {
         if (next_slot->name == first_slot->name)
           {
            cl_print("werror","\nMultiple occurences of slot ");
            cl_print("werror",first_slot->name->contents);
            cl_print("werror","\n");
            rv = TRUE;
           }
         next_slot = next_slot->next;
        }
      first_slot = first_slot->next;
     }
  
   return(rv);
  }
   
/***************************************************************/
/* find_slot_item: Finds a particular slot in a list of slots. */
/***************************************************************/
struct slp *find_slot_item(slot_ptr,slp_ptr)
  struct slot *slot_ptr;
  struct slp *slp_ptr;
  {
   while (slp_ptr != NULL)
     {
      if (slp_ptr->name == slot_ptr->slot_name) return (slp_ptr);
      slp_ptr = slp_ptr->next;
     }
  
   return(NULL);
  }

/********************************************/
/* returnSLPs: Returns data structures used */ 
/*   for parsing deftemplates on the LHS.   */
/********************************************/
returnSLPs(slps)
  struct slp *slps;
  {
   struct slp *tempSlps;

   while (slps != NULL)
     {
      tempSlps = slps->next;
      if (slps->expr != NULL) returnnodes(slps->expr);
      rtn_struct(slp,slps);
      slps = tempSlps;
     }
  }

/*################################################*/
/*#                                              #*/
/*# DEFRELATION ADDITIONS FOR THE MODIFY COMMAND #*/
/*#                                              #*/
/*################################################*/

/****************************************************************/
/* UpdateModify: Changes the modify command found on the RHS of */
/*   a rule into the appropriate retract and assert commands.   */
/****************************************************************/
UpdateModify(top)
  struct test *top;
  {
   struct test *modify_args, *test_ptr;
   struct test *next_one, *splice_arg, *temp_ptr;
   int fap, found;
   struct draw *dtmpl_name;
   struct dtmpl *dtmpl_ptr;
   struct slot *slotPtr;
   int position, searchID;
   struct fact *fPtr;
      
   /*===================================*/
   /* Change the modify into a retract. */
   /*===================================*/
   
   top->val.fun_ptr = find_function("retract");
   modify_args = top->arg_list;
   top->arg_list = NULL;
   splice_arg = top->next_arg;
   
   /*========================================*/
   /* Determine the fact address or index to */ 
   /* be retracted by the modify command.    */
   /*========================================*/
   
   if (modify_args->type == POINTER)
     {
      fap = get_fa_pointer(modify_args->val.hvalue);
      dtmpl_name = GetRelationForPattern(fap);
     }
   else if (modify_args->type == INDEX)
     {
      searchID = modify_args->val.index;
      fap = searchID;
      fPtr = get_next_fact(NULL);
      while ((fPtr == NULL) ? FALSE : (fPtr->ID < searchID))
        { fPtr = get_next_fact(fPtr); }
        
      if ((fPtr == NULL) ? TRUE : (fPtr->ID != searchID))
        {
         cl_print("werror","Unable to find fact\n");
         returntests(modify_args);
         return(FALSE);
        }
        
      if (fPtr->atoms[0].type == WORD)
        { dtmpl_name = fPtr->atoms[0].val.hvalue; }
      else
        { dtmpl_name = NULL; }
     }
   else
     { clips_system_error(9923); }
     
   /*========================================*/
   /* Make sure that the fact being modified */
   /* has a corresponding deftemplate.       */
   /*========================================*/
   
   if ((dtmpl_ptr = FindDeftemplate(dtmpl_name->contents)) == NULL)
     {
      cl_print("werror","\nOnly facts defined with deftemplate can be modified\n");
      returntests(modify_args);
      return(FALSE);
     }
           
   /*=============================================================*/
   /* Make sure all the slot names are valid for the deftemplate. */
   /*=============================================================*/
   
   temp_ptr = modify_args->next_arg->next_arg;
   while (temp_ptr != NULL)
     { 
      if ((slotPtr = FindSlot(dtmpl_ptr,temp_ptr->val.hvalue)) == NULL)
        {
         cl_print("werror","\nInvalid slot ");
         cl_print("werror",temp_ptr->val.hvalue->contents);
         cl_print("werror"," not defined in corresponding deftemplate\n");
         returntests(modify_args);
         return(NULL);
        }
      temp_ptr = temp_ptr->next_arg;
     }
   
   /*======================================*/
   /* Add the fact address to the argument */
   /* list of the retract command.         */
   /*======================================*/
   
   top->arg_list = modify_args;
   modify_args = modify_args->next_arg;
   top->arg_list->next_arg = NULL;
   top->arg_list->arg_list = NULL;
   
   /*=====================================================*/
   /* Add the assert stub splicing it between the retract */
   /* command and the command that follows the modify.    */
   /*=====================================================*/
   
   top->next_arg = get_struct(test);
   next_one = top->next_arg;
   next_one->type = FCALL;
   next_one->val.fun_ptr = modify_args->val.fun_ptr;
   next_one->next_arg = splice_arg;
   
   slotPtr = dtmpl_ptr->slot_list;
   while (slotPtr != NULL)
     {
      if (slotPtr->multi_slot)
        { next_one->val.fun_ptr = find_function("slow_assert"); }
      slotPtr = slotPtr->next;
     }
   
   /*========================================================*/
   /* Add the deftemplate name as the first assert argument. */
   /*========================================================*/
   
   next_one->arg_list = get_struct(test);
   next_one = next_one->arg_list;
   next_one->type = WORD;
   next_one->val.hvalue = dtmpl_name;
   next_one->next_arg = NULL;
   next_one->arg_list = NULL;
     
   /*=====================================*/
   /* Add the remaining assert arguments. */
   /*=====================================*/
    
   slotPtr = dtmpl_ptr->slot_list;
   position = 2;
   while (slotPtr != NULL)
     {
      /*=======================================*/
      /* Look for the slot name in the list of */
      /* slot definitions used in the modify.  */
      /*=======================================*/
      
      found = FALSE;
      test_ptr = modify_args->next_arg;
      while ((found == FALSE) && (test_ptr != NULL))
        {
         if (test_ptr->val.hvalue == slotPtr->slot_name)
           { found = TRUE; }
         else
           { test_ptr = test_ptr->next_arg; }
        }
      
      /*=====================================================*/
      /* If a value was specified for a slot in the modify   */
      /* command, then use that value in the assert command. */
      /*=====================================================*/
      
      if (found)
        { 
         /*===============================================*/
         /* Check for several different errors related to */
         /* assigning a value to a single field slot.     */
         /*===============================================*/
         
         if (slotPtr->multi_slot == FALSE)
           {
            if (test_ptr->arg_list == NULL)
              { 
               cl_print("werror","\nSingle field slot ");
               cl_print("werror",slotPtr->slot_name->contents);
               cl_print("werror"," must contain a single field value\n");
               returntests(modify_args);
               return(FALSE);
              }
            else if (test_ptr->arg_list->next_arg != NULL)
              {
               cl_print("werror","\nSingle field slot ");
               cl_print("werror",slotPtr->slot_name->contents);
               cl_print("werror"," must contain a single field value\n");
               returntests(modify_args);
               return(FALSE);
              }
            else if ((test_ptr->arg_list->type == BWORDS) ||
                     ((test_ptr->arg_list->type == FCALL) ? 
                      (test_ptr->arg_list->val.fun_ptr->fun_type == 'm') :
                      FALSE))
              {
               cl_print("werror","\nA multi-field value cannot be used ");
               cl_print("werror","in the single field slot ");
               cl_print("werror",slotPtr->slot_name->contents);
               cl_print("werror","\n");
               returntests(modify_args);
               return(FALSE);
              }
           }
         
         /*==========================================================*/
         /* Append the value supplied in the modify command to the   */
         /* end of the arguments being formed in the assert command. */
         /* If a multi field value is supplied, this value will be   */
         /* the last added to the assert since the multi field value */
         /* is place at the end of the list of slots in the          */
         /* deftemplate structure.                                   */
         /*==========================================================*/
         
         next_one->next_arg = test_ptr->arg_list;
         next_one = next_one->next_arg;
         test_ptr->arg_list = NULL; 
        }
        
      /*===================================================*/
      /* Otherwise if no value was specified for a slot in */
      /* the modify command, then retrieve the value to be */ 
      /* used directly from the fact.                      */
      /*===================================================*/
      
      else
        {
         if (slotPtr->multi_slot == FALSE)
           {
            next_one->next_arg = prod_gtvar(fap,position);
            next_one = next_one->next_arg;
            if (TopLevelCommand())
              { next_one->val.fun_ptr = find_function("(fact_get_var)"); }
           }
         else
           { 
            next_one->next_arg = prod_getend(fap,position);
            next_one = next_one->next_arg; 
            if (TopLevelCommand())
              { next_one->val.fun_ptr = find_function("(fact_get_end)"); }
           }
        }
        
      position++;
      slotPtr = slotPtr->next;
     }
     
   /*========================================================*/
   /* If the modify was issued at the top level prompt, then */
   /* enclose the modify within a progn so that both the     */
   /* retract and assert command will be executed.           */
   /*========================================================*/
   
   if (TopLevelCommand())
     {
      next_one = get_struct(test);
      next_one->type = top->type;
      next_one->val.fun_ptr = top->val.fun_ptr;
      next_one->next_arg = top->next_arg;
      next_one->arg_list = top->arg_list;
      
      top->val.fun_ptr = find_function("progn");
      top->next_arg = NULL;
      top->arg_list = next_one;
     }
   
   /*===========================================*/
   /* Return the arguments used with the modify */
   /* since they are no longer needed.          */
   /*===========================================*/
   
   returntests(modify_args);
   
   /*=======================================================*/
   /* Return TRUE indicating that the modify command was    */
   /* successfully updated to a retract and assert command. */
   /*=======================================================*/
   
   return(TRUE);
  }
  
/*****************************************************/
/* prod_getend: Produces an expression of the format */   
/*   (getend <pattern> <field>)                      */
/*****************************************************/
struct test *prod_getend(pattern,element)
  int pattern, element;
  {
   struct test *top;

   top = get_struct(test);
   top->type = FCALL;
   top->val.fun_ptr = find_function("(get_end)");
   top->arg_list = prod_gv_args(pattern,element);
   top->next_arg = NULL;
   
   return(top);
  }
  
/*******************************************/
/* ModifyParse: Parses the modify command. */
/*******************************************/
struct test *ModifyParse(top,in_file)
  struct test *top;
  char *in_file;
  {
   int multi, error = FALSE;
   struct token act_tkn;
   struct test *next_one, *assert_arg;
   struct test *new_test, *multi_test, *last_test;
   
   /*========================================================*/
   /* Parse the fact address or index to the modify command. */
   /*========================================================*/
   
   save_pp_buffer(" ");
   gettoken(in_file,&act_tkn);
   
   if (act_tkn.token == BWORD)
     {
      next_one = get_struct(test);
      next_one->type = POINTER;  
      next_one->val.hvalue = act_tkn.hashword;
     }
   else if (act_tkn.token == NUMBER)
     {
      if (! TopLevelCommand())
        {
         cl_print("werror","\nFact indexes can only be used by modify ");
         cl_print("werror","as a top level command\n");
         returntests(top);
         return(NULL);
        }
      next_one = get_struct(test);
      next_one->type = INDEX;  
      next_one->val.index = act_tkn.tknnumber;
     }
   else
     {
      exp_type_error("modify",1,"fact address or fact index");
      returntests(top); 
      return(NULL);
     }

   next_one->next_arg = NULL;
   next_one->arg_list = NULL;
   top->arg_list = next_one;
   next_one = top->arg_list;
   
   /*=================================*/
   /* Add the assert stub for modify. */
   /*=================================*/
   
   next_one->next_arg = get_struct(test);
   assert_arg = next_one->next_arg;
   assert_arg->type = FCALL;
   assert_arg->val.fun_ptr = find_function("assert");
   assert_arg->next_arg = NULL;
   assert_arg->arg_list = NULL;
   next_one = assert_arg;
   
   /*====================================*/
   /* Parse the remaining modify fields. */
   /*====================================*/
   
   gettoken(in_file,&act_tkn);
   while (act_tkn.token != RPAREN)
     {
      pp_backup();
      save_pp_buffer(" ");
      save_pp_buffer(act_tkn.print_rep);
      
      /*=================================================*/
      /* Slot definition begins with a left parenthesis. */
      /*=================================================*/
      
      if (act_tkn.token != LPAREN)
        {
         cl_print("werror","\nExpected a '(' to begin a slot definition\n");
         cl_print("werror","or a ')' to end slot definitions\n");
         returntests(top);
         return(NULL);
        }
        
      /*===============================*/
      /* The slot name must be a word. */
      /*===============================*/
      
      gettoken(in_file,&act_tkn);
      if (act_tkn.token != WORD)
        {
         cl_print("werror","\nExpected slot name to be a word\n");
         returntests(top);
         return(NULL);
        }
      
      next_one->next_arg = get_struct(test);
      next_one = next_one->next_arg;
      next_one->type = WORD;
      next_one->val.hvalue = act_tkn.hashword;
      next_one->arg_list = NULL;
      next_one->next_arg = NULL;

      save_pp_buffer(" ");

      /*====================================================*/
      /* Get the values to be stored in the specified slot. */
      /*====================================================*/
      
      multi_test = NULL;
      last_test = NULL;
      while (act_tkn.token != RPAREN)
        {
         multi = FALSE;
         new_test = GetAssertArgument(in_file,&act_tkn,&multi,&error); 
          
         if (error)
           {
            returntests(top);
            return(NULL);
           }
           
         if (multi)
           { assert_arg->val.fun_ptr = find_function("slow_assert"); }
          
         if (last_test == NULL)
           { multi_test = new_test; }
         else
           { last_test->next_arg = new_test; }
         last_test = new_test;
        }
   
      /*==================================================*/
      /* Slot definition begins with a right parenthesis. */
      /*==================================================*/
      
      if (act_tkn.token != RPAREN)   
        {
         cl_print("werror","\nExpected ')' to close slot definition\n");
         returntests(top);
         returntests(multi_test);
         return(NULL);
        }        

      next_one->arg_list = multi_test;
         
      gettoken(in_file,&act_tkn);
     }
   
   /*========================================================*/
   /* Check to see that no slot name is used more than once. */
   /*========================================================*/
    
   if (MultiplyDefinedModifySlots(top->arg_list->next_arg->next_arg))
     {
      returntests(top);
      return(NULL);
     }
  
   if (TopLevelCommand())
     { 
      if (! UpdateModify(top)) 
        {
         returntests(top);
         return(NULL);
        }
     }
     
   return(top);			 
  }
  
/*********************************************************/
/* MultiplyDefinedModifySlots: Determines if a slot name */  
/*   has been used more than once in a modify command.   */
/*********************************************************/
MultiplyDefinedModifySlots(first_slot)
  struct test *first_slot;
  {
   struct test *next_slot;
   int rv = FALSE;
   
   while (first_slot != NULL)
     {
      next_slot = first_slot->next_arg;
      while (next_slot != NULL)
        {
         if (next_slot->val.hvalue == first_slot->val.hvalue)
           {
            cl_print("werror","\nMultiple occurences of slot ");
            cl_print("werror",first_slot->val.hvalue->contents);
            cl_print("werror","\n");
            rv = TRUE;
           }
         next_slot = next_slot->next_arg;
        }
      first_slot = first_slot->next_arg;
     }
  
   return(rv);
  }
  
/****************************************************************/
/* ParseSlotLabel: Parses the beginning of a slot definition.   */
/*   Checks for opening left parenthesis and a valid slot name. */
/****************************************************************/
struct slot *ParseSlotLabel(inputSource,tempToken,dtmplPtr,error)
  char *inputSource;
  struct token *tempToken;
  struct dtmpl *dtmplPtr;
  int *error;
  {
   struct slot *slotPtr;
   
   /*========================*/
   /* Initialize error flag. */
   /*========================*/
   
   *error = FALSE;
   
   /*============================================*/
   /* If token is a right parenthesis, then fact */
   /* template definition is complete.           */
   /*============================================*/
   
   gettoken(inputSource,tempToken);
   if (tempToken->token == RPAREN)
     { return(NULL); }
   
   /*=======================================*/
   /* Put a space between the template name */ 
   /* and the first slot definition.        */
   /*=======================================*/
   
   pp_backup();
   save_pp_buffer(" ");
   save_pp_buffer(tempToken->print_rep);
      
   /*=======================================================*/
   /* Slot definition begins with opening left parenthesis. */
   /*=======================================================*/
   
   if (tempToken->token != LPAREN)
     {
      cl_print("werror","\nExpected a '(' to begin a slot definition\n");
      cl_print("werror","or a ')' to end slot definitions\n");
      *error = TRUE;
      return(NULL);
     }
     
   /*===========================*/
   /* Slot name must be a word. */
   /*===========================*/
      
   gettoken(inputSource,tempToken);
   if (tempToken->token != WORD)
     {
      cl_print("werror","\nExpected a slot name\n");
      *error = TRUE;
      return(NULL);
     }
    
   /*======================================================*/
   /* Check that the slot name is valid for this template. */
   /*======================================================*/
       
   if ((slotPtr = FindSlot(dtmplPtr,tempToken->hashword)) == NULL)
     {
      cl_print("werror","\nInvalid slot ");
      cl_print("werror",tempToken->tknword);
      cl_print("werror"," not defined in corresponding deftemplate\n");
      *error = TRUE;
      return(NULL);
     }
     
   /*====================================*/
   /* Return a pointer to the slot name. */
   /*====================================*/
   
   return(slotPtr);
  }
 
/****************************************************************/
/* FindSlot: Finds a specified slot in a deftemplate structure. */
/****************************************************************/
struct slot *FindSlot(dtmpl_ptr,name)
  struct dtmpl *dtmpl_ptr;
  struct draw *name;
  {
   struct slot *slotPtr;
   
   slotPtr = dtmpl_ptr->slot_list;
   while (slotPtr != NULL)
     {
      if (slotPtr->slot_name == name)
        { return(slotPtr); }
      slotPtr = slotPtr->next;
     }
     
   return(NULL);
  }
#endif
      
