/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*               VARIABLE MANAGER MODULE               */
   /*******************************************************/
   
#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "constant.h"
#include "variable.h"
#include "lhsparse.h"
#include "clipsmem.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   struct var_info        *get_variables();
   int                     check_fact_address();
   int                     check_pattern();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                     variable_analysis();
   struct test            *check_test();
   int                     check_variables();
   struct var_info        *find_variable();
   int                     flush_var_info();
   int                     print_var_info();
   int                     get_fa_pointer();
   int                     add_bind_name();
   int                     search_bind_list();
   int                     clear_bind_list();
   struct draw            *GetRelationForPattern();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char            *symbol_string();
   
#if DEFTEMPLATE
   extern struct dtmpl    *FindDeftemplate();
#endif
          
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
  
   static struct pat_info *cur_pat_info;
   static struct var_info *bind_names_list;
   static int              DeftemplatePattern;

/*#####################################*/
/* FUNCTIONS FOR GETTING THE VARIABLES */
/*#####################################*/

/*************************************************************/
/* VARIABLE_ANALYSIS:  For each pattern, determines and stores */
/*   the following information: pattern position, type of    */
/*   pattern (regular or not), fact address (if any).        */
/*************************************************************/
variable_analysis(pat_list)
  struct node *pat_list;
  {
   struct pat_info *new_pat, *head, *last_pat = NULL;
   int count = 1;
   
   flush_var_info();
   while (pat_list != NULL)
     {
      if (pat_list->type == PATTERN)
        {    
         new_pat = get_struct(pat_info);
         new_pat->position = count;
         new_pat->state = pat_list->state;
         new_pat->fact_address = pat_list->svalue;         
         new_pat->variables = get_variables(pat_list->right,count);
         new_pat->next = NULL;
         
         /*================================*/
         /* Save the relation information. */
         /*================================*/
         
         if ((pat_list->right->type != SINGLE) || (pat_list->right->bottom == NULL))
           { new_pat->relation = NULL; }
         else if ((pat_list->right->bottom->type != WORD) ||
                  (pat_list->right->bottom->right != NULL) ||
                  (pat_list->right->bottom->bottom != NULL))
           { new_pat->relation = NULL; }
         else
           { new_pat->relation = pat_list->right->bottom->svalue; }
         
         /*==================*/
         /* Add to the list. */
         /*==================*/
         
         if (last_pat == NULL)
           { head = new_pat; }
         else
           { last_pat->next = new_pat; }
        
         last_pat = new_pat;
         count++;
        }

      pat_list = pat_list->bottom;
     }

   cur_pat_info = head;
  }
  
/*************************************************************/
/* GET_VARIABLES:       */
/*************************************************************/
static struct var_info *get_variables(pat_fields,position)
  struct node *pat_fields;
  int position;
  { 
   int elem_count = 1;
   struct var_info *head = NULL, *last_var = NULL, *new_var;
  
   while (pat_fields != NULL)
     {
      if ((pat_fields->type == BWORD) || 
          (pat_fields->type == BWORDS))
        {    
         new_var = get_struct(var_info);
         new_var->type = pat_fields->type;
         new_var->name = pat_fields->svalue;
         new_var->pattern = position;
         new_var->element = elem_count;
         new_var->next = NULL;
         
         if (last_var == NULL)
           { head = new_var; }
         else
           { last_var->next = new_var; }
        
         last_var = new_var;
        }
      
      elem_count++;
      pat_fields = pat_fields->right;
     }
     
   return(head);
  }
  
/*###################################*/
/* FUNCTIONS FOR CHECKING VARIABLES. */
/*###################################*/

/************************************************************/
/* CHECK_VARIABLES: Verifies proper use of variables. */
/************************************************************/
int check_variables(pat_list)
  struct node *pat_list;
  {
   int error_flag = FALSE;
   struct pat_info *temp_pat;
   int pattern, element;
   struct test *rv;
   
   /*=====================================================*/
   /* Check that fact addresses are not reused or used as */
   /* variables within patterns.                          */
   /*=====================================================*/
   
   temp_pat = cur_pat_info;
   while (temp_pat != NULL)
     {
      if (temp_pat->fact_address != NULL)
        { 
         if (check_fact_address(temp_pat->fact_address,
                                         temp_pat->position) == TRUE)
           { error_flag = TRUE; }
        }
      temp_pat = temp_pat->next;
     }
     
   /*===========================================*/
   /* Check that variables used in patterns are */
   /* referenced properly.                      */         
   /*===========================================*/
   
   pattern = 0;
   temp_pat = cur_pat_info;
   while (pat_list != NULL)
     {
      element = 1;
      if (pat_list->type == PATTERN)
        {
#if DEFTEMPLATES 
         if (temp_pat->relation == NULL)
           { DeftemplatePattern = FALSE; }
         else if (FindDeftemplate(temp_pat->relation->contents) != NULL)
           { DeftemplatePattern = TRUE; }
         else
           { DeftemplatePattern = FALSE; }
         
#else
         DeftemplatePattern = FALSE;
#endif 

         pattern++;
         if (check_pattern(pat_list->right,pattern,element) == TRUE)
           { error_flag = TRUE; }
         temp_pat = temp_pat->next;
        }
      else if (pat_list->type == PAT_TEST)
        {
         if ((rv = check_test(pat_list->expression,1,pattern,element,OUTSIDE))
             != NULL)
           { 
            cl_print("werror","\nUndefined variable ");
            cl_print("werror",symbol_string(rv->val.hvalue));
            cl_print("werror"," referenced in test pattern:\n");
            pp_test(pat_list->expression,"werror");
            cl_print("werror","\n");
            error_flag = TRUE; 
           }
        }
      else
        {
		 clips_system_error(1701);
         cl_exit(4);
        }
   
      pat_list = pat_list->bottom;
     }
   
   return(error_flag);
  }
  
/************************************************************/
/* CHECK_FACT_ADDRESS: */
/************************************************************/
static int check_fact_address(name,position)
  struct draw *name;
  int position;
  {
   struct pat_info *temp_pat;
   struct var_info *temp_var;
   int error_flag = FALSE;
   
   temp_pat = cur_pat_info;
   while (temp_pat != NULL)
     {
      if ((temp_pat->fact_address == name) && 
          (temp_pat->position > position))
        { 
         cl_print("werror","\nDuplicate fact address ?");
         cl_print("werror",symbol_string(name));
         cl_print("werror"," used\n");
         error_flag = TRUE;
        }
        
      temp_var = temp_pat->variables;
      while (temp_var != NULL)
        {
         if (temp_var->name == name)
           { 
            cl_print("werror","\nFact address ?");
            cl_print("werror",symbol_string(name));
            cl_print("werror"," also used as variable name\n");
            error_flag = TRUE;
           }
         temp_var = temp_var->next;
        }
      
      temp_pat = temp_pat->next;
     }
     
   return(error_flag);
  }
  
/************************************************************/
/* CHECK_PATTERN */
/************************************************************/
static int check_pattern(pat_fields,pattern,element)
  struct node *pat_fields;
  int pattern, element;
  {
   int error_flag = FALSE;
   struct node *and_field, *or_field;
   int other_type, end;
   struct test *rv;
   
   while (pat_fields != NULL)
     {
      /*==================================================*/
      /* If variable has been bound, make sure a variable */
      /* with the same name but different type does not   */
      /* exist (i.e. ?x and $?x).                         */
      /*==================================================*/
      
      if ((pat_fields->type == BWORD) || (pat_fields->type == BWORDS))
        {
         if (pat_fields->type == BWORD)
           { other_type = BWORDS; }
         else
           { other_type = BWORD; }
           
         if (find_variable(other_type,pat_fields->svalue,
                           1,pattern,element,INSIDE) != NULL)
           {
            cl_print("werror","\nVariable name ");
            cl_print("werror",symbol_string(pat_fields->svalue));
            cl_print("werror"," used for both single and multi-field variable\n");
            error_flag = TRUE;
           }
        }
        
      /*======================================================*/
      /* Check that the field variables are previously bound. */
      /*======================================================*/
      
      or_field = pat_fields->bottom;
      while (or_field != NULL)
        {
         and_field = or_field;
         while (and_field != NULL)
           { 
            if (and_field->type == BWORD)
              {
               if (DeftemplatePattern) end = -1;
               else end = element + 1;
                 
               if (find_variable(and_field->type,and_field->svalue,
                           1,pattern,end,INSIDE) == NULL)
                 {
                  cl_print("werror","\nVariable name ");
                  cl_print("werror",symbol_string(and_field->svalue));
                  cl_print("werror"," was referenced in pattern #");
                  print_long_int("werror",(long int) pattern);
                  cl_print("werror"," field #");
                  print_long_int("werror",(long int) element);
                  cl_print("werror"," before it is defined\n");
                  error_flag = TRUE;
                 }
              }
            else if ((and_field->type == COAMP) || 
                     (and_field->type == FCALL))
              {
               if (DeftemplatePattern) end = -1;
               else end = element + 1;
               
               if ((rv = check_test(and_field->expression,1,pattern,end,
                                   INSIDE)) != NULL)
                 {
                  cl_print("werror","\nVariable name ");
                  cl_print("werror",symbol_string(rv->val.hvalue));
                  cl_print("werror"," was referenced in pattern #");
                  print_long_int("werror",(long int) pattern);
                  cl_print("werror"," field #");
                  print_long_int("werror",(long int) element);
                  cl_print("werror"," before it is defined\n");
                  error_flag = TRUE;
                 }
              }
            and_field = and_field->right;
           }
         or_field = or_field->bottom;
        }
        
      /*========================*/
      /* Move to the next field */
      /*========================*/
      
      element++;
      pat_fields = pat_fields->right;
     }
     
   return(error_flag);
  }
     
/************************************************************/
/* CHECK_TEST */
/************************************************************/
struct test *check_test(test_ptr,start,pattern,element,location)
  struct test *test_ptr;
  int start, pattern, element, location;
  {
   struct test *rv;
   int fap, type;
   
   while (test_ptr != NULL)
     {
      if ((test_ptr->type == BWORD) || 
          (test_ptr->type == BWORDS) ||
          (test_ptr->type == MAY_BE_POINTER))
        {
         if (test_ptr->type == BWORDS) type = BWORDS;
         else type = BWORD;
         
         if (find_variable(type,test_ptr->val.hvalue,
                            start,pattern,element,location) == NULL)
           { 
            if (test_ptr->type != MAY_BE_POINTER) return(test_ptr); 
            fap = get_fa_pointer(test_ptr->val.hvalue);
            if ((fap == 0) || 
                (fap < start) ||
                ((location == INSIDE) && (fap >= pattern)) ||
                ((location == OUTSIDE) && (fap > pattern)))
              { return(test_ptr); }
           }
        }
      else if ((test_ptr->type == FCALL) && (test_ptr->arg_list != NULL))
        { 
         if ((rv = check_test(test_ptr->arg_list,start,pattern,
                              element,location)) != NULL)
           { return(rv); }
        }
      test_ptr = test_ptr->next_arg;
     }
   
   return(NULL);
  }
  
/*#################################################*/
/* FUNCTIONS FOR ACCESSING AND MANAGING VARIABLES. */
/*#################################################*/

/************************************************************/
/* FIND_VARIABLE: */
/*    Note: -1 for element indicates that the variable can be anywhere */
/*    in the pattern.                   */
/************************************************************/
struct var_info *find_variable(type,name,start,pattern,element,location)
  int type;
  struct draw *name;
  int start, pattern, element, location;
  {
   struct pat_info *temp_pat;
   struct var_info *temp_var;
   
   /*========================================*/
   /* Skip patterns up to point of interest. */
   /*========================================*/
   
   temp_pat = cur_pat_info;
   while (temp_pat->position < start)
     { temp_pat = temp_pat->next; }
   
   /*=============================================*/
   /* Look for information in preceding patterns. */
   /*=============================================*/
   
   while (temp_pat->position < pattern)
     {
      if (temp_pat == NULL)
        {
		 clips_system_error(1702);
         cl_exit(4);
        }
      
      if (temp_pat->state != 'n')
        {
         temp_var = temp_pat->variables;
         while (temp_var != NULL)
           {
            if ((temp_var->type == type) && (temp_var->name == name))
              { return(temp_var); }
            temp_var = temp_var->next;
           }
        }
      
      temp_pat = temp_pat->next;
     }
     
   /*=======================================*/
   /* Look for information in same pattern. */
   /*=======================================*/
   
   if (temp_pat == NULL)
     {
	  clips_system_error(1703);
      cl_exit(4);
     }
      
   if ((temp_pat->state == 'n') && (location == OUTSIDE))
     { return(NULL); }
     
   /* Should the test for element be less than? */
   temp_var = temp_pat->variables;
   while ((temp_var != NULL) ?
          ((temp_var->element < element) || 
           (element == -1) ||
           (location == OUTSIDE)) :
          FALSE)
     {
      if ((temp_var->type == type) && (temp_var->name == name))
        { return(temp_var); }
      temp_var = temp_var->next;
     }
   
   return(NULL);
  }
        

/************************************************************/
/* FLUSH_VAR_INFO: Frees up current variable analysis info. */
/************************************************************/
flush_var_info()
  {
   struct pat_info *temp_pat;
   struct var_info *temp_var, *var_ptr;
   
   while (cur_pat_info != NULL)
     {
      temp_pat = cur_pat_info->next;
      var_ptr = cur_pat_info->variables;
      while (var_ptr != NULL)
        {
         temp_var = var_ptr->next;
         rtn_struct(var_info,var_ptr);
         var_ptr = temp_var;
        }
      rtn_struct(pat_info,cur_pat_info);
      cur_pat_info = temp_pat;
     }
  }
  
  
/************************************************************/
/* PRINT_VAR_INFO: */
/************************************************************/
print_var_info()
  {
   struct pat_info *temp_pat;
   struct var_info *temp_var;
   
   temp_pat = cur_pat_info;
   while (temp_pat != NULL)
     {
      printf("Pattern #%d state: %c",temp_pat->position,temp_pat->state);
      if (temp_pat->fact_address == NULL)
        { printf(" FA: None\n"); }
      else
        { printf(" FA: ?%s\n",symbol_string(temp_pat->fact_address)); }
        
      printf("  Variables\n");
      if (temp_pat->variables == NULL)
        { printf("   None\n"); }
      
      temp_var = temp_pat->variables;
      while (temp_var != NULL)
        {
         if (temp_var->type == BWORD)
           { printf("?"); }
         else if (temp_var->type == BWORDS)
           { printf("$?"); }
         else 
           { printf("!"); }
         
         printf("%s %d %d\n",symbol_string(temp_var->name),
                             temp_var->pattern,temp_var->element);
         temp_var = temp_var->next;
        } 
      
      temp_pat = temp_pat->next;
     }
  }
  
  
/********************************************************/
/* GET_FA_POINTER:                                      */
/********************************************************/
get_fa_pointer(name)
  struct draw *name;
  {
   struct pat_info *pat_ptr;
   
   pat_ptr = cur_pat_info;
   while (pat_ptr != NULL)
     {
      if (pat_ptr->fact_address == name)
        { return(pat_ptr->position); }
      pat_ptr = pat_ptr->next;
     }
     
   return(0);
  }
  
/********************************************************/
/* GetRelationForPattern:                               */
/********************************************************/
struct draw *GetRelationForPattern(patternNumber)
  int patternNumber;
  {
   struct pat_info *pat_ptr;
   
   pat_ptr = cur_pat_info;
   while ((pat_ptr != NULL) && (patternNumber > 1))
     {
      patternNumber--;
      pat_ptr = pat_ptr->next;
     }
    
   if (pat_ptr != NULL) return(pat_ptr->relation);
    
   return(NULL);
  }

/********************************************************/
/* SEARCH_BIND_LIST:                                      */
/********************************************************/
search_bind_list(type_sought,name_sought)
  int type_sought;
  struct draw *name_sought;
  {
   struct var_info *var_ptr;
   
   var_ptr = bind_names_list;
   while (var_ptr != NULL)
     {
      if ((var_ptr->name == name_sought) && 
          (var_ptr->type == type_sought))
        { return(TRUE); }
      var_ptr = var_ptr->next;
     }

   return(FALSE);
  }
  
/********************************************************/
/* ADD_BIND_NAME:                                      */
/********************************************************/
add_bind_name(type,new_name)
  int type;
  struct draw *new_name;
  {
   struct var_info *new_bind;
   
   if (search_bind_list(type,new_name) == TRUE) return;
   
   new_bind = get_struct(var_info);
   new_bind->type = type;
   new_bind->name = new_name;
   new_bind->next = bind_names_list;
   bind_names_list = new_bind;
  }
  

/********************************************************/
/* CLEAR_BIND_LIST:                                     */
/********************************************************/
clear_bind_list()
  {
   struct var_info *temp_bind;
   
   while (bind_names_list != NULL)
     {
      temp_bind = bind_names_list->next;
      rtn_struct(var_info,bind_names_list);
      bind_names_list = temp_bind;
     }
  }
 
/********************************************************/
/* PatternHasTemplate:                          */
/********************************************************/
PatternHasTemplate(patternNumber)
  int patternNumber;
  {
   struct pat_info *pat_ptr;
   
   pat_ptr = cur_pat_info;
   while ((pat_ptr != NULL) && (patternNumber > 1))
     {
      patternNumber--;
      pat_ptr = pat_ptr->next;
     }
    
   if (pat_ptr != NULL) 
   
#if DEFTEMPLATES 
     {
      if (pat_ptr->relation == NULL)
        { return(FALSE); }
      else if (FindDeftemplate(pat_ptr->relation->contents) != NULL)
        { return(TRUE); }
      else
        { return(FALSE); }
     }
#else
     { return(FALSE); }
#endif 
   else
     { return(FALSE); }
  } 
  
#endif

