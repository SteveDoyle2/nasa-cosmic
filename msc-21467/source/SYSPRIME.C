/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   SYSPRIME MODULE                   */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern int              fast_assert();
   extern int              retract();
   extern int              slow_assert();
   extern int              my_get_field();
   extern struct element  *fast_gv();
   extern char            *get_currentrule();
   extern char            *symbol_string();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct values     *bind_list = NULL;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct fbind      *gbl_lhs_binds;
   extern struct fbind      *gbl_rhs_binds;
   
/*******************************************/
/* SYSFCTNS: sets up definitions of system */
/*   defined functions.                    */
/*******************************************/
sysfctns()
  {
   sysprimary_define();
   syssecondary_define();
   sysio_define();
   syspred_define();
#if MULTIFIELD_FUNCTIONS
   multivar_define();
#endif
#if STRING_FUNCTIONS
   string_define();
#endif
  }
  
/********************************************************************/
/* CL_WHILE:                                                        */
/********************************************************************/
cl_while(while_result)
  VALUE_PTR while_result;
  {
   VALUE arg_ptr;
   
   runknown(1,&arg_ptr);
   while ((arg_ptr.val.fvalue != 0.0) && (get_execution_error() != TRUE))
     { 
      runknown(2,&arg_ptr);
      runknown(1,&arg_ptr); 
     }
   
   while_result->type = NUMBER;
   while_result->val.fvalue = 0.0;
   return;
  }

/********************************************************************/
/* CL_IF:                                                       */
/********************************************************************/
cl_if(if_result)
  VALUE_PTR if_result;
  {
   int num_a;
   VALUE arg_ptr;

   num_a = num_args();
   
   if ((num_a != 2) && (num_a != 3))
     {
      cl_print("werror","Wrong number of arguments to if\n");
      if_result->type = NUMBER;
      if_result->val.fvalue = 0.0;
      return;
     }

   runknown(1,&arg_ptr);

   /* IMPORTANT: Check neccessary tests */
   
   if ((arg_ptr.val.fvalue == 0.0) && (num_a == 3))
     {
      runknown(3,&arg_ptr); 
      if_result->type = arg_ptr.type;
      if_result->val = arg_ptr.val;
      return;
     }
   else if (arg_ptr.val.fvalue != 0.0)
     {
      runknown(2,&arg_ptr); 
      if_result->type = arg_ptr.type;
      if_result->val = arg_ptr.val;
      return;
     }

   if_result->type = NUMBER;
   if_result->val.fvalue = 0.0;
   return;   
  }

/******************************/
/* CL_CALL:  Calls function.  */
/******************************/
float cl_call()
  {
   int num_a, i;
   struct values arg_ptr;

   if ((num_a = arg_num_check("call",AT_LEAST,1)) == -1) return(0.0);
 
   for (i = 1 ; i <= num_a ; i++)
     { runknown(i,&arg_ptr); }

   return(num_a);
  }

/***********************************************************/
/* CL_BIND:  Performs bind operation on the rhs of a rule. */
/***********************************************************/
cl_bind(bind_result)
  VALUE_PTR bind_result;
  {
   struct values arg_ptr, *bind_ptr, *last_bind;
   int found = FALSE;
   char *var_name;

   /*===============================================*/
   /* Determine the name of the variable to be set. */
   /*===============================================*/

   runknown(1,&arg_ptr);
   var_name = arg_ptr.val.hvalue->contents;
 
   /*===========================================*/
   /* Determine the new value for the variable. */
   /*===========================================*/

   runknown(2,&arg_ptr);

   /*===============================================*/
   /* Search for the variable in the list of binds. */
   /*===============================================*/

   bind_ptr = bind_list;
   last_bind = NULL;

   while ((bind_ptr != NULL) && (found == FALSE))
     {
      if (bind_ptr->name == var_name)
        { found = TRUE; }
      else
        {
         last_bind = bind_ptr;
         bind_ptr = bind_ptr->next;
        }
     }

   /*========================================================*/
   /* If variable was not in the list of binds, then add it. */
   /*========================================================*/

   if (found == FALSE)
     {
      bind_ptr = get_struct(values);
      bind_ptr->name = var_name;
      bind_ptr->origin = NULL;
      bind_ptr->next = NULL;
      if (last_bind == NULL)
        { bind_list = bind_ptr; }
      else
        { last_bind->next = bind_ptr; }
     }

   /*================================*/
   /* Set the value of the variable. */
   /*================================*/

   bind_result->type = bind_ptr->type = arg_ptr.type;
   if (bind_result->type == MULTIPLE)
     {
      bind_result->origin = bind_ptr->origin = arg_ptr.origin;
      bind_result->begin = bind_ptr->begin = arg_ptr.begin;
      bind_result->end = bind_ptr->end = arg_ptr.end; 
     }
   else
     { 
      bind_ptr->val = arg_ptr.val;
      bind_result->val = arg_ptr.val;
     }
  
   return;
  }

/****************************************/
/* CL_HALT:                              */
/****************************************/
int cl_halt() 
  { 
   arg_num_check("halt",EXACTLY,0);
   set_execution_error(TRUE);
  }
  
/****************************************/
/* progn                                */
/****************************************/
progn(progn_value)
  VALUE_PTR progn_value;
  {
   VALUE eval_result, arg_ptr;
   int numa, i;

   numa = num_args();

   if (numa == 0)
     {
      progn_value->type = WORD;
      progn_value->val.hvalue = add_symbol("word"); 
      return;
     }

   i = 1;
   while ((i < numa) && (get_execution_error() != TRUE))
     {
      runknown(i,&arg_ptr); 
      i++;
     }
     
   if (get_execution_error() == TRUE)
     {
      progn_value->type = NUMBER; 
      progn_value->val.fvalue = 0.0;
      return;
     }

   runknown(numa,&eval_result);
   progn_value->type = eval_result.type;
   progn_value->val = eval_result.val;
   return;
  }

/****************************************/
/* MY_NOP:  This function does nothing. */
/****************************************/
float my_nop()
  { return(1.0); }
 
/*********************************************/
/* MY_CONSTANT:  This function does nothing. */
/*********************************************/
float my_constant()
  { return(1.0); }
 
/************************************************/
/* MY_NOTCONSTANT:  This function does nothing. */
/************************************************/
float my_notconstant()
  { return(1.0); }
 
/********************************************/
/* MY_EQ_VARS:  This function does nothing. */
/********************************************/
float my_eq_vars()
  { return(1.0); }
 
/*********************************************/
/* MY_NEQ_VARS:  This function does nothing. */
/*********************************************/
float my_neq_vars()
  { return(1.0); }
  
/********************************************/
/* MY_EQ_FIELD:  This function does nothing. */
/********************************************/
float my_eq_field()
  { return(1.0); }
 
/*********************************************/
/* MY_NEQ_FIELD:  This function does nothing. */
/*********************************************/
float my_neq_field()
  { return(1.0); }

/***********************************************************/
/* MY_BIND:  Performs bind operation on the rhs of a rule. */
/***********************************************************/
my_get_bind(bind_result)
  VALUE_PTR bind_result;
  {
   struct values *bind_ptr;
   char *var_name;
   struct element *elem_a;
   struct test *test_ptr;
   int extent, start, multi;
   struct fact *fact_ptr;
   
   /*============================================*/
   /* Get the name of the variable being sought. */
   /*============================================*/

   test_ptr = get_first_arg();
   var_name = symbol_string(test_ptr->val.hvalue);

   /*===============================================*/
   /* Search for the variable in the list of binds. */
   /*===============================================*/

   bind_ptr = bind_list;
   while (bind_ptr != NULL)
     {
      if (bind_ptr->name == var_name)
        {
         bind_result->type = bind_ptr->type;
         if (bind_result->type == NUMBER)
           { bind_result->val.fvalue = bind_ptr->val.fvalue; }
         else if (bind_result->type == MULTIPLE)
           {
            bind_result->origin = bind_ptr->origin;
            bind_result->begin = bind_ptr->begin;
            bind_result->end = bind_ptr->end; 
           }
         else
           {
            bind_result->val.hvalue = bind_ptr->val.hvalue;
           }
           
         return; 
        }

      bind_ptr = bind_ptr->next;
     }

   /*=====================================================*/
   /* If the variable was not found in the list of binds, */
   /* determine if the variable is only bound on the rhs  */
   /* of the rule.                                        */
   /*=====================================================*/
  
   test_ptr = test_ptr->next_arg;
   if (test_ptr->val.index <= 0)
     {				    
      set_execution_error(TRUE);	
      cl_print("werror","Variable ");
      cl_print("werror",var_name);
      cl_print("werror"," unbound in rule ");
      cl_print("werror",get_currentrule()); 
      bind_result->type = WORD;
      bind_result->val.hvalue = add_symbol("");
      return;
     }

   /*=====================================================*/
   /* Variable was bound on the LHS of the rule.  Extract */
   /* the value from one of the LHS patterns.             */
   /*=====================================================*/

   multi = FALSE;
   elem_a = fast_gv(test_ptr->val.index,
                    test_ptr->next_arg->val.index,&extent,
                    &fact_ptr,&start,&multi);
   
   if (! multi)
     {
      bind_result->type = elem_a->type;

      if (bind_result->type != NUMBER)
        { bind_result->val.hvalue = elem_a->val.hvalue; }
      else
        { bind_result->val.fvalue = elem_a->val.fvalue; }
     }
   else
     {
      bind_result->type = MULTIPLE;
      bind_result->origin = fact_ptr;
      bind_result->begin = start;
      bind_result->end = start + (extent - 1); 
     }

   return;
  }
  
/************************************************************************/
/* get_pointer:  Returns a pointer to the fact which matches one of the */
/*   patterns in a rule.  This function is called by retract to get the */
/*   value of the fact binders.                                         */
/*   The protocol for using this argument type has not yet been defined */
/*   for user functions.                                                */
/************************************************************************/
get_pointer(bound_var)
  VALUE_PTR bound_var;
  {
   int pattern, count;
   struct fbind *lhs_binds, *rhs_binds;
   struct fact *fact_ptr;
   struct test *arg_ptr;
   
   /*==================================*/
   /* Extract the pointer to the fact. */
   /*==================================*/

   lhs_binds = gbl_lhs_binds;
   rhs_binds = gbl_rhs_binds;
   count = 1;

   arg_ptr = get_first_arg();
   pattern = arg_ptr->val.index;

   while ((lhs_binds != NULL) && (count < pattern))
     {
      lhs_binds = lhs_binds->next;
      count++;
     }

   if (lhs_binds != NULL)
     { fact_ptr = lhs_binds->origin; }
   else
     {
      if (rhs_binds == NULL) 
        {
         clips_system_error(901);
         cl_exit(5);
        }
      while (count < pattern)
        {
         rhs_binds = rhs_binds->next;
         count++;
        }
      fact_ptr = rhs_binds->origin;
     }

   /*======================================================*/
   /* Return the unknown structure with the pointer value. */
   /*======================================================*/

   bound_var->type = POINTER;
   bound_var->origin = fact_ptr;
   return;
  }
  
/*****************************************************************/
/* MY_GET_VAR:  Extracts the nth element from the mth pattern of */ 
/*   a rule.                                                     */
/*****************************************************************/
my_get_var(bound_var)
  VALUE_PTR bound_var;
  {
   int pattern, element, count;
   struct fbind *lhs_binds, *rhs_binds;
   struct fact *fact_ptr;
   struct element *elem_ptr;
   struct fact_marker *marks;
   struct test *test_ptr;
   int extent;

   lhs_binds = gbl_lhs_binds;
   rhs_binds = gbl_rhs_binds;
   count = 1;

   test_ptr = get_first_arg();
   pattern = get_test_index(test_ptr);
   element = get_test_index(get_next_arg(test_ptr));
   

   while ((lhs_binds != NULL) && (count < pattern))
     {
      lhs_binds = lhs_binds->next;
      count++;
     }

   if (lhs_binds != NULL)
     { 
      fact_ptr = lhs_binds->origin;
      marks = lhs_binds->marker; 
     }
   else
     {
      while ((rhs_binds != NULL) && (count < pattern))
        {
         rhs_binds = rhs_binds->next;
         count++;
        }
      if (rhs_binds == NULL)
        {
         clips_system_error(902);
         cl_exit(5);
        }
      fact_ptr = rhs_binds->origin;
      marks = rhs_binds->marker;
     }

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
  
/*****************************************************************/
/* MY_GET_END: Used for modify function.  */
/*****************************************************************/
my_get_end(bound_var)
  VALUE_PTR bound_var;
  {
   int pattern, element, count;
   struct fbind *lhs_binds, *rhs_binds;
   struct fact *fact_ptr;
   struct test *test_ptr;

   lhs_binds = gbl_lhs_binds;
   rhs_binds = gbl_rhs_binds;
   count = 1;

   test_ptr = get_first_arg();
   pattern = get_test_index(test_ptr);
   element = get_test_index(get_next_arg(test_ptr));
   
   while ((lhs_binds != NULL) && (count < pattern))
     {
      lhs_binds = lhs_binds->next;
      count++;
     }

   if (lhs_binds != NULL)
     { fact_ptr = lhs_binds->origin; }
   else
     {
      while ((rhs_binds != NULL) && (count < pattern))
        {
         rhs_binds = rhs_binds->next;
         count++;
        }
      if (rhs_binds == NULL)
        {
         clips_system_error(972);
         cl_exit(5);
        }
      fact_ptr = rhs_binds->origin;
     }

   bound_var->type = MULTIPLE;
   bound_var->origin = fact_ptr;
   bound_var->begin = --element;
   bound_var->end = fact_ptr->fact_length - 1;
   return;
  }

/**********************************************************************/
/* FAST_GV:  Extracts the nth element from the mth pattern of a rule. */
/**********************************************************************/
struct element *fast_gv(pattern,element,extent,temp_pptr,start,multi)
  int pattern, element, *extent, *start, *multi;
  struct fact **temp_pptr;
  {
   int count;
   struct fbind *lhs_binds, *rhs_binds;
   struct fact *fact_ptr;
   struct element *elem_ptr;
   struct fact_marker *marks;

   lhs_binds = gbl_lhs_binds;
   rhs_binds = gbl_rhs_binds;
   count = 1;

   while ((lhs_binds != NULL) && (count < pattern))
     {
      lhs_binds = lhs_binds->next;
      count++;
     }

   if (lhs_binds != NULL)
     { 
      fact_ptr = lhs_binds->origin;
      marks = lhs_binds->marker; 
     }
   else
     {
      while ((rhs_binds != NULL) && (count < pattern))
        {
         rhs_binds = rhs_binds->next;
         count++;
        }
      if (rhs_binds == NULL)
        {
         clips_system_error(903);
         cl_exit(5);
        }
      fact_ptr = rhs_binds->origin;
      marks = rhs_binds->marker;
     }

   *extent = 1;

   if (marks != NULL)
     {
      if (marks->element < element)
        { element = bump_elm_num(marks,element,extent); }
      else if (marks->element == element)
        { 
         *extent = (marks->end - marks->start) + 1; 
         *multi = TRUE;
        }
     }
 
   element--;
   elem_ptr = &fact_ptr->atoms[element];
   
   *start = element;
   *temp_pptr = fact_ptr;
   return(elem_ptr);
  }

/************************************************************************/
/* BUMP_ELM_NUM:  Given a list of segment markers and the index to an   */
/*   variable in a pattern, this function computes the index to the     */
/*   to the element in the fact where the variable begins.  In the      */
/*   case of segment variables, it also computes the extent (or length) */
/*   of the segment.  Note that the extent should be given a default    */
/*   value of either -1 or 1 for variables other than segment variables */
/*   before calling this routine.  An extent of -1 for these variables  */
/*   will distinguish their extent as being different when it is        */
/*   necessary to note their difference from a segment variable with an */
/*   extent of 1.                                                       */
/************************************************************************/
bump_elm_num(mark_list,num,extent)
  struct fact_marker *mark_list;
  int num, *extent;
  {
   int new_num;

   new_num = num;
   while (mark_list != NULL)
     {
      if (mark_list->element == num)
        {
         *extent = (mark_list->end - mark_list->start) + 1;
         return(new_num);
        }
      else if (mark_list->element > num)
        { return(new_num); }

      new_num += (mark_list->end - mark_list->start);
      mark_list = mark_list->next;
     }

   return(new_num);
  }
  
  
/***********************************************************/
/* FLUSH_BIND_LIST:                                        */
/***********************************************************/
flush_bind_list()
  {
   struct values *next_bind;
   
   while (bind_list != NULL)
     {
      next_bind = bind_list->next;
      rtn_struct(values,bind_list);
      bind_list = next_bind;
     }
  }
  
/*******************************************/
/* SYSPRIMARY_DEFINE                    */
/*******************************************/
sysprimary_define()
  {
   /*======================*/
   /* Primary RHS actions. */
   /*======================*/

   define_function("assert",        'v', (int (*)()) fast_assert,    "fast_assert");
   define_function("slow_assert",   'v', (int (*)()) slow_assert,    "slow_assert");
   define_function("retract",       'v', (int (*)()) retract,        "retract");
#if ! STUDENT
   define_function("if",            'u', (int (*)()) cl_if,          "cl_if");
   define_function("while",         'u', (int (*)()) cl_while,       "cl_while");
#endif
   define_function("bind",          'u', (int (*)()) cl_bind,        "cl_bind");
   define_function("call",          'f', (int (*)()) cl_call,        "cl_call");
   define_function("halt",          'v', (int (*)()) cl_halt,        "cl_halt");
   define_function("progn",         'u', (int (*)()) progn,          "progn");

   /*========================*/
   /* System Only functions. */
   /*========================*/

   define_function("(nop)",           'f', (int (*)()) my_nop,        "my_nop");
   define_function("(constant)",      'f', (int (*)()) my_constant,   "my_constant");
   define_function("(notconstant)",   'f', (int (*)()) my_notconstant,"my_notconstant");
   define_function("(neq_vars)",      'f', (int (*)()) my_eq_vars,    "my_eq_vars");
   define_function("(eq_vars)",       'f', (int (*)()) my_neq_vars,   "my_neq_vars");
   define_function("(neq_field)",     'f', (int (*)()) my_eq_field,   "my_eq_field");
   define_function("(eq_field)",      'f', (int (*)()) my_neq_field,  "my_neq_field");
   define_function("(get_var)",       'u', (int (*)()) my_get_var,    "my_get_var");
   define_function("(get_bind)",      'u', (int (*)()) my_get_bind,   "my_get_bind");
   define_function("(pointer)",       'u', (int (*)()) get_pointer,   "get_pointer");
   define_function("(get_field)",     'u', (int (*)()) my_get_field,  "my_get_field");
   define_function("(get_end)",       'u', (int (*)()) my_get_end,    "my_get_end");
  }
