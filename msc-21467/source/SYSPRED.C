/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    SYSPRED MODULE                   */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "clips.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                       mult_eq();
   
/***************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
  
   float                   numget();
   float                   my_add();
   float                   my_and();
   float                   my_divide();
   float                   cl_eq();
   float                   my_gt();
   float                   my_gteq();
   float                   my_lt();
   float                   my_lteq();
   float                   my_mult();
   float                   my_neq();
   float                   my_not();
   float                   my_num_eq();
   float                   my_num_neq();
   float                   my_or();
   float                   my_sub();
   float                   numberp();
   float                   wordp();
   float                   stringp();

/******************************************************************/
/* SYSPRED_DEFINE: Defines standard math and predicate functions. */
/******************************************************************/
syspred_define()
  {
   extern struct test *multi_arg_parse();
   extern struct test *eq_parse();
   extern struct test *not_parse();

#if ! RUN_TIME
   extern int          add_func_parser();
#endif
   
   define_function("!",       'f', (int (*)()) my_not,      "my_not");
   define_function("not",     'f', (int (*)()) my_not,      "my_not");
   define_function("&&",      'f', (int (*)()) my_and,      "my_and");
   define_function("and",     'f', (int (*)()) my_and,      "my_and");
   define_function("||",      'f', (int (*)()) my_or,       "my_or");
   define_function("or",      'f', (int (*)()) my_or,       "my_or");
   define_function("+",       'f', (int (*)()) my_add,      "my_add");
   define_function("*",       'f', (int (*)()) my_mult,     "my_mult");
   define_function("-",       'f', (int (*)()) my_sub,      "my_sub");
   define_function("/",       'f', (int (*)()) my_divide,   "my_divide");
   define_function("<=",      'f', (int (*)()) my_lteq,     "my_lteq");
   define_function(">=",      'f', (int (*)()) my_gteq,     "my_gteq");
   define_function("<",       'f', (int (*)()) my_lt,       "my_lt");
   define_function(">",       'f', (int (*)()) my_gt,       "my_gt");
   define_function("=",       'f', (int (*)()) my_num_eq,   "my_num_eq");
   define_function("!=",      'f', (int (*)()) my_num_neq,  "my_num_neq");

   define_function("eq",      'f', (int (*)()) cl_eq,       "cl_eq");
   define_function("neq",     'f', (int (*)()) my_neq,      "my_neq");
   define_function("wordp",   'f', (int (*)()) wordp,       "wordp");
   define_function("stringp", 'f', (int (*)()) stringp,     "stringp");
   define_function("numberp", 'f', (int (*)()) numberp,     "numberp");
   
#if ! RUN_TIME
   add_func_parser("+",multi_arg_parse);
   add_func_parser("*",multi_arg_parse);
   add_func_parser("-",multi_arg_parse);
   add_func_parser("/",multi_arg_parse);
   add_func_parser("<=",multi_arg_parse);
   add_func_parser(">=",multi_arg_parse);
   add_func_parser("<",multi_arg_parse);
   add_func_parser(">",multi_arg_parse);
   add_func_parser("=",multi_arg_parse);
   add_func_parser("!=",multi_arg_parse);
   
   add_func_parser("&&",multi_arg_parse); 
   add_func_parser("and",multi_arg_parse);
   add_func_parser("||",multi_arg_parse);
   add_func_parser("or",multi_arg_parse);
   add_func_parser("!",not_parse);
   add_func_parser("not",not_parse);
   add_func_parser("eq",eq_parse);
   add_func_parser("neq",eq_parse);
#endif

  }

/****************************************/
/* CL_EQ:                               */
/****************************************/
float cl_eq()
  {
   VALUE item, next_item;
   ELEMENT_PTR elem_a, elem_b;
   int num_a, i, a_extent, b_extent;

   num_a = num_args();

   if (num_a == 0)
     { return(CLIPS_FALSE); }
   
   runknown(1,&item);
   
   if (get_valtype(item) == MULTIPLE)
     {
      elem_b = get_valelement(item,get_valbegin(item));
      b_extent = get_vallength(item); 
     }

   for (i = 2 ; i <= num_a ; i++)
     {
      runknown(i,&next_item);
      if (get_valtype(next_item) != get_valtype(item))
        { return(CLIPS_FALSE); }
      
      if (get_valtype(next_item) == NUMBER) 
        { if (get_valfloat(next_item) != get_valfloat(item)) return(CLIPS_FALSE); }
      else if (get_valtype(next_item) == MULTIPLE)
        {
         elem_a = get_valelement(next_item,get_valbegin(next_item));
         a_extent = get_vallength(next_item);

         if (mult_eq(elem_a,elem_b,a_extent,b_extent) == FALSE)
           { return(CLIPS_FALSE); }
        }
      else if (get_valtype(next_item) == POINTER)
        { if (next_item.origin != item.origin) return(CLIPS_FALSE); }
      else if (get_valstring(next_item) != get_valstring(item))
        { return(CLIPS_FALSE); }        
     }
   
   return(CLIPS_TRUE);
  }

/****************************************/
/* CL_NEQ:                              */
/****************************************/
float my_neq()
  {
   VALUE item, next_item;
   ELEMENT_PTR elem_a, elem_b;
   int num_a, i, a_extent, b_extent;
   
   num_a = num_args();

   if (num_a == 0)
     { return(CLIPS_FALSE); }
   
   runknown(1,&item);
   if (get_valtype(item) == MULTIPLE)
     {
      elem_b = get_valelement(item,get_valbegin(item));
      b_extent = get_vallength(item); 
     }

   for (i = 2 ; i <= num_a ; i++)
     {
      runknown(i,&next_item);
      if (get_valtype(next_item) != get_valtype(item))
        { /* skip this one */ }
      
      else if (get_valtype(next_item) == NUMBER)  
        { if (get_valfloat(next_item) == get_valfloat(item)) return(CLIPS_FALSE); }
      else if (next_item.type == MULTIPLE)
        {
         elem_a = get_valelement(next_item,get_valbegin(next_item));
         a_extent = get_vallength(next_item);

         if (mult_eq(elem_a,elem_b,a_extent,b_extent) == TRUE)
           { return(CLIPS_FALSE); }
        }
      else if (get_valtype(next_item) == POINTER)
        { if (next_item.origin == item.origin) return(CLIPS_FALSE); }
      else if (get_valstring(next_item) == get_valstring(item))
        { return(CLIPS_FALSE); }        
     }
   
   return(CLIPS_TRUE);
  }


/**************************************************/
/* MULT_EQ: determines if two segments are equal. */
/**************************************************/
static int mult_eq(elem_a,elem_b,a_extent,b_extent)
  ELEMENT_PTR elem_a, elem_b;
  int a_extent, b_extent;
  {
   if (a_extent != b_extent)
     { return(CLIPS_FALSE); }  

   while (a_extent != 0)
     {
      if (get_elmtype(elem_a) != get_elmtype(elem_b))
        { return(CLIPS_FALSE); }

      if (get_elmtype(elem_a) == NUMBER)
        {
         if (get_elmfloat(elem_a) != get_elmfloat(elem_b))
           { return(CLIPS_FALSE); }
        }
      else if (get_elmhash(elem_a) != get_elmhash(elem_b))
        { return(CLIPS_FALSE); }

      a_extent--;

      if (a_extent > 0) 
        {
         elem_a++;
         elem_b++;
        }
     }
   return(TRUE);
  }

/****************************************/
/* STRINGP:                             */
/****************************************/
float stringp()
  {
   VALUE item;
   
   if (arg_num_check("stringp",EXACTLY,1) == -1) return(CLIPS_FALSE);
   
   runknown(1,&item);

   if (get_valtype(item) == STRING)
     { return(CLIPS_TRUE); }
   else
     { return(CLIPS_FALSE); }
  }

/**************************************/
/* WORDP:                             */
/**************************************/
float wordp()
  {
   VALUE item;
   
   if (arg_num_check("wordp",EXACTLY,1) == -1) return(CLIPS_FALSE);
   
   runknown(1,&item);

   if (get_valtype(item) == WORD)
     { return(CLIPS_TRUE); }
   else
     { return(CLIPS_FALSE); }
  }

/****************************************/
/* NUMBERP                              */
/****************************************/
float numberp()
  {
   VALUE item;
   
   if (arg_num_check("numberp",EXACTLY,1) == -1) return(CLIPS_FALSE);
   
   runknown(1,&item);

   if (get_valtype(item) == NUMBER)
     { return(CLIPS_TRUE); }
   else
     { return(CLIPS_FALSE); }
  }

/****************************************/
/* not                                  */
/****************************************/
float my_not()
  {
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();
   if (test_ptr == NULL) { return(CLIPS_FALSE); }

   if (numget(test_ptr,"!") == CLIPS_FALSE)
     { return(CLIPS_TRUE); }
   
   return(CLIPS_FALSE);
  }

/****************************************/
/* and                                  */
/****************************************/
float my_and()
  {
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      if (numget(test_ptr,"&&") == CLIPS_FALSE)
        { return(CLIPS_FALSE); }
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }

/****************************************/
/* or                                   */
/****************************************/
float my_or()
  {
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      if (numget(test_ptr,"||") != CLIPS_FALSE)
        { return(CLIPS_TRUE); }
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_FALSE);
  }

/****************************************/
/* plus                                 */
/****************************************/
float my_add()
  {
   float total = 0.0;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      total += numget(test_ptr,"+");
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(total);
  } 

/****************************************/
/* multiply                             */
/****************************************/
float my_mult()
  {
   float total = 1.0;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      total *= numget(test_ptr,"*");
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(total);
  } 

/****************************************/
/* subtract                             */
/****************************************/
float my_sub()
  {
   float total;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();
   if (test_ptr == NULL) { return(0.0); }
   total = numget(test_ptr,"-");
   test_ptr = get_next_arg(test_ptr);

   while (test_ptr != NULL)
     {
      total -= numget(test_ptr,"-");
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(total);
  } 

/****************************************/
/* divide                               */
/****************************************/
float my_divide()
  {
   float total, num;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();
   if (test_ptr == NULL) { return(1.0); }
   total = numget(test_ptr,"/");
   test_ptr = get_next_arg(test_ptr);

   while (test_ptr != NULL)
     {
      num = numget(test_ptr,"/");
      if (num == 0)
        {
         cl_print("werror","Attempt to divide by zero\n");
         set_execution_error(TRUE);
         return(1.0);
        }
      total /= num;
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(total);
  }   

/****************************************/
/* less than or equal                   */
/****************************************/
float my_lteq()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,"<=");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,"<=");
      if (first > arg_value) { return(CLIPS_FALSE); }
      first = arg_value;
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }

/****************************************/
/* greater than or equal                */
/****************************************/
float my_gteq()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,">=");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,">=");
      if (first < arg_value) { return(CLIPS_FALSE); }
      first = arg_value;
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  } 

/****************************************/
/* less than                            */
/****************************************/
float my_lt()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,"<");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,"<");
      if (first >= arg_value) { return(CLIPS_FALSE); }
      first = arg_value;
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }
 
/****************************************/
/* greater than                         */
/****************************************/
float my_gt()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,">");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,">");
      if (first <= arg_value) { return(CLIPS_FALSE); }
      first = arg_value;
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }
 
/****************************************/
/* numeric equal                        */
/****************************************/
float my_num_eq()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,"=");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,"=");
      if (first != arg_value) { return(CLIPS_FALSE); }
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }
 
/****************************************/
/* numeric not equal                    */
/****************************************/
float my_num_neq()
  {
   float first;
   float arg_value;
   EXPR_PTR test_ptr;

   test_ptr = get_first_arg();

   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   first = numget(test_ptr,"!=");
   test_ptr = get_next_arg(test_ptr);
   if (test_ptr == NULL) { return(CLIPS_TRUE); }
   
   while (test_ptr != NULL)
     {
      arg_value = numget(test_ptr,"!=");
      if (first == arg_value) { return(CLIPS_FALSE); }
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(CLIPS_TRUE);
  }
