/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   GENERATE MODULE                   */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "clipsmem.h"
#include "lhsparse.h"
#include "variable.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                       rpl_var_gtfield();
   int                       rpl_var_gtvar();
   int                       init_gen_ptrs();
   
   struct test              *prod_gtvar();
   struct test              *prod_gv_args();
   struct test              *prod_gtfield();
   struct test              *prod_gf_args();
   struct test              *prod_four();
   struct test              *prod_two();
   struct test              *gen_and();
   struct test              *gen_or();
   struct test              *gen_pn_constant();
   struct test              *gen_jn_constant();
   struct test              *gen_pn_colon();
   struct test              *gen_jn_colon();
   struct test              *gen_pn_eq();
   struct test              *gen_jn_eq();
   struct test              *gen_field();
   struct test              *comp_jn_vars();
   struct test              *comp_pn_vars();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct funtab     *find_function();
   extern struct var_info   *find_variable();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
    
   struct funtab            *PTR_NOP;
   struct funtab            *PTR_CONSTANT;
   struct funtab            *PTR_NOTCONSTANT;
   struct funtab            *PTR_GET_VAR;
   struct funtab            *PTR_GET_FIELD;
   struct funtab            *PTR_NEQ_VARS;
   struct funtab            *PTR_EQ_VARS;
   struct funtab            *PTR_KTAND; 
   struct funtab            *PTR_KTOR; 
   struct funtab            *PTR_EQ;
   struct funtab            *PTR_NEQ; 
   struct funtab            *PTR_NOT;
   struct funtab            *PTR_EQ_FIELD;
   struct funtab            *PTR_NEQ_FIELD;
   
/****************************************************/
/* INIT_GEN_PTRS: Initializes the function pointers */
/*   used by the generate module.                   */
/****************************************************/
init_gen_ptrs()
  {
   PTR_NOP          = find_function("(nop)");
   PTR_GET_VAR      = find_function("(get_var)");
   PTR_CONSTANT     = find_function("(constant)");
   PTR_NOTCONSTANT  = find_function("(notconstant)");
   PTR_EQ_VARS      = find_function("(eq_vars)");
   PTR_NEQ_VARS     = find_function("(neq_vars)");
   PTR_KTAND        = find_function("&&");
   PTR_KTOR         = find_function("||"); 
   PTR_EQ           = find_function("eq");
   PTR_NEQ          = find_function("neq");
   PTR_NOT          = find_function("!");
   PTR_EQ_FIELD     = find_function("(eq_field)");
   PTR_NEQ_FIELD    = find_function("(neq_field)");
   PTR_GET_FIELD    = find_function("(get_field)");

   if ((PTR_NOP == NULL) || (PTR_GET_VAR == NULL) || 
       (PTR_CONSTANT == NULL) || (PTR_NOTCONSTANT == NULL) ||
       (PTR_EQ_VARS == NULL) || (PTR_NEQ_VARS == NULL) ||
       (PTR_KTAND == NULL) || (PTR_KTOR == NULL) ||
       (PTR_EQ == NULL) || (PTR_NEQ == NULL) || (PTR_NOT == NULL) ||
       (PTR_GET_FIELD == NULL) ||
       (PTR_EQ_FIELD == NULL) || (PTR_NEQ_FIELD == NULL))
     {
      clips_system_error(401);
      cl_exit(4);
     }
 
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/****************************************************/
/* PROD_GTVAR: Produces an expression of the format */   
/*   (getvar <pattern> <field>)                     */
/****************************************************/
struct test *prod_gtvar(pattern,element)
  int pattern, element;
  {
   struct test *top;

   top = get_struct(test);
   top->type = FCALL;
   top->val.fun_ptr = PTR_GET_VAR;
   top->arg_list = prod_gv_args(pattern,element);
   top->next_arg = NULL;
   
   return(top);
  }
  
/****************************************************/
/* PROD_GV_ARGS:                                    */
/****************************************************/
struct test *prod_gv_args(pattern,element)
  int pattern, element;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = INDEX;
   top->val.index = pattern;
   top->arg_list = NULL;
   top->next_arg = get_struct(test);
   top->next_arg->type = INDEX;
   top->next_arg->val.index = element;
   top->next_arg->arg_list = NULL;
   top->next_arg->next_arg = NULL;
   
   return(top);
  }
  

/****************************************************/
/* PROD_GTFIELD: Produces an expression of the format */   
/*   (getfield <field>)                     */
/****************************************************/
struct test *prod_gtfield(element)
  int element;
  {
   struct test *top;

   top = get_struct(test);
   top->type = FCALL;
   top->val.fun_ptr = PTR_GET_FIELD;
   top->arg_list = prod_gf_args(element);
   top->next_arg = NULL;
   
   return(top);
  }
  
/*********************************************/
/* PROD_GF_ARGS: Produces an index argument. */
/*********************************************/
struct test *prod_gf_args(element)
  int element;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = INDEX;
   top->val.index = element;
   top->arg_list = NULL;
   top->next_arg = NULL;
   
   return(top);
  }


/******************************************************/
/* PROD_FOUR:  Produces an argument list to be used   */
/*   with other expressions (primarily eq_vars or     */
/*   neq_vars expressions).                           */
/*   Example:                                         */
/*     If the third element of the first pattern is   */
/*     to be compared with the second element of      */
/*     the third pattern then the argument list       */
/*     (1 3 3 2) would be produced.                   */
/*     If the expression to be evaluated were eq_var, */
/*     then the arguments would be combined with the  */
/*     expression to produce (eq_vars 1 3 3 2).       */
/******************************************************/
struct test *prod_four(p1,e1,p2,e2)
  int p1, e1, p2, e2;
  {
   struct test *top_lvl, *arg_lvl;

   top_lvl = get_struct(test);
   top_lvl->type = INDEX;
   top_lvl->val.index = p1;
   top_lvl->next_arg = get_struct(test);
   top_lvl->arg_list = NULL;
   arg_lvl = top_lvl->next_arg;
   arg_lvl->type = INDEX;
   arg_lvl->val.index = e1;
   arg_lvl->next_arg = get_struct(test);
   arg_lvl->arg_list = NULL;
   arg_lvl = arg_lvl->next_arg;
   arg_lvl->type = INDEX;
   arg_lvl->val.index = p2;
   arg_lvl->next_arg = get_struct(test);
   arg_lvl->arg_list = NULL;
   arg_lvl = arg_lvl->next_arg;
   arg_lvl->type = INDEX;
   arg_lvl->val.index = e2;
   arg_lvl->next_arg = NULL;
   arg_lvl->arg_list = NULL;
   
   return(top_lvl);
  }
  
/***************************************************************/
/* PROD_TWO: */
/***************************************************************/
struct test *prod_two(e1,e2)
  int e1, e2;
  {
   struct test *top;

   top = get_struct(test);
   top->type = INDEX;
   top->val.index = e1;
   top->arg_list = NULL;
   top->next_arg = get_struct(test);
   top->next_arg->type = INDEX;
   top->next_arg->val.index = e2;
   top->next_arg->arg_list = NULL;
   top->next_arg->next_arg = NULL;
   
   return(top);
  }
  
/***************************************************************/
/* GEN_AND:  Generates an and function call with no arguments. */
/***************************************************************/
struct test *gen_and()
  {
   struct test *top;
  
   top = get_struct(test);
   top->type = FCALL;
   top->val.fun_ptr = PTR_KTAND;
   top->next_arg = NULL;
   top->arg_list = NULL;
   return(top);
  }
  
/*************************************************************/
/* GEN_OR:  Generates an or function call with no arguments. */
/*************************************************************/
struct test *gen_or()
  {
   struct test *top;
  
   top = get_struct(test);
   top->type = FCALL;
   top->val.fun_ptr = PTR_KTOR;
   top->next_arg = NULL;
   top->arg_list = NULL;
   return(top);
  }
  
/*********************************************************/
/* GEN_PN_CONSTANT: Generates a constant test expression */
/*   for use in the pattern network.                     */
/*   Convert value   To   (constant value)               */
/*   Convert ~value  To   (notconstant value)            */
/*********************************************************/
struct test *gen_pn_constant(field)
  struct node *field;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = FCALL;
   top->next_arg = NULL;
   
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_CONSTANT; }
   else
     { top->val.fun_ptr = PTR_NOTCONSTANT; }

   top->arg_list = gen_field(field);
     
   return(top);
  }
  
/*********************************************************/
/* GEN_JN_CONSTANT: Generates a constant test expression */
/*   for use in the join network.                        */
/*   Convert                                             */
/*      value                                            */
/*   To                                                  */
/*      (eq (getvar <pattern> <field>) value)            */
/*   Convert                                             */
/*      ~value                                           */
/*   To                                                  */
/*      (neq (getvar <pattern> <field>) value)           */
/*********************************************************/
struct test *gen_jn_constant(field,pattern,element)
  struct node *field;
  int pattern, element;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = FCALL;
   top->next_arg = NULL;
   
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_EQ; }
   else
     { top->val.fun_ptr = PTR_NEQ; }

   top->arg_list = prod_gtvar(pattern,element);
   top->arg_list->next_arg = gen_field(field);
     
   return(top);
  }
  
/********************************************************/
/* GEN_JN_COLON: Generates a test expression for use in */
/*   the join network for the such that operator.       */
/*   Convert                                            */
/*      :(expression)                                   */
/*   To                                                 */
/*      (expression)                                    */
/*   Convert                                            */
/*      ~:(expression)                                  */
/*   To                                                 */
/*      (! (expresion))                                 */
/*   The variables in the expressions are replaced with */
/*   getvar calls.                                      */
/********************************************************/
struct test *gen_jn_colon(field,pattern,element,anywhere)
  struct node *field;
  int pattern, element;
  int anywhere;
  {
   struct test *top;
   
   if (field->state == 'o')
     { top = field->expression; }
   else
     {
      top = get_struct(test);
      top->type = FCALL;
      top->val.fun_ptr = PTR_NOT;
      top->arg_list = field->expression;
      top->next_arg = NULL;
     }
        
   field->expression = NULL;
   rpl_var_gtvar(top,pattern,element,anywhere);
   return(top);
  }
  
/********************************************************/
/* GEN_PN_COLON: Generates a test expression for use in */
/*   the pattern network for the such that operator.    */
/*   Convert                                            */
/*      :(expression)                                   */
/*   To                                                 */
/*      (expression)                                    */
/*   Convert                                            */
/*      ~:(expression)                                  */
/*   To                                                 */
/*      (! (expresion))                                 */
/*   The variables in the expressions are replaced with */
/*   getfield calls.                                    */
/********************************************************/
struct test *gen_pn_colon(field,pattern,element)
  struct node *field;
  int pattern, element;
  {
   struct test *top;
   
   if (field->state == 'o')
     { top = field->expression; }
   else
     {
      top = get_struct(test);
      top->type = FCALL;
      top->val.fun_ptr = PTR_NOT;
      top->arg_list = field->expression;
      top->next_arg = NULL;
     }
        
   field->expression = NULL;
   rpl_var_gtfield(top,pattern,element);
   return(top);
  }
  
/********************************************************/
/* GEN_JN_EQ: Generates a test expression for use in    */
/*   the join network for the = operator.               */
/*   Convert                                            */
/*      =(expression)                                   */
/*   To                                                 */
/*      (eq (getvar <pattern> <field>) (expression))    */
/*   Convert                                            */
/*      ~=(expression)                                  */
/*   To                                                 */
/*      (neq (getvar <pattern> <field>) (expression))   */
/*   The variables in the expressions are replaced with */
/*   getvar calls.                                      */
/********************************************************/
struct test *gen_jn_eq(field,pattern,element,anywhere)
  struct node *field;
  int pattern, element, anywhere;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = FCALL;
   top->next_arg = NULL;
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_EQ; }
   else
     { top->val.fun_ptr = PTR_NEQ; }
   top->arg_list = prod_gtvar(pattern,element);
   top->arg_list->next_arg = field->expression;
   field->expression = NULL;
      
   rpl_var_gtvar(top->arg_list->next_arg,pattern,element,anywhere);
   return(top);
  }
  
/********************************************************/
/* GEN_PN_EQ: Generates a test expression for use in    */
/*   the join network for the = operator.               */
/*   Convert                                            */
/*      =(expression)                                   */
/*   To                                                 */
/*      (eq (getfield <field>) (expression))            */
/*   Convert                                            */
/*      ~=(expression)                                  */
/*   To                                                 */
/*      (neq (getfield <field>) (expression))           */
/*   The variables in the expressions are replaced with */
/*   getfield calls.                                    */
/********************************************************/
struct test *gen_pn_eq(field,pattern,element)
  struct node *field;
  int pattern, element;
  {
   struct test *top;
   
   top = get_struct(test);
   top->type = FCALL;
   top->next_arg = NULL;
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_EQ; }
   else
     { top->val.fun_ptr = PTR_NEQ; }
   top->arg_list = prod_gtfield(element);
   top->arg_list->next_arg = field->expression;
   field->expression = NULL;
      
   rpl_var_gtfield(top->arg_list->next_arg,pattern,element);
   return(top);
  }
  
/***************************************************/
/* RPL_VAR_GTVAR: Replaces occurences of variables */
/*   in expressions with get_var calls.            */
/***************************************************/
rpl_var_gtvar(expr,pattern,element,anywhere)
  struct test *expr;
  int pattern, element, anywhere;
  {
   struct var_info *var_ptr;
   int fap, type;
   
   while (expr != NULL)
     {
      if ((expr->type == BWORD) || 
          (expr->type == BWORDS) ||  /* Correction here */
          (expr->type == MAY_BE_POINTER))
        {
         if (expr->type == BWORDS) type = BWORDS;
         else type = BWORD;
         
         if (anywhere)
           {
            var_ptr = find_variable(type,expr->val.hvalue,
                                    1,pattern,-1,INSIDE);
           }
         else
           {
            var_ptr = find_variable(type,expr->val.hvalue,
                                    1,pattern,element+1,INSIDE);
           
           }
           
         if (var_ptr == NULL)
           {    
            if (expr->type != MAY_BE_POINTER)
              {
               clips_system_error(406);
               cl_exit(4);
              }
              
            fap = get_fa_pointer(expr->val.hvalue);
            
            if (fap == 0)
              {
               clips_system_error(402);
               cl_exit(4);
              }
             
            expr->type = FCALL;
            expr->val.fun_ptr = find_function("(pointer)");
            expr->arg_list = prod_gf_args(fap);
           }
         else
           {
            expr->type = FCALL;
            expr->val.fun_ptr = PTR_GET_VAR;
            expr->arg_list = prod_gv_args(var_ptr->pattern,var_ptr->element);
           }
        }
      else if (expr->type == FCALL)
        { rpl_var_gtvar(expr->arg_list,pattern,element,anywhere); }
        
      expr = expr->next_arg;
     }
  }
  
/***************************************************/
/* RPL_VAR_GTFIELD: Replaces occurences of variables */
/*   in expressions with get_field calls.            */
/***************************************************/
rpl_var_gtfield(expr,pattern,element)
  struct test *expr;
  int pattern, element;
  {
   struct var_info *var_ptr;
   
   while (expr != NULL)
     {
      /* Bug fix here 5/26/89 */
      if (expr->type == MAY_BE_POINTER) expr->type = BWORD;
      if ((expr->type == BWORD) || (expr->type == BWORDS))
        {
         var_ptr = find_variable(expr->type,expr->val.hvalue,
                                 pattern,pattern,element+1,INSIDE);
         if (var_ptr == NULL)
           { 
            clips_system_error(403);
            cl_exit(4);
           }
           
         expr->type = FCALL;
         expr->val.fun_ptr = PTR_GET_FIELD;
         expr->arg_list = prod_gf_args(var_ptr->element);
        }
      else if (expr->type == FCALL)
        { rpl_var_gtfield(expr->arg_list,pattern,element); }
        
      expr = expr->next_arg;
     }
  }
  
/****************************************************/
/* GEN_FIELD: Generates a constant expression value */
/*   of type string, word, or number.               */
/****************************************************/
struct test *gen_field(field)
  struct node *field;
  {
   struct test *top;

   top = get_struct(test);
   top->next_arg = NULL;
   top->arg_list = NULL;
   top->type = field->type;
   if (field->type == NUMBER)
     { top->val.fvalue = field->fvalue; }
   else
     { top->val.hvalue = field->svalue; }
     
   return(top);
  }
 
/****************************************************/
/* COMP_JN_VAR               */
/****************************************************/
struct test *comp_jn_vars(field,pattern,element,anywhere)
  struct node *field;
  int pattern, element, anywhere;
  {
   struct var_info *var_ptr;
   struct test *top;
   
   if (anywhere)
     {
      var_ptr = find_variable(field->type,field->svalue,
                              1,pattern,-1,INSIDE);
     }
   else
     { 
      var_ptr = find_variable(field->type,field->svalue,
                              1,pattern,element+1,INSIDE);
     }
                                 
   if (var_ptr == NULL)
     {
      clips_system_error(404);
      cl_exit(4);
     }
                                 
   top = get_struct(test);
   top->type = FCALL;
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_EQ_VARS; }
   else
     { top->val.fun_ptr = PTR_NEQ_VARS; }
   top->next_arg = NULL;
   top->arg_list = prod_four(pattern,element,
                             var_ptr->pattern,var_ptr->element);
 
   return(top);
  }

/****************************************************/
/* COMP_PN_VAR               */
/****************************************************/
struct test *comp_pn_vars(field,pattern,element)
  struct node *field;
  int pattern, element;
  {
   struct var_info *var_ptr;
   struct test *top;
   
   var_ptr = find_variable(field->type,field->svalue,
                                 pattern,pattern,element+1,INSIDE);
                                 
   if (var_ptr == NULL)
     {
      clips_system_error(405);
      cl_exit(4);
     }
                                 
   top = get_struct(test);
   top->type = FCALL;
   if (field->state == 'o')
     { top->val.fun_ptr = PTR_EQ_FIELD; }
   else
     { top->val.fun_ptr = PTR_NEQ_FIELD; }
   top->next_arg = NULL;
   top->arg_list = prod_two(element,var_ptr->element);
 
   return(top);
  }

#endif
