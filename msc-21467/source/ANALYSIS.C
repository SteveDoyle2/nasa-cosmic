/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   ANALYSIS MODULE                   */
   /*******************************************************/
   
#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "constant.h"
#include "lhsparse.h"
#include "analysis.h"
#include "clipsmem.h"

#define ANALYSIS_SWITCH 0

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
   
   int                       build_network_tests();
   int                       field_conversion();
   int                       extract_ands();
   int                       all_in_pattern();
   struct test              *combine_expressions();

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                       rule_analysis();
   struct test              *get_pn_expr();
   struct test              *get_join_expr();
   struct test              *get_not_expr();
   char                      get_join_logic();
   int                       count_joins();
   int                       get_node_type();
   int                       get_elem_count();
   int                       flush_expr_list();
   int                       print_nodes();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct test       *gen_and();
   extern struct test       *gen_or();
   extern struct test       *gen_pn_constant();
   extern struct test       *gen_jn_constant();
   extern struct test       *gen_pn_colon();
   extern struct test       *gen_jn_colon();
   extern struct test       *gen_pn_eq();
   extern struct test       *gen_jn_eq();
   extern struct test       *gen_field();
   extern struct test       *comp_jn_vars();
   extern struct test       *comp_pn_vars();
   extern struct var_info   *find_variable();
   extern struct test       *check_test();
   
/****************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS  */
/****************************************/

   static struct expr_info  *expr_tests;
   static int                DeftemplatePattern;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct funtab     *PTR_KTAND; 
   extern struct funtab     *PTR_KTOR; 
   
/************************************/
/* RULE_ANALYSIS:  Analyzes a rule. */
/************************************/
rule_analysis(rule)
  struct node *rule;
  {
   variable_analysis(rule);
   if (check_variables(rule) == TRUE) return(TRUE);
   build_network_tests(rule);
   return(FALSE);
  }

/************************************/
/* BUILD_NETWORK_TESTS:              */
/************************************/
static int build_network_tests(rule)
  struct node *rule;
  {
   int pattern, element;
   struct test *top_jn;
   struct node *field_ptr;
   struct field_tests rtn_expr;
   struct pn_test *new_fs, *last_fs, *top_fs;
   struct expr_info *next_pat, *last_pat = NULL, *top_pat = NULL;
   struct test *top_test, *last_test, *new_test;
   
   flush_expr_list();
   
   if (rule->type != PATTERN)
     {
      clips_system_error(101);
      cl_exit(4);
     }
     
   pattern = 1;
   while (rule != NULL)
     {
      DeftemplatePattern = PatternHasTemplate(pattern);
      
      next_pat = get_struct(expr_info);
      next_pat->pattern = pattern;
      next_pat->state = rule->state;
      next_pat->next = NULL;
      next_pat->otest = NULL;
      next_pat->pntl = NULL;
      next_pat->jnt = NULL;
      
      /*==============================*/
      /* Get the pattern information. */
      /*==============================*/
      
      element = 1;
      top_jn = NULL;
      top_fs = last_fs = NULL;
      field_ptr = rule->right;
      while (field_ptr != NULL)
        {
         field_conversion(field_ptr,pattern,element,&rtn_expr);
         
         new_fs = get_struct(pn_test);
         new_fs->element = element;
         if ((field_ptr->type == SINGLE) || (field_ptr->type == BWORD))
           { new_fs->type = SINGLE; }
         else if ((field_ptr->type == MULTIPLE) || 
                  (field_ptr->type == BWORDS))
           { new_fs->type = MULTIPLE; }
         else
           { 
            clips_system_error(102);
            cl_exit(4);
           }
         new_fs->pnt = rtn_expr.pnt;
         new_fs->next = NULL;
         
         if (last_fs == NULL)
           { top_fs = new_fs; }
         else
           { last_fs->next = new_fs; }
         last_fs = new_fs;
         
         
         top_jn = combine_expressions(top_jn,rtn_expr.jnt);
         
         field_ptr = field_ptr->right;
         element++;
        }
      
      /*==============================*/
      /* Get any test information. */
      /*==============================*/  
      
      top_test = last_test = NULL;
      rule = rule->bottom;
      while ((rule != NULL) ? (rule->type == PAT_TEST) : FALSE)
        {
         new_test = rule->expression;
         rule->expression = NULL;
         rpl_var_gtvar(new_test,pattern,element,0);
         if (last_test == NULL)
           { top_test = new_test; }
         else
           { last_test->next_arg = new_test; }
           
         last_test = new_test;
         rule = rule->bottom;
        } 
        
      if (top_test != NULL)
        {
         if (top_test->next_arg != NULL)
           {
            new_test = gen_and();
            new_test->arg_list = top_test;
            top_test = new_test;
           }
         
         if (next_pat->state == 'n')
           { next_pat->otest = top_test; }
         else
           { top_jn = combine_expressions(top_jn,top_test); }
        }   
        
      next_pat->jnt = top_jn;
      next_pat->pntl = top_fs;
      
      if (last_pat == NULL)
        { top_pat = next_pat; }
      else 
        { last_pat->next = next_pat; }
      last_pat = next_pat;
      
#if ANALYSIS_SWITCH 
     
      cl_print("wdialog","Pattern ");
      print_long_int("wdialog",(long int) pattern);
      cl_print("wdialog","\n");
      cl_print("wdialog"," Join test: ");
      pp_test(next_pat->jnt,"wdialog");
      cl_print("wdialog","\n");
      cl_print("wdialog"," Other tests: ");
      pp_test(next_pat->otest,"wdialog");
      cl_print("wdialog","\n");
      while (top_fs != NULL)
        {
         cl_print("wdialog"," Field Pattern Test ");
         print_long_int("wdialog",(long int) top_fs->element);
         cl_print("wdialog",": ");
         pp_test(top_fs->pnt,"wdialog");
         cl_print("wdialog","\n");
         top_fs = top_fs->next;
        }

#endif
      
      pattern++;
     }
   
   expr_tests = top_pat;
  }
   
/*******************************************************************/
/* FIELD_CONVERSION: Generates join and pattern tests for a field. */
/*******************************************************************/
static int field_conversion(pat_field,pattern,element,rtn_vals)
  struct node *pat_field;
  int pattern, element;
   struct field_tests *rtn_vals;
  {
   struct field_tests test_vals;
   int tests_in_pn = TRUE;
   struct node *pat_ptr;
   struct test *top_pn, *top_jn, *last_pn, *last_jn, *new_node;
   struct test *bind_test;
   
   if (pat_field == NULL)
     { 
      clips_system_error(103);
      cl_exit(4);
     }
   
   /*=================================================*/
   /* Determine if constant testing must be performed */
   /* in the join network. Only possible when a field */
   /* contains an or ('|') and references are made to */
   /* variables outside the pattern.                  */
   /*=================================================*/
   
   if (pat_field->bottom != NULL)
     { 
      if (pat_field->bottom->bottom != NULL)
        { tests_in_pn = all_in_pattern(pat_field->bottom,pattern,element); }
     }
   
   /*======================================*/
   /* Extract pattern network expressions. */
   /*======================================*/
   
   top_pn = last_pn = NULL;
   top_jn = last_jn = NULL;
    
   pat_ptr = pat_field->bottom;
   while (pat_ptr != NULL)
     {
      extract_ands(pat_ptr,pattern,element,tests_in_pn,&test_vals);
      
      if (test_vals.pnt != NULL)
        {
         if (last_pn == NULL)
           { top_pn = test_vals.pnt; }
         else
           { last_pn->next_arg = test_vals.pnt; }
         last_pn = test_vals.pnt;
        }
        
      if (test_vals.jnt != NULL)
        {
         if (last_jn == NULL)
           { top_jn = test_vals.jnt; }
         else
           { last_jn->next_arg = test_vals.jnt; }
         last_jn = test_vals.jnt;
        }
        
      pat_ptr = pat_ptr->bottom;
     }
  
   if ((top_pn != NULL) ? (top_pn->next_arg != NULL) : FALSE)
     {
      new_node = gen_or();
      new_node->arg_list = top_pn;
      top_pn = new_node;
     }
     
   if ((top_jn != NULL) ? (top_jn->next_arg != NULL) : FALSE)
     {
      new_node = gen_or();
      new_node->arg_list = top_jn;
      top_jn = new_node;
     }
 
   /*=============================================*/
   /* Attach expression for the binding instance. */
   /*=============================================*/
   
   if ((pat_field->type == BWORDS) || (pat_field->type == BWORD))
     {
      if (find_variable(pat_field->type,pat_field->svalue,
                            pattern,pattern,element,INSIDE) != NULL)
        { 
         bind_test = comp_pn_vars(pat_field,pattern,element); 
         top_pn = combine_expressions(bind_test,top_pn);
        }
      else if (find_variable(pat_field->type,pat_field->svalue,
                             1,pattern,element,INSIDE) != NULL)
        { 
         bind_test = comp_jn_vars(pat_field,pattern,element,0); 
         top_jn = combine_expressions(bind_test,top_jn);
        }
     }
   
      
   rtn_vals->pnt = top_pn;
   rtn_vals->jnt = top_jn;
  }
  
/****************************************************************/
/* EXTRACT_PN_AND   */
/****************************************************************/
static int extract_ands(and_field,pattern,element,test_in_pn,test_ptr)
  struct node *and_field;
  int pattern, element, test_in_pn;
  struct field_tests *test_ptr;
  {
   struct test *new_node;
   struct test *new_pn, *new_jn;
   struct test *top_pn = NULL, *top_jn = NULL;
   struct test *last_pn = NULL, *last_jn = NULL;
   
   /*=========================================================*/
   /* Loop through the list of field constraints and generate */
   /* test for those that can have pattern network tests.     */
   /*=========================================================*/
   
   while (and_field != NULL)
     {
      new_pn = NULL;
      new_jn = NULL;
      if ((and_field->type == STRING) || 
          (and_field->type == WORD) ||
          (and_field->type == NUMBER))
        { 
         if (test_in_pn == TRUE)
           { new_pn = gen_pn_constant(and_field); }
         else
           { new_jn = gen_jn_constant(and_field,pattern,element); }
        }
      else if (and_field->type == COAMP)
        {
         if ((test_in_pn == TRUE) &&
             (check_test(and_field->expression,pattern,pattern,
                              element + 1,INSIDE) == NULL))
           { new_pn = gen_pn_colon(and_field,pattern,element); }
         else
           { new_jn = gen_jn_colon(and_field,pattern,element,DeftemplatePattern); }
        }
      else if (and_field->type == FCALL)
        {
         if ((test_in_pn == TRUE) &&
             (check_test(and_field->expression,pattern,pattern,
                              element + 1,INSIDE) == NULL))
           { new_pn = gen_pn_eq(and_field,pattern,element); }
         else
           { new_jn = gen_jn_eq(and_field,pattern,element,DeftemplatePattern); }
        }
      else if ((and_field->type == BWORD) || (and_field->type == BWORDS))
        {
         if ((test_in_pn == TRUE) &&
             (find_variable(and_field->type,and_field->svalue,
                            pattern,pattern,element+1,INSIDE) != NULL))
           { new_pn = comp_pn_vars(and_field,pattern,element); }
         else
           { new_jn = comp_jn_vars(and_field,pattern,element,DeftemplatePattern); }
        }
        
      if (new_pn != NULL)
        {
         if (last_pn == NULL)
           { top_pn = new_pn; }
         else
           { last_pn->next_arg = new_pn; }
         last_pn = new_pn;
        }
        
      if (new_jn != NULL)
        {
         if (last_jn == NULL)
           { top_jn = new_jn; }
         else
           { last_jn->next_arg = new_jn; }
         last_jn = new_jn;
        }
        
      and_field = and_field->right;
     }
     
   /*===================================================*/
   /* Determine if an and function call must be wrapped */
   /* around the arguments.                             */
   /*===================================================*/
   
   if ((top_pn != NULL) ? (top_pn->next_arg != NULL) : FALSE)
     {
      new_node = gen_and();
      new_node->arg_list = top_pn;
      top_pn = new_node;
     }
     
   if ((top_jn != NULL) ? (top_jn->next_arg != NULL) : FALSE)
     {
      new_node = gen_and();
      new_node->arg_list = top_jn;
      top_jn = new_node;
     }
     
   test_ptr->pnt = top_pn;
   test_ptr->jnt = top_jn;
  }
  
/****************************************************************/
/* ALL_IN_PATTERN: Determines if all of the variable references */
/*   in a field can be referenced within the pattern.           */
/****************************************************************/
static int all_in_pattern(or_field,pattern,element)
  struct node *or_field;
  int pattern, element;
  {
   struct node *and_field;
   
   while (or_field != NULL)
     {
      and_field = or_field;
      while (and_field != NULL)
        {
         if (and_field->type == BWORD)
           {
            if (find_variable(and_field->type,and_field->svalue,
                           pattern,pattern,element+1,INSIDE) == NULL)
              { return(FALSE); }
           }
         else if ((and_field->type == COAMP) || 
                  (and_field->type == FCALL))
           {
            if (check_test(and_field->expression,pattern,pattern,
                              element + 1,INSIDE) != NULL)
              { return(FALSE); }
           }
           
         and_field = and_field->right;
        }
      or_field = or_field->bottom;
     }
     
   return(TRUE);
  }

/****************************************************/
/* COMBINE_EXPRESSIONS       */
/****************************************************/
static struct test *combine_expressions(exp1,exp2)
  struct test *exp1, *exp2;
  {
   struct test *chase_ptr;
   
   /*========*/
   /* CASE 1 */
   /*========*/
   
   if (exp1 == NULL)
     { return(exp2); }
     
   /*========*/
   /* CASE 2 */
   /*========*/
   
   if (exp2 == NULL)
     { return(exp1); }
     
   /*========*/
   /* CASE 3 */
   /*========*/
   
   if ((exp1->val.fun_ptr == PTR_KTAND) && 
       (exp2->val.fun_ptr != PTR_KTAND))
     {
      chase_ptr = exp1->arg_list;
      if (chase_ptr == NULL)
        { 
         rtn_struct(test,exp1);
         return(exp2);
        }
        
      while (chase_ptr->next_arg != NULL) 
        { chase_ptr = chase_ptr->next_arg; }
      
      chase_ptr->next_arg = exp2;
      return(exp1);
     }
     
   /*========*/
   /* CASE 4 */
   /*========*/
   
   if ((exp1->val.fun_ptr != PTR_KTAND) && 
       (exp2->val.fun_ptr == PTR_KTAND))
     {
      chase_ptr = exp2->arg_list;
      if (chase_ptr == NULL)
        { 
         rtn_struct(test,exp2);
         return(exp1);
        }
        
      exp2->arg_list = exp1;
      exp1->next_arg = chase_ptr;
      
      return(exp2);
     }
     
   /*========*/
   /* CASE 5 */
   /*========*/
     
   if ((exp1->val.fun_ptr == PTR_KTAND) && 
       (exp2->val.fun_ptr == PTR_KTAND))
     {
      chase_ptr = exp1->arg_list;
      if (chase_ptr == NULL)
        { 
         rtn_struct(test,exp1);
         return(exp2);
        }
        
      while (chase_ptr->next_arg != NULL) 
        { chase_ptr = chase_ptr->next_arg; }
      
      chase_ptr->next_arg = exp2->arg_list;
      rtn_struct(test,exp2);
      
      return(exp1);
     }
  
   /*========*/
   /* CASE 6 */
   /*========*/
   
   chase_ptr = gen_and();
   chase_ptr->arg_list = exp1;
   exp1->next_arg = exp2;
   return(chase_ptr);
  }
  
/******************************/
/* COUNT_JOINS:               */
/******************************/
int count_joins()
  {
   struct expr_info *chase_ptr;
   int count = 0;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(count);
  }
  
/******************************/
/* GET_JOIN_LOGIC:            */
/******************************/
char get_join_logic(join_number)
  int join_number;
  {
   struct expr_info *chase_ptr;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == join_number) 
        {
         if (chase_ptr->state == 'o') return('+');
         else if (chase_ptr->state == 'n') return('-');
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return('?');
  }
 
/******************************/
/* GET_JOIN_EXPR:            */
/******************************/
struct test *get_join_expr(join_number)
  int join_number;
  {
   struct expr_info *chase_ptr;
   struct test *temp_jnt;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == join_number) 
        {
         temp_jnt = chase_ptr->jnt;
         chase_ptr->jnt = NULL;
         return(temp_jnt);
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(NULL);
  }
  
/******************************/
/* GET_NOT_EXPR:            */
/******************************/
struct test *get_not_expr(join_number)
  int join_number;
  {
   struct expr_info *chase_ptr;
   struct test *temp_jnt;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == join_number) 
        {
         temp_jnt = chase_ptr->otest;
         chase_ptr->otest = NULL;
         return(temp_jnt);
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(NULL);
  }
  
/******************************/
/* GET_NODE_TYPE:            */
/******************************/
get_node_type(pattern,element)
  int pattern, element;
  {
   struct expr_info *chase_ptr;
   struct pn_test *field_ptr;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == pattern) 
        {
         field_ptr = chase_ptr->pntl;
         while (field_ptr != NULL)
           {
            if (field_ptr->element == element)
              { return(field_ptr->type); }
            field_ptr = field_ptr->next;
           }
           
         return(STOP);
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(KUNKNOWN);
  }

/******************************/
/* GET_ELEM_COUNT:            */
/******************************/
get_elem_count(pattern)
  int pattern;
  {
   struct expr_info *chase_ptr;
   struct pn_test *field_ptr;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == pattern) 
        {
         field_ptr = chase_ptr->pntl;
         count = 0;
         while (field_ptr != NULL)
           {
            count++;
            field_ptr = field_ptr->next;
           }
           
         return(count);
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(NULL);
  }
  
/******************************/
/* GET_PN_EXPR:            */
/******************************/
struct test *get_pn_expr(pattern,element)
  int pattern, element;
  {
   struct expr_info *chase_ptr;
   struct pn_test *field_ptr;
   struct test *temp_test;
   int count = 1;
   
   chase_ptr = expr_tests;
   while (chase_ptr != NULL)
     { 
      if (count == pattern) 
        {
         field_ptr = chase_ptr->pntl;
         while (field_ptr != NULL)
           {
            if (field_ptr->element == element)
              { 
               temp_test = field_ptr->pnt;
               field_ptr->pnt = NULL;
               return(temp_test);
              }
            field_ptr = field_ptr->next;
           }
           
         return(NULL);
        }
      chase_ptr = chase_ptr->next;
      count++;
     }
     
   return(NULL);
  }
  
/***********************************/
/* FLUSH_EXPR_LIST:                */
/***********************************/
flush_expr_list()
  {
   struct expr_info *temp_expr;
   struct pn_test *temp_pn, *pnt_ptr;
   
   while (expr_tests != NULL)
     {
      temp_expr = expr_tests->next;
      
      if (expr_tests->jnt != NULL) returntests(expr_tests->jnt);
      if (expr_tests->otest != NULL) returntests(expr_tests->otest);
      
      pnt_ptr = expr_tests->pntl;
      while (pnt_ptr != NULL)
        {
         temp_pn = pnt_ptr->next;
         if (pnt_ptr->pnt != NULL) returntests(pnt_ptr->pnt);
         rtn_struct(pn_test,pnt_ptr);
         pnt_ptr = temp_pn;
        }
      
      rtn_struct(expr_info,expr_tests);
      expr_tests = temp_expr;
     }
  }
  
  
/****************************************************************/
/* PRINT_NODES:           */
/****************************************************************/
print_nodes(node_ptr)
  struct node *node_ptr;
  {
   if (node_ptr == NULL) return (0);

   if ( (node_ptr->type != PAT_NOT) &&
        (node_ptr->type != PAT_AND) &&
        (node_ptr->type != PAT_OR) &&
        (node_ptr->type != PATTERN) && 
        (node_ptr->type != PAT_TEST) )
     { return (0); }

   while (node_ptr != NULL)
     {
      cl_print("wdialog","(");
      if (node_ptr->type == PAT_AND) cl_print("wdialog","and");
      else if (node_ptr->type == PAT_OR) cl_print("wdialog","or");
      else if (node_ptr->type == PAT_NOT) cl_print("wdialog","not");
      else if (node_ptr->type == PAT_TEST) cl_print("wdialog","test");
      else if (node_ptr->type == PATTERN) cl_print("wdialog","pattern");
      
      if (node_ptr->type == PAT_TEST)
        { pp_test(node_ptr->expression,"wdialog"); }
      print_nodes(node_ptr->right);
      cl_print("wdialog",")\n");
      node_ptr = node_ptr->bottom;
     }
  
   return(0);
  }
  
#endif
