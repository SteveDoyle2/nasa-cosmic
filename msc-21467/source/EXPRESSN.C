/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                  EXPRESSION MODULE                  */
   /*******************************************************/

#include "setup.h"

#if ! RUN_TIME

#include <stdio.h>

#include "constant.h"
#include "scanner.h"
#include "expressn.h"
#include "clipsmem.h"

/****************************************/
/* LOCAL INTERNAL FUNCTIONS DEFINITIONS */
/****************************************/
   
   struct test            *bind_parse();
   struct test            *if_parse();
   struct test            *retract_parse();
   struct test            *while_parse();
   
/*****************************************/
/* GLOBAL INTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/
   
   struct test            *fctn0_parse();
   struct test            *fctn1_parse();
   struct test            *fctn2_parse();
   struct test            *fctn_arg_parse();
   struct test            *group_actions();
   int                     returntests();
   int                     pp_test();
   int                     test_install();
   int                     test_deinstall();
   struct test            *copy_tests();
   struct test            *collect_arguments();
   struct test            *multi_arg_parse();
   struct test            *check_arg_list_parse();
   struct test            *eq_parse();
   struct test            *not_parse();
   struct test            *GetAssertArgument();
   struct test            *GetRhsPattern();
   struct test            *assert_parse();
   struct test            *BuildRhsAssert();
   
/*****************************************/
/* GLOBAL EXTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/
   
   extern struct funtab   *find_function();
   extern int              slow_assert();
   extern char            *symbol_string();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct construct    *construct_list = NULL;
   static struct func_parser  *func_parser_list = NULL;

/*****************************************************************/
/* INIT_EXPRN_PSR:                                               */
/*****************************************************************/
init_exprn_psr()
  {
   add_func_parser("assert",assert_parse);
#if (! BLOAD_ONLY)
   add_func_parser("bind",bind_parse);
#endif
   add_func_parser("if",if_parse);
   add_func_parser("while",while_parse);
   add_func_parser("retract",retract_parse);
  }
   
/*****************************************************************/
/* GROUP_ACTIONS: Returns progn test with actions if successful. */
/*   Returns Null if error detected.                             */
/*****************************************************************/
struct test *group_actions(in_file,rtn_tkn,read_first_paren)
  char *in_file;
  struct token *rtn_tkn;
  int read_first_paren;
  {
   struct test *top, *next_one, *last_one;
   struct token act_tkn;
   
   /*========================================================*/
   /* Continue until all appropriate commands are processed. */
   /*========================================================*/

   top = get_struct(test);
   top->type = FCALL;        
   top->val.fun_ptr = find_function("progn");
   top->next_arg = NULL;
   top->arg_list = NULL;

   last_one = NULL;  

   while (TRUE)
     {
      if (read_first_paren)
        { 
         gettoken(in_file,&act_tkn);
         
         if (act_tkn.token != LPAREN)
           {
            copy_tokens(rtn_tkn,&act_tkn);
            return(top);
           }
        }
      else
        { read_first_paren = 1; }
        
      next_one = fctn1_parse(in_file);

      if (next_one == NULL)
        {
         rtn_tkn->token = KUNKNOWN;
         returntests(top); 
         return(NULL);
        }        
         
      if (last_one == NULL)
        { top->arg_list = next_one; }
      else
        { last_one->next_arg = next_one; }

      last_one = next_one;

      pp_cr_and_indent();
     }
  }

  
/****************************************************************/
/* fctn0_parse: Parses a function. Assumes that none of the */
/*   function has been parsed yet.                              */
/****************************************************************/
struct test *fctn0_parse(in_file)
  char *in_file;
  {
   struct token act_tkn;
   struct test *top;
   
   /*=================================*/
   /* All functions begin with a '('. */
   /*=================================*/
      
   gettoken(in_file,&act_tkn);
   if (act_tkn.token != LPAREN) 
     { 
      cl_print("werror","\nFunction expressions must begin with a '('\n");
      return(NULL);
     }
       
   /*=================================*/
   /* Parse the rest of the function. */
   /*=================================*/
        
   top = fctn1_parse(in_file);
   return(top);
  }

/********************************************************/
/* fctn1_parse: Parses a function. Assumes that the */
/*   opening right parenthesis has already been parsed. */                             
/********************************************************/
struct test *fctn1_parse(in_file)
  char *in_file;
  {
   struct token act_tkn;
   struct test *top;
     
   /*========================*/
   /* Get the function name. */
   /*========================*/
        
   gettoken(in_file,&act_tkn);
   if (act_tkn.token != WORD)  
     {
      cl_print("werror","\nA function name must be a symbol or");
      cl_print("werror"," arithmetic operator\n");
      return(NULL);
     }
       
   /*=================================*/
   /* Parse the rest of the function. */
   /*=================================*/
        
   top = fctn2_parse(in_file,act_tkn.tknword);
   return(top);
  }

/********************************************************/
/* fctn2_parse: Parses a function. Assumes that the */
/*   opening right parenthesis and function name have   */
/*   already been parsed.                               */
/********************************************************/
struct test *fctn2_parse(in_file,name)
  char *in_file, *name;
  {
   struct funtab *ft_ptr;
   struct test *top;
   struct func_parser *fp_ptr;

   /*================================*/
   /* Has the function been defined? */
   /*================================*/

   if ((ft_ptr = find_function(name)) == NULL)
     {       
      cl_print("werror","\nMissing function declaration for ");
      cl_print("werror",name);
      cl_print("werror","\n");
      return(NULL); 
     }
     
   /*=============================*/
   /* Define top level structure. */
   /*=============================*/
   
   top = get_struct(test);
   top->type = FCALL;        
   top->val.fun_ptr = ft_ptr;
   top->next_arg = NULL;
   top->arg_list = NULL;

   /*=======================================================*/
   /* Check to see if function has its own parsing routine. */
   /*=======================================================*/

   fp_ptr = func_parser_list;
   while (fp_ptr != NULL)
     {
      if (strcmp(name,fp_ptr->name) == 0)
        { 
         top = (*fp_ptr->ip)(top,in_file);
         return(top);
        }
      fp_ptr = fp_ptr->next;
     }
   
   /*========================================*/
   /* Default parsing routine for functions. */
   /*========================================*/

   return(collect_arguments(top,in_file));
  }
  
/********************************************************/
/* collect_arguments:                                   */
/********************************************************/
struct test *collect_arguments(top,in_file)
  struct test *top;
  char *in_file;
  { 
   int error_flag;
   struct test *last_one, *next_one;
   
   /*========================================*/
   /* Default parsing routine for functions. */
   /*========================================*/

   last_one = NULL;

   while (TRUE)
     {
      save_pp_buffer(" ");     
      
      error_flag = FALSE;
      next_one = fctn_arg_parse(in_file,&error_flag);

      if (error_flag == TRUE)
        {
         returntests(top); 
         return(NULL);
        }
         
      if (next_one == NULL)
        {
         pp_backup();
         pp_backup();
         save_pp_buffer(")"); 
         return(top);
        }

      if (last_one == NULL)
        { top->arg_list = next_one; }
      else
        { last_one->next_arg = next_one; }

      last_one = next_one;
     }
  } 


/***********************************************************/
/* fctn_arg_parse                    */
/***********************************************************/
struct test *fctn_arg_parse(in_file,error_flag)
  char *in_file;
  int *error_flag;
  {
   struct test *top;
   struct token arg_tkn;

   gettoken(in_file,&arg_tkn);
   
   /*============================*/
   /* ')' counts as no argument. */
   /*============================*/
   
   if (arg_tkn.token == RPAREN)
     { return(NULL); }
        
   /*================================*/
   /* Parse constants and variables. */
   /*================================*/
   
   if ((arg_tkn.token == BWORD) || (arg_tkn.token == BWORDS) ||
       (arg_tkn.token == WORD) || (arg_tkn.token == STRING))
     {
      top = get_struct(test);
      top->type = arg_tkn.token;     
      top->val.hvalue = arg_tkn.hashword;
      top->next_arg = NULL;
      top->arg_list = NULL;
      return(top);
     }

   if (arg_tkn.token == NUMBER)
     {
      top = get_struct(test);
      top->type = NUMBER;    
      top->val.fvalue = arg_tkn.tknnumber;
      top->next_arg = NULL;
      top->arg_list = NULL;
      return(top);
     }

   /*======================*/
   /* Parse function call. */
   /*======================*/

   if (arg_tkn.token != LPAREN)
     {
      cl_print("werror","\nAn argument in a function call must be a ");
      cl_print("werror","constant, variable, or expression\n");
      *error_flag = TRUE;
      return(NULL);
     }
     
   top = fctn1_parse(in_file);
   if (top == NULL) *error_flag = TRUE;
   return(top);
  }

/***************************************************************/
/* assert_parse: Purpose is to parse the assert statement. The */
/*   parse of the statement is the return value.               */
/*   Assert syntax:                                            */
/*     (assert (<field>+))                                     */
/***************************************************************/
struct test *assert_parse(top,in_file)
  struct test *top;
  char *in_file;
  {
   int error;
   struct test *rv;
   
   save_pp_buffer(" ");
   inc_indent_depth(8);
   rv = BuildRhsAssert(top,in_file,TRUE,&error);
   dec_indent_depth(8);
   return(rv);
  }
  
/***************************************************************/
/* BuildRhsAssert:                                     */
/***************************************************************/
struct test *BuildRhsAssert(top,in_file,readFirstParen,error)
  struct test *top;
  char *in_file;
  int readFirstParen, *error;
  {
   struct token act_tkn;
   struct test *last_one, *next_one, *assert_list, *stub;
   int multi = FALSE;

   *error = FALSE;

   last_one = assert_list = NULL;
   multi = FALSE;
   while((next_one = GetRhsPattern(in_file,&act_tkn,&multi,
                                   error,TRUE,readFirstParen)) != NULL)
     {
      pp_cr_and_indent();
        
      stub = get_struct(test);
      stub->type = FCALL;
      if (multi)
        { stub->val.fun_ptr = find_function("slow_assert"); }
      else
        { stub->val.fun_ptr = find_function("assert"); }
      stub->arg_list = next_one;
      stub->next_arg = NULL;
      next_one = stub;
   
      if (last_one == NULL)
        { assert_list = next_one; }
      else
        { last_one->next_arg = next_one; }
      last_one = next_one;
      
      multi = FALSE;
      readFirstParen = TRUE;
     }
     
   if (act_tkn.token == RPAREN) 
     {
      pp_backup();
      pp_backup();
      save_pp_buffer(")");
     }
     
   if (*error)
     {
      returntests(top);
      returntests(assert_list);
      return(NULL);
     }
   
   if (assert_list == NULL)
     {
      cl_print("werror","\nAt least one fact must be asserted\n");
      returntests(top);
      return(NULL);
     }
   
   if (assert_list->next_arg != NULL)
     {
      top->val.fun_ptr = find_function("progn");
      top->arg_list = assert_list;
     }
   else
     { 
      top->val.fun_ptr = assert_list->val.fun_ptr;
      top->arg_list = assert_list->arg_list;
      assert_list->arg_list = NULL;
      returntests(assert_list);
     }
   
   return(top);
  }
  
/***********************************/
/* GetAssertArgument:              */
/***********************************/
struct test *GetAssertArgument(in_file,act_tkn,multi,error)
  char *in_file;
  struct token *act_tkn;
  int *multi, *error;
  {
   struct test *next_one;
   
   gettoken(in_file,act_tkn);
   if (act_tkn->token == RPAREN)
     { return(NULL); }
   
   if ((act_tkn->token == WORD) ? (strcmp(act_tkn->tknword,"=") == 0) : FALSE)
     {
      next_one = fctn0_parse(in_file);
      if (next_one != NULL)
        {
         if (next_one->val.fun_ptr->fun_type == 'm')
           { *multi = TRUE; }
        } 
      else
        { *error = TRUE; }
     }
   else if ((act_tkn->token == WORD) || (act_tkn->token == STRING) ||
           (act_tkn->token == BWORD) || (act_tkn->token == BWORDS) ||
           (act_tkn->token == NUMBER))
     {
      next_one = get_struct(test);
      next_one->arg_list = NULL;
      next_one->next_arg = NULL;
         
      next_one->type = act_tkn->token;
      if (act_tkn->token == NUMBER)
        { next_one->val.fvalue = act_tkn->tknnumber; }
      else
        { next_one->val.hvalue = act_tkn->hashword; }
         
      if (act_tkn->token == BWORDS)
        { *multi = TRUE; }
     } 
   else
     { 
      *error = TRUE;
      next_one = NULL;
     }
     
   return(next_one);
  }

/*************************************************************/
/* retract_parse: Purpose is to parse the retract statement. */
/*   The parse of the statement is the return value.         */
/*   Syntax: (retract <?-var>+ )                             */
/*************************************************************/
static struct test *retract_parse(top,in_file)
  struct test *top;
  char *in_file;
  {
   struct token act_tkn;
   struct test *last_one, *next_one;
   int error_flag = FALSE;
   int arg_num = 1;

   last_one = NULL;

   save_pp_buffer(" ");

   gettoken(in_file,&act_tkn);
   while (act_tkn.token != RPAREN)
     {
      if (act_tkn.token == NUMBER)
        {
         next_one = get_struct(test);
         next_one->type = NUMBER;  
         next_one->val.fvalue = act_tkn.tknnumber;
         next_one->next_arg = NULL;
         next_one->arg_list = NULL;
        }
      else if (act_tkn.token == BWORD)
        {
         next_one = get_struct(test);
         next_one->type = POINTER;  
         next_one->val.hvalue = act_tkn.hashword;
         next_one->next_arg = NULL;
         next_one->arg_list = NULL;
        }
      else
        { error_flag = TRUE; }

      if (error_flag == TRUE)
        {
         exp_type_error("retract",arg_num,"number or variable");
         returntests(top); 
         return(NULL);
        }

      if (last_one == NULL)
        { top->arg_list = next_one; }
      else
        { last_one->next_arg = next_one; }

      last_one = next_one;

      gettoken(in_file,&act_tkn);

      if (act_tkn.token != RPAREN) 
        {
         pp_backup();
         save_pp_buffer(" ");
         save_pp_buffer(act_tkn.print_rep);
        }
        
      arg_num++;
     }
    if (top->arg_list == NULL)
     {
      exp_num_error("retract",AT_LEAST,1);
      returntests(top);
      return(NULL);
     }

   return(top);
  }

/*********************************************************/
/* while_parse: purpose is to parse the while statement. */
/*   The parse of the statement is the return value.     */
/*   Syntax: (while <expression> <action>+)              */
/*********************************************************/
static struct test *while_parse(parse,infile)
  struct test *parse;
  char *infile;
  {
   struct token act_tkn;
   int read_first_paren;
   
   /*===============================*/
   /* Process the while expression. */
   /*===============================*/

   save_pp_buffer(" ");   

   parse->arg_list = fctn0_parse(infile);
   if (parse->arg_list == NULL) 
     { 
      returntests(parse);
      return(NULL);
     }

   /*====================================*/
   /* Process the do keyword if present. */
   /*====================================*/
   
   gettoken(infile,&act_tkn);
   if ((act_tkn.token == WORD) && (strcmp(act_tkn.tknword,"do") == 0))
     { 
      read_first_paren = TRUE; 
      pp_backup(); 
      save_pp_buffer(" "); 
      save_pp_buffer(act_tkn.print_rep); 
      inc_indent_depth(3);
      pp_cr_and_indent();
     }
   else if (act_tkn.token == LPAREN)
     { 
      read_first_paren = FALSE;
      pp_backup(); 
      inc_indent_depth(3);
      pp_cr_and_indent();
      save_pp_buffer(act_tkn.print_rep); 
     }
   else  
     {
      cl_print("werror","\nKeyword do or '(' expected in while construct\n");
      returntests(parse);
      return(NULL); 
     }
     
   /*============================*/
   /* Process the while actions. */
   /*============================*/

   parse->arg_list->next_arg = 
                 group_actions(infile,&act_tkn,read_first_paren);

   if (parse->arg_list->next_arg == NULL) 
     {
      returntests(parse); 
      return(NULL);
     } 
     
   pp_backup();
   pp_backup();
   save_pp_buffer(act_tkn.print_rep);

   /*======================================================*/
   /* Check for the closing right parenthesis of the while. */
   /*======================================================*/

   if (act_tkn.token != RPAREN)
     {
      cl_print("werror","\nExpected ')' to finish while ");
      cl_print("werror","or '(' to begin new action\n");
      returntests(parse);
      return(NULL);
     }

   dec_indent_depth(3);
   
   return(parse);
  }

/*********************************************************/
/* if_parse: purpose is to parse the if statement.  The  */
/*   parse of the statement is the return value.         */
/*   Syntax: (if <expression> then <action>+             */
/*               [ else <action>+ ] )                    */
/*********************************************************/
static struct test *if_parse(top,infile)
  struct test *top;
  char *infile;
  {   
   struct token act_tkn;
   
   /*============================*/
   /* Process the if expression. */
   /*============================*/

   save_pp_buffer(" "); 

   top->arg_list = fctn0_parse(infile);
   if (top->arg_list == NULL) 
     {
      returntests(top);
      return(NULL);
     }

   /*========================================*/
   /* Keyword 'then' must follow expression. */
   /*========================================*/

   inc_indent_depth(3);
   pp_cr_and_indent();

   gettoken(infile,&act_tkn);
   if ((act_tkn.token != WORD) || (strcmp(act_tkn.tknword,"then") != 0))
     { 
      cl_print("werror","\nKeyword then expected in if construct\n");
      returntests(top);
      return(NULL);
     }

   /*==============================*/
   /* Process the if then actions. */
   /*==============================*/

   pp_cr_and_indent();

   top->arg_list->next_arg = group_actions(infile,&act_tkn,1);
   if (top->arg_list->next_arg == NULL) 
     {
      returntests(top); 
      return(NULL); 
     }

   /*===========================================*/
   /* A ')' signals an if then without an else. */
   /*===========================================*/

   if (act_tkn.token == RPAREN) 
     {
      dec_indent_depth(3);
      pp_backup();
      pp_backup();
      save_pp_buffer(act_tkn.print_rep);
      return(top);
     }

   /*=============================================*/
   /* Keyword 'else' must follow if then actions. */
   /*=============================================*/

   if ((act_tkn.token != WORD) || (strcmp(act_tkn.tknword,"else") != 0))
     { 
      cl_print("werror","\nKeyword else or '(' expected in if construct\n");
      returntests(top);
      return(NULL);
     }

   pp_cr_and_indent();

   /*==============================*/
   /* Process the if else actions. */
   /*==============================*/

   top->arg_list->next_arg->next_arg = group_actions(infile,&act_tkn,TRUE);
   if (top->arg_list->next_arg->next_arg == NULL) 
    {
     returntests(top); 
     return(NULL); 
    } 
    
   pp_backup();
   pp_backup();
   save_pp_buffer(act_tkn.print_rep);

   /*======================================================*/
   /* Check for the closing right parenthesis of the if. */
   /*======================================================*/

   if (act_tkn.token != RPAREN)
     {
      cl_print("werror","\nExpected ')' to finish if construct ");
      cl_print("werror","or '(' to begin new action\n");
      returntests(top);
      return(NULL);
     }

   dec_indent_depth(3);  
   return(top);
  }
  
#if (! BLOAD_ONLY)
/***********************************************************/
/* bind_parse: purpose is to parse the bind statement. The */
/*   parse of the statement is the return value.           */
/*   Syntax:  (bind ?var <expression>)                     */
/***********************************************************/
static struct test *bind_parse(top,infile)
  struct test *top;
  char *infile;
  {
   struct token act_tkn;
   struct draw *var_name;
   struct test *set_ptr;
   int error_flag, var_type, set_type;

   save_pp_buffer(" "); 

   /*=============================================*/
   /* Next token must be the name of the variable */
   /* to be bound.                                */
   /*=============================================*/ 

   gettoken(infile,&act_tkn);
   if (act_tkn.token == BWORD)
     { var_type = BWORD; }
   else if (act_tkn.token == BWORDS)
     { var_type = BWORDS; }
   else
     {
      cl_print("werror","\nExpected binding variable after bind\n");
      returntests(top);
      return(NULL);
     }

   /*==============================*/
   /* Process the bind expression. */
   /*==============================*/

   save_pp_buffer(" "); 

   top->arg_list = get_struct(test);
   top->arg_list->type = WORD;
   top->arg_list->val.hvalue = act_tkn.hashword;
   top->arg_list->next_arg = NULL;
   top->arg_list->arg_list = NULL;
   var_name = act_tkn.hashword;

   error_flag = FALSE;
   top->arg_list->next_arg = fctn_arg_parse(infile,&error_flag);
   if (error_flag == TRUE) 
     {
      returntests(top); 
      return(NULL);
     }
     
   if (top->arg_list->next_arg == NULL)
     {
      cl_print("werror","\nExpected expression after binding variable\n");
      returntests(top); 
      return(NULL);
     }
     
   /*=========================================================*/
   /* Determine type of value to which variable is to be set. */
   /*=========================================================*/

   set_ptr = top->arg_list->next_arg;
   if (set_ptr->type == FCALL)
     {
      if (set_ptr->val.fun_ptr->fun_type == 'm')
        { set_type = BWORDS; }
      else
        { set_type = BWORD; }
     }
   else if (set_ptr->type == BWORDS)
     { set_type = BWORDS; }
   else
     { set_type = BWORD; }
     
   if (set_type != var_type)
     {
      cl_print("werror","\nMismatch of bind variable and expression\n");
      returntests(top); 
      return(NULL);
     }
   
   /*===========================================*/
   /* Bind construct must be closed with a ')'. */
   /*===========================================*/

   gettoken(infile,&act_tkn);
   if (act_tkn.token != RPAREN)
     {  
      cl_print("werror","\nExpected a ')' to close the bind construct\n");
      returntests(top);
      return(NULL);
     }

   add_bind_name(var_type,var_name);
   
   return(top);
  }
#endif

/*******************************************************************/
/* RETURNTESTS:  Returns a multiply linked list of test structures */
/*   to the list of free tests .                                   */
/*******************************************************************/
returntests(waste)
  struct test *waste;
  {
   if (waste != NULL)
     {
      returntests(waste->arg_list);
      returntests(waste->next_arg);
      rtn_struct(test,waste);
     }
  }
  
/****************************************************/
/* pp_test: Pretty prints a test construct.         */
/****************************************************/
pp_test(test_ptr,fileid)
  struct test *test_ptr;
  char *fileid;
  {

   if (test_ptr == NULL) 
     { return(1); }

   while (test_ptr != NULL)
     {
      if (test_ptr->type == BWORD) 
        { 
         cl_print(fileid,"?");
         cl_print(fileid,symbol_string(test_ptr->val.hvalue));
        }
      else if (test_ptr->type == BWORDS)
        { 
         cl_print(fileid,"$?");
         cl_print(fileid,symbol_string(test_ptr->val.hvalue));
        }
      else if (test_ptr->type == NUMBER)
        { print_num(fileid,test_ptr->val.fvalue); }
      else if (test_ptr->type == INDEX)
        { print_num(fileid,(float) test_ptr->val.index); }
      else if (test_ptr->type == WORD)
        { cl_print(fileid,symbol_string(test_ptr->val.hvalue)); }
      else if (test_ptr->type == STRING)
        {
         cl_print(fileid,"\"");
         cl_print(fileid,symbol_string(test_ptr->val.hvalue)); 
         cl_print(fileid,"\"");
        }
      else if (test_ptr->type == FCALL)
        {
         cl_print(fileid,"(");
         cl_print(fileid,test_ptr->val.fun_ptr->fun_name);
         if (test_ptr->arg_list != NULL) { cl_print(fileid," "); }
         pp_test(test_ptr->arg_list,fileid);
         cl_print(fileid,")");
        }
      else
        {
         clips_system_error(1101);
         cl_exit(5);
        }

      test_ptr = test_ptr->next_arg;
      if (test_ptr != NULL) cl_print(fileid," ");
     }

   return(1);
  }
  
/******************************************************************/
/* TEST_INSTALL:  Increments all occurrences in the hash table of */
/*   symbols found in an expression composed of test structures.  */ 
/******************************************************************/
test_install(expression)
  struct test *expression;
  {
   if (expression == NULL) { return; }
   
   while (expression != NULL)
     {
      if ((expression->type == STRING) || (expression->type == WORD))
        { inc_symbol_count(expression->val.hvalue); }
      test_install(expression->arg_list);
      expression = expression->next_arg;
     }
  }

/********************************************************************/
/* TEST_DEINSTALL:  Increments all occurrences in the hash table of */
/*   symbols found in an expression composed of test structures.    */ 
/********************************************************************/
test_deinstall(expression)
  struct test *expression;
  {
   if (expression == NULL) { return; }
   
   while (expression != NULL)
     {
      if ((expression->type == STRING) || (expression->type == WORD))
        { dec_symbol_count(expression->val.hvalue); }
      test_deinstall(expression->arg_list);
      expression = expression->next_arg;
     }
  }

/*************************/
/* ADD_CONSTRUCT:        */
/*************************/
add_construct(name,func_ptr)
  char *name;
  int (*func_ptr)();
  {
   struct construct *c_ptr;

   c_ptr = get_struct(construct);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_construct\n");
      return;
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = construct_list;
   construct_list = c_ptr;
  }

/****************************/
/* REMOVE_CONSTRUCT:        */
/****************************/
remove_construct(name)
  char *name;
  {
   struct construct *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = construct_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { construct_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(construct,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }

/***************************/
/* VALID_CONSTRUCT:        */
/***************************/
valid_construct(name)
  char *name;
  {
   struct construct *c_ptr;
   
   c_ptr = construct_list;
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        { return(TRUE); }
      c_ptr = c_ptr->next;
     }
     
   return(FALSE);
  }
  
/**********************************************/
/* PARSE_CONSTRUCT:                           */
/* Return values:                             */
/*  -1 : Unable to find construct.            */
/*   0 : Found construct, parse successful.   */
/*   1 : Found construct, parse unsuccessful. */
/**********************************************/
parse_construct(name,log_name)
  char *name, *log_name;
  {
   struct construct *c_ptr;
   int rv;
   
   c_ptr = construct_list;
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {  
         rv = (*c_ptr->ip)(log_name);
         set_pp_buffer_status(OFF);
         return(rv);
        }
      c_ptr = c_ptr->next;
     }
     
   return(-1);
  }
  
/*************************/
/* ADD_FUNC_PARSER:      */
/*************************/
add_func_parser(name,func_ptr)
  char *name;
  struct test *(*func_ptr)();
  {
   struct func_parser *c_ptr;

   c_ptr = get_struct(func_parser);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_func_parser\n");
      return(0);
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = func_parser_list;
   func_parser_list = c_ptr;
   return(1);
  }
 
/****************************/
/* REMOVE_FUNC_PARSER:    */
/****************************/
remove_func_parser(name)
  char *name;
  {
   struct func_parser *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = func_parser_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { func_parser_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(func_parser,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }
  
/****************************************************/
/* COPY_TESTS:                                      */  
/****************************************************/
struct test *copy_tests(original)
  struct test *original;
  {
   struct test *top_level, *next, *last;

   if (original == NULL) { return(NULL); }

   top_level = get_struct(test);
   top_level->type = original->type;
   top_level->val = original->val;
   top_level->arg_list = copy_tests(original->arg_list);
   top_level->next_arg = NULL;

   last = top_level;
   original = original->next_arg;
   while (original != NULL)
     {
      next = get_struct(test);
      next->type = original->type;
      next->val = original->val;
      next->arg_list = copy_tests(original->arg_list);
      next->next_arg = NULL;
      last->next_arg = next;
      last = next;
      original = original->next_arg;
     }

   return(top_level);
  }
  
  
/*****************************************************************/
/* multi_arg_parse: purpose is to parse statements that require  */
/*   at least two arguments, but may have more. Examples include */
/*   addition, multiplication, etc.                              */
/*****************************************************************/
struct test *multi_arg_parse(parse,infile)
  struct test *parse;
  char *infile;
  {
   return(check_arg_list_parse(parse,infile,AT_LEAST,2,TRUE,FALSE));
  }

/*****************************************************************/
/* eq_parse:                                                     */
/*****************************************************************/
struct test *eq_parse(parse,infile)
  struct test *parse;
  char *infile;        
  {
   return(check_arg_list_parse(parse,infile,AT_LEAST,2,FALSE,TRUE));
  }

/*****************************************************************/
/* not_parse:                                                    */
/*****************************************************************/
struct test *not_parse(parse,infile)
  struct test *parse;
  char *infile;
  {
   return(check_arg_list_parse(parse,infile,EXACTLY,1,TRUE,FALSE));
  }

/********************************************************************/
/* check_arg_list_parse: Purpose is to parse statements that only a */
/*   specific number of arguments and/or only numeric arguments.    */
/********************************************************************/
struct test *check_arg_list_parse(parse,infile,check_val,exp_num,
                                        must_be_numeric,may_be_pointer)
  struct test *parse;
  char *infile;
  int check_val, exp_num, must_be_numeric, may_be_pointer;
  {
   struct test *arg_ptr;
   int error, count, num_args;

   /*========================*/
   /* Collect the arguments. */
   /*========================*/

   if (collect_arguments(parse,infile) == NULL) return(NULL);

   /*======================*/
   /* Count the arguments. */
   /*======================*/

   arg_ptr = parse->arg_list;              
   num_args = 0;
   while (arg_ptr != NULL)
     {
      num_args++;
      arg_ptr = arg_ptr->next_arg;
     }

   /*=======================================*/
   /* Check for proper number of arguments. */
   /*=======================================*/

   error = 0; 
   if (check_val == EXACTLY)
     { if (num_args != exp_num) error = 1; }
   else if (check_val == AT_LEAST)
     { if (num_args < exp_num) error = 1; }
   else if (check_val == NO_MORE_THAN)
     { if (num_args > exp_num) error = 1; }

   if (error)                               
     {
      exp_num_error(parse->val.fun_ptr->fun_name,check_val,exp_num);
      returntests(parse);
      return(NULL);
     }

   /*==============================*/
   /* Check for numeric arguments. */
   /*==============================*/

   if ((must_be_numeric == FALSE) && (may_be_pointer == FALSE)) 
     { return(parse); }

   arg_ptr = parse->arg_list;                   
   count = 1;
   while (arg_ptr != NULL)
     {
      if (arg_ptr->type == FCALL)
        {
         if ((must_be_numeric == TRUE) &&
             (arg_ptr->val.fun_ptr->fun_type != 'f') &&
             (arg_ptr->val.fun_ptr->fun_type != 'i') &&
             (arg_ptr->val.fun_ptr->fun_type != 'u'))
           {
            exp_type_error(parse->val.fun_ptr->fun_name,count,
                           "number or numeric function");
            returntests(parse);
            return(NULL);
           }
        }
      else if (arg_ptr->type == BWORD)
        {
         if (may_be_pointer == TRUE)
           { arg_ptr->type = MAY_BE_POINTER; }
        }
      else if (arg_ptr->type != NUMBER)
        {
         if (must_be_numeric == TRUE)
           {
            exp_type_error(parse->val.fun_ptr->fun_name,count,
                              "number or numeric function");
            returntests(parse);
            return(NULL);
           }
        }
      arg_ptr = arg_ptr->next_arg;
      count++;
     }
   
   return(parse);
  }

/***************************************************************/
/* GetRhsPattern:                                              */
/* Returns NULL if ) is first token.                           */
/* Returns assert arguments in test format otherwise.          */
/* Error flag is set to true if an error occurs.               */
/***************************************************************/
struct test *GetRhsPattern(readSource,tempToken,multi,error,
                           variablesAllowed,readFirstParen)
  char *readSource;
  struct token *tempToken;
  int *multi, *error, variablesAllowed, readFirstParen;
  {
   struct test *last_one, *next_one, *first_one;

   *error = FALSE;
   last_one = NULL;
   
   /*=================================================*/
   /* Get the opening parenthesis of the RHS pattern. */
   /*=================================================*/
   
   if (readFirstParen)
     {
      gettoken(readSource,tempToken);
      if (tempToken->token == RPAREN) return(NULL);
      if (tempToken->token != LPAREN)
        {
         cl_print("werror","\nExpected RHS pattern to begin with a '('\n");
         *error = TRUE;
         return(NULL);
        }
     }
   
   /*================================================================*/
   /* Get the first field and determine if a template exists for it. */
   /*================================================================*/
   
   first_one = GetAssertArgument(readSource,tempToken,multi,error);
   if (*error)
     {
      cl_print("werror","\nBad argument for RHS pattern\n");
      return(NULL);
     }
     
   if (first_one == NULL)
     {
      cl_print("werror","\nRHS patterns must contain at least one field\n");
      *error = TRUE;
      return(NULL);
     }
     
#if DEFTEMPLATES && (! BLOAD_ONLY)
   if (first_one->type == WORD)
     {
      extern struct dtmpl *FindDeftemplate();
      extern struct test *ParseAssertTemplate();
      
      if (FindDeftemplate(tempToken->tknword) != NULL)
        {  
         first_one->next_arg = ParseAssertTemplate(readSource,tempToken,
                                                   multi,error,
                                                   tempToken->hashword);
         if (*error)
           {
            returntests(first_one);
            first_one = NULL;
           }
           
         return(first_one);
        }
     }
#endif
    
   last_one = first_one;
   
   /*===========================*/
   /* Get the remaining fields. */
   /*===========================*/
   
   save_pp_buffer(" ");
   while ((next_one = GetAssertArgument(readSource,tempToken,multi,error)) != NULL)
     { 
      last_one->next_arg = next_one;
      last_one = next_one;
      save_pp_buffer(" ");
     }
     
   pp_backup();
   pp_backup();
   save_pp_buffer(tempToken->print_rep);
     
   if (*error)
     {
      cl_print("werror","\nBad argument for RHS pattern\n");        
      returntests(first_one); 
      return(NULL);
     }
      
   return(first_one);
  }

#endif

  
