/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                 COMMAND LINE MODULE                 */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "scanner.h"
#include "rule.h"


/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                      get_next_event();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   char                    *expand_string_with_char();
   int                      set_mem_status_function();

#if ! RUN_TIME

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct test      *fctn2_parse();
   extern char             *exp_line();
   extern char             *append_to_string();
   extern char             *get_pp_buffer();
   
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static char             *com_str = NULL;
   static int               com_pos = 0;
   static int               com_max = 0;
   static int              (*event_function)() = get_next_event;
   static int               TopLevel = FALSE;
   static int              (*memory_status_function)() = NULL;
   
/**********************************************************/
/* EXPAND_COMMAND_STRING:                                 */
/**********************************************************/
expand_command_string(inchar)
  char inchar;
  {
   com_str = expand_string_with_char(inchar,com_str,&com_pos,&com_max,com_max+80);
  }
  
#if ! RUN_TIME  

/**********************************************************/
/* FLUSH_COMMAND_STRING:                                 */
/**********************************************************/
flush_command_string()
  {
   if (com_str != NULL) rm(com_str,com_max);
   com_str = NULL;
   com_pos = 0;
   com_max = 0;
  }
  
/**********************************************************/
/* SET_COMMAND_STRING:                                    */
/**********************************************************/
set_command_string(str)
  char *str;
  {
   int length;
   
   flush_command_string();
   length = strlen(str);
   com_str = exp_line(com_str,com_max,com_max + length + 1);
   if (com_str == NULL)
     {
      cl_print("werror","Unable to expand command buffer\n");
      cl_exit(1);
     }
     
   strcpy(com_str,str);
   com_max += (length + 1);
   com_pos += length;
  }
   
/**********************************************************/
/* APPEND_COMMAND_STRING:                                 */
/**********************************************************/
append_command_string(str)
  char *str;
  {
   com_str = append_to_string(str,com_str,&com_pos,&com_max);
   
   if (com_str == NULL)
     {
      cl_print("werror","Unable to expand command buffer\n");
      cl_exit(1);
     }
  }

/**********************************************************/
/* GET_COMMAND_STRING:                                       */
/**********************************************************/
char *get_command_string()
  {
   return(com_str);
  }
  
/**********************************************************/
/* MATCHING_PARENS:                                       */
/**********************************************************/
matching_parens(mstring)
  char *mstring;
  {
   int i;
   char inchar;
   int depth = 0;
   int gt_zero = 0;
   
   if (mstring == NULL) return(0);
   
   i = 0;
   while ((inchar = mstring[i++]) != EOS)
     {   
      if ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == '\t'))
        { i = do_white_space(mstring,i); }
      else if (inchar == '"')
        { 
         if (depth == 0) return(-1);
         i = do_string(mstring,i); 
        }
      else if (inchar == ';')
        { i = do_comment(mstring,i); }
      else if (inchar == '(')
        {
         depth++;
         gt_zero = TRUE;
        }
      else if (inchar == ')')
        { 
         if (depth > 0) depth--;
         else return(-1);
        }
      else 
        {
         if (depth == 0) return(-1);
        }
     }
     
   if ((depth == 0) && (gt_zero == TRUE))
     { return(1); }
     
   return(0);
  }
  
/**********************************************************/
/* DO_STRING:                                       */
/**********************************************************/
do_string(str,pos)
  char *str;
  int pos;
  {
   int inchar;
   
   inchar = str[pos];
   while (inchar  != '"')
     {
      if (inchar == '\\') 
        {
         pos++;
         inchar = str[pos];
        }
        
      if (inchar == EOS)
        { return(pos); }
        
      pos++;
      inchar = str[pos];
     }
     
   pos++;
   return(pos);
  }

  
/**********************************************************/
/* DO_COMMENT:                                       */
/**********************************************************/
do_comment(str,pos)
  char *str;
  int pos;
  {
   int inchar;
   
   inchar = str[pos];
   while ((inchar != '\n') && (inchar != '\r'))
     {
      if (inchar == EOS)
        { return(pos); }
        
      pos++;
      inchar = str[pos];
     }
     
   pos++;
   return(pos);
  }
  
/**********************************************************/
/* DO_WHITE_SPACE:                                       */
/**********************************************************/
do_white_space(str,pos)
  char *str;
  int pos;
  {
   int inchar;
   
   inchar = str[pos];
   while ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == '\t'))
     {
      pos++;
      inchar = str[pos];
     }
     
   return(pos);
  }
  
  
/************************************************************/
/* COMMAND_LOOP: prompts the user for clips commands and    */
/*   calls the appropriate functions to execute the command */
/************************************************************/
command_loop()
  {
   int inchar;
         
   set_execution_error(FALSE);
   print_prompt() ;
   while (TRUE)
     {
      flush_pp_buffer();
      set_pp_buffer_status(OFF);
      
      /*===================================================*/
      /* If a batch file is active, grab the command input */
      /* directly from the batch file, otherwise call the  */
      /* event function.                                   */
      /*===================================================*/
      
      if (batch_active() == TRUE)
        {
         inchar = ll_getc_batch("stdin",TRUE);
         if (inchar == EOF) 
           { (*event_function)(); }
         else
           { expand_command_string((char) inchar); }
        }
      else
        { (*event_function)(); }
	  
	  if (get_execution_error() == TRUE)
	    {
	     set_execution_error(FALSE);
	     flush_command_string();
	     fflush(stdin);
         print_prompt();
	    }

      if ((matching_parens(com_str) != 0) && (com_pos > 0))
        {
         if ((com_str[com_pos-1] == '\n') || (com_str[com_pos-1] == '\r'))
           {
            route_command(com_str);
            set_execution_error(FALSE);
            flush_command_string();
            print_prompt();
           }
        }

      rmv_old_facts();  
      rem_eph_symbols(); 
      flush_segments();
     }
  }

/*******************************************/
/* PRINT_PROMPT                            */
/*******************************************/
print_prompt()
   {
    cl_print("wclips","CLIPS> ");
      
    if (memory_status_function != NULL)
      { (*memory_status_function)(); }
   }

/*********************************************/
/* SET_MEM_STATUS_FUNCTION : Sets pointer to */
/*  memory status function                   */
/*********************************************/
set_mem_status_function(funptr)
  int (*funptr)();
  {
   memory_status_function = funptr;
  }

/*******************************************/
/* ROUTE_COMMAND:                          */
/*******************************************/
route_command(command)
  char *command;
  {
   VALUE result;
   struct test *top;
   char *command_name;
   struct token com_tkn;
   int error_flag = FALSE;
   
   if (command == NULL)
     { return(0); }
     
   open_str_source("command",command,0);
      
   gettoken("command",&com_tkn);
   if (com_tkn.token != LPAREN)
     {
      cl_print("werror","Expected a '('\n");
      close_str_source("command");
      return(0);
     }
   
   gettoken("command",&com_tkn);
   if (com_tkn.token != WORD) 
     {
      cl_print("werror","Expected a command.\n");
      close_str_source("command");
      return(0);
     }
     
   command_name = com_tkn.tknword;
   
   /*======================*/
   /* Evaluate Constructs. */
   /*======================*/
   
   error_flag = parse_construct(command_name,"command");
   if (error_flag != -1)
     {
      close_str_source("command");
      if (error_flag == 1)
        { 
         cl_print("werror","\nERROR:\n");
         print_in_chunks("werror",get_pp_buffer());
         cl_print("werror","\n");
        }
      return(error_flag);
     }
     
   /*===================================*/
   /* Parse Function Call. */
   /*===================================*/
   
   TopLevel = TRUE;
   top = fctn2_parse("command",command_name);
   TopLevel = FALSE;
   
   if (top == NULL)
     { 
      close_str_source("command");
      return(0); 
     }

   /*====================================*/
   /* Check for variables in Expression. */
   /*====================================*/

   if (variables_in_expression(top))
     {
      cl_print("werror","Variables are not allowed in top level expressions\n");
      returntests(top);
      close_str_source("command");
       return(0);
      }

   /*===================================*/
   /* Evaluate Top Level Function Call. */
   /*===================================*/

   generic_compute(top,&result);
   returntests(top);

   if (result.type != RVOID)
     { 
      print_value("wdialog",&result);
      cl_print("wdialog","\n");
     }

   close_str_source("command");
   return(1);
  }
 
/**********************************************************/
/* VARIABLES_IN_EXPRESSION:                               */
/**********************************************************/
int variables_in_expression(exp_ptr)
  struct test *exp_ptr;
  {  
   while (exp_ptr != NULL)
     {
      if (exp_ptr->type == FCALL)
        {
         if (variables_in_expression(exp_ptr->arg_list))
           { return(TRUE); }
        }
      else if ((exp_ptr->type == BWORDS) ||
               (exp_ptr->type == BWORD) ||
               (exp_ptr->type == POINTER))
        { return(TRUE); }

      exp_ptr = exp_ptr->next_arg;
     }

   return(FALSE);
  } 

#endif

/**********************************************************/
/* EXPAND_STRING_WITH_CHAR:                                 */
/**********************************************************/
char *expand_string_with_char(inchar,str,pos,max,new_size)
  char inchar;
  char *str;
  int *max, *pos, new_size;
  {
   extern char *exp_line();
   
   if (*pos >= (*max - 1))
     { 
      str = exp_line(str,*max,new_size);
      if (str == NULL)
        {
         cl_print("werror","Unable to expand string buffer\n");
         cl_exit(1);
        }
      *max = *max + 80;
     }
        
  if (inchar != '\b')
    { 
     str[*pos] = inchar;
     (*pos)++;
     str[*pos] = EOS;
    }
  else
    {
     if (*pos > 0) (*pos)--;
     str[*pos] = EOS;
    }
  
   return(str);
  }

/**********************************************************/
/* GET_NEXT_EVENT:                                        */
/**********************************************************/
int get_next_event()
  {
   int inchar;

   inchar = cl_getc("stdin");

   if (inchar == EOF) inchar = '\n';

   expand_command_string((char) inchar);
  }
     
/**********************************************************/
/* SET_EVENT_FUNCTION:                                    */
/* The return value should be indicated by the function */
/**********************************************************/
int (*set_event_function(fun_ptr))()
  int (*fun_ptr)();
  {
   int (*tmp_ptr)();
   
   tmp_ptr = event_function;
   event_function = fun_ptr;
   return(tmp_ptr);
  }
  
/**********************************************************/
/* TopLevelCommand:                                       */
/**********************************************************/
TopLevelCommand()
  { return(TopLevel); }
