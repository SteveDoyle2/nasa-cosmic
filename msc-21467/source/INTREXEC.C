/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                INTERFACE EXEC MODULE                */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "rule.h"
#include "engine.h"
  
#if ! RUN_TIME

/************************************************************/
/* CLEAR_FUNC STRUCTURE:                                     */
/************************************************************/
struct clear_func
  {
   char *name;  
   int (*ip)();
   struct clear_func *next;
  };

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   char                    *expand_string_with_char();
   int                      reset_command();
   int                      clear_command();
   int                      exit_command();
   int                      run_command();
   int                      conserve();
   float                    clips_mem();
   int                      clips_rel();
   float                    show_requests();
    
#if BREAKPOINTS
   int                      set_break();
   int                      remove_break();
   int                      show_breaks();
   int                      break_exec();
#endif

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct test      *fctn2_parse();
   extern struct pat_node  *network_pointer();
   extern int               arg_type_check();
   extern float             clips_time();
   extern char             *exp_line();
   extern char             *get_pp_buffer();
   extern struct ruleinfo  *find_rule();
   
#if TRACK_MEMORY
   extern float             mem_requests();
   extern float             mem_used();
#endif
   
#if CLP_HELP
   extern int               cl_help();
   extern int               cl_help_path();
#endif

#if CLP_RULE_COMP
   extern int               rules_to_c_code();
#endif
   
#if CLP_EDIT
   extern int               clp_edit();
#endif

#if CLP_TEXTPRO
   extern float             cl_fetch();
   extern float             cl_toss();
   extern float             cl_print_region();
#endif

   extern int               clips_options();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct clear_func *clear_list;
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   int                      CONSERVE_SPACE;

/*******************************************/
/* DEFINE_COMMANDS:                        */
/*******************************************/
define_commands()
  {
   init_interface_exec();
   init_interface_file();
   init_interface_browse();
   
#if CLP_HELP

   define_function("help",         'v', (int (*)()) cl_help, "cl_help");
   define_function("help-path",    'v', (int (*)()) cl_help_path,
                                                         "cl_help_path");
#endif

#if CLP_EDIT
   define_function("edit",         'v', (int (*)()) clp_edit, "clp_edit");
#endif

#if CLP_RULE_COMP
   define_function("rules-to-c",   'v', (int (*)()) rules_to_c_code, 
                                                      "rules_to_c_code");
#endif


#if CLP_TEXTPRO

   define_function("fetch",        'f', cl_fetch,       "cl_fetch");
   define_function("toss",         'f', cl_toss,         "cl_toss");
   define_function("print-region", 'f', cl_print_region,
                                                 "cl_print_region");

#endif
  }

/************************************************************/
/* INIT_INTERFACE_EXEC:                      */
/************************************************************/
init_interface_exec()
  {
   define_function("reset",        'v', reset_command,  "reset_command");
   define_function("clear",        'v', clear_command,  "clear_command");
   define_function("run",          'v', run_command,    "run_command");
   define_function("exit",         'v', exit_command,   "exit_command");
   define_function("conserve-mem", 'v', conserve,       "conserve");
   define_function("release-mem",  'v', clips_rel,      "clips_rel");
#if TRACK_MEMORY
   define_function("mem-used",     'f', (int (*)()) clips_mem, 
                   "clips_mem");
   define_function("mem-requests", 'f', (int (*)()) show_requests,  
                   "show_requests");
#endif

#if BREAKPOINTS
   define_function("set-break",    'v', set_break,    "set_break");
   define_function("remove-break", 'v', remove_break, "remove_break");
   define_function("show-breaks",  'v', show_breaks,  "show_breaks");
#endif
   define_function("options",      'v', clips_options,"clips_options");
  }

/********************************************************/
/* timed_run:  Calls a timing function to determine the */
/*   amount of time a run has taken.                    */
/********************************************************/
timed_run(run_limit)
  int run_limit;
  {
   int rules_fired;
   float end_time, start_time;

   start_time = clips_time();
   rules_fired = run(run_limit);
   end_time = clips_time();

   if (run_limit == rules_fired)
     { cl_print("wdialog","rule firing limit reached\n"); }
   print_long_int("wdialog",(long int) rules_fired);
   cl_print("wdialog"," rules fired\n");

   if (start_time != end_time)
     { 
      cl_print("wdialog","Run time is ");
      print_num("wdialog",end_time - start_time);
      cl_print("wdialog"," seconds\n");
     }  
  }
  
/************************************************/
/* clear_command: Clears the CLIPS environment. */
/*   Syntax:  (clear)                           */
/************************************************/
int clear_command()
  {
   if (arg_num_check("clear",EXACTLY,0) == -1) return(0);
   clear_clips();
   return(1);
  }
  
/****************************************************************/
/* clear_clips: the purpose of this function is to clear the    */
/*   CLIPS environment.  All rules and facts are removed. The   */
/*   effect is as if CLIPS were completely restarted.           */
/*   Syntax:  (clear)                                           */
/****************************************************************/
clear_clips()
  {
   struct ruleinfo *rule_ptr;
   struct clear_func *clear_ptr;
   
   /*========================================*/
   /* Function not allowed from RHS of rule. */
   /*========================================*/
   
   if (ExecutingRule()) 
     {
      cl_print("werror","WARNING: Clear command may not be performed ");
      cl_print("werror","during the execution of a rule\n");
      return;
     }
   
   /*====================================*/
   /* Initialize some global parameters. */
   /*====================================*/

   set_fact_id((FACT_ID) -1);      
   set_nid((FACT_ID) -2);
   set_agenda_count(0);
   
   /*======================================*/
   /* Remove all facts from the fact list. */
   /*======================================*/

   remove_all_facts();
  
   /*========================================*/
   /* Remove all bindings from the join net. */
   /*========================================*/

   flush_web(network_pointer());
   
   /*=========================================*/
   /* Remove all activations from the agenda. */
   /*=========================================*/
   
   remove_all_activations();

   /*===================================*/
   /* Remove all rules from the system. */
   /*===================================*/

   while ((rule_ptr = get_next_rule(NULL)) != NULL)
     { excise_rule(get_rule_name(rule_ptr)); }

   /*========================*/
   /* System Error Checking. */
   /*========================*/
   
#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
   if (bloaded() == FALSE)
#endif

   if (network_pointer() != NULL)
     { 
      clips_system_error(1001);
      cl_exit(5);
     }
     
   /*============================*/
   /* Calls all clear functions. */
   /*============================*/

   clear_ptr = clear_list;
   while (clear_ptr != NULL)
     {
      (*clear_ptr->ip)();
      clear_ptr = clear_ptr->next;
     }
  }

/****************************************/
/* CLIPS_MEM:  Returns amount of memory */
/*   allocated by CLIPS.                */
/****************************************/
#if TRACK_MEMORY
float clips_mem()
  {
   if (arg_num_check("mem-used",EXACTLY,0) == -1) return(0);
   return(mem_used());
  }
#endif

/******************************************/
/* CLIPS_REL:  Returns free memory stored */
/*   by CLIPS to the system.              */ 
/******************************************/
int clips_rel()
  {
   if (arg_num_check("release-mem",EXACTLY,0) == -1) return(0);
   release_mem(-1,FALSE);
   return(0);
  }
  
/**********************************************/
/* SHOW_REQUESTS:  Indicates number of memory */
/*   allocations made by CLIPS.               */
/**********************************************/
#if TRACK_MEMORY
float show_requests()
  {

   if (arg_num_check("mem-requests",EXACTLY,0) == -1) return(0);
   return(mem_requests());
  }
#endif
  
/**********************************************/
/* CONSERVE:  Determines whether pretty print */
/*   representation for defrules and deffacts */
/*   will be stored.                          */
/**********************************************/
int conserve()
  {
   char *argument;
   VALUE arg_ptr;

   if (arg_num_check("conserve-mem",EXACTLY,1) == -1) return(0);
   if (arg_type_check("conserve-mem",1,WORD,&arg_ptr) == FALSE) return(0);
 
   argument = get_valstring(arg_ptr);
 
   if (set_conserve(argument) == 0)
     {
      cl_print("werror","Legal arguments for conserve-mem are on and off\n");
      return(0);
     }
 
   return(1);
  }

/************************************************/
/* set_conserve:  Allows the setting of         */
/*   CONSERVE_SPACE through embedded operation. */       
/************************************************/
int set_conserve(argument)
  char *argument;
  {
   if (strcmp(argument,"on") == 0)
     { CONSERVE_SPACE = TRUE; }
   else if (strcmp(argument,"off") == 0)
     { CONSERVE_SPACE = FALSE; }
   else
     { return(0); }
   return(1);
  }
  
/************************************************/
/* reset_command: Resets the CLIPS environment. */
/*   Syntax:  (reset)                           */
/************************************************/
int reset_command()
  { 
   if (arg_num_check("reset",EXACTLY,0) == -1) return(0);  
   reset_clips();
   return(1);
  }
  
/***********************************************************/
/* run_command: begins execution of the production rules.  */
/*   Takes an optional argument which indicates the number */
/*   of rules to fire before stopping.                     */
/*   Syntax: (run [ <number> ])                            */
/***********************************************************/
int run_command()
  {
   int num_a;
   int run_limit;
   VALUE arg_ptr;

   if ((num_a = arg_num_check("run",NO_MORE_THAN,1)) == -1) return(0);

   if (num_a == 0)
     { run_limit = -1; }
   else if (num_a == 1)
     { 
      if (arg_type_check("run",1,NUMBER,&arg_ptr) == FALSE) return(0);
      run_limit = (int) get_valfloat(arg_ptr);
     }

   timed_run(run_limit); 
   return(1);
  }

/**********************************************/
/* exit_command: Exits the CLIPS environment. */
/*   Syntax:  (exit)                          */
/**********************************************/
int exit_command()
  {
   if (arg_num_check("exit",EXACTLY,0) == -1) return(0);  
#if VMS
   cl_exit(-2);  /* Fix for weird VMS com file problem. */
#else
   cl_exit(-1);
#endif
   return(1);
  }
  
#if BREAKPOINTS

/************************************************************/
/* FUNCTIONS FOR HANDLING BREAKPOINTS                       */
/************************************************************/

struct breakpoint
  {
   char *name;
   struct breakpoint *next;
  };
  
static struct breakpoint *break_list = NULL;
  
/************************************************************/
/* SET_BREAK:                                               */
/************************************************************/
int set_break()
  {
   VALUE arg_ptr;
   char *argument;

   if (arg_num_check("set_break",EXACTLY,1) == -1) return;

   if (arg_type_check("set_break",1,WORD,&arg_ptr) == FALSE) return;
   
   argument = get_valstring(arg_ptr);
   
   if (find_rule(argument) == NULL)
     {
      cl_print("werror","Rule ");
      cl_print("werror",argument);
      cl_print("werror"," does not exist\n");
      return;
     }
   
   add_break_name(argument);
  }
  
/************************************************************/
/* ADD_BREAK_NAME:                                          */
/************************************************************/
add_break_name(rule_name)
  char *rule_name;
  {
   struct breakpoint *b_ptr, *last_break;
   
   last_break = NULL;
   b_ptr = break_list;
   while (b_ptr != NULL)
     {
      if (strcmp(b_ptr->name,rule_name) == 0)
        { return; }
      last_break = b_ptr;
      b_ptr = b_ptr->next;
     }
   
   b_ptr = get_struct(breakpoint);
   if (b_ptr == NULL) return;
   b_ptr->name = gm2(strlen(rule_name) + 1);
   if (b_ptr->name == NULL) return;
   strcpy(b_ptr->name,rule_name);
   b_ptr->next = NULL;
   if (last_break == NULL)
     { 
      break_list = b_ptr; 
      add_exec_function("break",break_exec);
     }
   else
     { last_break->next = b_ptr; }
     
  }
  
/************************************************************/
/* REMOVE_BREAK:                                            */
/************************************************************/
int remove_break()
  {
   VALUE arg_ptr;
   char *argument;
   int nargs;

   if ((nargs = arg_num_check("remove_break",NO_MORE_THAN,1)) == -1) 
     { return; }
     
   if (nargs == 0)
     {
      remove_all_breaks();
      return;
     }

   if (arg_type_check("remove_break",1,WORD,&arg_ptr) == FALSE) return;
   
   argument = get_valstring(arg_ptr);
     
   if (remove_break_name(argument) == CLIPS_FALSE)
     {
      cl_print("werror","Rule ");
      cl_print("werror",argument);
      cl_print("werror"," does not have a breakpoint set.\n");
     }
  }
  
/************************************************************/
/* REMOVE_BREAK_NAME:                                      */
/************************************************************/
remove_break_name(rule_name)
  char *rule_name;
  {
   struct breakpoint *b_ptr, *last_break;
   
   last_break = NULL;
   b_ptr = break_list;
   while (b_ptr != NULL)
     {
      if (strcmp(b_ptr->name,rule_name) == 0)
        { 
         rm(b_ptr->name,strlen(b_ptr->name) + 1);
         if (last_break == NULL)
           { break_list = b_ptr->next; }
         else
           { last_break->next = b_ptr->next; }
         rtn_struct(breakpoint,b_ptr);
         
         if (break_list == NULL)
           { remove_exec_function("break"); }
         return(CLIPS_TRUE);
        }
      last_break = b_ptr;
      b_ptr = b_ptr->next;
     }
   return(CLIPS_FALSE);
  }
  
/************************************************************/
/* REMOVE_ALL_BREAKS:                                       */
/************************************************************/
remove_all_breaks()
  {
   struct breakpoint *b_ptr;
   
   while (break_list != NULL)
     {
      b_ptr = break_list->next;
      rm(break_list->name,strlen(break_list->name) + 1);
      rtn_struct(breakpoint,break_list);
      break_list = b_ptr;
     } 
   remove_exec_function("break");
  }

/************************************************************/
/* SHOW_BREAKS:                                             */
/************************************************************/
int show_breaks()
  {
   struct breakpoint *b_ptr;

   if (arg_num_check("show_breaks",EXACTLY,0) == -1) return;
   
   b_ptr = break_list;
   while (b_ptr != NULL)
     {
      cl_print("wdisplay",b_ptr->name);
      cl_print("wdisplay","\n");
      b_ptr = b_ptr->next;
     }
  }
  
/************************************************************/
/* BREAK_EXEC:                                              */
/************************************************************/
break_exec()
  {
   struct activation *act_ptr;
   char *break_name;
   struct breakpoint *b_ptr;
   
   act_ptr = get_next_activation(NULL);
   if (act_ptr == NULL) return;
   break_name = activation_rule_name(act_ptr);
   
   b_ptr = break_list;
   while (b_ptr != NULL)
     {
      if (strcmp(b_ptr->name,break_name) == 0)
        {
         cl_print("wdisplay","Breaking on rule ");
         cl_print("wdisplay",break_name);
         cl_print("wdisplay","\n");
         set_execution_error(TRUE);
         return;
        }
      b_ptr = b_ptr->next;
     }
  }
   
/*************************/
/* RULE_HAS_BREAK:       */
/*************************/
rule_has_break(rule_name)
  char *rule_name;
  {
   struct breakpoint *b_ptr;
   
   b_ptr = break_list;
   while (b_ptr != NULL)
     {
      if (strcmp(b_ptr->name,rule_name) == 0)
        { return(TRUE); }
        
      b_ptr = b_ptr->next;
     }
     
   return(FALSE);
  }
   
#endif

/*************************/
/* ADD_CLEAR_FUNCTION:    */
/*************************/
add_clear_function(name,func_ptr)
  char *name;
  int (*func_ptr)();
  {
   struct clear_func *c_ptr;

   c_ptr = get_struct(clear_func);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_clear_function\n");
      return(0);
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = clear_list;
   clear_list = c_ptr;
   return(1);
  }
 
/****************************/
/* REMOVE_CLEAR_FUNCTION:    */
/****************************/
remove_clear_function(name)
  char *name;
  {
   struct clear_func *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = clear_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { clear_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(clear_func,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }

 
#else
  
   int                      reset_command() {};
   int                      clear_command() {};
   int                      exit_command() {};
   int                      run_command() {};
   int                      conserve() {};
   float                    clips_mem() {};
   int                      clips_rel() {};
   float                    show_requests() {};
   int                      add_clear_function() {};
   int                      remove_clear_function() {};

#if ! CLP_HELP
   int                      cl_help() {};
   int                      cl_help_path() {};
#endif
 
#if ! CLP_EDIT
   float                    clp_edit() {};
#endif
 
#if ! CLP_TEXTPRO
   float                    cl_fetch() {};
   float                    cl_toss() {};
   float                    cl_print_region() {};
#endif

#if BREAKPOINTS
   int                      set_break() {};   
   int                      show_breaks() {};
   int                      remove_break() {};
#endif

#endif



