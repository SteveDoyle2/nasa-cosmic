/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*               INTERFACE BROWSE MODULE               */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "rule.h"
#include "engine.h"

#if ! RUN_TIME

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                        pprule_command();
   int                        facts_command();
   int                        rule_display();
   int                        agenda_command();
   int                        watch_command();
   int                        unwatch_command();
   int                        excise_command();
   int                        part_match();
   int                        show_pn();
   int                        set_watch_display_function();

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct ruleinfo    *find_rule();
   extern int                 arg_type_check();
   extern char               *check_name();
   extern struct activation  *get_next_activation();
   extern struct pat_node    *network_pointer();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static int                (*watch_display_function)() = NULL;

/*************************************************************/
/* INIT_INTERFACE_BROWSE:       */
/*************************************************************/
init_interface_browse()
  {
   define_function("watch",        'v', watch_command,      "watch_command");
   define_function("unwatch",      'v', unwatch_command,    "unwatch_command");
   define_function("facts",        'v', facts_command,      "facts_command");
   define_function("rules",        'v', rule_display,       "rule_display");
   define_function("agenda",       'v', agenda_command,     "agenda_command"); 
   define_function("pprule",       'v', pprule_command,     "pprule_command");
     
   define_function("excise",       'v', excise_command,     "excise_command");
   define_function("show-pn",      'v', show_pn,            "show_pn");
   define_function("matches",      'v', part_match,         "part_match");
  }
  
  

/*#########################################################*/
/* These functions provide many features of the user       */
/* interface, but are not required for embedded operation. */
/*#########################################################*/

/*************************************************************/
/* set_watch:  Sets the global watch variables to on or off. */
/*************************************************************/
int set_watch(item,act_value)
  char *item;
  int act_value; 
  {
   if ((act_value != ON) && (act_value != OFF))
     { 
      cl_print("werror","Watch items may only be set on or off\n");
      return(0); 
     }

   if (strcmp(item,"facts") == 0)            
     { set_facts_watch(act_value); } 
   else if (strcmp(item,"rules") == 0)      
     { set_rules_watch(act_value); } 
   else if (strcmp(item,"activations") == 0)
     { set_activations_watch(act_value); }
   else if (strcmp(item,"compilations") == 0)
     { set_compilations_watch(act_value); }
   else if (strcmp(item,"all") == 0)            
     { 
      set_facts_watch(act_value);      
      set_rules_watch(act_value);  
      set_activations_watch(act_value);
      set_compilations_watch(act_value);
     }        
   else
     {
      cl_print("werror",item);
      cl_print("werror"," is illegal argument for ");
      if (act_value)
        { cl_print("werror","watch\n"); }
      else
        { cl_print("werror","unwatch\n"); }
      return(0);
     }
     
   if (watch_display_function != NULL)
     { (*watch_display_function)(); }

   return(1);
  }
  
/**************************************************************/
/* SET_WATCH_DISPLAY_FUNCTION : This routine sets the function*/
/* which will be called by the watch() debugging routines to  */
/* update any user-defined debugging displays                 */
/**************************************************************/
int set_watch_display_function(funptr)
  int (*funptr)();
  {
   watch_display_function = funptr;
  }

/************************************************************/
/* rule_display: displays the list of rules.                */
/*   Syntax: (rules)                                        */
/************************************************************/
int rule_display()
  {
   struct ruleinfo *rule_list;

   if (arg_num_check("rules",EXACTLY,0) == -1) return(0);
   
   rule_list = NULL;
   while ((rule_list = get_next_rule(rule_list)) != NULL)
     {
      cl_print("wdisplay",get_rule_name(rule_list));
      cl_print("wdisplay","\n");
     }

   return(1);
  }

/************************************************************/
/* pretty_print: the driver which actually does the pretty  */
/*   printing of the rule.                                  */
/************************************************************/
pretty_print(rule_name,fileid)
  char *rule_name, *fileid;
  {
   struct ruleinfo *rule_ptr;

   rule_ptr = find_rule(rule_name);
   if (rule_ptr == NULL) 
     { 
      cl_print("werror","Unable to find rule ");
      cl_print("werror",rule_name);
      cl_print("werror","\n");
      return(FALSE);
     }

   if (get_rule_ppform(rule_ptr) == NULL) return(TRUE);
   print_in_chunks(fileid,get_rule_ppform(rule_ptr));
   return(TRUE);
  }
   
/********************************/
/* User interface command hooks */
/********************************/


/************************************************************/
/* watch_command: parses the watch command and sets the     */
/*   appropriate global variable to the value of 1. Fact    */
/*   assertions and retractions, and rule activations,      */
/*   deactivations, and firings are respectively monitored  */
/*   if facts or rules are watched.                         */
/************************************************************/
int watch_command()
  {
   VALUE arg_ptr;
   char *argument;
   int set_watch();

   if (arg_num_check("watch",EXACTLY,1) == -1) return(0);
   
   if (arg_type_check("watch",1,WORD,&arg_ptr) == FALSE) return(0);
   argument = get_valstring(arg_ptr);
   
   return(set_watch(argument,ON));
  }

/************************************************************/
/* unwatch_command: parses the unwatch command and sets the */
/*   appropriate global variable to the value of 0. Fact    */
/*   assertions and retractions, and rule activations,      */
/*   deactivations, and firings are respectively not        */
/*   monitored if facts or rules are unwatched.             */
/************************************************************/
int unwatch_command()
  {
   VALUE arg_ptr;
   char *argument;
   int set_watch();

   if (arg_num_check("unwatch",EXACTLY,1) == -1) return(0);
 
   if (arg_type_check("unwatch",1,WORD,&arg_ptr) == FALSE) return(0);
   
   argument = get_valstring(arg_ptr);
   
   return(set_watch(argument,OFF));
  }

/*****************************************************/
/* excise_command: excises a rule from the database. */
/*   Syntax: (excise <rule name>)                    */
/*****************************************************/
int excise_command()
  {
   char *rule_name;

   rule_name = check_name(1,1,"excise","rule name");
   if (rule_name == NULL) return;
   
   if (ExecutingRule()) 
     {
      cl_print("werror","WARNING: Excise command may not be performed ");
      cl_print("werror","during the execution of a rule\n");
      return;
     }
   
   if (excise_rule(rule_name) == FALSE)        
     {
      cl_print("werror","Unable to find rule ");
      cl_print("werror",rule_name);
      cl_print("werror","\n");
      return;
     }

   return;
  }

/************************************************************/
/* pprule_command: pretty prints a rule.                    */
/*   Syntax: (pprule <rule name>)                           */
/************************************************************/
int pprule_command()
  {
   char *rule_name;

   rule_name = check_name(1,1,"pprule","rule name");
   if (rule_name == NULL) return(0);

   pretty_print(rule_name,"wdisplay");

   return(1);
  }

/****************************************************************/
/* PART_MATCH:  Displays the set of partial matches for a rule. */
/****************************************************************/
int part_match()
  {
   char *rule_name;
   struct ruleinfo *rule_ptr;
   struct flink *match_list;
   struct patptr *pat_del;
   struct activation *agenda_ptr;
   int pat_count, flag;

   rule_name = check_name(1,1,"matches","rule name");
   if (rule_name == NULL) return(0);

   rule_ptr = find_rule(rule_name);
   if (rule_ptr == NULL) 
     {
      cl_print("werror","Unable to find rule ");
      cl_print("werror",rule_name);
      cl_print("werror","\n");
      return(0);
     }

   /*=======================================*/
   /* List individual matches for patterns. */
   /*=======================================*/

   pat_count = 1;
   pat_del = rule_ptr->pats;
   while (pat_del != NULL)
     {
      if (pat_del->lptr->path->lhs_log == 'e')
        { pat_count = 1; }
      if (pat_del->pptr != NULL)
        { match_list = pat_del->pptr->alpha; }
      else
        { match_list = NULL; }
        
      if (pat_del->lptr->path->next != NULL)
        {
         cl_print("wdisplay","Matches for Pattern ");
         print_long_int("wdisplay",(long int) pat_count);
         cl_print("wdisplay","\n");
         if (match_list == NULL) cl_print("wdisplay"," None\n");
        }
      
      while (match_list != NULL)
        {
         print_fact_basis("wdisplay",match_list->binds);
         cl_print("wdisplay","\n");
         match_list = match_list->next;
        }
      pat_count++;
      pat_del = pat_del->next;
     }

   /* old code for partial matches */
   pat_count = 0;
   pat_del = rule_ptr->pats;
   while (pat_del != NULL)
     {
      if (pat_del->lptr->path->lhs_log == 'e')
        { 
         pat_count = 0;
         match_list = NULL;
        }
      else
        { match_list = pat_del->lptr->path->beta; }
      
      if ((pat_count > 1) && (pat_del->lptr->path->next != NULL))
        {
         cl_print("wdisplay","Partial matches for patterns 1 - ");
         print_long_int("wdisplay",(long int) pat_count);
         cl_print("wdisplay","\n");
         if (match_list == NULL) { cl_print("wdisplay"," None\n"); }
        }
      else
        { match_list = NULL; }

      while (match_list != NULL)
        {
         print_fact_basis("wdisplay",match_list->binds);
         cl_print("wdisplay","\n");
         match_list = match_list->next;
        }
      pat_count++;
      pat_del = pat_del->next;
     }
   
   cl_print("wdisplay","Activations\n");
   agenda_ptr = get_next_activation(NULL);
   flag = 1;
   while (agenda_ptr != NULL)
     {
      if (strcmp(activation_rule_name(agenda_ptr),rule_name) == 0) 
        {
         flag = 0; 
         print_fact_basis("wdisplay",activation_basis(agenda_ptr));
         cl_print("wdisplay","\n");
        }
      agenda_ptr = get_next_activation(agenda_ptr);
     }
   if (flag) cl_print("wdisplay"," None\n");
   
   return(1);
  }
  
/*#####################*/
/* DEBUGGING FUNCTIONS */
/*#####################*/  

/*****************************************/
/* show_pn                               */
/*****************************************/
int show_pn()
  {
   show_compare_net(network_pointer(),1);
   return(0);
  }
  
/****************************************************/
/* FACTS_COMMAND:  Displays the facts in fact list. */
/*   Syntax: (facts)                                */
/****************************************************/
long int get_facts_arg(pos,num_arg)
  int pos, num_arg;
  {
   long int fint;
   VALUE arg_ptr;
   
   if (pos > num_arg) return(-1L);
     
   if (arg_type_check("facts",pos,NUMBER,&arg_ptr) == FALSE) return(-2L);
   fint = (long int) get_valfloat(arg_ptr);
   if (fint < 0) 
     {
      exp_type_error("facts",pos,"positive number");
      set_execution_error(TRUE);
      return(-2L);
     }
   return(fint);
  }
  
int facts_command()
  {   
   struct fact *fact_ptr;
   int num_arg;
   long int start, end, max;
   
   if ((num_arg = arg_num_check("facts",NO_MORE_THAN,3)) == -1) return(0);
   
   if ((start = get_facts_arg(1,num_arg)) == -2) return(0);
   if ((end = get_facts_arg(2,num_arg)) == -2) return(0);
   if ((max = get_facts_arg(3,num_arg)) == -2) return(0);
   
   fact_ptr = get_next_fact(NULL); 
   while (fact_ptr != NULL)
     {
      if ((fact_ptr->ID > end) && (end != -1)) return(1);
      if (max == 0) return(1);
      
      if (fact_ptr->ID >= start)
        {
         show_fact("wdisplay",fact_ptr);
         cl_print("wdisplay","\n");
         if (max > 0) max--;
        }
         
      fact_ptr = get_next_fact(fact_ptr);
     }

   return(1);
  }
  

/************************************************/
/* AGENDA_COMMAND: Prints out the agenda of the */
/*   rules that are ready to fire.              */
/*   Syntax: (agenda)                           */
/************************************************/
agenda_command()
  {   
   if (arg_num_check("agenda",EXACTLY,0) == -1) return(0);

   printagenda();
 
   return(1);
  }
  
#else

   int                        pprule_command() {};
   int                        facts_command() {};
   int                        rule_display() {};
   int                        agenda_command() {};
   int                        watch_command() {};
   int                        unwatch_command() {};
   int                        excise_command() {};
   int                        part_match() {};
   int                        show_pn() {};

#endif
