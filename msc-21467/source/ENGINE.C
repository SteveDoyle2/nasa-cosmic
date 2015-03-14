/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   ENGINE MODULE                     */
   /*******************************************************/

#include <stdio.h>

#include "clips.h"
#include "engine.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                     run();
   int                     add_activation();
   int                     remove_all_activations();
   int                     set_agenda_count();
   int                     set_activations_watch();
   int                     get_activations_watch();
   int                     purge_agenda();
   int                     clear_rule_from_agenda();
   int                     print_activation();
   struct activation      *get_next_activation();
   int                     set_execution_error();
   int                     get_execution_error();
   int                     get_change_agenda();
   int                     set_change_agenda();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct activation   *AGENDA = NULL;
   static long int             AGENDA_COUNT = 0;  
   static int                  watch_activations = OFF; 
   static int                  EXECUTION_ERROR = FALSE;
   static char                *currentrule = NULL;   
   static struct exec_func    *exec_list = NULL;  
   static int                  change_agenda = FALSE;
   static int                  executing = FALSE;     
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct fbind        *gbl_lhs_binds;
   extern struct fbind        *gbl_rhs_binds;
   
/**************************************************************/
/* RUN: Begins execution of rules.  If run limit is less than */
/*   zero, then rules will be executed until the agenda  is   */
/*   empty.  If run limit is greater than zero, then rules    */
/*   will be executed until either the agenda is empty, the   */
/*   run limit has been reached, or a rule execution error    */
/*   has occured.  Returns the number of rules fired.         */
/**************************************************************/
run(run_limit)
  int run_limit;
  {

   struct test *commands;
   struct fbind *local_vars;
   struct activation *rule_to_fire;
   int rules_fired = 0;
   char print_space[20];
   struct values result;
   struct exec_func *exec_ptr;
   
   /*=====================================================*/
   /* Fire rules until the agenda is empty, the run limit */
   /* has been reached, or a rule execution error occurs. */
   /*=====================================================*/
   
   EXECUTION_ERROR = FALSE;
   executing = TRUE;
   
   while ((AGENDA != NULL) && 
          (run_limit != 0) &&
          (EXECUTION_ERROR == FALSE))
     {
        
      /*==========================*/
      /* Bookkeeping and Tracing. */
      /*==========================*/
       
      currentrule = AGENDA->rule;
      rules_fired++;
      if (run_limit > 0) { run_limit--; }

      if (get_rules_watch() == ON)
        {         
         sprintf(print_space,"FIRE %4d ",rules_fired);
         cl_print("wtrace",print_space);
         cl_print("wtrace",currentrule);
         cl_print("wtrace",": ");
         print_fact_basis("wtrace",AGENDA->basis->binds);
         cl_print("wtrace","\n");
        }
   
      if (get_crsv_trace_watch() == ON)
        {
         sprintf(print_space,"F    %-4d   ",rules_fired);
         cl_print("wcrsv_tr",print_space);
         cl_print("wcrsv_tr",currentrule);
	     cl_print("wcrsv_tr","\n");
        }
   
      /*=============================================*/
      /* Execute the rule's right hand side actions. */
      /*=============================================*/
    
      change_agenda = TRUE;
      rule_to_fire = AGENDA;  
      commands = AGENDA->actions;
      local_vars = AGENDA->basis->binds;
      AGENDA = AGENDA->next;     

      gbl_lhs_binds = local_vars;
      gbl_rhs_binds = NULL;

      commands = commands->arg_list;
      while ((commands != NULL) && (EXECUTION_ERROR == FALSE))
        {
         generic_compute(commands,&result);
         commands = commands->next_arg;
        }
      commands = NULL;

      /*========================================*/
      /* Return the agenda node to free memory. */
      /*========================================*/
      
      returnbinds(local_vars);
      rtn_struct(flink,rule_to_fire->basis);
      rtn_struct(activation,rule_to_fire); 

      /*============================================*/
      /* Remove retracted facts, ephemeral symbols, */
      /* variable bindings, and temporary segments. */
      /*============================================*/

      rmv_old_facts();
      rem_eph_symbols();
      flush_bind_list();
      flush_segments();
      
      /*=============================================*/
      /* Execute exec list after performing actions. */
      /*=============================================*/
      
      exec_ptr = exec_list;
      while (exec_ptr != NULL)
        {
         (*exec_ptr->ip)();
         exec_ptr = exec_ptr->next;
        }
     }
     
   executing = FALSE;
   EXECUTION_ERROR = FALSE;
   return(rules_fired);
  }

/***************************************************************/
/* ADD_ACTIVATION: Adds a rule activation to the agenda.  This */
/*   function is called when all the patterns on the LHS of a  */
/*   rule have been satisfied.                                 */
/***************************************************************/
add_activation(rule_name,salience,rhs,binds)
  char *rule_name;
  int salience;
  struct test *rhs;
  struct flink *binds;
  {
   struct activation *new_act, *act_ptr, *last_act;

   /*======================================================*/
   /* Create the activation.  Store the join connected to  */ 
   /* the actions in the rhs and the list of fact bindings */
   /* in the lhs.                                          */
   /*======================================================*/

   new_act = get_struct(activation);
   new_act->id = AGENDA_COUNT++;
   new_act->actions = rhs;
   new_act->basis = binds;
   new_act->rule = rule_name;
   new_act->salience = salience;
   new_act->next = NULL;

   /*====================================================*/
   /* If activations are being watch, display a message. */
   /*====================================================*/

   if (watch_activations == ON)
     { 
      cl_print("wtrace","==> Activation ");
      print_activation("wtrace",new_act); 
      cl_print("wtrace","\n");
     }
        
   if (get_crsv_trace_watch() == ON)
     {
      cl_print("wcrsv_tr","AC   ");
      print_crsv_activation("wcrsv_tr",new_act);
      cl_print("wcrsv_tr","\n");
     }

   change_agenda = TRUE;

   /*==============================================*/
   /* Place the rule activation at the appropriate */ 
   /* place in the agenda.                         */
   /*==============================================*/

   if (AGENDA == NULL)
     {
      AGENDA = new_act;
      return;
     }

   act_ptr = AGENDA; 
   last_act = NULL;

   while ((act_ptr != NULL) ? 
          (activation_salience(act_ptr) > salience) : FALSE)
     {
      last_act = act_ptr;
      act_ptr = act_ptr->next;
     }

   if (last_act == NULL)               
     {
      /* Place at the front of the agenda. */
      new_act->next = AGENDA;    
      AGENDA = new_act;
     }
   else if (act_ptr == NULL)           
     {
      /* Place at the end of the agenda. */ 
      last_act->next = new_act;     
     }
   else                           
     {
      /* Place in the middle of the agenda. */
      new_act->next = last_act->next;     
      last_act->next = new_act;
     }
     
  }
  
/***************************************************************/
/* REMOVE_ALL_ACTIVATIONS:                     */
/***************************************************************/
remove_all_activations()
  {
   struct activation *temp_agenda;
   char print_space[20];
   
   while (AGENDA != NULL)
     {
	  if (watch_activations == ON)
	    { 
         cl_print("wtrace","<== Activation ");
         print_activation("wtrace",AGENDA); 
         cl_print("wtrace","\n");
	    }
	  
	  if (get_crsv_trace_watch() == ON)
        {
         cl_print("wcrsv_tr","D    ");
         sprintf(print_space,"%-6d ",AGENDA->salience);
         cl_print("wcrsv_tr",print_space);
         cl_print("wcrsv_tr",AGENDA->rule);
         cl_print("wcrsv_tr","\n");
        } 
        
      change_agenda = TRUE; 
      returnbinds(AGENDA->basis->binds);
      rtn_struct(flink,AGENDA->basis);
      temp_agenda = AGENDA->next;
      rtn_struct(activation,AGENDA);
      AGENDA = temp_agenda;
     }
  }
  
/***************************************************************/
/* SET_AGENDA_COUNT:                     */
/***************************************************************/
set_agenda_count(num)
  int num;
  {
   AGENDA_COUNT = num;
  }
  
/*********************************************************************/
/* SET_ACTIVATIONS_WATCH: */
/*********************************************************************/
set_activations_watch(value)
  int value;
  {
   watch_activations = value;
  }
    
/*********************************************************************/
/* GET_ACTIVATIONS_WATCH: */
/*********************************************************************/
get_activations_watch()
  {
   return(watch_activations);
  }

/*****************************************************************/
/* PURGE_AGENDA: Removes all instantiations from the agenda that */
/*   were triggered by the fact with ID fact_num.                */
/*****************************************************************/
purge_agenda(fact_num)
  FACT_ID fact_num;
  {
   struct activation *temp, *past;
   char print_space[20];

   /*============================================*/
   /* Loop struct all activations on the agenda. */
   /*============================================*/

   temp = AGENDA;
   past = NULL;

   while (temp != NULL)
     {
      if (find_id(fact_num,temp->basis->binds) == TRUE)
        {
         /*===============================================*/
         /* Indicate removal of activation if activations */
         /* are being watched.                            */
         /*===============================================*/

	     if (watch_activations == ON)
	       { 
            cl_print("wtrace","<== Activation ");
            print_activation("wtrace",temp); 
            cl_print("wtrace","\n");
	       }
	       
         if (get_crsv_trace_watch() == ON)
           {
            cl_print("wcrsv_tr","D    ");
            sprintf(print_space,"%-6d ",temp->salience);
            cl_print("wcrsv_tr",print_space);
            cl_print("wcrsv_tr",temp->rule);
            cl_print("wcrsv_tr","\n");
           }   
        
         change_agenda = TRUE;

         /*================================*/
         /* Remove activation from agenda. */
         /*================================*/

         returnbinds(temp->basis->binds); 
         rtn_struct(flink,temp->basis);
	     temp->basis = NULL;
	  	 if (past == NULL)
	       {
	        AGENDA = AGENDA->next;       
	        rtn_struct(activation,temp);
	        temp = AGENDA;
	       }
	     else
	      {
	       past->next = temp->next;
	       rtn_struct(activation,temp);
	       temp = past->next;          
	      }
	    }
      else
	    {
         past = temp;
         temp = temp->next;   
        }
     }
  }

/**********************************************************/
/* CLEAR_RULE_FROM_AGENDA: clears the agenda of a specified rule name */
/**********************************************************/
clear_rule_from_agenda(rule_name)
  char *rule_name;
  {
   struct activation *agenda_ptr, *agenda_before;
   char print_space[20];

   agenda_ptr = AGENDA;
   agenda_before = NULL;

   /*==============================================*/
   /* Loop through every activation on the agenda. */
   /*==============================================*/

   while (agenda_ptr != NULL)
     {
      if (strcmp(agenda_ptr->rule,rule_name) == 0)
        {
         /*====================================================*/
         /* Indicate removal if activations are being watched. */
         /*====================================================*/
         
	     if (watch_activations == ON)
	       { 
            cl_print("wtrace","<== Activation ");
            print_activation("wtrace",agenda_ptr);
            cl_print("wtrace","\n"); 
	       }
	       
         if (get_crsv_trace_watch() == ON)
           {
            cl_print("wcrsv_tr","D    ");
            sprintf(print_space,"%-6d ",agenda_ptr->salience);
            cl_print("wcrsv_tr",print_space);
            cl_print("wcrsv_tr",agenda_ptr->rule);
            cl_print("wcrsv_tr","\n");
           }   

         change_agenda = TRUE;
         
         /*=====================================*/
         /* Remove rule activation from agenda. */
         /*=====================================*/
    
         returnbinds(agenda_ptr->basis->binds); 
         rtn_struct(flink,agenda_ptr->basis);

         if (agenda_before == NULL)
           {
	        AGENDA = agenda_ptr->next;
            rtn_struct(activation,agenda_ptr);
            agenda_ptr = AGENDA;
           }
         else
           {
	        agenda_before->next = agenda_ptr->next;
            rtn_struct(activation,agenda_ptr);
            agenda_ptr = agenda_before->next;
           }
        }
      else
        {
         agenda_before = agenda_ptr;
         agenda_ptr = agenda_ptr->next;
        }
     }
  }
  
/***************************************************/
/* GET_NEXT_ACTIVATION:                            */
/***************************************************/
struct activation *get_next_activation(act_ptr)
  struct activation *act_ptr;
  {
   if (act_ptr == NULL)
     { return(AGENDA); }
   else
     { return(act_ptr->next); }
  }
  
/***************************************************/
/* GET_CHANGE_AGENDA:                              */
/***************************************************/
int get_change_agenda()
  { return(change_agenda); }
  
/***************************************************/
/* SET_CHANGE_AGENDA:                              */
/***************************************************/
int set_change_agenda(value)
  int value;
  { 
   change_agenda = value;
  }
  
/****************************************************/
/* MOVE_ACTIVATION_TO_TOP: Moves the nth activation */
/*   to the top of the agenda.                      */
/****************************************************/
move_activation_to_top(n)
  int n;
  {
   struct activation *tmp_ptr, *prev_ptr;
   int i = 1;
   
   tmp_ptr = AGENDA;
   prev_ptr = NULL;
   
   while ((i != n) && (tmp_ptr != NULL))
     {
      prev_ptr = tmp_ptr;
      tmp_ptr = get_next_activation(tmp_ptr);
      i++;
     }
   
   if (tmp_ptr == NULL) return(0);
   
   if (tmp_ptr != AGENDA)
     {
      prev_ptr->next = tmp_ptr->next;
      tmp_ptr->next = AGENDA;
      AGENDA = tmp_ptr;
     }
     
   change_agenda = TRUE;
   return(1);
  }
  
/*************************************************/
/* DELETE_ACTIVATION: Deletes the nth activation */
/*   on the agenda.                              */
/*************************************************/
delete_activation(n)
  int n;
  {
   struct activation *tmp_ptr, *prev_ptr;
   int i = 1;
   
   tmp_ptr = AGENDA;
   prev_ptr = NULL;
   
   while ((i != n) && (tmp_ptr != NULL))
     {
      prev_ptr = tmp_ptr;
      tmp_ptr = get_next_activation(tmp_ptr);
      i++;
     }
   
   if (tmp_ptr == NULL) return(0);
   
   returnbinds(tmp_ptr->basis->binds); 
   rtn_struct(flink,tmp_ptr->basis);
   tmp_ptr->basis = NULL;
   if (prev_ptr == NULL)
	 {
	  AGENDA = AGENDA->next;       
	  rtn_struct(activation,tmp_ptr);
	 }
   else
	 {
	  prev_ptr->next = tmp_ptr->next;
	  rtn_struct(activation,tmp_ptr); 
	 }
     
   change_agenda = TRUE;
   return(1);
  }
  
/***************************************************/
/* PRINT_ACTIVATION:                         */
/***************************************************/
print_activation(log_name,act_ptr)
  char *log_name;
  struct activation *act_ptr;
  {
   char print_space[20];
   
   sprintf(print_space,"%-6d ",act_ptr->salience);
   cl_print(log_name,print_space);
   cl_print(log_name,act_ptr->rule);
   cl_print(log_name,": ");
   print_fact_basis(log_name,act_ptr->basis->binds);
  }
  
/***************************************************/
/* PRINT_CRSV_ACTIVATION:                          */
/***************************************************/
print_crsv_activation(log_name,act_ptr)
  char *log_name;
  struct activation *act_ptr;
  {
   char print_space[20];
   struct fbind *list;   

   sprintf(print_space,"%-6d ",act_ptr->salience);
   cl_print(log_name,print_space);
   cl_print(log_name,act_ptr->rule);
   cl_print(log_name,": ");
   
   list = act_ptr->basis->binds;
   while (list != NULL)
     {
      if (list->whoset >= 0)
        {
         sprintf(print_space,"%ld ",list->whoset);
         cl_print(log_name,print_space);
        }
      list = list->next;
     }
  }  
  
/***************************************************/
/* PRINT_AGENDA:                         */
/***************************************************/
printagenda()
  {
   struct activation *agenda_ptr;
  
   agenda_ptr = get_next_activation(NULL);
   while (agenda_ptr != NULL)
     { 
      print_activation("wagenda",agenda_ptr);
      cl_print("wagenda","\n");
      agenda_ptr = get_next_activation(agenda_ptr);
     }
  }
  
/***************************************************/
/* SET_EXECUTION_ERROR:                           */
/***************************************************/
set_execution_error(value)
  int value;
  {
   EXECUTION_ERROR = value;
  }
  
/***************************************************/
/* GET_EXECUTION_ERROR:                           */
/***************************************************/
get_execution_error()
  {
   return(EXECUTION_ERROR);
  }
  
/*************************************************************/
/* GET_CURRENTRULE:                                            */
/*************************************************************/
char *get_currentrule()
  {
   return(currentrule);
  }
  
/******************************************************/
/* ExecutingRule: Returns TRUE if a rule is currently */
/*   being executed, otherwise FALSE.                 */
/******************************************************/
ExecutingRule()
  { return(executing); }

/**************************************************************/
/* SetExecutingRule: Sets the value of the executing variable */
/*   indicating that actions such as reset, clear, etc should */
/*   not be performed.                                        */
/**************************************************************/
SetExecutingRule(value)
  int value;
  { 
   executing = value; 
  }

/*************************/
/* ADD_EXEC_FUNCTION:    */
/*************************/
add_exec_function(name,func_ptr)
  char *name;
  int (*func_ptr)();
  {
   struct exec_func *c_ptr;

   c_ptr = get_struct(exec_func);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_exec_function\n");
      return(0);
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = exec_list;
   exec_list = c_ptr;
   return(1);
  }
 
/****************************/
/* REMOVE_EXEC_FUNCTION:    */
/****************************/
remove_exec_function(name)
  char *name;
  {
   struct exec_func *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = exec_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { exec_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(exec_func,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }
