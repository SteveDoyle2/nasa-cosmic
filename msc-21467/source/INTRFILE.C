/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                INTERFACE FILE MODULE                */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "rule.h"
#include "scanner.h"

#if ! RUN_TIME

/************************************************************/
/* SAVE_FUNC STRUCTURE:                                     */
/************************************************************/
struct save_func
  {
   char *name;  
   int (*ip)();
   struct save_func *next;
  };
  
/************************************************************/
/* BATCH_QUEUE STRUCTURE:                                   */
/************************************************************/

struct batch_entry
  {
   FILE *fp;
   struct batch_entry *next;
  };

/*******************************/
/* TRACE AND BATCH DEFINITIONS */
/*******************************/

#define BAT_SIZE 120
#define TRACE_SIZE 120
#define BUFFSIZE       80           /* Buffer allocation increments  */

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                        batch_on();
   int                        trace_on();
   int                        trace_off();
   int                        save_command();
   int                        load_command();
   int                        svfcts_command();
   int                        ldfcts_command();
   int                        bload_command();
   int                        bsave_command();
   int			              crsv_trace_on();
   int			              get_crsv_trace_watch();
   int			              crsv_trace_off();
   int                        set_dribble_status_function();

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char              *get_f_name();
   extern int                arg_type_check();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static FILE              *trace_fp = NULL;
   static FILE              *batch_fp = NULL;
   static char               bat_buff[BAT_SIZE];
   static char               trace_buff[TRACE_SIZE];
   static int                bat_loc;
   static int                trace_loc;
   static struct save_func  *save_list = NULL;
   static FILE		        *crsv_trace_fp = NULL;
   static int                crsv_trace_flag= OFF;
   static int               (*dribble_status_function)() = NULL;
   static struct batch_entry *batch_top = NULL;
   static struct batch_entry *batch_bot = NULL;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/
   
   extern int                 LOAD_FLAG;

/************************/
/* INIT_INTERFACE_FILE: */
/************************/
init_interface_file()
  {
   define_function("batch",         'v', batch_on,       "batch_on");
   define_function("dribble-on",    'v', trace_on,       "trace_on");
   define_function("dribble-off",   'v', trace_off,      "trace_off");
   define_function("crsv-trace-on", 'v', crsv_trace_on,  "crsv_trace_on");
   define_function("crsv-trace-off",'v', crsv_trace_off, "crsv_trace_off");
   define_function("save",          'v', save_command,   "save_command");
   define_function("load",          'v', load_command,   "load_command");
#if SAVE_FACTS
   define_function("save-facts",    'v', svfcts_command, "svfcts_command");
   define_function("load-facts",    'v', ldfcts_command, "ldfcts_command");
#endif
#if BLOAD_AND_BSAVE
   define_function("bsave",'v',bsave_command,"bsave_command");
#endif
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   define_function("bload",'v',bload_command,"bload_command");
#endif
  }
  
/*********************/
/* TRACING FUNCTIONS */
/*********************/

/*************************************************/
/* FIND_TRACE:                                   */
/*************************************************/
find_trace(log_name)
  char *log_name;
  {
   if ( (strcmp(log_name,"stdout") == 0) ||
        (strcmp(log_name,"stdin") == 0) ||
        (strcmp(log_name,"wclips") == 0) ||
        (strcmp(log_name,"wtrace") == 0) ||
        (strcmp(log_name,"wagenda") == 0) ||
        (strcmp(log_name,"werror") == 0) ||
        (strcmp(log_name,"wdisplay") == 0) ||
        (strcmp(log_name,"wdialog") == 0) )
     { return(TRUE); }

    return(FALSE);
  }

/*************************************************/
/* PRINT_TRACE:                                  */
/*************************************************/
print_trace(log_name,str)
  char *log_name, *str;
  {
   int i = 0;
     for (i = 0 ; str[i] != EOS ; i++)
    { add_character_to_trace_buffer(str[i]); }

   deact_router("trace");
   cl_print(log_name,str);
   act_router("trace");
  }

/*************************************************/
/* GETC_TRACE:                                   */
/*************************************************/
getc_trace(log_name)
  char *log_name;
  {
   int rv;
 
   deact_router("trace");
   rv = cl_getc(log_name);
 
   act_router("trace");
 
   add_character_to_trace_buffer(rv);
 
   return(rv);         
  }

/*************************************************/
/* ADD_CHARACTER_TO_TRACE_BUFFER:                */
/*************************************************/
add_character_to_trace_buffer(rv)
  int rv;
  {
   char nc;

   if (trace_loc >= TRACE_SIZE)
     {
      fprintf(trace_fp,"%s",trace_buff);
      trace_loc = 0;
     }
 
   if (rv == EOF) nc = '\n';
   else nc = (char) rv;
 
   if (nc == '\b')
     {
      if (trace_loc > 0) trace_loc--;
      trace_buff[trace_loc] = EOS;
      return;
     }

   trace_buff[trace_loc] = nc;
   trace_loc++;
   trace_buff[trace_loc] = EOS;
 
   if ((nc == '\n') || (nc == '\r'))
     {
      fprintf(trace_fp,"%s",trace_buff);
      trace_loc = 0;
     }
  }

/*************************************************/
/* UNGETC_TRACE:                                 */
/*************************************************/    
ungetc_trace(ch,log_name)
  int ch;
  char *log_name;
  {
   int rv;

   deact_router("trace");
   if (trace_loc > 0) trace_loc--;
   trace_buff[trace_loc] = EOS;
   rv = cl_ungetc(ch,log_name);
   act_router("trace");

   return(rv);
  }

/*************************************************/
/* EXIT_TRACE:                                   */
/*************************************************/
exit_trace(num)
  int num;
  {
   if (trace_fp != NULL) fclose(trace_fp);
  }

/*************************************************/
/* TRACE_ON:                                     */
/*************************************************/
int trace_on()
  {
   char *file_name;

   if ((file_name = get_f_name("dribble-on")) == NULL)
     { return(0); }
  
   return(open_dribble(file_name));
  }
  
/*************************************************/
/* OPEN_DRIBBLE:                                 */
/*************************************************/
open_dribble(file_name)
  char *file_name;
  {
   if (trace_fp != NULL)
     { close_dribble(); }
 
   trace_fp = fopen(file_name,"w");
   if (trace_fp == NULL)
     {
      open_error_message("trace",file_name);     
      return(0);
     }

   add_router("trace",           /* Router name     */
              40,                /* Priority        */
              find_trace,        /* Query function  */
              print_trace,       /* Print function  */
              getc_trace,        /* Getc function   */
              ungetc_trace,      /* Ungetc function */
              exit_trace);       /* Exit function   */

   trace_loc = 0;

   if (dribble_status_function != NULL)
     { (*dribble_status_function)(TRUE); }
     
   return(1);
  }
  
/*************************************************/
/* DRIBBLE_ACTIVE:                               */
/*************************************************/
dribble_active()
  {
   if (trace_fp != NULL) return(TRUE);
   
   return(FALSE);
  }

/*************************************************/
/* TRACE_OFF:                                    */
/*************************************************/
int trace_off()
  {
   if (arg_num_check("dribble-off",EXACTLY,0) == -1) return(0);
   return close_dribble();
  }

/*************************************************/
/* CLOSE_DRIBBLE:                                */
/*************************************************/
int close_dribble()
  {   
   if (dribble_status_function != NULL)
     { (*dribble_status_function)(FALSE); }
     
   if (trace_fp != NULL)
     { 
      del_router("trace");
      if (fclose(trace_fp) == 0)
        {
         trace_fp = NULL; 
         return(1); 
        }
     }
 
   trace_fp = NULL;
   return(0);
  }
  
/*******************************/
/* SET_DRIBBLE_STATUS_FUNCTION */
/*******************************/
int set_dribble_status_function(fnptr)
  int (*fnptr)();
  {
   dribble_status_function = fnptr;
  }
  
/*******************/
/* BATCH FUNCTIONS */
/*******************/


/*************************************************/
/* FIND_BATCH:                                   */
/*************************************************/
find_batch(log_name)
  char *log_name;
  {
   if (strcmp(log_name,"stdin") == 0)
     { return(TRUE); }

   return(FALSE);
  }

/*************************************************/
/* GETC_BATCH:                                   */
/*************************************************/
getc_batch(log_name)
  char *log_name;
  {
   return(ll_getc_batch(log_name,FALSE));
  }
  
/*************************************************/
/* LL_GETC_BATCH:                                   */
/*************************************************/
ll_getc_batch(log_name,return_on_eof)
  char *log_name;
  int return_on_eof;
  {
   int rv = EOF, flag = 1;

   /*=================================================*/
   /* Get a character until a valid character appears */
   /* or no more batch files are left.                */
   /*=================================================*/
   
   while ((rv == EOF) && (flag == 1))
     {
      rv = getc(batch_fp);
      if (rv == EOF) flag = remove_batch();
     }

   if ((rv == EOF) || (bat_loc >= BAT_SIZE))
     {
      if (bat_loc >= BAT_SIZE)
        { cl_print("werror","Batch file buffer overflowed\n"); }
      else if (bat_loc > 0)
        { cl_print("stdout",(char *) bat_buff); }
      del_router("batch");
      remove_batch();
      if (return_on_eof == TRUE)
        { return (EOF); }
      else
        { return(cl_getc(log_name)); }
     }

   bat_buff[bat_loc] = (char) rv;
   bat_loc++;
   bat_buff[bat_loc] = EOS;  

   if (((char) rv == '\n') || ((char) rv == '\r'))
     {
      cl_print("stdout",(char *) bat_buff);
      bat_loc = 0;
     }

   return(rv);
  }
    
/*************************************************/
/* UNGETC_BATCH:                                 */
/*************************************************/
ungetc_batch(ch,log_name)
  int ch, *log_name;
  {
   if (bat_loc > 0) bat_loc--;
   bat_buff[bat_loc] = EOS;
   return(ungetc(ch,batch_fp));
  }

/*************************************************/
/* EXIT_BATCH:                                   */
/*************************************************/
exit_batch(num)
  int num;
  { 
   fclose(batch_fp); 
   batch_fp = NULL;
  }

/*************************************************/
/* BATCH_ON:                                     */
/*************************************************/
int batch_on()
  {
   char *file_name;

   if ((file_name = get_f_name("batch")) == NULL) return;
   
   open_batch(file_name,FALSE);
  }
  
/*************************************************/
/* OPEN_BATCH:                                   */
/*************************************************/
open_batch(file_name,place_at_end)
  char *file_name;
  int place_at_end;
  {
   FILE *temp_fp;
   
   temp_fp = fopen(file_name,"r");

   if (temp_fp == NULL)
     { 
      open_error_message("batch",file_name);
      return(0);
     }

   if (batch_top == NULL)
     {
      add_router("batch",           /* Router name     */
                 20,                /* Priority        */
                 find_batch,        /* Query function  */
                 NULL,              /* Print function  */
                 getc_batch,        /* Getc function   */
                 ungetc_batch,      /* Ungetc function */
                 exit_batch);       /* Exit function   */
     }

   add_batch(place_at_end,temp_fp);

   return(1);
  }	
  
/***************************************************************/
/* add_batch                                                   */
/***************************************************************/
int add_batch(place_at_end,fptr)
  int place_at_end;
  FILE *fptr;
  {
   struct batch_entry *bptr;

   /*=========================*/
   /* Create the batch entry, */
   /*=========================*/
   
   bptr = get_struct(batch_entry);
   bptr->fp = fptr;
   bptr->next = NULL;
   
   if (batch_top == NULL)
     {
      batch_top = bptr;
      batch_bot = bptr;
      batch_fp = fptr;
      bat_loc = 0;
     }
   else if (place_at_end == FALSE)
     {
      bptr->next = batch_top;
      batch_top = bptr;
      batch_fp = fptr;
      bat_loc = 0;
     }
   else
     {
      batch_bot->next = bptr;
      batch_bot = bptr;
     }
  }
  

/***************************************************************/
/* remove_batch                                                */
/***************************************************************/
int remove_batch()
  {
   struct batch_entry *bptr;
   int rv;

   if (batch_top == NULL) return(0);
   
   fclose(batch_top->fp);
   
   bptr = batch_top;
   batch_top = batch_top->next;
   
   rtn_struct(batch_entry,bptr);
   
   if (batch_top == NULL)
     { 
      batch_bot = NULL;
      batch_fp = NULL;
      bat_loc = 0;
      rv = 0;
     }
   else
     { 
      batch_fp = batch_top->fp; 
      bat_loc = 0;
      rv = 1;
     }
     
   return(rv);
  }

  
/*************************************************/
/* BATCH_ACTIVE: */
/*************************************************/
batch_active()
  {
   if (batch_top != NULL) return(TRUE);
   
   return(FALSE);
  }

/************************/
/* CRSV TRACE FUNCTIONS */
/************************/

/*************************************************/
/* FIND_CRSV_TR:                                 */
/*************************************************/
find_crsv_tr(log_name)
  char *log_name;
  {
   if (strcmp(log_name,"wcrsv_tr") == 0) return(TRUE);
    
   return(FALSE);
  }

/*************************************************/
/* PRINT_CRSV_TR:                                */
/*************************************************/
print_crsv_tr(log_name,str)
  char *log_name, *str;
  {
   fprintf(crsv_trace_fp,"%s",str);
  }

/*************************************************/
/* EXIT_CRSV_TR:                                 */
/*************************************************/
exit_crsv_tr(num)
  int num;
  {
   if (crsv_trace_fp != NULL) fclose(crsv_trace_fp);
  }
  
/*************************************************/
/* CRSV_TRACE_ON:                                */
/*************************************************/
int crsv_trace_on()
  {
   char *file_name;

   if ((file_name = get_f_name("crsv-trace-on")) == NULL)
     { return; }
   
   open_crsv_trace(file_name);
  }

/*************************************************/
/*  GET_CRSV_TRACE_WATCH:                           */
/*************************************************/  
int get_crsv_trace_watch()
  {
   return(crsv_trace_flag);
  }

/*************************************************/
/* OPEN_CRSV_TRACE:                                 */
/*************************************************/
open_crsv_trace(file_name)
  char *file_name;
  {
   if (crsv_trace_fp != NULL)
     { close_crsv_trace(); }
 
   crsv_trace_fp = fopen(file_name,"w");
   if (crsv_trace_fp == NULL)
     {
      open_error_message("crsv_tr",file_name);     
      crsv_trace_flag= OFF;
      return(0);
     }

   add_router("crsv_tr",           /* Router name     */
              40,                  /* Priority        */
              find_crsv_tr,        /* Query function  */
              print_crsv_tr,       /* Print function  */
              NULL,                /* Getc function   */
              NULL,                /* Ungetc function */
              exit_crsv_tr         /* exit function   */
             );

   cl_print("wcrsv_tr","CRSV>");
   cl_print("wcrsv_tr","\n");
   
   crsv_trace_flag= ON;
   return(1);
  }
  
/*************************************************/
/* CRSV_TRACE_ACTIVE:                               */
/*************************************************/
crsv_trace_active()
  {
   if (crsv_trace_fp != NULL) return(TRUE);
	
   return(FALSE);
  }

/*************************************************/
/* CRSV_TRACE_OFF:                                  */
/*************************************************/
int crsv_trace_off()
  {
   if (arg_num_check("crsv-trace-off",EXACTLY,0) == -1) 
	  { return; }
	  
   close_crsv_trace();
  }

/*************************************************/
/* CLOSE_CRSV_TRACE:                                */
/*************************************************/
int close_crsv_trace()
  {
   if (crsv_trace_fp != NULL)
     { 
      del_router("crsv_tr");
      fclose(crsv_trace_fp);
     }
     
   crsv_trace_fp = NULL;
   crsv_trace_flag= OFF;
   
   return(1);
  }

/**************************/
/* LOAD AND SAVE COMMANDS */
/**************************/

/***************************************************************/
/* load_command: parses the load command and calls load_rules  */
/*   which will load a set of rules from a file.               */
/*   Syntax:  (load <file-name>)                               */
/***************************************************************/
int load_command()
  {
#if (! BLOAD_ONLY)
   VALUE arg_ptr;
   char *file_found;

   if (arg_num_check("load",EXACTLY,1) == -1) return(0);

   if (arg_type_check("load",1,STRING,&arg_ptr) == FALSE) return(0);

   file_found = get_valstring(arg_ptr);
   
   LOAD_FLAG = TRUE;

   if (load_rules(file_found) == -1) 
     {
      LOAD_FLAG = FALSE; 
      open_error_message("load",file_found);
      return(0); 
     }
   
   LOAD_FLAG = FALSE;
   return(1);
#else
   cl_print("wdisplay","Load is not available in this environment");
   cl_print("wdisplay"," - Use bload instead\n");
#endif
  }

/**********************************************/
/* save_command:  Executes the save commands. */
/*   Syntax:  (save <file-name>)              */ 
/**********************************************/ 
int save_command()
  {
   VALUE arg_ptr;
   char *file_found;

   if (arg_num_check("save",EXACTLY,1) == -1) return(0);
   
   if (arg_type_check("save",1,STRING,&arg_ptr) == FALSE) return(0);
   
   file_found = get_valstring(arg_ptr);
   
   if (save_rules(file_found) == FALSE) 
     {
      open_error_message("save",file_found);
      return(0);
     }
   
   return(1);
  }

/********************************************************/
/* save_rules:  Saves the current set of rules into the */
/*   specified file.                                    */ 
/********************************************************/ 
save_rules(file_found)
  char *file_found;
  {
   int file_open(), file_close();
   struct ruleinfo *rule_ptr;
   struct save_func *save_ptr;
   char *ppform;
   FILE *file_ptr;

   
   if ((file_ptr = fopen(file_found,"w")) == NULL)
     { return(FALSE); }
   set_fast_save(file_ptr);
   
   /*==========================*/
   /* Save unusual constructs. */
   /*==========================*/
   
   save_ptr = save_list;
   while (save_ptr != NULL)
     {
      (*save_ptr->ip)("** TeMp FiLe **");
      save_ptr = save_ptr->next;
     }

   /*=================*/
   /* Save the rules. */
   /*=================*/

   rule_ptr = get_next_rule(NULL);
   while (rule_ptr != NULL)
     {
      ppform = get_rule_ppform(rule_ptr);
      if (ppform != NULL)
        {
         print_in_chunks("** TeMp FiLe **",ppform);
         cl_print("** TeMp FiLe **","\n");
        }
      rule_ptr = get_next_rule(rule_ptr);
     }

   fclose(file_ptr);
   set_fast_save(NULL);
   
   return(TRUE);
  }
  
/*************************/
/* ADD_SAVE_FUNCTION:    */
/*************************/
add_save_function(name,func_ptr)
  char *name;
  int (*func_ptr)();
  {
   struct save_func *c_ptr;

   c_ptr = get_struct(save_func);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_exec_function\n");
      return(0);
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = save_list;
   save_list = c_ptr;
   return(1);
  }
 
/****************************/
/* REMOVE_SAVE_FUNCTION:    */
/****************************/
remove_save_function(name)
  char *name;
  {
   struct save_func *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = save_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { save_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(save_func,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }

#else

   int                        batch_on() {};
   int                        trace_on() {};
   int                        trace_off() {};
   int			              crsv_trace_on() {};
   int			              crsv_trace_off() {};
   int                        save_command() {};
   int                        load_command() {};
   int                        bsave_command() {};
   int                        bload_command() {};
   int                        add_save_function() {};
   int                        remove_save_function() {};
   int                        get_crsv_trace_watch() { return(FALSE); }
   
#endif

#if SAVE_FACTS

/******************************************************/
/* svfcts_command:  Executes the save-facts commands. */
/*   Syntax:  (save-facts <file-name>)                */ 
/******************************************************/ 
int svfcts_command()
  {
   VALUE arg_ptr;
   char *file_found;

   if (arg_num_check("save-facts",EXACTLY,1) == -1) return(0);

   if (arg_type_check("save-facts",1,STRING,&arg_ptr) == FALSE) return(0);

   file_found = get_valstring(arg_ptr);

   if (save_facts(file_found) == FALSE) 
     {
      open_error_message("save-facts",file_found);
      return(0);
     }

   return(1);
  }
  
/******************************************************/
/* ldfcts_command:  Executes the load-facts commands. */
/*   Syntax:  (load-facts <file-name>)                */ 
/******************************************************/ 
int ldfcts_command()
  {
   VALUE arg_ptr;
   char *file_found;

   if (arg_num_check("load-facts",EXACTLY,1) == -1) return(0);

   if (arg_type_check("load-facts",1,STRING,&arg_ptr) == FALSE) return(0);

   file_found = get_valstring(arg_ptr);

   if (load_facts(file_found) == FALSE) 
     {
      open_error_message("load-facts",file_found);
      return(0);
     }

   return(1);
  }
  

/********************************************************/
/* save_facts:  Saves the current set of facts into the */
/*   specified file.                                    */ 
/********************************************************/ 
save_facts(file_found)
  char *file_found;
  {
   int file_open(), file_close();   
   struct fact *list;
   FILE *file_ptr;
   
   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((file_ptr = fopen(file_found,"w")) == NULL)
     { return(FALSE); }
   
   set_fast_save(file_ptr);
     
   /*=================*/
   /* Save the facts. */
   /*=================*/

   list = get_next_fact(NULL); 
   while (list != NULL)
     {
      shw_elements("** TeMp FiLe **",list);
      list = get_next_fact(list);
     }
     
   /*=================*/
   /* Close the file. */
   /*=================*/
   
   fclose(file_ptr);
   set_fast_save(NULL);
     
   return(TRUE);
  }
  
/***********************************************/
/* SHW_ELEMENTS:  Displays elements of a fact. */
/***********************************************/
shw_elements(log_name,fact_ptr)
  char *log_name;
  struct fact *fact_ptr;
  {
   struct element *sublist;
   int length, i;
   
   sublist = fact_ptr->atoms;
   length = fact_ptr->fact_length;
   cl_print(log_name,"\(");
 
   for (i = 0; i < length ; i++)
     {
      prt_element(log_name,&sublist[i]);
      if (i + 1 != length)
        { cl_print(log_name," "); }
     }
   cl_print(log_name,"\)\n");
  }
 
/************************************************/
/* PRT_ELEMENT:                                 */
/************************************************/
prt_element(log_name,elem_ptr)
  char *log_name;
  struct element *elem_ptr;
  {
   char *pstrng;   
   extern char *str_print_rep();

   if (elem_ptr->type == NUMBER)
     { print_num(log_name,elem_ptr->val.fvalue); }
   else if (elem_ptr->type == WORD)
     { cl_print(log_name,symbol_string(elem_ptr->val.hvalue)); }
   else if (elem_ptr->type == STRING)
     {
      pstrng = str_print_rep(symbol_string(elem_ptr->val.hvalue));
      cl_print(log_name,pstrng);
     }
  }


/**********************************************/
/* load_facts:  Loads a set of facts from the */
/*   specified file.                          */ 
/**********************************************/ 
load_facts(file_found)
  char *file_found;
  {
   FILE *facts_file; 
   extern struct fact *add_fact(), *const_fact();
   struct fact *temp_fact;
   struct token input_tkn;

   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((facts_file = fopen(file_found,"r")) == NULL)
     { return(FALSE); }

   set_fast_load(facts_file);
     
   /*=================*/
   /* Load the facts. */
   /*=================*/

   input_tkn.token = 0;
   while (input_tkn.token != STOP)
     {
      temp_fact = const_fact("** TeMp FiLe **",&input_tkn);
      if (temp_fact != NULL)
        { add_fact(temp_fact); }
     }

   /*=================*/
   /* Close the file. */
   /*=================*/
   
   set_fast_load(NULL);
   fclose(facts_file);
     
   return(TRUE);
  }

/**********************************************/
/* const-fact:  Constructs the CLIPS internal */
/*    representation of a fact from the ascii */
/*    format produced by save-facts.          */
/**********************************************/
struct fact *const_fact(log_name,input_tkn)
  char *log_name;   
  struct token *input_tkn;
  {
   struct element *const_elm();
   struct fact *fact_rep();
   struct fact *new_fact;
   struct element *first_elm;

   gettoken("** TeMp FiLe **",input_tkn);
   if (input_tkn->token != LPAREN)
     { return(NULL); }

   gettoken(log_name,input_tkn);
   first_elm = const_elm(log_name,input_tkn);
   new_fact = fact_rep(first_elm);
   returnelements(first_elm);
   return(new_fact);
  }  


/*******************************************/
/* const_elm:  Constructs the elements for */
/*      a fact given the ascii representa- */
/*      tion of the fact.                  */
/*******************************************/
struct element *const_elm(log_name,input_tkn)
  char *log_name;
  struct token *input_tkn;
  {
   struct element *first_element, *last_element, *next_element;

   first_element = NULL;
   last_element = NULL;
 
   while ((input_tkn->token == WORD) || (input_tkn->token == NUMBER) || (input_tkn->token == STRING))
     { 
      next_element = get_struct(element);
      next_element->next = NULL;
      next_element->type = input_tkn->token;
      if (next_element->type == NUMBER)
        { next_element->val.fvalue = input_tkn->tknnumber; }
      else
        { next_element->val.hvalue = input_tkn->hashword; }
          
      if (last_element == NULL)                          
        { first_element = next_element; }
      else
        { last_element->next = next_element; }
      last_element = next_element;
      gettoken(log_name,input_tkn);
     }
 
   /*=============================================*/
   /* If the fact was not closed with a ')', then */
   /* an error has occured.                       */
   /*=============================================*/
 
   if (input_tkn->token != RPAREN)
     {
      cl_print("werror","load-facts expected a ')' to close a fact.\n");
      if (first_element != NULL) returnelements(first_element);
      return(NULL);
     }
 
   /*=======================================================*/
   /* If the fact has no fields, then an error has occured. */
   /* Otherwise, attach the linked list of fields to the    */
   /* fact structure.                                       */
   /*=======================================================*/
 
   if (first_element == NULL)
     {
      cl_print("werror","Null fact in load-facts input\n");
      return(NULL);
     }
 
   return(first_element);
  }

/***************************************************/
/* fact_rep:  Copies the elements in a linked list */
/*    to elements in an array and sets up a fact   */
/*    structure with to point to it.  No clean up  */
/*    of the original linked list is done.         */
/***************************************************/
struct fact *fact_rep(first_elm)
struct element *first_elm;
  {
   extern struct fact *get_el();
   struct element *elm_ptr, *new_ptr;
   struct fact *temp;
   int count;

   elm_ptr = first_elm;
   for(count = 0; elm_ptr != NULL; count++)
     { elm_ptr = elm_ptr->next; }

   temp = get_el(count);
   temp->list = NULL;
   temp->previous = NULL;
   temp->next = NULL;

   elm_ptr = first_elm;
   new_ptr = temp->atoms;

   for (count = 0; elm_ptr != NULL; count++)
     {
      new_ptr[count].val = elm_ptr->val;
      new_ptr[count].type = elm_ptr->type;
      elm_ptr = elm_ptr->next;
     }
 
   return(temp);
  }

#endif
