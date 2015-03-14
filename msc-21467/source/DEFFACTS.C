/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   DEFFACTS MODULE                   */
   /*******************************************************/
   
#include "setup.h"

#if DEFFACTS_CONSTRUCT

#include <stdio.h>

#include "constant.h"
#include "scanner.h"
#include "deffacts.h"
#include "access.h"
#include "clipsmem.h"
   
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   char                   *df_1st_phase();
   struct dfact           *find_deffact();
   struct dfact           *get_next_deffact();
   char                   *get_deffact_name();
   char                   *get_deffact_ppform();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   int                     parse_deffacts();
   int                     createinitial();
   int                     returnelements();
   int                     init_deffacts();
   int                     clear_deffacts();
   int                     undeffacts_command();
   int                     list_deffacts();
   int                     list_dfct_command();
   int                     ppdef_command();
   int                     save_deffacts();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char            *copy_pp_buffer();
   extern struct fact     *get_el();
   extern struct draw     *add_symbol();
   extern char            *check_name();
   extern struct test     *BuildRhsAssert();
   extern struct funtab   *find_function();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
  
   static struct token     inp_tkn;
   static int              deffacts_error;
   static struct dfact    *defptr = NULL;  
   static struct dfact    *deflist;

#if RUN_TIME

/*****************************/
/* SET_UP_DEFFACTS:          */
/*****************************/
set_up_deffacts()
  {
   add_reset_function("deffacts",init_deffacts);
  }

#else

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern int              CONSERVE_SPACE;
   extern int              LOAD_FLAG;

/*****************************/
/* SET_UP_DEFFACTS:          */
/*****************************/
set_up_deffacts()
  {
   createinitial();
   add_reset_function("deffacts",init_deffacts);
   add_clear_function("deffacts",clear_deffacts);
   add_save_function("deffacts",save_deffacts);
#if ! BLOAD_ONLY
   add_construct("deffacts",parse_deffacts);
#endif
   define_function("undeffacts",   'v', undeffacts_command, "undeffacts_command");
   define_function("list-deffacts",'v', list_dfct_command,  "list_dfct_command");
   define_function("ppdeffact",    'v', ppdef_command,      "ppdef_command");  
  }

/*****************************/
/* CLEAR_DEFFACTS:          */
/*****************************/
clear_deffacts()
  {
#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
   if (bloaded() == TRUE) return;
#endif

   remove_all_deffacts();
   createinitial();
  }
  
/*****************************/
/* SAVE_DEFFACTS:          */
/*****************************/
save_deffacts(log_name)
  char *log_name;
  {
   struct dfact *def_ptr;
   char *ppform;
   
   def_ptr = get_next_deffact(NULL);
   while (def_ptr != NULL)
     {
      ppform = get_deffact_ppform(def_ptr);
      if (ppform != NULL)
        {
         print_in_chunks(log_name,ppform);
         cl_print(log_name,"\n");
        }
      def_ptr = get_next_deffact(def_ptr);
     }
  }

#if ! BLOAD_ONLY
/***************************************************************/
/* PARSE_DEFFACTS:  The purpose of this function is to parse   */
/*   the deffacts statement into a list of facts which can be  */
/*   asserted when a reset is performed.  The name of the      */ 
/*   deffacts block is saved along with each fact for use with */
/*   the undeffacts statement.                                 */
/***************************************************************/
int parse_deffacts(read_source)
  char *read_source;
  {
   char *df_name;
   struct test *temp;
   struct dfact *new_dfact;

   deffacts_error = FALSE;
   set_pp_buffer_status(ON);

   flush_pp_buffer();          
   set_indent_depth(3);    
   save_pp_buffer("(deffacts ");

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (bloaded() == TRUE) 
     {
      cl_print("werror","\nCannot load deffacts with binary load in effect.\n");
      return(TRUE);
     }
#endif

   /*====================================================*/
   /* Parse the name and comment fields of the deffact.  */
   /* Excise the deffact if it already exists.           */
   /*====================================================*/  

   df_name = df_1st_phase(read_source);
   if (deffacts_error == TRUE) { return(deffacts_error); } 
   
   /*========================================*/
   /* Check that next token is a '(' or ')'. */
   /*========================================*/
   
   if ((inp_tkn.token != LPAREN) && (inp_tkn.token != RPAREN))
     {
      cl_print("werror","Expected RHS pattern to begin with a '('\n");
      deffacts_error = TRUE;
      return(deffacts_error);
     }

   if (inp_tkn.token == LPAREN)
     {
      temp = get_struct(test);
      temp->type = FCALL;
      temp->val.fun_ptr = find_function("assert");
      temp->next_arg = NULL;
      temp->arg_list = NULL;
      temp = BuildRhsAssert(temp,read_source,FALSE,&deffacts_error);

      if (deffacts_error == TRUE) { return(deffacts_error); }
   
      if (variables_in_expression(temp))
        {
         cl_print("werror","Variables cannot be accessed by a deffacts\n");
         returntests(temp);
         return(TRUE);
        }
     }
   else
     { 
      pp_backup();
      pp_backup();
      save_pp_buffer(")");
      temp = NULL; 
     }
   
   save_pp_buffer("\n");

   test_install(temp);
   new_dfact = get_struct(dfact);
   new_dfact->name = gm2(sizeof(char) * (strlen(df_name) + 1));
   strcpy(new_dfact->name,df_name);
   new_dfact->alist = temp;
   new_dfact->next = NULL;

   if (CONSERVE_SPACE == TRUE)
     { new_dfact->pp_form = NULL; }
   else
     { new_dfact->pp_form = copy_pp_buffer(); } 
   
   if (defptr == NULL)
     { deflist = new_dfact; }
   else
     { defptr->next = new_dfact; }
   defptr = new_dfact;

   return(deffacts_error);       
  }

/*********************************************************************/
/* df_1st_phase:                                                     */
/*********************************************************************/
static char *df_1st_phase(read_source)
  char *read_source;
  {
   char *df_name;

   /*===========================================================*/
   /* Get next token, which should be the name of the deffacts. */
   /*===========================================================*/


   gettoken(read_source,&inp_tkn);
   if (inp_tkn.token != WORD)
     { 
      cl_print("werror","\nMissing deffacts name\n");
      deffacts_error = TRUE;
      return(NULL);
     }
   
   df_name = inp_tkn.tknword;

   if ((delete_deffacts(df_name) == TRUE) && (get_compilations_watch() == ON))
     {
      cl_print("wdialog","Removing deffacts block ");
      cl_print("wdialog",df_name);
      cl_print("wdialog","\n");
     }

   /*==========================================================*/
   /* If watch rules is on, indicate deffacts being processed. */
   /*==========================================================*/

   if ((get_compilations_watch() == ON) && (LOAD_FLAG == TRUE))
     {
      cl_print("wdialog","Processing deffacts block ");
      cl_print("wdialog",df_name);
      cl_print("wdialog","\n");
     }
   else if (LOAD_FLAG == TRUE)
     { cl_print("wdialog","$"); }

   /*===========================*/
   /* Get comment if it exists. */
   /*===========================*/

   gettoken(read_source,&inp_tkn);
   if (inp_tkn.token == STRING)
     {
      pp_backup();
      save_pp_buffer(" ");
      save_pp_buffer(inp_tkn.print_rep);
      save_pp_buffer("\n   ");           
      gettoken(read_source,&inp_tkn);
     }
   else                                 
     {
      pp_backup();
      save_pp_buffer("\n   ");
      save_pp_buffer(inp_tkn.print_rep);
     }

   return(df_name);
  }
#endif
#endif 

/**************************************************************/
/* init_deffacts:  Copies the deffacts list to the fact list. */
/**************************************************************/
init_deffacts()
  {
   struct dfact *def_ptr;
   struct values result;

   SetExecutingRule(TRUE);
   def_ptr = deflist;
   while (def_ptr != NULL)
     {
      if (def_ptr->alist != NULL)
        { generic_compute(def_ptr->alist,&result); }
      def_ptr = def_ptr->next;
     }
   SetExecutingRule(FALSE);
  }
  
#if ! RUN_TIME

/*************************************************************/
/* createinitial:  Creates the initial fact, (initial-fact), */
/*   and places it on the deffacts list.                     */
/*************************************************************/
createinitial()
  {
   struct test *stub;
   struct dfact *new_dfact;

   /*==========================*/
   /* Create the initial fact. */
   /*==========================*/

   stub = get_struct(test);
   stub->type = FCALL;
   stub->val.fun_ptr = find_function("assert");
   stub->next_arg = NULL;
   stub->arg_list = get_struct(test);
   stub->arg_list->type = WORD;
   stub->arg_list->val.hvalue = add_symbol("initial-fact");
   stub->arg_list->next_arg = NULL;
   stub->arg_list->arg_list = NULL;

   test_install(stub);

   new_dfact = get_struct(dfact);
   new_dfact->name = gm2(sizeof(char) * (strlen("initial-fact") + 1));
   strcpy(new_dfact->name,"initial-fact");
   new_dfact->alist = stub;
   new_dfact->next = NULL;
   new_dfact->pp_form = NULL;

   defptr = deflist = new_dfact;  
  }

/***************************************************************/
/* REMOVE_ALL_DEFFACTS:                  */
/***************************************************************/
remove_all_deffacts()
  {
   struct test *stub;
   struct dfact *next_def;
   
   while (deflist != NULL)
     {
      stub = deflist->alist;
      test_deinstall(stub);
      returntests(stub);
      
      next_def = deflist->next;

      rm(deflist->name,sizeof(char) * (strlen(deflist->name) + 1));
      if (deflist->pp_form != NULL)
        {
         rm(deflist->pp_form,
                    sizeof(char) * (strlen(deflist->pp_form) + 1));
        }
      rtn_struct(dfact,deflist);

      deflist = next_def;
     }
   defptr = NULL;
  }
  
/*****************************************************/
/* delete_deffacts: Delete all facts in the deffacts */
/*   list which have the identifying name of the     */
/*   deffacts block to be removed.                   */
/*****************************************************/
delete_deffacts(deffacts_name)
   char *deffacts_name;
  {
   struct dfact *next_deffact, *last_deffact;
   struct dfact *cur_deffact;
   struct test *stub;
   
#if BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY
   if (bloaded() == TRUE) return(0);
#endif

   last_deffact = NULL;
   cur_deffact = deflist;
   while (cur_deffact != NULL)
     {
      next_deffact = cur_deffact->next;
      if (strcmp(cur_deffact->name,deffacts_name) == 0)
        {
         stub = cur_deffact->alist;
         test_deinstall(stub);
         returntests(stub);

         if (last_deffact == NULL)
           { deflist = next_deffact; }
         else
           { last_deffact->next = next_deffact; }

         if (defptr == cur_deffact)
           { defptr = last_deffact; }

         rm(cur_deffact->name,sizeof(char) * (strlen(cur_deffact->name) + 1));
         if (cur_deffact->pp_form != NULL)
           {
            rm(cur_deffact->pp_form,
                    sizeof(char) * (strlen(cur_deffact->pp_form) + 1));
           }
         rtn_struct(dfact,cur_deffact);
         return(1);
        }
  
      last_deffact = cur_deffact;
      cur_deffact = next_deffact;
     }

   return(0); 
  }

#endif

/******************************************************************/
/* FIND_DEFFACT:  Searches for a deffact in the list of deffacts. */
/*   Returns a pointer to the deffact if found, otherwise NULL.   */
/******************************************************************/
struct dfact *find_deffact(df_name)
  char *df_name;
  {
   struct dfact *df_ptr;

   df_ptr = deflist;
   while (df_ptr != NULL)
     {
      if (strcmp(df_ptr->name,df_name) == 0)
        { return(df_ptr); }
      
      df_ptr = df_ptr->next; 
     }

   return(NULL);
  }

/************************************************************/
/* SET_DEFLIST:                            */
/************************************************************/
set_deflist(def_ptr)
  struct dfact *def_ptr;
  {
   deflist = def_ptr; 
  }
  
/************************************************************/
/* get_next_deffact:                            */
/************************************************************/
struct dfact *get_next_deffact(def_ptr)
  struct dfact *def_ptr;
  {
   if (def_ptr == NULL)
     { return(deflist); }
   else
     { return(def_ptr->next); }
  }

/************************************************************/
/* get_deffact_name:                            */
/************************************************************/
char *get_deffact_name(def_ptr)
  struct dfact *def_ptr;
  { return(def_ptr->name); }
  
/************************************************************/
/* get_deffact_ppform:                            */
/************************************************************/
char *get_deffact_ppform(def_ptr)
  struct dfact *def_ptr;
  { return(def_ptr->pp_form); }

/**********************************************************/
/* pp_deffact: the driver which actually does the pretty  */
/*   printing of the deffact.                             */
/**********************************************************/
pp_deffact(df_name,fileid)
  char *df_name, *fileid;
  {
   struct dfact *df_ptr;

   df_ptr = find_deffact(df_name);
   if (df_ptr == NULL)        
     {
      cl_print("werror","Unable to find deffact ");
      cl_print("werror",df_name);
      cl_print("werror","\n");
      return(FALSE);
     }
     
   if (get_deffact_ppform(df_ptr) == NULL) return(TRUE);
   print_in_chunks(fileid,get_deffact_ppform(df_ptr));
   return(TRUE);
  }
  
#if ! RUN_TIME

/*****************************************************/
/* undeffacts_command: removes a deffacts statement. */
/*   Syntax: (undeffacts <deffacts name>)            */
/*****************************************************/
int undeffacts_command()
  {
   char *deffacts_name;

   deffacts_name = check_name(1,1,"undeffacts","deffacts name");
   if (deffacts_name == NULL) return(0);

   if (delete_deffacts(deffacts_name) == FALSE)
     { 
      cl_print("werror","Unable to find deffacts block named ");
      cl_print("werror",deffacts_name);
      cl_print("werror","\n");
      return(0);
     }

   return(1);
  }

/************************************************************/
/* ppdef_command: pretty prints a deffact.                  */
/*   Syntax: (pp_deffact <deffact name>)                    */
/************************************************************/
int ppdef_command()
  {
   char *df_name;

   df_name = check_name(1,1,"ppdeffact","deffact name");
   if (df_name == NULL) return(0);

   pp_deffact(df_name,"wdisplay");

   return(1);
  }

/************************************************************/
/* list_deffacts: displays the list of deffacts.            */
/*   Syntax: (list_deffacts)                                */
/************************************************************/
int list_deffacts()
  {
   struct dfact *df_ptr;
   char *name;

   df_ptr = get_next_deffact(NULL);
   while (df_ptr != NULL)
     {
      name = get_deffact_name(df_ptr);
      if (name != NULL)
        {
         cl_print("wdisplay",name);
         cl_print("wdisplay","\n");
        }
      df_ptr = get_next_deffact(df_ptr);
     }

   return(1);
  }

/************************************************************/
/* list_dfct_command: displays the list of deffacts.        */
/*   Syntax: (list_deffacts)                                */
/************************************************************/
list_dfct_command()
  {
   if (arg_num_check("list-deffacts",EXACTLY,0) == -1) return;

   list_deffacts();
  }

  
#else

   int                        undeffacts_command() {};
   int                        list_dfct_command() {};
   int                        ppdef_command() {};
   
#endif

  
#endif
  
