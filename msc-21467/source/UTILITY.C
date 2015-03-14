/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   UTILITY MODULE                    */
   /*******************************************************/
   
#include <stdio.h>

#include "clips.h"

/************************************************************/
/* RESET_FUNC STRUCTURE:                                     */
/************************************************************/
struct reset_func
  {
   char *name;  
   int (*ip)();
   struct reset_func *next;
  };
  
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
     
   struct fact_marker       *copy_marks();
   struct fact              *add_fact();
   struct fbind             *copy_binds();
   int                       print_fact_basis();
   int                       find_id();
   struct fbind             *newnid();
   int                       fact_compare();
   int                       createinitial();
   int                       reset_clips();
   int                       setnots();
   int                       flush_web();
   int                       clear_path();
   int                       rmv_side();
   int                       print_num();
   char                     *num_to_string();
   char                     *exp_line();
   char                     *append_to_string();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct draw       *add_symbol();
   extern struct pat_node   *network_pointer();
   extern struct fact       *get_el();
   extern char              *symbol_string();
   extern struct element    *fast_gv();

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/ 

   static long int           NID = -2;
   static struct fact       *segment_list = NULL;
   static struct reset_func *reset_list = NULL;
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/ 

   struct fbind             *gbl_lhs_binds = NULL;
   struct fbind             *gbl_rhs_binds = NULL;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct funtab     *PTR_GET_VAR;

/************************************************/
/* COPY_BINDS:  Copies a list of fact bindings. */
/************************************************/
struct fbind *copy_binds(list)
  struct fbind *list;
  {
   struct fbind *last = NULL;
   struct fbind *new, *head;

   for ( ; list != NULL ; list = list->next )
     {
      new = get_struct(fbind);
      new->next = NULL;
      new->origin = list->origin;
      new->whoset = list->whoset;
      if (list->marker != NULL) 
        { new->marker = copy_marks(list->marker); }
      else 
        { new->marker = NULL; }

      if (last == NULL)
        { head = last = new; }
      else
        { last = last->next = new; }
     }

   return(head);
  }

/************************************************************/
/* PRINT_FACT_BASIS: Prints out a list of fact id numbers   */
/*   associated with a partial match or rule instantiation. */
/************************************************************/
print_fact_basis(log_name,list)
  char *log_name;
  struct fbind *list;
  {
   char print_space[20];
   
   while (list != NULL)
     {
      if (list->whoset >= 0)
        {
         sprintf(print_space,"f-%ld",list->whoset);
         cl_print(log_name,print_space);
        }
      if (list->next != NULL) cl_print(log_name,",");
      list = list->next;
     }
     
  }

/***************************************************************/
/* FIND_ID: Searches for a fact id number in a list of fact id */
/*   numbers.  Returns TRUE (1) if the number is found in the  */
/*   list, otherwise FALSE (0) is returned.                    */
/***************************************************************/
find_id(id_number,id_list)
  FACT_ID id_number;
  struct fbind *id_list;
  {
   while (id_list != NULL)
     {
      if (id_list->whoset == id_number)
        { return(TRUE); }
      id_list = id_list->next; 
     }
   return(FALSE);
  }

/*****************************************************************/
/* COPY_MARKS:  Copies a list of structures indicating how many  */
/*   elements a segment variable or wildcard has bound.          */
/*****************************************************************/
struct fact_marker *copy_marks(mark_list)
  struct fact_marker *mark_list;
  {
   struct fact_marker *head = NULL, *last_mark = NULL, *new_mark;

   while (mark_list != NULL)
     {
      new_mark = get_struct(fact_marker);
      new_mark->next = NULL;
      new_mark->element = mark_list->element;
      new_mark->start = mark_list->start;
      new_mark->end = mark_list->end;

      if (last_mark == NULL)
        { head = new_mark; }
      else
        { last_mark->next = new_mark; }
      last_mark = new_mark;

      mark_list = mark_list->next;
     }
   
   return(head);
  }

/************************************************************/
/* NEWNID:  Creates a bind structure that indicates the     */
/*   fact to which a negated pattern has been bound.  Since */
/*   a non-existant fact has no fact id, the bind structure */
/*   is given a fake fact id (a unique negative integer).   */ 
/************************************************************/
struct fbind *newnid()
  {
   struct fbind *not_marker;

   not_marker = get_struct(fbind);
   not_marker->next = NULL;
   not_marker->origin = NULL;
   not_marker->marker = NULL;
   not_marker->whoset = NID;

   NID--;
   return(not_marker);
  }

/****************************************************************/
/* reset_clips: the purpose of this function is to reset the    */
/*   CLIPS environment.  The factlist is reset to the deffacts  */
/*   statements, the agenda is cleared, and the opnet is reset. */
/****************************************************************/
reset_clips()
  {
   struct reset_func *reset_ptr;
   
   /*========================================*/
   /* Function not allowed from RHS of rule. */
   /*========================================*/
   
   if (ExecutingRule()) 
     {
      cl_print("werror","WARNING: Reset command may not be performed ");
      cl_print("werror","during the execution of a rule\n");
      return;
     }
   
   /*====================================*/
   /* Initialize some global parameters. */
   /*====================================*/

   set_fact_id( (FACT_ID) -1);   
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

   /*============================================================*/
   /* Reset the not pattern counter and initialize not patterns. */
   /*============================================================*/

   NID = -2; 
   setnots(network_pointer());  

   /*============================*/
   /* Calls all reset functions. */
   /*============================*/

   reset_ptr = reset_list;
   while (reset_ptr != NULL)
     {
      (*reset_ptr->ip)();
      reset_ptr = reset_ptr->next;
     }
  }

/***********************************************/
/* flush_web: Removes all LHS and RHS bindings */
/*   from the join network.                    */
/*   Note:  Modified for join sharing.         */
/***********************************************/
flush_web(pat_net)
  struct pat_node *pat_net;
  {
   struct pat_node *pat_ptr;
   struct list *drive_list;

   pat_ptr = pat_net;
   while (pat_ptr != NULL)
     {
      if (pat_ptr->type == STOP)
        {
         rmv_side(pat_ptr->alpha);
         pat_ptr->alpha = NULL;
         drive_list = pat_ptr->path;
         while (drive_list != NULL)
          { 
           clear_path(drive_list->path);
           drive_list = drive_list->next;
          }
        }
      else
        { flush_web(pat_ptr->next_level); }
      pat_ptr = pat_ptr->same_level;
     }
  }

/**********************************************************/
/* clear_path:  Removes LHS and RHS bindings from a join, */
/*   and then calls itself recursively to remove bindings */
/*   from joins connected below this join.                */
/*   Note:  Modified for join sharing.                    */
/**********************************************************/
clear_path(join_ptr)
  struct internode *join_ptr;
  {
   struct list *list_of_joins;

   join_ptr->id = 0;
   if (join_ptr->beta != NULL)
     {
      rmv_side(join_ptr->beta);
      join_ptr->beta = NULL;
     }

   /*=========================================*/
   /* Call clear_path for each join connected */
   /* below this join.                        */
   /*=========================================*/

   list_of_joins = join_ptr->next;

   while (list_of_joins != NULL)
     { 
      clear_path(list_of_joins->path);
      list_of_joins = list_of_joins->next;
     }
  }

/*******************************************************/
/* rmv_side:  Returns all the binds in a list of binds */
/*   to the list of free binds.                        */
/*******************************************************/
rmv_side(pfl)
  struct flink *pfl;
  {
   struct flink *pfltemp;

   while (pfl != NULL)
     {
      returnbinds(pfl->binds);
      pfltemp = pfl->next;
      rtn_struct(flink,pfl);
      pfl = pfltemp;
     }
  }		

/************************************************************************/
/* setnots: The purpose of this code is to set the patterns that have   */
/*    a not condition before them to true until a pattern matches them. */
/************************************************************************/
setnots(pat_net)
  struct pat_node *pat_net;
  {
   struct list *list_of_entry_joins, *j_list_2;
   struct pat_node *pat_ptr;     
   struct internode *join;          
   struct flink *linker, *clinker;
   int join_test;

   pat_ptr = pat_net;

   while (pat_ptr != NULL)
     { 
      if (pat_ptr->type == STOP)
        {
         list_of_entry_joins = pat_ptr->path;
         while (list_of_entry_joins != NULL)
           {
            if ((list_of_entry_joins->path->rhs_log == '-') &&
                (list_of_entry_joins->path->lhs_log == 'e'))
              {
               join = list_of_entry_joins->path;
                  
               if (join->not_eval != NULL)
                 { join_test = join_compute(join->not_eval,NULL,NULL); }
               else
                 { join_test = TRUE; }
        
               if (join_test == TRUE)
                 {
                  linker = get_struct(flink);
                  linker->next = NULL;
                  linker->count = 0;
                  linker->binds = newnid();
                  join->id = linker->binds->whoset;
                  /*
                  join->id = NID;
                  NID--;
                  */

                  j_list_2 = join->next;
                  while (j_list_2->next != NULL)
                    {
                     clinker = get_struct(flink);
                     clinker->next = NULL;
                     clinker->count = 0;
                     clinker->binds = copy_binds(linker->binds);
                     drive(clinker,j_list_2->path,LHS);
                     j_list_2 = j_list_2->next;
                    }

                  drive(linker,j_list_2->path,LHS);
                 }
	          }
            list_of_entry_joins = list_of_entry_joins->next;
           }
        }
      else
        { setnots(pat_ptr->next_level); }
      pat_ptr = pat_ptr->same_level;
     }
  }
  
/************************************************************/
/* print_num:  Controls printout of floating point numbers. */
/************************************************************/
print_num(fileid,number)
  float number;
  char *fileid;
  {
   char *num_to_string(), *new_str;

   new_str = num_to_string(number);
   cl_print(fileid,new_str);

   return(1);
  }
  
/***************************************************/
/* print_long_int:  Controls printout of integers. */
/***************************************************/
print_long_int(fileid,number)
  long int number;
  char *fileid;
  {
   static char print_buff[16];
   
   sprintf(print_buff,"%ld",number);
   cl_print(fileid,print_buff);
  }

/***********************************************************/
/* num_to_string:  Converts number to clips string format. */  
/***********************************************************/
char *num_to_string(number)
  float number;
  {
   float tempa;
   int decimal, curr_dig, last_dig, count;
   static char num_str[20];

   if (number < 0)
     { tempa = - number; }
   else
     { tempa = number; }

   if ( ((tempa > 0) && (tempa < 0.0001)) ||
        (tempa > 99999) )     
     { 
      sprintf(num_str,"%6.4e",number); 
      return(num_str);
     }

   sprintf(num_str,"%-16.8f",number);

   decimal = 0;
   while (num_str[decimal] != '.') decimal++;

   curr_dig = decimal + 1;
   last_dig = decimal - 1;

   for (count = 0 ; count < 8 ; count++)
     { 
      if (num_str[curr_dig] != '0') last_dig = curr_dig;
      curr_dig++; 
     }

   last_dig++;
   num_str[last_dig] = EOS;
   return(num_str);
  }
  
/***********************************************************/
/* SET_NID: */
/***********************************************************/
set_nid(value)
  FACT_ID value;
  {
   NID = value;
  }
  
/*******************************************************************/
/* exp_line:  Allocates an additional 80 characters to the buffer. */
/*******************************************************************/
char *exp_line(line,line_max,new_max)
  char *line;
  int  line_max, new_max;
  {
   char *new_line;
    
   new_line = gm2(new_max * sizeof (char));
   if (new_line == NULL)
     { 
      cl_print("werror","Unable to expand line\n");
      cl_exit(1);
      return(NULL); 
     }

   if (line == NULL)
     { return(new_line); }
   
   strcpy(new_line,line);
   rm(line,sizeof (char) * line_max);
   return(new_line);
  }
  
/**********************************************************/
/* APPEND_TO_STRING:                                      */
/**********************************************************/
char *append_to_string(append_str,old_str,old_pos,old_max)
  char *append_str, *old_str;
  int *old_pos, *old_max;
  {
   int length;
   
   length = strlen(append_str);
   old_str = exp_line(old_str,*old_max,*old_max + length + 1);
   if (old_str == NULL) { return(NULL); }
     
   strcpy(&old_str[*old_pos],append_str);
   *old_max += (length + 1);
   *old_pos += length;
   
   return(old_str);
  }

/**************************************************************/
/* GET_LOG_NAME:             */
/**************************************************************/
char *get_log_name(position,t_str)
  int position;
  char *t_str;
  {
   char *log_id;
   struct values arg_ptr;
   struct draw *hash_ptr;
   
   runknown(position,&arg_ptr);

   if ((get_valtype(arg_ptr) == WORD) || (get_valtype(arg_ptr) == STRING))
     {
      log_id = arg_ptr.val.hvalue->contents;
      if ((strcmp(log_id,"t") == 0) || (strcmp(log_id,"T") == 0))
        { log_id = t_str; }
     }
   else if (get_valtype(arg_ptr) == NUMBER)
     {
      hash_ptr = add_symbol(num_to_string(get_valfloat(arg_ptr)));
      log_id = hash_ptr->contents; 
     }
   else
     { log_id = NULL; }
     
   return(log_id);
  } 
  
/*************************************************/
/* GET_F_NAME:                                   */
/*************************************************/
char *get_f_name(fun_name)
  char *fun_name;
  {
   int args;
   struct values arg_ptr;

   args = num_args();
   if (args != 1)
     { 
      exp_num_error(fun_name,EXACTLY,1);
      return(NULL);
     }

   runknown(1,&arg_ptr);
   if (get_valtype(arg_ptr) != STRING)
     {
      exp_type_error(fun_name,1,"file name");
      return(NULL);
     }
    
   return(get_valstring(arg_ptr));
  }

/*************************************************************/
/* CHECK_NAME:  For functions requiring a rule or deffacts   */
/*   name as an argument, this function checks for the right */
/*   number of arguments and checks to see that the argument */
/*   that is the name is of type word.                       */
/*************************************************************/
char *check_name(exp_args,name_pos,fun_name,type)
  int exp_args, name_pos;
  char *fun_name, *type;
  {
   int num_a; 
   struct values arg_ptr;

   num_a = num_args();
   if (num_a != exp_args)
     {
      exp_num_error(fun_name,EXACTLY,exp_args);
      return(NULL);
     }

   runknown(name_pos,&arg_ptr);

   if (get_valtype(arg_ptr) != WORD)
     { 
      exp_type_error(fun_name,name_pos,type);
      return(NULL);
     }

   return(get_valstring(arg_ptr));
  } 
  
/**********************************************************/
/* ARG_NUM_CHECK:  Checks that a function has the correct */
/*   number of arguments.                                 */
/**********************************************************/
arg_num_check(fun_name,check_val,exp_num)
  char *fun_name;
  int check_val, exp_num;
  {
   int num_a;

   num_a = num_args();
   if (check_val == EXACTLY)
     { if (num_a == exp_num) return(num_a); }
   else if (check_val == AT_LEAST)
     { if (num_a >= exp_num) return(num_a); }
   else if (check_val == NO_MORE_THAN)
     { if (num_a <= exp_num) return(num_a); }
   else
     {
      cl_print("werror","Function arg_num_check received an invalid argument\n");
      return(0);
     }

   exp_num_error(fun_name,check_val,exp_num);

   set_execution_error(TRUE);
   return(-1);
  }

/***********************************************************/
/* ARG_TYPE_CHECK:  Checks that a function has the correct */
/*   type for a particular argument.                       */
/***********************************************************/
int arg_type_check(fun_name,arg_num,exp_type,val_ptr)
  char *fun_name;
  int exp_type;
  int arg_num;
  struct values *val_ptr;
  {

   runknown(arg_num,val_ptr);
   
   if (val_ptr->type != exp_type)
     {  
      if (exp_type == NUMBER) exp_type_error(fun_name,arg_num,"number");
      else if (exp_type == WORD) exp_type_error(fun_name,arg_num,"word");
      else if (exp_type == STRING) exp_type_error(fun_name,arg_num,"string");
      else if (exp_type == MULTIPLE) exp_type_error(fun_name,arg_num,"multi-field");
        
      set_execution_error(TRUE);
      return(FALSE);
     }

   return(TRUE);
  }
  

/***********************************************************/
/* EXP_TYPE_ERROR:                               */
/***********************************************************/
exp_type_error(fun_name,arg_num,exp_type)
  char *fun_name;
  int arg_num;
  char *exp_type;
  {
   cl_print("werror","Function ");
   cl_print("werror",fun_name);
   cl_print("werror"," expected argument #");
   print_long_int("werror",(long int) arg_num);
   cl_print("werror"," to be of type ");
   cl_print("werror",exp_type);
   cl_print("werror","\n");
  }

/***********************************************************/
/* EXP_NUM_ERROR:                               */
/***********************************************************/
exp_num_error(fun_name,check_val,exp_num)
  char *fun_name;
  int check_val, exp_num; 
  {
   cl_print("werror","ERROR: ");
   cl_print("werror","Function ");
   cl_print("werror",fun_name);
      
   if (check_val == EXACTLY)
     { cl_print("werror"," expected exactly "); }
   else if (check_val == AT_LEAST)
     { cl_print("werror"," expected at least "); }
   else if (check_val == NO_MORE_THAN)
     { cl_print("werror"," expected no more than "); }

   print_long_int("werror",(long int) exp_num);
   cl_print("werror"," argument(s)\n");
  }
  
/********************************************************/
/* OPEN_ERROR_MESSAGE:                             */ 
/********************************************************/ 
int open_error_message(func_name,file_name)
  char *func_name, *file_name;
  {
   cl_print("werror","Function ");
   cl_print("werror",func_name);
   cl_print("werror"," was unable to open file ");
   cl_print("werror",file_name);
   cl_print("werror","\n");
  }
  
/***********************************************************/
/* GET_SEGMENT:                                            */
/***********************************************************/
struct fact *get_segment(size)
  int size;
  {
   struct fact *seg_ptr;
   
   if (size == 0) size = 1;
   seg_ptr = get_el(size);
   seg_ptr->next = segment_list;
   segment_list = seg_ptr;
   
   return(seg_ptr);
  }
  
/***********************************************************/
/* ADD_TO_SEGMENT_LIST:                                    */
/***********************************************************/
add_to_segment_list(fact_ptr)
  struct fact *fact_ptr;
  {
   fact_ptr->next = segment_list;
   segment_list = fact_ptr;
  }
  
/***********************************************************/
/* FLUSH_SEGMENTS:                                         */
/***********************************************************/
int flush_segments()
  {
   struct fact *seg_ptr;
	
   while (segment_list != NULL)
     {
      seg_ptr = segment_list;
      segment_list = segment_list->next;
      rtn_el(seg_ptr);
     }
  }
  
/***********************************************************/
/* PRINT_VALUE:                                            */
/***********************************************************/
print_value(fileid,arg_ptr)
  char *fileid;
  struct values *arg_ptr;
  {
   TYPE ptype;
   struct element *elem_ptr;
   int i; 
   ptype = arg_ptr->type;
   
   switch(ptype)
     {
      case RVOID:
        break;
      case WORD:
        cl_print(fileid,arg_ptr->val.hvalue->contents);
        break;
      case STRING:
        cl_print(fileid,"\"");
        cl_print(fileid,arg_ptr->val.hvalue->contents);
        cl_print(fileid,"\"");
        break;
      case NUMBER:
        print_num(fileid,arg_ptr->val.fvalue); 
        break;
      case MULTIPLE:
        elem_ptr = arg_ptr->origin->atoms;
        i = arg_ptr->begin;
        while (i <= arg_ptr->end)
          {
           print_element(fileid,&elem_ptr[i]);
           i++;
           if (i <= arg_ptr->end) cl_print(fileid," ");
          }
        break;
      default:
        cl_print("werror","Unsupported option "); 
        print_long_int("werror",(long int) arg_ptr->type);
        cl_print("werror"," to function printout\n");
	    set_execution_error(TRUE);
	    break;
	 }
  }
  
/****************************************/
/* numget                               */
/****************************************/
float numget(test_ptr,fun_name)
  struct test *test_ptr;
  char *fun_name;
  {
   struct values arg_ptr;
   struct element *elem_a;
   int ntype;
   float value;
   int extent;
   struct fact *temp_ptr;
   int start, multi;

   if (test_ptr->type == NUMBER)
     { return(get_test_float(test_ptr)); }
   else if (get_test_func(test_ptr) == PTR_GET_VAR)
     {         
      elem_a = fast_gv(get_test_index(get_arg_list(test_ptr)),
                       get_test_index(get_next_arg(get_arg_list(test_ptr))),
                       &extent,&temp_ptr,&start,&multi);
      ntype = get_elmtype(elem_a); 
      value = get_elmfloat(elem_a);
     }
   else
     {            
      generic_compute(test_ptr,&arg_ptr);
      ntype = get_valtype(arg_ptr);
      value = get_valfloat(arg_ptr);
     }

   if (ntype != NUMBER)
     {
      cl_print("werror","Function ");
      cl_print("werror",fun_name);
      cl_print("werror"," received a non-numeric argument\n");
      set_execution_error(TRUE);
      return(0.0);
     }

   return(value);
  }


/****************************************************************/
/* MULT_NTH:                                                    */
/****************************************************************/
struct values *mult_nth(val_ptr,num)
  struct values *val_ptr;
  int num;
  {
   static struct values mnth_value;
   struct element *elm_ptr;

   if (val_ptr->type != MULTIPLE)
     { return (NULL); }

   if ( (num > ((val_ptr->end - val_ptr->begin) + 1)) ||
        (num < 1))
     { return(NULL); }

   elm_ptr = val_ptr->origin->atoms;
   num = val_ptr->begin + num - 1;
   mnth_value.type = elm_ptr[num].type;
   if (mnth_value.type == NUMBER)
     { mnth_value.val.fvalue = elm_ptr[num].val.fvalue; }
   else
     { mnth_value.val.hvalue = elm_ptr[num].val.hvalue; }
   return(&mnth_value);
  }
  
/*************************/
/* ADD_RESET_FUNCTION:    */
/*************************/
add_reset_function(name,func_ptr)
  char *name;
  int (*func_ptr)();
  {
   struct reset_func *c_ptr;

   c_ptr = get_struct(reset_func);
   if (c_ptr == NULL)
     {
      cl_print("werror","Out of memory in add_reset_function\n");
      return(0);
     }

   c_ptr->name = name;
   c_ptr->ip = func_ptr;
   c_ptr->next = reset_list;
   reset_list = c_ptr;
   return(1);
  }
 
/****************************/
/* REMOVE_RESET_FUNCTION:    */
/****************************/
remove_reset_function(name)
  char *name;
  {
   struct reset_func *c_ptr, *last_ptr;

   last_ptr = NULL;
   c_ptr = reset_list;
   
   while (c_ptr != NULL)
     {
      if (strcmp(name,c_ptr->name) == 0)
        {
         if (last_ptr == NULL)
           { reset_list = c_ptr->next; }
         else
           { last_ptr->next = c_ptr->next; }
         rtn_struct(reset_func,c_ptr);
         return(1);
        }
      last_ptr = c_ptr;
      c_ptr = c_ptr->next;
     }
     
   return(0);
  }
  
/****************************/
/* CLIPS_SYSTEM_ERROR:      */
/****************************/
clips_system_error(error_id)
  int error_id;
  {
   cl_print("werror","*** CLIPS SYSTEM ERROR ***\n");
   cl_print("werror","ID = ");
   print_long_int("werror",(long int) error_id);
   cl_print("werror","\n");
   cl_print("werror","CLIPS data structures are in an inconsistent or corrupted state.\n");
   cl_print("werror","Please report this error.\n");
   cl_print("werror","**************************\n");  
  }
