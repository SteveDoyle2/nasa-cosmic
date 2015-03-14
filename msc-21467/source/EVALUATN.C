/*   CLIPS Version 4.30   4/25/89 */

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                  EVALUATION MODULE                  */
   /*******************************************************/
   
#include <stdio.h>

#include "clips.h"

/***************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/***************************************/

   extern struct draw       *add_symbol();
   extern char              *symbol_string();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct funtab     *fctn_list = NULL;
  
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   struct test              *new_fctn_args;

/*###############################################*/
/*###############################################*/
/*######                                   ######*/
/*###### FUNCTION DEFINITION AND EXECUTION ######*/
/*######                                   ######*/
/*###############################################*/
/*###############################################*/

/********************************************************************/
/* generic_compute:                                                 */
/********************************************************************/
int generic_compute(problem,compute_result)
  struct test *problem;
  struct values *compute_result;
  {
   float result;
   struct draw *dresult;
   char (*cp)();  
   struct draw *(*dp)(); 
   float (*fp)();
   char cbuff[2], rc;
   struct test *old_arg;

   if (problem == NULL)
     {
      clips_system_error(1201);
      cl_exit(5);
     }

   switch (problem->type)
     {
      case STRING:
      case WORD:
        compute_result->type = problem->type;
        compute_result->val.hvalue = problem->val.hvalue;
        break;
        
      case NUMBER:
        compute_result->type = NUMBER;
        compute_result->val.fvalue = problem->val.fvalue;
        break;
        
      case FCALL:
        old_arg = new_fctn_args;
        new_fctn_args = problem;
        
        switch(problem->val.fun_ptr->fun_type)
          {
	       case 'v' :     
	         (*problem->val.fun_ptr->ip)();                
             compute_result->type = RVOID;
             compute_result->val.fvalue = 0.0;
             break;
	       case 'i' : 
	         result = (float) (*problem->val.fun_ptr->ip)();                
             compute_result->type = NUMBER;
             compute_result->val.fvalue = result;  
             break;
	       case 'f' : 
	         fp = (float (*)()) problem->val.fun_ptr->ip;     
             result = (*fp)();           
             compute_result->type = NUMBER;
             compute_result->val.fvalue = result; 
             break;
	       case 's' : 
	         dp = (struct draw *(*)()) problem->val.fun_ptr->ip;
             dresult = (*dp)();
             compute_result->type = STRING;
             compute_result->val.hvalue = dresult;
             break;
	       case 'w' :  
	         dp = (struct draw *(*)()) problem->val.fun_ptr->ip;
             dresult = (*dp)();
             compute_result->type = WORD;
             compute_result->val.hvalue = dresult;
             break;
	       case 'c' : 
	         cp = (char (*)()) problem->val.fun_ptr->ip;
             rc = (*cp)();
             cbuff[0] = rc;
             cbuff[1] = EOS;
             compute_result->type = WORD;
             compute_result->val.hvalue = add_symbol(cbuff);
             break;
           case 'm' :
	         (*problem->val.fun_ptr->ip)(compute_result);
             break;
           case 'u' : 
	         (*problem->val.fun_ptr->ip)(compute_result);
             break;
             
           default  : 
             clips_system_error(1202);
             cl_exit(5);
             break;
          }
          
       new_fctn_args = old_arg;
       break;
       
     case BWORDS:
     case BWORD:
       cl_print("werror","\n*** ERROR ***\n");
       cl_print("werror","Variables cannot be accessed at this level\n");
       break;
     default:
       clips_system_error(1203);
       cl_exit(5);
       break;
     }

   return;     
  }

/*************************************************************/
/* define_function: Used to define a system or user external */
/*   function so that CLIPS can access it.                   */
/*************************************************************/
define_function(name,return_type,pointer,defn_name)
  char *name, *defn_name;
  char return_type;
  int (*pointer)();
  {

   struct funtab *new_function;

   if ( (return_type != 'i') &&
        (return_type != 'f') &&
        (return_type != 's') &&
        (return_type != 'w') &&
        (return_type != 'c') &&
        (return_type != 'v') &&
        (return_type != 'm') &&
        (return_type != 'u') )
     {
      cl_print("werror","ERROR: Illegal function type passed to define_function\n");
      cl_print("werror","Legal values are i, f, s, w, c, v, m, and u.\n");
      return(0); 
     }
   
   new_function = get_struct(funtab);
   new_function->fun_name = name;
   new_function->fun_type = return_type; 
   new_function->ip = pointer;
   new_function->next = fctn_list;
   new_function->defn_name = defn_name;

   fctn_list = new_function;
 
   return(1);
  }

/************************************************************/
/* find_function: Returns true (1) if a function name is in */
/*   the function list, otherwise returns false (0).        */
/************************************************************/
struct funtab *find_function(fun_name)
  char *fun_name;
  {
   struct funtab *fun_list;

   fun_list = fctn_list;
   if (fun_list == NULL) { return(NULL); }

   while (strcmp(fun_name,fun_list->fun_name) != 0) 
     { 
      fun_list = fun_list->next;
      if (fun_list == NULL) { return(NULL); }
     }

   return(fun_list);
  }

/***********************************************************/
/* RSTRING:  Purpose is to return a pointer to a character */
/* array that represents an argument to a function call,   */
/* called by the fctn.                                     */
/***********************************************************/
char *rstring(string_pos)
  int string_pos; 
  {
   int count = 1;
   struct values result;
   struct test *arg_ptr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   arg_ptr = new_fctn_args->arg_list;
   while ((arg_ptr != NULL) && (count < string_pos))
     {
      count++; 
      arg_ptr = arg_ptr->next_arg; 
     }

   if (arg_ptr == NULL)
     {
      non_exist_error("rstring",new_fctn_args->val.fun_ptr->fun_name,string_pos);
      set_execution_error(TRUE);
      return(NULL);
     }

   /*=================================================*/
   /* Return the value associated with that argument. */
   /*=================================================*/

   generic_compute(arg_ptr,&result);
   if ((result.type != WORD) && (result.type != STRING))
     {
      wrong_type_error("rstring",new_fctn_args->val.fun_ptr->fun_name,
                       string_pos,"string");
      set_execution_error(TRUE);
      return(NULL);
     }

   return(result.val.hvalue->contents);
  }
  
/***********************************************************/
/* RHASH:  Purpose is to return a pointer to a character   */
/* array that represents an argument to a function call,   */
/* called by the fctn.                                     */
/***********************************************************/
struct draw *rhash(string_pos)
  int string_pos; 
  {
   int count = 1;
   struct values result;
   struct test *arg_ptr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   arg_ptr = new_fctn_args->arg_list;
   while ((arg_ptr != NULL) && (count < string_pos))
     {
      count++; 
      arg_ptr = arg_ptr->next_arg; 
     }

   if (arg_ptr == NULL)
     {
      non_exist_error("rhash",new_fctn_args->val.fun_ptr->fun_name,string_pos);
      set_execution_error(TRUE);
      return(NULL);
     }

   /*=================================================*/
   /* Return the value associated with that argument. */
   /*=================================================*/

   generic_compute(arg_ptr,&result);
   if ((result.type != WORD) && (result.type != STRING))
     {
      wrong_type_error("rhash",new_fctn_args->val.fun_ptr->fun_name,
                       string_pos,"string");
      set_execution_error(TRUE);
      return(NULL);
     }

   return(result.val.hvalue);
  }

/**************************************************************/
/* rfloat:  Returns the nth argument of a function call. The  */
/*   argument should be a floating point number, otherwise an */
/*   error will occur.                                        */
/**************************************************************/
float rfloat(float_pos)
  int float_pos;
  {
   int count = 1;
   struct values result;
   struct test *arg_ptr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/
   
   arg_ptr = new_fctn_args->arg_list;
   while ((arg_ptr != NULL) && (count < float_pos))
     {
      count++; 
      arg_ptr = arg_ptr->next_arg; 
     }

   if (arg_ptr == NULL)
     {
      non_exist_error("rfloat",new_fctn_args->val.fun_ptr->fun_name,float_pos);
      set_execution_error(TRUE);
      return(1.0);
     }

   /*=================================================*/
   /* Return the value associated with that argument. */
   /*=================================================*/

   generic_compute(arg_ptr,&result);
   if (result.type != NUMBER)
     {
      wrong_type_error("rfloat",new_fctn_args->val.fun_ptr->fun_name,
                       float_pos,"number");
      set_execution_error(TRUE);
      return(1.0);
     }
   
   return(result.val.fvalue);   
  }

/********************************************************/
/* runknown: returns an argument thats type is unknown. */
/********************************************************/
struct values *runknown(arg_pos,val_ptr)
  int arg_pos;
  struct values *val_ptr;
  {
   int count = 1;
   struct test *arg_ptr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   arg_ptr = new_fctn_args->arg_list;
   while ((arg_ptr != NULL) && (count < arg_pos))
     {
      count++; 
      arg_ptr = arg_ptr->next_arg; 
     }

   if (arg_ptr == NULL)
     {
      non_exist_error("runknown",new_fctn_args->val.fun_ptr->fun_name,arg_pos);
      set_execution_error(TRUE);
      return(NULL);
     }

   /*=================================================*/
   /* Return the value associated with that argument. */
   /*=================================================*/

   generic_compute(arg_ptr,val_ptr);
   return(val_ptr);
  }

/********************************************************/
/* rmultype:  Returns the type of the nth element of an */
/*   argument of type MULTIPLE.                         */
/********************************************************/
int rmultype(result,nth)
  struct values *result;
  int nth; 
  {
   int length;
   struct element *elm_ptr;

   if (result->type != MULTIPLE) return(NULL);

   length = (result->end - result->begin) + 1;
   if ((nth > length) || (nth < 1)) return(NULL);

   elm_ptr = result->origin->atoms;
   return(elm_ptr[result->begin + nth - 1].type);
  }

/********************************************************/
/* rmulstring:                                          */
/********************************************************/
char *rmulstring(result,nth)
  struct values *result;
  int nth; 
  {
   int length;
   struct element *elm_ptr;

   if (result->type != MULTIPLE) return(NULL);

   length = (result->end - result->begin) + 1;
   if ((nth > length) || (nth < 1)) return(NULL);

   elm_ptr = result->origin->atoms;
   return(symbol_string(elm_ptr[result->begin + nth - 1].val.hvalue));
  }
  
/********************************************************/
/* rmulhash:                                          */
/********************************************************/
struct draw *rmulhash(result,nth)
  struct values *result;
  int nth; 
  {
   int length;
   struct element *elm_ptr;

   if (result->type != MULTIPLE) return(NULL);

   length = (result->end - result->begin) + 1;
   if ((nth > length) || (nth < 1)) return(NULL);

   elm_ptr = result->origin->atoms;
   return(elm_ptr[result->begin + nth - 1].val.hvalue);
  }

/********************************************************/
/* rmulfloat:                                          */
/********************************************************/
float rmulfloat(result,nth)
  struct values *result;
  int nth; 
  {
   int length;
   struct element *elm_ptr;

   if (result->type != MULTIPLE) return(0.0);

   length = (result->end - result->begin) + 1;
   if ((nth > length) || (nth < 1)) return(0.0);

   elm_ptr = result->origin->atoms;
   return(elm_ptr[result->begin + nth - 1].val.fvalue);
  }

/********************************************************/
/* make_unknown:                                        */
/********************************************************/
struct values *make_unknown(unk_type,string,number)
  struct draw *string;
  int unk_type;
  float number;
  {
   static struct values unk_val;
   
   unk_val.type = unk_type;
   if (unk_type == NUMBER)
     { unk_val.val.fvalue = number; }
   else
     { unk_val.val.hvalue = string; }
   
   return(&unk_val);
  }
  
/********************************************************/
/* assign_unknown:                                      */
/********************************************************/
assign_unknown(unk_type,string,number,unk_val)
  struct draw *string;
  int unk_type;
  float number;
  struct values *unk_val;
  { 
   unk_val->type = unk_type;
   if (unk_type == NUMBER)
     { unk_val->val.fvalue = number; }
   else
     { unk_val->val.hvalue = string; }
  }

/******************************************************************/
/* num_args: Returns the length of the argument list for a        */
/*   function call.  Useful for system and user defined functions */
/*   which accept a variable number of arguments.                 */
/******************************************************************/
num_args()
  {
   int count = 0;
   struct test *arg_ptr;

   arg_ptr = new_fctn_args->arg_list;

   while (arg_ptr != NULL)
     {
      count++; 
      arg_ptr = arg_ptr->next_arg;
     }
   
   return(count);
  }

/*********************************************/
/* ADD_ELEMENT:                              */
/*********************************************/
add_element(new_fact,position,data_type,svalue,ivalue)
  struct fact *new_fact;
  int position;
  int data_type;
  char *svalue;
  float ivalue;
  {
   struct element *elem_ptr;

   if ((position > new_fact->fact_length) ||
       (position < 1))
     { return(-1); }

   position--;
   elem_ptr = new_fact->atoms;

   elem_ptr[position].type = data_type;
   if (data_type == NUMBER)
     { elem_ptr[position].val.fvalue = ivalue; }
   else
     { elem_ptr[position].val.hvalue = add_symbol(svalue); }
 
   return(0);
  }
  
/*********************************************/
/* NON_EXIST_ERROR:                              */
/*********************************************/
static int non_exist_error(acc_fun_name,fun_name,arg_num)
  char *acc_fun_name, *fun_name;
  int arg_num;
  {
   cl_print("werror","Function ");
   cl_print("werror",acc_fun_name);
   cl_print("werror"," received a request from function ");
   cl_print("werror",fun_name);
   cl_print("werror"," for argument #");
   print_long_int("werror",(long int) arg_num);
   cl_print("werror"," which is non-existent\n");
  }
  
/*********************************************/
/* WRONG_TYPE_ERROR:                              */
/*********************************************/
static int wrong_type_error(acc_fun_name,fun_name,arg_num,type)
  char *acc_fun_name, *fun_name, *type;
  int arg_num;
  {
   cl_print("werror","Function ");
   cl_print("werror",acc_fun_name);
   cl_print("werror"," received a request from function ");
   cl_print("werror",fun_name);
   cl_print("werror"," for argument #");
   print_long_int("werror",(long int) arg_num);
   cl_print("werror"," which is not of type ");
   cl_print("werror",type);
   cl_print("werror","\n");
  }
  
/*****************/
/* GET_FCTN_LIST */
/*****************/
struct funtab *get_fctn_list()
  {
   return(fctn_list);
  }

/*****************/
/* SET_FCTN_LIST */
/*****************/
set_fctn_list(value)
  struct funtab *value;
  {
   fctn_list = value;
  }

