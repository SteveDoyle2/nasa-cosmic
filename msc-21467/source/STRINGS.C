/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    STRINGS MODULE                   */
   /*******************************************************/
   
#include "setup.h"
#if STRING_FUNCTIONS

#include "clips.h"
#include <stdio.h>
#include <ctype.h>

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                       eval_string();
   int                       lowercase();
   int                       str_index();
   float                     str_length();
   float                     str_compare();
   int                       string_define();
   struct draw              *sub_string();
   int                       uppercase();
   
/********************************************/
/* string_define:                           */
/********************************************/
string_define()
  {
   define_function("str-length",'f',str_length,"str_length");
   define_function("str-compare",'f',str_compare,"str_compare");
   define_function("upcase",'u',uppercase,"uppercase");
   define_function("lowcase",'u',lowercase,"lowercase");
   define_function("sub-string",'s',sub_string, "sub_string");
   define_function("str-index",'i',str_index, "str_index");
   define_function("eval",'m',eval_string,"eval_string");
  }

/********************************************************/
/* STR_LENGTH:  Returns the length of a STRING or WORD. */
/*   Syntax: (str_length <string-or-word>)              */
/********************************************************/
float str_length()
  {
   VALUE val1;

   /*===================================================*/
   /* Function str_length expects exactly one argument. */
   /*===================================================*/
   
   if (arg_num_check("str_length",EXACTLY,1) == -1)
     { return(-1); }

   /*================================================*/
   /* The argument should be of type WORD or STRING. */
   /*================================================*/
   
   runknown(1,&val1);
   if ((get_valtype(val1) != STRING) && (get_valtype(val1) != WORD))
     { 
      exp_type_error("str_length",1,"string or word");
      return(-1);
     }
     
   /*==========================================*/
   /* Return the length of the string or word. */
   /*==========================================*/
   
   return( (float) strlen(get_valstring(val1)));
  }
  
/******************************************************/
/* UPPERCASE:  Returns a STRING or WORD in uppercase. */
/*   Syntax: (upcase <string-or-word>)                */
/******************************************************/
int uppercase(rv)
  VALUE_PTR rv;
  {
   VALUE val1;
   int i, slen;
   char *osptr, *nsptr;

   /*===================================================*/
   /* Function uppercase expects exactly one argument. */
   /*===================================================*/
   
   if (arg_num_check("upcase",EXACTLY,1) == -1)
     { 
      set_execution_error(TRUE);
      set_vptype(rv,STRING);
      set_vpstring(rv,"");
      return;
     }

   /*================================================*/
   /* The argument should be of type WORD or STRING. */
   /*================================================*/
   
   runknown(1,&val1);
   if ((get_valtype(val1) != STRING) && (get_valtype(val1) != WORD))
     { 
      exp_type_error("upcase",1,"string or word");
      set_execution_error(TRUE);
      set_vptype(rv,STRING);
      set_vpstring(rv,"");
      return;
     }
   
   osptr = get_valstring(val1);
   slen = strlen(osptr) + 1;
   nsptr = gm2(slen);
   
   for (i = 0  ; i < slen ; i++)
     {
      if (islower(osptr[i]))
        { nsptr[i] = toupper(osptr[i]); }
      else 
        { nsptr[i] = osptr[i]; }
     }
   
   /*===============================*/
   /* Return the uppercased string. */
   /*===============================*/
   
   set_vptype(rv,get_valtype(val1));
   set_vpstring(rv,nsptr);
   rm(nsptr,slen);
   return;
  }
  
/******************************************************/
/* LOWERCASE:  Returns a STRING or WORD in lowercase. */
/*   Syntax: (lowcase <string-or-word>)               */
/******************************************************/
int lowercase(rv)
  VALUE_PTR rv;
  {
   VALUE val1;
   int i, slen;
   char *osptr, *nsptr;

   /*===================================================*/
   /* Function uppercase expects exactly one argument. */
   /*===================================================*/
   
   if (arg_num_check("lowcase",EXACTLY,1) == -1)
     { 
      set_execution_error(TRUE);
      set_vptype(rv,STRING);
      set_vpstring(rv,"");
      return;
     }

   /*================================================*/
   /* The argument should be of type WORD or STRING. */
   /*================================================*/
   
   runknown(1,&val1);
   if ((get_valtype(val1) != STRING) && (get_valtype(val1) != WORD))
     { 
      exp_type_error("lowcase",1,"string or word");
      set_execution_error(TRUE);
      set_vptype(rv,STRING);
      set_vpstring(rv,"");
      return;
     }
   
   osptr = get_valstring(val1);
   slen = strlen(osptr) + 1;
   nsptr = gm2(slen);
   
   for (i = 0  ; i < slen ; i++)
     {
      if (isupper(osptr[i]))
        { nsptr[i] = tolower(osptr[i]); }
      else 
        { nsptr[i] = osptr[i]; }
     }
   
   /*===============================*/
   /* Return the uppercased string. */
   /*===============================*/
   
   set_vptype(rv,get_valtype(val1));
   set_vpstring(rv,nsptr);
   rm(nsptr,slen);
   return;
  }
  
/************************************************************/
/* STR_COMPARE:  Compares two strings.                      */
/*   Syntax: (str_compare <string-1> <string-2> [<length>]) */
/*   Returns 0 is <string-1> and <string-2> are equal, < 0  */
/*   if <string-1> is less than <string-2>, and > 0 if      */
/*   <string-1> is greater than <string-2>. This function   */
/*   based on the C functions strcmp and strncmp.           */
/************************************************************/
float str_compare()
  {
   int nargs, length;
   VALUE val1, val2, val3;
   float rv;

   /*=======================================================*/
   /* Function str_compare expects either 2 or 3 arguments. */
   /*=======================================================*/
   
   if (arg_num_check("str_compare",AT_LEAST,2) == -1)
     { return(0); }
     
   if ((nargs = arg_num_check("str_compare",NO_MORE_THAN,3)) == -1)
     { return(0); }

   /*===========================================================*/
   /* The first two arguments should be of type WORD or STRING. */
   /*===========================================================*/
   
   runknown(1,&val1);
   if ((get_valtype(val1) != STRING) && (get_valtype(val1) != WORD))
     { 
      exp_type_error("str_compare",1,"string or word");
      return(0);
     }
     
   runknown(2,&val2);
   if ((get_valtype(val2) != STRING) && (get_valtype(val2) != WORD))
     { 
      exp_type_error("str_compare",2,"string or word");
      return(0);
     }
     
   /*===================================================*/
   /* Compare the strings. Use the 3rd argument for the */
   /* maximum length of comparison, if it is provided.  */         
   /*===================================================*/
   
   if (nargs == 3)
     {
      if (arg_type_check("str_compare",3,NUMBER,&val3) == FALSE)
        { return(0); }
        
      length = (int) get_valfloat(val3);
      rv = (float) strncmp(get_valstring(val1),get_valstring(val2),length);
     }
   else
     { rv = (float) strcmp(get_valstring(val1),get_valstring(val2)); }
   
   return(rv);
  }
       
/**********************************************/
/* sub_string:  Returns a portion of a string */
/*   and returns the pointer to a new string. */
/**********************************************/
struct draw *sub_string()
  {
   VALUE val_ptr;
   char *tmp_str, *ret_str;
   int start, end, i, j;
   HASH_PTR rv;

   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/
   
   if (arg_num_check("sub-string",EXACTLY,3) == -1)
     { 
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }

   if (arg_type_check("sub-string",1,NUMBER,&val_ptr) == FALSE)
     {
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }

   start = get_vpfloat(&val_ptr) - 1;

   if (arg_type_check("sub-string",2,NUMBER,&val_ptr) == FALSE) 
     { 
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }

   end = get_vpfloat(&val_ptr) - 1;

   if (arg_type_check("sub-string",3,STRING,&val_ptr) == FALSE) 
     { 
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }  

   /*================================================*/
   /* If parameters are out of range return an error */
   /*================================================*/
   
   if (start < 0) start = 0;
   if (end > strlen(get_vpstring(&val_ptr)))
     { end = strlen(get_vpstring(&val_ptr)); }

   /*==================================*/
   /* If the start is greater than the */
   /* end, return a null string.       */
   /*==================================*/

   if (start > end)
     { return(add_symbol("")); }
   
   /*=============================================*/
   /* Otherwise, allocate the string and copy the */
   /* designated portion of the old string to the */
   /* new string.                                 */
   /*=============================================*/
   
   else
     {
      ret_str = gm2(end - start +2);  /* (end - start) inclusive + EOS */
      tmp_str = get_vpstring(&val_ptr);
      for(j=0, i=start;i <= end; i++, j++)
        { *(ret_str+j) = *(tmp_str+i); }
      *(ret_str+j) = '\0';
     } 
     
   /*========================*/
   /* Return the new string. */
   /*========================*/

   rv = add_symbol(ret_str);
   rm(ret_str,end - start + 2);
   return(rv);
  }

/**********************************************************/
/* str_index: Returns the position of the first string in */
/*   the second. If string is not found, 0 is returned.   */
/**********************************************************/
int str_index()
  {
   VALUE val_ptr1, val_ptr2;
   char *strg1, *strg2;
   int i, j;

   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/
   
   if (arg_num_check("str-index",EXACTLY,2) == -1)
     { return(-1); }

   if (arg_type_check("str-index",1,STRING,&val_ptr1) == FALSE)
     { return(-1); }

   if (arg_type_check("str-index",2,STRING,&val_ptr2) == FALSE)
     { return(-1); }
 
   strg1 = get_vpstring(&val_ptr1);
   strg2 = get_vpstring(&val_ptr2);
 
   /*=================================*/
   /* Find the position in string2 of */
   /* string1 (counting from 1).      */
   /*=================================*/

   if (strlen(strg1) == 0) return(strlen(strg2) + 1);
   
   for (i=1; *strg2; i++, strg2++)
     {
      for (j=0; *(strg1+j) && *(strg1+j) == *(strg2+j); j++);
             
      if (*(strg1+j) == '\0') return(i);
     }
 
   return(0);
  }

/**************************************/
/* EVAL:  Evaluates a STRING or WORD. */
/*   Syntax: (eval <string-or-word>)  */
/**************************************/
#if (! RUN_TIME) && (! BLOAD_ONLY)
int eval_string(rv)
  VALUE_PTR rv;
  {
   VALUE val1;
   SEGMENT seg_ptr;
   struct test *top, *fctn0_parse();

   /*===================================================*/
   /* Function str_length expects exactly one argument. */
   /*===================================================*/
 
   if (arg_num_check("str_length",EXACTLY,1) == -1)
     {
      set_vptype(rv,NUMBER);
      set_vpfloat(rv,0.0); 
      return; 
     }

   /*================================================*/
   /* The argument should be of type WORD or STRING. */
   /*================================================*/
   
    runknown(1,&val1);
   if ((get_valtype(val1) != STRING) && (get_valtype(val1) != WORD))
     { 
      exp_type_error("str_length",1,"string or word");
      set_vptype(rv,NUMBER);
      set_vpfloat(rv,0.0); 
      return;
     }
   
   /*======================*/
   /* Evaluate the string. */
   /*======================*/
   
   if (open_str_source("eval",get_valstring(val1),0) == 0)
     {
      set_vptype(rv,NUMBER);
      set_vpfloat(rv,0.0);
      return; 
     }
      
   top = fctn0_parse("eval");
   
   if (top == NULL)
     { 
      close_str_source("eval");
      set_vptype(rv,MULTIPLE);
      set_vpbegin(rv,1);
      set_vpend(rv,0);
      seg_ptr = get_segment(0);
      set_vpsegment(rv,seg_ptr);
      return; 
     }

   generic_compute(top,rv);
   returntests(top);
     
   switch(rv->type)
     {
      case MULTIPLE:
        break;
        
      case RVOID:
        set_vptype(rv,MULTIPLE);
        set_vpbegin(rv,1);
        set_vpend(rv,0);
        seg_ptr = get_segment(0);
        set_vpsegment(rv,seg_ptr);
        break;
        
      case STRING:
      case WORD:
        seg_ptr = get_segment(1);
        set_segtype(seg_ptr,1,get_vptype(rv));
        set_seghash(seg_ptr,1,get_vphash(rv));
        set_vpsegment(rv,seg_ptr);
        set_vptype(rv,MULTIPLE);
        set_vpbegin(rv,1);
        set_vpend(rv,1);
        break;
        
      case NUMBER:
        seg_ptr = get_segment(1);
        set_segtype(seg_ptr,1,NUMBER);
        set_segfloat(seg_ptr,1,get_vpfloat(rv));
        set_vpsegment(rv,seg_ptr);
        set_vptype(rv,MULTIPLE);
        set_vpbegin(rv,1);
        set_vpend(rv,1);
        break;
     }
     
     
   close_str_source("eval");
  }
#else
int eval_string(rv)
  VALUE_PTR rv;
  {
   SEGMENT seg_ptr;
   
   cl_print("werror","Function eval does not work in run time modules\n");
   set_vptype(rv,MULTIPLE);
   set_vpbegin(rv,1);
   set_vpend(rv,0);
   seg_ptr = get_segment(0);
   set_vpsegment(rv,seg_ptr);
  }
#endif
#endif
_