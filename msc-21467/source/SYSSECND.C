/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*               SYSTEM SECONDARY MODULE               */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "clips.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   float                   do_nothing();
   float                   str_assert();
   float                   setgen();
   HASH_PTR                gensym();
   int                     str_cat();
   float                   cl_length();
   float                   my_subset();
   float                   cl_member();
   int                     cl_nth();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct fact     *assert();
   extern char            *num_to_string();
   extern struct draw     *add_symbol();
   extern float            clips_time();
   extern float            my_system();
   extern int              define_function();
   extern float            numget();
   extern char            *symbol_string();
  
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static long int         gen_number = 1;

/*******************************************/
/* SYSSECONDARY_DEFINE:           */
/*******************************************/
syssecondary_define()
  {
   define_function("gensym",        'w', (int (*)()) gensym,        "gensym");
   define_function("do_nothing",    'f', (int (*)()) do_nothing,    "do_nothing");
   define_function("do-nothing",    'f', (int (*)()) do_nothing,    "do_nothing");
   define_function("setgen",        'f', (int (*)()) setgen,        "setgen");
   define_function("str_cat",       'u', (int (*)()) str_cat,       "str_cat");
   define_function("str-cat",       'u', (int (*)()) str_cat,       "str_cat");
   define_function("system",        'f', (int (*)()) my_system,     "my_system");
   define_function("string_assert", 'f', (int (*)()) str_assert,    "str_assert");
   define_function("str_assert",    'f', (int (*)()) str_assert,    "str_assert");
   define_function("string-assert", 'f', (int (*)()) str_assert,    "str_assert");
   define_function("str-assert",    'f', (int (*)()) str_assert,    "str_assert");
   define_function("length",        'f', (int (*)()) cl_length,     "cl_length");
   define_function("nth",           'u', (int (*)()) cl_nth,        "cl_nth");
   define_function("member",        'f', (int (*)()) cl_member,     "cl_member");
   define_function("subset",        'f', (int (*)()) my_subset,     "my_subset"); 
   define_function("time",          'f', (int (*)()) clips_time,    "clips_time");
  }

/****************************************/
/* STR_CAT:                             */
/****************************************/
int str_cat(cat_value)
  VALUE_PTR cat_value;
  {
   VALUE arg_ptr;
   int numa, i, total, j;
   char *str_mem, **str_arr;
   struct draw *hash_ptr;
   
   set_vptype(cat_value,STRING);
   set_vpstring(cat_value,"");

   numa = num_args();
   if (numa == 0) return;
     
   str_arr = (char **) gm1(sizeof(char *) * numa);

   total = 1;
   for (i = 1 ; i <= numa ; i++)
     {
      runknown(i,&arg_ptr);
      if ((get_valtype(arg_ptr) == STRING) || (get_valtype(arg_ptr) == WORD))
        { str_arr[i - 1] = get_valstring(arg_ptr); }
      else if (get_valtype(arg_ptr) == NUMBER)
        {
         hash_ptr = add_symbol(num_to_string(get_valfloat(arg_ptr)));
         str_arr[i - 1] = symbol_string(hash_ptr);
        }
      else
        {
         cl_print("werror","Illegal argument type to str_cat\n");
         rm(str_arr,sizeof(char *) * numa);
         set_execution_error(TRUE);
         set_vpstring(cat_value,"");
         return;
        }
        
      total += strlen(str_arr[i - 1]);
     } 

   str_mem = gm2 ((sizeof(char) * total));

   j = 0;
   for (i = 0 ; i < numa ; i++)
     {
      sprintf(&str_mem[j],"%s",str_arr[i]);
      j += strlen(str_arr[i]);
     }

   set_vpstring(cat_value,str_mem);
   rm(str_mem,sizeof(char) * total);
   rm(str_arr,sizeof(char *) * numa);

   return;
  }


/****************************************/
/* CL_NTH:                              */
/****************************************/
cl_nth(nth_value)
  VALUE_PTR nth_value;
  {
   VALUE val_ptr1, val_ptr2;
   ELEMENT_PTR elm_ptr;
   int n;

   if (arg_num_check("nth",EXACTLY,2) == -1)
     {
      set_vptype(nth_value,WORD);
      set_vpstring(nth_value,"nil");
      return;
     }

   if ((arg_type_check("nth",1,NUMBER,&val_ptr1) == FALSE) ||
       (arg_type_check("nth",2,MULTIPLE,&val_ptr2) == FALSE))
     {
      set_vptype(nth_value,WORD);
      set_vpstring(nth_value,"nil");
      return;
     }

   n = (int) get_valfloat(val_ptr1);
   if ((n > get_vallength(val_ptr2)) || (n < 1))
     {
      set_vptype(nth_value,WORD);
      set_vpstring(nth_value,"nil");
      return;
     }

   elm_ptr = get_valelement(val_ptr2,n + get_valbegin(val_ptr2) - 1);

   set_vptype(nth_value,get_elmtype(elm_ptr));
   if (get_vptype(nth_value) == NUMBER)
     { set_vpfloat(nth_value,get_elmfloat(elm_ptr)); }
   else
     { set_vphash(nth_value,get_elmhash(elm_ptr)); }
   return;
  }

/* ------------------------------------------------------------------
 *    MY_SUBSET: This function compares two multi-field variables
 *               to see if the first is a subset of the second. It
 *               does not consider order. 
 *
 *    INPUTS:    Two arguments via CLIPS stack. First is the sublist
 *               multi-field variable, the second is the list to be
 *               compared to. Both should be of type MULTIPLE.
 *
 *    OUTPUTS:   One floating point number, 1.0 if the first list
 *               is a subset of the second, else 0.0 if it is not.
 *
 *    NOTES:     This function is called from CLIPS with the subset
 *               command. Repeated values in the sublist must also
 *               be repeated in the main list.
 * ------------------------------------------------------------------
 */

float my_subset()
  {
   VALUE item1, item2;
   TYPE mul_type;
   char *svalue;
   float fvalue;
   int i, length1;
   
   if (arg_num_check("subset",EXACTLY,2) == -1)
     return((float) FALSE);
     
   if (arg_type_check("subset",1,MULTIPLE,&item1) == FALSE)
     return((float) FALSE);
     
   if (arg_type_check("subset",2,MULTIPLE,&item2) == FALSE)
     return((float) FALSE);
     
   length1 = get_vallength(item1);
   
   for (i = 1 ; i <= length1 ; i++)
     {
      mul_type = rmultype(&item1,i);
      if (mul_type == NUMBER)
        { 
         fvalue = rmulfloat(&item1,i);
         svalue = NULL;
        }
      else
        { 
         svalue = rmulstring(&item1,i);
         fvalue = 0.0;
        }
        
      if (find_item_in_segment(mul_type,svalue,fvalue,&item2) == 0)
        { return(FALSE); }
     }
   
   return((float) TRUE);
  }

/***************************************/
/* CL_MEMBER:                          */
/***************************************/
float cl_member()
  {
   VALUE item1, item2;
   char *svalue;
   float fvalue;

   if (arg_num_check("member",EXACTLY,2) == -1)
     { return(CLIPS_FALSE); }
   
   runknown(1,&item1);
   if ((get_valtype(item1) != WORD) && 
       (get_valtype(item1) != STRING) &&
       (get_valtype(item1) != NUMBER))
     {
      cl_print("werror","ERROR: ");
      cl_print("werror","Function member expected argument #1 ");
      cl_print("werror","to be of type string, word, or number\n");                        
      set_execution_error(CLIPS_TRUE);
      return(CLIPS_FALSE);
     }

   if (arg_type_check("member",2,MULTIPLE,&item2) == CLIPS_FALSE)
     { return(CLIPS_FALSE); }
   
   if (get_valtype(item1) == NUMBER)
     {
      svalue = "";
      fvalue = get_valfloat(item1);
     }
   else
     {
      svalue = get_valstring(item1);
      fvalue = 0.0;
     }
     

   return(find_item_in_segment(get_valtype(item1),svalue,fvalue,&item2));
  }
  
/***************************************/
/* FIND_ITEM_IN_SEGMENT:               */
/***************************************/
find_item_in_segment(type,svalue,fvalue,val_ptr)
  TYPE type;
  char *svalue;
  float fvalue;
  VALUE_PTR val_ptr;
  {
   int mul_length, i;
   TYPE mul_type;
   
   mul_length = get_vplength(val_ptr);
   for (i = 1 ; i <= mul_length ; i++)
     {
      mul_type = rmultype(val_ptr,i);
      if (mul_type == type)
        {
         if (mul_type == NUMBER)
           { if (fvalue == rmulfloat(val_ptr,i)) return(i); }
         else
           { 
            if (svalue == rmulstring(val_ptr,i)) return(i); }
        }
     }
   
   return(FALSE);
  }

/**********************************************************/
/* STR_ASSERT:  Allows a fact to be asserted as a string. */
/**********************************************************/
float str_assert()
  {
   VALUE arg_ptr;

   if (arg_num_check("str_assert",EXACTLY,1) == -1) return(CLIPS_FALSE);

   if (arg_type_check("str_assert",1,STRING,&arg_ptr) == CLIPS_FALSE) 
     { return(CLIPS_FALSE); }

   assert(get_valstring(arg_ptr));
   return(CLIPS_TRUE);
  }

/****************************************/
/* DO_NOTHING:                          */
/****************************************/
float do_nothing()
  { return(CLIPS_TRUE); }

/****************************************/
/* SETGEN:                              */
/****************************************/
float setgen()
  {
   float fnum;
   VALUE val_ptr;

   if (arg_num_check("setgen",EXACTLY,1) == -1) return((float) gen_number);
   if (arg_type_check("setgen",1,NUMBER,&val_ptr) == FALSE) return((float) gen_number);
   
   fnum = get_valfloat(val_ptr);
   
   if (fnum < 1.0) 
     {
      exp_type_error("setgen",1,"number (greater than or equal to 1)");
      return((float) gen_number);
     }
     
   gen_number = (long int) fnum;
   return(fnum);
  }

/****************************************/
/* GENSYM:                              */
/****************************************/
HASH_PTR gensym()
  {
   int needed_space = 4;
   int i;
   long int countdown, tempa, tempb;
   static char genstring[15];

   arg_num_check("gensym",EXACTLY,0);
   countdown = gen_number;

   while (countdown > 0)
     {
      needed_space++;
      countdown = countdown / 10;
     }

   genstring[0] = 'g';
   genstring[1] = 'e';
   genstring[2] = 'n';
 
   tempa = gen_number;
   for (i = (needed_space - 2) ; i > 2 ; i--)
     {
      tempb = tempa - ((tempa / 10) * 10);
      genstring[i] = '0' + tempb;
      tempa = tempa / 10;
     }

   gen_number++;
   genstring[needed_space - 1] = '\0';
   return(add_symbol(genstring));
  }


/****************************************/
/* CL_LENGTH:                           */
/****************************************/
float cl_length()
  {
   VALUE item;

   if (arg_num_check("length",EXACTLY,1) == -1) return(CLIPS_FALSE);

   runknown(1,&item);

   return ( (float) get_vallength(item));
  }
