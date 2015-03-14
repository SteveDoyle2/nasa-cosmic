/*   CLIPS Version 4.30   4/25/89 */

#include "setup.h"

#if MULTIFIELD_FUNCTIONS

#include <stdio.h>
#include "clips.h"
#include "scanner.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                       sub_sequence();
   int                       mv_delete();
   int                       mv_append();
   int                       str_explode();
   struct draw              *str_implode();
   int                       segment_error_values();

/*********************************************/
/* MULTIVAR_DEFINE:                          */
/*********************************************/
multivar_define()
  {
   define_function("mv-subseq",'m',sub_sequence,"sub_sequence");
   define_function("mv-delete",'m',mv_delete, "mv_delete");
   define_function("mv-append",'m',mv_append, "mv_append");
   define_function("str-explode",'m',str_explode, "str_explode");
   define_function("str-implode",'s',str_implode, "str_implode");
  }

/*********************************************/
/* MV_DELETE:  Delete function for segments. */
/*   Note: delete does have to create a      */
/*   new segment.                            */
/*********************************************/
int mv_delete(seg_value)
  VALUE_PTR seg_value;
  {
   VALUE val_ptr1, val_ptr2;
   SEGMENT seg_ptr, orig_ptr;
   int start, end, i, j, n;
   int del = FALSE;

   if (arg_num_check("mv-delete",EXACTLY,2) == -1)
     {
      segment_error_values(seg_value);
      return(CLIPS_FALSE);
     }

   if ((arg_type_check("mv-delete",1,NUMBER,&val_ptr1) == FALSE) ||
       (arg_type_check("mv-delete",2,MULTIPLE,&val_ptr2) == FALSE))
     {
      set_execution_error(CLIPS_TRUE);
      segment_error_values(seg_value);
      return(CLIPS_FALSE);
     }

   /*=====================================================*/
   /* Get the field number to be deleted and check to see */
   /* that it exists within the segment boundaries.       */
   /*=====================================================*/

   n = (int) get_valfloat(val_ptr1);
   if ((n > get_vallength(val_ptr2)) || (n < 1))
     {
      segment_error_values(seg_value);
      return(CLIPS_FALSE);
     }
 
   /*=================================================*/
   /* Get a new segment with length one less than the */
   /* original.                                       */ 
   /*=================================================*/

   start = get_vpbegin(&val_ptr2);
   end = get_vpend(&val_ptr2);
   seg_ptr = get_segment(end - start);
   orig_ptr = get_vpsegment(&val_ptr2);

   /*======================================================*/
   /* Copy all but the deleted value from the old segment. */
   /*======================================================*/

   for(i=start, j=1; i<= end; i++, j++)
     {
      if(j == n && del == NULL)
        {
          j--;
          del = TRUE;
          continue;
        }
      set_segtype(seg_ptr,j,(get_segtype(orig_ptr,i)));
	  if (get_segtype(seg_ptr,j) == WORD ||
			  get_segtype(seg_ptr,j) == STRING)
        { set_seghash(seg_ptr,j,(get_seghash(orig_ptr,i))); }
      else
        { set_segfloat(seg_ptr,j,get_segfloat(orig_ptr,i)); }
     }

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   set_vptype(seg_value,MULTIPLE);
   set_vpbegin(seg_value,1);
   set_vpend(seg_value,end - start);
   set_vpsegment(seg_value,seg_ptr);
   return(CLIPS_TRUE);
  }

/*********************************************/
/* MV_APPEND:  Append function for segments. */
/*********************************************/
int mv_append(seg_value)
  VALUE_PTR seg_value;
  {
   VALUE val_ptr;
   VALUE_PTR val_arr;
   SEGMENT seg_ptr, orig_ptr;
   int start, end, i, j, k, seg_size, num_a;

   num_a = num_args(); 

   /*=========================================*/
   /* If no arguments are given return a NULL */
   /* multifield variable.                    */ 
   /*=========================================*/

   if (num_a == 0)
     {
       set_vptype(seg_value,MULTIPLE);
       set_vpbegin(seg_value,1);
       set_vpend(seg_value,0);
       seg_ptr = get_segment(0);
       set_vpsegment(seg_value,seg_ptr);
       return(CLIPS_TRUE);
     }

   else
     {

      /*========================================*/
      /* Get a new segment with length equal to */
      /* the total length of all the arguments. */
      /*========================================*/

      val_arr = (VALUE_PTR) gm1(sizeof(VALUE) * num_a);
      seg_size = 0;
      for(i = 1; i <= num_a; i++) 
        {
         runknown(i,&val_ptr);
         set_vptype(val_arr+i-1,get_valtype(val_ptr));
         if(get_valtype(val_ptr) == MULTIPLE)
           {
            set_vpsegment(val_arr+i-1,get_vpsegment(&val_ptr));
            start = get_valbegin(val_ptr);
            end = get_valend(val_ptr);
           }
         else if (get_valtype(val_ptr) == NUMBER)
           {
             set_vpfloat(val_arr+i-1,get_valfloat(val_ptr));
             start = end = -1;
           }
         else
           {
             set_vphash(val_arr+i-1,get_valhash(val_ptr));
             start = end = -1;
           }
         seg_size += end - start + 1;
         set_vpbegin(val_arr+i-1,start);
         set_vpend(val_arr+i-1,end);
        }
      seg_ptr = get_segment(seg_size);
   
      /*========================================*/
      /* Copy each argument into new segment.  */
      /*========================================*/
   
      for(k=0,j=1; k < num_a;k++) 
        {
         if (get_vptype(val_arr+k) == MULTIPLE)
           {
            start = get_vpbegin(val_arr+k);
            end = get_vpend(val_arr+k);
            orig_ptr = get_vpsegment(val_arr+k);
            for(i=start; i< end + 1; i++,j++)
              {
               set_segtype(seg_ptr,j,(get_segtype(orig_ptr,i)));
               if (get_segtype(seg_ptr,j) == NUMBER)
                 { set_segfloat(seg_ptr,j,get_segfloat(orig_ptr,i)); }
               else
                 { set_seghash(seg_ptr,j,(get_seghash(orig_ptr,i))); }
              }
           }
         else
		   {
            set_segtype(seg_ptr,j,(get_vptype(val_arr+k)));
            if (get_segtype(seg_ptr,j) == NUMBER)
              { set_segfloat(seg_ptr,j,get_vpfloat(val_arr+k)); }
            else
              { set_seghash(seg_ptr,j,(get_vphash(val_arr+k))); }
            j++; 
		   }
        }
         
      /*=========================*/
      /* Return the new segment. */
      /*=========================*/
   
      set_vptype(seg_value,MULTIPLE);
      set_vpbegin(seg_value,1);
      set_vpend(seg_value,seg_size);
      set_vpsegment(seg_value,seg_ptr);
      rm(val_arr,sizeof(VALUE) * num_a);
      return(CLIPS_TRUE);
     }
  }

/*********************************************/
/* STR_EXPLODE:  Explodes a string to a      */
/*   segment variable and returns the new.   */
/*   variable.                               */
/*********************************************/
int str_explode(str_value)
  VALUE_PTR str_value;
  {
   VALUE val_ptr;
   SEGMENT seg_ptr, t_assert();
   int end;

  if (arg_num_check("str-explode",EXACTLY,1) == -1)
    {
      set_execution_error(CLIPS_TRUE);
      segment_error_values(str_value);
      return(CLIPS_FALSE);
     }

  if (arg_type_check("str-explode",1,STRING,&val_ptr) == FALSE)
    {
      set_execution_error(CLIPS_TRUE);
      segment_error_values(str_value);
      return(CLIPS_FALSE);
     }

  seg_ptr = t_assert(get_valstring(val_ptr));
  if (seg_ptr == NULL) 
    {
     seg_ptr = get_segment(0);
     end = 0;
    }
  else
    { 
     add_to_segment_list(seg_ptr);
     end = get_seglength(seg_ptr);
    }

  /*=========================*/
  /* Return the new segment. */
  /*=========================*/

  set_vptype(str_value,MULTIPLE);
  set_vpbegin(str_value,1);
  set_vpend(str_value,end);
  set_vpsegment(str_value,seg_ptr);
  return(CLIPS_TRUE);
  } 

/*********************************************/
/* STR_IMPLODE:  Implodes a segment variable */
/*   to a string and returns the string.     */
/*********************************************/
struct draw *str_implode()
  {
   extern char *num_to_string();
   VALUE val_ptr;
   int strsize = 0;
   int i, j;
   char *tmp_str, *ret_str;
   HASH_PTR rv;
   
   if (arg_num_check("str-implode",EXACTLY,1) == -1)
     {
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }

   if (arg_type_check("str-implode",1,MULTIPLE,&val_ptr) == FALSE)
     { 
      set_execution_error(CLIPS_TRUE);
      return(add_symbol(""));
     }   

   /*===================================================*/
   /* Determine the size of the string to be allocated. */
   /*===================================================*/

   
   for (i = get_vpbegin(&val_ptr) ; i <= get_vpend(&val_ptr) ; i++)
     {
      if ((get_elmtype(get_vpelement(&val_ptr,i))) == NUMBER)
        {
         tmp_str = num_to_string(get_elmfloat(get_vpelement(&val_ptr,i)));
         strsize += strlen(tmp_str) + 1;
        }
      else if ((get_elmtype(get_vpelement(&val_ptr,i))) == STRING)
        {  
         strsize += strlen(get_elmstring(get_vpelement(&val_ptr,i))) + 3;
         tmp_str = get_elmstring(get_vpelement(&val_ptr,i));
         while(*tmp_str)
           {
            if(*tmp_str == '"') 
              { strsize++; }
            tmp_str++;
           }
        }
      else
        { strsize += strlen(get_elmstring(get_vpelement(&val_ptr,i))) + 1; }
     }

   /*=============================================*/
   /* Allocate the string and copy all components */
   /* of the MULTIPLE variable to it.             */
   /*=============================================*/

   if (strsize == 0) return(add_symbol(""));
   ret_str = gm2(strsize);
   for(j=0, i=get_vpbegin(&val_ptr); i <= get_vpend(&val_ptr) ; i++)
     {

      /*============================*/
      /* Convert numbers to strings */
      /*============================*/

      if ((get_elmtype(get_vpelement(&val_ptr,i))) == NUMBER)
        {
         tmp_str = num_to_string(get_elmfloat(get_vpelement(&val_ptr,i)));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }

      /*=======================================*/
      /* Enclose strings in quotes and preceed */
      /* imbedded quotes with a backslash      */
      /*=======================================*/

      else if ((get_elmtype(get_vpelement(&val_ptr,i))) == STRING)
        {
         tmp_str = get_elmstring(get_vpelement(&val_ptr,i));
         *(ret_str+j) = '"';
         j++;
         while(*tmp_str)
           {
            if(*tmp_str == '"')
              {
               *(ret_str+j) = '\\';
               j++;
              } 
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str+j) = '"';
         j++;
        }

      else
        {
         tmp_str = get_elmstring(get_vpelement(&val_ptr,i));
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         }
      *(ret_str+j) = ' ';
      j++;
     }
   *(ret_str+j-1) = '\0';

   /*====================*/
   /* Return the string. */
   /*====================*/
   
   rv = add_symbol(ret_str);
   rm(ret_str,strsize);
   return(rv);
  }
  
/****************************************************/
/* SUB_SEQUENCE: Subsequence function for segments. */
/****************************************************/
sub_sequence(sub_value)
  VALUE_PTR sub_value;
  {
   VALUE val_ptr;
   EXPR_PTR test_ptr;
   int start, end, length;
   extern float numget();
   
   /*========================================*/
   /* Check for correct number of arguments. */
   /*========================================*/
   
   if (arg_num_check("mv-subseq",EXACTLY,3) == -1)
     { 
      segment_error_values(sub_value);
      return; 
     }
     
   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */  
   /*=============================================*/
   
   test_ptr = get_first_arg();
   start = (int) numget(test_ptr,"mv-subseq");
   
   test_ptr = get_next_arg(test_ptr);
   end = (int) numget(test_ptr,"mv-subseq");
   
   if ((end < 1) || (end < start))
     { 
      segment_error_values(sub_value);
      return; 
     }

   /*==================================================*/
   /* Get the segment to be subdivided. Adjust lengths */  
   /* to conform to segment boundaries.                */
   /*==================================================*/
   
   test_ptr = get_next_arg(test_ptr);
   generic_compute(test_ptr,&val_ptr);
   if (get_valtype(val_ptr) != MULTIPLE)
     { 
      cl_print("werror","Function mv-subseq expected a ");
      cl_print("werror","multi-field value as third argument\n");
      set_execution_error(TRUE);
      segment_error_values(sub_value);
      return; 
     }
     
   length = get_vallength(val_ptr);
   if (start > length) 
     {
      segment_error_values(sub_value); 
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;
   
   /*=========================*/
   /* Return the new segment. */
   /*=========================*/
   
   set_vptype(sub_value,MULTIPLE);
   set_vpsegment(sub_value,get_valsegment(val_ptr));
   set_vpend(sub_value,get_valbegin(val_ptr) + end - 1);
   set_vpbegin(sub_value,get_valbegin(val_ptr) + start - 1);
   return;
  }
  

/**********************************************************/
/* SEGMENT_ERROR_VALUES:                                  */
/**********************************************************/
segment_error_values(sub_value)
  VALUE_PTR sub_value;
  {
   set_vptype(sub_value,MULTIPLE);
   set_vpsegment(sub_value,get_segment(0));
   set_vpbegin(sub_value,1);
   set_vpend(sub_value,0);
  }
#endif
