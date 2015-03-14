/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_
#include "network.h"
#endif

/*===================*/
/* Type definitions. */
/*===================*/

struct values
  {
   int type;
   char *name;
   union
     {
      struct draw *hvalue;
      float fvalue;
     } val;
   struct fact *origin;
   int begin;
   int end;
   struct values *next;
  };

typedef struct fact * SEGMENT;
typedef struct values VALUE;
typedef struct values * VALUE_PTR;
typedef struct test * EXPR_PTR;
typedef struct element * ELEMENT_PTR;
typedef struct draw * HASH_PTR;

/****************************************************************/
/* ------------ Functions for use in user programs ------------ */
/****************************************************************/

   extern int              define_function();    
   extern int              num_args();           
   extern float            rfloat();              
   extern char            *rstring();
   extern struct draw     *rhash();
   extern float            rmulfloat();
   extern char            *rmulstring();
   extern struct draw     *rmulhash();
   extern int              rmultype();         
   extern struct values   *runknown(); 

   extern int              act_router();
   extern int              add_router();
   extern int              cl_exit();
   extern int              cl_getc();
   extern int              cl_print();
   extern int              cl_ungetc();
   extern int              deact_router();
   extern int              del_router();
   extern struct values   *make_unknown();
   extern int              log_namep();
   extern struct draw     *add_symbol();
   extern SEGMENT          get_segment();
   extern struct fact     *get_el();
   extern struct fact     *assert();
   extern struct fact     *add_fact();
   extern char            *symbol_string();
   extern int              generic_compute();
   
/*==========================================================*/
/* Macros for accessing expressions directly with EXPR_PTR. */
/*==========================================================*/

extern struct test                *new_fctn_args;
   
#define get_first_arg()           (new_fctn_args->arg_list)
#define get_next_arg(test_ptr)    (test_ptr->next_arg)
#define get_arg_list(test_ptr)    (test_ptr->arg_list)
#define get_test_type(test_ptr)   (test_ptr->type)
#define get_test_index(test_ptr)  (test_ptr->val.index)
#define get_test_float(test_ptr)  (test_ptr->val.fvalue)
#define get_test_hash(test_ptr)   (test_ptr->val.hvalue)
#define get_test_string(test_ptr) (test_ptr->val.hvalue->contents)
#define get_test_func(test_ptr)   (test_ptr->val.fun_ptr)

/*===========================================*/
/* Macros for accessing values of a SEGMENT. */
/*===========================================*/
    
#define set_segtype(target,index,value)  ((target)->atoms[index-1].type = (value))  
#define set_segfloat(target,index,value) ((target)->atoms[index-1].val.fvalue = (value))  
#define set_seghash(target,index,value)  ((target)->atoms[index-1].val.hvalue = (value))  
#define set_segstring(target,index,value)  \
                     ((target)->atoms[index-1].val.hvalue = add_symbol(value))  

#define get_segtype(target,index)  ((target)->atoms[index-1].type) 
#define get_segfloat(target,index) ((target)->atoms[index-1].val.fvalue)  
#define get_seghash(target,index)  ((target)->atoms[index-1].val.hvalue) 
#define get_segstring(target,index) \
                         ((target)->atoms[index-1].val.hvalue->contents)
#define get_seglength(target) ((target)->fact_length) 

/*================================================*/
/* Macros for accessing values of an ELEMENT_PTR. */
/*================================================*/
    
#define set_elmtype(target,value)  ((target)->type = (value))  
#define set_elmfloat(target,value) ((target)->val.fvalue = (value))  
#define set_elmhash(target,value)  ((target)->val.hvalue = (value))  

#define get_elmtype(target)  ((target)->type) 
#define get_elmfloat(target) ((target)->val.fvalue)  
#define get_elmhash(target)  ((target)->val.hvalue)  
#define get_elmstring(target) ((target)->val.hvalue->contents)

/*=============================================*/
/* Macros for accessing values of a VALUE_PTR. */
/*=============================================*/
    
#define set_vptype(target,value)    ((target)->type = (value))  
#define set_vpfloat(target,value)   ((target)->val.fvalue = (value))  
#define set_vphash(target,value)    ((target)->val.hvalue = (value))
#define set_vpstring(target,value)  ((target)->val.hvalue = add_symbol(value))  
#define set_vpbegin(target,value)   ((target)->begin = (value) - 1) 
#define set_vpend(target,value)     ((target)->end = (value) - 1)
#define set_vpsegment(target,value) ((target)->origin = (value))

#define get_vptype(target)          ((target)->type) 
#define get_vpfloat(target)         ((target)->val.fvalue)  
#define get_vphash(target)          ((target)->val.hvalue)  
#define get_vpstring(target)        ((target)->val.hvalue->contents)   
#define get_vplength(target)        (((target)->type == MULTIPLE) ? \
                                       (((target)->end - (target)->begin) + 1) : -1)
#define get_vpbegin(target)         (((target)->type == MULTIPLE) ? \
                                       ((target)->begin + 1) : -1)
#define get_vpend(target)           (((target)->type == MULTIPLE) ? \
                                       ((target)->end + 1) : -1)
#define get_vpsegment(target)       (((target)->type == MULTIPLE) ? \
                                       ((target)->origin) : NULL)
#define get_vpelement(item,pos)     (&(item)->origin->atoms[pos - 1])

/*=========================================*/
/* Macros for accessing values of a VALUE. */
/*=========================================*/
    
#define set_valtype(target,value)    ((target).type = (value))  
#define set_valfloat(target,value)   ((target).val.fvalue = (value))  
#define set_valhash(target,value)    ((target).val.hvalue = (value))
#define set_valstring(target,value)  ((target).val.hvalue = add_symbol(value)) 
#define set_valbegin(target,value)   ((target).begin = (value) - 1) 
#define set_valend(target,value)     ((target).end = (value) - 1)
#define set_valsegment(target,value) ((target).origin = (value)) 

#define get_valtype(target)          ((target).type) 
#define get_valfloat(target)         ((target).val.fvalue)  
#define get_valhash(target)          ((target).val.hvalue)  
#define get_valstring(target)        ((target).val.hvalue->contents)  
#define get_vallength(target)        (((target).type == MULTIPLE) ? \
                                      (((target).end - (target).begin) + 1) : -1)
#define get_valbegin(target)         (((target).type == MULTIPLE) ? \
                                      ((target).begin + 1) : -1)
#define get_valend(target)           (((target).type == MULTIPLE) ? \
                                      ((target).end + 1) : -1)
#define get_valsegment(target)       (((target).type == MULTIPLE) ? \
                                       ((target).origin) : NULL)
#define get_valelement(item,pos)     (&(item).origin->atoms[pos - 1])

/*======================================*/
/* Old 4.1 Macros for accessing values. */
/*======================================*/

#define rtype(result)          ((result).type) 
#define rvalstring(result)     ((result).val.hvalue->contents)
#define rvalfloat(result)      ((result).val.fvalue)
#define rlength(result)        (((result).type == MULTIPLE) ? \
                                (((result).end - (result).begin) + 1) : -1)
      
 
