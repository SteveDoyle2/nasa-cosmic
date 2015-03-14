/*   CLIPS Version 4.30   4/25/89 */

#ifndef _EXPRESSN_

#define _EXPRESSN_

#ifndef _SYMBOL_
#include "symbol.h"
#endif

/************************************************************/
/* TEST STRUCTURE:                                          */
/************************************************************/
struct test 
   {
    int type;
    union
      {
       float fvalue;
       int index;
       struct funtab *fun_ptr;
       struct draw *hvalue;
       char *s_ptr;
      } val; 
    struct test *arg_list;
    struct test *next_arg;
   };
   
/************************************************************/
/* FUNTAB STRUCTURE:  Stores information about all user and */
/*   system defined functions.                              */
/*   Fun_name:  The name of the defined function.           */
/*   Fun_type:  Return value type of the function.          */
/*   Ip:        Pointer to the defined function.            */
/*   Next:      Pointer to the next function definition.    */
/************************************************************/
struct funtab
  {
   char *fun_name;
   char *defn_name; 
   char fun_type;  
   int (*ip)();
   struct funtab *next;
  };
  
/************************************************************/
/* CONSTRUCT STRUCTURE:                                          */
/************************************************************/
struct construct
  {
   char *name;  
   int (*ip)();
   struct construct *next;
  };

/************************************************************/
/* FUNC_PARSER STRUCTURE:                                          */
/************************************************************/
struct func_parser
  {
   char *name;  
   struct test *(*ip)();
   struct func_parser *next;
  };

#endif
