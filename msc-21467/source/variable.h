/*   CLIPS Version 4.30   4/25/89 */

#ifndef _SYMBOL_
#include "symbol.h"
#endif

struct pat_info
  {
   int position;
   char state;
   struct draw *fact_address;
   struct draw *relation;
   struct var_info *variables;
   struct pat_info *next;
  };
  
struct var_info
  {
   int type;
   struct draw *name;
   int pattern;
   int element;
   struct var_info *next;
  };
   
