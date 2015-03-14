/*   CLIPS Version 4.30   4/25/89 */

#ifndef _SYMBOL_
#include "symbol.h"
#endif

struct token
  {
   int token;
   float tknnumber;
   char *tknword;
   struct draw *hashword;
   char *print_rep;
  };
     
#define WORDLENGTH 512
