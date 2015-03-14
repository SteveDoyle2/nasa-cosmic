/*   CLIPS Version 4.30   4/25/89 */

#ifndef _EXPRESSN_
#include "expressn.h"
#endif

struct pn_test
  {
   int element;
   int type;              /* Type of pattern: Single or Multiple */
   struct test *pnt;      /* Test associated with pattern.       */
   struct pn_test *next;
  };
  
struct field_tests
  {
   struct test *pnt;
   struct test *jnt;
  };
  
  
struct expr_info
  {
   int pattern;
   char state;
   struct test *jnt;
   struct pn_test *pntl;
   struct test *otest;
   struct expr_info *next;
  };

   
