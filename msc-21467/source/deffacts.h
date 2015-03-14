/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_
#include "network.h"
#endif

/********************************************************************/
/* DFACT STRUCTURE:  Stores information about a deffacts construct. */
/*   Name:     The name of the deffacts construct.                  */
/*   Pp_form:  The string representation of the deffacts construct. */
/*               Used to pretty print deffacts constructs.          */
/*   Flist:    Pointer to the list of facts defined by this         */
/*               deffacts construct.                                */
/*   Next:     Pointer to the next deffacts definition structure.   */
/********************************************************************/
struct dfact
  {
   char *name;
   char *pp_form;
   struct test *alist;                
   struct dfact *next; 
  };

extern struct dfact   *find_deffact();
extern struct dfact   *get_next_deffact();
extern char           *get_deffact_name();
extern char           *get_deffact_ppform();
