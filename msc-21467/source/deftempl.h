/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_
#include "network.h"
#endif

/***********************************************************************/
/* DTMPL STRUCTURE:  Stores information about a deftemplate construct. */
/*   Name:     The name of the deftemplate construct.                  */
/*   Pp_form:  The string representation of the deftemplate construct. */
/*               Used to pretty print deftemplates constructs.         */
/*   Flist:    Pointer to the list of facts defined by this            */
/*               deftemplate construct.                                */
/*   Next:     Pointer to the next deftemplate definition structure.   */
/***********************************************************************/
struct dtmpl
  {
   char *name;
   char *pp_form; 
   struct slot *slot_list;             
   struct dtmpl *next; 
  };
  
struct slot_item
  {
   int type;
   union 
     {
      float number;
      struct draw *string;
     } value;
   struct slot_item *next;
  };
  
struct slot
  {
   struct draw *slot_name;
   struct slot_item *item_list;
   short int multi_slot;            
   struct slot *next;
  };

struct slp
  {
   struct draw *name;
   struct node *expr;
   struct slp *next;
  };
  
struct sap
  {
   struct draw *name;
   struct test *expr;
   struct sap *next;
  };
  
struct dtmpl *get_next_deftemplate();
char *get_deftemplate_name();
char *get_deftemplate_ppform();
