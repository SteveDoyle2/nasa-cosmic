/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_
#include "network.h"
#endif

/************************************************************/
/* RULEINFO STRUCTURE:                                      */
/************************************************************/
struct ruleinfo
  {
   char *name;
   struct ruleinfo *next;
   struct patptr *pats;
   char *pp_form;                 
  };
  
/************************************************************/
/* PATPTR STRUCTURE:                                        */
/************************************************************/
struct patptr
  {
   struct pat_node *pptr;
   struct list *lptr;			
   struct patptr *next;
  };

extern char             *get_rule_ppform();
extern char             *get_rule_name();
extern struct ruleinfo  *get_next_rule();
extern char             *get_currentrule();
extern int               set_currentrule();
