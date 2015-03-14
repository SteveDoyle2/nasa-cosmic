/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_
#include "network.h"
#endif

/************************************************************/
/* ACTIVATION STRUCTURE:                                    */
/************************************************************/
struct activation
  {
   struct flink *basis;         
   struct test *actions;
   long int id;
   char *rule;
   int salience;                    
   struct activation *next;       
  };
  
/************************************************************/
/* EXEC_FUNC STRUCTURE:                                     */
/************************************************************/
struct exec_func
  {
   char *name;  
   int (*ip)();
   struct exec_func *next;
  };
  
extern struct activation *get_next_activation();

#define activation_rule_name(act_ptr) (act_ptr->rule)
#define activation_basis(act_ptr) (act_ptr->basis->binds)
#define activation_salience(act_ptr) (act_ptr->salience)
