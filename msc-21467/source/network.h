/*   CLIPS Version 4.30   4/25/89 */

#ifndef _NETWORK_

#define _NETWORK_

#ifndef _EXPRESSN_
#include "expressn.h"
#endif

/*******************************************************************/
/* Pat_node:                                                       */
/* Used to join together the tree of elements which constitute     */
/* the pattern network.  For a given node, last_level points to    */
/* the previous element in the pattern, same_level points to other */
/* elements which may fill this slot in the pattern, and next      */
/* level points to the elements which may fill the next position   */
/* assuming that this element is matched.  Check_list holds the    */
/* actual pattern element (e.g. (?, blue | red , ~3, $?, end of    */
/* pattern)).  If the element to be matched is the end of the      */
/* pattern, then var_list will contain the list of variable        */ 
/* bindings, functions call, and predicate tests which must be     */
/* made for the pattern.                                           */
/*******************************************************************/
struct pat_node
  {
   struct pat_node *same_level;
   struct pat_node *prev;
   struct pat_node *next_level;
   struct pat_node *last_level;
   int type;
   struct test *eval;
   struct list *path;
   struct flink *alpha;
  };

/************************************************************/
/* LIST STRUCTURE:                                          */
/************************************************************/
struct list
  {
   struct internode *path; 
   struct list *next;         
  };

/************************************************************/
/* INTERNODE STRUCTURE:                                     */
/************************************************************/
struct internode
  {
   struct flink *beta;        /* Bindings from left hand side      */
   char lhs_log;              /* Left hand side logic:  '+' or 'e' */
   char rhs_log;              /* Right hand side logic: '+' or '-' */
   struct test *eval;
   struct test *not_eval;
   long int id;               /* Not id if "e -" join              */
   struct list *next;         /* The next join to enter            */
   struct internode *join_above;
   struct pat_node *entry_pat;
  };

/************************************************************/
/* MATCH STRUCTURE:                                         */
/************************************************************/
struct match
  {
   struct match *next;
   struct pat_node *slot;
  };


/************************************************************/
/* FLINK STRUCTURE:                                         */
/************************************************************/
struct flink
  {
   struct fbind *binds;
   long int count;
   struct flink *next;
  };

/************************************************************/
/* FBIND STRUCTURE:                                         */
/************************************************************/
struct fbind
  {
   struct fbind *next;
   long int whoset;
   struct fact *origin;
   struct fact_marker *marker;
  };

/********************************************************/
/* Fact_marker is used in the pattern matching process  */
/* to match $? and $?variables.  It is used to indicate */
/* that a single pattern element may span more than one */
/* fact element.                                        */
/********************************************************/

struct fact_marker
   {
    int element;
    int start;                                   
    int end;
    struct fact_marker *next;
   };

/************************************************************/
/* FACT STRUCTURE:                                          */
/************************************************************/
struct fact
  {
   struct element *atoms;   
   struct match *list;          
   long int ID;
   int fact_length;               
   struct fact *next;     
   struct fact *previous;  
  };

/*************************************************************/
/* ELEMENT STRUCTURE:                                        */
/*************************************************************/
struct element
  {
   int type;
   union
     {
      struct draw *hvalue;
      float fvalue;
     } val;
   struct element *next;
  };
  
/***********************************************************/
/* FHASH STRUCTURE:                                        */
/***********************************************************/
struct fhash 
  {
   struct fact *fptr;
   struct fhash *next;
  };
  
extern struct fact *get_next_fact();

#define SIZE_FACT_HASH  541

#endif
