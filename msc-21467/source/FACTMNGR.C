/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                FACT MANAGER MODULE                  */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "scanner.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   struct fact            *get_el();
   struct fact            *add_fact();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct draw     *add_symbol();
   extern struct element  *fast_gv();
   extern struct pat_node *network_pointer();
   extern char            *symbol_string();

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct fact     *garbage_facts = NULL;
   static struct fhash    *fact_hashtable[SIZE_FACT_HASH];
   static int              watch_facts;
   static struct fact     *last_fact;
   static struct fact     *factlist;
   static long int         ID; 
   static int              change_facts = FALSE; 
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern struct fbind    *gbl_lhs_binds;
   extern struct fbind    *gbl_rhs_binds;
   extern struct funtab   *PTR_GET_VAR;
   
/*******************************************************************/
/* FACT_COMPARE:  Determines if two facts are identical.  Returns  */ 
/*   TRUE (1) if identical, and FALSE (0) if not identical.        */
/*******************************************************************/
fact_compare(fact1,fact2)
  struct fact *fact1, *fact2; 
  {
   struct element *elem1, *elem2;
   int length, i = 0;

   length = fact1->fact_length;
   if (length != fact2->fact_length)
     { return(FALSE); }

   elem1 = fact1->atoms;
   elem2 = fact2->atoms;

   /*==================================================*/
   /* Compare each field of both facts until the facts */
   /* match completely or the facts mismatch.          */
   /*==================================================*/

   while (i < length)
     {
      /*=========================================*/
      /* Check that the fields are the same type */
      /* (i.e. string or float).                 */
      /*=========================================*/

      if (elem1[i].type != elem2[i].type)
        { return(FALSE); }

      /*=============================================*/
      /* Check to see that the fields match exactly. */
      /*=============================================*/

      if (elem1[i].type == NUMBER)
        {
         if (elem1[i].val.fvalue != elem2[i].val.fvalue)
           { return(FALSE); }
        }
      else
        {
         if (elem1[i].val.hvalue != elem2[i].val.hvalue)
           { return(FALSE); }
        }

      i++;
     }
   return(TRUE); 
  }
  
/***************************************/
/* DISPLAYFACTS:                       */
/***************************************/
displayfacts()
  {   
   struct fact *fact_ptr;
   
   fact_ptr = get_next_fact(NULL); 
   while (fact_ptr != NULL)
     {
      show_fact("wdisplay",fact_ptr);
      cl_print("wdisplay","\n");
      fact_ptr = get_next_fact(fact_ptr);
     }
  }
  
/***************************************/
/* SHOW_FACT:  Displays a single fact. */
/***************************************/
show_fact(log_name,fact_ptr)
  char *log_name;
  struct fact *fact_ptr;
  {
   char print_space[20];
   
   sprintf(print_space,"f-%-5ld (",fact_ptr->ID);
   cl_print(log_name,print_space);
   show_elements(log_name,fact_ptr);
   cl_print(log_name,")");
  }
  
/************************************************/
/* SHOW_ELEMENTS:  Displays elements of a fact. */
/************************************************/
show_elements(log_name,fact_ptr)
  char *log_name;
  struct fact *fact_ptr;
  {
   struct element *sublist;
   int length, i;

#if DEFTEMPLATES && (! RUN_TIME) && (! BLOAD_ONLY)
   if (PrintTemplateFact(log_name,fact_ptr) == TRUE) return;
#endif

   sublist = fact_ptr->atoms;
   length = fact_ptr->fact_length;

   for (i = 0; i < length ; i++)
     {
      print_element(log_name,&sublist[i]);
      if (i + 1 != length)
        { cl_print(log_name," "); }
     }
  }

/************************************************/
/* PRINT_ELEMENT: */
/************************************************/
print_element(log_name,elem_ptr)
  char *log_name;
  struct element *elem_ptr;
  {
   if (elem_ptr->type == NUMBER) 
     { print_num(log_name,elem_ptr->val.fvalue); }
   else if (elem_ptr->type == WORD)
     { cl_print(log_name,symbol_string(elem_ptr->val.hvalue)); }
   else if (elem_ptr->type == STRING)
     { 
      cl_print(log_name,"\"");
      cl_print(log_name,symbol_string(elem_ptr->val.hvalue));
      cl_print(log_name,"\""); 
     }
  }
  

/******************************************************/
/* HASH_FACT:  Returns a hash value for a given fact. */
/******************************************************/
int hash_fact(fptr)
  struct fact *fptr;
  {
   int length, i = 0;
   unsigned short int tvalue, hash_value = 0;
   struct element *eptr;
   union
     {
      float fv;
      unsigned short int liv;
     } fis;

   length = fptr->fact_length;
   eptr = fptr->atoms;

   while (i < length)
     {
      if (eptr[i].type == NUMBER)
        {
         fis.fv = eptr[i].val.fvalue;
         tvalue = fis.liv;
        } 
      else
       { tvalue = ((unsigned short int) eptr[i].val.hvalue) / sizeof(char *); }
      hash_value += tvalue; 
      i++;
     }

   hash_value = hash_value % SIZE_FACT_HASH;
   return( (int) hash_value); 
  }

/******************************************************************/
/* FACT_EXISTS:  Determines if a fact in the the fact hash table. */
/*   Returns TRUE (1) if it is and FALSE (0) otherwise.           */
/******************************************************************/
int fact_exists(fptr,hash_value)
  struct fact *fptr;
  int hash_value;
  {
   struct fhash *hash_ptr;

   hash_ptr = fact_hashtable[hash_value];

   while (hash_ptr != NULL)
     {
      if (fact_compare(fptr,hash_ptr->fptr) == 1) return(1);
      hash_ptr = hash_ptr->next;
     }

   return(0);
  }

/*******************************************************/
/* ADD_HASH_FACT:  Adds a fact to the fact hash table. */
/*******************************************************/
add_hash_fact(fptr,hash_value)
  struct fact *fptr;
  int hash_value;
  {
   struct fhash *newhash, *temp;

   newhash = get_struct(fhash);
   newhash->fptr = fptr;

   temp = fact_hashtable[hash_value];
   fact_hashtable[hash_value] = newhash;
   newhash->next = temp;
  }

/************************************************************/
/* DEL_HASH_FACT:  Removes a fact from the fact hash table. */
/************************************************************/
del_hash_fact(fptr)
  struct fact *fptr;
  {
   int hash_value;
   struct fhash *hptr, *prev = NULL;

   hash_value = hash_fact(fptr);
   
   hptr = fact_hashtable[hash_value];
   while (hptr != NULL)
     {
      if (hptr->fptr == fptr)
        {
         if (prev == NULL)
           { 
            fact_hashtable[hash_value] = hptr->next;
            rtn_struct(fhash,hptr);
            return(1);
           }
         else
           {
            prev->next = hptr->next;
            rtn_struct(fhash,hptr);
            return(1);
           }
        }
      prev = hptr;
      hptr = hptr->next;
     }
   return(0);
  }
  

/*************************************************************/
/* RETRACT_FACT:  Retracts a fact from the fact list given a */
/*   pointer to the fact.                                    */
/*************************************************************/
retract_fact(fact_ptr)
  struct fact *fact_ptr;  
  {
   FACT_ID fact_num;
   struct fact *temp_ptr;
   struct match *match_list;
   char print_space[20];

   /*======================================================*/
   /* Check to see if the fact has already been retracted. */
   /*======================================================*/

   temp_ptr = garbage_facts;
   while (temp_ptr != NULL)
     { 
      if (temp_ptr == fact_ptr) 
        { return(0); }
      temp_ptr = temp_ptr->next;
     }

   /*=========================================*/
   /* Show retraction if facts being watched. */
   /*=========================================*/

   fact_num = fact_ptr->ID;

   if (watch_facts == ON)
     {
      cl_print("wtrace","<== ");
      show_fact("wtrace",fact_ptr);
      cl_print("wtrace","\n");
     } 
     
   if (get_crsv_trace_watch() == ON)
     {
	  cl_print("wcrsv_tr","R    ");
      sprintf(print_space,"%-5ld  ",fact_ptr->ID);
      cl_print("wcrsv_tr",print_space);
	  print_element("wcrsv_tr",&(fact_ptr->atoms[0]));        
      cl_print("wcrsv_tr","\n");
     }  
     
   change_facts = TRUE;
    
   /*=====================================*/
   /* Delete the fact from the fact list. */
   /*=====================================*/

   del_hash_fact(fact_ptr);
   /* Save the list of pattern matches. */
   match_list = fact_ptr->list;

   if (fact_ptr == last_fact)
     { last_fact = fact_ptr->previous; }
   
   if (fact_ptr->previous == NULL)
     {
      /* Delete the head of the fact list. */
      factlist = factlist->next;
      if (factlist != NULL)
        { factlist->previous = NULL; }
     }
   else
     {
      /* Delete a fact other than the head of the fact list. */
      fact_ptr->previous->next = fact_ptr->next;
      if (fact_ptr->next != NULL)
        { fact_ptr->next->previous = fact_ptr->previous; }
     }

   temp_ptr = garbage_facts;
   garbage_facts = fact_ptr;
   fact_ptr->next = temp_ptr;

   /*================================================*/
   /* Loop through the list of all the patterns that */
   /* matched the fact.                              */
   /*================================================*/

   match_retract(match_list,fact_num);

   /*===============================================*/
   /* Remove all activations that contain this fact */
   /* from the agenda.                              */
   /*===============================================*/

   purge_agenda(fact_num);
   return(1);
  }
  
/********************************************************************/
/* RMV_OLD_FACTS:  Returns facts that have been retracted to the    */
/*   pool of available memory.  It is necessary to postpone         */
/*   returning the facts to memory because rhs actions retrieve     */
/*   their variable bindings directly from the fact data structure. */
/********************************************************************/
rmv_old_facts()
  {
   struct fact *fact_ptr;
	
   while (garbage_facts != NULL)
     {
      fact_ptr = garbage_facts;
      garbage_facts = garbage_facts->next;
      fact_deinstall(fact_ptr);
      rtn_el(fact_ptr);
     }
  }

/*********************************************************************/
/* ADD_FACT: Places a fact onto the end of the fact list and calls   */
/*   compare to filter the fact through the pattern network. Returns */  
/*   null if the fact was already in the knowledge base, and a       */
/*   pointer to the fact if it was not in the knowledge base.        */
/*********************************************************************/
struct fact *add_fact(new_fact)
  struct fact *new_fact;
  {
   int hash_value;
   char print_space[20];

   /*========================================================*/
   /* If fact assertions are being checked for duplications, */
   /* then search the fact list for a duplicate fact.        */
   /*========================================================*/

   hash_value = hash_fact(new_fact);
   
   if (fact_exists(new_fact,hash_value) == 1)
     {
      rtn_el(new_fact);
      return(NULL);
     }
        
   add_hash_fact(new_fact,hash_value);

   /*===================================================*/
   /* Add the fact to the fact list. Set the ID for the */ 
   /* fact and install the symbols used by the fact in  */
   /* the symbol table.                                 */
   /*===================================================*/

   new_fact->next = NULL;
   new_fact->list = NULL;
   new_fact->previous = last_fact;
   if (last_fact == NULL)
     { factlist = new_fact; }
   else
     { last_fact->next = new_fact; }
   last_fact = new_fact;
   
   ID++;
   new_fact->ID = ID;
   fact_install(new_fact);

   /*===============================================*/
   /* Indicate the addition of the fact to the fact */
   /* list if facts are being watched.              */
   /*===============================================*/

   if (watch_facts == ON)
     {
      cl_print("wtrace","==> ");
      show_fact("wtrace",new_fact);
      cl_print("wtrace","\n");
     }

   if (get_crsv_trace_watch() == ON)
     {
      cl_print("wcrsv_tr","AS   ");
      sprintf(print_space,"%-5ld (",new_fact->ID);
      cl_print("wcrsv_tr",print_space);
      show_elements("wcrsv_tr",new_fact);
      cl_print("wcrsv_tr",")");
      cl_print("wcrsv_tr","\n");
     }
     
   change_facts = TRUE;
   
   /*==============================================*/
   /* Filter the fact through the pattern network. */
   /*==============================================*/

   compare(new_fact,new_fact->atoms,network_pointer(),1,0,NULL,NULL);

   return(new_fact);
  }
  
/*********************************************************************/
/* REMOVE_ALL_FACTS: */
/*********************************************************************/
remove_all_facts()
  {
   struct fact *temp_fact;
   struct match *pat_matches, *temp_match;
   
   while (factlist != NULL)
     {
      temp_fact = factlist->next;
      pat_matches = factlist->list;
      while (pat_matches != NULL)
        {
         temp_match = pat_matches->next;
         rtn_struct(match,pat_matches);
         pat_matches = temp_match;
        } 
      del_hash_fact(factlist);
      fact_deinstall(factlist);
      rtn_el(factlist);
      factlist = temp_fact;
      change_facts = TRUE;
     }
   last_fact = NULL;
  }

/************************************************************/
/* fact_clear: clears the fact list of all pointers which   */
/*   point to a specific pattern.  The pointers are used to */
/*   to remember which patterns were matched by a fact to   */
/*   make retraction easier.  When a rule is excised, the   */
/*   pointers need to be removed.                           */
/************************************************************/
fact_clear(pat_ptr)
  struct pat_node *pat_ptr;
  {
   struct fact *fact_ptr;
   struct match *match_before, *match_ptr;
 
   /*===========================================*/
   /* Loop through every fact in the fact list. */
   /*===========================================*/
  
   fact_ptr = factlist;
   while (fact_ptr != NULL)
     {
      match_before = NULL;
      match_ptr = fact_ptr->list;

      /*========================================*/
      /* Loop through every match for the fact. */
      /*========================================*/

      while (match_ptr != NULL)
        {
         if (match_ptr->slot == pat_ptr)
           {
            /* Remove the match. */
	        if (match_before == NULL)
	          {
	           fact_ptr->list = match_ptr->next;
               rtn_struct(match,match_ptr);
               match_ptr = fact_ptr->list;
              }
            else
	         {
	          match_before->next = match_ptr->next;
	          rtn_struct(match,match_ptr);
              match_ptr = match_before->next;
             }
	       }
         else
	      {  
           /* Move on to the next match. */
	       match_before = match_ptr;
	       match_ptr = match_ptr->next;
          }
	   }
     fact_ptr = fact_ptr->next;
    }
  }

/*********************************************************************/
/* SET_FACTS_WATCH: */
/*********************************************************************/
set_facts_watch(value)
  int value;
  {
   watch_facts = value;
  }

/*********************************************************************/
/* GET_FACTS_WATCH: */
/*********************************************************************/
get_facts_watch()
  { return(watch_facts); }
  
/*************************************************************/
/* new assert command                                        */
/*************************************************************/
int fast_assert()
  {
   struct element *elem_a;
    
   int num_a, cur_arg;
   struct values arg_ptr;
   struct test *test_ptr;
   struct fact *fact_ptr;
   struct element *elem_ptr;
   int extent;
   struct fact *temp_ptr;
   int start, multi;
   
   num_a = num_args();
   cur_arg = 0;
   fact_ptr = get_el(num_a);
   elem_ptr = fact_ptr->atoms;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      if (test_ptr->type == NUMBER)
        {
         elem_ptr[cur_arg].type = NUMBER;
         elem_ptr[cur_arg].val.fvalue = test_ptr->val.fvalue;
        }
      else if ((test_ptr->type == STRING) || (test_ptr->type == WORD))
        {
         elem_ptr[cur_arg].type = test_ptr->type;
         elem_ptr[cur_arg].val.hvalue = test_ptr->val.hvalue;
        }
      else if (test_ptr->val.fun_ptr == PTR_GET_VAR)
        {
         elem_a = fast_gv(test_ptr->arg_list->val.index,
                          test_ptr->arg_list->next_arg->val.index,&extent,
                          &temp_ptr,&start,&multi);
         elem_ptr[cur_arg].type = elem_a->type;
         if (elem_a->type == NUMBER)
           { elem_ptr[cur_arg].val.fvalue = elem_a->val.fvalue; }
         else
           { elem_ptr[cur_arg].val.hvalue = elem_a->val.hvalue; }
        }
      else
        {       
         generic_compute(test_ptr,&arg_ptr);

         if (arg_ptr.type == NUMBER)
           {
            elem_ptr[cur_arg].type = NUMBER;
            elem_ptr[cur_arg].val.fvalue = arg_ptr.val.fvalue;
           }
         else
           {
            elem_ptr[cur_arg].type = arg_ptr.type;
            elem_ptr[cur_arg].val.hvalue = arg_ptr.val.hvalue;
           }
        }

      test_ptr = get_next_arg(test_ptr);
      cur_arg++;
     }
   
   
   add_fact(fact_ptr);

   return(1);
  }

/*************************************************************/
/* slow assert command                                       */
/*************************************************************/
int slow_assert()
  {
   struct element *elem_a;
    
   struct values arg_ptr;
   struct test *test_ptr;
   struct fact *fact_ptr, *temp_ptr;
   struct element *elem_ptr;
   struct element *head;
   struct element *last_add = NULL, *last_elem, *new_elem;
   int pat, elem, i;
   int count = 0;
   int extent, start, multi;
   
   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      if (test_ptr->type == NUMBER)
        {
         elem_ptr = get_struct(element);
         elem_ptr->type = NUMBER;
         elem_ptr->val.fvalue = test_ptr->val.fvalue;
         elem_ptr->next = NULL;
         count++;
        }
      else if ((test_ptr->type == STRING) || (test_ptr->type == WORD))
        {
         elem_ptr = get_struct(element);
         elem_ptr->type = test_ptr->type;
         elem_ptr->val.hvalue = test_ptr->val.hvalue;
         elem_ptr->next = NULL;
         count++;
        }
      else if (test_ptr->val.fun_ptr == PTR_GET_VAR)
        {
         pat = test_ptr->arg_list->val.index;
         elem = test_ptr->arg_list->next_arg->val.index;
         elem_a = fast_gv(pat,elem,&extent,&temp_ptr,&start,&multi);
         count += extent;
         if (extent == 0)
           { elem_ptr = NULL; }
         else 
           {
            elem_ptr = get_struct(element);
            elem_ptr->type = elem_a->type;
            elem_ptr->val = elem_a->val;
            elem_ptr->next = NULL;
            last_elem = elem_ptr;
            for (i = 2 ; i <= extent ; i++)
              {
               elem_a++;
               new_elem = get_struct(element);
               last_elem->next = new_elem;
               new_elem->type = elem_a->type;
               new_elem->val = elem_a->val;
               new_elem->next = NULL;
               last_elem = new_elem;
              }
           }
        }
      else
        {            
         generic_compute(test_ptr,&arg_ptr);
         if (arg_ptr.type == NUMBER)
           {
            elem_ptr = get_struct(element);
            elem_ptr->type = arg_ptr.type;
            count++;
            elem_ptr->type = NUMBER;
            elem_ptr->val.fvalue = arg_ptr.val.fvalue;
            elem_ptr->next = NULL;
           }
         else if (get_valtype(arg_ptr) == MULTIPLE)
           {
            extent = (arg_ptr.end - arg_ptr.begin) + 1;
            count += extent;
             
            elem_ptr = last_elem = NULL;
            elem_a = arg_ptr.origin->atoms;
            i = arg_ptr.begin;
            while (i <= arg_ptr.end)
              {
               new_elem = get_struct(element);
               new_elem->type = elem_a[i].type;
               new_elem->val = elem_a[i].val;
               new_elem->next = NULL;
               if (last_elem != NULL)
                 { last_elem->next = new_elem; }
               else
                 { elem_ptr = new_elem; }
               last_elem = new_elem;
               i++;
              }
             
           }      
         else
           {
            elem_ptr = get_struct(element);
            elem_ptr->type = arg_ptr.type;
            count++;
            elem_ptr->type = arg_ptr.type;
            elem_ptr->val.hvalue = arg_ptr.val.hvalue;
            elem_ptr->next = NULL;
           }
        }

      if (last_add == NULL)
        { head = elem_ptr; }
      else
        { last_add->next = elem_ptr; }

      if (elem_ptr != NULL)
        {
         while (elem_ptr->next != NULL)
           { elem_ptr = elem_ptr->next; }
         last_add = elem_ptr;
        }

      test_ptr = get_next_arg(test_ptr);
     }
     
   if (count == 0) return(0);
   
   fact_ptr = get_el(count); 
   elem_ptr = fact_ptr->atoms;
   i = 0;
   while (head != NULL)
     {
      elem_ptr[i].type = head->type;
      elem_ptr[i].val = head->val;
      last_add = head->next;
      rtn_struct(element,head);
      head = last_add;
      i++;
     }
   
   add_fact(fact_ptr);

   return(1);
  }

/************************************************************/
/* RETRACT:                                             */
/************************************************************/
int retract()
  {
   FACT_ID fact_num;
   int found_fact, pattern, count;
   struct fact *ptr, *fact_ptr;
   struct test *test_ptr;
   struct fbind *lhs_binds, *rhs_binds;

   test_ptr = get_first_arg();

   while (test_ptr != NULL)
     {
      if ((test_ptr->type == NUMBER) || (test_ptr->type == INDEX))
        {
         if (test_ptr->type == NUMBER)
           { fact_num = (FACT_ID) test_ptr->val.fvalue; }
         else
           { fact_num = (FACT_ID) test_ptr->val.index; }
         found_fact = FALSE;
         ptr = factlist;
         while (ptr != NULL)
           {
            if (ptr->ID == fact_num)
              { 
               retract_fact(ptr);
               ptr = NULL;
               found_fact = TRUE;
              }
            else
              { ptr = ptr->next; }
           }
         if (found_fact == FALSE)
           {
            cl_print("werror","Fact ");
            print_num("werror",(float) fact_num);
            cl_print("werror"," does not exist\n");
           }
        }
      else 
        {

         lhs_binds = gbl_lhs_binds;
         rhs_binds = gbl_rhs_binds;
         count = 1;

         pattern = test_ptr->arg_list->val.index;

         while ((lhs_binds != NULL) && (count < pattern))
           {
            lhs_binds = lhs_binds->next;
            count++;
           }

         if (lhs_binds != NULL)
           { fact_ptr = lhs_binds->origin; }
         else
           {
            while (count < pattern)
              {
               rhs_binds = rhs_binds->next;
               count++;
              }
            fact_ptr = rhs_binds->origin;
           }

         retract_fact(fact_ptr);
        }
      test_ptr = get_next_arg(test_ptr);
     }
   
   return(1);
  }
  
  /* Get_el will depend on largest memory size available. If mem sizes 
   range up to 200 this will allow facts of approximately 12 to 16 
   elements in length without going to system memory. */

/*****************************************************************/
/* GET_EL:  Allocates a fact structure / element array structure */
/*   combination for the storage of a fact.                      */
/*****************************************************************/
struct fact *get_el(size)
  int size;
  {
   struct fact *fact_ptr;
   struct element *atoms;
   
   if (size > 0)
     {
      atoms = (struct element *) gm2(sizeof(struct element) * size);
      if (atoms == NULL) return(NULL);
     }
   else
     { atoms = NULL; }

   fact_ptr = get_struct(fact);
   fact_ptr->fact_length = size;
   fact_ptr->ID = 0;
   fact_ptr->atoms = atoms;
   fact_ptr->next = NULL;
   fact_ptr->previous = NULL;
   fact_ptr->list = NULL;
    
   return(fact_ptr);
  }
  
/*****************************************************************/
/* RTN_EL:                                                       */
/*****************************************************************/
rtn_el(fact_ptr)
  struct fact *fact_ptr;
  {
   if (fact_ptr->fact_length > 0)
     { rm(fact_ptr->atoms,sizeof(struct element) * fact_ptr->fact_length); }
   rtn_struct(fact,fact_ptr);
  }
  
/***************************************************************/
/* FACT_INSTALL:  Increments all occurrences in the hash table */
/*   of symbols found in the atoms of a fact.                  */ 
/***************************************************************/
fact_install(new_fact)
  struct fact *new_fact;
  {
   int length, i;
   struct element *elem_ptr;

   length = new_fact->fact_length;

   elem_ptr = new_fact->atoms;
   for (i = 0 ; i < length ; i++)
     {
      if ((elem_ptr[i].type == WORD) || (elem_ptr[i].type == STRING))
        { inc_symbol_count(elem_ptr[i].val.hvalue); }
     }            
  }

/*****************************************************************/
/* FACT_DEINSTALL:  Decrements all occurrences in the hash table */
/*   of symbols found in the atoms of a fact.                    */ 
/*****************************************************************/
fact_deinstall(new_fact)
  struct fact *new_fact;
  {
   int length, i;
   struct element *elem_ptr;

   length = new_fact->fact_length;

   elem_ptr = new_fact->atoms;
   for (i = 0 ; i < length ; i++)
     {
      if ((elem_ptr[i].type == WORD) || (elem_ptr[i].type == STRING))
        { dec_symbol_count(elem_ptr[i].val.hvalue); }
     }            
  }

/*****************************************************************/
/* SET_FACT_ID:                    */ 
/*****************************************************************/
set_fact_id(value)
  FACT_ID value;
  {
   ID = value;
  }
  
/*****************************************************************/
/* GET_NEXT_FACT:                    */ 
/*****************************************************************/
struct fact *get_next_fact(fact_ptr)
  struct fact *fact_ptr;
  {
   if (fact_ptr == NULL)
     { return(factlist); }
   
   return(fact_ptr->next);
  }
  
/********************************************************/
/* ASSERT:                                              */
/********************************************************/
struct fact *assert(str)
  char *str;
  {
   struct fact *t_assert();
   struct fact *fact_ptr;

   fact_ptr = t_assert(str);
   
   if (fact_ptr == NULL) return(0);
   
   return(add_fact(fact_ptr));
  }

/*******************************************************/
/* T_ASSERT:  Returns a fact structure that represents */
/*   the string sent as the argument.                  */
/*   Note:  Duplicates the functionality of assert ex- */
/*          cept it does not assert the fact.          */
/*******************************************************/
struct fact *t_assert(str)
  char *str;
  {
   extern struct fact *get_el();
   struct token inp_tkn;
   struct element *head, *next_elem, *last_elem;
   struct fact *fact_ptr;
   int num_elems = 0;

   last_elem = NULL;
 
   open_str_source("assert_str",str,0);
 
   gettoken("assert_str",&inp_tkn);
   while (inp_tkn.token != STOP)
     {   
      num_elems++;
      next_elem = get_struct(element);
      next_elem->next = NULL;
      if (inp_tkn.token == NUMBER)
        {
         next_elem->val.fvalue = inp_tkn.tknnumber;
         next_elem->type = NUMBER;
        }
      else if ((inp_tkn.token == WORD) || (inp_tkn.token == STRING))
        {
         next_elem->type = inp_tkn.token;
         next_elem->val.hvalue = inp_tkn.hashword;
         next_elem->type = inp_tkn.token;
        }
      else
        {
         next_elem->type = STRING;
         next_elem->val.hvalue = add_symbol(inp_tkn.print_rep);
        }

      if (last_elem == NULL)
        { head = next_elem; }
      else
        { last_elem->next = next_elem; }
      last_elem = next_elem;

      gettoken("assert_str",&inp_tkn);
     }   

   close_str_source("assert_str");
   if (num_elems == 0)
     { return(NULL); }
 
   fact_ptr = get_el(num_elems);

   next_elem = head;
   last_elem = fact_ptr->atoms;
   num_elems = 0;

   while (next_elem != NULL)
     {   
      last_elem[num_elems].type = next_elem->type;
      last_elem[num_elems].val = next_elem->val;
      num_elems++;
      next_elem = next_elem->next;
     }  
        
   returnelements(head);
 
   return(fact_ptr);
  }
  
/***************************************************************/
/* RETURNELEMENTS:  Returns a multiply linked list of elements */
/*   structures to the list of free elements.                  */
/***************************************************************/
returnelements(freed)
  struct element *freed;
  {
   struct element *temp;

   while (freed != NULL)
     {
      temp = freed->next;
      rtn_struct(element,freed);
      freed = temp;
     }
  }  
  
/***************************************************/
/* GET_CHANGE_FACTS:                              */
/***************************************************/
int get_change_facts()
  { return(change_facts); }
  
/***************************************************/
/* SET_CHANGE_FACTS:                              */
/***************************************************/
int set_change_facts(value)
  int value;
  { 
   change_facts = value;
  }
  
/***************************************************/
/* GetGarbageFacts:                              */
/***************************************************/
struct fact *GetGarbageFacts()
  { return(garbage_facts); }

