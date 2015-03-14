#include <stdio.h>
#include "crsv.h"
#if MAC_LSC
#include <strings.h>
#else
#include <string.h>
#endif
/******************************************************
 *  This file contains the functions used to store
 *  strings in CRSV, a basic hash table.
 ******************************************************
 */


/* ===========  Functions defined Externally but used here  =========== */

/*=============================
 * in CRSV.C
 *=============================
 */
 
extern void end_crsv();

/*=============================
 * in CRSVPRNT.C
 *=============================
 */
 
extern void send_message();

/*=============================
 * in CRSVMEM.C
 *=============================
 */
 
extern char      *gen_alloc();
extern void       gen_free();
extern char      *gm2();
extern int        rm();


/* ===========  Functions defined here for Global use  ================ */

HASH     *add_symbol();
void      init_symbol_table();
/*^char     *symbol_string();*/


/* ===========  Functions defined here for internal use  ============== */

struct mem_ptr
  {
   struct mem_ptr *next;
  };

struct mem_ptr *temp_mem_ptr;

#define get_struct(type) \
  ((memory_table[sizeof(struct type)] == NULL) ? \
   ((struct type *) gen_alloc(sizeof(struct type),"add_symbol")) :\
   ((temp_mem_ptr = memory_table[sizeof(struct type)]),\
    memory_table[sizeof(struct type)] = temp_mem_ptr->next,\
    ((struct type *) temp_mem_ptr)))
    
#define rtn_struct(type,struct_ptr) \
  (temp_mem_ptr = (struct mem_ptr *) struct_ptr,\
   temp_mem_ptr->next = memory_table[sizeof(struct type)], \
   memory_table[sizeof(struct type)] = temp_mem_ptr)
   
static int       hash_symbol();
static void      clear_symbol_table();

/* ===========  Variables defined here for Global use  ================ */

/* ===========  Variables defined Externally but used here  =========== */


/*=============================
 * in CRSVMEM.C
 *=============================
 */
 
extern struct mem_ptr *memory_table[];


/* ===========  Variables defined here for internal use  ============== */

static HASH       **symbol_table; 


/* ======================================================================== */

   
/*####################################################################*/
/*####################################################################*/
/*###                                                              ###*/
/*###                      HASHING FUNCTIONS                       ###*/
/*###                                                              ###*/
/*####################################################################*/
/*####################################################################*/
	  
/************************************************************************/
/* ADD_SYMBOL:  Searches for the string in the hash table. If the       */ 
/*   string is already in the hash table, then the address of the       */
/*   string hash is returned.  Otherwise, the string is hashed into the */
/*   table and the address of the string hash is also returned.         */
/************************************************************************/
HASH *add_symbol(str)
   char *str;
   {
    int tally;
    unsigned int length;
    HASH *past, *peek;
    
    /*====================================*/
    /* Get the hash value for the string. */
    /*====================================*/

    if (str == NULL)
      {
       send_message("\n**ERROR**  Null string passed to add_symbol",NO);
       end_crsv();
      }

    tally = hash_symbol(str,HASHSIZE);
    peek  = symbol_table[tally];

    /*==================================================*/
    /* If the hash table entry for the string is empty, */
    /* then place the string at that location, and      */
    /* return the address of the string.                */
    /*==================================================*/

    if (peek == NULL)
      {
       peek = get_struct(hash_entry); 
       symbol_table[tally] = peek;
       length = strlen(str) + 1;
       peek->contents = (char *) gm2(length);
       if (peek->contents == NULL)
         {
          send_message
             ("\n**ERROR** Unable to allocate memory in add_symbol\n",NO);
          end_crsv();
         }
       peek->next = NULL;
       peek->bucket = tally;
       (void)strcpy(peek->contents,str);
       return(peek);
      }

    /*==================================================*/
    /* Search for the string in the list of entries for */ 
    /* this hash location.  If the string is found,     */
    /* then return the address of the string.           */
    /*==================================================*/

    while (peek != NULL)
      {
       if (strcmp(str,peek->contents) == 0)
	     { return(peek); }
       past = peek;
       peek = peek->next;
      }

    /*==================================================*/
    /* Add the string at the end of the list of entries */
    /* for this hash location.  Return the address of   */
    /* the string.                                      */
    /*==================================================*/

    past->next = get_struct(hash_entry);
    peek = past->next;
    length = strlen(str) + 1;
    peek->contents = (char *) gm2(length);
    if (peek->contents == NULL)
      {
       send_message("\n**ERROR** Unable to allocate memory in add_symbol\n",NO);
       end_crsv();
      }
    peek->next = NULL;
    peek->bucket = tally;
    (void)strcpy(peek->contents,str);
    
    return(peek);
   }

/***********************************************************************/
/* INIT_SYMBOL_TABLE: Purpose is to initialize the hash table to NULL. */
/***********************************************************************/

void init_symbol_table()
{
    int i;

    IF(symbol_table NEQ NULL) THEN
       clear_symbol_table();

    ELSE
       symbol_table = (HASH **) gm2(sizeof (HASH *) * HASHSIZE);
       IF(symbol_table EQ NULL) THEN
          send_message
             ("\n**ERROR** Unable to allocate memory in hash table\n",NO);
          end_crsv();
       END_IF
    END_IF

    for (i=0; i< HASHSIZE; i++)
        symbol_table[i] = NULL;
}

/*********************************************************************/
/* CLEAR_SYMBOL_TABLE: Purpose is free memory tied up in hash table. */
/*********************************************************************/
static void clear_symbol_table()
{
   int   i;
   HASH *temp1, *temp2;

   for (i = 0; i < HASHSIZE; i++) DO
      temp1 = symbol_table[i];
      
      WHILE(temp1 NEQ NULL) DO
         temp2 = temp1->next;
         (void)rm(temp1->contents,(unsigned)(strlen(temp1->contents) + 1));
         rtn_struct(hash_entry,temp1);
         temp1 = temp2;
      END_WHILE
   END_FOR

   return;
}

/************************************************************************/
/* HASH_SYMBOL:  Computes the hash table location for the given string. */
/************************************************************************/
static int hash_symbol(word,range)
  char *word;
  int range;
  {
   int tally = 0, posn = 0;

   while (*word != EOS)
     {
      tally = (tally + (*word) * (posn % 16)) % 8192;
      word++; 
      posn++;           
     }

   return(tally % range);
  }

/*********************************************/
/* SYMBOL_STRING:                            */
/*********************************************/
/*^char *symbol_string(d_ptr)
  HASH *d_ptr;
  {
   return(d_ptr->contents);
  }*/



