/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    HASH MODULE                      */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "symbol.h"
#include "clipsmem.h"
#include "constant.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
   
   struct draw               *rem_symbol();
   int                        add_eph_symbol();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   struct draw               *add_symbol();
   int                        init_symbol_table();
   int                        hash_symbol();
   int                        inc_symbol_count();
   int                        dec_symbol_count();
   int                        rem_eph_symbols();
   char                      *symbol_string();
    
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct draw       **symbol_table; 
   static struct eph_draw    *eph_symbol_list = NULL;

/*####################################################################*/
/*####################################################################*/
/*###                                                              ###*/
/*###                      HASHING FUNCTIONS                       ###*/
/*###                                                              ###*/
/*####################################################################*/
/*####################################################################*/
	  
/*******************************************************************/
/* ADD_SYMBOL:  Searches for the string in the hash table. If the  */ 
/*   string is already in the hash table, then the address of the  */
/*   string is returned.  Otherwise, the string is hashed into the */
/*   table and the address of the string is also returned.         */
/*******************************************************************/
struct draw *add_symbol(str)
   char *str;
   {
    int tally, length;
    struct draw *past, *peek;
    
    /*====================================*/
    /* Get the hash value for the string. */
    /*====================================*/

    if (str == NULL)
      {
       clips_system_error(801);
       cl_exit(5);
      }

    tally = hash_symbol(str,HASHSIZE);
    peek = symbol_table[tally];

    /*==================================================*/
    /* If the hash table entry for the string is empty, */
    /* then place the string at that location, and      */
    /* return the address of the string.                */
    /*==================================================*/

    if (peek == NULL)
      {
       peek = get_struct(draw); 
       symbol_table[tally] = peek;
       length = strlen(str) + 1;
       peek->contents = (char *) gm2(length);
       if (peek->contents == NULL)
         {
          cl_print("werror","Unable to allocate memory in add_symbol\n");
          cl_exit(1);
         }
       peek->next = NULL;
       peek->bucket = tally;
       peek->count = 0;
       strcpy(peek->contents,str);
       add_eph_symbol(peek);
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

    past->next = get_struct(draw);
    peek = past->next;
    length = strlen(str) + 1;
    peek->contents = (char *) gm2(length);
    if (peek->contents == NULL)
      {
       cl_print("werror","Unable to allocate memory in add_symbol\n");
       cl_exit(1);
      }
    peek->next = NULL;
    peek->bucket = tally;
    peek->count = 0;
    strcpy(peek->contents,str);
    
    add_eph_symbol(peek);
    return(peek);
   }

/***********************************************************************/
/* INIT_SYMBOL_TABLE: Purpose is to initialize the hash table to NULL. */
/***********************************************************************/
init_symbol_table()
   {
    int i;

    symbol_table = (struct draw **)
                gm2(sizeof (struct draw *) * HASHSIZE);
    if (symbol_table == NULL)
      {
       cl_print("werror","Unable to allocate memory for hash table\n");
       cl_exit(1);
      }
    for (i=0; i<HASHSIZE; i++)
        symbol_table[i] = NULL;
   }

/************************************************************************/
/* HASH_SYMBOL:  Computes the hash table location for the given string. */
/************************************************************************/
int hash_symbol(word,range)
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

/***************************************************************/
/* INC_SYMBOL_COUNT:  Increments the counter for the number of */
/*   occurences of a string store in a particular location in  */
/*   the hash table.                                           */
/***************************************************************/
inc_symbol_count(hash_loc)
  struct draw *hash_loc;
  {
   if (hash_loc->count == -1)
     {
      clips_system_error(802);
      cl_exit(5);
     }

   hash_loc->count++;
  }

/******************************************************************/
/* dec_symbol_count:  Decrements the counter for the number of    */
/*   occurences of a string store in a particular location in the */
/*   hash table. If the counter is set to zero, then the hash     */
/*   structure and its associated string is removed from the hash */
/*   table.                                                       */ 
/******************************************************************/
dec_symbol_count(hash_loc)
  struct draw *hash_loc;
  {
   if (hash_loc->count == -1)
     {
      clips_system_error(803);
      cl_exit(5);
     }

   if (hash_loc->count == 0)
     {
      clips_system_error(804);
      cl_exit(5);
     }

   hash_loc->count--;

   if (hash_loc->count != 0) return(hash_loc->count);

   add_eph_symbol(hash_loc);

   return(0);
  }

/************************************************************/
/* REM_SYMBOL:  Removes a hash structure and its associated */
/*   string from the hash table.  The string is return to   */
/*   the pool of free memory.                               */ 
/************************************************************/
static struct draw *rem_symbol(hash_loc)
  struct draw *hash_loc;
  {
   struct draw *prev_hash, *check_hash;

   prev_hash = NULL;
   check_hash = symbol_table[hash_loc->bucket];

   while (check_hash != hash_loc)
     {
      prev_hash = check_hash;
      check_hash = check_hash->next;

      if (check_hash == NULL)
        {
         clips_system_error(805);
         cl_exit(5);
        }
     }
     
   if (prev_hash == NULL)
     { symbol_table[hash_loc->bucket] = hash_loc->next; }
   else
     { prev_hash->next = check_hash->next; }

   hash_loc->bucket = -1;
   
   return(hash_loc);
  }
  
/******************************************************************/
/* ADD_EPH_SYMBOL:  Adds a hash location to the list of ephemeral */
/*   symbols.  These locations have a zero count indicating that  */
/*   no structure is using the symbol.                            */
/******************************************************************/
static int add_eph_symbol(hash_loc)
  struct draw *hash_loc;  
  {
   struct eph_draw *temp;
     
   if (hash_loc->count != 0)
     {
      clips_system_error(806);
      cl_exit(5);
     }

   temp = get_struct(eph_draw);
   temp->watch_draw = hash_loc;
   temp->next = eph_symbol_list;
   eph_symbol_list = temp;

   return(0);
  }

/*************************************************************/
/* REM_EPH_SYMBOLS:  Searches the list of ephemeral symbols  */
/*   and removes any of the symbols that have a count of     */
/*   zero. These symbols may have been temporarily allocated */
/*   for rule parsing, expression evaluation, etc.           */
/*************************************************************/
rem_eph_symbols() 
  {
   struct eph_draw *temp;
   struct draw *deletion_list = NULL, *next_draw;

   /*=====================================================*/
   /* Determine the symbols to be deleted. Symbols with a */
   /* count value of 0 need to be removed. Symbols  with  */
   /* a bucket value of -1 have already been placed in    */
   /* the list of symbols to be deleted.                  */
   /*=====================================================*/
   
   while (eph_symbol_list != NULL)
     {
      if ((eph_symbol_list->watch_draw->count == 0) &&
          (eph_symbol_list->watch_draw->bucket != -1))
        { 
         next_draw = rem_symbol(eph_symbol_list->watch_draw);
         next_draw->next = deletion_list;
         deletion_list = next_draw; 
        }
      temp = eph_symbol_list->next;
      rtn_struct(eph_draw,eph_symbol_list);
      eph_symbol_list = temp;
     }
     
   /*=====================*/
   /* Delete the symbols. */
   /*=====================*/
 
   while (deletion_list != NULL)
     {
      next_draw = deletion_list->next;
      rm(deletion_list->contents,strlen(deletion_list->contents) + 1);
      rtn_struct(draw,deletion_list);
      deletion_list = next_draw;
     }
  }
  
/*********************************************/
/* SYMBOL_STRING:                            */
/*********************************************/
char *symbol_string(d_ptr)
  struct draw *d_ptr;
  {
   return(d_ptr->contents);
  }
  
/********************/
/* GET_SYMBOL_TABLE */
/********************/
struct draw **get_symbol_table()
  {
   return(symbol_table);
  }

/********************/
/* SET_SYMBOL_TABLE */
/********************/
set_symbol_table(value)
  struct draw **value;
  {
   symbol_table = value;
  }



