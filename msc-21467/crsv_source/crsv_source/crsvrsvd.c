#include <stdio.h>
#include "crsv.h"

/*********************************************************************
 * This file contains the functions used to compare RHS actions to a
 * list of CLIPS defined actions (the reserved words). If a token is
 * not found in that list, it can be assumed to be an external function
 * call. The list of reserved words will have to be updated when new
 * actions are added to CLIPS. Only entry point is through the function
 * check_ex_funcs().
 *********************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern void  memory_error();
extern char *gen_alloc();


/* ===========  Functions defined here for Global use  ================ */

int  check_ex_funcs();


/* ===========  Functions defined here for internal use  ============== */

void            lower();
void            store_token();
struct letter  *new_letter();
int             load_reserved_words();
void            reset_word_list();
short int       next_word_letter();


/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined Externally but used here  =========== */


/* ===========  Variables defined here for internal use  ============== */

/* ------------------------- Macro Definitions --------------------------- */

#define TOKEN_SIZE         40
#define DUPLICATE_TOKEN    -2
#define NOT_TOKEN          -1
#define NULL_TOKEN          0

#define CASE_OFFSET ('a' - 'A')


/* ----------------------- Structure Definitions ------------------------- */

struct letter
  {
   char value;
   struct letter *prev;
   struct letter *step;
   struct letter *next;
   short int key;
  };

struct rsvd_word {
   char *word;
   short int key;
   };


/* -------------------- Internal Global Variables ------------------------- */

/* -----------------------
 *  CLIPS Actions. This
 *  list is valid for
 *  CLIPS Release 4.2
 * -----------------------
 */

struct rsvd_word rsvd_list[] = {
     "AND",           53,
     "ASSERT",         3,
     "BATCH",         52,
     "BIND",           6,
     "CALL",          44,
     "CLEAR",          1,
     "CLOSE",          7,
     "CONSERVE-MEM",   8,
     "DRIBBLE-ON",     9,
     "DRIBBLE-OFF",   10,
     "EQ",            55,
     "EVENP",         80,
     "EXCISE",        11,
     "EXIT",          78,
     "FACTS",          5,
     "FETCH",         73,
     "FORMAT",        12,
     "FPRINTOUT",     13,
     "GENSYM",        14,
     "HALT",          15,
     "HELP",          71,
     "HELP-PATH",     72,
     "IF",            16,
     "LENGTH",        17,
     "LIST-DEFFACTS", 18,
     "LOAD",          19,
     "LOAD-FACTS",    41,
     "MATCHES",       20,
     "MEM-USED",      21,
     "MV-APPEND",     46,
     "MV-DELETE",     45,
     "MV-SUBSEQ",     49,
     "NEQ",           66,
     "NTH",           22,
     "NOT",           56,
     "NUMBERP",       77,
     "ODDP",          79,
     "OPEN",          23,
     "OR",            54,
     "PPDEFFACT",     24,
     "PPRULE",        25,
     "PRINTOUT",      26,
     "PRINT_REGION",  74,
     "READ",          27,
     "READLINE",      28,
     "RELEASE-MEM",   29,
     "RESET",         30,
     "RETRACT",       31,
     "RULES",          4,
     "RUN",           32,
     "SAVE",          42,
     "SAVE-FACTS",    43,
     "SETGEN",        33,
     "STR_CAT",       34,
     "STR_ASSERT",    40,
     "STR-EXPLODE",   47,
     "STR-IMPLODE",   48,
     "STR-INDEX",     51,
     "STRING_ASSERT", 35,
     "STRINGP",       76,
     "SUBSET",         2,
     "SUB-STRING",    50,
     "SYSTEM",        36,
     "TOSS",          75,
     "UNWATCH",       37,
     "WATCH",         38,
     "WHILE",         39,
     "WORDP",         81,
     "!",             57,
     "&&",            58,
     "||",            59,
     ">",             60,
     "<",             61,
     ">=",            62,
     "<=",            63,
     "=",             64,
     "!=",            65,
     "/",             67,
     "*",             68,
     "+",             69,
     "-",             70,
   };

#define NUM_RSV_WORDS     81

static struct letter *token_tree     = NULL;
static struct letter *current_letter = NULL;


/* ======================================================================== */

/* --------------------------------------------------
 *  NAME   :  check_ex_funcs()
 *
 *  PURPOSE:  This function takes a word
 *            and compares it to the list
 *            of reserved words. If the
 *            word is found, it returns
 *            TRUE, otherwise FALSE.
 *
 *  INPUTS:   A pointer to the word to be checked.
 *
 *  RETURNS:  An integer, either TRUE or FALSE.
 *
 *  NOTES:    The token comparison functions were
 *            provided by Brian Donnell. He used
 *            them for the CLIPS tutorial parser.
 * --------------------------------------------------
 */

int check_ex_funcs(word)
char *word;
{
   int index;
   short int key;

   IF(token_tree EQ NULL) THEN
      (void)load_reserved_words();
   ELSE
      reset_word_list();
   END_IF

   for(index = 0; word[index] NEQ EOS; index++) DO
      key = next_word_letter(word[index]);
   END_FOR

   IF(key > 0) THEN
      return(YES);
   ELSE
      return(NO);
   END_IF
}

/* ======================================================================== */

static short int next_word_letter(ch)
  char ch;
  {
   struct letter *lptr;

   lptr = current_letter;
   if (current_letter == NULL)
     return(NOT_TOKEN);
   else if (ch == EOS)
      return(NULL_TOKEN);
   if (lptr->step != NULL)
     lptr = lptr->step;
   else
     return(NOT_TOKEN);
   for ( ; lptr != NULL ; lptr = lptr->next)
     if (lptr->value == ch)
       break;
   if (lptr == NULL)
     return(NOT_TOKEN);
   current_letter = lptr;
   return(current_letter->key);
  }

/* ------------------------------------------------------------------------ */

static void reset_word_list()
  {
   current_letter = token_tree;
  }

/* ------------------------------------------------------------------------ */

static int load_reserved_words()
   {
   int i;

   token_tree = new_letter();
   token_tree->key = NULL_TOKEN;
   for(i = 0; i < NUM_RSV_WORDS; i++)
     {
      lower(rsvd_list[i].word);
      store_token(rsvd_list[i].word,rsvd_list[i].key);
     }
   current_letter = token_tree;
   return(TRUE);
   }

/* ------------------------------------------------------------------------ */

static void lower(str)
  char *str;
  {
   int index;

   for (index = 0 ; str[index] != EOS ; index++)
    if ((str[index] >= 'A') && (str[index] <= 'Z'))
      str[index] += CASE_OFFSET;
  }

/* ------------------------------------------------------------------------ */

static void store_token(token,key)
  char *token;
  short int key;
  {
   int index;
   struct letter *lptr,*prev,*lnode;

   lptr = token_tree;
   for (index = 0 ; token[index] != EOS ; index++)
     {
      if (lptr->step == NULL)
        {
         lptr->step = new_letter();
         lptr->step->value = token[index];
         lptr->step->prev = lptr;
         lptr = lptr->step;
        }
      else
        {
         prev = lptr;
         for (lptr = lptr->step ; lptr != NULL ; lptr = lptr->next)
           {
            lnode = lptr;
            if (lptr->value == token[index])
              break;
           }
         if (lptr == NULL)
           {
            lnode->next = new_letter();
            lnode->next->value = token[index];
            lnode->next->prev = prev;
            lptr = lnode->next;
           }
       }
     }
   if (lptr->key == NOT_TOKEN)
     lptr->key = key;
   else
     lptr->key = DUPLICATE_TOKEN;
  }

/* ------------------------------------------------------------------------ */

static struct letter *new_letter()
  {
   struct letter *lptr;

   if ((lptr = (struct letter *) gen_alloc(sizeof(struct letter),
      "new_letter")) == NULL)
      memory_error();
   lptr->value = EOS;
   lptr->prev = NULL;
   lptr->step = NULL;
   lptr->next = NULL;
   lptr->key = NOT_TOKEN;
   return(lptr);
  }
