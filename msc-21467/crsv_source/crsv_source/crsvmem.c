#include <stdio.h>
#include "crsv.h"

/******************************************************
 *  This file contains the functions used to allocate
 *  new memory blocks for each of the data structures
 *  used in CRSV
 ******************************************************
 */


/* ===========  Functions defined Externally but used here  =========== */

extern char *malloc();
extern int   free();

/*=============================
 * in CRSVPRNT.C
 *=============================
 */

extern void send_message();
extern void error_message();

/*=============================
 * in CRSV.C
 *=============================
 */

extern void end_crsv();


/* ===========  Functions defined here for Global use  ================ */

FB_PTR     alloc_fb();
FBR_PTR    alloc_fbr();
REL_PTR    alloc_rel();
FLD_PTR    alloc_fld();
EXF_PTR    alloc_ex_func();
WL_PTR     alloc_wl();
NL_PTR     alloc_nl();
RULE_PTR   alloc_rule();
RR_PTR     alloc_rr();
VAR_PTR    alloc_var();
DR_PTR     alloc_def_rel();
DF_PTR     alloc_def_fld();
DT_PTR     alloc_def_tmp();
TN_PTR     alloc_token_node();
DS_PTR     alloc_def_slot();
TK_PTR     alloc_token();
COUNTER   *alloc_counter();
ACT_R_PTR      alloc_a_rule();
ACT_REL_PTR    alloc_a_rel();
COMP_R_PTR     alloc_c_rule();
COMP_P_PTR     alloc_cp();
FIELD_PTR      alloc_field();
REL_LIST_PTR   alloc_rl();
SPE_FACT_PTR   alloc_fact_list();
RULE_NAME_PTR  alloc_rule_list();
MULT_REL_PTR   alloc_mult_rel();

void   free_fb();
void   free_fbr();
void   free_rel();
void   free_fld();
void   free_ex_func();
void   free_wl();
void   free_nl();
void   free_rule();
void   free_rr();
void   free_var();
void   free_def_rel();
void   free_def_ex_func();
void   free_def_fld();
void   free_def_slots();
void   free_token_node();
void   free_token();
WL_PTR free_word_and_get_next();
NL_PTR free_number_and_get_next();
/*^void   free_mult_rel();*/
void   free_rl();
void   free_a_rule();
void   free_cp();
void   free_c_rule();
void   free_rule_list();
void   free_field();
void   free_a_rel();
void   free_fact_list();


void       memory_error();
char      *gen_alloc();
void       gen_free();
/*^char      *disp_alloc();*/
/*^void       disp_free();*/
char      *gm2();
int        rm();
long int   mem_used();
long int   max_mem_used();
long int   mem_requests();

int        release_mem();


/* ===========  Functions defined here for internal use  ============== */

#if MAC_LSC
static int   return_block();
static int   allocate_block();
static char *request_block();
static int   init_new_buffer();
static int   init_mem();
#endif


/* ===========  Variables defined here for Global use  ================ */

/* ===========  Variables defined Externally but used here  =========== */

/* ===========  Variables defined here for internal use  ============== */

#if MAC_LSC
#define INITBUFFERSIZE   8192
#define BUFFERSIZE       8192
#endif

#define MEM_TABLE_SIZE    300

struct mem_ptr
  {
   struct mem_ptr *next;
  };

static long int         mem_amount = 0;
static long int         mem_calls  = 0;
static long int         max_mem    = 0;

struct mem_ptr         *memory_table[MEM_TABLE_SIZE];

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   gen_alloc()
 *
 *  PURPOSE:   This function is a general
 *             purpose memory allocation
 *             function. All CRSV memory
 *             allocations are through
 *             this function.
 *
 *  INPUTS:    The number of bytes needed.
 *
 *  OUTPUT:    A pointer to the new memory
 *             chunk.
 * ----------------------------------------
 */

char *gen_alloc(size, called_from)
unsigned int size;
char *called_from;
{
   char *mem_ptr;

#if   MAC_LSC
   extern char *request_block();

   mem_ptr = request_block(size);
   if (mem_ptr == NULL)
     {
      release_mem(((size * 5 > 1024) ? size * 5 : 1024));
      mem_ptr = request_block(size);
      if (mem_ptr == NULL)
        {
         release_mem(-1);
         mem_ptr = request_block(size);
         if (mem_ptr == NULL)
           {
            send_message("\nERROR: out of memory",NO);
            end_crsv();
           }
        }
     }
#else
   mem_ptr = malloc(size);
   if (mem_ptr == NULL)
     {
      release_mem((int)(size * 5 > 1024 ? size * 5 : 1024));
      mem_ptr = malloc(size);
      if (mem_ptr == NULL)
        {
         release_mem(-1);
         mem_ptr = malloc(size);
         if (mem_ptr == NULL)
           {
            return(NULL);
           }
        }
      }
#endif

   IF(CHECK_DEBUG IS_ON) THEN
      sprintf(msg_buf,"\nMEM %ld ALLOC by: %s (%d)", mem_ptr, called_from,
         size);
      send_message(msg_buf,NO);
   END_IF

   mem_amount += size;
   mem_calls++;
   IF(mem_amount > max_mem) THEN
      max_mem = mem_amount;
   END_IF

   return(mem_ptr);
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   disp_alloc()
 *
 *  PURPOSE:   This function is a slight mod
 *             to the general
 *             purpose memory allocation
 *             function. It is only for
 *             use by Mac display functions
 *             which need memory.
 *
 *  INPUTS:    The number of bytes needed.
 *
 *  OUTPUT:    A pointer to the new memory
 *             chunk.
 * ----------------------------------------
 */

/*^char *disp_alloc(size)
unsigned int size;
{
   char *mem_ptr;

#if   MAC_LSC
   extern char *request_block();

   mem_ptr = request_block(size);
   if (mem_ptr == NULL)
     {
      release_mem(((size * 5 > 1024) ? size * 5 : 1024));
      mem_ptr = request_block(size);
      if (mem_ptr == NULL)
        {
         release_mem(-1);
         mem_ptr = request_block(size);
         if (mem_ptr == NULL)
           {
            end_crsv();
           }
        }
     }
#else
   mem_ptr = malloc(size);
   if (mem_ptr == NULL)
     {
      release_mem((int)(size * 5 > 1024 ? size * 5 : 1024));
      mem_ptr = malloc(size);
      if (mem_ptr == NULL)
        {
         release_mem(-1);
         mem_ptr = malloc(size);
         if (mem_ptr == NULL)
           {
            return(NULL);
           }
        }
      }
#endif

   mem_amount += size;
   mem_calls++;
   IF(mem_amount > max_mem) THEN
      max_mem = mem_amount;
   END_IF

   return(mem_ptr);
}*/

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   gen_free()
 *
 *  PURPOSE:   This function is a general
 *             purpose memory release
 *             function. All CRSV memory
 *             releases are through
 *             this function.
 *
 *  INPUTS:    A pointer to the memory to
 *             be released.
 *
 *  OUTPUT:    Nothing.
 * ----------------------------------------
 */

void gen_free(waste, size)
char *waste;
unsigned int size;
{
#if    MAC_LSC
   int  rtn = return_block(waste,size);

   if (rtn < 0)
     {
      sprintf(msg_buf, "\nRelease error in genfree (%d)", rtn);
      send_message(msg_buf,NO);
      return;
     }
#else
   free(waste);
#endif

   IF(CHECK_DEBUG IS_ON) THEN
      sprintf(msg_buf,"\nMEM %ld FREE (%d)",waste, size);
      send_message(msg_buf,NO);
   END_IF
   mem_amount -= size;
   mem_calls--;

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   disp_free()
 *
 *  PURPOSE:   This function is a slight mod
 *             to the general
 *             purpose memory release
 *             function. It is only for
 *             use by Mac display functions
 *             which need memory.
 *
 *  INPUTS:    A pointer to the memory to
 *             be released.
 *
 *  OUTPUT:    Nothing.
 * ----------------------------------------
 */

/*^void disp_free(waste,size)
char *waste;
int size;
{

#if    MAC_LSC
   int rtn;

   rtn = return_block(waste,size);
   if (rtn < 0)
     {
      return;
     }
#else
   (void) free (waste);
#endif

   mem_amount -= size;
   mem_calls--;

   return;
}*/

/*******************************************/
/* MEM_USED:  Returns the amount of memory */
/*   currently allocated by CLIPS.         */
/*******************************************/

long int mem_used()
  {
   return(mem_amount);
  }

/**********************************************/
/* MAX_MEM_USED:  Returns the maximum amount  */
/*   of memory ever allocated by CRSV.        */
/**********************************************/

long int max_mem_used()
  {
   return(max_mem);
  }

/***********************************************************/
/* MEM_REQUESTS:  Returns the number of outstanding memory */
/*   memory calls made through memory functions.           */
/***********************************************************/

long int mem_requests()
  {
   return( mem_calls);
  }


/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  ALLOC_FB
 *
 *  PURPOSE:  This function allocates
 *            memory for a new fact
 *            block structure and fills
 *            in the appropriate fields.
 *
 *  INPUTS:   None.
 *
 *  OUTPUT:   A pointer to a fact block
 *            structure.
 * ----------------------------------------
 */

FB_PTR alloc_fb()
{
   FB_PTR temp_fb;

   if((temp_fb = (FB_PTR) gen_alloc(sizeof(FACT_BLOCK),"alloc_fb")) == NULL)
      memory_error();                             /* Allocate memory */

   temp_fb->name       = NULL;                    /* Clear name pointer */
   temp_fb->file       = NULL;                    /* Clear file pointer */
   temp_fb->next_block = NULL;                    /* Clear FB pointer   */
   temp_fb->num_facts  = 0;                       /* Clear num of facts */

   return(temp_fb);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_fb
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            fact blocks.
 *
 *  INPUTS:   A pointer to the head of
 *            the fact block list (FACT_BLOCK *).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            file name stored with each
 *            fact block since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_fb(list)
FB_PTR list;
{
   FB_PTR temp;

   while(list NEQ NULL)
    {
      temp = list;
      list = list->next_block;
      gen_free((char *)temp, (unsigned)sizeof(FACT_BLOCK));
    }
   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_FBR
 *
 *  PURPOSE:   This function allocates
 *             memory for a new fact
 *             block/relation structure
 *             and fills in the appropriate
 *             fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a fact block/
 *             relation structure.
 * ----------------------------------------
 */

FBR_PTR alloc_fbr()
{
   FBR_PTR temp;

   if ((temp = (FBR_PTR) gen_alloc(sizeof(struct fbr_list),"alloc_fbr")) ==
      NULL)
      memory_error();

   temp->fact_block = NULL;
   temp->next_fbr   = NULL;
   temp->num_fields = 0;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_fbr
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            fact blocks/relations.
 *
 *  INPUTS:   A pointer to the head of the
 *            fact block/rel list (FBR_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_fbr(list)
FBR_PTR list;
{
  FBR_PTR temp;

  while(list NEQ NULL)
    {
       temp = list;
       list = list->next_fbr;
       gen_free((char *)temp, (unsigned)sizeof(struct fbr_list));
    }
   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :  ALLOC_RR
 *
 *  PURPOSE:   This function allocates
 *             memory for a new rule/
 *             relation structure and
 *             fills in the appropriate
 *             fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a rule/
 *             relation structure.
 * ----------------------------------------
 */

RR_PTR alloc_rr()
{
   RR_PTR temp;

   if((temp = (RR_PTR) gen_alloc(sizeof(struct rr_list),"alloc_rr")) == NULL)
      memory_error();

   temp->rule       = NULL;
   temp->next_rr    = NULL;
   temp->num_fields = 0;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_rr
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            rule/relation structures.
 *
 *  INPUTS:   A pointer to the head of the
 *            rule/rel list (RR_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_rr(list)
RR_PTR list;
{
   RR_PTR temp;

   while(list != NULL)
    {
      temp = list;
      list = list->next_rr;
      gen_free((char *)temp, (unsigned)sizeof(struct rr_list));
    }

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_REL
 *
 *  PURPOSE:   This function allocates
 *             memory for a new relation
 *             structure and fills in
 *             the appropriate fields.
 *
 *   INPUTS:   None.
 *
 *   OUTPUT:   A pointer to a fact relation
 *             structure.
 * ----------------------------------------
 */

REL_PTR alloc_rel()
{
   REL_PTR temp;

   if((temp = (REL_PTR) gen_alloc(sizeof(RELATION),"alloc_rel")) == NULL)
      memory_error();                            /* Allocate memory */

   temp->name         = NULL;                    /* Clear all pointers */
   temp->lft_rel      = NULL;
   temp->rht_rel      = NULL;
   temp->rule_list    = NULL;
   temp->fb_list      = NULL;
   temp->assert_list  = NULL;
   temp->retract_list = NULL;
   temp->field_list   = NULL;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_rel
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            relations.
 *
 *  INPUTS:   A pointer to the head of the
 *            relation list (RELATION *).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_rel(list)
REL_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_rel NEQ NULL) THEN
         free_rel(list->lft_rel);
      END_IF

      IF(list->rht_rel NEQ NULL) THEN
         free_rel(list->rht_rel);
      END_IF

      free_rr(list->rule_list);
      free_fbr(list->fb_list);
      free_rr(list->retract_list);
      free_rr(list->assert_list);
      free_fld(list->field_list);
      gen_free((char *)list, (unsigned)sizeof(RELATION));
   END_IF

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_RULE
 *
 *  PURPOSE:    This function allocates
 *              memory for a new rule
 *              structure and fills in
 *              the appropriate fields.
 *
 *   INPUTS:   None.
 *
 *   OUTPUT:   A pointer to a rule structure.
 * ----------------------------------------
 */

RULE_PTR alloc_rule()
{
   RULE_PTR temp;

   if ((temp = (RULE_PTR) gen_alloc(sizeof(RULE),"alloc_rule")) == NULL)
      memory_error();                            /* Allocate memory */

   temp->name          = NULL;                   /* Clear all pointers */
   temp->file          = NULL;
   temp->comment       = NULL;
   temp->lft_rule      = NULL;
   temp->rht_rule      = NULL;
   temp->salience_set  = NO;
   temp->salience_val  = 0;
   temp->num_patterns  = 0;
   temp->num_actions   = 0;
   temp->num_retracts  = 0;
   temp->num_asserts   = 0;
   temp->num_printouts = 0;
   temp->num_ex_funcs  = 0;
   temp->num_nots      = 0;
   temp->num_ifs       = 0;
   temp->num_whiles    = 0;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_rule
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            rules.
 *
 *  INPUTS:   A pointer to the head of
 *            the rule list (RULE *).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            file name stored with each
 *            fact block since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_rule(list)
RULE_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_rule NEQ NULL) THEN
         free_rule(list->lft_rule);
      END_IF

      IF(list->rht_rule NEQ NULL) THEN
         free_rule(list->rht_rule);
      END_IF

      gen_free((char *)list, (unsigned)sizeof(RULE));
   END_IF

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_FLD
 *
 *  PURPOSE:   This function allocates
 *             memory for a new field
 *             structure and fills in
 *             the appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a field
 *             structure.
 * ----------------------------------------
 */

FLD_PTR alloc_fld()
{
   FLD_PTR temp;

   if ((temp = (FLD_PTR) gen_alloc(sizeof(struct field),"alloc_fld")) == NULL)
      memory_error();

   temp->field_num  = 0;
   temp->next_field = NULL;
   temp->lit_values = NULL;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_fld
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            fields.
 *
 *  INPUTS:   A pointer to the head of
 *            the field list (FIELD *).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_fld(list)
FLD_PTR list;
{
   FLD_PTR temp;

   while(list NEQ NULL)
    {
      temp = list;
      list  = list->next_field;
      if(temp->lit_values != NULL)
        free_wl(temp->lit_values);
      gen_free((char *)temp, (unsigned)sizeof(FIELD));
    }

   return;
}

/* -------------------------------------------------------
 *  Name : ALLOC_EX_FUNC()
 *         This function allocate the memory for an external
 *         function during processing external function
 *  Return:
 *         Pointer poiniting to the allocated structure (EXF_PTR)
 * --------------------------------------------------------
 */

EXF_PTR alloc_ex_func()
{
   EXF_PTR temp;

   if ((temp = (EXF_PTR) gen_alloc(sizeof(struct ex_func),"alloc_ex")) == NULL)
      memory_error();

   temp->LHS_rule_list = NULL;
   temp->RHS_rule_list = NULL;
   temp->lft_ex_func   = NULL;
   temp->rht_ex_func   = NULL;
   temp->argument_list = NULL;

   return(temp);
}

/* -------------------------------------------------------------------------
 *  Name: FREE_EX_FUNC();
 *          This function frees the memory that ocuppied by the
 *          external function (EXF_PTR)
 *  Return: Nothing
 * --------------------------------------------------------------------------
 */

void free_ex_func(list)
EXF_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_ex_func NEQ NULL) THEN
         free_ex_func(list->lft_ex_func);
      END_IF

      IF(list->rht_ex_func NEQ NULL) THEN
         free_ex_func(list->rht_ex_func);
      END_IF

      free_rr(list->LHS_rule_list);
      free_rr(list->RHS_rule_list);
      free_fld(list->argument_list);

      gen_free((char *)list, (unsigned)sizeof(EX_FUNC));
   END_IF

   return;
}


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_WL
 *
 *  PURPOSE:   This function allocates
 *             memory for a new word list
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a word list
 *             structure.
 * ----------------------------------------
 */

WL_PTR alloc_wl()
{
   WL_PTR temp;

   if ((temp = (WL_PTR) gen_alloc(sizeof(struct word_list),"all0c_wl")) ==
      NULL)
      memory_error();

   temp->next_word  = NULL;
   temp->word       = NULL;
   temp->times_used = 0;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_wl
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            words.
 *
 *  INPUTS:   A pointer to the head of
 *            the word list (WL_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_wl(list)
WL_PTR list;
{
   WL_PTR temp;

   while(list != NULL)
    {
      temp = list;
      list = list->next_word;
      gen_free((char *)temp, (unsigned)sizeof(WORD_LIST));
    }
   return;
}

/* ======================================================================== */

/* ------------------------------------------------------------------
 *  NAME   : free_word_and_get_next()
 *
 *  PURPOSE: This function frees the current word and returns a
 *           pointer to the next word in the list.
 *
 *  INPUTS:  Three arguments,
 *           The address of the head-of-the-list pointer (WL_PTR *),
 *           a pointer to the word to be freed (WL_PTR),
 *           and a pointer to the previous word (WL_PTR).
 *
 *  RETURNS: A pointer to the next word in the list (WL_PTR).
 * ------------------------------------------------------------------
 */

WL_PTR free_word_and_get_next(head_word,cur_word,last_word)
   WL_PTR *head_word,cur_word,last_word;
{
   WL_PTR old_word;

   old_word = cur_word;

   IF (last_word EQ NULL) THEN
      *head_word = old_word->next_word;
   ELSE
      last_word->next_word = old_word->next_word;
   END_IF

   cur_word = old_word->next_word;

   gen_free((char *)old_word, (unsigned)sizeof(WORD_LIST));

   return(cur_word);
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_NL
 *
 *  PURPOSE:   This function allocates
 *             memory for a new number list
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a number list
 *             structure.
 * ----------------------------------------
 */

NL_PTR alloc_nl()
{
   NL_PTR temp;

   temp = (NL_PTR) gen_alloc(sizeof(struct number_list),"alloc_nl");

   IF(temp EQ NULL) THEN
      memory_error();
   END_IF

   temp->next_number = NULL;
   temp->number      = 0.0;
   temp->times_used  = 0;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_nl
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            numbers.
 *
 *  INPUTS:   A pointer to the head of
 *            the number list (NL_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_nl(list)
NL_PTR list;
{
   NL_PTR temp;

   while ( list != NULL)
    {
      temp = list;
      list = list->next_number;
      gen_free((char *)temp, (unsigned)sizeof(NUM_LIST));
    }

   return;
}

/* ======================================================================== */

/* ------------------------------------------------------------------
 *  NAME   : free_number_and_get_next()
 *
 *  PURPOSE: This function frees the current number and returns a
 *           pointer to the next number in the list.
 *
 *  INPUTS:  Three arguments,
 *           The address of the head-of-the-list pointer (NL_PTR *),
 *           a pointer to the number to be freed (NL_PTR),
 *           and a pointer to the previous number (NL_PTR).
 *
 *  RETURNS: A pointer to the next number in the list (NL_PTR).
 * ------------------------------------------------------------------
 */

NL_PTR free_number_and_get_next(head_num,cur_num,last_num)
   NL_PTR *head_num,cur_num,last_num;
{
   NL_PTR old_num;

   old_num = cur_num;

   IF (last_num EQ NULL) THEN
      *head_num = old_num->next_number;
   ELSE
      last_num->next_number = old_num->next_number;
   END_IF

   cur_num = old_num->next_number;
   gen_free((char *)old_num, (unsigned)sizeof(NUM_LIST));

   return(cur_num);
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_VAR
 *
 *  PURPOSE:   This function allocates
 *             memory for a variable list
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a variable
 *             list structure.
 * ----------------------------------------
 */

VAR_PTR alloc_var()
{
   VAR_PTR temp;

   if ((temp = (VAR_PTR) gen_alloc(sizeof(struct variable_list),
      "alloc_var")) == NULL)
      memory_error();

   temp->next_variable   = NULL;
   temp->prev_variable   = NULL;
   temp->relation        = NULL;
   temp->ex_func         = NULL;
   temp->var             = NULL;
   temp->field_info      = NULL;
   temp->level_retracted = 0;
   temp->block_type      = 0;
   temp->count           = 0;
   temp->type            = 0;
   temp->bound_in_not    = NO;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_var
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            variables.
 *
 *  INPUTS:   A pointer to the head of the
 *            variable list (VAR_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_var(list)
VAR_PTR list;
{
   VAR_PTR temp;

   while(list NEQ NULL)
    {
     IF (list->relation NEQ NULL) THEN
       free_rel(list->relation);
     END_IF

     IF (list->field_info NEQ NULL) THEN
        free_def_fld(list->field_info);
     END_IF

     temp = list;
     list = list->next_variable;
     gen_free((char *)temp, (unsigned)sizeof(VARIABLE_LIST));
    }
   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_DEF_REL
 *
 *  PURPOSE:   This function allocates
 *             memory for a defrelation
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a defrelation
 *             structure.
 * ----------------------------------------
 */

DR_PTR alloc_def_rel()
{
   DR_PTR temp;

   if ((temp = (DR_PTR) gen_alloc(sizeof(struct def_relation),
      "alloc_def_rel")) == NULL)
      memory_error();

   temp->name        = NULL;
   temp->file        = NULL;
   temp->lft_def_rel = NULL;
   temp->rht_def_rel = NULL;
   temp->max_fields  = 10000;          /* Defaults to essentially any */
   temp->min_fields  = 0;              /* number of fields */
   temp->fields      = NULL;
   temp->created_by  = CRSV_MADE;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_def_rel
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            def relations
 *
 *  INPUTS:   A pointer to the head of the
 *            def relations list (DR_PTR).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            file name stored with each
 *            fact block since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_def_rel(list)
DR_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_def_rel NEQ NULL) THEN
         free_def_rel(list->lft_def_rel);
      END_IF

      IF(list->rht_def_rel NEQ NULL) THEN
         free_def_rel(list->rht_def_rel);
      END_IF

      free_def_fld(list->fields);
      gen_free((char *)list, (unsigned)sizeof(DEF_REL));
   END_IF

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_DEF_TMP
 *
 *  PURPOSE:   This function allocates
 *             memory for a deftemplate
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a deftemplate
 *             structure.
 * ----------------------------------------
 */

DT_PTR alloc_def_tmp()
{
   DT_PTR temp;

   if ((temp = (DT_PTR) gen_alloc(sizeof(struct def_template),
      "alloc_def_tmp")) == NULL)
      memory_error();

   temp->name        = NULL;
   temp->file        = NULL;
   temp->lft_def_tmp = NULL;
   temp->rht_def_tmp = NULL;
   temp->slots       = NULL;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_def_tmp
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            def templates
 *
 *  INPUTS:   A pointer to the head of the
 *            def templates list (DT_PTR).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            file name stored with each
 *            fact block since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_def_tmp(list)
DT_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_def_tmp NEQ NULL) THEN
         free_def_tmp(list->lft_def_tmp);
      END_IF

      IF(list->rht_def_tmp NEQ NULL) THEN
         free_def_tmp(list->rht_def_tmp);
      END_IF

      free_def_slots(list->slots);
      gen_free((char *)list, (unsigned)sizeof(DEF_TMP));
   END_IF

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :  free_def_slots
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            def templates slots
 *
 *  INPUTS:   A pointer to the head of the
 *            def templates slots list (DS_PTR).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            slot name stored with each
 *            slot since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_def_slots(slot)
DS_PTR slot;
{
   WHILE (slot) DO
      DS_PTR previous_slot = slot;
      TN_PTR defaults = previous_slot->defaults;

      slot = slot->next_slot;
      gen_free ((char *)previous_slot, (unsigned)sizeof (DEF_SLOT));

      WHILE (defaults) DO
         TN_PTR previous_default = defaults;

         defaults = defaults->next;
         free_token (previous_default->token);
         free_token_node (previous_default);
      DONE;
   DONE;

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_DEF_SLOT
 *
 *  PURPOSE:   This function allocates
 *             memory for a new def slot
 *             structure and fills in
 *             the appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a def slot
 *             structure.
 * ----------------------------------------
 */

DS_PTR alloc_def_slot()
{
   DS_PTR temp;

   if ((temp = (DS_PTR) gen_alloc(sizeof(struct def_slot),"alloc_def_slot")) ==
      NULL)
      memory_error();

   temp->name  = NULL;
   temp->position = 0;
   temp->next_slot = NULL;
   temp->max_elements = 9900;
   temp->min_elements = 0;
   temp->defaults = NULL;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_DEF_FLD
 *
 *  PURPOSE:   This function allocates
 *             memory for a def_field
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a def_field
 *             structure.
 * ----------------------------------------
 */

DF_PTR alloc_def_fld()
{
   DF_PTR temp;

   if ((temp = (DF_PTR) gen_alloc(sizeof(struct def_field),"alloc_def_fld")) ==
      NULL)
      memory_error();

   temp->position         = 0;
   temp->next_field       = NULL;
   temp->allow_word       = YES;
   temp->allow_string     = YES;
   temp->allow_number     = YES;
   temp->possible_words   = NULL;
   temp->possible_strings = NULL;
   temp->possible_numbers = NULL;
   temp->set_max          = NO;
   temp->set_min          = NO;
   temp->min_range        = -1000.0;
   temp->max_range        = 1000.0;

   return(temp);
}


/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_def_fld
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            field definitions.
 *
 *  INPUTS:   A pointer to the head of the
 *            field list (DF_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_def_fld(list)
DF_PTR list;
{
   DF_PTR temp;

   while(list != NULL)
    {
      temp = list;
      list = list->next_field;
      free_wl(temp->possible_words);
      free_wl(temp->possible_strings);
      free_nl(temp->possible_numbers);
      gen_free((char *)temp, (unsigned)sizeof(DEF_FLD));
    }

   return;
}

/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_DEF_EX_FUNC
 *
 *  PURPOSE:   This function allocates
 *             memory for a defrelation
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a defrelation
 *             structure.
 * ----------------------------------------
 */

DE_PTR alloc_def_ex_func()
{
   DE_PTR temp;

   if ((temp = (DE_PTR)gen_alloc(sizeof(struct def_external),
      "alloc_def_ex")) == NULL)
      memory_error();

   temp->name             = NULL;
   temp->true_name        = NULL;
   temp->assert_list      = NULL;
   temp->retract_list     = NULL;
   temp->file             = NULL;
   temp->lft_def_ext      = NULL;
   temp->rht_def_ext      = NULL;
   temp->return_word      = NO;
   temp->return_string    = NO;
   temp->return_number    = NO;
   temp->max_arguments    = 10000;          /* Defaults to essentially any */
   temp->min_arguments    = 0;              /* number of arguments */
   temp->arguments        = NULL;

   return(temp);
}

/* ==================================================================== */

/* ----------------------------------------
 *  NAME   :  free_def_ex_func
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            def externals
 *
 *  INPUTS:   A pointer to the head of the
 *            def relations list (DE_PTR).
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   HACK warning! I do not free the
 *            file name stored with each
 *            function block since they are
 *            currently muliply referenced.
 *            This means I am creating
 *            garbage if the program runs
 *            multiple times!
 * ----------------------------------------
 */

void free_def_ex_func(list)
DE_PTR list;
{
   IF(list NEQ NULL) THEN
      IF(list->lft_def_ext NEQ NULL) THEN
         free_def_ex_func(list->lft_def_ext);
      END_IF

      IF(list->rht_def_ext NEQ NULL) THEN
         free_def_ex_func(list->rht_def_ext);
      END_IF

      IF(list->assert_list NEQ NULL) THEN
         free_wl(list->assert_list);
      END_IF

      IF(list->retract_list NEQ NULL) THEN
         free_wl(list->retract_list);
      END_IF

      free_def_fld(list->arguments);
      gen_free((char *)list, (unsigned)sizeof(DEF_EXT));
   END_IF

   return;
}


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   ALLOC_COUNTER
 *
 *  PURPOSE:   This function allocates
 *             memory for a counter
 *             structure and fills in the
 *             appropriate fields.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    A pointer to a counter
 *             structure.
 * ----------------------------------------
 */

COUNTER *alloc_counter()
{
   COUNTER *temp;

   if ((temp = (struct counter *) gen_alloc(sizeof(struct counter),
      "alloc_counter")) == NULL)
      memory_error();

   temp->prime_count   = 0;
   temp->count_A       = 0;
   temp->count_B       = 0;
   temp->count_C       = 0;

   return(temp);
}

/**********************************************************************/

/* ----------------------------------------
 *  NAME   :  alloc_token_node
 *
 *  PURPOSE:  This function allocates the
 *            memory required by a token
 *            node
 *
 *  INPUTS:   None
 *
 *  OUTPUT:   A pointer to an empty
 *            structure (TN_PTR).
 *
 *  NOTE  :   None
 * ----------------------------------------
 */

TN_PTR alloc_token_node ()
{
   TN_PTR temp;

   if ((temp = (struct token_node *) gen_alloc(sizeof(struct token_node),
      "alloc_token_node")) == NULL)
      memory_error();

   temp->token   = NULL;
   temp->next    = NULL;

   return(temp);
}


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :  free_token_node
 *
 *  PURPOSE:  This function frees the
 *            memory used by a token
 *            node
 *
 *  INPUTS:   A pointer to the node which is
 *            to be freed.
 *
 *  OUTPUT:   Nothing
 *
 *  NOTE  :   A token pointed at by the
 *            node is not freed.
 * ----------------------------------------
 */

void free_token_node (node)
 TN_PTR node;
 {
   gen_free ((char *)node, (unsigned)sizeof(TOKEN_NODE));
   return;
 }


/* ======================================================================== */

MULT_REL_PTR alloc_mult_rel()
{
   MULT_REL_PTR temp;

   if ((temp = (MULT_REL_PTR) gen_alloc(sizeof(MULT_REL),"alloc_mult_rel")) ==
      NULL)
      memory_error();

   temp->rel_name = NULL;
   temp->next_rel = NULL;

   return(temp);
}

 /********************************************************************/

/*^ void free_mult_rel(list)
 MULT_REL_PTR list;
 {
     MULT_REL_PTR temp,temp1;

          temp = list;
          while( temp != NULL)
            {
               temp1 = temp;
               temp = temp->next_rel;
               gen_free((char *)temp1, (unsigned)sizeof(MULT_REL));
            }

     return;
 }*/


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :  alloc_token
 *
 *  PURPOSE:  This function allocates the
 *            memory required by a token
 *
 *  INPUTS:   None
 *
 *  OUTPUT:   A pointer to an empty
 *            structure (TK_PTR).
 *
 *  NOTE  :   None
 * ----------------------------------------
 */

TK_PTR alloc_token ()
{
   TK_PTR temp;

   if ((temp = (struct token *) gen_alloc(sizeof(struct token),
      "alloc_token")) == NULL)
      memory_error();

   temp->token   = 0;
   temp->tknnumber = 0;
   temp->tknword = NULL;
   temp->hashword = NULL;
   temp->print_rep = NULL;

   return(temp);
}


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :  free_token
 *
 *  PURPOSE:  This function frees the
 *            memory used by a token
 *
 *  INPUTS:   A pointer to the node which is
 *            to be freed.
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_token (node)
 TK_PTR node;
 {
   gen_free ((char *)node, (unsigned)sizeof(TOKEN));
   return;
 }


/* ======================================================================== */

/* ============================================== *
 *   THE BELOW FUCTIONS ARE SPECIALIZED FOR       *
 *   PROCESSING DRIBBLE FILE.                     *
 * ============================================== */

/* ----------------------------------------------- *
 * NAME    : ALLOC_RL                              *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for REL_LIST  *
 *    structure and initialize it.                 *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the REL_LIST structure     *
 * ----------------------------------------------- */

 REL_LIST_PTR alloc_rl()

 {
     REL_LIST_PTR temp;


     if((temp = (REL_LIST_PTR)gen_alloc(sizeof(REL_LIST),"alloc_rl")) == NULL)
       memory_error();
     temp->next = NULL;

     return(temp);
 }

/* ----------------------------------------
 *  NAME   :  free_rl()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the relations.
 *
 *  INPUTS:   A pointer to the head of the
 *            field list (REL_LIST_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */

void free_rl(list)
REL_LIST_PTR list;
{
   REL_LIST_PTR temp;

   while(list != NULL)
    {
      temp = list;
      list = list->next;
      gen_free((char *)temp, (unsigned)sizeof(REL_LIST));
    }

   return;
}


/* ----------------------------------------------- *
 * NAME    : ALLOC_A_RULE                          *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for ACT_RULE  *
 *    structure and initialize it.                 *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the ACT_RULE structure     *
 * ----------------------------------------------- */

 ACT_R_PTR alloc_a_rule()

 {
   ACT_R_PTR temp;

   if((temp  = (ACT_R_PTR) gen_alloc(sizeof(ACT_RULE),"alloc_a_rule")) == NULL)
      memory_error();

   temp->next = NULL;
   temp->activate_list = NULL;
   temp->fire_num = 0;
   temp->activate_num  = 0;
   temp->deactivate_num = 0;
   return(temp);
 }


/* ----------------------------------------
 *  NAME   :  free_a_rule()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the activated rules.
 *
 *  INPUTS:   A pointer to the head of the
 *            rule_list (ACT_R_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_a_rule(list)
 ACT_R_PTR list;
 {
    ACT_R_PTR temp;

    while(list != NULL)
     {
       temp = list;
       list = list->next;
       if (temp->activate_list != NULL)
        free_rl(temp->activate_list);
       gen_free((char *)temp, (unsigned)sizeof(ACT_RULE));
     }

   return;
 }

/* ----------------------------------------------- *
 * NAME    : ALLOC_CP                              *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for COMP_P    *
 *    structure and initialize it.                 *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the COMP_P structure       *
 * ----------------------------------------------- */

 COMP_P_PTR alloc_cp()

 {
    COMP_P_PTR  temp;

    if((temp = (COMP_P_PTR) gen_alloc(sizeof(COMP_P),"alloc_cp")) == NULL)
       memory_error();

    temp->next = NULL;
    temp->plus_num = 0;
    temp->equal_num = 0;

    return(temp);
 }


/* ----------------------------------------
 *  NAME   :  free_cp()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the compiled pattern.
 *
 *  INPUTS:   A pointer to the head of the
 *            rule_list (COMP_P_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_cp(list)
 COMP_P_PTR list;
 {
   COMP_P_PTR temp;

   while ( list != NULL)
    {
      temp = list;
      list  = list->next;
      gen_free ((char *)temp, (unsigned)sizeof(COMP_P));
    }

   return;
 }

/* ----------------------------------------------- *
 * NAME    : ALLOC_C_RULE                          *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for COMP_R    *
 *    structure and initialize it.                 *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the COMP_R   structure     *
 * ----------------------------------------------- */

 COMP_R_PTR alloc_c_rule()

 {
   COMP_R_PTR temp;

   if((temp = (COMP_R_PTR) gen_alloc(sizeof(COMP_R),"alloc_c_rule")) == NULL)
     memory_error();

   temp->next = NULL;
   temp->combination_num  = 0;
   temp->comb_list = NULL;

   return(temp);
 }

/* ----------------------------------------
 *  NAME   :  free_c_rule()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the compiled rules.
 *
 *  INPUTS:   A pointer to the head of the
 *            rule_list (COMP_P_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_c_rule(list)
 COMP_R_PTR list;
 {
  COMP_R_PTR temp;

  while(list != NULL)
   {
     temp = list;
     list = list->next;
     free_cp(temp->comb_list);
     gen_free((char *)temp, (unsigned)sizeof(COMP_R));
   }
   return;
 }

/* ----------------------------------------------- *
 * NAME    : ALLOC_FIELD                           *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for FIELD_LIST*
 *    structure and initialize it.                 *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the FIELD_LIST structure   *
 * ----------------------------------------------- */

 FIELD_PTR alloc_field()

{
   FIELD_PTR temp;

   if((temp = (FIELD_PTR) gen_alloc(sizeof(FIELD_LIST),"alloc_field")) ==
      NULL)
      memory_error();
   temp->num = 0;
   temp->next = NULL;
   return(temp);
}

/* ----------------------------------------
 *  NAME   :  free_field()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the fields.
 *
 *  INPUTS:   A pointer to the head of the
 *            rule_list (FIELD_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_field(list)
 FIELD_PTR list;
 {
   FIELD_PTR temp;

   while(list != NULL)
    {
      temp =list;
      list = list->next;
      gen_free((char *)temp, (unsigned)sizeof(FIELD));
    }

   return;
 }
/* ----------------------------------------------- *
 * alloc_fact_list()                               *
 *   allocate the memory to store the detail       *
 *   information a relation.                       *
 * ----------------------------------------------- */


 SPE_FACT_PTR alloc_fact_list()
 {
     SPE_FACT_PTR temp;

     if((temp = (SPE_FACT_PTR) gen_alloc(sizeof (SPE_FACT_LIST),
        "alloc_fact_list")) == NULL)
        memory_error();
     temp->next = NULL;
     temp->field = NULL;
     temp->num_activated_a = 0;
     temp->num_deactivated_a = 0;
     temp->activate_list_a = NULL;
     temp->deactivate_list_a = NULL;
     temp->num_activated_d = 0;
     temp->num_deactivated_d = 0;
     temp->activate_list_d = NULL;
     temp->deactivate_list_d = NULL;
     temp->retracted = NO;
     return(temp);
 }

 /* ----------------------------------------
 *  NAME   :  free_fact_list()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            instances of a relation.
 *
 *  INPUTS:   A pointer to the head of the
 *            fact_list (SPE_FACT_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_fact_list(list)
 SPE_FACT_PTR list;
 {
   SPE_FACT_PTR temp;

      while(list != NULL)
    {
      temp = list;
      list  = list->next;
      free_field(temp->field);
      free_rule_list(temp->activate_list_a);
      free_rule_list(temp->activate_list_d);
      free_rule_list(temp->deactivate_list_a);
      free_rule_list(temp->deactivate_list_d);
      gen_free((char *)temp, (unsigned)sizeof(SPE_FACT_LIST));
    }

   return;
 }

/* ----------------------------------------------- *
 * NAME    : ALLOC_A_REL                           *
 *                                                 *
 * PURPOSE :                                       *
 *    This function allocates memory for           *
 *     ACT_RELATION structure and initialize it.   *
 *                                                 *
 * INPUT   : Nothing.                              *
 *                                                 *
 * RETURN  : pointer to the ACT_RELATION structure *
 * ----------------------------------------------- */

 ACT_REL_PTR alloc_a_rel()

 {
   ACT_REL_PTR temp;

   if((temp = (ACT_REL_PTR) gen_alloc (sizeof(ACT_RELATION),"alloc_a_rel")) ==
      NULL)
      memory_error();

   temp->left = NULL;
   temp->right = NULL;
   temp->known = NO;
   temp->current_active = 0;
   temp->max_occurences = 0;
   temp->spec_fact_list = NULL;
   temp->num_assert = 0;
   temp->num_retract = 0;
   temp->activate_num  = 0;
   temp->deactivate_num = 0;

   return(temp);

 }

  /* ----------------------------------------
 *  NAME   :  free_a_rel()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            the activated relations.
 *
 *  INPUTS:   A pointer to the head of the
 *            relation_list (ACT_REL_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_a_rel(list)
 ACT_REL_PTR list;
 {
  IF(list NEQ NULL) THEN
      IF(list->left NEQ NULL) THEN
         free_a_rel(list->left);
      END_IF
      IF(list->right NEQ NULL) THEN
         free_a_rel(list->right);
      END_IF

      free_fact_list(list->spec_fact_list);
      gen_free((char *)list, (unsigned)sizeof(ACT_RELATION));
   END_IF

   return;
 }
/* ------------------------------------------------------ *
 *  allocate memory to store information about a rule     *
 *  that activated aor deactivated because of the         *
 *  retraction or assertion of a relation.                *
 * ------------------------------------------------------ */


 RULE_NAME_PTR alloc_rule_list()
 {
   RULE_NAME_PTR temp;

   if((temp = (RULE_NAME_PTR) gen_alloc(sizeof(struct rule_name_list),
      "alloc_rule_list")) == NULL)
      memory_error();
   temp->name = NULL;
   temp->counter = 0;
   temp->last_fact = NO;
   temp->next = NULL;
   return(temp);
 }

/* ----------------------------------------
 *  NAME   :  free_rule_list()
 *
 *  PURPOSE:  This function frees all the
 *            memory tied up in a list of
 *            rules that were involed in the
 *            retraction or assertion of rules
 *
 *  INPUTS:   A pointer to the head of the
 *            fact_list (RULE_NAME_PTR).
 *
 *  OUTPUT:   Nothing
 * ----------------------------------------
 */
 void free_rule_list(list)
 RULE_NAME_PTR list;
 {
   RULE_NAME_PTR temp;

      while(list != NULL)
    {
      temp = list;
      list = list->next;
      gen_free((char *)temp, (unsigned)sizeof(RULE_NAME));
    }

   return;
 }
/* ====================== END OF DRIBBLE FILE STUFFS ==================== */


/* ======================================================================== */

/* ----------------------------------------
 *  NAME   :   MEMORY_ERROR
 *
 *  PURPOSE:   This function calls
 *             error_message() and
 *             end_crsv() to exit from the
 *             program when malloc()
 *             returns NULL during a
 *             memory-allocation routine.
 *
 *  INPUTS:    None.
 *
 *  OUTPUT:    Nothing useful.
 * ----------------------------------------
 */

void memory_error()
{
   error_message(ERROR,"Out of memory.");
   end_crsv();
}


/* ======================================================================== */

/****************************************************/
/*        CLIPS Memory Module Stuff                 */
/****************************************************/

/*****************************************************************/
/* RELEASE_MEM:  Returns all the lists of stored data structures */
/*   to the pool of free memory.                                 */
/*****************************************************************/

int release_mem(maximum)
int maximum;
{
   struct mem_ptr *tmp_mem, *mptr;
   unsigned int i;
   int amount = 0;

   send_message("\n*** DEALLOCATING MEMORY ***\n",NO);

   for (i = (MEM_TABLE_SIZE - 1) ; i >= sizeof(char *) ; i--)
     {
      mptr = memory_table[i];
      while (mptr != NULL)
        {
         tmp_mem = mptr->next;
         gen_free((char *)mptr, i);
         mptr = tmp_mem;
         amount += i;
        }
      memory_table[i] = NULL;
      if ((amount > maximum) && (maximum > 0))
        {
         send_message("\n*** MEMORY  DEALLOCATED ***\n",NO);
         return;
        }
     }

   send_message("\n*** MEMORY  DEALLOCATED ***\n",NO);
  }


/*****************************************************************/
/* GM2: Allocates memory and does not initialize it              */
/*      (used by hash stuff)                                     */
/*****************************************************************/
char *gm2(size)
   unsigned int size;
  {
   struct mem_ptr *strmem;

   if (size < sizeof(char *)) size = sizeof(char *);

   if (size >= MEM_TABLE_SIZE) return(gen_alloc(size,"gm2"));

   strmem = (struct mem_ptr *) memory_table[size];
   if (strmem == NULL)
     {
      return(gen_alloc(size, "gm2"));
     }

   memory_table[size] = strmem->next;

   return ( (char *) strmem);
  }

/*****************************************************************/
/* RM: Returns memory (used by hash stuff)                       */
/*****************************************************************/
int rm(str,size)
  char *str;
  unsigned int size;
  {
   struct mem_ptr *strmem;

   IF(size < sizeof(char *)) THEN
      size = sizeof(char *);
   END_IF

   IF(size >= MEM_TABLE_SIZE) THEN
      gen_free((char *)str, size);
      return(0);
   END_IF

   strmem = (struct mem_ptr *) str;
   strmem->next = memory_table[size];
   memory_table[size] = strmem;
   return(1);
  }

/*****************************************************************/
/* BLOCK MEMORY FUNCTIONS (for Mac)                              */
/*****************************************************************/

#if MAC_LSC

struct mem_info
  {
   struct mem_info *prev_mem;
   struct mem_info *next_free;
   struct mem_info *last_free;
   int size;
  };

struct buf_info
  {
   struct buf_info *next_buf;
   struct buf_info *prev_buf;
   struct mem_info *buf_top_mem;
   int size;
  };

static struct buf_info   *top_buf;
static int                buf_info_size;
static int                info_size;
static int                init_alloc = FALSE;

/***********************/
/* init_mem:           */
/***********************/
static int init_mem(req_size)
  int req_size;
  {
   extern char *malloc();
   struct mem_info *mem_buf;
   unsigned int init_req;
   int usable_buf_size;

   if (sizeof(char) != 1)
     {
      send_message("\nSize of character data is not 1\n",NO);
      send_message("\nMemory allocation functions may not work\n",NO);
      return(0);
     }

   info_size = sizeof(struct mem_info);
   info_size = (((info_size - 1) / 4) + 1) * 4;

   buf_info_size = sizeof(struct buf_info);
   buf_info_size = (((buf_info_size - 1) / 4) + 1) * 4;

   init_req = (INITBUFFERSIZE > req_size ? INITBUFFERSIZE : req_size);
   init_req += info_size * 2 + buf_info_size;
   init_req = (((init_req - 1) / 4) + 1) * 4;

   usable_buf_size = init_req - (2 * info_size) - buf_info_size;

   /* make sure we get a buffer big enough to be usable */
   if ((req_size < INITBUFFERSIZE) &&
       (usable_buf_size <= req_size + info_size))
     {
      init_req = req_size + info_size * 2 + buf_info_size;
      init_req = (((init_req - 1) / 4) + 1) * 4;
      usable_buf_size = init_req - (2 * info_size) - buf_info_size;
     }

   top_buf = (struct buf_info *) malloc(init_req);

   if (top_buf == NULL)
     {
      send_message("\nUnable to allocate initial memory pool\n",NO);
      return(0);
     }

   top_buf->next_buf = NULL;
   top_buf->prev_buf = NULL;
   top_buf->buf_top_mem = (struct mem_info *) ((char *) top_buf +
      buf_info_size);
   top_buf->size = usable_buf_size;

   mem_buf = (struct mem_info *) ((char *) top_buf + buf_info_size +
      info_size + usable_buf_size);
   mem_buf->next_free = NULL;
   mem_buf->last_free = NULL;
   mem_buf->prev_mem = top_buf->buf_top_mem;
   mem_buf->size = 0;

   top_buf->buf_top_mem->next_free = NULL;
   top_buf->buf_top_mem->last_free = NULL;
   top_buf->buf_top_mem->prev_mem = NULL;
   top_buf->buf_top_mem->size = usable_buf_size;

   init_alloc = TRUE;
   return(1);
  }

/********************************/
/* init_new_buffer:             */
/********************************/
static int init_new_buffer(buf_ptr,req_size)
  struct buf_info *buf_ptr;
  int req_size;
  {
   extern char *malloc();

   unsigned int buf_size;
   int usable_buf_size;
   struct buf_info *new_buf;
   struct mem_info *top_new_buf;

   buf_size = (BUFFERSIZE > req_size ? BUFFERSIZE : req_size);
   buf_size += buf_info_size + info_size * 2;
   buf_size = (((buf_size - 1) / 4) + 1) * 4;

   usable_buf_size = buf_size - buf_info_size - (2 * info_size);

   /* make sure we get a buffer big enough to be usable */
   if ((req_size < BUFFERSIZE) &&
       (usable_buf_size <= req_size + info_size))
     {
      buf_size = req_size + info_size * 2 + buf_info_size;
      buf_size = (((buf_size - 1) / 4) + 1) * 4;
      usable_buf_size = buf_size - (2 * info_size) - buf_info_size;
     }

   new_buf = (struct buf_info *) malloc(buf_size);

   if (new_buf == NULL) return(0);

   new_buf->next_buf = NULL;
   new_buf->prev_buf = buf_ptr;
   new_buf->buf_top_mem = (struct mem_info *) ((char *) new_buf +
      buf_info_size);
   new_buf->size = usable_buf_size;
   buf_ptr->next_buf = new_buf;

   top_new_buf = (struct mem_info *) ((char *) new_buf + buf_info_size +
      info_size + usable_buf_size);
   top_new_buf->next_free = NULL;
   top_new_buf->last_free = NULL;
   top_new_buf->size = 0;
   top_new_buf->prev_mem = new_buf->buf_top_mem;

   new_buf->buf_top_mem->next_free = NULL;
   new_buf->buf_top_mem->last_free = NULL;
   new_buf->buf_top_mem->prev_mem = NULL;
   new_buf->buf_top_mem->size = usable_buf_size;

   return(1);
  }

/********************************/
/* request_block:               */
/********************************/
static char *request_block(req_size)
  int req_size;
  {
   struct mem_info *mem_ptr, *last_free;
   struct buf_info *buf_ptr;

   /*==================================================*/
   /* Allocate initial memory pool block if it has not */
   /* already been allocated.                          */
   /*==================================================*/

   if (init_alloc == FALSE)
      {
       if (init_mem(req_size) == 0) return(NULL);
      }

   /*====================================================*/
   /* Make sure that the amount of memory requested will */
   /* fall on a word boundary.  Assume a word size of 4. */
   /*====================================================*/

   req_size = (((req_size - 1) / 4) + 1) * 4;

   /*=====================================================*/
   /* Search through the list of free memory for a block  */
   /* of the appropriate size.  If a block is found, then */
   /* allocate and return a pointer to it.                */
   /*=====================================================*/

   buf_ptr = top_buf;

   while (buf_ptr != NULL)
     {
      mem_ptr = buf_ptr->buf_top_mem;

      while (mem_ptr != NULL)
        {
         if ((mem_ptr->size == req_size) ||
             (mem_ptr->size > (req_size + info_size)))
           {
            allocate_block(buf_ptr,mem_ptr,req_size);

            return((char *) mem_ptr + info_size);
           }
         mem_ptr = mem_ptr->next_free;
        }

      if (buf_ptr->next_buf == NULL)
        {
         if (init_new_buffer(buf_ptr,req_size) == 0)  /* get another buffer */
           { return(NULL); }
        }
      buf_ptr = buf_ptr->next_buf;
     }

   end_crsv();
  }

/*************************/
/* allocate_block:       */
/*************************/
static int allocate_block(buffer_ptr,block_ptr,req_size)
  struct buf_info *buffer_ptr;
  struct mem_info *block_ptr;
  int req_size;
  {
   struct mem_info *split_mem, *next_mem;

   /*================================================*/
   /* Size of memory block is an exact match for the */
   /* requested amount of memory.                    */
   /*================================================*/
   if (req_size == block_ptr->size)
     {
      block_ptr->size = - req_size;
      if (block_ptr->last_free == NULL)
        {
         if (block_ptr->next_free != NULL)
           { buffer_ptr->buf_top_mem = block_ptr->next_free; }
         else
           { buffer_ptr->buf_top_mem = (struct mem_info *) ((char *)
                buffer_ptr + buf_info_size + info_size + buffer_ptr->size); }

        }
      else
        { block_ptr->last_free->next_free = block_ptr->next_free; }

      if (block_ptr->next_free != NULL)
        { block_ptr->next_free->last_free = block_ptr->last_free; }

      block_ptr->last_free = NULL;
      block_ptr->next_free = NULL;
      return(1);
     }

   /*=============================================*/
   /* Memory block is larger than memory request. */
   /* Split it.                                   */
   /*=============================================*/

   next_mem = (struct mem_info *)
              ((char *) block_ptr + info_size + block_ptr->size);

   split_mem = (struct mem_info *)
                  ((char *) block_ptr + (info_size + req_size));

   split_mem->size = block_ptr->size - (req_size + info_size);
   split_mem->prev_mem = block_ptr;

   split_mem->next_free = block_ptr->next_free;
   split_mem->last_free = block_ptr->last_free;

   next_mem->prev_mem = split_mem;

   if (split_mem->last_free == NULL)
     { buffer_ptr->buf_top_mem = split_mem; }
   else
     { split_mem->last_free->next_free = split_mem; }

   if (split_mem->next_free != NULL)
     { split_mem->next_free->last_free = split_mem; }

   block_ptr->size = - req_size;
   block_ptr->last_free = NULL;
   block_ptr->next_free = NULL;

   return(1);
  }

/*************************/
/* return_block:         */
/*************************/
static int return_block(mem_ptr,size)
  char *mem_ptr;
  int size;
  {
   struct mem_info *info_ptr, *last_mem, *next_mem, *nn_mem;
   struct buf_info *buf_ptr;
   char             buf[128];

   size = (((size - 1) / 4) + 1) * 4;

   info_ptr = (struct mem_info *) ((char *) mem_ptr - info_size);

   IF(info_ptr == NULL) THEN
      return(-1);
   END_IF

   IF(info_ptr->size >= 0) THEN
      return(-2);
   END_IF

   IF(info_ptr->size != -size) THEN
      return(-3);
   END_IF

   info_ptr->size = - info_ptr->size;

   /* find out which buffer you're in */

   nn_mem = info_ptr;
   while (nn_mem->prev_mem != NULL)
     { nn_mem = nn_mem->prev_mem; }
   buf_ptr = (struct buf_info *) ((char *) nn_mem - buf_info_size);

   last_mem = info_ptr->prev_mem;
   next_mem = (struct mem_info *) ((char *) mem_ptr + size);

   if (buf_ptr->buf_top_mem == NULL)
     { return(-4); }

   buf_ptr->buf_top_mem->last_free = info_ptr;
   info_ptr->next_free = buf_ptr->buf_top_mem;
   info_ptr->last_free = NULL;

   buf_ptr->buf_top_mem = info_ptr;

   /*=====================================================*/
   /* Combine this block with previous block if possible. */
   /*=====================================================*/

   if (last_mem != NULL)
     {
      if (last_mem->size > 0)
        {
         last_mem->size += (info_size + info_ptr->size);

         if (next_mem != NULL)
           { next_mem->prev_mem = last_mem; }
         else
           { return(-5); }

         /* Detach last free from list. */

         if (last_mem->last_free != NULL)
           { last_mem->last_free->next_free = last_mem->next_free; }

         if (last_mem->next_free != NULL)
           { last_mem->next_free->last_free = last_mem->last_free; }

         last_mem->next_free = info_ptr->next_free;
         if (info_ptr->next_free != NULL)
           { info_ptr->next_free->last_free = last_mem; }
         last_mem->last_free = NULL;
         buf_ptr->buf_top_mem = last_mem;
         info_ptr->last_free = NULL;
         info_ptr->next_free = NULL;
         info_ptr = last_mem;
        }
     }

   /*=================================================*/
   /* Combine this block with next block if possible. */
   /*=================================================*/

   if (next_mem == NULL) return(-6);
   if (info_ptr == NULL) return(-7);

   if (next_mem->size > 0)
     {
      info_ptr->size += (info_size + next_mem->size);

      nn_mem = (struct mem_info *) ((char *) next_mem + next_mem->size +
         info_size);
      if (nn_mem != NULL)
        { nn_mem->prev_mem = info_ptr; }
      else
        { return(-8); }

      if (next_mem->last_free != NULL)
        { next_mem->last_free->next_free = next_mem->next_free; }

      if (next_mem->next_free != NULL)
        { next_mem->next_free->last_free = next_mem->last_free; }

     }

   /* Free the buffer if we can, but */
   /* don't free the first buffer    */
   /* if it's the only one.          */

   if ((info_ptr->prev_mem == NULL) &&
       (info_ptr->size == buf_ptr->size))
     {
      if (buf_ptr->prev_buf != NULL)
        {
         buf_ptr->prev_buf->next_buf = buf_ptr->next_buf;
         if (buf_ptr->next_buf != NULL)
           { buf_ptr->next_buf->prev_buf = buf_ptr->prev_buf; }
         free(buf_ptr);
        }
      else
        {
         if (buf_ptr->next_buf != NULL)
           {
            buf_ptr->next_buf->prev_buf = NULL;
            top_buf = buf_ptr->next_buf;
            free((char *) buf_ptr);
           }
        }
     }

   return(1);
  }
#endif


