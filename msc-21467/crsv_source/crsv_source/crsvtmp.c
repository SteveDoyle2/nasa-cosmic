#include <stdio.h>
#include "crsv.h"
#if MAC_LSC
#include <strings.h>
#else
#include <string.h>
#endif

/********************************************************************
 *  This file contains all the functions used to process, store, or
 *   retreive deftemplate information.
 ********************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern DT_PTR     alloc_def_tmp();
extern TN_PTR     alloc_token_node();
extern DS_PTR     alloc_def_slot();
extern FIELD_PTR  alloc_field();
extern void       free_def_slots();
extern void       free_token_node();
extern TK_PTR     alloc_token();
extern void       free_token();
extern char      *gen_alloc();
extern void       gen_free();

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern TK_PTR      gettoken_ptr();
extern void        ungettoken_ptr();

/* -----------------------------
 *  From the file: CRSVHASH.C
 * -----------------------------
 */

extern HASH *add_symbol();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void     error_message();
extern void     send_message();

/* -----------------------------
 *  From the file: CRSVPROC.C
 * -----------------------------
 */

extern RELATION *find_rel();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern void     find_rht_paren();
extern void     queue_token();
extern TK_PTR   dequeue_token();

/* -----------------------------
 *  From the file: CRSVREL.C
 * -----------------------------
 */

int        process_defrelation();

/* -----------------------------
 *  From the system
 * -----------------------------
 */

extern double atof();


/* ===========  Functions defined here for Global use  ================ */

int      process_deftemplate();
int      count_temps();
char     *ret_token_type();
DT_PTR   find_def_tmp();
DS_PTR   find_def_slot_by_position();
DS_PTR   find_def_slot();

/* ===========  Functions defined here for internal use  ============== */

static DT_PTR     find_create_def_tmp();
static void       duplicate_def_tmp();
static void       free_defrel_tokens();
static DS_PTR     create_def_slot();
static void       assign_slot_positions ();
static void       queue_var_list ();

/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern int          VERBOSE;              /* Verbose printout flag */
extern char        *cur_obj_name;         /* Name of current object   */
extern FILE        *cur_file;             /* current file pointer  */
extern char        *cur_file_name;        /* Name of the current file */

extern DEF_REL     *def_rel_head;      /* First Element in Defrelation list */

extern DT_PTR       def_tmp_head;      /* First Element in Deftemplate list */
extern RELATION    *rel_head;          /* First element in relation list */

/* -----------------------------
 *  From the file: CRSVREL.C
 * -----------------------------
 */

extern int rel_created_by;             /* Source of the defrelation     */

/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined here for internal use  ============== */

static TN_PTR defrel_tokens = NULL;

/* ======================================================================= */
/*                  FUNCTIONS TO PROCESS DEFTEMPLATES                      */
/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_deftemplate()
 *
 *  PURPOSE: This function reads and stores in the
 *           appropriate data structures all of the
 *           information defined in a deftemplate
 *           construct.
 *
 *  INPUTS:  None.
 *
 *  RETURNS: An integer, -99 (RESTART) if it got
 *           hopelessly confused while trying to
 *           read the deftemplate, a -1 if it found
 *           some kind of syntax error, otherwise, a
 *           1 (OK).
 *
 *  NOTES:   Process_deftemplate acts like a preprocessor.  It reads
 *           the tokens which comprise the deftemplate, creates the
 *           deftemplate structure containing template specific data,
 *           and constructs a defrelation, which is processed by
 *           process_defrelation, for the data shared by deftemplate
 *           and defrelation.
 * ------------------------------------------------------------------
 */

#define AddTokenToDefrel(x) queue_token(&defrel_tokens, x)

int process_deftemplate()
{
   int rtn;
   DT_PTR cur_def_tmp;
   TK_PTR tknstruct;
   int found_multi_field = NO;

   /*============================*/
   /* Check for deftemplate name */
   /*============================*/

   tknstruct = gettoken_ptr (cur_file);

   IF(tknstruct->token NEQ WORD) THEN
      IF (VERBOSE IS_ON) THEN
        error_message(ERROR, "Expected a WORD following deftemplate, ");
        sprintf(msg_buf,
           "found <%.40s> of type %.40s.",
           tknstruct->tknword,ret_token_type(tknstruct->token));
        send_message(msg_buf,NO);
      ELSE
        error_message(ERROR, "Expected a name following deftemplate keyword!");
      END_IF
      find_rht_paren();
      free_token (tknstruct);
      return(ERROR);
   END_IF

   cur_obj_name  = tknstruct->tknword;

   IF (VERBOSE IS_ON) THEN
      sprintf(msg_buf,"\n\nProcessing deftemplate: %.40s",cur_obj_name);
      send_message(msg_buf,NO);
   END_IF

   /*==========================================================*/
   /* Check if template has been previously used as a relation */
   /*==========================================================*/

   IF (find_rel(rel_head, cur_obj_name) NEQ NULL) THEN
      sprintf(msg_buf,
        "Template <%.40s> being redefined, previously defined as a relation.",
         cur_obj_name);
      error_message(ERROR, msg_buf);
      free_token (tknstruct);
      return(RESTART);
   END_IF

   /*==========================================*/
   /* Add the template name to the defrelation */
   /*==========================================*/

   AddTokenToDefrel (tknstruct);

   /*======================================*/
   /* Get pointer to deftemplate structure */
   /*======================================*/

   cur_def_tmp = find_create_def_tmp(&def_tmp_head, cur_obj_name);

   /*===================================*/
   /* Begin processing deftemplate info */
   /*===================================*/

   tknstruct = gettoken_ptr(cur_file);

   /*===================================*/
   /* Read and process deftemplate info */
   /*===================================*/

   WHILE (tknstruct->token NEQ RPAREN) DO
      TK_PTR keyword, field_name;
      int tkn_type = 0;

      /*======================================*/
      /* Find opening paren, and get key word */
      /*======================================*/

      IF(tknstruct->token NEQ LPAREN) THEN
        error_message(ERROR,"Expecting a '(' inside a deftemplate");
        free_token (tknstruct);
        free_defrel_tokens ();
        return(RESTART);
      END_IF

      keyword = gettoken_ptr (cur_file);

      /*============================================*/
      /* Process "field" and "multi-field" keywords */
      /*============================================*/

      IF((keyword->token EQ WORD) AND
         (strcmp("field", keyword->tknword) IS_NIL OR
          strcmp ("multi-field", keyword->tknword) IS_NIL)) THEN
         int field_is_multi_valued = keyword->tknword[0] EQ 'm';

         /*===============================*/
         /* Process "multi-field" keyword */
         /*===============================*/

         IF (field_is_multi_valued) THEN
            keyword->hashword = add_symbol("field");
            keyword->tknword = keyword->hashword->contents;

            IF (found_multi_field) THEN
               error_message (ERROR,
                  "Multiple multi-field slot descriptions found.  ");
               send_message ("All occurrances after first converted ",NO);
               send_message ("to field description",NO);

               field_is_multi_valued = NO;
            ELSE
               found_multi_field = YES;
            END_IF
         END_IF

         /*====================*/
         /* Get the field name */
         /*====================*/

         field_name = gettoken_ptr (cur_file);

         IF (field_name->token NEQ WORD) THEN
            IF (VERBOSE IS_ON) THEN
              sprintf(msg_buf,
                "Unrecognized slot name <%.40s>, Expected a WORD got a %.40s.",
                 field_name->tknword,ret_token_type(field_name->token));
            ELSE_IF (CHECK_STYLE IS_ON)
              sprintf (msg_buf,
                 "Unrecognized slot name %.40s inside a deftemplate",
                 field_name->tknword);
            END_IF
            error_message(ERROR, msg_buf);
            find_rht_paren();
         ELSE_IF (find_def_slot (cur_def_tmp, field_name->tknword) NEQ NULL)
            error_message (ERROR,
               "Multiply defined template slot.  New definition ignored.");
            find_rht_paren();
         ELSE
            DS_PTR cur_def_slot = create_def_slot (cur_def_tmp,
               field_name->tknword);
            TN_PTR default_cur;

            /*===========================================================*/
            /* Place a left paren., "field" keyword, and field name into */
            /* the defrelation.                                          */
            /*===========================================================*/

            AddTokenToDefrel (tknstruct);
            AddTokenToDefrel (keyword);
            AddTokenToDefrel (field_name);

            /*=========================================*/
            /* Override default max/min element limits */
            /*=========================================*/

            IF (field_is_multi_valued IS_NO) THEN
               cur_def_slot->max_elements = 1;
               cur_def_slot->min_elements = 1;
            END_IF

            /*===================================*/
            /* Read what should be a left paren. */
            /*===================================*/

            tknstruct = gettoken_ptr(cur_file);

            /*============================*/
            /* Read and process slot info */
            /*============================*/

            tkn_type = 0;

            WHILE (tknstruct->token NEQ RPAREN) DO

               /*============================================================*/
               /* Find opening paren, and get single field attribute keyword */
               /*============================================================*/

               IF(tknstruct->token NEQ LPAREN) THEN
                  error_message(ERROR,
                    "Expecting a left paren in deftemplate slot description");
                  free_token (tknstruct);
                  free_defrel_tokens ();
                  return(RESTART);
               END_IF

               /*=================================*/
               /* Read the attribute keyword name */
               /*=================================*/

               keyword = gettoken_ptr (cur_file);

               /*===============================*/
               /* Process the default attribute */
               /*===============================*/

               IF((keyword->token EQ WORD) AND
                  (strcmp("default", keyword->tknword) IS_NIL)) THEN

                  free_token (tknstruct);
                  free_token (keyword);

                  tkn_type |= 16;

                  /*=============================================*/
                  /* Handle error of multiple default attributes */
                  /*=============================================*/

                  IF (cur_def_slot->defaults) THEN
                     error_message (ERROR,
                        "Multiple default attributes within slot description");
                     find_rht_paren();
                  ELSE

                     /*=====================================*/
                     /* Read the first value in the default */
                     /*=====================================*/

                     tknstruct = gettoken_ptr (cur_file);

                     /*=========================================*/
                     /* Process the default value keyword ?NONE */
                     /*=========================================*/

                     IF (tknstruct->token EQ BWORD AND
                        strcmp (tknstruct->tknword, "NONE") IS_NIL) THEN
                        free_token (tknstruct);
                        tknstruct = gettoken_ptr (cur_file);

                        /*======================================*/
                        /* Expect no values after ?NONE keyword */
                        /*======================================*/

                        IF (tknstruct->token NEQ RPAREN) THEN
                           error_message (ERROR,
                              "Expected right parenthesis after ?NONE.");
                           find_rht_paren();
                        END_IF

                     /*==============================*/
                     /* Process default value tokens */
                     /*==============================*/

                     ELSE
                        do {
                           IF (tknstruct->token NEQ WORD AND
                              tknstruct->token NEQ NUMBER AND
                              tknstruct->token NEQ STRING) THEN
                              sprintf (msg_buf,
                                  "Unrecognized default %.40s in deftemplate",
                                   tknstruct->tknword);
                              error_message (ERROR, msg_buf);
                              free_token (tknstruct);
                           ELSE
                              queue_token (&cur_def_slot->defaults, tknstruct);
                           END_IF

                           tknstruct = gettoken_ptr (cur_file);
                         } while (field_is_multi_valued AND
                           tknstruct->token NEQ RPAREN);

                         /*===============================================*/
                         /* Terminating loop without finding right paren. */
                         /* indicates that multiple values were provided  */
                         /* for a single value slot.                      */
                         /*===============================================*/

                         IF (tknstruct->token NEQ RPAREN) THEN
                            error_message (WARNING,
                              "Multiple default values not permitted in a ");
                            send_message ("single value slot.",YES);
                            find_rht_paren();
                         END_IF
                     END_IF

                     free_token (tknstruct);

                  END_IF

               /*=====================================================*/
               /* Process multi-value keywords when multi-value field */
               /*=====================================================*/

               ELSE_IF((keyword->token EQ WORD) AND field_is_multi_valued AND
                  (strcmp("min-number-of-elements", keyword->tknword) IS_NIL OR
                  strcmp("max-number-of-elements", keyword->tknword) IS_NIL))
                  int *num_elements = (keyword->tknword[1] EQ 'a' ?
                     &cur_def_slot->max_elements :
                     &cur_def_slot->min_elements);

                  /*=========================================================*/
                  /* Read and discard tokens until the span value is reached */
                  /*=========================================================*/

                  free_token (tknstruct);
                  free_token (keyword);
                  tknstruct = gettoken_ptr (cur_file);

                  /*=======================================*/
                  /* Process the span keyword of ?VARIABLE */
                  /*=======================================*/

                  IF((tknstruct->token EQ BWORD) AND
                     strcmp("VARIABLE", tknstruct->tknword) IS_NIL) THEN
                     *num_elements = (keyword->tknword[1] EQ 'a' ? 9900 : 0);

                  /*=============================*/
                  /* Process a non-numeric value */
                  /*=============================*/

                  ELSE_IF(tknstruct->token NEQ NUMBER)
                     IF (VERBOSE IS_ON) THEN
                        error_message(ERROR, "Expected a number for field ");
                        sprintf (msg_buf, "length, but found a %.40s",
                           ret_token_type(tknstruct->token));
                        send_message(msg_buf,NO);
                     ELSE
                        error_message(ERROR,
                           "Expecting a number for field length definition");
                     END_IF

                  /*=========================*/
                  /* Process a numeric value */
                  /*=========================*/

                  ELSE
                     *num_elements = tknstruct->tknnumber;
                  END_IF

               /*========================================================*/
               /* Process an attribute keyword that is not recognized by */
               /* deftemplate                                            */
               /*========================================================*/


               ELSE

                  /*======================================================*/
                  /* Copy all tokens for the attribute to the defrelation */
                  /*======================================================*/

                  AddTokenToDefrel (tknstruct);
                  AddTokenToDefrel (keyword);

                  do {
                     tknstruct = gettoken_ptr (cur_file);

                     /*================================================*/
                     /* Set the possible types of slot values in the   */
                     /* variable tkn_type through a bit-wise operation */
                     /*================================================*/

                     IF ((keyword->token EQ WORD) AND
                         (strcmp("type",keyword->tknword) IS_NIL)) THEN

                       IF(strcmp("WORD",tknstruct->tknword) IS_NIL) THEN
                         tkn_type |= 1;
                       ELSE_IF(strcmp("STRING",tknstruct->tknword) IS_NIL)
                         tkn_type |= 2;
                       ELSE_IF(strcmp("NUMBER",tknstruct->tknword) IS_NIL)
                         tkn_type |= 4;
                       ELSE
                         IF(tknstruct->token NEQ RPAREN) THEN
                           tkn_type |= 8;
                           IF (VERBOSE) THEN
                             sprintf(msg_buf,
               "<%.40s> not a valid type. Expected <WORD, STRING or NUMBER>.",
                                     tknstruct->tknword);
                           ELSE
                             sprintf(msg_buf,"<%.40s> not a valid type.",
                                     tknstruct->tknword);
                           END_IF
                           error_message(ERROR,msg_buf);
                         END_IF

                       END_IF

                     END_IF

                     AddTokenToDefrel (tknstruct);
                  } while (tknstruct->token NEQ RPAREN);
               END_IF

               tknstruct = gettoken_ptr (cur_file);
            END_WHILE

            AddTokenToDefrel (tknstruct);

           /*================================================*/
           /* Test default Values against user defined types */
           /*================================================*/

           default_cur = cur_def_slot->defaults;

           IF(tkn_type EQ 16) THEN
             IF (VERBOSE IS_ON) THEN
               sprintf(msg_buf,
                  "No type specified for defaults in slot <%.40s>.",
                  cur_def_slot->name);
               error_message(WARNING,msg_buf);
             ELSE
               error_message(ERROR,"No type specified for defaults.");
             END_IF
           END_IF

           WHILE(default_cur) DO     /* OTHER THAN ?NONE */

             IF((((default_cur->token->token EQ WORD) AND
                ((tkn_type & 1) NEQ 1)) OR
               ((default_cur->token->token EQ STRING) AND
                ((tkn_type & 2) NEQ 2)) OR
               ((default_cur->token->token EQ NUMBER) AND
                ((tkn_type & 4) NEQ 4))) AND
                (((tkn_type & 8) NEQ 8) AND
                 (tkn_type NEQ 0))) THEN
               sprintf(msg_buf,"Default value <%.40s> not of proper type.",
                       default_cur->token->tknword);
               error_message(ERROR,msg_buf);
             END_IF

             default_cur = default_cur->next;

           END_WHILE

         END_IF


      /*=========================================================*/
      /* Process a keyword that is not recognized by deftemplate */
      /*=========================================================*/

      ELSE
         sprintf(msg_buf,
            "Unrecognized definition: %.40s inside a deftemplate",
            tknstruct->tknword);
         error_message(ERROR, msg_buf);
         find_rht_paren();
      END_IF

      tknstruct = gettoken_ptr (cur_file);
   END_WHILE

   /*===========================================*/
   /* Add the closing paren. to the defrelation */
   /*===========================================*/

   AddTokenToDefrel (tknstruct);

   /*==================================================================*/
   /* Scan slot descriptions to assign field position (the multi-field */
   /* slot is placed last)                                             */
   /*==================================================================*/

   assign_slot_positions (cur_def_tmp);

   /*===========================================================*/
   /* Substitute field positions for field names in the implied */
   /* defrelation.                                              */
   /*===========================================================*/

   {
      TN_PTR tknnode = defrel_tokens->next;

      /*=====================================================*/
      /* WHILE the closing right paren. has not been reached */
      /*=====================================================*/

      WHILE (tknnode->token->token NEQ RPAREN) DO
         int paren_level = 1;

         tknnode = tknnode->next;

         /*==================================*/
         /* Process field id to field number */
         /*==================================*/

         IF (tknnode->token->token EQ WORD AND
            strcmp (tknnode->token->tknword, "field") IS_NIL) THEN
            TK_PTR field_id = tknnode->next->token;

            IF (field_id->token EQ WORD) THEN
               DS_PTR def_slot = find_def_slot (cur_def_tmp,
                  field_id->tknword);

               field_id->token = NUMBER;
               field_id->tknnumber = def_slot->position;
            END_IF
         END_IF

         /*=====================================*/
         /* Chain forward to the closing paren. */
         /*=====================================*/

         WHILE (paren_level NEQ 0) DO
            IF (tknnode->token->token EQ LPAREN) THEN
               paren_level++;
            ELSE_IF (tknnode->token->token EQ RPAREN)
               paren_level--;
            END_IF

            tknnode = tknnode->next;
         DONE
      DONE
   }

   /*======================================================*/
   /* Make the defrelation tokens be read next by gettoken */
   /*======================================================*/

   (void)set_get_token_queue (defrel_tokens);
   defrel_tokens = NULL;

   /*=========================================================*/
   /* Process the defrelation with its source set to template */
   /*=========================================================*/

   rel_created_by = TEMPLATE_MADE;
   rtn = process_defrelation();
   rel_created_by = USER_MADE;

  return(rtn);
}

/* ======================================================================= */

/* -----------------------------------------------------------------
 *  NAME   : ret_token_type()
 *  PURPOSE: Returns the string representation associated with the
 *           token field of the structure token.  This function is
 *           to be used in the error traps especially when VERBOSE
 *           is turned on.
 *
 *  INPUTS : token number
 *
 *  RETURNS: string representation.
 *
 * -----------------------------------------------------------------
 */

char *ret_token_type(token)
int token;
{

  switch(token) {
    case LNOT      : return("~");
    case LAND      : return("&");
    case LOR       : return("|");
    case COAMP     : return("COAMP");
    case SEPARATOR : return("=>");
    case BINDER    : return("<-");
    case MULTIPLE  : return("$?");
    case LPAREN    : return("(");
    case RPAREN    : return(")");
    case SINGLE    : return("?");

    case BWORD     : return("?<var>");
    case BWORDS    : return("$?<var>");
    case OPERATOR  : return("OPERATOR");
    case KUNKNOWN  : return("KUNKNOWN");
    case FCALL     : return("FUNCTION CALL");
    case STOP      : return("END OF FILE");
    case PATTERN   : return("PATTERN");

    case WORD      : return("WORD");
    case POINTER   : return("POINTER");
    case NUMBER    : return("NUMBER");
    case STRING    : return("STRING");
  }
  return("Unknown Token Type");
}



/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : assign_slot_positions()
 *
 *  PURPOSE: Determines the mapping of slot names into field positions.
 *
 *  INPUTS : A pointer to the template.
 *
 *  RETURNS: None
 *
 *  Note: Algorithm assumes that no more than one multi-valued slot
 *        will be found in the template.
 * ------------------------------------------------------------------
 */

static void assign_slot_positions (template)
 DT_PTR template;
 {
   int position = 1;                        /* Field position */
   DS_PTR mf_ptr = NULL;                /* Pointer to multi-field slot */
   DS_PTR def_slot = template->slots;   /* Pointer to current slot */

   /*===================*/
   /* Process all slots */
   /*===================*/

   WHILE (def_slot) DO

      /*===================================================================*/
      /* Assign position to field slots and record location of multi-field */
      /*===================================================================*/

      IF (def_slot->min_elements NEQ 1 OR def_slot->max_elements NEQ 1) THEN
         mf_ptr = def_slot;
      ELSE
         def_slot->position = ++position;
      END_IF

      def_slot = def_slot->next_slot;
   DONE

   /*===========================================*/
   /* Assign field position to multi-field slot */
   /*===========================================*/

   IF (mf_ptr) THEN
      mf_ptr->position = ++position;
   END_IF

   /*====================================================*/
   /* Record the position of the last (by position) slot */
   /*====================================================*/

   template->max_slot_pos = position;
 }

/* ------------------------------------------------------------------
 *  NAME   : free_defrel_tokens()
 *
 *  PURPOSE: This function frees all of the tokens that have been queued
 *           to describe the implied defrelation sturcutre.
 *
 *  INPUTS : None
 *
 *  RETURNS: None
 * ------------------------------------------------------------------
 */

static void free_defrel_tokens ()
 {
   WHILE (defrel_tokens) DO
      free_token (dequeue_token (&defrel_tokens));
   DONE
 }

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_create_def_tmp()
 *
 *  PURPOSE: This function searches through a tree of
 *           deftemplates to find a specific name. If it doesn't
 *           find an entry with that name, it creates one.
 *
 *  INPUTS : A pointer to the root pointer of deftemplates (DT_PTR *)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns a
 *           pointer to the newly created structure.
 *
 *  Note:    The tree is modified directly by changing the value of
 *           the (relative) root pointer passed to find_create_def_tmp.
 * ------------------------------------------------------------------
 */

static DT_PTR find_create_def_tmp(node, name)
DT_PTR  *node;
char   *name;
{
   int rtn;

   /*====================================================*/
   /* Process empty tree (i.e. Create a new deftemplate) */
   /*====================================================*/

   IF(*node EQ NULL) THEN
      *node        = alloc_def_tmp();
      (*node)->name  = name;
      (*node)->file  = cur_file_name;

   /*===================*/
   /* Process tree node */
   /*===================*/

   ELSE
      rtn = strcmp(name, (*node)->name);

      /*==========================*/
      /* Continue traversing tree */
      /*==========================*/

      IF(rtn < 0) THEN
         return (find_create_def_tmp(&((*node)->lft_def_tmp), name));
      ELSE_IF(rtn > 0)
         return (find_create_def_tmp(&((*node)->rht_def_tmp), name));

      /*=============================*/
      /* Found duplicate deftemplate */
      /*=============================*/

      ELSE
         duplicate_def_tmp(*node);
         (*node)->file = cur_file_name;
      END_IF
   END_IF

   return(*node);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : duplicate_def_tmp()
 *
 *  PURPOSE: This function purges the old deftemplate info and prints
 *           an error message if a duplicate template definition is found.
 *
 *  INPUTS : A pointer to the template node.
 *
 *  RETURNS: Nothing
 *
 *  NOTES  : None
 * ------------------------------------------------------------------
 */

static void duplicate_def_tmp(node)
DT_PTR node;
{
   sprintf(msg_buf, "Redefining template %.40s, erasing old info", node->name);
   error_message(WARNING, msg_buf);

   free_def_slots (node->slots);

   node->slots = NULL;

   return;
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_tmp()
 *
 *  PURPOSE: This function searches through a tree of
 *           deftemplates to find a specific name.
 *
 *  INPUTS : A pointer to the tree of deftemplates (DT_PTR)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 * ------------------------------------------------------------------
 */

DT_PTR find_def_tmp(node, name)
DT_PTR node;
char *name;
{
   int rtn;

   /*=================*/
   /* Search complete */
   /*=================*/

   IF(node EQ NULL OR (rtn = strcmp (name, node->name)) IS_NIL) THEN
      return(node);

   /*=================*/
   /* Continue search */
   /*=================*/
   ELSE
      return(find_def_tmp(( rtn < 0 ? node->lft_def_tmp : node->rht_def_tmp),
         name));
   END_IF
}


/* ******************************************  */

int count_temps(node)
DT_PTR node;
{
  if(node)
    return(1 + count_temps(node->lft_def_tmp) +
           count_temps(node->rht_def_tmp));
  else
    return(0);
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_slot()
 *
 *  PURPOSE: This function searches through a deftemplate's slots to
 *           find a specific name.
 *
 *  INPUTS : A pointer to the deftemplate (DT_PTR)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 * ------------------------------------------------------------------
 */

DS_PTR find_def_slot(template, name)
   DT_PTR template;
   char *name;
{
   DS_PTR slot = template->slots;

   WHILE (slot NEQ NULL AND strcmp(name, slot->name) NEQ 0) DO
      slot = slot->next_slot;
   DONE;

   return (slot);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_slot_by_position()
 *
 *  PURPOSE: This function searches through a deftemplate's slots to
 *           find a specific position.
 *
 *  INPUTS : A pointer to the deftemplate (DT_PTR)
 *           and the position of the slot to search for (int)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 * ------------------------------------------------------------------
 */

DS_PTR find_def_slot_by_position(template, position)
   DT_PTR template;
   int position;
{
   DS_PTR slot = template->slots;

   WHILE (slot NEQ NULL AND (position >= slot->position + slot->max_elements OR
      position < slot->position)) DO
      slot = slot->next_slot;
   DONE;

   return (slot);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : create_def_slot()
 *
 *  PURPOSE: This function creates a specific named slot.
 *
 *  INPUTS : A pointer to the deftemplate (DEF_TMP *) whose slot is
 *           being created and a pointer to the name of the new slot.
 *
 *  RETURNS: The new slot structure.
 * ------------------------------------------------------------------
 */

static DS_PTR create_def_slot(template, name)
   DT_PTR template;
   char *name;
{
   DS_PTR new_slot = alloc_def_slot();
   DS_PTR *slot_ptr = &(template->slots);

   new_slot->name = name;
   new_slot->next_slot = NULL;

   WHILE (*slot_ptr) DO
      slot_ptr = &((*slot_ptr)->next_slot);
   DONE;

   *slot_ptr = new_slot;

   return (new_slot);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : templ_pattern_to_rel_pattern()
 *
 *  PURPOSE: This function converts a template pattern into a
 *           standard relation pattern.
 *
 *  INPUTS : A flag, assert_pattern, that is set to true when the
 *           pattern is being asserted and the name of the template.
 *
 *  RETURNS: None
 *
 *  Notes:   The resulting relation pattern is placed in the gettoken
 *           queue so that it will be read by the proper pattern
 *           analysis routines.
 * ------------------------------------------------------------------
 */

void templ_pattern_to_rel_pattern(name, assert_pattern,var_list)
   char *name;
   int assert_pattern;
   FIELD_PTR *var_list;
{
   DT_PTR template = find_def_tmp (def_tmp_head, name);
   TN_PTR *slots = (TN_PTR *)
      gen_alloc ((unsigned)(sizeof (TN_PTR) * (template->max_slot_pos + 1)),
                  "templ_pattern_to_rel_pattern");
   TK_PTR tknstruct = gettoken_ptr (cur_file);
   int position,i;

   /*================================================================*/
   /* The following variables should have been declared in the local */
   /* blocks where they were used.  A compiler bug resulted in the   */
   /* declarations having to be scoped to the entire function.       */
   /*================================================================*/

   TN_PTR *tokens;
   DS_PTR slot_desc;
   TN_PTR *token_list_ptr;
   char *tempstr;

   /*============================*/
   /* Initialize the slots table */
   /*============================*/

   /* bzero ((char *)slots, sizeof (TN_PTR) * (template->max_slot_pos + 1)); */

   /*    tempstr = (char *)slots; */

    for(i = 0; i <  (template->max_slot_pos + 1) ; i++)
      slots[i] = NULL;

   /*====================================*/
   /* Process the fields of the template */
   /*====================================*/

   WHILE (tknstruct->token NEQ RPAREN) DO

      /*=============================================*/
      /* Process relation style syntax in a template */
      /*=============================================*/

      IF (tknstruct->token NEQ LPAREN) THEN
         error_message (ERROR,
              "Expected left parenthesis within template pattern.");

         ungettoken_ptr (tknstruct);

         return;

      /*======================*/
      /* Process valid syntax */
      /*======================*/

      ELSE
         free_token (tknstruct);

         /*=======================*/
         /* Process the slot name */
         /*=======================*/

         tknstruct = gettoken_ptr (cur_file);
         slot_desc = find_def_slot (template, tknstruct->tknword);


         IF(slot_desc NEQ NULL) THEN

           free_token (tknstruct);

           /*==============================*/
           /* Locate the slot's value list */
           /*==============================*/

           token_list_ptr = &slots[slot_desc->position];

           /*==================================================*/
           /* Copy the slot's value tokens into the slot table */
           /*==================================================*/
           i = 1;
           WHILE(i) DO
             tknstruct = gettoken_ptr (cur_file);
             if (tknstruct->token EQ RPAREN) 
               i--;
             else if (tknstruct->token EQ LPAREN) 
               i++;
             else if (((tknstruct->token EQ BWORD) ||
			(tknstruct->token EQ BWORDS)) && (!assert_pattern))
               queue_var_list(var_list,tknstruct->tknword);
             if(i)
              queue_token (token_list_ptr, tknstruct);
           DONE

           free_token (tknstruct);
         ELSE
           IF(VERBOSE IS_ON) THEN
              sprintf(msg_buf,
                 "Slot <%.40s> is not defined in deftemplate <%.40s>.",
                 tknstruct->tknword,template->name);
              error_message(ERROR,msg_buf);
           ELSE
              error_message(ERROR,"Slot not defined in a deftemplate.");
           END_IF
           free_token (tknstruct);
           find_rht_paren();
         END_IF
      END_IF

      tknstruct = gettoken_ptr (cur_file);
   DONE

   /*=============================================================*/
   /* Process the slot table into the equivalent relation pattern */
   /*=============================================================*/

   FOR (position = 2; position <= template->max_slot_pos; position++) DO
      tokens = &slots[position];

      /*=========================================*/
      /* Get the description of the current slot */
      /*=========================================*/

      slot_desc = find_def_slot_by_position (template, position);

      /*===========================================*/
      /* Process explicit slot value specification */
      /*===========================================*/

      IF (*tokens) THEN
         WHILE (*tokens) DO
            ungettoken_ptr (dequeue_token (tokens));
         DONE

      /*=======================================*/
      /* Implied slot value (inside an assert) */
      /*=======================================*/

      ELSE_IF (assert_pattern)
         TN_PTR tknnode = slot_desc->defaults;

         IF (tknnode) THEN
            WHILE (tknnode) DO
               TK_PTR copy = alloc_token();
               char *str1 = (char *)tknnode->token , *str2 = (char *) copy;
               
             /*  bcopy ((char *)tknnode->token, (char *)copy, sizeof(TOKEN));*/

               for (i = 0; i < sizeof(TOKEN) ;i++)
                 str2[i] = str1[i];

               ungettoken_ptr (copy);

               tknnode = tknnode->next;
            DONE
         ELSE
            TK_PTR null_token = alloc_token();

            null_token->token = WORD;
            null_token->tknword = "nil";
            null_token->hashword = add_symbol(null_token->tknword);

            ungettoken_ptr (null_token);
         END_IF

      /*==============================================*/
      /* Implied slot value (a pattern to be matched) */
      /*==============================================*/

      ELSE
         TK_PTR wildcard = alloc_token();

         IF (slot_desc->min_elements EQ 1 AND
            slot_desc->max_elements EQ 1) THEN
            wildcard->tknword = "?";
            wildcard->token = SINGLE;
         ELSE
            wildcard->tknword = "$?";
            wildcard->token = MULTIPLE;
         END_IF

         ungettoken_ptr (wildcard);
      END_IF
   DONE

   /*============================*/
   /* Process the closing paren. */
   /*============================*/

   ungettoken_ptr (tknstruct);

   /*=====================*/
   /* Free the slot table */
   /*=====================*/

   gen_free ((char *)slots, (unsigned)(sizeof (TN_PTR) * (template->max_slot_pos + 1)));
}

/* ======================================================================= */

void queue_var_list(var_list,var)
FIELD_PTR *var_list;
char *var;
{
   FIELD_PTR node,temp;
  
   
   temp = *var_list;
   while(temp != NULL)
    {
       if(strcmp (var, temp->word) == 0)
         break;
       temp = temp->next;
    }
   if (temp == NULL)
     {
       node = alloc_field();
       node->word = var;
       node->num = 1;
       node->next = *var_list;
       *var_list = node;
     }
   else
      temp->num += 1;
     
}
