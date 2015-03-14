#include <stdio.h>
#include "crsv.h"

/********************************************************************
 *  This file contains all the functions used to process defrelation
 *  structures and store or retreive defrelation information.
 ********************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern DR_PTR     alloc_def_rel();
extern DF_PTR     alloc_def_fld();
extern WL_PTR     alloc_wl();
extern NL_PTR     alloc_nl();
extern void       free_wl();
extern void       free_nl();
extern WL_PTR     free_word_and_get_next();
extern NL_PTR     free_number_and_get_next();

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern int      gettoken();
extern void     save_token();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void     error_message();
extern void     send_message();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern void     find_rht_paren();
extern WL_PTR   add_to_word_list();
extern NL_PTR   add_to_number_list();
extern int      is_word_in_list();
extern int      is_number_in_list();

/* -----------------------------
 *  From the file: CRSVPROC.C
 * -----------------------------
 */

extern RELATION  *find_rel();

/* -----------------------------
 *  From the system
 * -----------------------------
 */

extern double atof();


/* ===========  Functions defined here for Global use  ================ */

int      process_defrelation();
DEF_FLD *copy_field_info();
void     check_field_info();
void     check_var_field_info();
void     check_min_num_fields();


/* ===========  Functions defined here for internal use  ============== */

DEF_REL   *find_create_def_rel();
DEF_REL   *find_def_rel();
int        process_field_def();
DEF_FLD   *find_def_fld();
void       process_fld_type();
void       process_fld_word_list();
void       process_fld_string_list();
void       process_fld_number_list();
void       process_fld_range();
int        compare_fld_types();
int        compare_fld_word_lists();
int        compare_fld_string_lists();
int        compare_fld_numbers();
int        compare_fld_number_lists();
int        compare_number_list_with_range();
int        compare_fld_ranges();
int        in_number_range();
void       duplicate_def_rel();


/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern char        *cur_obj_name;         /* Name of current object   */
extern FILE        *cur_file;             /* current file pointer  */
extern char        *cur_file_name;        /* Name of the current file */

extern DEF_REL     *def_rel_head;      /* First Element in Defrelation list */
extern RELATION    *rel_head;          /* First Element in relation list */

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern float  TKNNUMBER;                  /* gettoken number return  */
extern char  *TKNWORD;                    /* gettoken string storage */


/* ===========  Variables defined here for Global use  ================ */

int rel_created_by = USER_MADE;

/* ===========  Variables defined here for internal use  ============== */

DEF_REL *current_dr;
int      IS_A_TEMP;


/* ======================================================================= */
/*                  FUNCTIONS TO PROCESS DEFRELATIONS                      */
/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_defrelation()
 *
 *  PURPOSE: This function reads and stores in the
 *           appropriate data structures all of the
 *           information defined in a defrelation
 *           construct.
 *
 *  INPUTS:  None.
 *
 *  RETURNS: An integer, -99 (RESTART) if it got
 *           hopelessly confused while trying to
 *           read the defrelation, a -1 if it found
 *           some kind of syntax error, otherwise, a
 *           1 (OK).
 *
 *  NOTES:   The global symbol, def_rel_head, points at the newly
 *           processed symbol.
 * ------------------------------------------------------------------
 */

int process_defrelation()
{
   int        token;
   DEF_REL   *cur_def_rel;

   /*============================*/
   /* Check for defrelation name */
   /*============================*/

   token = gettoken(cur_file);

   IF((token NEQ WORD)AND(token NEQ STRING) AND (token NEQ NUMBER)) THEN
      error_message(ERROR, "Expected a name following defrelation keyword!");
      find_rht_paren();
      return(ERROR);
   END_IF

   cur_obj_name  = TKNWORD;

   /*======================================*/
   /* Get pointer to defrelation structure */
   /*======================================*/

   def_rel_head = find_create_def_rel(def_rel_head, cur_obj_name);
   cur_def_rel  = current_dr;
   cur_def_rel->created_by = rel_created_by;

   IF (rel_created_by EQ TEMPLATE_MADE) THEN
     IS_A_TEMP = 1;
   ELSE
     IS_A_TEMP = 0;
   END_IF

   /*==============================================================*/
   /* If Verbose flag is on and relation was not already processed */
   /* as a Template, then print out "Processing defrelation ..."   */
   /*==============================================================*/



   IF((VERBOSE IS_ON) AND (!IS_A_TEMP)) THEN
     sprintf(msg_buf,"\n\nProcessing defrelation: %.40s.",cur_obj_name);
     send_message(msg_buf,NO);
   END_IF

   /*============================================*/
   /* Check if relation has been previously used */
   /*============================================*/

   IF((find_rel(rel_head, cur_obj_name)) AND (!IS_A_TEMP)) THEN
      sprintf(msg_buf, "Relation %.40s defined after being used.",
         cur_obj_name);
      error_message(WARNING, msg_buf);
   END_IF

   /*===================================*/
   /* Begin processing defrelation info */
   /*===================================*/

   token = gettoken(cur_file);

   IF(token NEQ LPAREN) THEN
      IF (!IS_A_TEMP)
         error_message(ERROR, "Expecting a left paren inside a defrelation.");
      return(RESTART);
   END_IF


   /*=================================*/
   /* Check for ART style defrelation */
   /*=================================*/

   IF((token EQ BWORD) OR (token EQ BWORDS)) THEN
      find_rht_paren();                   /* Ignore ART style code */
      find_rht_paren();                   /* Provided for compatibility */
      return(ERROR);                      /* Return error, not restart */
   END_IF

   /*===================================*/
   /* Read and process defrelation info */
   /*===================================*/

   while(token NEQ RPAREN) DO

      /*======================================*/
      /* Find opening paren, and get key word */
      /*======================================*/

      IF(token NEQ LPAREN) THEN
         IF (!IS_A_TEMP)
           error_message(ERROR, "Expecting a left paren inside a defrelation.");
         return(RESTART);
      END_IF

      token = gettoken(cur_file);

      IF((token EQ WORD) AND
         (strcmp("min-number-of-fields", TKNWORD) EQ 0)) THEN

         /*==============================*/
         /* Process min-number-of-fields */
         /*==============================*/

         token = gettoken(cur_file);
         IF((token EQ BWORD) AND (strcmp("VARIABLE", TKNWORD) EQ 0)) THEN
            cur_def_rel->min_fields = 0;
         ELSE_IF(token NEQ NUMBER)
            error_message(ERROR,
                          "Expecting a number for field length definition.");
         ELSE
            cur_def_rel->min_fields = TKNNUMBER;
         END_IF
         find_rht_paren();


      ELSE_IF((token EQ WORD) AND
         (strcmp("max-number-of-fields", TKNWORD) EQ 0))

         /*==============================*/
         /* Process max-number-of-fields */
         /*==============================*/

         token = gettoken(cur_file);
         IF((token EQ BWORD) AND (strcmp("VARIABLE", TKNWORD) EQ 0)) THEN
            cur_def_rel->max_fields = 10000;
         ELSE_IF(token NEQ NUMBER)
            error_message(ERROR,
                          "Expecting a number for field length definition.");
         ELSE
            cur_def_rel->max_fields = TKNNUMBER;
         END_IF
         find_rht_paren();


      ELSE_IF((token EQ WORD) AND
         (strcmp("field", TKNWORD) EQ 0))

         /*============================*/
         /* Process a field definition */
         /*============================*/

         process_field_def(cur_def_rel);

      ELSE

         /*========================*/
         /* Unrecognized key word! */
         /*========================*/

         sprintf(msg_buf,
            "Unrecognized definition: %.40s inside a defrelation.",
            TKNWORD);
         error_message(ERROR, msg_buf);
         find_rht_paren();
      END_IF

      token = gettoken(cur_file);
   END_WHILE

  return(OK);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_create_def_rel()
 *
 *  PURPOSE: This function searches through a list of
 *           defrelations to find a specific name. If it doesn't
 *           find an entry with that name, it creates one.
 *
 *  INPUTS : A pointer to the list of defrelations (DEF_REL *)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns a
 *           pointer to the newly created structure.
 * ------------------------------------------------------------------
 */

DEF_REL *find_create_def_rel(node, name)
DR_PTR  node;
char   *name;
{
   int rtn;

   IF(node EQ NULL) THEN                       /* A new defrelation! */
      node        = alloc_def_rel();
      node->name  = name;
      node->file  = cur_file_name;
      current_dr  = node;
   ELSE
      rtn = strcmp(name, node->name);
      IF(rtn < 0) THEN
         node->lft_def_rel = find_create_def_rel(node->lft_def_rel, name);
      ELSE_IF(rtn > 0)
         node->rht_def_rel = find_create_def_rel(node->rht_def_rel, name);
      ELSE
         duplicate_def_rel(name);
         node->max_fields = 10000;
         node->min_fields = 0;
         node->file       = cur_file_name;
         current_dr       = node;
      END_IF
   END_IF

   return(node);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : duplicate_def_rel()
 *
 *  PURPOSE: This function merely prints an error message if
 *           a duplicate relation definition is found.
 *
 *  INPUTS : A pointer to the relation name.
 *
 *  RETURNS: Nothing
 *
 *  NOTES  : This function is called to avoid putting a large
 *           string buffer on the stack many times.
 * ------------------------------------------------------------------
 */

static void duplicate_def_rel(name)
char *name;
{
   sprintf(msg_buf, "Redefining relation %.40s, erasing old info.", name);
   error_message(WARNING, msg_buf);

   return;
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_rel()
 *
 *  PURPOSE: This function searches through a list of
 *           defrelations to find a specific name.
 *
 *  INPUTS : A pointer to the list of defrelations (DEF_REL *)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 * ------------------------------------------------------------------
 */

DEF_REL *find_def_rel(node, name)
DR_PTR  node;
char   *name;
{
   int rtn;

   IF(node EQ NULL) THEN                       /* A new defrelation! */
      return(NULL);
   ELSE
      rtn = strcmp(name, node->name);
      IF(rtn < 0) THEN
         return(find_def_rel(node->lft_def_rel, name));
      ELSE_IF(rtn > 0)
         return(find_def_rel(node->rht_def_rel, name));
      ELSE
         return(node);                         /* An old defrelation! */
      END_IF
   END_IF
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_field_def()
 *
 *  PURPOSE: This function reads and stores in the
 *           appropriate data structures all of the
 *           information about fields from a defrelation.
 *
 *  INPUTS:  A pointer to the current defrelation.
 *
 *  RETURNS: An integer, either OK or ERROR.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 *                      Pseudo Code (may not be complete)
 *
 *   get token
 *   IF token is not a number THEN
 *      send error message
 *      find right paren
 *      return ERROR
 *   ELSE_IF token is < 2 OR > max
 *      send error message
 *      find right paren
 *      return ERROR
 *   END_IF
 *   find_def_field()   -(Check for duplicate field info, create new struct)
 *   IF field was found THEN
 *      send error message
 *      reset values
 *   ELSE
 *      create a new list entry
 *      reset list pointers
 *   END_IF
 *   get token
 *   WHILE token is not a ')' DO    -(Switch on field keywords)
 *      IF token is not a '(' THEN
 *         send error message
 *         find right paren
 *         return ERROR
 *      END_IF
 *      get token
 *
 *      CASE "type"
 *         process_fld_type()
 *      CASE "allowed-strings"
 *         process_fld_string_list()
 *      CASE "allowed-words"
 *         process_fld_word_list()
 *      CASE "allowed-numbers"
 *         process_fld_number_list()
 *      CASE "range"
 *         process_fld_range()
 *      CASE anything else
 *         send error message
 *         find right paren
 *      END CASE
 *      get token
 *   END_WHILE
 *   return OK
 * ------------------------------------------------------------------
 */

static int process_field_def(cur_def_rel)
DEF_REL *cur_def_rel;
{
   int token;
   DEF_FLD *cur_def_fld;

   token = gettoken(cur_file);

   /*=======================*/
   /* Check position number */
   /*=======================*/

   IF(token NEQ NUMBER) THEN
      error_message(ERROR, "Field keyword must be followed by a number.");
      find_rht_paren();
      return(ERROR);
   ELSE_IF((TKNNUMBER < 2) OR (TKNNUMBER > cur_def_rel->max_fields))
      sprintf(msg_buf,"Field position, %d, is too big or too small.",
                  (int) TKNNUMBER);
      error_message(ERROR, msg_buf);
      find_rht_paren();
      return(ERROR);
   END_IF

   /*==============================================*/
   /* Check if field postion has already been used */
   /*==============================================*/

   cur_def_fld = find_def_fld((int) TKNNUMBER, cur_def_rel->fields);

   IF(cur_def_fld NEQ NULL) THEN
      error_message(WARNING, "Duplicate field position, redefining values.");

      free_wl(cur_def_fld->possible_words);
      free_wl(cur_def_fld->possible_strings);
      free_nl(cur_def_fld->possible_numbers);

      cur_def_fld->possible_words   = NULL;
      cur_def_fld->possible_strings = NULL;
      cur_def_fld->possible_numbers = NULL;
      cur_def_fld->allow_word       = YES;
      cur_def_fld->allow_string     = YES;
      cur_def_fld->allow_number     = YES;
      cur_def_fld->set_max          = NO;
      cur_def_fld->set_min          = NO;
   ELSE
      cur_def_fld             = alloc_def_fld();
      cur_def_fld->position   = (int) TKNNUMBER;
      cur_def_fld->next_field = cur_def_rel->fields;
      cur_def_rel->fields     = cur_def_fld;
   END_IF


   /*=======================================*/
   /* Field found, start examining keywords */
   /*=======================================*/

   token = gettoken(cur_file);

   while(token NEQ RPAREN) DO

      /*======================================*/
      /* Find opening paren, and get key word */
      /*======================================*/

      IF(token NEQ LPAREN) THEN
         IF (!IS_A_TEMP)
           error_message(ERROR,
                 "Expecting a left paren inside field definition.");
         return(RESTART);
      END_IF

      token = gettoken(cur_file);

      /*========================*/
      /* Process field keywords */
      /*========================*/

      IF((token EQ WORD) AND (strcmp("type", TKNWORD) EQ 0)) THEN
         process_fld_type(cur_def_fld);

      ELSE_IF((token EQ WORD) AND (strcmp("allowed-strings", TKNWORD) EQ 0))
         process_fld_string_list(cur_def_fld);

      ELSE_IF((token EQ WORD) AND (strcmp("allowed-words", TKNWORD) EQ 0))
         process_fld_word_list(cur_def_fld);

      ELSE_IF((token EQ WORD) AND (strcmp("allowed-numbers", TKNWORD) EQ 0))
         IF((cur_def_fld->set_min IS_YES) OR
            (cur_def_fld->set_max IS_YES)) THEN
            error_message(WARNING,
               "Both RANGE and ALLOWED_NUMBERS have been defined for this field, the RANGE definition will be ignored.");
            cur_def_fld->set_min   = NO;
            cur_def_fld->set_max   = NO;
            cur_def_fld->min_range = -1000;
            cur_def_fld->max_range = 1000;
         END_IF
         process_fld_number_list(cur_def_fld);

      ELSE_IF((token EQ WORD) AND (strcmp("range", TKNWORD) EQ 0))
         IF(cur_def_fld->possible_numbers NEQ NULL) THEN
            error_message(WARNING,
               "Both RANGE and ALLOWED_NUMBERS have been defined for this field, the RANGE definition will be ignored.");
            find_rht_paren();
         ELSE
            process_fld_range(cur_def_fld);
         END_IF

      ELSE

         /*========================*/
         /* Unrecognized key word! */
         /*========================*/

         IF (!IS_A_TEMP) THEN
           sprintf(msg_buf, "Unrecognized key word: %.40s inside a %s.",
                       TKNWORD, "field definition");
           error_message(ERROR, msg_buf);
         END_IF

         find_rht_paren();
      END_IF

      token = gettoken(cur_file);
   END_WHILE

  return(OK);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_fld()
 *
 *  PURPOSE: This function searches through a list of
 *           field definitions to find a specific field number.
 *
 *  INPUTS : A pointer to the list of field definitions (DF_PTR *)
 *           and a field number to search for (int).
 *
 *  RETURNS: If the number is found, it returns a pointer to
 *           that structure. If it isn't found, it returns NULL.
 * ------------------------------------------------------------------
 */

DF_PTR find_def_fld(number, list)
int number;
DF_PTR   list;

{
   DF_PTR temp;

   temp = list;
   while(temp NEQ NULL) DO
      IF(number EQ temp->position) THEN
         return(temp);
      END_IF

      temp = temp->next_field;
   END_WHILE

   return(NULL);
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_fld_type()
 *
 *  PURPOSE: This function reads and inteprets the
 *           allowable data type for a field
 *
 *  INPUTS:  A pointer to the current field definition (DF_PTR *).
 *
 *  RETURNS: nothing.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 *                      Pseudo Code (may not be complete)
 *
 *   get token
 *   WHILE token isn't a ')' DO
 *      IF token is ?VARIABLE THEN
 *         set all allowed types to YES
 *         find right paren
 *         return OK
 *      ELSE_IF token is "NUMBER" THEN
 *         set allowed number flag
 *      ELSE_IF token is "WORD" THEN
 *         set allowed word flag
 *      ELSE_IF token is "STRING" THEN
 *         set allowed string flag
 *      ELSE
 *         send error message
 *      END_IF
 *      get token
 *   END_WHILE
 *   IF no flags are set THEN
 *      send error message
 *      return ERROR
 *   END_IF
 *   return OK
 * ------------------------------------------------------------------
 */

void process_fld_type(cur_def_fld)
DF_PTR cur_def_fld;
{
   int token;
   int set_num    = NO;
   int set_word   = NO;
   int set_string = NO;

   token = gettoken(cur_file);

   while(token NEQ RPAREN) DO
      IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
         cur_def_fld->allow_word   = YES;
         cur_def_fld->allow_number = YES;
         cur_def_fld->allow_string = YES;
         find_rht_paren();
         return;
      ELSE_IF(strcmp(TKNWORD,"NUMBER") EQ 0)
         set_num = YES;
      ELSE_IF(strcmp(TKNWORD,"STRING") EQ 0)
         set_string = YES;
      ELSE_IF(strcmp(TKNWORD,"WORD") EQ 0)
         set_word = YES;
      ELSE_IF(!IS_A_TEMP)
         error_message(ERROR, "Unknown field or argument type.");
      END_IF

      token = gettoken(cur_file);
   END_WHILE

   cur_def_fld->allow_word   = set_word;
   cur_def_fld->allow_string = set_string;
   cur_def_fld->allow_number = set_num;

   return;
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_fld_word_list()
 *
 *  PURPOSE: This function reads a list of
 *           allowable words for a field
 *
 *  INPUTS:  A pointer to the current field definition (DF_PTR *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 *                      Pseudo Code (may not be complete)
 *
 *   get token
 *   WHILE token isn't a ')' DO
 *      IF token is ?VARIABLE THEN
 *         set word list to NULL
 *         find right paren
 *         return OK
 *      ELSE_IF token is not a "WORD" THEN
 *         send error message
 *      ELSE
 *         add word to word list
 *      END_IF
 *      get token
 *   END_WHILE
 *   return OK
 * ------------------------------------------------------------------
 */

void process_fld_word_list(cur_def_fld)
DF_PTR cur_def_fld;
{
   int token;

   token = gettoken(cur_file);

   /*============================*/
   /* Read list of allowed words */
   /*============================*/

   while(token NEQ RPAREN) DO

      /*===============================*/
      /* If ?VARIABLE used, clear list */
      /*===============================*/

      IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
         free_wl(cur_def_fld->possible_words);
         cur_def_fld->possible_words = NULL;
         find_rht_paren();
         return;

      /*===========================*/
      /* If not a WORD, then error */
      /*===========================*/

      ELSE_IF((token NEQ WORD) AND (!IS_A_TEMP))
         sprintf(msg_buf, "%s is of wrong type. Expecting a WORD.",TKNWORD);
         error_message(ERROR, msg_buf);

      /*========================*/
      /* Else, add word to list */
      /*========================*/

      ELSE
          cur_def_fld->possible_words =
                      add_to_word_list(cur_def_fld->possible_words,
                                       TKNWORD, WORD,YES);
      END_IF
   token = gettoken(cur_file);
   END_WHILE

   return;
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_fld_string_list()
 *
 *  PURPOSE: This function reads a list of
 *           allowable strings for a field
 *
 *  INPUTS:  A pointer to the current field definition (DF_PTR *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 */

void process_fld_string_list(cur_def_fld)
DF_PTR cur_def_fld;
{
   int token;

   token = gettoken(cur_file);

   /*============================*/
   /* Read list of allowed words */
   /*============================*/

   while(token NEQ RPAREN) DO

      /*===============================*/
      /* If ?VARIABLE used, clear list */
      /*===============================*/

      IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
         free_wl(cur_def_fld->possible_strings);
         cur_def_fld->possible_strings = NULL;
         find_rht_paren();
         return;

      /*=============================*/
      /* If not a STRING, then error */
      /*=============================*/

      ELSE_IF((token NEQ STRING) AND (!IS_A_TEMP))
         sprintf(msg_buf, "%s is of wrong type. Expecting a STRING.",TKNWORD);
         error_message(ERROR,msg_buf);

      /*==========================*/
      /* Else, add string to list */
      /*==========================*/

      ELSE
          cur_def_fld->possible_strings =
                           add_to_word_list(cur_def_fld->possible_strings,
                                            TKNWORD, STRING,YES);
      END_IF
   token = gettoken(cur_file);
   END_WHILE

   return;
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_fld_number_list()
 *
 *  PURPOSE: This function reads a list of
 *           allowable numbers for a field
 *
 *  INPUTS:  A pointer to the current field definition (DF_PTR *).
 *
 *  RETURNS: nothing.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 */

void process_fld_number_list(cur_def_fld)
DF_PTR cur_def_fld;
{
   int token;

   token = gettoken(cur_file);

   /*============================*/
   /* Read list of allowed words */
   /*============================*/

   while(token NEQ RPAREN) DO

      /*===============================*/
      /* If ?VARIABLE used, clear list */
      /*===============================*/

      IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
         free_nl(cur_def_fld->possible_numbers);
         cur_def_fld->possible_numbers = NULL;
         find_rht_paren();
         return;

      /*=============================*/
      /* If not a NUMBER, then error */
      /*=============================*/

      ELSE_IF((token NEQ NUMBER) AND (!IS_A_TEMP))
         sprintf(msg_buf, "%s is of wrong type. Expecting a NUMBER.",TKNWORD);
         error_message(ERROR,msg_buf);

      /*==========================*/
      /* Else, add number to list */
      /*==========================*/

      ELSE
          cur_def_fld->possible_numbers =
                          add_to_number_list(cur_def_fld->possible_numbers,
                                             TKNNUMBER,YES);
      END_IF
   token = gettoken(cur_file);
   END_WHILE

   return;
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_fld_range()
 *
 *  PURPOSE: This function reads the min and max range
 *           values a number can have.
 *
 *  INPUTS:  A pointer to the current field definition (DF_PTR *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 */

void process_fld_range(cur_def_fld)
DF_PTR cur_def_fld;
{
   int token;

   IF(cur_def_fld->allow_number IS_NO) THEN
      error_message(ERROR,
            "Type definition does not include NUMBER, RANGE is inappropriate.");
      find_rht_paren();
      return;
   END_IF

   token = gettoken(cur_file);

   /*===========================================*/
   /* Process MIN, If ?VARIABLE used, clear min */
   /*===========================================*/

   IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
      cur_def_fld->set_min = NO;

   /*=========================*/
   /* If a RPAREN, then error */
   /*=========================*/

   ELSE_IF(token EQ RPAREN)
      error_message(ERROR, "Expecting min value to define range.");
      return;

   /*=============================*/
   /* If not a NUMBER, then error */
   /*=============================*/

   ELSE_IF(token NEQ NUMBER)
      error_message(ERROR, "Expecting a number for min value to define range.");

   ELSE
      cur_def_fld->set_min   = YES;
      cur_def_fld->min_range = TKNNUMBER;
   END_IF

   token = gettoken(cur_file);

   /*===========================================*/
   /* Process MAX, If ?VARIABLE used, clear max */
   /*===========================================*/

   IF((token EQ BWORD) AND (strcmp(TKNWORD,"VARIABLE") EQ 0)) THEN
      cur_def_fld->set_max = NO;

   /*=========================*/
   /* If a RPAREN, then error */
   /*=========================*/

   ELSE_IF(token EQ RPAREN)
      error_message(ERROR, "Expecting max value to define range.");
      return;

   /*=============================*/
   /* If not a NUMBER, then error */
   /*=============================*/

   ELSE_IF(token NEQ NUMBER)
      error_message(ERROR, "Expecting a number for max value to define range.");

   ELSE
      cur_def_fld->set_max   = YES;
      cur_def_fld->max_range = TKNNUMBER;
   END_IF

   find_rht_paren();
   return;
}

/* ------------------------------------------------------------------
 *  NAME   : copy_field_info()
 *
 *  PURPOSE: This function copies the field info from the current
 *           field into a new def_field structure and returns a
 *           pointer to that structure.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the relation name, (char *)
 *           the position in the pattern, (int)
 *
 *  RETURNS: A pointer to the new structure (DEF_FLD *)
 *
 *  NOTES:   Does not check field number here!
 * ------------------------------------------------------------------
 */

DEF_FLD *copy_field_info(rel_name,field_pos)
   char *rel_name;
   int   field_pos;
{
   DEF_FLD *new_def_fld,*cur_fld;
   DEF_REL *rel;
   WL_PTR   wl_ptr;
   NL_PTR   nl_ptr;

   /*===========================*/
   /* Find defrelation (if any) */
   /*===========================*/

   rel = find_def_rel(def_rel_head, rel_name);

   /*=======================================*/
   /* No defrelation, flag error and return */
   /*=======================================*/

   IF(rel EQ NULL) THEN
      def_rel_head = find_create_def_rel(def_rel_head, rel_name);
      IF (strcmp(rel_name, "initial-fact") NEQ 0) THEN
         sprintf(msg_buf, "Relation <%.40s> used prior to its definition.",
            rel_name);
         error_message(WARNING, msg_buf);
      END_IF
      return(NULL);
   END_IF

   cur_fld = find_def_fld(field_pos, rel->fields);
   IF(cur_fld EQ NULL) THEN

      /*================================*/
      /* This field not defined, return */
      /*================================*/

      return(NULL);
   END_IF

   new_def_fld = alloc_def_fld();

   /*================*/
   /* Copy type info */
   /*================*/

   new_def_fld->allow_word   = cur_fld->allow_word;
   new_def_fld->allow_string = cur_fld->allow_string;
   new_def_fld->allow_number = cur_fld->allow_number;

   /*================*/
   /* Copy word list */
   /*================*/

   for (wl_ptr = cur_fld->possible_words; wl_ptr NEQ NULL;
         wl_ptr = wl_ptr->next_word) DO
      new_def_fld->possible_words =
         add_to_word_list(new_def_fld->possible_words, wl_ptr->word, WORD,NO);
   END_FOR

   /*==================*/
   /* Copy string list */
   /*==================*/

   for (wl_ptr = cur_fld->possible_strings; wl_ptr NEQ NULL;
         wl_ptr = wl_ptr->next_word) DO
      new_def_fld->possible_strings =
         add_to_word_list(new_def_fld->possible_strings, wl_ptr->word, STRING,
         NO);
   END_FOR

   /*==================*/
   /* Copy number list */
   /*==================*/

   for (nl_ptr = cur_fld->possible_numbers; nl_ptr NEQ NULL;
         nl_ptr = nl_ptr->next_number) DO
      new_def_fld->possible_numbers =
         add_to_number_list(new_def_fld->possible_numbers, nl_ptr->number, NO);
   END_FOR

   /*=================*/
   /* Copy range info */
   /*=================*/

   new_def_fld->set_max   = cur_fld->set_max;
   new_def_fld->set_min   = cur_fld->set_min;
   new_def_fld->max_range = cur_fld->max_range;
   new_def_fld->min_range = cur_fld->min_range;

   return(new_def_fld);
}


/* ======================================================================= */
/*                  FUNCTIONS TO CHECK DEFRELATION INFO                    */
/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : check_field_info()
 *
 *  PURPOSE: This function compares info from relations
 *           used in a rule or deffact to the defrelation
 *           paramaters and prints appropriate error
 *           messages.
 *
 *  INPUTS:  Four arguments,
 *           A pointer to the relation name, (char *)
 *           the type of the value, WORD, STRING, NUMBER,(int)
 *           the position in the pattern, (int)
 *           and the literal value of the field (char *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   Does not check field number here!
 * ------------------------------------------------------------------
 */

void check_field_info(name, type, position, value)
char *name, *value;
int   type,  position;
{
   DEF_REL  *def_rel;
   DEF_FLD  *fld;
   float     number;
   char     *def_form;

   /*===========================*/
   /* Find defrelation (if any) */
   /*===========================*/

   def_rel = find_def_rel(def_rel_head, name);

   /*=======================================*/
   /* No defrelation, flag error and return */
   /*=======================================*/

   IF(def_rel EQ NULL) THEN
      def_rel_head = find_create_def_rel(def_rel_head, name);
      IF (strcmp(name, "initial-fact") NEQ 0) THEN
         sprintf(msg_buf, "Relation <%.40s> used prior to its definition.",
            name);
         error_message(WARNING, msg_buf);
      END_IF
      return;
   END_IF

   def_form = def_rel->created_by NEQ TEMPLATE_MADE ? "defrelation" : 
      "deftemplate";

   /*==================*/
   /* Check field info */
   /*==================*/

   fld = find_def_fld(position, def_rel->fields);
   IF(fld EQ NULL) THEN

      /*================================*/
      /* This field not defined, return */
      /*================================*/

      return;
   ELSE

      /*=====================================*/
      /* Check field type and literal values */
      /*=====================================*/

      switch (type) {

         case WORD:
              /*=====================*/
              /* Check WORDS allowed */
              /*=====================*/

              IF(fld->allow_word NEQ YES) THEN
                 sprintf(msg_buf,
                    "Used a WORD in field %d, which isn't allowed by %s.",
                    position, def_form);
                 error_message(ERROR, msg_buf);


              /*=================*/
              /* Check WORD list */
              /*=================*/

              ELSE_IF (fld->possible_words EQ NULL OR
                 is_word_in_list(fld->possible_words, value) IS_NO)

                 /* ------------------------------------------------
                  *  If LITERAL is on, the word will be added to the
                  *  ALLOWED_WORDS list.  If VERBOSE is on, a message
                  *  will be printed which will tell the user that
                  *  this is being done.
                  * ------------------------------------------------
                  */

                 IF (LITERAL IS_ON) THEN

                    fld->possible_words = add_to_word_list(fld->possible_words,
                         value, WORD, YES);

                    IF (VERBOSE IS_ON) THEN
                      sprintf(msg_buf,
                        "Adding <%.40s> to ALLOWED-WORDS list of %s field %d.",
                          value, def_rel->name, fld->position);
                       error_message(WARNING, msg_buf);
                    END_IF

                 /* ----------------------------------------------------------
                  *  ELSE_IF The permitted list was not NULL (i.e. ?VARIABLE)
                  *  AND check style messages are turned on
                  *  Generate a style warning message
                  * ----------------------------------------------------------
                  */

                 ELSE_IF (fld->possible_words NEQ NULL)
                    sprintf(msg_buf,
                     "Used a WORD <%.40s> in field %d of %s %s",value,position,
                        name,"that is not in the ALLOWED-WORDS list.");
                    error_message(WARNING, msg_buf);
                 END_IF

              END_IF
              break;

         case STRING:
              /*=======================*/
              /* Check STRINGS allowed */
              /*=======================*/

              IF(fld->allow_string NEQ YES) THEN
                 sprintf(msg_buf,
                    "Used a STRING in field %d, which isn't allowed by %s.",
                         position, def_form);
                 error_message(ERROR,msg_buf);

              /*====================*/
              /* Check STRINGS list */
              /*====================*/

              ELSE_IF(fld->possible_strings EQ NULL OR
                 is_word_in_list(fld->possible_strings, value) IS_NO)

                 /* ---------------------------------------
                  *  Add string to WORD list if LITERAL
                  *  flag turned on.
                  * ---------------------------------------
                  */

                 IF (LITERAL IS_ON) THEN

                    fld->possible_strings = 
                       add_to_word_list(fld->possible_strings,value,STRING,NO);

                    IF (VERBOSE IS_ON) THEN
                      sprintf(msg_buf,
                      "Adding <%.40s> to ALLOWED-STRINGS list of %s field %d.",
                         value, def_rel->name, fld->position);
                        error_message(WARNING,msg_buf);
                    END_IF

                 /* ------------------------------
                  *  ELSE_IF The permitted list was not NULL (i.e. ?VARIABLE)
                  *  AND check style messages are turned on
                  *  Generate a style warning message
                  * ------------------------------
                  */

                 ELSE_IF (fld->possible_strings NEQ NULL)
                    sprintf(msg_buf,
                       "Used a STRING (%.40s) in field %d that is not in the ALLOWED_STRINGS list.",
                       value, position);
                    error_message(ERROR, msg_buf);
                 END_IF

              END_IF
              break;

         case NUMBER:
              /*=======================*/
              /* Check NUMBERS allowed */
              /*=======================*/

              IF(fld->allow_number NEQ YES) THEN
                 sprintf(msg_buf,
           "Used a NUMBER <%s> in relation <%s> field %d, which isn't allowed by %s.",
                    value,def_rel->name,position, def_form);
                 error_message(ERROR,msg_buf);
              ELSE
                 /*====================*/
                 /* Check NUMBERS list */
                 /*====================*/

                 number = (float) atof(value);
                 IF (fld->possible_numbers EQ NULL OR 
                    is_number_in_list(fld->possible_numbers, number) IS_NO)
                    THEN

                    /* ---------------------------------------
                     *  Add number to NUMBERS list if LITERAL
                     *  flag turned on.
                     * ---------------------------------------
                     */

                    IF (LITERAL IS_ON) THEN

                      fld->possible_numbers =
                           add_to_number_list(fld->possible_numbers,number,NO);

                      IF (VERBOSE IS_ON) THEN
                         sprintf(msg_buf,
                          "Adding <%.40s> to the ALLOWED-NUMBERS list of %s field %d.",
                              value, def_rel->name, fld->position);
                         error_message(WARNING,msg_buf);
                      END_IF
    
                    /* ------------------------------
                     *  ELSE_IF The permitted list was not NULL(i.e. ?VARIABLE)
                     *  AND check style messages are turned on
                     *  Generate a style warning message
                     * ------------------------------
                     */

                    ELSE_IF (fld->possible_numbers NEQ NULL)
                       sprintf(msg_buf,
                          "Used a NUMBER (%.40s) in relation %s field %d that is not in the ALLOWED_NUMBERS list.",
                          value, def_rel->name, position);
                       error_message(ERROR, msg_buf);
                    END_IF
                 END_IF

                 /*=====================*/
                 /* Check NUMBERS range */
                 /*=====================*/

                 IF(fld->set_max EQ YES) THEN
                    IF(number > fld->max_range) THEN
                       sprintf(msg_buf,
                          "Used a value (%f) in field %d greater than range max (%f).",
                          number, position, fld->max_range);
                       error_message(ERROR, msg_buf);
                    END_IF
                 END_IF

                 IF(fld->set_min EQ YES) THEN
                    IF(number < fld->min_range) THEN
                       sprintf(msg_buf,
                          "Used a value (%f) in field %d smaller than range min (%f).",
                          number, position, fld->min_range);
                       error_message(ERROR, msg_buf);
                    END_IF
                 END_IF
              END_IF

              break;

         }
   END_IF
}

/* ------------------------------------------------------------------
 *  NAME   : check_var_field_info()
 *
 *  PURPOSE: This function checks to see that a variable is used
 *           in a field whose defrelation info is consistent with
 *           the info from fields in which the variable was used
 *           previously.
 *
 *  INPUTS:  Four arguments,
 *           A pointer to the relation name, (char *)
 *           a pointer to the variable name, (char *),
 *           a pointer to the structure containing the cumulative
 *              field info for that variable (DEF_FLD *),
 *           and the position in the pattern, (int).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   Does not check field number here!
 * ------------------------------------------------------------------
 */

void check_var_field_info(rule_side,rel_name, var_name, var_fld, position)
   char    *rel_name, *var_name;
   DEF_FLD *var_fld;
   int      rule_side, position;
{
   DEF_REL  *def_rel;
   DEF_FLD  *fld;
   int       found_common;
   int       num_in_common,tot_num_in_common = 0;

   /*===========================*/
   /* Find defrelation (if any) */
   /*===========================*/

   def_rel = find_def_rel(def_rel_head, rel_name);

   /*=======================================*/
   /* No defrelation, flag error and return */
   /*=======================================*/

   IF(def_rel EQ NULL) THEN
      def_rel_head = find_create_def_rel(def_rel_head, rel_name);
      IF (strcmp(rel_name, "initial-fact") NEQ 0) THEN
         sprintf(msg_buf, "Relation <%.40s> used prior to its definition.",
            rel_name);
         error_message(WARNING, msg_buf);
      END_IF
      return;
   END_IF

   /*==================*/
   /* Check field info */
   /*==================*/

   fld = find_def_fld(position, def_rel->fields);
   IF(fld EQ NULL) THEN

      /*================================*/
      /* This field not defined, return */
      /*================================*/

      return;
   END_IF

   /*============================*/
   /* Compare data types allowed */
   /*============================*/

   found_common = compare_fld_types(var_fld,fld);
   IF(found_common IS_NO) THEN
    IF(rule_side == LHS) THEN
      sprintf(msg_buf,
         "Due to the inconsistency of the field definitions, this rule MAY NOT fire. Variable <%.40s> has incompatible type definitions.",
         var_name);
      error_message(WARNING, msg_buf);
    ELSE
      sprintf(msg_buf,"variable <%.40s> has incompatible type definitions.",
              var_name);
      error_message(ERROR,msg_buf);
    END_IF

   /*===========================================================*/
   /* If data types are not exclusive, look at lists and ranges */
   /*===========================================================*/

    ELSE

      /*====================*/
      /* Compare word lists */
      /*====================*/

          num_in_common = compare_fld_word_lists(var_name,var_fld,fld);

       /* ------------------------------------------------------
        *  If there are no words in common and LITERAL is Off,
        *  then print out an error message, because if LITERAL
        *  is on, we are adding values to the allowed lists
        *  as the file is parsed and there is no need to display
        *  error messages that will be cleared up by further
        *  parsing.
        * -------------------------------------------------------
        */

       IF ((num_in_common == 0) AND (LITERAL IS_OFF)) THEN
          sprintf(msg_buf,
          "Variable <%.40s> used in fields whose ALLOWED_WORDS definitions have no common values.", 
              var_name);
          error_message(ERROR, msg_buf);
       ELSE_IF (num_in_common > 0 )
          tot_num_in_common += num_in_common;
       END_IF

      /*======================*/
      /* Compare string lists */
      /*======================*/

      num_in_common = compare_fld_string_lists(var_name,var_fld,fld);
      IF ((num_in_common == 0) AND (LITERAL IS_OFF)) THEN
          sprintf(msg_buf,
          "Variable <%.40s> used in arguments whose ALLOWED_STRINGS definitions have no common values.", 
              var_name);
          error_message(ERROR, msg_buf);
      ELSE_IF (num_in_common > 0)
         tot_num_in_common += num_in_common;
      END_IF

      /*=================*/
      /* Compare numbers */
      /*=================*/

     num_in_common = compare_fld_numbers(var_name,var_fld,fld);

     if (num_in_common > 0)
       tot_num_in_common += num_in_common;
      /*=====================================================*/
      /* If no common values found in ANY of the categories, */
      /*    print error message                              */
      /*=====================================================*/

      IF(tot_num_in_common EQ 0) THEN
       IF(rule_side == LHS) THEN
         sprintf(msg_buf,
            "Due to the inconsistency of the field defintions, this rule MAY NOT fire.  Variable <%.40s> has no common allowed values.",
            var_name);
         error_message(WARNING, msg_buf);
       END_IF

      /*============================================*/
      /* Otherwise, if only one common value found, */
      /*    print warning message                   */
      /*============================================*/

      ELSE_IF(tot_num_in_common EQ 1)
         sprintf(msg_buf, "Field definitions allow variable <%.40s> only one possible value.",
                 var_name);
         error_message(WARNING,msg_buf);
      END_IF
   END_IF
}



/* ------------------------------------------------------------------
 *  NAME   : compare_fld_types()
 *
 *  PURPOSE: This function checks the types allowed in the current
 *           field against those used in previous fields.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: YES if found a common type,
 *           NO  if didn't.
 * ------------------------------------------------------------------
 */

int compare_fld_types(var_fld,cur_fld)
   DEF_FLD *var_fld,*cur_fld;
{
   int found_common = NO;

   /*===========================*/
   /* Check if word types match */
   /*===========================*/

   IF(var_fld->allow_word IS_YES) THEN
      IF(cur_fld->allow_word IS_YES) THEN
         found_common = YES;
      ELSE
         var_fld->allow_word     = NO;
         free_wl(var_fld->possible_words);
         var_fld->possible_words = NULL;
      END_IF
   END_IF

   /*=============================*/
   /* Check if string types match */
   /*=============================*/

   IF(var_fld->allow_string IS_YES) THEN
      IF(cur_fld->allow_string IS_YES) THEN
         found_common = YES;
      ELSE
         var_fld->allow_string     = NO;
         free_wl(var_fld->possible_strings);
         var_fld->possible_strings = NULL;
      END_IF
   END_IF

   /*=============================*/
   /* Check if number types match */
   /*=============================*/

   IF(var_fld->allow_number IS_YES) THEN
      IF(cur_fld->allow_number IS_YES) THEN
         found_common = YES;
      ELSE
         var_fld->allow_number     = NO;
         free_nl(var_fld->possible_numbers);
         var_fld->possible_numbers = NULL;
         var_fld->set_min          = NO;
         var_fld->set_max          = NO;
         var_fld->min_range        = -1000;
         var_fld->max_range        = 1000;
      END_IF
   END_IF

   return(found_common);
}

/* ------------------------------------------------------------------
 *  NAME   : compare_fld_word_lists()
 *
 *  PURPOSE: This function compares the list of allowed words in the
 *           current field against the cumulative word list from
 *           previous fields.
 *
 *  INPUTS:  Three arguments,
 *           The variable's name (char *),
 *           a pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if no common values,
 *           1000 (no count) if either word list is NULL,
 *           or the number of common words if neither is NULL.
 * ------------------------------------------------------------------
 */

int compare_fld_word_lists(var_name,var_fld,cur_fld)
   char    *var_name;
   DEF_FLD *var_fld,*cur_fld;
{
   WL_PTR var_wl, cur_wl, old_word = NULL;
   int    num_in_common, found_word;

   /*=========================================*/
   /* If no words allowed, return -1, no error */
   /*=========================================*/

   IF(var_fld->allow_word IS_NO) THEN
      return(-1);
   END_IF

   var_wl = var_fld->possible_words;
   cur_wl = cur_fld->possible_words;

   /*==================================================================*/
   /* If no word list specified by current field, no need to calculate */
   /*    overlap                                                       */
   /*==================================================================*/

   IF(cur_wl EQ NULL) THEN
      return(1000);
   END_IF

   /*=========================================================*/
   /* Otherwise, if no word list specified by previous fields */
   /*=========================================================*/

   IF(var_wl EQ NULL) THEN

      /*===============================================================*/
      /* Copy current field's word list, no need to count common words */
      /*===============================================================*/

      while(cur_wl NEQ NULL) DO
         var_wl = add_to_word_list(var_wl, cur_wl->word, WORD,NO);
         cur_wl = cur_wl->next_word;
      END_WHILE
      var_fld->possible_words = var_wl;
      return(1000);
   END_IF

   /*==============================*/
   /* If neither word list is NULL */
   /*==============================*/

   num_in_common = 0;

   while (var_wl NEQ NULL) DO
      found_word = NO;
      cur_wl     = cur_fld->possible_words;
      while (cur_wl NEQ NULL) DO

         /*===================*/
         /* Compare each word */
         /*===================*/

         IF (strcmp(cur_wl->word,var_wl->word) EQ 0) THEN
            found_word = YES;
            break;
         END_IF
         cur_wl = cur_wl->next_word;
      END_WHILE

      /*==================================================================*/
      /* If a word was in a previous field, but not in the current field, */
      /*    delete it from the variable's cumulative word list            */
      /*==================================================================*/

      IF(found_word IS_NO) THEN
         var_wl = free_word_and_get_next(&var_fld->possible_words,
                                          var_wl,old_word);

      /*======================================*/
      /* Otherwise, count it as a common word */
      /*======================================*/

      ELSE
         ++num_in_common;
         old_word = var_wl;
         var_wl   = var_wl->next_word;
      END_IF
   END_WHILE

/*
   IF(num_in_common EQ 0) THEN
      sprintf(msg_buf,"Variable <%.40s> used in field %d whose ALLOWED_WORDS definitions have no common values.",
           cur_fld->position,   var_name);
      error_message(ERROR, msg_buf);
   END_IF
*/

   return(num_in_common);
}

/* ------------------------------------------------------------------
 *  NAME   : compare_fld_string_lists()
 *
 *  PURPOSE: This function compares the list of allowed strings in
 *           the current field against the cumulative string list
 *           from previous fields.
 *
 *  INPUTS:  Three arguments,
 *           The variable's name (char *),
 *           a pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if no common values,
 *           1000 (no count) if either string list is NULL,
 *           or the number of common strings if neither is NULL.
 * ------------------------------------------------------------------
 */

int compare_fld_string_lists(var_name,var_fld,cur_fld)
   char    *var_name;
   DEF_FLD *var_fld,*cur_fld;
{
   WL_PTR var_wl, cur_wl, old_string = NULL;
   int    num_in_common, found_string;

   /*============================================*/
   /* If no strings allowed, return -1, no error */
   /*============================================*/

   IF(var_fld->allow_string IS_NO) THEN
      return(-1);
   END_IF

   var_wl = var_fld->possible_strings;
   cur_wl = cur_fld->possible_strings;

   /*====================================================================*/
   /* If no string list specified by current field, no need to calculate */
   /*    overlap                                                         */
   /*====================================================================*/

   IF(cur_wl EQ NULL) THEN
      return(1000);
   END_IF

   /*===========================================================*/
   /* Otherwise, if no string list specified by previous fields */
   /*===========================================================*/

   IF(var_wl EQ NULL) THEN

      /*===================================================================*/
      /* Copy current field's string list, no need to count common strings */
      /*===================================================================*/

      while(cur_wl NEQ NULL) DO
         var_wl = add_to_word_list(var_wl, cur_wl->word, STRING,NO);
         cur_wl = cur_wl->next_word;
      END_WHILE

      var_fld->possible_strings = var_wl;
      return(1000);
   END_IF

   /*================================*/
   /* If neither string list is NULL */
   /*================================*/

   num_in_common = 0;

   while (var_wl NEQ NULL) DO
      found_string = NO;
      cur_wl       = cur_fld->possible_strings;
      while (cur_wl NEQ NULL) DO

         /*=====================*/
         /* Compare each string */
         /*=====================*/

         IF (strcmp(cur_wl->word,var_wl->word) EQ 0) THEN
            found_string = YES;
            break;
         END_IF
         cur_wl = cur_wl->next_word;
      END_WHILE

      /*====================================================================*/
      /* If a string was in a previous field, but not in the current field, */
      /*    delete it from the variable's cumulative string list            */
      /*====================================================================*/

      IF(found_string IS_NO) THEN
         var_wl = free_word_and_get_next(&var_fld->possible_strings,
                                          var_wl,old_string);

      /*========================================*/
      /* Otherwise, count it as a common string */
      /*========================================*/

      ELSE
         ++num_in_common;
         old_string = var_wl;
         var_wl     = var_wl->next_word;
      END_IF
   END_WHILE

   /*=======================*/
   /* If no common strings, */
   /*    print warning      */
   /*=======================*/

/*
   IF (num_in_common EQ 0) THEN
      sprintf(msg_buf,"Variable <%.40s> used in fields whose ALLOWED_STRINGS definitions have no common values.",
              var_name);
      error_message(ERROR, msg_buf);
   END_IF
*/

   return(num_in_common);
}

/* ------------------------------------------------------------------
 *  NAME   : compare_fld_numbers()
 *
 *  PURPOSE: This function compares the numeric information from
 *           the current field against the cumulative numeric info
 *           from previous fields.
 *
 *  INPUTS:  Three arguments,
 *           The variable's name (char *),
 *           a pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if no common values,
 *           1000 (no count) if counting was inappropriate,
 *           or the number of common numeric values.
 * ------------------------------------------------------------------
 */

int compare_fld_numbers(var_name,var_fld,cur_fld)
   char    *var_name;
   DEF_FLD *var_fld,*cur_fld;
{
   int  num_in_common;

   /*==============================================*/
   /* If no numbers allowed, return -1, no error   */
   /*==============================================*/

   IF(var_fld->allow_number IS_NO) THEN
      return(-1);
   END_IF

   /*================================================*/
   /* If no range set, compare only the number lists */
   /*================================================*/

   IF((var_fld->set_min IS_NO) AND (var_fld->set_max IS_NO) AND
      (cur_fld->set_min IS_NO) AND (cur_fld->set_max IS_NO)) THEN
      num_in_common = compare_fld_number_lists(var_fld, cur_fld);

      /*======================*/
      /* If no common values, */
      /*    print warning     */
      /*======================*/

      IF(num_in_common EQ 0) THEN
         sprintf(msg_buf,
            "Variable <%.40s> used in fields whose ALLOWED_NUMBERS definitions have no common values.",
            var_name);
         error_message(ERROR, msg_buf);
      END_IF

   /*=======================================================*/
   /* Otherwise, if no number list, compare only the ranges */
   /*=======================================================*/

   ELSE_IF((var_fld->possible_numbers EQ NULL) AND
           (cur_fld->possible_numbers EQ NULL))
      num_in_common = compare_fld_ranges(var_fld, cur_fld);

      /*==================*/
      /* If no overlap,   */
      /*    print warning */
      /*==================*/

      IF(num_in_common EQ 0) THEN
         sprintf(msg_buf,
            "Variable <%.40s> used in fields/arguments whose RANGE \
definitions do not overlap.", var_name);
         error_message(ERROR, msg_buf);
      END_IF

   /*=========================================*/
   /* Otherwise, compare number list to range */
   /*=========================================*/

   ELSE
      num_in_common = compare_number_list_with_range(var_fld, cur_fld);

      /*======================*/
      /* If no common values, */
      /*    print warning     */
      /*======================*/
/*
      IF(num_in_common EQ 0) THEN
         sprintf(msg_buf,
            "Variable <%.40s> used in fields/arguments which do not have \
common values in the RANGE and ALLOWED-NUMBERS definitions.", var_name);
         error_message(ERROR, msg_buf);
      END_IF
*/

   END_IF

   return(num_in_common);
}

/* ------------------------------------------------------------------
 *  NAME   : compare_fld_number_lists()
 *
 *  PURPOSE: This function compares the list of allowed numbers in
 *           the current field against the cumulative number list
 *           from previous fields.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if no common values,
 *           1000 (no count) if either number list is NULL,
 *           or the number of common numbers if neither is NULL.
 * ------------------------------------------------------------------
 */

int compare_fld_number_lists(var_fld,cur_fld)
   DEF_FLD *var_fld,*cur_fld;
{
   NL_PTR var_nl, cur_nl, old_num = NULL;
   int    num_in_common, found_number;

   var_nl = var_fld->possible_numbers;
   cur_nl = cur_fld->possible_numbers;

   /*============================================*/
   /* If current field doesn't have number list, */
   /*    don't reset, don't count common values  */
   /*============================================*/

   IF(cur_nl EQ NULL) THEN
      return(1000);
   END_IF

   /*=================================================*/
   /* Else if previous fields don't have number list, */
   /*    copy current field's number list,            */
   /*    don't count common values                    */
   /*=================================================*/

   IF(var_nl EQ NULL) THEN
      while(cur_nl NEQ NULL) DO
         var_nl = add_to_number_list(var_nl, cur_nl->number,NO);
         cur_nl = cur_nl->next_number;
      END_WHILE

      var_fld->possible_numbers = var_nl;
      return(1000);
   END_IF

   /*=========================*/
   /* If neither list is NULL */
   /*=========================*/

   num_in_common = 0;

   while (var_nl NEQ NULL) DO
      found_number = NO;
      cur_nl = cur_fld->possible_numbers;
      while (cur_nl NEQ NULL) DO

         /*=====================*/
         /* Compare each number */
         /*=====================*/

         IF (cur_nl->number EQ var_nl->number) THEN
            found_number = YES;
            break;
         END_IF
         cur_nl = cur_nl->next_number;
      END_WHILE

      /*====================================================================*/
      /* If a number was in a previous field, but not in the current field, */
      /*    delete it from the variable's cumulative number list            */
      /*====================================================================*/

      IF(found_number IS_NO) THEN
         var_nl = free_number_and_get_next(&var_fld->possible_numbers,
                                          var_nl,old_num);

      /*========================================*/
      /* Otherwise, count it as a common number */
      /*========================================*/

      ELSE
         ++num_in_common;
         old_num = var_nl;
         var_nl  = var_nl->next_number;
      END_IF
   END_WHILE

   return(num_in_common);
}

/* ------------------------------------------------------------------
 *  NAME   : compare_fld_ranges()
 *
 *  PURPOSE: This function compares the range of allowed numbers in
 *           the current field against the cumulative number range
 *           from previous fields.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if ranges don't overlap,
 *           1 if max_range = min_range,
 *           1000 (no count) if ranges overlap by more than 1.
 * ------------------------------------------------------------------
 */

int compare_fld_ranges(var_fld,cur_fld)
   DEF_FLD *var_fld, *cur_fld;
{
   /*==================================================*/
   /* reset the min range to be the greater of the two */
   /*==================================================*/

   IF(cur_fld->set_min IS_YES) THEN
      IF(var_fld->set_min IS_NO) THEN
         var_fld->set_min   = YES;
         var_fld->min_range = cur_fld->min_range;
      ELSE
         IF(var_fld->min_range < cur_fld->min_range) THEN
            var_fld->min_range = cur_fld->min_range;
         END_IF
      END_IF
   END_IF

   /*=================================================*/
   /* reset the max range to be the lesser of the two */
   /*=================================================*/

   IF(cur_fld->set_max IS_YES) THEN
      IF(var_fld->set_max IS_NO) THEN
         var_fld->set_max   = YES;
         var_fld->max_range = cur_fld->max_range;
      ELSE
         IF(var_fld->max_range > cur_fld->max_range) THEN
            var_fld->max_range = cur_fld->max_range;
         END_IF
      END_IF
   END_IF

   /*=============================================================*/
   /* If both range limits have been set, check the range overlap */
   /*=============================================================*/

   IF((var_fld->set_min IS_ON) AND (var_fld->set_max IS_ON)) THEN

      /*================================================*/
      /* If no overlap, reset range values and return 0 */
      /*================================================*/

      IF(var_fld->max_range < var_fld->min_range) THEN
         var_fld->max_range = -1000.0;
         var_fld->min_range = 1000.0;
         return(0);

      /*=========================================*/
      /* If range_min EQ range_max, overlap is 1 */
      /*=========================================*/

      ELSE_IF(var_fld->max_range EQ var_fld->min_range)
         return(1);
      END_IF
   END_IF

   /*================================*/
   /* Otherwise, overlap is infinite */
   /*================================*/

   return(1000);
}

/* --------------------------------------------------------------------
 *  NAME   : compare_fld_ranges()
 *
 *  PURPOSE: This function is called when there is a mix of number
 *           list and range.  It checks to see that the numbers in
 *           number list are all within the range.  The resulting
 *           intersection is a number list.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the cumulative field info (DEF_FLD *),
 *           and a pointer to the current field info (DEF_FLD *).
 *
 *  RETURNS: 0 if no numbers from the number list are inside the range,
 *           1000 (no count) if the range has not been specified,
 *           the count of numbers in the range otherwise.
 * --------------------------------------------------------------------
 */

int compare_number_list_with_range(var_fld, cur_fld)
   DEF_FLD *var_fld, *cur_fld;
{
   NL_PTR nl_ptr,old_num = NULL;
   int    num_in_common = 0;

   /*=================================================*/
   /* If it is the var field that has the number list */
   /*=================================================*/

   IF(var_fld->possible_numbers NEQ NULL) THEN

      /*==============================================================*/
      /* If the current field has no range, don't count common values */
      /*==============================================================*/

      IF((cur_fld->set_min IS_NO) AND (cur_fld->set_max IS_NO)) THEN
         return(1000);
      END_IF

      /*=================================================================*/
      /* Otherwise, check to make sure each number in the list is within */
      /*    the range, and count the common values                       */
      /*=================================================================*/

      nl_ptr = var_fld->possible_numbers;
      while(nl_ptr NEQ NULL) DO
         IF(in_number_range(nl_ptr->number,cur_fld)) THEN
            ++num_in_common;
            old_num = nl_ptr;
            nl_ptr  = nl_ptr->next_number;
         ELSE
            nl_ptr = free_number_and_get_next(&var_fld->possible_numbers,
                                              nl_ptr,old_num);
         END_IF
      END_WHILE

   /*================================================================*/
   /* Otherwise, if it is the current field that has the number list */
   /*================================================================*/

   ELSE
      nl_ptr = cur_fld->possible_numbers;

      /*===============================================================*/
      /* If the var field has no range, copy the list, but don't count */
      /*    common values                                              */
      /*===============================================================*/

      IF((var_fld->set_min IS_NO) AND (var_fld->set_max IS_NO)) THEN
         while(nl_ptr NEQ NULL) DO
            var_fld->possible_numbers =
               add_to_number_list(var_fld->possible_numbers,nl_ptr->number,NO);
            nl_ptr = nl_ptr->next_number;
         END_WHILE
         return(1000);

      /*===================================================================*/
      /* Otherwise, copy only the numbers within the range, and count them */
      /*===================================================================*/

      ELSE
         while(nl_ptr NEQ NULL) DO
            IF(in_number_range(nl_ptr->number,var_fld)) THEN
               ++num_in_common;
               var_fld->possible_numbers =
                  add_to_number_list(var_fld->possible_numbers,nl_ptr->number,
                  NO);
           END_IF
            nl_ptr = nl_ptr->next_number;
         END_WHILE

         var_fld->set_min   = NO;
         var_fld->set_max   = NO;
         var_fld->min_range = -1000;
         var_fld->max_range = 1000;
      END_IF
   END_IF

   return(num_in_common);
}

/* --------------------------------------------------------------------
 *  NAME   : in_number_range()
 *
 *  PURPOSE: This function determines whether or not a number is within
 *           the given field's range of allowed numbers.
 *
 *  INPUTS:  Two arguments,
 *           The number (int),
 *           and a pointer to the field info (DEF_FLD *).
 *
 *  RETURNS: TRUE if the number is inside the range,
 *           and FALSE otherwise.
 * --------------------------------------------------------------------
 */

int in_number_range(number,fld_ptr)
   float    number;
   DEF_FLD *fld_ptr;
{
   IF((fld_ptr->set_min IS_YES) AND (number < fld_ptr->min_range)) THEN
      return(FALSE);
   END_IF

   IF((fld_ptr->set_max IS_YES) AND (number > fld_ptr->max_range)) THEN
      return(FALSE);
   END_IF

   return(TRUE);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : check_num_fields()
 *
 *  PURPOSE: This function compares the number of fields
 *           used in a rule or deffact to the defrelation
 *           paramaters and prints appropriate error
 *           messages (if needed).
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the relation name, (char *)
 *           the number of fields in the pattern (int).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   The number of fields paramater is negative if a
 *           multiple field variable was used in the pattern.
 * ------------------------------------------------------------------
 */

void check_min_num_fields(name, org_fields,assert_flag)
char *name;
int   org_fields, assert_flag;
{
   DEF_REL  *def_rel;
   int       num_fields, USED_MULTI;

   /*===========================*/
   /* Find defrelation (if any) */
   /*===========================*/

   def_rel = find_def_rel(def_rel_head, name);

   /*=======================================*/
   /* No defrelation, flag error and return */
   /*=======================================*/

   IF(def_rel EQ NULL) THEN
      def_rel_head = find_create_def_rel(def_rel_head, name);
      IF (strcmp(name, "initial-fact") NEQ 0) THEN
         sprintf(msg_buf, "Relation <%.40s> used prior to its definition.",
            name);
         error_message(WARNING, msg_buf);
      END_IF
      return;
   END_IF

   /*===========================*/
   /* Check for multi-field use */
   /*===========================*/

   IF(org_fields < 0) THEN
      num_fields = abs(org_fields);
      USED_MULTI = YES;
   ELSE
      num_fields = org_fields;
      USED_MULTI = NO;
   END_IF

   /*==================*/
   /* Check min fields */
   /*==================*/

   IF((def_rel->min_fields > num_fields) AND (USED_MULTI IS_NO)) THEN
      sprintf(msg_buf,"Fewer fields used than defrelation <%.40s> requires.",
               name);
      error_message(ERROR, msg_buf);
   END_IF

   /*==================*/
   /* Check max fields */
   /*==================*/

   IF((def_rel->max_fields < num_fields) AND (USED_MULTI IS_NO)) THEN
      sprintf(msg_buf,"more fields used than defrelation <%.40s> allows.",
               name);
      error_message(ERROR, msg_buf);
   ELSE_IF((def_rel->max_fields EQ num_fields) AND
           (USED_MULTI IS_YES))
      IF (!assert_flag) THEN
        error_message(WARNING, "Unecessary $? variable used.");
      END_IF
   END_IF
}
