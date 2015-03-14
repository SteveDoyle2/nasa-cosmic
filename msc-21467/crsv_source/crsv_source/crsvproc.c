#include <stdio.h>
#include "crsv.h"
#if MAC_LSC
#include <strings.h>
#else
#include <string.h>
#endif
/********************************************************************
 *  This file contains all the functions used to process rules and
 *  deffacts.
 ********************************************************************
 */


/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVDEFS.C
 * ----------------------------
 */

extern DEF_FLD *copy_field_info ();
extern void     check_min_num_fields ();
extern DEF_REL  *find_def_rel ();
extern DEF_FLD  *find_def_fld ();
extern DEF_REL  *find_create_def_rel ();

/* -----------------------------
 *  From the file: CRSVEXF.C
 * -----------------------------
 */

 extern void     check_argument_info ();
 extern void     check_var_argument_info ();
 extern DE_PTR   find_def_ex_func ();
 extern DA_PTR   copy_argument_info ();

/* -----------------------------
 *  From the file: CRSVHASH.C
 * ----------------------------
 */

 extern HASH    *add_symbol ();

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern FB_PTR     alloc_fb ();
extern FBR_PTR    alloc_fbr ();
extern REL_PTR    alloc_rel ();
extern EXF_PTR    alloc_ex_func ();
extern FLD_PTR    alloc_fld ();
extern RULE_PTR   alloc_rule ();
extern RR_PTR     alloc_rr ();
extern TK_PTR	  alloc_token ();
extern VAR_PTR    alloc_var ();
extern void       gen_free ();
extern char      *gen_alloc ();
extern void       free_var ();
extern FIELD_PTR  alloc_field ();
extern void       free_field ();
extern MULT_REL  *alloc_mult_rel ();
extern void       free_def_fld ();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * ----------------------------
 */

extern void     find_rht_paren ();
extern WL_PTR   add_to_word_list ();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * ----------------------------
 */

extern void     error_message ();
extern void     send_message ();
extern void     send_text ();

/* -----------------------------
 *  From the file: CRSVREL.C
 * -----------------------------
 */

extern DEF_FLD *copy_field_info ();
extern void     check_field_info ();
extern void     check_var_field_info ();
extern void     check_min_num_fields ();
extern DEF_REL  *find_def_rel ();
extern DEF_FLD  *find_def_fld ();

/* -----------------------------
 *  From the file: CRSVRSVD.C
 * ----------------------------
 */

extern int      check_ex_funcs ();

/* -----------------------------
 *  From the file: CRSVSTK.C
 * -----------------------------
 */

extern void reset_stack ();
extern void push ();
extern void pop ();
extern int  inside_if ();
extern int  inside_while ();
extern int  num_nest_levels ();
extern int  describe_previous ();
extern void mark_retraction ();

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern int      gettoken ();
extern TK_PTR   gettoken_ptr ();
extern void     ungettoken_ptr ();
extern void     save_token ();

/* -----------------------------
 *  From the file: CRSVTMP.C
 * -----------------------------
 */

 extern DEF_TMP *find_def_tmp ();
 extern void     templ_pattern_to_rel_pattern ();

/* -----------------------------
 *  From the file: CRSVUSR.C
 * -----------------------------
 */

extern void usr_deffact ();
extern void usr_rule ();


/* -----------------------------
 *  From the system:
 * -----------------------------
 */

extern double atof ();


/* ===========  Functions defined here for Global use  ================ */

int         process_rule ();
int         process_fact_block ();
RELATION   *find_rel ();
EX_FUNC    *find_ex_func ();


/* ===========  Functions defined here for internal use  ============== */

RELATION  *find_create_rel ();
EXF_PTR    find_create_ex_func ();
FLD_PTR    get_fld_ptr ();
FLD_PTR    get_arg_ptr ();
RULE      *find_rule ();
FIELD_PTR  process_ex_funcs ();
FIELD_PTR  find_args ();
void       check_variables ();
VAR_PTR    find_variable ();
int        process_LHS_retract ();
int        process_RHS_retract ();
int        process_RHS_modify ();
void       process_LHS_variable ();
void       process_RHS_variable ();
FIELD_PTR  find_bound_words ();
MULT_REL  *check_multiple_relations ();
void       store_not_variable ();
void       link_to_not_variable_list ();
void       process_fb_relation ();
static int process_rule_LHS();


/* ===========  Variables defined here for Global use  ================ */

/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern char        *cur_obj_name;        /* Name of current object   */
extern int          cur_line_num;        /* Current line number   */

extern FILE        *cur_file;            /* current file pointer  */
extern char        *cur_file_name;       /* Name of the current file */

extern FACT_BLOCK  *fb_head;             /* First Element in Fact block list */
extern RULE        *rule_head;           /* First Element in Rule list       */
extern RELATION    *rel_head;            /* First Element in Relation list   */
extern EX_FUNC     *ex_funcs_head;       /* First Element in Ex Funcs list   */
extern DR_PTR       def_rel_head;        /* First Element in Defrelation list*/
extern DE_PTR       def_ext_head;        /* First Element in Defexternal list*/
extern DT_PTR       def_tmp_head;        /* First Element in Deftemplate list*/
extern VAR_PTR      retract_head;        /* First Element in Retract list    */
extern VAR_PTR      variable_head;       /* First Element in Variable list   */
extern VAR_PTR      not_variable_head;   /* First Element in the negation of
                                            variable list                    */


/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern float  TKNNUMBER;                    /* gettoken number return  */
extern char  *TKNWORD;                      /* gettoken string storage */


/* ===========  Variables defined here for internal use  ============== */

REL_PTR      cur_rel;                      /* Current relation pointer */
RULE_PTR     cur_rule;                     /* Current rule pointer */
EXF_PTR      cur_ex_func;                  /* Current external function ptr */


/* ====================================================================
 * ==========       Fact Block Processing Functions       =============
 * ====================================================================
 */

/* ------------------------------------------------------------------
 *  NAME   :  process_fact_block () 
 *
 *  PURPOSE:  This function reads all information about a
 *            deffacts structure and stores it in the
 *            appropriate structure.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  An integer, OK if deffacts was processed properly, 
 *            ERROR if there was a recoverable syntax error, and
 *            RESTART if there was a gross syntax error which
 *            prevents me from finding the closing paren.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 *               Psuedo Code (may not be complete) 
 *
 *    Get fact block name
 *    Check if it is a new fact block, warn if duplicate name
 *    Allocate memory for a new FB structure
 *    Check for fact block comment
 *    Process each relation
 * ------------------------------------------------------------------
 */

int process_fact_block () 
{
   int           rtn;
   int           token;
   char         *cur_fb_name;
   FACT_BLOCK   *cur_fb;

   /*=========================*/
   /* Process fact block name */
   /*=========================*/

   token = gettoken (cur_file);

   IF (token NEQ WORD) THEN
      error_message (ERROR, "Expected a name following deffacts keyword!");
      find_rht_paren ();
      return (ERROR);
   END_IF

   cur_fb_name  = TKNWORD;
   cur_obj_name = cur_fb_name;

   IF (VERBOSE IS_ON) THEN
      sprintf (msg_buf, "\n\nProcessing fact block: %.40s.", cur_fb_name);
      send_message (msg_buf,NO);
   END_IF

   /*====================================*/
   /*Check for duplicate fact block name */
   /*====================================*/

   IF (new_FB_name (cur_fb_name, fb_head) NEQ YES) THEN
      error_message (WARNING, "Duplicate deffacts name.");
   END_IF

   /*==============================*/
   /* Create new fact block struct */
   /*==============================*/

   cur_fb = alloc_fb ();                 /* Create new fact block structure */
   cur_fb->name = cur_fb_name;          /* Store Fact block name in it  */
   cur_fb->file = cur_file_name;        /* along with current file name */

   cur_fb->next_block = fb_head;        /* Put this FB at start of list */
   fb_head = cur_fb;                    /* and reset start of list pointer */

   /*==============================*/
   /* Check for fact block comment */
   /*==============================*/

   token = gettoken (cur_file);
   IF (token EQ STRING) THEN             /* Fact block has a comment */
      token = gettoken (cur_file);       /* so read next token */
   ELSE_IF (CHECK_COMMENTS IS_ON) 
      error_message (WARNING, "Comment not used.");
   END_IF

   /*==============================*/
   /* Process fact block relations */
   /*==============================*/

   while (token NEQ RPAREN) DO
      IF (token EQ LPAREN) THEN                  /* Should have a ' (' */
         cur_fb->num_facts++;
         rtn = find_simple_relation (DEFFACTS);  /* Find relation name */
         IF (rtn NEQ ERROR) THEN
            process_fb_relation (cur_fb);        /* Process relation */
         END_IF

      ELSE                                      /* Error if not a ' (' */
         error_message (ERROR, "Expected a \' (\', can't process this deffact.");
         return (RESTART);
      END_IF
      token = gettoken (cur_file);                /* Get next token */
   END_WHILE

   usr_deffact (cur_fb);                          /* Call user function */
   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  new_FB_name () 
 *
 *  PURPOSE:  This function checks to see if the name of the
 *            current deffacts has been previously used.
 *
 *  INPUTS :  The name of the current deffacts block (char *), 
 *            and a pointer to the list of deffacts (FB_PTR).
 *
 *  RETURNS:  An integer, either YES if this is a new deffacts
 *            name or NO if it is a duplicate.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static int new_FB_name (name, list_head) 
char   *name;
FB_PTR  list_head;
{
   FB_PTR temp;

   temp = list_head;
   while (temp NEQ NULL) DO
      IF (strcmp (name, temp->name) EQ 0) THEN
         return (NO);
      ELSE
         temp = temp->next_block;
      END_IF
   END_WHILE

   return (YES);
}

/* ===================================================================
 * =============       Rule Processing Functions       ===============
 * ===================================================================
 */

/* ------------------------------------------------------------------
 *  NAME   :  process_rule () 
 *
 *  PURPOSE:  This function reads all information about a
 *            rule and stores it in the appropriate structure.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  An integer, OK if rule was processed properly, 
 *            ERROR if there was a recoverable syntax error, and
 *            RESTART if there was a gross syntax error which
 *            prevents me from finding the closing paren.
 *
 *  NOTES  :
 * ------------------------------------------------------------------
 */

int process_rule () 
{
   int      token, rtn;
   VAR_PTR  temp_var, temp_not_var;

   /*=====================*/
   /* Check for rule name */
   /*=====================*/

   token = gettoken (cur_file);

   IF (token NEQ WORD) THEN
      error_message (ERROR, "Expected a name following defrule keyword!");
      find_rht_paren ();
      return (ERROR);
   END_IF

   cur_obj_name  = TKNWORD;

   IF (VERBOSE IS_ON) THEN
      sprintf (msg_buf, "\n\nProcessing rule: %.40s", cur_obj_name);
      send_message (msg_buf,NO);
   END_IF

   /*=================================*/
   /* Check pointer to rule structure */
   /*=================================*/

   rule_head = find_rule (rule_head, cur_obj_name); /* also sets cur_rule */

   /*===================*/
   /* Check for comment */
   /*===================*/

   token = gettoken (cur_file);
   IF (token EQ STRING) THEN             /* Rule has a comment */
      cur_rule->comment = TKNWORD;      /* so read next token */
      token = gettoken (cur_file);
   ELSE_IF (CHECK_COMMENTS IS_ON)
      cur_rule->comment = NULL;
      error_message (WARNING, "Comment not used.");
   END_IF

   /*=====================*/
   /* Process LHS of rule */
   /*=====================*/

   not_variable_head = NULL;
   rtn = process_rule_LHS (cur_rule, token);
   IF (rtn NEQ OK) THEN
      return (rtn);
   END_IF

   /* ============================================================ */
   /*   Check for inconsistency of the negation of the variables   */
   /*   on the LHS of the rule.                                    */
   /* ============================================================ */

    temp_var = variable_head;
    if (not_variable_head != NULL) 
     {
	    while (temp_var != NULL) 
	     {
	       temp_not_var = not_variable_head;
	       while (temp_not_var != NULL) 
	        {
	          if (strcmp (temp_not_var->var, temp_var->var) == 0) 
	           {
	             rtn = compare_field_info (temp_not_var->field_info, 
	             		temp_var->field_info);
	             if (rtn == TRUE) 
	               {
	                 sprintf (msg_buf, 
                            "rule <%.40s> may not fire due to the negation of \
variable <%.40s>.", 
	                           cur_rule->name, temp_not_var->var);
	                 error_message (WARNING, msg_buf);
	               }
	           }
	           temp_not_var = temp_not_var->next_variable;
	        }
	       temp_var = temp_var->next_variable;
	     }
	   }
    not_variable_head = NULL;
   /*=============================*/
   /* Initialize nest-level stack */
   /*=============================*/

   reset_stack ();

   /*=====================*/
   /* Process RHS of rule */
   /*=====================*/

   rtn = process_rule_RHS (cur_rule);
   IF (rtn NEQ OK) THEN
      return (rtn);
   END_IF

   usr_rule (cur_rule, variable_head, retract_head);

   /*=====================================*/
   /* Report on variable binding problems */
   /*=====================================*/

   check_variables ();

   return (rtn);
}

/* ==================================================================== */
static int compare_field_info (fld1, fld2) 
DEF_FLD *fld1, *fld2;
{
   int type;

   /* ----------------------------------------------------
    *
    *             *****   NOTE  *****
    *
    *   This function has a bug which cannot be fixed as
    *   it is a byproduct of the control mechanism of
    *   CRSV.  fld2 in this function is the field values
    *   of a variable found in a relation.  If this 
    *   relation was created by a template, and the 
    *   field is a multi-field, and the variable is in
    *   a position other than the first, then unless
    *   this multi-field exists in a prior rule in which
    *   the position of the variable had been supplied a
    *   value the variable will have no field values.
    *   If this is the case, fld2 will be <nil> upon
    *   entering this function.  The fix for this will
    *   be to have a condition which will return FALSE
    *   if fld2 is <nil> and return.
    * ----------------------------------------------------
    */

    IF (fld2 IS_NIL) THEN
       return (FALSE);
    END_IF

   if (fld1->allow_word == YES) 
     type = WORD;
   else if (fld1->allow_string == YES) 
     type = STRING;
   else if (fld1->allow_number == YES) 
     type = NUMBER;
   else
     error_message (WARNING, " INTERNAL ERROR.");

   switch (type) 
     {
       case WORD:
          if ((fld2->allow_string == YES) || (fld2->allow_number == YES) ||
              (fld2->allow_word == NO)) 
             return (FALSE);
          if (fld2->possible_words == NULL) 
             return (FALSE);
          if (fld2->possible_words->next_word != NULL) 
             return (FALSE);
          if (strcmp (fld2->possible_words->word, fld1->possible_words->word) ==
             0) 
             return (TRUE);
          break;
       case STRING:
          if ((fld2->allow_string == NO) || (fld2->allow_number == YES) ||
              (fld2->allow_word == YES)) 
             return (FALSE);
          if (fld2->possible_words == NULL) 
             return (FALSE);
          if (fld2->possible_words->next_word != NULL) 
             return (FALSE);
          if (strcmp (fld2->possible_words->word, fld1->possible_words->word) ==
             0) 
             return (TRUE);
          break;
       case NUMBER:
          if ((fld2->allow_string == YES) || (fld2->allow_number == NO) ||
              (fld2->allow_word == YES)) 
             return (FALSE);
          if (fld2->possible_numbers == NULL) 
             return (FALSE);
          if (fld2->possible_numbers->next_number != NULL) 
             return (FALSE);
          if (fld2->possible_numbers->number == fld1->possible_numbers->number) 
             return (TRUE);
          break;
     }
   return (FALSE);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_rule () 
 *
 *  PURPOSE:  This function checks to see if the name of the
 *            current rule has been previously used. If it is
 *            a new name, then create a structure for the info, 
 *            and stores it in the proper place in the binary
 *            tree. If it an old name, give warning and use old
 *            structure.
 *
 *  INPUTS :  A pointer to the list of rules (RULE_PTR), 
 *            and the name of the current rule (char *).
 *
 *  RETURNS:  A pointer to the top of the rule binary tree.
 *
 *  NOTES  :  This function is called recursively to process
 *            through the tree. It may add a node to the tree
 *            a therefore always returns the tree root. It
 *            also sets the global variable cur_rule so I can
 *            modify the current rule structure.
 * -------------------------------------------------------------
 */

static RULE *find_rule (node, name) 
RULE_PTR  node;
char     *name;
{
   int rtn;

   IF (node EQ NULL) THEN                       /* A new rule! */
      node       = alloc_rule ();
      node->name = name;
      node->file = cur_file_name;
      cur_rule   = node;
   ELSE
      rtn = strcmp (name, node->name);
      IF (rtn < 0) THEN
         node->lft_rule = find_rule (node->lft_rule, name);
      ELSE_IF (rtn > 0) 
         node->rht_rule = find_rule (node->rht_rule, name);
      ELSE
         error_message (WARNING, "Duplicate rule name, erasing old info.");
         cur_rule = node;
      END_IF
   END_IF

   return (node);
}

/* ===================================================================
 * ===========      LHS of Rule Processing Functions     =============
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_rule_LHS () 
 *
 *  PURPOSE:  This function reads and stores all the info which
 *            can be gathered by processing the LHS of a rule.
 *
 *  INPUTS :  A pointer to the current rule structure (RULE_PTR), 
 *            and a pointer to the last token read.
 *
 *  RETURNS:  An integer, OK if rule was processed properly, 
 *            ERROR if there was a recoverable syntax error, and
 *            RESTART if there was a gross syntax error which
 *            prevents me from finding the closing paren.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static int process_rule_LHS (current_rule, old_token) 
int      old_token;
RULE_PTR current_rule;
{
   int token;
   int rtn;

   token = old_token;

   /*==========================*/
   /* Check declare features   */
   /*==========================*/

   IF (token EQ LPAREN) THEN
      token = gettoken (cur_file);

      IF ((token EQ WORD) AND (strcmp (TKNWORD, "declare") EQ 0)) THEN
         token = gettoken (cur_file);

         IF (token NEQ LPAREN) THEN
            error_message (ERROR, "Expected a \' (\' following declare.");
            find_rht_paren ();

         ELSE

            /*============================*/
            /* Check for salience & value */
            /*============================*/

            token = gettoken (cur_file);
            IF ((token EQ WORD) AND (strcmp (TKNWORD, "salience") EQ 0)) THEN
               current_rule->salience_set = YES;
               token = gettoken (cur_file);
               IF (token EQ RPAREN) THEN
                  error_message (ERROR, "Forgot to include salience value!");

               ELSE_IF (token NEQ NUMBER) 
                  current_rule->salience_val = 0;
                  error_message (ERROR, "Expected a number for salience value.");
                  find_rht_paren ();

               ELSE
                  current_rule->salience_val = (int) TKNNUMBER;
                  find_rht_paren ();
               END_IF
               find_rht_paren ();               /* Finish declare  */
            END_IF

         token = gettoken (cur_file);           /* And get next token */
         END_IF

      ELSE
         save_token (token, TKNWORD, TKNNUMBER);
         token     = LPAREN;
         TKNWORD   = " (";
         TKNNUMBER = 0.00;
      END_IF
   ELSE_IF ((token NEQ BWORD) AND (token NEQ SEPARATOR)) 
      error_message (ERROR, "Expected a \' (\' or variable on LHS.");
      return (RESTART);
   END_IF

   /*=====================*/
   /* Process rest of LHS */
   /*=====================*/

   while (token NEQ SEPARATOR) DO
      IF (token EQ LPAREN) THEN
         rtn = rule_find_relation (current_rule, NO, NO);
         IF (rtn EQ RESTART) THEN
            return (RESTART);
         END_IF
      ELSE_IF (token EQ BWORD) 
         rtn = process_LHS_retract (current_rule, NO);
         IF (rtn EQ RESTART) THEN
            return (RESTART);
         END_IF
         rtn = rule_find_relation (current_rule, YES, NO);
         IF (rtn EQ RESTART) THEN
            return (RESTART);
         END_IF
      ELSE
         error_message (ERROR, "Expected a \' (\' or variable on LHS.");
         return (RESTART);
      END_IF
      token = gettoken (cur_file);
   END_WHILE

   return (OK);
}

/* ===================================================================
 * ===========      RHS of Rule Processing Functions     =============
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_rule_RHS () 
 *
 *  PURPOSE:  This function reads and stores all the info which
 *            can be gathered by processing the RHS of a rule.
 *
 *  INPUTS :  A pointer to the current rule structure (RULE_PTR), 
 *            and an integer which specifies whether or not
 *            you are currently in an if statement.
 *
 *  RETURNS:  An integer, OK if rule was processed properly, 
 *            ERROR if there was a recoverable syntax error, and
 *            RESTART if there was a gross syntax error which
 *            prevents me from finding the closing paren.
 *
 *  NOTES  :  This function can be called recursively for each
 *            level of if or while nesting on the RHS of the
 *            rule.
 *            This function really needs to be cleaned up so
 *            that it does a better job of handling errors. The
 *            process_XX functions need to be fixed to return
 *            values which can be checked for RESTART, and
 *            this function also needs to return RESTART.
 * -------------------------------------------------------------
 */

static int process_rule_RHS (current_rule) 
RULE_PTR  current_rule;
{
   int token;

   token = gettoken (cur_file);

   while (token NEQ RPAREN) DO

      /*===========================*/
      /* Check for opening paren   */
      /*===========================*/

      IF (token NEQ LPAREN) THEN
         IF (inside_if ()) THEN
            IF (strcmp (TKNWORD, "else") EQ 0) THEN

               /*===============================*/
               /*  Update the nest-level stack  */
               /*===============================*/

               push (_else_);
            ELSE
               error_message (ERROR, "Invalid entry inside an if.");
            END_IF
         ELSE_IF (token EQ STOP) 
            error_message (ERROR, "Missing closing right paren.");
            return (RESTART);
         ELSE
            error_message (ERROR, "Expecting an \' (\' on RHS of rule.");
         END_IF

      ELSE
         token = gettoken (cur_file);
         current_rule->num_actions++;

         /*===================*/
         /* Check for assert  */
         /*===================*/

         IF (strcmp (TKNWORD, "assert") EQ 0) THEN
            current_rule->num_asserts++;
            (void) process_assert (current_rule, NO);

         /*======================*/
         /* Check for load-facts */
         /*======================*/

         ELSE_IF (strcmp (TKNWORD, "load-facts") EQ 0) 
            current_rule->num_asserts++;
            error_message (WARNING, 
                "load-facts used, assertion lists may be incomplete.");
            (void)find_bound_words (NULL, RHS, NO, NO);

         /*===================*/
         /* Check for retract */
         /*===================*/

         ELSE_IF (strcmp (TKNWORD, "retract") EQ 0) 
            current_rule->num_retracts++;
            (void) process_RHS_retract (current_rule, NO);

         /*======================*/
         /* Check for modify     */
         /*======================*/

         ELSE_IF (strcmp (TKNWORD, "modify") EQ 0) 
            current_rule->num_asserts++;
            current_rule->num_retracts++;
            (void) process_RHS_modify (current_rule);

         /*======================*/
         /* Check for if         */
         /*======================*/

         ELSE_IF (strcmp (TKNWORD, "if") EQ 0) 
            current_rule->num_ifs++;
            current_rule->num_actions--;
            (void)process_if (current_rule);

         /*======================*/
         /* Check for while      */
         /*======================*/

         ELSE_IF (strcmp (TKNWORD, "while") EQ 0) 
            current_rule->num_whiles++;
            current_rule->num_actions--;
            (void)process_while (current_rule);

         /*=========================*/
         /* Check for string assert */
         /*=========================*/

         ELSE_IF ((strcmp (TKNWORD, "string_assert") EQ 0) OR
                 (strcmp (TKNWORD, "str_assert") EQ 0)) 
            current_rule->num_asserts++;
            error_message (WARNING, 
                 "str_assert used, assertion lists may be incomplete.");
            (void)find_bound_words (NULL, RHS, NO, NO);

         /*==============================*/
         /* Check for external functions */
         /*==============================*/

         ELSE_IF ((token EQ WORD) AND (check_ex_funcs (TKNWORD) IS_NO)) 
            current_rule->num_ex_funcs++;
            (void)process_ex_funcs (NULL, current_rule, TKNWORD, RHS);

         /*================*/
         /* Check for bind */
         /*================*/

         ELSE_IF (strcmp (TKNWORD, "bind") EQ 0) 
            current_rule->num_actions++;
            (void)process_bind (current_rule);

         ELSE
            (void)find_bound_words (NULL, RHS, NO, NO);
         END_IF
      END_IF
      token = gettoken (cur_file);
   END_WHILE

   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_assert () 
 *
 *  PURPOSE:  This function is called everytime an assert
 *            is found on the RHS of a rule. It stores the
 *            appropriate info about the relation (s) being
 *            asserted and handles any variables found.
 *
 *  INPUTS :  A pointer to the current rule (RULE_PTR). A flag
 *            indicating if the retract is an explicit statement
 *            or implied by a modify statement.
 *
 *  RETURNS:  An integer, OK if the assert was processed without
 *            problems, ERROR if a syntax error is found.
 * ------------------------------------------------------------
 */

static int process_assert (current_rule, implied) 
RULE_PTR current_rule;
int implied;
{
   char     *cur_rel_name;
   RR_PTR    temp_rr;
   int       rtn;
   int       token;
   char     *statement = implied IS_YES ? "a modify" : "an assert";
   char     *action = implied IS_YES ? "modified" : "asserted";

   token = gettoken (cur_file);

   IF (token NEQ LPAREN) THEN
      sprintf (msg_buf, "At least one fact must be %s.", action);
      error_message (ERROR, msg_buf);

      find_rht_paren ();

      return (ERROR);
   END_IF

   LOOP
      rtn = find_simple_relation (ASSERT);

      IF (rtn NEQ ERROR) THEN
         cur_rel_name  = TKNWORD;

         rel_head = find_create_rel (rel_head, cur_rel_name);

         IF (find_def_tmp (def_tmp_head, cur_rel_name)) THEN
            templ_pattern_to_rel_pattern (cur_rel_name, YES,NULL);
         END_IF

         temp_rr              = alloc_rr ();
         temp_rr->rule        = current_rule;
         temp_rr->next_rr     = cur_rel->assert_list;
         temp_rr->num_fields  = find_fields (cur_rel, NULL, RHS, NO, NULL, NO);
         cur_rel->assert_list = temp_rr;

         IF (CHECK_DEFRELS IS_ON) THEN
            check_min_num_fields (cur_rel->name, temp_rr->num_fields, YES);
         END_IF
      END_IF
   UNTIL (implied OR (token = gettoken (cur_file)) NEQ LPAREN);

   IF (!implied AND token NEQ RPAREN) THEN
      sprintf ("Expected a \') \' to close %s.", statement);
      error_message (ERROR, msg_buf);
      return (ERROR);
   END_IF

   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_if () 
 *
 *  PURPOSE:  This function is called everytime an if
 *            is found on the RHS of a rule. It stores the
 *            appropriate info and handles any variables found.
 *
 *  INPUTS :  A pointer to the current rule (RULE *).
 *
 *  RETURNS:  An integer, OK if the if was processed without
 *            problems, ERROR if a syntax error is found.
 *
 *  NOTES  :  Recursively calls process_rule_RHS () 
 * -------------------------------------------------------------
 */

static int process_if (current_rule) 
RULE_PTR current_rule;
{
   int  token;
   int  rtn;
   int  nest_level;

   /*===============================*/
   /*  Update the nest-level stack  */
   /*===============================*/

   push (_if_);
   IF ((nest_level = num_nest_levels (_if_)) > 2) THEN
      sprintf (msg_buf, "%d levels of nested if's.", nest_level);
      error_message (WARNING, msg_buf);
   END_IF

   /*===================*/
   /*  Get a new token  */
   /*===================*/

   token = gettoken (cur_file);

   /*====================*/
   /*  Error if not a (  */
   /*====================*/

   IF (token NEQ LPAREN) THEN
      error_message (ERROR, "Expected a \' (\' after an if.");
      find_rht_paren ();
      return (ERROR);

   /*=================================*/
   /*  Otherwise, process if actions  */
   /*=================================*/

   ELSE
      /*============================*/
      /*  Process the function call */
      /*============================*/

      token = gettoken (cur_file);
      (void)find_bound_words (NULL, RHS, NO, NO);

      /*=================*/
      /*  Find the then  */
      /*=================*/

      token = gettoken (cur_file);
      IF ((token EQ WORD) AND (strcmp (TKNWORD, "then") EQ 0)) THEN
         rtn = process_rule_RHS (current_rule);

         /*===============================*/
         /*  Update the nest-level stack  */
         /*===============================*/

         pop ();
         return (rtn);
      ELSE
         error_message (ERROR, "Expected a then after an if.");
         find_rht_paren ();
      END_IF
   END_IF

   pop ();                          /* It should never get here */
   return (ERROR);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_while () 
 *
 *  PURPOSE:  This function is called everytime a while
 *            is found on the RHS of a rule. It stores the
 *            appropriate info and handles any variables found.
 *
 *  INPUTS :  A pointer to the current rule (RULEPTR).
 *
 *  RETURNS:  An integer, OK if the while was processed without
 *            problems, ERROR if a syntax error is found.
 *
 *  NOTES  :  Recursively call process_rule_RHS
 * -------------------------------------------------------------
 */

static int process_while (current_rule) 
RULE_PTR current_rule;
{
   int  token;
   int  rtn;
   int  nest_level;

   /*===============================*/
   /*  Update the nest-level stack  */
   /*===============================*/

   push (_while_);
   IF ((nest_level = num_nest_levels (_while_)) > 1) THEN
      sprintf (msg_buf, "%d levels of nested while's.", nest_level);
      error_message (WARNING, msg_buf);
   END_IF

   /*===================*/
   /*  Get a new token  */
   /*===================*/

   token = gettoken (cur_file);

   /*====================*/
   /*  Error if not a (  */
   /*====================*/

   IF (token NEQ LPAREN) THEN
      error_message (ERROR, "Expecting \' (\' after a while.");
      find_rht_paren ();
      return (ERROR);

   /*====================================*/
   /*  Otherwise, process while actions  */
   /*====================================*/

   ELSE
      /*=============================*/
      /* Process comparison function */
      /*=============================*/

      token = gettoken (cur_file);
      (void)find_bound_words (NULL, RHS, NO, NO);

      /*=================*/
      /*  Optional 'do'  */
      /*=================*/

      token = gettoken (cur_file);
      IF ( NOT ((token EQ WORD) AND (strcmp (TKNWORD, "do") EQ 0)) ) THEN
         save_token (token, TKNWORD, TKNNUMBER);
      END_IF

      rtn = process_rule_RHS (current_rule);

      /*===============================*/
      /*  Update the nest-level stack  */
      /*===============================*/

      pop ();
      return (rtn);
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_bind () 
 *
 *  PURPOSE:  This function is called every time a bind
 *            is found on the RHS of a rule. It stores the
 *            appropriate info and handles any variables found.
 *
 *  INPUTS :  A pointer to the current rule (RULE *).
 *
 *  RETURNS:  An integer, OK if the bind was processed without
 *            problems, ERROR if a syntax error is found.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

/*ARGSUSED*/ /* Make lint happy */
static int process_bind (current_rule) 
RULE_PTR current_rule;
{
   int token;
	
   /*===========================================*/
   /*  First get binding variable and store it  */
   /*===========================================*/

   token = gettoken (cur_file);

   IF (token NEQ BWORD) THEN
      error_message (ERROR, "Must use a variable inside a bind operation.");
      find_rht_paren ();
      return (ERROR);
   END_IF

   (void)process_LHS_variable (NULL, NULL, BWORD, 0, YES, NO, 0, NO);

   /*==========================================*/
   /*  Then process any remaining bound words  */
   /*==========================================*/

   token = gettoken (cur_file);

   IF (token EQ LPAREN) THEN
      token = gettoken (cur_file);
      (void)find_bound_words (NULL, RHS, NO, NO);
      find_rht_paren ();
   ELSE
      (void)find_bound_words (NULL, RHS, NO, NO);
   END_IF
   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_ex_funcs () 
 *
 *  PURPOSE:  This function is called everytime an external
 *            function is found in a rule, both RHS and LHS.
 *
 *  INPUTS :  Pointer to token_list;
 *            A pointer to the current rule (RULE *), 
 *            the name of the function (char *), and
 *            the side of the rule where it is called (int).
 *
 *  RETURNS:  An integer, currently always returns OK.
 * -------------------------------------------------------------
 */

static FIELD_PTR process_ex_funcs (arg_list, current_rule, func_name, side_of_rule) 
FIELD_PTR arg_list;
RULE *current_rule;
char *func_name;
int   side_of_rule;
{
   RR_PTR temp_rr;
   int num_args;

   ex_funcs_head = find_create_ex_func (ex_funcs_head, func_name);

   temp_rr        = alloc_rr ();
   temp_rr->rule  = current_rule;

   IF (side_of_rule EQ LHS) THEN
      temp_rr->next_rr           = cur_ex_func->LHS_rule_list;
      cur_ex_func->LHS_rule_list = temp_rr;
   ELSE
      temp_rr->next_rr           = cur_ex_func->RHS_rule_list;
      cur_ex_func->RHS_rule_list = temp_rr;
   END_IF
   arg_list = find_args (&num_args, arg_list, cur_ex_func, side_of_rule);
   temp_rr->num_fields = num_args;
   return (arg_list);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_create_ex_funcs () 
 *
 *  PURPOSE:  Works similarly to find_create_rel (), this function
 *            updates the binary tree to hold a new external
 *            function or finds its position in the tree if it
 *            is NOT a new function.
 *
 *  INPUTS :  A pointer to the head node (initially) (EXF_PTR), and
 *            the name of the function (char *).
 *
 *  RETURNS:  A pointer to the head node.
 *
 *  NOTES  :  Recursive, sets a global pointer (current_ex_func) to
 *            point to the node containing the current function
 *            when it is either found or created.
 * -------------------------------------------------------------
 */

static EXF_PTR find_create_ex_func (node, func_name) 
   EXF_PTR  node;
   char    *func_name;
{
   int rtn;

   IF (node EQ NULL) THEN        /* A new external function! */
      node        = alloc_ex_func ();
      node->name  = func_name;
      cur_ex_func = node;
   ELSE
      rtn = strcmp (func_name, node->name);
      IF (rtn < 0) THEN
         node->lft_ex_func = find_create_ex_func (node->lft_ex_func, func_name);
      ELSE_IF (rtn > 0) 
         node->rht_ex_func = find_create_ex_func (node->rht_ex_func, func_name);
      ELSE
         cur_ex_func   = node;
      END_IF
   END_IF
   return (node);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_args () 
 *
 *  PURPOSE:  Corresponds to find_arguments () for relations, this
 *            function processes the arguments of an external
 *            function call.
 *
 *  INPUTS :  A pointer to the current external function (EXF_PTR), 
 *            and the side of the rule where the function was found (int).
 *
 *  RETURNS:  The number of arguments.
 *
 *  NOTES  :  The number of arguments is counted differently than
 *            the number of fields in a relation, because the
 *            relation name is counted as a field, but the function
 *            name is NOT counted as an argument.
 * -------------------------------------------------------------
 */

static FIELD_PTR find_args (num_args, arg_list, current_ex_func, side_of_rule) 
FIELD_PTR arg_list;
EXF_PTR current_ex_func;
int     side_of_rule;
int    *num_args;
{
   int token;
   DE_PTR def_ex_func;
   FIELD_PTR temp_arg;

   /*======================*/
   /* Initialize variables */
   /*======================*/

   temp_arg    = arg_list;
   
   if ((arg_list != NULL) && (temp_arg != NULL)) 
    {
       TKNWORD = temp_arg->word;
       token   = temp_arg->num;
       if (token == NUMBER) 
         TKNNUMBER = atof (TKNWORD);
       temp_arg = temp_arg->next;
    }
   else
     token       = gettoken (cur_file);
   *num_args     = 0;

   while (token NEQ RPAREN) DO
      switch (token) DO
         case SINGLE:
         case MULTIPLE:
            (*num_args) ++;
            error_message (ERROR, "Wildcard used in an inappropriate place.");
            break;

         case BWORD:
            (*num_args) ++;
            IF (side_of_rule EQ LHS) THEN
               (void)process_LHS_variable (NULL, current_ex_func, BWORD, *num_args, 
                  NO, NO, FUNCTION, NO);
            ELSE_IF (side_of_rule EQ RHS) 
               (void)process_RHS_variable (BWORD, *num_args, NO, FUNCTION);
            END_IF
            break;

         case BWORDS:
            (*num_args) ++;
            IF (side_of_rule EQ LHS) THEN
               (void)process_LHS_variable (NULL, current_ex_func, BWORDS, 0, NO, NO, 
                        FUNCTION, NO);
            ELSE_IF (side_of_rule EQ RHS) 
               (void)process_RHS_variable (BWORDS, 0, NO, FUNCTION);
            END_IF
            break;

         case LPAREN:
		if ((arg_list != NULL) && (temp_arg != NULL)) 
		 {
       		  TKNWORD = temp_arg->word;
        	  token   = temp_arg->num;
        	  if (token == NUMBER) 
                  TKNNUMBER = atof (TKNWORD);
       		  temp_arg = temp_arg->next;
    		 }
   		else
                 token = gettoken (cur_file);
               IF ((token EQ WORD) AND (check_ex_funcs (TKNWORD) IS_NO)) THEN
                  cur_rule->num_ex_funcs++;
                  temp_arg = process_ex_funcs (temp_arg, cur_rule, TKNWORD, LHS);
               ELSE
                  temp_arg = find_bound_words (temp_arg, RHS, NO, NO);
               END_IF
               (*num_args) ++;
               break;
  
         case WORD:
         case STRING:
         case NUMBER:
            (*num_args) ++;
            (void)process_args (token, TKNWORD, *num_args, current_ex_func);
            break;

         default:
            error_message (ERROR, "Unknown symbol found!");
            break;
      DONE
      if ((arg_list != NULL) && (temp_arg != NULL)) 
       {
        TKNWORD = temp_arg->word;
        token   = temp_arg->num;
        if (token == NUMBER) 
         TKNNUMBER = atof (TKNWORD);
        temp_arg = temp_arg->next;
       }
      else
        token       = gettoken (cur_file);
   END_WHILE

   IF (CHECK_DEFRELS IS_ON) THEN
      def_ex_func = find_def_ex_func (def_ext_head, current_ex_func->name);
      IF (def_ex_func NEQ NULL) THEN
         IF ((*num_args < def_ex_func->min_arguments) OR
            (*num_args > def_ex_func->max_arguments)) THEN
            error_message (ERROR, "Number of arguments mismatch\n");
            sprintf (msg_buf, "        -- expected between %d and %d --\n", 
                   def_ex_func->min_arguments, def_ex_func->max_arguments);
            send_text (msg_buf, 8);
         END_IF
      ELSE
         sprintf (msg_buf, 
           "External function <%.20s> used prior to its definition.", 
               current_ex_func->name);
         error_message (WARNING, msg_buf);
      END_IF
   END_IF
   
   return (temp_arg);
}

/* ===================================================================
 *                       Process variables
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_LHS_retract () 
 *
 *  PURPOSE:  This function builds a list of variables that
 *            are bound on the LHS to a fact.
 *
 *  INPUTS :  A pointer to the current rule (RULE *).
 *
 *  RETURNS:  An integer, OK if everything is kosher, else
 *            it returns ERROR.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

/*ARGSUSED*/ /* Make lint happy */
static int process_LHS_retract (current_rule, inside_or) 
RULE_PTR current_rule;
int      inside_or;
{
   int      token;
   VAR_PTR  temp_rl;
   char     *store_wrd;

   store_wrd = TKNWORD;

   /*=========================================*/
   /*  Make sure this really is a retraction! */
   /*=========================================*/

   token = gettoken (cur_file);
   IF (token NEQ BINDER) THEN
      return (ERROR);
   END_IF

   token = gettoken (cur_file);
   IF (token NEQ LPAREN) THEN
      return (ERROR);
   END_IF

   /*===============================================*/
   /*  Syntactically correct retraction, now make   */
   /*  sure variable isn't bound to a regular value */
   /*===============================================*/

   IF ((temp_rl = find_variable (store_wrd, variable_head)) NEQ NULL) THEN
      sprintf (msg_buf, "Variable <%.50s> bound to both a fact and a value.", 
         store_wrd);
      error_message (ERROR, msg_buf);

   ELSE_IF ((temp_rl = find_variable (store_wrd, retract_head)) NEQ NULL) 
      IF (NOT inside_or) THEN
         sprintf (msg_buf, "Variable <%.50s> bound to more than one fact.", 
            store_wrd);
         error_message (ERROR, msg_buf);
      END_IF
   ELSE

      /*==============================*/
      /*  Put Variable name on stack  */
      /*==============================*/

      temp_rl                  = alloc_var ();
      temp_rl->var             = store_wrd;
      temp_rl->level_retracted = NOT_RETRACTED;

      /*===================================*/
      /*  Set values in new retract object */
      /*===================================*/

      temp_rl->next_variable      = retract_head;
      temp_rl->prev_variable      = NULL;

      IF (retract_head NEQ NULL) THEN
         retract_head->prev_variable = temp_rl;
      END_IF

      retract_head                = temp_rl;

   END_IF

   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_RHS_retract () 
 *
 *  PURPOSE:  This function is called when a retract/modify
 *            statement is found on the RHS of a rule. It checks
 *            all the variables and gives any necessary warnings.
 *
 *  INPUTS :  A pointer to the current rule (RULE *). A flag
 *            indicating if the retract is an explicit statement
 *            or implied by a modify statement.
 *
 *  RETURNS:  An integer, OK if the statement was processed without
 *            problems, ERROR if a syntax error is found.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static int process_RHS_retract (current_rule, implied) 
RULE_PTR current_rule;
int implied;
{
   int      rtn = OK;
   int      token;
   VAR_PTR  temp_rl;
   RR_PTR   temp_rr;
   int      prev_retract;
   char*    Statement = implied IS_YES ? "Modify" : "Retract";
   char*    statement = implied IS_YES ? "modify" : "retract";
   char*    valid_retract_spec = implied IS_YES ? "inside modify" :
      "or number inside retract";

   IF (inside_while () IS_YES) THEN
      IF (inside_if () IS_YES) THEN
         sprintf (msg_buf, "%s inside both a while and an if.", Statement);
         error_message (WARNING, msg_buf);
      ELSE
         sprintf (msg_buf, "%s inside a while", Statement);
         error_message (ERROR, msg_buf);
      END_IF;
   END_IF

   token = gettoken (cur_file);

   while (token NEQ RPAREN) DO

      IF (token EQ NUMBER) THEN
         IF (implied) THEN
            rtn = ERROR;
            error_message (ERROR, 
               "Modifying a fact specified by its index number.");
         ELSE
            error_message (WARNING, 
                  "Retracting a fact by using the index number.");
         END_IF
      ELSE_IF (token NEQ BWORD) 
         rtn = ERROR;
         sprintf (msg_buf, "Expecting a variable %s.", valid_retract_spec);
         error_message (ERROR, msg_buf);
      ELSE
         temp_rl = find_variable (TKNWORD, retract_head);


         /*===================================================*/
         /*  If retraction variable is found, check relation  */
         /*===================================================*/

         IF (temp_rl NEQ NULL) THEN

            /*========================================================*/
            /*  Make sure that no invalid multiple retractions occur  */
            /*========================================================*/

            IF (temp_rl->level_retracted EQ NOT_RETRACTED) THEN
               prev_retract = NOT_RETRACTED;
            ELSE

               /*==================================================*/
               /*  If multiple retraction, classify it by testing  */
               /*  nested-if levels.                               */
               /*==================================================*/

               prev_retract = describe_previous (temp_rl);
            END_IF

            SWITCH (prev_retract) DO

               /*====================*/
               /*  ERROR Conditions  */
               /*====================*/

               case OUTSIDE_IF:
               case SUPERLEVEL:
               case SAME:

                  sprintf (msg_buf, 
                     "Multiple retraction/modication of variable %.40s.", 
                     TKNWORD);
                  error_message (ERROR, msg_buf);
               break;

               /*======================*/
               /*  WARNING Conditions  */
               /*======================*/

               case SUBLEVEL:
               case SEPARATE:
               case COMPANION_IF:

                  IF (prev_retract EQ COMPANION_IF) THEN
                     sprintf (msg_buf, 
                        "Variable <%.40s> %s", 
                        TKNWORD, "retracted/modified in both if and else.");
                  ELSE
                     sprintf (msg_buf, 
                        "%s of variable %.40s", 
                        "Possible multiple retraction/modication.", TKNWORD);
                  END_IF
                  error_message (WARNING, msg_buf);

                  /* the break was intentionally left off here */

               /*=================*/
               /*  OK Conditions  */
               /*=================*/

               case NOT_RETRACTED:
               case OK_RETRACT:

                  /*==================================================*/
                  /*  If relation was found, store rule name in list  */
                  /*==================================================*/

                  IF (temp_rl->relation NEQ NULL) THEN
                     temp_rr              = alloc_rr ();
                     temp_rr->rule        = current_rule;
                     temp_rr->next_rr     = temp_rl->relation->retract_list;
                     temp_rl->relation->retract_list = temp_rr;
                  END_IF

                  /*========================================*/
                  /*  And mark retract object as retracted  */
                  /*========================================*/

                  mark_retraction (temp_rl);
               break;

               default:
               break;
            END_SWITCH

         /*===================================================*/
         /*  Error if variable wasn't found in retract list  */
         /*===================================================*/

         ELSE
            sprintf (msg_buf, "Attempted to %s unbound fact.", statement);
            error_message (ERROR, msg_buf);
         END_IF

      END_IF

      token = gettoken (cur_file);
   END_WHILE

   return (rtn);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_RHS_modify () 
 *
 *  PURPOSE:  This function is called when a modify statement
 *            is found on the RHS of a rule. It checks all the
 *            variables and gives any necessary warnings.
 *
 *  INPUTS :  A pointer to the current rule (RULE *).
 *
 *  RETURNS:  An integer, OK if the statement was processed without
 *            problems, ERROR if a syntax error is found.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static int process_RHS_modify (current_rule) 
   RULE_PTR current_rule;
{
   TK_PTR tknstruct = gettoken_ptr (cur_file);
   char *variable_name = tknstruct->tknword;
   TK_PTR right_paren = alloc_token ();
   int rtn;

   /*=====================================*/
   /* Construct a right parenthesis token */
   /*=====================================*/

   right_paren->token = RPAREN;
   right_paren->tknword = ") ";

   /*===================================================================*/
   /* Place the retraction spec. followed by the right parenthesis back */
   /* onto the input queue.                                             */
   /*===================================================================*/

   ungettoken_ptr (tknstruct);
   ungettoken_ptr (right_paren);

   /*==============================*/
   /* Process the retraction spec. */
   /*==============================*/

   IF ((rtn = process_RHS_retract (current_rule, YES)) IS_OK) THEN
      /*===================================================*/
      /* Convert the retraction spec. into a template name */
      /*===================================================*/

      VAR_PTR retraction_spec = find_variable (variable_name, retract_head);
      char *template_name = retraction_spec->relation->name;

      IF (find_def_tmp (def_tmp_head, template_name)) THEN
         /*===============================================================*/
         /* Construct tokens for the template name and a left parenthesis */
         /*===============================================================*/

         TK_PTR left_paren = alloc_token ();
         TK_PTR template = alloc_token ();

         left_paren->token = LPAREN;
         left_paren->tknword = " (";

         template->tknword = template_name;
         template->token = WORD;
         template->hashword = add_symbol (template_name);
         template->print_rep = template_name;

         /*=============================================================*/
         /* Place left parenthesis and template name on the input queue */
         /*=============================================================*/

         ungettoken_ptr (left_paren);
         ungettoken_ptr (template);

         /*==========================================*/
         /* Process the rest of the modify statement */
         /*==========================================*/

         rtn = process_assert (current_rule, YES);
      ELSE
         /*=================================================*/
         /* Process the modification of a standard relation */
         /*=================================================*/

         rtn = ERROR;
         error_message (ERROR, 
            "The modify statement can not modify relations.");
      END_IF
   ELSE
      /*===========================*/
      /* Skip the modify statement */
      /*===========================*/

      find_rht_paren ();
   END_IF

   return (rtn);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_variable () 
 *
 *  PURPOSE:  This function will search a variable list for
 *            a specific variable name.
 *
 *  INPUTS :  A pointer to the variable name to search for
 *            (char *), and a pointer to the variable list.
 *
 *  RETURNS:  If the variable name is found in the list, it
 *            returns a pointer to that list node (VAR_PTR), 
 *            otherwise, it returns NULL.
 *
 *  NOTES  :  Used for both retract variables and bound
 *            variable lists.
 * -------------------------------------------------------------
 */

static VAR_PTR find_variable (word, list) 
char    *word;
VAR_PTR   list;
{
   VAR_PTR temp;

   temp = list;
   while (temp NEQ NULL) DO
      IF (strcmp (word, temp->var) EQ 0) THEN
         return (temp);
      END_IF

      temp = temp->next_variable;
   END_WHILE

   return (NULL);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_variable () 
 *
 *  PURPOSE:  This function is called after all other rule
 *            processing has been completed and scans through
 *            both the retract variable list and the bound
 *            variable list for problems.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Uses global variables for both variable lists.
 * -------------------------------------------------------------
 */

static void check_variables () 
{
   VAR_PTR  t1, t2;

   /*=====================================*/
   /* Check for bound facts not retracted */
   /*=====================================*/

   t1 = retract_head;
   while (t1 NEQ NULL) DO
      IF (t1->level_retracted EQ NOT_RETRACTED) THEN
         sprintf (msg_buf, "Fact bound to <%.40s>, but not retracted.", t1->var);
         error_message (WARNING, msg_buf);
      END_IF
      t2 = t1->next_variable;
      gen_free ((char *) t1, sizeof (VARIABLE_LIST));
      t1 = t2;
   END_WHILE

   retract_head  = NULL;

   /*============================*/
   /* Clean up regular variables */
   /*============================*/

   t1 = variable_head;
   while (t1 NEQ NULL) DO
      IF (t1->count <= 1) THEN
         sprintf (msg_buf, "Variable <%.40s> bound, but not used.", t1->var);
         error_message (WARNING, msg_buf);
      END_IF
      t2 = t1->next_variable;
      free_def_fld (t1->field_info);
      gen_free ((char *) t1, sizeof (VARIABLE_LIST));
      t1 = t2;
   END_WHILE

   variable_head = NULL;
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_LHS_variable () 
 *
 *  PURPOSE:  This function is called when a bound word is
 *            found anywhere on the LHS of a rule. It stores
 *            all the appropriate information about that
 *            variable.
 *
 *  INPUTS :  Four arguments, 
 *             A pointer to the current relation (REL_PTR), 
 *             A flag describing the variable type, either a
 *               BWORD or BWORDS (int), 
 *             A number telling the field position (if any) 
 *               in which the variable was found (int), 
 *             A flag telling if variable was found inside a
 *               bind operation (YES or NO) (int), and
 *             A flag telling if variable was found inside a
 *               not pattern (YES or NO) (int).
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Certain operations on the RHS of a rule also call
 *            this, such as bind, since they can create new
 *            variables.
 * -------------------------------------------------------------
 */

static void process_LHS_variable (current_rel, current_ex_func, type, field_pos, 
                                 in_bind, in_not, code, not_flag) 
int     type, field_pos, in_bind, in_not, code;
short   not_flag;
REL_PTR current_rel;
EX_FUNC *current_ex_func;
{
   VAR_PTR   temp_rl;

   /*============================================*/
   /*  Make sure variable isn't bound to a fact  */
   /*============================================*/

   IF ((temp_rl = find_variable (TKNWORD, retract_head)) NEQ NULL) THEN
      sprintf (msg_buf, "Variable <%.40s> bound to both a fact and a value.", 
         TKNWORD);
      error_message (ERROR, msg_buf);
   ELSE

      /*==========================================*/
      /*  Check if variable has already been used */
      /*==========================================*/

      temp_rl = find_variable (TKNWORD, variable_head);

      IF (temp_rl EQ NULL) THEN
        if (not_flag) 
          {
           sprintf (msg_buf, 
           "Variable <%.40s> used before being bound.", TKNWORD);
           error_message (ERROR, msg_buf);
           return;
          }
         /*===================================*/
         /*  New Variable, put name on stack  */
         /*===================================*/

         temp_rl       = alloc_var ();
         temp_rl->var  = TKNWORD;

         /*====================================*/
         /*  Set values in new variable object */
         /*====================================*/

         temp_rl->next_variable   = variable_head;
         temp_rl->prev_variable   = NULL;
         if (code == REL) 
           temp_rl->relation      = current_rel;
         else
           temp_rl->ex_func       = current_ex_func;
         temp_rl->type            = type;
         temp_rl->count++;

         IF ((CHECK_DEFRELS IS_ON) AND (type NEQ MULTIPLE) AND
             (field_pos > 0)) THEN
            IF (code == REL) THEN
               temp_rl->field_info = copy_field_info (current_rel->name, field_pos);
            ELSE_IF (code == FUNCTION) 
               temp_rl->field_info = copy_argument_info (current_ex_func->name, 
                                                        field_pos);
            END_IF
         END_IF

         IF (in_not > 0) THEN
            temp_rl->bound_in_not = in_not;
         ELSE
            temp_rl->bound_in_not = NO;
         END_IF

         IF (variable_head NEQ NULL) THEN
            variable_head->prev_variable = temp_rl;
         END_IF

         variable_head = temp_rl;
      ELSE /* temp_rl != NULL */
         if ((not_flag) && (CHECK_DEFRELS IS_ON) && (field_pos > 0)) 
           {
             temp_rl->count += 1;
             store_not_variable (current_rel->name, temp_rl->var, field_pos);
             return;
           }

         /*==========================================================*/
         /*  Variable previously used, check whether single or multi */
         /*==========================================================*/

         IF (type NEQ temp_rl->type) THEN
            sprintf (msg_buf, 
               "Variable <%.40s> bound to both single and multiple fields.", 
               temp_rl->var);
            error_message (ERROR, msg_buf);
         END_IF

         /*==============================*/
         /* Check defrelation field info */
         /*==============================*/

         IF ((CHECK_DEFRELS IS_ON) AND (type NEQ MULTIPLE) AND
                 (field_pos > 0)) THEN
            IF (code EQ REL) THEN
               IF (temp_rl->field_info EQ NULL) THEN
                  temp_rl->field_info = copy_field_info (current_rel->name, 
                     field_pos);
               ELSE
                  check_var_field_info (LHS, current_rel->name, temp_rl->var, 
                                       temp_rl->field_info, field_pos);
               END_IF
            ELSE_IF (code EQ FUNCTION) 
               IF (temp_rl->field_info EQ NULL) THEN
                  temp_rl->field_info = copy_argument_info (current_ex_func->name, 
                     field_pos);
               ELSE
                  check_var_argument_info (current_ex_func->name, temp_rl->var, 
                                          temp_rl->field_info, field_pos, LHS);
               END_IF
            END_IF
         END_IF

         /*=============================================*/
         /*  Check if it was originally bound in a not  */
         /*=============================================*/

         IF (temp_rl->bound_in_not > 0) THEN
            IF (in_not EQ NO) THEN
	       sprintf(msg_buf,
                   "Variable <%s> : Can only use variables bound in a not inside that not.",temp_rl->var);
               error_message (WARNING, msg_buf);
            ELSE_IF (cur_rule->num_nots NEQ temp_rl->bound_in_not) 
               sprintf(msg_buf,
               " Variable <%s> bound inside a not is not constrained by\n        the previous NOT pattern(s) ",
			temp_rl->var);
               error_message(WARNING,msg_buf);
           END_IF
         END_IF

         /*=============================================*/
         /*  Don't increment counter if used in a bind  */
         /*=============================================*/

         IF (in_bind NEQ YES) THEN
            temp_rl->count++;
         END_IF
      END_IF
   END_IF
}

/* ==================================================================== */
/*   Store the information of the negative variable for latter checking */
/* ==================================================================== */
void store_not_variable (rel_name, var_name, position) 
char *rel_name, *var_name;
int position;

{
  DEF_REL *def_rel;
  DEF_FLD *fld;

  def_rel = find_def_rel (def_rel_head, rel_name);
  if (def_rel == NULL) 
   {
      def_rel_head = find_create_def_rel (def_rel_head, rel_name);
      sprintf (msg_buf, 
         "Relation <%.40s> was referenced before it is defined.", rel_name);
      error_message (ERROR, msg_buf);
      return;
   }
  fld = find_def_fld (position, def_rel->fields);
  if (fld == NULL) 
    {
       sprintf (msg_buf, "field %d is not defined.", position);
       error_message (WARNING, msg_buf);
       return;
    }

  if ((fld->allow_word == YES) && (fld->allow_string == NO) &&
  	  (fld->allow_number == NO)) 
  {
     if ((fld->possible_words != NULL) &&
         (fld->possible_words->next_word == NULL)) 
      {
        link_to_not_variable_list (cur_rel, var_name, position);
      }
  }

  if ((fld->allow_word == NO) && (fld->allow_string == YES) &&
  	  (fld->allow_number == NO)) 
  {
     if ((fld->possible_strings != NULL) &&
         (fld->possible_strings->next_word == NULL)) 
      {
        link_to_not_variable_list (cur_rel, var_name, position);
      }
  }
  if ((fld->allow_word == NO) && (fld->allow_string == NO) &&
  	  (fld->allow_number == YES)) 
  {
     if ((fld->possible_numbers != NULL) &&
         (fld->possible_numbers->next_number == NULL)) 
      {
        link_to_not_variable_list (cur_rel, var_name, position);
      }
  }
}
/* ======================================================== *
 *
 * ======================================================== *
 */

void link_to_not_variable_list (current_rel, var_name, position) 
REL_PTR current_rel;
char  *var_name;
int position;

{
   VAR_PTR temp_var;
   

        temp_var = alloc_var ();

        temp_var->var = gen_alloc ((unsigned)strlen (var_name)+1,
                        "link_to_not_variable_list");

        (void)strcpy (temp_var->var, var_name);

        temp_var->next_variable = not_variable_head;
        temp_var->prev_variable = NULL;
        temp_var->relation = current_rel;
        temp_var->field_info = copy_field_info (current_rel->name, position);
        if (not_variable_head != NULL) 
          not_variable_head->prev_variable = temp_var;
        not_variable_head = temp_var;

}
/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_RHS_variable () 
 *
 *  PURPOSE:  This function is called when a bound word is
 *            found anywhere on the RHS of a rule. It stores
 *            all the appropriate information about that
 *            variable.
 *
 *  INPUTS :  Three arguments, 
 *             A flag describing the variable type, either a
 *               BWORD or BWORDS (int), 
 *             A number indicating the field position (if any) 
 *               in which the variable was found (int), 
 *             A flag telling if variable was found inside a
 *               not pattern (YES or NO) (int).
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static void process_RHS_variable (type, field_pos, in_not, code) 
int     type, field_pos, in_not, code;
{
   VAR_PTR   temp_rl;

   /*============================================*/
   /*  Make sure variable isn't bound to a fact  */
   /*============================================*/

   IF ((temp_rl = find_variable (TKNWORD, retract_head)) NEQ NULL) THEN
      sprintf (msg_buf, "Variable <%.40s> bound to both a fact and a value.", 
         TKNWORD);
      error_message (ERROR, msg_buf);
   ELSE

      /*==========================================*/
      /*  Check if variable has already been used */
      /*==========================================*/

      temp_rl = find_variable (TKNWORD, variable_head);

      IF (temp_rl EQ NULL) THEN

         /*==================================*/
         /*  New Variable, probably unbound  */
         /*==================================*/

         sprintf (msg_buf, "Variable <%.40s> used before being bound!", TKNWORD);
         error_message (ERROR, msg_buf);
      ELSE

         /*==================================================*/
         /*  Variable previously bound, check type and count  */
         /*==================================================*/

         IF (type NEQ temp_rl->type) THEN
            sprintf (msg_buf, 
               "Variable <%.40s> bound to both single and multiple fields.", 
               temp_rl->var);
            error_message (ERROR, msg_buf);
         END_IF

         /*==============================*/
         /* Check defrelation field info */
         /*==============================*/

         IF ((CHECK_DEFRELS IS_ON) AND (type NEQ MULTIPLE) AND
                 (field_pos > 0)) THEN
            IF (code EQ REL) THEN
               IF (temp_rl->field_info EQ NULL) THEN
                  temp_rl->field_info = copy_field_info (cur_rel->name, 
                     field_pos);
               ELSE
                  check_var_field_info (RHS, cur_rel->name, temp_rl->var, 
                                       temp_rl->field_info, field_pos);
               END_IF
            ELSE_IF (code EQ FUNCTION) 
               IF (temp_rl->field_info EQ NULL) THEN
                  temp_rl->field_info = copy_argument_info (cur_ex_func->name, 
                     field_pos);
               ELSE
                  check_var_argument_info (cur_ex_func->name, temp_rl->var, 
                                          temp_rl->field_info, field_pos, RHS);
               END_IF
            END_IF
         END_IF

         /*=============================================*/
         /*  Check if it was originally bound in a not  */
         /*=============================================*/

         IF (temp_rl->bound_in_not > 0) THEN
            IF (in_not EQ NO) THEN
               sprintf(msg_buf,"Undefined variable <%s> referenced on rhs of rule",
			temp_rl->var);
               error_message (ERROR, msg_buf);
            ELSE_IF (cur_rule->num_nots NEQ temp_rl->bound_in_not) 
	       sprintf(msg_buf,
	"Variable <%s> bound inside a not is not constrained by\n the previous NOT pattern(s)",
			temp_rl->var);
               error_message (WARNING,msg_buf);
            END_IF
         END_IF

         temp_rl->count++;
      END_IF
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_bound_words () 
 *
 *  PURPOSE:  This function searches pattern fields and action
 *            arguments looking for variables. If it finds them
 *            it calls the appropriate function to store the
 *            information.
 *
 *  INPUTS :  Two arguments, 
 *             A flag telling what side of rule this was called
 *               from (RHS or LHS) (int), and
 *             A flag telling if function was called from inside
 *               a not pattern (YES or NO) (int).
 *
 *  RETURNS:  An integer, but currently nothing meaningful.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */

static FIELD_PTR find_bound_words (field_ptr, side_of_rule, in_not, 
   first_field_var) 
FIELD_PTR field_ptr;
int side_of_rule, in_not, first_field_var;
{
   int token, count, all_var;
   FIELD_PTR temp_field;

   count = 1;

   IF (first_field_var EQ YES) THEN
      all_var = YES;
   ELSE
      all_var = NO;
   END_IF

   temp_field = field_ptr;
   while (count NEQ 0) DO
      if ((field_ptr != NULL) && (temp_field != NULL)) 
       {
         TKNWORD = temp_field->word;
         token   = temp_field->num;
         if (token == NUMBER) 
           TKNNUMBER = atof (TKNWORD);
         temp_field = temp_field->next;
       }
      else
        token = gettoken (cur_file);
      IF (token EQ LPAREN) THEN
        if ((field_ptr != NULL) && (temp_field != NULL)) 
         {
           TKNWORD = temp_field->word;
           token   = temp_field->num;
           if (token == NUMBER) 
             TKNNUMBER = atof (TKNWORD);
           temp_field = temp_field->next;
          }
         else
           token = gettoken (cur_file);
         IF ((token EQ WORD) AND (check_ex_funcs (TKNWORD) IS_NO)) THEN
            cur_rule->num_ex_funcs++;
            temp_field = process_ex_funcs (temp_field, cur_rule, TKNWORD, LHS);
         ELSE
            count++;
         END_IF
      ELSE_IF (token EQ RPAREN) 
         count--;
      ELSE_IF ((token EQ BWORD) OR (token EQ BWORDS)) 
         IF (side_of_rule EQ RHS) THEN
            (void)process_RHS_variable (token, 0, in_not, 0);
         ELSE_IF (side_of_rule EQ LHS) 
            (void)process_LHS_variable (NULL, NULL, token, 0, NO, 
                      in_not, 0, NO);
         END_IF
      ELSE_IF ((token EQ SINGLE) OR (token EQ MULTIPLE)) 
         IF (side_of_rule EQ RHS) THEN
            error_message (ERROR, "Wildcard used in an inappropriate place.");
         END_IF
      ELSE
         all_var = NO;
      END_IF

   END_WHILE

   IF (all_var EQ YES) THEN
      error_message (WARNING, "Pattern contains all variables!");
   END_IF

   return (temp_field);
}

/* ===================================================================
 * ===========       Relation Processing Functions       =============
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_simple_relation () 
 *
 *  PURPOSE:  This function is called when looking for the
 *            relation name in a deffacts or an assert, i.e.
 *            places where I expect to find only 'simple'
 *            pattern constructs. Once the relation is found, 
 *            it calls functions which do the right thing with
 *            the rest of the pattern.
 *
 *  INPUTS :  A flag telling if function was called from inside
 *            an ASSERT or a DEFFACTS (int).
 *
 *  RETURNS:  An integer, OK if relation was found and processed
 *            without problems, ERROR if an incorrect or invalid
 *            item is found.
 *
 *  NOTES  :  Should consider if RESTART is necessary anywhere.
 * -------------------------------------------------------------
 */

static int find_simple_relation (type) 
int type;
{
   int  token;
   int  rtn;

   token = gettoken (cur_file);

   /*==============================*/
   /* Check for illegal constructs */
   /*==============================*/

   IF ((token EQ WORD) AND ((strcmp (TKNWORD, "not") EQ 0) OR
                           (strcmp (TKNWORD, "or") EQ 0) OR
                           (strcmp (TKNWORD, "and") EQ 0) OR
                           (strcmp (TKNWORD, "test") EQ 0)) ) THEN
      sprintf (msg_buf, "\'%.40s\' not allowed in a deffacts or assert.", 
         TKNWORD);
      error_message (ERROR, msg_buf);
      find_rht_paren ();              /* Should find rht paren next! */
      rtn = ERROR;

   /*=============================*/
   /* Check for illegal variables */
   /*=============================*/

   ELSE_IF ((token EQ BWORD) OR (token EQ BWORDS) OR (token EQ SINGLE) OR
           (token EQ MULTIPLE)) 
      IF (type EQ DEFFACTS) THEN
         error_message (ERROR, 
                       "Variables or wildcards not allowed in a deffacts.");
         find_rht_paren ();
      ELSE_IF (type EQ ASSERT) 
         error_message (WARNING, 
                    "Variable used as the first field in an assert.");
         (void)find_bound_words (NULL, RHS, NO, NO);
      END_IF
      rtn = ERROR;

   /*========================*/
   /* Check for legal tokens */
   /*========================*/

   ELSE_IF ((token EQ WORD) OR (token EQ STRING) OR (token EQ NUMBER)) 
      rtn = OK;

   /*====================================*/
   /* Otherwise this is an unknown token */
   /*====================================*/

   ELSE
      error_message (ERROR, 
                "Unexpected item found, expecting a WORD, STRING, or NUMBER.");
      find_rht_paren ();
      rtn = ERROR;
   END_IF
   return (rtn);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  rule_find_relation () 
 *
 *  PURPOSE:  This function is called when looking for the
 *            relation name in a rule. Once the relation is found, 
 *            it calls functions which do the right thing with
 *            the rest of the pattern.
 *
 *  INPUTS :  Three arguments, 
 *             A pointer to the current rule (RULE_PTR), 
 *             A flag telling if the pattern is bound to a
 *               variable for retraction (YES or NO) (int), and
 *             A flag telling if function was called from inside
 *               a not (YES or NO) (int).
 *
 *  RETURNS:  An integer, OK if processed OK, RESTART if
 *            it got confused.
 *
 *  NOTES  :  Should consider if RESTART is necessary anywhere.
 * -------------------------------------------------------------
 */

static int rule_find_relation (current_rule, in_retract, in_not) 
int      in_retract, in_not;
RULE_PTR current_rule;
{
   int           token;
   int           rtn;
   MULT_REL_PTR  list;
   HASH         *temp_hash;

   list = NULL;
   token = gettoken (cur_file);

   /*=======================*/
   /*  Process a test case  */
   /*=======================*/

   IF ((token EQ WORD) AND (strcmp (TKNWORD, "test") EQ 0)) THEN
      token = gettoken (cur_file);
      IF (token NEQ LPAREN) THEN
         error_message (ERROR, "Expecting \' (\' after a test.");
         find_rht_paren ();
      ELSE
         save_token (token, TKNWORD, TKNNUMBER);
         (void)find_bound_words (NULL, RHS, NO, NO);
      END_IF

   /*======================*/
   /*  Process a not case  */
   /*======================*/

   ELSE_IF ((token EQ WORD) AND (strcmp (TKNWORD, "not") EQ 0)) 

      /*============================*/
      /*  A not can't be retracted! */
      /*============================*/

      IF (in_retract IS_YES) THEN
         error_message (ERROR, "Can't retract a negated fact!");
         find_rht_paren ();
      ELSE_IF (in_not IS_YES) 
         error_message (ERROR, "Can't nest negated facts.");
         find_rht_paren ();
      ELSE

         /*==================================*/
         /*  Past not, now process relation  */
         /*==================================*/

         token = gettoken (cur_file);
         IF (token NEQ LPAREN) THEN
            error_message (ERROR, "Expecting \' (\' after a pattern not.");
            find_rht_paren ();
         ELSE
            current_rule->num_nots++;
            (void)rule_find_relation (current_rule, NO, current_rule->num_nots);
            find_rht_paren ();              /* Should find rht paren next! */
         END_IF
      END_IF

   /*======================*/
   /*  Process wildcards   */
   /*======================*/

   ELSE_IF ((token EQ SINGLE) OR (token EQ MULTIPLE)) 
      error_message (WARNING, 
         "Wildcard used in first field.");
      current_rule->num_patterns++;
      list = check_multiple_relations (list, current_rule, in_not);
      if (list == NULL) 
          (void)find_bound_words (NULL, LHS, in_not, YES);
      else
          (void)process_rule_relation (current_rule, list, in_retract, in_not);

   /*======================*/
   /*  Process variables   */
   /*======================*/

   ELSE_IF ((token EQ BWORD) OR (token EQ BWORDS)) 
      sprintf (msg_buf, "Variable <%.40s> was used in first field.", TKNWORD);
      error_message (WARNING, msg_buf);
      current_rule->num_patterns++;
      (void)process_LHS_variable (NULL, NULL, token, 0, NO, in_not, 0, NO);
      list = check_multiple_relations (list, current_rule, in_not);
      if (list == NULL) 
        (void)find_bound_words (NULL, LHS, in_not, YES);
      else
        (void)process_rule_relation (current_rule, list, in_retract, in_not);

   /*=====================*/
   /*  Process 'or' case  */
   /*=====================*/

   ELSE_IF (strcmp (TKNWORD, "or") EQ 0) 
      token = gettoken (cur_file);
      while (token NEQ RPAREN) DO
         IF (token EQ BWORD) THEN
            rtn = process_LHS_retract (current_rule, YES);
            IF (rtn EQ ERROR) THEN
               return (RESTART);
            END_IF
            (void)rule_find_relation (current_rule, YES, NO);
         ELSE_IF (token NEQ LPAREN) 
            error_message (ERROR, "Expecting \' (\' after a pattern or or and.");
            find_rht_paren ();
         ELSE
            (void)rule_find_relation (current_rule, NO, NO);
         END_IF
         token = gettoken (cur_file);
      END_WHILE

   /*======================*/
   /*  Process 'and' case  */
   /*======================*/

   ELSE_IF (strcmp (TKNWORD, "and") EQ 0) 
      token = gettoken (cur_file);
      while (token NEQ RPAREN) DO
         IF (token EQ BWORD) THEN
            rtn = process_LHS_retract (current_rule, NO);
            IF (rtn EQ ERROR) THEN
               return (RESTART);
            END_IF
            (void)rule_find_relation (current_rule, YES, NO);
         ELSE_IF (token NEQ LPAREN) 
            error_message (ERROR, "Expecting \' (\' after a pattern or or and.");
            find_rht_paren ();
         ELSE
            (void)rule_find_relation (current_rule, NO, NO);
         END_IF
         token = gettoken (cur_file);
      END_WHILE

   /*====================================*/
   /*  Process a notted relation ~<rel>  */
   /*====================================*/

   ELSE_IF (token EQ LNOT) 
      token = gettoken (cur_file);

      IF ((token EQ WORD) OR (token EQ STRING) OR (token EQ NUMBER)) THEN
         list = alloc_mult_rel ();
         (void) strcpy (list->rel_name, TKNWORD);
         (void) process_rule_relation (current_rule, list, in_retract, in_not);

      ELSE_IF ((token EQ BWORD) OR (token EQ BWORDS)) 
         error_message (WARNING, 
            "Variable used in first field");
         current_rule->num_patterns++;
         (void)process_LHS_variable (NULL, NULL, token, 0, NO, in_not, 0, NO);
         list = check_multiple_relations (list, current_rule, in_not);
         if (list == NULL) 
           (void)find_bound_words (NULL, LHS, in_not, YES);
         else
           (void)process_rule_relation (current_rule, list, in_retract, in_not);
      ELSE
         error_message (ERROR, "Illegal item following a \~.");
         list = check_multiple_relations (list, current_rule, in_not);
         if (list == NULL) 
           (void)find_bound_words (NULL, LHS, in_not, YES);
         else
            (void)process_rule_relation (current_rule, list, in_retract, in_not);
      END_IF

   /*===============================*/
   /*  Process a regular relation!  */
   /*===============================*/

   ELSE_IF ((token EQ WORD) OR (token EQ STRING) OR (token EQ NUMBER)) 
     list = alloc_mult_rel ();
     temp_hash = add_symbol (TKNWORD);
     list->rel_name = temp_hash->contents;
     list = check_multiple_relations (list, current_rule, in_not);
     (void)process_rule_relation (current_rule, list, in_retract, in_not);

   /*==================================================*/
   /*  Don't know what this is, (shouldn't get here!)  */
   /*==================================================*/

   ELSE
      error_message (ERROR, "Unexpected item found, expecting a pattern.");
      find_rht_paren ();
   END_IF

   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  process_fb_relation () 
 *
 *  PURPOSE:  This function is called when a relation has been
 *            found in a deffacts block. It stores the appropriate
 *            information in the relations structure.
 *
 *  INPUTS :  A pointer to the current fact block (FACT_BLOCK *).
 *
 *  RETURNS:  Nothing
 *
 *  NOTES  :  Should be careful with memory out in alloc call!.
 * -------------------------------------------------------------
 */

static void process_fb_relation (cur_fb) 
FACT_BLOCK *cur_fb;
{
   char     *cur_rel_name;
   FBR_PTR   temp_fbr;

   cur_rel_name  = TKNWORD;

   /*===============================*/
   /* Put relation in relation list */
   /*===============================*/

   rel_head = find_create_rel (rel_head, cur_rel_name);

   IF (find_def_tmp (def_tmp_head, cur_rel_name)) THEN
      templ_pattern_to_rel_pattern (cur_rel_name, YES,NULL);
   END_IF

   /*============================*/
   /* Fill in relation structure */
   /*============================*/

   temp_fbr             = alloc_fbr ();
   temp_fbr->fact_block = cur_fb;
   temp_fbr->next_fbr   = cur_rel->fb_list;
   temp_fbr->num_fields = find_fields (cur_rel, NULL, RHS, NO,NULL, NO);

   IF (CHECK_DEFRELS IS_ON) THEN
      check_min_num_fields (cur_rel->name, temp_fbr->num_fields, NO);
   END_IF

   cur_rel->fb_list     = temp_fbr;
   return;
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_create_rel () 
 *
 *  PURPOSE:  This function searches through a list of
 *            relations to find a specific name. If it doesn't
 *            find the name, it creates a new relation structure
 *            and adds it to the list.
 *
 *  INPUTS :  A pointer to the list of relations (RELATION *) 
 *            and a pointer to the name to search for (char *) 
 *
 *  RETURNS:  A pointer to the relation structure for the
 *            relation name
 *
 *  NOTES  :
 * ------------------------------------------------------------------
 */

static RELATION *find_create_rel (node, name) 
REL_PTR  node;
char    *name;
{
   int rtn;

   IF (node EQ NULL) THEN        /* A new relation! */
      node = alloc_rel ();
      node->name = name;
      cur_rel = node;
   ELSE
      rtn = strcmp (name, node->name);
      IF (rtn < 0) THEN
         node->lft_rel = find_create_rel (node->lft_rel, name);
      ELSE_IF (rtn > 0) 
         node->rht_rel = find_create_rel (node->rht_rel, name);
      ELSE
         cur_rel = node;
      END_IF
   END_IF
   return (node);
}

/* ==================================================================== */

/* ------------------------------------------------------------------
 *  NAME   : find_rel () 
 *
 *  PURPOSE: This function searches through a list of
 *           relations to find a specific name.
 *
 *  INPUTS : A pointer to the list of relations (RELATION *) 
 *           and a pointer to the name to search for (char *) 
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 *
 *  NOTES  : Primarily for calling by the CRSVDEFS functions.
 * ------------------------------------------------------------------
 */

RELATION *find_rel (node, name) 
REL_PTR  node;
char    *name;
{
   int rtn;

   IF (node EQ NULL) THEN        /* A new relation! */
      return (NULL);
   ELSE
      rtn = strcmp (name, node->name);
      IF (rtn < 0) THEN
         return (find_rel (node->lft_rel, name));
      ELSE_IF (rtn > 0) 
         return (find_rel (node->rht_rel, name));
      ELSE
         return (node);
      END_IF
   END_IF
}


/* ==================================================================== */


/* ------------------------------------------------------------------
 *  NAME   : find_ex_func () 
 *
 *  PURPOSE: This function searches through a list of
 *           relations to find a specific name.
 *
 *  INPUTS : A pointer to the list of relations (RELATION *) 
 *           and a pointer to the name to search for (char *) 
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 *
 *  NOTES  : Primarily for calling by the CRSVDEFS functions.
 * ------------------------------------------------------------------
 */

EX_FUNC *find_ex_func (node, name) 
EXF_PTR  node;
char    *name;
{
   int rtn;

   IF (node EQ NULL) THEN        /* A new external function! */
      return (NULL);
   ELSE
      rtn = strcmp (name, node->name);
      IF (rtn < 0) THEN
         return (find_create_ex_func (node->lft_ex_func, name));
      ELSE_IF (rtn > 0) 
         return (find_create_ex_func (node->rht_ex_func, name));
      ELSE
         return (node);
      END_IF
   END_IF
}

/* ==================================================================== */


/*
 * -------------------------------------------------------------
 *  NAME   :  process_rule_relation () 
 *
 *  PURPOSE:  This function is called when a relation has been
 *            found inside a rule. It stores the appropriate
 *            information in the relations structure.
 *
 *  INPUTS :  Three arguments, 
 *             A pointer to the current rule (RULE_PTR), 
 *             A flag telling if the pattern is bound to a
 *               variable for retraction (YES or NO) (int), and
 *             A flag telling if function was called from inside
 *               a not (YES or NO) (int).
 *
 *  RETURNS:  An integer, but currently doesn't return anything.
 *
 *  NOTES  :  Should be carefule with memory out in alloc call!.
 * -------------------------------------------------------------
 *                     Psuedo Code
 *
 *   Set current relation name
 *   Find relation structure pointer
 *   Allocate new Rule/Relation pointer
 *   Put Rule/Rel pointer into linked list
 *   process each field until closing paren is found
 * ------------------------------------------------------------
 */

static int process_rule_relation (current_rule, rel_list, in_retract, in_not) 
int      in_retract, in_not;
RULE_PTR current_rule;
MULT_REL_PTR rel_list;

{
   char          *cur_rel_name;
   RR_PTR        temp_rr;
   MULT_REL_PTR  temp;
   HASH *temp_hash;
   FIELD_PTR     fld_list, temp_fld, tail_list, var_list;
   int token, count = 1;
   int num_template = 0, num_rel = 0;
   int is_deftmp = NO;

   FOR (temp = rel_list;temp != NULL; temp = temp->next_rel) DO
       IF (find_def_tmp (def_tmp_head, temp->rel_name)) THEN
          num_template++;
       ELSE
          num_rel++;
       END_IF
   DONE

   IF (num_template NEQ 0) THEN
      IF (num_rel NEQ 0 OR num_template NEQ 1) THEN
         error_message (ERROR, 
            "Template names may not be used in a logical expression.");
         find_rht_paren ();
         return (ERROR);
      ELSE
         var_list = NULL;
         is_deftmp = YES;
         templ_pattern_to_rel_pattern (rel_list->rel_name, NO, &var_list);
      END_IF
   END_IF

   fld_list = NULL;

   /* =================================================== */
   /* Store the fields of the relation (s) for latter      */
   /* processing. This is done because of the possibility */
   /* of multiple relations.                              */
   /* =================================================== */

   WHILE (count > 0) DO
      token = gettoken (cur_file);

      IF (token == RPAREN) THEN
         count--;
      ELSE_IF (token == LPAREN) 
         count++;
      END_IF

      /* --------------------------------------------
       *  If the token is $? or $?var then set the
       *  wildcard tag of the relation structure to
       *  one.  This will be used when creating the
       *  defrelation file.
       * --------------------------------------------
       */

      temp_fld = alloc_field ();
      temp_hash = add_symbol (TKNWORD);
      temp_fld->word = temp_hash->contents;
      temp_fld->num = token;           /* Type of the token */

      IF (fld_list == NULL) THEN
         tail_list = temp_fld;
         fld_list = temp_fld;
      ELSE
         tail_list->next = temp_fld;
         tail_list = tail_list->next;
      END_IF
   DONE

   temp = rel_list;
   WHILE (temp != NULL) DO
      cur_rel_name = temp->rel_name;
      rel_head = find_create_rel (rel_head, cur_rel_name);
      current_rule->num_patterns++;
      temp_rr              = alloc_rr ();
      temp_rr->rule        = current_rule;
      temp_rr->next_rr     = cur_rel->rule_list;
      temp_rr->num_fields  = find_fields (cur_rel, fld_list, LHS, in_not, 
						var_list,is_deftmp);
      cur_rel->rule_list   = temp_rr;

      IF (CHECK_DEFRELS IS_ON) THEN
         check_min_num_fields (cur_rel->name, temp_rr->num_fields, NO);
      END_IF

      IF (in_retract IS_YES) THEN
         retract_head->relation = cur_rel;
      END_IF
      temp = temp->next_rel;
   DONE

   return (OK);
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_multiple_relations () 
 *
 *  PURPOSE:  This function is called after a relation or other
 *            first field info has been found. It checks to see
 *            if the relation is followed by an OR or an AND
 *            (& or |) in which case it strips the other
 *            relation names and advances the counter.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  This is a pretty stupid function now. It should
 *            do something to allow the storage of relation info
 *            for multiple relations. At present, it merely
 *            skips them so they don't cause other processing
 *            problems.
 * ------------------------------------------------------------
 */

static MULT_REL_PTR check_multiple_relations (list, rule, in_not) 
MULT_REL_PTR list;
RULE_PTR rule;
int in_not;
{
   int token;
   int exit_flag;
   int expect_lp;
   MULT_REL_PTR temp;
   HASH *temp_hash;

   /*======================*/
   /* Initialize variables */
   /*======================*/

   token       = gettoken (cur_file);
   expect_lp   = NO;
   exit_flag   = NO;

   IF ((token EQ LAND) OR (token EQ LOR)) THEN
      while (exit_flag IS_NO) DO
         token = gettoken (cur_file);
         switch (token) DO
            case BWORD:
            case BWORDS:
                 expect_lp = NO;
                 rule->num_patterns++;
                 (void)process_LHS_variable (NULL, NULL, token,
                           0, NO, in_not, 0, NO);
                 sprintf (msg_buf, 
                    "Variable <%.40s> was used in first field.", TKNWORD);
        		   error_message (WARNING, msg_buf);
                 token     = gettoken (cur_file);
                 break;
            case STRING:
            case NUMBER:
                 temp = alloc_mult_rel ();
                 temp_hash = add_symbol (TKNWORD);
                 temp->rel_name = temp_hash->contents;
                 temp->next_rel = list;
                 list = temp;
                 expect_lp = NO;
                 token     = gettoken (cur_file);
	             break;
            case WORD:
                 IF ((strcmp (TKNWORD, "=") EQ 0) OR
                    (strcmp (TKNWORD, ":") EQ 0)) THEN
                    expect_lp = YES;
                 ELSE
                    temp = alloc_mult_rel ();
                    temp_hash = add_symbol (TKNWORD);
                    temp->rel_name = temp_hash->contents;
                    temp->next_rel = list;
                    list = temp;
                    expect_lp = NO;
                    token     = gettoken (cur_file);
                 END_IF
                 break;

            case LPAREN:
                 IF (expect_lp NEQ YES) THEN
                    error_message (ERROR, 
                                  "Unexpected '(' in relation field.");
                 END_IF
                 (void)find_bound_words (NULL, RHS, NO, NO);
                 expect_lp = NO;
                 token     = gettoken (cur_file);
                 break;

            case SINGLE:
            case MULTIPLE:
                 error_message (ERROR, "Invalid wildcard after '&' or '|'.");
                 expect_lp = NO;
                 token     = gettoken (cur_file);
                 break;

            case LNOT:
                 expect_lp = YES;
                 break;
         DONE

         IF (expect_lp EQ YES) THEN
            exit_flag = NO;
         ELSE_IF ((token EQ LAND) OR (token EQ LOR)) 
            exit_flag = NO;
         ELSE
            exit_flag = YES;
         END_IF

      END_WHILE
   END_IF

   save_token (token, TKNWORD, TKNNUMBER);
   return (list);
}

/* ===================================================================
 * =============       Field Processing Functions       ==============
 * ===================================================================
 */

/* ------------------------------------------------------------------
 * NAME   :     find_fields () 
 *
 * PURPOSE:     This function is called after a relation has been
 *              found and breaks out the remaining fields in the
 *              pattern. It counts the number of fields and stores
 *              literal values.
 *
 * INPUTS:      A pointer to the current relation structure, a
 *              flag to tell which side of the rule called it, and
 *              a flag to tell if currently in a not.
 *
 * RETURNS:     An integer, the number of fields in the pattern.
 *              If a $?var was found, the number of fields will
 *              be negative, so use the absolute value. If an
 *              error is found, it returns zero.
 *
 * ------------------------------------------------------------------
 *                       Pseudo Code
 *
 *   get token
 *   set field counter to 1 (for relation) 
 *   set new field flag to YES (again, for relation) 
 *   while the token is not a RPAREN, do
 *      if the token is a variable or wildcard, then
 *         ignore token, increment field counter
 *      else if the token is an '&', or a '|', then
 *         ignore token, decrement field counter
 *      else if the token is an '=' (OPERATOR) then
 *         set expect LPAREN
 *      else if the token is a ':' (COAMP), then
 *         set expect LPAREN
 *      else if token is a LPAREN, then
 *         if expect LPAREN is on, then
 *            find_rht_paren
 *            increment field counter
 *         else
 *            PUNT, to next rule, error in code!
 *         end if
 *      else if the token is a '~', then
 *         ignore token
 *      else if the token is a WORD, STRING or NUMBER, then
 *         increment field counter
 *         process field
 *      else
 *         Error, don't know what this is
 *      end if
 *      get next token
 *   end while
 * ------------------------------------------------------------------
 */

static int find_fields (current_rel, field_list, side_of_rule, in_not, 
				var_list, is_deftmp) 
REL_PTR current_rel;
FIELD_PTR field_list, var_list;
int     side_of_rule, in_not, is_deftmp;
{
   int   token;
   int   expect_lp;
   int   num_fields;
   int   found_multi;
   short not_flag;
   FIELD_PTR temp_fld;

   /*======================*/
   /* Initialize variables */
   /*======================*/

   temp_fld = field_list;
   if ((field_list != NULL) && (temp_fld != NULL)) 
    {
       token   = temp_fld->num;
       TKNWORD = temp_fld->word;
       if (token == NUMBER) 
         TKNNUMBER = atof (TKNWORD);
       temp_fld = temp_fld->next;
    }
   else
       token   = gettoken (cur_file);
   num_fields  = 1;
   expect_lp   = NO;
   found_multi = NO;
   not_flag    = NO;

   while (token NEQ RPAREN) DO
      switch (token) DO
         case SINGLE:
            num_fields++;
            expect_lp = NO;
            IF (side_of_rule EQ RHS) THEN
               error_message (ERROR, "Wildcard used in an inappropriate place.");
            END_IF
            break;

         case BWORD:
            num_fields++;
            expect_lp = NO;
            IF (side_of_rule EQ LHS) THEN
               if ((is_deftmp) && (not_flag))
                 not_flag = find_var_in_template(var_list,TKNWORD);
               (void)process_LHS_variable (current_rel, NULL, BWORD, num_fields, 
               		NO, in_not, REL, not_flag);
            ELSE_IF (side_of_rule EQ RHS) 
               (void)process_RHS_variable (BWORD, num_fields, in_not, REL);
            END_IF
            not_flag = NO;
            break;

         case MULTIPLE:
            num_fields++;
            IF ((found_multi IS_YES) AND (side_of_rule EQ LHS)) THEN
               error_message (WARNING, 
                             "Multiple $? variables used in a single pattern.");
            END_IF
            found_multi = YES;
            expect_lp = NO;
            IF (side_of_rule EQ RHS) THEN
               error_message (ERROR, "Wildcard used in an inappropriate place.");
            END_IF
            break;

         case BWORDS:
            num_fields++;
            IF ((found_multi IS_YES) AND (side_of_rule EQ LHS)) THEN
               error_message (WARNING, 
                             "Multiple $? variables used in a single pattern.");
            END_IF
            found_multi = YES;
            expect_lp = NO;
            IF (side_of_rule EQ LHS) THEN
               if ((is_deftmp) && (not_flag))
                 not_flag = find_var_in_template(var_list,TKNWORD);
               (void)process_LHS_variable (current_rel, NULL, BWORDS, 0, NO, 
               		in_not, REL, not_flag);
            ELSE_IF (side_of_rule EQ RHS) 
               (void)process_RHS_variable (BWORDS, 0, in_not, REL);
            END_IF
            not_flag = NO;
            break;

         case LAND:
         case LOR:
            num_fields--;
            expect_lp = NO;
            break;

         case LPAREN:
            IF (expect_lp IS_YES) THEN
               if ((field_list != NULL) && (temp_fld != NULL)) 
                {
                 token   = temp_fld->num;
                 TKNWORD = temp_fld->word;
                 if (token == NUMBER) 
                 TKNNUMBER = atof (TKNWORD);
                 temp_fld = temp_fld->next;
                }
               else
                 token   = gettoken (cur_file);
               num_fields++;
               IF ((token EQ WORD) AND (check_ex_funcs (TKNWORD) IS_NO)) THEN
                  IF (CHECK_DEFRELS IS_ON) THEN
                     check_rtn_type_w_defrel (current_rel->name, TKNWORD, num_fields);
                  END_IF
                  cur_rule->num_ex_funcs++;
                  temp_fld = process_ex_funcs (temp_fld, cur_rule, TKNWORD, 
                     side_of_rule);
               ELSE
                  temp_fld = find_bound_words (temp_fld, RHS, in_not, NO);
               END_IF
               expect_lp = NO;
            ELSE
               error_message (ERROR, 
                             "Unexpected Left Parentheses! Punting from rule.");
               return (0);
            END_IF
            break;

         case LNOT:
            expect_lp = NO;
            not_flag  = YES;
            break;

         case WORD:
            IF ((strcmp (TKNWORD, "=") EQ 0) OR (strcmp (TKNWORD, ":") EQ 0)) THEN
               expect_lp = YES;
            ELSE
               num_fields++;
               (void)process_field (token, TKNWORD, num_fields, current_rel);
               not_flag = NO;
            END_IF
            break;


         case STRING:
         case NUMBER:
            num_fields++;
            (void)process_field (token, TKNWORD, num_fields, current_rel);
            not_flag = NO;
            break;

         default:
            error_message (ERROR, "Unknown symbol found!");
            break;
      DONE

      if ((temp_fld != NULL) && (field_list != NULL)) 
      {
       token   = temp_fld->num;
       TKNWORD = temp_fld->word;
       if (token == NUMBER) 
         TKNNUMBER = atof (TKNWORD);
       temp_fld = temp_fld->next;
      }
      else
        token = gettoken (cur_file);
   END_WHILE

   IF (found_multi IS_YES) THEN
      return (-num_fields);
   ELSE
      return (num_fields);
   END_IF
}

/* ==================================================================== */
 
int find_var_in_template(list,name)
FIELD_PTR list;
char *name;
{
  FIELD_PTR temp;

    temp = list;
    while(temp != NULL)
      {
         if(strcmp (temp->word,name) == 0)
           break;
         temp = temp->next;
      }
    if((temp != NULL) && ( temp->num > 1))
       return(NO);
    else
       return(YES);
}

/* ---------------------------------------------------------------
 * NAME  :  process_field () 
 *
 * PUROSE:  This function is called after a literal value
 *          has been found in a field. It searches the
 *          list of words already used for that field in
 *          that relationship and store it appropriately.
 *          It may also call the appropriate functions for
 *          comparing to any defrelation values!
 *
 * INPUTS:  Four arguments:
 *
 *  int   type      -  The type of the literal value
 *  char *word      -  A pointer to the literal value
 *  int   num_field -  The number of the field word was found in
 *  REL_PTR current_rel -  A pointer to the current relation structure.
 *
 * RETURNS:     An integer, basically useless (always OK) 
 * ------------------------------------------------------------------
 *                          Pseudo Code
 *
 *   Find field pointer
 *   while word list pointer is not NULL, do
 *      compare word with word list element
 *      if they are the same word, then
 *         return
 *      end if
 *      set word list pointer to next element
 *   end while
 *   allocate new word list element
 *   put word in element
 *   put element at top of list
 *   hook list back up to field pointer in current relation
 * ---------------------------------------------------------------
 */

static int process_field (type, word, num_field, current_rel) 
REL_PTR  current_rel;
int      type, num_field;
char    *word;
{
   FLD_PTR   temp_fld;

   temp_fld = get_fld_ptr (num_field, current_rel);

   temp_fld->lit_values = add_to_word_list (temp_fld->lit_values, word, type, 
      NO);

   /*========================*/
   /* Check Defrelation info */
   /*========================*/

   IF (CHECK_DEFRELS IS_ON) THEN
      check_field_info (current_rel->name, type, num_field, word);
   END_IF

   return (OK);
}

/* ==================================================================== */

/* -------------------------------------------------------
 *  NAME   :  get_fld_ptr () 
 *
 *  PURPOSE:  This function finds the field pointer
 *            associated with a field number. (e.g.
 *            the structure associated with the second
 *            field). If no structure was previously
 *            associated, one is created.
 *
 *  INPUTS:   Two arguments:
 *
 *    int     num     - Number of the field we want
 *    REL_PTR current_rel - Pointer to current relation structure
 *
 *  RETURNS:  A pointer to the proper field (FLD_PTR).
 * -------------------------------------------------------
 */

static FLD_PTR get_fld_ptr (num, current_rel) 
int num;
REL_PTR current_rel;
{
   FLD_PTR temp_fld;

   temp_fld = current_rel->field_list;
   while (temp_fld NEQ NULL) DO
      IF (num EQ temp_fld->field_num) THEN
         return (temp_fld);
      END_IF
      temp_fld = temp_fld->next_field;
   END_WHILE

   temp_fld             = alloc_fld ();
   temp_fld->field_num  = num;
   temp_fld->next_field = current_rel->field_list;
   current_rel->field_list  = temp_fld;

   return (temp_fld);
}


static int process_args (type, word, num_arg, node) 
int type;
char *word;
int num_arg;
EXF_PTR node;

{
   FLD_PTR   temp_arg;

   temp_arg = get_arg_ptr (num_arg, node);

   temp_arg->lit_values = add_to_word_list (temp_arg->lit_values, word, type, 
      NO);

   /*========================*/
   /* Check Defexternal info */
   /*========================*/

   IF (CHECK_DEFRELS IS_ON) THEN
      check_argument_info (node->name, type, num_arg, word);
   END_IF

   return (OK);
}

/* -------------------------------------------------------
 *  NAME   :  get_arg_ptr () 
 *
 *  PURPOSE:  This function finds the argument pointer
 *            associated with an argument number. (e.g.
 *            the structure associated with the second
 *            argument). If no structure was previously
 *            associated, one is created.
 *
 *  INPUTS:   Two arguments:
 *
 *    int      num       - Number of the argument we want
 *    EX_FUNC *cur_ex    - Pointer to current external
 *                         function structure
 *
 *  RETURNS:  A pointer to the proper field (FLD_PTR).
 * -------------------------------------------------------
 */

static FLD_PTR get_arg_ptr (num, cur_ex) 
int num;
EX_FUNC *cur_ex;
{
   FLD_PTR temp_arg;

   temp_arg = cur_ex->argument_list;
   while (temp_arg NEQ NULL) DO
      IF (num EQ temp_arg->field_num) THEN
         return (temp_arg);
      END_IF
      temp_arg = temp_arg->next_field;
   END_WHILE

   temp_arg               = alloc_fld ();
   temp_arg->field_num    = num;
   temp_arg->next_field   = cur_ex->argument_list;
   cur_ex->argument_list  = temp_arg;

   return (temp_arg);
}

/* =======================================================
 * check_rtn_type_w_defrel () 
 *    This funtion checks to see if the return type of an
 *    external function is compatible with the definition of
 *    a relation.  This is called when the return value of an
 *    external function is a field in a relation
 *
 * arguments:
 *    rel_name  : name of the relation which called the external function
 *    func_name : name of the external function
 *    position  : position of the external function in the relation
 * =======================================================
 */

static int check_rtn_type_w_defrel (rel_name, func_name, position) 
char *rel_name, *func_name;
int position;

{
   DR_PTR    def_rel;
   DE_PTR    def_ex;
   DEF_FLD  *field;

   def_rel = find_def_rel (def_rel_head, rel_name);
   if (def_rel != NULL) 
   {
      def_ex = find_def_ex_func (def_ext_head, func_name);
      if (def_ex != NULL) 
      {
         field = find_def_fld (position, def_rel->fields);
         if (field != NULL) 
         {
            if (( (def_ex->return_word IS_YES) &&   (field->allow_word IS_NO)) ||
               ((def_ex->return_string IS_YES) &&
                (field->allow_string IS_NO)) ||
               ((def_ex->return_number IS_YES) && (field->allow_number IS_NO)) ) 
            {
               error_message (ERROR, 
                  "Incompatible type definition. Return type of external function.\n");
               sprintf (msg_buf, 
                  "        <%.40s> is not allowed in position %d of relation <%s>.", 
                  func_name, position, rel_name);
               send_text (msg_buf, 8);
            }
         } 				
      }
   }
}
