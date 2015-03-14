#include <stdio.h>
#include "crsv.h"

/********************************************************************
 *  This file contains all the functions used to process defexternal
 *  structures and store or retreive defexternal information.
 ********************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */
extern DE_PTR     alloc_def_ex_func();
extern DA_PTR     alloc_def_fld();
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

extern EX_FUNC  *find_ex_func();

/* ---------------------------------------------------------------
 *   From the file: CRSVDEFREL.C
 *
 *   These functions are used for both defrelation and defexternal
 *   in order to process the arguments of an external function or
 *   the fields of a relation.
 * ---------------------------------------------------------------
 */
 extern void   process_fld_string_list(); /* Process the string list of
                                             defexternal                     */
 extern void   process_fld_number_list(); /* Process the number list of
                                             defexternal                     */
 extern void   process_fld_range();       /* Process the number range of
                                             defexternal                     */
 extern void   process_fld_type();        /* Process the argument type of
                                             defexternal                     */
 extern void   process_fld_word_list();   /* Process the word list of
                                             defexternal                     */
 extern int    compare_fld_types();
 extern int    compare_fld_word_lists();
 extern int    compare_fld_string_lists();
 extern int    compare_fld_numbers();

/* -----------------------------
 *  From the system
 * -----------------------------
 */

extern double atof();


/* ===========  Functions defined here for Global use  ================ */

int      process_defexternal();
void     check_argument_info();
void     check_var_argument_info();
/*^void     check_min_num_arguments();*/


/* ===========  Functions defined here for internal use  ============== */

DEF_EXT   *find_create_def_ex_func();
DEF_EXT   *find_def_ex_func();
int        process_argument_def();
DEF_ARG   *find_def_arg();
void       duplicate_def_ex();


/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern char        *cur_obj_name;         /* Name of current object         */
extern FILE        *cur_file;             /* current file pointer           */
extern char        *cur_file_name;        /* Name of the current file       */

extern DEF_EXT     *def_ext_head;         /* First Element in Defexternal
                                             list                           */
extern EX_FUNC     *ex_funcs_head;        /* First element in external list */

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern float  TKNNUMBER;                  /* gettoken number return  */
extern char  *TKNWORD;                    /* gettoken string storage */


/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined here for internal use  ============== */

DEF_EXT *current_de;


/* ======================================================================= */
/*                  FUNCTIONS TO PROCESS DEFEXTERNALS                      */
/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_defexternal()
 *
 *  PURPOSE: This function reads and stores in the
 *           appropriate data structures all of the
 *           information defined in a defexternal
 *           construct.
 *
 *  INPUTS:  None.
 *
 *  RETURNS: An integer, -99 (RESTART) if it got
 *           hopelessly confused while trying to
 *           read the defexternal, a -1 if it found
 *           some kind of syntax error, otherwise, a
 *           1 (OK).
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 */

int process_defexternal()
{
   int                    token;
   struct word_list       *relation;
   DEF_EXT                *cur_def_ex_funcs;

   /*============================*/
   /* Check for defexternal name */
   /*============================*/

   token = gettoken(cur_file);

   IF(token NEQ WORD) THEN
      error_message(ERROR, "Expected a name following defexternal keyword!");
      find_rht_paren();
      return(ERROR);
   END_IF

   cur_obj_name  = TKNWORD;

   IF(VERBOSE IS_ON) THEN
      sprintf(msg_buf,"\n\nProcessing defexternal: %.40s",cur_obj_name);
      send_message(msg_buf,NO);
   END_IF

   /*============================================*/
   /* Check if function has been previously used */
   /*============================================*/

   IF(find_ex_func(ex_funcs_head, cur_obj_name) NEQ NULL) THEN
      sprintf(msg_buf, "External function %.40s defined after being used.\n",
                    cur_obj_name);
      error_message(WARNING, msg_buf);
   END_IF

   /*======================================*/
   /* Get pointer to defexternal structure */
   /*======================================*/
   def_ext_head     = find_create_def_ex_func(def_ext_head, cur_obj_name);
   cur_def_ex_funcs = current_de;

   /*===================================*/
   /* Begin processing defexternal info */
   /*===================================*/

   token = gettoken(cur_file);

   IF(token NEQ LPAREN) THEN
      error_message(ERROR, "Expecting a left paren inside a defexternal");
      return(RESTART);
   END_IF


   /*=================================*/
   /* Check for ART style defexternal */
   /*=================================*/

   IF((token EQ BWORD) OR (token EQ BWORDS)) THEN
      find_rht_paren();                   /* Ignore ART style code */
      find_rht_paren();                   /* Provided for compatibility */
      return(ERROR);                      /* Return error, not restart */
   END_IF

   /*===================================*/
   /* Read and process defexternal info */
   /*===================================*/

   while(token NEQ RPAREN) DO

      /*======================================*/
      /* Find opening paren, and get key word */
      /*======================================*/

      IF(token NEQ LPAREN) THEN
         error_message(ERROR, "Expecting a left paren inside a defexternal");
         return(RESTART);
      END_IF

      token = gettoken(cur_file);
      IF((token EQ WORD) AND
         (strcmp("true-function-name",TKNWORD) EQ 0)) THEN

           /*============================================*
            *  Process true function name                *
            *============================================*/

           token = gettoken(cur_file);
           IF((token NEQ WORD) AND (token NEQ STRING)) THEN
             error_message(ERROR, "Expecting a word or a string for ");
             send_message("true-function-name in defexternal",NO);
           ELSE
             cur_def_ex_funcs->true_name = TKNWORD;
           END_IF
           find_rht_paren();

      ELSE_IF((token EQ WORD) AND (strcmp("asserts",TKNWORD) EQ 0))

          /*===========================================================*
           *  Process the relations asserted by the external function  *
           *===========================================================*/

           token = gettoken(cur_file);
           IF(token == RPAREN) THEN
               error_message(ERROR, "Expecting a word, string, ?NONE ");
               send_message("or ?VARIABLE after asserts in defexternal",NO);
           END_IF

           while (token NEQ RPAREN) DO
              IF(token EQ BWORD) THEN
                 IF((strcmp(TKNWORD,"VARIABLE") NEQ 0) AND
                    (strcmp(TKNWORD,"NONE") NEQ 0)) THEN
           		     error_message(ERROR,
           		        "Unidentified word, expected VARIABLE or NONE after ?");
           		 END_IF

              ELSE_IF((token NEQ WORD) AND (token NEQ STRING))
                 error_message(ERROR, "Expecting a word, string, ?NONE, or ");
                 send_message("?VARIABLE after asserts in defexternal",NO);
                 find_rht_paren();

              ELSE
                 relation = alloc_wl();
                 relation->word = TKNWORD;
                 relation->next_word = cur_def_ex_funcs->assert_list;
                 cur_def_ex_funcs->assert_list = relation;
              END_IF
              token = gettoken(cur_file);
           END_WHILE

     ELSE_IF((token EQ WORD) AND (strcmp("retracts",TKNWORD) EQ 0))

          /*============================================================*
           *  Process the relations retracted by the external function  *
           *============================================================*/

           token = gettoken(cur_file);
           IF(token == RPAREN) THEN
               error_message(ERROR, "Expecting a word, string, ?NONE ");
               send_message("or ?VARIABLE after retracts in defexternal",NO);
           END_IF

           while (token NEQ RPAREN) DO
              IF(token EQ BWORD) THEN
                 IF((strcmp(TKNWORD,"VARIABLE") NEQ 0) AND
                    (strcmp(TKNWORD,"NONE") NEQ 0)) THEN
           		     error_message(ERROR,
           		        "Unidentified word, expected VARIABLE or NONE after ?");
           		 END_IF

              ELSE_IF((token NEQ WORD) AND (token NEQ STRING))
                 error_message(ERROR, "Expecting a word, string, ?NONE, or ");
                 send_message("?VARIABLE after retracts in defexternal",NO);
                 find_rht_paren();

              ELSE
                 relation = alloc_wl();
                 relation->word = TKNWORD;
                 relation->next_word = cur_def_ex_funcs->retract_list;
                 cur_def_ex_funcs->retract_list = relation;
              END_IF
                 token = gettoken(cur_file);
           END_WHILE

      ELSE_IF((token EQ WORD) AND (strcmp("min-number-of-args", TKNWORD) EQ 0))

         /*=================================*/
         /* Process min-number-of-arguments */
         /*=================================*/

         token = gettoken(cur_file);
         IF((token EQ BWORD) AND (strcmp("VARIABLE", TKNWORD) EQ 0)) THEN
            cur_def_ex_funcs->min_arguments = 0;
         ELSE_IF(token NEQ NUMBER)
            error_message(ERROR, 
               "Expecting a number for argument min-number-of-argument in defexternal");
         ELSE
            cur_def_ex_funcs->min_arguments = TKNNUMBER;
         END_IF
         find_rht_paren();

      ELSE_IF((token EQ WORD) AND (strcmp("max-number-of-args", TKNWORD) EQ 0))

         /*=================================*/
         /* Process max-number-of-arguments */
         /*=================================*/

         token = gettoken(cur_file);
         IF((token EQ BWORD) AND (strcmp("VARIABLE", TKNWORD) EQ 0)) THEN
            cur_def_ex_funcs->max_arguments = 10000;
         ELSE_IF(token NEQ NUMBER)
            error_message(ERROR, 
               "Expecting a number for argument max-number-of-arguments definition");
         ELSE
            cur_def_ex_funcs->max_arguments = TKNNUMBER;
         END_IF
         find_rht_paren();

      ELSE_IF((token EQ WORD) AND  (strcmp("return-type",TKNWORD) == 0))

         /*================================*
          * Process return-type            *
          *================================*/

          token = gettoken(cur_file);
          IF(token NEQ WORD) THEN
             error_message(ERROR, "Expecting NUMBER, STRING, WORD, ");
             send_message("MULTI, or NONE for return type",NO);
          ELSE
	         IF(strcmp(TKNWORD,"MULTI") EQ 0) THEN
	            cur_def_ex_funcs->return_word   = YES;
	            cur_def_ex_funcs->return_string = YES;
	            cur_def_ex_funcs->return_number = YES;

	         ELSE_IF((strcmp(TKNWORD,"FLOAT") EQ 0) OR
	                 (strcmp(TKNWORD,"NUMBER") EQ 0) OR
	                 (strcmp(TKNWORD,"INTEGER") EQ 0))
	         		cur_def_ex_funcs->return_number = YES;

	         ELSE_IF((strcmp(TKNWORD,"NONE")) EQ 0)
	            cur_def_ex_funcs->return_word   = NO;
	            cur_def_ex_funcs->return_string = NO;
	            cur_def_ex_funcs->return_number = NO;

	         ELSE_IF(strcmp(TKNWORD,"STRING") EQ 0)
	         		cur_def_ex_funcs->return_string = YES;

	         ELSE_IF((strcmp(TKNWORD,"CHARACTER") EQ 0) OR
	                 (strcmp(TKNWORD,"WORD") EQ 0))
	         		cur_def_ex_funcs->return_word = YES;
	         ELSE
                error_message(ERROR, "Expecting NUMBER, STRING, WORD, ");
                send_message("MULTI, or NONE for return type",NO);
	         END_IF
	      END_IF

          if(token != RPAREN)
             find_rht_paren();

      ELSE_IF((token EQ WORD) AND (strcmp("argument", TKNWORD) EQ 0))

         /*=============================== */
         /* Process an argument definition */
         /*=============================== */
         (void)process_argument_def(cur_def_ex_funcs);

      ELSE

         /*========================*/
         /* Unrecognized key word! */
         /*========================*/

         sprintf(msg_buf,
            "Unrecognized definition: %.40s inside a defexternal", TKNWORD);
         error_message(ERROR, msg_buf);
         find_rht_paren();
      END_IF

      token = gettoken(cur_file);
   END_WHILE
  return(OK);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_create_def_ex_func()
 *
 *  PURPOSE: This function searches through a list of
 *           defexternals to find a specific name. If it doesn't
 *           find an entry with that name, it creates one.
 *
 *  INPUTS : A pointer to the list of defexternals (DEF_EX_FUNC *)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns a
 *           pointer to the newly created structure.
 * ------------------------------------------------------------------
 */

static DEF_EXT *find_create_def_ex_func(node, name)
DE_PTR  node;
char   *name;
{
   int rtn;

   IF(node EQ NULL) THEN                       /* A new defexternal! */
      node        = alloc_def_ex_func();
      node->name  = name;
      node->file  = cur_file_name;
      current_de  = node;
   ELSE
      rtn = strcmp(name, node->name);
      IF(rtn < 0) THEN
         node->lft_def_ext = find_create_def_ex_func(node->lft_def_ext, name);
      ELSE_IF(rtn > 0)
         node->rht_def_ext = find_create_def_ex_func(node->rht_def_ext, name);
      ELSE
         duplicate_def_ex(name);
         node->max_arguments = 10000;
         node->min_arguments = 0;
         node->file       = cur_file_name;
         current_de       = node;
      END_IF
   END_IF

   return(node);
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : duplicate_def_ex()
 *
 *  PURPOSE: This function merely prints an error message if
 *           a duplicate external function definition is found.
 *
 *  INPUTS : A pointer to the external function name.
 *
 *  RETURNS: Nothing
 *
 *  NOTES  : This function is called to avoid putting a large
 *           string buffer on the stack many times.
 * ------------------------------------------------------------------
 */

static void duplicate_def_ex(name)
char *name;
{
   sprintf(msg_buf,
      "Duplicate function definition for <%.40s>, erasing old info", name);
   error_message(WARNING, msg_buf);

   return;
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : find_def_ex_func()
 *
 *  PURPOSE: This function searches through a list of
 *           defexternals to find a specific name.
 *
 *  INPUTS : A pointer to the list of defexternals (DEF_EX_FUNC *)
 *           and a pointer to the name to search for (char *)
 *
 *  RETURNS: If the name is found, it returns a pointer to
 *           that structure. If it isn't found it returns NULL.
 * ------------------------------------------------------------------
 */

DEF_EXT *find_def_ex_func(node, name)
DE_PTR  node;
char   *name;
{
   int rtn;

   IF(node EQ NULL) THEN                       /* A new defexternal! */
      return(NULL);
   ELSE
      rtn = strcmp(name, node->name);
      IF(rtn < 0) THEN
         return(find_def_ex_func(node->lft_def_ext, name));
      ELSE_IF(rtn > 0)
         return(find_def_ex_func(node->rht_def_ext, name));
      ELSE
         return(node);                         /* An old defexternal! */
      END_IF
   END_IF
}

/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : process_argument_def()
 *
 *  PURPOSE: This function reads and stores in the
 *           appropriate data structures all of the
 *           information about arguments from a defexternal.
 *
 *  INPUTS:  A pointer to the current defexternal.
 *
 *  RETURNS: An integer, either OK or ERROR.
 *
 *  NOTES:   None.
 * ------------------------------------------------------------------
 */

static int process_argument_def(cur_def_ex_func)
DEF_EXT *cur_def_ex_func;
{
   int token;
   DA_PTR cur_def_arg;

   token = gettoken(cur_file);

   /*=======================*/
   /* Check position number */
   /*=======================*/

   IF(token NEQ NUMBER) THEN
      error_message(ERROR, "Argument keyword must be followed by a number");
      find_rht_paren();
      return(ERROR);
   ELSE_IF((TKNNUMBER < 1) OR (TKNNUMBER > cur_def_ex_func->max_arguments))
      sprintf(msg_buf,"Argument position, %d, is too big or too small",
                  (int) TKNNUMBER);
      error_message(ERROR, msg_buf);
      find_rht_paren();
      return(ERROR);
   END_IF

   /*=================================================*/
   /* Check if argument postion has already been used */
   /*=================================================*/

   cur_def_arg = find_def_arg((int) TKNNUMBER, cur_def_ex_func->arguments);

   IF(cur_def_arg NEQ NULL) THEN
      error_message(WARNING, "Duplicate argument position, redefining values");

      free_wl(cur_def_arg->possible_words);
      free_wl(cur_def_arg->possible_strings);
      free_nl(cur_def_arg->possible_numbers);

      cur_def_arg->possible_words   = NULL;
      cur_def_arg->possible_strings = NULL;
      cur_def_arg->possible_numbers = NULL;
      cur_def_arg->allow_word       = YES;
      cur_def_arg->allow_string     = YES;
      cur_def_arg->allow_number     = YES;
      cur_def_arg->set_max          = NO;
      cur_def_arg->set_min          = NO;
   ELSE
      cur_def_arg             = alloc_def_fld();
      cur_def_arg->position   = (int) TKNNUMBER;
      cur_def_arg->next_field = cur_def_ex_func->arguments;
      cur_def_ex_func->arguments     = cur_def_arg;
   END_IF


   /*==========================================*/
   /* Argument found, start examining keywords */
   /*==========================================*/

   token = gettoken(cur_file);

   while(token NEQ RPAREN) DO

      /*======================================*/
      /* Find opening paren, and get key word */
      /*======================================*/

      IF(token NEQ LPAREN) THEN
         error_message(ERROR,
            "Expecting a left paren inside argument definition");
         return(RESTART);
      END_IF

      token = gettoken(cur_file);

      /*===========================*/
      /* Process argument keywords */
      /*===========================*/

      IF((token EQ WORD) AND (strcmp("type", TKNWORD) EQ 0)) THEN
         process_fld_type(cur_def_arg);

      ELSE_IF((token EQ WORD) AND (strcmp("allowed-strings", TKNWORD) EQ 0))
		   process_fld_string_list(cur_def_arg);
      ELSE_IF((token EQ WORD) AND (strcmp("allowed-words", TKNWORD) EQ 0))
         process_fld_word_list(cur_def_arg);

      ELSE_IF((token EQ WORD) AND (strcmp("allowed-numbers", TKNWORD) EQ 0))
         IF((cur_def_arg->set_min IS_YES) OR
            (cur_def_arg->set_max IS_YES)) THEN
            error_message(WARNING,
               "Both RANGE and ALLOWED_NUMBERS have been defined for this ");
            send_message
               ("argument.\n        The RANGE definition will be ignored",YES);
            cur_def_arg->set_min   = NO;
            cur_def_arg->set_max   = NO;
            cur_def_arg->min_range = -1000;
            cur_def_arg->max_range = 1000;
         END_IF
          process_fld_number_list(cur_def_arg);

      ELSE_IF((token EQ WORD) AND (strcmp("range", TKNWORD) EQ 0))
         IF(cur_def_arg->possible_numbers NEQ NULL) THEN
            error_message(WARNING,
               "Both RANGE and ALLOWED_NUMBERS have been defined for this ");
            send_message
               ("argument.\n        The RANGE definition will be ignored",YES);
            find_rht_paren();
         ELSE
            process_fld_range(cur_def_arg);
            /*process_arg_range(cur_def_arg);*/
         END_IF

      ELSE

         /*========================*/
         /* Unrecognized key word! */
         /*========================*/

         sprintf(msg_buf,
            "Unrecognized key word: %.40s inside a argument definition",
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
 *  NAME   : find_def_arg()
 *
 *  PURPOSE: This function searches through a list of
 *           argument definitions to find a specific argument number.
 *
 *  INPUTS : A pointer to the list of argument definitions (DA_PTR *)
 *           and a argument number to search for (int).
 *
 *  RETURNS: If the number is found, it returns a pointer to
 *           that structure. If it isn't found, it returns NULL.
 * ------------------------------------------------------------------
 */

static DA_PTR find_def_arg(number, list)
int number;
DA_PTR   list;
{
   DA_PTR temp;

   temp = list;
   while(temp NEQ NULL) DO
      IF(number EQ temp->position) THEN
         return(temp);
      END_IF

      temp = temp->next_field;
   END_WHILE

   return(NULL);
}




/* ------------------------------------------------------------------
 *  NAME   : copy_argument_info()
 *
 *  PURPOSE: This function copies the argument info from the current
 *           argument into a new def_argument structure and returns a
 *           pointer to that structure.
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the function name, (char *)
 *           the position in the pattern, (int)
 *
 *  RETURNS: A pointer to the new structure (DEF_ARG *)
 *
 *  NOTES:   Does not check argument number here!
 * ------------------------------------------------------------------
 */

DEF_ARG *copy_argument_info(ex_func_name,argument_pos)
   char *ex_func_name;
   int   argument_pos;
{
   DEF_ARG *new_def_arg,*cur_arg;
   DEF_EXT *ex_func;
   WL_PTR   wl_ptr;
   NL_PTR   nl_ptr;

   /*===========================*/
   /* Find defexternal (if any) */
   /*===========================*/

   ex_func = find_def_ex_func(def_ext_head, ex_func_name);

   /*=======================================*/
   /* No defexternal, flag error and return */
   /*=======================================*/

   IF(ex_func EQ NULL) THEN
      def_ext_head = find_create_def_ex_func(def_ext_head, ex_func_name);
      sprintf(msg_buf,
         "External function <%.40s> used prior to its definition.",
         ex_func_name);
      error_message(WARNING, msg_buf);
      return(NULL);
   END_IF

   cur_arg = find_def_arg(argument_pos, ex_func->arguments);
   IF(cur_arg EQ NULL) THEN

      /*================================*/
      /* This argument not defined, return */
      /*================================*/

      return(NULL);
   END_IF

   new_def_arg = alloc_def_fld();

   /*================*/
   /* Copy type info */
   /*================*/

   new_def_arg->allow_word   = cur_arg->allow_word;
   new_def_arg->allow_string = cur_arg->allow_string;
   new_def_arg->allow_number = cur_arg->allow_number;

   /*================*/
   /* Copy word list */
   /*================*/

   for (wl_ptr = cur_arg->possible_words; wl_ptr NEQ NULL;
         wl_ptr = wl_ptr->next_word) DO
      new_def_arg->possible_words =
         add_to_word_list(new_def_arg->possible_words, wl_ptr->word, WORD,NO);
   END_FOR

   /*==================*/
   /* Copy string list */
   /*==================*/

   for (wl_ptr = cur_arg->possible_strings; wl_ptr NEQ NULL;
         wl_ptr = wl_ptr->next_word) DO
      new_def_arg->possible_strings =
         add_to_word_list(new_def_arg->possible_strings, wl_ptr->word, STRING,
            NO);
   END_FOR

   /*==================*/
   /* Copy number list */
   /*==================*/

   for (nl_ptr = cur_arg->possible_numbers; nl_ptr NEQ NULL;
         nl_ptr = nl_ptr->next_number) DO
      new_def_arg->possible_numbers =
         add_to_number_list(new_def_arg->possible_numbers, nl_ptr->number, NO);
   END_FOR

   /*=================*/
   /* Copy range info */
   /*=================*/

   new_def_arg->set_max   = cur_arg->set_max;
   new_def_arg->set_min   = cur_arg->set_min;
   new_def_arg->max_range = cur_arg->max_range;
   new_def_arg->min_range = cur_arg->min_range;

   return(new_def_arg);
}


/* ======================================================================= */
/*                  FUNCTIONS TO CHECK DEFEXTERNAL INFO                    */
/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : check_argument_info()
 *
 *  PURPOSE: This function compares info from functions
 *           used in a rule or deffact to the defexternal
 *           paramaters and prints appropriate error
 *           messages.
 *
 *  INPUTS:  Four arguments,
 *           A pointer to the function name, (char *)
 *           the type of the value, WORD, STRING, NUMBER,(int)
 *           the position in the pattern, (int)
 *           and the literal value of the argument (char *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   Does not check argument number here!
 * ------------------------------------------------------------------
 */

void check_argument_info(name, type, position, value)
char *name, *value;
int   type,  position;
{
   DEF_EXT  *def_ex_func;
   DEF_ARG  *arg;
   float     number;

   /*===========================*/
   /* Find defexternal (if any) */
   /*===========================*/

   def_ex_func = find_def_ex_func(def_ext_head, name);

   /*=======================================*/
   /* No defexternal, flag error and return */
   /*=======================================*/

   IF(def_ex_func EQ NULL) THEN
      def_ext_head = find_create_def_ex_func(def_ext_head, name);
      return;
   END_IF

   /*=====================*/
   /* Check argument info */
   /*=====================*/
   arg = find_def_arg(position, def_ex_func->arguments);
   IF(arg EQ NULL) THEN

      /*===================================*/
      /* This argument not defined, return */
      /*===================================*/

      return;
   ELSE

      /*========================================*/
      /* Check argument type and literal values */
      /*========================================*/

      switch (type) {

         case WORD:
              /*=====================*/
              /* Check WORDS allowed */
              /*=====================*/

              IF(arg->allow_word NEQ YES) THEN
                 sprintf(msg_buf,
                    "Used a WORD in argument %d, which isn't allowed",
                    position);
                 error_message(ERROR, msg_buf);
                 sprintf(msg_buf, "\n        by defexternal %.40s", name);
                 send_message(msg_buf,NO);

              /*=================*/
              /* Check WORD list */
              /*=================*/

              ELSE_IF(arg->possible_words NEQ NULL)
                 IF(is_word_in_list(arg->possible_words, value) IS_NO) THEN
                    sprintf(msg_buf,
                       "Used a WORD (%.40s) in argument %d that is not in the",
                            value, position);
                    error_message(ERROR, msg_buf);
                    send_message("\n        ALLOWED_WORDS list",NO);
                 END_IF
              END_IF
              break;


         case STRING:
              /*=======================*/
              /* Check STRINGS allowed */
              /*=======================*/

              IF(arg->allow_string NEQ YES) THEN
                 sprintf(msg_buf,
                    "Used a STRING in argument %d, which isn't allowed",
                    position);
                 error_message(ERROR,msg_buf);
                 sprintf(msg_buf, "\n        by defexternal %.40s", name);
                 send_message(msg_buf,NO);

              /*====================*/
              /* Check STRINGS list */
              /*====================*/

              ELSE_IF(arg->possible_strings NEQ NULL)
                 IF(is_word_in_list(arg->possible_strings, value) IS_NO) THEN
                    sprintf(msg_buf,
                    "Used a STRING (%.40s) in argument %d that is not in the",
                            value, position);
                    error_message(ERROR, msg_buf);
                    send_message("\n        ALLOWED_STRINGS list",NO);
                 END_IF
              END_IF
              break;


         case NUMBER:
              /*=======================*/
              /* Check NUMBERS allowed */
              /*=======================*/

              IF(arg->allow_number NEQ YES) THEN
                 sprintf(msg_buf,
                         "Used a NUMBER in argument %d, which isn't allowed",
                         position);
                 error_message(ERROR,msg_buf);
                 sprintf(msg_buf, "\n        by defexternal %.40s", name);
                 send_message(msg_buf,NO);
              ELSE
                 /*====================*/
                 /* Check NUMBERS list */
                 /*====================*/

                 number = (float) atof(value);
                 IF(arg->possible_numbers NEQ NULL) THEN
                    IF(is_number_in_list(arg->possible_numbers,
                                         number) IS_NO) THEN
                       sprintf(msg_buf,
                          "Used a NUMBER (%.40s) in argument %d that is not ",
                          value, position);
                       error_message(ERROR, msg_buf);
                       send_message("in the\n        ALLOWED_NUMBERS list",NO);
                    END_IF
                 END_IF

                 /*=====================*/
                 /* Check NUMBERS range */
                 /*=====================*/

                 IF(arg->set_max EQ YES) THEN
                    IF(number > arg->max_range) THEN
                       sprintf(msg_buf,
                          "Used a value (%f) in argument %d greater than range max (%f)", 
                               number, position, arg->max_range);
                       error_message(ERROR, msg_buf);
                    END_IF
                 END_IF

                 IF(arg->set_min EQ YES) THEN
                    IF(number < arg->min_range) THEN
                       sprintf(msg_buf,
                          "Used a value (%f) in argument %d smaller than range min (%f)", 
                               number, position, arg->min_range);
                       error_message(ERROR, msg_buf);
                    END_IF
                 END_IF
              END_IF

              break;
         }
   END_IF
}

/* ------------------------------------------------------------------
 *  NAME   : check_var_argument_info()
 *
 *  PURPOSE: This function checks to see that a variable is used
 *           in a argument whose defexternal info is consistent with
 *           the info from arguments in which the variable was used
 *           previously.
 *
 *  INPUTS:  Four arguments,
 *           A pointer to the function name, (char *)
 *           a pointer to the variable name, (char *),
 *           a pointer to the structure containing the cumulative
 *              argument info for that variable (DEF_ARG *),
 *           and the position in the pattern, (int).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   Does not check argument number here!
 * ------------------------------------------------------------------
 */

void check_var_argument_info(ex_func_name, var_name, var_arg, position,side)
   char    *ex_func_name, *var_name;
   DEF_ARG *var_arg;
   int      position;
   short    side;
{
   DEF_EXT  *def_ex_func;
   DEF_ARG  *arg;
   int       found_common;
   int       num_in_common,tot_num_in_common = 0;

   /*===========================*/
   /* Find defexternal (if any) */
   /*===========================*/

   def_ex_func = find_def_ex_func(def_ext_head, ex_func_name);

   /*=======================================*/
   /* No defexternal, flag error and return */
   /*=======================================*/

   IF(def_ex_func EQ NULL) THEN
      def_ext_head = find_create_def_ex_func(def_ext_head, ex_func_name);
      sprintf(msg_buf,
         "External function <%.40s> used prior to its definition.",
         ex_func_name);
      error_message(WARNING, msg_buf);
      return;
   END_IF

   /*=====================*/
   /* Check argument info */
   /*=====================*/

   arg = find_def_arg(position, def_ex_func->arguments);
   IF(arg EQ NULL) THEN

      /*===================================*/
      /* This argument not defined, return */
      /*===================================*/

      return;
   END_IF

   /*============================*/
   /* Compare data types allowed */
   /*============================*/

   found_common = compare_fld_types(var_arg,arg);
   IF(found_common IS_NO) THEN
      IF(side == LHS) THEN
         error_message(WARNING,
           "Due to the inconsistency of the field definitions, this rule MAY NOT fire ");
         sprintf(msg_buf, "\n        variable <%.40s> has incompatible type definitions",
            var_name);
         send_message(msg_buf,YES);
      ELSE
         sprintf(msg_buf,
            "Variable <%.40s> is of the wrong type for argument %d ",
            var_name, arg->position);
         error_message(ERROR, msg_buf);
         sprintf(msg_buf,"\n        of defexternal <%.40s>", ex_func_name);
         send_message(msg_buf,NO);	
      END_IF

   /*===========================================================*/
   /* If data types are not exclusive, look at lists and ranges */
   /*===========================================================*/

   ELSE

      /*====================*/
      /* Compare word lists */
      /*====================*/

      num_in_common = compare_fld_word_lists(var_name,var_arg,arg);
      if (num_in_common == 0)
       {
          sprintf(msg_buf,
          "Variable <%.40s> used in arguments whose ALLOWED_WORDS definitions", 
              var_name);
          error_message(ERROR, msg_buf);
          send_message("\n        have no common values",NO);
      }
     else if (num_in_common > 0)
      tot_num_in_common += num_in_common;
      /*======================*/
      /* Compare string lists */
      /*======================*/

      num_in_common = compare_fld_string_lists(var_name,var_arg,arg);
      if (num_in_common == 0)
       {
          sprintf(msg_buf,
          "Variable <%.40s> used in arguments whose ALLOWED_STRINGS definitions", 
              var_name);
          error_message(ERROR, msg_buf);
          send_message("\n        have no common values",NO);
      }
     else if (num_in_common > 0)
      tot_num_in_common += num_in_common;
      /*=================*/
      /* Compare numbers */
      /*=================*/

     num_in_common = compare_fld_numbers(var_name,var_arg,arg);
     if (num_in_common > 0)
      tot_num_in_common += num_in_common;
      /*=====================================================*/
      /* If no common values found in ANY of the categories, */
      /*    print error message                              */
      /*=====================================================*/

      IF((tot_num_in_common EQ 0) AND (side EQ LHS)) THEN
	     error_message(ERROR,
                "Argument definitions won't allow this rule to fire since ");
	     sprintf(msg_buf,
                "variable\n        <%.40s> has no common allowed values",
                var_name);
	     send_message(msg_buf,NO);

      /*============================================*/
      /* Otherwise, if only one common value found, */
      /*    print warning message                   */
      /*============================================*/

      ELSE_IF((tot_num_in_common EQ 1) AND (side EQ LHS))
         sprintf(msg_buf,
         "Argument definitions allow variable <%.40s> only one possible value",
                 var_name);
         error_message(WARNING,msg_buf);
      END_IF
   END_IF
}


/* ======================================================================= */

/* ------------------------------------------------------------------
 *  NAME   : check_num_arguments()
 *
 *  PURPOSE: This function compares the number of arguments
 *           used in a rule or deffact to the defexternal
 *           paramaters and prints appropriate error
 *           messages (if needed).
 *
 *  INPUTS:  Two arguments,
 *           A pointer to the function name, (char *)
 *           the number of arguments in the pattern (int).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES:   The number of arguments paramater is negative if a
 *           multiple argument variable was used in the pattern.
 * ------------------------------------------------------------------
 */

/*^void check_min_num_arguments(name, org_arguments)
char *name;
int   org_arguments;
{
   DEF_EXT  *def_ex_func;
   int       num_arguments, USED_MULTI;

*/   /*===========================*/
   /* Find defexternal (if any) */
   /*===========================*/

/*^   def_ex_func = find_def_ex_func(def_ext_head, name);

*/   /*=======================================*/
   /* No defexternal, flag error and return */
   /*=======================================*/

/*^   IF(def_ex_func EQ NULL) THEN
      def_ext_head = find_create_def_ex_func(def_ext_head, name);
      sprintf(msg_buf,
         "External function <%.40s> used prior to its definition.",
         name);
      error_message(WARNING, msg_buf);
      return;
   END_IF

*/   /*===========================*/
   /* Check for multi-argument use */
   /*===========================*/

/*^   IF(org_arguments < 0) THEN
      num_arguments = abs(org_arguments);
      USED_MULTI = YES;
   ELSE
      num_arguments = org_arguments;
      USED_MULTI = NO;
   END_IF

*/   /*=====================*/
   /* Check min arguments */
   /*=====================*/

/*^   IF((def_ex_func->min_arguments > num_arguments) AND (USED_MULTI IS_NO)) THEN
      error_message(ERROR, "Fewer arguments used than defexternal requires");
   END_IF

*/   /*=====================*/
   /* Check max arguments */
   /*=====================*/

/*^   IF((def_ex_func->max_arguments < num_arguments) AND (USED_MULTI IS_NO)) THEN
      error_message(ERROR, "More arguments used than defexternal allows");
   ELSE_IF((def_ex_func->max_arguments EQ num_arguments) AND
           (USED_MULTI IS_YES))
      error_message(WARNING, "Unecessary $? variable used");
   END_IF
}
*/
