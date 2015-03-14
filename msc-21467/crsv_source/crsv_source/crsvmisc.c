#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "crsv.h"

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void     error_message();
extern void     send_message();

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern int      gettoken();

/* -----------------------------
 *  From the file: CRSVHASH.C
 * -----------------------------
 */

extern HASH    *add_symbol();

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern WL_PTR     alloc_wl();
extern NL_PTR     alloc_nl();
extern TN_PTR     alloc_token_node();

/* -----------------------------
 *  From the file: CRSVPC.C
 * -----------------------------
 */

#if IBM_TBC || IBM_MSC
extern int        pc_window();
#endif


/* -----------------------------
 *  From the file: CRSVUNIX.C
 * -----------------------------
 */

#if UNIX_V
extern int        pc_window();
#endif


/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern void       free_token_node();


/* -----------------------------
 *  From the system:
 * -----------------------------
 */

extern double atof();


/* ===========  Functions defined here for Global use  ================ */

int      check_inputs();
int      check_top_construct();
int      find_top_construct();
void     find_rht_paren();
WL_PTR   add_to_word_list();
NL_PTR   add_to_number_list();
int      is_word_in_list();
int      is_number_in_list();
FLD_PTR  sort_field_list();

void    set_CHECK_RULES();
void    set_CHECK_RELATIONS();
void    set_CHECK_EX_FLAG();
void    set_CHECK_COMMENTS();
void    set_VERBOSE();
void    set_CHECK_STYLE();
void    set_CREATE_DEFRELS();
void    set_CHECK_DEFRELS();
void    set_CHECK_DEBUG();
void    set_ANALYZE_TRACE();
void    set_LITERAL();

void queue_token ();
TK_PTR dequeue_token ();

/* ===========  Functions defined here for internal use  ============== */

static char *extract_arg();
static void  display_options();


/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern FILE     *cur_file;                    /* Pointer to current file */
extern int      MAX_SINGLE_LIST;


/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern float  TKNNUMBER;                    /* gettoken number return  */
extern char  *TKNWORD;                      /* gettoken string storage */


/* ===========  Variables defined here for internal use  ============== */


/* ==============================================================
 * ================  Miscellaneous Functions  ===================
 * ==============================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :     queue_token
 *
 *  PURPOSE:     Adds a token to a specified queue.
 *
 *  INPUTS :     A doubly dereferenced pointer to the head of
 *               the queue, the pointer to the token being added.
 *
 *  RETURNS:     None
 * -------------------------------------------------------------
 */

void queue_token (token_list_ptr, token)
 TN_PTR *token_list_ptr;
 TK_PTR token;
 {
   TN_PTR new_node = alloc_token_node();

   new_node->token = token;

   IF (*token_list_ptr EQ NULL) THEN
      *token_list_ptr = new_node;
   ELSE
      TN_PTR tail = *token_list_ptr;

      WHILE (tail->next NEQ NULL) DO
         tail = tail->next;
      DONE

      tail->next = new_node;
   END_IF
 }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :     dequeue_token
 *
 *  PURPOSE:     Removes (and returns) the oldest token in a
 *               queue.
 *
 *  INPUTS :     A doubly dereferenced pointer to the head of
 *               the queue.
 *
 *  RETURNS:     The pointer to the token which was removed from
 *               the queue.
 * -------------------------------------------------------------
 */

TK_PTR dequeue_token (token_list_ptr)
 TN_PTR *token_list_ptr;
 {
   TN_PTR old_head = *token_list_ptr;
   TK_PTR token = (*token_list_ptr)->token;

   *token_list_ptr = (*token_list_ptr)->next;
   free_token_node (old_head);

   return (token);
 }


/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :      check_inputs
 *
 *  PURPOSE:      This function checks to see that all command
 *                line options are valid, that enough arguments
 *                are input, and builds the list of files to be
 *                processed.
 *
 *  INPUTS:
 *   int   *argc         Pointer to the number of arguments on
 *                       the command line.
 *   char *argv[]        An array of pointers to the arguments.
 *   char **c_file       Pointer to character pointer which is
 *                       to point at the file name for storing
 *                       defrelations.
 *   char **t_file       Pointer to character pointer which is
 *                       to point at the dribble file name.
 *
 *  RETURNS:       An integer flag. ERROR (-1) if an error is
 *                 found, or OK (0) if all arguments are OK.
 *                 Also, argv and argc are modified to contain
 *                 only the names of files to be processed.
 * -------------------------------------------------------------
 */

int check_inputs(argc, argv, c_file, t_file)
int *argc;
char **argv, **c_file, **t_file;
{
   int i, j;

   IF((*argc EQ 2) AND (strcmp("?", argv[1]) EQ 0)) THEN
      display_options();
      return(ERROR);
   END_IF

   /*=============================*/
   /* Set default flag conditions */
   /*=============================*/

   ANALYZE_TRACE   = OFF;             /*  Option flag T  */
   CREATE_DEFRELS  = OFF;             /*  Option flag C  */
   CHECK_DEFRELS   = ON;              /*  Option flag D  */
   CHECK_EX_FLAG   = OFF;              /*  Option flag E  */
   LITERAL         = OFF;             /*  Option flag L  */
   CHECK_RULES     = OFF;              /*  Option flag R  */
   CHECK_STYLE     = ON;              /*  Option flag S  */
   VERBOSE         = OFF;             /*  Option flag V  */
   CHECK_RELATIONS = OFF;              /*  Option flag X  */
   CHECK_DEBUG     = OFF;             /*  Option flag Z  */
   CHECK_COMMENTS  = OFF;             /*  Not used currently, may use later */

   FOR (i = 1; i < *argc;) DO
      IF(argv[i][0] EQ '-') THEN
         char *option = extract_arg (argv, argc, i);
         int suffix_arg = OFF; /* Set to ON when 'c', 'm', or 't' code found */

         FOR(j = 1; j < strlen(option); j++) DO
            char c = option[j];

            IF (isalpha(c) AND islower(c)) THEN
               c = toupper (c);
            END_IF

            IF(((c EQ 'T') OR (c EQ 'C') OR (c EQ 'M')) AND suffix_arg IS_ON) THEN
               send_message
                  ("\nSyntax error: Simultaneous 'c', 'm', and 't' options\n",NO);
               display_options();
               return(ERROR);
            ELSE_IF(c EQ 'C')
               CREATE_DEFRELS = ON;
               suffix_arg = ON;

               IF ((*c_file = extract_arg (argv, argc, i)) == NULL) THEN
                  send_message
                     ("\nNo defrelation file specified after '-c'.\n",NO);
                  display_options();
                  return (ERROR);
               END_IF
            ELSE_IF(c EQ 'D')
               CHECK_DEFRELS = OFF;
            ELSE_IF(c EQ 'E')
               CHECK_EX_FLAG = ON;
            ELSE_IF(c EQ 'H')
               display_options();
               return(ERROR);
            ELSE_IF(c EQ 'L')
               LITERAL = ON;
            ELSE_IF(c EQ 'M')
               int max;
               char *max_str = extract_arg (argv, argc, i);

               suffix_arg = ON;
               max = atoi(max_str);
               IF (max > 0) THEN
                 MAX_SINGLE_LIST = max;
               ELSE
                 send_message("\n'-m' must be followed by a max length.\n",NO);
                 display_options();
                 return(ERROR);
               END_IF
            ELSE_IF(c EQ 'R')
               CHECK_RULES = ON;
            ELSE_IF(c EQ 'S')
               CHECK_STYLE = OFF;
            ELSE_IF(c EQ 'T')
               ANALYZE_TRACE = ON;
               suffix_arg = ON;

               IF ((*t_file = extract_arg (argv, argc, i)) == NULL) THEN
                  send_message ("\nNo trace file specified after '-t'.\n",NO);

                  display_options();
                  return (ERROR);
               END_IF
            ELSE_IF(c EQ 'V')
               VERBOSE = ON;
            ELSE_IF(c EQ 'X')
               CHECK_RELATIONS = ON;
            ELSE_IF(c EQ 'Z')
               CHECK_DEBUG = ON;
            ELSE
               send_message("\nInvalid option code.",NO);
               display_options();
               return(ERROR);
            END_IF
         END_FOR
      ELSE
         i++;
      END_IF
   END_FOR

   IF(*argc <= 1) THEN
      send_message
         ("\nNot enough arguments! No file(s) to cross-reference.\n",NO);
      display_options();
      return(ERROR);
   END_IF

   return(OK);
}


/* ==================================================================== */



/*
 * -------------------------------------------------------------
 *  NAME   :  extract_arg
 *
 *  PURPOSE:  Removes an argument from the argument list
 *            specified via argv.
 *
 *  INPUTS :  The argument list and the argument to remove.
 *
 *  RETURNS:  The removed argument.
 * -------------------------------------------------------------
 */

static char *extract_arg (argv, argc, arg)
   char **argv;
   int *argc;
   int arg;
{
   char *extracted_arg = argv[arg];
   char *str1, *str2;
   int i;

   IF (arg != --(*argc)) THEN
     for(;arg < *argc;)
        {
           argv[arg] = argv[arg+1];
           arg++;
        }
      /*   bcopy((char *)(argv + arg + 1), (char *)(argv + arg),
         sizeof(argv[0]) * (*argc) - arg);*/
   END_IF

   return (extracted_arg);
}


/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  display_options
 *
 *  PURPOSE:  This function prints a list of the valid CRSV
 *            options.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  Nothing
 * -------------------------------------------------------------
 */

static void display_options()
{
   send_message("\nSyntax: crsv [-options [file-name]] rule-files ",NO);
   send_message("[-options [file-name]] ...",NO);
   send_message("\nValid options are:\n",NO);
   send_message("     r - Display rules or deffacts summary info\n",NO);
   send_message("     x - Display cross referenced relations and field info\n",NO);
   send_message("     e - Do not display external function info\n",NO);
   send_message("     c - Create a file with defrelation info\n",NO);
   send_message("         'file-name' names the output file\n",NO);
   send_message("     s - Turn off style warnings and code analysis\n",NO);
   send_message("     l - Add rule field values to defrelation defaults\n",NO);
   send_message("     m - Maximum number of single field values printed\n",NO);
   send_message("         (follow m by a space and the new max number)\n",NO);
   send_message("     d - Do not use defrelations to verify rules\n",NO);
   send_message("     t - Analyze trace file \n",NO);
   send_message("         'file-name' names the trace file\n",NO);
   send_message("? or h - Display valid options\n",NO);
   send_message("     v - Be verbose\n",NO);
   send_message("NOTE: Options 'c', 'm', and 't' may NOT be simultaneously\n",NO);
   send_message("      used in the same options argument.\n",NO);
}


/* ==================================================================== */

/*
 * ----------------------------------------------------------------
 *  NAME   :       check_top_construct
 *
 *  PURPOSE:       This function checks the input file for
 *                 the start of a rule or fact block. If neither
 *                 construct is found, an error message is
 *                 printed out.
 *
 *  INPUTS:        Two arguments,
 *                 a pointer to the input file (FILE *), and
 *                 the current token (int).
 *
 *  RETURNS:       An integer flag which means:
 *     ERROR       (-1)  if an error is found,
 *     DEFRULE     (1)   if at the beginning of a rule,
 *     DEFFACTS    (2)   if at the beginning of a fact block,
 *     DEFRELATION (3)   if at the beginning of a defrelation,
 *     DEFEXTERNAL (4)   if at the beginning of a defexternal.
 *     DEFTEMPLATE (5)   if at the beginning of a deftemplate.
 *
 *  NOTES  :       This function is called when I would normally
 *                 expect the next item to be a def... something.
 *                 It returns error if the very next thing ISN'T
 *                 a def... something.
 *
 *                 Note that the opening parenthesis is supposed
 *                 to have already been stripped off.
 * ----------------------------------------------------------------
 */

int check_top_construct(source, token)
FILE *source;
int   token;
{
   int    rtn;

   IF(token NEQ LPAREN) THEN
      error_message(ERROR,
       "Expected left parenthesis to begin defrule or deffacts statement");
      return(ERROR);
   ELSE
      token = gettoken(source);

      IF(token NEQ WORD) THEN
         rtn = ERROR;
      ELSE_IF(strcmp(TKNWORD,"deffacts") EQ 0)
         rtn = DEFFACTS;
      ELSE_IF(strcmp(TKNWORD,"defrule") EQ 0)
         rtn = DEFRULE;
      ELSE_IF(strcmp(TKNWORD,"defrelation") EQ 0)
         rtn = DEFRELATION;
      ELSE_IF(strcmp(TKNWORD,"defexternal") EQ 0)
         rtn = DEFEXTERNAL;
      ELSE_IF(strcmp(TKNWORD,"deftemplate") EQ 0)
         rtn = DEFTEMPLATE;
      ELSE
         rtn = ERROR;
      END_IF

      IF(rtn EQ ERROR) THEN
         error_message (ERROR, "Expected a defrule, deffacts, defrelation, ");
         send_message ("defexternal, or deftemplate construct but found a ",NO);
         send_message (TKNWORD,NO);
         send_message (" construct.\n        Check that all constructs ",NO);
         send_message("have the proper number of matching parentheses.\n",NO);
         return(ERROR);
      ELSE
         return(rtn);
      END_IF
   END_IF
}

/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :       find_top_construct
 *
 *  PURPOSE:       This function searches the input file for
 *                 the start of a rule or fact block. If neither
 *                 construct is found, an error message is
 *                 printed out.
 *
 *  INPUTS:        Two arguments,
 *                 a pointer to the input file (FILE *), and
 *                 the current token (int).
 *
 *  RETURNS:       An integer flag which means:
 *     ERROR       (-1)  if an error is found,
 *     DEFRULE     (1)   if at the beginning of a rule,
 *     DEFFACTS    (2)   if at the beginning of a fact block.
 *     DEFRELATION (3)   if at the beginning of a defrelation block
 *     DEFEXTERNAL (4)   if at the beginning of a defexternal block
 *     DEFTEMPLATE (5)   if at the beginning of a deftemplate block
 *
 *  NOTES  :       This function is called when I have gotten
 *                 really confused somehow. It searches all
 *                 tokens until a def... something is found. If
 *                 one is never found, it returns ERROR when
 *                 it reaches the end of the file.
 *
 *                 Note that the opening parenthesis is supposed
 *                 to have already been stripped off.
 * -------------------------------------------------------------
 */

int find_top_construct(source, token)
FILE *source;
int token;
{
   while(TRUE) DO
      IF(token EQ STOP) THEN
         return(ERROR);

      ELSE_IF(token EQ LPAREN)
         token = gettoken(source);

         IF((token EQ WORD) AND (strcmp(TKNWORD,"deffacts") EQ 0)) THEN
            return(DEFFACTS);
         ELSE_IF((token EQ WORD) AND (strcmp(TKNWORD,"defrule") EQ 0))
            return(DEFRULE);
         ELSE_IF((token EQ WORD) AND (strcmp(TKNWORD,"defrelation") EQ 0))
            return(DEFRELATION);
         ELSE_IF((token EQ WORD) AND (strcmp(TKNWORD,"defexternal") EQ 0))
            return(DEFEXTERNAL);
         ELSE_IF((token EQ WORD) AND (strcmp(TKNWORD,"deftemplate") EQ 0))
            return(DEFTEMPLATE);
         END_IF
      END_IF

      token = gettoken(source);
   END_WHILE
}

/* ======================================================================== */

/* -----------------------------------------------
 *  NAME   :         find_rht_paren
 *
 *  PURPOSE:         This function searches the
 *                   input file for an unmatched
 *                   right paren. It uses gettoken
 *                   and so is not confused by
 *                   strings.
 *
 *  INPUTS:          None.
 *
 *  RETURNS:         Nothing.
 * -------------------------------------------------
 */

void find_rht_paren()
{
   int count;
   int token;

   count = 1;

   while (count NEQ 0) DO
      token = gettoken(cur_file);
      IF(token EQ LPAREN) THEN
         count++;
      ELSE_IF(token EQ RPAREN)
         count--;
      END_IF
   END_WHILE
}

/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   : sort_field_list
 *
 *  PURPOSE: This function sorts a list of FLD_PTR's so that
 *           the fields are in numerical order.
 *
 *  INPUTS : A pointer to the field list (FLD_PTR),
 *
 *  RETURNS: A pointer to the sorted list (FLD_PTR).
 * -------------------------------------------------------------
 */

FLD_PTR sort_field_list(list)
FLD_PTR list;
{
   FLD_PTR temp1, temp2, temp3, prev, new_list;

   IF(list->next_field EQ NULL) THEN
      return(list);                     /* Dont sort a single element list! */
   END_IF

   temp1    = list->next_field;
   new_list = list;
   new_list->next_field = NULL;

   while(temp1 NEQ NULL) DO
      temp3 = temp1->next_field;

      temp2 = new_list;
      prev  = NULL;
      while(temp2 NEQ NULL) DO
         IF(temp1->field_num <= temp2->field_num) THEN
            break;
         END_IF
         prev  = temp2;
         temp2 = temp2->next_field;
      END_WHILE

      IF(prev NEQ NULL) THEN
         temp1->next_field = prev->next_field;
         prev->next_field  = temp1;
      ELSE
         temp1->next_field = new_list;
         new_list          = temp1;
      END_IF

      temp1 = temp3;
   END_WHILE

   return(new_list);
}

/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   : add_to_word_list
 *
 *  PURPOSE: This function checks to see if a word is already
 *           in a word list. If it is, it increments the times
 *           used counter. If it isn't, it adds the word to
 *           the FRONT of the list.
 *
 *  INPUTS : Three arguments, a pointer to the word list (WL_PTR),
 *           the word to search for (char *), and the type of the
 *           word.
 *
 *  RETURNS: The modified word list (WL_PTR).
 * -------------------------------------------------------------
 */

WL_PTR add_to_word_list(list, word, type, duplication_check)
WL_PTR  list;
char   *word;
int     type,duplication_check;
{
   int     rtn;
   WL_PTR  temp_wl;
   HASH   *temp_hash;
   char   buf[80];

   temp_wl  = list;

   while(temp_wl NEQ NULL) DO

      /* --------------------------------------------------
       *  If the type is a NUMBER then convert the string
       *  representation to a float value and then compare
       *  values.  This allows for such cases as 200 and
       *  200.0 to be considered the same number.
       * ---------------------------------------------------
       */

      IF(type EQ NUMBER) THEN
        IF (atof(word) EQ atof(temp_wl->word)) THEN
           rtn = 0;
        ELSE
           rtn = 1;
        END_IF
      ELSE
        rtn = strcmp(word, temp_wl->word);
      END_IF
      IF(rtn EQ 0) THEN
         if (duplication_check)
          {
            sprintf(buf,
               "<%s> has been listed more than once in the allowed list.",
               word);
            error_message(WARNING,buf);
          }
         temp_wl->times_used++;
         return(list);
      END_IF

      temp_wl = temp_wl->next_word;
   END_WHILE

   temp_wl              = alloc_wl();
   temp_hash            = add_symbol(word);
   temp_wl->word        = temp_hash->contents;
   temp_wl->next_word   = list;
   temp_wl->times_used  = 1;
   temp_wl->type        = type;

   return(temp_wl);
}

/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   : is_word_in_list
 *
 *  PURPOSE: This function checks to see if a word is in a
 *           word list. If it is, it return YES, otherwise
 *           it returns NO.
 *
 *  INPUTS : Two arguments, a pointer to the word list (WL_PTR),
 *           and the word to search for (char *).
 *
 *  RETURNS: An integer, either YES or NO.
 * -------------------------------------------------------------
 */

int is_word_in_list(list, word)
WL_PTR  list;
char   *word;
{
   int    rtn;
   WL_PTR temp_wl;

   temp_wl  = list;

   while(temp_wl NEQ NULL) DO
      rtn = strcmp(word, temp_wl->word);
      IF(rtn EQ 0) THEN
         return(YES);
      END_IF

      temp_wl = temp_wl->next_word;
   END_WHILE

   return(NO);
}

/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   : add_to_number_list
 *
 *  PURPOSE: This function checks to see if a number is already
 *           in a number list. If it is, it increments the times
 *           used counter. If it isn't, it adds the number to
 *           the FRONT of the list.
 *
 *  INPUTS : Two arguments, a pointer to the number list (NL_PTR),
 *           and the number to search for (char *).
 *
 *  RETURNS: The modified number list (NL_PTR).
 * -------------------------------------------------------------
 */

NL_PTR add_to_number_list(list, number,duplication_check)
NL_PTR  list;
float   number;
int     duplication_check;
{
   NL_PTR temp_nl;
   char buf[80];

   temp_nl  = list;

   while(temp_nl NEQ NULL) DO
      IF(number EQ temp_nl->number) THEN
        if (duplication_check)
         {
           sprintf(buf,
              "<%f> has been listed more than once in the allowed list.",
              number);
           error_message(WARNING, buf);
         }
         temp_nl->times_used++;
         return(list);
      END_IF

      temp_nl = temp_nl->next_number;
   END_WHILE

   temp_nl              = alloc_nl();
   temp_nl->number      = number;
   temp_nl->next_number = list;
   temp_nl->times_used  = 1;

   return(temp_nl);
}


/* ======================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   : is_number_in_list
 *
 *  PURPOSE: This function checks to see if a number is in a
 *           number list. If it is, it return YES, otherwise
 *           it returns NO.
 *
 *  INPUTS : Two arguments, a pointer to the number list (NL_PTR),
 *           and the number to search for (char *).
 *
 *  RETURNS: An integer, either YES or NO.
 * -------------------------------------------------------------
 */

int is_number_in_list(list, number)
NL_PTR  list;
float   number;
{
   NL_PTR temp_nl;

   temp_nl = list;

   while(temp_nl NEQ NULL) DO
      IF(number EQ temp_nl->number) THEN
         return(YES);
      END_IF

      temp_nl = temp_nl->next_number;
   END_WHILE

   return(NO);
}

void set_CHECK_RULES(flag)
int flag;
{
   CHECK_RULES = flag;
}

void set_LITERAL(flag)
int flag;
{
   LITERAL = flag;
}

void set_CHECK_RELATIONS(flag)
int flag;
{
   CHECK_RELATIONS = flag;
}

void set_CHECK_COMMENTS(flag)
int flag;
{
   CHECK_COMMENTS = flag;
}

void set_CHECK_STYLE(flag)
int flag;
{
   CHECK_STYLE = flag;
}

void set_CREATE_DEFRELS(flag)
int flag;
{
   CREATE_DEFRELS = flag;
}

void set_CHECK_DEFRELS(flag)
int flag;
{
   CHECK_DEFRELS = flag;
}

void set_CHECK_DEBUG(flag)
int flag;
{
   CHECK_DEBUG = flag;
}

void set_VERBOSE(flag)
int flag;
{
   VERBOSE = flag;
}

void set_CHECK_EX_FLAG(flag)
int flag;
{
   CHECK_EX_FLAG = flag;
}

void set_ANALYZE_TRACE(flag)
int flag;
{
   ANALYZE_TRACE = flag;
}


