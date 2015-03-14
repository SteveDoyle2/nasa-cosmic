#include <stdio.h>
#include "crsv.h"

/*****************************************************************
 *  This file contains the functions used to parse the CLIPS
 *  rule file. The functions are similar to the token parsing
 *  functions used in CLIPS, but there are changes. The primary
 *  entry point is through the gettoken function.
 *****************************************************************
 */


/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the system
 * -----------------------------
 */

extern double   atof();

/* -----------------------------
 *  From the file: CRSVUSR.C
 * -----------------------------
 */

extern void usr_start_comment();
extern void usr_process_comment();
extern void usr_end_comment();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void error_message();

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */
 
extern TK_PTR alloc_token();
extern void free_token();
extern char *gen_alloc();

/* -----------------------------
 *  From the file: CRSVHASH.C
 * -----------------------------
 */
 
extern HASH  *add_symbol();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */
 
extern TK_PTR dequeue_token();
extern void queue_token ();


/* ===========  Functions defined here for Global use  ================ */

int       set_get_token_queue();
int       gettoken();
TK_PTR    gettoken_ptr();
void      ungettoken_ptr();
void      save_token();
char     *strsave();

/* ===========  Functions defined here for internal use  ============== */

int  fetchnum();
int  fetchvar();
int  fetchany();


/* ===========  Variables defined here for Global use  ================ */

float  TKNNUMBER;                    /* gettoken number return  */
char  *TKNWORD;                      /* gettoken string return  */
HASH  *HASHWORD;                     /* gettoken hash table storage */


/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern int cur_line_num;             /* Current line number   */


/* ===========  Variables defined here for internal use  ============== */


static int    token_saved = NO;         /* flag for saved token */
static int    sv_token;
static float  sv_num;
static char  *sv_word;
static HASH  *sv_hash;

static TN_PTR unget_tokens = NULL;


/* ===================  PARSER FUNCTIONS  ======================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  set_get_token_queue
 *
 *  PURPOSE:  Permits substitution of the unget token queue.
 *
 *  INPUTS:   The new queue.
 *
 *  RETURNS:  An integer, currently OK.
 * -------------------------------------------------------------
 */ 

int set_get_token_queue (new_queue)
 TN_PTR new_queue;
 {
   unget_tokens = new_queue;

   return (OK);
 }

/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  gettoken_ptr
 *
 *  PURPOSE:  This function scans the file for the next token.
 *
 *  INPUTS:   A single argument, a pointer to the current file.
 *
 *  RETURNS:  A pointer to a token structure containing all of
 *            the token attributes is returned.
 * -------------------------------------------------------------
 */ 

TK_PTR gettoken_ptr(infile)
 FILE *infile;
 {
   TK_PTR tknstruct = alloc_token ();

   tknstruct->token = gettoken (infile);
   tknstruct->tknword = TKNWORD;
   tknstruct->hashword = HASHWORD;
   tknstruct->tknnumber = TKNNUMBER;
   tknstruct->print_rep = tknstruct->tknword;

   return (tknstruct);
 }

/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  ungettoken_ptr
 *
 *  PURPOSE:  Places a token (via TK_PTR) back onto the input
 *            buffer.
 *
 *  INPUTS :  The token to unget.
 *
 *  RETURNS:  Nothing
 *
 *  NOTE   :  Unlike the UNIX unget, an arbitrary number of tokens
 *            may be placed back on the input buffer.
 * -------------------------------------------------------------
 */ 

void ungettoken_ptr (tknstruct)
 TK_PTR tknstruct;
 {
   queue_token (&unget_tokens, tknstruct);
 }

/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  gettoken
 *
 *  PURPOSE:  This function scans the file for the next token.
 *            The type of the token (word, string, number, 
 *            variable, etc.) is returned.
 *            If the token has an associated string, then the
 *            value of this string hashed and stored in the global 
 *            variable, TKNWORD. If the token is a number, then 
 *            its value is stored in the global variable,
 *            TKNNUMBER.
 *
 *  INPUTS:   A single argument, a pointer to the current file.
 *
 *  RETURNS:  An integer, the token ID number, or ERROR, if the
 *            token contained bad characters.
 * -------------------------------------------------------------
 */ 

int gettoken(infile)
   FILE  *infile;
{
   int inchar;
   int token;

   if (token_saved IS_YES)
   {
      token = get_saved_token();
      return(token);
   }
   
   IF (unget_tokens NEQ NULL) THEN
      TK_PTR tknstruct = dequeue_token (&unget_tokens);
      int dequeued_token = tknstruct->token;

      TKNWORD = tknstruct->tknword;
      HASHWORD = tknstruct->hashword;
      TKNNUMBER = tknstruct->tknnumber;

      free_token (tknstruct);

      return (dequeued_token);
   END_IF

   /*=======================================*/
   /* Set Unknown default values for token. */
   /*=======================================*/

   token     = KUNKNOWN;
   TKNWORD   = "unknown";
   HASHWORD  = NULL;
   TKNNUMBER = 0.0;

   /*==============================================*/
   /* Remove all white space before processing the */ 
   /* gettoken() request.                          */
   /*==============================================*/

   inchar = savegetc(infile);
   
   while ((inchar == ' ') || (inchar == '\n') || (inchar == '\f') ||
          (inchar == '\r') || (inchar == ';') || (inchar == '\t'))
   {
      /*=======================*/
      /* Remove comment lines. */
      /*=======================*/
 
      if (inchar == ';')               
      {
         usr_start_comment(';');
         while (((inchar = savegetc(infile))!= '\n') &&
                 (inchar != '\r') && (inchar != EOF))
            usr_process_comment(inchar);
         usr_end_comment('\n');
      }
      /*========================*/
      /* Increment line counter */
      /*========================*/
      if (inchar == '\n')
         cur_line_num++;
      inchar = savegetc(infile);
   }

   /*==========================*/
   /* Process Symbolic Tokens. */
   /*==========================*/

   if (((inchar >= 'a') && (inchar <= 'z')) ||
       ((inchar >= 'A') && (inchar <= 'Z')))
   {
      unsavegetc(inchar,infile);
      if (fetchvar(infile,&TKNWORD,NULL) == ERROR)
         return (ERROR);
      token = WORD;
   }

   /*========================*/
   /* Process String Tokens. */
   /*========================*/

   else if (inchar == '"')                          
   {
      if (fetchany(infile,&TKNWORD) == ERROR)
         return(ERROR);
      token = STRING;
   }

   /*===============================================*/
   /* Process Number Tokens beginning with a digit. */
   /*===============================================*/
   
   else if (((inchar >= '0') && (inchar <= '9')) ||
            (inchar == '.'))     
   {
      unsavegetc(inchar,infile);
      if (fetchnum(infile, &TKNWORD, NO) == ERROR)
         return (ERROR);
      TKNNUMBER = (float) atof(TKNWORD);
      token = NUMBER;
   }

   /*==================================================================*/
   /* Process Number Tokens beginning with a "-" and the "-" Operator. */
   /*==================================================================*/

   else if (inchar == '-')
   {
      inchar = savegetc(infile);
      if ((inchar == '.') ||        /* number or decimal follows, */
          ((inchar >= '0') &&       /* process as numerical token */
           (inchar <= '9')))
      {
         unsavegetc(inchar,infile);
         if (fetchnum(infile, &TKNWORD, YES) == ERROR)
            return (ERROR);
         TKNNUMBER = (float) atof(TKNWORD);
         token = NUMBER;
      }
      else                         /* otherwise, use it as the */
      {                            /* subtraction operator     */
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, "-") == ERROR)
            return (ERROR);
         token = WORD;
      }
   }

   /*==================================================================*/
   /* Process Number Tokens beginning with a "+" and the "+" Operator. */
   /*==================================================================*/

   else if (inchar == '+')
   {
      inchar = savegetc(infile);
      if ((inchar == '.') ||         /* number or decimal follows, */
          ((inchar >= '0') &&        /* process as numerical token */
           (inchar <= '9')))
      {
         unsavegetc(inchar,infile);
         if (fetchnum(infile, &TKNWORD, NO) == ERROR)
            return (ERROR);
         TKNNUMBER = (float) atof(TKNWORD);
         token = NUMBER;
      }
      else                                /* otherwise, use as the */
      {                                   /* ADD operator.         */
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, "+") == ERROR)
            return (ERROR);
         token = WORD;         
      }
   }
   
   /*============================*/
   /* Process ? and ?var Tokens. */
   /*============================*/
   
   else if (inchar == '?')
   {
      inchar = savegetc(infile);
      if (((inchar >= 'a') && (inchar <= 'z')) ||
           ((inchar >= 'A') && (inchar <= 'Z')))
      {
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, NULL) == ERROR)
            return (ERROR);
         token = BWORD;
      }
      else
      {
         TKNWORD = "?";
         token = SINGLE;
         unsavegetc(inchar,infile);
      }
   }

   /*==============================*/
   /* Process "&" and "&&" Tokens. */
   /*==============================*/

   else if (inchar == '&')
   {
      if ((inchar = savegetc(infile)) == '&')
      {
         TKNWORD = "&&";
         token = WORD;
      }
      else
      {
         TKNWORD = "&";
         token   = LAND;
         unsavegetc(inchar,infile);
      }
   }

   /*==============================*/
   /* Process $? and $?var Tokens. */
   /*==============================*/

   else if (inchar == '$')
   {
      if ((inchar = savegetc(infile)) == '?')
      {
         inchar = savegetc(infile);
         if (((inchar >= 'a') && (inchar <= 'z')) ||
             ((inchar >= 'A') && (inchar <= 'Z')))
         {
            unsavegetc(inchar,infile);
            if (fetchvar(infile, &TKNWORD, NULL) == ERROR)
               return (ERROR);
            token = BWORDS;
         }
         else
         {
            TKNWORD = "$?";
            token   = MULTIPLE;
            unsavegetc(inchar,infile);
         }
      }
      else 
      {
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, "$") == ERROR)
            return (ERROR);
         token = WORD;         
      }
   }

   /*==============================*/
   /* Process "|" and "||" Tokens. */
   /*==============================*/

   else if (inchar == '|') 
   {
      if ((inchar = savegetc(infile)) == '|')
      {
         TKNWORD = "||";
         token = WORD;
      }
      else
      {
         TKNWORD = "|";
         token   = LOR;
         unsavegetc(inchar,infile);
      }
   }

   /*==============================*/
   /* Process "=" and "=>" Tokens. */
   /*==============================*/
 
   else if (inchar == '=') 
   {
      if ((inchar = savegetc(infile)) == '>')
      { 
         TKNWORD = "=>";
         token   = SEPARATOR;
      }
      else
      {
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, "=") == ERROR)
            return (ERROR);
         token = WORD;         
      }
   }

   /*==============================*/
   /* Process "<" and "<-" Tokens. */
   /*==============================*/

   else if (inchar == '<')
   {
      if ((inchar = savegetc(infile)) == '-')
      {
         TKNWORD = "<-";
         token   = BINDER;
      }
      else
      {
         unsavegetc(inchar,infile);
         if (fetchvar(infile, &TKNWORD, "<") == ERROR)
            return (ERROR);
         token = WORD;         
      }
   }
   
   /*===================================*/
   /* Process "(", ")", and "~" Tokens. */
   /*===================================*/

   else if (inchar == '(')                         
   { 
      TKNWORD = "(";
      token   = LPAREN;
   }
   else if (inchar == ')')                          
   {
      TKNWORD = ")";
      token   = RPAREN;
   }
   else if (inchar == '~') 
   { 
      TKNWORD = "~";
      token   = LNOT;
   }

   /*============================*/
   /* Process End-of-File Token. */
   /*============================*/

   else if (inchar == EOF)
   {
      token   = STOP;
      TKNWORD = "stop"; 
   }

   /*=======================*/
   /* Process Other Tokens. */
   /*=======================*/

   else 
   {
      unsavegetc(inchar,infile);
      if (fetchvar(infile, &TKNWORD, NULL) == ERROR)
         return (ERROR);
      token = WORD;
   }
   return(token);
}

/* ==================================================================== */

/* -------------------------------------------
 *  NAME   :    save_token
 *
 *  PURPOSE:    This function stores a token
 *              and it's associated value, 
 *              either word or number in a 
 *              buffer for reuse.
 *
 *  INPUTS:     Three arguments
 *   int    token  - The token number
 *   float  number - tokens numerical value
 *   char  *word   - tokens string value
 *
 *  RETURNS:     Nothing useful.
 * -------------------------------------------
 */

void save_token(token, word, number)
int token;
float number;
char *word;
{
   sv_token = token;
   sv_num   = number;
   sv_word  = word;
   sv_hash  = HASHWORD;
   
   token_saved = YES;
}


/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  fetchvar
 *
 *  PURPOSE:  This function gets one legal variable name
 *            from the input file. A legal variable name
 *            consists of any character that is not a 
 *            delimeter.
 *
 *  INPUTS :  A pointer to the input file (FILE *).
 *            A pointer to the string to be saved (char **).
 *            A string containing the first letters of the
 *               string to be saved (char *).
 *
 *  RETURNS:  OK or ERROR.
 * -------------------------------------------------------------
 */

static int fetchvar(infile,tknword,beg_str)
   FILE *infile;
   char **tknword;
   char *beg_str;
{
   int count = 0;
   int inchar;
   char buf[WORDLENGTH];
   
   if (beg_str != NULL)
      while (beg_str[count] != EOS)
      {
         buf[count] = beg_str[count];
         ++count;
      }

   inchar = savegetc(infile);
   while ( (inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') &&
           (inchar != ' ') && (inchar != ';') &&
           (inchar >= ' ') && (inchar < '~') )
   {
      buf[count++] = inchar;
      inchar = savegetc(infile);
   }

   buf[count] = EOS;

   /*===================================================*/
   /* Stuff last character back into buffer and give    */
   /* the string a permanent address through strsave(). */
   /*===================================================*/

   unsavegetc(inchar,infile);
   *tknword = strsave(buf);  
   return(OK);
}

/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  fetchany
 *
 *  PURPOSE:  This function gets one string, from a beginning
 *            double quote to the ending double quote.
 *
 *  INPUTS :  A pointer to the input file (FILE *).
 *            A pointer to the string to be saved (char **).
 *
 *  RETURNS:  OK or ERROR.
 * -------------------------------------------------------------
 */

static int fetchany(infile,tknword)
   FILE *infile;
   char **tknword;
{
   int count = 1;
   int inchar,lastchar;
   char buf[WORDLENGTH];

   buf[0] = '"';
   inchar = savegetc(infile);
   
   while (inchar != '"')
   {
      if (count > (WORDLENGTH - 2))
      {
         buf[count] = EOS;
         *tknword = strsave(buf);
         
         sprintf(buf,"Maximum token length of %d exceeded\n",WORDLENGTH);
         error_message(ERROR, buf);
         return (ERROR);
      }

      if (inchar == EOF)
      {
         unsavegetc(inchar,infile);
         buf[count] = EOS;
         *tknword = strsave(buf);
         
         error_message (ERROR,
            "End of file encountered while reading string\n");
         return (ERROR);
      }

/*
      if (inchar == '\\')
         inchar = savegetc(infile);
*/

      if (inchar == '\n')
         cur_line_num++;

      buf[count++] = inchar;
      lastchar = inchar;
      inchar = savegetc(infile);
      if ((lastchar == '\\') && (inchar == '"')) {
        buf[count++] = inchar;
        inchar = savegetc(infile);
      }
        
   }

   /*=================================================*/
   /* Add an end-quote and an EOS to buf and give the */
   /* string a permanent address through strsave().   */
   /*=================================================*/
   
   buf[count++] = '"';
   buf[count] = EOS;
   *tknword = strsave(buf);
   return(OK);
}

/* ====================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  fetchnum
 *
 *  PURPOSE:  This function gets a number from the input
 *            file. The number may be preceeded by a '+'
 *            or a '-' sign, and may be in exponential form.
 *
 *  INPUTS :  A pointer to the input file (FILE *), a pointer
 *            to the string to be saved (char **), and a
 *            flag telling whether or not this number is
 *            negative (int).
 *
 *  RETURNS:  OK or ERR.
 *
 *  NOTE   :  Major difference from standard CLIPS token 
 *            reader, which returns floating point values.
 * -------------------------------------------------------------
 */

static int fetchnum(infile, tknword, is_neg)
   FILE *infile;
   char **tknword;
   int   is_neg;
{
   int count = 0;
   int inchar,phase;
   char buf[WORDLENGTH];

/* Phases:                    */

#     define INTEGER_PART 0
#     define DECIMAL_PART 1
#     define EXPONENT_BEGIN 2
#     define EXPONENT_VALUE 3

   if (is_neg IS_YES)
      buf[count++] = '-';

   inchar = savegetc(infile);
   phase  = INTEGER_PART;

   while ((inchar != '(') && (inchar != ')') &&
          (inchar != '&') && (inchar != '|') &&
          (inchar != ' ') && (inchar != ';') &&
          (inchar >= ' ') && (inchar <= '~'))
   {
      if ((inchar >= '0') && (inchar <= '9'))
      {
         if (phase == EXPONENT_BEGIN)
            phase = EXPONENT_VALUE;
      }
      else if (inchar == '.')
      {
         if (phase == INTEGER_PART)
            phase = DECIMAL_PART;
         else
         {
            error_message (ERROR,"Extraneous decimal point found in number\n");
            buf[count++] = inchar;
            buf[count]   = EOS;
            /* read string 'til next delimeter */
            (void)fetchvar(infile,tknword,buf); 
            return (ERROR);
         }
      }
      else if ((inchar == 'E') || (inchar == 'e'))
      {
         if ((phase == INTEGER_PART) || (phase == DECIMAL_PART))
            phase = EXPONENT_BEGIN;
         else
         {
            error_message (ERROR,"Invalid character found in number\n");
            buf[count++] = inchar;
            buf[count]   = EOS;
            /* read string 'til next delimeter */
            (void)fetchvar(infile,tknword,buf); 
            return (ERROR);
         }
      }
      else if ((inchar == '-') || (inchar == '+'))
      {
         if (phase == EXPONENT_BEGIN)
            phase = EXPONENT_VALUE;
         else
         {
            error_message (ERROR,"Extraneous sign found in number\n");
            buf[count++] = inchar;
            buf[count]   = EOS;
            /* read string 'til next delimeter */
            (void)fetchvar(infile,tknword,buf); 
            return (ERROR);
         }
      }
      else
      {
         error_message (ERROR,"Invalid character found in number\n");
         buf[count++] = inchar;
         buf[count]   = EOS;
         /* read string 'til next delimeter */
         (void)fetchvar(infile,tknword,buf);
         return (ERROR);
      }

      buf[count++] = inchar;
      inchar = savegetc(infile);
   }

   buf[count] = EOS;

   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the number.                */
   /*=======================================*/

   unsavegetc(inchar,infile);
   *tknword = strsave(buf);  
   return(OK);
  }

/* ---------------------------------------------------------------------- */

static savegetc(file)
FILE *file;
{
   return(getc(file));
}

/* ---------------------------------------------------------------------- */

static unsavegetc(c,file)
char  c;
FILE *file;
{
   (void)ungetc(c, file);
}

/* ---------------------------------------------------------------------- */

static int get_saved_token()
{
   token_saved = NO;
   TKNWORD     = sv_word;
   HASHWORD    = sv_hash;
   TKNNUMBER   = sv_num;
   
   return(sv_token);
}

/* ---------------------------------------------------------------------- */

/* ------------------------------------------------
 *  NAME   :  strsave
 *
 *  PURPOSE:  This functions takes a pointer to a
 *            string, and puts it the hash table
 *            in the appropriate way.
 *
 *  INPUTS:   A pointer to the original string.
 *
 *  RETURN:   A pointer to the new string.
 * ------------------------------------------------
 */

char *strsave(s)
   char *s;
{
   HASHWORD = add_symbol(s);

   return(HASHWORD->contents);
}
