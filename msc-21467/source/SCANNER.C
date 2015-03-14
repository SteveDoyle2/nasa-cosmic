/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   SCANNER MODULE                    */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "scanner.h"
#include "clipsmem.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
  
   char                   *copy_pp_buffer();
   int                     expand_pp_buffer();
   struct draw            *fetchany();
   float                   fetchnum();
   struct draw            *fetchvar();
   int                     flush_pp_buffer();
   char                   *get_pp_buffer();
   struct draw            *add_symbol();
   int                     gettoken();
   int                     pp_backup();
   int                     save_pp_buffer();
   int                     savegetc();
   int                     unsavegetc();
   char                   *str_print_rep();
 
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char            *num_to_string();
   extern char            *strcpy();
   extern char            *symbol_string();

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static char             GLOBALSTR[WORDLENGTH];    
   static char             PRINT_REP[WORDLENGTH];
   static int              pp_buffer_status = OFF; 
   static int              NUM_ERROR;
   static int              INDENT_DEPTH;
   
/*####################################################################*/
/*####################################################################*/
/*###                                                              ###*/
/*###                          SCANNER                             ###*/
/*###                                                              ###*/
/*####################################################################*/
/*####################################################################*/

/**********************************************************************/
/* GETTOKEN: Scans infile for the next token.  The type of the token  */
/*   (word, string, number, variable, etc.) is return in the global   */
/*   variable token.  If the token has an associated string, then the */
/*   value of this string and its hash table entry are stored in the  */
/*   global variables TKNWORD and HASHWORD respectively.  If the      */
/*   token is a number, then its value is stored in the global        */
/*   variable TKNNUMBER.                                              */
/**********************************************************************/
gettoken(infile,tknstruct)
 char *infile;
 struct token *tknstruct;
 {
   int inchar;
   float fetchnum();

   /*=======================================*/
   /* Set Unknown default values for token. */
   /*=======================================*/

   tknstruct->token = KUNKNOWN;
   tknstruct->tknword = "unknown";
   tknstruct->hashword = NULL;
   tknstruct->tknnumber = 0.0;
   tknstruct->print_rep = "unknown";

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
         inchar = savegetc(infile);
         while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) )
           { inchar = savegetc(infile); }
        }
      inchar = savegetc(infile);
     }

   /*==========================*/
   /* Process Symbolic Tokens. */
   /*==========================*/

   if (((inchar >= 'a') && (inchar <= 'z')) ||
       ((inchar >= 'A') && (inchar <= 'Z')))
     {
      tknstruct->token = WORD;
      unsavegetc(inchar,infile);
      tknstruct->hashword = fetchvar(infile,0);
      tknstruct->tknword = symbol_string(tknstruct->hashword);
      tknstruct->print_rep = tknstruct->tknword;                    
     }

   /*========================*/
   /* Process String Tokens. */
   /*========================*/

   else if (inchar == '"')                          
     {
      tknstruct->hashword = fetchany(infile);
      tknstruct->tknword = symbol_string(tknstruct->hashword);
      tknstruct->token = STRING; 
      tknstruct->print_rep = str_print_rep(tknstruct->tknword);          
     }

   /*===============================================*/
   /* Process Number Tokens beginning with a digit. */
   /*===============================================*/
   
   else if (((inchar >= '0') && (inchar <= '9')) ||
            (inchar == '.'))     
     {
      unsavegetc(inchar,infile);
      tknstruct->tknnumber = fetchnum(infile);
      if (NUM_ERROR == TRUE)
        {
         tknstruct->token = KUNKNOWN;
         tknstruct->tknword = "unknown";
         sprintf(PRINT_REP,GLOBALSTR);
         tknstruct->print_rep = PRINT_REP;
        }
      else
        {
         tknstruct->tknword = num_to_string(tknstruct->tknnumber);      
         tknstruct->token = NUMBER;
         tknstruct->print_rep = tknstruct->tknword;
        }              
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
         tknstruct->tknnumber = - fetchnum(infile);
         if (NUM_ERROR == TRUE)
           {
            tknstruct->token = KUNKNOWN;
            tknstruct->tknword = "unknown";
            sprintf(PRINT_REP,GLOBALSTR);
            tknstruct->print_rep = PRINT_REP;
           }
         else
           {
            tknstruct->tknword = num_to_string(tknstruct->tknnumber);      
            tknstruct->token = NUMBER;
            tknstruct->print_rep = tknstruct->tknword;
           } 
        }
      else
        {
         GLOBALSTR[0] = '-';
         unsavegetc(inchar,infile); 
         tknstruct->token = WORD; 
         tknstruct->hashword = fetchvar(infile,1);
         tknstruct->tknword = symbol_string(tknstruct->hashword);
         tknstruct->print_rep = tknstruct->tknword; 
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
         tknstruct->tknnumber = fetchnum(infile);
         if (NUM_ERROR == TRUE)
           {
            tknstruct->token = KUNKNOWN;
            tknstruct->tknword = "unknown";
            sprintf(PRINT_REP,GLOBALSTR);
            tknstruct->print_rep = PRINT_REP;
           }
         else
           {
            tknstruct->tknword = num_to_string(tknstruct->tknnumber);      
            tknstruct->token = NUMBER;
            tknstruct->print_rep = tknstruct->tknword;
           }
        }
      else                                
        {  
         GLOBALSTR[0] = '+';
         unsavegetc(inchar,infile); 
         tknstruct->token = WORD; 
         tknstruct->hashword = fetchvar(infile,1);
         tknstruct->tknword = symbol_string(tknstruct->hashword);
         tknstruct->print_rep = tknstruct->tknword;
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
          tknstruct->hashword = fetchvar(infile,0);
          tknstruct->tknword = symbol_string(tknstruct->hashword);
	      tknstruct->token = BWORD;
          sprintf(PRINT_REP,"?%s",tknstruct->tknword);
          tknstruct->print_rep = PRINT_REP;
         }
       else
         {
	      tknstruct->token = SINGLE;
	      tknstruct->tknword = "?";
          unsavegetc(inchar,infile);
          tknstruct->print_rep = "?";
         }
     }

   /*==============================*/
   /* Process "&" and "&&" Tokens. */
   /*==============================*/

   else if (inchar == '&')
     {
      if ((inchar = savegetc(infile)) == '&')
	   {
        tknstruct->token = WORD; 
        tknstruct->hashword = add_symbol("&&");
        tknstruct->tknword = symbol_string(tknstruct->hashword);
        tknstruct->print_rep = tknstruct->tknword;
	   }
     else
	  {
	   unsavegetc(inchar,infile);
       tknstruct->print_rep = "&";               
	   tknstruct->token =  LAND;
	   tknstruct->tknword = "&";
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
            tknstruct->hashword = fetchvar(infile,0);
            tknstruct->tknword = symbol_string(tknstruct->hashword);
            tknstruct->token = BWORDS;
            sprintf(PRINT_REP,"$?%s",tknstruct->tknword);
            tknstruct->print_rep = PRINT_REP;     
           }
         else
	      {
	       tknstruct->token = MULTIPLE;
	       tknstruct->tknword = "$?";
           tknstruct->print_rep = "$?";              
           unsavegetc(inchar,infile);
	      }
	    }
      else 
        { 
         tknstruct->token = WORD; 
         GLOBALSTR[0] = '$';
         unsavegetc(inchar,infile); 
         tknstruct->hashword = fetchvar(infile,1); 
         tknstruct->tknword = symbol_string(tknstruct->hashword);
         tknstruct->print_rep = tknstruct->tknword; 
        }
     }

   /*==============================*/
   /* Process "|" and "||" Tokens. */
   /*==============================*/

   else if (inchar == '|') 
     {
      if ((inchar = savegetc(infile)) == '|')
	{
     tknstruct->token = WORD; 
     tknstruct->hashword = add_symbol("||");
     tknstruct->tknword = symbol_string(tknstruct->hashword);
     tknstruct->print_rep = tknstruct->tknword; 
	}
      else
	{
	 unsavegetc(inchar,infile);
	 tknstruct->token = LOR;
	 tknstruct->tknword = "|";
     tknstruct->print_rep = "|";               
	}
     }

   /*==============================*/
   /* Process "=" and "=>" Tokens. */
   /*==============================*/
 
   else if (inchar == '=') 
     {
      if ((inchar = savegetc(infile)) == '>')
        { 
         tknstruct->token = SEPARATOR;
         tknstruct->tknword = "=>";
         tknstruct->print_rep = "=>";               
        }
      else
        { 
         tknstruct->token = WORD; 
         GLOBALSTR[0] = '=';
         unsavegetc(inchar,infile);
         tknstruct->hashword = fetchvar(infile,1); 
         tknstruct->tknword = symbol_string(tknstruct->hashword);
         tknstruct->print_rep = tknstruct->tknword; 
	    }
     }

   /*=====================================*/
   /* Process "<", "<-", and "<=" Tokens. */
   /*=====================================*/

   else if (inchar == '<')
     {
      if ((inchar = savegetc(infile)) == '-')
	    {
         tknstruct->token = BINDER;
         tknstruct->tknword = "<-";
         tknstruct->print_rep = "<-";               
        }
      else  
        { 
         tknstruct->token = WORD; 
         GLOBALSTR[0] = '<';
         unsavegetc(inchar,infile); 
         tknstruct->hashword = fetchvar(infile,1); 
         tknstruct->tknword = symbol_string(tknstruct->hashword);
         tknstruct->print_rep = tknstruct->tknword;  
        } 
     }
   
   /*========================================*/
   /* Process "(", ")", "~", and ":" Tokens. */
   /*========================================*/

   else if (inchar == '(')                         
     { 
      tknstruct->token = LPAREN;
      tknstruct->tknword = "(";
      tknstruct->print_rep = "(";              
     }
   else if (inchar == ')')                          
     {
      tknstruct->token= RPAREN;
      tknstruct->tknword = ")";
      tknstruct->print_rep = ")";               
     }
   else if (inchar == '~') 
     { 
      tknstruct->token = LNOT;
      tknstruct->tknword = "~";
      tknstruct->print_rep = "~";               
     }
   else if (inchar == ':')
     { 
      tknstruct->token = WORD; 
      unsavegetc(inchar,infile);
      tknstruct->hashword = fetchvar(infile,0);
      tknstruct->tknword = symbol_string(tknstruct->hashword);
      tknstruct->print_rep = tknstruct->tknword;             
     }

   /*============================*/
   /* Process End-of-File Token. */
   /*============================*/

   else if ((inchar == EOF) || (inchar == 0) || (inchar == 3))
     {
      tknstruct->token = STOP;
      tknstruct->tknword = "stop";
      tknstruct->print_rep = "";               
     } 
   
   /*==========================*/
   /* Process Other Tokens. */
   /*==========================*/

   else 
     {   
      tknstruct->token = WORD; 
      GLOBALSTR[0] = inchar; 
      tknstruct->hashword = fetchvar(infile,1); 
      tknstruct->tknword = symbol_string(tknstruct->hashword);
      tknstruct->print_rep = tknstruct->tknword;         
     }
     
   save_pp_buffer(tknstruct->print_rep); 
   return;     
  }

/*********************************************************************/
/* FETCHVAR:  The purpose of fetchvar is to input one legal variable */
/*   name. A legal variable name consists of a letter followed by    */
/*   any printable ascii character in the range from ' ' to '~' with */
/*   the exception of '<', '(', ')', '&', '|', ' ', '"', and ';'.    */
/*********************************************************************/
struct draw *fetchvar(infile,count)
  char *infile;
  int count;
  {
   int inchar;
   
   inchar = savegetc(infile);
   while ( (inchar != '<') && (inchar != '"') &&
           (inchar != '(') && (inchar != ')') &&
           (inchar != '&') && (inchar != '|') &&
           (inchar != ' ') && (inchar != ';') && 
           (inchar >= ' ') && (inchar < '~') )
     { 
      if (count > (WORDLENGTH - 2))
        {
         cl_print("werror","SYSTEM ERROR\n");
         sprintf(GLOBALSTR,"Maximum token length of %d exceeded\n",WORDLENGTH);
	     cl_print("werror",GLOBALSTR);
         cl_exit(2);
        }

      GLOBALSTR[count++] = inchar;
      inchar = savegetc(infile);
     }

   GLOBALSTR[count] = EOS;
   
   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the word through hasher.   */
   /*=======================================*/

   unsavegetc(inchar,infile);  
   return(add_symbol(GLOBALSTR));
  }
 
/********************************************************************/
/* FETCHANY:  The purpose of fetchany is to input one word with any */
/*   embedded characters. It is delimited by a leading and trailing */
/*   double quote.                                                  */
/********************************************************************/
struct draw *fetchany(infile)
  char *infile;
  {
   int count = 0;
   int inchar;

   inchar = savegetc(infile);                          
   while ((inchar != '"') && (inchar != EOF))      
     {
      if (count > (WORDLENGTH - 2))
        {
         cl_print("werror","SYSTEM ERROR\n");
         sprintf(GLOBALSTR,"Maximum token length of %d exceeded\n",WORDLENGTH);
	     cl_print("werror",GLOBALSTR);
         cl_exit(2);
        }

      if (inchar == '\\')
        { inchar = savegetc(infile); }
 
      GLOBALSTR[count++] = inchar;
      inchar = savegetc(infile);                 
     }
       
   GLOBALSTR[count] = EOS;
   return(add_symbol(GLOBALSTR));
  }
  
/********************************************************************/
/* FETCHNUM:  The purpose of fetchnum is to input a numeric value   */
/*   and return it as a token. The number may be proceeded by a '+' */
/*   or a '-' sign, and may be in exponential form.                 */
/********************************************************************/
float fetchnum(infile)
  char *infile;
  {
   int count = 0;
   int inchar, phase;
   double atof(); 
   
   /* Phases:              */
   /*   0 = integral       */
   /*   1 = decimal        */
   /*   2 = exponent-begin */
   /*   3 = exponent-value */
   /*   5 = done           */
   /*   9 = error          */
   
   NUM_ERROR = FALSE;
   
   inchar = savegetc(infile);
   phase = 0; 
   
   while ((phase != 5) && (phase != 9))
     {
      if (phase == 0)
        { 
         if ((inchar >= '0') && (inchar <= '9'))
           { GLOBALSTR[count++] = inchar; }
         else if (inchar == '.')
           {
            GLOBALSTR[count++] = inchar; 
            phase = 1; 
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            GLOBALSTR[count++] = inchar;  
            phase = 2; 
           }
         else if ( (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') ||
                   (inchar == ' ') || (inchar == ';') || 
                   (inchar < ' ') || (inchar > '~') )
           { phase = 5; }
         else 
           {
            phase = 9;
            GLOBALSTR[count++] = inchar; 
           }
        }
      else if (phase == 1)
        {
         if ((inchar >= '0') && (inchar <= '9'))
           {
            GLOBALSTR[count++] = inchar;
           }
         else if ((inchar == 'E') || (inchar == 'e'))
           {
            GLOBALSTR[count++] = inchar; 
            phase = 2; 
           }
         else if ( (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') ||
                   (inchar == ' ') || (inchar == ';') || 
                   (inchar < ' ') || (inchar > '~') )
           { phase = 5; }
         else
           {
            phase = 9;
            GLOBALSTR[count++] = inchar; 
           }
        }
      else if (phase == 2)
        {
         if ((inchar >= '0') && (inchar <= '9'))
           { 
            GLOBALSTR[count++] = inchar;
            phase = 3; 
           }
         else if ((inchar == '+') || (inchar == '-'))
           {
            GLOBALSTR[count++] = inchar;
            phase = 3; 
           }
         else if ( (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') ||
                   (inchar == ' ') || (inchar == ';') || 
                   (inchar < ' ') || (inchar > '~') )
           { phase = 5; }
         else
           {
            phase = 9;
            GLOBALSTR[count++] = inchar; 
           }
        }
      else if (phase == 3)
        { 
         if ((inchar >= '0') && (inchar <= '9'))
           { GLOBALSTR[count++] = inchar; }
         else if ( (inchar == '(') || (inchar == ')') ||
                   (inchar == '&') || (inchar == '|') ||
                   (inchar == ' ') || (inchar == ';') || 
                   (inchar < ' ') || (inchar > '~') )
           { phase = 5; }
         else
           {
            phase = 9;
            GLOBALSTR[count++] = inchar; 
           }
        }  
        
      if ((phase != 5) && (phase != 9)) 
        { inchar = savegetc(infile); }
     }
    
   GLOBALSTR[count] = EOS;

   if (phase == 9)
     {
      NUM_ERROR = TRUE; 
      return(0.0);
     }

   /*=======================================*/
   /* Stuff last character back into buffer */
   /* and return the number.                */
   /*=======================================*/

   unsavegetc(inchar,infile);  
   return((float) atof(GLOBALSTR));
  }
  
/********************************************************************/
/* COPY_TOKENS:                                                        */ 
/********************************************************************/
copy_tokens(token1,token2)
  struct token *token1, *token2;
  {
   token1->token = token2->token;
   token1->tknword = token2->tknword;
   token1->tknnumber = token2->tknnumber;
   token1->hashword = token2->hashword;
   token1->print_rep = token2->print_rep;
  }
  
/********************************************************************/
/* SAVEGETC:                                                        */ 
/********************************************************************/
savegetc(infile)
  char *infile;
  {
   int inchar;
            
   inchar = cl_getc(infile);
   return(inchar);
  }

/********************************************************************/
/* UNSAVEGETC:                                                      */
/********************************************************************/
unsavegetc(outchar,infile)
  int outchar;
  char *infile;
  {   
   cl_ungetc(outchar,infile);
  }

/*####################################################################*/
/*####################################################################*/
/*###                                                              ###*/
/*###                    PRETTY PRINT FUNCTIONS                    ###*/
/*###                                                              ###*/
/*####################################################################*/
/*####################################################################*/

static int pp_save_pos = 0;
static int pp_save_max = 0;
static int pp_lp_1 = 0;
static int pp_lp_2 = 0;
char *pp_buffer = NULL;

/**********************************************************/
/* FLUSH_PP_BUFFER:  Resets the pretty print save buffer. */
/**********************************************************/
flush_pp_buffer()
  {
   if (pp_buffer == NULL) return(0);
   pp_lp_1 = 0;
   pp_lp_2 = 0;
   pp_save_pos = 0;
   pp_buffer[0] = EOS;
   return(1);
  }

/***********************************************************/
/* EXPAND_PP_BUFFER:  Expands the pretty print save buffer */
/*   to accomodate more text.                              */
/***********************************************************/
expand_pp_buffer()
  {
   char *new_pp;

   new_pp = gm2( (pp_save_max + 512) * sizeof (char) );

   if (new_pp == NULL)
     {
      cl_print("werror","Unable to expand string buffer\n");
      cl_exit(1);
     }
   
   if (pp_buffer == NULL)
     {
      pp_buffer = new_pp;
      pp_save_pos = 0;
     }
   else
     { 
      strcpy(new_pp,pp_buffer);
      rm(pp_buffer,sizeof (char) * pp_save_max);
      pp_buffer = new_pp;
     }

   pp_save_max += 512;
   return(1);
  }

/*******************************************************/
/* SAVE_PP_BUFFER:  Appends a string to the end of the */
/*   pretty print save buffer.                         */
/*******************************************************/
save_pp_buffer(str)
  char *str;
  {
   char *s_ptr;

   if (pp_buffer_status == OFF) return;

   if (strlen(str) + pp_save_pos + 1 >= pp_save_max)
     { expand_pp_buffer(); }

   s_ptr = pp_buffer + pp_save_pos;

   strcpy(s_ptr,str);
   
   pp_lp_2 = pp_lp_1;
   pp_lp_1 = pp_save_pos;
   pp_save_pos += strlen(str);
  }

/****************************************************/
/* PP_BACKUP:  Removes the last string added to the */
/*   pretty print save buffer.  Only capable of     */
/*   backing up for the two most recent additions.  */
/****************************************************/
pp_backup()
  {
   if ((pp_buffer_status == OFF) || (pp_buffer == NULL)) return;
   
   pp_save_pos = pp_lp_1;
   pp_lp_1 = pp_lp_2;
   pp_buffer[pp_save_pos] = EOS;
  }

/*****************************************************/
/* COPY_PP_BUFFER:  Makes a copy of the pretty print */
/*   save buffer.                                    */
/*****************************************************/
char *copy_pp_buffer()
  {
   int length;
   char *new_str;

   length = (1 + strlen(pp_buffer)) * sizeof (char);
   new_str = gm2(length);
   if (new_str == NULL)
     {
      cl_print("werror","Unable to allocate string buffer in copy_pp_buffer\n");
      return(NULL);
     }
   strcpy(new_str,pp_buffer);
   return(new_str);
  }
  
/*****************************************************/
/* GET_PP_BUFFER                                    */
/*****************************************************/
char *get_pp_buffer()
  {
   return(pp_buffer);
  }

/**************************************************************/
/* PRINT_IN_CHUNKS:  Prints a string in chunks to accomodate  */
/*   systems which have a limit on the maximum size of a      */
/*   string which can be printed.                             */
/**************************************************************/
print_in_chunks(log_name,str)
  char *log_name, *str;
  {
   char tc, *s_ptr;

   s_ptr = str;
   
   if (s_ptr == NULL) return;

   while (strlen(s_ptr) > 500)
     {
      tc = s_ptr[500];
      s_ptr[500] = EOS;
      cl_print(log_name,s_ptr);
      s_ptr[500] = tc;
      s_ptr += 500;
     }

   cl_print(log_name,s_ptr);
  }
  
  
/********************************************************/
/* PP_cr_and_indent:  Prints white spaces into the pretty */
/*   print buffer.                                      */
/********************************************************/
pp_cr_and_indent()
  {
   int i;

   GLOBALSTR[0] = '\n';

   for (i = 1 ; i <= INDENT_DEPTH ; i++)
     { GLOBALSTR[i] = ' '; }
   GLOBALSTR[i] = EOS;

   save_pp_buffer(GLOBALSTR);
  }
  
/********************************************************/
/* INC_INDENT_DEPTH:                                    */
/********************************************************/
inc_indent_depth(value)
  int value;
  {
   INDENT_DEPTH += value;
  }
  
/********************************************************/
/* DEC_INDENT_DEPTH:                                    */
/********************************************************/
dec_indent_depth(value)
  int value;
  {
   INDENT_DEPTH -= value;
  }
  
/********************************************************/
/* SET_INDENT_DEPTH:                                    */
/********************************************************/
set_indent_depth(value)
  int value;
  {
   INDENT_DEPTH = value;
  }
  
/***************************************************************/
/* SET_PP_BUFFER_STATUS: Sets pp_buffer_status flag to boolean */
/*   value of ON or OFF.                                       */
/***************************************************************/
set_pp_buffer_status(value)
  int value;
  {
   pp_buffer_status = value;
  }

/********************************************************/
/* STR_PRINT_REP:                                       */
/********************************************************/
char *str_print_rep(str)
  char *str;
  {
   int i = 0;
   int j = 0;
   int count = 2;
   
   /*============================================*/
   /* Determine if print buffer is large enough. */
   /*============================================*/
   
   while (str[i] != EOS)
     {
      if ((str[i] == '"') || (str[i] == '\\'))
        { count += 2; }
      else
        { count++; }
      i++;
     }
     
   if (count > (WORDLENGTH - 2))
     {
      cl_print("werror","SYSTEM ERROR\n");
      cl_print("werror","Printed representation of string\n");
      sprintf(GLOBALSTR,"exceeds maximum token length of %d\n",WORDLENGTH);
	  cl_print("werror",GLOBALSTR);
      cl_exit(2);
     }
     
   /*=========================*/
   /* Print string to buffer. */
   /*=========================*/
   
   i = 0;
   PRINT_REP[j++] = '"';
   while (str[i] != EOS)
     {
      if ((str[i] == '"') || (str[i] == '\\'))
        { 
         PRINT_REP[j++] = '\\';
         PRINT_REP[j++] = str[i];
        }
      else
        { PRINT_REP[j++] = str[i]; }
      i++;
     }
    
   PRINT_REP[j++] = '"';
   PRINT_REP[j] = EOS;
     
   return(PRINT_REP);
  }
