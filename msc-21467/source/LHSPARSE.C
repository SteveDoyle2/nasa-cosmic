/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                   LHSPARSE MODULE                   */
   /*******************************************************/

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "constant.h"
#include "scanner.h"
#include "lhsparse.h"
#include "clipsmem.h"

/****************************************/
/* LOCAL INTERNAL FUNCTIONS DEFINITIONS */
/****************************************/

   int                     declaration_parse();
   struct node            *group_patterns();
   struct node            *connected_pattern_parse();
   struct node            *not_pattern_parse();
   struct node            *test_pattern();
   struct node            *assignment_parse();
   struct node            *simple_pattern_parse();
   struct node            *sequence_restriction_parse();
   struct node            *conjuctive_restriction_parse();
   struct node            *literal_restriction_parse();
   struct node            *initial_pattern();
   
/*****************************************/
/* GLOBAL INTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/

   struct node            *rule_body_parse();
   struct node            *get_node();
   struct node            *restriction_parse();
   int                     returnnodes();

/*****************************************/
/* GLOBAL EXTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/

   extern struct test     *fctn0_parse();
   extern struct draw     *add_symbol();
   extern char            *symbol_string();
   
/****************************/
/* LOCAL INTERNAL VARIABLES */
/****************************/

   static int              LHS_ERROR = FALSE;
   
/*****************************/
/* GLOBAL EXTERNAL VARIABLES */
/*****************************/
   
   extern int              glo_salience;
   
/****************************************************/
/* RULE_BODY_PARSE:                                 */
/*                                                  */
/*   <rule-body> ::= { (declare <declaration>*) }   */
/*                     <lhs-pattern>* => <action>*  */
/****************************************************/
struct node *rule_body_parse(read_source,lhs_tkn)
  char *read_source;
  struct token *lhs_tkn;
  {
   struct node *data_spec, *other_stuff;
   
   LHS_ERROR = FALSE;
   
   /*============================================*/
   /* Parse the first pattern as a special case. */
   /*============================================*/
   
   if (lhs_tkn->token == LPAREN)
     { 
      gettoken(read_source,lhs_tkn);
      if (lhs_tkn->token == WORD)
        {
         if (strcmp(lhs_tkn->tknword,"declare") == 0)
           { 
            declaration_parse(read_source);
            data_spec = NULL;
           }
         else if (strcmp(lhs_tkn->tknword,"test") == 0)
           { data_spec = test_pattern(read_source); }
         else if (strcmp(lhs_tkn->tknword,"not") == 0)
           { data_spec = not_pattern_parse(read_source); }
         else if ((strcmp(lhs_tkn->tknword,"and") == 0) || 
                  (strcmp(lhs_tkn->tknword,"or") == 0))
           { data_spec = connected_pattern_parse(read_source,lhs_tkn); }         
         else
           { data_spec = simple_pattern_parse(read_source,lhs_tkn); }
        }
      else
        { data_spec = simple_pattern_parse(read_source,lhs_tkn); }
     }
   else if (lhs_tkn->token == BWORD)
     { data_spec = assignment_parse(read_source,lhs_tkn->hashword); }
   else if (lhs_tkn->token == SEPARATOR)
     { return(initial_pattern()); }
   else 
     { 
      cl_print("werror","\nExpected a fact address or '(' to begin ");
      cl_print("werror","a pattern\n");
      LHS_ERROR = TRUE;
      return(NULL);
     }
     
   if (LHS_ERROR == TRUE)
     {
      returnnodes(data_spec);
      return(NULL);
     }
   
   pp_cr_and_indent();
   
   /*============================================*/
   /* Parse the other patterns in the lhs. */
   /*============================================*/
   
   other_stuff = group_patterns(read_source,SEPARATOR,"=>");
   
   if (LHS_ERROR == TRUE)
     {
      returnnodes(data_spec);
      return(NULL);
     }
     
   if (data_spec == NULL)
     { data_spec = other_stuff; }
   else
     { data_spec->bottom = other_stuff; }
     
   if (data_spec == NULL) return(initial_pattern());
   
   return(data_spec);
  }

/*********************************************************/
/* declaration_parse:  Parses the declaration of a rule. */
/*                                                       */
/*   <declaration> ::= (salience <number>)               */
/*********************************************************/
static int declaration_parse(read_source)
  char *read_source;
  {  
   struct token sal_tkn;   

   float salience;

   /*===========================*/
   /* Next token must be a '('. */
   /*===========================*/

   save_pp_buffer(" ");   

   gettoken(read_source,&sal_tkn);                     
   if (sal_tkn.token != LPAREN)
     {  
      cl_print("werror","\nMissing '(' after declare\n");
      LHS_ERROR = TRUE;
      return(0);
     }
   
   /*=========================================*/
   /* Next token must be the word "salience". */
   /*=========================================*/
   
   gettoken(read_source,&sal_tkn);
   if (strcmp(sal_tkn.tknword,"salience") != 0)
     {
      cl_print("werror","\nOnly salience declarations are allowed\n");
      LHS_ERROR = TRUE;
      return(0);
     }

   /*==============================*/
   /* Next token must be a number. */
   /*==============================*/

   save_pp_buffer(" ");     

   gettoken(read_source,&sal_tkn);
   if (sal_tkn.token != NUMBER)
     {
      cl_print("werror","\nSalience value must be numeric.\n");
      LHS_ERROR = TRUE;
      return(0);
     }

   /*=======================================================*/
   /* Salience number must be in the range -10000 to 10000. */
   /*=======================================================*/

   salience = sal_tkn.tknnumber;

   if ((salience > MAX_SALIENCE) || (salience < MIN_SALIENCE))
     {  
      cl_print("werror","\nSalience value out of range ");
      print_long_int("werror",(long int) MIN_SALIENCE);
      cl_print("werror"," to ");
      print_long_int("werror",(long int) MAX_SALIENCE);
      cl_print("werror","\n");
      LHS_ERROR = TRUE;
      return(0);
     }

   /*============================================*/
   /* Salience declaration is closed with a ')'. */
   /*============================================*/   

   gettoken(read_source,&sal_tkn);
   if (sal_tkn.token != RPAREN)
     {
      cl_print("werror","\nMissing ')' in Salience declaration\n");
      LHS_ERROR = TRUE;
      return(0);
     }

   /*===================================*/
   /* Declaration is closed with a ')'. */
   /*===================================*/
 
   gettoken(read_source,&sal_tkn);
   if (sal_tkn.token != RPAREN)
     {
      cl_print("werror","\nMissing end ')' in declaration\n");
      LHS_ERROR = TRUE;
      return(0);
     }

   /*==============================================*/
   /* Return the value of the salience through the */
   /* global variable salience_value.              */
   /*==============================================*/

   glo_salience = (int) salience;
   return(0);
  }

/****************************************************************/
/* LHS_PATTERN:                                             */
/****************************************************************/
static struct node *lhs_pattern(read_source,terminator)
  char *read_source;
  int terminator;
  {
   struct token lhs_tkn;
   struct node *data_spec;
   
   gettoken(read_source,&lhs_tkn);
   if (lhs_tkn.token == LPAREN)
     { 
      gettoken(read_source,&lhs_tkn);
      if (lhs_tkn.token == WORD)
        {
         if (strcmp(lhs_tkn.tknword,"test") == 0)
           { data_spec = test_pattern(read_source); }
         else if (strcmp(lhs_tkn.tknword,"not") == 0)
           { data_spec = not_pattern_parse(read_source); }
         else if ((strcmp(lhs_tkn.tknword,"and") == 0) || 
                  (strcmp(lhs_tkn.tknword,"or") == 0))
           { data_spec = connected_pattern_parse(read_source,&lhs_tkn); }         
         else
           { data_spec = simple_pattern_parse(read_source,&lhs_tkn); }
        }
      else
        { data_spec = simple_pattern_parse(read_source,&lhs_tkn); }
     }
   else if (lhs_tkn.token == BWORD)
     { data_spec = assignment_parse(read_source,lhs_tkn.hashword); }
   else if (lhs_tkn.token == terminator)
     { return(NULL);  }
   else 
     { 
      cl_print("werror","\nExpected a fact address or '(' to begin ");
      cl_print("werror","a pattern\n");
      LHS_ERROR = TRUE;
      return(NULL);
     }

   if (LHS_ERROR == TRUE) 
     {
      returnnodes(data_spec);
      return(NULL);
     }
     
   return(data_spec);
  }
  
/*******************************************************************/
/* CONNECTED_PATTERN_PARSE:  Handles the parsing of pattern data   */
/*   specifications that begin with one of the logical connectors: */
/*   "and" and "or".  This routine is entered with the            */
/*   scanner pointing to the logical connector.  It is exited with */
/*   the scanner pointing to the closing right parenthesis of the  */
/*   connected pattern construct.                                  */
/*                                                                 */
/*   <connected-pattern> ::=  (and <condition>)+) |                */
/*                            (or  <condition>)+)                  */
/*******************************************************************/
static struct node *connected_pattern_parse(read_source,con_tkn)
  char *read_source;
  struct token *con_tkn;
  {
   int connector_value;
   struct node *data_spec;

   /*==========================================================*/
   /* Use appropriate spacing for pretty printing of the rule. */
   /*==========================================================*/

   if (strcmp(con_tkn->tknword,"or") == 0)  
     { 
      connector_value = PAT_OR;
      save_pp_buffer("  "); 
     }
   else
     {
      connector_value = PAT_AND;
      save_pp_buffer(" ");
     }

   /*========================================================*/
   /* Advance the scanner to the first point of interest in  */
   /* pattern that follows the logical connector: Either the */
   /* fact binder for the pattern or the pattern's first     */
   /* element slot.                                          */
   /*========================================================*/

   inc_indent_depth(5);
   data_spec = get_node();
   data_spec->type = connector_value;
   
   data_spec->right = group_patterns(read_source,RPAREN,")");
   
   dec_indent_depth(5);
         
   if (data_spec->right == NULL)
     {
      cl_print("werror","\nNo patterns within logical pattern construct\n");
      returnnodes(data_spec);
      LHS_ERROR = TRUE;
      return(NULL);
     }
     
   return(data_spec);
  }
  
/*******************************************************************/
/* GROUP_PATTERNS:                                                 */
/*******************************************************************/
static struct node *group_patterns(read_source,terminator,term_str)
  int terminator;
  char *read_source, *term_str;
  {
   struct node *last_node, *new_node, *data_spec;
  
   last_node = data_spec = NULL;
   
   while (TRUE)
     {
      new_node = lhs_pattern(read_source,terminator);
      
      if (LHS_ERROR == TRUE) 
        {
         returnnodes(data_spec);
         return(NULL);
        }
        
      if (new_node == NULL)
        {
         pp_backup();
         pp_backup();
         pp_cr_and_indent();
         save_pp_buffer(term_str);
           
         return(data_spec);
        }
        
      if (last_node == NULL)
        { data_spec = new_node; }
      else
        { last_node->bottom = new_node; }
        
      last_node = new_node;
      pp_cr_and_indent();
     }
  }
  
/*******************************************************************/
/* NOT_PATTERN_PARSE:                      */
/*******************************************************************/
static struct node *not_pattern_parse(read_source)
  char *read_source;
  {
   struct token not_tkn;
   struct node *data_spec;

   /*==========================================================*/
   /* Use appropriate spacing for pretty printing of the rule. */
   /*==========================================================*/

   save_pp_buffer(" ");

   /*=======================================================*/
   /* Get the opening left parenthesis for the not pattern. */
   /*=======================================================*/
   
   gettoken(read_source,&not_tkn);
   if (not_tkn.token == BWORD)
     { 
      cl_print("werror","\nAttempt to bind a not'ed pattern to ");
      cl_print("werror","a fact address\n"); 
      LHS_ERROR = TRUE;
      return(NULL);
     }
   else if (not_tkn.token != LPAREN)
     { 
      cl_print("werror","\nExpected a '(' to follow the logical pattern not\n");
      LHS_ERROR = TRUE;
      return(NULL);
     }
     
   /*========================================================*/
   /* Get the template name for the not pattern. Ensure that */
   /* a logical AND or OR was not used.                      */
   /*========================================================*/
   
   gettoken(read_source,&not_tkn);
   if ((not_tkn.token == WORD) &&
       ((strcmp(not_tkn.tknword,"and") == 0) ||
        (strcmp(not_tkn.tknword,"or") == 0) ||
        (strcmp(not_tkn.tknword,"test") == 0) ||
        (strcmp(not_tkn.tknword,"not") == 0)))
     { 
      cl_print("werror","\nLogical construct used within a not construct\n");
      LHS_ERROR = TRUE;
      return(NULL);
     }

   /*======================================================*/
   /* Parse the patterns within the logical not construct. */
   /*======================================================*/
   
   data_spec = simple_pattern_parse(read_source,&not_tkn);
   
   if (LHS_ERROR == TRUE)
     {
      returnnodes(data_spec); 
      return(NULL);
     }
     
   data_spec->state = 'n';
   data_spec->svalue = NULL;
  
   /*================================*/
   /* Return the data specification. */
   /*================================*/
     
   gettoken(read_source,&not_tkn);
   if (not_tkn.token != RPAREN)
     {
      cl_print("werror","\nExpected a ')' to close the logical pattern not\n");
      LHS_ERROR = TRUE;
      returnnodes(data_spec);
      return(NULL);
     }

   return(data_spec);
  }
  
  
/***********************************************************/
/* TEST_PATTERN:  Finished the parse of statements         */
/*   of the form:                                          */
/*                                                         */
/*   (test <safe-form>)                                    */
/* Does not advance beyond the test pattern parse.         */
/***********************************************************/
static struct node *test_pattern(read_source)
  char *read_source;
  {
   struct node *data_spec;
   struct token tst_tkn;
       
   /*=======================================================*/
   /* Create the data specification for the test construct. */
   /*=======================================================*/

   save_pp_buffer(" ");
   data_spec = get_node();
   data_spec->type = PAT_TEST;
   data_spec->expression = fctn0_parse(read_source);

   if (data_spec->expression == NULL)
     {
      LHS_ERROR = TRUE;
      returnnodes(data_spec);
      return(NULL);
     }

   /*================================================================*/
   /* Check for the closing right parenthesis of the test construct. */
   /*================================================================*/

   gettoken(read_source,&tst_tkn);
   if (tst_tkn.token != RPAREN)
     { 
      cl_print("werror","\nMissing closing ')' for test pattern\n");
      LHS_ERROR = TRUE;
      returnnodes(data_spec);
      return(NULL);
     }
  
   return(data_spec);
  }
  
/***************************************************/
/* ASSIGNMENT_PARSE:  Parses assigned-lhs-patterns */
/*   of the form:                                  */
/*                                                 */
/*   <local-variable> <- <sequence-restriction>    */
/*   Entered with fact_address already parsed.     */
/***************************************************/
static struct node *assignment_parse(read_source,fact_address)
  char *read_source;
  struct draw *fact_address;
  {
   struct node *data_spec;
   struct token lhs_tkn;

   /*===================================*/
   /* Store the name of the variable to */
   /* which the pattern is bound.       */
   /*===================================*/

   data_spec = get_node();
   data_spec->type = PATTERN;
   data_spec->state = 'o';
   data_spec->svalue = fact_address;
   data_spec->expression = NULL;
   data_spec->right = NULL;
   data_spec->bottom = NULL;

   save_pp_buffer(" ");         

   /*===============================================*/
   /* Check for binder token, "<-", after variable. */
   /*===============================================*/

   gettoken(read_source,&lhs_tkn);
   if (lhs_tkn.token != BINDER)
     {
      cl_print("werror","\nMissing <- after fact address ?");
      cl_print("werror",symbol_string(fact_address));
      cl_print("werror","\n");
      LHS_ERROR = TRUE;
      returnnodes(data_spec);
      return(NULL);
     }

   save_pp_buffer(" ");          

   /*================================================*/
   /* Check for opening left parenthesis of pattern. */
   /*================================================*/ 
        
   gettoken(read_source,&lhs_tkn);
   if (lhs_tkn.token != LPAREN)
     {
      cl_print("werror","\nMissing '(' after ?");
      cl_print("werror",symbol_string(fact_address));
      cl_print("werror"," <- \n");
      LHS_ERROR = TRUE;
      returnnodes(data_spec);
      return(NULL);
     }

   /*======================================================*/
   /* Parse the pattern and return the data specification. */
   /*======================================================*/    

   gettoken(read_source,&lhs_tkn);

   data_spec->right = sequence_restriction_parse(read_source,&lhs_tkn);
   if (LHS_ERROR == TRUE) 
     {
      returnnodes(data_spec);
      return(NULL);
     }

   return(data_spec);
  }
  
  
/*******************************************************/
/* SIMPLE_PATTERN_PARSE:  Parses assigned-lhs-patterns */
/*   of the form:                                      */
/*                                                     */
/*   <sequence-restriction>                            */
/*******************************************************/
static struct node *simple_pattern_parse(read_source,rln_tkn)
  char *read_source;
  struct token *rln_tkn;
  {
   struct node *data_spec;

   data_spec = get_node();
   data_spec->type = PATTERN;
   data_spec->state = 'o';
   data_spec->svalue = NULL;
   data_spec->expression = NULL;
   data_spec->bottom = NULL;
   data_spec->right = sequence_restriction_parse(read_source,rln_tkn);

   if (LHS_ERROR == TRUE)
     {
      returnnodes(data_spec); 
      return(NULL);
     }

   return(data_spec);
  }
  
  
/********************************************************************/
/* SEQUENCE_RESTRICTION_PARSE:                                      */
/********************************************************************/
static struct node *sequence_restriction_parse(read_source,lhs_tkn)
  char *read_source;
  struct token *lhs_tkn;
  {
   struct node *head;
   struct node *next_element, *last_element = NULL;

   head = NULL;
 
   /*========================*/
   /* Check for deftemplate. */
   /*========================*/
 
#if DEFTEMPLATES
   if (lhs_tkn->token == WORD)
     {
      extern struct dtmpl *FindDeftemplate();
      extern struct node *dtmpl_lhs_parse();
      
      if (FindDeftemplate(lhs_tkn->tknword) != NULL)
        {
         head = dtmpl_lhs_parse(read_source,lhs_tkn->hashword);
         if (head == NULL)
           { 
            LHS_ERROR = TRUE;
            return(NULL);
           }
         return(head);
        }
     }
#endif
   
   /*==============================*/
   /* Parse other simple patterns. */
   /*==============================*/
   
   while (lhs_tkn->token != RPAREN)
     {
      next_element = restriction_parse(read_source,lhs_tkn);
  
      if (LHS_ERROR == TRUE)
        {
         returnnodes(head);
         return(NULL);
        }

      if (last_element == NULL)
        { head = next_element; }
      else
        { last_element->right = next_element; }
      last_element = next_element;

      if (lhs_tkn->token != RPAREN)   
        {
         pp_backup(); 
         save_pp_buffer(" ");
         save_pp_buffer(lhs_tkn->print_rep);
        }      
     }

   if (head == NULL)
     {
      cl_print("werror","\nA pattern must have one or more fields\n");
      LHS_ERROR = TRUE;
     }

   return(head);			   
  }
  
/*************************************************************/
/* RESTRICTION_PARSE:                                        */
/*                                                           */
/*   <restriction>  ::= ? | $? | $<variable> | <conjunction> */
/*************************************************************/
struct node *restriction_parse(read_source,rst_tkn)
  char *read_source;
  struct token *rst_tkn;
  {      
   struct node *head;

   if ((rst_tkn->token == SINGLE) || 
       (rst_tkn->token == BWORDS) || 
       (rst_tkn->token == MULTIPLE))
     {
      head = get_node();
      head->type = rst_tkn->token;
      head->state = 'o';
      if (rst_tkn->token == BWORDS)
	    { head->svalue = rst_tkn->hashword; }
      gettoken(read_source,rst_tkn);
     }
   else
     { head = conjuctive_restriction_parse(read_source,rst_tkn); }
   
   return(head);			   
  }
  
/********************************************************************/
/* CONJUCTIVE_RESTRICTION_PARSE:                                    */
/*                                                                  */
/* <conjuction> ::= <disjunction> | <conjunction> & <disjunction>   */
/*                                                                  */
/* <disjuction> ::= <literal> | <disjunction> "|" <literal>         */
/********************************************************************/
static struct node *conjuctive_restriction_parse(read_source,cnj_tkn)
  char *read_source;
  struct token *cnj_tkn;
  {
   struct node *bind_node;
   struct node *new_node, *next_or, *next_and;
   int conn_type;
   
   /*=====================================*/
   /* Get the first node and determine if */
   /* it is a binding variable.           */
   /*=====================================*/
   
   new_node = literal_restriction_parse(read_source,cnj_tkn);
   
   if (LHS_ERROR == TRUE)
     { return(NULL); }
   
   gettoken(read_source,cnj_tkn);
   
   if ((new_node->type == BWORD) && 
       (new_node->state == 'o') &&
       (cnj_tkn->token != LOR))
     { 
      bind_node = new_node; 
      next_or = NULL;
      next_and = NULL;
     }
   else
     {
      bind_node = get_node();
      bind_node->type = SINGLE;
      bind_node->state = 'o';
      bind_node->bottom = new_node;
      next_or = new_node;
      next_and = new_node;
     }
     
   /*=====================================*/
   /* Get the first node and determine if */
   /* it is a binding variable.           */
   /*=====================================*/
      
   while ((cnj_tkn->token == LOR) || (cnj_tkn->token == LAND))
     {
      conn_type = cnj_tkn->token;

      gettoken(read_source,cnj_tkn);
      new_node = literal_restriction_parse(read_source,cnj_tkn);      

      if (LHS_ERROR == TRUE)
        { 
         returnnodes(bind_node);
         return(NULL);
        }
        
      if (conn_type == LOR)
        {
         if (next_or == NULL)
           { bind_node->bottom = new_node; }
         else
           { next_or->bottom = new_node; }
         next_or = new_node;
         next_and = new_node;
        }
      else if (conn_type == LAND)
        {
         if (next_and == NULL)
           { 
            bind_node->bottom = new_node;
            next_or = new_node;
           }
         else
           { next_and->right = new_node; }
         next_and = new_node;
        }
      else
        {
         clips_system_error(501);
         cl_exit(4);
        }

      /*==================================================*/
      /* Determine if any more restrictions are connected */
      /* to the current list of restrictions.             */
      /*==================================================*/

      gettoken(read_source,cnj_tkn);
     }
     
   return(bind_node);
  }

/*******************************************/
/* LITERAL_RESTRICTION_PARSE:              */
/*                                         */
/*   <literal> ::= <term> | ~ <term>       */
/*                                         */
/*   <term>    ::= <atom>         |        */
/*                 <variable>     |        */
/*                 = <value-form> |        */
/*                 : <predicate>           */
/*                                         */
/*   <atom>    ::= <symbol> |              */
/*                 <string> |              */
/*                 <number>                */
/*******************************************/
static struct node *literal_restriction_parse(read_source,ltr_tkn)
  char *read_source;
  struct token *ltr_tkn;
  {     
   struct node *head;
   
   head = get_node();

   /*=======================================================*/
   /* Determine if the field has a '~' preceding it.  If it */
   /* does, then the field has 'n' or negative state logic. */
   /* Otherwise the field has 'o' or ordinary state logic.  */
   /*=======================================================*/

   if (ltr_tkn->token == LNOT)
     {
      gettoken(read_source,ltr_tkn);
      head->state = 'n';
     }
   else
     { head->state = 'o'; }
   
   /*================================================================*/ 
   /* Determine if the field is valid.  Valid fields are ?variables, */
   /* words, strings, numbers, :(expression), and =(expression).     */
   /*================================================================*/ 
   
   head->type = ltr_tkn->token;

   if (ltr_tkn->token == WORD)
     {
      if (strcmp(ltr_tkn->tknword,"=") == 0)
        {
         head->type = FCALL;
         head->expression = fctn0_parse(read_source);
         if (head->expression == NULL) 
           {
            LHS_ERROR = TRUE;
            returnnodes(head);
            return(NULL);
           }       
        }
      else if (strcmp(ltr_tkn->tknword,":") == 0)
        {
         head->type = COAMP;
         head->expression = fctn0_parse(read_source);
         if (head->expression == NULL) 
           {
            LHS_ERROR = TRUE;
            returnnodes(head);
            return(NULL);
           }       
        }
      else
        { head->svalue = ltr_tkn->hashword; }
     }
   else if ((ltr_tkn->token == BWORD)  || 
       (ltr_tkn->token == STRING))
     { head->svalue = ltr_tkn->hashword; }
   else if (ltr_tkn->token == NUMBER)
     { head->fvalue = ltr_tkn->tknnumber; }
   else
     {  
      cl_print("werror","\nExpected a <string>, <word>, <number>, ");
      cl_print("werror","<variable>, ':', or '='\n ");
      LHS_ERROR = TRUE;
      returnnodes(head);
      return(NULL);
     }

   return(head);
  }

/***********************************************************/
/* INITIAL_PATTERN: Creates the pattern (initial-fact) for */
/*   use in rules which have no lhs patterns.              */
/***********************************************************/
static struct node *initial_pattern()
  {
   struct node *head;
   
   head = get_node();
   head->type = PATTERN;
   head->state = 'o';
   head->right = get_node();
   head->right->type = SINGLE;
   head->right->state = 'o';
   head->right->bottom = get_node();
   head->right->bottom->state = 'o';
   head->right->bottom->type = WORD;
   head->right->bottom->svalue = add_symbol("initial-fact");
   return(head);
  }
  
/***********************************************************/
/* GET_NODE                                                */
/***********************************************************/
struct node *get_node()
  {
   struct node *new_node;
   
   new_node = get_struct(node);
   new_node->type = KUNKNOWN;
   new_node->state = '0';
   new_node->fvalue = 0.0;
   new_node->svalue = NULL;
   new_node->expression = NULL;
   new_node->right = NULL;
   new_node->bottom = NULL;
   
   return(new_node);
  }
   
/*******************************************************************/
/* RETURNNODES:  Returns a multiply linked list of node structures */
/*   to the list of free nodes.                                    */
/*******************************************************************/
returnnodes(waste)
  struct node *waste;
  {
   if (waste != NULL)
     {
      returnnodes(waste->right);
      returnnodes(waste->bottom);
      if (waste->expression != NULL) returntests(waste->expression); 
      rtn_struct(node,waste);
     } 
  }

#endif
