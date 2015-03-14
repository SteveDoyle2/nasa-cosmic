#include <stdio.h>
#include "crsv.h"

/*****************************************************************
 *  This file contains the stubs for user defined functions which
 *  process rule information. This file should be replaced, or
 *  the functions modified to allow user defined processing of
 *  the information in the rule system.
 *****************************************************************
 */

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_startup
 *
 *  PURPOSE:  This function is called during early startup, just
 *            after processing all command line arguments.
 *
 *  INPUTS :  None.
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

void usr_startup()
{}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_new_file()
 *
 *  PURPOSE:  This function is called before a file of rules
 *            begins processing.
 *
 *  INPUTS :  The name of the file (char *)
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_new_file(name)
char *name;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_start_comment()
 *
 *  PURPOSE:  This function is called when CRSV finds the 
 *            initial semi-colon which starts a comment line.
 *
 *  INPUTS :  A single character, (a ';') (char)
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_start_comment(c)
char c;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_process_comment()
 *
 *  PURPOSE:  This function is called for each character that
 *            is in a comment line. This allows the user to 
 *            capture comment information, if so desired.
 *
 *  INPUTS :  The character in the comment line (char)
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_process_comment(c)
char c;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_end_comment()
 *
 *  PURPOSE:  This function is called when a comment line ends
 *
 *  INPUTS :  A character, typically a newline (\n) (char)
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_end_comment(c)
char c;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_deffact()
 *
 *  PURPOSE:  This function is called after processing all the
 *            information in a deffact block.
 *
 *  INPUTS :  A pointer to the structure holding the deffact
 *            info (FACT_BLOCK *).
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_deffact(cur_fb)
FACT_BLOCK *cur_fb;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_rule()
 *
 *  PURPOSE:  This function is called after processing all the
 *            information in a rule.
 *
 *  INPUTS :  Three arguments,
 *            A pointer to the structure holding all the rule
 *            information (RULE *),
 *            A pointer to the variable list for bound variables,
 *             (VAR_PTR), and
 *            A pointer to the variable list for variables bound
 *            to facts (VAR_PTR).
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_rule(cur_rule, variables, retracts)
RULE    *cur_rule;
VAR_PTR  variables, retracts;
{ }

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  usr_summary()
 *
 *  PURPOSE:  This function is called after all information has
 *            been processed to allow user processing at the end.
 *
 *  INPUTS :  Four arguments,
 *             A pointer to the deffacts list (FACT_BLOCK *)
 *             A pointer to the rules list (RULE *)
 *             A pointer to the relations list (RELATION *)
 *             A pointer to the list of external functions
 *               (EX_FUNC *)
 *
 *  RETURNS:  None.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

/*ARGSUSED*/
void usr_summary(deffacts, rules, relations, ex_funcs)
FACT_BLOCK  *deffacts;
RULE        *rules;
RELATION    *relations;
EX_FUNC    *ex_funcs;
{ }
