#include <stdio.h>
#include "crsv.h"
#if MAC_LSC
#include <strings.h>
#else
#include <string.h>
#endif
/********************************************************************
 *  This file contains all the functions used to display messages
 *  during execution as well as the functions which display summary
 *  information at the end of a run. There are entry points for
 *  each type of summary printout; rules, deffacts, relations
 *  and external functions, as well as message functions.
 ********************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

#if(IBM_TBC || IBM_MSC) 

/* -------------------
 *  from CRSVPC.C
 * -------------------
 */

extern void crsv_error_message();
extern void crsv_message();

#endif

/* -------------------------
 *  From the file: CRSV.C
 * -------------------------
 */

extern DT_PTR def_tmp_head;

/* ---------------------------
 *  From the file: CRSVTMP.C
 * ---------------------------
 */

extern DT_PTR find_def_tmp();
extern DS_PTR find_def_slot_by_position();

/* ---------------------------
 *  From the file: CRSVTR.C
 * ---------------------------
 */

extern ACT_REL_PTR find_act_rel();


/* ===========  Functions defined here for Global use  ================ */

void rel_printout();
void rule_printout();
void fb_printout();
void ex_funcs_printout();
void error_message();
void send_message();
void send_text ();

/* ===========  Functions defined here for internal use  ============== */

void print_rule_info();
void print_find_rule();
void print_find_rel();
void print_rel_info();
void print_ex_func_info();
void print_find_ex_func();
void print_def_rel_info();
void print_find_def_rel();
void print_field_defs();


/* ===========  Variables defined here for Global use  ================ */

/* -----------------------
 *  From the file: CRSV.C
 * -----------------------
 */

extern int NUM_ERRORS;
extern int NUM_WARNINGS;


/* ===========  Variables defined Externally but used here  =========== */

extern char     *cur_file_name;         /* Name of the current file */
extern char     *cur_obj_name;          /* Name of current object   */
extern int       cur_line_num;          /* Current line number      */
extern short     issued_error_message;  /* Set when error_message called */
extern ACT_REL_PTR  act_rel_head;       /* List of dribble file relations */


/* ===========  Variables defined here for internal use  ============== */


/* ===================================================================
 *                       FACT BLOCK Printout Functions
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  fb_printout
 *
 *  PURPOSE:  This function prints summary information about
 *            all the deffacts used in a KB.
 *
 *  INPUTS :  A pointer to a list of deffacts (FB_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
void fb_printout(list_head)
FB_PTR list_head;
{
   FB_PTR   temp;

   IF(list_head NEQ NULL) THEN   
      send_message
         ("\n\n===================== DEFFACTS SUMMARY ====================",NO);
      send_message
         ("\n  Name                                      # of facts  File",NO);

      temp = list_head;
      while(temp NEQ NULL) DO
         sprintf(msg_buf, "\n  %-43s  %6d   %-25s", temp->name, 
            temp->num_facts, temp->file);
         send_message(msg_buf,NO);
         temp = temp->next_block;
      END_WHILE

      send_message("\n\n",NO);
   END_IF
}

/* ===================================================================
 *                          RULE Printout Functions
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  rule_printout
 *
 *  PURPOSE:  This function prints summary information about
 *            all the rules used in a KB.
 *
 *  INPUTS :  A pointer to a list of rules (RULE_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Merely prints a header then calls a recursive
 *            function which actually works through the
 *            binary tree.
 * -------------------------------------------------------------
 */

/*ARGSUSED*/ /* Make lint happy */
void rule_printout(node, drb_node)
RULE_PTR   node;
ACT_R_PTR  drb_node;
{
   ACT_R_PTR  rule_ptr;

   IF(node NEQ NULL) THEN
      send_message
         ("\n\n======================= RULE SUMMARY ======================",NO);
      print_find_rule(node);
   END_IF
   
   IF((ANALYZE_TRACE IS_ON AND VERBOSE IS_ON) AND (drb_node NEQ NULL)) THEN
      send_message
("\n\n===================== Dribble file rule summary ====================",NO);

      send_message
         ("Rule                                                    Times  Times  Times\n",NO);
      send_message
         ("Names                                                   Fired  Acti   Deact\n",NO);
      send_message
         ("                                                               vated  ivated\n",NO);
      send_message
         ("-----                                                   -----  -----  ------\n",NO);

      rule_ptr = drb_node;
      while(rule_ptr NEQ NULL) DO
         sprintf(msg_buf,"%-54.54s  %5d  %5d  %5d\n",
                 rule_ptr->name,
                 rule_ptr->fire_num, 
                 rule_ptr->activate_num, 
                 rule_ptr->deactivate_num);
          send_message(msg_buf,NO);
          rule_ptr = rule_ptr->next;
      END_WHILE
   END_IF
}

/*
 * -------------------------------------------------------------
 *  NAME   :  print_find_rule
 *
 *  PURPOSE:  This is a recursive function which processes
 *            through all nodes in a binary tree of rule
 *            structures.
 *
 *  INPUTS :  A pointer to a list of rules (RULE_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Just processes binary tree nodes, actual
 *            printing done in print_rule_info.
 * -------------------------------------------------------------
 */
 
static void print_find_rule(node)
RULE_PTR node;
{
   IF(node NEQ NULL) THEN
     print_find_rule(node->lft_rule);
     print_rule_info(node);
     print_find_rule(node->rht_rule);
   END_IF
}

/*
 * -------------------------------------------------------------
 *  NAME   :  print_rule_info
 *
 *  PURPOSE:  This is the function which actually prints
 *            rule summary information.
 *
 *  INPUTS :  A pointer to a rule (RULE_PTR *)
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

static void print_rule_info(node)
RULE_PTR node;
{
   char *dum;

   IF(node->salience_set IS_YES) THEN
      dum = "Yes";
   ELSE
      dum = "No ";
   END_IF

   sprintf(msg_buf, "\n\nINFO FOR RULE: %.40s\nFrom file    : %.20s",
      node->name, node->file);
   send_message(msg_buf,NO);
   send_message(
   "\nSalience  Salience   # of      # of     # of      # of     # of",NO);
   send_message(
   "\n  Set?     Value    patterns  Actions  Retracts  Asserts  Ex Funcs",NO); 

   sprintf(msg_buf, "\n  %s  %8d  %7d  %8d  %7d  %8d  %7d", dum,
      node->salience_val, node->num_patterns, node->num_actions,
      node->num_retracts, node->num_asserts,  node->num_ex_funcs);
   send_message(msg_buf,NO);
}
      
/* ===================================================================
 *                       Relation Printout Functions
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  rel_printout
 *
 *  PURPOSE:  This function prints summary information about
 *            all the relations used in a KB.
 *
 *  INPUTS :  A pointer to a list of relations (REL_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Merely prints a header then calls a recursive
 *            function which actually works through the
 *            binary tree.
 * -------------------------------------------------------------
 */

void rel_printout(node /* , drb_node*/)
REL_PTR      node;
/*ACT_REL_PTR  drb_node; Not currently used. */
{
   send_message
      ("\n\n===============  RELATION/TEMPLATE SUMMARY  ===============",NO);

   print_find_rel(node);
}


/*
 * -------------------------------------------------------------
 *  NAME   :  print_find_rel
 *
 *  PURPOSE:  This is a recursive function which processes
 *            through all nodes in a binary tree of relation
 *            structures.
 *
 *  INPUTS :  A pointer to a list of relations (REL_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Just processes binary tree nodes, actual
 *            printing done in print_rel_info.
 * -------------------------------------------------------------
 */
 
static void print_find_rel(node)
REL_PTR node;
{
   IF(node NEQ NULL) THEN
     print_find_rel(node->lft_rel);
     print_rel_info(node);
     print_find_rel(node->rht_rel);
   END_IF
}

/*
 * -------------------------------------------------------------
 *  NAME   :  print_rel_info
 *
 *  PURPOSE:  This is the function which actually prints
 *            relation summary information.
 *
 *  INPUTS :  A pointer to a relation (REL_PTR *)
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

static void print_rel_info(node)
REL_PTR node;
{
   FBR_PTR     temp_fbr;
   RR_PTR      temp_rr;
   FLD_PTR     temp_fld;
   WL_PTR      temp_wl;
   DT_PTR      template;
   DS_PTR      slot;
   int         MULTI;
   ACT_REL_PTR temp_act;
 
   send_message
      ("\n\n\n-----------------------------------------------------------\n",NO);

   template = find_def_tmp(def_tmp_head,node->name);
   IF(template) THEN
     sprintf(msg_buf,"   TEMPLATE NAME: %.40s",template->name);
   ELSE
     sprintf(msg_buf,"   RELATION NAME: %.40s",node->name);
   END_IF
   send_message(msg_buf,NO);
   send_message
      ("\n-----------------------------------------------------------",NO);
   
   IF(node->fb_list NEQ NULL) THEN
      send_message("\n\nAppears in the following deffacts:",NO);
      temp_fbr = node->fb_list;
      while(temp_fbr NEQ NULL) DO
         IF(temp_fbr->num_fields > 0) THEN
            sprintf(msg_buf, "\n%.40s  (%d fields)",
                    temp_fbr->fact_block->name, temp_fbr->num_fields);
            send_message(msg_buf,NO);
         ELSE
            sprintf(msg_buf, "\n%.40s  (%d fields, with $?)",
                    temp_fbr->fact_block->name, abs(temp_fbr->num_fields));
            send_message(msg_buf,NO);
         END_IF
         temp_fbr = temp_fbr->next_fbr;
      END_WHILE
   END_IF

   IF(node->rule_list NEQ NULL) THEN
      send_message("\n\nAppears on the LHS of the following rules:",NO);
      temp_rr = node->rule_list;
      while(temp_rr NEQ NULL) DO
         IF(temp_rr->num_fields > 0) THEN
            sprintf(msg_buf, "\n   %.40s  (%d fields)",
                    temp_rr->rule->name, temp_rr->num_fields);
            send_message(msg_buf,NO);
         ELSE
            sprintf(msg_buf, "\n   %.40s  (%d fields, with $?)",
                    temp_rr->rule->name, abs(temp_rr->num_fields));
            send_message(msg_buf,NO);
         END_IF
         temp_rr = temp_rr->next_rr;
      END_WHILE
   END_IF

   IF(node->assert_list NEQ NULL) THEN
      send_message("\n\nAsserted by the following rules:",NO);
      temp_rr = node->assert_list;
      while(temp_rr NEQ NULL) DO
         IF(temp_rr->num_fields > 0) THEN
            sprintf(msg_buf, "\n   %.40s  (%d fields)",
                    temp_rr->rule->name, temp_rr->num_fields);
            send_message(msg_buf,NO);
         ELSE
            sprintf(msg_buf, "\n   %.40s  (%d fields, with $?)",
                    temp_rr->rule->name, abs(temp_rr->num_fields));
            send_message(msg_buf,NO);
         END_IF
         temp_rr = temp_rr->next_rr;
      END_WHILE
   END_IF

   IF(node->retract_list NEQ NULL) THEN
      send_message("\n\nRetracted by the following rules:",NO);
      temp_rr = node->retract_list;
      while(temp_rr NEQ NULL) DO
         sprintf(msg_buf, "\n   %.40s", temp_rr->rule->name);
         send_message(msg_buf,NO);
         temp_rr = temp_rr->next_rr;
      END_WHILE
   END_IF

   temp_fld = node->field_list;
   IF(temp_fld) THEN
      send_message("\n\nThe following literal values were used:",NO);

      MULTI = 0;

      WHILE (temp_fld) DO 

         IF(template) THEN
           slot = find_def_slot_by_position(template,temp_fld->field_num);
           IF((slot->min_elements NEQ 1) OR (slot->max_elements NEQ 1)) THEN
             IF(!MULTI) THEN
               sprintf(msg_buf,"\n   In multi-field <%.40s>",slot->name);
               send_message(msg_buf,NO);
               MULTI = 1;
             END_IF
           ELSE
             sprintf(msg_buf,"\n   In field <%.40s>",slot->name);
             send_message(msg_buf,NO);
           END_IF
         ELSE
           sprintf(msg_buf, "\n   In field %d:", temp_fld->field_num);
           send_message(msg_buf,NO);
         END_IF

         temp_wl = temp_fld->lit_values;
         WHILE (temp_wl) DO
            sprintf(msg_buf, "\n\t%.40s (%d times)", 
                    temp_wl->word, temp_wl->times_used);
            send_message(msg_buf,NO);
            temp_wl = temp_wl->next_word;
         END_WHILE
         temp_fld = temp_fld->next_field;
      END_WHILE
   END_IF

   IF(ANALYZE_TRACE IS_ON AND VERBOSE IS_ON) THEN
      send_message("\n\nIn the dribble file, this relation was ",NO);
      temp_act = find_act_rel(act_rel_head, node->name);
      IF(temp_act EQ NULL) THEN
         send_message("never asserted or retracted",NO);
      ELSE
         sprintf(msg_buf,"\n   Asserted  %5d times", temp_act->num_assert);
         send_message(msg_buf,NO);
         sprintf(msg_buf,"\n   Retracted %5d times", temp_act->num_retract);
         send_message(msg_buf,NO);
         sprintf(msg_buf,
              "\n   A max of  %5d facts with this relation existed at one time",
              temp_act->max_occurences);
         send_message(msg_buf,NO);
      END_IF
   END_IF
}


/* ===================================================================
 *                 External Function Printout Functions
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  ex_funcs_printout
 *
 *  PURPOSE:  This function prints summary information about
 *            all the external functions used in a KB.
 *
 *  INPUTS :  A pointer to a list of external function
 *            (EXF_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
void ex_funcs_printout(list_head)
EXF_PTR list_head;
{
   send_message
      ("\n\n================ EXTERNAL FUNCTIONS SUMMARY ===============",NO);
   print_find_ex_func(list_head);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  print_find_ex_func
 *
 *  PURPOSE:  This is a recursive function which processes
 *            through all nodes in a binary tree of external
 *            function structures.
 *
 *  INPUTS :  A pointer to a list of functions (EXF_PTR *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Just processes binary tree nodes, actual
 *            printing done in print_ex_func_info.
 * -------------------------------------------------------------
 */
 
static void print_find_ex_func(node)
EXF_PTR node;
{
   IF(node NEQ NULL) THEN
     print_find_ex_func(node->lft_ex_func);
     print_ex_func_info(node);
     print_find_ex_func(node->rht_ex_func);
   END_IF
}

/*
 * -------------------------------------------------------------
 *  NAME   :  print_ex_func_info
 *
 *  PURPOSE:  This is the function which actually prints
 *            external function summary information.
 *
 *  INPUTS :  A pointer to a function (EXF_PTR *)
 *
 *  RETURNS:  Nothing.
 * -------------------------------------------------------------
 */

static void print_ex_func_info(node)
EXF_PTR node;
{
   RR_PTR   temp_rr;
 
   sprintf(msg_buf, "\n\n----- FUNCTION: %.40s -----", node->name);
   send_message(msg_buf,NO);

   IF(node->LHS_rule_list NEQ NULL) THEN
      send_message("\n\nAppears on the LHS of the following rules:",NO);
      temp_rr = node->LHS_rule_list;
      while(temp_rr NEQ NULL) DO
         IF(temp_rr->num_fields EQ 1) THEN
            sprintf(msg_buf, "\n%.40s  (1 argument)",
                    temp_rr->rule->name);
         ELSE
            sprintf(msg_buf, "\n%.40s  (%d arguments)",
                    temp_rr->rule->name, temp_rr->num_fields);
         END_IF
         send_message(msg_buf,NO);
         temp_rr = temp_rr->next_rr;
      END_WHILE
   END_IF

   IF(node->RHS_rule_list NEQ NULL) THEN
      send_message("\n\nAppears on the RHS of the following rules:",NO);
      temp_rr = node->RHS_rule_list;
      while(temp_rr NEQ NULL) DO
         IF(temp_rr->num_fields EQ 1) THEN
            sprintf(msg_buf, "\n%.40s  (1 argument)",
                    temp_rr->rule->name);
         ELSE
            sprintf(msg_buf, "\n%.40s  (%d arguments)",
                    temp_rr->rule->name, temp_rr->num_fields);
         END_IF
         send_message(msg_buf,NO);
        temp_rr = temp_rr->next_rr;
      END_WHILE
   END_IF
}
	   
/* ===================================================================
 *                 Defrelation Printout Functions
 *               (primarily for debugging purposes)
 * ===================================================================
 */

void def_rel_printout(node)
DR_PTR node;
{
   IF(node NEQ NULL) THEN
      send_message
         ("\n\n=================== Defrelation SUMMARY ==================",NO);
      print_find_def_rel(node);
   END_IF
}

static void print_find_def_rel(node)
DR_PTR node;
{
   IF(node NEQ NULL) THEN
     print_find_def_rel(node->lft_def_rel);
     print_def_rel_info(node);
     print_find_def_rel(node->rht_def_rel);
   END_IF
}

static void print_def_rel_info(node)
DR_PTR node;
{
   sprintf(msg_buf, "\n\nINFO FOR Defrelation: %.40s\nFrom file    : %.20s",
           node->name, node->file);
   send_message(msg_buf,NO);
   sprintf(msg_buf, "\nMax number fields: %d   Min number fields: %d", 
           node->max_fields, node->min_fields);
   send_message(msg_buf,NO);
   print_field_defs(node->fields);
}

static void print_field_defs(list)
DF_PTR list;
{
   DF_PTR temp;
   WL_PTR temp_wl;
   NL_PTR temp_nl;
   
   temp = list;
   
   while(temp NEQ NULL) DO
      sprintf(msg_buf, "\n\nFor Field %d words: %d strings: %d numbers: %d",
              temp->position, temp->allow_word, 
              temp->allow_string, temp->allow_number);
      send_message(msg_buf,NO);

      send_message("\nAllowed strings: ",NO);
      temp_wl = temp->possible_strings;
      IF(temp_wl EQ NULL) THEN
         send_message("pointer is null",NO);
      ELSE
         while(temp_wl NEQ NULL) DO
            sprintf(msg_buf, "%.40s ",temp_wl->word);
            send_message(msg_buf,NO);
            temp_wl = temp_wl->next_word;
         END_WHILE
      END_IF

      send_message("\nAllowed words: ",NO);
      temp_wl = temp->possible_words;
      IF(temp_wl EQ NULL) THEN
         send_message("pointer is null",NO);
      ELSE
         while(temp_wl NEQ NULL) DO
            sprintf(msg_buf, "%.40s ",temp_wl->word);
            send_message(msg_buf,NO);
            temp_wl = temp_wl->next_word;
         END_WHILE
      END_IF

      send_message("\nAllowed numbers: ",NO);
      temp_nl = temp->possible_numbers;
      IF(temp_nl EQ NULL) THEN
         send_message("pointer is null",NO);
      ELSE
         while(temp_nl NEQ NULL) DO
            sprintf(msg_buf, "%f ",temp_nl->number);
            send_message(msg_buf,NO);
            temp_nl = temp_nl->next_number;
         END_WHILE
      END_IF

      sprintf(msg_buf,
         "\nRANGE min set: %d min value %f max set %d max value %f",
         temp->set_min, temp->min_range, temp->set_max, temp->max_range);
      send_message(msg_buf,NO);

      temp = temp->next_field;
   END_WHILE
}
      
/* ===================================================================
 *                 Processing Message Functions
 *
 *  All outputs from anywhere in CRSV are sent through these two
 *  functions. Any future outputs should also be sent through here!
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME:          error_message
 *
 *  PURPOSE:       This function prints error messages that are
 *                 generated at various points during processing.
 *                 It adds information about where the error
 *                 occurred (file, object, approx. line #).
 *                 Currently, it merely prints to stdout using a
 *                 call to printf.
 *
 *  INPUTS:        Two arguments. The first is an integer which
 *                 represents the type, WARNING or ERROR. The
 *                 second is a pointer to the message.
 *
 *  RETURNS:       Nothing.
 * -------------------------------------------------------------
 */

void error_message(type, str)
int   type;
char *str;
{
   char *buf = type EQ ERROR ? "ERROR  " : "Warning";
   char  big_buf[WORDLENGTH];
   static char *last_file_with_msg = NULL;

   IF (type EQ ERROR) THEN
     NUM_ERRORS++;
   ELSE
     NUM_WARNINGS++;
   END_IF
   
   /* --------------------------------------------------
    *  Only output style warnings if CHECK_STYLE is not
    *  turned on (the s option was not specified)
    * ---------------------------------------------------
    */

   IF ((type EQ WARNING AND CHECK_STYLE IS_ON) OR
       (type EQ ERROR)) THEN

     issued_error_message = YES;

     IF (last_file_with_msg NEQ cur_file_name AND VERBOSE IS_OFF) THEN
        last_file_with_msg = cur_file_name;

        send_message
    ("\n\n========================  PROCESSING FILE  =======================",NO);
        send_message("\n\nFILENAME: ",NO);
        send_message (cur_file_name,NO);
        send_message ("\n",NO);
     END_IF

#if MAC_LSC
       sprintf(big_buf, "\n%s (%5d) in: %-.58s\n        %s", 
         buf, cur_line_num, cur_obj_name, str);
       display_print(big_buf);
#else /* !MAC_LSC */
#if IBM_TBC || IBM_MSC
   IF (!IBM_COM_LINE) THEN
         char error_str[25];

         sprintf(error_str,"\n%s (%5d) in: ", buf, cur_line_num);
         sprintf(big_buf, "%-.55s\n       %s", cur_obj_name, str);

         crsv_error_message(error_str,big_buf);
   ELSE
     	 sprintf(big_buf, "\n%s (%5d) in: %-.58s\n        %s", 
        	 buf, cur_line_num, cur_obj_name, str);

      	(void)printf(big_buf);
   END_IF
#else /* !(IBM_TBC || IBM_MSC) */
   sprintf(big_buf, "\n%s (%5d) in: %-.58s\n        ",
      buf, cur_line_num, cur_obj_name);

   send_text (big_buf, 8);
   send_text (str, 8);
#endif /* IBM_TBC || IBM_MSC */
#endif /* MAC_LSC*/

   END_IF

   return;
}

/*
 * -------------------------------------------------------------
 *  NAME:         send_message
 *
 *  PURPOSE:      This function prints messages to the same
 *                place error_message does, but without the
 *                added info. Currently, it merely
 *                prints to stdout using a call to printf.
 *
 *  INPUTS:       A single argument, a string pointer to the 
 *                message to be printed.
 *
 *  RETURNS:      Nothing.
 *
 *  NOTES:        Does not add leading CR/LF.
 * -------------------------------------------------------------
 */

void send_message(str,is_warning)
char *str;
int is_warning;
{

if((is_warning) && (CHECK_STYLE IS_OFF))
  return;
#if MAC_LSC
   display_print(str);
#else /* !MAC_LSC */
#if IBM_TBC || IBM_MSC
   IF (!IBM_COM_LINE) THEN
      crsv_message(str);
   ELSE
      (void)printf("%s", str);
   END_IF
#else /*!( IBM_TBC || IBM_MSC) */
   send_text (str, 0);
#endif/*  IBM_TBC || IBM_MSC */
#endif /* MAC_LSC */

   return;
}

/*
 * -------------------------------------------------------------
 *  NAME:         send_text
 *
 *  PURPOSE:      This function prints word-wrapped text to the
 *                output device.
 *
 *  INPUTS:       The new text to print and the line pad when
 *                a wrap occurs.
 *
 *  RETURNS:      Nothing.
 *
 *  NOTES:        Does not add leading CR/LF.
 *                The line pad is ignored when explicit new
 *                line characters are processed.
 * -------------------------------------------------------------
 */

void send_text(str, pad)
char *str;
int pad;
{
   static int space_left = LINELENGTH;
   char *next_line = strchr (str, '\n');

   IF (next_line NEQ NULL) THEN
      IF (next_line NEQ str) THEN
         *next_line = '\0';
         send_text (str, pad);
         *next_line = '\n';
      END_IF

      (void)printf ("\n");
      space_left = LINELENGTH;

      IF (*(next_line + 1) NEQ '\0') THEN
         send_text (next_line + 1, pad);
      END_IF
   ELSE
      int len = strlen (str), p;

      IF (len > space_left) THEN
         char *continuation_string = &str[space_left];
   
         WHILE (*continuation_string != ' ' AND continuation_string > str) DO
            continuation_string --;
         DONE
   
         IF (continuation_string == str AND (continuation_string = strchr (&str[space_left], ' ')) EQ NULL) THEN
            (void)printf ("%s\n", str);
            space_left = LINELENGTH;
         ELSE
            *continuation_string = '\0';
            (void)printf ("%s\n", str);
            *continuation_string = ' ';
   
            FOR (p = 0; p < pad; p++) DO
               (void)printf (" ");
            DONE

            space_left = LINELENGTH - pad;

            send_text (continuation_string + 1, pad);
         END_IF
      ELSE
         (void)printf ("%s", str);
         space_left -= len;
      END_IF
   END_IF
   
   return;
}

