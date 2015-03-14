#include <stdio.h>
#include "crsv.h"

/*
 *****************************************************************
 *  This file contains the functions used for final analysis of
 *  the knowledge base. 
 *****************************************************************
 */

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern NL_PTR     alloc_nl();
extern COUNTER   *alloc_counter();
extern void       gen_free();

/* -----------------------------
 *  From the file: CRSVPRN.C
 * -----------------------------
 */

extern void    send_message();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern int     is_word_in_list();

/* ----------------------------
 *  From the file: CRSVTMP.C
 * ----------------------------
 */

extern int     count_temps();
extern DT_PTR  find_def_tmp();

/* -----------------------------
 *  From the file: CRSVTR.C
 * -----------------------------
 */

extern void    check_dynamic_rules();
extern void    check_dynamic_relations();


/* ===========  Functions defined here for Global use  ================ */

void   analysis();


/* ===========  Functions defined here for internal use  ============== */

void     check_RHS_loops();
void     find_loop_info();
void     print_loop_warning();
void     count_loops();
int      check_match_assertions();
void     find_salience_info();
void     print_salience_warnings();
void     count_saliences();
void     count_salience_values();
DEF_EXT *check_def_ex_assertions();



/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */

extern DEF_EXT     *def_ext_head;
extern DT_PTR       def_tmp_head;
extern int          NUM_ERRORS;
extern int          NUM_WARNINGS;


/* ===========  Variables defined here for internal use  ============== */

#define WARNING_MIN_RULES    10

#define MAX_LOOPS            50
#define WARNING_LOOPS        10

#define MAX_SALIENCE         50
#define WARNING_SALIENCE     25
#define MAX_SALIENCE_LEVELS   7

NUM_LIST  *salience_head;

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  analysis()
 *
 *  PURPOSE:  This function is the primary entry point for a
 *            series of functions which attempt to analyze the
 *            collected info on a CLIPS knowledge base.
 *
 *  INPUTS :  Eight arguments,
 *             A pointer to the deffacts list (FACT_BLOCK *)
 *             A pointer to the rules list (RULE *)
 *             A pointer to the relations list (RELATION *)
 *             A pointer to the list of external functions
 *               (WORD_LIST *)
 *             A pointer to the list of rules found in the
 *             the dribble file (ACT_RULE *)
 *             A pointer to the list of relations found in the
 *             the dribble file (ACT_RELATION *)
 *             A pointer to the list of defined relations
 *               (DEF_REL *)
 *             A pointer to the list of defined external functions
 *               (DEF_EXT *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  This function should merely be used as the 
 *            starting point. Actual analysis is done in
 *            other functions.
 * -------------------------------------------------------------
 */
 
/*ARGSUSED*/ /* Make lint happy. */
void analysis(my_fb_head, my_rule_head, my_rel_head, my_ex_funcs_head, my_act_rule_head, 
              my_act_rel_head, my_def_rel_head, my_def_ext_head)
FACT_BLOCK    *my_fb_head;
RULE          *my_rule_head;
RELATION      *my_rel_head;
WORD_LIST     *my_ex_funcs_head;
ACT_RULE      *my_act_rule_head;
ACT_RELATION  *my_act_rel_head;
DEF_REL       *my_def_rel_head;
DEF_EXT       *my_def_ext_head;
{
   int num_rules, num_rels;

   send_message
      ("\n\n===================== STATIC ANALYSIS SUMMARY ====================\n",NO);

   num_rules = check_salience(my_rule_head);

   check_RHS_loops(my_rule_head);

   num_rels  = check_match_assertions(my_rel_head);


   IF(ANALYZE_TRACE IS_ON) THEN
      send_message
         ("\n\n===================== DYNAMIC ANALYSIS SUMMARY ===================\n",NO);

      check_dynamic_rules(my_rule_head, my_act_rule_head);
      check_dynamic_relations(my_rel_head, my_def_rel_head, my_act_rel_head);
   END_IF

   send_message
      ("\n\n==================================================================",NO);

   sprintf(msg_buf, "\n\nTotal number of rules ......... %d",num_rules);
   send_message(msg_buf,NO);

   sprintf(msg_buf," \nTotal number of templates ..... %d",
           count_temps(def_tmp_head));
   send_message(msg_buf,NO);

   sprintf(msg_buf, "\nTotal number of relations ..... %d",num_rels);
   send_message(msg_buf,NO);

   IF (NUM_ERRORS > 0) THEN
     sprintf(msg_buf, "\n\nTotal number of Errors ........ %d",NUM_ERRORS);
     send_message(msg_buf,NO);
   END_IF

   IF ((CHECK_STYLE IS_ON) AND (NUM_WARNINGS > 0)) THEN
     sprintf(msg_buf, "\nTotal number of Warnings ...... %d",NUM_WARNINGS);
     send_message(msg_buf,NO);
   END_IF
}

/* ======================================================================= */
/*                        CHECK SALIENCE FUNCTIONS                         */
/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_salience()
 *
 *  PURPOSE:  This function examines all the rules in the
 *            KB for overuse of salience and prints a 
 *            warning if one is necessary.
 *
 *  INPUTS :  A pointer to the rules list (RULE *)
 *
 *  RETURNS:  An integer, the number of rules in the KB.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
static int check_salience(node)
RULE *node;
{ 
   struct counter     *count_ptr;
   int rtn;

   count_ptr = alloc_counter();
   
   salience_head = NULL;

   find_salience_info(node, count_ptr);
   print_salience_warnings(count_ptr);

   rtn = count_ptr->prime_count;
   gen_free((char *)count_ptr, sizeof(struct counter));
   return(rtn);
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_salience_info()
 *
 *  PURPOSE:  This is a recursive function which processes
 *            the binary tree of rules.
 *
 *  INPUTS :  A pointer to the rules list (RULE *), and
 *            a pointer to a structure for count info.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Actual processing of salience info is done in 
 *            count_saliences function.
 * -------------------------------------------------------------
 */
 
static void find_salience_info(node, count)
RULE  *node;
struct counter *count;
{
   IF(node NEQ NULL) THEN
     find_salience_info(node->lft_rule, count);
     count_saliences(node, count);
     find_salience_info(node->rht_rule, count);
   END_IF
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  count_saliences()
 *
 *  PURPOSE:  This function counts the number of times
 *            salience is used.
 *
 *  INPUTS :  A pointer to the current rule (RULE *), and
 *            a pointer to a structure for count info.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
static void count_saliences(temp, count)
RULE *temp;
struct counter *count;
{
   count->prime_count++;

   IF(temp->salience_set EQ YES) THEN
      count->count_A++;
      count_salience_values(temp->salience_val, count);
   END_IF
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  count_salience_values()
 *
 *  PURPOSE:  This function counts the number of times
 *            a specific salience value is used.
 *
 *  INPUTS :  The current salience value (int), and
 *            a pointer to a structure for count info.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Stores salience values in a number list.
 * -------------------------------------------------------------
 */
 
static void count_salience_values(number, count)
struct counter *count;
int number;
{
   struct number_list *temp;

   temp  = salience_head;

   while(temp NEQ NULL) DO
      IF(temp->number EQ number) THEN
        temp->times_used++;
        return;
      END_IF

      temp = temp->next_number;
   END_WHILE

   temp              = alloc_nl();
   temp->number      = (float) number;
   temp->next_number = salience_head;
   temp->times_used  = 1;
   salience_head     = temp;
   count->count_B++;
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  print_salience_warnings()
 *
 *  PURPOSE:  This function is called after all salience info
 *            has been gathered and decides whether or not
 *            to print warnings.
 *
 *  INPUTS :  A pointer to the structure with count info (COUNTER *).
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Uses global #define flags to determine when
 *            salience has been overused.
 * -------------------------------------------------------------
 */
 
static void print_salience_warnings(count)
struct counter *count;
{
   float temp1;

   IF((CHECK_STYLE IS_ON) AND (count->prime_count NEQ 0)) THEN
      temp1 = (float) count->count_A * 100.0 / (float) count->prime_count;

     IF(((temp1 > WARNING_SALIENCE) AND 
          (count->prime_count > WARNING_MIN_RULES)) OR
         (temp1 > MAX_SALIENCE)) THEN
         sprintf(msg_buf,
            "\nWarning: A high percentage of rules (%4.2f%%) have explicit salience values",
            temp1);
         send_message(msg_buf,NO);
      END_IF
      
      IF(count->count_B > MAX_SALIENCE_LEVELS) THEN
         sprintf(msg_buf, 
         "\nWarning: A large number of different salience values used (%d). ",
          count->count_B);
         send_message(msg_buf,NO);
      END_IF
   END_IF
}

/* ======================================================================= */
/*                        CHECK IF/WHILE USAGE                             */
/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_RHS_loops()
 *
 *  PURPOSE:  This function examines all the rules in the
 *            KB for overuse of ifs or whiles and prints a 
 *            warning if one is necessary.
 *
 *  INPUTS :  A pointer to the rules list (RULE *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
static void check_RHS_loops(node)
RULE *node;
{ 
   struct counter *count_ptr;

   count_ptr = alloc_counter();

   find_loop_info(node, count_ptr);
   print_loop_warning(count_ptr);

   gen_free((char *)count_ptr, sizeof(struct counter));
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  find_loop_info()
 *
 *  PURPOSE:  This is a recursive function which processes
 *            the binary tree of rules.
 *
 *  INPUTS :  A pointer to the rules list (RULE *), and
 *            a pointer to a structure for count info.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Actual processing of if/while info is done in 
 *            count_loops function.
 * -------------------------------------------------------------
 */
 
static void find_loop_info(node, count)
RULE  *node;
struct counter *count;
{
   IF(node NEQ NULL) THEN
     find_loop_info(node->lft_rule, count);
     count_loops(node, count);
     find_loop_info(node->rht_rule, count);
   END_IF
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  count_loops()
 *
 *  PURPOSE:  This function counts the number of times
 *            if or while loops are used.
 *
 *  INPUTS :  A pointer to the current rule (RULE *), and
 *            a pointer to a structure for count info.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */
 
static void count_loops(temp, count)
RULE *temp;
struct counter *count;
{
   count->prime_count++;

   IF(temp->num_ifs > 0) THEN
      count->count_A++;
   END_IF

   IF(temp->num_whiles > 0) THEN
      count->count_B++;
   END_IF
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  print_loop_warnings()
 *
 *  PURPOSE:  This function is called after all if/while info
 *            has been gathered and decides whether or not
 *            to print warnings.
 *
 *  INPUTS :  A pointer to the structure with count info (COUNTER *).
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Uses global #define flags to determine when
 *            if/while has been overused.
 * -------------------------------------------------------------
 */
 
static void print_loop_warning(count)
struct counter *count;
{
   float perc_ifs, perc_whiles;

   IF((CHECK_STYLE IS_ON) AND (count->prime_count NEQ 0)) THEN
      perc_ifs    = ((float) count->count_A/(float) count->prime_count)*100.0;
      perc_whiles = ((float) count->count_B/(float) count->prime_count)*100.0;

      /*==============*/
      /*  Check If's  */
      /*==============*/

      IF(((perc_ifs > WARNING_LOOPS) AND 
         (count->prime_count > WARNING_MIN_RULES)) OR
         (perc_ifs > MAX_LOOPS)) THEN
         sprintf(msg_buf,
            "\nWarning: If statements used in %4.1f%% of the rules", perc_ifs);
         send_message(msg_buf,NO);
      END_IF

      /*=================*/
      /*  Check while's  */
      /*=================*/

      IF(((perc_whiles > WARNING_LOOPS) AND 
         (count->prime_count > WARNING_MIN_RULES)) OR
         (perc_whiles > MAX_LOOPS)) THEN
         sprintf(msg_buf, 
                "\nWarning: While statements used in %4.1f%% of the rules",
               perc_whiles);
         send_message(msg_buf,NO);
      END_IF
   END_IF
}

/* ======================================================================= */
/*                    CHECK FOR UNASSERTED RELATIONS                       */
/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_match_assertions()
 *
 *  PURPOSE:  This function scans the list of relations to
 *            make sure all of them are asserted.
 *
 *  INPUTS :  A pointer to the relations list (RELATION *)
 *
 *  RETURNS:  The number of relations used.
 *
 *  NOTES  :
 * -------------------------------------------------------------
 */
 
static int check_match_assertions(node)
RELATION *node;
{ 
   IF(node NEQ NULL) THEN
      int left = check_match_assertions(node->lft_rel);

      IF(CHECK_STYLE IS_ON) THEN
         IF((node->assert_list EQ NULL) AND 
            (node->fb_list EQ NULL) AND
            (check_def_ex_assertions(node->name, def_ext_head) EQ NULL) AND
            (strcmp(node->name, "initial-fact") NEQ 0)) THEN
            sprintf(msg_buf, "\nWarning: Relation %.40s is never asserted!",
               node->name);
            send_message(msg_buf,NO);
         END_IF
      END_IF

      IF (find_def_tmp(def_tmp_head,node->name)) THEN
        return (left + check_match_assertions(node->rht_rel));
      ELSE
        return (left + 1 + check_match_assertions(node->rht_rel));
      END_IF
   ELSE
      return (0);
   END_IF
}


/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  check_def_ex_assertions()
 *
 *  PURPOSE:  This function checks to see if the given 
 *            relation is listed as being asserted by a
 *            an external function.
 *
 *  INPUTS :  A pointer to the name of the current relation  
 *            (char *), and a pointer to the external function
 *            definition.
 *
 *  RETURNS:  A pointer to the external function definition, 
 *            or NULL if relation is not asserted.
 *
 *  NOTES  :  Uses the global def_external list pointer.
 *            Currently Stubbed out!
 * -------------------------------------------------------------
 */
 
static DEF_EXT *check_def_ex_assertions(rel_name, node)
char *rel_name;
DEF_EXT *node;
{
   DEF_EXT *rtn;
   int      found;

   IF(node NEQ NULL) THEN
      rtn = check_def_ex_assertions(rel_name, node->lft_def_ext);
      IF(rtn EQ NULL) THEN
         found = is_word_in_list(node->assert_list, rel_name);
         IF(found EQ YES) THEN
            return(node);
         ELSE
            rtn = check_def_ex_assertions(rel_name, node->rht_def_ext);
            return(rtn);
         END_IF
      ELSE
         return(rtn);
      END_IF
   END_IF

   return(NULL);
}

