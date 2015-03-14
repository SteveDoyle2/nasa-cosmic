#include <stdio.h>
#include <math.h>
#include "crsv.h"

/*==================================================
 *  This file contains all the functions used to 
 *  create a file of defrelation info.
 *==================================================
 */
 
/* ===========  Functions defined Externally but used here  =========== */

/* -------------------
 *  From the system
 * -------------------
 */

FILE *fopen();

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern COUNTER *alloc_counter();
extern char    *gen_alloc();
extern void     gen_free();
extern DF_PTR   alloc_def_fld();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern FIELD  *sort_field_list();
extern WL_PTR add_to_word_list();
extern NL_PTR add_to_number_list();
extern int    is_word_in_list();
extern int    is_number_in_list();


/* -----------------------------
 *  From the file: CRSVTMP.C
 * -----------------------------
 */

extern DS_PTR   find_def_slot_by_position();
extern DT_PTR   find_def_tmp();


/* ------------------------
 *  From the file: CRSV.C
 * ------------------------
 */

extern DT_PTR   def_tmp_head;
extern DR_PTR   def_rel_head;


/* ---------------------------
 *  From the file: CRSVREL.C
 * ---------------------------
 */

extern DR_PTR   find_def_rel();
extern DF_PTR   find_def_fld();


/* ===========  Functions defined here for Global use  ================ */

void     create_def_rel_file();
void     create_def_ext_file();


/* ===========  Functions defined here for internal use  ============== */

void   loop_through_relations();
void   output_def_rel_info();
void   count_rr_fields();
void   count_fbr_fields();
void   examine_field_values();
void   examine_template_field_values();
void   print_temp_defaults();
int    determine_type();
void   output_list_items();
void   output_relation_field_info();
void   determine_range();

void   loop_through_functions();
void   output_def_ext_info();


/* ===========  Variables defined here for Global use  ================ */


/* ===========  Variables defined Externally but used here  =========== */

/* ===========  Variables defined here for internal use  ============== */

#define VARIABLE           1
#define WORD_OR_STRING     2
#define STRING_OR_NUMBER   3
#define WORD_OR_NUMBER     4

FILE *fp;


/* ========================================================================= */
/*                    CREATE DEFRELATION FUNCTIONS                           */
/* ========================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   : create_def_rel_file()
 *
 *  PURPOSE: This function creates a file with defrelations info
 *           for each of the known defrelations. It makes a 'best'
 *           guess at some of the appropriate info.
 *
 *  INPUTS : Two arguments, a pointer to the head of the
 *           relations list (RELATION *), and a pointer to
 *           the name of the file for defrelation info (char *);
 *
 *  RETURNS: Nothing.
 * -------------------------------------------------------------
 */ 

void create_def_rel_file(rel_list, file_name)
char      *file_name;
RELATION  *rel_list;
{

   /*========================*/
   /* Open defrelations file */
   /*========================*/

#if MAC_LSC
   GotoDefrelationsDirectory();
#endif

   fp = fopen(file_name, "w");
   IF(fp EQ NULL) THEN

      /*=================================================*/
      /* If file can't be opened, print error and return */
      /*=================================================*/

      sprintf(msg_buf, "Unable to open file %.20s", file_name);
      error_message(ERROR, msg_buf);
      return;
   END_IF
   
   /*=============================*/
   /* Loop through relations list */
   /*=============================*/

   loop_through_relations(rel_list);

   fprintf(fp,"\n");
   fclose(fp);
   
#if MAC_LSC
   SetDefrelationsFileCreator();
#endif
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  loop_through_relations()
 *
 *  PURPOSE:  This is a recursive function which processes
 *            through all the nodes of the binary tree of
 *            relations.
 *
 *  INPUTS :  A pointer to the list of relations (REL_PTR)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Actual output of relations info is in the
 *            output_def_rel_info function.
 * -------------------------------------------------------------
 */

static void loop_through_relations(node)
REL_PTR node;
{
   IF(node NEQ NULL) THEN
     loop_through_relations(node->lft_rel);
     output_def_rel_info(node);
     loop_through_relations(node->rht_rel);
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  output_def_rel_info()
 *
 *  PURPOSE:  This function writes all the defrelation info
 *            to a file.
 *
 *  INPUTS :  A pointer to the current relation (REL_PTR)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

static void output_def_rel_info(node)
REL_PTR node;
{
   FIELD     *temp_fld;
   int        min = 1000;
   int        max = 0;
   DT_PTR     template;
   DR_PTR     defrelation;
   DF_PTR     def_field;
   register   i;

   IF(VERBOSE IS_ON) THEN
      sprintf(msg_buf, "\nCreating defrelation for %.40s", node->name);
      send_message(msg_buf,NO);
   END_IF

   template = find_def_tmp(def_tmp_head,node->name);
   defrelation = find_def_rel(def_rel_head,node->name);

   /* -------------------------------------------
    *  If LITERAL is on, then add the values of
    *  node (relation) to the defrelation 
    *  structure.
    * -------------------------------------------
    */

   IF(template) THEN
     fprintf(fp,"\n\n(deftemplate %s",node->name);
   ELSE

     /*================================================================*/
     /* Search various lists to determine min and max number of fields */
     /*================================================================*/

     count_rr_fields(node->rule_list,    &min, &max);
     count_rr_fields(node->assert_list,  &min, &max);
     count_fbr_fields(node->fb_list,     &min, &max);

     IF (min NEQ max) THEN
        fprintf(fp,"\n\n;; ---------------------------------------------------");
        fprintf(fp,"\n;; NOTICE FOR RELATION: %s",node->name);
        fprintf(fp,"\n;; Due to the use of wildcards the information in the");
        fprintf(fp,"\n;; defrelation structure is not guaranteed accurate.");
        fprintf(fp,"\n;; ---------------------------------------------------");
     END_IF

     fprintf(fp,"\n\n(defrelation %s", node->name);
     fprintf(fp,"\n   (min-number-of-fields %d)", min);
     fprintf(fp,"\n   (max-number-of-fields %d)", max);
   END_IF

   /*============================================*/
   /* Process field list for specific field info */
   /*============================================*/
   
   temp_fld = node->field_list;
   IF(temp_fld NEQ NULL) THEN

      /*=========================*/
      /* Process known arguments */
      /*=========================*/
   
      node->field_list = sort_field_list(node->field_list);

      temp_fld = node->field_list;
      i = 2;
      WHILE (temp_fld NEQ NULL) DO

         WHILE (temp_fld->field_num > i) DO
            fprintf(fp,"\n   (field %d", i++);
            fprintf(fp,"\n      (type ?VARIABLE)");
            fprintf(fp,"\n      (allowed-words ?VARIABLE)");
            fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
            fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
            fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
            fprintf(fp,")");
         END_WHILE


         /* ------------------------------------------------
          *  If the relation was defined by a deftemplate,
          *  then parse the template values.  If LITERAL
          *  is ON then any values found in the parsing of
          *  the rules will be printed in the appropriate
          *  allowed list, else only the information found
          *  in the deftemplate structures will be printed.
          * ------------------------------------------------
          */

         IF(template) THEN
             examine_template_field_values(temp_fld,template,defrelation);
             break;
         ELSE
           fprintf(fp,"\n   (field %d", temp_fld->field_num);      
         END_IF

         /* ---------------------------------------------------------
          *  In order to keep the allowed values for a field that
          *  were defined in a defrelation as well as those that
          *  are defined in a rule, the def_relation and relation
          *  lists must be parsed. (LITERAL option turned on)
          * ---------------------------------------------------------
          */

         IF ((defrelation) AND (!template)) THEN
            def_field = defrelation->fields;
            IF (def_field) THEN 
              WHILE (def_field) DO
                 IF (def_field->position EQ temp_fld->field_num) THEN
                   output_relation_field_info(def_field);
                 END_IF
                 def_field = def_field->next_field;
              END_WHILE
            ELSE
              examine_field_values(temp_fld->lit_values);
            END_IF
         ELSE_IF((!defrelation)AND(!template))
	    examine_field_values(temp_fld->lit_values);
         END_IF


         fprintf(fp,")");

         i        = temp_fld->field_num + 1;
         temp_fld = temp_fld->next_field;
      END_WHILE
      
      WHILE (i <= max) DO
         fprintf(fp,"\n   (field %d",i);
         IF ((defrelation) AND (!template)) THEN
           fprintf(fp,"\n      (type");
           def_field = defrelation->fields;
           IF (def_field) THEN
             WHILE (def_field) DO
               IF (def_field->position EQ i) THEN
                 IF (def_field->allow_word)
                    fprintf(fp," WORD");
                 IF (def_field->allow_string)
                    fprintf(fp," STRING");
                 IF (def_field->allow_number)
                    fprintf(fp," NUMBER");
                 fprintf(fp,")");
                 IF (def_field->allow_word)
                    fprintf(fp,"\n      (allowed-words ?VARIABLE)");
                 IF (def_field->allow_string)
                    fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
                 IF (def_field->allow_number) THEN
                    IF (def_field->set_max OR def_field->set_min) THEN
                       fprintf(fp,"\n      (range");
                       IF (def_field->set_min) THEN
                         fprintf(fp," %3.2f",def_field->min_range);
                       ELSE
                         fprintf(fp," ?VARIABLE");
                       END_IF
                       IF (def_field->set_max) THEN
                         fprintf(fp," %3.2f)",def_field->max_range);
                       ELSE
                         fprintf(fp," ?VARIABLE)");
                       END_IF
                    ELSE
                      fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
                      fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
                    END_IF
                 END_IF
               END_IF
               def_field = def_field->next_field;
             END_WHILE
             fprintf(fp,")");
           ELSE
             fprintf(fp," ?VARIABLE)");
             fprintf(fp,"\n      (allowed-words ?VARIABLE)");
             fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
             fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
             fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
             fprintf(fp,")");
           END_IF
         ELSE
           fprintf(fp,"\n      (type ?VARIABLE)");
           fprintf(fp,"\n      (allowed-words ?VARIABLE)");
           fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
           fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
           fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
           fprintf(fp,")");
         END_IF
         i++;
      DONE

   ELSE_IF(min NEQ 1000 OR max NEQ 0)

      /*===========================*/
      /* Process unknown arguments */
      /*===========================*/
   
      for(i = 2; i <= max; i++) DO
         fprintf(fp,"\n   (field %d",i);
         IF ((defrelation) AND (!template)) THEN
           fprintf(fp,"\n      (type");
           def_field = defrelation->fields;
           IF (def_field) THEN
             WHILE (def_field) DO
               IF (def_field->position EQ i) THEN
                 IF (def_field->allow_word)
                    fprintf(fp," WORD");
                 IF (def_field->allow_string)
                    fprintf(fp," STRING");
                 IF (def_field->allow_number)
                    fprintf(fp," NUMBER");
                 fprintf(fp,")");
                 IF (def_field->allow_word)
                    fprintf(fp,"\n      (allowed-words ?VARIABLE)");
                 IF (def_field->allow_string)
                    fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
                 IF (def_field->allow_number) THEN
                    IF (def_field->set_max OR def_field->set_min) THEN
                       fprintf(fp,"\n      (range");
                       IF (def_field->set_min) THEN
                         fprintf(fp," %3.2f",def_field->min_range);
                       ELSE
                         fprintf(fp," ?VARIABLE");
                       END_IF
                       IF (def_field->set_max) THEN
                         fprintf(fp," %3.2f)",def_field->max_range);
                       ELSE
                         fprintf(fp," ?VARIABLE)");
                       END_IF
                    ELSE
                      fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
                      fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
                    END_IF
                 END_IF
               END_IF
               def_field = def_field->next_field;
             END_WHILE
             fprintf(fp,")");
           ELSE
             fprintf(fp," ?VARIABLE)");
             fprintf(fp,"\n      (allowed-words ?VARIABLE)");
             fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
             fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
             fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
             fprintf(fp,")");
           END_IF
         ELSE
           fprintf(fp,"\n      (type ?VARIABLE)");
           fprintf(fp,"\n      (allowed-words ?VARIABLE)");
           fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
           fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
           fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
           fprintf(fp,")");
         END_IF
      END_FOR
   END_IF

   fprintf(fp,"\n   )");
}


/* ==================================================================== */

/*
 * ------------------------------------------------------------------
 *  NAME   :  output_relation_field_info()
 *
 *  PURPOSE:  This function is used to output defrelation info
 *            to a file specified with the -c option.  The
 *            function presently outputs allowed-number field
 *            values.  It takes the values from the def_relation
 *            list.  This function only works when there exists
 *            a defrelation statement for the current relation.
 *
 *  INPUTS :  A pointer to the field of the defrelation (DF_PTR)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  This function only takes values from the def_relation
 *            list.  It will be expanded to take values from the
 *            relation list when no defrelations have been supplied.
 * ------------------------------------------------------------------
 */

static void output_relation_field_info(fields)
DF_PTR fields;
{
   NL_PTR num_list;
   WL_PTR wlist;
   int num_words = 0;
   int num_strings = 0;
   int num_numbers = 0;

   /* ------------------------------
    *  Print out the field type(s)
    * ------------------------------
    */

   fprintf(fp,"\n      (type");

   IF (fields->allow_number) THEN
     fprintf(fp," NUMBER");
     num_list = fields->possible_numbers;
     WHILE (num_list) DO
       num_numbers++;
       num_list = num_list->next_number;
     END_WHILE
   END_IF
   IF (fields->allow_word) THEN
     fprintf(fp," WORD");
     wlist = fields->possible_words;
     WHILE (wlist) DO
       num_words++;
       wlist = wlist->next_word;
     END_WHILE
   END_IF
   IF (fields->allow_string) THEN
     fprintf(fp," STRING");
     wlist = fields->possible_strings;
     WHILE (wlist) DO
       num_strings++;
       wlist = wlist->next_word;
     END_WHILE
   END_IF

   fprintf(fp,")");

   /* -------------------------------
    *  Print out the allowed values
    * -------------------------------
    */

   IF (fields->allow_string) THEN
      fprintf(fp,"\n      (allowed-strings");
      wlist = fields->possible_strings;
      IF (num_strings <= MAX_SINGLE_LIST) THEN
        IF (wlist) THEN
          WHILE(wlist) DO
            fprintf(fp," %s",wlist->word);
            wlist = wlist->next_word;
          END_WHILE
        ELSE
          fprintf(fp," ?VARIABLE");
        END_IF
      ELSE
        fprintf(fp," ?VARIABLE");
      END_IF
      fprintf(fp,")");
   END_IF

   IF (fields->allow_word) THEN
      fprintf(fp,"\n      (allowed-words");
      wlist = fields->possible_words;
      IF (num_words <= MAX_SINGLE_LIST) THEN
        IF (wlist) THEN
          WHILE(wlist) DO
            fprintf(fp," %.40s",wlist->word);
            wlist = wlist->next_word;
          END_WHILE
        ELSE
          fprintf(fp," ?VARIABLE");
        END_IF
      ELSE
        fprintf(fp," ?VARIABLE");
      END_IF
      fprintf(fp,")");
   END_IF

   IF (fields->allow_number) THEN
      IF (fields->set_min OR fields->set_max) THEN
         fprintf(fp,"\n      (range");
         IF (fields->set_min) THEN
           fprintf(fp," %3.2f",fields->min_range);
         ELSE
           fprintf(fp," ?VARIABLE");
         END_IF
         IF (fields->set_max) THEN
           fprintf(fp," %3.2f)",fields->max_range);
         ELSE
           fprintf(fp," ?VARIABLE)");
         END_IF
      ELSE
        fprintf(fp,"\n      (allowed-numbers");
        num_list = fields->possible_numbers;
        IF (num_list) THEN
          IF (num_numbers <= MAX_SINGLE_LIST) THEN
            WHILE(num_list) DO
              fprintf(fp," %3.2f",num_list->number);
              num_list = num_list->next_number;
            END_WHILE
            fprintf(fp,")");
          ELSE
            fprintf(fp," ?VARIABLE)");
          END_IF
        ELSE
          fprintf(fp," ?VARIABLE)");
          fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
        END_IF
      END_IF
   END_IF
}


/* =========================================================================
 * =========================================================================
 *                   Counting fields functions
 *
 *   These functions count the number of fields used for this relation in
 *   various rules, deffacts, and asserts.
 *   They currently arn't too smart, merely returning the smallest and
 *   biggest numbers found. They should be more intelligent about how
 *   many times that number of fields is used.
 * =========================================================================
 * =========================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  count_rr_fields()
 *
 *  PURPOSE:  This function searches a rule_relation list for
 *            the minimum and maximum number of fields used.
 *
 *  INPUTS :  A pointer to the rule_relation list (RR_PTR), and
 *            integer pointers to the min and max currently
 *            found (int *).
 *
 *  RETURNS:  Nothing, but it can modify the min/max pointers.
 *
 *  NOTES  :  Called many times for the various rr lists.
 * -------------------------------------------------------------
 */

static void count_rr_fields(rr_list, min, max)
RR_PTR   rr_list;
int     *min, *max;
{
   RR_PTR  temp;
   
   temp = rr_list;
   while(temp NEQ NULL) DO
      IF(CHECK_DEBUG IS_ON) THEN
         sprintf(msg_buf, "\nInside rr, min: %5d, max: %5d, obj: %s (%5d)", 
                   *min, *max, temp->rule->name, temp->num_fields);
         send_message(msg_buf,NO);
      END_IF
      IF(abs(temp->num_fields) < *min) THEN
         *min = abs(temp->num_fields);
      END_IF

      IF(abs(temp->num_fields) > *max) THEN
         *max = abs(temp->num_fields);
      END_IF

      temp = temp->next_rr;
   END_WHILE
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  count_fbr_fileds()
 *
 *  PURPOSE:  This function searches a fact block structure for
 *            the minimum and maximum number of fields used.
 *
 *  INPUTS :  A pointer to the fact block list (FBR_PTR), and
 *            integer pointers to the min and max currently
 *            found (int *).
 *
 *  RETURNS:  Nothing, but it can modify the min/max pointers.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

static void count_fbr_fields(fbr_list, min, max)
FBR_PTR  fbr_list;
int     *min, *max;
{
   FBR_PTR  temp;
   
   temp = fbr_list;
   while(temp NEQ NULL) DO
      IF(CHECK_DEBUG IS_ON) THEN
         sprintf(msg_buf, "\nInside fbr, min: %5d, max: %5d, obj: %5d", 
                *min, *max, temp->num_fields);
         send_message(msg_buf,NO);
      END_IF

      IF(abs(temp->num_fields) < *min) THEN
         *min = temp->num_fields;
      END_IF

      IF(abs(temp->num_fields) > *max) THEN
         *max = temp->num_fields;
      END_IF

      temp = temp->next_fbr;
   END_WHILE
}

/* ========================================================================= */

/*
 * -------------------------------------------------------------------
 *  NAME   :  examine_field_values()
 *
 *  PURPOSE:  This function examines a word list to determine
 *            type, range and allowed values.
 *
 *  INPUTS :  A pointer to list of literal values used in
 *            this field.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  This function is also somewhat stupid about what
 *            it does. Eventually need to implement some of the 
 *            decisions in a rule base to improve the quality of 
 *            the 'guesses' in areas such as type or allowed values.
 * -------------------------------------------------------------------
 */

void examine_field_values(list)
WORD_LIST *list;
{
   int        type;
   COUNTER   *counts;
   int        length, num_words, num_strings, num_numbers;

   counts      = alloc_counter();
   
   /*================================================*/
   /* Decide what type this field is and gather info */
   /* on number of times each type is used.          */
   /*================================================*/

   type        = determine_type(list, counts);

   length      = counts->prime_count;
   num_words   = counts->count_A;
   num_strings = counts->count_B;
   num_numbers = counts->count_C;
   
   IF(CHECK_DEBUG IS_ON) THEN
      sprintf(msg_buf, "\nType of list is %d and length is %d", type, length);
      send_message(msg_buf,NO);
   END_IF

   /*==================*/
   /* Output WORD info */
   /*==================*/

   IF(type EQ WORD) THEN
      fprintf(fp,"\n      (type WORD)");
      IF(num_words <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-words");
         output_list_items(list, WORD);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-words ?VARIABLE)");
      END_IF

   /*====================*/
   /* Output STRING info */
   /*====================*/

   ELSE_IF(type EQ STRING)
      fprintf(fp,"\n      (type STRING)");
      IF(num_strings <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-strings");
         output_list_items(list, STRING);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
      END_IF

   /*====================*/
   /* Output NUMBER info */
   /*====================*/

   ELSE_IF(type EQ NUMBER)
      fprintf(fp,"\n      (type NUMBER)");
      IF(num_numbers <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-numbers");
         output_list_items(list, NUMBER);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
         determine_range(list);
      END_IF

   /*=============================*/
   /* Output WORD and STRING info */
   /*=============================*/

   ELSE_IF(type EQ WORD_OR_STRING)
      fprintf(fp,"\n      (type WORD STRING)");

      IF(num_strings <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-strings ");
         output_list_items(list, STRING);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
      END_IF

      IF(num_words <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-words ");
         output_list_items(list, WORD);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-words ?VARIABLE)");
      END_IF

   /*===============================*/
   /* Output NUMBER and STRING info */
   /*===============================*/

   ELSE_IF(type EQ STRING_OR_NUMBER)
      fprintf(fp,"\n      (type NUMBER STRING)");

      IF(num_strings <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-strings ");
         output_list_items(list, STRING);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
      END_IF

      IF(num_numbers <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-numbers ");
         output_list_items(list, NUMBER);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
         determine_range(list);
      END_IF

   /*=============================*/
   /* Output WORD and NUMBER info */
   /*=============================*/

   ELSE_IF(type EQ WORD_OR_NUMBER)
      fprintf(fp,"\n      (type WORD NUMBER)");

      IF(num_words <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-words ");
         output_list_items(list, WORD);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-words ?VARIABLE)");
      END_IF

      IF(num_numbers <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-numbers ");
         output_list_items(list, NUMBER);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
         determine_range(list);
      END_IF

   /*=========================================*/
   /* Couldn't determin type output ?VARIABLE */
   /*=========================================*/

   ELSE_IF(type EQ VARIABLE)
      fprintf(fp,"\n      (type ?VARIABLE)");

      IF(num_words <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-words ");
         output_list_items(list, WORD);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-words ?VARIABLE)");
      END_IF

      IF(num_strings <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-strings ");
         output_list_items(list, STRING);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
      END_IF

      IF(num_numbers <= MAX_SINGLE_LIST) THEN
         fprintf(fp,"\n      (allowed-numbers ");
         output_list_items(list, NUMBER);
         fprintf(fp,")");
      ELSE
         fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
         determine_range(list);
      END_IF
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  examine_template_field_values()
 *
 *  PURPOSE:  This function will determine the type(s) of a 
 *            multi-field variable as well as all the allowed
 *            values for each type.
 *
 *  INPUTS :  A pointer to the field_list of a relation.
 *            A pointer to the template that created the relation.
 *            A pointer to the defrelation that holds all
 *              pertinent information.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  What is printed depends on if LITERAL is On or Off.
 * -------------------------------------------------------------
 */

#define BLOCKSIZE 80
#define STRCAT(a,b) strcat (strcat (a, " "), b)

void examine_template_field_values(flist,template,defrel)
FIELD *flist;
DT_PTR template;
DR_PTR defrel;
{
  WL_PTR  wlist;
  NL_PTR  nlist;
  DS_PTR  slot;
  DF_PTR  field;
  int     MULTI = 0;
  int     TYPE = 0;
  int     str_size = BLOCKSIZE;
  int     num_size = BLOCKSIZE;
  int     word_size = BLOCKSIZE;
  char    *fnc_name = "examine_multi_field_values";
  char    *new_word_vals = gen_alloc(word_size,fnc_name);
  char    *new_str_vals  = gen_alloc(str_size,fnc_name);
  char    *new_num_vals  = gen_alloc(num_size,fnc_name);
  char    fl_buf[20];

  /* ========================================================
   *  Allocate memory to create the lists for allowed words,
   *  allowed strings, and allowed numbers.  Allocate in BLOCKSIZE
   *  byte chunks.  When current chunk is exceeded, allocate
   *  new chunk which is BLOCKSIZE + old chunk size and free old
   *  chunk.
   * =========================================================
   */

  *new_word_vals = '\0';
  *new_str_vals = '\0';
  *new_num_vals = '\0';

  WHILE (flist) DO

    /* ==========================================================
     *  Process the template relations found in the rules first.
     *  If the type is a WORD then bit-wise OR a 1 with the local
     *  variable TYPE.  If type is a STRING OR a 2, a NUMBER OR
     *  a 4.  In this manner, the variable TYPE will be able to
     *  hold any/all of the types the multi-field can have.
     * ==========================================================
     */


    slot = find_def_slot_by_position(template,flist->field_num);
    IF(((slot->max_elements NEQ 1) OR (slot->min_elements NEQ 1)) AND
      (MULTI EQ 0)) THEN
      MULTI = 1;
      fprintf(fp,"\n   (multi-field %.40s",slot->name);
    ELSE_IF (MULTI NEQ 1)
      fprintf(fp,"\n   (field %.40s",slot->name);
    END_IF
      
    /* ===================================================
     *  The information to be printed will come from the 
     *  defrelation structure.  If LITERAL is On, then
     *  values were added to the allowed lists of def-
     *  relation during the parsing of the rules, else
     *  defrelation holds only that information found
     *  in the previously defined defrelation or def-
     *  template structures.
     * ====================================================
     */

    field = defrel->fields;
    IF (field) THEN
       WHILE (field) DO
          IF (field->position EQ flist->field_num) THEN
            IF (field->allow_word) THEN
              TYPE |= 1;
            ELSE_IF (field->allow_string)
              TYPE |= 2;
            ELSE_IF (field->allow_number)
              TYPE |= 4;
            END_IF

            IF (field->possible_words) THEN
               wlist = field->possible_words;
               WHILE (wlist) DO
                 IF ((strlen(new_word_vals)+strlen(wlist->word)+1) >=
                          word_size) THEN
                    char *old_word_vals = new_word_vals;
                    new_word_vals = gen_alloc(word_size+BLOCKSIZE,fnc_name);
                    *new_word_vals = '\0';
                    strcpy (new_word_vals, old_word_vals);
                    gen_free(old_word_vals,word_size);
                    word_size += BLOCKSIZE;
                  END_IF
                  STRCAT (new_word_vals, wlist->word);
                  wlist = wlist->next_word;
               END_WHILE
            END_IF
            IF (field->possible_strings) THEN
               wlist = field->possible_strings;
               WHILE (wlist) DO
                  IF ((strlen(new_str_vals)+strlen(wlist->word)+1) >=
                      str_size) THEN
                     char *old_str_vals = new_str_vals;
                     new_str_vals = gen_alloc(str_size+BLOCKSIZE,fnc_name);
                     *new_str_vals = '\0';
                     strcpy (new_str_vals, old_str_vals);
                     gen_free(old_str_vals,str_size);
                     str_size += BLOCKSIZE;
                   END_IF
                   STRCAT (new_str_vals, wlist->word);
                   wlist = wlist->next_word;
                END_WHILE
             END_IF
             IF (field->possible_numbers) THEN
                nlist = field->possible_numbers;
                WHILE (nlist) DO
                   sprintf(fl_buf,"%f",nlist->number);
                   IF ((strlen(new_num_vals)+strlen(fl_buf)+1) >=
                        num_size) THEN
                     char *old_num_vals = new_num_vals;
                     new_num_vals = gen_alloc(num_size+BLOCKSIZE,fnc_name);
                     *new_num_vals = '\0';
                     strcpy (new_num_vals, old_num_vals);
                     gen_free(old_num_vals,num_size);
                     num_size += BLOCKSIZE;
                   END_IF
                   STRCAT (new_num_vals, fl_buf);
                   nlist = nlist->next_number;
                END_WHILE
             END_IF
          END_IF
          field = field->next_field;
       END_WHILE
    END_IF


    IF((MULTI EQ 0) OR (flist->next_field EQ NULL)) THEN

      fprintf(fp,"\n      (type");

      /* ================================================
       *  To remove the type, bit-wise AND the variable
       *  TYPE with the three defined values and print
       *  out the appropriate type.
       * =================================================
       */

      IF ((TYPE & 1) EQ 1)
        fprintf(fp," WORD");
      IF ((TYPE & 2) EQ 2)
        fprintf(fp," STRING");
      IF ((TYPE & 4) EQ 4)
        fprintf(fp," NUMBER");
      fprintf(fp,")");

      /* ===================================================
       *  If the variables which hold the allowed types 
       *  have a strlen greater than zero (they contain at
       *  least one value) then printout the list.
       *  LITERAL option must be turned on.
       * ====================================================
       */


      IF (strlen(new_word_vals)) 
         fprintf(fp,"\n      (allowed-words%s)",new_word_vals);
      IF (strlen(new_num_vals))
         fprintf(fp,"\n      (allowed-numbers%s)",new_num_vals);
      IF (strlen(new_str_vals)) 
         fprintf(fp,"\n      (allowed-strings%s)",new_str_vals);

      *new_word_vals = '\0';
      *new_num_vals = '\0';
      *new_word_vals = '\0';

      print_temp_defaults(slot->defaults);
    END_IF

    flist = flist->next_field;
  END_WHILE

      /* ===================================================
       *  Free the memory used to hold the allowed types
       * ===================================================
       */

      gen_free(new_word_vals,word_size);
      gen_free(new_num_vals,num_size);
      gen_free(new_str_vals,str_size);

}

/* ============================================================ */

/*
 * --------------------------------------------------------------
 *
 *    NAME   : print_temp_defaults()
 *
 *    PURPOSE : This function will print out the default values
 *              for a template when the -c option is turned on.
 * 
 *     INPUTS : Pointer to the defaults list of a template.
 *
 *    RETURNS : Nothing.
 *
 *      NOTES : None.
 *
 * --------------------------------------------------------------
 */

void print_temp_defaults(def_list)
TOKEN_NODE *def_list;
{
  IF (def_list) THEN
    fprintf(fp,"\n      (default");

    while(def_list) {
      fprintf(fp," %s",def_list->token->tknword);
      def_list = def_list->next;
    }
    fprintf(fp,")");
  ELSE
    fprintf(fp,"\n      (default ?NONE)");
  END_IF
    fprintf(fp,")");
}



/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  determine_type()
 *
 *  PURPOSE:  This function searches a list of words and tries
 *            to determine what type (NUMBER, WORD, STRING) the 
 *            list best corresponds to.
 *
 *  INPUTS :  A pointer to the list of words (WORD_LIST *) and
 *            a pointer to a counter structure for storing
 *            count information (COUNTER *).
 *
 *  RETURNS:  An integer, the type this list most likely
 *            corresponds too. Valid returns are WORD, STRING,
 *            NUMBER, WORD_OR_NUMBER, WORD_OR_STRING, VARIABLE,
 *            and STRING_OR_NUMBER.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

int determine_type(list, count)
WORD_LIST *list;
COUNTER   *count;
{
   WORD_LIST  *temp;
   
   count->prime_count = 0;           /* Number of items in list */
   count->count_A     = 0;           /* Number of WORDs in list */
   count->count_B     = 0;           /* Number of STRINGs in list */
   count->count_C     = 0;           /* Number of NUMBERs in list */

   /*============================================*/
   /* Loop through list counting type occurences */
   /*============================================*/

   temp = list;
   WHILE (temp) DO
      count->prime_count++;

      switch (temp->type) {

         case WORD:
              count->count_A++;
              break;

         case STRING:
              count->count_B++;
              break;

         case NUMBER:
              count->count_C++;
              break;

         default:
              error_message(ERROR, "Unknown type in word list");
              IF(CHECK_DEBUG IS_ON) THEN
                 sprintf(msg_buf, "\nType is: %d, word is: %.40s", temp->type, 
                                                            temp->word);
                 send_message(msg_buf,NO);
              END_IF
              break;
         }

      temp = temp->next_word;
   END_WHILE
   
   /*==========================================*/
   /* Simple case, only one type used in field */
   /*==========================================*/

   IF((count->count_A EQ 0) AND (count->count_B EQ 0)) THEN
      return(NUMBER);
   ELSE_IF((count->count_A EQ 0) AND (count->count_C EQ 0)) 
      return(STRING);
   ELSE_IF((count->count_B EQ 0) AND (count->count_C EQ 0))
      return(WORD);
   END_IF
   
   /*==========================================*/
   /* Not so simple cases, multiple types used */
   /*==========================================*/

   IF(count->count_B EQ 0) THEN
      return(WORD_OR_NUMBER);
   ELSE_IF(count->count_A EQ 0)
      return(STRING_OR_NUMBER);
   ELSE_IF(count->count_C EQ 0)
      return(WORD_OR_STRING);
   ELSE
      return(VARIABLE);
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  output_list_items()
 *
 *  PURPOSE:  This function prints a list of the allowed items
 *            by type. Each word of the allowed type is printed
 *            to a file.
 *
 *  INPUTS :  A pointer to the list of words (WORD_LIST *) and
 *            a flag identifying what type to print.
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

void output_list_items(list, type)
WORD_LIST *list;
int        type;
{
   WORD_LIST  *temp;

   /*============================================*/
   /* Loop through list printing type occurences */
   /*============================================*/

   temp = list;
   while(temp NEQ NULL) DO
      IF(temp->type EQ type) THEN
         fprintf(fp," %s", temp->word);
      END_IF;

      temp = temp->next_word;
   END_WHILE
   
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  determine_range()
 *
 *  PURPOSE:  This function searches a list of words and tries
 *            to determine what range the numbers represent and
 *            then outputs range info the a file.
 *
 *  INPUTS :  A pointer to the list of words (WORD_LIST *).
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Very simplistic for now. Should eventually do 
 *            something with decimal places or number of 
 *            different values to determine whether or not a 
 *            range field is appropriate.
 * -------------------------------------------------------------
 */

/*ARGSUSED*/ /* Make lint happy */
void determine_range(list)
WORD_LIST *list;
{
   fprintf(fp, "\n      (range ?VARIABLE ?VARIABLE)");
}


/* ========================================================================= */
/*                    CREATE DEFEXTERNAL FUNCTIONS                           */
/* ========================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   : create_def_ext_file()
 *
 *  PURPOSE: This function creates a file with defexternal info
 *           for each of the known external functions. It makes 
 *           a 'best' guess at some of the appropriate info.
 *
 *  INPUTS : Two arguments, a pointer to the head of the
 *           external functions list (EX_FUNC *), and a pointer
 *           to the name of the file for defexternal info
 *           (char *).
 *
 *  RETURNS: Nothing.
 *
 *  NOTES  : This function assumes that the defrelation info
 *           has already been put into the same file and
 *           appends the defexternal info.
 * -------------------------------------------------------------
 */ 

void create_def_ext_file(func_list, file_name)
char      *file_name;
EX_FUNC   *func_list;
{
   /*========================*/
   /* Open defexternals file */
   /*========================*/

#if MAC_LSC
   GotoDefrelationsDirectory();
#endif

   fp = fopen(file_name, "a");
   IF(fp EQ NULL) THEN

      /*=================================================*/
      /* If file can't be opened, print error and return */
      /*=================================================*/

      sprintf(msg_buf, "Unable to open file %.20s", file_name);
      error_message(ERROR, msg_buf);
      return;
   END_IF
   
   /*=============================*/
   /* Loop through functions list */
   /*=============================*/

   loop_through_functions(func_list);

   fprintf(fp,"\n");
   fclose(fp);
   
#if MAC_LSC
   SetDefrelationsFileCreator();
#endif
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  loop_through_functions()
 *
 *  PURPOSE:  This is a recursive function which processes
 *            through all the nodes of the binary tree of
 *            external functions.
 *
 *  INPUTS :  A pointer to the list of functions (EX_FUNC *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  Actual output of relations info is in the
 *            output_def_ext_info function.
 * -------------------------------------------------------------
 */

static void loop_through_functions(node)
EX_FUNC *node;
{
   IF(node NEQ NULL) THEN
     loop_through_functions(node->lft_ex_func);
     output_def_ext_info(node);
     loop_through_functions(node->rht_ex_func);
   END_IF
}

/* ==================================================================== */

/*
 * -------------------------------------------------------------
 *  NAME   :  output_def_ext_info()
 *
 *  PURPOSE:  This function writes all the defexternal info
 *            to a file.
 *
 *  INPUTS :  A pointer to the current function (EX_FUNC *)
 *
 *  RETURNS:  Nothing.
 *
 *  NOTES  :  
 * -------------------------------------------------------------
 */

static void output_def_ext_info(node)
EX_FUNC *node;
{
   FIELD     *temp_fld;
   int        min = 1000;
   int        max = 0;
   register   i;
   
   IF(VERBOSE IS_ON) THEN
      sprintf(msg_buf, "\nCreating defexternal for %.40s", node->name);
      send_message(msg_buf,NO);
   END_IF
   
   fprintf(fp,"\n\n(defexternal %s", node->name);

   /*================================================================*/
   /* Search various lists to determine min and max number of fields */
   /*================================================================*/
   
   count_rr_fields(node->LHS_rule_list, &min, &max);
   count_rr_fields(node->RHS_rule_list, &min, &max);

   fprintf(fp,"\n   (min-number-of-args %d)", min);
   fprintf(fp,"\n   (max-number-of-args %d)", max);

   /*===================================================*/
   /* Output default Info which can't be easily derived */
   /*===================================================*/
   
   fprintf(fp,"\n   (asserts ?NONE)");
   fprintf(fp,"\n   (retracts ?NONE)");
   fprintf(fp,"\n   (return-type NUMBER)");

   /*=======================*/
   /* Process argument list */
   /*=======================*/
   
   temp_fld = node->argument_list;
   IF(temp_fld NEQ NULL) THEN

      /*=========================*/
      /* Process known arguments */
      /*=========================*/
   
      node->argument_list= sort_field_list(node->argument_list);

      temp_fld = node->argument_list;
      i = 1;
      while(temp_fld NEQ NULL) DO
         while (temp_fld->field_num > i) DO
            fprintf(fp,"\n   (argument %d", i++);
            fprintf(fp,"\n      (type ?VARIABLE)");
            fprintf(fp,"\n      (allowed-words ?VARIABLE)");
            fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
            fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
            fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
            fprintf(fp,")");
         DONE

         fprintf(fp,"\n   (argument %d", temp_fld->field_num);
         examine_field_values(temp_fld->lit_values);
         fprintf(fp,")");

         i        = temp_fld->field_num + 1;
         temp_fld = temp_fld->next_field;
      END_WHILE

      while(i <= max) DO
            fprintf(fp,"\n   (argument %d", i++);
            fprintf(fp,"\n      (type ?VARIABLE)");
            fprintf(fp,"\n      (allowed-words ?VARIABLE)");
            fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
            fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
            fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
            fprintf(fp,")");
      DONE

   ELSE_IF(min NEQ 1000 OR max NEQ 0)

      /*===========================*/
      /* Process unknown arguments */
      /*===========================*/
   
      for(i = 1; i <= max; i++) DO
         fprintf(fp,"\n   (argument  %d", i);
         fprintf(fp,"\n      (type ?VARIABLE)");
         fprintf(fp,"\n      (allowed-words ?VARIABLE)");
         fprintf(fp,"\n      (allowed-strings ?VARIABLE)");
         fprintf(fp,"\n      (allowed-numbers ?VARIABLE)");
         fprintf(fp,"\n      (range ?VARIABLE ?VARIABLE)");
         fprintf(fp,")");
      END_FOR
   END_IF
         
   fprintf(fp,"\n   )");
}

