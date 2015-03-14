/*******************************************************************
 *      programer:                                                 *
 *                Huyen-anh (Bebe) Ly                              *
 *                Artificial Intelligence Section (MPAD)           *
 *                NASA/JSC   FM7, Houston, TX 77058                *
 *      Purpose :                                                  *
 *                This program is designed for dynamically         *
 *                checking an expert system, which is written      *
 *                in CLIPS.                                        *
 *      Description:                                               *
 *                One way to verify the dynamic information of     *
 *                an expert system is to look at the trace file    *
 *                of the expert system. This file records the      *
 *                behavior of an expert system during run time.    *
 *                This program is designed to scan the trace or    *
 *                dribble files,and use the defrelation and        *
 *                deftemplate forms to verify the facts.  The      *
 *                trace file and the definition forms are provided *
 *                by the user. This program also provides users    *
 *                information about the behavior of the rules, and *
 *                the related relations.                           *
 *******************************************************************/


#include <stdio.h>
#include "crsv.h"
#include <ctype.h>

#define NUMBER_ONLY        1
#define WORD_ONLY          2
#define STRING_ONLY        3
#define NUM_WORD_ONLY      4
#define NUM_STRING_ONLY    5
#define WORD_STRING_ONLY   6
#define ALL                7


#define MAX_DEACTIVATE_RATIO   0.3      /* Max ratio of deactivated rules */


/* ======================================================================== */
/* ==============  Functions defined here for External use  =============== */
/* ======================================================================== */

char   *getline();
int     scan_dribble_file();
int     scan_trace_file();
void    check_for_dyn_unassrt_rels();
void    check_for_dyn_unact_rules();


/* ======================================================================== */
/* ==============  Functions defined Externally but used here  ============ */
/* ======================================================================== */

/* -----------------------------
 *  From the SYSTEM
 * -----------------------------
 */

extern double          atof();
 
/* -----------------------------
 *  From the file: CRSVPRST.C
 * -----------------------------
 */

extern void  process_rel_activation();
extern void  process_rule_activation();
extern void  process_rel_deactivation();
extern void  process_rule_deactivation();
extern void  process_rule_fire();
extern void  process_misc();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern int            send_message();
extern void           error_message ();
 
/* -----------------------------
 *  From the file: CRSVPROC.C
 * -----------------------------
 */

extern DEF_REL        *find_def_rel();
 
/* -----------------------------
 *  From the file: CRSV.C
 * -----------------------------
 */
 
extern FILE *open_file();
extern void close_file();
extern void  end_crsv(); 


/* ======================================================================== */
/* ==============  Functions defined here for Internal use  =============== */
/* ======================================================================== */

int         check_word();
int         check_string();
int         check_number();
void        num_string_check();
void        string_word_check();
void        num_word_check();
int         check_rel_fields();
void        evaluate_rel();
void        print_rel();
void        print_rule_list();
void        print_spec_rel_in_drib();
void        print_dribble_sum();
ACT_REL_PTR find_act_rel();
void        check_dynamic_rules();
void        check_dynamic_relations();
int         process_trace_file();


/* ======================================================================== */
/* ==============  Variables defined Externally but used here  ============ */
/* ======================================================================== */

extern ACT_R_PTR    act_rule_head;  /* First node of the activated rules
                                       found in a trace file             */
extern ACT_REL_PTR  act_rel_head;   /* First node of the activated
                                       relation found in a trace file    */
extern DR_PTR       def_rel_head;   /* First node of the definitions of
                                       the relations                     */
extern RULE        *rule_head;      /* First node of the rule list       */

extern int cur_line_num;
extern char *cur_obj_name;

/* ======================================================================== */
/* ==============  Variables defined here for Internal use  =============== */
/* ======================================================================== */

ACT_REL_PTR last_act_rel;       /* last activated relation               */
int max_occurences;             /* keep track with the maximum occurrences
                                   at one time of an activated relation  */


/* ======================================================================== */

 /*
  * -----------------------------------------------------
  *    Function name : getline
  *    This function will return a pointer to a string;
  *    which contains a line of text from a file
  *    Argument(s) :
  *          fp  : File pointer pointing to the current
  *                line in the trace file.
  *
  *    Return: NULL if End of file
  *            Pointer pointing to the string of text
  * ------------------------------------------------------
  */
  char *getline(fp)
  FILE *fp;

  {
     static char  str[256];
     int i = 0,c;

   IF((c = fgetc(fp)) == EOF)THEN
       return(NULL);
   ELSE_IF(c != '\n')
         str[i++] = c;

      WHILE (((c = fgetc(fp)) != '\n')&&(c != EOF)) DO
            str[i++] = (char)c;
      DONE

      str[i] = 0;
   ELSE
         str[0] = 0;
   END_IF

   return(str);
}

 /*******************************************************
  *  scan_trace_file()
  *     scan the trace file,a and call the appropriate
  *     function to process the trace file.
  *  Argument:
  *     fp : File pointer - Points to a current position
  *          of the trace file.
  *  Return : OK or ERROR
  *
  *******************************************************
  */
  static int scan_trace_file(fp)
  FILE *fp;
  {
    char  *str;

    cur_line_num = 0;
    cur_obj_name = "Trace file";

    IF ((str = getline(fp)) NEQ NULL) THEN
       cur_line_num ++;

       IF (strncmp(str,"CRSV>",5) != 0) THEN
          IF (strncmp(str,"CLIPS>",6) != 0) THEN
             return(ERROR);
          ELSE
             return (scan_dribble_file(fp));
          END_IF
       END_IF
    ELSE
      return(ERROR);
    END_IF

    WHILE((str = getline(fp)) NEQ NULL) DO
          cur_line_num++;
          IF(str[0] == 'A')THEN
             IF(str[1] == 'S')THEN
                process_rel_activation(&str[2]);
             ELSE
                process_rule_activation(&str[2]);
             END_IF
          ELSE_IF(str[0] == 'R')
             process_rel_deactivation(&str[2]);
          ELSE_IF(str[0] == 'D')
             process_rule_deactivation(&str[2]);
          ELSE_IF (str[0] == 'F')
             process_rule_fire(&str[2]);
          END_IF
    DONE

    return(OK);
  }


 /*******************************************************
  *  scan_dribble_file()
  *     scan the dribble file,and call the appropriate
  *     functions to process the dribble file.
  *  Argument:
  *     fp : File pointer - Points to a current position
  *          of the dribble file.
  *  Return : always OK - meaningless
  *  
  *******************************************************
  */
static int scan_dribble_file(fp)
   FILE *fp;
{
   char  *str;

   cur_obj_name = "Dribble file";
   WHILE ((str = getline(fp)) NEQ NULL) DO
      cur_line_num++;
      IF(strncmp(str,"CLIPS>",6) == NULL)THEN
         /* Do nothing. This line is ignored */
      ELSE_IF(strncmp(str,"==> f-",6) == NULL)
         process_rel_activation(&str[6]);
      ELSE_IF(strncmp(str,"==> Activation",14) == NULL)
         process_rule_activation(&str[14]);
      ELSE_IF(strncmp(str,"<== f-",6) == NULL)
         process_rel_deactivation(&str[6]);
      ELSE_IF(strncmp(str,"<== Activation",14) == NULL)
         process_rule_deactivation(&str[14]);
      ELSE_IF(strncmp(str,"FIRE",4) == NULL)
         process_rule_fire(&str[4]);
      ELSE_IF(str[0] != 0)
         process_misc(str);
      END_IF
   DONE

   return(OK);
}
  

/* ----------------------------------------------------
 *    Name     : check_word()
 *    Purpose  : Checks to see if the relation's field
 *               is a WORD and has been specified in the
 *               allowed-words list of the particular defrelation.
 *    Arguments: words - Pointer to the allowed-words list.
 *               fld_ptr - Pointer to a field of a relation.
 *    Returns  : ERROR if the field does not meet the
 *               specification;else returns OK.
 * --------------------------------------------------------
 */

 static int check_word(words,fld_ptr,fact_ptr,name)
 WL_PTR words;
 FIELD_PTR fld_ptr;
 SPE_FACT_PTR fact_ptr;
 char *name;
 {
    WL_PTR current_word;

    if(((fld_ptr->word[0] == '-')||
      (fld_ptr->word[0] == '+'))
      &&(isdigit(fld_ptr->word[1])))
      return(ERROR);
    if(isdigit(fld_ptr->word[0]))
      return(ERROR);
    if(fld_ptr->word[0] == '"')
      return(ERROR);
    if(words!= NULL)
     {
       current_word = words;
       while(current_word != NULL)
         {
           if(strcmp(current_word->word,fld_ptr->word) == 0)
              return(OK);
           else
             current_word = current_word->next_word;
         }
       sprintf(msg_buf,"In fact:  f-%d ", fact_ptr->fact_num);
       error_message(ERROR,msg_buf);
       print_rel(fact_ptr,name);
       sprintf(msg_buf,
      "\n        Used a WORD <%.40s> in field %d that is not in the ALLOWED_WORDS list",
          fld_ptr->word,fld_ptr->num);
       send_message(msg_buf,NO);
       return(OK);
     }
    else
      return(OK);
 }


/* ------------------------------------------------------------
 * Name        : check_string()
 * Purpose     : Checks to see if the a relation's field
 *               is a STRING and specified in the allowed-strings
 *               list of the defrelation.
 * Arguments   : strings_ptr - Pointer to the allowed-strings.
 *               fld_ptr     - Pointer to a field of a relation.
 * Returns     : ERROR if the field does not meet the specification.
 *               OK if everything is ok.
 * -------------------------------------------------------------*/

 static int check_string(strings_ptr,fld_ptr,fact_ptr,name)
 WL_PTR strings_ptr;
 FIELD_PTR fld_ptr;
 SPE_FACT_PTR fact_ptr;
 char *name;
 {
    WL_PTR current_string;

    if(fld_ptr->word[0] != '"')
      return(ERROR);
    if(strings_ptr != NULL)
      {
        current_string = strings_ptr;
        while(current_string != NULL)
          {
             if(strcmp(current_string->word,fld_ptr->word) == 0)
                return(OK);
             else
                current_string = current_string->next_word;
          }
       sprintf(msg_buf,"In fact:  f-%d ", fact_ptr->fact_num);
       error_message(ERROR,msg_buf);
       print_rel(fact_ptr,name);
       sprintf(msg_buf,
      "\n         Used a STRING <%.40s> in field %d that is not in the ALLOWED_STRINGS list",
          fld_ptr->word,fld_ptr->num);
       send_message(msg_buf,NO);
        return(OK);
      }
    return(OK);
 }



/* -------------------------------------------------
 * Name        :  check_number();
 *
 * Purpose     :  Checks to see if a relation's field
 *                is a string and specified
 *                in the allowed-numbers list.
 *
 * Arguments   :  def_fld_ptr - pointer to a specified
 *                field in defrelation.
 *                act_fld_ptr - pointer to a relation's
 *                field.
 *
 * Returns     :  ERROR if the field does not not meet
 *                the specification.
 *                OK if everything is ok.
 *                an error message is also generated
 *                when error is detected.
 * -------------------------------------------------*/

  static int check_number(def_fld_ptr,act_fld_ptr,fact_ptr,name)
  DF_PTR def_fld_ptr;
  FIELD_PTR act_fld_ptr;
  SPE_FACT_PTR fact_ptr;
  char *name;
  {
     float num;
     NL_PTR current_number;

     if((act_fld_ptr->word[0] != '-')
        &&(act_fld_ptr->word[0] != '+')
        &&(!isdigit(act_fld_ptr->word[0])))
          return(ERROR);
     if(((act_fld_ptr->word[0] == '-')
        ||(act_fld_ptr->word[0] == '+'))
        &&(!isdigit(act_fld_ptr->word[1])))
          return(ERROR);

     num = (float)atof(act_fld_ptr->word);

     /* If there is a retriction in the number (according to defrelation ).*/

     if(def_fld_ptr->possible_numbers != NULL)
       {
          current_number = def_fld_ptr->possible_numbers;
          while(current_number != NULL)
            {
               if(current_number->number == num)
                 return(OK);
               else
                 current_number = current_number->next_number;
            }
          sprintf(msg_buf,"In fact:  f-%d ", fact_ptr->fact_num);
          error_message(ERROR,msg_buf);
          print_rel(fact_ptr,name);
          sprintf(msg_buf,
            "\n         Used a NUMBER <%f> in field %d that is not in the ALLOWED-NUMBERS list",
               num,act_fld_ptr->num);
          send_message(msg_buf,NO);
          return(OK);
       }

     if(def_fld_ptr->set_max == YES)            /* If the maximum value has been set */
       {
          if (num > def_fld_ptr->max_range)     /* If the value is greater than the maximum */
            {

              sprintf(msg_buf,"%f is out of range. Max allowed value is %f",
                      num,def_fld_ptr->max_range);
              error_message(ERROR,msg_buf);
              sprintf(msg_buf,"\n         f-%d   ",fact_ptr->fact_num);
              send_message(msg_buf,NO);
              print_rel(fact_ptr,name);
              return(OK);
            }
       }
     if(def_fld_ptr->set_min == YES)           /* If the minimum value has been set */
       {
          if(num < def_fld_ptr->min_range)     /* If the value is less than the minimum value */
           {
            sprintf(msg_buf,"%f is out of range. Min allowed value is %f",
                    num,def_fld_ptr->min_range);
            error_message(ERROR,msg_buf);
            sprintf(msg_buf,"\n         f-%d   ",fact_ptr->fact_num);
            send_message(msg_buf,NO);
            print_rel(fact_ptr,name);
            return(OK);
           }
       }
     return(OK);
  }



 /* ---------------------------------------------
  * Prints error message.
  * ---------------------------------------------
  */
 static void error_mess(fld_ptr,fact_ptr,code,name)
 FIELD_PTR fld_ptr;
 SPE_FACT_PTR fact_ptr;
 int code;
 char *name;
 {
      switch (code)
       {
          case  NUMBER_ONLY:
             sprintf(msg_buf,
                "<%.40s> is not allowed. Expected a NUMBER in field %d of",
                fld_ptr->word, fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

          case  WORD_ONLY :
              sprintf(msg_buf,
                   "<%.40s> is not allowed. Expected a WORD in field %d of\n",
                   fld_ptr->word,fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

          case  STRING_ONLY :
              sprintf(msg_buf,
                   "<%.40s> is not allowed. Expected a STRING in field %d of",
                   fld_ptr->word,fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

          case  NUM_WORD_ONLY :
              sprintf(msg_buf,
         "<%.40s> is not allowed. Expected a NUMBER or a WORD in field %d of",
                      fld_ptr->word,fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

          case  NUM_STRING_ONLY :
             sprintf(msg_buf,
      "<%.40s> is not allowed. Expected a NUMBER or a STRING in field %d of",
                    fld_ptr->word,fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

         case  WORD_STRING_ONLY :
             sprintf(msg_buf,
          "<%.40s> is not allowed. Expected a WORD or a STRING in field %d of",
                  fld_ptr->word,fld_ptr->num);
             error_message(ERROR,msg_buf);
             sprintf(msg_buf,"\n       relation <%.40s>. Used in fact:",name);
             send_message(msg_buf,NO);
             break;

         case  ALL :
             sprintf(msg_buf,"\nINTERNAL ERROR : Please report this error!!");
             send_message(msg_buf,NO);
             break;
     }
     sprintf(msg_buf,"\n         f-%d   ",fact_ptr->fact_num);
     send_message(msg_buf,NO);
     print_rel(fact_ptr,name);
}


/* -------------------------------------------------
 * Name        :  num_string_check();
 * Purpose     :  Checks to see if a relation's field
 *                is a string or a number and if it
 *                has been specified in the allowed-numbers
 *                and allowed-strings lists.
 * Arguments   :  def_fld_ptr - pointer to a specified
 *                field in defrelation.
 *                act_fld_ptr - pointer to a relation's
 *                field.
 * Returns     :  Nothing meaningful, but it will generate
 *                an error message if an error is detected.
 * -------------------------------------------------
 */

static void num_string_check(def_fld_ptr,act_fld_ptr,fact_ptr,name)
DF_PTR    def_fld_ptr;
FIELD_PTR act_fld_ptr;
SPE_FACT_PTR fact_ptr;
char *name;
{
   if(check_string(def_fld_ptr->possible_strings,
                   act_fld_ptr,fact_ptr,name) == ERROR)
   {
       if(check_number(def_fld_ptr,act_fld_ptr,fact_ptr,name) == ERROR)
          error_mess(act_fld_ptr,fact_ptr,NUM_STRING_ONLY,name);
   }
}

/* ------------------------------------------------ *
 * Name        :  string_word_check();
 * Purpose     :  Checks to see if a relation's field
 *                is a string or a word and if it is
 *                specified in the allowed-strings or
 *                allowed-words lists.
 * Arguments   :  def_fld_ptr - pointer to a specified
 *                field in defrelation.(DF_PTR)
 *                act_fld_ptr - pointer to a relation's
 *                field.(FIELD_PTR)
 *                realtion    - pointer to the relation.
 *                (ACT_REL_PTR)
 * Returns     :  Nothing, but it will generate
 *                an error message if an error is detected.
 * ------------------------------------------------*/
  static void string_word_check(def_fld_ptr,act_fld_ptr,fact_ptr,name)
  DF_PTR def_fld_ptr;
  FIELD_PTR act_fld_ptr;
  SPE_FACT_PTR fact_ptr;
  char *name;

  {
        if(check_string(def_fld_ptr->possible_strings,act_fld_ptr,fact_ptr,
           name) == ERROR)
           {
             if(check_word(def_fld_ptr->possible_words,act_fld_ptr,fact_ptr,
                name) == ERROR)
                    error_mess(act_fld_ptr,fact_ptr,WORD_STRING_ONLY,name);
           }
 }

/* ------------------------------------------------- *
 * Name        :  num_word_check();
 * Purpose     :  Checks to see if a relation's field
 *                is a NUMBER or a WORD and if it is
 *                specified in the allowed-numbers or
 *                allowed-words lists.
 * Arguments   :  def_fld_ptr - pointer to a specified
 *                field in defrelation.(DF_PTR)
 *                act_fld_ptr - pointer to a relation's
 *                field.(FIELD_PTR)
 *                fact_ptr    - pointer to the fact in trace file.
 *                (ACT_REL_PTR)
 * Returns     :   Nothing, but it will generate
 *                 an error message if an error is detected.
 * ------------------------------------------------- */
 void num_word_check(def_fld_ptr,act_fld_ptr,fact_ptr,name)
  DF_PTR def_fld_ptr;
  FIELD_PTR act_fld_ptr;
  SPE_FACT_PTR fact_ptr;
  char *name;
   {
        if(check_number(def_fld_ptr,act_fld_ptr,fact_ptr,name) == ERROR)
         {
            if(check_word(def_fld_ptr->possible_words,act_fld_ptr,fact_ptr,name) ==
               ERROR)
               error_mess(act_fld_ptr,fact_ptr,NUM_WORD_ONLY,name);
                
     }
   }


/* ----------------------------------------------------------- *
 * Name        :  num_word_str_check();
 * Purpose     :  Checks to see if a relation's field
 *                is a NUMBER , a WORD or a STRING and
 *                if it is specified in the allowed-numbers or
 *                allowed-words or allowed-strings lists.
 * Arguments   :  def_fld_ptr - pointer to a specified
 *                field in defrelation.(DF_PTR)
 *                act_fld_ptr - pointer to a relation's
 *                field.(FIELD_PTR)
 *                fact_ptr   - pointer to a fact in trace file.
 *                (ACT_REL_PTR)
 * Returns     :  Nothing, but it will generate
 *                an error message if an error is detected.
 * ------------------------------------------------------------ */
 static void num_word_str_check(def_fld_ptr,act_fld_ptr,fact_ptr,name)
 DF_PTR def_fld_ptr;
 FIELD_PTR act_fld_ptr;
 SPE_FACT_PTR fact_ptr;
 char *name;

  {
     if(check_string(def_fld_ptr->possible_strings,act_fld_ptr,fact_ptr,
        name) == ERROR)
       {
            if(check_number(def_fld_ptr,act_fld_ptr,fact_ptr,name) == ERROR)
                 {
          if(check_word(def_fld_ptr->possible_words,act_fld_ptr,fact_ptr,
             name) == ERROR)
             error_mess(act_fld_ptr,fact_ptr,ALL,name);
         }
      }
  }
/* ------------------------------------------------------------ *
 *  Checks each field of a relation to make sure it
 *  meet the specification of the defrelation.
 *  - fact_ptr : the fact that needs to be crossed reference with definition.
 *  - node     : the node that contains the definition of the relation.
 *  - name     : name of the relation.
 * ------------------------------------------------------------ */

 static int check_rel_fields(fact_ptr,node,name)
 SPE_FACT_PTR fact_ptr;
 DR_PTR node;
 char *name;

 {
   DF_PTR def_fld_ptr;
   FIELD_PTR act_fld_ptr;
   int i;

   if(fact_ptr->number_field > 1)
     {
       act_fld_ptr = fact_ptr->field;
       while(act_fld_ptr != NULL)
         {
           i = act_fld_ptr->num;
           def_fld_ptr = node->fields;

           /* ----------------------------------------- *
            *   Check the specification of the  field. *
            * ----------------------------------------- */

           while((def_fld_ptr !=NULL)&&(def_fld_ptr->position != i))
           {
             def_fld_ptr = def_fld_ptr->next_field;
           }
           /* ----------------------------------------- *
            *  If not specified,assumed to be OK        *
            * ----------------------------------------- */

           if(def_fld_ptr == NULL)
               return(OK);

            /* ---------------------------------------- *
             *   Word only!                             *
             * ---------------------------------------- */

           if((def_fld_ptr->allow_word == YES)
               &&(def_fld_ptr->allow_string == NO)
               &&(def_fld_ptr->allow_number == NO))
             {
              if(check_word(def_fld_ptr->possible_words,act_fld_ptr,fact_ptr,
                 name) == ERROR)
               {
                 error_mess(act_fld_ptr,fact_ptr,WORD_ONLY,name);
               }
             }
           /* ------------------------------------------ *
            *      String only!                          *
            * ------------------------------------------ */

           else if((def_fld_ptr->allow_string == YES)&&
                   (def_fld_ptr->allow_number == NO)&&
                   (def_fld_ptr->allow_word   == NO))
             {
              if(check_string(def_fld_ptr->possible_strings,act_fld_ptr,
                 fact_ptr,name) == ERROR)
                {
                  error_mess(act_fld_ptr,fact_ptr,STRING_ONLY,name);
                }
             }
           /* ------------------------------------------ *
            *           Number only!                     *
            * ------------------------------------------ */

           else if((def_fld_ptr->allow_number == YES)&&
                   (def_fld_ptr->allow_string == NO)&&
                   (def_fld_ptr->allow_word == NO))
             {
              if(check_number(def_fld_ptr,act_fld_ptr,fact_ptr,name) == ERROR)
                {
                  error_mess(act_fld_ptr,fact_ptr,NUMBER_ONLY,name);
                }
             }
          /* ------------------------------------------- *
           *   String and number are allowed.            *
           * ------------------------------------------- */

           else if((def_fld_ptr->allow_number == YES)&&
                   (def_fld_ptr->allow_string == YES)&&
                   (def_fld_ptr->allow_word == NO))
               {
                 num_string_check(def_fld_ptr,act_fld_ptr,fact_ptr,name);
               }
           /* -------------------------------------------- *
            *         String and Word only!                *
            * -------------------------------------------- */

           else if((def_fld_ptr->allow_number == NO)&&
                   (def_fld_ptr->allow_string == YES)&&
                   (def_fld_ptr->allow_word == YES))
              {
                string_word_check(def_fld_ptr,act_fld_ptr,fact_ptr,name);
              }
           /* ------------------------------------------- *
            *         Number and Word only!               *
            * ------------------------------------------- */

           else if((def_fld_ptr->allow_number == YES)&&
                   (def_fld_ptr->allow_string == NO)&&
                   (def_fld_ptr->allow_word == YES))
              {
                num_word_check(def_fld_ptr,act_fld_ptr,fact_ptr,name);
              }
           /* ---------------------------------------------- *
            *       Number,Word  and String are allowed.     *
            * ---------------------------------------------- */

           else if((def_fld_ptr->allow_number == YES)&&
                   (def_fld_ptr->allow_string == YES)&&
                   (def_fld_ptr->allow_word == YES))
              {
                num_word_str_check(def_fld_ptr,act_fld_ptr,fact_ptr,name);
              }
           act_fld_ptr = act_fld_ptr->next;

         }
     }
     return (OK);
 }

/* ------------------------------------------------------- *
 * Evaluates each relation which was asserted during
 * the run time. Since defrelation is a binary tree structure
 * list, recursive is used.
 * Defrelation is provided by either the user or the CRSV.
 * If it is provided by CRVS the evaluation process
 *   will stop and a warning message will be generated.
 * If it is provided by the user the evaluation process
 *   will check each field to detect any conflict bettween
 *   the relation and its definition.
 * -------------------------------------------------------- */

void evaluate_rel(fact_ptr,name)
SPE_FACT_PTR fact_ptr;
char *name;
{
   DR_PTR node;


   if((node = find_def_rel(def_rel_head,name)) == NULL)
      {
        if(strcmp(name,"initial-fact") != 0)
          {
            sprintf(msg_buf,"No definition for relation <%.40s> provided",
                    name);
            error_message(WARNING,msg_buf);
          }
      }
   else if(node->created_by == CRSV_MADE)
     {
        sprintf(msg_buf,"No definition for relation <%.40s> provided",name);
        error_message(WARNING,msg_buf);
     }
   else
     {
        if(fact_ptr->number_field > node->max_fields)
          {
             sprintf(msg_buf,
                     "Too many fields used in fact: \n         f-%d ",
                     fact_ptr->fact_num);
             error_message(ERROR,msg_buf);
             print_rel(fact_ptr,name);
          }
        else if(fact_ptr->number_field < node->min_fields)
          {
             sprintf(msg_buf,
                  "Not enough fields used in fact: \n         f-%d ",
                  fact_ptr->fact_num);
             error_message(ERROR,msg_buf);
             print_rel(fact_ptr,name);
              }
        else
              {
                (void)check_rel_fields(fact_ptr,node,name);
              }
     }
}

/* ------------------------------------------------ *
 *  PRINT_REL()
 *    Print a relation, includes the relation's name
 *         and its fields.
 *
 *  Argument:
 *     fact_ptr : pointer pointing to the relation
 *     name     : name of the relation.
 *
 *  Returns: nothing.
 * ------------------------------------------------ */

static void print_rel(fact_ptr,name)
SPE_FACT_PTR fact_ptr;
char *name;
{
     FIELD_PTR fld_ptr;

     sprintf(msg_buf,"(%s",name);
     send_message(msg_buf,NO);
     fld_ptr = fact_ptr->field;
     while(fld_ptr!=NULL)
       {
         sprintf(msg_buf,"  %s",fld_ptr->word);
         send_message(msg_buf,NO);
         fld_ptr = fld_ptr->next;
       }
     send_message(")",NO);

}
 /* ---------------------------------------------------------------
  * Print_rule_list()
  *    This function prints all the rules that were activated or
  *    deactivated because of the assertion or retraction of
  *    the relation.
  * Arguments:
  *   - RULE_NAME_PTR  list;
  *           head of the rule list.
  * Return: Nothing;
  * ----------------------------------------------------------------
  */
  static void print_rule_list(list)
  RULE_NAME_PTR list;
  {
    RULE_NAME_PTR  node;

    node = list;
    while(node != NULL)
      {
         if(node->last_fact == YES)
          {
             sprintf(msg_buf,"         <%.40s> ( %d times, last fact)\n",
                     node->name,node->counter);
             send_message(msg_buf,NO);
           }
         else
          {
             sprintf(msg_buf,"         <%.40s> ( %d times)\n",
                     node->name,node->counter);
             send_message(msg_buf,NO);
          }
         node = node->next;
      }

  }
 /* -------------------------------------------------------
 *  void PRINT_SPE_REL_IN_DRIB()
 *     This function will travel through the defrelation list
 *     and specifically print all the important information
 *     about each relation during the process of trace file.
 *        
 *  Argument:
 *     Node : a node from defrelation list.
 *
 *  Returns: Nothing.
 * -------------------------------------------------------
 */

  static void print_spec_rel_in_drib(node)
  ACT_REL_PTR node;
  {
    SPE_FACT_PTR fact_ptr;

   if(node->left != NULL)
      print_spec_rel_in_drib(node->left);
   sprintf(msg_buf, "Relation                  : %.40s\n", node->name);
   send_message(msg_buf,NO);
   sprintf(msg_buf,"has been asserted         : %d times\n",
           node->num_assert);
   send_message(msg_buf,NO);
   sprintf(msg_buf,"Retracted                 : %d times\n",node->num_retract);
   send_message(msg_buf,NO);
   sprintf(msg_buf,"Max existing at one time  : %d \n",node->max_occurences);
   send_message(msg_buf,NO);
   fact_ptr = node->spec_fact_list;

   while(fact_ptr != NULL)
   {
      sprintf(msg_buf,"   f-%d  ",fact_ptr->fact_num);
      send_message(msg_buf,NO);
      print_rel(fact_ptr,node->name);
      if((fact_ptr->num_activated_a == 0)&&(fact_ptr->num_deactivated_a == 0)&&
         (fact_ptr->num_activated_d == 0)&&(fact_ptr->num_deactivated_d == 0))
        {
           send_message(
           "   was not involved in Activating or Deactivating any rule\n",NO);
        }
      else
        {
           if(fact_ptr->num_activated_a > 0)
             {
               sprintf(msg_buf, "\n   Involved in Activating by being asserted     : %d  times \n",
                  fact_ptr->num_activated_a);
               send_message(msg_buf,NO);
               send_message("      Activated rules were:\n",NO);
               print_rule_list(fact_ptr->activate_list_a);
             }
           if(fact_ptr->num_deactivated_a > 0)
             {
               sprintf(msg_buf,"   Involved in Deactivating by being asserted   : %d  times \n",
                  fact_ptr->num_deactivated_a);
               send_message(msg_buf,NO);
               send_message("      Deactivated rules were:\n",NO);
               print_rule_list(fact_ptr->deactivate_list_a);
             }
           if(fact_ptr->num_activated_d > 0)
             {
               sprintf(msg_buf,
               "   Involved in Activating by being retracted    : %d  times \n",
                       fact_ptr->num_activated_d);
               send_message(msg_buf,NO);
               send_message("      Activated rules were:\n",NO);
               print_rule_list(fact_ptr->activate_list_d);
             }
           if(fact_ptr->num_deactivated_d > 0)
             {
               sprintf(msg_buf,
               "   Involved in Deactivating by being retracted  : %d  times \n",
                       fact_ptr->num_deactivated_d);
               send_message(msg_buf,NO);
               send_message("      Deactivated rules were:\n",NO);
               print_rule_list(fact_ptr->deactivate_list_d);
             }
        }
        send_message("\n",NO);
        fact_ptr = fact_ptr->next;
  }

   send_message(" ------------------------------------------------------\n",NO);
   if(node->right != NULL)
      print_spec_rel_in_drib(node->right);
}

 /* ----------------------------------------- *
  * This function returns the node            *
  * which has the same name as the provided   *
  * <name>.                                   *
  * ----------------------------------------- */

 ACT_REL_PTR find_act_rel(node,name)
 ACT_REL_PTR node;
 char *name;
 {
     int rtn;

     if(node == NULL)
        return(NULL);

     rtn = strcmp(name,node->name);
     if(rtn < 0)
       return(find_act_rel(node->left,name));
     else if (rtn > 0)
       return(find_act_rel(node->right,name));
     else
       return(node);
 }

 /* ---------------------------------------------------- *
  *  evaluate each relation found in dribble file        *
  * ---------------------------------------------------- */

static void check_for_dyn_unassrt_rels(def_rel_ptr)
DR_PTR        def_rel_ptr;
{
   ACT_REL_PTR rel_ptr;
    
   IF(def_rel_ptr EQ NULL) THEN
      return;
   END_IF

   check_for_dyn_unassrt_rels(def_rel_ptr->lft_def_rel);
   
   IF(def_rel_ptr->created_by EQ USER_MADE) THEN
      rel_ptr = find_act_rel(act_rel_head,def_rel_ptr->name);
      IF(rel_ptr EQ NULL) THEN
         sprintf(msg_buf,"\nRelation <%.40s> was not asserted",
                 def_rel_ptr->name);
         send_message(msg_buf,NO);
      END_IF
   END_IF
   
   check_for_dyn_unassrt_rels(def_rel_ptr->rht_def_rel);
}
  
/* ==========================================================
 * This function checks if the rules are activated
 * during execution. If they have been activated, it checks
 * the number of times they were deactivated.
 * ==========================================================
 */
  
void check_for_dyn_unact_rules(rule_ptr)
RULE          *rule_ptr;
{ 
   ACT_R_PTR rule;
   float     ratio;
   
   IF(rule_ptr EQ NULL) THEN
      return;
   END_IF

   check_for_dyn_unact_rules(rule_ptr->lft_rule);
   
   rule = act_rule_head;               /* Use the global variable */
   while(rule NEQ NULL) DO
      IF(strcmp(rule_ptr->name, rule->name) EQ 0) THEN
         break;
      END_IF
      rule = rule->next;
   END_WHILE
    
   IF(rule EQ NULL) THEN
      sprintf(msg_buf,"\nRule <%.50s> was not activated",
                      rule_ptr->name);
      send_message(msg_buf,NO);
   ELSE
      ratio = (float) rule->deactivate_num / (float) rule->activate_num;
      IF(ratio >= MAX_DEACTIVATE_RATIO) THEN
          sprintf(msg_buf,"\nRule <%.37s> was deactivated %5.1f%% of the time",
                  rule_ptr->name, ratio * 100.0);
          send_message(msg_buf,NO);
       END_IF
   END_IF
     
   check_for_dyn_unact_rules(rule_ptr->rht_rule);
   
   return;
  }

  /* -----------------------------------------
   * PROCESS_TRACE_FILE()
   *   This function will  scan through the
   *   trace file ,is created by user;and
   *   store all information concerning about
   *   each relation,each rule ect...
   *
   * Argument :
   *    file_name : Name of trace file.
   *
   * return:
   * -----------------------------------------
   */

 int process_trace_file(file_name)
 char *file_name;
 {

      FILE  *fp;

      if((fp = open_file(file_name)) EQ NULL)
         {
           send_message("Can't open trace file \n",NO);
           end_crsv();
          }
      send_message("\n",NO);
      last_act_rel = NULL;
      max_occurences = 0;
      if(scan_trace_file(fp) == ERROR)
        {
          error_message(ERROR,"CRSV Trace file was not found \n");
        }

      /* ============================================== *
       *  cross referencing relations found in in the   *
       *  trace file.                                   *
       * ============================================== *
       */

      else
       {
          cur_line_num = 0;
       }
      (void)close_file(fp);
 }

/*ARGSUSED*/ /* Make lint happy */
void check_dynamic_relations(my_rel_head, my_def_rel_head, my_act_rel_head)
RELATION      *my_rel_head;
DEF_REL       *my_def_rel_head;
ACT_RELATION  *my_act_rel_head;
{
   check_for_dyn_unassrt_rels(my_def_rel_head);
}

/*ARGSUSED*/ /* Make lint happy */
void check_dynamic_rules(my_rule_head, my_act_rule_head)
RULE          *my_rule_head;
ACT_RULE      *my_act_rule_head;
{
   check_for_dyn_unact_rules(my_rule_head);
}
