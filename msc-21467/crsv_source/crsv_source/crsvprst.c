/* ========================================================= *
 *  This file contains all the fuctions that are used to     *
 *  process the trace file.                                  *
 * ========================================================= */

#include <stdio.h>
#include <ctype.h>
#include "crsv.h"
#if MAC_LSC
#include <strings.h>
#else
#include <string.h>
#endif

#define ASSERTION           1
#define RETRACTION          2

/* Extern functions  */

 extern ACT_R_PTR      alloc_a_rule();       /* From csrvmem.c */
 extern ACT_REL_PTR    alloc_a_rel();
 extern SPE_FACT_PTR   alloc_fact_list();
 extern COMP_R_PTR     alloc_c_rule();
 extern COMP_P_PTR     alloc_cp();
 extern FIELD_PTR      alloc_field();
 extern REL_LIST_PTR   alloc_rl();
 extern RULE_NAME_PTR  alloc_rule_list();
 extern void           evaluate_rel();       /* from trace.c */
 extern char          *gen_alloc();          /* from crsvmem.c */
 extern HASH          *add_symbol();         /* from crsvhash.c */
 extern void          send_message();        /* From crsvprnt.c*/
 extern void          error_message();

/* global variables which have been defined from another file */

 extern ACT_R_PTR    act_rule_head;
 extern ACT_REL_PTR  act_rel_head;
 extern COMP_R_PTR   comp_rule_head;
 extern DR_PTR       def_rel_head;
 extern ACT_REL_PTR  last_act_rel;        /* last active relation            */
 extern int          max_occurences;      /* keep track with the maximum     */
                                          /* occurrences at one time         */
                                          /* of an active relation           */
 extern short        last_operation;      /* last operation code , RETRACTION*/
                                          /* or ASSERTION                    */
 extern short        first_relation;      /* Is it first relation in the     */
                                          /* trace file ?                    */
 extern short        first_rule;          /* Is it first rule in the trace   */
                                          /* file ?                          */

 /* ---------   Local variables  ---------- */

 ACT_REL_PTR cur_act_rel;        /* current relation returned by             */
                                 /* find_create_act_rel                      */
 SPE_FACT_PTR cur_fact;          /* specific information about the current   */
                                 /* relation                                 */
 int fact_num;                   /* the number corresponds to the            */
                                 /* current fact in the trace  file          */

 HASH    *save_word;             /* A pointer to a hash word for saving      */
                                 /* words in the symbol table                */

 /* --------- Local functions ---------------*/

 static void        skip_blank();
 static void        get_token_from_str();
 int                get_num_from_str();
 static ACT_REL_PTR find_create_act_rel();
 void               process_rel_activation();
 void               process_rule_activation();
 void               process_misc();
 static void        search_act_facts();
 static void        insert_rule();
 static int         search_fact();
 static int         store_rule_name();
 void               process_rel_deactivation();
 void               process_rule_deactivation();
 void               process_rule_fire();

/* ============================================== *
 *   Skip all the consecutive blanks in a string  *
 * ============================================== */

 static void skip_blank(str,i)
 char *str;
 int *i;

 {
    while(str[*i] == ' ')
      (*i) ++;
 }

 /* =========================================== *
  * This fuction will get a token from a string *
  * =========================================== */

 static void get_token_from_str(str,tkn,i)
 char *str, *tkn;
 int *i;

 {
    int j = 0;

    skip_blank(str,i);
    if(str[*i] == '"')
     {
        tkn[j++] = str[(*i)++];
        while(str[*i] != '"' || str [*i - 1] == '\\')
         {
            tkn[j++] = str[(*i)++];
         }
        tkn[j++] = str[(*i)++];
     }
    else
	 {
	    while((str[*i] != ' ')&&(str[*i] != ')'))
	      {
                tkn[j++] = str[(*i)++];
	      }
	 }
    tkn[j] = 0;
 }

 /* ----------------------------------- *
  * Get a number from a string          *
  * ----------------------------------- */

 int get_num_from_str(str,i)
 char *str;
 int *i;

{
     int j = 0;
   int sign = 1;

     skip_blank(str,i);

   IF (str[*i] == '-') THEN
      sign = -1;
      (*i) ++;
   ELSE_IF (str[*i] == '+')
      (*i) ++;
   END_IF

   WHILE (isdigit(str[*i])) DO
      j = 10 * j + ((int)str[*i] - (int)'0');
      (*i) ++;
   DONE

   return (sign * j);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  find_create_act_rel()
 *
 *  PURPOSE:  This function searches through a list of
 *            relations to find a specific name. If it doesn't
 *            find the name, it creates a new relation structure
 *            and adds it to the list.
 *
 *  INPUTS :  A pointer to the list of relations (ACT_REL_PTR)
 *            and the name of the node being searched (char *)
 *
 *  RETURNS:  A pointer to the relation structure which has a same
 *            name as the passed argument.
 *
 *  NOTES  :
 * ------------------------------------------------------------------
 */

static ACT_REL_PTR find_create_act_rel(node, name)
ACT_REL_PTR  node;
char    *name;
{
   int rtn;

   IF(node EQ NULL) THEN        /* A new relation! */
      node        = alloc_a_rel();
      save_word   = add_symbol(name);
      node->name  = save_word->contents;
      cur_act_rel = node;
   ELSE
      rtn = strcmp(name, node->name);
      IF(rtn < 0) THEN
         node->left = find_create_act_rel(node->left, name);
      ELSE_IF(rtn > 0)
         node->right = find_create_act_rel(node->right, name);
      ELSE
         cur_act_rel = node;
      END_IF
   END_IF
   return(node);
}
/*
 * -----------------------------------------------------------------------
 *   Fuction name : Process_rel_activate
 *         This function will put a relation's info. into an act_relation
 *         structure and link it to the global link list (act_rel_head).
 *   Argument(s) :
 *      str : char *str;
 *   Return : Nothing
 * -----------------------------------------------------------------------
*/
 void process_rel_activation(str)
 char  *str;
  {
    SPE_FACT_PTR fact_ptr;
    FIELD_PTR  field_ptr,temp_fld_ptr;
    int i,num;
    char name[WORDLENGTH],relation_name[WORDLENGTH];

    /* ================================== *
     *  Form to be scanned:               *
     *   0        (initial-fact)          *
     * ================================== */

    i = 0;

    num = get_num_from_str(str,&i);        /* get the fact number */
    if(!first_relation)
      {
          if (num > 1)
             {
               error_message(WARNING, "Incomplete trace file!\n        ");
               send_message(
                  "The first asserted relation in the trace file is\n    ",YES);
               send_message(
                  "    not fact 0. Trace file analysis may be incomplete",YES);
             }
          first_relation = TRUE;
      }

    /* -------------------------------------------------- *
     *  Allocate space for new relation and initialize it *
     * -------------------------------------------------- */

    /* scanning the relation name  */

    while(str[i] != '(')
       i++;
    i++;
    skip_blank(str,&i);
    get_token_from_str(str,relation_name,&i);
    if(VERBOSE IS_ON)
      {
         sprintf(msg_buf,"\n\nProcessing activated fact f-%d <%.40s>",num,
                 relation_name);
         send_message(msg_buf,NO);
      }
    /* ---------------------------------------------------- *
     *  Search through the relation list to see if relation *
     *  has existed.If it has,increase the assertion counter*
     *  by 1,else allocate new node and link it to the list.*
     *  All of these will be done by "find_create_act_rel", *
     *  and use the relation name as the key.               *
     * -----------------------------------------------------*/

    act_rel_head = find_create_act_rel(act_rel_head,relation_name);
    cur_act_rel->current_active ++;
    if(cur_act_rel->max_occurences < cur_act_rel->current_active)
            cur_act_rel->max_occurences = cur_act_rel->current_active;
    cur_act_rel->num_assert ++;

    /* ------------------------------------------------------------- *
     * Get new maximum occurences of a relation at one time          *
     * if neccesary.                                                 *
     * From here down <fact> will be mentioned instead of <relation> *
     * since I assume fact is an instance of a relation.             *
     * ------------------------------------------------------------- */

    /* -------------------------------- *
     *  Store the fields of a fact.     *
     * -------------------------------- */

    fact_ptr = alloc_fact_list();
    fact_ptr->number_field = 1;
    fact_ptr->fact_num = num;     		  /* num = 0 for <initial-fact> */

    /* scanning for fact's fields */

    while(str[i] != ')')
      {
         while(str[i] == ' ')
            i++;
         if(str[i] == ')')
            break;
         (fact_ptr->number_field)++;      /* Increase number of fields in the
                                             record by one */
         get_token_from_str(str,name,&i); /* Get the next field */

         /* ------------------------------------------------ *
          *  Allocate the space to store a field of the      *
          *  fact and initialize it.                         *
          * ------------------------------------------------ */

         field_ptr       = alloc_field();
         field_ptr->num  = fact_ptr->number_field;
         save_word       = add_symbol(name);
         field_ptr->word = save_word->contents;

         /* ------------------------------------------------ *
          *  Insert the field to the fact                    *
          * ------------------------------------------------ */

         if(fact_ptr->field == NULL)
           fact_ptr->field = field_ptr;
         else if (fact_ptr->field->next == NULL)
           fact_ptr->field->next = field_ptr;
         else
           {
              temp_fld_ptr = fact_ptr->field;
              while (temp_fld_ptr->next != NULL)
                 temp_fld_ptr  = temp_fld_ptr->next;
              temp_fld_ptr->next = field_ptr;

           }

      }
    /* --------------------------------------------
     *  Insert the fact into the global list.
     * --------------------------------------------
     */
     fact_ptr->next = cur_act_rel->spec_fact_list;
     cur_act_rel->spec_fact_list = fact_ptr;
     last_operation = ASSERTION;
     fact_num = num;
     if(CHECK_DEFRELS IS_ON)
       {
          if(def_rel_head == NULL)
           {
               error_message(WARNING, "Relations have not been defined.\n   ");
               send_message(
                  "      Relations in the trace file can't be verified.",YES);
            }
          else
            {
               evaluate_rel(fact_ptr,relation_name);
            }
       }

   }

  /*
  * ----------------------------------------------------------------------
  *   Name: process_rule_activate();
  *         This function records all informations about the activated
  *         rule.
  * ----------------------------------------------------------------------
  */

 void process_rule_activation(str)
 char  *str;

 {
    int i = 0,j,num;
    char name[WORDLENGTH];

    /* =============================================================== *
     *  Form to be scanned :                                           *
     *      <salience> <rule-name>: [f-]<num>, ,[f-]<num>              *
     * =============================================================== */


    /* ------------------------------------------------ *
     *   Get salience value of the rule                 *
     *   This value is stored but have not been used yet*
     * ------------------------------------------------ */

    num =  get_num_from_str(str,&i);
    skip_blank(str,&i);

    /* ------------------------------------------------- *
     *    Get the name of the rule.                      *
     * ------------------------------------------------- */
    j = 0;
    while(str[i] != ':')
      name[j++] = str[i++];
    name[j] = 0;
    j =  0;
    if(VERBOSE IS_ON)
      {
        sprintf(msg_buf,"\n\nProcessing activated rule <%.40s>",name);
        send_message(msg_buf,NO);
      }
    i++;
    skip_blank(str,&i);

    /* -------------------------------------------------- *
     *      Get the string containing informations about  *
     *      which facts trigger this rule                 *
     * -------------------------------------------------- */

/*    while(i < strlen(str))
       factstr[j++] = str[i++];
    factstr[j] = 0;*/

    /* ------------------------------------------------------ *
     *    Allocate space for new rule,initialize it,and       *
     *    link it to the rule list.                           *
     * ------------------------------------------------------ */
    insert_rule(name,&str[i],num);

      /* -------------------------------------------------- *
       * Looking for the facts that activate this rule.     *
       * -------------------------------------------------- */
    search_act_facts(name,&str[i]);
 }



 /* --------------------------------------------------- *
  *  insert_rule()                                      *
  *    Allocate space for new rule,initialize it,and    *
  *    link it to the rule list.                        *
  * --------------------------------------------------- */

 static void insert_rule(name,factstr,num)
 char *name,*factstr;
 int num;
  {
    ACT_R_PTR  rule_ptr,temp_rule_ptr;
    REL_LIST_PTR alist;

    if(act_rule_head == NULL)   /* The list is empty  */
      {
         act_rule_head           = alloc_a_rule();
         save_word               = add_symbol(name);
         act_rule_head->name     = save_word->contents;
         act_rule_head->salience = num;

         act_rule_head->activate_list = alloc_rl();
         save_word = add_symbol(factstr);
         act_rule_head->activate_list->activate_string = save_word->contents;
         act_rule_head->activate_num = 1;
      }
    else
      {
         temp_rule_ptr = act_rule_head;
         rule_ptr = act_rule_head;
         while(temp_rule_ptr != NULL)
           {
             if(strcmp(temp_rule_ptr->name,name) == 0)
               break;
             rule_ptr = temp_rule_ptr;
             temp_rule_ptr = temp_rule_ptr->next;
           }

         if(temp_rule_ptr != NULL)   /* rule has been inserted into the list */
           {
              temp_rule_ptr->activate_num ++;
              alist = alloc_rl();

              /* ===================================================== *
               * store the string which contains the information about *
               * the facts that trigger the rule                       *
               * ===================================================== */

              save_word              = add_symbol(factstr);
              alist->activate_string = save_word->contents;

              /* ============================================= *
               * Link alist to the list                        *
               * ============================================= */

              alist->next = temp_rule_ptr->activate_list;
              temp_rule_ptr->activate_list = alist;

           }
          else        /* new rule */
	       {
	         temp_rule_ptr           = alloc_a_rule();
	         save_word               = add_symbol(name);
	         temp_rule_ptr->name     = save_word->contents;
	         /* salience is currently not used */
	         temp_rule_ptr->salience = num;

	         temp_rule_ptr->activate_list = alloc_rl();
	         /* same thing as activate_list */
	         save_word = add_symbol(factstr);	
	         temp_rule_ptr->activate_list->activate_string = save_word->contents;
	         temp_rule_ptr->activate_num = 1;
	         rule_ptr->next = temp_rule_ptr;
	
           }
      }

  }
 /* -------------------------------------------------- *
  * search_act_facts()                                 *
  *  Looking for the facts that activate this rule.    *
  * -------------------------------------------------- */
static void search_act_facts(name,factstr)
char *name,*factstr;
{
     short flag;
     int i, num;

     /* the rule was activated because of the retraction of a certain fact,*/
     /* and there is only a (not) pattern on the left-hand side of the rule */

   IF ((factstr[0] == 0)&&(last_operation == RETRACTION)) THEN
          flag = FALSE;
          (void)search_fact(act_rel_head,fact_num,&flag);
      IF (flag == TRUE) THEN
         cur_fact->num_activated_d ++;
         IF (ANALYZE_TRACE IS_ON AND VERBOSE IS_ON) THEN
	          store_rule_name(name,fact_num);
         END_IF
      END_IF
   END_IF

     i = 0;
   WHILE(i < strlen(factstr)) DO/* <factstr> is the string which stores   */
                                  /* info about the facts that activate the */
                                  /* rule                                   */
      WHILE ((!(isdigit(factstr[i])))&&(i < strlen(factstr))) DO
           i++;
      DONE

      IF(i < strlen(factstr))THEN
         num = 0;
         WHILE (isdigit(factstr[i])) DO
            num = num * 10 + ((int)factstr[i++] - (int)'0');
         DONE
	 /* ------------------------------------------------- *
	  *  Increases the relations' activate counter by 1.  *
	  * ------------------------------------------------- */
	
	         flag = FALSE;
	         cur_fact = NULL;
                 (void)search_fact(act_rel_head,num,&flag);
         IF (flag == TRUE) THEN
            cur_act_rel->activate_num  ++;
            IF (last_operation == ASSERTION) THEN
               cur_fact->num_activated_a ++;
            ELSE_IF (last_operation == RETRACTION)
               cur_fact->num_activated_d ++;
		
		 /* ---------------------------------------------------- *
		  *  Be more specific about the facts that activate the  *
		  *  rule if the user requested.                         *
		  * ---------------------------------------------------- */
		
               IF (ANALYZE_TRACE IS_ON AND VERBOSE IS_ON) THEN
		              store_rule_name(name,num);
               END_IF
            END_IF
         END_IF
      END_IF
   DONE
}


/* ---------------------------------------------- *
 * Search_fact()                                  *
 *   This function will look for the fact that    *
 *   has same number  as <num> from the global    *
 *   list.                                        *
 * Argumments                                     *
 *   relation_ptr : head of the list              *
 *   num          : fact's number                 *
 *   flag         : TRUE if found                 *
 * Returns:  ERROR or OK                          *
 * ---------------------------------------------- */

static int search_fact(relation_ptr,num,flag)
ACT_REL_PTR relation_ptr;
int num;
short *flag;

{
   SPE_FACT_PTR temp_ptr;

   if(relation_ptr == NULL)
      return(ERROR);
   temp_ptr = relation_ptr->spec_fact_list;
   while((temp_ptr != NULL)&&(!(*flag)))
      {
         if(temp_ptr->fact_num == num)
            {
               *flag = TRUE;
               cur_act_rel = relation_ptr;   /* global current active relation */
               cur_fact = temp_ptr;          /* global current active fact */
            }
         temp_ptr = temp_ptr->next;
      }
   if((*flag) == FALSE)
      (void)search_fact(relation_ptr->left,num,flag);
   if((*flag) == FALSE)
      (void)search_fact(relation_ptr->right,num,flag);

   return (OK);
}


 /* ----------------------------------------------------------- *
  * This function adds rules to the activated rule list in the  *
  * spec_fact_list field of a relation.                         *
  * ----------------------------------------------------------- */

 static int store_rule_name(name,num)
  char *name;
  int num;
  {
     RULE_NAME_PTR rule_name_ptr,a_rule_name;

     if(last_operation == ASSERTION)
       {
         rule_name_ptr = cur_fact->activate_list_a;
         while(rule_name_ptr != NULL)
           {
             if(strcmp(name,rule_name_ptr->name) == 0)
                break;
             rule_name_ptr = rule_name_ptr->next;
           }
         if(rule_name_ptr == NULL)
           {
             a_rule_name       = alloc_rule_list();
             save_word         = add_symbol(name);
             a_rule_name->name = save_word->contents;
             if(fact_num == num)
               a_rule_name->last_fact = YES;
             a_rule_name->counter = 1;
             a_rule_name->next = cur_fact->activate_list_a;
             cur_fact->activate_list_a = a_rule_name;
           }
         else
           {
             rule_name_ptr->counter ++;
             if(fact_num == num)
               rule_name_ptr->last_fact = YES;
           }
       }
     else if (last_operation == RETRACTION)
       {
         rule_name_ptr = cur_fact->activate_list_d;
         while(rule_name_ptr != NULL)
           {
             if(strcmp(name,rule_name_ptr->name) == 0)
               break;
             rule_name_ptr = rule_name_ptr->next;
           }
         if(rule_name_ptr == NULL)
           {
             a_rule_name          = alloc_rule_list();
             save_word            = add_symbol(name);
             a_rule_name->name    = save_word->contents;
             a_rule_name->counter = 1;
             if(fact_num == num)
               a_rule_name->last_fact = YES;
             a_rule_name->next = cur_fact->activate_list_d;
             cur_fact->activate_list_d = a_rule_name;
           }
         else
           {
             rule_name_ptr->counter ++;
             if(fact_num == num)
               rule_name_ptr->last_fact = YES;
           }
       }
  }

 /*
  * ----------------------------------------------------------------------
  *   Name : process_rel_deactivate();
  *      This function is used for setting the flag of the relation which
  *      has been retract from the agenda.
  *
  *   Arguments : char *str;
  *               a linefrom the trace file that indicates the
  *               retraction is taken place.
  *   Return: nothing .
  * ----------------------------------------------------------------------
  */

 void process_rel_deactivation(str)
 char *str;

   {

      int i = 0,num;
      short flag;

      /* ==================================================== *
       *  Form to be scanned :                                *
       *     0     (initial-fact)                             *
       * ==================================================== */

      num =  get_num_from_str(str,&i);    /* Get the fact number */

      IF (VERBOSE IS_ON) THEN
          sprintf(msg_buf,"\n\nProcessing retracted fact f-%d",num);
          send_message(msg_buf,NO);
      END_IF

      flag = FALSE;

      /* Look for the fact that being retracted,*/
      (void)search_fact(act_rel_head,num,&flag);
      IF (flag == TRUE) THEN
              /* and store its address in cur_ac_rel    */
              cur_act_rel->num_retract ++;
              cur_act_rel->current_active -= 1;
    	      last_operation = RETRACTION;
	      fact_num = num;
      END_IF
   }


 /*
  * ----------------------------------------------------------------------
  *   name : process_rule_deactivate()
  *       This function will get the name of the rule has been deactivated
  *       and use it to search for the rule in the list.Once it is found
  *       the deactivate_num of the rule will be increased by one.
  *   arguments:
  *      char *str
  *        a line in the trace file that indicates a rule has been
  *        deactivated.
  *   return: nothing.
  * ----------------------------------------------------------------------
  */

 void process_rule_deactivation(str)
 char *str;

 {
   ACT_R_PTR        rule_ptr;
   RULE_NAME_PTR    name_ptr;
   int              i = 0,j;
   short            flag;
   char             name[WORDLENGTH];

   (void)get_num_from_str(str,&i);          /* Get the salience of a rule */
   skip_blank(str,&i);
   j = 0;

   /* get the name of the rule that been deactivated*/

   while((str[i] != '\n') && (str[i] != ' '))
     {
       name[j++] = str[i++];
     }
   name[j] = 0;
   if(VERBOSE IS_ON)
     {
       sprintf(msg_buf,"\n\nProcessing deactivated rule <%.40s>",name);
       send_message(msg_buf,NO);
     }

   /*----- Search for the rule -------*/

   rule_ptr = act_rule_head;
   while(rule_ptr != NULL)
   {
     if(strcmp(name,rule_ptr->name) ==  0)
       break;
     rule_ptr = rule_ptr->next;
   }
   if(rule_ptr != NULL)
      rule_ptr->deactivate_num  ++;

   /* ================================================= *
    *  If the last operation is RETRACTION, increase    *
    *  the number of rules being deactivated by the     *
    *  retraction of the relation by 1                  *
    * ================================================= */

   if(last_operation == RETRACTION)
     {
       flag = FALSE;
       (void)search_fact(act_rel_head,fact_num,&flag);
       if(flag == TRUE)
         {
               cur_act_rel->deactivate_num ++;
	       name_ptr = cur_fact->deactivate_list_d;
               cur_fact->num_deactivated_d ++;
	
	       /* ====================================== *
	        * Store the names of the rules that are  *
	        * deactivated by the retraction of this  *
	        * relation.                              *
	        * ====================================== */
	
	       if(ANALYZE_TRACE IS_ON AND VERBOSE IS_ON)
	         {
	            while(name_ptr != NULL)
	              {
	                 if(strcmp(name_ptr->name,name) == 0)
	                    break;
	                 name_ptr = name_ptr->next;
	              }
	            if(name_ptr != NULL)
                      name_ptr->counter ++;
	            else
	              {
	                 name_ptr          = alloc_rule_list();
	                 save_word         = add_symbol(name);
	                 name_ptr->name    = save_word->contents;
	                 name_ptr->counter = 1;
	                 name_ptr->next    = cur_fact->deactivate_list_d;
	                 cur_fact->deactivate_list_d = name_ptr;
	              }
	
	         }
          }
      }


   /* ================================================= *
    *  If the last operation is ASSERTION, increase     *
    *  the number of rules being deactivated by the     *
    *  assertion of the relation by 1                   *
    * ================================================= */

   else if (last_operation == ASSERTION)
      {
         flag = FALSE;
         (void)search_fact(act_rel_head,fact_num,&flag);
         if(flag == TRUE)
           {
                 cur_act_rel->deactivate_num ++;
	         name_ptr = cur_fact->deactivate_list_a;
                 cur_fact->num_deactivated_a ++;
	
	       /* ====================================== *
	        * Store the names of the rules that are  *
	        * deactivated by the assertion of this   *
	        * relation.                              *
	        * ====================================== */
	
	         if(ANALYZE_TRACE IS_ON AND VERBOSE IS_ON)
	         {
	            while(name_ptr != NULL)
	              {
	                 if(strcmp(name_ptr->name,name) == 0)
	                    break;
	                 name_ptr = name_ptr->next;
	              }
	            if(name_ptr != NULL)
                      name_ptr->counter ++;
	            else
	              {
	                 name_ptr          = alloc_rule_list();
	                 save_word         = add_symbol(name);
	                 name_ptr->name    = save_word->contents;
	                 name_ptr->counter = 1;
	                 name_ptr->next    = cur_fact->deactivate_list_a;
	                 cur_fact->deactivate_list_a = name_ptr;
	              }
	          }
          }
      }

 }
  	
 /*
  * ----------------------------------------------------------------------
  *  void process_rule_fire()
  *        recording all the informations about the rule being fired
  * ----------------------------------------------------------------------
  */
  void process_rule_fire(str)
  char *str;

  {
     int i = 0,j,fire_num;
     char  name[WORDLENGTH];
     ACT_R_PTR rule_ptr;

     j = 0;
     skip_blank(str,&i);
     fire_num = get_num_from_str(str,&i);
     if(first_rule == FALSE)
       {
          if(fire_num > 1)
            {
               error_message(WARNING, "Incomplete trace file!");
               send_message
                  ("\n         The first rule firing in the trace file is not",YES);
               send_message
                  ("\n         FIRE 1. Trace file analysis may be incomplete",YES);
            }
          first_rule = TRUE;
       }
     skip_blank(str,&i);
     while((str[i] != '\n')&&(str[i] != ' '))
       name[j++] = str[i++];
     name[j] = 0;
     if(VERBOSE IS_ON)
       {
           sprintf(msg_buf,"\n\nProcessing rule firing <%.40s>",name);
           send_message(msg_buf,NO);
       }
     rule_ptr = act_rule_head;
       while(rule_ptr != NULL)
       {
          if(strcmp(rule_ptr->name,name) == 0)
             break;
          rule_ptr = rule_ptr->next;
       }
     if(rule_ptr != NULL)
        rule_ptr->fire_num ++;
   }


 /*
  * ----------------------------------------------------------------------
  * void process_misc()
  *     Processing unknown text string from dribble file.  This is for 
  *     the case when users forgot to output  an EOL character.  As a
  *     result, information for an operation may start in the middle of 
  *     a line.
  * ----------------------------------------------------------------------
  */
   
  
void process_misc(str)
char *str;
{
   WHILE (str) DO    
      str = strpbrk (str, "=F<");    

      IF (str EQ NULL) THEN
         break;
      ELSE_IF (strncmp (str, "==> f-", 6) == 0)
         process_rel_activation (&str[6]);
         break;
      ELSE_IF (strncmp (str, "==> Activation ", 15) == 0)
         process_rule_activation (&str[15]);
         break;
      ELSE_IF (strncmp (str, "FIRE ", 5) == 0)
         process_rule_fire (&str[5]);
         break;
      ELSE_IF (strncmp (str, "<== f-", 6) == 0)
         process_rel_deactivation (&str[6]);
         break;
      ELSE_IF (strncmp (str, "<== Activation", 15) == 0)
         process_rule_deactivation (&str[15]);
         break;
      ELSE
         str++;
      END_IF
   DONE
}
