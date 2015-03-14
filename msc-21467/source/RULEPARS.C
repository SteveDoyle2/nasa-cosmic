/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                  RULE PARSER MODULE                 */
   /*******************************************************/
  
#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)
 
#include <stdio.h>

#include "constant.h"
#include "scanner.h"
#include "variable.h"
#include "lhsparse.h"
#include "clipsmem.h"
#include "rule.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   char                   *rp_1st_phase();
   struct node            *rp_2nd_phase();
   struct test            *rp_3rd_phase();
   struct internode       *action_link();
   int                     rpl_bword();
   int                     error_alignment();
   
/***************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/
   
   int                     parse_rule();
   int                     load_rules();
   int                     load_from_log_name();
   int                     rtn_ruleinfo();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern int                add_pat_list();
   extern struct node       *rule_body_parse();
   extern struct test       *group_actions();
   extern struct internode  *construct_joins();
   extern struct node       *copy_nodes();
   extern char              *copy_pp_buffer();
   extern struct test       *copy_tests();    
   extern struct funtab     *find_function();
   extern FILE              *fopen();
   extern struct node       *reorder_patterns();
   extern int                pp_backup();
   extern int                save_pp_buffer();
   extern struct var_info   *find_variable();
   extern int                search_bind_list();
   extern struct node       *get_node();
   extern struct pat_node   *network_pointer();
   extern char              *get_pp_buffer();
   extern struct draw       *add_symbol();
   extern char              *symbol_string();
   
   extern char              *strcpy();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct token       inp_tkn;
   static struct ruleinfo   *temp_rule;
   static int                PARSE_ERROR;
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   int                       glo_salience;
   int                       LOAD_FLAG = FALSE;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern int                CONSERVE_SPACE;
    
/*********************************************************************/
/* parse_rule():                                                     */
/*   This function compiles the left and right hand sides of a rule. */
/*********************************************************************/
int parse_rule(read_source)
  char *read_source;
  {
   char *rule_name;
   struct internode *rhs_link;
   struct node *convert_rep, *temp_node;
   struct internode *last_join;
   struct test *actions;
   struct list *temp_list;
   struct pat_node *old_network_pointer;
   struct test *new_actions;

   PARSE_ERROR = FALSE;
   set_pp_buffer_status(ON);

   /*================================================*/
   /* Flush the buffer which stores the pretty print */ 
   /* representation for a rule.  Add the already    */ 
   /* parsed keyword defrule to this buffer.         */
   /*================================================*/
   
   flush_pp_buffer();            
   save_pp_buffer("(defrule ");

   /*=========================================================*/
   /* Rules cannot be loaded when a binary load is in effect. */
   /*=========================================================*/
   
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   if (bloaded() == TRUE) 
     {
      cl_print("werror","\nCannot load defrules with binary load in effect.\n");
      return(TRUE);
     }
#endif

   /*============================================================*/
   /* Parse the name and comment fields of the rule.  Excise the */
   /* rule if it already exists.  This functions sets the values */
   /* of the global variables currentrule and temp_rule.         */
   /*============================================================*/  

   rule_name = rp_1st_phase(read_source);
   if (rule_name == NULL)
    { 
     PARSE_ERROR = TRUE;
     return(PARSE_ERROR); 
    }
    
   temp_rule->name = gm2(strlen(rule_name) + 1);
   strcpy(temp_rule->name,rule_name);
   rule_name = temp_rule->name;

   /*============================*/
   /* Parse the LHS of the rule. */
   /*============================*/

   convert_rep = rp_2nd_phase(read_source);
   if (PARSE_ERROR == TRUE)
     {
      rtn_ruleinfo(temp_rule);
      return(PARSE_ERROR);
     }

   /*============================*/
   /* Parse the RHS of the rule. */
   /*============================*/

   clear_bind_list();
   actions = rp_3rd_phase(read_source);
   
   if (PARSE_ERROR == TRUE)
     {
      rtn_ruleinfo(temp_rule);
      returnnodes(convert_rep); 
      return(PARSE_ERROR);
     }   
   
   /*===========================================================*/
   /* The top level of the construct representing the LHS of a  */
   /* rule is assumed to be an OR.  If the implied OR is at the */
   /* top level of the pattern construct, then remove it.       */
   /*===========================================================*/

   if (convert_rep->type == PAT_OR)
     { 
      temp_node = convert_rep;
      convert_rep = convert_rep->right;
      temp_node->right = NULL;
      temp_node->bottom = NULL;
      returnnodes(temp_node);
     }

   while (convert_rep != NULL)
     {
      if (convert_rep->type == PAT_AND)
        { temp_node = convert_rep->right; }
      else if (convert_rep->type == PATTERN)
        { temp_node = convert_rep; }
      else
        {
         clips_system_error(701);
         cl_exit(4);
        }
 
      if (temp_node->type == PAT_TEST)
        {
         PARSE_ERROR = TRUE;
         cl_print("werror","\nFirst pattern in a rule may not be a test\n");
        }
      else
        { PARSE_ERROR = rule_analysis(temp_node); }
        
      if (PARSE_ERROR == TRUE) 
        {
         remove_rule_network(temp_rule->pats);
         rtn_ruleinfo(temp_rule);
         returnnodes(convert_rep);
         returntests(actions);
         return(PARSE_ERROR);
        }    
        
      /*==================================*/
      /* Determine if the actions are ok. */
      /*==================================*/
      
      new_actions = copy_tests(actions);
      rpl_bword(new_actions);
      if (PARSE_ERROR == TRUE)
        {
         remove_rule_network(temp_rule->pats);
         rtn_ruleinfo(temp_rule);
         returntests(actions);
         returntests(new_actions);
         returnnodes(convert_rep);
         return(PARSE_ERROR);
        }
      
      old_network_pointer = network_pointer();
      last_join = construct_joins(&old_network_pointer,temp_rule);
      set_network_pointer(old_network_pointer);
      
      if (PARSE_ERROR == TRUE) 
        {
         remove_rule_network(temp_rule->pats);
         rtn_ruleinfo(temp_rule);
         returnnodes(convert_rep);
         returntests(actions);
         returntests(new_actions);
         return(PARSE_ERROR);
        }
        
      if (last_join == NULL)
        {
         clips_system_error(702);
         cl_exit(4);
        }

      /*=============================================*/
      /* Connect last join of the rule to link node. */
      /*=============================================*/

      rhs_link = action_link(rule_name,glo_salience);
      
      rhs_link->eval->next_arg->next_arg = new_actions; 
      test_install(rhs_link->eval);
      rhs_link->join_above = last_join;
      rhs_link->entry_pat = NULL; 

      /* Intermediate join link. */
         
    
      temp_list = last_join->next;
      last_join->next = get_struct(list);
      last_join->next->next = temp_list;
      last_join->next->path = rhs_link;
      add_pat_list(NULL,last_join->next,temp_rule); 

      temp_node = convert_rep->bottom;
      convert_rep->bottom = NULL;
      returnnodes(convert_rep);
      convert_rep = temp_node;
     }

   returntests(actions);
   flush_expr_list();
   flush_var_info();
   clear_bind_list();
    
   /*======================================*/
   /* Save the nice printout of the rules. */
   /*======================================*/

   save_pp_buffer("\n");
   if (CONSERVE_SPACE == TRUE)
     { temp_rule->pp_form = NULL; }
   else
     { temp_rule->pp_form = copy_pp_buffer(); } 
   
   /*===============================================*/
   /* Rule completely parsed. Add to list of rules. */
   /*===============================================*/

   add_rule(temp_rule);
   return(0);
  }

/********************************************************************/
/* rp_1st_phase:  Performs the 1st phase of rule parsing.  Gets the */
/*   name and comment fields of a rule. Returns name of rule is no  */
/*   errors are detected, otherwise returns NULL.                   */
/********************************************************************/
static char *rp_1st_phase(read_source)
  char *read_source;
  {
   char *rule_name;
   
   /*============================================*/
   /* Next token should be the name of the rule. */
   /*============================================*/   

   gettoken(read_source,&inp_tkn);                   
   if (inp_tkn.token != WORD)
     { 
      cl_print("werror","\nMissing rule name!\n");
      return(NULL);
     }
     
   rule_name = inp_tkn.tknword;

   /*=======================================================*/
   /* If rule is already in knowledge base, then remove it. */
   /*=======================================================*/

   excise_rule(rule_name);

   /*===============================================================*/
   /* If rules being watched, indicate that rule is being compiled. */
   /*===============================================================*/

   if ((get_compilations_watch() == TRUE) && (LOAD_FLAG == TRUE))
     {
      cl_print("wdialog","Compiling rule: ");
      cl_print("wdialog",rule_name);
      cl_print("wdialog"," ");
     }
   else if (LOAD_FLAG == TRUE)
     { cl_print("wdialog","*"); }

   /*=================================*/
   /* Add rule name to list of rules. */
   /*=================================*/

   temp_rule = get_struct(ruleinfo);
   temp_rule->name = NULL; 
   temp_rule->pp_form = NULL;
   temp_rule->pats = NULL;
   temp_rule->next = NULL;

   /*===========================*/
   /* Get comment if it exists. */
   /*===========================*/

   save_pp_buffer(" ");                 

   gettoken(read_source,&inp_tkn);
   if (inp_tkn.token == STRING)
     {
      save_pp_buffer("\n   ");           
      gettoken(read_source,&inp_tkn);
     }
   else                                 
     {
      pp_backup();
      save_pp_buffer("\n   ");
      save_pp_buffer(inp_tkn.print_rep);
     }

   return(rule_name);
  }

/******************************************************************/
/* rp_2nd_phase:  Performs the 2nd phase of rule parsing.  Parses */
/*   LHS of a rule into an intermediate representation.           */
/******************************************************************/
static struct node *rp_2nd_phase(read_source)
  char *read_source;
  {
   struct node *logicalrepresentation, *convert_rep;

   /*==========================================================*/
   /* Create the logical representation for the left hand side */
   /* of the rule.                                             */
   /*==========================================================*/

   glo_salience = 0;

   set_indent_depth(3);        
   logicalrepresentation = rule_body_parse(read_source,&inp_tkn);

   if (logicalrepresentation == NULL)
     { 
      PARSE_ERROR = TRUE;
      return(NULL);  
     }

   /*======================================================*/
   /* Convert logical representation to new representation */
   /* with only a single top level or.                     */
   /*======================================================*/

   convert_rep = get_node();
   convert_rep->type = PAT_AND;
   convert_rep->right = copy_nodes(logicalrepresentation);

   convert_rep = reorder_patterns(convert_rep);

   returnnodes(logicalrepresentation);

   return(convert_rep);
  }

/******************************************************************/
/* rp_3rd_phase:  Performs the 3rd phase of rule parsing.  Parses */
/*   the RHS of a rule.                                           */
/******************************************************************/
static struct test *rp_3rd_phase(read_source)
  char *read_source;
  {
   struct test *actions;
   struct token end_tkn;

   /*=========================================================*/
   /* Process the actions on the right hand side of the rule. */ 
   /*=========================================================*/

   save_pp_buffer("\n   ");
   set_indent_depth(3);

   actions = group_actions(read_source,&end_tkn,TRUE);

   if (actions == NULL) 
     { 
      PARSE_ERROR = TRUE;
      return(NULL); 
     }
     
   /*=============================*/
   /* Reformat the closing token. */
   /*=============================*/
   
   pp_backup();
   pp_backup();
   save_pp_buffer(end_tkn.print_rep);

   /*======================================================*/
   /* Check for the closing right parenthesis of the rule. */
   /*======================================================*/

   if (end_tkn.token != RPAREN)
     {
      cl_print("werror","\nExpected ')' to finish rule or '(' to begin new action\n");
      PARSE_ERROR = TRUE;
      returntests(actions);
      return(NULL);
     }
     
   return(actions);
  }

/********************************************************************/
/* action_link:  Creates a "link" which will be used to connect the */
/*   join network with the actions of the rule.                     */
/********************************************************************/
static struct internode *action_link(rule_name,salience)
  char *rule_name;
  int salience;
  {
   struct internode *rhs_link;

   /*=======================================================*/
   /* Create link which connects to the actions of the rule */
   /*=======================================================*/

   rhs_link = get_struct(internode);          
   rhs_link->id = 0;
   rhs_link->beta = NULL;
   rhs_link->lhs_log = '+';
   rhs_link->rhs_log = 't';
   rhs_link->eval = NULL;
   rhs_link->not_eval = NULL;
   rhs_link->join_above = NULL;
   rhs_link->entry_pat = NULL;
   rhs_link->next = NULL;

   /*======================================================*/
   /* Store the name of the rule and its salience value in */
   /* the right hand side of the link internode.           */
   /*======================================================*/
 
   rhs_link->eval = get_struct(test);
   rhs_link->eval->type = WORD;
   rhs_link->eval->val.hvalue = add_symbol(rule_name);
   rhs_link->eval->next_arg = get_struct(test);
   rhs_link->eval->arg_list = NULL;
   rhs_link->eval->next_arg->type = INDEX;
   rhs_link->eval->next_arg->val.index = salience;
   rhs_link->eval->next_arg->next_arg = NULL;
   rhs_link->eval->next_arg->arg_list = NULL;

   return(rhs_link);
  }


/*****************************************************************/
/* RPL_BWORD:                                                    */
/*****************************************************************/
static int rpl_bword(list)
  struct test *list;
  {
   while (list != NULL)
     {
      if ((list->type == BWORD) || (list->type == BWORDS))
        { if (replace_variable(list,FALSE) == FALSE) return; }
      else if (list->type == MAY_BE_POINTER)
        { if (replace_variable(list,TRUE) == FALSE) return; }
      else if (list->type == POINTER)
        { if (replace_fact_address(list) == FALSE) return; }
     
#if DEFTEMPLATES
      else if ((list->type == FCALL) && 
               (list->val.fun_ptr == find_function("modify")))
        { 
         if (UpdateModify(list)) 
           { rpl_bword(list->arg_list); }
         else
           {
            PARSE_ERROR = TRUE;
            return;
           }
        }
#endif
      else
        {
         if ((list->type == FCALL) && (list->arg_list != NULL))
           { rpl_bword(list->arg_list); }
        }

      list = list->next_arg;
     }
  }

/***********************************************/
/* replace_fact_address:                            */
/***********************************************/
replace_fact_address(list)
  struct test *list;
  {    
   struct test *arg_lvl;
   int fap;
   
   fap = get_fa_pointer(list->val.hvalue);
   if (fap == 0)
     { 
      cl_print("werror","\nFact address ?");
      cl_print("werror",symbol_string(list->val.hvalue));
      cl_print("werror"," is used in the RHS of the rule,\n");
      cl_print("werror","but not assigned in the LHS of the rule\n");
      PARSE_ERROR = TRUE;
      return(FALSE);
     }
     
   list->type = FCALL;
   list->val.fun_ptr = find_function("(pointer)");
   list->arg_list = get_struct(test);
   arg_lvl = list->arg_list;
   arg_lvl->type = INDEX;
   arg_lvl->val.index = fap;
   arg_lvl->next_arg = NULL;
   arg_lvl->arg_list = NULL;
   
   return(TRUE);
  }

/******************************************/
/* replace_variable:                      */
/******************************************/
replace_variable(list,may_be_pointer)
  struct test *list;
  int may_be_pointer;
  {
   struct draw *var_name;
   struct test *arg_lvl;
   struct var_info *var_ptr1;
   int pat_num, elm_num, fap;
   int bound_also;
   int search_type;
   
   /*===========================================*/
   /* Determine if the variable is single field */
   /* or multi-field variable.                  */
   /*===========================================*/
   
   if (list->type == BWORDS) search_type = BWORDS;
   else search_type = BWORD;
   
   /*=================================*/
   /* Check to see if the variable is */
   /* bound on the LHS of the rule.   */
   /*=================================*/
   
   var_name = list->val.hvalue;
   var_ptr1 = find_variable(search_type,var_name,
                               1,count_joins(),-1,OUTSIDE);
      
   /*=================================*/
   /* Check to see if the variable is */
   /* bound on the RHS of the rule.   */
   /*=================================*/
   
   bound_also = search_bind_list(search_type,var_name);
     
   /*========================================*/
   /* Check also if the variable is bound as */
   /* a fact address on the LHS of the rule. */
   /*========================================*/
     
   if (search_type == BWORD)
     { fap = get_fa_pointer(var_name); }
   else 
     { fap = 0; }
        
   /*==========================================================*/
   /* If variable is not defined on LHS or RHS of rule then... */
   /*==========================================================*/
   
   if ((var_ptr1 == NULL) && (bound_also == FALSE) && (fap == 0))
     {
      cl_print("werror","\nUndefined variable ");
      cl_print("werror",symbol_string(list->val.hvalue));
      cl_print("werror"," referenced on the RHS of the rule\n");  
      PARSE_ERROR = TRUE;
      return(FALSE);
     }
     
   /*=================================================*/
   /* Else if variable is used as both a fact address */
   /* and pattern variable then...                    */
   /*=================================================*/
 
   else if ((fap > 0) && ((bound_also == TRUE) || (var_ptr1 != NULL)))
     {
      cl_print("werror","\nFact address ");
      cl_print("werror",symbol_string(list->val.hvalue));
      cl_print("werror"," also used as variable on the RHS of the rule\n");
      PARSE_ERROR = TRUE;
      return(FALSE);
     }
     
   /*=======================================================*/
   /* Else if variable is a fact address and fact addresses */
   /* can be used by the function then...                   */
   /*=======================================================*/
   
   else if ((fap > 0) && (may_be_pointer == TRUE))
     {    
      list->type = FCALL;
      list->val.fun_ptr = find_function("(pointer)");
      list->arg_list = get_struct(test);
      arg_lvl = list->arg_list;
      arg_lvl->type = INDEX;
      arg_lvl->val.index = fap;
      arg_lvl->next_arg = NULL;
      arg_lvl->arg_list = NULL;
     }
         
   /*=======================================================*/
   /* Else if variable is a fact address and fact addresses */
   /* cannot be used by the function then...                */
   /*=======================================================*/
   
    else if ((fap > 0) && (may_be_pointer == FALSE))
     { 
      cl_print("werror","\nFact address ");
      cl_print("werror",symbol_string(list->val.hvalue));
      cl_print("werror"," improperly referenced on the RHS of the rule\n");
      PARSE_ERROR = TRUE;
      return(FALSE);
     }
     
   /*==================================================*/
   /* Else if variable is bound on the LHS of the rule */
   /* and not rebound on the RHS of the rule then...   */
   /*==================================================*/
   
   else if ((var_ptr1 != NULL) && (bound_also == FALSE))
     {
      list->type = FCALL;
      list->val.fun_ptr = find_function("(get_var)");
      list->arg_list = get_struct(test);
      arg_lvl = list->arg_list;
      arg_lvl->type = INDEX;
      arg_lvl->val.index = var_ptr1->pattern;
      arg_lvl->next_arg = get_struct(test);
      arg_lvl->arg_list = NULL;
      arg_lvl = arg_lvl->next_arg;
      arg_lvl->type = INDEX;
      arg_lvl->val.index = var_ptr1->element;
      arg_lvl->next_arg = NULL;
      arg_lvl->arg_list = NULL;
     }
     
   /*=====================================================*/
   /* Else if variable is rebound on RHS of rule then ... */
   /*=====================================================*/
   
   else if (bound_also == TRUE)
     {
      list->type = FCALL;
      list->val.fun_ptr = find_function("(get_bind)");
      list->arg_list = get_struct(test);
      arg_lvl = list->arg_list;
      arg_lvl->type = WORD;
      arg_lvl->val.hvalue = var_name;
      arg_lvl->arg_list = NULL;
      arg_lvl->next_arg = get_struct(test);
      arg_lvl = arg_lvl->next_arg;

      if (var_ptr1 == NULL)
        {
         pat_num = -1;
         elm_num = -1;
        }
      else
        {
         pat_num = var_ptr1->pattern;
         elm_num = var_ptr1->element;
        } 

      arg_lvl->type = INDEX;
      arg_lvl->val.index = pat_num;
      arg_lvl->arg_list = NULL;
      arg_lvl->next_arg = get_struct(test);
      arg_lvl = arg_lvl->next_arg;
      arg_lvl->type = INDEX;
      arg_lvl->val.index = elm_num;
      arg_lvl->next_arg = NULL;
      arg_lvl->arg_list = NULL;
     }
     
   return(TRUE);
  }
        
/*****************************************************************/
/* load_rules:  Loads the set of defrules and deffacts contained */
/*   within the file specified by file_name.                     */
/*****************************************************************/
int load_rules(file_name)
  char *file_name;
  {
   FILE *file_ptr;

   /*=======================================*/
   /* Open the file specified by file_name. */
   /*=======================================*/
  
   if ((file_ptr = fopen(file_name,"r")) == NULL)
     { return(-1); }
   set_fast_load(file_ptr);

   load_from_log_name("** LoAd FiLe **");

   /*=================*/
   /* Close the file. */
   /*=================*/
   
   fclose(file_ptr);
   set_fast_load(NULL);

   return(0);
     
  }

/*************************************************************/
/* LOAD_FROM_LOG_NAME:                                       */
/*************************************************************/
int load_from_log_name(read_source)
  char *read_source;
  {
   int error_flag, construct_flag;

   /*==================================================*/
   /* Parse the file until the end of file is reached. */
   /*==================================================*/
 
   set_execution_error(FALSE);
   error_flag = FALSE;
   gettoken(read_source,&inp_tkn);

   while ((inp_tkn.token != STOP) && (get_execution_error() == FALSE))
     {
      error_flag = error_alignment(error_flag,read_source);

      if (inp_tkn.token == STOP) { return; }

      if ((inp_tkn.token == WORD) && (error_flag == FALSE))
              
        {
	     construct_flag = parse_construct(inp_tkn.tknword,read_source);
         if (construct_flag == 1)
            {
             cl_print("werror","\nERROR:\n");
             print_in_chunks("werror",get_pp_buffer());
             cl_print("werror","\n");
             error_flag = TRUE;
            }
         else 
           { error_flag = FALSE; }
         flush_pp_buffer();        
        }
 
      gettoken(read_source,&inp_tkn);
     }

   /*==================================================*/
   /* Print a carriage return if *'s and $'s are being */
   /* printed to indicate defrules and deffacts being  */
   /* processed.  Close the file.                      */
   /*==================================================*/
 
   if ((get_compilations_watch() == FALSE) && (LOAD_FLAG == TRUE)) 
     { cl_print("wdialog","\n"); }
  }

/*************************************************************/
/* error_alignment                                           */
/*************************************************************/
static int error_alignment(error_flag,read_source)
  int error_flag;
  char *read_source;
  {

   if (error_flag == FALSE)
     {
      if (inp_tkn.token != LPAREN)
        {
         cl_print("werror","\nExpected left parenthesis to begin ");
         cl_print("werror","defrule or deffacts statement\n");
         error_flag = TRUE;
        }
      else
        {
         gettoken(read_source,&inp_tkn);
         
         if ((inp_tkn.token == WORD) && (valid_construct(inp_tkn.tknword) == 0))
           {
            cl_print("werror","\nFound unrecognized construct ");
            cl_print("werror",inp_tkn.tknword);
            cl_print("werror","\n");
            cl_print("werror","Check that all constructs ");
            cl_print("werror","have the proper number of matching parentheses\n");
            error_flag = TRUE;
           }
         else
           {
            flush_pp_buffer(); 
            return(error_flag);
           }
        }
     }

   /* Error Correction */
   while ((error_flag == TRUE) && (inp_tkn.token != STOP))
     {
      while ((inp_tkn.token != LPAREN) && (inp_tkn.token != STOP))
        { gettoken(read_source,&inp_tkn); }
      if (inp_tkn.token != STOP)
        { 
         gettoken(read_source,&inp_tkn);
         if ((inp_tkn.token == WORD) && (valid_construct(inp_tkn.tknword) == 1))
           {
            flush_pp_buffer(); 
            return(FALSE);
           }
        }
     }
   if (inp_tkn.token == STOP) { return(-1); }
   return(error_flag);
  }
  
/***************************************/
/* RTN_RULEINFO:          */
/***************************************/
rtn_ruleinfo(waste)
  struct ruleinfo *waste;
  {
   if (waste == NULL) { return(0); }

   if (waste->name != NULL)
     { rm(waste->name,strlen(waste->name) + 1); }
   
   if (waste->pp_form != NULL)
     { rm(waste->pp_form,strlen(waste->pp_form) + 1); }
     
   rtn_struct(ruleinfo,waste);
   return(1);
  }

/******************************************/
/* INIT_CONSTRUCTS:                       */
/******************************************/
init_constructs()
  {
   extern int parse_rule();
   
#if ART_CONSTRUCTS
   extern int parse_art();
   
   add_construct("def-viewpoint-levels",parse_art);
   add_construct("def-named-viewpoints",parse_art);
   add_construct("defschema",parse_art);
   add_construct("defcontradiction",parse_art);
   add_construct("defrelation",parse_art);
   add_construct("defexternal",parse_art);
   add_construct("defglobal",parse_art);
#endif
   
   add_construct("defrule",parse_rule);
  }
   
#if ART_CONSTRUCTS

/***************************************************************/
/* PARSE_ART:                                   */
/***************************************************************/
int parse_art(read_source)
  char *read_source;
  {
   int depth = 1;
   struct token art_tkn;
   
   set_pp_buffer_status(OFF);
   flush_pp_buffer(); 
   
   /*===============================================================*/
   /* If watch rules is on, indicate art construct being processed. */
   /*===============================================================*/

   if ((get_compilations_watch() == ON) && (LOAD_FLAG == TRUE))
     { cl_print("wdialog","Warning: ART construct not supported\n"); }
   else if (LOAD_FLAG == TRUE)
     { cl_print("wdialog","?"); }

   /*================================*/
   /* Check for closing parenthesis. */
   /*================================*/
   
   while (depth != 0)
     {   
      gettoken(read_source,&art_tkn);
      if (art_tkn.token == LPAREN)
        { depth++; }
      else if (art_tkn.token == RPAREN)
        { depth--; }
     } 
   
   return(0); 
  }
 
#endif

#endif
