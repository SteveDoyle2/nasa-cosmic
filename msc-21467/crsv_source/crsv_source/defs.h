/*
 ******************************************************
 *   xrefdef.h :  This file defines the basic data    *
 *                structures used in the rule cross   *
 *                reference program. It also defines  *
 *                a number of macros used.            *
 ******************************************************
 */

/*
 * =============================================================
 *                   Basic data structures
 * =============================================================
 */ 

/*
 * --------------------------------------------
 * Hash_entry structure: Used throughout CRSV for
 * storing hash table entries.
 * --------------------------------------------
 */

typedef struct hash_entry
  {
   int                  bucket;
   char                *contents;
   struct hash_entry   *next;
  } HASH, *HASH_PTR;

typedef struct multi_rel_list
  {
   char *rel_name;
   struct multi_rel_list *next_rel;
  }MULT_REL,*MULT_REL_PTR;

/*
 * ------------------------------------------
 * Rule structure: Information about rules
 *   is stored in structures of this type.
 * ------------------------------------------
 */
 
typedef struct rule {
     char          *name;                /* Name of the rule              */
     char          *file;                /* Name of file rule came from   */
     char          *comment;             /* Rule comment                  */
     struct rule   *lft_rule;            /* Pointer to left rule          */
     struct rule   *rht_rule;            /* Pointer to right rule         */
     int            salience_set;        /* Salience value defined?       */
     int            salience_val;        /* Salience value, if set        */
     int            num_patterns;        /* # of patterns on LHS          */
     int            num_actions;         /* # of actions on RHS           */
     int            num_retracts;        /* # of retractions on RHS       */
     int            num_asserts;         /* # of assertions on RHS        */
     int            num_printouts;       /* # of printouts on RHS         */
     int            num_ex_funcs;        /* # of external function calls  */
     int            num_nots;            /* # of not patterns on LHS      */
     int            num_ifs;             /* # of if structures on RHS     */
     int            num_whiles;          /* # of while structures on RHS  */
    } RULE, *RULE_PTR;
      

/*
 * ----------------------------------------------
 * Rule/Relation Structure: These structures
 *  are used to link relations with the rules
 *  they are used in. Each structure points to
 *  the rule the relation is used in and to the
 *  next structure in this link list. It also 
 *  stores the number of the pattern in the list
 *  of patterns on the LHS of a rule.
 * ----------------------------------------------
 */
 
typedef struct rr_list {
     RULE_PTR          rule;               /* Pointer to rule structure */
     struct rr_list   *next_rr;            /* Pointer to next rule in list */
     int               num_fields;         /* Number of fields in relation */
    } *RR_PTR;


/*
 * ----------------------------------------------
 * Fact Block Structure: This structure holds 
 *  information about blocks of facts. For ART
 *  or CLIPS, this is information about deffact
 *  structures.
 * ----------------------------------------------
 */
 
typedef struct fact_block {
     char               *name;            /* Name of the fact block */
     char               *file;            /* Name of file */
     struct fact_block  *next_block;      /* Pointer to next block */
     int                 num_facts;       /* number of facts in block */
    } FACT_BLOCK, *FB_PTR;


/*
 * ----------------------------------------------
 * Fact Block/Relation Structure: These structures
 *  are used to link relations with the fact blocks
 *  they are used in.
 * ----------------------------------------------
 */
 
typedef struct fbr_list {
     FB_PTR             fact_block;   /* Pointer to fact block structure */
     struct fbr_list   *next_fbr;     /* Pointer to next element in list */
     int                num_fields;   /* Number of fields in relation    */
    } *FBR_PTR;


/*
 * ----------------------------------------------
 * Word List Structure: This structure stores
 *    information about a linked list of words
 *    that have been used as literal values.
 * ----------------------------------------------
 */
 
typedef struct word_list {
     struct word_list   *next_word;
     char               *word;
     int                 times_used;
     int                 type;
     } WORD_LIST, *WL_PTR;

/*
 * ----------------------------------------------
 * Number List Structure: This structure stores
 *    information about a linked list of numbers
 *    that have been used as literal values.
 * ----------------------------------------------
 */

typedef struct number_list {
     struct number_list  *next_number;
     float                number;
     int                  times_used;
     } NUM_LIST, *NL_PTR;

/*
 * ----------------------------------------------
 * Field Structure: This structure stores
 *    information about a linked list of fields.
 * ----------------------------------------------
 */

typedef struct field {
     int                field_num;
     struct field      *next_field;
     struct word_list  *lit_values;
    } FIELD, *FLD_PTR;
 
 


/*
 * ----------------------------------------------
 * Relation structure: This structure stores 
 *  information about relations.
 * ----------------------------------------------
 */
 
typedef struct relation {
     char              *name;          /* Name of relation                 */
     struct relation   *lft_rel;       /* Pointer to left relation         */
     struct relation   *rht_rel;       /* Pointer to right relation        */
     RR_PTR             rule_list;     /* Pointer to linked list of rules  */
                                       /* where this relation is used on   */
                                       /* on the LHS                       */
     FBR_PTR            fb_list;       /* Pointer to linked list of fact   */
                                       /* blocks this relation appears in  */
     RR_PTR             retract_list;  /* Pointer to linked list of rules  */
                                       /* where this relation is retracted */
     RR_PTR             assert_list;   /* Pointer to linked list of rules  */
                                       /* where this relation is asserted  */
     FLD_PTR            field_list;    /* Pointer to linked list of fields */
    } RELATION, *REL_PTR;

typedef struct ex_func
{
   char           *name;               /* name of function                 */
   RR_PTR          LHS_rule_list;      /* Pointer to linked list of rules  */
                                       /* where this function is called on */
                                       /* the LHS                          */
   RR_PTR          RHS_rule_list;      /* Pointer to linked list of rules  */
                                       /* where this function is called on */
                                       /* the RHS                          */
   struct ex_func *lft_ex_func;        /* Pointer to left function         */
   struct ex_func *rht_ex_func;        /* Pointer to right function        */
   FLD_PTR         argument_list;      /* Pointer to linked list of arguments */
} EX_FUNC, *EXF_PTR;

/*
 * ------------------------------------------------
 * Variable List Structure: This structure stores
 *    information about a linked list of variables
 *    used on either the LHS or RHS of a rule.
 * ------------------------------------------------
 */
 
typedef struct variable_list {
     struct variable_list   *next_variable;
     struct variable_list   *prev_variable;
     RELATION               *relation;       /*variable is from a relation NULL if not any*/
     EXF_PTR                 ex_func;        /*variable is from an external function NULL if not any */
     struct def_field       *field_info;
     char                   *var;
     int                     level_retracted;
     int                     block_type;     
     int                     count;
     int                     type;
     int                     bound_in_not;
     } VARIABLE_LIST, *VAR_PTR;


 /*
 * -------------------------------------------------
 *  Defrelation structure: This structure contains
 *    information about relation definitions.
 * -------------------------------------------------
 */

typedef struct def_relation {
   char                *name;            /* Relation name */
   char                *file;            /* File it came from */
   struct def_relation *lft_def_rel;     /* Pointer to left defrelation */
   struct def_relation *rht_def_rel;     /* Pointer to right defrelation */
   int                  max_fields;      /* Max number of fields (10000) */
   int                  min_fields;      /* Min number of fields (0) */
   struct def_field    *fields;          /* Pointer to field definitions */
   short                created_by;
   } DEF_REL, *DR_PTR;

/*
 * -------------------------------------------------
 *  Def_field structure: This structure contains
 *    information about field definitions.
 * -------------------------------------------------
 */

typedef struct def_field {
   int                  position;          /* Field position */
   struct def_field    *next_field;        /* next field in list */
   short                allow_word;        /* Allow words? (YES) */
   short                allow_string;      /* Allow strings? (YES) */
   short                allow_number;      /* Allow numbers? (YES) */
   struct word_list    *possible_words;    /* List of allowed words */
                                           /* (NULL is any)   */
   struct word_list    *possible_strings;  /* List of allowed strings */
                                           /* (NULL is any)   */
   struct number_list  *possible_numbers;  /* List of allowed numbers */
                                           /* (NULL is any)   */
   short                set_max;           /* Use max range? (NO) */
   short                set_min;           /* Use min range? (NO) */
   float                max_range;         /* Max numerical value */
   float                min_range;         /* Min numerical value */
   } DEF_FLD,*DF_PTR,DEF_ARG,*DA_PTR;
   


/*
 * ---------------------------------------------------
 *  Defextrnal structure: This structure contains
 *    information about external function definitions.
 * ---------------------------------------------------
 */

typedef struct def_external {
   char                *name;               /* Relation name */
   char                *file;               /* File it came from */
   char                *true_name;          /* True function name */
   short               return_word;         /* return WORD type? (YES) */
   short               return_string;       /* return STRING type? (YES) */
   short               return_number;       /* return NUMBER type? (YES) */
   struct word_list    *assert_list;        /* List of relations asserted by the function*/
   struct word_list    *retract_list;       /* List of relations retracted by the function*/
   struct def_external *lft_def_ext;        /* Pointer to left defrelation */
   struct def_external *rht_def_ext;        /* Pointer to right defrelation */
   int                  max_arguments;      /* Max number of fields (10000) */
   int                  min_arguments;      /* Min number of fields (0) */
   struct def_field    *arguments;          /* Pointer to field definitions */
   } DEF_EXT,*DE_PTR;
   

/* ========================================== *
 *   dribble file stuffs                      *
 * ========================================== */
 

/*
 * --------------------------------------------------
 *  struct rel_list
 *     This structure is used to keep the string which
 *     contains the informations about the relations
 *     that activate a rule.
 * ---------------------------------------------------
 */
 typedef struct rel_list
   {
       char  *activate_string;
       struct rel_list *next;
   }REL_LIST,*REL_LIST_PTR;
   
   
/*
 * --------------------------------------------------
 * Act_Rule structure : This structure is used for
 * keeping information of the rules  in dribble file
 * ---------------------------------------------------
*/
 
 typedef struct act_rule
   {
      char                 *name;
      int                   salience;
      REL_LIST_PTR          activate_list;      /* list of relations that trigger the rule */
      int                   fire_num;           /* Number of time the rule is fired */
      int                   activate_num;       /* Number of time the rule is activated */
      int                   deactivate_num;     /* number of time the rule is deactivated */
      struct act_rule       *next;              /* point to next rule */
   }ACT_RULE,*ACT_R_PTR;


/* ----------------------------------------------
 *  struct compiling_pattern
 *      This structure is used to store all the 
 *      combinations of pattern match(OR pattern)
 *      of each rule.
 * ----------------------------------------------
 */
  
typedef struct compiling_pattern
  {
  
     int                        plus_num;   /* Number of new joins */
     int                        equal_num;  /* Number of old joins */
     struct compiling_pattern  *next;
     
  }COMP_P,*COMP_P_PTR;
  
/* -----------------------------------------------------------------
 * struct compiling_rule
 *       This structure is used to store all the rules
 *       in the system.
 * Note: Currently is not used.
 * -----------------------------------------------------------------
 */ 

typedef struct compiling_rule
  {
     char                     *name;             /* Name of rule            */
     int                       combination_num;  /* Number of ways patterns */
     COMP_P_PTR                comb_list;        /* are constructed         */
     struct compiling_rule    *next;
  }COMP_R,*COMP_R_PTR;

/* ----------------------------------------------------------------
 * struct field_list
 *    This structure is used to store all the value of the fields 
 *    in a relation.
 * ----------------------------------------------------------------
 */  
  
typedef  struct field_list
   {
        char                *word; /* literal value  of the field */  
        int                  num;  /* the order of the field in the relation */
        struct field_list   *next;
   }FIELD_LIST,*FIELD_PTR;
 
 /* --------------------------- */  

 typedef struct rule_name_list
   {
      char *name;
      int counter;                  
      short last_fact;              /* YES/NO */
      struct rule_name_list *next;
   }RULE_NAME,*RULE_NAME_PTR;

/* --------------------------------------------------
 *
 * --------------------------------------------------
 */
 
 typedef struct spe_fact_list
    {
       int             fact_num;         /* fact's number in dribble file */
       int             number_field;     /* Number of fields in a fact */
       FIELD_PTR       field;            /* Pointer pointing to field-list */
       int             num_activated_a;  /* Number of rules activated by the assertion of the fact */
       int             num_deactivated_a;/* Number of rules deactivated by the assertion of the fact */
       RULE_NAME_PTR   activate_list_a;  /* List of rules activated by the assertion of the fact */
       RULE_NAME_PTR   deactivate_list_a;/* List of rules the fact deactivates by being asserted*/
       int             num_deactivated_d;/* Number of rules deactivated by the retraction of the fact*/
       int             num_activated_d;  /* Number of rules activated by the retraction of the fact*/
       RULE_NAME_PTR   activate_list_d;  /* List of rules activated by the retraction of the fact*/
       RULE_NAME_PTR   deactivate_list_d;/* List of rules deactivated by the retraction of the fact */
       short           retracted;        /* flag if fact is retracted or not */
       struct spe_fact_list *next;       /* Next fact */
    }SPE_FACT_LIST,*SPE_FACT_PTR;        
   
/*
 * --------------------------------------------------
 * Act_Relation structure : this structure is used for
 *   keeping track with relations in dribble file.
 * --------------------------------------------------
 */
 
 typedef struct act_rel_str 
   {
     char                    *name;            /* relation name   */
     short                    known;           /* Flag if this relation is recognized */
     int                      current_active;  /* Number of times relation is asserted at a point in time */
     int                      max_occurences;  /* Max times relation is asserted at one point in time */
     SPE_FACT_PTR             spec_fact_list;  /* Specific relation list */
     int                      num_assert;      /* Number of asserting times */
     int                      num_retract;     /* Number of retracting times */
     int                      activate_num;    /* number of rules the relation activates*/
     int                      deactivate_num;  /* number of rules the relation deactivates*/
     struct act_rel_str      *left;            /* points to left relation*/
     struct act_rel_str      *right;           /* points to right relation */
   }ACT_RELATION,*ACT_REL_PTR; 
   


/* ================   End of dribble file stuffs  ================= */

/* ================   Start of deftemplate stuffs  ================= */

/*
 * ----------------------------------------------
 *  Token structure: Used by the newest version
 *     of gettoken (see Clips).  Copied here to
 *     assist in the implementation of an
 *     ungettoken (with multiple unget's
 *     capability).
 * ----------------------------------------------
 */

typedef struct token
  {
   int token;
   float tknnumber;
   char *tknword;
   HASH_PTR hashword;
   char *print_rep;
  } TOKEN, *TK_PTR;
     
typedef struct token_node
  {
   TOKEN *token;
   struct token_node *next;
  } TOKEN_NODE, *TN_PTR;

/*
 * -------------------------------------------------
 *  Def_slot structure: This structure contains
 *    information about slot definitions.
 * -------------------------------------------------
 */

typedef struct def_slot {
   char                *name;              /* Slot name */
   int                  position;          /* Field position */
   struct def_slot     *next_slot;         /* next slot in list */
   int                  max_elements;      /* Max number of elements */
   int                  min_elements;      /* Min number of elements */
   TN_PTR               defaults;          /* Slot default value(s) */
   } DEF_SLOT,*DS_PTR;
   
 /*
  * -------------------------------------------------
  *  Deftemplate structure: This structure contains
  *    information about template definitions.
  * -------------------------------------------------
  */

typedef struct def_template {
   char                *name;            /* Relation name */
   char                *file;            /* File it came from */
   struct def_template *lft_def_tmp;     /* Pointer to left deftemplate */
   struct def_template *rht_def_tmp;     /* Pointer to right deftemplate */
   short		max_slot_pos;	 /* The max. field position assigned
					    to the set of slots. */
   DS_PTR               slots;           /* Pointer to slot definitions */
   } DEF_TMP, *DT_PTR;

typedef struct template {
     char              *name;          /* Name of template                 */
     struct template   *lft_tmp;       /* Pointer to left template         */
     struct template   *rht_tmp;       /* Pointer to right template        */
     RR_PTR             rule_list;     /* Pointer to linked list of rules  */
                                       /* where this template is used on   */
                                       /* on the LHS                       */
     FBR_PTR            fb_list;       /* Pointer to linked list of fact   */
                                       /* blocks this template appears in  */
     RR_PTR             retract_list;  /* Pointer to linked list of rules  */
                                       /* where this template is retracted */
     RR_PTR             assert_list;   /* Pointer to linked list of rules  */
                                       /* where this template is asserted  */
     FLD_PTR            field_list;    /* Pointer to linked list of fields */
    } TEMPLATE, *TMP_PTR;

/* ================   End of deftemplate stuffs  ================= */

/*
 * -------------------------------------------------
 *  Counter structure: This structure is used for
 *     counting items in a few places.
 * -------------------------------------------------
 */

typedef struct counter {
   int   prime_count;
   int   count_A;
   int   count_B;
   int   count_C;
   } COUNTER;
   
   
/*
 * --------------------------------------------
 * Stack structure: used in CRSVSTK.C -- just
 *    your run-of-the-mill push-n-pop stack
 * --------------------------------------------
 */

typedef struct if_stack {
   int level;
   int type;
   int next_level;
   struct if_stack *next;
   } STK_ELT, *STK_PTR;
   

  
/* =============================================================
 *                   General Purpose Macros
 * =============================================================
 */ 

/* ================= Logic Macros ==================== */

#define AND      &&
#define OR       ||
#define EQ       ==
#define NEQ      !=
#define NOT      !

/* ================  Flag Macros  ==================== */

#define TRUE       1
#define FALSE      0
#define NIL        0
#define ON         1
#define OFF        0
#define YES        1
#define NO         0
#define ERROR     -1
#define WARNING   -2
#define OK         0
#define ACTIVATE   1    /* dribble stuffs */
#define DEACTIVATE 0	/* dribble stuffs */
#define RETRACT    2	/* dribble stuffs */

#define IS_ON       == 1
#define IS_OFF      == 0
#define IS_TRUE     == 1
#define IS_FALSE    == 0
#define IS_NIL      == 0
#define IS_YES      == 1
#define IS_NO       == 0
#define IS_ERROR    == -1
#define IS_OK       == 0

/* ===============  Block Coding Constructs  =============== */

#define IF(e)        if(e)
#define THEN         {
#define ELSE_IF(e)   } else if (e) {
#define ELSE         } else {
#define END_IF       }

#define FOR(e)       for(e)
#define END_FOR      }
#define WHILE(e)     while(e)
#define END_WHILE    }
#define SWITCH(e)    switch(e)
#define END_SWITCH   }
#define DO           {
#define DONE         }
#define LOOP         do {
#define UNTIL(e)     } while(!(e))

/* ===============  Character Macros  ======================= */

#define SPACE      ' '
#define TAB        '\t'
#define BACKSPACE  '\b'
#define NEWLINE    '\n'
#define RETURN     '\r'
#define EOS        '\0'

/* ========================  Token constants  ========================== */

#define LNOT         1
#define LAND         2
#define LOR          3
#define COAMP        4
#define SEPARATOR    5
#define BINDER       6
#define MULTIPLE     7
#define LPAREN       8
#define RPAREN       9
#define SINGLE       10

#define KTAND        11
#define KTOR         12
#define KEQ          13

#define BWORD        14
#define BWORDS       15
#define OPERATOR     16
#define KUNKNOWN     17
#define FCALL        18
#define STOP         19
#define PATTERN      20

#define WORD         21
#define POINTER      22
#define NUMBER       23
#define STRING       24

#define EQ_VARS      25
#define NEQ_VARS     26
#define NOTBWORD     27
#define NOTBWORDS    28
#define CONSTANT     29
#define NOTCONSTANT  30
#define NOP          31
#define CUR_ELEM     32
#define GET_VAR      33
#define GET_BIND     34
#define REL          35
#define FUNCTION     36



#define NOT_RETRACTED (-1)
#define OK_RETRACT      0
#define OUTSIDE_IF    (-2)
#define SEPARATE      (-3)
#define SUPERLEVEL    (-4)
#define SUBLEVEL      (-5)
#define COMPANION_IF  (-6)
#define SAME          (-7)

#define _if_    1
#define _else_  2
#define _while_ 3

#if LINT && GENERIC
extern char *sprintf ();
#endif
