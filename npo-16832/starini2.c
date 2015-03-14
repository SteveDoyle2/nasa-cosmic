




	/*****************************************************************
	 * starini2.c			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Contains part 2 of the initialization code for the STAR
	 *   interpreter.  Due to a constraint imposed by VAX-11C, it has
	 *   been necessary to subdivide the initialization code into
	 *   several files, each file containing definitions for no more
	 *   than 256 global variables.
	 *
	 * The routines and data structures contained in this file may be
	 *   referenced by external routines for a given application, but
	 *   in general should not be altered.  C code for linking
	 *   external routines into the STAR interpreter is located in
	 *   "starlink.c", which is generally modified to suit each
	 *   particular application.
	 *****************************************************************/

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include "stardefs.h"
#include "starcomm.h"


/* I. INITIALIZATION (continued). <><><><><><><><><><><><><><><><><><><><><><>*/

/* EXTERNAL REFERENCES. ------------------------------------------------------*/

extern struct unit_t concept_g;
extern struct unit_t class_g;
extern struct unit_t attribute_g;
extern struct unit_t function_g;
extern struct unit_t variable_g;
extern struct unit_t element_g;
extern struct unit_t rule_g;
extern struct unit_t boolean_g;
extern struct unit_t rule_mode_g;

extern struct unit_t name_g;
extern struct unit_t member_of_g;
extern struct unit_t comment_g;
extern struct unit_t subclass_of_g;
extern struct unit_t members_g;
extern struct unit_t subclasses_g;
extern struct unit_t pattern_g;
extern struct unit_t aspects_g;
extern struct unit_t value_g;
extern struct unit_t default_g;
extern struct unit_t if_needed_g;
extern struct unit_t if_asserted_g;
extern struct unit_t if_retracted_g;
extern struct unit_t side_effects_g;
extern struct unit_t abbreviation_g;
extern struct unit_t n_arguments_g;
extern struct unit_t arguments_g;
extern struct unit_t temporary_g;
extern struct unit_t algorithm_g;
extern struct unit_t bindings_g;
extern struct unit_t mode_g;
extern struct unit_t condition_g;
extern struct unit_t action_g;
extern struct unit_t case_g;
extern struct unit_t result_value_g;
extern struct unit_t break_value_g;
extern struct unit_t skip_value_g;
extern struct unit_t return_value_g;
extern struct unit_t stop_value_g;

extern struct unit_t pound_sign_g;
extern struct unit_t control_g;
extern struct unit_t alternatives_g;

extern struct unit_t true_g;
extern struct unit_t false_g;
extern struct unit_t single_test_g;
extern struct unit_t single_application_g;
extern struct unit_t multiple_application_g;
extern struct unit_t nil_g;

/* CLASSES (continued). ------------------------------------------------------*/

struct string_t rule6_s_actual = {155,
 "     The class of all rules.  Typically subdivided into a\
\nhierarchy of subclasses or rulesets.  A class of rules is\
\nused as the first argument to 'invoke'."};
struct unit_t rule6_s = {STR,0,1,0,1};
struct r_element_t rule6 = {1,&comment_g,&rule6_s,0};
struct list_t rule5_l_actual = {0,0};
struct unit_t rule5_l = {LIS,0,1,0,1};
struct r_element_t rule5 = {1,&subclasses_g,&rule5_l,&rule6};
struct list_t rule4_l_actual = {0,0};
struct unit_t rule4_l = {LIS,0,1,0,1};
struct r_element_t rule4 = {1,&members_g,&rule4_l,&rule5};
struct r_element_t rule3 = {1,&subclass_of_g,&concept_g,&rule4};
struct r_element_t rule2 = {1,&member_of_g,&class_g,&rule3};
struct token_t rule1_t_actual = {4,"RULE"};
struct unit_t rule1_t = {TOK,0,1,0,1};
struct r_element_t rule1 = {1,&name_g,&rule1_t,&rule2};
struct record_t rule_actual = {6,&rule1};

struct string_t bool6_s_actual = {47,
 "The class of boolean values 'true' and 'false'."};
struct unit_t bool6_s = {STR,0,1,0,1};
struct r_element_t bool6 = {1,&comment_g,&bool6_s,0};
struct list_t bool5_l_actual = {0,0};
struct unit_t bool5_l = {LIS,0,1,0,1};
struct r_element_t bool5 = {1,&subclasses_g,&bool5_l,&bool6};
struct l_element_t bool42 = {1,&false_g,0};
struct l_element_t bool41 = {1,&true_g,&bool42};
struct list_t bool4_l_actual = {2,&bool41};
struct unit_t bool4_l = {LIS,0,1,0,1};
struct r_element_t bool4 = {1,&members_g,&bool4_l,&bool5};
struct r_element_t bool3 = {1,&subclass_of_g,&element_g,&bool4};
struct r_element_t bool2 = {1,&member_of_g,&class_g,&bool3};
struct token_t bool1_t_actual = {7,"BOOLEAN"};
struct unit_t bool1_t = {TOK,0,1,0,1};
struct r_element_t bool1 = {1,&name_g,&bool1_t,&bool2};
struct record_t bool_actual = {6,&bool1};

struct string_t rmod6_s_actual = {92,
 "     The class of rule modes 'single_test',\
\n'single_application' and 'multiple_application'."};
struct unit_t rmod6_s = {STR,0,1,0,1};
struct r_element_t rmod6 = {1,&comment_g,&rmod6_s,0};
struct list_t rmod5_l_actual = {0,0};
struct unit_t rmod5_l = {LIS,0,1,0,1};
struct r_element_t rmod5 = {1,&subclasses_g,&rmod5_l,&rmod6};
struct l_element_t rmod43 = {1,&multiple_application_g,0};
struct l_element_t rmod42 = {1,&single_application_g,&rmod43};
struct l_element_t rmod41 = {1,&single_test_g,&rmod42};
struct list_t rmod4_l_actual = {3,&rmod41};
struct unit_t rmod4_l = {LIS,0,1,0,1};
struct r_element_t rmod4 = {1,&members_g,&rmod4_l,&rmod5};
struct r_element_t rmod3 = {1,&subclass_of_g,&element_g,&rmod4};
struct r_element_t rmod2 = {1,&member_of_g,&class_g,&rmod3};
struct token_t rmod1_t_actual = {9,"RULE_MODE"};
struct unit_t rmod1_t = {TOK,0,1,0,1};
struct r_element_t rmod1 = {1,&name_g,&rmod1_t,&rmod2};
struct record_t rmod_actual = {6,&rmod1};

/* ATTRIBUTES. ---------------------------------------------------------------*/

struct string_t name3_s_actual = {146,
 "     The unique name given to a particular RECORD.\
\nAsserting or retracting values for 'name' has automatic\
\nside-effects in the internal directory."};
struct unit_t name3_s = {STR,0,1,0,1};
struct r_element_t name3 = {1,&comment_g,&name3_s,0};
struct r_element_t name2 = {1,&member_of_g,&attribute_g,&name3};
struct token_t name1_t_actual = {4,"NAME"};
struct unit_t name1_t = {TOK,0,1,0,1};
struct r_element_t name1 = {1,&name_g,&name1_t,&name2};
struct record_t name_actual = {3,&name1};

struct string_t meof3_s_actual = {169,
 "     The immediate parent class of a named RECORD.\
\nAsserting or retracting values for 'member_of' has\
\nautomatic side-effects in the 'members' entry for the\
\nparent class."};
struct unit_t meof3_s = {STR,0,1,0,1};
struct r_element_t meof3 = {1,&comment_g,&meof3_s,0};
struct r_element_t meof2 = {1,&member_of_g,&attribute_g,&meof3};
struct token_t meof1_t_actual = {9,"MEMBER_OF"};
struct unit_t meof1_t = {TOK,0,1,0,1};
struct r_element_t meof1 = {1,&name_g,&meof1_t,&meof2};
struct record_t meof_actual = {3,&meof1};

struct string_t comm3_s_actual = {85,
 "     Used to include comments within the definitions of\
\nnamed RECORDs of all classes."};
struct unit_t comm3_s = {STR,0,1,0,1};
struct r_element_t comm3 = {1,&comment_g,&comm3_s,0};
struct r_element_t comm2 = {1,&member_of_g,&attribute_g,&comm3};
struct token_t comm1_t_actual = {7,"COMMENT"};
struct unit_t comm1_t = {TOK,0,1,0,1};
struct r_element_t comm1 = {1,&name_g,&comm1_t,&comm2};
struct record_t comm_actual = {3,&comm1};

struct string_t suof3_s_actual = {186,
 "     The immediate encompassing class of a given\
\nclass.  Asserting or retracting values for 'subclass_of'\
\nhas automatic side-effects in the 'subclasses' entry for\
\nthe encompassing class."};
struct unit_t suof3_s = {STR,0,1,0,1};
struct r_element_t suof3 = {1,&comment_g,&suof3_s,0};
struct r_element_t suof2 = {1,&member_of_g,&attribute_g,&suof3};
struct token_t suof1_t_actual = {11,"SUBCLASS_OF"};
struct unit_t suof1_t = {TOK,0,1,0,1};
struct r_element_t suof1 = {1,&name_g,&suof1_t,&suof2};
struct record_t suof_actual = {3,&suof1};

struct string_t memb3_s_actual = {139,
 "     The immediate members of a given class.\
\nAutomatically updated by changes made to the 'member_of'\
\nentries for individual named RECORDs."};
struct unit_t memb3_s = {STR,0,1,0,1};
struct r_element_t memb3 = {1,&comment_g,&memb3_s,0};
struct r_element_t memb2 = {1,&member_of_g,&attribute_g,&memb3};
struct token_t memb1_t_actual = {7,"MEMBERS"};
struct unit_t memb1_t = {TOK,0,1,0,1};
struct r_element_t memb1 = {1,&name_g,&memb1_t,&memb2};
struct record_t memb_actual = {3,&memb1};

struct string_t subc3_s_actual = {132,
 "     The immediate subclasses of a given class.\
\nAutomatically updated by changes made to the 'subclass_of'\
\nentries of other classes."};
struct unit_t subc3_s = {STR,0,1,0,1};
struct r_element_t subc3 = {1,&comment_g,&subc3_s,0};
struct r_element_t subc2 = {1,&member_of_g,&attribute_g,&subc3};
struct token_t subc1_t_actual = {10,"SUBCLASSES"};
struct unit_t subc1_t = {TOK,0,1,0,1};
struct r_element_t subc1 = {1,&name_g,&subc1_t,&subc2};
struct record_t subc_actual = {3,&subc1};

struct string_t patt3_s_actual = {109,
 "     Used to store values and other aspects of attributes\
\nto be inherited from classes to members of classes."};
struct unit_t patt3_s = {STR,0,1,0,1};
struct r_element_t patt3 = {1,&comment_g,&patt3_s,0};
struct r_element_t patt2 = {1,&member_of_g,&attribute_g,&patt3};
struct token_t patt1_t_actual = {7,"PATTERN"};
struct unit_t patt1_t = {TOK,0,1,0,1};
struct r_element_t patt1 = {1,&name_g,&patt1_t,&patt2};
struct record_t patt_actual = {3,&patt1};

struct string_t aspe3_s_actual = {161,
 "     An attribute used to distinguish a RECORD specifying\
\nvarious aspects of an attribute within a named RECORD,\
\nrather than simply the value for that attribute."};
struct unit_t aspe3_s = {STR,0,1,0,1};
struct r_element_t aspe3 = {1,&comment_g,&aspe3_s,0};
struct r_element_t aspe2 = {1,&member_of_g,&attribute_g,&aspe3};
struct token_t aspe1_t_actual = {7,"ASPECTS"};
struct unit_t aspe1_t = {TOK,0,1,0,1};
struct r_element_t aspe1 = {1,&name_g,&aspe1_t,&aspe2};
struct record_t aspe_actual = {3,&aspe1};

struct string_t valu3_s_actual = {176,
 "     An aspect specifying the value of an attribute for a\
\nparticular named RECORD when other aspects are present.\
\nThe function 'determine' is used to access an inherited\
\nvalue."};
struct unit_t valu3_s = {STR,0,1,0,1};
struct r_element_t valu3 = {1,&comment_g,&valu3_s,0};
struct r_element_t valu2 = {1,&member_of_g,&attribute_g,&valu3};
struct token_t valu1_t_actual = {5,"VALUE"};
struct unit_t valu1_t = {TOK,0,1,0,1};
struct r_element_t valu1 = {1,&name_g,&valu1_t,&valu2};
struct record_t valu_actual = {3,&valu1};

struct string_t defa3_s_actual = {131,
 "     An aspect specifying a UNIT to be returned by the\
\nfunction 'estimate' when applied to a particular\
\nnamed RECORD and attribute."};
struct unit_t defa3_s = {STR,0,1,0,1};
struct r_element_t defa3 = {1,&comment_g,&defa3_s,0};
struct r_element_t defa2 = {1,&member_of_g,&attribute_g,&defa3};
struct token_t defa1_t_actual = {7,"DEFAULT"};
struct unit_t defa1_t = {TOK,0,1,0,1};
struct r_element_t defa1 = {1,&name_g,&defa1_t,&defa2};
struct record_t defa_actual = {3,&defa1};

struct string_t ifne3_s_actual = {146,
 "     An aspect specifying a UNIT to be evaluated and\
\nreturned by the function 'calculate' when applied to a\
\nparticular named RECORD and attribute."};
struct unit_t ifne3_s = {STR,0,1,0,1};
struct r_element_t ifne3 = {1,&comment_g,&ifne3_s,0};
struct r_element_t ifne2 = {1,&member_of_g,&attribute_g,&ifne3};
struct token_t ifne1_t_actual = {9,"IF_NEEDED"};
struct unit_t ifne1_t = {TOK,0,1,0,1};
struct r_element_t ifne1 = {1,&name_g,&ifne1_t,&ifne2};
struct record_t ifne_actual = {3,&ifne1};

struct string_t ifas3_s_actual = {110,
 "     An aspect specifying a LIST of functions to call\
\nwhenever a value for a particular attribute is asserted."};
struct unit_t ifas3_s = {STR,0,1,0,1};
struct r_element_t ifas3 = {1,&comment_g,&ifas3_s,0};
struct r_element_t ifas2 = {1,&member_of_g,&attribute_g,&ifas3};
struct token_t ifas1_t_actual = {11,"IF_ASSERTED"};
struct unit_t ifas1_t = {TOK,0,1,0,1};
struct r_element_t ifas1 = {1,&name_g,&ifas1_t,&ifas2};
struct record_t ifas_actual = {3,&ifas1};

struct string_t ifre3_s_actual = {111,
 "     An aspect specifying a LIST of functions to call\
\nwhenever a value for a particular attribute is retracted."};
struct unit_t ifre3_s = {STR,0,1,0,1};
struct r_element_t ifre3 = {1,&comment_g,&ifre3_s,0};
struct r_element_t ifre2 = {1,&member_of_g,&attribute_g,&ifre3};
struct token_t ifre1_t_actual = {12,"IF_RETRACTED"};
struct unit_t ifre1_t = {TOK,0,1,0,1};
struct r_element_t ifre1 = {1,&name_g,&ifre1_t,&ifre2};
struct record_t ifre_actual = {3,&ifre1};

struct string_t side3_s_actual = {194,
 "     The entry 'side_effects -> true' within the\
\ndefinition of an attribute indicates that one or both of\
\nthe aspects 'if_asserted' and 'if_retracted' are currently\
\nspecified for that attribute."};
struct unit_t side3_s = {STR,0,1,0,1};
struct r_element_t side3 = {1,&comment_g,&side3_s,0};
struct r_element_t side2 = {1,&member_of_g,&attribute_g,&side3};
struct token_t side1_t_actual = {12,"SIDE_EFFECTS"};
struct unit_t side1_t = {TOK,0,1,0,1};
struct r_element_t side1 = {1,&name_g,&side1_t,&side2};
struct record_t side_actual = {3,&side1};

struct string_t abbr3_s_actual = {164,
 "     Used to specify a single character to appear in\
\nabbreviated EXPRESSIONs involving a particular built-in\
\nfunction.  May not be used with user-defined functions."};
struct unit_t abbr3_s = {STR,0,1,0,1};
struct r_element_t abbr3 = {1,&comment_g,&abbr3_s,0};
struct r_element_t abbr2 = {1,&member_of_g,&attribute_g,&abbr3};
struct token_t abbr1_t_actual = {12,"ABBREVIATION"};
struct unit_t abbr1_t = {TOK,0,1,0,1};
struct r_element_t abbr1 = {1,&name_g,&abbr1_t,&abbr2};
struct record_t abbr_actual = {3,&abbr1};

struct string_t narg3_s_actual = {125,
 "     Used to specify the number of UNITs to be sent as\
\narguments to a built-in function or other externally\
\ndefined function."};
struct unit_t narg3_s = {STR,0,1,0,1};
struct r_element_t narg3 = {1,&comment_g,&narg3_s,0};
struct r_element_t narg2 = {1,&member_of_g,&attribute_g,&narg3};
struct token_t narg1_t_actual = {11,"N_ARGUMENTS"};
struct unit_t narg1_t = {TOK,0,1,0,1};
struct r_element_t narg1 = {1,&name_g,&narg1_t,&narg2};
struct record_t narg_actual = {3,&narg1};

struct string_t argu3_s_actual = {86,
 "     Specifies a LIST of variables to be used as arguments\
\nto a user-defined function."};
struct unit_t argu3_s = {STR,0,1,0,1};
struct r_element_t argu3 = {1,&comment_g,&argu3_s,0};
struct r_element_t argu2 = {1,&member_of_g,&attribute_g,&argu3};
struct token_t argu1_t_actual = {9,"ARGUMENTS"};
struct unit_t argu1_t = {TOK,0,1,0,1};
struct r_element_t argu1 = {1,&name_g,&argu1_t,&argu2};
struct record_t argu_actual = {3,&argu1};

struct string_t temp3_s_actual = {110,
 "     Specifies a LIST of variables to be given local\
\nbindings within the execution of a user-defined function."};
struct unit_t temp3_s = {STR,0,1,0,1};
struct r_element_t temp3 = {1,&comment_g,&temp3_s,0};
struct r_element_t temp2 = {1,&member_of_g,&attribute_g,&temp3};
struct token_t temp1_t_actual = {9,"TEMPORARY"};
struct unit_t temp1_t = {TOK,0,1,0,1};
struct r_element_t temp1 = {1,&name_g,&temp1_t,&temp2};
struct record_t temp_actual = {3,&temp1};
