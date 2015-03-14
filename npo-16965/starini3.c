




	/*****************************************************************
	 * starini3.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains part 3 of the initialization code for the STAR
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

/* ATTRIBUTES (continued). ---------------------------------------------------*/

struct string_t algo3_s_actual = {167,
 "     Used to specify either a CONNECTION to an externally\
\ndefined function or a LIST of UNITs defining the steps to\
\nbe taken in the execution of a particular function."};
struct unit_t algo3_s = {STR,0,1,0,1};
struct r_element_t algo3 = {1,&comment_g,&algo3_s,0};
struct r_element_t algo2 = {1,&member_of_g,&attribute_g,&algo3};
struct token_t algo1_t_actual = {9,"ALGORITHM"};
struct unit_t algo1_t = {TOK,0,1,0,1};
struct r_element_t algo1 = {1,&name_g,&algo1_t,&algo2};
struct record_t algo_actual = {3,&algo1};

struct string_t bind3_s_actual = {247,
 "     Used in the definition of a variable to specify a\
\nLIST of bindings.  The first element in this LIST is the\
\ncurrent value, accessible through the use of the 'dot'\
\nfunction.  Any other bindings are shadowed as a result of\
\ndynamic scoping rules."};
struct unit_t bind3_s = {STR,0,1,0,1};
struct r_element_t bind3 = {1,&comment_g,&bind3_s,0};
struct r_element_t bind2 = {1,&member_of_g,&attribute_g,&bind3};
struct token_t bind1_t_actual = {8,"BINDINGS"};
struct unit_t bind1_t = {TOK,0,1,0,1};
struct r_element_t bind1 = {1,&name_g,&bind1_t,&bind2};
struct record_t bind_actual = {3,&bind1};

struct string_t mode3_s_actual = {108,
 "     Specifies the mode assigned to a rule as\
\n'single-test', 'single-application' or\
\n'multiple-application'."};
struct unit_t mode3_s = {STR,0,1,0,1};
struct r_element_t mode3 = {1,&comment_g,&mode3_s,0};
struct r_element_t mode2 = {1,&member_of_g,&attribute_g,&mode3};
struct token_t mode1_t_actual = {4,"MODE"};
struct unit_t mode1_t = {TOK,0,1,0,1};
struct r_element_t mode1 = {1,&name_g,&mode1_t,&mode2};
struct record_t mode_actual = {3,&mode1};

struct string_t cond3_s_actual = {34,
 "Specifies the condition of a rule."};
struct unit_t cond3_s = {STR,0,1,0,1};
struct r_element_t cond3 = {1,&comment_g,&cond3_s,0};
struct r_element_t cond2 = {1,&member_of_g,&attribute_g,&cond3};
struct token_t cond1_t_actual = {9,"CONDITION"};
struct unit_t cond1_t = {TOK,0,1,0,1};
struct r_element_t cond1 = {1,&name_g,&cond1_t,&cond2};
struct record_t cond_actual = {3,&cond1};

struct string_t acti3_s_actual = {151,
 "     Specifies the action of a rule.  Also used in the\
\ncontext of the 'branch' function to specify the action\
\nassociated with a particular alternative."};
struct unit_t acti3_s = {STR,0,1,0,1};
struct r_element_t acti3 = {1,&comment_g,&acti3_s,0};
struct r_element_t acti2 = {1,&member_of_g,&attribute_g,&acti3};
struct token_t acti1_t_actual = {6,"ACTION"};
struct unit_t acti1_t = {TOK,0,1,0,1};
struct r_element_t acti1 = {1,&name_g,&acti1_t,&acti2};
struct record_t acti_actual = {3,&acti1};

struct string_t case3_s_actual = {107,
 "     Used in the context of the 'branch' function to\
\nspecify one of the possible alternatives for matching."};
struct unit_t case3_s = {STR,0,1,0,1};
struct r_element_t case3 = {1,&comment_g,&case3_s,0};
struct r_element_t case2 = {1,&member_of_g,&attribute_g,&case3};
struct token_t case1_t_actual = {4,"CASE"};
struct unit_t case1_t = {TOK,0,1,0,1};
struct r_element_t case1 = {1,&name_g,&case1_t,&case2};
struct record_t case_actual = {3,&case1};

struct string_t resu3_s_actual = {91,
 "     Used by the function 'result' to indicate a value to\
\nbe returned by the 'do' function."};
struct unit_t resu3_s = {STR,0,1,0,1};
struct r_element_t resu3 = {1,&comment_g,&resu3_s,0};
struct r_element_t resu2 = {1,&member_of_g,&attribute_g,&resu3};
struct token_t resu1_t_actual = {12,"RESULT_VALUE"};
struct unit_t resu1_t = {TOK,0,1,0,1};
struct r_element_t resu1 = {1,&name_g,&resu1_t,&resu2};
struct record_t resu_actual = {3,&resu1};

struct string_t brea3_s_actual = {110,
 "     Used by the function 'break' to indicate a value to\
\nbe returned by 'repeat', 'while', 'for' or 'through'."};
struct unit_t brea3_s = {STR,0,1,0,1};
struct r_element_t brea3 = {1,&comment_g,&brea3_s,0};
struct r_element_t brea2 = {1,&member_of_g,&attribute_g,&brea3};
struct token_t brea1_t_actual = {11,"BREAK_VALUE"};
struct unit_t brea1_t = {TOK,0,1,0,1};
struct r_element_t brea1 = {1,&name_g,&brea1_t,&brea2};
struct record_t brea_actual = {3,&brea1};

struct string_t skip3_s_actual = {141,
 "     Used by the function 'skip' to indicate an immediate\
\njump to the next iteration for the functions 'repeat',\
\n'while', 'for' or 'through'."};
struct unit_t skip3_s = {STR,0,1,0,1};
struct r_element_t skip3 = {1,&comment_g,&skip3_s,0};
struct r_element_t skip2 = {1,&member_of_g,&attribute_g,&skip3};
struct token_t skip1_t_actual = {10,"SKIP_VALUE"};
struct unit_t skip1_t = {TOK,0,1,0,1};
struct r_element_t skip1 = {1,&name_g,&skip1_t,&skip2};
struct record_t skip_actual = {3,&skip1};

struct string_t retu3_s_actual = {98,
 "     Used by the function 'return' to specify the value to\
\nbe returned by a user-defined function."};
struct unit_t retu3_s = {STR,0,1,0,1};
struct r_element_t retu3 = {1,&comment_g,&retu3_s,0};
struct r_element_t retu2 = {1,&member_of_g,&attribute_g,&retu3};
struct token_t retu1_t_actual = {12,"RETURN_VALUE"};
struct unit_t retu1_t = {TOK,0,1,0,1};
struct r_element_t retu1 = {1,&name_g,&retu1_t,&retu2};
struct record_t retu_actual = {3,&retu1};

struct string_t stop3_s_actual = {140,
 "     Used by the function 'stop' to specify an immediate\
\nhalt to rule-based operation, plus a value to be returned\
\nby the 'invoke' function."};
struct unit_t stop3_s = {STR,0,1,0,1};
struct r_element_t stop3 = {1,&comment_g,&stop3_s,0};
struct r_element_t stop2 = {1,&member_of_g,&attribute_g,&stop3};
struct token_t stop1_t_actual = {10,"STOP_VALUE"};
struct unit_t stop1_t = {TOK,0,1,0,1};
struct r_element_t stop1 = {1,&name_g,&stop1_t,&stop2};
struct record_t stop_actual = {3,&stop1};

/* VARIABLES. ----------------------------------------------------------------*/

struct l_element_t poun41 = {1,&nil_g,0};
struct list_t poun4_l_actual = {1,&poun41};
struct unit_t poun4_l = {LIS,0,1,0,1};
struct r_element_t poun4 = {1,&bindings_g,&poun4_l,0};
struct string_t poun3_s_actual = {175,
 "     Used to store the last UNIT returned by the STAR\
\ninterpreter in the user cycle.  The value may be retrieved\
\nas '.pound_sign' or by using the special escape character\
\n'#'."};
struct unit_t poun3_s = {STR,0,1,0,1};
struct r_element_t poun3 = {1,&comment_g,&poun3_s,&poun4};
struct r_element_t poun2 = {1,&member_of_g,&variable_g,&poun3};
struct token_t poun1_t_actual = {10,"POUND_SIGN"};
struct unit_t poun1_t = {TOK,0,1,0,1};
struct r_element_t poun1 = {1,&name_g,&poun1_t,&poun2};
struct record_t poun_actual = {4,&poun1};

struct list_t cont4_l_actual = {0,0};
struct unit_t cont4_l = {LIS,0,1,0,1};
struct r_element_t cont4 = {1,&bindings_g,&cont4_l,0};
struct string_t cont3_s_actual = {127,
 "     Used to store the current ruleset in use by the\
\nfunction 'invoke'.  The value may be accessed but not\
\naltered by the user."};
struct unit_t cont3_s = {STR,0,1,0,1};
struct r_element_t cont3 = {1,&comment_g,&cont3_s,&cont4};
struct r_element_t cont2 = {1,&member_of_g,&variable_g,&cont3};
struct token_t cont1_t_actual = {7,"CONTROL"};
struct unit_t cont1_t = {TOK,0,1,0,1};
struct r_element_t cont1 = {1,&name_g,&cont1_t,&cont2};
struct record_t cont_actual = {4,&cont1};

struct list_t alte4_l_actual = {0,0};
struct unit_t alte4_l = {LIS,0,1,0,1};
struct r_element_t alte4 = {1,&bindings_g,&alte4_l,0};
struct string_t alte3_s_actual = {138,
 "     Used to store the currently active members of the\
\nruleset in use by 'invoke'.  The value may be accessed but\
\nnot altered by the user."};
struct unit_t alte3_s = {STR,0,1,0,1};
struct r_element_t alte3 = {1,&comment_g,&alte3_s,&alte4};
struct r_element_t alte2 = {1,&member_of_g,&variable_g,&alte3};
struct token_t alte1_t_actual = {12,"ALTERNATIVES"};
struct unit_t alte1_t = {TOK,0,1,0,1};
struct r_element_t alte1 = {1,&name_g,&alte1_t,&alte2};
struct record_t alte_actual = {4,&alte1};

/* ELEMENTS. -----------------------------------------------------------------*/

struct r_element_t true2 = {1,&member_of_g,&boolean_g,0};
struct token_t true1_t_actual = {4,"TRUE"};
struct unit_t true1_t = {TOK,0,1,0,1};
struct r_element_t true1 = {1,&name_g,&true1_t,&true2};
struct record_t true_actual = {2,&true1};

struct r_element_t fals2 = {1,&member_of_g,&boolean_g,0};
struct token_t fals1_t_actual = {5,"FALSE"};
struct unit_t fals1_t = {TOK,0,1,0,1};
struct r_element_t fals1 = {1,&name_g,&fals1_t,&fals2};
struct record_t fals_actual = {2,&fals1};

struct string_t stes3_s_actual = {119,
 "     Specifies that a rule is to be tested only once,\
\npossibly applied, and then removed from the LIST of active\
\nrules."};
struct unit_t stes3_s = {STR,0,1,0,1};
struct r_element_t stes3 = {1,&comment_g,&stes3_s,0};
struct r_element_t stes2 = {1,&member_of_g,&rule_mode_g,&stes3};
struct token_t stes1_t_actual = {11,"SINGLE_TEST"};
struct unit_t stes1_t = {TOK,0,1,0,1};
struct r_element_t stes1 = {1,&name_g,&stes1_t,&stes2};
struct record_t stes_actual = {3,&stes1};

struct string_t sapp3_s_actual = {153,
 "     Specifies that a rule may be tested any number of\
\ntimes, but must be removed from the LIST of active rules\
\ndirectly following its first application."};
struct unit_t sapp3_s = {STR,0,1,0,1};
struct r_element_t sapp3 = {1,&comment_g,&sapp3_s,0};
struct r_element_t sapp2 = {1,&member_of_g,&rule_mode_g,&sapp3};
struct token_t sapp1_t_actual = {18,"SINGLE_APPLICATION"};
struct unit_t sapp1_t = {TOK,0,1,0,1};
struct r_element_t sapp1 = {1,&name_g,&sapp1_t,&sapp2};
struct record_t sapp_actual = {3,&sapp1};

struct string_t mapp3_s_actual = {125,
 "     Specifies that a rule may be tested and applied any\
\nnumber of times without being removed from the LIST of\
\nactive rules."};
struct unit_t mapp3_s = {STR,0,1,0,1};
struct r_element_t mapp3 = {1,&comment_g,&mapp3_s,0};
struct r_element_t mapp2 = {1,&member_of_g,&rule_mode_g,&mapp3};
struct token_t mapp1_t_actual = {20,"MULTIPLE_APPLICATION"};
struct unit_t mapp1_t = {TOK,0,1,0,1};
struct r_element_t mapp1 = {1,&name_g,&mapp1_t,&mapp2};
struct record_t mapp_actual = {3,&mapp1};

struct r_element_t niln2 = {1,&member_of_g,&element_g,0};
struct token_t niln1_t_actual = {3,"NIL"};
struct unit_t niln1_t = {TOK,0,1,0,1};
struct r_element_t niln1 = {1,&name_g,&niln1_t,&niln2};
struct record_t niln_actual = {2,&niln1};
