




	/*****************************************************************
	 * starini1.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains part 1 of the initialization code for the STAR
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


/* I. INITIALIZATION. <><><><><><><><><><><><><><><><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * This section contains C data structures and an initialization
	 *   function which combine to set up the fixed portion of the
	 *   STAR knowledge base.  Most of the initialization is com-
	 *   pleted at compile time, but two remaining aspects are de-
	 *   layed until the beginning of a session with STAR.
	 *
	 * Since C does not allow "unions" to be initialized, it is ne-
	 *   cessary to link the "unit_t" data structures with their
	 *   associated contents ("number_t", "token_t" data structures,
	 *   etc.) at run time.  This is accomplished by the
	 *   "initialize_s" function based on the tables "t_init_g",
	 *   "s_init_g", "l_init_g" and "r_init_g", which contain the
	 *   pairs to be united.
	 *
	 * The other operation performed at run time is the setting up of
	 *   STAR named RECORDs defining the built-in commands of STAR
	 *   and all externally-defined functions pertaining to a
	 *   particular application.  The table "bif_init_g" contains the
	 *   information necessary for the initialization of built-in
	 *   functions.  Information for the initialization of
	 *   externally-defined functions is provided in the file
	 *   "starlink.c".
	 *****************************************************************/

/* NAMED RECORDS. ------------------------------------------------------------*/

	/*****************************************************************
	 * The following global variables implement piece by piece the
	 *   data structures which embody the initialized portion of the
	 *   STAR knowledge base, minus the built-in and external
	 *   functions.  The named RECORDs implemented here are thus
	 *   accessible in the rest of the STAR code as "concept_g" for
	 *   "concept", "class_g" for "class" and so forth.  As these are
	 *   the actual "unit_t" data structures and not pointers to
	 *   such, they are often referenced as "&concept_g", etc. in the
	 *   code.
	 *****************************************************************/

	/*****************************************************************
	 * Each entry immediately below specifies five values for the
	 *   initialization of a built-in named RECORD:
	 *
	 *	(1) the type "REC",
	 *	(2) "1" indicating a named RECORD,
	 *	(3) "1" indicating a built-in UNIT,
	 *	(4) "0" (= "BY_VALUE") default for argum field,
	 *	(5) the initial count of containment in other UNITs.
	 *****************************************************************/

struct unit_t concept_g = {REC,1,1,0,8};
struct unit_t class_g = {REC,1,1,0,12};
struct unit_t attribute_g = {REC,1,1,0,32};
struct unit_t function_g = {REC,1,1,0,3};
struct unit_t variable_g = {REC,1,1,0,6};
struct unit_t element_g = {REC,1,1,0,6};
struct unit_t rule_g = {REC,1,1,0,3};
struct unit_t boolean_g = {REC,1,1,0,5};
struct unit_t rule_mode_g = {REC,1,1,0,6};

struct unit_t name_g = {REC,1,1,0,49};
struct unit_t member_of_g = {REC,1,1,0,49};
struct unit_t comment_g = {REC,1,1,0,46};
struct unit_t subclass_of_g = {REC,1,1,0,11};
struct unit_t members_g = {REC,1,1,0,11};
struct unit_t subclasses_g = {REC,1,1,0,11};
struct unit_t pattern_g = {REC,1,1,0,2};
struct unit_t aspects_g = {REC,1,1,0,2};
struct unit_t value_g  = {REC,1,1,0,2};
struct unit_t default_g = {REC,1,1,0,2};
struct unit_t if_needed_g = {REC,1,1,0,2};
struct unit_t if_asserted_g = {REC,1,1,0,2};
struct unit_t if_retracted_g = {REC,1,1,0,2};
struct unit_t side_effects_g = {REC,1,1,0,2};
struct unit_t abbreviation_g = {REC,1,1,0,2};
struct unit_t n_arguments_g = {REC,1,1,0,2};
struct unit_t arguments_g = {REC,1,1,0,2};
struct unit_t temporary_g = {REC,1,1,0,2};
struct unit_t algorithm_g = {REC,1,1,0,2};
struct unit_t bindings_g = {REC,1,1,0,5};
struct unit_t mode_g = {REC,1,1,0,2};
struct unit_t condition_g = {REC,1,1,0,2};
struct unit_t action_g = {REC,1,1,0,2};
struct unit_t case_g = {REC,1,1,0,2};
struct unit_t result_value_g = {REC,1,1,0,2};
struct unit_t break_value_g = {REC,1,1,0,2};
struct unit_t skip_value_g = {REC,1,1,0,2};
struct unit_t return_value_g = {REC,1,1,0,2};
struct unit_t stop_value_g = {REC,1,1,0,2};

struct unit_t pound_sign_g = {REC,1,1,0,2};
struct unit_t control_g = {REC,1,1,0,2};
struct unit_t alternatives_g = {REC,1,1,0,2};

struct unit_t true_g = {REC,1,1,0,2};
struct unit_t false_g = {REC,1,1,0,2};
struct unit_t single_test_g = {REC,1,1,0,2};
struct unit_t single_application_g = {REC,1,1,0,2};
struct unit_t multiple_application_g = {REC,1,1,0,2};
struct unit_t nil_g = {REC,1,1,0,4};

	/*****************************************************************
	 * The remaining initialization code sets up definitions for
	 *   individual built-in classes, attributes, variables and
	 *   elements.
	 *****************************************************************/

/* CLASSES. ------------------------------------------------------------------*/

struct string_t conc6_s_actual = {182,
 "     The top or universal class.  All named RECORDs are\
\nmembers of 'concept'.  The six major subclasses of\
\n'concept' partition the set of named RECORDs according\
\nto primary function."};
struct unit_t conc6_s = {STR,0,1,0,1};
struct r_element_t conc6 = {1,&comment_g,&conc6_s,0};
struct l_element_t conc56 = {1,&rule_g,0};
struct l_element_t conc55 = {1,&element_g,&conc56};
struct l_element_t conc54 = {1,&variable_g,&conc55};
struct l_element_t conc53 = {1,&function_g,&conc54};
struct l_element_t conc52 = {1,&attribute_g,&conc53};
struct l_element_t conc51 = {1,&class_g,&conc52};
struct list_t conc5_l_actual = {6,&conc51};
struct unit_t conc5_l = {LIS,0,1,0,1};
struct r_element_t conc5 = {1,&subclasses_g,&conc5_l,&conc6};
struct list_t conc4_l_actual = {0,0};
struct unit_t conc4_l = {LIS,0,1,0,1};
struct r_element_t conc4 = {1,&members_g,&conc4_l,&conc5};
struct r_element_t conc3 = {1,&subclass_of_g,&nil_g,&conc4};
struct r_element_t conc2 = {1,&member_of_g,&class_g,&conc3};
struct token_t conc1_t_actual = {7,"CONCEPT"};
struct unit_t conc1_t = {TOK,0,1,0,1};
struct r_element_t conc1 = {1,&name_g,&conc1_t,&conc2};
struct record_t conc_actual = {6,&conc1};

struct string_t clas6_s_actual = {188,
 "     The class of all classes.  Classes are listed as\
\nmembers of 'class' or its subclasses in addition to being\
\norganized into a hierarchy by the attributes 'subclass_of'\
\nand 'subclasses'."};
struct unit_t clas6_s = {STR,0,1,0,1};
struct r_element_t clas6 = {1,&comment_g,&clas6_s,0};
struct list_t clas5_l_actual = {0,0};
struct unit_t clas5_l = {LIS,0,1,0,1};
struct r_element_t clas5 = {1,&subclasses_g,&clas5_l,&clas6};
struct l_element_t clas49 = {1,&rule_mode_g,0};
struct l_element_t clas48 = {1,&boolean_g,&clas49};
struct l_element_t clas47 = {1,&rule_g,&clas48};
struct l_element_t clas46 = {1,&element_g,&clas47};
struct l_element_t clas45 = {1,&variable_g,&clas46};
struct l_element_t clas44 = {1,&function_g,&clas45};
struct l_element_t clas43 = {1,&attribute_g,&clas44};
struct l_element_t clas42 = {1,&class_g,&clas43};
struct l_element_t clas41 = {1,&concept_g,&clas42};
struct list_t clas4_l_actual = {9,&clas41};
struct unit_t clas4_l = {LIS,0,1,0,1};
struct r_element_t clas4 = {1,&members_g,&clas4_l,&clas5};
struct r_element_t clas3 = {1,&subclass_of_g,&concept_g,&clas4};
struct r_element_t clas2 = {1,&member_of_g,&class_g,&clas3};
struct token_t clas1_t_actual = {5,"CLASS"};
struct unit_t clas1_t = {TOK,0,1,0,1};
struct r_element_t clas1 = {1,&name_g,&clas1_t,&clas2};
struct record_t clas_actual = {6,&clas1};

struct string_t attr6_s_actual = {249,
 "     The class of all attributes.  Attributes are used as\
\nkeys to the entries in RECORDs, appearing as the lefthand\
\nmember of each entry pair.  Automatic side-effects may be\
\ngenerated when a value for a particular attribute is\
\nasserted or retracted."};
struct unit_t attr6_s = {STR,0,1,0,1};
struct r_element_t attr6 = {1,&comment_g,&attr6_s,0};
struct list_t attr5_l_actual = {0,0};
struct unit_t attr5_l = {LIS,0,1,0,1};
struct r_element_t attr5 = {1,&subclasses_g,&attr5_l,&attr6};
struct l_element_t attr429 = {1,&stop_value_g,0};
struct l_element_t attr428 = {1,&return_value_g,&attr429};
struct l_element_t attr427 = {1,&skip_value_g,&attr428};
struct l_element_t attr426 = {1,&break_value_g,&attr427};
struct l_element_t attr425 = {1,&result_value_g,&attr426};
struct l_element_t attr424 = {1,&case_g,&attr425};
struct l_element_t attr423 = {1,&action_g,&attr424};
struct l_element_t attr422 = {1,&condition_g,&attr423};
struct l_element_t attr421 = {1,&mode_g,&attr422};
struct l_element_t attr420 = {1,&bindings_g,&attr421};
struct l_element_t attr419 = {1,&algorithm_g,&attr420};
struct l_element_t attr418 = {1,&temporary_g,&attr419};
struct l_element_t attr417 = {1,&arguments_g,&attr418};
struct l_element_t attr416 = {1,&n_arguments_g,&attr417};
struct l_element_t attr415 = {1,&abbreviation_g,&attr416};
struct l_element_t attr414 = {1,&side_effects_g,&attr415};
struct l_element_t attr413 = {1,&if_retracted_g,&attr414};
struct l_element_t attr412 = {1,&if_asserted_g,&attr413};
struct l_element_t attr411 = {1,&if_needed_g,&attr412};
struct l_element_t attr410 = {1,&default_g,&attr411};
struct l_element_t attr49 = {1,&value_g,&attr410};
struct l_element_t attr48 = {1,&aspects_g,&attr49};
struct l_element_t attr47 = {1,&pattern_g,&attr48};
struct l_element_t attr46 = {1,&subclasses_g,&attr47};
struct l_element_t attr45 = {1,&members_g,&attr46};
struct l_element_t attr44 = {1,&subclass_of_g,&attr45};
struct l_element_t attr43 = {1,&comment_g,&attr44};
struct l_element_t attr42 = {1,&member_of_g,&attr43};
struct l_element_t attr41 = {1,&name_g,&attr42};
struct list_t attr4_l_actual = {29,&attr41};
struct unit_t attr4_l = {LIS,0,1,0,1};
struct r_element_t attr4 = {1,&members_g,&attr4_l,&attr5};
struct r_element_t attr3 = {1,&subclass_of_g,&concept_g,&attr4};
struct r_element_t attr2 = {1,&member_of_g,&class_g,&attr3};
struct token_t attr1_t_actual = {9,"ATTRIBUTE"};
struct unit_t attr1_t = {TOK,0,1,0,1};
struct r_element_t attr1 = {1,&name_g,&attr1_t,&attr2};
struct record_t attr_actual = {6,&attr1};

struct string_t func6_s_actual = {244,
 "     The class of all functions.  Contains as members all\
\nbuilt-in functions of STAR.  Also, external functions and\
\nuser-defined functions may appear either as direct members\
\nof 'function' or as members of user-defined subclasses of\
\n'function'."};
struct unit_t func6_s = {STR,0,1,0,1};
struct r_element_t func6 = {1,&comment_g,&func6_s,0};
struct list_t func5_l_actual = {0,0};
struct unit_t func5_l = {LIS,0,1,0,1};
struct r_element_t func5 = {1,&subclasses_g,&func5_l,&func6};
struct list_t func4_l_actual = {0,0};
struct unit_t func4_l = {LIS,0,1,0,1};
struct r_element_t func4 = {1,&members_g,&func4_l,&func5};
struct r_element_t func3 = {1,&subclass_of_g,&concept_g,&func4};
struct r_element_t func2 = {1,&member_of_g,&class_g,&func3};
struct token_t func1_t_actual = {8,"FUNCTION"};
struct unit_t func1_t = {TOK,0,1,0,1};
struct r_element_t func1 = {1,&name_g,&func1_t,&func2};
struct record_t func_actual = {6,&func1};

struct string_t vari6_s_actual = {215,
 "     The class of all variables.  Contains as direct\
\nmembers the built-in variables of STAR.  User-defined\
\nvariables may appear as direct members of 'variable' or\
\nas members of user-defined subclasses of 'variable'."};
struct unit_t vari6_s = {STR,0,1,0,1};
struct r_element_t vari6 = {1,&comment_g,&vari6_s,0};
struct list_t vari5_l_actual = {0,0};
struct unit_t vari5_l = {LIS,0,1,0,1};
struct r_element_t vari5 = {1,&subclasses_g,&vari5_l,&vari6};
struct l_element_t vari43 = {1,&alternatives_g,0};
struct l_element_t vari42 = {1,&control_g,&vari43};
struct l_element_t vari41 = {1,&pound_sign_g,&vari42};
struct list_t vari4_l_actual = {3,&vari41};
struct unit_t vari4_l = {LIS,0,1,0,1};
struct r_element_t vari4 = {1,&members_g,&vari4_l,&vari5};
struct r_element_t vari3 = {1,&subclass_of_g,&concept_g,&vari4};
struct r_element_t vari2 = {1,&member_of_g,&class_g,&vari3};
struct token_t vari1_t_actual = {8,"VARIABLE"};
struct unit_t vari1_t = {TOK,0,1,0,1};
struct r_element_t vari1 = {1,&name_g,&vari1_t,&vari2};
struct record_t vari_actual = {6,&vari1};

struct string_t elem6_s_actual = {254,
 "     The class of all elements, or quantities in the\
\ndomain of the application.  The built-in element 'nil'\
\nappears as a direct member of 'element', while the\
\nremaining five built-in elements appear as members of\
\nthe subclasses 'boolean' and 'rule_mode'."};
struct unit_t elem6_s = {STR,0,1,0,1};
struct r_element_t elem6 = {1,&comment_g,&elem6_s,0};
struct l_element_t elem52 = {1,&rule_mode_g,0};
struct l_element_t elem51 = {1,&boolean_g,&elem52};
struct list_t elem5_l_actual = {2,&elem51};
struct unit_t elem5_l = {LIS,0,1,0,1};
struct r_element_t elem5 = {1,&subclasses_g,&elem5_l,&elem6};
struct l_element_t elem41 = {1,&nil_g,0};
struct list_t elem4_l_actual = {1,&elem41};
struct unit_t elem4_l = {LIS,0,1,0,1};
struct r_element_t elem4 = {1,&members_g,&elem4_l,&elem5};
struct r_element_t elem3 = {1,&subclass_of_g,&concept_g,&elem4};
struct r_element_t elem2 = {1,&member_of_g,&class_g,&elem3};
struct token_t elem1_t_actual = {7,"ELEMENT"};
struct unit_t elem1_t = {TOK,0,1,0,1};
struct r_element_t elem1 = {1,&name_g,&elem1_t,&elem2};
struct record_t elem_actual = {6,&elem1};
