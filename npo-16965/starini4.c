




	/*****************************************************************
	 * starini4.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains part 4 of the initialization code for the STAR
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

/* INTERNAL UNITS. -----------------------------------------------------------*/

	/*****************************************************************
	 * The alphabetical listing of named RECORDs known to STAR is
	 *   kept separate from the rest of the STAR knowledge base as
	 *   the value of the global variable "directory_g".  This list
	 *   is used to locate particular named RECORDs by reference
	 *   name.  The form of "directory_g" makes use of the available
	 *   UNIT data structure types, being a LIST of 26 LISTs, one for
	 *   each starting letter, "A" to "Z".  This LIST of LISTs is
	 *   augmented automatically as new named RECORDs are defined
	 *   within STAR.
	 *****************************************************************/

	/*****************************************************************
	 * The second internal UNIT is "garbage_g", which is used to
	 *   store UNITs tentatively tagged for garbage collection and
	 *   freeing of their defining structures (see "starcode.c").
	 *   The form of "garbage_g" is that of a STAR LIST.
	 *****************************************************************/

struct unit_t directory_g = {LIS,0,1,0,1000};
struct unit_t garbage_g   = {LIS,0,1,0,1000};

struct list_t dire26_l_actual = {0,0};
struct unit_t dire26_l = {LIS,0,1,0,1};
struct l_element_t dire26 = {1,&dire26_l,0};
struct list_t dire25_l_actual = {0,0};
struct unit_t dire25_l = {LIS,0,1,0,1};
struct l_element_t dire25 = {1,&dire25_l,&dire26};
struct list_t dire24_l_actual = {0,0};
struct unit_t dire24_l = {LIS,0,1,0,1};
struct l_element_t dire24 = {1,&dire24_l,&dire25};
struct list_t dire23_l_actual = {0,0};
struct unit_t dire23_l = {LIS,0,1,0,1};
struct l_element_t dire23 = {1,&dire23_l,&dire24};
struct l_element_t dire222 = {1,&variable_g,0};
struct l_element_t dire221 = {1,&value_g,&dire222};
struct list_t dire22_l_actual = {2,&dire221};
struct unit_t dire22_l = {LIS,0,1,0,1};
struct l_element_t dire22 = {1,&dire22_l,&dire23};
struct list_t dire21_l_actual = {0,0};
struct unit_t dire21_l = {LIS,0,1,0,1};
struct l_element_t dire21 = {1,&dire21_l,&dire22};
struct l_element_t dire202 = {1,&true_g,0};
struct l_element_t dire201 = {1,&temporary_g,&dire202};
struct list_t dire20_l_actual = {2,&dire201};
struct unit_t dire20_l = {LIS,0,1,0,1};
struct l_element_t dire20 = {1,&dire20_l,&dire21};
struct l_element_t dire197 = {1,&subclass_of_g,0};
struct l_element_t dire196 = {1,&subclasses_g,&dire197};
struct l_element_t dire195 = {1,&stop_value_g,&dire196};
struct l_element_t dire194 = {1,&skip_value_g,&dire195};
struct l_element_t dire193 = {1,&single_test_g,&dire194};
struct l_element_t dire192 = {1,&single_application_g,&dire193};
struct l_element_t dire191 = {1,&side_effects_g,&dire192};
struct list_t dire19_l_actual = {7,&dire191};
struct unit_t dire19_l = {LIS,0,1,0,1};
struct l_element_t dire19 = {1,&dire19_l,&dire20};
struct l_element_t dire184 = {1,&rule_mode_g,0};
struct l_element_t dire183 = {1,&rule_g,&dire184};
struct l_element_t dire182 = {1,&return_value_g,&dire183};
struct l_element_t dire181 = {1,&result_value_g,&dire182};
struct list_t dire18_l_actual = {4,&dire181};
struct unit_t dire18_l = {LIS,0,1,0,1};
struct l_element_t dire18 = {1,&dire18_l,&dire19};
struct list_t dire17_l_actual = {0,0};
struct unit_t dire17_l = {LIS,0,1,0,1};
struct l_element_t dire17 = {1,&dire17_l,&dire18};
struct l_element_t dire162 = {1,&pound_sign_g,0};
struct l_element_t dire161 = {1,&pattern_g,&dire162};
struct list_t dire16_l_actual = {2,&dire161};
struct unit_t dire16_l = {LIS,0,1,0,1};
struct l_element_t dire16 = {1,&dire16_l,&dire17};
struct list_t dire15_l_actual = {0,0};
struct unit_t dire15_l = {LIS,0,1,0,1};
struct l_element_t dire15 = {1,&dire15_l,&dire16};
struct l_element_t dire143 = {1,&n_arguments_g,0};
struct l_element_t dire142 = {1,&nil_g,&dire143};
struct l_element_t dire141 = {1,&name_g,&dire142};
struct list_t dire14_l_actual = {3,&dire141};
struct unit_t dire14_l = {LIS,0,1,0,1};
struct l_element_t dire14 = {1,&dire14_l,&dire15};
struct l_element_t dire134 = {1,&multiple_application_g,0};
struct l_element_t dire133 = {1,&mode_g,&dire134};
struct l_element_t dire132 = {1,&member_of_g,&dire133};
struct l_element_t dire131 = {1,&members_g,&dire132};
struct list_t dire13_l_actual = {4,&dire131};
struct unit_t dire13_l = {LIS,0,1,0,1};
struct l_element_t dire13 = {1,&dire13_l,&dire14};
struct list_t dire12_l_actual = {0,0};
struct unit_t dire12_l = {LIS,0,1,0,1};
struct l_element_t dire12 = {1,&dire12_l,&dire13};
struct list_t dire11_l_actual = {0,0};
struct unit_t dire11_l = {LIS,0,1,0,1};
struct l_element_t dire11 = {1,&dire11_l,&dire12};
struct list_t dire10_l_actual = {0,0};
struct unit_t dire10_l = {LIS,0,1,0,1};
struct l_element_t dire10 = {1,&dire10_l,&dire11};
struct l_element_t dire093 = {1,&if_retracted_g,0};
struct l_element_t dire092 = {1,&if_needed_g,&dire093};
struct l_element_t dire091 = {1,&if_asserted_g,&dire092};
struct list_t dire09_l_actual = {3,&dire091};
struct unit_t dire09_l = {LIS,0,1,0,1};
struct l_element_t dire09 = {1,&dire09_l,&dire10};
struct list_t dire08_l_actual = {0,0};
struct unit_t dire08_l = {LIS,0,1,0,1};
struct l_element_t dire08 = {1,&dire08_l,&dire09};
struct list_t dire07_l_actual = {0,0};
struct unit_t dire07_l = {LIS,0,1,0,1};
struct l_element_t dire07 = {1,&dire07_l,&dire08};
struct l_element_t dire062 = {1,&function_g,0};
struct l_element_t dire061 = {1,&false_g,&dire062};
struct list_t dire06_l_actual = {2,&dire061};
struct unit_t dire06_l = {LIS,0,1,0,1};
struct l_element_t dire06 = {1,&dire06_l,&dire07};
struct l_element_t dire051 = {1,&element_g,0};
struct list_t dire05_l_actual = {1,&dire051};
struct unit_t dire05_l = {LIS,0,1,0,1};
struct l_element_t dire05 = {1,&dire05_l,&dire06};
struct l_element_t dire041 = {1,&default_g,0};
struct list_t dire04_l_actual = {1,&dire041};
struct unit_t dire04_l = {LIS,0,1,0,1};
struct l_element_t dire04 = {1,&dire04_l,&dire05};
struct l_element_t dire036 = {1,&control_g,0};
struct l_element_t dire035 = {1,&condition_g,&dire036};
struct l_element_t dire034 = {1,&concept_g,&dire035};
struct l_element_t dire033 = {1,&comment_g,&dire034};
struct l_element_t dire032 = {1,&class_g,&dire033};
struct l_element_t dire031 = {1,&case_g,&dire032};
struct list_t dire03_l_actual = {6,&dire031};
struct unit_t dire03_l = {LIS,0,1,0,1};
struct l_element_t dire03 = {1,&dire03_l,&dire04};
struct l_element_t dire023 = {1,&break_value_g,0};
struct l_element_t dire022 = {1,&boolean_g,&dire023};
struct l_element_t dire021 = {1,&bindings_g,&dire022};
struct list_t dire02_l_actual = {3,&dire021};
struct unit_t dire02_l = {LIS,0,1,0,1};
struct l_element_t dire02 = {1,&dire02_l,&dire03};
struct l_element_t dire017 = {1,&attribute_g,0};
struct l_element_t dire016 = {1,&aspects_g,&dire017};
struct l_element_t dire015 = {1,&arguments_g,&dire016};
struct l_element_t dire014 = {1,&alternatives_g,&dire015};
struct l_element_t dire013 = {1,&algorithm_g,&dire014};
struct l_element_t dire012 = {1,&action_g,&dire013};
struct l_element_t dire011 = {1,&abbreviation_g,&dire012};
struct list_t dire01_l_actual = {7,&dire011};
struct unit_t dire01_l = {LIS,0,1,0,1};
struct l_element_t dire01 = {1,&dire01_l,&dire02};
struct list_t dire_actual = {26,&dire01};

struct list_t garb_actual = {0,0};
