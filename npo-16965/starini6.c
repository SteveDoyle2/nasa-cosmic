




	/*****************************************************************
	 * starini6.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains part 6 of the initialization code for the STAR
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

extern struct unit_t conc5_l;
extern struct unit_t conc4_l;
extern struct unit_t clas5_l;
extern struct unit_t clas4_l;
extern struct unit_t attr5_l;
extern struct unit_t attr4_l;
extern struct unit_t func5_l;
extern struct unit_t func4_l;
extern struct unit_t vari5_l;
extern struct unit_t vari4_l;
extern struct unit_t elem5_l;
extern struct unit_t elem4_l;
extern struct unit_t rule5_l;
extern struct unit_t rule4_l;
extern struct unit_t bool5_l;
extern struct unit_t bool4_l;
extern struct unit_t rmod5_l;
extern struct unit_t rmod4_l;
extern struct unit_t poun4_l;
extern struct unit_t cont4_l;
extern struct unit_t alte4_l;
extern struct unit_t dire26_l;
extern struct unit_t dire25_l;
extern struct unit_t dire24_l;
extern struct unit_t dire23_l;
extern struct unit_t dire22_l;
extern struct unit_t dire21_l;
extern struct unit_t dire20_l;
extern struct unit_t dire19_l;
extern struct unit_t dire18_l;
extern struct unit_t dire17_l;
extern struct unit_t dire16_l;
extern struct unit_t dire15_l;
extern struct unit_t dire14_l;
extern struct unit_t dire13_l;
extern struct unit_t dire12_l;
extern struct unit_t dire11_l;
extern struct unit_t dire10_l;
extern struct unit_t dire09_l;
extern struct unit_t dire08_l;
extern struct unit_t dire07_l;
extern struct unit_t dire06_l;
extern struct unit_t dire05_l;
extern struct unit_t dire04_l;
extern struct unit_t dire03_l;
extern struct unit_t dire02_l;
extern struct unit_t dire01_l;
extern struct unit_t directory_g;
extern struct unit_t garbage_g;

extern struct list_t conc5_l_actual;
extern struct list_t conc4_l_actual;
extern struct list_t clas5_l_actual;
extern struct list_t clas4_l_actual;
extern struct list_t attr5_l_actual;
extern struct list_t attr4_l_actual;
extern struct list_t func5_l_actual;
extern struct list_t func4_l_actual;
extern struct list_t vari5_l_actual;
extern struct list_t vari4_l_actual;
extern struct list_t elem5_l_actual;
extern struct list_t elem4_l_actual;
extern struct list_t rule5_l_actual;
extern struct list_t rule4_l_actual;
extern struct list_t bool5_l_actual;
extern struct list_t bool4_l_actual;
extern struct list_t rmod5_l_actual;
extern struct list_t rmod4_l_actual;
extern struct list_t poun4_l_actual;
extern struct list_t cont4_l_actual;
extern struct list_t alte4_l_actual;
extern struct list_t dire26_l_actual;
extern struct list_t dire25_l_actual;
extern struct list_t dire24_l_actual;
extern struct list_t dire23_l_actual;
extern struct list_t dire22_l_actual;
extern struct list_t dire21_l_actual;
extern struct list_t dire20_l_actual;
extern struct list_t dire19_l_actual;
extern struct list_t dire18_l_actual;
extern struct list_t dire17_l_actual;
extern struct list_t dire16_l_actual;
extern struct list_t dire15_l_actual;
extern struct list_t dire14_l_actual;
extern struct list_t dire13_l_actual;
extern struct list_t dire12_l_actual;
extern struct list_t dire11_l_actual;
extern struct list_t dire10_l_actual;
extern struct list_t dire09_l_actual;
extern struct list_t dire08_l_actual;
extern struct list_t dire07_l_actual;
extern struct list_t dire06_l_actual;
extern struct list_t dire05_l_actual;
extern struct list_t dire04_l_actual;
extern struct list_t dire03_l_actual;
extern struct list_t dire02_l_actual;
extern struct list_t dire01_l_actual;
extern struct list_t dire_actual;
extern struct list_t garb_actual;

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

extern struct record_t conc_actual;
extern struct record_t clas_actual;
extern struct record_t attr_actual;
extern struct record_t func_actual;
extern struct record_t vari_actual;
extern struct record_t elem_actual;
extern struct record_t rule_actual;
extern struct record_t bool_actual;
extern struct record_t rmod_actual;
extern struct record_t name_actual;
extern struct record_t meof_actual;
extern struct record_t comm_actual;
extern struct record_t suof_actual;
extern struct record_t memb_actual;
extern struct record_t subc_actual;
extern struct record_t patt_actual;
extern struct record_t aspe_actual;
extern struct record_t valu_actual;
extern struct record_t defa_actual;
extern struct record_t ifne_actual;
extern struct record_t ifas_actual;
extern struct record_t ifre_actual;
extern struct record_t side_actual;
extern struct record_t abbr_actual;
extern struct record_t narg_actual;
extern struct record_t argu_actual;
extern struct record_t temp_actual;
extern struct record_t algo_actual;
extern struct record_t bind_actual;
extern struct record_t mode_actual;
extern struct record_t cond_actual;
extern struct record_t acti_actual;
extern struct record_t case_actual;
extern struct record_t resu_actual;
extern struct record_t brea_actual;
extern struct record_t skip_actual;
extern struct record_t retu_actual;
extern struct record_t stop_actual;
extern struct record_t poun_actual;
extern struct record_t cont_actual;
extern struct record_t alte_actual;
extern struct record_t true_actual;
extern struct record_t fals_actual;
extern struct record_t stes_actual;
extern struct record_t sapp_actual;
extern struct record_t mapp_actual;
extern struct record_t niln_actual;

/* UNION INITIALIZATION TABLES. ----------------------------------------------*/

struct l_init_entry_t l_init_g[] =
	/*******************************************************
	 * Initialization table for LISTs in the fixed portion
	 *   of the STAR knowledge base.  Used by "initia-
	 *   lize_s" in a similar manner to "t_init_g".
	 *******************************************************/
  {
    {&conc5_l,&conc5_l_actual},
    {&conc4_l,&conc4_l_actual},
    {&clas5_l,&clas5_l_actual},
    {&clas4_l,&clas4_l_actual},
    {&attr5_l,&attr5_l_actual},
    {&attr4_l,&attr4_l_actual},
    {&func5_l,&func5_l_actual},
    {&func4_l,&func4_l_actual},
    {&vari5_l,&vari5_l_actual},
    {&vari4_l,&vari4_l_actual},
    {&elem5_l,&elem5_l_actual},
    {&elem4_l,&elem4_l_actual},
    {&rule5_l,&rule5_l_actual},
    {&rule4_l,&rule4_l_actual},
    {&bool5_l,&bool5_l_actual},
    {&bool4_l,&bool4_l_actual},
    {&rmod5_l,&rmod5_l_actual},
    {&rmod4_l,&rmod4_l_actual},
    {&poun4_l,&poun4_l_actual},
    {&cont4_l,&cont4_l_actual},
    {&alte4_l,&alte4_l_actual},
    {&dire26_l,&dire26_l_actual},
    {&dire25_l,&dire25_l_actual},
    {&dire24_l,&dire24_l_actual},
    {&dire23_l,&dire23_l_actual},
    {&dire22_l,&dire22_l_actual},
    {&dire21_l,&dire21_l_actual},
    {&dire20_l,&dire20_l_actual},
    {&dire19_l,&dire19_l_actual},
    {&dire18_l,&dire18_l_actual},
    {&dire17_l,&dire17_l_actual},
    {&dire16_l,&dire16_l_actual},
    {&dire15_l,&dire15_l_actual},
    {&dire14_l,&dire14_l_actual},
    {&dire13_l,&dire13_l_actual},
    {&dire12_l,&dire12_l_actual},
    {&dire11_l,&dire11_l_actual},
    {&dire10_l,&dire10_l_actual},
    {&dire09_l,&dire09_l_actual},
    {&dire08_l,&dire08_l_actual},
    {&dire07_l,&dire07_l_actual},
    {&dire06_l,&dire06_l_actual},
    {&dire05_l,&dire05_l_actual},
    {&dire04_l,&dire04_l_actual},
    {&dire03_l,&dire03_l_actual},
    {&dire02_l,&dire02_l_actual},
    {&dire01_l,&dire01_l_actual},
    {&directory_g,&dire_actual},
    {&garbage_g,&garb_actual},
    {0,0}
  };

struct r_init_entry_t r_init_g[] =
	/*******************************************************
	 * Initialization table for RECORDs in the fixed por-
	 *   tion of the STAR knowledge base.  Used by "initia-
	 *   lize_s" in a similar manner to "t_init_g".
	 *******************************************************/
  {
    {&concept_g,&conc_actual},
    {&class_g,&clas_actual},
    {&attribute_g,&attr_actual},
    {&function_g,&func_actual},
    {&variable_g,&vari_actual},
    {&element_g,&elem_actual},
    {&rule_g,&rule_actual},
    {&boolean_g,&bool_actual},
    {&rule_mode_g,&rmod_actual},
    {&name_g,&name_actual},
    {&member_of_g,&meof_actual},
    {&comment_g,&comm_actual},
    {&subclass_of_g,&suof_actual},
    {&members_g,&memb_actual},
    {&subclasses_g,&subc_actual},
    {&pattern_g,&patt_actual},
    {&aspects_g,&aspe_actual},
    {&value_g,&valu_actual},
    {&default_g,&defa_actual},
    {&if_needed_g,&ifne_actual},
    {&if_asserted_g,&ifas_actual},
    {&if_retracted_g,&ifre_actual},
    {&side_effects_g,&side_actual},
    {&abbreviation_g,&abbr_actual},
    {&n_arguments_g,&narg_actual},
    {&arguments_g,&argu_actual},
    {&temporary_g,&temp_actual},
    {&algorithm_g,&algo_actual},
    {&bindings_g,&bind_actual},
    {&mode_g,&mode_actual},
    {&condition_g,&cond_actual},
    {&action_g,&acti_actual},
    {&case_g,&case_actual},
    {&result_value_g,&resu_actual},
    {&break_value_g,&brea_actual},
    {&skip_value_g,&skip_actual},
    {&return_value_g,&retu_actual},
    {&stop_value_g,&stop_actual},
    {&pound_sign_g,&poun_actual},
    {&control_g,&cont_actual},
    {&alternatives_g,&alte_actual},
    {&true_g,&true_actual},
    {&false_g,&fals_actual},
    {&single_test_g,&stes_actual},
    {&single_application_g,&sapp_actual},
    {&multiple_application_g,&mapp_actual},
    {&nil_g,&niln_actual},
    {0,0}
  };
