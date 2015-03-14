




	/*****************************************************************
	 * starplus.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * This file contains definitions for three built-in functions of
	 *   STAR which tend to be machine-dependent in nature.  These
	 *   are the STAR commands "system", which passes a command
	 *   string along to the operating system of the host computer,
	 *   "suspend", which saves an image of the currently executing
	 *   process in a file for subsequent reentry at a later time,
	 *   and "exit", which terminates a session with STAR.
	 *
	 * The functions "system" and "suspend" are not currently
	 *   implemented in the VAX/VMS version of STAR.
	 *****************************************************************/


/* I. "SUSPEND" IMPLEMENTATION CODE. <><><><><><><><><><><><><><><><><><><><><*/

	/*****************************************************************
	 * Not implemented for the VAX/VMS version of STAR.
	 *****************************************************************/


/* II. SYSTEM, SUSPEND AND EXIT FUNCTION DEFINITIONS. <><><><><><><><><><><><>*/

/* INCLUDE FILES AND EXTERNAL REFERENCES. ------------------------------------*/

#include "stardefs.h"
#include "starcomm.h"

extern struct unit_t *terr_s();
extern struct unit_t *cerr_s();
extern struct unit_t *verr_s();
extern struct unit_t *perr_s();

/* FUNCTION DEFINITIONS. -----------------------------------------------------*/

struct unit_t *system_f(str1)
struct unit_t *str1;
  {
  char *fname="system";
  extern struct unit_t *make_string();
  terr_check(str1,STR,fname,1);
  return(make_string("Not implemented for VAX/VMS version."));
  }

struct unit_t *suspend_f(str1)
struct unit_t *str1;
  {
  char *fname="suspend";
  extern struct unit_t *make_string();
  terr_check(str1,STR,fname,1);
  return(make_string("Not implemented for VAX/VMS version."));
  }

	/*****************************************************************
	 * Note: if a particular application requires a signal of some
	 *   type or another to be sent out upon exiting the interpreter,
	 *   the call setting up the signal may be inserted in "exit_f"
	 *   as indicated in the statement commented out below.
	 *****************************************************************/

struct unit_t *exit_f()
  {
  char *fname="exit";
/* kill(getpid(),SIGQUIT); Sample signal sent out by "exit". */
  exit();
  }

/* EXTENSION TO BUILT-IN FUNCTION TABLE. -------------------------------------*/

struct built_in_function_entry_t plus_init_g[] = 
	/*******************************************************
	 * Contains the necessary information for run-time
	 *   initialization of the built-in commands "system",
	 *   "suspend" and "exit".  Format for the entries is
	 *   the same as for the remainder of the built-in
	 *   functions as listed in the table "bif_init_g".
	 *   Each function is specified by name (in lowercase,
	 *   as the string represents the function's reference
	 *   name, optional abbreviation character, number of
	 *   arguments, implementing C function, label for the
	 *   CONNECTION to the C function, argument passing
	 *   method (= "BY_VALUE" for C functions) and text for
	 *   the "comment" field in the function's definition
	 *   in STAR.  This information is used by the function
	 *   "initialize_s" to perform the actual
	 *   initialization.
	 *******************************************************/
  {
    {"system","",1,system_f,"C_SYSTEM_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n  <<<Not implemented for VAX/VMS version of STAR.>>>\
\n\
\n     Use the characters of STRING1 to specify a command to\
\nthe operating system.  Returns STRING1 as the result."},
    {"suspend","",1,suspend_f,"C_SUSPEND_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
\n\
\n  <<<Not implemented for VAX/VMS version of STAR.>>>\
\n\
\n     Saves an image of the currently running executable\
\nfile in a file named by STRING1, so that the session may\
\nbe reentered at a later time.  STRING1 is returned."},
    {"exit","",0,exit_f,"C_EXIT_FUNCTION",BY_VALUE,
 "     () => ...\
\n\
\n     Exit the STAR interpreter."},

    {0,0,0,0,0,0,0}
  };

/* III. BY-REFERENCE FORMS OF THE BUILT-IN FUNCTIONS. <><><><><><><><><><><><>*/

	/*****************************************************************
	 * These functions may be called by external functions defined in
	 *   languages which pass arguments by reference.  The functions
	 *   contained here allow the external functions to command the
	 *   operation of the various built-in functions of STAR.  For
	 *   languages which pass arguments by value, the primary forms
	 *   of the built-in functions as contained in the file
	 *   "starbifs.c" (and above in this file) may be used.
	 *****************************************************************/

/* NUMERICAL FUNCTIONS. ------------------------------------------------------*/

struct unit_t *negatef_(num1)
struct unit_t **num1;
  {
  extern struct unit_t *negate_f();
  return(negate_f(*num1));
  }

struct unit_t *addf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *add_f();
  return(add_f(*num1,*num2));
  }

struct unit_t *subtractf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *subtract_f();
  return(subtract_f(*num1,*num2));
  }

struct unit_t *multiplyf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *multiply_f();
  return(multiply_f(*num1,*num2));
  }

struct unit_t *dividef_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *divide_f();
  return(divide_f(*num1,*num2));
  }

struct unit_t *minimumf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *minimum_f();
  return(minimum_f(*num1,*num2));
  }

struct unit_t *maximumf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *maximum_f();
  return(maximum_f(*num1,*num2));
  }

/* TOKEN FUNCTIONS. ----------------------------------------------------------*/

struct unit_t *locatef_(tok1)
struct unit_t **tok1;
  {
  extern struct unit_t *locate_f();
  return(locate_f(*tok1));
  }

struct unit_t *testf_(tok1)
struct unit_t **tok1;
  {
  extern struct unit_t *test_f();
  return(test_f(*tok1));
  }

/* STRING FUNCTIONS. ---------------------------------------------------------*/

struct unit_t *characterf_(str1,num1)
struct unit_t **str1,**num1;
  {
  extern struct unit_t *character_f();
  return(character_f(*str1,*num1));
  }

struct unit_t *fetchf_(str1,num1)
struct unit_t **str1,**num1;
  {
  extern struct unit_t *fetch_f();
  return(fetch_f(*str1,*num1));
  }

struct unit_t *releasef_(str1,num1)
struct unit_t **str1,**num1;
  {
  extern struct unit_t *release_f();
  return(release_f(*str1,*num1));
  }

struct unit_t *joinf_(str1,str2)
struct unit_t **str1,**str2;
  {
  extern struct unit_t *join_f();
  return(join_f(*str1,*str2));
  }

struct unit_t *findf_(str1,str2)
struct unit_t **str1,**str2;
  {
  extern struct unit_t *find_f();
  return(find_f(*str1,*str2));
  }

struct unit_t *lengthf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *length_f();
  return(length_f(*str1));
  }

/* LIST FUNCTIONS. -----------------------------------------------------------*/

struct unit_t *selectf_(lis1,num1)
struct unit_t **lis1,**num1;
  {
  extern struct unit_t *select_f();
  return(select_f(*lis1,*num1));
  }

struct unit_t *replacef_(lis1,num1,uni1)
struct unit_t **lis1,**num1,**uni1;
  {
  extern struct unit_t *replace_f();
  return(replace_f(*lis1,*num1,*uni1));
  }

struct unit_t *deletef_(lis1,num1)
struct unit_t **lis1,**num1;
  {
  extern struct unit_t *delete_f();
  return(delete_f(*lis1,*num1));
  }

struct unit_t *insertf_(lis1,num1,uni1)
struct unit_t **lis1,**num1,**uni1;
  {
  extern struct unit_t *insert_f();
  return(insert_f(*lis1,*num1,*uni1));
  }

struct unit_t *takef_(lis1,num1)
struct unit_t **lis1,**num1;
  {
  extern struct unit_t *take_f();
  return(take_f(*lis1,*num1));
  }

struct unit_t *dropf_(lis1,num1)
struct unit_t **lis1,**num1;
  {
  extern struct unit_t *drop_f();
  return(drop_f(*lis1,*num1));
  }

struct unit_t *appendf_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *append_f();
  return(append_f(*lis1,*lis2));
  }

struct unit_t *sizef_(lis1)
struct unit_t **lis1;
  {
  extern struct unit_t *size_f();
  return(size_f(*lis1));
  }

struct unit_t *unionf_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *union_f();
  return(union_f(*lis1,*lis2));
  }

struct unit_t *intersectionf_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *intersection_f();
  return(intersection_f(*lis1,*lis2));
  }

struct unit_t *differencef_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *difference_f();
  return(difference_f(*lis1,*lis2));
  }

/* RECORD FUNCTIONS. ---------------------------------------------------------*/

struct unit_t *getf_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *get_f();
  return(get_f(*rec1,*att1));
  }

struct unit_t *putf_(rec1,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
  {
  extern struct unit_t *put_f();
  return(put_f(*rec1,*att1,*uni1));
  }

struct unit_t *omitf_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *omit_f();
  return(omit_f(*rec1,*att1));
  }

struct unit_t *detachf_(rec1,lis1)
struct unit_t **rec1,**lis1;
  {
  extern struct unit_t *detach_f();
  return(detach_f(*rec1,*lis1));
  }

struct unit_t *attachf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *attach_f();
  return(attach_f(*rec1,*rec2));
  }

struct unit_t *keyf_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *key_f();
  return(key_f(*rec1));
  }

struct unit_t *imagef_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *image_f();
  return(image_f(*rec1));
  }

struct unit_t *buildf_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *build_f();
  return(build_f(*lis1,*lis2));
  }

struct unit_t *definef_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *define_f();
  return(define_f(*rec1));
  }

struct unit_t *createf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *create_f();
  return(create_f(*rec1,*rec2));
  }

struct unit_t *assertf_(rec1,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
  {
  extern struct unit_t *assert_f();
  return(assert_f(*rec1,*att1,*uni1));
  }

struct unit_t *retractf_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *retract_f();
  return(retract_f(*rec1,*att1));
  }

struct unit_t *modifyf_(rec1,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
  {
  extern struct unit_t *modify_f();
  return(modify_f(*rec1,*att1,*uni1));
  }

struct unit_t *revisef_(rec1,lis1)
struct unit_t **rec1,**lis1;
  {
  extern struct unit_t *revise_f();
  return(revise_f(*rec1,*lis1));
  }

struct unit_t *mergef_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *merge_f();
  return(merge_f(*rec1,*rec2));
  }

struct unit_t *dotf_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *dot_f();
  return(dot_f(*rec1));
  }

struct unit_t *newf_(rec1,uni1)
struct unit_t **rec1,**uni1;
  {
  extern struct unit_t *new_f();
  return(new_f(*rec1,*uni1));
  }

struct unit_t *setf_(rec1,uni1)
struct unit_t **rec1,**uni1;
  {
  extern struct unit_t *set_f();
  return(set_f(*rec1,*uni1));
  }

struct unit_t *oldf_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *old_f();
  return(old_f(*rec1));
  }

struct unit_t *determinef_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *determine_f();
  return(determine_f(*rec1,*att1));
  }

struct unit_t *estimatef_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *estimate_f();
  return(estimate_f(*rec1,*att1));
  }

struct unit_t *calculatef_(rec1,att1)
struct unit_t **rec1,**att1;
  {
  extern struct unit_t *calculate_f();
  return(calculate_f(*rec1,*att1));
  }

struct unit_t *obtainf_(rec1,att1,att2)
struct unit_t **rec1,**att1,**att2;
  {
  extern struct unit_t *obtain_f();
  return(obtain_f(*rec1,*att1,*att2));
  }

struct unit_t *pathf_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *path_f();
  return(path_f(*rec1));
  }

struct unit_t *enumeratef_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *enumerate_f();
  return(enumerate_f(*rec1));
  }

/* EXPRESSION FUNCTIONS. -----------------------------------------------------*/

struct unit_t *operationf_(exp1)
struct unit_t **exp1;
  {
  extern struct unit_t *operation_f();
  return(operation_f(*exp1));
  }

struct unit_t *applicationf_(exp1)
struct unit_t **exp1;
  {
  extern struct unit_t *application_f();
  return(application_f(*exp1));
  }

struct unit_t *formulatef_(rec1,lis1)
struct unit_t **rec1,**lis1;
  {
  extern struct unit_t *formulate_f();
  return(formulate_f(*rec1,*lis1));
  }

/* LOGICAL FUNCTIONS. --------------------------------------------------------*/

struct unit_t *numberf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *number_f();
  return(number_f(*uni1));
  }

struct unit_t *tokenf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *token_f();
  return(token_f(*uni1));
  }

struct unit_t *stringf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *string_f();
  return(string_f(*uni1));
  }

struct unit_t *listf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *list_f();
  return(list_f(*uni1));
  }

struct unit_t *recordf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *record_f();
  return(record_f(*uni1));
  }

struct unit_t *expressionf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *expression_f();
  return(expression_f(*uni1));
  }

struct unit_t *connectionf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *connection_f();
  return(connection_f(*uni1));
  }

struct unit_t *nullf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *null_f();
  return(null_f(*uni1));
  }

struct unit_t *equalf_(uni1,uni2)
struct unit_t **uni1,**uni2;
  {
  extern struct unit_t *equal_f();
  return(equal_f(*uni1,*uni2));
  }

struct unit_t *lessf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *less_f();
  return(less_f(*num1,*num2));
  }

struct unit_t *greaterf_(num1,num2)
struct unit_t **num1,**num2;
  {
  extern struct unit_t *greater_f();
  return(greater_f(*num1,*num2));
  }

struct unit_t *inf_(uni1,lis1)
struct unit_t **uni1,**lis1;
  {
  extern struct unit_t *in_f();
  return(in_f(*uni1,*lis1));
  }

struct unit_t *subsetf_(lis1,lis2)
struct unit_t **lis1,**lis2;
  {
  extern struct unit_t *subset_f();
  return(subset_f(*lis1,*lis2));
  }

struct unit_t *isaf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *isa_f();
  return(isa_f(*rec1,*rec2));
  }

struct unit_t *withinf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *within_f();
  return(within_f(*rec1,*rec2));
  }

struct unit_t *notf_(rec1)
struct unit_t **rec1;
  {
  extern struct unit_t *not_f();
  return(not_f(*rec1));
  }

struct unit_t *andf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *and_f();
  return(and_f(*rec1,*rec2));
  }

struct unit_t *orf_(rec1,rec2)
struct unit_t **rec1,**rec2;
  {
  extern struct unit_t *or_f();
  return(or_f(*rec1,*rec2));
  }

struct unit_t *existsf_(lis1,rec1,exp1)
struct unit_t **lis1,**rec1,**exp1;
  {
  extern struct unit_t *exists_f();
  return(exists_f(*lis1,*rec1,*exp1));
  }

struct unit_t *everyf_(lis1,rec1,exp1)
struct unit_t **lis1,**rec1,**exp1;
  {
  extern struct unit_t *every_f();
  return(every_f(*lis1,*rec1,*exp1));
  }

struct unit_t *whichf_(lis1,rec1,exp1)
struct unit_t **lis1,**rec1,**exp1;
  {
  extern struct unit_t *which_f();
  return(which_f(*lis1,*rec1,*exp1));
  }

/* I/O FUNCTIONS. ------------------------------------------------------------*/

struct unit_t *parsef_()
  {
  extern struct unit_t *parse_f();
  return(parse_f());
  }

struct unit_t *displayf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *display_f();
  return(display_f(*uni1));
  }

struct unit_t *inputf_()
  {
  extern struct unit_t *input_f();
  return(input_f());
  }

struct unit_t *outputf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *output_f();
  return(output_f(*str1));
  }

struct unit_t *formatf_(str1,lis1)
struct unit_t **str1,**lis1;
  {
  extern struct unit_t *format_f();
  return(format_f(*str1,*lis1));
  }

struct unit_t *savef_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *save_f();
  return(save_f(*str1));
  }

struct unit_t *stashf_(str1,lis1)
struct unit_t **str1,**lis1;
  {
  extern struct unit_t *stash_f();
  return(stash_f(*str1,*lis1));
  }

struct unit_t *loadf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *load_f();
  return(load_f(*str1));
  }

struct unit_t *readf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *read_f();
  return(read_f(*str1));
  }

struct unit_t *writef_(str1,str2)
struct unit_t **str1,**str2;
  {
  extern struct unit_t *write_f();
  return(write_f(*str1,*str2));
  }

struct unit_t *extendf_(str1,str2)
struct unit_t **str1,**str2;
  {
  extern struct unit_t *extend_f();
  return(extend_f(*str1,*str2));
  }

struct unit_t *spellf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *spell_f();
  return(spell_f(*uni1));
  }

struct unit_t *unspellf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *unspell_f();
  return(unspell_f(*str1));
  }

struct unit_t *scanf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *scan_f();
  return(scan_f(*str1));
  }

/* EVALUATION FUNCTIONS. -----------------------------------------------------*/

struct unit_t *quotef_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *quote_f();
  return(quote_f(*uni1));
  }

struct unit_t *evaluatef_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *evaluate_f();
  return(evaluate_f(*uni1));
  }

struct unit_t *preparef_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *prepare_f();
  return(prepare_f(*uni1));
  }

struct unit_t *applyf_(rec1,lis1)
struct unit_t **rec1,**lis1;
  {
  extern struct unit_t *apply_f();
  return(apply_f(*rec1,*lis1));
  }

struct unit_t *iff_(rec1,uni1)
struct unit_t **rec1,**uni1;
  {
  extern struct unit_t *if_f();
  return(if_f(*rec1,*uni1));
  }

struct unit_t *ifelsef_(rec1,uni1,uni2)
struct unit_t **rec1,**uni1,**uni2;
  {
  extern struct unit_t *ifelse_f();
  return(ifelse_f(*rec1,*uni1,*uni2));
  }

struct unit_t *branchf_(uni1,lis1)
struct unit_t **uni1,**lis1;
  {
  extern struct unit_t *branch_f();
  return(branch_f(*uni1,*lis1));
  }

struct unit_t *dof_(lis1)
struct unit_t **lis1;
  {
  extern struct unit_t *do_f();
  return(do_f(*lis1));
  }

struct unit_t *repeatf_(lis1)
struct unit_t **lis1;
  {
  extern struct unit_t *repeat_f();
  return(repeat_f(*lis1));
  }

struct unit_t *whilef_(uni1,lis1)
struct unit_t **uni1,**lis1;
  {
  extern struct unit_t *while_f();
  return(while_f(*uni1,*lis1));
  }

struct unit_t *forf_(uni1,uni2,uni3,lis1)
struct unit_t **uni1,**uni2,**uni3,**lis1;
  {
  extern struct unit_t *for_f();
  return(for_f(*uni1,*uni2,*uni3,*lis1));
  }

struct unit_t *throughf_(lis1,rec1,lis2)
struct unit_t **lis1,**rec1,**lis2;
  {
  extern struct unit_t *through_f();
  return(through_f(*lis1,*rec1,*lis2));
  }

/* RULE-BASED OPERATION. -----------------------------------------------------*/

struct unit_t *invokef_(rec1,lis1)
struct unit_t **rec1,**lis1;
  {
  extern struct unit_t *invoke_f();
  return(invoke_f(*rec1,*lis1));
  }

/* CONTROL FLOW FUNCTIONS. ---------------------------------------------------*/

struct unit_t *resultf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *result_f();
  return(result_f(*uni1));
  }

struct unit_t *breakf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *break_f();
  return(break_f(*uni1));
  }

struct unit_t *skipf_()
  {
  extern struct unit_t *skip_f();
  return(skip_f());
  }

struct unit_t *returnf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *return_f();
  return(return_f(*uni1));
  }

struct unit_t *stopf_(uni1)
struct unit_t **uni1;
  {
  extern struct unit_t *stop_f();
  return(stop_f(*uni1));
  }

/* UTILITY FUNCTIONS. --------------------------------------------------------*/

struct unit_t *pausef_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *pause_f();
  return(pause_f(*str1));
  }

struct unit_t *systemf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *system_f();
  return(system_f(*str1));
  }

struct unit_t *suspendf_(str1)
struct unit_t **str1;
  {
  extern struct unit_t *suspend_f();
  return(suspend_f(*str1));
  }

struct unit_t *exitf_()
  {
  extern struct unit_t *exit_f();
  return(exit_f());
  }
