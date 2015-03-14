




	/*****************************************************************
	 * starini5.c			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Contains part 5 of the initialization code for the STAR
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

extern struct unit_t conc1_t;
extern struct unit_t clas1_t;
extern struct unit_t attr1_t;
extern struct unit_t func1_t;
extern struct unit_t vari1_t;
extern struct unit_t elem1_t;
extern struct unit_t rule1_t;
extern struct unit_t bool1_t;
extern struct unit_t rmod1_t;
extern struct unit_t name1_t;
extern struct unit_t meof1_t;
extern struct unit_t comm1_t;
extern struct unit_t suof1_t;
extern struct unit_t memb1_t;
extern struct unit_t subc1_t;
extern struct unit_t patt1_t;
extern struct unit_t aspe1_t;
extern struct unit_t valu1_t;
extern struct unit_t defa1_t;
extern struct unit_t ifne1_t;
extern struct unit_t ifas1_t;
extern struct unit_t ifre1_t;
extern struct unit_t side1_t;
extern struct unit_t abbr1_t;
extern struct unit_t narg1_t;
extern struct unit_t argu1_t;
extern struct unit_t temp1_t;
extern struct unit_t algo1_t;
extern struct unit_t bind1_t;
extern struct unit_t mode1_t;
extern struct unit_t cond1_t;
extern struct unit_t acti1_t;
extern struct unit_t case1_t;
extern struct unit_t resu1_t;
extern struct unit_t brea1_t;
extern struct unit_t skip1_t;
extern struct unit_t retu1_t;
extern struct unit_t stop1_t;
extern struct unit_t poun1_t;
extern struct unit_t cont1_t;
extern struct unit_t alte1_t;
extern struct unit_t true1_t;
extern struct unit_t fals1_t;
extern struct unit_t stes1_t;
extern struct unit_t sapp1_t;
extern struct unit_t mapp1_t;
extern struct unit_t niln1_t;

extern struct token_t conc1_t_actual;
extern struct token_t clas1_t_actual;
extern struct token_t attr1_t_actual;
extern struct token_t func1_t_actual;
extern struct token_t vari1_t_actual;
extern struct token_t elem1_t_actual;
extern struct token_t rule1_t_actual;
extern struct token_t bool1_t_actual;
extern struct token_t rmod1_t_actual;
extern struct token_t name1_t_actual;
extern struct token_t meof1_t_actual;
extern struct token_t comm1_t_actual;
extern struct token_t suof1_t_actual;
extern struct token_t memb1_t_actual;
extern struct token_t subc1_t_actual;
extern struct token_t patt1_t_actual;
extern struct token_t aspe1_t_actual;
extern struct token_t valu1_t_actual;
extern struct token_t defa1_t_actual;
extern struct token_t ifne1_t_actual;
extern struct token_t ifas1_t_actual;
extern struct token_t ifre1_t_actual;
extern struct token_t side1_t_actual;
extern struct token_t abbr1_t_actual;
extern struct token_t narg1_t_actual;
extern struct token_t argu1_t_actual;
extern struct token_t temp1_t_actual;
extern struct token_t algo1_t_actual;
extern struct token_t bind1_t_actual;
extern struct token_t mode1_t_actual;
extern struct token_t cond1_t_actual;
extern struct token_t acti1_t_actual;
extern struct token_t case1_t_actual;
extern struct token_t resu1_t_actual;
extern struct token_t brea1_t_actual;
extern struct token_t skip1_t_actual;
extern struct token_t retu1_t_actual;
extern struct token_t stop1_t_actual;
extern struct token_t poun1_t_actual;
extern struct token_t cont1_t_actual;
extern struct token_t alte1_t_actual;
extern struct token_t true1_t_actual;
extern struct token_t fals1_t_actual;
extern struct token_t stes1_t_actual;
extern struct token_t sapp1_t_actual;
extern struct token_t mapp1_t_actual;
extern struct token_t niln1_t_actual;

extern struct unit_t conc6_s;
extern struct unit_t clas6_s;
extern struct unit_t attr6_s;
extern struct unit_t func6_s;
extern struct unit_t vari6_s;
extern struct unit_t elem6_s;
extern struct unit_t rule6_s;
extern struct unit_t bool6_s;
extern struct unit_t rmod6_s;
extern struct unit_t name3_s;
extern struct unit_t meof3_s;
extern struct unit_t comm3_s;
extern struct unit_t suof3_s;
extern struct unit_t memb3_s;
extern struct unit_t subc3_s;
extern struct unit_t patt3_s;
extern struct unit_t aspe3_s;
extern struct unit_t valu3_s;
extern struct unit_t defa3_s;
extern struct unit_t ifne3_s;
extern struct unit_t ifas3_s;
extern struct unit_t ifre3_s;
extern struct unit_t side3_s;
extern struct unit_t abbr3_s;
extern struct unit_t narg3_s;
extern struct unit_t argu3_s;
extern struct unit_t temp3_s;
extern struct unit_t algo3_s;
extern struct unit_t bind3_s;
extern struct unit_t mode3_s;
extern struct unit_t cond3_s;
extern struct unit_t acti3_s;
extern struct unit_t case3_s;
extern struct unit_t resu3_s;
extern struct unit_t brea3_s;
extern struct unit_t skip3_s;
extern struct unit_t retu3_s;
extern struct unit_t stop3_s;
extern struct unit_t poun3_s;
extern struct unit_t cont3_s;
extern struct unit_t alte3_s;
extern struct unit_t stes3_s;
extern struct unit_t sapp3_s;
extern struct unit_t mapp3_s;

extern struct string_t conc6_s_actual;
extern struct string_t clas6_s_actual;
extern struct string_t attr6_s_actual;
extern struct string_t func6_s_actual;
extern struct string_t vari6_s_actual;
extern struct string_t elem6_s_actual;
extern struct string_t rule6_s_actual;
extern struct string_t bool6_s_actual;
extern struct string_t rmod6_s_actual;
extern struct string_t name3_s_actual;
extern struct string_t meof3_s_actual;
extern struct string_t comm3_s_actual;
extern struct string_t suof3_s_actual;
extern struct string_t memb3_s_actual;
extern struct string_t subc3_s_actual;
extern struct string_t patt3_s_actual;
extern struct string_t aspe3_s_actual;
extern struct string_t valu3_s_actual;
extern struct string_t defa3_s_actual;
extern struct string_t ifne3_s_actual;
extern struct string_t ifas3_s_actual;
extern struct string_t ifre3_s_actual;
extern struct string_t side3_s_actual;
extern struct string_t abbr3_s_actual;
extern struct string_t narg3_s_actual;
extern struct string_t argu3_s_actual;
extern struct string_t temp3_s_actual;
extern struct string_t algo3_s_actual;
extern struct string_t bind3_s_actual;
extern struct string_t mode3_s_actual;
extern struct string_t cond3_s_actual;
extern struct string_t acti3_s_actual;
extern struct string_t case3_s_actual;
extern struct string_t resu3_s_actual;
extern struct string_t brea3_s_actual;
extern struct string_t skip3_s_actual;
extern struct string_t retu3_s_actual;
extern struct string_t stop3_s_actual;
extern struct string_t poun3_s_actual;
extern struct string_t cont3_s_actual;
extern struct string_t alte3_s_actual;
extern struct string_t stes3_s_actual;
extern struct string_t sapp3_s_actual;
extern struct string_t mapp3_s_actual;

/* UNION INITIALIZATION TABLES. ----------------------------------------------*/

struct t_init_entry_t t_init_g[] =
	/*******************************************************
	 * Initialization table for TOKENs in the fixed portion
	 *   of the STAR knowledge base.  Used by the routine
	 *   "initialize_s" to complete the initialization of
	 *   C "unions", which cannot be initialized at compile
	 *   time.
	 *******************************************************/
  {
    {&conc1_t,&conc1_t_actual},
    {&clas1_t,&clas1_t_actual},
    {&attr1_t,&attr1_t_actual},
    {&func1_t,&func1_t_actual},
    {&vari1_t,&vari1_t_actual},
    {&elem1_t,&elem1_t_actual},
    {&rule1_t,&rule1_t_actual},
    {&bool1_t,&bool1_t_actual},
    {&rmod1_t,&rmod1_t_actual},
    {&name1_t,&name1_t_actual},
    {&meof1_t,&meof1_t_actual},
    {&comm1_t,&comm1_t_actual},
    {&suof1_t,&suof1_t_actual},
    {&memb1_t,&memb1_t_actual},
    {&subc1_t,&subc1_t_actual},
    {&patt1_t,&patt1_t_actual},
    {&aspe1_t,&aspe1_t_actual},
    {&valu1_t,&valu1_t_actual},
    {&defa1_t,&defa1_t_actual},
    {&ifne1_t,&ifne1_t_actual},
    {&ifas1_t,&ifas1_t_actual},
    {&ifre1_t,&ifre1_t_actual},
    {&side1_t,&side1_t_actual},
    {&abbr1_t,&abbr1_t_actual},
    {&narg1_t,&narg1_t_actual},
    {&argu1_t,&argu1_t_actual},
    {&temp1_t,&temp1_t_actual},
    {&algo1_t,&algo1_t_actual},
    {&bind1_t,&bind1_t_actual},
    {&mode1_t,&mode1_t_actual},
    {&cond1_t,&cond1_t_actual},
    {&acti1_t,&acti1_t_actual},
    {&case1_t,&case1_t_actual},
    {&resu1_t,&resu1_t_actual},
    {&brea1_t,&brea1_t_actual},
    {&skip1_t,&skip1_t_actual},
    {&retu1_t,&retu1_t_actual},
    {&stop1_t,&stop1_t_actual},
    {&poun1_t,&poun1_t_actual},
    {&cont1_t,&cont1_t_actual},
    {&alte1_t,&alte1_t_actual},
    {&true1_t,&true1_t_actual},
    {&fals1_t,&fals1_t_actual},
    {&stes1_t,&stes1_t_actual},
    {&sapp1_t,&sapp1_t_actual},
    {&mapp1_t,&mapp1_t_actual},
    {&niln1_t,&niln1_t_actual},
    {0,0}
  };

struct s_init_entry_t s_init_g[] =
	/*******************************************************
	 * Initialization table for STRINGs in the fixed por-
	 *   tion of the STAR knowledge base.  Used by "initia-
	 *   lize_s" in a similar manner to "t_init_g".
	 *******************************************************/
  {
    {&conc6_s,&conc6_s_actual},
    {&clas6_s,&clas6_s_actual},
    {&attr6_s,&attr6_s_actual},
    {&func6_s,&func6_s_actual},
    {&vari6_s,&vari6_s_actual},
    {&elem6_s,&elem6_s_actual},
    {&rule6_s,&rule6_s_actual},
    {&bool6_s,&bool6_s_actual},
    {&rmod6_s,&rmod6_s_actual},
    {&name3_s,&name3_s_actual},
    {&meof3_s,&meof3_s_actual},
    {&comm3_s,&comm3_s_actual},
    {&suof3_s,&suof3_s_actual},
    {&memb3_s,&memb3_s_actual},
    {&subc3_s,&subc3_s_actual},
    {&patt3_s,&patt3_s_actual},
    {&aspe3_s,&aspe3_s_actual},
    {&valu3_s,&valu3_s_actual},
    {&defa3_s,&defa3_s_actual},
    {&ifne3_s,&ifne3_s_actual},
    {&ifas3_s,&ifas3_s_actual},
    {&ifre3_s,&ifre3_s_actual},
    {&side3_s,&side3_s_actual},
    {&abbr3_s,&abbr3_s_actual},
    {&narg3_s,&narg3_s_actual},
    {&argu3_s,&argu3_s_actual},
    {&temp3_s,&temp3_s_actual},
    {&algo3_s,&algo3_s_actual},
    {&bind3_s,&bind3_s_actual},
    {&mode3_s,&mode3_s_actual},
    {&cond3_s,&cond3_s_actual},
    {&acti3_s,&acti3_s_actual},
    {&case3_s,&case3_s_actual},
    {&resu3_s,&resu3_s_actual},
    {&brea3_s,&brea3_s_actual},
    {&skip3_s,&skip3_s_actual},
    {&retu3_s,&retu3_s_actual},
    {&stop3_s,&stop3_s_actual},
    {&poun3_s,&poun3_s_actual},
    {&cont3_s,&cont3_s_actual},
    {&alte3_s,&alte3_s_actual},
    {&stes3_s,&stes3_s_actual},
    {&sapp3_s,&sapp3_s_actual},
    {&mapp3_s,&mapp3_s_actual},
    {0,0}
  };
