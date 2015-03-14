/*
 **************************************************
 *  crsv.h :  This file defines the global flags  *
 *            and variables used in CRSV,the rule *
 *            cross reference, style analysis and *
 *            verification tool.                  * 
 **************************************************
 */
 
/* ============================  Special Flags  ============================ */
#define GENERIC       0  /* For use on any machine                   */
#define MAC_LSC       1  /* Macintosh with Lightspeed C (2.13)       */
#define IBM_TBC       0  /* IBM PC with Turbo C (1.5) and Curses     */
#define IBM_MSC       0  /* IBM PC with Microsoft C (5.0) and Curses */

#define WORDLENGTH  512  /* Max characters per word                  */
#define HASHSIZE    167
#define LINELENGTH   80  /* Number of characters on an output line   */

#define DEFRULE       1  /* First top level construct                */
#define DEFFACTS      2  /* Second top level construct               */
#define DEFRELATION   3  /* Third top level construct                */
#define DEFEXTERNAL   4  /* Fourth top level construct               */
#define DEFTEMPLATE   5  /* Fifth top level construct                */

#define ASSERT        7  /* Major Action                             */
#define TEST          8  /* Another major action                     */

#define IN_BIND       5  /* A special flag                           */
#define IN_NOT        6  /* Another flag                             */
#define RESTART     -99  /* Flag to mark restart                     */

#define RHS           1
#define LHS           2
#define CRSV_MADE     0
#define USER_MADE     1
#define TEMPLATE_MADE 2

#if MAC_LSC
#define MAX_FILES    20
#define MAX_NAME    80
#else
#define MAX_FILES   10
#define MAX_NAME   40
#endif

/* ========================  GLOBAL OPTIONS FLAGS  ========================= */
extern int    CHECK_RULES;      /* Check rules flag              */
extern int    CHECK_RELATIONS;  /* Check relations flag          */
extern int    CHECK_EX_FLAG;    /* Check external functions flag */
extern int    CHECK_COMMENTS;   /* Report on missing comments    */
extern int    VERBOSE;          /* Verbose printout flag         */
extern int    CHECK_STYLE;      /* Check code Style              */
extern int    CREATE_DEFRELS;   /* Create Defrelations File flag */
extern int    CHECK_DEFRELS;    /* Check defrelations flag       */
extern int    CHECK_DEBUG;      /* Display Debug info            */
extern int    ANALYZE_TRACE;    /* Analyze the trace file        */
extern int    CHECK_DEBUG;      /* For memory checking           */
extern int    LITERAL;          /* For field checking            */

extern int    IBM_COM_LINE;     /* Use command line for IBM_PC   */

extern int    MAX_SINGLE_LIST;  /* The length of a token list    */
                                /* when the wildcard ?VARIABLE   */
                                /* is substituted for the        */
                                /* explicit list                 */

/* ==========================  OTHER GLOBAL DATA  ========================== */

extern char   msg_buf[WORDLENGTH];  /* Error Message buffer */

/* ======================  INCLUDE DATA DEFINITIONS  ======================= */

#include "defs.h"  /* Include structure definitions */
