/*   CLIPS Version 4.30   4/25/89 */

/****************************************************************/
/* This file is the general header file for CLIPS. It contains  */
/* the global definitions used by CLIPS, and the compiler flags */
/* you have to edit to create a version of CLIPS for a specific */
/* system.                                                      */
/*                                                              */
/* If CLIPS is being used as an embedded tool, then this file   */
/* should be included in at least the main program. Any other   */
/* files which contain functions referring to things defined in */
/* this file must also include this file.                       */
/****************************************************************/

/****************************************************************/
/* -------------------- COMPILER FLAGS ------------------------ */
/****************************************************************/

/****************************************************************/
/* Flag denoting what kind of machine CLIPS is to run on. Only  */
/* one of these flags should be turned on (set to 1) at a time. */
/****************************************************************/

#define GENERIC       1   /* Generic CLIPS (any machine)           */
#define VMS           0   /* VAX VMS                               */
#define UNIX_V        0   /* UNIX System V or 4.2bsd or HP Unix    */
#define UNIX_7        0   /* UNIX System III Version 7 or Sun Unix */
#define MAC_LSC       0   /* Apple Macintosh, with Lightspeed C    */
#define IBM_MSC       0   /* IBM PC, with Microsoft C (5.1)        */
#define IBM_LATTICE   0   /* IBM PC, Lattice C (3.0) not tested!   */
#define IBM_TBC       0   /* IBM PC, with Turbo C (1.5 or higher)  */
#define IBM_ZTC       0   /* IBM PC, with Zortech C (1.07)         */


/****************************************************************/
/* EXTENDED MATH PACKAGE FLAG: If this is on, then the extended */
/* math package functions will be available for use, (normal    */
/* default). If this flag is off, then the extended math        */ 
/* functions will not be available, and the 30K or so of space  */
/* they require will be free. Usually a concern only on PC type */
/* machines.                                                    */
/****************************************************************/

#define EX_MATH  1

/***********************************************************/
/* TIMING FLAG. If this is on, then CLIPS will call the    */ 
/* clips_time function after executing all possible rules. */ 
/* Usually used only for testing purposes.                 */
/***********************************************************/

#define CLP_TIME  0

/***************************************************************/
/* TRACK_MEMORY:  If this is on, then CLIPS will keep track of */
/*   how much memory it has allocated through genalloc and     */
/*   genfree.  This number can then be retrieved through the   */
/*   memory function.                                          */
/***************************************************************/

#define TRACK_MEMORY 1

/****************************************************************/
/* TEXT PROCESSING : Turn on this flag for support of the       */
/* hierarchical lookup system.                                  */
/****************************************************************/

#define CLP_TEXTPRO 1

/****************************************************************/
/* HELP: To implement the help facility, set the flag below and */
/* specify the path and name of the help data file your system. */
/* This options requires text processing!                       */
/****************************************************************/

#define CLP_HELP 1

#if CLP_HELP

#define HELP_DEFAULT "clips.hlp"

#endif

/****************************************************************/
/* EDITOR: If this flag is turned on, an integrated EMACS style */
/*   editor can be called directly from CLIPS                   */
/****************************************************************/

#define  CLP_EDIT  1

#if GENERIC || MAC_LSC                  
#undef CLP_EDIT                         /* Editor can't be used */
#define  CLP_EDIT  0                    /* with Generic or Mac  */
#endif

/****************************************************************/
/* RULE COMPILER: If this flag is turned on, you may compile    */
/*   CLIPS rules to C code for a run-time executable.           */
/****************************************************************/

#define  CLP_RULE_COMP  0

/****************************************************/
/* RUN_TIME:  Produces a run-time module for CLIPS. */
/****************************************************/

#define RUN_TIME 0

/****************************************************/
/* DEFFACTS_CONSTRUCT:  Determines whether deffacts */
/*   construct is included.                         */
/****************************************************/

#define DEFFACTS_CONSTRUCT 1

/*************************************************/
/* DEFTEMPLATES:  Determines whether deftemplate */
/*   construct is included.                      */
/*************************************************/

#define DEFTEMPLATES 1

/*************************************************************/
/* SAVE_FACTS:  Allows inclusion of the functions save-facts */
/*   and load-facts.                                         */
/*************************************************************/

#define SAVE_FACTS 1

/******************************************************************/
/* STUDENT: Produces a version for use in teaching environments.  */
/*   Limits salience values to -2 to +2 and does not allow if and */
/*   while expressions.                                           */
/******************************************************************/

#define STUDENT 0

/*******************************************/
/* BASIC_IO: Includes printout, fprintout, */
/*   read, open, and close functions.      */
/*******************************************/

#define BASIC_IO 1

/***************************************************/
/* EXT_IO: Includes format and readline functions. */
/***************************************************/

#define EXT_IO 1

/************************************************/
/* STRING_FUNCTIONS: Includes string functions: */
/*   str-length, str-compare, upcase, lowcase,  */
/*   sub-string, str-index, and eval.           */
/************************************************/

#define STRING_FUNCTIONS 1

/*********************************************/
/* MULTIFIELD_FUNCTIONS: Includes multifield */
/*   functions:  mv-subseq, mv-delete,       */
/*   mv-append, str-explode, str-implode.    */
/*********************************************/

#define MULTIFIELD_FUNCTIONS 1

/**********************************************************************/
/* ART_CONSTRUCTS: Provides dummy ART constructs such as defrelation  */
/*   and defschema which allow ART constructs to be read from a file, */
/*   but not interpreted. Useful when converting in either direction. */
/**********************************************************************/

#define ART_CONSTRUCTS 1

/**********************************************************************/
/* BREAKPOINTS:                                                       */
/**********************************************************************/

#define BREAKPOINTS 1

/***************************************************************************/
/* MAC_SYSTEM: If using MAC_LSC and this flag is set, then SystemTask will */
/*   be called between rule firings. Allows Desk Accessories such as alarm */
/*   clock to continue working while a CLIPS program is running. Will slow */
/*   execution a little bit, but might allow network software among others */
/*   to work.                                                              */
/***************************************************************************/

#define MAC_SYSTEM 1

/************************************************************************/
/* BLOCK_MEMORY: Causes memory to be allocated in large blocks. MAC_LSC */
/*   option will also cause memory to be allocated in large blocks.     */
/*   INITBUFFERSIZE and BUFFERSIZE should both be set to less than the  */
/*   maximum size of a signed integer. On a 16-bit machine, they should */
/*   be less than 32768.                                                */
/************************************************************************/

#define BLOCK_MEMORY 0

#if MAC_LSC
#undef BLOCK_MEMORY
#define BLOCK_MEMORY 1
#endif

#if BLOCK_MEMORY

#define INITBUFFERSIZE 8192   
#define BUFFERSIZE 8192      

#endif

/*************************************************************************/
/* BLOAD_ONLY:      Enables bload command and disables the load command. */
/* BLOAD:           Enables bload command.                               */
/* BLOAD_AND_BSAVE: Enables bload, and bsave commands.                   */
/*************************************************************************/

#define BLOAD_ONLY      0
#define BLOAD           0
#define BLOAD_AND_BSAVE 1

/*****************************************************************/
/* WINDOW_INTERFACE : Set this flag if you are recompiling the   */
/*   IBM-PC MS-DOS Window Interface or the Macintosh LSC Window  */
/*   Interface. Currently, when enabled, this flag disables the  */
/*   more processing used by the help system. Note that in order */
/*   to remake IBM-PC window interface, you MUST have the Turbo  */
/*   C Compiler (V1.5 or higher) or the Microsoft C Compiler     */
/*   (V5.1 or  higher) and Aspen Scientific's Curses Screen      */
/*   Management Package (V4.0 or better). In order to remake the */
/*   Macintosh window interface, you must have the LSC compiler. */
/*****************************************************************/

#define WINDOW_INTERFACE 0
