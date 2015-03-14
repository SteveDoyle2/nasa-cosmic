#include <stdio.h>
#include "crsv.h"

/* ===========  Functions defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern int      gettoken();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void     fb_printout();
extern void     rule_printout();
extern void     rel_printout();
extern void     ex_funcs_printout();
extern void     def_rel_printout();

extern void     send_message();

/* -----------------------------
 *  From the file: CRSVRSVD.C
 * -----------------------------
 */

extern int      check_ex_funcs();

/* -----------------------------
 *  From the file: CRSVMISC.C
 * -----------------------------
 */

extern int      check_inputs();
extern int      check_top_construct();
extern int      find_top_construct();
extern void     find_rht_paren();

/* -----------------------------
 *  From the file: CRSVUSR.C
 * -----------------------------
 */

extern void usr_startup();
extern void usr_new_file();
extern void usr_summary();

/* -----------------------------
 *  From the file: CRSVANAL.C
 * -----------------------------
 */

extern void analysis();

/* -----------------------------
 *  From the file: CRSVDEFREL.C
 * -----------------------------
 */

extern int process_defrelation();

/* -----------------------------
 *  From the file: CRSVEXF.C
 * -----------------------------
 */

extern int process_defexternal();

/* -----------------------------
 *  From the file: CRSVTMP.C
 * -----------------------------
 */

extern int process_deftemplate();

/* -----------------------------
 *  From the file: CRSVWRT.C
 * -----------------------------
 */

extern void create_def_rel_file();
extern void create_def_ext_file();

/* -----------------------------
 *  From the file: CRSVPROC.C
 * -----------------------------
 */

extern int process_rule();
extern int process_fact_block();

/* -----------------------------
 *  From the file: CRSVPRNT.C
 * -----------------------------
 */

extern void error_message();

/* -----------------------------
 *  From the file: CRSVMEM.C
 * -----------------------------
 */

extern void free_fb();
extern void free_rule();
extern void free_rel();
extern void free_ex_func();      
extern void free_var();
extern void free_def_rel();
extern void free_def_ex_func();
extern void free_a_rule();
extern void free_c_rule();
extern void free_a_rel();
extern long int     mem_used();
extern long int     max_mem_used();
extern long int     mem_requests();
extern int          release_mem();

/* -----------------------------
 *  From the file: CRSVHASH.C
 * -----------------------------
 */

extern void     init_symbol_table();

/* -----------------------------
 * From the file :  CRSVPRST.C
 * -----------------------------
 */

extern int process_trace_file();


/* ===========  Functions defined here for Global use  ================ */

void       end_crsv();


/* ===========  Functions defined here for internal use  ============== */

void         close_file();
FILE        *open_file();
void         init_globals();
int          process_main();


/* ===========  Variables defined here for Global use  ================ */

char        *cur_obj_name;          /* Name of current object           */
int          cur_line_num;          /* Current line number              */
FILE        *cur_file;              /* current file pointer             */
char        *cur_file_name;         /* Name of the current file         */
short        last_operation;        /* RETRACTION or ASSERTION          */
short        first_relation;
short        first_rule;
short        issued_error_message;  /* Set upon call to error_message   */

int          CHECK_RULES;           /* Check rules flag                 */
int          CHECK_RELATIONS;       /* Check relations flag             */
int          CHECK_EX_FLAG;         /* Check external functions flag    */
int          CHECK_COMMENTS;        /* Report on missing comments       */
int          VERBOSE;               /* Verbose printout flag            */
int          CHECK_STYLE;           /* Check code Style                 */
int          CREATE_DEFRELS;        /* Create Defrelations File flag    */
int          CHECK_DEFRELS;         /* Check defrelations flag          */
int          CHECK_DEBUG;           /* Display Debug info               */
int          ANALYZE_TRACE;         /* Analize trace file               */
int          LITERAL;               /* Field value checker              */
int          MAX_SINGLE_LIST = 5;       /* The length of a token list when  */
                                    /* the wildcard ?VARIABLE is        */
                                    /* substituted for the explicit     */
                                    /* list                             */
int          NUM_ERRORS;            /* Number of errors in a File       */
int          NUM_WARNINGS;          /* Number of warnings in a File     */
int          IBM_COM_LINE;

char         msg_buf[WORDLENGTH];   /* Global Message buffer            */

FACT_BLOCK  *fb_head = NULL;        /* First Element in Fact block list */
RULE        *rule_head = NULL;      /* First Element in Rule list       */
RELATION    *rel_head = NULL;       /* First Element in Relation list   */
EX_FUNC     *ex_funcs_head = NULL;  /* First Element in Ex Funcs list   */
VAR_PTR      retract_head = NULL;   /* First Element in Retract list    */
VAR_PTR      variable_head = NULL;  /* First Element in Variable list   */
DEF_REL     *def_rel_head = NULL;   /* First Element in Defrelation     */
                                    /* list                             */
DEF_EXT     *def_ext_head = NULL;   /* First Element in Defexternal     */
                                    /* list                             */
DEF_TMP     *def_tmp_head = NULL;   /* First Element in Deftemplate     */
                                    /* list                             */
ACT_REL_PTR  act_rel_head = NULL;   /* first node in trace relation     */
                                    /* list                             */
ACT_R_PTR    act_rule_head = NULL;  /* first node in trace rule list    */
COMP_R_PTR   comp_rule_head = NULL; /* first node in trace list of      */
                                    /* rules in the system              */
VAR_PTR      not_variable_head = NULL;  

/* ===========  Variables defined Externally but used here  =========== */

/* -----------------------------
 *  From the file: CRSVTKN.C
 * -----------------------------
 */

extern float  TKNNUMBER;                    /* gettoken number return   */
extern char  *TKNWORD;                      /* gettoken string storage  */

#if IBM_TBC

/* -----------------------------
 *  For the Turbo C Linker
 * -----------------------------
 */

extern unsigned _stklen = 24576;             /* Set stack size to 8K    */

#endif

/* ===========  Variables defined here for internal use  ============== */


/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  main
 *
 *  PURPOSE:  This is the main routine for the program.
 *            All processing is done through here.
 *
 *  INPUTS :  Two, C standard argc and argv. 
 *
 *  RETURNS:  Nothing
 *
 *  NOTES  :  On the Macintosh, picking up argc and argv 
 *            doesn't mean much.
 * -------------------------------------------------------------
 */

main(argc, argv)
int argc;
char *argv[];
{
   char   *def_rel_file;	            /* Name for defrelations file */
   char   *trace_file_name;       	    /* Name for the trace_file    */
   char   c_file_list[MAX_FILES][MAX_NAME]; /* only used when IBM_COM_LINE is true */
   int    rtn;
   int    exit_flag,i;

#if MAC_LSC
   MacCRSVInterface();    /* On the Mac, call the interface (never returns!) */
#endif

#if IBM_TBC || IBM_MSC
   if(argc == 1)
    {
      IBM_COM_LINE = FALSE;
      PC_CRSV_interface();   /* On the PC, call the interface (never returns!) */
    }
   else
      IBM_COM_LINE = TRUE;
#endif

   /*======================================*/
   /* For Generic Versions of this program */
   /*======================================*/

   exit_flag = NO;
   while(exit_flag IS_NO) DO

      /*==================================*/
      /* Check user inputs and file names */
      /*==================================*/

      rtn = check_inputs(&argc, argv, &def_rel_file, &trace_file_name);

      IF(rtn IS_ERROR) THEN

         /*===========================================*/
         /* If inputs invalid, or no files, then exit */
         /*===========================================*/

         exit_flag = YES;
      END_IF

      IF(exit_flag IS_NO) THEN

         /*===========================*/
         /* Call Main processing loop */
         /*===========================*/

/* The following code is for those who do not want to use the interface and recompile the codes */
/* This is not clean, but this is the only way I could think of, at this time, to over come the problem */
/* Recompiling and relinking the code could take quite a long time, so I guess we have to sacrify the speed a little bit*/

         if(IBM_COM_LINE)
          {
             argc--;
             if(argc > MAX_FILES)
	      {
		sprintf(msg_buf,"Too many files, limit is %d\n",MAX_FILES);
                error_message(ERROR,msg_buf);
                send_message("NOTE!!! If you wish to have more files\n");
	        send_message("Please change the flag to GENERIC(crsv.h) and recompile and relink the files - Use the makefile provided\n");
		end_crsv();
              }
	     for(i = 0; i < argc ; i++)
	      {
                 if(strlen(argv[i + 1]) > (MAX_NAME - 1))
		   {
		     sprintf(msg_buf,"File %s is too long, limit is %d\n",argv[i],MAX_NAME);
                     error_message(ERROR,msg_buf);
                     send_message("NOTE!!! If you wish to have the longer file name\n");
	             send_message("Please change the flag to GENERIC(crsv.h) and recompile and relink the files - Use the makefile provided\n");
		     end_crsv();
		   }
                 strcpy(c_file_list[i],argv[i + 1]);
              }
              exit_flag = process_main(argc, c_file_list,def_rel_file, trace_file_name);
          }
	 else
           exit_flag = process_main(argc - 1, argv + 1, 
                                  def_rel_file, trace_file_name);
      END_IF

   END_WHILE

   end_crsv();
}


/* ===================================================================
 *                       Main Processing Loop
 * ===================================================================
 */

int process_main(num_files, file_list, def_rel_file, trace_file_name)
int    num_files;
#if MAC_LSC || IBM_TBC || IBM_MSC
char file_list[MAX_FILES][MAX_NAME];
char def_rel_file[MAX_NAME];
char trace_file_name[MAX_NAME];
#else
char   **file_list;
char   *def_rel_file;
char   *trace_file_name;
#endif
{
   int    flag = 0;
   int    i;
   int    token, rtn;

   /*===========================*/
   /*  Do initialization stuff  */
   /*===========================*/

   send_message("\n           CRSV Version 1.2 (5/89)",NO);
   send_message("\nCLIPS Cross Reference, Style and Verification Tool\n",NO);

   init_globals();
   usr_startup();

   /*=====================================*/
   /*  For each file in the file list...  */
   /*=====================================*/

   for(i = 0; i < num_files ;   i++) DO

      /*===============================*/
      /*  Set up file processing info  */
      /*===============================*/

      cur_file_name = file_list[i];           /* Set global file name */
      cur_line_num  = 1;                      /* and line count */

      /*=================*/
      /*  Open the file  */
      /*=================*/

      cur_file = open_file(cur_file_name);
      IF(cur_file EQ NULL) THEN               /* If error opening file... */
         sprintf(msg_buf,"Error opening file \"%s\"", cur_file_name);
         error_message(ERROR,msg_buf);
      ELSE
         usr_new_file(cur_file_name);            /* and tell usr function */

         issued_error_message = OFF;

         IF(VERBOSE IS_ON) THEN
            send_message
         ("\n\n========================  PROCESSING FILE  =======================",NO);
            sprintf(msg_buf, "\n\nFILENAME: %s", cur_file_name);
            send_message(msg_buf,NO);
         END_IF

         /*==========================*/
         /*  Start reading the file  */
         /*==========================*/

         token = gettoken(cur_file);

         while(token NEQ STOP) DO
            cur_obj_name = "none";

            /*===========================*/
            /*  Find a def... something  */
            /*===========================*/

            IF(flag NEQ RESTART) THEN
               rtn = check_top_construct(cur_file, token);
            ELSE
               rtn = find_top_construct(cur_file, token);
            END_IF

            /*================================*/
            /*  Process the def... something  */
            /*================================*/

            IF(rtn EQ DEFFACTS) THEN
               flag = process_fact_block();
            ELSE_IF(rtn EQ DEFRULE)
               flag = process_rule();
            ELSE_IF(rtn EQ DEFRELATION)
               flag = process_defrelation();
            ELSE_IF(rtn EQ DEFEXTERNAL)
               flag = process_defexternal();
            ELSE_IF(rtn EQ DEFTEMPLATE)
               flag = process_deftemplate();
            ELSE_IF(rtn EQ ERROR)
               flag = RESTART;
            END_IF

            token = gettoken(cur_file);
         END_WHILE

         IF(VERBOSE IS_ON OR issued_error_message IS_ON) THEN
            send_message
 ("\n\n===================  FINISHED PROCESSING FILE  ===================\n",NO);
         END_IF
         close_file(cur_file);
      END_IF
   END_FOR
   
  /*========================*/
  /*  Process Trace File    */
  /*========================*/
     
   IF(ANALYZE_TRACE IS_ON)THEN
      issued_error_message = OFF;

      IF (VERBOSE IS_ON) THEN
         send_message
    ("\n\n========================  PROCESSING FILE  =======================",NO);
         sprintf(msg_buf, "\n\nFILENAME: %s", trace_file_name);
         send_message(msg_buf,NO);
      END_IF

      process_trace_file(trace_file_name);

      IF(VERBOSE IS_ON OR issued_error_message IS_ON) THEN
         send_message
  ("\n\n===================  FINISHED PROCESSING FILE  ===================\n",NO);
      END_IF
   END_IF
   
  /*==================*/
  /*  Final analysis  */
  /*==================*/
     
   analysis(fb_head, rule_head, rel_head, ex_funcs_head, act_rule_head,
      act_rel_head, def_rel_head, def_ext_head);

  /*=========================================*/
  /*  Print out cross references, as needed  */
  /*=========================================*/
     
   IF(CHECK_RULES IS_ON) THEN
      fb_printout(fb_head);
      rule_printout(rule_head, act_rule_head);
   END_IF

   IF(CHECK_RELATIONS IS_ON) THEN
      rel_printout(rel_head);
   END_IF

   IF(CHECK_EX_FLAG IS_ON) THEN
      ex_funcs_printout(ex_funcs_head);
   END_IF

  /*===================================================*/
  /*  Create defrelations and defexternals, if needed  */
  /*===================================================*/
     
   IF(CREATE_DEFRELS IS_ON) THEN
      sprintf(msg_buf, "\n\nStoring definitions in file: %s\n", 
              def_rel_file);
      send_message(msg_buf,NO);
      create_def_rel_file(rel_head, def_rel_file);
      create_def_ext_file(ex_funcs_head, def_rel_file);
   END_IF
   
   IF(CHECK_DEBUG IS_ON) THEN
      def_rel_printout(def_rel_head);
   END_IF

   usr_summary(fb_head, rule_head, rel_head, ex_funcs_head);

   send_message
  ("\n\n====================  CRSV ANALYSIS COMPLETED  ===================\n",NO);

   IF(CHECK_DEBUG IS_ON) THEN
      sprintf(msg_buf, "\n\nCurrent mem used: %ld", mem_used());
      send_message(msg_buf,NO);
      sprintf(msg_buf, "\nMax     mem used: %ld", max_mem_used());
      send_message(msg_buf,NO);
   END_IF      

   send_message("\n",NO);

   return(YES);
}

/* ===================================================================
 *                       Misc. Functions
 * ===================================================================
 */

/*
 * -------------------------------------------------------------
 *  NAME   :  open_file
 *
 *  PURPOSE:  This function opens a file. Prints an
 *            error message if the file can't be opened.
 *
 *  INPUTS:   A single argument, a string pointer to the 
 *            name of the file to be opened.
 *
 *  RETURNS:  An integer, ERROR (-1) if file can't be opened,
 *            otherwise, OK (0).
 * -------------------------------------------------------------
 */

FILE *open_file(file_name)
char *file_name;
{
   FILE *fp;
   
#if MAC_LSC
   GotoFileDirectory(file_name);
#endif

   fp = fopen(file_name, "r");
   
   return(fp);
}

/*
 * -------------------------------------------------------------
 *  NAME   :  close_file
 *
 *  PURPOSE:  This function closes a file.
 *
 *  INPUTS :  A single argument, a FILE pointer to the 
 *            file to be closed.
 *
 *  RETURNS:  Nothing useful.
 * -------------------------------------------------------------
 */
 
void close_file(file)
FILE *file;
{
   (void)fclose(file);
}

/* ======================================================================= */

/*
 * -------------------------------------------------------------
 *  NAME   :  init_globals
 *
 *  PURPOSE:  This function sets all global data to something
 *            appropriate for starting up or looping through
 *            multiple runs of the program.
 *
 *  INPUTS :  None. 
 *
 *  RETURNS:  Nothing
 * -------------------------------------------------------------
 */

void init_globals()
{
  /*===========================*/
  /* Init the global variables */
  /*===========================*/

   cur_file_name    = "none";
   cur_obj_name     = "none";
   cur_line_num     = 1;
   last_operation   = ERROR;
   
   NUM_ERRORS = 0;
   NUM_WARNINGS = 0;

  /*======================*/
  /* Clear all data lists */
  /*======================*/

   free_fb(fb_head);
   free_rule(rule_head);
   free_rel(rel_head);
   free_var(retract_head);
   free_ex_func(ex_funcs_head);
   free_def_rel(def_rel_head);
   free_def_ex_func(def_ext_head);
   free_var(variable_head);
   free_a_rel(act_rel_head);
   free_a_rule(act_rule_head);
   free_c_rule(comp_rule_head);

   fb_head          = NULL;
   rule_head        = NULL;
   rel_head         = NULL;
   retract_head     = NULL;
   ex_funcs_head    = NULL;
   def_rel_head     = NULL;
   def_ext_head     = NULL;
   variable_head    = NULL;
   act_rel_head     = NULL;
   act_rule_head    = NULL;
   comp_rule_head   = NULL;

  /*=============================*/
  /* Clear the symbol hash table */
  /*=============================*/

   init_symbol_table();
   
   IF(CHECK_DEBUG IS_ON) THEN
      release_mem(-1);
      sprintf(msg_buf, "\n\nCurrent mem used: %ld", mem_used());
      send_message(msg_buf,NO);
      sprintf(msg_buf, "\nMax     mem used: %ld", max_mem_used());
      send_message(msg_buf,NO);
   END_IF
}
   
void end_crsv()
{
   exit(1);
}
