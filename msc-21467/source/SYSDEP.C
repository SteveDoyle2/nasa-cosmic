/*   CLIPS Version 4.30   4/25/89 */

#include <stdio.h>

#include "setup.h"
#include "clips.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   float                   clips_time();
   char                   *genalloc();
   int                     genexit();
   int                     genfree();
   char                   *genrealloc();
   int                     init_clips();
   int                     init_streams();

#if TRACK_MEMORY
   float                   mem_used();
   float                   mem_requests();
#endif

   float                   my_system();
   int                     sysdep_inits();
   int                     system();
   int                     (*redraw_screen)() = NULL;
   int                     (*pause_env)() = NULL;
   int                     (*cont_env)() = NULL;

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/
     
   extern int              add_router();
   extern int              cl_print();
   extern int              fileexit();      
   extern int              filegetc();      
   extern int              fileprint();       
   extern int              fileungetc();     
   extern int              findfile();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static long int         mem_amount = 0;
   static long int         mem_calls = 0;

/*************************************************/
/* INIT_CLIPS: Performs initialization of CLIPS. */
/*************************************************/
#if RUN_TIME

init_clips()
  {
   init_streams();        /* Initalizes streams for I/O.       */
   sysdep_inits();        /* System dependent initializations. */   
#if DEFFACTS_CONSTRUCT
   set_up_deffacts();
#endif
  }
  
#else

init_clips()
  {
   init_streams();        /* Initalizes streams for I/O.       */
   init_symbol_table();   /* Initializes the hash table.       */
   sysdep_inits();        /* System dependent initializations. */
   sysfctns();            /* Define system functions.          */
   define_commands();     /* Define user interface functions.  */
   mathfctns();           /* Define math package functions.    */
   usrfuncs();            /* Define user functions.            */
#if (! BLOAD_ONLY)
   init_constructs();     /* Initializes constructs.           */
#endif
#if DEFFACTS_CONSTRUCT
   set_up_deffacts();
#endif
#if DEFTEMPLATES && (! BLOAD_ONLY)
   SetupDeftemplates();
#endif
   init_gen_ptrs();       /* Initializes function pointers.    */
   init_exprn_psr();      /* Initializes expression parsers.   */
  }

/***************************************************************/
/* SET_REDRAW_FUNCTION: Redraws the screen if clipswin is main */
/*                       or does nothing.                      */
/***************************************************************/
int set_redraw_function(fun_ptr)
  int (*fun_ptr)();
  {
   redraw_screen = fun_ptr;
  }
  
/***************************************************************/
/* SET_PAUSE_FUNCTION : Puts terminal in a normal state if     */
/*                      clipswin is main or does nothing.      */
/***************************************************************/
int set_pause_env_function(fun_ptr)
  int (*fun_ptr)();
  {
   pause_env = fun_ptr;
  }

/***************************************************************/
/* SET_CONT_ENV_FUNCTION: Returns terminal to special screen   */
/* interface state if clipswin is main or does nothing         */
/***************************************************************/
int set_cont_env_function(fun_ptr)
  int (*fun_ptr)();
  {
   cont_env = fun_ptr;
  }

/*****************************************************************/
/* REROUTE_STDIN: Reroutes stdin to read initially from the file */
/*                specified on the command line with -f option.  */
/*****************************************************************/
reroute_stdin(argc,argv)
int	argc;
char *argv[];
  {
   int i;

   /* If no arguments return */
   if (argc < 3)
     { return; }

   /* If argv was not passed then forget it */
   if (argv == NULL)
      return;

   for (i = 1 ; i < argc ; i++)
     {
      if (strcmp(argv[i],"-f") == 0)
        {
         if (i > (argc-1))
           {
            cl_print("werror","No file found for -f option\n");
            return;
           }
         else
           open_batch(argv[++i],TRUE);
        }
     }
  }


#endif

/***************************************************/
/* GENALLOC: A generic memory allocation function. */
/***************************************************/
char *genalloc(size)
  int size;
  {
   char *mem_ptr;

#if   BLOCK_MEMORY
   extern char *request_block();   

   mem_ptr = request_block(size);
   if (mem_ptr == NULL)
     { 
      release_mem(((size * 5 > 1024) ? size * 5 : 1024),TRUE);
      mem_ptr = request_block(size);
      if (mem_ptr == NULL) 
        {
         release_mem(-1,TRUE);
         mem_ptr = request_block(size);
         if (mem_ptr == NULL)
           {
            cl_print("werror","ERROR: out of memory\n");
            cl_exit(1);
           }
        }
     }
#else
   extern char *malloc();

   mem_ptr = malloc(size);
   if (mem_ptr == NULL)
     { 
      release_mem(((size * 5 > 1024) ? size * 5 : 1024),TRUE);
      mem_ptr = malloc(size);
      if (mem_ptr == NULL) 
        {
         release_mem(-1,TRUE);
         mem_ptr = malloc(size);
         if (mem_ptr == NULL)
           {
            cl_print("werror","ERROR: out of memory\n");
            cl_exit(1);
           }
        }
     }
#endif

#if TRACK_MEMORY
   mem_amount += size;
   mem_calls++;
#endif

   return(mem_ptr);
  }

/****************************************************/
/* GENFREE: A generic memory deallocation function. */
/****************************************************/
genfree(waste,size)
  char *waste;
  int size;
  {

#if    BLOCK_MEMORY
   if (return_block(waste,size) == -1)
     { 
      cl_print("werror","Release error in genfree\n");
      return(-1);
     }
#else
   free(waste);
#endif

#if TRACK_MEMORY
   mem_amount -= size;
   mem_calls--;
#endif

   return(0);
  }

/*********************************************************/
/* GENREALLOC() : Simple (i.e. dumb) version of realloc. */
/* Should be reimplemented later.                        */
/*********************************************************/
char *genrealloc(oldaddr,oldsz,newsz)
  char *oldaddr;
  unsigned oldsz,newsz;
  {
   char *newaddr;
   int i,limit;

   newaddr = genalloc(newsz);
   limit = (oldsz < newsz) ? oldsz : newsz;
   for (i = 0 ; i < limit ; i++)
     newaddr[i] = oldaddr[i];
   genfree(oldaddr,oldsz);
   return(newaddr);
  }
  
#if TRACK_MEMORY

/*******************************************/
/* MEM_USED:  Returns the amount of memory */
/*   currently allocated by CLIPS.         */
/*******************************************/
float mem_used()
  {
   return( (float) mem_amount);
  }

/***********************************************************/
/* MEM_REQUESTS:  Returns the number of outstanding memory */
/*   memory calls made through memory functions.           */
/***********************************************************/
float mem_requests()
  {
   return( (float) mem_calls);
  }
  
/******************************************/
/* UPDATE_MEM_USED:                       */
/******************************************/
update_mem_used(value)
  long int value;
  {
   mem_amount += value;
  }
  
/******************************************/
/* UPDATE_MEM_REQUESTS:                   */
/******************************************/
update_mem_requests(value)
  int value;
  {
   mem_calls += value;
  }

#endif

/**********************************************/
/* INIT_STREAMS:  Initializes output streams. */
/**********************************************/
init_streams()
  {
   int findfile(), fileprint(), fileexit(), filegetc(), fileungetc();
   int str_fnd(), str_getc(), str_ungetc();

   add_router("fileio",0,findfile,fileprint,filegetc,fileungetc,fileexit);
   add_router("string",0,str_fnd,NULL,str_getc,str_ungetc,NULL);
  }

/*************************************************************/
/* CLIPS_TIME: A function to return a floating point number  */
/*   which indicates the present time. Used internally by    */
/*   CLIPS for timing rule firings and debugging.            */
/*************************************************************/

#if CLP_TIME

#if   VMS
#include timeb
#endif

#if   IBM_MSC
#include <sys\types.h>
#include <sys\timeb.h>
#endif

#if   IBM_TBC
#include <bios.h>
#endif

#if IBM_ZTC
#include <time.h>
#endif

#if   UNIX_7
#include <sys/types.h>
#include <sys/timeb.h>
#endif

#if   UNIX_V
#include <sys/types.h>
#include <sys/times.h>
#endif
		
#endif

float clips_time()
  {
#if   CLP_TIME

#if   VMS || IBM_MSC ||  UNIX_7
   float sec, msec, time;
   int temp;
   struct timeb time_pointer;
	
   ftime(&time_pointer);
   temp = time_pointer.time;
   temp = temp - ((temp/10000) * 10000);
   sec  = (float) temp;
   msec = (float) time_pointer.millitm;
   return(sec + (msec / 1000.0));
#endif

#if   UNIX_V
   long t_int;
   float t;
   struct tms buf;

   t_int = times(&buf);
   t = (float) t_int / 60.0; 
   return(t);
#endif

#if   MAC_LSC
   unsigned long int result;
   
   result = TickCount();

   return((float) result / 60.0);
#endif

#if   IBM_TBC
   unsigned long int result;
   
   result = biostime(0,(long int) 0);

   return((float) result / 18.2);
#endif

#if IBM_ZTC
   clock_t result;

   result = clock();

   return((float) result / CLK_TCK);
#endif

#if IBM_LATTICE || GENERIC
   return(0.0);
#endif

#else

   return(0.0);            /* When CLIPS_TIME is not being used */

#endif                   
  }


/*****************************************************************/
/* MY_SYSTEM:  This function can be called from CLIPS.  It will  */
/*   form a command string from its arguments, and pass this     */
/*   string to the operating system.  As currently defined, this */
/*   function does nothing, however, code has been included      */
/*   which should allow this function to work under VAX VMS and  */
/*   UNIX compatible systems.                                    */
/*****************************************************************/

#if   IBM_MSC || IBM_ZTC
#include <process.h>
#endif

float my_system()
  {
   char comm_buff[256];
   int buff_index = 0;
   int numa, i, j;
   VALUE arg_ptr;
   char *str_ptr, next_char;

   comm_buff[0] = EOS;
   
   if ((numa = arg_num_check("system",AT_LEAST,1)) == -1) return(0.0);

   for (i = 1 ; i <= numa; i++)
     {
      runknown(i,&arg_ptr);
      if ((get_valtype(arg_ptr) != STRING) &&
          (get_valtype(arg_ptr) != WORD))
        {
         set_execution_error(TRUE);
         exp_type_error("system",i,"word or string");
         return(0.0);
        }
      
     str_ptr = arg_ptr.val.hvalue->contents;
     j = 0;
     while ((next_char = str_ptr[j++]) != EOS)
       { 
        if (buff_index < 255)
          { comm_buff[buff_index++] = next_char; }
        else
          {
           set_execution_error(TRUE);
           cl_print("werror","Command buffer overflow in system function");
           return(0.0);
          }
       }
     comm_buff[buff_index] = EOS;
    } 

#if VMS
   if (pause_env != NULL) (*pause_env)();
   vms_system(comm_buff);
   if (cont_env != NULL) (*cont_env)(1);
   if (redraw_screen != NULL) (*redraw_screen)();
#endif

#if   UNIX_7 || UNIX_V || IBM_MSC || IBM_TBC || IBM_ZTC
   if (pause_env != NULL) (*pause_env)();
   system(comm_buff);
   if (cont_env != NULL) (*cont_env)(1);
   if (redraw_screen != NULL) (*redraw_screen)();
#else

#if ! VMS
   cl_print("wdialog",
            "System function not fully defined for this system.\n");
#endif

#endif	

   return(1.0);
  }


#if   VMS
#include <descrip.h>
#include <ssdef.h>
#include <stsdef.h>

extern int LIB$SPAWN();

int vms_system(cmd)
  char *cmd;
  {
   long status, complcode;
   struct dsc$descriptor_s cmd_desc;

   cmd_desc.dsc$w_length = strlen(cmd);
   cmd_desc.dsc$a_pointer = cmd;
   cmd_desc.dsc$b_class = DSC$K_CLASS_S;
   cmd_desc.dsc$b_dtype = DSC$K_DTYPE_T;

   status = LIB$SPAWN(&cmd_desc,0,0,0,0,0,&complcode,0,0,0);
   if ((status == SS$_NORMAL) && ((complcode & STS$M_SUCCESS) != 0))
     { return 0; }
   else
     { return -1; }
  }
#endif
 
/**************************************************************/
/* The following two functions are provided to trap control-c */
/* in order to interrupt the execution of a program.          */
/**************************************************************/

#if ! WINDOW_INTERFACE

#if   VMS
#include signal
#endif

#if UNIX_V || UNIX_7
#include <signal.h>
#endif

#if IBM_TBC || IBM_MSC
#include <dos.h>
#endif

#if IBM_ZTC
#include <int.h>
#endif

#endif

sysdep_inits()
  {
#if ! WINDOW_INTERFACE

#if MAC_LSC && MAC_SYSTEM
   int call_st();
#endif

#if   VMS || IBM_ZTC || UNIX_V || UNIX_7
   int catch_ctrl_c();
#endif

#if   IBM_TBC || IBM_MSC
   void interrupt catch_ctrl_c();
#endif

#if MAC_LSC && MAC_SYSTEM
   add_exec_function("systemtask",call_st);
#endif

#if   VMS || UNIX_V || UNIX_7
   signal(SIGINT,catch_ctrl_c);
#endif

#if IBM_TBC
   /* CTRL-C Handler for DOS is at 0x23 -- DOS automatically
      restores this vector upon program exit */

   setvect(0x23,catch_ctrl_c);
#endif

#if IBM_MSC 
   _dos_setvect(0x23,catch_ctrl_c);
#endif

#if IBM_ZTC
   int_intercept(0x23,catch_ctrl_c,256);
#endif

#endif
  }
  
#if ! WINDOW_INTERFACE

#if MAC_LSC && MAC_SYSTEM
call_st()
  { SystemTask(); }
#endif

#if   VMS || UNIX_V || UNIX_7
catch_ctrl_c()
  {
   set_execution_error(TRUE);
   signal(SIGINT,catch_ctrl_c);
  }
#endif

#if   IBM_TBC || IBM_MSC
void interrupt catch_ctrl_c(bp,di,si,ds,es,dx,cx,bx,ax,ip,cs,flgs)
  {
   set_execution_error(TRUE);
  }
#endif

#if IBM_ZTC
catch_ctrl_c()
  {
   set_execution_error(TRUE);
  }
#endif

#endif

/******************************************/
/* GENEXIT:  A generic exit function.     */
/*   Error codes:                         */
/*    -1 - Normal exit                    */
/*     1 - Out of memory exit             */
/*     2 - Arbitrary limit violation exit */
/*     3 - Memory release error exit      */
/*     4 - Rule parsing exit              */
/*     5 - Run time exit                  */
/*     6 - Rule maintenance exit          */
/******************************************/
genexit(num)
  int num;
  {
   exit(num);
  }
 
#if ! RUN_TIME 
/***************************************************************/
/* clips_options: Lists the compiler flags that have been turned on. */
/***************************************************************/
clips_options()
  {
   if (arg_num_check("options",EXACTLY,0) == -1) return;
  
   cl_print("wdisplay","Machine type: ");
   
#if GENERIC
   cl_print("wdisplay","Generic ");
#endif
#if VMS
   cl_print("wdisplay","VAX VMS ");
#endif
#if UNIX_V
   cl_print("wdisplay","UNIX System V or 4.2BSD ");
#endif
#if UNIX_7
   cl_print("wdisplay","UNIX System III Version 7 ");
#endif
#if MAC_LSC
   cl_print("wdisplay","Apple Macintosh using LightSpeed C ");
#endif
#if IBM_MSC
   cl_print("wdisplay","IBM PC with Microsoft C");
#endif
#if IBM_LATTICE
   cl_print("wdisplay","IBM PC with Lattice C");
#endif
#if IBM_TBC
   cl_print("wdisplay","IBM PC with Turbo C");
#endif
#if IBM_ZTC
   cl_print("wdisplay","IBM PC with Zortech C");
#endif
cl_print("wdisplay","\n");

cl_print("wdisplay","Editor is ");
#if CLP_EDIT
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Extended math package is ");
#if EX_MATH
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Rule compiler is ");
#if CLP_RULE_COMP
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Block memory is ");
#if BLOCK_MEMORY
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Help system is ");
#if CLP_HELP
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Deffacts construct is ");
#if DEFFACTS_CONSTRUCT
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Deftemplate construct is ");
#if DEFTEMPLATES
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Text processing package is ");
#if CLP_TEXTPRO
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","String function package is ");
#if STRING_FUNCTIONS
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Multifield function package is ");
#if MULTIFIELD_FUNCTIONS
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Timing flag is ");
#if CLP_TIME
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Memory Tracking is ");
#if TRACK_MEMORY
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Run time module is ");
#if RUN_TIME
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Save facts command is ");
#if SAVE_FACTS
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Student option is ");
#if STUDENT
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Basic I/O is ");
#if BASIC_IO
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif
  
cl_print("wdisplay","Extended I/O is ");
#if EXT_IO
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","ART constructs is ");
#if ART_CONSTRUCTS
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

cl_print("wdisplay","Breakpoint function is ");
#if BREAKPOINTS
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif

#if MAC_LSC
cl_print("wdisplay","Macintosh system is ");
#if MAC_SYSTEM
  cl_print("wdisplay","ON\n");
#else
  cl_print("wdisplay","OFF\n");
#endif
#endif

cl_print("wdisplay","Bload capability is ");
#if BLOAD_ONLY
  cl_print("wdisplay","BLOAD ONLY");
#endif
#if BLOAD
  cl_print("wdisplay","BLOAD");
#endif
#if BLOAD_AND_BSAVE
  cl_print("wdisplay","BLOAD AND BSAVE");
#endif
#if (! BLOAD_ONLY) && (! BLOAD) && (! BLOAD_AND_BSAVE)
  cl_print("wdisplay","OFF ");
#endif
cl_print("wdisplay","\n");

cl_print("wdisplay","Window Interface flag is ");
#if WINDOW_INTERFACE
   cl_print("wdisplay","ON\n");
#else
   cl_print("wdisplay","OFF\n");
#endif
  }

#else
clips_options() {};
#endif
 


