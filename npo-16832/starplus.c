




	/*****************************************************************
	 * starplus.c			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * This file contains definitions for three built-in functions of
	 *   STAR which tend to be machine-dependent in nature.  These
	 *   are the STAR commands "system", which passes a command
	 *   string along to the operating system of the host computer,
	 *   "suspend", which saves an image of the currently executing
	 *   process in a file for subsequent reentry at a later time,
	 *   and "exit", which terminates a session with STAR.
	 *
	 * The definitions in this file are set up so as to work on a
	 *   SUN Microsystems workstation.  If STAR is to be installed on
	 *   a UNIX system other than the SUN or on a non-UNIX system,
	 *   the definitions for either or both of "system" and "suspend"
	 *   may fail to compile or work properly.  In the event of
	 *   compile errors or improper operation, it is possible to
	 *   produce a reduced version of STAR with either or both of
	 *   "system" and "suspend" transformed into "no-op" functions.
	 *   To transform "system" into a no-op function, remove the
	 *   definition for "STAR_SYSTEM" directly below this comment
	 *   before compiling "starplus.c".  To transform "suspend" into
	 *   a no-op function, remove the definition for "STAR_SUSPEND".
	 *
	 * The definition for "suspend" draws heavily upon borrowed code
	 *   for the routine "unexec" and thus includes the original
	 *   form and documentation for this code.  The "unexec" routine
	 *   has been affected in two ways in order for it to work in
	 *   SUN's version of UNIX:
	 *
	 *	(1) It was found that in line 230 it is necessary to
	 *	    round the data start address UP instead of down to
	 *	    the nearest segment boundary.  If STAR is to be
	 *	    installed on a UNIX system other than SUN, the "else"
	 *	    clause following line 230 may need to be altered in
	 *	    a similar manner.
	 *
	 *	(2) Also, it was found to be necessary to define the
	 *	    constant "TEXT_START" locally as indicated directly
	 *	    below.  This value is SUN-dependent and may vary for
	 *	    other UNIX systems.
	 *
	 * Also contained within this file are the "by reference"
	 *   versions of the built-in functions of STAR.  These functions
	 *   have names which end in "f_" as opposed to "_f" for the "by
	 *   value" versions.
	 *****************************************************************/

#define STAR_SYSTEM
#define STAR_SUSPEND

#define TEXT_START 0x8000


/* I. BORROWED CODE FOR UNEXEC ROUTINE. <><><><><><><><><><><><><><><><><><><>*/

#ifdef STAR_SUSPEND /* Include definition for "suspend" function. */

/* 
 * unexec.c - Convert a running program into an a.out file.
 * 
 * Author:	Spencer W. Thomas
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified somewhat since then.
 *
 * Synopsis:
 *	unexec (new_name, a_name, data_start, bss_start, entry_address)
 *	char *new_name, *a_name;
 *	unsigned data_start, bss_start, entry_address;
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If a_name is non-NULL, the symbol table will be taken from the given file.
 * 
 * The boundaries within the a.out file may be adjusted with the data_start 
 * and bss_start arguments.  Either or both may be given as 0 for defaults.
 * 
 * Data_start gives the boundary between the text segment and the data
 * segment of the program.  The text segment can contain shared, read-only
 * program code and literal data, while the data segment is always unshared
 * and unprotected.  Data_start gives the lowest unprotected address.  Since
 * the granularity of write-protection is on 1k page boundaries on the VAX, a
 * given data_start value which is not on a page boundary is rounded down to
 * the beginning of the page it is on.  The default when 0 is given leaves the
 * number of protected pages the same as it was before.
 * 
 * Bss_start indicates how much of the data segment is to be saved in the
 * a.out file and restored when the program is executed.  It gives the lowest
 * unsaved address, and is rounded up to a page boundary.  The default when 0
 * is given assumes that the entire data segment is to be stored, including
 * the previous data and bss as well as any additional storage allocated with
 * break (2).
 *
 * The new file is set up to start at entry_address.
 *
 * If you make improvements I'd like to get them too.
 * harpo!utah-cs!thomas, thomas@Utah-20
 *
 */

#ifndef emacs
#define PERROR perror
#define ERROR fprintf
#define ERRORF stderr,
#else
#include "config.h"
#define PERROR(file) error ("Failure operating on %s", file)
#define ERROR error
#define ERRORF
#endif

#ifdef USG
#include <sys/types.h>
#endif
#include <sys/param.h>
#include <stdio.h>
/* #include <sys/dir.h> */
#include <sys/stat.h>
#include <a.out.h>
#include <errno.h>

#ifndef USG /* USGlossage -- unexec does not work yet.  */
extern etext;
extern end;
extern edata;

static struct exec hdr, ohdr;
static int pagemask;

/* ****************************************************************
 * unexec
 *
 * driving logic.
 */
unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int new, a_out = -1;

  if (a_name && (a_out = open( a_name, 0 )) < 0)
    {
      PERROR (a_name);
      return -1;
    }
  if ((new = creat (new_name, 0666)) < 0)
    {
      PERROR( new_name );
      return -1;
    }

  pagemask = getpagesize () - 1;

  if (make_hdr( new, a_out, data_start, bss_start, entry_address) < 0 ||
      copy_text_and_data( new ) < 0 ||
      copy_sym( new, a_out ) < 0)
    {
      close (new);
      /* unlink( new_name );	    	/* Failed, unlink new a.out */
      return -1;	
    }

  close (new);
  if (a_out >= 0)
    close (a_out);
  mark_x (new_name);
  return 0;
}

/* ****************************************************************
 * make_hdr
 *
 * Make the header in the new a.out from the header in core.
 * Modify the text and data sizes.
 */
static int
make_hdr( new, a_out, data_start, bss_start, entry_address)
int new, a_out;
unsigned data_start, bss_start, entry_address;
{
    /* Get symbol table info from header of a.out file if given one. */
    if ( a_out >= 0 )
    {
	if ( read( a_out, &ohdr, sizeof hdr ) != sizeof hdr )
	{
	    PERROR( "Couldn't read header from a.out file" );
	    return -1;
	}

	if N_BADMAG( ohdr )
	{
	    ERROR( ERRORF "a.out file doesn't have legal magic number\n" );
	    return -1;
	}
	hdr.a_syms = ohdr.a_syms;
    }
    else
	hdr.a_syms = 0;			/* No a.out, so no symbol info. */

    /* Construct header from user structure. */
    hdr.a_magic = ZMAGIC;
    hdr.a_trsize = 0;
    hdr.a_drsize = 0;
    hdr.a_entry = entry_address;

    /* Adjust data/bss boundary. */
    if ( bss_start != 0 )
    {
	bss_start = (bss_start + pagemask) & ~pagemask;	      /* (Up) to page bdry. */
	if ( bss_start > sbrk (0))
	{
	    ERROR( ERRORF
		"unexec: Specified bss_start( %u ) is past end of program.\n",
		bss_start );
	    return -1;
	}
    }
    else
      bss_start = (sbrk (0) + pagemask) & ~pagemask;

    /* Adjust text/data boundary. */
    if (!data_start)
      data_start = (int) &etext;
#ifdef sun
    data_start = (data_start + (SEGSIZ - 1)) & ~(SEGSIZ - 1); /* (Up) to segment boundary. */
#else
    data_start = data_start & ~pagemask; /* (Down) to page boundary. */
#endif

    if ( data_start > bss_start )	/* Can't have negative data size. */
    {
	ERROR( ERRORF
	    "unexec: data_start(%u) can't be greater than bss_start( %u ).\n",
	    data_start, bss_start );
	return -1;
    }

    hdr.a_bss = sbrk (0) - bss_start;
    hdr.a_data = bss_start - data_start;
    hdr.a_text = data_start - TEXT_START;

    if ( write( new, &hdr, sizeof hdr ) != sizeof hdr )
    {
	PERROR( "Couldn't write header to new a.out file" );
	return -1;
    }
    return 0;
}

/* ****************************************************************
 * copy_text_and_data
 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data( new )
int new;
{
    int nwrite, ret;
    int end;
    int i;
    int ptr;
    char buf[80];
    extern int errno;

    lseek (new, (long) N_TXTOFF (hdr), 0);

    end = hdr.a_text + hdr.a_data;
    for (i = 0, ptr = TEXT_START; i < end;)
      {
	nwrite = 128;
	if (nwrite > end - i) nwrite = end - i;
	ret = write (new, ptr, nwrite);
	if (ret == -1 && errno == EFAULT)
	  {
	    lseek (new, (long) (N_TXTOFF (hdr) + i + nwrite), 0);
	  }
	else if (nwrite != ret)
	  {
	    sprintf(buf, "Write failure in unexec: ptr 0x%x size 0x%x nwrite 0x%x errno %d",
			 ptr, nwrite, ret, errno);
	    PERROR(buf);
	    return -1;
	  }
	i += nwrite;
	ptr += nwrite;
      }

    return 0;
}

/* ****************************************************************
 * copy_sym
 *
 * Copy the relocation information and symbol table from the a.out to the new
 */
static int
copy_sym( new, a_out )
int new, a_out;
{
    char page[1024];
    int n;

    if ( a_out < 0 )
	return 0;

    lseek( a_out, (long)N_SYMOFF(ohdr), 0 );	/* Position a.out to symtab.*/
    while ( (n = read( a_out, page, sizeof page )) > 0 )
    {
	if ( write( new, page, n ) != n )
	{
	    PERROR( "Error writing symbol table to new a.out" );
	    ERROR( ERRORF "new a.out should be ok otherwise\n" );
	    return 0;
	}
    }
    if ( n < 0 )
    {
	PERROR( "Error reading symbol table from a.out,\n" );
	ERROR( ERRORF "new a.out should be ok otherwise\n" );
    }
    return 0;
}

/* ****************************************************************
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x( name )
char *name;
{
    struct stat sbuf;
    int um;

    um = umask( 777 );
    umask( um );
    if ( stat( name, &sbuf ) == -1 )
    {
	PERROR ( "Can't stat new a.out" );
	ERROR( ERRORF "Setting protection to %o\n", 0777 & ~um );
	sbuf.st_mode = 0777;
    }
    sbuf.st_mode |= 0111 & ~um;
    if ( chmod( name, sbuf.st_mode ) == -1 )
	PERROR( "Couldn't change mode of new a.out to executable" );

}
#endif /* not USG */

#endif /* (#ifdef STAR_SUSPEND). */

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
#ifdef STAR_SYSTEM
  char *fname="system";
  terr_check(str1,STR,fname,1);
  system(s_start(str1));
#endif /* (#ifdef STAR_SYSTEM). */
  return(str1);
  }

struct unit_t *suspend_f(str1)
struct unit_t *str1;
  {
#ifdef STAR_SUSPEND
  char *fname="suspend";
  extern struct unit_t nil_g;
  extern unexec();
  extern char *argv0_g;
  int i;
  terr_check(str1,STR,fname,1);
  i = unexec(s_start(str1),argv0_g,0,0,TEXT_START);
  if(i != 0) return(&nil_g);
#endif /* (#ifdef STAR_SUSPEND). */
  return(str1);
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
\n     Use the characters of STRING1 to specify a command to\
\nthe operating system.  Returns STRING1 as the result."},
    {"suspend","",1,suspend_f,"C_SUSPEND_FUNCTION",BY_VALUE,
 "     (STRING1) => STRING\
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
