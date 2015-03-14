#include <stdio>
#include <setjmp>
#include <ctype>
#include <rmsdef>

#define capitalch toupper

#define TEMPWITHFILE "SYS$SCRATCH:ZED.TMP"

#define INPUT  0
#define OUTPUT 1
#define MAXINT       2147483647         /*  2**31 - 1  */
#define ENDSTREAMCH  EOF
#define BYTESPERWORD 4
#define MAXSTRLENGTH 604                                      /* Maximum MXLL */
#define FILENAMELENGTH 255
#define MAX_STACK_LEVEL 10
#define MAXGLOBEXEC 604  /* Max. # of times a global can be applied to a line */
#define noutput  5    /* default no of lines output when output queue is full */
#define combsize 400                                /* size of command buffer */
#define clbsize  (MAXSTRLENGTH  / BYTESPERWORD) + 1
                                      /* size of current line buffer in words */
#define lscbsize 12 + 2 * (MAXSTRLENGTH  / BYTESPERWORD)
                                        /* size of last string command buffer */
#define csebsize 100              /* size of current search expression buffer */
#define numbuff 4                                   /* number of text buffers */
#define tosource 0
#define fromsource -1
#define altto -2                                                /* for TO /s/ */
#define altfrom -3                                            /* for FROM /s/ */
#define namesize 1 + (4 / BYTESPERWORD)

#define null 0                                         /*  null pointer value */

#define endmask    16
#define newmask    4
#define plusmask    2
#define movmask    8
#define minusmask   1
#define ormask      28
#define newmovmask  12
#define endplusmask  18

#define PLUSSYM  -1
#define MINUSSYM -2
#define ENDSYM  -3
#define CURRSYM   -4
#define OTHERSYM -5

/* For a chain of strings preceded by link words */

struct gen_rec
{
  struct gen_rec *nextg;
  char *stringb;
};

struct cur_rec 
{
      struct cur_rec *fptr;
      struct cur_rec *bptr;
      int lineno;
      int symb;
      char controlch;
      char *stringb;
};
#define minlinesize 6

struct com_rec
{
      int name;
      struct com_rec *arg1;
      struct com_rec *arg2;
      struct com_rec *nextcom;
};

struct glob_rec
{
      int gnumber;
      int gcount;
      int genabled;
      struct com_rec *gcom;
      struct glob_rec *gnext;
};

#define gsize 5         /*  This is the size of the g vector */

struct proc_rec
{
      char *pname;
      struct com_rec *pbody;
      struct proc_rec *pnext;
};

#define QN 0
#define QU 1
#define QW 2
#define QS 3
#define QL 4
#define QE QL
#define QP 5
#define QC 6
#define QWI 7
#define QWL 8
#define QWR 9
#define QINT 10
#define QSEP 11
#define QSTR 12

struct se_rec
{
      struct se_rec *SESEQ;
      int *SEQS;
      int SEAND;
      struct se_rec *SENEXT;
};

#define SESIZE 4

   /*****************************************************************
    *     Commands are assigned numbers by the following rationales *
    *                                                               *
    *   imask  independent of source commands                       *
    *   rmask  closes current line buffer                           *
    *   vmask  verifies current line if any previous request made   *
    *   cmask  repeatable commands                                  *
    *   gmask  opens current line buffer                            *
    *   smask  command to be copied into last string command buffer *
    *   emask  search expression argument to be copied             *
    ****************************************************************/

#define imask   64
#define gmask  128
#define vmask  256
#define rmask  512
#define cmask 1024
#define smask 2048
#define emask 4096


#define CQM	1
#define CFLUSH	2
#define CUNDO	3
#define CTO	4
#define CDBUFF	5
#define CSHBUFF	6
#define CTBUFF	7
#define CFROM	8
#define CIFEOF	9
#define CULEOF	10
#define CELSE	11
#define CEM	12
#define CCOLS   13
#define CUTEOF	16
#define CH      17                         /* Not imask because it might be . */

#define CINT   imask + 1
#define CSTOP  imask + 2
#define CCOMM  imask + 3
#define CHELP  imask + 4
#define CDEBUG imask + 5
#define CWORD  imask + 6
#define CZ     imask + 7
#define CC     imask + 8
#define CRPT   imask + 15
#define CV     imask + 16
#define CVN    imask + 17
#define CVT    imask + 18
#define CWARN  imask + 19
#define CERRSTOP imask + 20
#define CFN    imask + 21
#define IMFILE imask + 22
#define IMTEXT imask + 23
#define IMINB  imask + 24
#define IMBUFF imask + 25
#define IMCOPY imask + 26
#define CVE imask + 27
#define CCG imask + 28
#define CDG imask + 29
#define CEG imask + 30
#define CGA imask + 31
#define CGB imask + 32
#define CGE imask + 33
#define CVI imask + 34
#define CVG imask + 35
#define CSHG    imask + 36
#define CCPROC  imask + 37
#define CPROC   imask + 38
#define CSHPROC imask + 39
#define CMXLL   imask + 40
#define CDUMMY  imask + 41
#define CSHD    imask + 42
#define CSHS    imask + 43
#define CSHF    imask + 44
#define CQ      imask + 45
#define CVX     imask + 46
#define CTR     imask + 47
#define CAGP    imask + 48
#define CCS     imask + 49
#define CTAB    imask + 50
#define CDETAB  imask + 51
#define CX      imask + 52

#define CSHC gmask + 1
#define CLCL gmask + 2
#define CUCL gmask + 3
#define CDOL gmask + 4
#define CPER gmask + 5
#define CBRA gmask + 6
#define CKET gmask + 7
#define CDEL gmask + 8
#define CSPC gmask + 9
#define CPR  gmask + 10
#define CPA  gmask + 11
#define CPB  gmask + 12
#define CCC  gmask + 13

#define CT  rmask + 1
#define CTL rmask + 2
#define CW  rmask + 3

#define CM     rmask + vmask + 1
#define CDREST rmask + vmask + 3
#define CSA    rmask + vmask + 7
#define CSB    rmask + vmask + 8
#define CEQ    rmask + vmask + 9
#define CR     rmask + vmask  + 10

#define CIC cmask + rmask + 1
#define CIS cmask + rmask + 2

#define CN  cmask + rmask + vmask + 1
#define CP  cmask + rmask + vmask + 2
#define CCL cmask + rmask + vmask + 3
#define CD  cmask + rmask + vmask + 4
#define CI  cmask + rmask + vmask + 5

#define CDO cmask + imask + 1

#define CRLSC cmask + gmask + 1

#define CDFA smask + gmask + 1
#define CDFB smask + gmask + 2
#define CDTA smask + gmask + 3
#define CDTB smask + gmask + 4

#define CLC smask + gmask + 5
#define CUC smask + gmask + 6

#define CA smask + cmask + gmask + 1
#define CB smask + cmask + gmask + 2
#define CE smask + cmask + gmask + 3
#define CAP smask + cmask + gmask + 4
#define CBP smask + cmask + gmask + 5
#define CEP smask + cmask + gmask + 6

#define CELIF emask + 12
#define CELUL emask + 13
#define CWH emask + 14
#define CUT emask + 15

#define CON emask + imask + 34

#define CIF emask + cmask + rmask + 3
#define CUL emask + cmask + rmask + 4

#define CF emask + cmask + rmask + vmask + 6
#define CBF emask + cmask + rmask + vmask + 7
#define CDF emask + cmask + rmask + vmask + 8

/******************** BCPL GLOBALS ***********************************/

int debug;

struct cur_rec *currentl;          /* pointer to current line in output queue */
struct cur_rec *(*lowestl);
                            /* pointer to the lowest line in the output queue */
struct cur_rec *(*highestl);                    /* pointer to the highest one */
int lineinput;                  /* TRUE if any lines have been read from from */
int linesinput;                                 /* no of lines read from from */
FILE *fromf;                            /*  These are pointers to i/o streams */
FILE *altfromf, *curfromf;
FILE *withf;
FILE *verf;
FILE *tof;
FILE *alttof, *curtof;
FILE *eprintf;                               /* File to send edit commands to */
char *tofilename;
char *alttofilename;
char *fromfilename;
int from_file_created;     /* flag saying we created the source to start with */
FILE *user_stack[MAX_STACK_LEVEL];        /* For command file buffer pointers */
int user_level;                                              /* Stack pointer */
int file_is_ftn;
char *rat, *rfm;

FILE *cur_input_fd;
FILE *cur_output_fd;

char *endinsertstr;             /* string to end a series of input lines with */
int halt_lineno;                       /* line number set by the HALT command */
int cur_col;               /* current character pointer position (start at 1) */
char withch;                     /* last character taken from the with stream */
int withcol;                                      /* column of current withch */
int clevel;                                      /* current level of commands */
char *combuff;                                   /* pointer to command buffer */
char *ptrcbuff;                        /* pointer to limit of used part of it */
char *limcbuff;                                       /* pointer to end of it */

struct com_rec *currcom;                  /* pointer to current command group */
char *wordset;                     /* The characters which can occur in words */
int *fqst;                  /* these five global variables are arguments used */
char *fst;                                              /* by procedure match */
char *fqs;
int ffch;
int flch;

int tt_chan;                   /* terminal channel number for ^C AST handling */
int interrupt;             /* flag that's set when there's a ^C AST interrupt */
int mxll;               /* holds maximum allowed input length of source lines */
int swvg;                 /* those globals beginning with sw hold the setting */
int swv;                                               /* of the given switch */
int swvi;
int swvn;
int swvt;
int swve;
int swvx;
int swerrstop;
int swwarn;
int swfn;
int swtr;
int swcs;
int swx;
int swdetab;
int tabvalue;
int not_yet_interactive;                /* TRUE until we set interactive TRUE */
int interactive;                /* TRUE if with and ver are interactive files */
int verreq1;          /* requests verification before current line is changed */
int verreq2;              /* requests verification on reaching end of current */
                                                          /* line of commands */

struct se_rec *csebuff;        /* pointer to current search expression buffer */
int cseopen;                    /* TRUE if a current search expression exists */
struct com_rec *lscbuff;         /* pointer to the last string command buffer */
int lscopen;                          /* TRUE if a last string command exists */
int currto;                                                /* current to file */
int currfrom;                                            /* current from file */

struct proc_rec *proclist;           /* pointer to list of current procedures */
struct glob_rec *glist;         /* pointer to list of current global commands */
struct glob_rec *endglist;                             /*  pointer to its end */
int *cpysp;             /* pointer to buffer command seq is being copied into */
int *limcpysp;                                        /* pointer to end of it */
int *ptrcpysp;                    /* pointer to first not yet used word in it */

int goutdisabled;                /* if TRUE globals and outputnl are disabled */
struct com_rec *prescomseq;    /*  pointer to the currently executing com seq */

int clboccupied;                   /* TRUE if current line buffer is occupied */
char *clbuff;                               /* pointer to current line buffer */

jmp_buf jmpenv;
