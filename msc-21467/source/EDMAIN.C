/*   CLIPS Version 4.30   4/25/89 */

/*
 * This program is in public domain; written by Dave G. Conroy.
 * This file contains the main driving routine, and some keyboard processing
 * code, for the MicroEMACS screen editor.
 *
 * REVISION HISTORY:
 *
 * 1.0  Steve Wilhite, 30-Nov-85
 *      - Removed the old LK201 and VT100 logic. Added code to support the
 *        DEC Rainbow keyboard (which is a LK201 layout) using the the Level
 *        1 Console In ROM INT.
 *        See "rainbow.h" for the function key definitions.
 *
 * 2.0  George Jones, 12-Dec-85
 *      - Ported to Amiga.
 *
 * 2.1  Chris Culbert, 25-Jul-86
 *      - Ported to HP9000 computers and modified commands to look more
 *        like the Zmacs editor NASA uses on the Symbolics.
 *
 * 2.2  Bebe Ly, 09-Jan-87
 *      - Added functions for global search and replace, query search 
 *        and replace, and parenthesis matching.
 *
 *
 * 3.0  Chris Culbert, August 1987
 *      - Integrated with CLIPS tool. Added functions to do rule
 *        compiling and editor entry and exit clean up.
 *        Massive rearranging of code and general clean up.
 */

#include        <stdio.h>
#include        "ed.h"

#if     CLP_EDIT
#if     ! RUN_TIME

#if     VMS
#include        <ssdef.h>
#define GOOD    (SS$_NORMAL)
#endif

#ifndef GOOD
#define GOOD    0
#endif

#define EXIT	-999

int     currow;                         /* Working cursor row           */
int     curcol;                         /* Working cursor column        */
int     fillcol;                        /* Current fill column          */
int     thisflag;                       /* Flags, this command          */
int     lastflag;                       /* Flags, last command          */
int     curgoal;                        /* Goal column                  */
BUFFER  *curbp;                         /* Current buffer               */
WINDOW  *curwp;                         /* Current window               */
BUFFER  *bheadp = NULL;                 /* BUFFER listhead              */
WINDOW  *wheadp;                        /* WINDOW listhead              */
BUFFER  *blistp;                        /* Buffer list BUFFER           */
short   kbdm[NKBDM] = {CTLX|')'};       /* Macro                        */
short   *kbdmip;                        /* Input  for above             */
short   *kbdmop;                        /* Output for above             */
char    pat[NPAT];                      /* Pattern                      */
char    lastbufn[NBUFN];                /* Last buffer name             */

typedef struct  {
        short   k_code;                 /* Key code                     */
        int     (*k_fp)();              /* Routine to handle it         */
}       KEYTAB;

extern  int     ctrlg();                /* Abort out of things          */
extern  int     quit();                 /* Quit                         */
extern  int     ctlxlp();               /* Begin macro                  */
extern  int     ctlxrp();               /* End macro                    */
extern  int     ctlxe();                /* Execute macro                */
extern  int     fileread();             /* Get a file, read only        */
extern  int     filevisit();            /* Get a file, read write       */
extern  int     filewrite();            /* Write a file                 */
extern  int     filesave();             /* Save current file            */
extern  int     filename();             /* Adjust file name             */
extern  int     getccol();              /* Get current column           */
extern  int     gotobol();              /* Move to start of line        */
extern  int     forwchar();             /* Move forward by characters   */
extern  int     gotoeol();              /* Move to end of line          */
extern  int     backchar();             /* Move backward by characters  */
extern  int     forwline();             /* Move forward by lines        */
extern  int     backline();             /* Move backward by lines       */
extern  int     forwpage();             /* Move forward by pages        */
extern  int     backpage();             /* Move backward by pages       */
extern  int     gotobob();              /* Move to start of buffer      */
extern  int     gotoeob();              /* Move to end of buffer        */
extern  int     setfillcol();           /* Set fill column.             */
extern  int     setmark();              /* Set mark                     */
extern  int     swapmark();             /* Swap "." and mark            */
extern  int     forwsearch();           /* Search forward               */
extern  int     backsearch();           /* Search backwards             */
extern  int     frwsr();                /* Search forward and replace   */
extern  int     querysr();              /* Query replace                */
extern  int     bkwrdrpl();             /* Backward search and replace  */
extern  int     bkwrdcr();              /* Backward confirmed replace   */
extern  int     smatchb();              /* Search for matching bracket  */
extern  int     showcpos();             /* Show the cursor position     */
extern  int     nextwind();             /* Move to the next window      */
extern  int     prevwind();             /* Move to the previous window  */
extern  int     onlywind();             /* Make current window only one */
extern  int     splitwind();            /* Split current window         */
extern  int     mvdnwind();             /* Move window down             */
extern  int     mvupwind();             /* Move window up               */
extern  int     enlargewind();          /* Enlarge display window.      */
extern  int     shrinkwind();           /* Shrink window.               */
extern  int     listbuffers();          /* Display list of buffers      */
extern  int     usebuffer();            /* Switch a window to a buffer  */
extern  int     killbuffer();           /* Make a buffer go away.       */
extern  int     reposition();           /* Reposition window            */
extern  int     refresh();              /* Refresh the screen           */
extern  int     twiddle();              /* Twiddle characters           */
extern  int     tab();                  /* Insert tab                   */
extern  int     newline();              /* Insert CR-LF                 */
extern  int     indent();               /* Insert CR-LF, then indent    */
extern  int     openline();             /* Open up a blank line         */
extern  int     deblank();              /* Delete blank lines           */
extern  int     quote();                /* Insert literal               */
extern  int     backword();             /* Backup by words              */
extern  int     forwword();             /* Advance by words             */
extern  int     forwdel();              /* Forward delete               */
extern  int     backdel();              /* Backward delete              */
extern  int     kill_fwd();             /* Kill forward                 */
extern  int     yank();                 /* Yank back from killbuffer.   */
extern  int     upperword();            /* Upper case word.             */
extern  int     lowerword();            /* Lower case word.             */
extern  int     upperregion();          /* Upper case region.           */
extern  int     lowerregion();          /* Lower case region.           */
extern  int     capword();              /* Initial capitalize word.     */
extern  int     delfword();             /* Delete forward word.         */
extern  int     delbword();             /* Delete backward word.        */
extern  int     killregion();           /* Kill region.                 */
extern  int     copyregion();           /* Copy region to kill buffer.  */
extern  int     spawncli();             /* Run CLI in a subjob.         */
extern  int     spawn();                /* Run a command in a subjob.   */
extern  int     quickexit();            /* low keystroke style exit.    */
extern  int     gotoline();             /* goto a specific line #       */

extern  char   *malloc();
extern  char   *calloc();
extern  int     temp_quit();            /* Temporary exit from editor   */
extern  int     compile_region();	/* Compile a region of code     */
extern	int	compile_file();		/* Compile a whole file of code */
extern  int 	init_cmp_router(); 	/* Initialize new router        */
extern	int	kill_cmp_router();	/* remove new router		*/

/*
 * Command table.
 * This table  is *roughly* in ASCII order, left to right across the
 * characters of the command. This expains the funny location of the
 * control-X commands.
 */
KEYTAB  keytab[] = {
        CTRL|'@',               setmark,
        CTRL|'A',               gotobol,
        CTRL|'B',               backchar,
        CTRL|'C',               spawncli,      /* Run CLI in subjob.   */
        CTRL|'D',               forwdel,
        CTRL|'E',               gotoeol,
        CTRL|'F',               forwchar,
        CTRL|'G',               ctrlg,
        CTRL|'H',               backdel,
        CTRL|'I',               tab,
        CTRL|'J',               indent,
        CTRL|'K',               kill_fwd,
        CTRL|'L',               refresh,
        CTRL|'M',               newline,
        CTRL|'N',               forwline,
        CTRL|'O',               openline,
        CTRL|'P',               backline,
        CTRL|'Q',               quote,         /* Often unreachable    */
        CTRL|'R',               backsearch,
        CTRL|'S',               forwsearch,    /* Often unreachable    */
	CTRL|'T',               twiddle,
        CTRL|'V',               forwpage,
        CTRL|'W',               killregion,
        CTRL|'Y',               yank,
        CTRL|'Z',               quickexit,     /* quick save and exit  */
        CTLX|CTRL|'B',          listbuffers,
        CTLX|CTRL|'C',          quit,          /* Hard quit.           */
        CTLX|CTRL|'F',          filevisit,
        CTLX|CTRL|'L',          lowerregion,
        CTLX|CTRL|'O',          deblank,
        CTLX|CTRL|'N',          mvdnwind,
        CTLX|CTRL|'P',          mvupwind,
        CTLX|CTRL|'R',          filename,
        CTLX|CTRL|'S',          filesave,      /* Often unreachable    */
	CTLX|CTRL|'T',		compile_region,
        CTLX|CTRL|'U',          upperregion,
        CTLX|CTRL|'V',          fileread,
        CTLX|CTRL|'W',          filewrite,
        CTLX|CTRL|'X',          swapmark,
        CTLX|CTRL|'Z',          shrinkwind,
        CTLX|'!',               spawn,         /* Run 1 command.       */
        CTLX|'=',               showcpos,
        CTLX|':',               gotoline,
        CTLX|'(',               ctlxlp,
        CTLX|')',               ctlxrp,
        CTLX|'1',               onlywind,
        CTLX|'2',               splitwind,
        CTLX|'B',               usebuffer,
        CTLX|'E',               ctlxe,
        CTLX|'F',               setfillcol,
        CTLX|'K',               killbuffer,
        CTLX|'M',               smatchb,
        CTLX|'N',               nextwind,
        CTLX|'P',               prevwind,
        CTLX|'Q',               temp_quit,
        CTLX|'R',               bkwrdrpl,
	CTLX|'S',		frwsr,
        CTLX|'Z',               enlargewind,
        META|CTRL|'H',          delbword,
        META|'!',               reposition,
        META|'.',               setmark,
        META|'>',               gotoeob,
        META|'<',               gotobob,
        META|'B',               backword,
        META|'C',               capword,
        META|'D',               delfword,
        META|'F',               forwword,
        META|'J',               forwsearch,            /* To replace C-S */
        META|'L',               lowerword,
        META|'R',               bkwrdcr,
	META|'S',		querysr,
	META|'T',		compile_file,
        META|'U',               upperword,
        META|'V',               backpage,
        META|'W',               copyregion,
        META|'Z',               filesave,              /* To replace C-S */
        META|DEL_KEY,           delbword,
        DEL_KEY,                backdel
};

#define NKEYTAB (sizeof(keytab)/sizeof(keytab[0]))        


static float clp_edit2()
{
        register int    c;
        register int    f;
        register int    n;
        register int    mflag;
        register int    rtn_flag;
        char            bname[NBUFN];
        int num_a;
        char *fileName;
        VALUE arg_ptr;
        
   /*====================*/
   /* Get the file name. */
   /*====================*/
   
   if ((num_a = arg_num_check("edit",NO_MORE_THAN,1)) == -1) return(0.0);

   if (num_a == 1)
     { 
      if (arg_type_check("edit",1,STRING,&arg_ptr) == FALSE) return(0.0);
      fileName = get_valstring(arg_ptr);
     }
   
   if(bheadp == NULL) {

	/**********************************************/
	/* Initial entry, set up buffers and pointers */
	/**********************************************/

        strcpy(bname, "main");                  /* Work out the name of */
        if (num_a > 0)                     /* the default buffer.  */
                makename(bname,fileName);
        edinit(bname);                          /* Buffers, windows.    */
        vtinit();                               /* Displays.            */
        if (num_a > 0) {
                update();                       /* You have to update   */
                readin(fileName);             /* in case "[New file]" */
                }

	init_cmp_router();			/* Prepare the compile  */
        deact_router("cmp_router");		/* router.              */
        }
   else {

	/**********************************************************/
	/* Return from temporary exit, reset necessary stuff only */
	/**********************************************************/

	(*term.t_open)();

        if (num_a > 0) {
           filevisit_guts(fileName);
           }
        }

   sgarbf = TRUE;                          /* Force screen update  */
   lastbufn[0] = '\0';                     /* Make sure last name  */
                                           /* is cleared out       */

   lastflag = 0;                           /* Fake last flags.     */
loop:
        update();                               /* Fix up the screen    */
        c = getkey();
        if (mpresf != FALSE) {
                mlerase();
                update();
                if (c == ' ')                   /* ITS EMACS does this  */
                        goto loop;
        }
        f = FALSE;
        n = 1;
        if (c == (CTRL|'U')) {                  /* ^U, start argument   */
                f = TRUE;
                n = 4;                          /* with argument of 4 */
                mflag = 0;                      /* that can be discarded. */
                mlwrite("Arg: 4");
                while ((c=getkey()) >='0' && c<='9' || c==(CTRL|'U') || c=='-'){
                        if (c == (CTRL|'U'))
                                n = n*4;
                        /*
                         * If dash, and start of argument string, set arg.
                         * to -1.  Otherwise, insert it.
                         */
                        else if (c == '-') {
                                if (mflag)
                                        break;
                                n = 0;
                                mflag = -1;
                        }
                        /*
                         * If first digit entered, replace previous argument
                         * with digit and set sign.  Otherwise, append to arg.
                         */
                        else {
                                if (!mflag) {
                                        n = 0;
                                        mflag = 1;
                                }
                                n = 10*n + c - '0';
                        }
                        mlwrite("Arg: %d", (mflag >=0) ? n : (n ? -n : -1));
                }
                /*
                 * Make arguments preceded by a minus sign negative and change
                 * the special argument "^U -" to an effective "^U -1".
                 */
                if (mflag == -1) {
                        if (n == 0)
                                n++;
                        n = -n;
                }
        }
        if (c == (CTRL|'X'))                    /* ^X is a prefix       */
                c = CTLX | getctl();
        if (kbdmip != NULL) {                   /* Save macro strokes.  */
                if (c!=(CTLX|')') && kbdmip>&kbdm[NKBDM-6]) {
                        ctrlg(FALSE, 0);
                        goto loop;
                }
                if (f != FALSE) {
                        *kbdmip++ = (CTRL|'U');
                        *kbdmip++ = n;
                }
                *kbdmip++ = c;
        }
        rtn_flag = execute(c, f, n);                /* Do it.               */
        if(rtn_flag == EXIT)
           return(0.0);
	else
           goto loop;
}

/*
 * Initialize all of the buffers and windows. The buffer name is passed down
 * as an argument, because the main routine may have been told to read in a
 * file by default, and we want the buffer name to be right.
 */
edinit(bname)
char    bname[];
{
        register BUFFER *bp;
        register WINDOW *wp;

        bp     = bfind(bname, TRUE, 0);              /* First buffer        */
        blistp = bfind("[List]", TRUE, BFTEMP);      /* Buffer list buffer  */
        wp     = (WINDOW *) genalloc(sizeof(WINDOW));  /* First window        */
        if (bp==NULL || wp==NULL || blistp==NULL)
                return(EXIT);
        curbp  = bp;                            /* Make this current    */
        wheadp = wp;
        curwp  = wp;
        wp->w_wndp  = NULL;                     /* Initialize window    */
        wp->w_bufp  = bp;
        bp->b_nwnd  = 1;                        /* Displayed.           */
        wp->w_linep = bp->b_linep;
        wp->w_dotp  = bp->b_linep;
        wp->w_doto  = 0;
        wp->w_markp = NULL;
        wp->w_marko = 0;
        wp->w_toprow = 0;
        wp->w_ntrows = term.t_nrow-1;           /* "-1" for mode line.  */
        wp->w_force = 0;
        wp->w_flag  = WFMODE|WFHARD;            /* Full.                */
}
        
/*
 * This is the general command execution routine. It handles the fake binding
 * of all the keys to "self-insert". It also clears out the "thisflag" word,
 * and arranges to move it to the "lastflag", so that the next command can
 * look at it. Return the status of command.
 */
execute(c, f, n)
{
        register KEYTAB *ktp;
        register int    status;

        ktp = &keytab[0];                       /* Look in key table.   */
        while (ktp < &keytab[NKEYTAB]) {
                if (ktp->k_code == c) {
                        thisflag = 0;
                        status   = (*ktp->k_fp)(f, n);
                        lastflag = thisflag;
                        return (status);
                }
                ++ktp;
        }

        /*
         * If a space was typed, fill column is defined, the argument is non-
         * negative, and we are now past fill column, perform word wrap.
         */
        if (c == ' ' && fillcol > 0 && n>=0 && getccol(FALSE) > fillcol)
                wrapword();

        if ((c>=0x20 && c<=0x7E)                /* Self inserting.      */
        ||  (c>=0xA0 && c<=0xFE)) {
                if (n <= 0) {                   /* Fenceposts.          */
                        lastflag = 0;
                        return (n<0 ? FALSE : TRUE);
                }
                thisflag = 0;                   /* For the future.      */
                status   = linsert(n, c);
                lastflag = thisflag;
                return (status);
        }
        lastflag = 0;                           /* Fake last flags.     */
        return (FALSE);
}

/*
 * Read in a key.
 * Do the standard keyboard preprocessing. Convert the keys to the internal
 * character set.
 */
getkey()
{
        register int    c;

        c = (*term.t_getchar)();
	if ((c & META) == META) return(c);

#if IBM_MSC || IBM_TBC || IBM_ZTC
	if (c > 255) {
         switch (c) {
            case UP_ARROW    :
	                      return (CTRL | 'P');
            case DOWN_ARROW  :
                              return (CTRL | 'N');
            case LEFT_ARROW  :
                              return (CTRL | 'B');
            case RIGHT_ARROW :
                              return (CTRL | 'F');
            case PGUP_KEY    :
                              return (META | 'V');
            case PGDN_KEY    :
                              return (CTRL | 'V');
            case HOME_KEY    :
                              return (META | '<');
            case END_KEY     :
                              return (META | '>');
            case CTRL_LEFT_ARROW  :
                              return (META | 'B');
            case CTRL_RIGHT_ARROW :
                              return (META | 'F');
            case CTRL_AT_SIGN     :
                              return (CTRL | '@');
  	    default :
                              return (CTRL | 'G');
            }
	}
#endif

        if (c == METACH) {                      /* Apply M- prefix      */
                c = getctl();
                return (META | c);
        }

        if (c>=0x00 && c<=0x1F)                 /* C0 control -> C-     */
                c = CTRL | (c+'@');
        return (c);
}

/*
 * Get a key.
 * Apply control modifications to the read key.
 */
getctl()
{
        register int    c;
        c = (*term.t_getchar)();
        if (c>='a' && c<='z')                   /* Force to upper       */
                c -= 0x20;
        if (c>=0x00 && c<=0x1F)                 /* C0 control -> C-     */
                c = CTRL | (c+'@');
        return (c);
}

/*
 * Fancy quit command, as implemented by Norm. If the current buffer has
 * changed do a write current buffer and exit emacs, otherwise simply exit.
 */
quickexit(f, n)
{
        if ((curbp->b_flag&BFCHG) != 0          /* Changed.             */
        && (curbp->b_flag&BFTEMP) == 0)         /* Real.                */
                filesave(f, n);
        return(quit(f, n));                     /* conditionally quit   */
}

/*
 * Quit command. If an argument, always quit. Otherwise confirm if a buffer
 * has been changed and not written out. Normally bound to "C-X C-C".
 */
quit(f, n)
{
        register int    s;

        if (f != FALSE                          /* Argument forces it.  */
        || anycb() == FALSE                     /* All buffers clean.   */
                                                /* User says it's OK.   */
        || (s=mlyesno("Modified Buffers! Quit")) == TRUE) {
                vttidy();
                full_cleanup();
                return(EXIT);
        }
        return (s);
}

/*
 * Temporary exit from editor. Leave all data structures
 * intact, but tidy up video interface.
 * Connected to "C-X Q".
 */

temp_quit()
{
   vttidy();
   return(EXIT);
}

/*
 * Begin a keyboard macro.
 * Error if not at the top level in keyboard processing. Set up variables and
 * return.
 */
ctlxlp(f, n)
{
        if (kbdmip!=NULL || kbdmop!=NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        mlwrite("[Start macro]");
        kbdmip = &kbdm[0];
        return (TRUE);
}

/*
 * End keyboard macro. Check for the same limit conditions as the above
 * routine. Set up the variables and return to the caller.
 */
ctlxrp(f, n)
{
        if (kbdmip == NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        mlwrite("[End macro]");
        kbdmip = NULL;
        return (TRUE);
}

/*
 * Execute a macro.
 * The command argument is the number of times to loop. Quit as soon as a
 * command gets an error. Return TRUE if all ok, else FALSE.
 */
ctlxe(f, n)
{
        register int    c;
        register int    af;
        register int    an;
        register int    s;

        if (kbdmip!=NULL || kbdmop!=NULL) {
                mlwrite("Not now");
                return (FALSE);
        }
        if (n <= 0) 
                return (TRUE);
        do {
                kbdmop = &kbdm[0];
                do {
                        af = FALSE;
                        an = 1;
                        if ((c = *kbdmop++) == (CTRL|'U')) {
                                af = TRUE;
                                an = *kbdmop++;
                                c  = *kbdmop++;
                        }
                        s = TRUE;
                } while (c!=(CTLX|')') && (s=execute(c, af, an))==TRUE);
                kbdmop = NULL;
        } while (s==TRUE && --n);
        return (s);
}

/*
 * Abort.
 * Beep the beeper. Kill off any keyboard macro, etc., that is in progress.
 * Sometimes called as a routine, to do general aborting of stuff.
 */
ctrlg(f, n)
{
        (*term.t_beep)();
        if (kbdmip != NULL) {
                kbdm[0] = (CTLX|')');
                kbdmip  = NULL;
        }
        return (ABORT);
}

full_cleanup()
{

/*   Clear all data structures */

   kill_all_buffers(bheadp);     /* Clear all existing buffers   */

   kill_all_windows();           /* Clear all windows            */

   kill_video_buffers();	 /* Kill special video buffers   */

   kill_cmp_router();		 /* Get rid of special router    */

/*   Clear all global pointers */

   curwp  = NULL;                /* Current window               */
   curbp  = NULL;                /* Current buffer               */
   wheadp = NULL;                /* Head of list of windows      */
   bheadp = NULL;                /* Head of list of buffers      */
   blistp = NULL;                /* Buffer for C-X C-B           */
   kbdmip = NULL;                /* Input pointer for above      */
   kbdmop = NULL;                /* Output pointer for above     */
   pat[0] = '\0';                /* Search pattern               */
   lastbufn[0] = '\0';           /* Name of Last buffer accessed */
}

/*
 * Dispose of all buffers. Clear the buffer (ask first
 * if the buffer has been changed). Then free the header
 * line and the buffer header. Called for full cleanup.
 */

kill_all_buffers(top_buf)
BUFFER **top_buf;
{
   register BUFFER *bp;

   bp = *top_buf;
   while(bp != NULL) {
        spec_clear(bp);                         /* Blow text away.      */

        genfree((char *) bp->b_linep,           /* And free pointer     */
	         sizeof(LINE)+ bp->b_linep->l_size);

        *top_buf = bp->b_bufp;                       /* Find next buffer     */
        genfree((char *) bp, sizeof(BUFFER));   /* Release buffer block */
	bp = *top_buf;
        }

   return (TRUE);
}

kill_all_windows()
{
   register WINDOW *wp;
   register WINDOW *wp1;

   wp = wheadp;
   while(wp != NULL) {
        wp1 = wp->w_wndp;
        genfree((char *) wp, sizeof(WINDOW));
	wp  = wp1;
        }

   return (TRUE);
}

/*
 * This routine blows away all of the text in a
 * buffer. Does NOT care if text has been changed!
 */

spec_clear(bp)
register BUFFER *bp;
{
        register LINE   *lp;
      
        bp->b_flag  &= ~BFCHG;                  /* Not changed          */
        while ((lp=lforw(bp->b_linep)) != bp->b_linep)
                lfree(lp);
        bp->b_dotp  = bp->b_linep;              /* Fix "."              */
        bp->b_doto  = 0;
        bp->b_markp = NULL;                     /* Invalidate "mark"    */
        bp->b_marko = 0;
        return (TRUE);
}

clp_edit() {
    float f;
        extern int (*redraw_screen)();
        extern int (*pause_env)();
        extern int (*cont_env)();

    if (pause_env != NULL) (*pause_env)() ;
	f = clp_edit2() ;
	if (cont_env != NULL) (*cont_env)(0) ;
    if (redraw_screen != NULL) (*redraw_screen)() ;
	return(f) ;
}

#else

clp_edit() {}

#endif
#endif
