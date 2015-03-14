/*   CLIPS Version 4.30   4/25/89 */

#include      <stdio.h>
#include        "ed.h"

#if   CLP_EDIT && ! RUN_TIME

/* ==========================================================================
 *                              ANSI Terminal
 * ==========================================================================
 */
  
#if     ANSI

/*
 * The routines in this section provide support for ANSI style terminals
 * over a serial line. The serial I/O services are provided by routines in
 * "termio.c". It compiles into nothing if not an ANSI device.
 */

#define NROW    23                      /* Screen size.                 */
#define NCOL    77                      /* Edit if you want to.         */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */

extern  int     ttopen();               /* Forward references.          */
extern  int     ttgetc();
extern  int     ttputc();
extern  int     ttflush();
extern  int     ttclose();
extern  int     ansimove();
extern  int     ansieeol();
extern  int     ansieeop();
extern  int     ansibeep();
extern  int     ansiopen();

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
TERM    term    = {
        NROW-1,
        NCOL,
        ansiopen,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        ansimove,
        ansieeol,
        ansieeop,
        ansibeep
};

ansimove(row, col)
{
        ttputc(ESC);
        ttputc('[');
        ansiparm(row+1);
        ttputc(';');
        ansiparm(col+1);
        ttputc('H');
}

ansieeol()
{
        ttputc(ESC);
        ttputc('[');
        ttputc('K');
}

ansieeop()
{
        ttputc(ESC);
        ttputc('[');
        ttputc('J');
}

ansibeep()
{
        ttputc(BEL);
        ttflush();
}

ansiparm(n)
register int    n;
{
        register int    q;

        q = n/10;
        if (q != 0)
                ansiparm(q);
        ttputc((n%10) + '0');
}

ansiopen()
{
#if     UNIX_7 || UNIX_V
        register char *cp;
        char *getenv();

        if ((cp = getenv("TERM")) == NULL) {
                puts("Shell variable TERM not defined!");
                exit(1);
        }
        if (strcmp(cp, "vt100") != 0) {
                puts("Terminal type not 'vt100'!");
                exit(1);
        }
#endif
        ttopen();
}

#endif

/* ==========================================================================
 *                              VT52 Terminal
 * ==========================================================================
 */
  
#if     VT52

/*
 * The routines in this section
 * provide support for VT52 style terminals
 * over a serial line. The serial I/O services are
 * provided by routines in "termio.c". It compiles
 * into nothing if not a VT52 style device. The
 * bell on the VT52 is terrible, so the "beep"
 * routine is conditionalized on defining BEL.
 */

#define NROW    24                      /* Screen size.                 */
#define NCOL    80                      /* Edit if you want to.         */
#define BIAS    0x20                    /* Origin 0 coordinate bias.    */
#define ESC     0x1B                    /* ESC character.               */
#define BEL     0x07                    /* ascii bell character         */

extern  int     ttopen();               /* Forward references.          */
extern  int     ttgetc();
extern  int     ttputc();
extern  int     ttflush();
extern  int     ttclose();
extern  int     vt52move();
extern  int     vt52eeol();
extern  int     vt52eeop();
extern  int     vt52beep();
extern  int     vt52open();

/*
 * Dispatch table. All the
 * hard fields just point into the
 * terminal I/O code.
 */
TERM    term    = {
        NROW-1,
        NCOL,
        vt52open,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        vt52move,
        vt52eeol,
        vt52eeop,
        vt52beep
};

vt52move(row, col)
{
        ttputc(ESC);
        ttputc('Y');
        ttputc(row+BIAS);
        ttputc(col+BIAS);
}

vt52eeol()
{
        ttputc(ESC);
        ttputc('K');
}

vt52eeop()
{
        ttputc(ESC);
        ttputc('J');
}

vt52beep()
{
#ifdef  BEL
        ttputc(BEL);
        ttflush();
#endif
}

vt52open()
{
#if     UNIX_7 || UNIX_V
        register char *cp;
        char *getenv();

        if ((cp = getenv("TERM")) == NULL) {
                puts("Shell variable TERM not defined!");
                exit(1);
        }
        if (strcmp(cp, "vt52") != 0 && strcmp(cp, "z19") != 0) {
                puts("Terminal type not 'vt52'or 'z19' !");
                exit(1);
        }
#endif
        ttopen();
}

#endif

/* ==========================================================================
 *                              IBM PC Code
 * ==========================================================================
 */
  
#if   IBM_PC                            /* Should be an IBM PC using    */
                                        /* the Microsoft C compiler     */
#if   IBM_MSC || IBM_TBC || IBM_ZTC     /* or the Turbo C compiler      */
                                        /* or the Zortech C compiler    */
#include        <dos.h>

#define NROW    25                      /* Screen size. rows            */
#define NCOL    80                      /* Columns                      */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */
#define SPACE   32

#define	SCADC	0xb8000000L		/* CGA address of screen RAM	*/
#define	SCADM	0xb0000000L		/* MONO address of screen RAM	*/

#define MONOCRSR 0x0B0D			/* monochrome cursor	    */
#define CGACRSR 0x0607			/* CGA cursor		    */

#define	CDCGA	0			/* color graphics card		*/
#define	CDMONO	1			/* monochrome text card		*/
#define	CDSENSE	9			/* detect the card type		*/

#define NDRIVE	3			/* number of screen drivers	*/

extern  int     pc_open();
extern  int     ttopen();               /* Forward references.          */
extern  int     pc_getc();
extern  int     pc_putc();
extern  int     ttflush();
extern  int     ttclose();
extern  int     pc_move();
extern  int     pc_eeol();
extern  int     pc_eeop();
extern  int     pc_beep();

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */

TERM    term    = {
        NROW-1,
        NCOL,
        pc_open,
        ttclose,
        pc_getc,
        pc_putc,
        ttflush,
        pc_move,
        pc_eeol,
        pc_eeop,
        pc_beep
};

int dtype = -1;				/* current display type		*/
long scadd;				/* address of screen ram	*/
int *scptr[NROW];			/* pointer to screen lines	*/
unsigned int sline[NCOL];		/* screen line image		*/
union REGS rg;

pc_open()
{
 scinit(CDSENSE);
 ttopen();
}

scinit(type)	/* initialize the screen head pointers */

int type;	/* type of adapter to init for */

{
	union {
		long laddr;	/* long form of address */
		int *paddr;	/* pointer form of address */
	} addr;
	int i;

	/* if asked...find out what display is connected */
	if (type == CDSENSE)
		type = getboard();

	/* if we have nothing to do....don't do it */
	if (dtype == type)
		return(TRUE);

	switch (type) {
		case CDMONO:	/* Monochrome adapter */
				scadd = SCADM;
				break;

		case CDCGA:	/* Color graphics adapter */
				scadd = SCADC;
				break;

	}
	dtype = type;

	/* initialize the screen pointer array */
	for (i = 0; i < NROW; i++) {
		addr.laddr = scadd + (long)(NCOL * i * 2);
		scptr[i] = addr.paddr;
	}
	return(TRUE);
}

/* getboard:	Determine which type of display board is attached.
		Current known types include:

		CDMONO	Monochrome graphics adapter
		CDCGA	Color Graphics Adapter
*/

/* getbaord:	Detect the current display adapter
		if MONO		set to MONO
		   CGA		set to CGA
*/

int getboard()

{
	int type;	/* board type to return */

	type = CDCGA;
	int86(0x11, &rg, &rg);
	if ((((rg.x.ax >> 4) & 3) == 3))
		type = CDMONO;
        return(type);
}

pc_getc()
{
   int intrpt = 22;                  /* ROM-BIOS call for keyboard read */

   rg.h.al = 0;                  /* Clear input registers   */
   rg.h.ah = 0;                  /* and set service to read */

   int86(intrpt, &rg, &rg);

   if(rg.h.al != 0)            /* If low byte is not clear */
     return(rg.h.al);          /* then return value        */
   else {                           /* else,                    */
     switch(rg.h.ah) {         /* check hi byte for code   */
       case  3 :
                return(CTRL_AT_SIGN);
       case 71 :
                return(HOME_KEY);
       case 72 :
                return(UP_ARROW);
       case 73 :
                return(PGUP_KEY);
       case 75 :
                return(LEFT_ARROW);
       case 77 :
                return(RIGHT_ARROW);
       case 79 :
                return(END_KEY);
       case 80 :
                return(DOWN_ARROW);
       case 81 :
                return(PGDN_KEY);
       case 115 :
                return(CTRL_LEFT_ARROW);
       case 116 :
                return(CTRL_RIGHT_ARROW);
       default :
			 return(BADKEY);
       }
   }
}

pc_putc(c)
{
	rg.h.ah = 14;		/* write char to screen with current attrs */
	rg.h.al = c;
        rg.h.bh = 0;
	rg.h.bl = 0x07;
	int86(0x10, &rg, &rg);
}

pc_move(row, col)
{
	rg.h.ah = 2;		/* set cursor position function code */
	rg.h.dl = col;
	rg.h.dh = row;
	rg.h.bh = 0;		/* set screen page number */
	int86(0x10, &rg, &rg);
}

pc_eeol()
{
	unsigned int attr;	/* attribute byte mask to place in RAM */
	unsigned int *lnptr;	/* pointer to the destination line */
	int i;
	int ccol;	/* current column cursor lives */
	int crow;	/*	   row	*/

	/* find the current cursor position */
	rg.h.ah = 3;		/* read cursor position function code */
	rg.h.bh = 0;		/* current video page */
	int86(0x10, &rg, &rg);
	ccol = rg.h.dl;		/* record current column */
	crow = rg.h.dh;		/* and row */

	/* build the attribute byte and setup the screen pointer */
	attr = 0x0700;
	lnptr = &sline[0];
	for (i=0; i < term.t_ncol; i++)
		*lnptr++ = SPACE | attr;

	if (dtype == CDCGA) {
		/* wait for vertical retrace to be off */
		while ((inp(0x3da) & 8))
			;
	
		/* and to be back on */
		while ((inp(0x3da) & 8) == 0)
			;
	}			

	/* and send the string out */
	memmove(scptr[crow]+ccol, &sline[0], (term.t_ncol-ccol)*2);
}

pc_eeop()
{
	int attr;		/* attribute to fill screen with */

	rg.h.ah = 6;		/* scroll page up function code */
	rg.h.al = 0;		/* # lines to scroll (clear it) */
	rg.x.cx = 0;		/* upper left corner of scroll */
	rg.x.dx = (term.t_nrow << 8) | (term.t_ncol - 1);
				/* lower right corner of scroll */
	attr = 0x07;
	rg.h.bh = attr;
	int86(0x10, &rg, &rg);
}

pc_beep()
{
        pc_putc(BEL);
        ttflush();
}

#endif
#endif


/* ==========================================================================
 *                         Termcap Terminal
 * ==========================================================================
 */

#if TERMCAP

/*
 * The routines in this section provide support for terminals supported
 * through the UNIX termcap capability.
 *
 * You need to include the termcap library at link time 
 * or else most of the calls to t____ functions don't work 
 */

#define NROW    24
#define NCOL    80
#define BEL     0x07
#define ESC     0x1B

extern int      ttopen();
extern int      ttgetc();
extern int      ttputc();
extern int      ttflush();
extern int      ttclose();
extern int      tcapmove();
extern int      tcapeeol();
extern int      tcapeeop();
extern int      tcapbeep();
extern int      tcapopen();
extern int      tput();
extern char     *tgoto();

#define TCAPSLEN 315

char tcapbuf[TCAPSLEN];
char    PC,
        *CM,
        *CL,
        *CE,
        *UP,
        *CD;


TERM term = {
        NROW-1,
        NCOL,
        tcapopen,
        ttclose,
        ttgetc,
        ttputc,
        ttflush,
        tcapmove,
        tcapeeol,
        tcapeeop,
        tcapbeep
};

tcapopen()
{
        char *getenv();
        char *t, *p, *tgetstr();
        char tcbuf[1024];
        char *tv_stype;
        char err_str[72];

        if ((tv_stype = getenv("TERM")) == NULL)
        {
                puts("Environment variable TERM not defined!");
                exit(1);
        }

        if((tgetent(tcbuf, tv_stype)) != 1)
        {
                sprintf(err_str, "Unknown terminal type %s!", tv_stype);
                puts(err_str);
                exit(1);
        }

        p = tcapbuf;
        t = tgetstr("pc", &p);
        if(t)
                PC = *t;

        CD = tgetstr("cd", &p);
        CM = tgetstr("cm", &p);
        CE = tgetstr("ce", &p);
        UP = tgetstr("up", &p);

        if(CD == NULL || CM == NULL || CE == NULL || UP == NULL)
        {
                puts("Incomplete termcap entry\n");
                exit(1);
        }

        if (p >= &tcapbuf[TCAPSLEN])
        {
                puts("Terminal description too big!\n");
                exit(1);
        }
        ttopen();
}

tcapmove(row, col)
register int row, col;
{
        putpad(tgoto(CM, col, row));
}

tcapeeol()
{
        putpad(CE);
}

tcapeeop()
{
        putpad(CD);
}

tcapbeep()
{
        ttputc(BEL);
}

putpad(str)
char    *str;
{
        tputs(str, 1, ttputc);
}

putnpad(str, n)
char    *str;
{
        tputs(str, n, ttputc);
}
#endif

#endif          /* end original CLP_EDIT definition */
