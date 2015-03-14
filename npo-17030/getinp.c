#include <stdio.h>


#define MAXLINE  128

static char ENDFILE;				/* end of file flag */
static char ENDLINE;				/* end of line flag */
static int CURP;					/* cursor position */
static char LINE[MAXLINE];			/* line buffer */
static char token[MAXLINE];		/* token */
static int token_p;				/* token pointer */
FILE *fopen();					/* input soure file ptr */
extern FILE *ifp;
extern char progname[];
extern double atof();

initinp()
{
	ENDFILE = 0;
	ENDLINE = 0;
	if ( (ifp = fopen(progname, "r")) == NULL) {
		exit(1);
	}
	readline();
}

/*  readline reads the next input line into array LINE,
	increment LINENO, sets CURP.
	If EOF then ENDFILE is set to true.
*/
readline()
{
    reread:
	if ( fgets(LINE,MAXLINE, ifp) == NULL) 
		ENDFILE = 1;
	else {
	    if (LINE[0] == '*') goto reread;
		ENDLINE = 0;
		CURP = 0;
	}

}

/* get token into token buffer */

double getnum()
{
	char ch;
	int	sp, comment;

	token_p = 0;		/* reset token pointer */
    ch = getone();
    if (ENDFILE) {
err:   printf(" +++ MORE DATA EXPECTED +++\n");
       return(0.0);
    }
    while (ch==' ' || ch=='\n' || ch=='\t') {
       ch = getone();
       if (ENDFILE) goto err;
    }
    while (ch=='.' || isdigit(ch)) {
       token[token_p++] = ch;
       ch = getone();
       if (ENDFILE) goto getout;
    }
getout:  token[token_p++] = '\0';
     return(atof(token));
}


/* get a character from LINE buffer */

getone()
{
	char ch;

	if (ENDLINE)
		readline();
	if (ENDFILE) return(-1);
	else {
	    ch = LINE[CURP++];
		if (ch=='\n') ENDLINE = 1;
		return(ch);
	}
}

