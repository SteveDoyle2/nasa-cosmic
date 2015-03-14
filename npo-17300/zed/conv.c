#include "glob"

write_qst (qst)
int qst[];
{
  int i;
  for (i = QN; i <= QINT; i++) writef ("QST[%d] = %d\n", i, qst[i]);
  writef ("QST[QSEP] = %c\n", qst[QSEP]);
  writef ("QST[QSTR] = %s\n", qst[QSTR]);
}

writef (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
char *format;
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
  fprintf (cur_output_fd,
           format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

dwritef (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
char *format;
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
  fprintf (verf, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

wrch (ch)
char ch;
{
  fputc (ch, cur_output_fd);
}

writen (n)
int n;
{
  writef ("%d", n);
}

char rdch()
{
  char ch;
  ch = fgetc (cur_input_fd) & 0xff;

/******************************************************************************
   Logic: 
 interactive || user_level == 0  ==>  either commands we're
                                      typing or commands taken from a WITH file;
          eprintf != NULL        ==>  we have to have somewhere to write to;
          cur_input_fd == withf  ==>  we aren't reading an insert or
                                      source file.
******************************************************************************/

  if ((interactive || user_level == 0) 
      && eprintf != NULL && cur_input_fd == withf)
  {
    fputc (ch, eprintf);
    fflush (eprintf);
  }
  return ch;
}

FILE *findinput (logname)
char *logname;
{
  FILE *fd;
  if ((fd = fopen (logname, "r")) == NULL)
     return 0;
  else return fd;
}

FILE *findoutput (logname)
char *logname;
{
  FILE *fd;
  if ((fd = fopen (logname, "w")) == NULL)
     return 0;
  else return fd;
}

selectinput (fd)
FILE *fd;
{
  cur_input_fd = fd;
}

selectoutput (fd)
FILE *fd;
{
  cur_output_fd = fd;
}

stop (n)
int n;
{
  exit (n);
}

int ungetchar (ch)
char ch;
{
  return ungetc (ch, cur_input_fd);
}

/*****************************************
 * Print a string using writef, verified *
 * according to the Cambridge (post-@)   *
 * rules for the ! command (loosely)     *
 ****************************************/

verify_string (st)
char *st;
{
  int l = strlen (st);
  int i;
  for (i = 0; i < l; i++)
    if (isprint (st[i]))
       writef ("%c", capitalch(st[i]));
    else wrch ('^');
  wrch ('\n');
  for (i = 0; i < l; i++)
    if (isupper (st[i])) wrch ('-');
    else if (st[i] == '\177') wrch ('?');
    else if (iscntrl (st[i])) wrch (st[i] + 64);
    else wrch (' ');
  wrch ('\n');
}      

/****************************************
 * Verify a string in hexadecimal mode  *
 ***************************************/

verify_hex (st)
char *st;
{
  int l = strlen (st);
  int i;
  for (i = 0; i < l; i++)
    writef ("%02X", st[i] & 0xff);
  wrch ('\n');
}      

/**************************************
 * Strip trailing spaces off a string *
 *************************************/

strip_spaces (str)
char *str;
{
  int i = strlen (str) - 1;
  while (i > 0 && isspace (str[i])) str[i--] = '\0';
}

/***************************************************************
 * Compress spaces: remove leading spaces, and compress others *
 **************************************************************/

compress_spaces (str)
char *str;
{
  char b[MAXSTRLENGTH];
  int space = FALSE;
  int i = 0;
  int j = 0;
  int len = strlen (str) - 1;
  while (i <= str && isspace (str[i])) i++;
  while (i <= len)
    if (isspace (str[i]) && space) i++;
    else
    {
      if (isspace (str[i])) space = TRUE;
      else space = FALSE;
      b[j++] = str[i++];
    }
  b[j] = '\0';
  if (strlen (str) != strlen (b)) strcpy (str, b);
}

/*************************************************************
 * Detab a string.  Tab values set every <tabvalue> columns. *
 * Complain if exceeding MXLL.                               *
 ************************************************************/

detab (str)
char *str;
{
  int i;
  int j = 0;
  int tabbed = FALSE;
  char b[MAXSTRLENGTH];
  for (i = 0; i < strlen (str); i++)
  {
    if (str[i] == '\t')
    {
      tabbed = TRUE;
      b[j++] = ' ';
      while (j % tabvalue != 0)
      {
        if (j > mxll) execerror ('e');
        b[j++] = ' ';
      }
    }
    else
    {
      b[j++] = str[i];
      if (j > mxll) execerror ('e');
    }
  }
  b[j] = '\0';
  if (tabbed) strcpy (str, b);
}

/**************************************
 * Write a string out using writef if *
 * hex mode, writehex otherwise.      *
 *************************************/

writeu (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
char *format;
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
  if (swx) writehex (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  else writef (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

writehex (format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
char format[];
int arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
{
  char str[MAXSTRLENGTH];
  int i;
  int len;
  sprintf (str, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  len = strlen (str);
  i = 0;
  while (i < len)
  {
    writef ("%02X", str[i++] & 0xff);
    if (i % 4 == 0) wrch (' ');
  }
}

set_interactive()
{
  interactive = (isatty (fileno (withf)) && isatty (fileno (verf)));
  if (interactive && not_yet_interactive)
  {
    init_interrupts();
    not_yet_interactive = FALSE;
  }
  swv = interactive;
  swerrstop = ! interactive;
}
