/* section "main" */

#include "glob"
#include <climsgdef>
#include <descrip>

/*****************************************************************************
 *                                                                           *
 *   Holds the main program, the execute command sequence procedure and      *
 *  procedures to carry out simple miscellanous commands.                    *
 *                                                                           *
 ****************************************************************************/

main (argc, argv)
int argc;
char *argv[];
{
   char temp[FILENAMELENGTH];
   char temp2[MAXSTRLENGTH];
   char temp3[MAXSTRLENGTH];
   char temp4[FILENAMELENGTH];
   char temp5[FILENAMELENGTH];
   char *ptr;
   int i;

   debug = FALSE;

   wordset = temp2;

   from_file_created = FALSE;
   tofilename = temp;
   fromfilename = temp4;
   alttofilename = temp5;
   initoutputq();
   goutdisabled = FALSE;

   present ("INPUT_FILE", TRUE, fromfilename);
   i = find_file (fromfilename, ".LIS", OUTPUT);
   if (i != RMS$_SUC && i != RMS$_FNF) 
     stop (i);
   if (i == RMS$_FNF)
   {

/*   Now we need to create the FROM file if possible   */
 
     from_file_created = TRUE;
     if ((fromf = fopen (fromfilename, "w")) == NULL)
     {
       printf ("FROM file not open: /%s/", fromfilename);
       stop (7);
     }
     fgetname (fromf, fromfilename);
   }
   else
     fromf = fopen (fromfilename, "r");
   if (! from_file_created)
     file_is_ftn = (ratrfm (fromfilename, &rat, &rfm));
   else
     file_is_ftn = FALSE;

   if (present ("WITH", TRUE, temp) == CLI$_PRESENT)
   {
      if ((i = find_file (temp, ".COM", INPUT)) != RMS$_SUC)
        stop (i);
      withf = fopen (temp, "r");
   }
   else withf = fopen ("SYS$INPUT", "r");
   if (withf == NULL)
   {
     printf ("WITH file not open: /%s/", temp);
     stop (7);
   }

   if (present ("VER", TRUE, temp) == CLI$_PRESENT)
   {
      i = find_file (temp, ".LOG", OUTPUT);
      if (i != RMS$_SUC && i != RMS$_FNF)
        stop (i);
      verf = fopen (temp, "w");
   }
   else verf = fopen ("SYS$OUTPUT", "w");
   if (verf == NULL)
   {
     printf ("VER file not open: /%s/", temp);
     stop (7);
   }

/* The EPRINT stuff must come before the TO stuff since it uses
   the same buffer for the filename
*/
   if (present ("EPRINT", TRUE, temp) == CLI$_PRESENT)
   {
     i = find_file (temp, ".JOU", OUTPUT);
     if (i != RMS$_SUC && i != RMS$_FNF)
       stop (9);
     ptr = strchr (temp, ';');
     if (ptr != 0)
       *ptr = '\0';
     eprintf = fopen (temp, "w");
   }
   else eprintf = NULL;

   if (present ("OUTPUT_FILE", TRUE, tofilename) == CLI$_PRESENT);
   else
   {
     strcpy (tofilename, fromfilename);
     ptr = strchr (tofilename, ';');
     if (ptr != 0)
       *ptr = '\0';
   }
   i = find_file (tofilename, ".LIS", OUTPUT);
   if (i != RMS$_SUC && i != RMS$_FNF)
     stop (9);
   if (strncmp (tofilename, "_NL:", 4) == 0 || from_file_created)
     tof = fopen (tofilename, "w");
   else
   {
     if (*rat == '\0')
       tof = fopen (tofilename, "w", rfm);
     else
       tof = fopen (tofilename, "w", rat, rfm);
   }
   if (tof == NULL)
   {
     printf ("TO file not open: /%s/", tofilename);
     stop (7);
   }
   else
     fgetname (tof, tofilename);
   selectinput (withf);
   selectoutput (verf);
   altfromf = null;
   alttof = null;
   curfromf = fromf;
   curtof = tof;

   not_yet_interactive = TRUE;
   set_interactive();

   if (interactive)
     writef ("ZED (1.8) entered\n");

/*   if (debug) writef ("FROM is %d \n", fromf);                          */
/*   if (debug) writef ("WITH is %d \n", withf);                          */
/*   if (debug) writef ("VER is %d \n", verf);                            */
/*   if (debug) writef ("TO is %d \n", tof);                              */

   combuff = getspace (combsize);
   limcbuff = combuff + combsize * BYTESPERWORD;
   verreq1 = FALSE;
   verreq2 = FALSE;
   halt_lineno = ENDSYM;
   clevel = 1;
   withcol = 0;
   user_level = 0;

   clbuff = getspace (clbsize);
   clboccupied = FALSE;

   csebuff = getspace (csebsize);
   if (debug) writef ("csebuff == %d\n", csebuff);                   
   cseopen = FALSE;

   lscbuff = getspace (lscbsize);
   if (debug) writef ("lscbuff == %d\n", lscbuff);                   
   lscopen = FALSE;

   glist = null;
   endglist = null;
   proclist = null;
   cpysp = null;
   limcpysp = null;
   ptrcpysp = null;

   swve = FALSE;
   swvg = TRUE;
   swvn = TRUE;
   swvi = FALSE;
   swvt = TRUE;
   swvx = FALSE;
   swwarn = TRUE;
   swfn = FALSE;
   swtr = FALSE;
   swcs = FALSE;
   swdetab = FALSE;
   tabvalue = 8;
   mxll = 400;
   cur_col = 0;

   procword ("A-Za-z0-9");
   endinsertstr = temp3;
   strcpy (endinsertstr, "Z");
   if (interactive)
     init_interrupts();
   else
     interrupt = FALSE;

   if (debug) writef ("Initialization complete\n");

   setjmp (jmpenv);

   prescomseq = currcom;
   while (TRUE)
   {
     if (interactive)
     {
       if (interrupt)
         do_interrupt();       /* In case we hit BREAK when it's not looping */
       if (cur_col == 0)
         if (verreq1 || verreq2)
           verify();
         else
           wrch (':');
       else
       { 
         if (verreq1 || verreq2)
           verify();
         for (i = 1; i <= cur_col - 1; i++)
           if (swx)
             writef ("  ");
           else
             wrch (' ');
         wrch ('>');
       }
     }
     else
     {
       if (verreq1 || verreq2)
         verify();
       if (debug)
         writef ("%d> ", user_level);
     }
     ptrcbuff = combuff;
     rdwithch();
     if (withch == ENDSTREAMCH)
       procq();
     rdcomseq (&currcom);
     if (swve)
     {
       wrcomseq (currcom);
       wrch ('\n');
     }
     obeycomseq (currcom);
   }
}          

int obeycomseq (ptr)
struct com_rec *ptr;
{
   /********************************************************************
    *   Obeys pointed to command sequence.                             *
    *  prescomseq is used to hold a pointer to the beginning of the    *
    *  command sequence to help error diagnosis by procedure execerror *
    *******************************************************************/

   int i;
   int comname;
   int condition;
   struct com_rec *oldcomseq;

/*******************************************************************************
* The way we implement AGP is this: if we encounter AGP we set abandon = TRUE. *
* This forces exit of the current command group via the WHILE statement in     *
* here.  We then set a return code of FALSE.  If any IF-type commands see this *
* return code, they pass it back by setting abandon again.  If any repetitive  *
* commands (WH, etc) see this, they abandon what they are doing and go on.     *
*******************************************************************************/

   int abandon = FALSE;
   oldcomseq = prescomseq;
   prescomseq = ptr;
   while (ptr != null && (! abandon))
   {
     if (interrupt)
     {
       do_interrupt();
       execerror ('y');
     }
     comname = ptr->name;
     if ((comname & smask) != 0)
     {
       /**********************************************************
        *   Copy the command into the last string command buffer *
        *********************************************************/

       lscopen = TRUE;
       newlsc (ptr);
     }
     if ((comname & emask) != 0)
     {
       /**********************************************
        *   Copy the search expression argument into *
        *  the current search expression buffer.     *
        *********************************************/

       newcse (ptr->arg1);
     }
     if (! lineinput && (comname & imask) == 0)
     {
       /****************************************
        *   Input the first line of the source *
        ***************************************/

       lineinput = TRUE;
       inputln();
       if (debug)
         writef ("Inputting first line and setting lineinput = TRUE\n");
     }
     if ((comname & gmask) != 0)
     {
       /********************************************************
        *   Open the current line buffer (if not open already) *
        *******************************************************/

       verreq1 = TRUE;
       if (! clboccupied)
         getcl();
     }
     if ((comname & rmask) != 0)
     {
       /*********************************************
        *   Close the current line buffer (if open) *
        ********************************************/

       if (clboccupied)
         returncl();
     }
     if ((comname & vmask) != 0)
     {
       /******************************************************************
        *   Moving to a new line, test if old line requires verification *
        *  first.                                                        *
        *****************************************************************/

       if (verreq1) verify();

/*  The following line is an awful kludge designed to handle searches */

       if ((comname & emask) != 0) cur_col = 0;
       verreq2 = TRUE;
     }
     switch (comname)
     {
       case CINT : for (i = 1; i <= ptr->arg1; i++)
                     if (! obeycomseq (ptr->arg2))
                       break;
                   break;

       case CA : proca (ptr->arg1, ptr->arg2, FALSE);
                 break;

       case CB : procb (ptr->arg1, ptr->arg2, FALSE);
                 break;

       case CC : procc (&ptr);
                 break;

       case CD : procd (ptr->arg1, ptr->arg2);
                 break;

       case CE : proce (ptr->arg1, ptr->arg2, FALSE);
                 break;

       case CF : procf (ptr->arg1);
                 break;

       case CH : proch (ptr->arg1);
                 break;

       case CI : procm (ptr->arg1);
                 procim (&ptr);
                 break;

       case CM : procm (ptr->arg1);
                 break;

       case CN : procn();
                 break;

       case CP : procp();
                 break;

       case CQ : procq();
                 break;

       case CR : procd (ptr->arg1, ptr->arg2);
                 procim (&ptr);
                 break;

       case CT : proct (ptr->arg1);
                 break;

       case CV : procsw (&swv, ptr->arg1);
                 break;

       case CW : procw();
                 break;

       case CX : procsw (&swx, ptr->arg1);
                 break;

       case CZ : procz (ptr->arg2);
                 break;

       case CAP : proca (ptr->arg1, ptr->arg2, TRUE);
                  break;

       case CBF : procbf (ptr->arg1);
                  break;

       case CBP : procb (ptr->arg1, ptr->arg2, TRUE);
                  break;

       case CCC : proccc (ptr->arg2);
                  break;

       case CCG : proccg (ptr->arg1);
                  break;

       case CCL : proccl (ptr->arg2);
                  break;

       case CCS : procsw (&swcs, ptr->arg1);
                  break;

       case CDF : procdf (ptr->arg1);
                  break;

       case CDG : procdeg (ptr->arg1, FALSE);
                  break;

       case CDO : procdo (ptr->arg1);
                  break;

       case CEG : procdeg (ptr->arg1, TRUE);
                  break;

       case CEM : procem();
                  break;

       case CEP : proce (ptr->arg1, ptr->arg2, TRUE);
                  break;

       case CEQ : proceq (ptr->arg1);
                  break;

       case CFN : procsw (&swfn, ptr->arg1);
                  break;

       case CGA : proccpy (&ptr, FALSE);
                  break;

       case CGB : proccpy (&ptr, FALSE);
                  break;

       case CGE : proccpy (&ptr, FALSE);
                  break;

       case CIC : procic();
                  break;

       case CIF : condition = findse (ptr->arg1);
                  if (condition)
                    if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                  break;

       case CIS : procis (ptr->arg2, '\n');       /* 2nd arg is control ch */
                  break;

       case CLC : proclc (ptr->arg1);
                  break;

       case CON : proccpy (&ptr, FALSE);
                  break;

       case CPA : procpa (ptr->arg1);
                  break;

       case CPB : procpb (ptr->arg1);
                  break;

       case CPR : cur_col = 0;
                  break;

       case CQM : procqm();
                  break;

       case CSA : procsa (ptr->arg1);
                  break;

       case CSB : procsb (ptr->arg1);
                  break;

       case CTL : proctl (ptr->arg1);
                  break;

       case CTO : proctobuff (ptr);
                  break;

       case CTR : procsw (&swtr, ptr->arg1);
                  break;

       case CUC : procuc (ptr->arg1);
                  break;

       case CUL : condition = ! findse (ptr->arg1);
                  if (condition)
                    if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                  break;

       case CUT : while (! findse (ptr->arg1))
                    if (! obeycomseq (ptr->arg2)) break;
                  break;

       case CVE : procsw (&swve, ptr->arg1);
                  break;

       case CVG : procsw (&swvg, ptr->arg1);
                  break;

       case CVI : procsw (&swvi, ptr->arg1);
                  break;

       case CVN : procsw (&swvn, ptr->arg1);
                  break;

       case CVT : procsw (&swvt, ptr->arg1);
                  break;

       case CVX : procsw (&swvx, ptr->arg1);
                  break;

       case CWH : while (findse (ptr->arg1))
                    if (! obeycomseq (ptr->arg2))
                      break;
                  break;

       case CAGP : abandon = TRUE;
                   break;

       case CBRA :
       case CDEL :
       case CDOL :
       case CKET :
       case CPER :
       case CSPC : single_char (comname);
                   break;

       case CDFA : procdfa (ptr->arg1);
                   break;

       case CDFB : procdfb (ptr->arg1);
                   break;

       case CDTA : procdta (ptr->arg1);
                   break;

       case CDTB : procdtb (ptr->arg1);
                   break;

       case CLCL : proclcl();
                   break;

       case CRPT : while (TRUE)
                   if (! obeycomseq (ptr->arg2))
                     break;
                   break;

       case CSHC : procshc (ptr->arg1);
                   break;

       case CSHD : procshd();
                   break;

       case CSHF : procshf();
                   break;

       case CSHG : procshg (ptr->arg1);
                   break;

       case CSHS : procshs();
                   break;

       case CTAB : proctab (ptr->arg1);
                   break;

       case CUCL : procucl();
                   break;

       case CCOMM : proccomm (ptr->arg2);
                    break;

       case CCOLS : proccols (ptr->arg1);
                    break;

       case CCPROC : proccproc (ptr->arg1);
                     break;

       case CDBUFF : procdbuff (ptr->arg1);
                     break;

       case CDEBUG : debug = ! debug;
                     break;

       case CDETAB : procsw (&swdetab, ptr->arg1);
                     break;

       case CDREST : procdrest();
                     break;

       case CERRSTOP : procsw (&swerrstop, ptr->arg1);
                       break;

       case CELIF : if (! condition)
                    {
                      condition = findse (ptr->arg1);
                      if (condition)
                        if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                    }
                    break;

       case CELSE : if (! condition)
                      if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                    break;

       case CELUL : if (! condition)
                    {
                      condition = ! findse (ptr->arg1);
                      if (condition)
                        if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                    }
                    break;

       case CFLUSH : outputnl (MAXINT, tosource, tof);
                     break;

       case CFROM : procfrombuff (ptr);
                    break;

       case CHELP : prochelp();
                    break;

       case CWORD : procword (ptr->arg2);
                    break;

       case CIFEOF : condition = (currentl->symb & endmask) != 0;
                     if (condition)
                       if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                     break;

       case CMXLL : procmxll (ptr->arg1);
                    break;

       case CPROC : proccpy (&ptr, TRUE);
                    break;

       case CRLSC : procrlsc();
                    break;

       case CSHBUFF : procshbuff (ptr->arg1);
                      break;

       case CSHPROC : procshproc (ptr->arg1);
                      break;

       case CSTOP : procstop();
                    break;

       case CTBUFF : proctbuff (ptr->arg1);
                     break;

       case CULEOF : condition = ((currentl->symb & endmask) == 0);
                     if (condition)
                       if (! obeycomseq (ptr->arg2)) abandon = TRUE;
                     break;

       case CUNDO : procundo();
                    break;

       case CUTEOF : while ((currentl->symb & endmask) ==  0)
                       if (! obeycomseq (ptr->arg2))
                         break;
                     break;

       case CWARN : procsw (&swwarn, ptr->arg1);
                    break;

       default : execerror ('u', ptr->name);
     }
     if ((comname & vmask) != 0) cur_col = 0;
     ptr = ptr->nextcom;
   }
   prescomseq = oldcomseq;
   if (abandon) return FALSE;
   else return TRUE;
}

execerror (ch, arg)
char ch;
int *arg;
{
   /***************************************************************************
    *   Called if an error is found during execution of commands. Prints an   *
    *  error message and the current command sequence unless at the outermost *
    *  level and verifies the current line. Abandons the command sequence     *
    *  in the command buffer or terminates run according to                   *
    *  switch errstop.                                                        *
    **************************************************************************/

   writef ("**");
   switch (ch)
   {
     case 'a' : writef ("Inconsistent arguments");
                break;

     case 'b' : writef ("Buffer or file %d is open", arg);
                break;

     case 'c' : writef ("Copy procedure failed");
                break;

     case 'd' : writef ("Buffer %d is not empty", arg);
                break;

     case 'e' : writef ("Maximum string length exceeded");
                break;

     case 'f' : writef ("File /%s/ not found", arg);
                break;

     case 'g' : writef ("Global %d not found", arg);
                break;

     case 'h' : writef ("Improper use of HALT");
                break;

     case 'i' : writef ("Illegal attempt to input a new line");
                break;

     case 'k' : writef ("Invalid control character %c", arg);
                break;

     case 'l' : writef ("Line %d not found", arg);
                break;

     case 'm' : writef ("No match");
                break;

     case 'n' : writef ("No more previous lines available");
                break;

     case 'o' : writef ("Internal error - ZED workspace full");
                break;

     case 'p' : writef ("Procedure %s not found", arg);
                break;

     case 'q' : writef ("Global %d obeyed more than %d times in one line",
                         arg, MAXGLOBEXEC);
                break;

     case 'r' : writef ("No current search expression");
                break;

     case 's' : writef ("Source exhausted");
                break;

     case 't' : writef ("Line too long for input");
                break;

     case 'u' : writef ("Unrecognised command %d", arg);
                break;

     case 'v' : writef ("No last string command");
                break;

     case 'w' : writef ("Illegal word specifier");
                break;

     case 'x' : writef ("Procedure %s exists already", arg);
                break;

     case 'y' : writef ("BREAK");
                break;

     case 'z' : writef ("Exceeded bounds of line");
                break;

      default : writef ("Unidentified error during command execution");
                break;
   }
   wrch ('\n');
   if (prescomseq != currcom)
   {
     writef ("while obeying\n");
     wrcomseq (prescomseq);
     wrch ('\n');
   }
   goutdisabled = FALSE;
   if (lineinput)
   {
     if (swvi) procem();
     else procqm();
   }
   if (swerrstop)
   {
     fclose (tof);
     delete (tofilename);
     deletefiles();
    
/* The rationale here is that if we have deselected the alternate TO
   file then the user probably wanted the output sent to that file.   */
 
     if (currto != altto && alttof != null)
     { 
       flush_altfile();
       fclose (alttof);
     } 
     stop (42);
   }
   longjmp (jmpenv, 8);
}

/***********************************************************************
 *   The following procedures carry out various miscellaneous commands *
 **********************************************************************/

procrlsc()
{
  int dummy;
  if (! lscopen) execerror ('v');
   obeycomseq (lscbuff);
}

proccomm (st)
char *st;
{
   writef (st);
   wrch ('\n');
}

procundo()
{
   verreq1 = TRUE;
   cur_col = 0;                  /* Seems a good idea to do a PR */
   clboccupied = FALSE;
}

prochelp()
{
  char b[100];
  FILE *helpfile = fopen ("ZED$HLP", "r");
  if (helpfile == 0) return;
  while (! feof (helpfile))
  {
    fgets (b, 99, helpfile);
    writef ("%s", b);
  }
  fclose (helpfile);
}

procsw (ptrsw, arg)
int *ptrsw, arg;
{
   /*****************************************
    *   Sets the switch pointed at by ptrsw *
    ****************************************/

   if (debug) writef ("Setting switch %d with sym %d\n", ptrsw, arg);
   if (arg == PLUSSYM)
    *ptrsw = TRUE;
   else
   {
     if (arg != MINUSSYM) execerror ('a');
     *ptrsw = FALSE;
   }
}

procshd()
{
   if (cseopen)
   {
     writef ("Current search expression: ");
     wrse (csebuff, FALSE);
   }
   else writef ("No current search expression");
   wrch ('\n');
   if (lscopen)
   {
     writef ("\' command:                ");
     wrcomseq (lscbuff);
   }
   else writef ("No previous string command");
   wrch ('\n');
   writef ("Characters in words: \"");
   writeu ("%s", wordset);
   writef ("\"\n");
   writef ("Insert terminator: ");
   writeu ("%s", endinsertstr);
   wrch ('\n');
   if (halt_lineno != ENDSYM) writef ("Halting at line #%d\n", halt_lineno);
}

procshf()
{
  char temp[FILENAMELENGTH];
  writef ("FROM  : /%s/\n", fgetname (curfromf, temp));
  writef ("TO    : /%s/\n", fgetname (curtof, temp));
  writef ("WITH  : /%s/\n", fgetname (withf, temp));
  writef ("VER   : /%s/\n", fgetname (verf, temp));
  writef ("EPRINT: /%s/\n", (eprintf == NULL ? "(not open)" : 
          fgetname (eprintf, temp)));
}

/******************************************
 * This procedure was formerly bound into *
 * the procedure following it             *
 *****************************************/

wrtf (sw)
int sw;
{
  if (sw) writef ("+  "); else writef ("-  ");
}

procshs()
{
   writef ("V");
   wrtf (swv);
   writef ("FN");
   wrtf (swfn);
   writef ("VE");
   wrtf (swve);
   writef ("VG");
   wrtf (swvg);
   writef ("VI");
   wrtf (swvi);
   writef ("VN");
   wrtf (swvn);
   writef ("VT");
   wrtf (swvt);
   writef ("VX");
   wrtf (swvx);
   writef ("ERRSTOP");
   wrtf (swerrstop);
   writef ("WARN");
   wrtf (swwarn);
   writef ("TR");
   wrtf (swtr);
   writef ("CS");
   wrtf (swcs);
   writef ("DETAB");
   wrtf (swdetab);
   writef ("X");
   if (swx) writef ("+"); else writef ("-");
   wrch ('\n');
}

procmxll (length)
int length;
{
  if (length > MAXSTRLENGTH - 2)
  {
    length = MAXSTRLENGTH - 2;
    if (swwarn) writef ("Maximum line length set to %d\n", length);
  }
  if (length <= 0)
  {
    length = 1;
    if (swwarn) writef ("Maximum line length set to %d\n", length);
  }
  mxll = length;
}

char pfcc_to_internal (ch)
char ch;
{
  char ch2;
  switch (ch)
  {
    case ' ' : ch2 = '\n';
               break;

    case '1' : ch2 = '\f';
               break;

    case '+' : ch2 = '\r';
               break;

    default : execerror ('k', ch);
              break;
  }
  return ch2;
}

proccc (str)
char *str;
{
  char ch;
  if (strlen (str) == 0) ch = ' ';
     else ch = str[0];
  currentl->controlch = pfcc_to_internal (ch);
}

procz (str)
char *str;
{
   int i;
   for (i = 0; i <= strlen (str); i++)  endinsertstr[i] = toupper (str[i]);
}

proch (arg)
int arg;
{
  switch (arg)
  {
    case CURRSYM : halt_lineno = currentl->lineno;
                   break;

    case PLUSSYM : 
    case MINUSSYM :
    case OTHERSYM : execerror ('h');
                    break;

    default : halt_lineno = arg;        /* handles ENDSYM as well */
              break;
   }
}

procq()
{
  if (user_level != 0) closewith();
     else procw();
}

procstop()
{
  fclose (tof);
  delete (tofilename);
  deletefiles();

/* The rationale here is that if we have deselected the alternate TO
   file then the user probably wanted the output sent to that file.   */

  if (currto != altto && alttof != null)
  {
    flush_altfile();
    fclose (alttof);
  }
  stop (23);
}

int present (qual_name, getval, ret_name)
int getval;
char *qual_name, *ret_name;
 {
   int ret_value;
   struct dsc$descriptor_s parse_desc, return_desc;
   parse_desc.dsc$w_length = strlen (qual_name);
   parse_desc.dsc$a_pointer = qual_name;
   parse_desc.dsc$b_class = DSC$K_CLASS_S;
   parse_desc.dsc$b_dtype = DSC$K_DTYPE_T;
   ret_value = cli$present (&parse_desc);
   if (ret_value != CLI$_ABSENT && ret_value != CLI$_NEGATED && getval)
    {
      int get_value;
      return_desc.dsc$b_class = DSC$K_CLASS_D;
      return_desc.dsc$b_dtype = DSC$K_DTYPE_T;
      return_desc.dsc$a_pointer = 0;
      get_value = cli$get_value (&parse_desc, &return_desc);
      return_desc.dsc$a_pointer[return_desc.dsc$w_length] = '\0';
      strcpy (ret_name, return_desc.dsc$a_pointer);
    }
/*   printf ("/%s: %d\n", qual_name, ret_value);      */
   return (ret_value);
 }

deletefiles()
{
  fclose (fromf);
  if (from_file_created) delete (fromfilename);
  if (eprintf != NULL)
  {
    fgetname (eprintf, tofilename);
    fclose (eprintf);
    delete (tofilename);
  }
}

long find_file (file_name, def_name, operation)
char *file_name, *def_name;                       /* default name */
 {
   long ret_status;
   struct dsc$descriptor_s file_desc, def_desc, result_desc;
   char *msg;
   char *ptr;
   int version_present = FALSE;

/* If we find any characters after a semi-colon, a version was
   specified.  */

   if (strcspn (file_name, ";") < strlen (file_name) - 1)
     version_present = TRUE;
   file_desc.dsc$w_length = strlen (file_name);
   file_desc.dsc$a_pointer = file_name;
   file_desc.dsc$b_dtype = DSC$K_DTYPE_T;
   file_desc.dsc$b_class = DSC$K_CLASS_S;
   result_desc.dsc$a_pointer = 0;
   result_desc.dsc$b_class = DSC$K_CLASS_D;
   result_desc.dsc$b_dtype = DSC$K_DTYPE_T;
   def_desc.dsc$w_length = strlen (def_name);
   def_desc.dsc$a_pointer = def_name;
   def_desc.dsc$b_dtype = DSC$K_DTYPE_T;
   def_desc.dsc$b_class = DSC$K_CLASS_S;
   switch
((ret_status = lib$find_file (&file_desc, &result_desc, &0, &def_desc)) & 0xfff)
    {
      case RMS$_ACT & 0xfff : msg = "File in use";
                              break;

      case RMS$_DEV & 0xfff : msg = "Error in device spec";
                              break;

      case RMS$_DIR & 0xfff : msg = "Error in directory name";
                              break;

      case RMS$_DNF & 0xfff : msg = "Directory not found";
                              break;

      case RMS$_FLK & 0xfff : msg = "File locked";
                              break;

      case RMS$_FNF & 0xfff : msg = "File not found";
                              break;

      case RMS$_PRV & 0xfff : msg = "File protected";
                              break;
 
      case RMS$_SUC & 0xfff : msg = "File found";
                              break;

      case RMS$_SYN & 0xfff : msg = "Syntax error";
                              break;

      case RMS$_TYP & 0xfff : msg = "Error in file type";
                              break;

      case RMS$_VER & 0xfff : msg = "Error in version number";
                              break;

      case RMS$_WLD & 0xfff : msg = "Invalid wild card operation";
                              break;

      default :               msg = "Unknown status code";
                              break;
    }
   if (result_desc.dsc$a_pointer != 0)
   {
    strncpy (file_name, result_desc.dsc$a_pointer, result_desc.dsc$w_length);
    file_name[result_desc.dsc$w_length] = '\0';
   }
   else strcpy (file_name, "");
 if (ret_status != RMS$_SUC && (operation != OUTPUT || ret_status != RMS$_FNF))
      fprintf (stderr, "%s: %s\n", msg, file_name);

/* If we're outputting, we want to strip off the version number if one wasn't
   specified, otherwise it might collide with an existing file.        */

   if (operation == OUTPUT && (! version_present))
   {
     ptr = strchr (file_name, ';');
     *ptr = '\0';
   }
   return (ret_status);
 }

proctab (n)
int n;
{
  if (n <= 0)
  {
    n = 1;
    if (swwarn) writef ("Tab set to %d\n", n);
  }
  tabvalue = n;
}
