/* section "write" */

#include "glob"

/*****************************************************************************
 *                                                                           *
 *     This section contains procedure wrcomseq, which prints the pointed    *
 *    to command sequence to the current output, and also those procedures   *
 *    which it calls.                                                        *
 *                                                                           *
 ****************************************************************************/

wrcomseq (ptr)
struct com_rec *ptr;
{
   /***********************************************************************
    *   Prints out the pointed to command sequence to the current output. *
    **********************************************************************/

   if (ptr == null) return;
   do
   {
     wrscom (&ptr);
     ptr = ptr->nextcom;
     if (ptr == null) return;
     writef ("; ");
   } while (TRUE);
}

wrscom (add)
struct com_rec *(*add);
{
   /*************************************************************************
    *   Print a single command which a pointer to is held in address add.   *
    *  add is given to the procedure as the command may be of more than one *
    *  command block.                                                       *
    ************************************************************************/

   struct com_rec *ptr = *add;
   switch (ptr->name)
   {
     case CINT : if (ptr->arg1 != 1) writen (ptr->arg1);
                 wrcomgp (ptr->arg2);
                 break;

     case CA : wrch ('A');
               wrqstst (ptr->arg1, ptr->arg2);
               break;

     case CB : wrch ('B');
               wrqstst (ptr->arg1, ptr->arg2);
               break;

     case CC : wrch ('C');
               wrtext (add);
               break;
               
     case CD : wrch ('D');
               wr2values (ptr->arg1, ptr->arg2);
               break;

     case CE : wrch ('E');
               wrqstst (ptr->arg1, ptr->arg2);
               break;

     case CF : wrch ('F');
               wrse (ptr->arg1, TRUE);
               break;

     case CH : wrch ('H');
               wrvalue (ptr->arg1);
               break;

     case CI : wrch ('I');
               wrvalue (ptr->arg1);
               wrtext (add);
               break;

     case CM : wrch ('M');
               wrvalue (ptr->arg1);
               break;

     case CN : wrch ('N');
               break;

     case CP : wrch ('P');
               break;

     case CQ : wrch ('Q');
               break;

     case CR : wrch ('R');
               wr2values (ptr->arg1, ptr->arg2);
               wrtext (add);
               break;

     case CT : wrch ('T');
               wrvalue (ptr->arg1);
               break;

     case CV : wrch ('V');
               wrvalue (ptr->arg1);
               break;

     case CW : wrch ('W');
               break;

     case CX : wrch ('X');
               wrvalue (ptr->arg1);
               break;

     case CZ : wrch ('Z');
               wrst (ptr->arg1, ptr->arg2, TRUE);
               break;

     case CAP : writef ("AP");
                wrqstst (ptr->arg1, ptr->arg2);
                break;

     case CBF : writef ("BF"); 
                wrse (ptr->arg1, TRUE);
                break;

     case CBP : writef ("BP");
                wrqstst (ptr->arg1, ptr->arg2);
                break;

     case CCC : writef ("CC");
                wrst (ptr->arg1, ptr->arg2, TRUE);
                break;

     case CCG : writef ("CG");
                wrvalue (ptr->arg1);
                break;

     case CCL : writef ("CL");
                wrst (ptr->arg1, ptr->arg2, TRUE);
                break;

     case CCS : writef ("CS");
                wrvalue (ptr->arg1);
                break;

     case CDF : writef ("DF");
                wrse (ptr->arg1, TRUE);
                break;

     case CDG : writef ("DG");
                wrvalue (ptr->arg1);
                break;

     case CDO : writef ("DO");
                wrname (ptr->arg1);
                break;

     case CEG : writef ("EG");
                wrvalue (ptr->arg1);
                break;

     case CEM : writef ("!");
                break;

     case CEP : writef ("EP");
                wrqstst (ptr->arg1, ptr->arg2);
                break;

     case CEQ : writef ("=");
                wrvalue (ptr->arg1);
                break;

     case CFN : writef ("FN");
                wrvalue (ptr->arg1);
                break;

     case CGA : writef ("GA");
                wrqstst (ptr->arg1, ptr->arg2);
                wrdummy (add);
                break;

     case CGB : writef ("GB");
                wrqstst (ptr->arg1, ptr->arg2);;
                wrdummy (add);
                break;

     case CGE : writef ("GE");
                wrqstst (ptr->arg1, ptr->arg2);;
                wrdummy (add);
                break;

     case CIC : writef ("IC");
                break;

     case CIF : writef ("IF");
                wrse (ptr->arg1, TRUE);
                wrthenelse (add);
                break;

     case CIS : writef ("IS");
                wrst (ptr->arg1, ptr->arg2, TRUE);
                break;

     case CLC : writef ("LC");
                wrqst (ptr->arg1);
                break;
                
     case CON : writef ("ON");
                wrse (ptr->arg1, TRUE);
                wrcomgp (ptr->arg2);;
                wrdummy (add);
                break;

     case CQM : writef ("?");
                break;

     case CPA : writef ("PA");
                wrqst (ptr->arg1);
                break;

     case CPB : writef ("PB");
                wrqst (ptr->arg1);
                break;

     case CPR : writef ("PR");
                break;

     case CSA : writef ("SA");
                wrqst (ptr->arg1);
                break;

     case CSB : writef ("SB");
                wrqst (ptr->arg1);
                break;

     case CTL : writef ("TL");
                wrvalue (ptr->arg1);
                break;

     case CTO : writef ("TO");
                wrbuff (ptr->arg1, ptr->arg2);
                break;

     case CTR : writef ("TR");
                wrvalue (ptr->arg1);
                break;

     case CUC : writef ("UC");
                wrqst (ptr->arg1);
                break;
                
     case CUL : writef ("UL");
                wrse (ptr->arg1, TRUE);
                wrthenelse (add);
                break;

     case CUT : writef ("UT");
                wrse (ptr->arg1, TRUE);
                wrcomgp (ptr->arg2);
                break;

     case CVE : writef ("VE");
                wrvalue (ptr->arg1);
                break;

     case CVG : writef ("VG");
                wrvalue (ptr->arg1);
                break;

     case CVI : writef ("VI");
                wrvalue (ptr->arg1);
                break;

     case CVN : writef ("VN");
                wrvalue (ptr->arg1);
                break;

     case CVT : writef ("VT");
                wrvalue (ptr->arg1);
                break;

     case CVX : writef ("VX");
                wrvalue (ptr->arg1);
                break;

     case CWH : writef ("WH");
                wrse (ptr->arg1, TRUE);
                wrcomgp (ptr->arg2);
                break;

     case CAGP : writef ("AGP");
                 break;

     case CBRA : writef ("<");
                 break;

     case CDEL : writef ("#");
                 break;

     case CDFA : writef ("DFA");
                 wrqst (ptr->arg1);
                 break;

     case CDFB : writef ("DFB");
                 wrqst (ptr->arg1);
                 break;

     case CDOL : writef ("$");
                 break;

     case CDTA : writef ("DTA");
                 wrqst (ptr->arg1);
                 break;

     case CDTB : writef ("DTB");
                 wrqst (ptr->arg1);
                 break;

     case CKET : writef (">");
                 break;

     case CLCL : writef ("LCL");
                 break;

     case CPER : writef ("%");
                 break;

     case CRPT : writef ("RPT");
                 wrcomgp (ptr->arg2);
                 break;

     case CSHC : writef ("SHC");
                 break;

     case CSHD : writef ("SHD");
                 break;

     case CSHF : writef ("SHF");
                 break;

     case CSHG : writef ("SHG");
                 wrvalue (ptr->arg1);
                 break;

     case CSHS : writef ("SHS");
                 break;

     case CSPC : writef ("_");
                 break;

     case CTAB : writef ("TAB");
                 wrvalue (ptr->arg1);
                 break;

     case CUCL : writef ("UCL");
                 break;

     case CCOLS : writef ("COLS");
                  break;

     case CCOMM : writef ("COMM");
                  wrst (ptr->arg1, ptr->arg2, FALSE);
                  break;

     case CCPROC : writef ("CPROC");
                   wrname (ptr->arg1);
                   break;

     case CDBUFF : writef ("DBUFF");
                   wrvalue (ptr->arg1);
                   break;

     case CDEBUG : writef ("DEBUG");
                   break;

     case CDETAB : writef ("DETAB");
                   wrvalue (ptr->arg1);
                   break;

     case CDREST : writef ("DREST");
                   break;

     case CERRSTOP : writef ("ERRSTOP");
                     wrvalue (ptr->arg1);
                     break;

     case CFLUSH : writef ("FLUSH");
                   break;

     case CFROM : writef ("FROM");
                  wrbuff (ptr->arg1, ptr->arg2);
                  break;

     case CHELP : writef ("HELP");
                  break;

     case CIFEOF : writef ("IFEOF");
                   wrthenelse (add);
                   break;

     case CMXLL : writef ("MXLL");
                  wrvalue (ptr->arg1);
                  break;

     case CPROC : writef ("PROC");
                  wrname (ptr->arg1);;
                  wrcomgp (ptr->arg2);
                  wrdummy (add);
                  break;

     case CRLSC : writef ("'");
                  break;

     case CSHBUFF : writef ("SHBUFF");
                    break;

     case CSHPROC : writef ("SHPROC");
                    wrname (ptr->arg1);
                    break;

     case CSTOP : writef ("STOP");
                  break;

     case CTBUFF : writef ("TBUFF");
                   wrvalue (ptr->arg1);
                   break;

     case CULEOF : writef ("ULEOF");
                   wrthenelse (add);
                   break;

     case CUNDO : writef ("UNDO");
                  break;

     case CUTEOF : writef ("UTEOF");
                   wrcomgp (ptr->arg2);
                   break;

     case CWARN : writef ("WARN");
                  wrvalue (ptr->arg1);
                  break;

     case CWORD : writef ("WORD");
                  wrst (ptr->arg1, ptr->arg2, TRUE);
                  break;

     case CDUMMY : writef ("DUMMY");
                   break;

     default : writef ("ERROR unrecognised command - %d\n", ptr->name);
               longjmp (jmpenv, 8);
               break;
   }
}

wrdummy (add)
struct com_rec *(*add);
{
   struct com_rec *ptr = (*add)->nextcom;

/* DCJM parentheses added to clear shg error */
/* writef (" wrdummy called\n");          */

   if (ptr != null) *add = ptr;
}

/***********************************************************************
 *     The following procedures are called by wrscom and print out the *
 *    command arguments they are given -                               *
 *                                                                     *
 *    wrvalue        outputs +, -, ., *, <integer> or <blank>          *
 *    wr2values      2 of the above                                    *
 *    wrname         a name                                            *
 *    wrst           a string bracketed by its separator               *
 *    wrqst          a qualified string                                *
 *    wrqstst        a qualified string followed by a unqualified one  *
 *    wrse           a search expression                               *
 *    wrcomgp        a command group                                   *
 *    wrtext         a text argument                                   *
 *    wrthenelse     conditional arguments                             *
 **********************************************************************/

wrvalue (no)
int no;
{
   switch (no)
   {
     case PLUSSYM : wrch ('+');
                    break;

     case MINUSSYM : wrch ('-');
                     break;

     case CURRSYM : wrch ('.');
                    break;

     case ENDSYM : wrch ('*');
                   break;

     case OTHERSYM :
     case 0 :
                     break;

     default : writen (no);
               break;
   }
}

wr2values (a1, a2)
int a1, a2;
{
   wrvalue (a1);
   if (a2 == OTHERSYM) return;
   wrch (' ');
   wrvalue (a2);
}

wrname (str)
char *str;
{
   wrch (' ');
   writef ("%s", str);
}

wrst (sep, str, hexok)
char sep, *str;
int hexok;
{
   wrch (sep);
   if (hexok) writeu ("%s", str); else writef ("%s", str);
   wrch (sep);
}

wrqst (qst)
int qst[];
{
   wrch (' ');
   if (qst[QC]) wrch ('C');
   if (qst[QN]) wrch ('N');
   if (qst[QW]) wrch ('W');
   if (qst[QU]) wrch ('U');
   if (qst[QS]) wrch ('S');
   if (qst[QINT] < 0)
   {
     if (qst[QWI])
     {
       wrch ('[');
       if (qst[QWL] != OTHERSYM) writen (qst[QWL]);
       wrch (',');
       if (qst[QWR] != OTHERSYM) writen (qst[QWR]);
       wrch (']');
     }
     else if (qst[QP])
        wrch ('P');
     else if (qst[QE])
        wrch ('E');
     else wrch ('B');
   }
   else
   {
     if (qst[QINT] != 1) writen (qst[QINT]);
     if (qst[QL]) wrch ('L');
   }
   wrch (qst[QSEP]);
   writeu ("%s", qst[QSTR]);
   wrch (qst[QSEP]);
}

wrqstst (qst, st)
int qst[];
char *st;
{
   wrqst (qst);
   writeu ("%s", st);
   wrch (qst[QSEP]);
}

wrse (se, check)
int check;
struct se_rec *se;
{
   if (check && (se == csebuff))
   {
     writef (" &");
     return;
   }
   if (se->SESEQ == null)
      wrqst (se->SEQS);
   else
   {
     writef (" (");
     wrse (se->SESEQ, TRUE);
     writef (")");
   }
   if (se->SENEXT == null) return;
   if (se->SEAND)
     writef (" &");
   else writef (" |");
   wrse (se->SENEXT, TRUE);
}

wrcomgp (ptr)
struct com_rec *ptr;
{
   writef (" ( ");
   wrcomseq (ptr);
   writef (")");
}

wrtext (add)
struct com_rec *(*add);
{
   /********************************************************************
    *   Shifts the pointer held in add on one command block and prints *
    *  out the new command block. gives an error if the new block does *
    *  not indicate inserted material.                                 *
    *******************************************************************/

   struct com_rec *ptr = (*add)->nextcom;
   struct gen_rec *gptr;
   *add = ptr;
   switch (ptr->name)
   {
     case IMTEXT : break;

     case IMBUFF : writef (" BUFF");
                   wrvalue (ptr->arg1);
                   break;

     case IMCOPY : writef (" COPY");
                   wrvalue (ptr->arg1);
                   break;

     case IMINB : gptr = ptr->arg1;
                  wrch ('\n');
                  while (gptr != null)
                  {
                    writeu ("%s", &gptr->stringb);
                    wrch ('\n');
                    gptr = gptr->nextg;
                  }
                  writeu ("%s\n", endinsertstr);
                  break;

     case IMFILE : writef ("/%s/", ptr->arg1);
                   break;

     default : writef ("ERROR inserted material lost");
               break;
   }
}

wrthenelse (add)
struct com_rec *(*add);
{
   /**********************************************************************
    *   Print out the arguments of an conditional command including any  *
    *  associated elif or elul commands. Moves the pointer in add on as  *
    *  required.                                                         *
    *********************************************************************/

   struct com_rec *com = *add;
   writef (" THEN");
   wrcomgp (com->arg2);
   com = com->nextcom;
   if (com == null) return;
   switch (com->name)
   {
     case CELSE : writef (" ELSE");
                  wrcomgp (com->arg2);
                  *add = com;
                  break;

     case CELIF : writef (" ELIF");
                  wrse (com->arg1, TRUE);
                  *add = com;
                  wrthenelse (add);
                  break;

     case CELUL : writef (" ELUL");
                  wrse (com->arg1, TRUE);
                  *add = com;
                  wrthenelse (add);
                  break;
   }
}

wrbuff (no, typ)
int no, typ;
{
  if (no == 0) return;
  if (typ != IMFILE)
  {
    writef (" BUFF");
    wrvalue (no);
  }
  else writef ("/%s/", no);
}
