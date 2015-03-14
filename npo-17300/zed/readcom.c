/* section  "readcom" */

#include "glob"

/******************************************************************************
 *                                                                            *
 *   This section contains procedure rdcomseq, which parses a command         *
 *  sequence and creates a data structure representing it, and also the       *
 *  procedures it calls.                                                      *
 *                                                                            *
 *                                                                            *
 *****************************************************************************/

int rdcomseq (add)
struct com_rec *(*add);
{
   /***************************************************************************
    *   Parses a command sequence defined as either  [<separator>] <command>  *
    *  <command sequence>  or  [<separator>] <terminator>.   A pointer to the *
    *  result is placed in the location pointed to by add.                    *
    **************************************************************************/

   int *getcomsp();
   struct com_rec *(*rdscom());
   char *str = "    ";
   do
   {
     rdswithch();
     while (withch == ';')
     {
       rdwithch();
       rdswithch();
     }
     if (withch == '\\') while (withch != '\n') rdwithch();
     if (withch == '\n')
      {
       withcol = 0;
       if (clevel == 1)
       {
         *add = null;
         return;
       }
       else
       {
         if (interactive)
           wrch ('+');
         rdwithch();
         if (withch == ENDSTREAMCH)
           parseerror ('p');
         continue;
       }
     }
     if (withch == ')')
     {
       if (clevel < 2) parseerror('p');
       *add = null;
       return;
     }
     if (withch == '(')
     {
       struct com_rec *ptr = getcomsp (4);
       *add = ptr;
       ptr->name = CINT;
       ptr->arg1 = 1;
       if (debug) writef ("rdcomseq calling rdcomgp\n");
       rdcomgp (ptr->arg2);
       add = &ptr->nextcom;
     }
     else
     {
       rdcname (str);
       add = rdscom (add, str);
     }
   } while (TRUE);
}

struct com_rec *(*rdscom (add, str))
struct com_rec *(*add);
char *str;
{
   /***************************************************************************
    *   Parses a simple command defined as  <command name> [<argument 1>      *
    *  [<argument 2>]].   A pointer to it is placed at the location indicated *
    *  by add and the result returned is an address for a pointer to the next *
    *  command. The command name is read from str.                            *
    **************************************************************************/

   int *getcomsp();
   struct com_rec *ptr = getcomsp (4);
   *add = ptr;

   if (debug) writef ("rdscom: str = %s\n", str);  

   switch (strlen (str))
   {
     case 0 : ptr->name = CINT;
              ptr->arg1 = rdwithn();
              rdwithch();
              rdcomgp (&(ptr->arg2));
              break;

     case 1 : switch (str[0])
              {
                case '?' : ptr->name = CQM;
                           break;

                case '!' : ptr->name = CEM;
                           break;

                case '=' : ptr->name = CEQ;
                           rdint (&(ptr->arg1));
                           break;

                case '\'' : ptr->name = CRLSC;
                            break;

                case '#' : ptr->name = CDEL;
                           break;

                case '<' : ptr->name = CBRA;
                           break;

                case '>' : ptr->name = CKET;
                           break;

                case '%' : ptr->name = CPER;
                           break;

                case '_' : ptr->name = CSPC;
                           break;

                case '$' : ptr->name = CDOL;
                           break;

                case 'A' : ptr->name = CA;
                           rdqstst (&(ptr->arg1), &(ptr->arg2));
                           break;

                case 'B' : ptr->name = CB;
                           rdqstst (&(ptr->arg1), &(ptr->arg2));
                           break;

                case 'C' : ptr->name = CC;
                           rdsource (&ptr, TRUE);
                           break;

                case 'D' : ptr->name = CD;
                           rd2lnno (&(ptr->arg1), &(ptr->arg2));
                           break;

                case 'E' : ptr->name = CE;
                           rdqstst (&(ptr->arg1), &(ptr->arg2));
                           break;

                case 'F' : ptr->name = CF;
                           rdse (&(ptr->arg1));
                           break;

                case 'H' : ptr->name = CH;
                           rdlnno (&(ptr->arg1));
                           break;

                case 'I' : ptr->name = CI;
                           rdlnno (&(ptr->arg1));
                           rdtext (&ptr);
                           break;

                case 'M' : ptr->name = CM;
                           rdlnmno (&(ptr->arg1));
                           break;

                case 'N' : ptr->name = CN;
                           break;

                case 'P' : ptr->name = CP;
                           break;

                case 'Q' : ptr->name = CQ;
                           break;

                case 'R' : ptr->name = CR;
                           rd2lnno (&(ptr->arg1), &(ptr->arg2));
                           rdtext (&ptr);
                           break;

                case 'T' : ptr->name = CT;
                           rdtnum (&(ptr->arg1));
                           break;

                case 'V' : ptr->name = CV;
                           rdsw (&(ptr->arg1));
                           break;

                case 'W' : ptr->name = CW;
                           break;

                case 'X' : ptr->name = CX;
                           rdsw (&(ptr->arg1));
                           break;

                case 'Z' : ptr->name = CZ;
                           rdmaxst (&(ptr->arg1), &(ptr->arg2), 16);
                           break;

                case '\f' :
                case '\r' : parseerror ('f');
                            break;

                default  : parseerror ('c');
                           break;
              }
              break;

              case 2 : switch (str[0])
                       {
                         case 'A' : if (str[1] == 'P')
                                    {
                                      ptr->name = CAP;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;
                                      
                         case 'B' : if (str[1] == 'F')
                                    {
                                      ptr->name = CBF;
                                      rdse (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'P')
                                    {
                                      ptr->name = CBP;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'C' : if (str[1] == 'C')
                                    {
                                      ptr->name = CCC;
                                      rdmaxst (&(ptr->arg1), &(ptr->arg2), 1);
                                    }
                                    else if (str[1] == 'G')
                                    {
                                      ptr->name = CCG;
                                      rdtnum (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'L')
                                    {
                                      ptr->name = CCL;
                                      rdst (&(ptr->arg1), &(ptr->arg2), TRUE);
                                    }
                                    else if (str[1] == 'S')
                                    {
                                      ptr->name = CCS;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'D' : if (str[1] == 'F')
                                    {
                                      ptr->name = CDF;
                                      rdse (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'G')
                                    {
                                      ptr->name = CDG;
                                      rdtnum (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'O')
                                    {
                                      ptr->name = CDO;
                                      rdpname (&(ptr->arg1), FALSE);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'E' : if (str[1] == 'G')
                                    {
                                      ptr->name = CEG;
                                      rdtnum (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'P')
                                    {
                                      ptr->name = CEP;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'F' : if (str[1] == 'N')
                                    {
                                      ptr->name = CFN;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'G' : if (str[1] == 'A')
                                    {
                                      ptr->name = CGA;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                      dummycom (&ptr);
                                    }
                                    else if (str[1] == 'B')
                                    {
                                      ptr->name = CGB;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                      dummycom (&ptr);
                                    }
                                    else if (str[1] == 'E')
                                    {
                                      ptr->name = CGE;
                                      rdqstst (&(ptr->arg1), &(ptr->arg2));
                                      dummycom (&ptr);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'I' : if (str[1] == 'C')
                                      ptr->name = CIC;
                                    else if (str[1] == 'F')
                                    {
                                      ptr->name = CIF;
                                      rdse (&(ptr->arg1));
                                      rdthenelse (&ptr);
                                    }
                                    else if (str[1] == 'S')
                                    {
                                      ptr->name = CIS;
                                      rdst (&(ptr->arg1), &(ptr->arg2), TRUE);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'L' : if (str[1] == 'C')
                                    {
                                      ptr->name = CLC;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'O' : if (str[1] == 'N')
                                    {
                                      ptr->name = CON;
                                      rdse (&(ptr->arg1));
                                      rdcomgp (&(ptr->arg2));
                                      dummycom (&ptr);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'P' : if (str[1] == 'A')
                                    {
                                      ptr->name = CPA;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'B')
                                    {
                                      ptr->name = CPB;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'R')
                                    ptr->name = CPR;
                                    else parseerror ('c');
                                    break;

                         case 'S' : if (str[1] == 'A')
                                    {
                                      ptr->name = CSA;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'B')
                                    {
                                      ptr->name = CSB;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'T' : if (str[1] == 'L')
                                    {
                                      ptr->name = CTL;
                                      rdtnum (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'O')
                                    {
                                      ptr->name = CTO;
                                      if (! rdbuff (&ptr, FALSE))
                                         parseerror ('i');
                                    }
                                    else if (str[1] == 'R')
                                    {
                                      ptr->name = CTR;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'U' : if (str[1] == 'C')
                                    {
                                      ptr->name = CUC;
                                      rdqst (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'L')
                                    {
                                      ptr->name = CUL;
                                      rdse (&(ptr->arg1));
                                      rdthenelse (&ptr);
                                    }
                                    else if (str[1] == 'T')
                                    {
                                      ptr->name = CUT;
                                      rdse (&(ptr->arg1));
                                      rdcomgp (&(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'V' : if (str[1] == 'E')
                                    {
                                      ptr->name = CVE;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'G')
                                    {
                                      ptr->name = CVG;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'I')
                                    {
                                      ptr->name = CVI;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'N')
                                    {
                                      ptr->name = CVN;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'T')
                                    {
                                      ptr->name = CVT;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'X')
                                    {
                                      ptr->name = CVX;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'W' : if (str[1] == 'H')
                                    {
                                      ptr->name = CWH;
                                      rdse (&(ptr->arg1));
                                      rdcomgp (&(ptr->arg2));
                                     }
                                     else parseerror ('c');
                                     break;

                           default : parseerror ('c');
                                     break;
                       }
                       break;

              case 3 : switch (str[0])
                       {
                         case 'A' : if (str[1] == 'G' && str[2] == 'P')
                                      ptr->name = CAGP;
                                    else parseerror ('c');
                                    break;

                         case 'D' : if (str[1] == 'F')
                                    {
                                      if (str[2] == 'A')
                                      {
                                        ptr->name = CDFA;
                                        rdqst (&(ptr->arg1));
                                      }
                                      else if (str[2] == 'B')
                                      {
                                        ptr->name = CDFB;
                                        rdqst (&(ptr->arg1));
                                      }
                                      else parseerror ('c');
                                    }
                                    else if (str[1] == 'T')
                                    {
                                      if (str[2] == 'A')
                                      {
                                        ptr->name = CDTA;
                                        rdqst (&(ptr->arg1));
                                      }
                                      else if (str[2] == 'B')
                                      {
                                        ptr->name = CDTB;
                                        rdqst (&(ptr->arg1));
                                      }
                                      else parseerror ('c');
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'L' : if (str[1] == 'C' && str[2] == 'L')
                                      ptr->name = CLCL;
                                    else parseerror ('c');
                                    break;

                         case 'R' : if (str[1] == 'P' && str[2] == 'T')
                                    {
                                      ptr->name = CRPT;
                                      rdcomgp (&(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'S' : if (str[1] == 'H')
                                       if (str[2] == 'C')
                                       {
                                         ptr->name = CSHC;
                                         rdqst (&(ptr->arg1));
                                       }
                                       else if (str[2] == 'D')
                                          ptr->name = CSHD;
                                       else if (str[2] == 'F')
                                         ptr->name = CSHF;
                                       else if (str[2] == 'G')
                                       {
                                         ptr->name = CSHG;
                                         rdtnum (&(ptr->arg1));
                                       }
                                       else if (str[2] == 'S')
                                          ptr->name = CSHS;
                                       else parseerror ('c');
                                    else parseerror ('c');
                                    break;

                         case 'T' : if (str[1] == 'A' && str[2] == 'B')
                                    {
                                      ptr->name = CTAB;
                                      rdnum (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'U' : if (str[1] == 'C' && str[2] == 'L')
                                      ptr->name = CUCL;
                                    else parseerror ('c');
                                    break;

                          default : parseerror ('c');
                                    break;
                       }
                       break;

              case 4 : switch (str[0])
                       {
                         case 'C' : if (str[1] == 'O' && str[2] == 'M' &&
                                        str[3] == 'M')
                                    {
                                      ptr->name = CCOMM;
                                      rdst (&(ptr->arg1), &(ptr->arg2), FALSE);
                                    }
                                    else if (str[1] == 'P' && str[2] == 'R' &&
                                             str[3] == 'O')
                                    {
                                      ptr->name = CCPROC;
                                      rdpname (&(ptr->arg1), TRUE);
                                    }
                                    else if (str[1] == 'O' && str[2] == 'L' &&
                                             str[3] == 'S')
                                    {
                                      ptr->name = CCOLS;
                                      rdtnum (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'D' : if (str[1] == 'B' && str[2] == 'U' &&
                                        str[3] == 'F')
                                    {
                                      ptr->name = CDBUFF;
                                      rdbuffno (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'E' && str[2] == 'T' &&
                                             str[3] == 'A')
                                    {
                                      ptr->name = CDETAB;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'R' && str[2] == 'E' &&
                                             str[3] == 'S')
                                       ptr->name = CDREST;
                                    else if (str[1] == 'E' && str[2] == 'B' &&
                                             str[3] == 'U')
                                       ptr->name = CDEBUG;
                                    else parseerror ('c');
                                    break;

                         case 'E' : if (str[1] == 'R' && str[2] == 'R' &&
                                        str[3] == 'S')
                                    {
                                      ptr->name = CERRSTOP;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'F' : if (str[1] == 'L' && str[2] == 'U' &&
                                        str[3] == 'S')
                                       ptr->name = CFLUSH;
                                    else if (str[1] == 'R' && str[2] == 'O' &&
                                             str[3] == 'M')
                                    {
                                      ptr->name = CFROM;
                                      if (! rdbuff (&ptr, FALSE))
                                         parseerror ('i');
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'H' : if (str[1] == 'E' && str[2] == 'L' &&
                                    str[3] == 'P')
                                       ptr->name = CHELP;
                                    else parseerror ('c');
                                    break;

                         case 'I' : if (str[1] == 'F' && str[2] == 'E' &&
                                        str[3] == 'O')
                                    {
                                      ptr->name = CIFEOF;
                                      rdthenelse (&ptr);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'M' : if (str[1] == 'X' && str[2] == 'L' &&
                                        str[3] == 'L')
                                    {
                                      ptr->name = CMXLL;
                                      rdnum (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'P' : if (str[1] == 'R' && str[2] == 'O' &&
                                        str[3] == 'C')
                                    {
                                      ptr->name = CPROC;
                                      rdpname (&(ptr->arg1), FALSE);
                                      rdcomgp (&(ptr->arg2));
                                      dummycom (&ptr);
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'S' : if (str[1] == 'H' && str[2] == 'B' &&
                                        str[3] == 'U')
                                       ptr->name = CSHBUFF;
                                    else if (str[1] == 'H' && str[2] == 'P' &&
                                             str[3] == 'R')
                                    {
                                      ptr->name = CSHPROC;
                                      rdpname (&(ptr->arg1), TRUE);
                                    }
                                    else if (str[1] == 'T' && str[2] == 'O' &&
                                             str[3] == 'P')
                                       ptr->name = CSTOP;
                                    else parseerror ('c');
                                    break;

                         case 'T' : if (str[1] == 'B' && str[2] == 'U' &&
                                        str[3] == 'F')
                                    {
                                      ptr->name = CTBUFF;
                                      rdbuffno (&(ptr->arg1));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'U' : if (str[1] == 'L' && str[2] == 'E' &&
                                        str[3] == 'O')
                                    {
                                      ptr->name = CULEOF;
                                      rdthenelse (&ptr);
                                    }
                                    else if (str[1] == 'N' && str[2] == 'D' &&
                                             str[3] == 'O')
                                       ptr->name = CUNDO;
                                    else if (str[1] == 'T' && str[2] == 'E' &&
                                             str[3] == 'O')
                                    {
                                      ptr->name = CUTEOF;
                                      rdcomgp (&(ptr->arg2));
                                    }
                                    else parseerror ('c');
                                    break;

                         case 'W' : if (str[1] == 'A' && str[2] == 'R' &&
                                        str[3] == 'N')
                                    {
                                      ptr->name = CWARN;
                                      rdsw (&(ptr->arg1));
                                    }
                                    else if (str[1] == 'O' && str[2] == 'R' &&
                                             str[3] == 'D')
                                    {
                                      ptr->name = CWORD;
                                      rdst (&(ptr->arg1), &(ptr->arg2), TRUE);
                                    }
                                    else parseerror ('c');
                                    break;

                          default : parseerror ('c');
                                    break;
                       }
                       break;

             default : parseerror ('c');
                       break;
   }
   return &ptr->nextcom;
}

/*****************************************************************
 *   These procedures parse value arguments and place the result *
 *  in the given address.                                        *
 ****************************************************************/

rdsw (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdsw (%d)\n", arg);
   if (arg == PLUSSYM || arg == MINUSSYM)
   {
     rdwithch();
     *add = arg;
   }
   else parseerror ('a');
}

rdlnmno (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdlnmno (%d)\n", arg);
   if (arg == CURRSYM || arg == OTHERSYM) parseerror ('a');
   rdwithch();
   *add = arg;
}

rdlnno (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdlnno (%d)\n", arg);
   if (arg == PLUSSYM || arg == MINUSSYM) arg = OTHERSYM;
   if (arg != OTHERSYM) rdwithch();
   *add = arg;
}

rdtnum (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdtnum (%d)\n", arg);
   if (!(arg > 0 || arg == PLUSSYM)) arg = OTHERSYM;
   if (arg != OTHERSYM) rdwithch();
   *add = arg;
}

rd2lnno (add1, add2)
int *add1, *add2;
{
   *add1 = rdvalue();
   if (*add1 == PLUSSYM || *add1 == MINUSSYM) parseerror ('i');
   if (*add1 == OTHERSYM)
   {
     *add2 = OTHERSYM;
     return;
   }
   rdwithch();
   *add2 = rdvalue();
   if (debug) writef ("rd2lnno (%d, %d)\n", *add1, *add2);
   if (*add2 == PLUSSYM || *add2 == MINUSSYM) parseerror ('i');
   if (*add2 == OTHERSYM) return;
   if (0 < *add1 && 0 < *add2 && *add2 < *add1) parseerror ('i');
   rdwithch();
}

rdnum (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdnum (%d)\n", arg);
   if (arg < 0)
     arg = OTHERSYM;
   else rdwithch();
   *add = arg;
}

rdint (add)
int *add;
{
   int arg = rdvalue();
   if (debug) writef ("rdint (%d)\n", arg);
   if (arg < 0) parseerror ('a');
   rdwithch();
   *add = arg;
}

rdbuffno (add)
int *add;
{
   int temp;
   rdswithch();
   if (isdigit (withch))
   {
     temp = rdwithn();
     if (debug) writef ("rdbuffno (%d)\n", temp);
     if (temp > numbuff) parseerror ('a');
     rdwithch();
   }
   else temp = 0;
   *add = temp;
}

/*******************************************************
 * Parse a buffer specification.  If namereqd is TRUE, *
 * it must be present (no defaulting to zero).         *
 *  Modified to allow filenames for FROM/s/ and TO/s/. *
 * Sets arg2, which is read by procfrombuff etc.  It   *
 * doesn't matter for the I BUFFn commands.            *
 ******************************************************/

int rdbuff (add, namereqd)
int namereqd;
struct com_rec *(*add);
{
  struct com_rec *ptr;
  char *rds();
  char st[4];
  ptr = *add;
  rdswithch();
  if (isntsep (withch))
  {
    if (isalpha (withch))
    {
      rdcname (st);
      if (strlen (st) == 4)
      {
        if (st[0] == 'B' && st[1] == 'U' && st[2] == 'F' && st[3] == 'F')
        {
          rdbuffno (&(ptr->arg1));
          ptr->arg2 = IMBUFF;
          return (TRUE);
        }
        else if (st[0] == 'C' && st[1] == 'O' && st[2] == 'P' && st[3] == 'Y')
        {
          rdbuffno (&(ptr->arg1));
          ptr->arg2 = IMCOPY;
          return (FALSE);
        }
        else parseerror ('a');
      }
      else parseerror ('a');
    }
    else
    {
      ptr->arg2 = IMBUFF;
      if (namereqd) parseerror ('a');
         else ptr->arg1 = 0;
    }
  }
  else
  {
    ptr->arg2 = IMFILE;
    ptr->arg1 = rds (FALSE);
    if (withch != '\n') rdwithch();
  }
  if (debug) writef ("rdbuff: argument = %s%d\n", st, ptr->arg1);
  return (TRUE);
}

int rdvalue()
{
   /*******************************************************************
    *   Returns a number or +, -, * or . if they are next on the CIS. *
    ******************************************************************/

   int n;
   rdswithch();
   if (isdigit (withch))
   {
     n = rdwithn();
     if (n == 0) parseerror ('z');
   }
   else switch (withch)
   {
     case '+' : n = PLUSSYM;
                break;

     case '-' : n = MINUSSYM;
                break;

     case '.' : n = CURRSYM;
                break;

     case '*' : n = ENDSYM;
                break;

     default  : n = OTHERSYM;
                break;
   }
   if (debug) writef ("rdvalue (%d)\n", n);
   return n;
}

int rdwithn()
{
   /*******************************
    *   Reads a positive integer. *
    ******************************/

   int sum = 0;
   do
   {
     sum = (sum * 10) + withch - '0';
     withch = rdch();
     if (! isdigit(withch)) break;
     withcol = withcol + 1;
   } while (TRUE);
   ungetchar (withch);
   if (sum <= 0) parseerror ('z');
   if (debug) writef ("rdwithn (%d)\n", sum);
   return sum;
}

/**********************************************************************
 *   These procedures parse names, keeping the first four characters. *
 *********************************************************************/

rdpname (add, nullallowed)
int *add, nullallowed;
{
   /****************************************************************
    *   nullallowed == TRUE implies that the argument is optional. *
    ***************************************************************/

   int *getcomsp();
   int *ptr = getcomsp (namesize);
   *add = ptr;
   rdswithch();
   withch = capitalch (withch);
   if (isalpha (withch))
     rdcname (ptr);
   else if (nullallowed)
      ptr[0] = '\0';
   else parseerror ('a');
}

rdcname (str)
char *str;
{
   /**************************************************************
    *   Reads a name and places the first four letters into str. *
    *************************************************************/

   int i = 1;
   if (isdigit (withch))
   {
     str[0] = '\0';
     return;
   }
   str[0] = capitalch (withch);
   if (isalpha (str[0]))
   {
      do
      {
        rdwithch();
        withch = capitalch (withch);
        if (! isalpha (withch)) break;
        if (i > 3) continue;
        i = i + 1;
        str[i-1] = withch;
      } while (TRUE);
   }
   else rdwithch();
   str[i] = '\0';
}

/*******************************************************************
 *   These procedures parse string and search expression arguments *
 ******************************************************************/

rdst (add1, add2, hexok)
char *add1, *(*add2);
int hexok;
{
   /**************************************************************
    *   Parses a string, the string separator is placed in add1. *
    *************************************************************/

   char *rds();
   rdswithch();
   *add1 = withch;
   *add2 = rds (hexok);

   if (debug) writef ("rdst (%c, %s, %d)\n", withch, *add2, hexok);

   if (withch != '\n') rdwithch();
}

rdmaxst (add1, add2, maxn)
char *add1, *(*add2);
int maxn;
{
   /********************************************************
    *   Parses a string of maximum length maxn, the string *
    *  separator is placed in add1.                        *
    *******************************************************/

   char *rds();
   rdswithch();
   *add1 = withch;
   *add2 = rds (TRUE);
   if (strlen (*add2) > maxn) parseerror ('s');
   if (debug) writef ("rdmaxst (%c, %s)\n", withch, *add2);
   if (withch != '\n') rdwithch();
}

rdqst (add)
int *(*add);
{
   /********************************
    *   Parses a qualified string. *
    *******************************/

   int *rdqs();
   *add = rdqs (FALSE);
/*   if (debug) writef ("rdqst (%s)\n", (*add)[QSTR]);           */
   if (withch != '\n') rdwithch();
}

rdqstst (add1, add2)
char *(add1[]), *(add2[]);
{
   /***************************************************************
    *   Parses a qualified string followed by an unqualified one. *
    **************************************************************/

   int *rdqs();
   char *rds();
   int *getcomsp();
   *add1 = rdqs (FALSE);
   if (withch == '\n')
   {
     *add2 = getcomsp (1);
     (*add2)[0] = '\0';
   }
   else
   {
     *add2 = rds (TRUE);
     if (withch != '\n') rdwithch();
   }
}

rdse (add)
struct se_rec *(*add);
{
   /**************************************************************************
    *   Reads a search expression, which is either &, a qualified string or  *
    *  a sequence of them enclosed in brackets.                              *
    *************************************************************************/

   int *rdqs();
   int *getcomsp();
   struct se_rec *ptr;
   rdswithch();
   if (withch == '&' || withch == '\n' || withch == ';')
   {
     ptr = csebuff;
     if (withch != '\n') rdwithch();
   }
   else
   {
     ptr = getcomsp (SESIZE);
     if (withch == '(')
     {
       ptr->SEQS = null;
       if (withch != '\n') rdwithch();
/*       if (debug) writef ("&(ptr->SESEQ) = %d\n", &(ptr->SESEQ));    */
       rdseseq (&(ptr->SESEQ));
       if (withch != ')') parseerror ('i');
       rdwithch();
     }
     else
     {
       ptr->SESEQ = null;
       ptr->SEQS = rdqs (TRUE);
       if (withch != '\n') rdwithch();
     }
     ptr->SENEXT = null;
   }
   *add = ptr;
}

/********************************************
 * This used to be bound into the procedure *
 * following it                             *
 *******************************************/

find()
{
  /*****************************************************************
   *   Finds the next significant character, ignoring new lines or *
   *  comments.                                                    *
   ****************************************************************/

  while (withch == '\\' || withch == '\n' || withch == ' ')
  {
    rdswithch();
    if (withch == '\\')
      while (withch != '\n')
        rdwithch();
    if (withch != '\n')
      break;
    if (interactive)
      wrch ('+');
    withcol = 0;
    rdwithch();
  }
}

rdseseq (add)
struct se_rec *(*add);
{
   /*******************************************************************
    *   Reads a sequence of qualified string arguments separated by & *
    *  or | until a closing bracket is reached.                       *
    ******************************************************************/

   do
   {
     find();
     if (withch == '&') parseerror ('i');
     rdse (add);
     find();
     if (withch == '&')
       (*add)->SEAND = TRUE;
     else if (withch == '|')
       (*add)->SEAND = FALSE;
     else return;
     rdwithch();
     add = &((*add)->SENEXT);
   } while (TRUE);
}

int *rdqs (nallowed)
int nallowed;
{
   /**********************************************************************
    *   Reads a qualified string argument. nallowed is TRUE implies that *
    *  the n qualifier is permissable.                                   *
    *********************************************************************/

   char *rds();
   char savewithch;
   struct se_rec *septr;
   int *getcomsp();
   int *ptr = getcomsp (QSTR + 1);
   int i;
   for (i = QN; i <= QWI; i++) ptr[i] = FALSE;
   ptr[QINT] = 0;

/* Here's some fancy footwork that lets us parse an & as best we can */
   rdswithch();
   if (withch == '&')
   {
     if (cseopen)
     {
       septr = csebuff;
       while (septr->SESEQ != null) septr = septr->SESEQ;
       for (i = QN; i <= QSTR; i++) ptr[i] = (septr->SEQS)[i];
       rdwithch();
       rdswithch();
       ptr[QSEP] = withch;
       if (ptr[QN] && (! nallowed)) parseerror ('q');
       return ptr;
     }
     else parseerror ('e');
   }
   rdswithch();
   withch = capitalch (withch);
   while (isalnum (withch) || withch == '[' || withch == '_')
   {
     switch (withch)
     {
       case 'C' : if (ptr[QU] || ptr[QW] || ptr[QS] || ptr[QE] ||
                      ptr[QWI] || ptr[QP] || ptr[QL] || ptr[QINT] != 0)
                     parseerror ('q');
                  ptr[QC] = TRUE;
                  break;

       case 'N' : if ((! nallowed) || ptr[QN]) parseerror ('q');
                  ptr[QN] = TRUE;
                  break;

       case 'U' : if (ptr[QU]) parseerror ('q');
                  ptr[QU] = TRUE;
                  break;

       case 'W' : if (ptr[QW]) parseerror ('q');
                  ptr[QW] = TRUE;
                  break;

       case 'S' : if (ptr[QS]) parseerror ('q');
                  ptr[QS] = TRUE;
                  break;

       case 'E' : if (ptr[QINT] != 0 && (! ptr[QWI])) parseerror ('q');
                  ptr[QE] = TRUE;
                  ptr[QINT] = -1;
                  break;

       case 'B' : if (ptr[QINT] != 0 && (! ptr[QWI])) parseerror ('q');
                  ptr[QE] = FALSE;
                  ptr[QINT] = -1;
                  break;

       case 'P' : if (ptr[QINT] != 0 && (! ptr[QWI])) parseerror ('q');
                  ptr[QP] = TRUE;
                  ptr[QINT] = -1;
                  break;

       case 'L' : if (ptr[QL] || (ptr[QINT] < 0)) parseerror ('q');
                  if (ptr[QINT] == 0) ptr[QINT] = 1;
                  ptr[QL] = TRUE;
                  break;

       case '_' :
       case '[' : if (ptr[QINT] > 0) parseerror ('q');
                  ptr[QINT] = -1;
                  ptr[QWI] = TRUE;
                  savewithch = withch;
                  if (savewithch == '[') savewithch = ']';
                  rdwithch();
                  if (withch == ',')
                    ptr[QWL] = OTHERSYM;
                  else
                  {
                    if (! isdigit (withch)) parseerror ('u');
                    ptr[QWL] = rdwithn();
                    rdwithch();
                    if (withch != ',')
                    {
                      if (withch != savewithch) parseerror ('u');
                      else ptr[QWR] = ptr[QWL];
                      break;
                    }
                  }
                  rdwithch();
                  if (withch == savewithch && ptr[QWL] != OTHERSYM)
                    ptr[QWR] = OTHERSYM;
                  else
                  {
                    if (! isdigit (withch)) parseerror ('u');
                    ptr[QWR] = rdwithn();
                    rdwithch();
                    if (withch != savewithch) parseerror ('q');
                  }
                  break;

        default : if (isdigit (withch))
                  {
                    if (!(0 <= ptr[QINT] && ptr[QINT] <= 1)) parseerror ('q');
                    ptr[QINT] = rdwithn();
                  }
                  else parseerror ('u');
                  break;
     }
     rdwithch();
     rdswithch();
     withch = capitalch (withch);
   }
   if (ptr[QINT] == 0) ptr[QINT] = 1;
   ptr[QSEP] = withch;
   ptr[QSTR] = rds (TRUE);
   if (debug) writef ("rdqs: sep %c, str %s\n", ptr[QSEP], ptr[QSTR]);
   return ptr;
}

char *rds (hexok)
int hexok;
{
 /*****************************************************************************
  *   Reads a string argument from the CIS and returns a pointer to it. It is *
  *  stored in the command buffer.                                            *
  ****************************************************************************/

   char v[MAXSTRLENGTH];
   char sep = withch;
   char *ptr;
   int *getcomsp();
   int i;
   int size = 0;
   if (isntsep (sep))
      parseerror ('a');
   rdwithch();
   if (hexok && swx) rdsh (v, &size, sep);
   else do
   {
     if (withch == sep || withch == '\n') break;
     if (size == MAXSTRLENGTH - 1) parseerror ('s');
     v[size++] = withch;
     rdwithch();
   } while (TRUE);
   ptr = getcomsp (size / BYTESPERWORD + 1);
   ptr[size] = '\0';
   for (i = 0; i <= size-1; i++) ptr[i] = v[i];

/*   if (debug) writef ("rds: string %s\n", ptr);               */

   return ptr;
}

/*********************************************
 *   These procedures parse text arguments. *
 ********************************************/

rdtext (add)
struct com_rec *(*add);
{
   /**********************************************************
    *   Parses material to be inserted by an I or R command. *
    *********************************************************/

   int *getcomsp();
   struct com_rec *ptr = getcomsp (4);
   (*add)->nextcom = ptr;
   *add = ptr;
   rdswithch();
   if (withch == '\\') while (withch != '\n') rdwithch();
   if (withch == '\n')
   {
     if (clevel == 1)
        ptr->name = IMTEXT;
     else
     {
       char tempst[MAXSTRLENGTH];
       char *temp;
       int length;
       struct com_rec *(*add2) = &(ptr->arg1);
       ptr->name = IMINB;
       while (rdtextin (tempst))
       {
         int i;
         length = strlen (tempst) / BYTESPERWORD + 2;
         temp = getcomsp (length);
         for (i = 0; i <= strlen (tempst); i++) temp[i+4] = tempst[i];
         *add2 = temp;
         add2 = temp;
         if (debug) writef ("rdtext: %s\n", tempst);
       }
       *add2 = null;
     }
     return;
   }
   if (isntsep (withch))
   {
     if (rdbuff (&ptr, TRUE)) ptr->name = IMBUFF;
     else ptr->name = IMCOPY;
     if (ptr->arg1 == 0) parseerror ('a');
   }
   else
   {
     ptr->name = IMFILE;
     ptr->arg1 = rds (FALSE);
     if (withch != '\n') rdwithch();
   }
}

rdsource (add, copyok)
struct com_rec *(*add);
int copyok;
{
   int *getcomsp();
   struct com_rec *ptr = getcomsp (4);
   (*add)->nextcom = ptr;
   *add = ptr;
   if (isntsep (withch))
   {
     if (rdbuff (&ptr, FALSE)) ptr->name = IMBUFF;
     else if (copyok) ptr->name = IMCOPY;
          else parseerror ('i');
   }
   else
   {
     ptr->name = IMFILE;
     ptr->arg1 = rds (FALSE);
     if (withch != '\n') rdwithch();
   }
}

int isntsep (ch)
char ch;
{
   char sep = ch;
   return (sep != '/' && sep != '*' && sep != '+' && sep != '-' && sep != '.' &&
       sep != ',' && sep != ':' && sep != '?' && sep != '[' && sep != '\'' &&
       sep != '!' && sep != '"');
}

int rdtextin (st)
char *st;
{
   /********************************************************************
    *   Reads a line of text into the string variable st. Returns TRUE *
    *  unless the line contains the end_insert string (non-case-       *
    *  sensitive), or a control-Z (since it's so natural).             *
    *******************************************************************/

  int i = 0;
  char ch = rdch();
  int secondch = FALSE;
  char hexdig();
  char cur;
  if (swx) while (ch == ' ') ch = rdch();
  if (swx) while (ch != '\n' && ch != ENDSTREAMCH)
  {
     if (! isxdigit (ch))
     {                             /* Make sure withch isn't \n or ^Z */
       withch = ch;
       parseerror ('x');
     }
     if (i == mxll)
     {
       ungetchar (ch);
       if (swwarn) writef ("WARNING - input line too long\n");
       break;
     }
     if (secondch)
     {
       st[i++] = ((cur << 4) + hexdig (ch)) & 0xff;
       secondch = FALSE;
     }
     else
     {
       cur = hexdig (ch);
       secondch = TRUE;
     }
     ch = rdch();
     while (ch == ' ') ch = rdch();           /* Spaces ignored in hex mode */
   }
   else while (ch != '\n' && ch != ENDSTREAMCH)
   {
     if (i == mxll)
     {
       ungetchar (ch);
       if (swwarn) writef ("WARNING - input line too long\n");
       break;
     }
     st[i++] = ch;
     ch = rdch();
   }
   st[i] = '\0';
   if (secondch) parseerror ('x');

/*   if (debug) writef ("rdtextin: %s\n", st);           */

   if (i == strlen (endinsertstr))
   {
      char temp[MAXSTRLENGTH];
      int j;
      for (j = 0; j <= strlen (st); j++) temp[j] = toupper (st[j]);
      if (strcmp (temp, endinsertstr) == 0) return FALSE;
   }

/* ^Z is an alternate end-of-input... */

   if (ch == ENDSTREAMCH) return FALSE;
   return TRUE;
}

/*****************************************
 *   These parse command group arguments *
 ****************************************/

rdcomgp (add)
struct com_rec *(*add);
{
   /*******************************************************************
    *   Parses a command group defined as either  <simplecommand>  or *
    *  <bra> <command sequence> <ket>  . A pointer to the result is   *
    *  placed in address add.                                         *
    ******************************************************************/

   clevel = clevel + 1;
   if (clevel >= 20) parseerror ('l');
   rdswithch();
   if (withch == '\n') parseerror ('g');
   if (withch == '(')
   {
     rdwithch();
     rdcomseq (add);
     if (withch == ')')
     {
       if (clevel < 1) parseerror ('p');
       rdwithch();
     }
     else parseerror ('p');
   }
   else
   {
     char *str = "   ";
     struct com_rec *(*temp);
     rdcname (str);
     if (debug) writef ("rdcomgp calling rdscom on %s\n", str);
     temp = rdscom (add, str);
     *temp = null;

/* This is awful... */

     if ((*(add-2) == CINT) && ((*add)->name && cmask) == 0)
        *(add-1) = 1;
   }
   clevel = clevel - 1;
}

rdthenelse (add)
struct com_rec *(*add);
{
   /***********************************************************************
   /*   Parses the arguments of an IF type command, ie a THEN command     *
   /*  group followed by an indeterminate number of elif and elul command *
   /*  groups optionally followed by a ELSE command group.                *
   /**********************************************************************/

   char *str = "   ";
   int *getcomsp();
   struct com_rec *(*tempadd) = &((*add)->nextcom);
   rdswithch();
   if (withch == '\n') parseerror ('g');
   if (withch == '(')
     rdcomgp (&((*add)->arg2));
   else
   {
     rdcname (str);
     if (strlen (str) == 4 && str[0] == 'T' && str[1] == 'H' && str[2] == 'E' &&
         str[3] == 'N')
     {
       rdcomgp (&((*add)->arg2));
       if (debug) writef ("THEN\n");
     }
     else
     {
       struct com_rec *(*temp);
       clevel = clevel + 1;
       temp = rdscom (&((*add)->arg2), str);
       clevel = clevel - 1;
       *temp = null;
     }
   }
   rdswithch();
   if (withch != 'E' && withch != 'e') return;
   rdcname (str);
   if (strlen (str) == 4 && str[0] == 'E' && str[1] == 'L')
   {
     struct com_rec *ptr = getcomsp (4);
     *tempadd = ptr;
     *add = ptr;
     if (str[2] == 'S' && str[3] == 'E')
     {
       if (debug) writef ("ELSE\n");
       ptr->name = CELSE;
       rdcomgp (&(ptr->arg2));
     }
     else if (str[2] == 'I' && str[3] == 'F')
     {
       if (debug) writef ("ELIF\n");
       ptr->name = CELIF;
       rdse (&(ptr->arg1));
       rdthenelse (add);
     }
     else if (str[2] == 'U' && str[3] == 'L')
     {
       if (debug) writef ("ELUL\n");
       ptr->name = CELUL;
       rdse (&(ptr->arg1));
       rdthenelse (add);
     }
     else parseerror ('c');
   }
   else
   {
     rdscom (tempadd, str);
     *add = *tempadd;
   }
}

/*************************************
 *   Miscellanous parsing procedures *
 ************************************/

dummycom (add)
struct com_rec *(*add);
{
   int *getcomsp();
   struct com_rec *ptr = getcomsp (4);
   ptr->name = CDUMMY;
   (*add)->nextcom = ptr;
   *add = ptr;
}

rdwithch()
{
   if (user_level == 0) withcol = withcol + 1;
   withch = rdch();
   if (withch == ENDSTREAMCH && user_level != 0) closewith();
}

closewith()
{
  char s1[FILENAMELENGTH], s2[FILENAMELENGTH];
  FILE *tempf;
   if (debug) writef ("Closewith()\n");
  fgetname (withf, s1);
  fclose (withf);
  tempf = fopen (TEMPWITHFILE, "r");
  if (tempf != NULL)
  {
    if (strcmp (fgetname (tempf, s2), s1) == 0)
    {
      fclose (tempf);
      delete (s1);
    }
    else fclose (tempf);
  }
  withf = user_stack[--user_level];
  selectinput (withf);
  set_interactive();
  longjmp (jmpenv, 8);
}

rdswithch()
{
   /*****************************************************************
    *   Finds the next non blank character in the input starting at *
    *  the current character.                                       *
    ****************************************************************/

   while (withch == ' ')
   {
     if (user_level == 0) withcol = withcol + 1;
     withch = rdch();
     if (withch == ENDSTREAMCH && user_level != 0) closewith();
   }
}

parseerror (ch)
{
   /*********************************************************************
    *   Called if a parsing error is found. flags the error, prints out *
    *  an error message and terminates the run or abandons the current  *
    *  line of input and the contents of the current line buffer        *
    *  according to switch errstop.                                     *
    ********************************************************************/

   int i;
   for (i = 1; i <= withcol - 1 + cur_col; i++) wrch (' ');
   writef (">\n**");
   if (withch != ENDSTREAMCH)
      while (withch != '\n' && withch != ENDSTREAMCH) withch = rdch();
   withcol = 0;
   clevel = 1;
   switch (ch)
   {
     case 'a' : writef ("Argument expected");
                break;

     case 'b' : writef ("Too much inserted material");
                break;

     case 'c' : writef ("Unknown command");
                break;

     case 'e' : writef ("No previous search expression");
                break;

     case 'f' : writef ("Significant control character");
                break;

     case 'g' : writef ("Command group expected");
                break;

     case 'i' : writef ("Illegal argument");
                break;

     case 'l' : writef ("Commands nested too deep");
                break;

     case 'o' : writef ("Command buffer overflow, command sequence too long");
                break;

     case 'p' : writef ("Parenthesis error");
                break;

     case 'q' : writef ("Inconsistent or repeated string qualifiers");
                break;

     case 'r' : writef ("Command is not repeatable");
                break;

     case 's' : writef ("String argument too long");
                break;

     case 'u' : writef ("Unknown string qualifier");
                break;

     case 'x' : writef ("Invalid hex digit or odd number of characters");
                break;

     case 'z' : writef ("Zero not allowed");
                break;

      default : writef ("Unidentified parsing error");
                break;
   }
   wrch ('\n');
   if (swerrstop)
   {
     fclose (tof);
     delete (tofilename);
     deletefiles();
     stop (8);
   }
   longjmp (jmpenv, 8);
}

int *getcomsp (n)
int n;
{
  /*****************************************************************************
   *   Allocates a n word vector from the command buffer and returns a pointer *
   *  to it. Checks for overflow.                                              *
   ****************************************************************************/

   char *temp = ptrcbuff;
   ptrcbuff = ptrcbuff + n * BYTESPERWORD;
   if (ptrcbuff >= limcbuff) parseerror ('o');
   return temp;
}

/**************************************************************
 * Read a hex string into vector v, store the resulting size  *
 * in *asize, until reaching a separation character sep on \n *
 *************************************************************/

rdsh (v, asize, sep)
char *v, sep;
int *asize;
{
  int size = *asize;
  int secondch = FALSE;
  char hexdig();
  char cur;
  do
  {
    rdswithch();                               /* Spaces ignored in hex mode */
    if (withch == sep || withch == '\n') break;
    if (! isxdigit (withch)) parseerror ('x');
    if (size == mxll) parseerror ('s');
    if (secondch)
    {
      cur = (cur << 4) + hexdig (withch);
      v[size++] = cur & 0xff;
      secondch = FALSE;
    }
    else
    {
      cur = hexdig (withch);
      secondch = TRUE;
    }
    rdwithch();
  } while (TRUE);
  if (secondch) parseerror ('x');
  *asize = size;
}

char hexdig (ch)
char ch;
{
  if (isdigit (ch)) return ch - '0';
  else return toupper (ch) - 'A' + 10;
}
