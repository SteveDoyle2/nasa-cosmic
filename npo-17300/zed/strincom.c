/* section "stringcom" */

#include "glob"

/*****************************************************************************
 *                                                                           *
 *   Contains procedures to carry out string handling commands and the       *
 *  procedures they call in particular procedure findmatch which tries       *
 *  to find a qualified string in a string.                                  *
 *                                                                           *
 ****************************************************************************/

/************************************
 *   Current line buffer procedures *
 ***********************************/

getcl()
{
   /***************************************************************************
    *   Copies current line from the output queue to the current line buffer. *
    **************************************************************************/

   char *str = currentl->stringb;
   if (debug)
     writef ("getcl()\n");
   if (((currentl->symb) & endmask) != 0)
     execerror ('s');
   strcpy (clbuff, str);
   clboccupied = TRUE;
}

returncl()
{
   /***********************************************************************
    *   Copies current line from current line buffer to the output queue. *
    **********************************************************************/

   int *getspace();
   char *str;
   int wordsize = strlen (clbuff);
   if (debug)
     writef ("returncl()\n");
   if (wordsize > mxll)
     execerror ('t');
   if (strlen (currentl->stringb) < wordsize)
   {
     struct cur_rec *newlptr = 
        getspace (minlinesize + wordsize / BYTESPERWORD + 1);
     struct cur_rec *f = currentl->fptr;
     struct cur_rec *b = currentl->bptr;
     newlptr->stringb = newlptr+1;
     newlptr->fptr = f;
     newlptr->bptr = b;
     if (f == null)
       highestl[currfrom] = newlptr;
     else f->bptr = newlptr;
     if (b == null)
        lowestl[currto] = newlptr;
     else b->fptr = newlptr;
     newlptr->lineno = currentl->lineno;
     newlptr->symb = currentl->symb;
     newlptr->controlch = currentl->controlch;
     freespace (currentl);
     currentl = newlptr;
   }
   str = currentl->stringb;
   strcpy (str, clbuff);
   currentl->symb = currentl->symb | plusmask;
   clboccupied = FALSE;
}

shuffle (chno, displacement)
int chno, displacement;
{
   /*******************************************************************
    *   Shuffles all characters after chno in the current line buffer *
    *  outwards by displacement places (-ve displacement allowed).    *
    *  Alters the length accordingly.                                 *
    ******************************************************************/

   int temp = strlen (clbuff) + displacement;
   int i;
/*   if (debug) writef ("shuffle (%d, %d)\n", chno, displacement); */
   if (temp >= mxll) execerror ('e');
   if (displacement > 0)
     for (i = strlen (clbuff)-1; i >= chno; i--)
        clbuff[displacement+i] = clbuff[i];
   else for (i = chno; i <= strlen (clbuff)-1; i++)
      clbuff[displacement+i] = clbuff[i];
   clbuff[temp] = '\0';
}

/*********************************
 *   Matching a qualified string *
 ********************************/

int findmatch (qst, st, ptrwanted, lwindow)
char *st;
int qst[];
int ptrwanted, lwindow;
{
   /************************************************************************
    *   Tries to find the qualified string qst in string st. If successful *
    *  it returns the number of the character in st just previous to the   *
    *  match. Otherwise it calls error if ptrwanted is TRUE or returns     *
    *  -1.                                                                 *
    ***********************************************************************/

  int i, j;
  int n = qst[QINT];
  int fqslen;
  int rwindow;
  char s1[MAXSTRLENGTH];
  char s2[MAXSTRLENGTH];
  fqst = qst;
  fst = st;
  fqs = qst[QSTR];
  fqslen = strlen (fqs);
  if (debug)
    writef ("findmatch(): searching for /%s/ in\n/%s/\n", fqs, st);
  ffch = lwindow;

/* Next line is actually redundant but in there so we don't assume
   a value for OTHERSYM                                             */

  if (fqst[QWI] && fqst[QWL] == OTHERSYM)
    ffch = lwindow;
  else
    if (fqst[QWI] && fqst[QWL] > lwindow)
      ffch = fqst[QWL] - 1;
  flch = strlen (st);
  rwindow = fqst[QWR] == OTHERSYM ? flch : fqst[QWR];
  if (fqst[QU])
  {

/* Left as is so that the null character is copied */

    for (i = 0; i <= fqslen; i++) s1[i] = capitalch (fqs[i]);
    fqs = s1;
    for (j = 0; j <= flch; j++) s2[j] = capitalch (fst[j]);
    fst = s2;
  }
  if (fqst[QS])
  {
    do
    {
      if (ffch >= flch)
      {
        if (ptrwanted)
           execerror ('m');
        else return -1;
      }
      ffch = ffch + 1;
    } while (fst[ffch-1] == ' ');
    ffch = ffch - 1;
    while (fst[flch-1] == ' ') flch = flch - 1;
  }
  if (n < 1)
  {
    /*************************************************
     *   One of the qualifiers [], B, E or P is set. *
     ************************************************/

    int temp = flch - fqslen;
    if (fqst[QWI]) temp = rwindow - fqslen;

/* [n,m]p// is set to match eol if n > strlen (st) */

    if (temp >= ffch)
    {
      if (fqst[QE])
      {
        if (match (temp)) return temp;
      }
      else if (fqst[QP])
      {
        if (fqslen == 0 && ffch >= flch)
          return flch;
        else
         if (match (ffch) && temp == ffch)
           return ffch;
      }
      else
        if (match (ffch))
          return ffch;                    /* B qualifier */
      else
        if (fqst[QWI])                                    /* Window set  */
      {
        if (debug)
          writef ("Searching %s from %d to %d for %s\n",
                  fst, ffch, rwindow - fqslen, fqs);
        if (ffch <= flch)
          for (i = ffch; i <= rwindow - fqslen; i++)
            if (match (i))
              return i;
      }
    }
  }
  else
    if (fqst[QL])
  {
    int temp = flch - fqslen;
    if (fqst[QWI])
      temp = rwindow - fqslen;
    if (temp >= ffch)
      for (i = temp; i >= ffch; i--)
        if (match (i))
          if (--n == 0)
            return i;
  }
  else
    for (i = ffch; i <= flch - fqslen; i++)
      if (match (i))
        if (--n == 0)
          return i;
  if (ptrwanted)
    execerror ('m');
  if (debug)
    writef ("findmatch() returning -1\n");
  return -1;
}

int match (i)
int i;
{
   /***********************************************************************
    *   Tests if string fqs matches string fst starting from the i + 1 th *
    *  character in fs. Also checks fqs is a word if that qualifier has   *
    *  been specified.  Return FALSE if we go past end of st.             *
    **********************************************************************/

   int fqslen = strlen (fqs);
   int j;
   if (debug)
     writef ("match (%d): %d\n", i, fqslen);

/* Need this next line in case some asshole tries to e[n]//thing/ when
   n > length of line (like I did).  Since we're not going to bother
   to put spaces on the end, we'll reject it.                          */

   if (i > strlen (fst))
     return FALSE;
   for (j = 0; j <= fqslen - 1; j++)
     if (fqs[j] != fst[i + j] || i + j > strlen (fst))
       return FALSE;
   if (fqst[QW])
     return isword (i);
   return TRUE;
}


/**********************************************************
 * Check for a word uses the set of characters in wordset *
 *********************************************************/

int isword (i)
int i;
{
  int temp = i + strlen (fqs) + 1;
  if (temp <= flch && isinword (fst[temp-1]))
       return FALSE;
  if (i > ffch && isinword (fst[i-1]))
       return FALSE;
  return TRUE;
}

/***********************************
 * WORD// disables the W qualifier *
 * so this returns TRUE if so.     *
 **********************************/

int isinword (ch)
char ch;
{
  if (strlen (wordset) == 0) return TRUE;
  else return (strchr (wordset, ch) != 0);
}

procword (st)
char *st;
{
  int slen = strlen (st);
  int wlen = 0;
  int i, quoting, minus;
  char minusch;
  quoting = FALSE;
  minus = FALSE;
  for (i = 0; i < slen; i++)
  {
    char ch = st[i];
    if (quoting)
    {
      wordset[wlen++] = ch;
      quoting = FALSE;
    }
    else if (minus)
    {
      char j;
      if (!( (isupper (minusch) && isupper (ch)) ||
             (islower (minusch) && islower (ch)) ||
             (isdigit (minusch) && isdigit (ch))) ||
             (minusch > ch)) execerror ('w');
      else for (j = minusch + 1; j <= ch; j++) wordset[wlen++] = j;
      minus = FALSE;
    }
    else if (ch == '-') minus = TRUE;
    else if (ch == '"') quoting = TRUE;
    else
    {
      minusch = ch;
      wordset[wlen++] = ch;
    }
  }
 wordset[wlen] = '\0';
}

/***********************
 *   String operations *
 **********************/

procdfa (qst)
int qst[];
{
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   clbuff[n + strlen (qst[QSTR])] = '\0';
}

procdfb (qst)
int qst[];
{
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   clbuff[n] = '\0';
}

procdta (qst)
int qst[];
{
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   n = n + strlen (qst[QSTR]);
   shuffle (n, cur_col - n);
}

procdtb (qst)
int qst[];
{
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   shuffle (n, cur_col - n);
}

proca (qst, st, point)
int qst[];
char *st;
int point;
{
   int i;
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   n = n + strlen (qst[QSTR]);
   shuffle (n, strlen (st));
   for (i = 0; i <= strlen (st)-1; i ++) clbuff[n + i] = st[i];
   if (point) cur_col = n + strlen (st);
}

procb (qst, st, point)
int qst[];
char *st;
int point;
{
   int i;
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   shuffle (n, strlen (st));
   for (i = 0; i <= strlen (st) - 1; i++) clbuff[n + i] = st[i];
   if (point) cur_col = n + strlen (st);
}

proce (qst, st, point)
int qst[];
char *st;
int point;
{
   int i;
   int n = findmatch (qst, clbuff, TRUE, cur_col);
   int displacement = strlen (st) - strlen (qst[QSTR]);

   if (displacement != 0)    /* If new string is a different length from old */
     shuffle (n + strlen (qst[QSTR]), displacement);
   for (i = 0; i <= strlen (st) - 1; i++) clbuff[n + i] = st[i];
   if (point) cur_col = n + strlen (st);
}

procpa (qst)
int qst[];
{
   cur_col = findmatch (qst, clbuff, TRUE, cur_col) + strlen (qst[QSTR]);
}

procpb (qst)
int qst[];
{
   cur_col = findmatch (qst, clbuff, TRUE, cur_col);
}

doglobals (ptr)
struct glob_rec *ptr;
{
   if (! goutdisabled && ((currentl->symb & endmask) == 0))
   {
     while (ptr != null)
     {
       if (ptr->genabled)
       {
         struct com_rec *com = ptr->gcom;

         if (debug)
           writef ("Global %d enabled\n", ptr->gnumber);

         if (com->name == CON)
         {
            if (debug)
              writef ("Executing an ON command\n");
            if (findse (com->arg1))
            {
              if (debug)
                writef ("Obeying command sequence at %d\n", com->arg2);
              obeycomseq (com->arg2);
              if (debug)
                writef ("Obeyed command sequence\n");
              ptr->gcount = ptr->gcount + 1;
            }
         }
         else
         {
           int *qst = com->arg1;
           int qstlen = strlen (qst[QSTR]);
           char *st = com->arg2;
           int stlen = strlen (st);
           int displacement;
           int n;
           if (clboccupied)
              n = findmatch (qst, clbuff, FALSE, cur_col);
           else
             n = findmatch (qst, currentl->stringb, FALSE, cur_col);
           if (n != -1)
           {
             int globcount = 0;
             if (! clboccupied)
               getcl();
             verreq1 = TRUE;
             do
             {
               int i;
               switch (com->name)
               {
                 case CGA : n = n + qstlen;
                            shuffle (n, stlen);
                            for (i = 0; i <= stlen-1; i++)
                               clbuff[n + i] = st[i];
                            break;

                 case CGB : shuffle (n, stlen);
                            for (i = 0; i <= stlen-1; i++)
                               clbuff[n + i] = st[i];
                            n = n + qstlen;
                            break;

                 case CGE : displacement = stlen - qstlen;
                            if (displacement != 0)
                               shuffle (n + qstlen, displacement);
                            for (i = 0; i <= stlen-1; i++)
                               clbuff[n + i] = st[i];
                            break;

                 default :  writef ("**ERROR bad global\n");
                            break;
               }
               n += stlen;
               if (qstlen == 0)
                 n++;
               ptr->gcount++;
               if (globcount++ > MAXGLOBEXEC)
                 execerror ('q', ptr->gnumber);
               n = findmatch (qst, clbuff, FALSE, n);
             } while ((n != -1) && (qst[QINT] >= 1));
           }
         }
       }
       ptr = ptr->gnext;
     }
     if (clboccupied)
       returncl();
     if (! swvg)
       verreq1 = FALSE;
   }
}

/*********************
 *    Line splitting *
 ********************/

procsa (qst)
int qst[];
{
   if ((currentl->symb & endmask) != 0) execerror ('s');
   split (findmatch (qst, currentl->stringb, TRUE, cur_col) +
          strlen (qst[QSTR]));
}

procsb (qst)
int qst[];
{
   if ((currentl->symb & endmask) != 0)
     execerror ('s');
   split (findmatch (qst, currentl->stringb, TRUE, cur_col));
}

split (n)
int n;
{
   /*********************************************************************
    *   Breaks the current line into two lines, splitting after the nth *
    *  character.                                                       *
    ********************************************************************/

   int *getspace();
   char *cst = currentl->stringb;
   struct cur_rec *f = currentl->fptr;
   int size = strlen (cst) - n;
   int i;
   char *tst;
   struct cur_rec *temp = getspace (minlinesize + 1 + size / BYTESPERWORD);

/* Note the '+1' in the following line.  Guess why?              */

   temp->stringb = temp + 1;
   tst = temp->stringb;
   temp->lineno = currentl->lineno;
   temp->controlch = currentl->controlch;
   temp->symb = movmask;
   for (i = 0; i <= size-1; i++) tst[i] = cst[n + i];
   cst[n] = '\0';
   tst[size] = '\0';
   verify();
   verreq1 = TRUE;
   temp->bptr = currentl;
   currentl->fptr = temp;
   temp->fptr = f;
   if (f == null)
      highestl[currfrom] = temp;
   else f->bptr = temp;
   currentl = temp;
}

/************************
 *   Search expressions *
 ***********************/

procf (se)
struct se_rec *se;
{
   if (swfn)
     procn();
   while (! findse (se))
     procn();
}

procbf (se)
struct se_rec *se;
{
   if (swfn)
     procp();
   while (! findse (se))
     procp();
}

procdf (se)
struct se_rec *se;
{
   struct cur_rec *hptr = currentl->bptr;
   struct cur_rec *f;
   goutdisabled = TRUE;
   if (! findse (se) || swfn)
     do    
     {
       if ((currentl->symb & endmask) != 0)
         break;
       f = currentl->fptr;
       freespace (currentl);
       if (f == null)
       {
         currentl = hptr;
         highestl[currfrom] = hptr;
         inputln();
       }
       else
         currentl = f;
     } while (! findse (se));
   currentl->bptr = hptr;
   if (hptr == null)
     lowestl[currto] = currentl;
   else
     hptr->fptr = currentl;
   currentl->symb = currentl->symb | minusmask;
   goutdisabled = FALSE;
   doglobals (glist);
   if ((currentl->symb & endmask) != 0)
     execerror ('s');
}

int findnzse (se, st)
struct se_rec *se;
char *st;
{
  do
  {
    int temp;
    if (se->SESEQ == null)
    {
      if ((se->SEQS)[QC])
      {
        char ch = ' ';
        char *str = (se->SEQS)[QSTR];
        if (strlen (str) != 0)
          ch = *str;
        ch = pfcc_to_internal (ch);
        temp = (ch == currentl->controlch);
      }
      else
      {
        temp = findmatch (se->SEQS, st, FALSE, cur_col);
        if ((se->SEQS)[QN])
          if (temp == -1)
            temp = TRUE;
          else
            temp = FALSE;
        else
          if (temp == -1)
            temp = FALSE;
          else 
            temp = TRUE;
      }
    }
    else
      temp = findnzse (se->SESEQ, st);
    if (se->SENEXT == null)
    {
      if (debug)
        writef ("findnzse() returning %s\n", temp ? "TRUE" : "FALSE");
      return temp;
    }
    if (se->SEAND)
    {
      if (! temp)
        return FALSE;
    }
    else
      if (temp)
        return TRUE;
    se = se->SENEXT;
  } while (TRUE);
}

findse (se)
struct se_rec *se;
{
   /*****************************************************************
    *   Tests if the search expression se matches the current line. *
    *  Returns FALSE if the current line is the eof one.            *
    ****************************************************************/

   char *st;
   if (debug)
     writef ("findse()\n");
   if (clboccupied)
      st = clbuff;
   else st = currentl->stringb;
   if ((currentl->symb & endmask) != 0)
     return FALSE;
   return findnzse (se, st);
}

proclcl()
{
  int length, i;
  length = strlen (clbuff);
  for (i = 0; i < length; i++)
    clbuff[i] = tolower (clbuff[i]);
  verify();
}

procucl()
{
  int length, i;
  length = strlen (clbuff);
  for (i = 0; i < length; i++)
    clbuff[i] = toupper (clbuff[i]);
  verify();
}

procshc (qst)
int qst[];
{
  int n = findmatch (qst, clbuff, TRUE, cur_col);
  writef ("%d\n", n + 1);
}

single_char (com)
int com; 
{
  if (cur_col == strlen (clbuff) && com != CBRA)
    execerror ('z');
  switch (com)
  {
    case CDOL : clbuff[cur_col] = tolower (clbuff[cur_col]);
                break;

    case CBRA : if (cur_col == 0)
                  execerror ('z');
                if (debug)
                  writef ("CBRA\n");
                cur_col = cur_col - 1;
                break;

    case CKET : if (debug)
                  writef ("CKET %d\n", cur_col);
                break;

    case CPER : clbuff[cur_col] = toupper (clbuff[cur_col]);
                break;

    case CDEL : shuffle (cur_col + 1, -1);
                if (debug)
                  writef ("CDEL %d\n", cur_col);
                break;

    case CSPC : clbuff[cur_col] = ' ';
                break;

      default : writef ("ERROR - unknown character command %n\n", com);
                break;
  }
  if (com != CBRA && com != CDEL)
    cur_col = cur_col + 1;
}

proclc (qst)
int qst[];
{
  int n = findmatch (qst, clbuff, TRUE, cur_col);
  int i;
  for (i = 0; i < strlen (qst[QSTR]); i++)
    clbuff[n+i] = tolower (clbuff[n+i]);
}

procuc (qst)
int qst[];
{
  int n = findmatch (qst, clbuff, TRUE, cur_col);
  int i;
  for (i = 0; i < strlen (qst[QSTR]); i++)
    clbuff[n+i] = toupper (clbuff[n+i]);
}
