/* section "copy" */

/********************************************************************
 *                                                                  *
 *   This section holds procedures to copy command sequences        *
 *  and to execute global and procedure commands                    *
 *                                                                  *
 *******************************************************************/

#include "glob"

int proccpy (add, pcommand)
int pcommand;
struct com_rec *(*add);
 {
   /***********************************************************
    *   This procedure executes proc (if pcommand is TRUE) or *
    *  creates new global commands (if FALSE).                *
    *********************************************************/

   int i;
   int *getspace();
   struct com_rec *cpyscom();
   char *cpyst();
   struct proc_rec *(*findproc());
   struct com_rec *ptr = *add;
   int size = (ptr->nextcom - ptr) * gsize;
   *add = ptr->nextcom;

   if (pcommand && findproc (ptr->arg1, FALSE) != null)
      execerror ('x', ptr->arg1);

   if (debug)
   {
     writef ("ptr->nextcom = %d, ptr = %d\n", ptr->nextcom, ptr);
     writef("Size of space allocated for copying is %d\n", size);
   }                                                                   

   cpysp = getspace (size + gsize);
   ptrcpysp = cpysp + gsize;
   limcpysp = ptrcpysp + size;

   if (pcommand)
   {
     cpysp->pnext = proclist;
     proclist = cpysp;
     proclist->pname = cpyst (ptr->arg1);
     cpycg (ptr->arg2, &proclist->pbody);
   }
   else
   {
     cpysp->gcount = 0;
     cpysp->genabled = TRUE;
     cpysp->gcom = cpyscom (ptr);
     (cpysp->gcom)->nextcom = null;
     cpysp->gnext = null;
     if (glist == null)
     {
       cpysp->gnumber = 1;
       glist = cpysp;
       endglist = cpysp;
     }
     else
     {
       cpysp->gnumber = endglist->gnumber + 1;
       if (debug) writef ("Creating global #%d\n", endglist->gnumber+1);
       endglist->gnext = cpysp;
       endglist = cpysp;
     }
     if (swv)
       writef ("Global %d\n", endglist->gnumber);
     if (lineinput)
       doglobals (endglist);
   }
   if (debug)
   {
     writef("Copying to %d\n", cpysp);
     for (i = 0; i <= 12; i++) writef ("word %d is %d\n", i, cpysp[i]);
   }                                                                      
}

newcse (se)
struct se_rec *se;
 {
   struct se_rec *cpyse();
   if (se == csebuff)
   {
     if (! cseopen) execerror ('r');
     return;
   }
   cseopen = TRUE;
   cpysp = csebuff;
   limcpysp = csebuff + csebsize;
   ptrcpysp = csebuff;
   cpyse (se);
 }

newlsc (com)
int *com;
 {
   struct com_rec *cpyscom();
   cpysp = lscbuff;
   limcpysp = lscbuff + lscbsize;
   ptrcpysp = lscbuff;
   cpyscom (com);
   cpysp->nextcom = null;
 }

/***********************************************
 *   These procedures copy a command sequence. *
 **********************************************/

cpycg (ptr, add)
struct com_rec *ptr;
struct com_rec *(*add);
 {
   struct com_rec *cpyscom();
   while (ptr != null)
   {
     *add = cpyscom (ptr);
     ptr = ptr->nextcom;
     add = &((*add)->nextcom);
   }
   *add = null;
 }

struct com_rec *cpyscom (ptr)
struct com_rec *ptr;
{
  int *cpyqst(), *getcpysp();
  char *cpyst();
  struct se_rec *cpyse();
  struct com_rec *v = getcpysp (4);
  if (debug)
    writef ("cpyscom(): v = %d\n", v);
  v->name = ptr->name;
  switch (ptr->name)
  {
    case CDO :
    case CCPROC :
    case IMFILE :
    case CPROC :
    case CSHPROC : v->arg1 = cpyst (ptr->arg1);
                   break;

    case CA :
    case CAP :
    case CB :
    case CBP :
    case CDFA :
    case CDFB :
    case CDTA :
    case CDTB :
    case CE :
    case CEP :
    case CGA :
    case CGB :
    case CGE :
    case CLC :
    case CPA :
    case CPB :
    case CSA :
    case CSB :
    case CSHC :
    case CUC  : v->arg1 = cpyqst (ptr->arg1);
                break;

    case CBF :
    case CDF :
    case CELIF :
    case CELUL :
    case CF :
    case CIF :
    case CON :
    case CUL :
    case CUT :
    case CWH :   v->arg1 = cpyse (ptr->arg1);
                 break;

    case IMINB : cpytext (ptr->arg1, &(v->arg1));
                 break;

    case CFROM :
    case CTO   : if (ptr->arg2 == IMFILE) v->arg1 = cpyst (ptr->arg1);
                   else v->arg1 = ptr->arg1;
                 break;

    default : v->arg1 = ptr->arg1;
              break;
  }
  switch (ptr->name)
  {
    case CA :
    case CAP :
    case CB :
    case CBP :
    case CCC :
    case CCL :
    case CCOMM :
    case CE :
    case CEP :
    case CGA :
    case CGB :
    case CGE :
    case CIS :
    case CWORD :
    case CZ :    v->arg2 = cpyst (ptr->arg2);
                 break;

    case CELIF :
    case CELSE :
    case CELUL :
    case CINT :
    case CIF :
    case CIFEOF :
    case CON :
    case CPROC :
    case CRPT :
    case CUL :
    case CULEOF :
    case CUT :
    case CUTEOF :
    case CWH :    cpycg (ptr->arg2, &(v->arg2));
                  break;

    default : v->arg2 = ptr->arg2;
              break;
  }
  return v;
}

char *cpyst (st)
char *st;
{
   int *getcpysp();
   int i;
   int wordlength = strlen (st);
   char *ptr = getcpysp (wordlength / BYTESPERWORD + 1);
   if (debug)
     writef ("cpyst(): wordlength = %d\n", wordlength);
   strcpy (ptr, st);
   return ptr;
}

int *cpyqst (qst)
int qst[];
{
   int *getcpysp();
   char *cpyst();
   int i;
   int *ptr = getcpysp (1 + QSTR);
   for (i = 0; i <= QSTR; i++)
     ptr[i] = qst[i];
   if (debug)
     writef ("cpyqst(): qst = %s\n", qst[QSTR]);
   ptr[QSTR] = cpyst (qst[QSTR]);
   return ptr;
}

struct se_rec *cpyse (se)
struct se_rec *se;
{
   int *getcpysp();
   struct se_rec *ptr;
   if (se == csebuff) return csebuff;
   ptr = getcpysp (SESIZE);
   if (se->SESEQ == null)
   {
     if (debug)
       writef ("cpyse()\n");
     ptr->SESEQ = null;
     ptr->SEQS = cpyqst (se->SEQS);
   }
   else
   {
     ptr->SEQS = null;
     ptr->SESEQ = cpyse (se->SESEQ);
   }
   if (se->SENEXT == null)
     ptr->SENEXT = null;
   else
   {
     ptr->SEAND = se->SEAND;
     ptr->SENEXT = cpyse (se->SENEXT);
   }
   return ptr;
}

cpytext (text, add)
struct gen_rec *text;
char *(*add);
{
   int *getcpysp();
   while (text != null)
   {
     int i;
     int wordlength = strlen (&text->stringb);
     char *ptr = getcpysp (wordlength / BYTESPERWORD + 2);
     strcpy (ptr + 4, &text->stringb);
     *add = ptr;
     add = ptr;
     text = text->nextg;
   }
   *add = null;
}

int *getcpysp (size)
int size;
{
   int *temp = ptrcpysp;

/*   if (debug) writef ("getcpysp called to allocate %d words\n", size); */

   ptrcpysp = ptrcpysp + size;
   if (ptrcpysp > limcpysp)
   {
     if (swwarn) writef ("WARNING - search expression too long for re-use\n");
     cseopen = FALSE;
     ptrcpysp = cpysp;
     return cpysp;
   }
   return temp;
}


/*************************************************
 *   These procedures execute procedure commands *
 ************************************************/

procdo (namest)
char *namest;
{
   struct proc_rec *(*findproc());
   int *add = findproc (namest, TRUE);

/*   writef("do about to execute"); wrcomseq([add[pbody);wrch('\n'); */

   obeycomseq ((*add)->pbody);
}

/*************************************
 * This procedure was formerly bound *
 * into the procedure following it   *
 ************************************/

int shproc (ptr)
struct proc_rec *ptr;
{
  writef ("%s\n    ", ptr->pname);
  wrcomseq (ptr->pbody);
  wrch ('\n');
}

procshproc (namest)
char *namest;
{
   struct proc_rec *(*findproc());
   if (strlen (namest) == 0)
   {
     struct proc_rec *ptr = proclist;
     while (ptr != null)
     {
       shproc (ptr);
       ptr = ptr->pnext;
     }
   }
   else
   {
     struct proc_rec *(*add) = findproc (namest, TRUE);
     shproc(*add);
   }
}

proccproc (namest)
char *namest;
{
   struct proc_rec *(*findproc());
   if (strlen (namest) == 0)
   {
     struct proc_rec *ptr = proclist;
     while (proclist != null)
     {
       ptr = proclist->pnext;
       freespace (proclist);
       proclist = ptr;
     }
   }
   else
   {
     struct proc_rec *(*add) = findproc (namest, TRUE);
     struct proc_rec *ptr = (*add)->pnext;                   /* DCJM         */
     freespace (*add);
     *add = ptr;
   }
}

struct proc_rec *(*findproc (namest, callerror))
char *namest;
int callerror;
{
   struct proc_rec *(*add) = &proclist;
   int l = strlen (namest);
   char *st;
   int cond;
   int i;

   /*   writef("namest is %s\n", namest);                   */

   while (! *add == null)
   {
     st = (*add)->pname;                            /* DCJM */

   /*     writef("comparing against %s\n", st);             */

     cond = TRUE;

/* We can leave this as is because comparing the first bytes
   of a BCPL string for equality of string length is taken care
   of in C by comparing the last bytes (both nulls) */

     for (i = 0; i <= l && cond; i++)
       if (st[i] != namest[i]) cond = FALSE;
     if (cond) return (add);
     add = &(*add)->pnext;
   }
   if (callerror) execerror ('p', namest);
   return (null);
}

/**********************************************
 *   These procedures execute global commands *
 *********************************************/

procdeg (no, enable)
int no, enable;
{
   struct glob_rec *(*findglobal());
   if (no == OTHERSYM)
   {
     struct glob_rec *ptr = glist;
     while (ptr != null)
     {
       ptr->genabled = enable;
       ptr = ptr->gnext;
     }
   }
   else if (no == PLUSSYM)
   {
     if (! endglist == null)
              endglist->genabled = enable;
   }
   else
   {
     struct glob_rec *(*add) = findglobal (no);
     (*add)->genabled = enable;                      /* DCJM      */
   }
}

/*************************************
 * This procedure was formerly bound *
 * into the procedure following it   *
 ************************************/

int shg (ptr)
struct glob_rec *ptr;
{
  char ch;
  if (ptr->genabled)
     ch = 'E';
     else ch = 'D';
  writef ("%2d %c %3d ", ptr->gnumber, ch, ptr->gcount);
  wrcomseq (ptr->gcom);
  wrch ('\n');
}

procshg (no)
int no;
{
   struct glob_rec *(*findglobal());
   if (no == OTHERSYM)
   {
     struct glob_rec *ptr = glist;
     while (ptr != null)
     {
       shg (ptr);
       ptr = ptr->gnext;
     }
   }
   else if (no == PLUSSYM)
   {
     if (! endglist == null) shg (endglist);
   }
   else
   {
     struct glob_rec *(*add) = findglobal (no);
     shg (*add);
   }
}

proccg (no)
int no;
{
   struct glob_rec *(*findglobal());
   if (no == OTHERSYM)
   {
     struct glob_rec *ptr = glist;
     while (glist != null)
      {
       ptr = glist->gnext;
       freespace (glist);
       glist = ptr;
     }
     endglist = null;
   }
   else if (no == PLUSSYM)
   {
     if (!endglist == null)
     {
       struct glob_rec *ptr = glist;
       if (glist == endglist)
       {
         freespace (glist);
         glist = null;
         endglist = null;
         return;
       }
       while (ptr->gnext != endglist) ptr = ptr->gnext;
       freespace (endglist);
       endglist = ptr;
       endglist->gnext = null;
     }
   }
   else
   {
     struct glob_rec *(*add) = findglobal (no);
     struct glob_rec *nptr = (*add)->gnext;                    /* DCJM  */
     freespace (*add);
     *add = nptr;
     if (nptr == null)
     {
       if (glist == null)
         endglist = null;

/* Used to be add - pnext - which was wrong anyway - now we use 4   */

       else endglist = add - 4;
    }
   }
}

struct glob_rec *(*findglobal (no))
int no;
{
   struct glob_rec *(*add) = &glist;
   while (*add != null)
   {
     if ((*add)->gnumber == no) return (add);      /* DCJM     */
     add = &((*add)->gnext);
   }
   execerror ('g', no);
}
