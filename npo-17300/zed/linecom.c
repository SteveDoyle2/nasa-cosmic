/* section "linecom" */

#include "glob"

/*****************************************************************************
 *                                                                           *
 *   Contains procedures to carry out commands dealing with whole lines i.e. *
 *  to insert or delete whole lines or to move around the source.            *
 *  Also some miscellaneous procedures to execute commands.                  *
 *                                                                           *
 ****************************************************************************/

/*********************
 *   Move procedures *
 ********************/

next()
{
   /************************************************************************
    *   Makes the next line the current one, assumes that there already is *
    *  a next line                                                         *
    ***********************************************************************/

   if (interrupt)
   {
     do_interrupt();
     execerror ('y');
   }
   if (halt_lineno != ENDSYM)
   {
     if (currentl->lineno == halt_lineno)
     {
       writef ("*** HALT\n");
       longjmp (jmpenv, 8);
     }
     if (currentl->lineno > halt_lineno) execerror ('h');
   }
   if (currto != 0) currentl->symb = currentl->symb | movmask;
   currentl = currentl->fptr;
   cur_col = 0;
   doglobals (glist);
}

previous()
{
   /******************************************************************
    *   Makes previous line the current one, assumes that there is a *
    *  previous one.                                                 *
    *****************************************************************/

   if (interrupt)
   {
     do_interrupt();
     execerror ('y');
   }
   if (currfrom != -1) currentl->symb = currentl->symb | movmask;
   currentl = currentl->bptr;
}

procn()
{
   if (currentl->fptr == null)
     if ((currentl->symb & endmask) == 0)
       inputln();
     else execerror ('s');
   else
   {
     if (debug) writef ("Next\n");
     next();
   }
}

procp()
{
   if (currentl->bptr == null)
     execerror ('n');
   else previous();
}

/****************************************************
 * The verify()s in the following procedure are     *
 * in case we have a global active that makes a     *
 * change while we're moving forwards.  We can't    *
 * put it in next() because that screws up proccl() *
 ****************************************************/

procm (arg)
int arg;
{
   int saverreq2 = verreq2;
   switch (arg)
   {
     case PLUSSYM : while (currentl->fptr != null)
                    {
                      next();
                      if (verreq1) verify();
                    }
                    break;

     case MINUSSYM : while (currentl->bptr != null) previous();
                     break;

     case ENDSYM : while (currentl->fptr != null)
                   {
                     next();
                     if (verreq1) verify();
                   }
                   while ((currentl->symb & endmask) == 0) inputln();
                   break;

     case CURRSYM :
     case OTHERSYM : break;

     default : if (arg < currentl->lineno)
                do procp(); while
         ((arg < currentl->lineno) || ((currentl->symb & newmovmask) != 0));
  else while (arg > currentl->lineno || (currentl->symb & newmovmask) != 0)
          {
            procn();
            if (verreq1) verify();
          }
       if (arg != currentl->lineno) execerror ('l', arg);
               break;
   }
  verreq2 = saverreq2;    /* To handle verification after a global correctly */
}

/************************
 *   Display procedures *
 ***********************/

prclno()
{
   /***************************************************************
    *   Sends a copy of the current line no to the current output *
    **************************************************************/

   if ((currentl->symb & ormask) == 0)
     writef ("%d.", currentl->lineno);
   else
   {
     if ((currentl->symb & newmask) != 0)
       if ((currentl->symb & endmask) != 0)
         writef (" ?*");
       else writef ("**.", currentl->lineno);
     else if ((currentl->symb & endmask) != 0)
         writef ("%d*", currentl->lineno);
       else writef ("(%d)", currentl->lineno);
   }
   if (currentl->controlch != '\n')
   {
     if (swx)
     writehex ("%c", currentl->controlch);
     else switch (currentl->controlch)
     {
       case '\f' : wrch ('1');
                   break;

       case '\r' : wrch ('+');
                   break;

       default : wrch ('?');
                 break;
     }
   }
   wrch ('\n');
}

prcitext()
{
   /********************************************************************
    *   Verifies a copy of the current line text to the current output *
    *******************************************************************/

   if (swvx) prcxtext();
   else if (clboccupied) verify_string (clbuff);
        else verify_string (currentl->stringb);
}

prcltext()
{
   /*****************************************************************
    *   Sends a copy of the current line text to the current output *
    ****************************************************************/

   if (swx)
   {
     if (clboccupied) writehex ("%s", clbuff);
     else writehex ("%s", currentl->stringb);
     wrch ('\n');
   }
   else
     if (clboccupied)
       writef ("%s \n", clbuff);
     else                          /* space to correct obscure C RTL bug */
       writef ("%s \n", currentl->stringb); 
}

prcxtext()
{
   /*********************************************************
    *   Verifies a copy of the current line in hexadecimal  *
    ********************************************************/

  if (clboccupied) verify_hex (clbuff);
  else verify_hex (currentl->stringb); 
}

prcl()
{
   /*****************************************************************
    *   Sends a copy of the current line to the current output      *
    *  including the line number and any other relevant information *
    ****************************************************************/

   if ((currentl->symb & ormask) == 0)
     writef ("%6d", currentl->lineno);
   else
   {
     if ((currentl->symb & newmask) != 0)
       if ((currentl->symb & endmask) != 0)
         writef ("?*");
       else writef ("  ****", currentl->lineno);
     else if ((currentl->symb & endmask) != 0)
         writef ("%d*", currentl->lineno);
       else writef ("(%4d)", currentl->lineno);
   }
   if ((currentl->symb & minusmask) == 0)
     wrch (' ');
   else wrch ('-');
   if ((currentl->symb & plusmask) == 0)
     wrch (' ');
   else wrch ('+');
   writef ("%s", currentl->stringb);
   wrch ('\n');
}

proct (arg)
int arg;
{
   int i;
   verreq1 = FALSE;
   verreq2 = FALSE;
   if (arg == PLUSSYM)
   do
   {
     prcltext();
     if (currentl->fptr == null) return;
     next();
   } while (TRUE);
   if (arg == OTHERSYM) arg = MAXINT;
   if ((currentl->symb & endmask) != 0)
   {
     verreq2 = TRUE;
     return;
   }
   prcltext();
   for (i = 2; i <= arg; i++)
   {
     procn();
     if ((currentl->symb & endmask) != 0)
     {
       verreq2 = TRUE;
       return;
     }
     prcltext();
   }
}

proctl (arg)
int arg;
{
   int i;
   verreq1 = FALSE;
   verreq2 = FALSE;
   if (arg == PLUSSYM)
   do
   {
     prcl();
     if (currentl->fptr == null) return;
     next();
   } while (TRUE);
   if (arg == OTHERSYM) arg = MAXINT;
   prcl();
   for (i = 2; i <= arg; i ++)
   {
     if ((currentl->symb & endmask) != 0) break;
     procn();
     prcl();
   }
}

verify()
{
   /***********************************************************************
    *   Verifies the current line according to the switches VN, VT and VI *
    **********************************************************************/

   if (swv)
   {
     if (swvn) prclno();
     if (swvi) prcitext();
     else if (swvt) prcltext();
   }
   verreq1 = FALSE;
   verreq2 = FALSE;
}

/*************************
 *   Deletion procedures *
 ************************/

procdrest()
{
   struct cur_rec *f = currentl->fptr;
   struct cur_rec *nf;
   while (f != null)
   {
     nf = f->fptr;
     freespace (f);
     f = nf;
   }
   (currentl->stringb)[0] = '\0';
   currentl->fptr = null;
   currentl->symb = endmask + newmask;
}

procd (l1, l2)
int l1, l2;
 {
   struct cur_rec *ptr1, *ptr2, *hptr;
   if (l2 == CURRSYM) ptr2 = currentl;
   if (l1 == OTHERSYM)
   {
     ptr1 = currentl;
     ptr2 = currentl;
   }
   else
   {
     procm (l1);
     ptr1 = currentl;

     /******************************************************
      *   Line l1 has been found and made the current line *
      *****************************************************/

     goutdisabled = TRUE;

     switch (l2)
     {
       case OTHERSYM : break;

       case CURRSYM : while (currentl != ptr2)
                      {
                        if (currentl->fptr == null) execerror ('a');
                        next();
                      }
                      break;

       case ENDSYM : while (currentl->fptr != null) next();
                     while ((currentl->symb & endmask) == 0) inputln();
                     break;

       default :
         while (l2 > currentl->lineno || (currentl->symb & newmovmask) != 0)
            procn();
         if (l2 != currentl->lineno) execerror ('l', l2);
         break;
     }
     goutdisabled = FALSE;
     ptr2 = currentl;

     /******************************************************
      *   Line l2 has been found and made the current line *
      *****************************************************/

   }
   hptr = ptr1->bptr;
   while (ptr1 != ptr2)
   {
     ptr1 = ptr1->fptr;
     freespace (ptr1->bptr);
   }

   /****************************************************
    *   Lines l1 to one less than l2 have been deleted *
    ***************************************************/

   if ((ptr2->symb & endmask) == 0)
   {
     procn();
     freespace (ptr2);
   }

   /**********************************************************************
    *   Unless line 2 was the dummy end of file line it has been deleted *
    *********************************************************************/

   if (hptr == null)
     lowestl[currto] = currentl;
   else hptr->fptr = currentl;
   currentl->bptr = hptr;
   currentl->symb = currentl->symb | minusmask;
}

/**************************
 *   Insertion procedures *
 *************************/

procis (str, ch)
char *str, ch;
{
   struct cur_rec *hptr = currentl->bptr;
   int size;
   int i;
   struct cur_rec *ptr;
   if (swcs) compress_spaces (str);
   size = strlen (str);
   ptr = getspace (minlinesize + size / BYTESPERWORD + 1);
   ptr->stringb = ptr+1;
   ptr->bptr = hptr;
   ptr->fptr = currentl;
   currentl->bptr = ptr;
   if (hptr == null)
       lowestl[currto] = ptr;
     else hptr->fptr = ptr;
   ptr->lineno = currentl->lineno;
   ptr->symb = newmask;
   ptr->controlch = ch;
   for (i = 0; i <= size-1; i++) (ptr->stringb)[i] = str[i];
   ptr->stringb[size] = '\0';
}

procic()
{
   if ((currentl->symb & endmask) != 0) execerror ('s');
   if (clboccupied)
     procis (clbuff, currentl->controlch);
   else procis (currentl->stringb, currentl->controlch);
}

procim (add)
struct com_rec *(*add);
{
   /******************************************************************
    *   Inserts the appropriate type of inserted material before the *
    *  current line and moves the pointer held in address add on one *
    *  control block.                                                *
    *****************************************************************/

   FILE *infile;
   char *filename;
   struct com_rec *ptr = (*add)->nextcom;
   struct gen_rec *gptr;
   char tempst[MAXSTRLENGTH];
   int buff;
   struct cur_rec *hptr, *ptr1, *ptr2;
   *add = ptr;
   switch (ptr->name)
   {
     case IMTEXT : while (rdtextin (tempst)) procis (tempst, '\n');
                   break;

     case IMFILE: filename = ptr->arg1;
                  if ((infile = fopen (filename, "r")) == NULL)
                     execerror ('f', filename);
                  else
                  {
                    char ch;
                    char *t_rfm, *t_rat;
                    int t_file_is_ftn = ratrfm (filename, &t_rat, &t_rfm);
                    selectinput (infile);
                    ch = rdch();
                    while (ch != ENDSTREAMCH)
                    {
                      ch = readfromline (ch, tempst, t_file_is_ftn);
                      procis (tempst, ch);
                      ch = rdch();
                    }
                    selectinput (withf);
                    fclose (infile);
                  }
                  break;

     case IMINB : gptr = ptr->arg1;      /* arg1 must point to a chain    */
                  while (gptr != null)   /* of strings preceded by a      */
                  {                      /* link word.  Versatile little  */
                    procis (&gptr->stringb, '\n');   /* bugger, isn't it? */
                    gptr = gptr->nextg;
                  }
                  break;

     case IMBUFF : buff = ptr->arg1;
       hptr = currentl->bptr;
       ptr1 = lowestl[buff];
       ptr2 = (highestl[buff])->bptr;
       if (buff == currto || buff == currfrom) execerror ('b', buff);
       if (ptr2 == null)
       {
         if (swwarn) writef ("WARNING - buffer %d empty\n", buff);
         return;
       }
       ptr2->fptr = currentl;
       ptr1->bptr = hptr;
       lowestl[buff] = highestl[buff];
       (highestl[buff])->bptr = null;
       currentl->bptr = ptr2;
       if (hptr == null)
         lowestl[currto] = ptr1;
       else hptr->fptr = ptr1;
     break;

     case IMCOPY : buff = ptr->arg1;
                   ptr1 = lowestl[buff];
                   if (buff == currto || buff == currfrom)
                      execerror ('b', buff);
                   if (ptr1->fptr == null)
                   {
                     if (swwarn)
                        writef ("WARNING - buffer %d empty\n", buff);
                     return;
                   }
                   do
                   {
                     procis (ptr1->stringb, ptr1->controlch);
                     ptr1 = ptr1->fptr;
                   } while (ptr1->fptr != null);
                   break;
   }
}

switchwith (tofile)
FILE *tofile;
{
  user_stack[user_level++] = withf;
  withf = tofile;
  selectinput (withf);
  set_interactive();
  if (debug)
    writef ("User level now %d\n", user_level);
}

procc (add)
struct com_rec *(*add);
{
   /*****************************************************************
    *   Commands the appropriate type of argument and moves the     *
    *  pointer held in address add on one control block.            *
    *****************************************************************/

   FILE *infile;
   char *filename;
   char tbuff[FILENAMELENGTH];
   struct com_rec *ptr = (*add)->nextcom;
   struct gen_rec *gptr;
   int buff;
   struct cur_rec *hptr, *ptr1, *ptr2;
   *add = ptr;
   switch (ptr->name)
   {
     case IMFILE: filename = ptr->arg1;
                  if (debug) writef ("Commanding file /%s/\n", filename);
                  if ((infile = fopen (filename, "r")) == NULL)
                     execerror ('f', filename);
                  else switchwith (infile);
                  break;

/* For the moment, do buffers by writing them to files... */

     case IMBUFF : 
     case IMCOPY : buff = ptr->arg1;
                   ptr1 = lowestl[buff];
                   if (buff == currto || buff == currfrom)
                      execerror ('b', buff);
                   if (ptr1->fptr == null)
                   {
                     if (swwarn)
                        writef ("WARNING - buffer %d empty\n", buff);
                     return;
                   }
                   infile = fopen (TEMPWITHFILE, "w");
                   if (infile == 0) exit (42);
                   if (debug) writef ("Using temporary file /%s/\n",
                       fgetname (infile, tbuff));
                   do
                   {
                     fprintf (infile, "%s\n", ptr1->stringb);
                     ptr1 = ptr1->fptr;
                   } while (ptr1->fptr != null);
                   fgetname (infile, tbuff);
                   fclose (infile);
                   if (ptr->name == IMBUFF) procdbuff (buff);
                   if ((infile = fopen (tbuff, "r")) == NULL)
                      execerror ('f', filename);
                   else switchwith (infile);
                   break;
   }
}

/****************************
 *   Line joining procedure *
 ***************************/

proccl (st)
char *st;
{
   int s3length;
   char *s1 = currentl->stringb;
   char *s2, *s3;
   int s1length = strlen (s1);
   struct cur_rec *temp, *f, *p;
   verreq1 = TRUE;
   procn();
   if ((currentl->symb & endmask) != 0) execerror ('s');
   f = currentl->fptr;
   s2 = currentl->stringb;
   currentl = currentl->bptr;
   p = currentl->bptr;
   s3length = s1length + strlen (st) + strlen (s2);
   if (s3length > mxll) execerror ('e');
   temp = getspace (minlinesize + 1 + s3length / BYTESPERWORD);
   temp->stringb = temp+1;
   temp->lineno = currentl->lineno;
   temp->controlch = currentl->controlch;
   temp->symb = currentl->symb | plusmask;
   s3 = temp->stringb;
   strcpy (s3, s1);
   strcat (s3, st);
   strcat (s3, s2);
   s3[s3length] = '\0';
   temp->bptr = p;
   if (p == null)
     lowestl[currto] = temp;
   else p->fptr = temp;
   temp->fptr = f;
   if (f == null)
     highestl[currfrom] = temp;
   else f->bptr = temp;
   freespace (currentl->fptr);
   freespace (currentl);
   currentl = temp;
}

/***************************************
 *    Miscellaneous command procedures *
 **************************************/

proceq (n)
int n;
{
   struct cur_rec *ptr = currentl;
   if (currfrom != fromsource) execerror ('b', currfrom);
   do
   {
     ptr->lineno = n;
     ptr->symb = ptr->symb & endplusmask;
     ptr = ptr->fptr;
     if (ptr == null) break;
     n = n + 1;
   } while (TRUE);
   linesinput = n;
}

procw()
{
  int i;
  if (currto != tosource) execerror ('b', currto);
  if (currfrom != fromsource) execerror ('b', currfrom);
  if (swwarn)
    for (i = 1; i <= numbuff; i++)
      if (lowestl[i] != highestl[i])
        {
          if (interactive)
            execerror ('d', i);
          writef ("WARNING - buffer %d not empty\n", i);
        }
  if (! lineinput) inputln();
  while (currentl->fptr != null)
  {
    next();
    if (verreq1) verify();
  }
  while ((currentl->symb & endmask) == 0)
  {
    verreq1 = FALSE;
    inputln();
    if (verreq1) verify();
  }

  if (debug) writef ("procw: lowestl = %d, currentl = %d\n",
              lowestl[tosource], currentl);
  while (lowestl[tosource] != currentl)
    outputnl (MAXINT, tosource, tof);

/* Take care of outputting to any alternate TO file */

  flush_altfile();
  if (alttof != null) fclose (alttof);

/* Closedown all files */

  if (altfromf != null)
    fclose (altfromf);
  fclose (tof);
  fclose (verf);
  fclose (fromf);
  fclose (withf);
  deletefiles();
  stop (1);
}

procqm()
{
   /*************************************
    *   Carries out editing command "?" *
    ************************************/

   prclno();
   prcltext();
   verreq1 = FALSE;
   verreq2 = FALSE;
}

procem()
{
   /*************************************
    *   Carries out editing command "!" *
    ************************************/

   prclno();
   prcitext();
   verreq1 = FALSE;
   verreq2 = FALSE;
}

proccols (n)
int n;
{
   int length, i;
   switch (n)
   {
     case PLUSSYM :  if (clboccupied)
                       length = strlen (clbuff);
                     else
                       length = strlen (currentl->stringb);
                     break;

     case OTHERSYM : if (interactive)
                       length = 80;
                     else
                       length = 132;
                     break;

     default :       length = n;
                     break;
   }
   for (i = 1; i <= length; i++)
     wrch (i % 10 == 0 ? ' ' : (i % 10) + 48);
   wrch ('\n');
   verify();
}
