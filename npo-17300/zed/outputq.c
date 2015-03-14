/* section "outputq" */

#include "glob"

/******************************************************************************
 *                                                                            *
 *   Contains procedures to set up a heap, allocate and free space in it,     *
 *  to input new lines and to handle the buffers.                             *
 *                                                                            *
 *****************************************************************************/

/*************************************************************
 *   Procedures to input and output lines of the source file *
 ************************************************************/

outputnl (n, source, dest)
int n, source;
FILE *dest;
{
  /****************************************************************************
   *  Sends either the lowest n lines or all lines before the current line in *
   *  the output queue to to, depending which is least, freeing the blocks    *
   *  they were held in.                                                      *
   *  N.B. This was originally coded to use currto.  This led to problems     *
   *  if you called flush() or getspace() needed to make room while a buffer  *
   *  was open for writing.                                                   *
   ***************************************************************************/

   int i;
   char b[MAXSTRLENGTH];
   selectoutput (dest);
   for (i = 1; i <= n; i++)
   {
     if (lowestl[source] == currentl || lowestl[source]->fptr == null)
     {
       lowestl[source]->bptr = null;
       selectoutput (verf);
       return;
     }
     if (swtr || swdetab) strcpy (b, lowestl[source]->stringb);
     if (swtr) strip_spaces (b);
     if (swdetab) detab (b);
     if (file_is_ftn)               /* God, FTN files are a pain-in-the-*** */
     {
       if (lowestl[source]->controlch == '\n' ||
           lowestl[source]->controlch == '\f')
         wrch (lowestl[source]->controlch);
     }
     if (swtr || swdetab)
       writef ("%s", b);
     else writef ("%s", (lowestl[source])->stringb);
     if (file_is_ftn)        /* Made simpler to handle because we only handle */
     {                       /*  the cases of ' ', '+', and '1'. */
       wrch ('\r');
       fflush (dest);                /* VaxC makes this awful thing necessary */
     }
     else if (lowestl[source]->controlch == '\f' ||
         lowestl[source]->controlch == '\r')
       wrch (lowestl[source]->controlch);
     else wrch ('\n');
     lowestl[source] = (lowestl[source])->fptr;
     freespace ((lowestl[source])->bptr);
   }
   (lowestl[source])->bptr = null;
   selectoutput (verf);
   if (debug)
     dwritef ("Next line to output is:\n%s\n", lowestl[source]->stringb);
}

inputln()
{
  /****************************************************************************
   *   Reads a line from FROM to the end of the output queue and makes it the *
   *  new current line. Requires previous current line to be the last line    *
   *  before.                                                                 *
   ***************************************************************************/

   int *getspace();
   char b[MAXSTRLENGTH];
   char readfromline();
   int size = 0;
   int i;
   char ch;
   struct cur_rec *tempptr;

   if (debug)
     dwritef ("inputln()\n");

/* This'll have to change for the new order of things... I think...
   Besides, how could it happen anyway???
   (Footnote: it hasn't mattered yet.  I wonder how I got away with it?)  */

   if (highestl[currfrom] != currentl) execerror ('i');
   if (halt_lineno != ENDSYM)
   {
     if (currentl->lineno == halt_lineno)
     {
       writef ("*** HALT\n");
       longjmp (jmpenv, 8);
     }
     if (currentl->lineno > halt_lineno) execerror ('h');
   }
   selectinput (curfromf);
   ch = rdch();
   if (ch == ENDSTREAMCH)
    {

     /*************************************
      *   Create a dummy end of file line *
      ************************************/

     if (debug)
       dwritef ("Creating dummy end of file line\n");
     selectinput (withf);
     tempptr = getspace (minlinesize);
     tempptr->stringb = tempptr+1;
     tempptr->symb = endmask;
     tempptr->controlch = '\n';
   }
   else
   {
     /***********************
      *   Read the new line *
      **********************/

     char savech = readfromline (ch, b, file_is_ftn);
     selectinput (withf);
     if (swtr)
       strip_spaces (b);
     if (swcs)
       compress_spaces (b);
     if (swdetab)
       detab (b);
     size = strlen (b);
     tempptr = getspace (size / BYTESPERWORD + 1 + minlinesize);
     tempptr->stringb = tempptr+1;
     tempptr->controlch = savech;
     tempptr->symb = 0;
     strcpy (tempptr->stringb, b);
     if (debug)
       dwritef ("Using space from %d to %d\n", tempptr, 
               (tempptr->stringb) + size);
   }
   if (currentl == null)
     lowestl[currto] = tempptr;

   /***********************************************
    *   The new line is the first one to be input *
    **********************************************/
   else
   {
     currentl->fptr = tempptr;
     if (currto != 0) currentl->symb = currentl->symb | movmask;
   }
   linesinput = linesinput + 1;
   tempptr->lineno = linesinput;
   tempptr->fptr = null;
   tempptr->bptr = currentl;
   (tempptr->stringb)[size] = '\0';
   highestl[currfrom] = tempptr;
   currentl = tempptr;
   if (debug)
   {
     dwritef ("currentl = %d, currentl->stringb = %d,", currentl,
             currentl->stringb);
     dwritef ("currentl->bptr = %d\n", currentl->bptr);
   }
   cur_col = 0;
   doglobals (glist);
}

/**********************************************
 *   These procedures handle the text buffers *
 *********************************************/

int procdbuff (buff)
int buff;
{
   int i;
   if (buff == 0)
   {
     if (currto != tosource) execerror ('b', currto);
     if (currfrom != fromsource) execerror ('b', currfrom);
     for (i = 1; i <= numbuff; i++) procdbuff (i);
   }
   else
   {
     struct cur_rec *f = lowestl[buff];
     struct cur_rec *f2;
     if (currto == buff || currfrom == buff) execerror ('b', buff);
     f2 = f->fptr;
     while (f2 != null)
     {
       freespace (f);
       f = f2;
       f2 = f->fptr;
     }
     lowestl[buff] = f;
     highestl[buff] = f;
     f->bptr = null;
   }
}

procshbuff()
{
   int buff;
   for (buff = 1; buff <= numbuff; buff++)
        if (lowestl[buff] != highestl[buff]) writef ("buffer %d\n", buff);
}

proctbuff (buff)
int buff;
{
   int i;
   if (buff == 0)
   {
     if (currto != tosource) execerror ('b', currto);
     if (currfrom != fromsource) execerror ('b', currfrom);
     for (i = 1; i <= numbuff; i++) if (lowestl[i] != highestl[i])
     {
       writef (" buffer %d -\n", i);
       proctbuff (i);
     }
   }
   else
   {
     struct cur_rec *f = lowestl[buff];
     if (currto == buff || currfrom == buff) execerror ('b', buff);
     while (f->fptr != null)
     {
       writef ("%s\n", f->stringb);
       f = f->fptr;
     }
   }
}

proctobuff (ptr)
struct com_rec *ptr;
{
  if (ptr->arg2 == IMFILE) tofile (ptr->arg1);
  else tobuff (ptr->arg1);
}

tobuff (buff)
int buff;
{
   /*****************************************************
    *   Note buff = 0 is equivalent to buff = tosource  *
    ****************************************************/

  struct cur_rec *hptr = currentl->bptr;
  struct cur_rec *e = highestl[currto];
  struct cur_rec *p = (highestl[buff])->bptr;

/* if ((currentl->symb & endmask) != 0) execerror ('s');             */

  if (buff == currfrom) execerror ('b', buff);
  if (swwarn && currto == buff)
     writef ("WARNING - buffer selected already\n");
  currentl->bptr = p;
  if (p == null)
    lowestl[buff] = currentl;
  else p->fptr = currentl;
  e->bptr = hptr;
  if (hptr == null)
    lowestl[currto] = e;
  else hptr->fptr = e;
  currto = buff;
  if (buff == tosource && alttof != null)
  {
    flush_altfile();
    curtof = tof;
  }
}

procfrombuff (ptr)
struct com_rec *ptr;
{
  if (ptr->arg2 == IMFILE) fromfile (ptr->arg1);
  else frombuff (ptr->arg1);
}

switch_b (b, buff, hptr)
struct cur_rec *b, *hptr;
int buff;
{
   currentl->fptr = b;
   lowestl[buff] = null;
   if (b == null)
     highestl[buff] = currentl;
   else b->bptr = currentl;
   lowestl[currfrom] = hptr;
   if (hptr == null)
     lowestl[currfrom] = null;
   else hptr->bptr = null;
}

frombuff (buff)
int buff;
{
  struct cur_rec *hptr = currentl->fptr;
  struct cur_rec *b;
  if (buff == 0) buff = fromsource;
  b = lowestl[buff];
  if (currto == buff) execerror ('b', buff);
  if (swwarn && currfrom == buff)
     writef ("WARNING - buffer selected already\n");

/* We get around the problem of being at the end of the buffer (since
   FROM would tack the next line after the current line) by
   moving back a line, switching as before, then moving forward.
   If there is no line to move back to, we read in the new one
   straight away (so that it will be tacked onto the main queue)
   and then move back onto it (inputln() moves currentl forward).       */

  if ((currentl->symb & endmask) != 0)
  {
    if (b == null && buff != altfrom && buff != fromsource) execerror ('s');

/* If the current buffer isn't empty */

    if (currentl->bptr != null)
    {
      currentl = currentl->bptr;
      switch_b (b, buff, hptr);
      currfrom = buff;
      if (buff == altfrom) inputln();
      else if (currentl->fptr != null) currentl = currentl->fptr;
    }
    else
    {
      if (buff == altfrom)
      {
        inputln();
        currentl = currentl->bptr; /* Since we've ensured altfrom isn't empty */
      }
      else currentl = lowestl[buff];        /* I give up... */
      switch_b (b, buff, hptr);
    }
    verify();
  }
  else switch_b (b, buff, hptr);
  currfrom = buff;
  if (buff == fromsource && altfromf != null)
    curfromf = fromf;
}

/*********************
 * Change input file *
 ********************/

fromfile (filename)
char *filename;
{
  char temps1[FILENAMELENGTH], temps2[FILENAMELENGTH], ch;
  FILE *tempf;
  tempf = fopen (filename, "r");

/* Check in case we're attempting to access the same file we're already
   reading; that's okay.  Just need to hook the queue back up. */

  if (tempf != NULL && altfromf != null)
  {
    if (strcmp (fgetname (altfromf, temps1), fgetname (tempf, temps2)) == 0)
    {
      frombuff (altfrom);
      fclose (tempf);
      curfromf = altfromf;
      return;
    }
  }

/* Check to see if it's empty; it's much easier to fault this than
   try to read it (since the main file might be empty too, and it's
   horrendous trying to get the pointers right in that case)         */

  selectinput (tempf);
  ch = rdch();
  if (ch == ENDSTREAMCH)
  {
    writef ("**FROM file is empty\n");
    fclose (tempf);
    return;
  }
  else
  {
    ungetchar (ch);
    selectinput (withf);
  }

/* Now we know it's a different file, so check to see if there's
   an old one, and close it.  */

  if (altfromf != null) fclose (altfromf);
  altfromf = tempf;
  curfromf = altfromf;
  frombuff (altfrom);  
}

/**********************
 * Change output file *
 *********************/

tofile (filename)
char *filename;
{
  char temps[FILENAMELENGTH];

/* Check in case we're attempting to access the same file we're already
   writing; that's okay.  Just need to hook the queue back up.
   If it isn't there, fine.                                  */

  strcpy (temps, filename);
  if (alttof != null)
  {
    find_file (temps, ".LIS", OUTPUT);
    if (strcmp (alttofilename, temps) == 0)
    {
      curtof = alttof;
      tobuff (altto);
      return;
    }
  }

/* Now we know it's a different file, so check to see if there's
   an old one, and close it.  */

  if (alttof != null)
  {
    flush_altfile();
    fclose (alttof);
  }

/* Use same output format as the current output file */

  if (! from_file_created)
  {
    if (*rat == '\0') alttof = fopen (filename, "w", rfm);
    else alttof = fopen (filename, "w", rat, rfm);
  }
  else alttof = fopen (filename, "w");
  if (alttof == NULL) execerror ('f', filename);
  fgetname (alttof, alttofilename);
  curtof = alttof;
  tobuff (altto);  
}

initoutputq()
{
   /*************************************************************************
    *   Sets up the output queue.                                           *
    ************************************************************************/


   int *getspace();
   int i;
   linesinput = 0;
   currentl = null;
   lowestl = getspace (numbuff - altto) - altto;
   highestl = getspace (numbuff - altto) - altto;
   for (i = altto; i <= numbuff; i++) if (i != fromsource)
   {
     /*************************************
      *   Note tosource is equal to zero. *
      ************************************/

     struct cur_rec *ptr = getspace (minlinesize);
     ptr->stringb = ptr + 1;
     ptr->controlch = '\n';
     ptr->symb = endmask + newmask;
     (ptr->stringb)[0] = '\0';
     ptr->fptr = null;
     ptr->bptr = null;
     lowestl[i] = ptr;
     highestl[i] = ptr;
   }
   lowestl[altfrom] = null;
   highestl[altfrom] = null;
/*   lowestl[altto] = null;  */
   lowestl[fromsource] = null;
   highestl[fromsource] = null;
   lowestl[tosource] = null;
   currto = tosource;
   currfrom = fromsource;
   lineinput = FALSE;
}


/*********************************************
 *   Procedures to get and return heap space *
 ********************************************/

int freespace(ptr)
char *ptr;
{
   free (ptr);
}

int *getspace (size)
int size;
{
   /**************************************************************************
    *   Allocates a block of size words in the                               *
    *  output queue heap and returns a pointer to it. If a block cannot be   *
    *  found outputnl (noutput) is called, noutput being a manifest constant *
    *************************************************************************/

   char *tempptr;
   do
   {
     if (debug)
       dwritef ("Allocating %d words to ", size);
     tempptr = malloc (size * BYTESPERWORD);
     if (debug)
       dwritef ("%d\n", tempptr);
     if (tempptr >= 1)
       return tempptr;

      /*****************************************
       *  A large enough block cannot be found *
       ****************************************/

     if (goutdisabled || lowestl[currto] == currentl || 
         (lowestl[currto])->fptr == null)
       execerror ('o');
     if (debug)
       dwritef ("Flushing to make room in getspace()\n");
     makeroom (noutput);
   } while (TRUE);
}

/*************************************************************************
 * We can't simply call outputnl, it might be allocated to another file. *
 * We need to flush both the main and alternate files.                   *
 ************************************************************************/

makeroom (n)
int n;
{
  outputnl (n, tosource, tof);
  if (alttof != null && highestl[altto] != lowestl[altto])
    outputnl (n, altto, alttof);
}

/********************************************************
 * Output any lines in ALTTO buffer to the ALTTOF file. *
 *******************************************************/

flush_altfile()
{
  if (highestl[altto] != lowestl[altto])
  {
    struct cur_rec *savecurrentl = currentl;
    currentl = highestl[altto];
    while (lowestl[altto] != currentl) outputnl (MAXINT, altto, alttof);
    currentl = savecurrentl;
  }
}

/* Readfromline() reads a string from FROM and returns the    */
/* control character for the line.                            */
/* We enter having already read one character, which is in ch */

char readfromline (ch, b, is_ftn)
char ch, *b;
int is_ftn;
{
  int size = 0;
  char savech;
  if (is_ftn)
    if (ch != '\f' && ch != '\n') savech = '\r';
    else
    {
      savech = ch;
      ch = rdch();
    }
  do
  {
    if (((ch == '\n' || ch == '\f') && (! is_ftn)) || ch == '\r')
      break;
    if (size >= mxll)
    {
      ungetchar (ch);
      if (swwarn) writef ("WARNING - input line too long\n");
      break;
    }
    b[size++] = ch;
    ch = rdch();
  } while (TRUE);
  b[size] = '\0';
  return is_ftn ? savech : ch;
}
