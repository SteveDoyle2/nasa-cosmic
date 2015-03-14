/*
*       This routine uses the FAB to get the record format
*       of a file.  Return TRUE if rat is FTN.
*/

#include "glob"
#include <rms>

struct FAB fab;                 /* declare local fab structure */

int ratrfm (filenam, arat, arfm)
char **arat, **arfm, *filenam;
{
  char filename[FILENAMELENGTH];
  int status;                     /* rms status variable */
  int rat, rfm;
  strcpy (filename, filenam);
  fab = cc$rms_fab;            /* Initialize fab structure */
  fab.fab$l_fna = &filename;   /* set filename in fab */
  fab.fab$b_fns = strlen(filename); /* Set length of filename in fab */
  if ((status = sys$open (&fab)) != RMS$_NORMAL) execerror ('f', filename);
  rfm = fab.fab$b_rfm;
  rat = fab.fab$b_rat;
  if ((status = sys$close(&fab)) != RMS$_NORMAL) execerror ('f', filename);
  if (rat & FAB$M_CR) *arat = "rat=cr";
  else if (rat & FAB$M_FTN) *arat = "rat=ftn";
  else if (rat & FAB$M_BLK) *arat = "rat=blk";
  else if (rat & FAB$M_PRN) *arat = "rat=prn";
  else *arat = "";
  switch (rfm)
  {
    case FAB$C_FIX : *arfm = "rfm=fix";
                     break;

    case FAB$C_STM : *arfm = "rfm=stm";
                     break;

    case FAB$C_STMCR : *arfm= "rfm=stmcr";
                     break;

    case FAB$C_STMLF : *arfm= "rfm=stmlf";
                     break;

    case FAB$C_UDF : *arfm= "rfm=udf";
                     break;

    default :
    case FAB$C_VAR : *arfm= "rfm=var";
                     break;

    case FAB$C_VFC : *arfm= "rfm=vfc";
                     break;
  }
  if (rat & FAB$M_FTN) return TRUE; else return FALSE;
}
