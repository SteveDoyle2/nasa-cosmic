/*************************************************************************
   delete.c 2.14 - deletes specified plot from the user's spectral library.
*************************************************************************/
#include <stdio.h>
#include "spam.h"
delentry(buf)
unsigned char *buf;
{
    int i,index,libr,icount;
    char plot[16*20],locbuf[80];
    static struct parameter kwds[]={
        {"plotname",2,1,-16,
	    "name(s) of library spectra to delete","",20,0},
        };
    kwds[0].value = plot;
    kwds[0].input_count = &icount;
    if (par(buf,kwds,1)) return;	  /* Get plot name, with possible ?s. */
    for (i=0;i<icount;i++) {
	strcpy(locbuf,plot+i*20);
        get_libplot_num(locbuf,&index,&libr,0,2);  /* Get full name.       */
        if (index==-1) 
	    printf("Couldn't find a user-library plot named \"%s\".\n",locbuf);
        while (index<0) {
	    query("New name (RETURN for READY) ? ",locbuf);
	    if (locbuf[0]=='\0') return;
	    if (!check_old_name(locbuf)) {
		get_libplot_num(locbuf,&index,&libr,0,2);
                if (index==-1) 
	            printf("Couldn't find a user-library plot named \"%s\".\n",
		        locbuf);
	    }
        }
        if (libr_delete_entry(locbuf,user_lib)!=-1) user_lib_changed = 1;
    }
}
