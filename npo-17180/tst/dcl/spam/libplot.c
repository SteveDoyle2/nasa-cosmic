/*************************************************************************
   libplot.c 2.14 - libplot plots spectra from the spectral libraries.
*************************************************************************/
#include <stdio.h>
#include "spam.h"

libplot(buf)
unsigned char *buf;
{
    int in=0,i,items,fd,j,k,libr,count;
    char curve[16*80];
    unsigned char *data,*malloc();
    static struct parameter kwds[]={
        {"dataset",2,1,-16,"dataset name(s)","",80,0},
        };
    if (img_in_core==0 && feature_range==0) {
	printf("No image or spectral resampling range chosen.  ");
        printf("Type \"help libplot\" for help.\n");
        return;
    }
    if (plots_num==16) {
	printf("There aren't any colors left.  Please erase a plot.\n");
        return;
    }
    if (display_mode==1) {
	printf("No libplots in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
	return;
    }
    data = malloc(img_numchan);
    if (data==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    kwds[0].value = curve;
    kwds[0].input_count = &count;
    if (par(buf,kwds,1)) return;
    for (k=0;k<count;k++) {
        get_libplot_num(curve+k*80,&i,&libr,0,3);
	if (i==-1) printf("Couldn't find a library spectrum labeled \"%s\".\n",
	    curve+k*80);
        while (i<0) {
	    query("Name (RETURN for READY) ? ",curve+k*80);
	    if (*(curve+k*80)=='\0') {
		draw_plots();
		free(data);
		return;
	    }
	    else if (check_old_name(curve+k*80)) ;
	    else {
		get_libplot_num(curve+k*80,&i,&libr,0,3);
	        if (i==-1)
		    printf("Couldn't find a library spectrum labeled \"%s\".\n",
	                curve+k*80);
	    }
        }
        get_plot_num(curve+k*80,1,&j);
        if (j!=-1) 			/* Make sure curve isn't plotted.     */
	    printf("\"%s\" is already plotted.\n",curve+k*80);
        else if (!get_lib_data(i,libr,data))
            save_plot_data(curve+k*80,data,1,0,0,0,0,-1,0);
    }
    draw_plots();
    free(data);
}
