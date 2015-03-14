/*************************************************************************
   norm.c 2.15 - plot normalization selection
   norm changes the current normalization of plots between amplitude
   normalization (the default) and equal-energy normalization.  If the
   normalization mode is changed when plots are up, the plots will be
   redrawn.  Using the norm command without parameters will refill the
   plots using the current normalization.
*************************************************************************/
#include <stdio.h>
#include "spam.h"
norm(buf)
unsigned char *buf;
{
    char amp[80],area[80];
    int i;
    static struct parameter kwds[]={
        {"amplitudenorm",3,0,0,"amplitude normalization","",80,0},
        {"areanorm",3,0,0,"area normalization","",80,0}
        };
    if (display_mode==1) {
	printf("No renormalization in display mode.  Use the \"return\" ");
	printf("command to exit.\n");
	return;
    }
    kwds[0].value = amp;
    kwds[1].value = area;
    if (par(buf,kwds,2)) return;	/* get norm mode, return on error */
/*
/*      if currently using amplitude scaling, or user specified amplitude
/*      scaling, calculate new mindn and maxdn for plots on screen.
*/
    if ((plots_norm_mode==1 || amp[0]!='\0') && area[0]=='\0') {
	plots_norm_mode=1;
        plots_mindn=255;
        plots_maxdn=0;
        for (i=0;i<16;i++) if (plots[i].name!=0) {
            if (plots[i].mindn < plots_mindn) plots_mindn = plots[i].mindn;
            if (plots[i].maxdn > plots_maxdn) plots_maxdn = plots[i].maxdn;
        }
    }
/*
/*      otherwise, if at least one plot exists, calculate factor for
/*      normalization, and then calculate pseudo mindn and maxdn using
/*      normalization factor; these are used internally only, and are
/*      not output on any scales.
*/
    else if ((plots_norm_mode==2 || area[0] != '\0') && amp[0]=='\0') {
	plots_norm_mode=2;
	for (i=0;i<16 && plots[i].name==0;i++) ;
        if (i!=16) {
	    plots_norm=(200*plots[i].sum)/(2*plots[i].maxdn);
        }
        plots_mindn=255;
        plots_maxdn=0;
        for (i=0;i<16;i++) if (plots[i].name!=0) {
            if (plots[i].mindn*plots_norm/plots[i].sum < plots_mindn) 
                plots_mindn = plots[i].mindn*plots_norm/plots[i].sum;
            if (plots[i].maxdn*plots_norm/plots[i].sum > plots_maxdn) 
                plots_maxdn = plots[i].maxdn*plots_norm/plots[i].sum;
        }
    }
    else printf("Conflicting normalizations.  Ignoring command.\n");
/*
/*      if there are any plots on the screen, redraw them, disabling the
/*      cursor tracking temporarily.
*/
    if (plots_num != 0) draw_plots();
}
