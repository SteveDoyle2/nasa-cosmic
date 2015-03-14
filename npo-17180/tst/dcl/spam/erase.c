/**********************************************************************
   %M% %I% - erase specific plots and/or the histogram.
   erase erases plots, individually or in groups.  Plots can be
   specified by name using the plotname keyword, or by group using
   the keywords libplots, dataplots, curvegens, and functions.  The
   histogram can be erased with the hist keyword.
**********************************************************************/
#include <stdio.h>
#include "spam.h"
erase(buf)
unsigned char *buf;
{
    int icount,i,j,index;
    char lib[10],data[10],plot[16*20],hist[10],curvegens[10],functions[10];
    char cluster[10],locbuf[80],feature[10];
    static struct parameter kwds[]={
        {"libplots",3,0,0,"erase library plots","",10,0},
        {"dataplots",3,0,0,"erase data plots","",10,0},
        {"plotname",2,0,-16,
	    "name(s) of individual plot(s) to erase (default is none) ",
	    "",20,0},
        {"hist",3,0,0,"erase histogram, if any","",10,0},
        {"curvegens",3,0,0,"erase curvegen curves","",10,0},
        {"functions",3,0,0,"erase computed curves","",10,0},
        {"clusterplots",3,0,0,"erase cluster plots","",10,0},
        {"feature",3,0,0,"erase feature extraction info, if any","",10,0},
        };
    kwds[0].value = lib;
    kwds[1].value = data;
    kwds[2].value = plot;
    kwds[2].input_count = &icount;	/* get # of individual plots to erase */
    kwds[3].value = hist;
    kwds[4].value = curvegens;
    kwds[5].value = functions;
    kwds[6].value = cluster;
    kwds[7].value = feature;
    if (par(buf,kwds,8)) return;
    if (plot[0]!='\0' && display_mode==0) {	   /* erase specific plot(s). */
        for (j=0;j<icount;j++) {
	    strcpy(locbuf,plot+j*20);
            get_plot_num(locbuf,0,&i);
	    if (i==-1) printf("Plot \"%s\" doesn't exist.  ",locbuf);
	    else if (i==-2) printf("The name \"%s\" is ambiguous.  ",locbuf);
            while (i<0) {
                query("Name of plot (RETURN for READY) ? ",locbuf);
		if (locbuf[0]=='\0') {
    		    if (display_mode==0) draw_plots();
		    return;
		}
                else if (!check_old_name(locbuf)) {
		    get_plot_num(locbuf,0,&i);
		    if (i==-1) printf("Plot \"%s\" doesn't exist.  ",locbuf);
	            else if (i==-2)
			printf("The name \"%s\" is ambiguous.  ",locbuf);
		}
            }
            erase_plot(i);
        }
    }
    if (lib[0]=='\0' && data[0]=='\0' && plot[0]=='\0' && hist[0]=='\0' &&
	    curvegens[0]=='\0' && functions[0]=='\0' && cluster[0]=='\0' &&
	    feature[0]=='\0') {
        if (display_mode==0) {
	    for (i=0;i<16;i++) if (plots[i].name!=0) erase_plot(i);
        }
        else printf("Can't erase plots in display mode.\n");
        if (hist_on) erase_hist();
	else erase_fea();
    }
    if (data[0]!='\0') {	/* erase dataplots - dataplots are marked by  */
        if (display_mode==0) {	/* a plottype of 0 in the plots array.        */
	    for (i=0;i<16;i++)
                if (plots[i].name != 0 && plots[i].plottype==0) erase_plot(i);
        }
	else printf("Can't erase dataplots in display mode.\n");
    }
    if (lib[0]!='\0') {		/* erase libplots - libplots are marked by  */
	if (display_mode==0) {	/* a plottype of 1 in the plots array.      */
	    for (i=0;i<16;i++)
                if (plots[i].name != 0 && plots[i].plottype==1) erase_plot(i);
        }
        else printf("Can't erase libplots in display mode.\n");
    }
    if (curvegens[0]!='\0') {	/* erase curvegens - curvegens are marked by  */
        if (display_mode==0) {	/* a plottype of 2 in the plots array.        */
	    for (i=0;i<16;i++)
                if (plots[i].name != 0 && plots[i].plottype==2) erase_plot(i);
        }
	else printf("Can't erase curvegen plots in display mode.\n");
    }
    if (functions[0]!='\0') {	/* erase funcplots - funcplots are marked by  */
        if (display_mode==0) {	/* a plottype of 3 in the plots array.        */
	    for (i=0;i<16;i++)
                if (plots[i].name != 0 && plots[i].plottype==3) erase_plot(i);
        }
	else printf("Can't erase function plots in display mode.\n");
    }
    if (cluster[0]!='\0') {	/* erase cluster plots, marked by a plottype  */
        if (display_mode==0) {	/* of 3 in the plots array.                   */
	    for (i=0;i<16;i++)
                if (plots[i].name != 0 && plots[i].plottype==4) erase_plot(i);
        }
	else printf("Can't erase cluster plots in display mode.\n");
    }
    if (hist[0]!='\0' && hist_on) erase_hist();
    if (feature[0]!='\0') erase_fea();
    if (display_mode==0) draw_plots();		/* redraw current plots.     */
}
