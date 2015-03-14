/************************************************************************
    keep.c 2.14 - save spam session for later
************************************************************************/
#include <stdio.h>
#include "spam.h"
keep(buf)
unsigned char *buf;
{
    int unitnum,t,i,j;
    if (!img_in_core) {
	printf("No saveable information.  Type \"help keep\" for help.\n");
	return;
    }
    unitnum=vosopen("spam.ses",1,1,10000);
    if (unitnum == -1) {
        printf("Couldn't open session file for output.\n");
        return;
    }
    j=1;
/*
/*     write out image info
*/
    voswrite(unitnum,&img_bw,1,j,1,sizeof(img_bw));
    voswrite(unitnum,&img_numchan,1,j+=sizeof(img_bw),1,sizeof(img_numchan));
    voswrite(unitnum,&img_nl,1,j+=sizeof(img_numchan),1,sizeof(img_nl));
    voswrite(unitnum,&img_sl,1,j+=sizeof(img_nl),1,sizeof(img_sl));
    voswrite(unitnum,&img_el,1,j+=sizeof(img_sl),1,sizeof(img_el));
    voswrite(unitnum,&img_ns,1,j+=sizeof(img_el),1,sizeof(img_ns));
    voswrite(unitnum,&img_sb,1,j+=sizeof(img_ns),1,sizeof(img_sb));
    voswrite(unitnum,&img_eb,1,j+=sizeof(img_sb),1,sizeof(img_eb));
    voswrite(unitnum,&img_swl,1,j+=sizeof(img_eb),1,sizeof(img_swl));
    voswrite(unitnum,&img_ewl,1,j+=sizeof(img_swl),1,sizeof(img_ewl));
    voswrite(unitnum,&img_generic,1,j+=sizeof(img_ewl),1,sizeof(img_generic));
    voswrite(unitnum,&img_modified,1,j+=sizeof(img_generic),1,
	sizeof(img_modified));
    j+=sizeof(img_modified);
    if (!img_modified) t=strlen(img_name);
    else t=strlen("spam.img");
    voswrite(unitnum,&t,1,j,1,sizeof(t));
    j+=sizeof(t);
    if (!img_modified) voswrite(unitnum,img_name,1,j,1,t);
    else voswrite(unitnum,"spam.img",1,j,1,t);
    j+=t;
/*
/*      write out stretch globals
*/
    voswrite(unitnum,&stretch_start,1,j,1,sizeof(stretch_start));
    voswrite(unitnum,&stretch_end,1,j+=sizeof(stretch_start),1,
        sizeof(stretch_end));
/*
/*      write out cursor shape and size
*/
    voswrite(unitnum,&cursor_nl,1,j+=sizeof(stretch_end),1,sizeof(cursor_nl));
    voswrite(unitnum,&cursor_ns,1,j+=sizeof(cursor_nl),1,sizeof(cursor_ns));
/*
/*      write out disp_ globals
*/
    voswrite(unitnum,&disp_band,1,j+=sizeof(cursor_ns),1,sizeof(disp_band));
/*
/*      write out plots information
*/
    voswrite(unitnum,&plots_mindn,1,j+=sizeof(disp_band),1,sizeof(plots_mindn));
    voswrite(unitnum,&plots_maxdn,1,j+=sizeof(plots_mindn),1,
        sizeof(plots_maxdn));
    voswrite(unitnum,&plots_norm,1,j+=sizeof(plots_maxdn),1,sizeof(plots_norm));
    voswrite(unitnum,&plots_norm_mode,1,j+=sizeof(plots_norm),1,
        sizeof(plots_norm_mode));
    voswrite(unitnum,&plots_maxy,1,j+=sizeof(plots_norm_mode),1,
        sizeof(plots_maxy));
    voswrite(unitnum,&plots_miny,1,j+=sizeof(plots_maxy),1,sizeof(plots_miny));
    voswrite(unitnum,plots,1,j+=sizeof(plots_miny),1,16*sizeof(struct plot));
    j+=16*sizeof(struct plot);
    for (i=0;i<16;i++) if (plots[i].name!=0) {
	t=strlen(plots[i].name);
	voswrite(unitnum,&t,1,j,1,sizeof(t));
        j+=sizeof(t);
	voswrite(unitnum,plots[i].name,1,j,1,t);
        j+=t;
        voswrite(unitnum,plots[i].data,1,j,1,img_numchan);
        j+=img_numchan;
        voswrite(unitnum,plots[i].variance,1,j,1,img_numchan);
        j+=img_numchan;
    }
/*
/*      write out hist information
*/
    voswrite(unitnum,&hist_on,1,j,1,sizeof(hist_on));
    j+=sizeof(hist_on);
    if (hist_on) {
        voswrite(unitnum,&hist_nbins,1,j,1,sizeof(hist_nbins));
        voswrite(unitnum,&hist_scale,1,j+=sizeof(hist_nbins),1,
	    sizeof(hist_scale));
        voswrite(unitnum,&hist_nelts,1,j+=sizeof(hist_scale),1,
	    sizeof(hist_nelts));
        voswrite(unitnum,&hist_sum,1,j+=sizeof(hist_nelts),1,sizeof(hist_sum));
        voswrite(unitnum,&hist_sum2,1,j+=sizeof(hist_sum),1,sizeof(hist_sum2));
        voswrite(unitnum,hist_bins,1,j+=sizeof(hist_sum2),1,
            hist_nbins*sizeof(*hist_bins));
        j+=hist_nbins*sizeof(*hist_bins);
    }
    vosclose(unitnum);
/*
/*      write out current band
*/
    unitnum=vosopen("spam.lst",1,img_nl,img_bw);
    if (unitnum == -1) {
         printf("Couldn't save current band in keep.\n");
         return;
    }
    voswrite(unitnum,img_currband,1,1,img_nl,img_bw);
    vosclose(unitnum);
/*
/*      if image was changed using filter or func, save changed image
*/
    if (img_modified) {
        unitnum=vosopen("spam.img",1,img_nl,img_ns);
        if (unitnum == -1) {
	    printf("Couldn't save changed image in keep.\n");
	    return;
        }
	voswrite(unitnum,img_data,1,1,img_nl,img_ns);
	vosclose(unitnum);
    }
/*
/*      exit
*/
    if (display_mode==1) printf(
        "When you restore this spam session, you will be in graphics mode.\n");
}
