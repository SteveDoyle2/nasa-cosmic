/************************************************************************
   histsubs.c 2.14 - save_hist_data() saves data to be graphed.
   draw_hist() graphs the current data in the histogram area, erasing
   any old graphs first.
************************************************************************/
#include <math.h>
#include <stdio.h>
#include "spam.h"
save_hist_data(bins,nbins)
long int *bins;
int nbins;
{
    int i;
    unsigned char *calloc();
    if (hist_nbins!=0) cfree(hist_bins);
    hist_scale = (nbins-1)/258+1;
    if (hist_scale!=1) printf(
        "Histogram bars have been scaled to represent %d values each.\n",
        hist_scale);
    hist_nbins = (nbins+hist_scale-1)/hist_scale;
    hist_bins = (int *) calloc(hist_nbins,4);	    /* calloc inits memory */
    if (hist_bins==NULL) {
	printf("Insufficient memory.\n");
	return(1);
    }
    hist_nelts = 0;
    hist_sum = 0;
    hist_sum2 = 0;
    for (i=0;i<nbins;i++) {
        hist_nelts += bins[i];
        hist_sum += (float)i*bins[i];
        hist_sum2 += (float)i*i*bins[i];
        hist_bins[i/hist_scale]+=bins[i];
    }
    return(0);
}


draw_hist()
{
    unsigned char graph[256],buf[80];
    long int maxval;
    int i,j,k,l,binwid,number_spacing;
    short int t1,t2,c,c8=8,c9=9,c255=255,c512=512,x[2],y[2];
    float f1=1.;
    int cx=77,cy=496;           /* line, samp of lower-left hist corner.  */
    float u,sdev;
    double sqrt();
    if (hist_nbins==0) return;	/* no histogram data to graph.            */
    vdareafi(&unit,&c9,&c0,&c0,&c0,&c1,&c1,&c512,&c512);
    if (display_mode==0) {
	t1=plots_hoff;
	t2=plots_voff+30;
	vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&t2,&c512,&c512);
    }
/*
/* draw horizontal axis with tick marks and scale.  Binwid = width of hist bar
/* in pixels.  Number_spacing is the number of bins per scale value, always
/* a multiple of 5, unless number of bins is less than 10.
*/
    x[0]=cx;
    y[0]=cy;
    binwid = max(258/hist_nbins,1);
    x[1]=x[0]+hist_nbins*binwid-1;      /* draw histogram horizontal axis.  */
    y[1]=y[0];
    vdvector(&unit,&c8,&c255,&c0,&c0,&c2,x,y);
    vdvector(&unit,&c1,&c0,&c0,&c255,&c2,x,y);
    if (hist_nbins<=15) number_spacing=1;
    else number_spacing = max(hist_nbins/32,1)*5;
    for (i=0;i<hist_nbins;i++)
	if (i/max(hist_nbins/25,1)*max(hist_nbins/25,1)==i) {
	x[0]=cx+i*binwid+binwid/2;                      /* draw tick mark.  */
	y[0]=cy;
	x[1]=x[0];
	y[1]=y[0]+2;
        vdvector(&unit,&c8,&c255,&c0,&c0,&c2,x,y);
        vdvector(&unit,&c1,&c0,&c0,&c255,&c2,x,y);
        if (i/number_spacing*number_spacing==i) {	/* label tick mark. */
	    sprintf(buf,"%d",i*hist_scale);
	    c=strlen(buf);
	    t1=cx+i*binwid-3*c+binwid/2;
	    t2=508;
	    vdtxtsiz(&c7,&f1);
	    vdtext(&unit,&c8,&c255,&c0,&c0,&t1,&t2,&c1,&c,buf);
	    vdtext(&unit,&c1,&c0,&c0,&c255,&t1,&t2,&c1,&c,buf);
	}
    }
/*
/* get max value and normalize for 120-pixel maximum bar height
*/
    maxval=0;                              /* get maximum value for norm    */
    for (i=0;i<hist_nbins;i++) if (hist_bins[i]>maxval) maxval=hist_bins[i];
    if (maxval==0) return;               /* all-zero histogram -> return  */
    for (i=0;i<hist_nbins;i++) graph[i] = (120.*hist_bins[i])/maxval+.5;
/*
/* draw histogram bars.
*/
    for (i=0;i<hist_nbins;i++) for (j=0;j<binwid;j++) {
	x[0]=cx+i*binwid+j;
	y[0]=cy;
	x[1]=x[0];
	y[1]=y[0]-graph[i];
        vdvector(&unit,&c8,&c255,&c0,&c0,&c2,x,y);
        vdvector(&unit,&c1,&c0,&c0,&c255,&c2,x,y);
    }
/*
/* write out statistical information.
*/
    u=hist_sum/hist_nelts;
    sdev = sqrt((double)(hist_sum2 - 2*u*hist_sum + hist_nelts*u*u) / 
        (hist_nelts-1.));
    sprintf(buf,"N = %d",hist_nelts);
    t1=cx+270;
    t2=cy-80;
    c=strlen(buf);
    vdtxtsiz(&c7,&f1);
    vdtext(&unit,&c8,&c255,&c0,&c0,&t1,&t2,&c1,&c,buf);
    vdtext(&unit,&c1,&c0,&c0,&c255,&t1,&t2,&c1,&c,buf);
    sprintf(buf,"MEAN = %5.2f",u);
    t2=cy-60;
    c=strlen(buf);
    vdtext(&unit,&c8,&c255,&c0,&c0,&t1,&t2,&c1,&c,buf);
    vdtext(&unit,&c1,&c0,&c0,&c255,&t1,&t2,&c1,&c,buf);
    sprintf(buf,"SDEV = %5.2f",sdev);
    t2=cy-40;
    c=strlen(buf);
    vdtext(&unit,&c8,&c255,&c0,&c0,&t1,&t2,&c1,&c,buf);
    vdtext(&unit,&c1,&c0,&c0,&c255,&t1,&t2,&c1,&c,buf);
    hist_on=1;			/* set global flag indicating hist on   */
    vdovron(&unit);
    vdflush(&unit);
}


/*****************************************************************************/
/* erase_hist() - erase histogram from overlay plane.                        */
/*****************************************************************************/
erase_hist()
{
    short int c8=8,c512=512,t1;
    cfree(hist_bins);
    t1=plots_voff;
    vdareafi(&unit,&c8,&c0,&c0,&c0,&c1,&t1,&c512,&c512);
    vdflush(&unit);
    hist_nbins=0;
    hist_on=0;
    vdovroff(&unit);	/* turn overlay off (not really necessary) */
}
