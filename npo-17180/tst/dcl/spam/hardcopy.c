/**********************************************************************
    hardcopy.c 2.14 - Outputs spectra to LA50 printer.
**********************************************************************/
#include <stdio.h>
#include "spam.h"

#if VT240LA50
#include "plot.h"
hardcopy(buf)
char *buf;
{
    int status;
    int DUMMY,i,i1,line;
    float avg;
    float yscale,temp,yfact;
    int xoff,yoff,xpos,ypos,delta;
    int ymin,ymax;
    float xdel;
    unsigned char list[16],ans[80];
    for (i=0; i<16; i++) list[i] = 0;
/*
/*      write out up to 4 plots to the LA50
*/
    if (parse(buf,list)>0) {
/*
            parse parses the name of plots to be copied
            list[i] = 1 -- plot i will be copied
                    = 0 -- plot i will not be copied
            parse returns -1 on error, otherwise number of plots
*/
        ymin=255; ymax=0;
        Yscale(&ymin,&ymax,list);

        status = terminit();
        line = 2;
        status = setline(line);

        xaxes(img_swl,img_ewl,"WAVELENGTH(MICRONS)");
        yaxes(ymin,ymax,"DN",&yscale,plots_norm_mode);

        xdel = 500.0/(img_numchan-1);

        delta = 20;
        xoff = 100;
        yoff = 400;

        for (i=0; i<16; i++) if ((list[i] == 1)  && (line < 7)){
            status = hmove(xoff,yoff);
            status = setline(line);
            avg = (float)plots[i].sum/(float)img_numchan;
            xpos = xoff;

            for(i1=0; i1<img_numchan; i1++) {
		if (plots_norm_mode==1) temp = 1.0;
		else temp = (float)plots_norm/plots[i].sum;
		yfact = 380./(plots_maxy-plots_miny);
		ypos = 400-yfact*(temp*plots[i].data[i1]-plots_miny)+0.5;
                xpos = xoff + (xdel*i1);
                status = vector(xpos,ypos);
            }
/*
                line identification mark
*/
            status = hmove(600, delta);
            status = vector(630, delta);
            status = text(640, delta-5, plots[i].name);

            delta+= 20;
            line++;
        }

        status = copy(); 
        status = termend();
    }
/*
/*      write histogram out to the LA50
*/
    if (hist_on) {
        status = terminit();
        line = 2;
        status = setline(line);
        display_hist();
        status = copy();
        status = termend();
    }
}


/*************************************************************************
    Yscale - get min and max value for plots to be printed.
*************************************************************************/
Yscale(ymin,ymax,list)
int *ymin, *ymax;
unsigned char *list;
{
        int i,avg,diff1,diff2;

        for (i=0; i<16; i++)
        if (list[i] == 1) 
        if (plots_norm_mode == 1) {
                if (plots[i].maxdn > *ymax) *ymax = plots[i].maxdn;
                if (plots[i].mindn < *ymin) *ymin = plots[i].mindn;
        	}
        else {
                avg = (float)plots[i].sum/(float)img_numchan;
                diff1 = plots[i].maxdn - avg;
                diff2 = plots[i].mindn - avg;
                if (diff1 > *ymax) *ymax = diff1;
                if (diff2 < *ymin) *ymin = diff2;
        	}

	if (plots_norm_mode!=1) {
	    if (-diff2>diff1) *ymax = -diff2;
	    if (-diff1<diff2) *ymin = -diff1;
	}

        *ymax = *ymax + 5;
        *ymin = *ymin - 5;

	if (plots_norm_mode == 1) {
		if(*ymax > 255) *ymax = 255;
		if (*ymin < 0 ) *ymin = 0;
		}
}

/****************************************************************************
    terminit - initialize the Rainbow
****************************************************************************/
terminit(dummy)
int dummy;
{
        esc = 27;

        hdr[0] = esc;
        hdr[1] = 'P';
        hdr[2] = 'p';
        hdr[3] = '\0';

        trlr[0] = esc;
        trlr[1] = '\\';
        trlr[2] = '\0';

        fprintf(stdout,"%sS(E)%s",hdr,trlr);
        return(1);
}


/**************************************************************************
    setline - sets a line characteristics for vector drawing
**************************************************************************/
setline(option)
int option;
{
        int i;

        if (option >= 7) return(0);
        else  fprintf(stdout,"%sW(P%1d)%s",hdr,option-1,trlr);
        return(1);
}
 

/**************************************************************************
    termend - end use of graphics
**************************************************************************/
termend()
{
        int i;
        fprintf(stdout,"%sS(E)%s",hdr,trlr);
        return(1);
}


/**************************************************************************
    copy - copy VT240/Rainbow screen to the LA50
**************************************************************************/
copy()
{
        fprintf(stdout,"%sS(H)%s",hdr,trlr); 
        return(1);
}


/**************************************************************************
    vector - draw a single vector from the current point to xb,yb
**************************************************************************/
vector(xb,yb)
int xb,yb;
{
        iand(&xb,&yb);

        fprintf(stdout,"%sV[%3d,%3d]%s",hdr,xb,yb,trlr);
        return(1);
}


/**************************************************************************
    hmove - set cursor position for graph
**************************************************************************/
hmove(xb,yb)
int xb,yb;
{
        iand(&xb,&yb);

        fprintf(stdout,"%sp[%3d,%3d]%s", hdr,xb,yb,trlr);
        return(1);
}


/****************************************************************************
    iand
****************************************************************************/
iand(xb,yb)
int *xb, *yb;
{
        if (*yb & 1) *yb--;
        if (*xb & 1) *xb--;
}


/****************************************************************************
    xaxes - write horizontal axes with scale
****************************************************************************/
xaxes(start,end,header)
float start,end;
char *header;
{
        int xpos,ypos,status,i;
        char string[5];
        int xoff,yoff;

        xoff = 100;
        yoff = 400;

        status = hmove(xoff, yoff);
        status = vector(xoff+500, yoff);

        for(i=0; i<=8; i++) {
		xpos = xoff+i*62.5;
        	status = hmove(xpos,yoff);
        	status = vector(xpos,yoff+10);

        	sprintf(string,"%4.2f",start+i*(end-start)/8);
        	status = text(xpos-10,yoff+20,string);
        }

        status = text(xoff+180, yoff+50, header);
}


/***************************************************************************
    yaxes - write vertical axes with scale
        if mode = 1 -- yaxes label will be plotted
        else no label will be plotted
***************************************************************************/
yaxes(start,end,header,scale,mode)
int start,end,mode;
char *header;
float *scale;
{
        int i,status,nincr;
        char string[4];
        int xoff, yoff, ypos;
	float pincr;

        xoff = 100; yoff = 400;
        status = hmove(xoff, yoff);
        status = vector(xoff, yoff-380);

        *scale = 380./(end-start);

        nincr = (end-start)/10;
        pincr = 380/((float)(end-start)/nincr);
        for(i=0; i<=(end-start)/nincr; i++) {
		ypos = 20+pincr*i+0.5;
                status = hmove(xoff-10,ypos);
                status = vector(xoff,ypos);
                if (mode == 1) {
                        sprintf(string,"%3d",end-i*nincr);
                        status = text(xoff-50,ypos-5,string);
                }
        }

        if (mode == 1) status = text(xoff-80,yoff-200,header);
}


/**********************************************************************/
/* parse the plot names to be copied                                  */
/**********************************************************************/
parse(buf,list)
unsigned char *buf, *list;
{
    int icount,i,j,number=0;
    char plot[16*20];
    char locbuf[32];
    static struct parameter kwds[]={
        {"plotname",2,0,-4,
            "name(s) of individual plot(s) to copy (default is none) ",
            "",20,0},
        };

    kwds[0].value = plot;
    kwds[0].input_count = &icount;

    if (par(buf,kwds,1)) return(-1);

    if (plot[0]!='\0') for (j=0;j<icount;j++) {
        strcpy(locbuf,plot+j*20);
        get_plot_num(locbuf,0,&i);
        if (i<0) printf("Plot \"%s\" doesn't exist.  ",locbuf);
        else {
            list[i] = 1;
            number++;
        }
    }
    else for (j=0; j<16; j++) if (plots[j].name != 0) {
        list[j] = 1;
        number++;
    }
    return(number);
} 


/***************************************************************************
    text - write text at given coordinates
***************************************************************************/
text(xb,yb,string)
int xb,yb;
char *string;
{
        char pos[7];
        int length;
        int x,y;

        x=xb;
        y=yb;

        if(y && 1) y=y-1;
        if(x && 1) x=x-1;

        sprintf(pos, "%3d,%3d",x,y);
        fprintf(stdout,"%sp[%7s]%s", hdr,pos,trlr);

        length = strlen(string);

        if(length < 1) printf("string length",length);
        fprintf(stdout,"%st'%s'%s",hdr,string,trlr);
        
        return(1);
}


/*****************************************************************************
    display_hist - similar to draw_hist; draws current histogram on screen
*****************************************************************************/
display_hist()
{
    unsigned char graph[256],buf[80];
    long int maxval;
    int i,j,k,l,binwid,number_spacing;
    int cx=45,cy=300,c;           /* line, samp of lower-left hist corner.  */
    float u,sdev;
    double sqrt();
    if (hist_nbins==0) return;	/* no histogram data to graph.            */
/*
/* draw horizontal axis with tick marks and scale.  Binwid = width of hist bar
/* in pixels.  Number_spacing is the number of bins per scale value, always
/* a multiple of 5, unless number of bins is less than 10.
*/
    binwid = 2*max(258/hist_nbins,1);
    hmove(cx,cy);
    vector(cx+hist_nbins*binwid-1,cy);
    if (hist_nbins<=15) number_spacing=1;
    else number_spacing = max(hist_nbins/32,1)*5;
    for (i=0;i<hist_nbins;i++)
	if (i/max(hist_nbins/25,1)*max(hist_nbins/25,1)==i) {
        hmove(cx+i*binwid+binwid/2,cy);
        vector(cx+i*binwid+binwid/2,cy+2);
        if (i/number_spacing*number_spacing==i) {	/* label tick mark. */
	    sprintf(buf,"%d",i*hist_scale);
            c=strlen(buf);
	    text(cx+i*binwid+binwid/2-4*c,cy+15,buf);
	}
    }
/*
/* get max value and normalize for 180-pixel maximum bar height
*/
    maxval=0;                              /* get maximum value for norm    */
    for (i=0;i<hist_nbins;i++) if (hist_bins[i]>maxval) maxval=hist_bins[i];
    if (maxval==0) return;               /* all-zero histogram -> return  */
    for (i=0;i<hist_nbins;i++) graph[i] = (180.*hist_bins[i])/maxval+.5;
/*
/* draw histogram bars.
*/
    for (i=0;i<hist_nbins;i++) for (j=0;j<binwid;j++) if (graph[i]!=0) {
        hmove(cx+i*binwid+j,cy);
        vector(cx+i*binwid+j,cy-graph[i]);
    }
/*
/* write out statistical information.
*/
    u=hist_sum/hist_nelts;
    sdev = sqrt((double)(hist_sum2 - 2*u*hist_sum + hist_nelts*u*u) / 
        (hist_nelts-1.));
    sprintf(buf,"N = %d",hist_nelts);
    text(580,180,buf);
    sprintf(buf,"MEAN = %5.2f",u);
    text(580,210,buf);
    sprintf(buf,"SDEV = %5.2f",sdev);
    text(580,240,buf);
}
#else
hardcopy()
{
    printf("Hardcopy is not available on this machine.\n");
}
#endif
