/**************************************************************************
   plot.c 2.15 - plot out spectral data from user-specified band area.
**************************************************************************/
#include <stdio.h>
#include <math.h>
#include "spam.h"
plot(buf)
unsigned char *buf;
{
    unsigned char *point,*malloc();
    int i,j,k,n,sn,x,y,bad_name=1,loc[2];
    int def,plotloc_mode,error,button_number,not_done=1,image_area=0;
    float sum,sum2,u,ssum,ssum2,su,stddev;
    char name[80],variance[10],stats[10];
    unsigned char *chardata,*var_data,byte;
    int average[2],var_flag,calc_stats;
    static struct parameter kwds[]={
        {"name",2,0,1,"plot name (default is PLOTn) ","",80,0},
        {"average",0,0,2,
	    "averaging-box height and width (default is current cursor size) ",
	    "",0,0},
        {"location",0,0,2,"location in image (line, sample) ","",0,0},
        {"variance",3,0,0,"show variance for spectral averages","",10,0},
	{"stats",3,0,0,"calculate mean and std dev for each plot","",10,0},
        };
    if (img_in_core==0) {
        printf("No image selected.  For help, type \"help\".\n");
        return;
    }
    if (display_mode==1) {
        printf("No dataplots in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
        return;
    }
    if (plots_num==16) {
        printf("There aren't any colors left.  Please erase a plot.\n");
        return;
    }
    kwds[0].value = name;
    kwds[1].value = (char *) average;
    kwds[1].input_count = &def;
    kwds[2].value = (char *) loc;
    kwds[2].input_count = &plotloc_mode;
    kwds[3].value = variance;
    kwds[4].value = stats;
    if (par(buf,kwds,5)) return;      /* get area to average over, if any.  */
    if (def!=0) {
	while (average[0]<1 || average[0]>img_nl) {
	    printf("Illegal averaging area.  ");
	    query("Number of lines in averaging area? ",buf);
	    sscanf(buf,"%d",&average[0]);
        }
	while (average[1]<1 || average[1]>img_bw) {
	    printf("Illegal averaging area.  ");
	    query("Number of samples in averaging area? ",buf);
	    sscanf(buf,"%d",&average[1]);
        }
        cursor_nl=average[0];
        cursor_ns=average[1];			/* setup new cursor and     */
        vdcuroff(&unit,&c1);			/* then switch to new       */
        vdcuron(&unit,&c1,&c0,&c0);		/* definition.              */
    }
    if (plotloc_mode) {
        loc[0]-=img_sl-1;
	while (loc[0]-cursor_nl+1<1 || loc[0]>img_nl) {
	    printf("Plot location's line number is illegal for this image.  ");
	    query("Line number? ",buf);
	    sscanf(buf,"%d",&loc[0]);
            loc[0]-=img_sl-1;
        }
	while (loc[1]-cursor_ns+1<1 || loc[1]>img_bw) {
	    printf("Plot location's sample number is illegal for this image.");
	    query("  Sample number? ",buf);
	    sscanf(buf,"%d",&loc[1]);
        }
    }
    if (name[0]!='\0') {
        while (check_new_name(name)) {
	    query("New name (RETURN for READY) ? ",name);
	    if (name[0]=='\0') return;
	}
    }
    if (variance[0]=='\0') var_flag=0;
    else var_flag=1;
    if (stats[0]=='\0') calc_stats=0;
    else calc_stats=1;
    point = malloc(img_numchan);
    if (point==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    var_data = malloc(img_numchan);
    if (var_data==NULL) {
        printf("Insufficient memory.\n");
        free(point);
        return;
    }
    if (!plotloc_mode) {
        printf("For each point to be plotted, position the cursor and press\n");
        printf("button 1; to stop plotting, use button 4.\n\n");
    }
    while (not_done) {
	if (plotloc_mode) {
	    y=loc[0];
	    x=loc[1];
        }
        else {
	    get_cursor_position(&button_number,&x,&y,image_area,0x9);
            not_done=button_number!=4;
        }
        vdcuroff(&unit,&c1);
        if (plots_num==16 && not_done) {
            printf("There aren't any colors left.  Please erase a plot.\n");
            not_done=0;
        }
        if (not_done) {
            chardata = (unsigned char *)img_data;  /* pointer to image data. */
	    if (calc_stats) ssum = ssum2 = 0.0;
            for (i=0;i<img_numchan;i++) {	/* accumulate data for all   */
                sum=0.;				/* channels, averaging if    */
                sum2=0.;			/* if necessary.             */
                for (j=y-cursor_nl;j<y;j++) for (k=x-cursor_ns;k<x;k++) {
                    byte = chardata[j*img_ns+img_bw*i+k];
                    sum += byte;
                    sum2 += byte*byte;
                }
                n=cursor_ns*cursor_nl;
                point[i] = sum/n+.5;
                if (var_flag && n!=1) {
                    u=sum/n;
                    var_data[i] = sqrt((double)(sum2-2*u*sum+n*u*u)/(n-1.))+.5;
		}
                else var_data[i]=0;
		if (calc_stats) {
		    ssum += sum/n;
		    ssum2 += (sum/n)*(sum/n);
		}
            }
	    i=0;
	    if (name[0]=='\0') while (i>=0) {
	        sprintf(name,"PLOT%d",++img_generic);
                get_plot_num(name,1,&i);
	    }
            printf("Line %d   Sample %d",y+img_sl-1,x);
	    if (calc_stats) {
		su = ssum/img_numchan;
		sn = img_numchan;
		stddev = sqrt((double)((ssum2-2*su*ssum+sn*su*su)/(sn-1.)));
		printf("   mean=%-6.2f   std dev=%.2f",su,stddev);
	    }
	    printf("   (%s)\n",name);
            save_plot_data(name,point,0,x,y,cursor_nl,cursor_ns,-1,
		var_data);
	    draw_plots();	/* This also restarts the cursor tracking.   */
        }
        name[0]='\0';	/* don't use user-specified name more than once.     */
	if (plotloc_mode) not_done=0;
    }
    free(point);
    free(var_data);
    vdcuron(&unit,&c1,&c0,&c0);
}
