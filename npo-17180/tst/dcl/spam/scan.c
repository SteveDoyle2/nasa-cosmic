/****************************************************************************
   scan.c 2.14 - movie scan function
   The scan command accepts a range of bands and displays them in rapid
   succession.  This enables the user to quickly identify areas of high
   spectral variability. 
****************************************************************************/
#include <stdio.h>
#include "spam.h"
scan(buf)
unsigned char *buf;
{
    long int i;
    int chan[2],speed,done,start,end,band,k,bands_per_plane;
    int num_horiz,num_vert,number_flag;
    short int t1,t2,t3,t4,t5,c8=8,c40=40,c10=10,c,c42=42;
    short int c255=255,c512=512,t,c26=26,c72=72;
    short int c18=18;
    short int ss,sl,es,el;
    char logical_zero=0,number[80];
    float f1=1.;
    int *j;
    unsigned char *base,*calloc();
    long int stime,timenow();
    int num_bands;
    unsigned char buf2[10];
    static struct parameter kwds[]={
        {"bands",0,0,2,"bands to scan (default is as many as possible) ","",
            0,0},
        {"speed",0,0,1,"scan interval in ms (default is 250 ms) ","",0,0},
        {"number",3,0,0,"display band numbers (default is none) ","",80,0}
        };
    if (img_in_core==0) {
	printf("No input file has been selected.\n");
	return;
    }
    if (display_mode==1) {
	printf("No scans in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
	return;
    }
    bands_per_plane = (512/img_bw) * (512/img_nl);
    done = 0;
    kwds[0].value = (char *) chan;
    kwds[1].value = (char *) &speed;
    kwds[2].value = number;
    chan[0]=img_sb;
    chan[1]=img_sb+min(img_numchan,3*bands_per_plane)-1;
    speed=250;				/* default speed is 250 ms per band. */
    if (par(buf,kwds,3)) return;	/* get any values for range, speed.  */
    chan[0]-=img_sb-1;
    chan[1]-=img_sb-1;
    if (chan[0]<1 || chan[1]<1) {
	printf("Starting or ending band number is too small for image.\n");
	return;
    }
    if (chan[0]>img_numchan || chan[1]>img_numchan) {
	printf("Starting or ending band number is too large for the image.\n");
	return;
    }
    if (chan[0]>=chan[1]) {
	printf("Starting band number is greater than or equal to ");
	printf("ending band number.\n");
	return;
    }
    if (chan[1]-chan[0]+1 > 3*bands_per_plane) {
	printf("Too many bands selected.  You may scan only %d",
            min(img_numchan,3*bands_per_plane));
        printf(" bands for this image.\n");
        return;
    }
    base=calloc(128*img_nl,4);			/* allocate image memory for  */
    if (base==NULL) {				/* 1 plane.   This is used to */
	printf("Insufficient memory.\n");	/* fill red and green planes  */
	return;					/* 1 at a time with extract() */
    }
    printf("Scanning bands %d through %d.\n",chan[0]+img_sb-1,chan[1]+img_sb-1);
    printf("To exit the scan function, press button 4 on the cursor.\n");
    printf("One moment, please . . .\n");
    start = chan[0];
    end = chan[1];
    num_bands = end-start+1;
    number_flag=0;
    if (number[0]=='\0') ;
    else if (end>99 && img_bw<18)
        printf("Bands are too narrow to permit band number display.\n");
    else if (end>9 && img_bw<12)
        printf("Bands are too narrow to permit band number display.\n");
    else number_flag=1;
/*
/*  load channels to display into rastertech, first set to green plane, next
/*  to blue plane, remainder to red plane.  (first cover what we're doing
/*  using black overlay plane.)
*/
    vdareafi(&unit,&c8,&c1,&c1,&c1,&c1,&c1,&c512,&c512);
    vdovron(&unit);
    num_horiz = 512/img_bw;
    num_vert = 512/img_nl;
    if (number_flag) vdtxtsiz(&c7,&f1);
    for (k=0;k<=num_bands/num_horiz;k++) {
        extract(start+k*num_horiz,min(start+(k+1)*num_horiz-1,end),base);
        t=img_bw*min(num_horiz,num_bands-k*num_horiz);
        i=t*img_nl;
        if (k<num_vert) t1=k*img_nl+1;
        else if (k<2*num_vert) t1=(k-num_vert)*img_nl+1;
        else t1=(k-2*num_vert)*img_nl+1;
        t2=t1+img_nl-1;
        if (i>0) {
            if (k<num_vert) vdareawr(&unit,&c2,&i,base,&c1,&t1,&t,&t2);
            else if (k<2*num_vert) vdareawr(&unit,&c1,&i,base,&c1,&t1,&t,&t2);
            else vdareawr(&unit,&c4,&i,base,&c1,&t1,&t,&t2);
            if (number_flag) for (t5=start+k*num_horiz;
                    t5<=min(start+(k+1)*num_horiz-1,end);t5++) {
                sprintf(buf2,"%-3d",t5);
                t3=1+(t5-start-k*num_horiz)*img_bw;
                if (k<num_vert) {
                    t4=k*img_nl+10;
                    vdtext(&unit,&c2,&c0,&c5,&c0,&t3,&t4,&c1,&c3,buf2);
                }
                else if (k<2*num_vert) {
                    t4=(k-num_vert)*img_nl+10;
                    vdtext(&unit,&c1,&c0,&c0,&c5,&t3,&t4,&c1,&c3,buf2);
                }
                else {
                    t4=(k-2*num_vert)*img_nl+10;
                    vdtext(&unit,&c4,&c5,&c0,&c0,&t3,&t4,&c1,&c3,buf2);
                }
            }
        }
    }
    cfree(base);				/* free allocated memory.     */
    t1=40+(img_bw-32);
    t2=t1+40;
/*
/*  initialization
*/
    t1=img_bw;
    t2=img_nl;
    vdareafi(&unit,&c8,&c0,&c0,&c0,&c1,&c1,&t1,&t2);	 /* open o-pln hole   */
    band=1;						 /* for 1 band.       */
/* 
/*  band display loop
*/
    while (done==0) {				/* until button is pushed...  */
	stime = timenow();			/* get current time.          */
        if (band==1) {				/* for first band, set window */
		t2=1;				/* to display first band in   */
                t3=1;
		vdsetwin(&unit,&c7,&t2,&t3);	/* red plane.                 */
		vddispln(&unit,&c2);
	}
        else if (band<=bands_per_plane) {	/* for other band in red pln, */
		t2+=img_bw;			/* move display window to     */
                if (t2>513-img_bw) {
                    t2=1;
                    t3+=img_nl;
                }
                vdsetwin(&unit,&c7,&t2,&t3);	/* display correct band.      */
        }
	else if (band==bands_per_plane+1) {	/* if first bnd in green, set */
		t2=1;				/* window to display first    */
                t3=1;
		vdsetwin(&unit,&c7,&t2,&t3);	/* band in green plane.       */
		vddispln(&unit,&c1);
	}
	else if (band<=2*bands_per_plane) {	/* for other band in green    */
		t2+=img_bw;			/* plane, move display window */
                if (t2>513-img_bw) {
                    t2=1;
                    t3+=img_nl;
                }
                vdsetwin(&unit,&c7,&t2,&t3);	/* to display correct band.   */
	}
	else if (band==2*bands_per_plane+1) {	/* if first bnd in green, set */
		t2=1;				/* window to display first    */
                t3=1;
		vdsetwin(&unit,&c7,&t2,&t3);	/* band in green plane.       */
		vddispln(&unit,&c4);
	}
	else  {					/* for other band in green    */
		t2+=img_bw;			/* plane, move display window */
                if (t2>513-img_bw) {
                    t2=1;
                    t3+=img_nl;
                }
                vdsetwin(&unit,&c7,&t2,&t3);	/* to display correct band.   */
	}
	band++;					/* increment band number and  */
	if (band>num_bands) band=1;		/* reset to 1 if necessary.   */
        while (timenow()-stime < speed) ;	/* wait for time to elapse.   */
	vdswitch(&unit,&c1,&c4,&t1);		/* read button 4.             */
	done = t1;
    }						/* continue scanning . . .    */
/*
/*  reinitialize
*/
    band--;				/* band contains number of next band  */
    if (band==0) band=num_bands;	/* to display so set for current band */
    band+=start-1;			/* and store for hists, etc.          */
    disp_band=band;
    vdareafi(&unit,&c7,&c0,&c0,&c0,&c1,&c1,&c512,&c512);  /* clear rgb planes */
    vddispln(&unit,&c4);		/* redisplay red plane through luts.  */
    vdsetwin(&unit,&c7,&c1,&c1);	/* reset display window.              */
    vdareafi(&unit,&c8,&c0,&c0,&c0,&c1,&c1,&c512,&c512);    /* clear overlay  */
    vdovroff(&unit);			/* and turn it off.                   */
/*
/*      display last (now current) band in strip area.
*/
    redraw();
/*
/*      write out band number, restore plots and histogram (if any).
*/
    sprintf(buf2,"%-3d",disp_band+img_sb-1);
    vdtxtsiz(&c10,&f1);
    t1=40+(img_bw-32);
    vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&c18,&c1,&c3,buf2);
    draw_plots();
    if (hist_on) draw_hist();			/* redraw histogram, if any.  */
    vdflush(&unit);
}
