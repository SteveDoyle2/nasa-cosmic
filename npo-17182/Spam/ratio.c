/**************************************************************************
   ratio.c 2.15 - ratio two bands and display the result
**************************************************************************/
#include <stdio.h>
#include "spam.h"
ratio(buf)
unsigned char *buf;
{
    float *data,*calloc(),min,max;
    unsigned char *malloc(),inbuf[80];
    int *hist;
    int x,y,temp,sum,low,high;
    long int n;
    register unsigned char *p,*q;
    register float *r;
    register int i,j;
    short int t1,t2,c23=23;
    int kwd_mode,band[2],graph_area=1,button_number;
    static struct parameter kwds[]={
        {"bands",0,0,2,"bands to ratio (default is cursor selection) ","",0,0},
        };
    if (img_in_core==0) {
        printf("No image selected.  For help, type \"help\".\n");
        return;
    }
    if (display_mode==1) {
        printf("No band ratios in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
        return;
    }
    kwds[0].value = (char *) band;		/* get bands, if specified. */
    kwds[0].input_count = &kwd_mode;
    if (par(buf,kwds,1)) return;
    if (!kwd_mode) { 				/* reset cursor size and    */
        cursor_nl=1;				/* get bands from user.     */
        cursor_ns=1;
        vdcuroff(&unit,&c1);
        vdcuron(&unit,&c1,&c0,&c0);
        printf("Position the cursor in the graph area at the wavelength of\n");
        printf("the first (numerator) band and press button 1.\n");
	get_cursor_position(&button_number,&x,&y,graph_area,0x1);
        band[0] = (x-plots_hoff)*(img_numchan-1.)/plots_width+img_sb+.5;
        printf("Repeat for the second band.\n");
	get_cursor_position(&button_number,&x,&y,graph_area,0x1);
        band[1] = (x-plots_hoff)*(img_numchan-1.)/plots_width+img_sb+.5;
        printf("Okay.  One moment please . . .\n");
    }
    else {
        while (band[0]<img_sb || band[0]>img_sb+img_numchan-1) {
            printf("First band number is illegal for this image.  ");
            query("Band number? ",inbuf);
            sscanf(inbuf,"%d",&band[0]);
        }
        while (band[1]<img_sb || band[1]>img_sb+img_numchan-1) {
            printf("Second band number is illegal for this image.  ");
            query("Band number? ",inbuf);
            sscanf(inbuf,"%d",&band[1]);
        }
    }
    band[0]-=img_sb;
    band[1]-=img_sb;
    data = calloc(img_bw*img_nl,4);		/* allocate space for ratios. */
    if (data==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    hist = (int *)calloc(500,sizeof(int));
    if (hist==NULL) {
        printf("Insufficient memory.\n");
        cfree(data);
        return;
    }
    r=data-1;					/* store ratios at r (data).  */
    p = img_data + band[0]*img_bw;
    q = p+(band[1]-band[0])*img_bw;
    for (i=img_nl;i!=0;i--) {
        for (j=img_bw;j!=0;j--) {
            if (*q!=0) *++r = (float)(*p)/(*q);
            else *++r = 0.;
            if (*r<5) hist[(int)(*r *100)]++;
            p++;
            q++;
        }
        p+=img_ns-img_bw;
        q+=img_ns-img_bw;
    }
    p = img_currband;/* norm (saturate 1.5% at the ends) and fill image band. */
    r = data;
    for (sum=0,low=0;sum<.015*img_bw*img_nl && low<500;low++) sum+=hist[low];
    for (sum=0,high=499;sum<.015*img_bw*img_nl && high>=0;high--)
        sum+=hist[high];
    if (high==-1 || low==500) {
        printf("Error calculating min and max ratio.  Consult programmer.\n");
        cfree(data);
        cfree(hist);
    }
    max = (high+1)/100.;
    min = (low-1)/100.;
    for (i=0;i<img_nl*img_bw;i++) {
        temp = (*r++ -min)/(max-min)*255;
        if (temp<17) temp=17;
        else if (temp>255) temp=255;
        *p++ = temp;
    }
    t1=img_bw;
    t2=img_nl;				/* display band in normal area.       */
    n=img_nl*img_bw;
    vdareawr(&unit,&c4,&n,img_currband,&c1,&c1,&t1,&t2);
    printf("Ratio displayed is band %d / band %d.\n",band[0]+img_sb,
	band[1]+img_sb);
    t1=40+(img_bw-32);
    t2=t1+40;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&c1,&t2,&c23);	/* erase band number  */
    vdflush(&unit);
    cfree(data);
    cfree(hist);
}
