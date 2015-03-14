/****************************************************************************
   disp.c 2.15 - band display command
   command displays single user-specified band to left of graphics, or two
   or more (16 or fewer) side by side without graphics display.  Graphics
   are restored in the latter case with the "return" command.
****************************************************************************/
#include <stdio.h>
#include "spam.h"
disp(buf)
unsigned char *buf;
{
    long int i;
    int icount,chan[2],start,end,band;
    short int c,c10=10,c18=18,c40=40,c23=23,c72=72,c255=255,t1,t2;
    short int c512=512;
    float f1=1.;
    char logical_zero=0;
    unsigned char *calloc(),*base;
    char buf2[80];
    static struct parameter kwds[]={
        {"bands",0,0,-2,"band(s) to display","",0,0},
        };
    if (img_in_core==0) {
        printf("No image has been selected.\n");
        return;
    }
    kwds[0].value = (char *) chan;
    kwds[0].input_count = &icount;
    if (par(buf,kwds,1)) return;        /* get bands, if specified.        */
    if (icount==0) {			/* disp chan[0] thru chan[1].      */
        chan[0]=1;
        chan[1]=min(img_numchan,512/img_bw);
    }
    else if (icount==1) {			/* one band range          */
        chan[0]-=img_sb-1;
        chan[1]=chan[0];
    }
    else {
        chan[0]-=img_sb-1;			/* allow for subimages     */
        chan[1]-=img_sb-1;
    }
    if (chan[0]<1 || chan[1]<1) {		/* error checking.         */
	printf("Band number is too small for image.\n");
	return;
    }
    if (chan[0]>img_numchan || chan[1]>img_numchan) {
        printf("Band number is too large for image.\n");
        return;
    }
    if (chan[0]>chan[1]) {
	printf("Second band number is smaller than first.\n");
	return;
    }
    if ((chan[1]-chan[0]+1)*img_bw > 512) {
        printf("Too many bands.  You may only display %d",
            min(img_numchan,512/img_bw));
        printf(" bands at one time for this image.\n");
        return;
    }
    if (chan[0]==chan[1]) {		/* write single band with #        */
        if (display_mode==1) {
            printf("No single-band displays in display mode.  ");
            printf("Use the \"return\" command to exit.\n");
            return;
        }
	extract(chan[0],chan[0],img_currband);	/* get band into memory.     */
        t1=img_bw;			/* display band to left of graphics. */
        t2=img_nl;
        i=img_nl*img_bw;
        vdareawr(&unit,&c4,&i,img_currband,&c1,&c1,&t1,&t2);
        disp_band=chan[0];			/* save current band number, */
        t1=40+(img_bw-32);
        t2=t1+40;
        vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&c1,&t2,&c23);	/* erase     */
        vdtxtsiz(&c10,&f1);					/* last one, */
        sprintf(buf2,"%-3d",chan[0]+img_sb-1);		/* and write new.    */
        c=strlen(buf2);
        vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&c18,&c1,&c,buf2);
        draw_plots();                           /* replace averaging boxes.  */
    }
    else /* multi-band display without graphics */ {
        start = chan[0];			/* store start and end band  */
        end = chan[1];				/* numbers.                  */
        vdareafi(&unit,&c2,&c0,&c0,&c0,&c1,&c1,&c512,&c512);	/* clear grn */
        if (display_mode==0) vddispln(&unit,&c2);               /* disp grn. */
        display_mode=1;				/* set for display mode.     */
	base=calloc((end-start+1)*img_bw*img_nl/4+1,4);	/* allocate image    */
        if (base==NULL) {				/* memory.           */
	    printf("Insufficient memory.\n");
	    return;
	}
        extract(start,end,base);		/* get requested bands into  */
        t1=img_bw*(end-start+1);		/* allocated memory.         */
        t2=img_nl;
        i=t1*t2;
        vdareawr(&unit,&c2,&i,base,&c1,&c1,&t1,&t2);	/* write to grn pln. */
	cfree(base);					/* dealloc memory.   */
	disp_startband=start;		/* save bands currently displayed    */
	disp_endband=end;		/* for disp mode hists, stretches.   */
    }
    vdflush(&unit);
}
