/*********************************************************************
   hist.c 2.15 - subroutine does histograms of dn value or Hamming
   distance in display or graphics mode.
*********************************************************************/
#include <stdio.h>
#include "spam.h"
hist(buf)
unsigned char *buf;
{
    int icount,k,k1,k2,x,y,t1,t2,l1,s1,l2,s2;
    int j_cursor_ns,j_img_bw,mcount,mask[20][2];
    int error,button_number,image_area=0;
    char locbuf[80],hamm[80];
    unsigned char *calloc(),*malloc(),*bitmask;
    long int *hist;			/* pointer to accumulating table */
    register unsigned char *j1,*j2;
    register unsigned char hd;
    register long int i,j;
    register unsigned char *p1;
    int area[2];
    static struct parameter kwds[]={
        {"area",0,0,2,"area height and width (default is strip or image) ",
	    "",0,0},
        {"hammingdist",3,0,0,"use Hamming distance (default is dn)","",80,0},
        {"mask",0,0,-40,"band ranges to mask (1st-2nd,3rd-4th,...)","",0,0},
        };
    if (img_in_core==0) {
        printf("No image selected.  For help, type \"help\".\n");
        return;
    }
    kwds[0].value = (char *) area;
    kwds[0].input_count = &icount;	/* icount will = 2 if user      */
    kwds[1].value = hamm;		/*     specifies area.          */
    kwds[2].value = (char *)mask;
    kwds[2].input_count = &mcount;
    if (par(buf,kwds,3)) return;	/* get area and mode from user. */
    if (hamm[0]=='\0') hist = (int *)calloc(256,4);	/* alloc accum area.  */
    else hist = (int *)calloc(img_numchan+1,4);		/* calloc does init.  */
    if (hist==NULL) {
	printf("Insufficient memory.\n");
	return;
    }
    while (icount!=0 &&			/* check to see if area is okay. */
	    (area[0]<1 || area[1]<1 || area[0]>img_nl || area[1]>img_bw)) {
        printf("Illegal area.  ");
        if (area[0]<1 || area[0]>img_nl) {
            query("Number of lines? ",locbuf);
	    i=sscanf(locbuf,"%d",&area[0]);
            if (i!=1) area[0]=0;
        }
        if (area[1]<1 || area[1]>img_bw) {
            query("Number of samples? ",locbuf);
            i=sscanf(locbuf,"%d",&area[1]);
            if (i!=1) area[1]=0;
        }
    }
/*
/*      Create bit masks corresponding to specified bands (mask keyword) and
/*      store at bitmask.
*/
    bitmask = malloc(img_numchan);
    if (bitmask==NULL) {
        printf("Insufficient memory.\n");
        cfree(hist);
        return;
    }
    if (makemask(mask,mcount,bitmask)==-1) {
        cfree(hist);
        free(bitmask);
        return;
    }
/*
/*      set up (initialization)
*/
    if (hamm[0]!='\0' && icount==0) {		/* default area for Hammhist */
        printf("Using area of 5 lines by 5 samples.\n");
        area[0]=5;
        area[1]=5;
    }
    p1 = img_data;				/* output before dma usage.  */
    if (icount!=0 || hamm[0]!='\0') {		/* get location with cursor. */
        cursor_nl=area[0];			/* redef cursor size, shape. */
        cursor_ns=area[1];
        vdcuroff(&unit,&c1);			/* Start cursor in new size. */
        vdcuron(&unit,&c1,&c0,&c0);
        printf("Position the cursor and press button 1.\n");
        get_cursor_position(&button_number,&x,&y,image_area,0x1);
        printf("Okay.\n");
    }
    vdcuroff(&unit,&c1);    /* disable cursor immediately for user feedback. */
    printf("One moment please . . .\n");
    if (hamm[0]=='\0') {			/* dn histogram              */
        if (icount != 0) {			/* cursor-selected area      */
            if (display_mode==1) i=(x-1)/img_bw+disp_startband-1;
            else i=disp_band-1;			/* i = band #, 0<=i<numchan  */
            if (display_mode==1) x=(x-1)%img_bw+1; /* x=samp in band,>0,<=bw */
            for (j=y-cursor_nl;j<y;j++) for (k=x-cursor_ns;k<x;k++)
                hist[p1[i*img_bw+j*img_ns+k]]++;
        }
        else {					/* entire strip/image        */
            if (display_mode==0) {		/*     (entire strip)        */
                for (i=0;i<img_nl;i++) for (j=0;j<img_bw;j++)
                    hist[p1[(disp_band-1)*img_bw+img_ns*i+j]]++;
            }
            else {				/*     (entire image)        */
                i=img_nl*img_ns;
                for (j=0;j<i;j++) hist[*p1++]++;
            }
        }
        if (save_hist_data(hist,256)) return;   /* create histogram.         */
    }
    else {				        /* Hamming distance hist     */
        x=(x-1)%img_bw+1;
	j=(img_numchan+7)/8;	 	/* reg j =  # of bytes per spectrum */
        j_cursor_ns = j * cursor_ns;
	j_img_bw = j * img_bw;
/*
/* Set p1 to point to one byte
/* past last byte in table.  Then, for each pair of corresponding bytes,
/* look up Hamming distance using hamm_lut table (calculated in gen_table
/* subroutine) and adjust histogram accumulating table accordingly.
/* Note that although the histogram represents every possible pairing (and
/* some pairings twice, i.e., the same pair in both orders), not all are
/* calculated.  The variable pairs (l1,s1) and (l2,s2) may be thought of
/* as pointing to the spectra being compared; each spectrum is compared with
/* every other different spectrum only once.  The zero distances for each
/* spectra with itself are added in later.
*/
	j1=hamm_ampbits+(y-cursor_nl)*j*img_bw+(x-cursor_ns)*j;
	j2=hamm_ampbits+(y-cursor_nl)*j*img_bw+(x-cursor_ns)*j;
	for (l1=y-cursor_nl;l1<y;l1++) {
            for (s1=x-cursor_ns;s1<x;s1++) {
		l2=l1;
		j2+=j_img_bw*(l1-y+cursor_nl);
	        while (l2<y) {
		    if (l1==l2++) {
			j2+=j*(s1-x+cursor_ns+1);
			s2=s1+1;
		    }
		    else s2=x-cursor_ns;
	            while (s2<x) {
	                hd=0;
	                for (i=0;i<j;i++) hd+=
                            hamm_lut[(*j1++)&bitmask[i]][(*j2++)&bitmask[i]];
	                hist[hd]+=2;
	                j1-=j;
			s2++;
	            }
		    j2+=j_img_bw-j_cursor_ns;
	        }
	        j1+=j;
	        j2-=j_img_bw*cursor_nl;
	    }
	    j1+=j_img_bw-j_cursor_ns;
	}
        hist[0]+=cursor_ns*cursor_nl;
        if (save_hist_data(hist,img_numchan+1)) return;	/* create histogram. */
    }
    draw_hist();	/* Do the histogram.  Also restarts cursor.          */
    vdflush(&unit);
    cfree(hist);	/* free temporary storage area.                      */
    free(bitmask);
}
