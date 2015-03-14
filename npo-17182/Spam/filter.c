/**************************************************************************
   filter.c 2.15 - perform box filter in spectral direction on image
**************************************************************************/
#include <stdio.h>
#include "spam.h"
filter(buf)
unsigned char *buf;
{
    int window,trifilt;
    int i,j,k,l,sum;
    char buf2[5];
    short int t1,t2,c,c18=18,c10=10,c255=255;
    register unsigned char *p,*q,*line;
    register int halfwin;
    unsigned char *malloc(),triangle[10],*divlut;
    float **wgt,triarea,weight,f1=1.0;
    unsigned char *calloc();
    static struct parameter kwds[]={
        {"window",0,1,1,"window width","",0,0},
        {"triangle",3,0,0,"triangle filter","",10,0},
        };
    if (img_in_core==0) {
        printf("No image selected.  For help, type \"help\".\n");
        return;
    }
    kwds[0].value = (char *) &window;
    kwds[1].value = (char *) triangle;
    if (par(buf,kwds,2)) return;
    while (window<=1 || window/2*2==window) {
        if (window<=1) printf("Window size is too small.");
        else printf("Window size should be an odd number.");
        query("  Window size? ",buf);
        sscanf(buf,"%d",&window);
    }
    if (strcmp(triangle,"found")==0) trifilt=1;
    else trifilt=0;
    printf("One moment please . . .\n");
/*
/*      do box (or triangle) filter
*/
    halfwin = window/2;
    line = malloc(img_numchan+window-1);
    if (line==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    if (trifilt) {
	wgt = (float **)calloc(window,4);
	if (wgt==NULL) {
	    printf("Insufficient memory.\n");
	    return;
	}
	for (i=0;i<window;i++) {
	    wgt[i] = (float *)calloc(256,4);
	    if (wgt[i]==NULL) break;
	}
	if (i!=window) {
	    printf("Insufficient memory.\n");
	    return;
	}
	for (triarea=0.0,i=0;i<halfwin;i++) {
	    weight = 0.5+0.5*i/halfwin;
	    for (j=0;j<256;j++) *(wgt[i]+j) = *(wgt[window-1-i]+j) = weight*j;
	    triarea += 2*weight;
	}
	for (j=0;j<256;j++) *(wgt[halfwin]+j) = j;
	triarea += 1.0;
	divlut = malloc((int)(triarea*255+0.5));
	if (divlut==NULL) {
	    printf("Insufficient memory.\n");
	    return;
	}
	for (i=(int)(triarea*255+0.5)-1;i>=0;i--) *(divlut+i) = i/triarea+0.5;
    }
    else {
	divlut = malloc(window*255+1);
	if (divlut==NULL) {
	    printf("Insufficient memory.\n");
	    return;
	}
	for (i=window*255;i>=0;i--) *(divlut+i) = i/window;
    }
    p = img_data;
    for (j=0;j<img_nl;j++) {
        for (i=0;i<img_bw;i++) {
            q = line + halfwin;
            for (k=0;k<img_numchan;k++) {
                *q++ = *p;
                p+=img_bw;
            }
            p -= img_ns;
            for (k=0;k<halfwin;k++) {   /* reflect at endpoints */
                *(line+halfwin-k-1) = *(line+halfwin+k+1);
                *(line+img_numchan+halfwin+k) = *(line+img_numchan+halfwin-2-k);
            }
	    if (!trifilt) {	/* box filter */
                for (sum=0,k=0;k<window;k++) sum+= *(line+k);
                for (k=0;k<img_numchan;k++) {
                    *p = *(divlut+sum);
                    sum = sum - *(line+k) + *(line+k+window);
                    p+=img_bw;
                }
	    }
	    else {	/* triangle filter */
		for (k=0;k<img_numchan;k++) {
		    for (sum=0,l=0;l<window;l++) sum+= *(wgt[l] + *(line+k+l));
		    *p = *(divlut+sum);
		    p+=img_bw;
		}
	    }
            p -= img_ns;
            p++;
        }
        p+=img_ns-img_bw;
    }
    free(line);
    if (trifilt) cfree(wgt);
    free(divlut);
/*
/*      redraw current band(s) and averaging boxes
*/
    redraw();
    if (display_mode==0) {
        t1=40+(img_bw-32);  /* redraw band number (may be preceded by ratio) */
        vdtxtsiz(&c10,&f1);
        sprintf(buf2,"%d",disp_band);
        c=strlen(buf2);
        vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&c18,&c1,&c,buf2);
	draw_plots();
    }
    img_modified = 1;
}
