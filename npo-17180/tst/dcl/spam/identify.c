/************************************************************************
    %M% %I% - Attempts to match spectral classes against the library.
************************************************************************/
#include <stdio.h>
#include "spam.h"

identify(buf)
unsigned char *buf;
{
    int count[16];
    int i,class,th_amp,th_slope,th1,th2,l1,l2,l3,k1,tcount;
    int nread,entry_max,index,allocerr=0,bands_used;
    int match[16][17],thresh[2],break_flag,mask[20][2],mcount;
    unsigned char ibuf[80],*talloc(),*bitmask;
    char name[20];
    static struct parameter kwds[]={
        {"thresholds",0,0,2,
            "amp and slope thresholds (defaults are program-selected)","",80,0},
        {"mask",0,0,-40,"band ranges to mask (1st-2nd,...)","",0,0},
        };
    if (display_mode!=0) {
        printf("You must be in graphics mode to use identify.  ");
        printf("Use \"return\".\n");
        return;
    }
    if (img_in_core==0) {
        printf("You must have an image before using identify.  For help, ");
        printf("type \"help\".\n");
	return;
    }
    kwds[0].value = (char *)thresh;
    kwds[0].input_count = &tcount;
    kwds[1].value = (char *)mask;
    kwds[1].input_count = &mcount;
    if (par(buf,kwds,2)) return;
    for (i=0;i<16;i++) {
        spectra[i].data = talloc(img_numchan);
        spectra[i].encoding = talloc((img_numchan+7)/4);
        if (spectra[i].data==NULL || spectra[i].encoding==NULL) {
            tfree(1);
            return;
        }
    }
    class = get_plot("cluster.spc",1);
    if (class==-1) {
        tfree(0);
        return;
    }
    bitmask = talloc(img_numchan);
    if (bitmask==NULL) {
        tfree(1);
        return;
    }
    if ((bands_used=makemask(mask,mcount,bitmask))==-1) {
        tfree(0);
        return;
    }
    if (tcount!=0 && (thresh[0]<0 || thresh[1]<0 || thresh[0]>img_numchan ||
            thresh[1]>img_numchan)) {
        printf("Threshold value is bad.  Using program-selected thresholds.\n");
        tcount=0;
    }
    if (tcount!=0) {
        th_amp = thresh[0];
        th_slope = thresh[1];
    }
    else {
        th_amp = img_numchan/10*bands_used/img_numchan;
        th_slope = img_numchan/5*bands_used/img_numchan;
        printf("Using thresholds %d (amp) and %d (slope).\n",th_amp,th_slope);
    }

    entry_max = 16;
    for (l1=0; l1<class; l1++) count[l1]=0;

    for (l2=0; l2<class; l2++) {
        break_flag=0;
        for (nread=1; nread <= master_lib[0].lib_length &&
                master_lib[nread].encoding!=0; nread++) {
            plot_dist(spectra[l2].encoding,master_lib[nread].encoding,&th1,
                &th2,bitmask);

            if(th1 <=th_amp && th2 <=th_slope) {
                count[l2] += 1;
                if (count[l2] > entry_max) {
                    printf("Too many possibilities for class %d.  Use ",l2+1);
                    printf("lower threshold.\n");
                    break_flag=1;
                }
                else match[l2][count[l2]-1] = nread;
            }
            if (break_flag) break;
        }
    }

    for (l2=0; l2<class; l2++) {
        break_flag=0;
        for (nread=1; nread <= user_lib[0].lib_length &&
                user_lib[nread].encoding!=0; nread++) {
            plot_dist(spectra[l2].encoding,user_lib[nread].encoding,&th1,
                &th2,bitmask);

            if(th1 <=th_amp && th2 <=th_slope) {
                count[l2] += 1;
                if (count[l2] > entry_max) {
                    printf("Too many possibilities for class %d.  Use ",l2+1);
                    printf("lower threshold.\n");
                    break_flag=1;
                }
                else match[l2][count[l2]-1] = nread+1000;
            }
            if (break_flag) break;
        }
    }

    for (l1=0; l1<class; l1++) if (count[l1] > 0) {
        printf("\n Candidate minerals for class[%3d] are \n",l1+1);
        l3 = count[l1];
        for (l2=0; l2<l3 && l2 < entry_max; l2++){
            index = match[l1][l2];
            if (index<1000)
                printf("%5d : %s\n",l2+1,master_lib[index].name);
            else printf("%5d : %s\n",l2+1,user_lib[index-1000].name);
        }
        match[l1][l2] = -1;
    }
    tfree(0);
}
