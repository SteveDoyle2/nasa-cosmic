#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "spam.h"

/***********************************************************************
                merge.c 2.15 - Cluster merge routine
***********************************************************************/
merge(buf)
unsigned char *buf;
{
    int i,l,status,class[32],ccount,from_class,to_class,allocerr=0,nclass;
    int prompt=0,empty[16];
    unsigned char not_done,merge_count,ibuf[80],name[32],*listi,*malloc();
    static struct parameter kwds[]={
        {"classes",0,0,-32,"from-to pairs","",0,0}
    };

    if (display_mode!=0) {
        printf("Merges must be done in graphics mode.  ");
        printf("Type \"return\" to exit.\n");
        return;
    }
    if (img_in_core==0) {
        printf("You don't have an image.  For help, type \"help\".\n");
        return;
    }
    kwds[0].value = (char *)class;
    kwds[0].input_count = &ccount;
    if (par(buf,kwds,1)) return;
    if (ccount/2*2 != ccount) {
        printf("Odd number of class numbers.  Please specify set of pairs.\n");
        return;
    }
    if (ccount==0) prompt=1;
    listi = malloc(img_bw*img_nl);
    if (listi==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    status = get_list(listi,0,"cluster.map"); 
    if (status == -1) {
        free(listi);
        return;
    }
    for (i=0;i<16;i++) {
        spectra[i].data = malloc(img_numchan);
        spectra[i].encoding = malloc((img_numchan+7)/4);
        if (spectra[i].data==NULL || spectra[i].encoding==NULL) allocerr=1;
    }
    if (!allocerr) nclass = get_plot("cluster.spc",0);
    if (allocerr || nclass==-1) {
        free(listi);
        if (allocerr) printf("Insufficient memory.\n");
        for (i=0;i<16;i++) {
            if (spectra[i].data!=NULL) free(spectra[i].data);
            if (spectra[i].encoding!=NULL) free(spectra[i].encoding);
        }
        return;
    }

    merge_count = 0;
    for (i=0;i<nclass;i++) empty[i]=0;

    for (i=0;i<ccount || prompt;i+=2) {
        if (!prompt) {
            from_class = class[i];
            to_class = class[i+1];
        }
        else {
            query("From class, to class (RETURN to stop)? ",ibuf);
            for (l=0;l<strlen(ibuf);l++) if (ispunct(ibuf[l])) ibuf[l]=' ';
            sscanf(ibuf,"%d %d",&from_class,&to_class);
        }
        if (prompt && ibuf[0]=='\0') break;
        from_class--;
        to_class--;
        if (from_class<0 ||from_class>=nclass ||to_class<0 ||to_class>=nclass) {
            printf("Class number is illegal for current clustering.  ");
            printf("Ignored.\n");
        }
        else if (from_class==to_class) ;
        else {
            change_class(listi,from_class,to_class,img_nl*img_bw);
            if (plots[from_class].name!=0) erase_plot(from_class);
            if (avg_spec(listi,to_class,img_nl*img_bw,0)==-1) return;
            sprintf(name,"Class%d",to_class+1);
            save_plot_data(name,spectra[to_class].data,4,1,1,1,1,to_class,0);
            if (prompt) draw_plots();
            image_draw(listi,to_class);
            merge_count += 1;
            empty[to_class] = 0;
            empty[from_class] = 1;
        }
    }
    if (merge_count > 0) {
        while (empty[nclass-1]) nclass--;
        for (i=0;i<nclass;i++) if (empty[i]) {
            change_class(listi,nclass-1,i,img_nl*img_bw);
            if (avg_spec(listi,i,img_nl*img_bw,0)==-1) return;
            if (plots[nclass-1].name!=0) erase_plot(nclass-1);
            sprintf(name,"Class%d",i+1);
            save_plot_data(name,spectra[i].data,4,1,1,1,1,i,0);
            image_draw(listi,i);
            nclass--;
            empty[i]=0;
            while (empty[nclass-1]) nclass--;
        }
        draw_plots();
        status = put_list(listi,"cluster.map");
        if (status!=-1) put_plot(nclass,"cluster.spc");
    }
    free(listi);
    for (i=0;i<16;i++) {
        free(spectra[i].data);
        free(spectra[i].encoding);
    }
}
