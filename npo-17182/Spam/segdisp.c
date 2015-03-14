/***********************************************************************
           segdisp.c 2.15 - Display routines for segmentation
***********************************************************************/
#include <stdio.h>
#include "spam.h"
segdisp(buf)
unsigned char *buf;
{
    int i,class,ccount;
    char mapkwd[10],speckwd[10],name[20],ans[80];
    int classes[16],include[16];
    unsigned char *listi,*talloc();
    static struct parameter kwds[]={
        {"maponly",3,0,0,"display cluster map only","",0,0},
        {"spectraonly",3,0,0,"display cluster spectra only","",0,0},
        {"classes",0,0,-16,"spectral classes to display","",0,0},
    };
    if (img_in_core==0) {
        printf("You must get an image before using the segdisp command.  ");
        printf("Type \"help\" for help.\n");
        return;
    }
    if (display_mode!=0) {
        printf("You must be in graphics mode to use segdisp.  Use the ");
        printf("\"return\" command.\n");
        return;
    }
    listi = talloc(img_bw*img_nl);
    if (listi==NULL) {
        tfree(1);
        return;
    }
    if (get_list(listi,0,"cluster.map")==-1) {
        tfree(0);
        return;
    }
    for (i=0;i<16;i++) {
        spectra[i].data = talloc(img_numchan);
        spectra[i].encoding = talloc((img_numchan+7)/4);
        if (spectra[i].data==NULL || spectra[i].encoding==NULL) {
            tfree(1);
            return;
        }
    }
    class = get_plot("cluster.spc",0);
    if (class==-1) {
        tfree(0);
        return;
    }
    kwds[0].value = mapkwd;
    kwds[1].value = speckwd;
    kwds[2].value = (char *)classes;
    kwds[2].input_count = &ccount;
    if (par(buf,kwds,3)) return;
    if (mapkwd[0]=='\0' && speckwd[0]=='\0') {
        strcpy(mapkwd,"found");		/* default is both */
        strcpy(speckwd,"found");
    }
    if (plots_num!=0 && speckwd[0]!='\0') {
        query("This will erase your current plots.  Are you sure? ",ans);
        if (ans[0]=='n' || ans[0]=='N') {
            tfree(0);
            printf("Type \"help saveplot\" for info on saving plots in a ");
            printf("local library.\n");
            return;
        }
        for (i=0;i<16;i++) if (plots[i].name!=0) erase_plot(i);
    }
    for (i=0;i<16;i++) include[i]=0;
    if (ccount!=0) {
        for (i=0;i<ccount;i++) {
            if (classes[i]<1 || classes[i]>class) {
                printf("Class numbers must be between 1 and %d, ",class);
                printf("inclusive, for this image.\nIgnoring illegal value.\n");
            }
            else include[classes[i]-1]=1;
        }
    }
    else for (i=0;i<class;i++) include[i]=1;
    if (mapkwd[0]!='\0') {
        if (ccount==0) image_draw(listi,-1);
        else for (i=0;i<16;i++) if (include[i]) image_draw(listi,i);
    }
    if (speckwd[0]!='\0') {
        for (i=0;i<16;i++) if (include[i]) {
            sprintf(name,"Class%d",i+1); 
            save_plot_data(name,spectra[i].data,4,1,1,1,1,i,0);
        }
        draw_plots();
    }
    vdflush(&unit);
    tfree(0);
    return;
}
