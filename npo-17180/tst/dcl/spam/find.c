/***************************************************************************
   find.c 2.14 - finds all spectra with signature similar to existing plot
   in either the image or the spectral library(s).
***************************************************************************/
#include <stdio.h>
#include "spam.h"
find(buf)
unsigned char *buf;
{
    int i,j,k,l;
    int thresh[2],mask[20][2];
    long int nbytes;
    int index,icount,hist_mode,lib_mode,mcount;
    short int t,t1,t2,r,g,b;
    unsigned char *hd,*malloc(),*bitmask,byte;
    register unsigned char *q;
    char plot[80],hist[10],lib[10];
    static struct parameter kwds[]={
        {"plotname",2,1,1,"name of plot","",40,0},
        {"threshold(s)",0,1,-2,"error threshold(s) ","",0,0},
        {"histogram",3,0,0,"Hamming dist histogram (default is no) ","",10,0},
	{"library",3,0,0,
	    "search the library(s) instead of the image (default is image) ",
	    "",10,0},
        {"mask",0,0,-40,"band ranges to mask (1st-2nd,3rd-4th,...)","",0,0},
        };
    if (display_mode==1) {
        printf("No finds in display mode.  Type \"return\" to exit.\n");
        return;
    }
    if (plots_num==0) {
	printf("There aren't any plots up.\n");
        return;
    }
    thresh[0] = 0;
    thresh[1] = img_numchan;
    kwds[0].value = plot;
    kwds[1].value = (char *) thresh;
    kwds[1].input_count = &icount;
    kwds[2].value = hist;
    kwds[3].value = lib;
    kwds[4].value = (char *)mask;
    kwds[4].input_count = &mcount;
    if (par(buf,kwds,5)) return;     /* get threshold, if any, and plotname  */
    if (lib[0]=='\0' && !img_in_core) {
	printf("No image is available for image finds.  Use \"get\".\n");
	return;
    }
/*
/*      Get index of plot in plots array.
*/
    get_plot_num(plot,0,&i);	     /* get index corresponding to plotname  */
    if (i==-1) printf("Plot \"%s\" doesn't exist.  ",plot);
    else if (i==-2) printf("The name \"%s\" is ambiguous.  ",plot);
    while (i<0) {
	query("Name of plot (RETURN for READY) ? ",plot);
        if (plot[0]=='\0') return;
        if (!check_old_name(plot)) {
	    get_plot_num(plot,0,&i);
            if (i==-1) printf("Plot \"%s\" doesn't exist.  ",plot);
            else if (i==-2) printf("The name \"%s\" is ambiguous.  ",plot);
	}
    }
/*
/*      Initialization.  Miscellaneous messages, memory allocation.
*/
    if (lib[0]=='\0') lib_mode=0;
    else lib_mode=1;
    if (hist[0]=='\0') hist_mode=0;
    else hist_mode=1;
    if (lib_mode && hist_mode) {
	printf("The hist keyword may not be used with library finds.  ");
	printf("Ignored.\n");
    }
    nbytes = img_bw*img_nl;
/*
/*      Create bit masks corresponding to specified bands (mask keyword) and
/*      store at bitmask.
*/
    bitmask = malloc(img_numchan);
    if (bitmask==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    if (makemask(mask,mcount,bitmask)==-1) {
        free(bitmask);
        return;
    }
/*
/*      allocate memory for Hamming distances
*/
    if (!lib_mode) {
        printf("One moment please . . .\n");
        hd = malloc(nbytes);
        if (hd==NULL) {
            printf("Insufficient memory.\n");
            free(bitmask);
            return;
        }
    }
/*
/*      Allocate space for (possibly double-) binary encoding of plot,
/*      encode, and then use search lib for find similar spectra in the
/*      library, or search image to find similar spectra in image and color
/*      them with color "i" using image_draw.
*/
    q=malloc((img_numchan+7)/4);		/* q -> plot encoding.       */
    if (q==NULL) {
	printf("Insufficient memory.\n");
        free(bitmask);
	if (!lib_mode) free(hd);
	return;
    }
    compact_code(plots[i].data,q,(int)((float)plots[i].sum/img_numchan+.5));
    if (lib_mode) search_lib(q,thresh[0],thresh[1],bitmask);
    else {
	if (! search_image(q,i,thresh[0],thresh[1],hd,hist_mode,bitmask))
            image_draw(hd,i);
        free(hd);				/* free distance table.      */
    }
    free(bitmask);
    free(q);
}


/************************************************************************/
/* Search the image for similar spectra                                 */
/************************************************************************/
search_image(code_plot,l1,th_amp,th_slope,hd,hist_mode,bitmask)
unsigned char *code_plot,*hd,*bitmask;
int l1,th_amp,th_slope,hist_mode;
{
	unsigned char com[80];
	int *hist_amp_dist,*hist_slope_dist,*calloc();
	int j,k,l,i,th1,th2,match;
	unsigned char *buff,*malloc();

        hist_amp_dist = calloc(img_numchan,sizeof(int));
        if (hist_amp_dist==NULL) {
             printf("Insufficient memory.\n");
             return(1);
        }
        hist_slope_dist = calloc(img_numchan,sizeof(int));
        if (hist_slope_dist==NULL) {
             printf("Insufficient memory.\n");
             cfree(hist_amp_dist);
             return(1);
        }
        buff = malloc(img_numchan);
        if (buff==NULL) {
            printf("Insufficient memory.\n");
            cfree(hist_amp_dist);
            cfree(hist_slope_dist);
            return(1);
        }
	for (l=0; l<img_numchan; l++) {
		hist_amp_dist[l] = 0;
		hist_slope_dist[l] = 0;
		}

	j=0;
	match = 0;
	
	for (l=0; l<img_nl; l++)
	for (k=0; k<img_bw; k++){

	get_code(j,buff);
	plot_dist(code_plot,buff,&th1,&th2,bitmask);

	if (th1 <= th_amp && th2 <= th_slope) {
		match++;
		hd[j++] = l1;
		hist_amp_dist[th1]++;
		hist_slope_dist[th2]++;
		}
	else	hd[j++] = l1+1;
	}

	printf ("%d samples matched with the given spectrum\n",match);

        if (hist_mode) {
	    printf ("Amplitude distance histogram\n");
	    save_hist_data(hist_amp_dist,img_numchan); 
	    draw_hist();

	    query("For slope distance histogram, press RETURN.\n",com);
	    save_hist_data(hist_slope_dist,img_numchan);
	    draw_hist();
	}
        cfree(hist_amp_dist);
        cfree(hist_slope_dist);
        free(buff);
	return(0);
}

/**************************************************************************/
/* search the library(s) for similar spectra                              */
/**************************************************************************/
search_lib(code_plot,th_amp,th_slope,bitmask)
unsigned char *code_plot,*bitmask;
int th_amp,th_slope;
{
    int th1,th2,nread;
    printf("   MINERAL NAME(index)      AMPLITUDE SLOPE\n");

    for (nread=1; nread<=master_lib[0].lib_length &&
            master_lib[nread].encoding!=0; nread++) {
    	plot_dist(code_plot,master_lib[nread].encoding,&th1,&th2,bitmask);
    	if (th1 <= th_amp && th2 <= th_slope) {
            printf("%20s(%3d)%8d%8d\n", 
	        master_lib[nread].name,nread,th1,th2);
	}
    }
    for (nread=1; nread<=user_lib[0].lib_length &&
            user_lib[nread].encoding!=0; nread++) {
	plot_dist(code_plot,user_lib[nread].encoding,&th1,&th2,bitmask);
	if (th1 <= th_amp && th2 <= th_slope) {
	    printf("%20s(%2du)%8d%8d\n", 
	        user_lib[nread].name,nread,th1,th2);
	}
    }
}
