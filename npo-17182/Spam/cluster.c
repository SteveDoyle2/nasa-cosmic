#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "spam.h"

/****************************************************************************
    cluster.c 2.15 - performs clustering of spectra
****************************************************************************/
cluster(buf)
unsigned char *buf;
{
    int i,f1,l1,class,th_amp,th_slope,status,noise_level;
    int noise[2],clust[2],ncount,ccount;
    int j,k,l,k1,k2,nsamp1,allocerr=0,bands_used;
    unsigned char not_done,avg,*listi,*talloc(),*bitmask;
    int count[16];
    int auto_seg,mask[20][2],mcount;
    char com[3],ibuf[80],manmode[10];
    static struct parameter kwds[]={
        {"noisethresh",0,0,2,"noise thresholds (amp,slope) ","",0,0},
        {"clusterthresh",0,0,2,"clustering threshold (amp, slope) ","",0,0},
        {"manual",3,0,0,"interactive clustering (yes, no) ","",10,0},
        {"mask",0,0,-40,"band ranges to mask (1st-2nd,...) ","",0,0}
    };
/*
/*          see if current mode is okay for clustering
*/
    if (img_in_core==0) {
        printf("You must have an image to use cluster.  ");
        printf("Type \"help get\" for more info.\n");
        return;
    }
    if (display_mode!=0) {
        printf("You must be in graphics mode to use cluster.  ");
        printf("Use the \"return\" command.\n");
        return;
    }
    if (plots_num!=0) {
        printf("Cluster will erase current plots and image colorings.  ");
        query("Is this okay? ",ibuf);
        if (ibuf[0]=='n' || ibuf[0]=='N') {
            printf("You may use the \"saveplot\" command to save plots ");
            printf("in a user library.\n");
            printf("Type \"help saveplot\" for more info.\n");
            return;
        }
	for (i=0;i<16;i++) if (plots[i].name!=0) erase_plot(i);
	draw_plots();
    }
    redraw();
/*
/*          initialize and get user parameters
*/
    listi = talloc(img_bw*img_nl);
    if (listi==NULL) {tfree(1); return;}
    for (i=0;i<16;i++) {
        spectra[i].data = talloc(img_numchan);
        spectra[i].encoding = talloc((img_numchan+7)/4);
        if (spectra[i].data==NULL || spectra[i].encoding==NULL) {
            tfree(1);
            return;
        }
    }
    kwds[0].value = (char *) noise;
    kwds[0].input_count = &ncount;
    kwds[1].value = (char *) clust;
    kwds[1].input_count = &ccount;
    kwds[2].value = manmode;
    kwds[3].value = (char *) mask;
    kwds[3].input_count = &mcount;
    if (par(buf,kwds,4)) return;
    if (manmode[0]=='\0') auto_seg=1;
    else auto_seg=0;
    if (ncount==0) {
        noise[0]=img_numchan/3;
        noise[1]=max(noise[0]+3,img_numchan/2);
        printf("Using noise thresholds %d (amp) and %d (slope).\n",
            noise[0],noise[1]);
    }
    if (ccount!=0) {
        if (clust[0]<0||clust[0]>img_numchan||clust[1]<0||clust[1]>img_numchan){
            printf("Thresholds must be between 0 and %d.  Using ",img_numchan);
            printf("program-selected values.\n");
            ccount = 0;
        }
    }
    bitmask = talloc(img_numchan);
    if (bitmask==NULL) {tfree(1); return;}
    if ((bands_used=makemask(mask,mcount,bitmask))==-1) {tfree(0); return;}
/*
/*          init pixel map to 0's, except where pixel is noise; these get 15.
*/
    not_done=1;
    while (not_done) {
        nsamp1 = 0;
        j = 0;
        for (l=0; l<img_nl; l++)
        for (k=0; k<img_bw; k++){
            *(listi+j) = 0;
            get_zerox(j,&k1,&k2);
            if ((k1 > noise[0]) || (k2 > noise[1]))  {
                nsamp1 +=1;
                *(listi+j) = 15;
            }
            j ++;
        }
        printf("\n %d samples were examined",img_bw*img_nl);
        printf("\n %d samples were found to be feature-less\n\n",nsamp1);
        if (!auto_seg) {
            query("Would you like to respecify noise thresholds? ",ibuf);
            if (ibuf[0]=='y' || ibuf[0]=='Y') {
                query("Enter new noise threshold values (amplitude & slope) : ",
                    ibuf);
                for (i=0;i<strlen(ibuf);i++) if (ispunct(ibuf[i])) ibuf[i]=' ';
                sscanf(ibuf,"%d %d",&noise[0],&noise[1]);
            }
            else not_done=0;
        }
        else not_done=0;
    }
/*
/*          get clustering thresholds from user, if necessary, sample at regular
/*          line intervals, and get actual number of classes according to 
/*          sampling.  If iterative clustering (man option), let user recluster
/*          if necessary.
*/
    noise_level = 15;	
    not_done = 1;

    while (not_done==1) {
        if (ccount==-1) {
            query("Enter cluster threshold values (amplitude & slope) : ",
                ibuf);
            for (i=0;i<strlen(ibuf);i++) if (ispunct(ibuf[i])) ibuf[i]=' ';
            sscanf(ibuf,"%d %d",&th_amp,&th_slope);
        }
        else if (ccount==2) {
            th_amp=clust[0];
            th_slope=clust[1];
        }
        else {
            th_amp=img_numchan/5*bands_used/img_numchan;
            th_slope=img_numchan/3*bands_used/img_numchan;
            printf("Using clustering thresholds %d (amp) and %d (slope).\n",
                th_amp,th_slope);
        }

        class = 14;	/* maximum number of classes.   */
        redraw();	/* redraw current band.         */
        l1 = sample(listi,class,th_amp,th_slope,noise_level,auto_seg,bitmask);
        if (l1==-1) {tfree(0); return;}	/* allocation failure */
        class = l1;	/* sample samples areas of the image and  */
                        /* breaks into classes, ret # of classes. */
        if (auto_seg != 1) {
            printf("\nOptions : C - continue to cluster\n");
            printf("          R - try to re-sample\n");
            printf("          Q - quit\n");
            query("Option? : ",com);
            if (com[0] > 'Z') com[0] = com[0] - 32;
            if(com[0] == 'R') {
                not_done=1;
                ccount= -1;
            }
            else  if (com[0] == 'C') not_done = 0;
            else {tfree(0); return;}
        }
        else not_done = 0;
    }

    printf("One moment please...\n");
/*
/*          fill in pixel map with class for each pixel (use closest class
/*          in terms of error), assigning class "class" to those pixels
/*          which can't be grouped.
*/
    cluster_by_plot(listi,class,noise_level,th_amp,th_slope,count,bitmask);
/*
/*          fill plots array with representative (average) spectrum for
/*          each class.
*/
    for (l1=0; l1<class; l1++)
        if (avg_spec(listi,l1,count[l1],0)==-1) {tfree(0); return;}
/*
/*          if performing automatic segmentation,
/*          attempt to merge similar spectral classes and update pixel map.
/*          get new average spectra into plots array, plot spectra, and display
/*          the new pixel map in the band area.
/*              if manual cluster, draw current pixel map and plot
/*          spectra, then let the user iteratively merge and display results.
*/
    if (auto_seg == 1) {
        merge_spec(listi,th_amp,th_slope,&class,auto_seg,count,noise_level,
            bitmask);
        cluster_by_plot(listi,class,noise_level,th_amp,th_slope,count,bitmask);
        for (l1=0; l1<class; l1++)
            if (avg_spec(listi,l1,count[l1],0)==-1) {tfree(0); return;}
        peek_spec(class,count);
        image_draw(listi,-1);
    }
    else {
        image_draw(listi,-1);
        peek_spec(class,count);
loop:
   query("Do you want to try automatic merge ? (y/n) : ",com);

        if (com[0] == 'y' || com[0] == 'Y') {
            merge_spec(listi,th_amp,th_slope,&class,auto_seg,count,noise_level,
                bitmask);
            for (l1=0; l1<class; l1++)
                if (avg_spec(listi,l1,count[l1],0)==-1) {tfree(0); return;}
            image_draw(listi,-1);
/*          printf("sum=%d\n",spectra[1].sum);			*/
            peek_spec(class,count);
/*          printf("sum=%d\n",spectra[1].sum);			*/

query("Do you want to try clustering based on merged spectra? (y/n) : ",com);
            if (com[0] == 'y' || com[0] == 'Y') {		
                printf("One moment please...\n");

                for (l1=0; l1<class; l1++) {
                    avg = (float)spectra[l1].sum/(float)img_numchan + 0.5;
                    compact_code(spectra[l1].data,spectra[l1].encoding,avg);
                }

                cluster_by_plot(listi,class,noise_level,th_amp,th_slope,count,
                    bitmask);
                image_draw(listi,-1);
                for (l1=0; l1<class; l1++)
                    if (avg_spec(listi,l1,count[l1],0)==-1) {tfree(0); return;}
                peek_spec(class,count);
                goto loop;
            }
        }
    }
    status = put_list(listi,"cluster.map");
    if (status != -1) put_plot(class,"cluster.spc");
    tfree(0);
}


/**************************************************************************
       For each class, displays the average spectrum for the class.
**************************************************************************/
peek_spec(class,count)
int class;
int *count;
{
	int l1,sum;
        char name[32];

	for (l1=0;l1<16;l1++) if (plots[l1].name!=0) erase_plot(l1);
	for (l1=0; l1<class ; l1++) 
	   if (count[l1] > 0) {
		sprintf(name,"Class%d",l1+1);
		save_plot_data(name,spectra[l1].data,4,1,1,1,1,l1,0);
		printf("    Class %2d : %4d\n",l1+1, count[l1]);
		}
        draw_plots();
}


/************************************************************************
    cluster plots according to sample spectra
************************************************************************/
cluster_by_plot(listi,class,noise_level,th_amp,th_slope,count,bitmask)
unsigned char *listi,*bitmask;
int class,noise_level,th_amp,th_slope;
int *count;
{
    int lc,l1,l2,i,j1,k1,k2,th1,th2;
    int tmin,mclass,nbytes;
    unsigned char *data,avg,*buff,*malloc();
    register j;
    register unsigned char *p,*q;

    nbytes= img_bw * img_nl;
    data=malloc(img_numchan);
    buff=malloc((img_numchan+7)/8*2);
    if (data==NULL || buff==NULL) {
        printf("Insufficient memory.\n");
        if (data!=NULL) free(data);
        return(-1);
    }

/*********************************************************************/
/* start clustering with selected spectrum plots */
/*********************************************************************/

    for(l1 = 0; l1 <= class; l1++) count[l1]=0;

    for(j=0; j<img_nl*img_bw; j++) {

        tmin = th_amp + th_slope;
        mclass = -1;

        if (*(listi+j) == noise_level) *(listi+j) = 15;
        else {
            get_code(j,buff);

            *(listi+j) = class;

            for (l1=0; l1<class; l1++){
                plot_dist(spectra[l1].encoding,buff,&th1,&th2,bitmask);
                k1 = th1 + th2;
                if (th1 <= th_amp && th2 <= th_slope && k1 <= tmin) {
                    tmin = k1;
                    *(listi+j) = l1;
                }
            }
        }
    }

    p = listi;
    for (j=img_bw*img_nl;j!=0;j--) (*(count+ (*p++)))++;

    free(data);
    free(buff);
    return(0);
}


/**************************************************************************
    routine get noise thresholds from the user and displays histograms
**************************************************************************/
zerox_noise(listi,debug)
unsigned char *listi,debug;
{
	int thresh1,thresh2,nsamp,nsamp1,k,l,k1,k2;
        long int j;
	unsigned char com[3],ibuf[80];
	int *hist_amp,*hist_slope,*calloc();

        hist_amp = calloc(img_numchan,4);
        hist_slope = calloc(img_numchan,4);
        if (hist_amp==NULL || hist_slope==NULL) {
            printf("Insufficient memory.\n");
            if (hist_amp!=NULL) cfree(hist_amp);
            return(-1);
        }

	printf("\nEnter noise threshold values (amplitude & slope "); 
	query("fluctuation rate) : ",ibuf);
	sscanf(ibuf,"%d %d",&thresh1,&thresh2);

	for (l=0; l<img_numchan; l++){
		hist_amp[l] = 0;
		hist_slope[l] = 0;
		}

	nsamp = 0;	
	nsamp1 = 0;
	j = 0;
	for (l=0; l<img_nl; l++)
	for (k=0; k<img_bw; k++){
		*(listi+j) = 0;
		nsamp += 1;
		get_zerox(j,&k1,&k2);

		if ((k1 > thresh1) || (k2 > thresh2))  {
			hist_amp[k1] += 1;
			hist_slope[k2] += 1;
			nsamp1 +=1;
			*(listi+j) = 15;
			}
		j ++;
	}

	printf("\n %d samples were examined",nsamp);
	printf("\n %d samples were found to be feature-less\n",nsamp1);

	if (debug == 1) {
	printf("Amplitude zero crossing histogram\n");
	save_hist_data(hist_amp,img_numchan);
	draw_hist();

	query("Slope zero crossing histogram display?(y/n)\n",com);
	if (com[0] == 'y') {
		save_hist_data(hist_slope,img_numchan);
		draw_hist();
		}
	}
        cfree(hist_amp);
        cfree(hist_slope);
	vdflush(&unit);
	return(nsamp1);	
}

/**************************************************************************
    routine get noise thresholds from the user and displays histograms
**************************************************************************/
trinary_noise(listi)
unsigned char *listi;
{
	int nsamp,nsamp1,j,k,l,k1,thresh;
	unsigned char avg,ibuf[80],*buff,*malloc();

        buff=malloc(img_numchan);
        if (buff==NULL) {
            printf("Insufficient memory.\n");
            return(-1);
        }

	query("\n Enter amplitude threshold value for flat spectra :",ibuf);
	sscanf(ibuf,"%d",&thresh);

	nsamp = 0;	
	nsamp1 = 0;
	j = 0;

	for (l=0; l<img_nl; l++)
	for (k=0; k<img_bw; k++){
		*(listi+j) = 0;
		nsamp += 1;
		get_data(k,l,buff);
		get_avg(buff,&avg);	
		k1 = flat_spec(buff,avg,thresh);

		if (k1 == 0){
			nsamp1 +=1;
			*(listi+j) = 15;
			}
		j += 1;
	}

	printf("\n %d samples were examined",nsamp);
	printf("\n %d samples were found to be feature-less\n",nsamp1);
        free(buff);
	return(nsamp1);
}


/******************************************************************************
Sample goes through the image picking candidate spectra.  If a spectrum is
unlike any spectra previously sampled (as defined by the user's thresholds),
the spectra is added to the list of spectra seen so far stored in the plots
array.
******************************************************************************/
sample(listi,class,th_amp,th_slope,noise_level,quiet,bitmask)
int class,th_amp,th_slope,noise_level,quiet;
unsigned char *listi,*bitmask;
{
    int x1,y1,lc,l1,l2,i,j,j1,k,l,ifound;
    int nbytes,nsamp,th1,th2;
    unsigned char avg,*buff,*malloc();
    float f1,f2;
    int kskip,lskip,kstart,lstart;
    int dell,delk,loop;

    lc = 0;
    buff = malloc((img_numchan+7)/8*2);
    if (buff==NULL) {
        printf("Insufficient memory.\n");
        return(-1);
    }

/* generate pseudo spectrum library from the given image 
   do not include the boundary points                   */

    dell = img_nl/class;
    delk = img_bw;
    kstart = delk;
    loop = 1;

    while (lc<class && loop < 4) {
    	lstart = dell/2;
    	kstart = delk/2; 
    	loop++;
	
        if (dell==0) dell=1;
        if (delk==0) delk=1;

    	for (l=lstart; l<img_nl && lc<class; l+= dell)
                for (k=kstart; k<img_bw && lc<class; k+= delk){
/*          printf("l=%d,k=%d,lc=%d\n",l,k,lc); */

	    j = l*img_bw + k;

/* check if there are noisy spectrums, if so then skip them */
	    if (*(listi+j) != noise_level) {
		ifound = 0;
		get_code(j,buff);

		for (l1=0; l1<lc && ifound != 1; l1++){
		    plot_dist(spectra[l1].encoding,buff,&th1,&th2,bitmask);
		    if (th1 <= th_amp && th2 <= th_slope) ifound = 1;
		}

/* if no match was found add the spetrum to the list */
		if (ifound == 0) {

	    	    for(i=0; i<(img_numchan+7)/8*2; i++) 
		        spectra[lc].encoding[i] = buff[i];

		    if (quiet == 0) {
		   	plot_point(k,l,lc,3);
		   	printf("sample[%3d] position(%4d,%4d)\n",lc+1,k,
                            l+img_sl-1);
		    }
		    lc += 1;
		}
	    }
	}
	dell = dell/2;
	delk = delk/2;
    }
    free(buff);
    return(lc);
}

/***********************************************************************
    class merging routine
***********************************************************************/
merge_spec(listi,th_amp,th_slope,class,quiet,count,noise_level,bitmask)
unsigned char *listi,quiet,*bitmask;
int *class,th_amp,th_slope;
int *count;
{
	int l1,l2,index,num_thresh,th1,th2,k1,f1;
	unsigned char avg;
        char numbyte, numbyte2;

        numbyte = (img_numchan+7)>>3;
	numbyte2 = numbyte<<1;
/*
	encode the average spectra
*/
	for (l1=0; l1<(*class); l1++) {
	    avg = (float)spectra[l1].sum/(float)img_numchan + 0.5;
	    compact_code(spectra[l1].data,spectra[l1].encoding,avg);
	}

	num_thresh = 10;
/*
   merge the class based on the cluster average spectrum
*/
	for (l1=0; l1<(*class); l1++){
	th1 = th_amp+1; th2 = th_slope+1;

	for (l2=0; l2<l1 && (th1 > th_amp || th2 > th_slope); l2++)
	        if (count[l2] > 0){
	        plot_dist(spectra[l1].encoding,spectra[l2].encoding,&th1,&th2,
                    bitmask);
	        }

	if (th1 <= th_amp && th2 <= th_slope) {
		if (quiet == 0)
		   printf("class[%d] is merged with class[%d]\n",l1+1,l2);

		change_class(listi,l1,l2-1,count[l1]);
		count[l2-1] += count[l1];
		count[l1] = 0;
	        if (avg_spec(listi,l2-1,count[l2-1],0)==-1) return; 
	        avg = (float)spectra[l2-1].sum/(float)img_numchan + 0.5;
	        compact_code(spectra[l2-1].data,spectra[l2-1].encoding,avg);
		}
	}
/*
   sort out insignificant sample spectrum
*/
	for (l1=0; l1<(*class); l1++) 
	if (count[l1] <= num_thresh) {
		change_class(listi,l1,noise_level,count[l1]);
		count[l1] = 0;
		}

	index = 0;
	for (l1=0; l1<(*class); l1++) 
	if (count[l1] > 0) {
	   if (quiet == 1) {
	   	if (avg_spec(listi,l1,count[l1],0)==-1) return; 
		avg = (float)spectra[l1].sum/(float)img_numchan + 0.5;
		compact_code(spectra[l1].data,spectra[index].encoding,avg);
		}
	   else change_class(listi,l1,index,count[l1]);
           count[index] = count[l1];
	   index++;
	}
	*class = index;
}
