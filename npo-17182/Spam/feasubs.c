/****************************************************************************
        feasubs.c 2.15 - Feature extraction subroutines
****************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "spam.h"

struct feature *feature_analysis(spectrum,fp)
unsigned char *spectrum;
struct feature *fp;
{
    unsigned char *malloc(),im_err;
    if (fp==NULL) fp=(struct feature *)malloc(sizeof(struct feature));
    if (fp!=NULL) fp->peaks = (struct peak *)malloc(10*sizeof(struct peak));
    if (fp!=NULL) fp->area = malloc(img_numchan);
    if (fp==NULL || fp->peaks==NULL || fp->area==NULL) {
	free_fp(fp);
	return(NULL);
    }
    else {
        im_err = feature_extracter(spectrum,fp);
	if (im_err) {
	    free_fp(fp);
	    return(NULL);
	}
        feature_adjust(spectrum,fp);
        feature_encoder(spectrum,fp);
        return(fp);
    }
}

free_fp(fp)
struct feature *fp;
{
    if (fp==NULL) return;
    if (fp->peaks!=NULL) free(fp->peaks);
    if (fp->area!=NULL) free(fp->area);
    free(fp);
}

free_feature_dbase()
{
    int i,lib_length;
    struct feature *fp;
    if (feature_dbase!=NULL) {
        lib_length = master_lib[0].lib_length + user_lib[0].lib_length;
        for (fp=feature_dbase,i=0;i<lib_length;fp++,i++) free_fp(fp);
        free(feature_dbase);
    }
    feature_range = 0;
}

feature_extracter(spec,fspec)
unsigned char  *spec;
struct feature *fspec;

/* this routine extracts absorption features from a given spectrum and
   describes each feature with 5 parameters; they are starting location,
   ending location, depth, minimum point, and width. Also, the spectrum 
   is restructured so that non-feature areas are set to zero */
{
    int no_peak,   wmax, emax, prev_end, left, right, min;
    int i1,i2,i3,j,equal_count,pre_depth_th;
    int peak_start,peak_end,peak_loc;
    double channel_gap,slope,depth;
    unsigned char *realloc();

    pre_depth_th = 5;

    channel_gap = (img_ewl-img_swl)/(img_numchan-1);
    wmax = 0.3/channel_gap;     /* width / channel gap  */  
    emax = wmax/5;              /* wmax / 5 */
    no_peak = 0;
    prev_end = 0;
    i1 = 1;

    while (i1 < (img_numchan-1)) {
        if (((spec[i1] < spec[i1-1]) && (spec[i1] <= spec[i1+1])) ||
                ((spec[i1] <= spec[i1-1]) && (spec[i1] < spec[i1+1]))) {
            depth = spec[i1];
            peak_loc = i1;
            peak_start = peak_end = peak_loc;
              
 /* peak_start-search lasts until the max of (previous
    peak_end, half of the expected width max from the min) */

            left = ((prev_end+1) > (peak_loc-wmax))? prev_end+1 : peak_loc-wmax;
            equal_count = 0;
            j = peak_loc - 1;

            while ((j >= left) && (peak_start == peak_loc)) { 
                if (spec[j] > spec[j-1]) peak_start = j + equal_count;
                else {
                    if (spec[j] == spec[j-1]) {
                        equal_count++;
                        if ((equal_count >= 5)) peak_start = j+equal_count-1;
                    }
                    else equal_count  = 0;
                }
                j--;
            }
            if (peak_start == peak_loc) peak_start = left+equal_count-1;

         /* peak_end-search lasts until the min of */
         /* (20 bands from the minimum, image_nb)  */
        
            right = ((img_numchan-2) < (peak_loc+wmax)) ? 
                (img_numchan-2) : peak_loc+wmax;
            equal_count = 0;
            j = peak_loc+1;
            while (spec[peak_loc] == spec[j]) j++;

            j--;
            while ((j <= right) && (peak_end == peak_loc)) {
                if (spec[j] > spec[j+1]) peak_end = j-equal_count;
                else {
                    if (spec[j] == spec[j+1]) {
                        equal_count++;
                        if (equal_count >= emax) peak_end = j-equal_count+1;
                    }
                    else equal_count = 0;
                }
                j++;
            }

            if (peak_end == peak_loc) peak_end = right-equal_count+1;

         /* ignore insignificant absorption features and if the */
         /* incomplete valley has been found (i.e., peak_end =  */
         /* image_nb) then treat it exceptionally               */

            if (peak_end < img_numchan) {
                slope = ((float)spec[peak_end]-spec[peak_start]) /
                    (peak_end-peak_start);
                depth = slope * (peak_loc-peak_start) +
                    spec[peak_start]-spec[peak_loc];
            }
            else {
                depth = (float)spec[peak_start]-spec[peak_loc];
                slope = 0.0;
            }
            min = (spec[peak_start] < spec[peak_end]) ? 
                spec[peak_start] : spec[peak_end];
            if ((depth > pre_depth_th) && (min > spec[peak_loc])) {
        
            /* an absorption peak has been found, allocate storage */
            /* for its information and then store it            */
        
            /* update peak_start */
		if (no_peak!=0 && no_peak%10==0) {
		    fspec->peaks =
			(struct peak *)realloc(fspec->peaks,(no_peak+10)*
			    sizeof(struct peak));
		    if (fspec->peaks==NULL) {
			printf("Insufficient memory.\n");
			return(1);
		    }
		}
                fspec->peaks[no_peak].start = peak_start;
                fspec->peaks[no_peak].loc = peak_loc;
                fspec->peaks[no_peak].end = peak_end;
                fspec->peaks[no_peak].depth = 0;
                fspec->peaks[no_peak].width = peak_end - peak_start + 1;
                no_peak++;
                prev_end = peak_end;
                i1 = peak_end;
            }
        }
        fspec->no_peak = no_peak;
        i1++;
    }
    return(0);
}

feature_encoder(spec, fspec)
unsigned char *spec;
struct feature *fspec;
/*
    This routine extracts areas within the feature and normalizes 
*/     
{
    int no_peak, i,i1,i2, peak_start,peak_end,peak_loc;
    float sum_energy, scale;
    double slope,depth,area_max;
        
    area_max = 255;

    no_peak = fspec->no_peak;
    for (i=0;i<img_numchan;i++) fspec->area[i]=0;
    for (i1=0; i1<no_peak; i1++) {
        peak_start = fspec->peaks[i1].start;
        peak_end = fspec->peaks[i1].end;
        slope = ((float)spec[peak_end] - spec[peak_start]) /
	    (peak_end - peak_start);

        for (i2 = peak_start; i2 <= peak_end; i2++) {
            depth = slope * (i2 - peak_start) + spec[peak_start] - spec[i2];
            fspec->area[i2] = depth>=0 ? depth : 0;
        }

        peak_loc = fspec->peaks[i1].loc;  
        fspec->peaks[i1].depth = fspec->area[peak_loc]; 
    }

    if (no_peak > 0) {
        sum_energy = 0.0; 
        for (i1 = 0; i1 < img_numchan; i1++) 
            sum_energy += (float)fspec->area[i1];

        scale = area_max/sum_energy;

        for (i1 = 0; i1 < img_numchan; i1++)
            fspec->area[i1] = (float)fspec->area[i1] * scale;
    }
}

feature_adjust(spec, fspec)
unsigned char *spec;
struct feature *fspec;
{
    int i1,i2,i3,index,no_peak,peak_gap,gap_th,width_th;
    int final_depth_th,peak_start,peak_end,peak_loc;
    double channel_gap,slope,depth;

    channel_gap = (img_ewl-img_swl)/(img_numchan-1);
    no_peak = fspec->no_peak;
    gap_th = 0.1/channel_gap;
    width_th = 0.02/channel_gap;
    final_depth_th = 7;

    i1 = 0;
    while (i1 < no_peak-1) {
        peak_gap = 0;
        i2 = i1;

        while (i2 < no_peak && peak_gap < gap_th) {
            peak_gap = fspec->peaks[i2+1].loc - fspec->peaks[i2].loc;
            i2++;
        }

        i2--;
        if (i2 != i1) {
            peak_start = fspec->peaks[i1].start; 
            peak_end = fspec->peaks[i2].end;

            for (i3=i1; i3<=i2; i3++) {
                fspec->peaks[i3].start = peak_start;
                fspec->peaks[i3].end = peak_end;
                fspec->peaks[i3].depth = -1;
            }
            i1 = i2;
        }
        else i1++;
    }
    index = 0;
    for (i1 = 0; i1 < no_peak; i1++) {
        peak_start = fspec->peaks[i1].start;
        peak_end = fspec->peaks[i1].end;
        peak_loc = fspec->peaks[i1].loc;

        slope = ((float)spec[peak_end] - spec[peak_start]) /
            (peak_end - peak_start);

        if (i1 > 0 && fspec->peaks[i1].depth != -1) {

            for (i2 = peak_start; i2<=peak_loc; i2++) {
                depth = slope * (i2-peak_start) + spec[peak_start];
                if (spec[i2] >= depth) peak_start = i2;
            }

            for (i2 = peak_end; i2 >=peak_loc; i2--) {
                depth = slope * (i2-peak_start) + spec[peak_start];
                if (spec[i2] >= depth) peak_end = i2;
            }
        }

        if ((peak_end - peak_start) > width_th) {
            if (fspec->peaks[i1].depth == 0) {
                for (i2=peak_start; i2<=peak_end; i2++) 
                    if (spec[peak_loc] > spec[i2]) peak_loc = i2;
            }

            depth = slope * (peak_loc - peak_start) + spec[peak_start] -
		spec[peak_loc];

            if (depth > final_depth_th) {
                fspec->peaks[index].start = peak_start;
                fspec->peaks[index].loc = peak_loc;
                fspec->peaks[index].end = peak_end;
                fspec->peaks[index].depth = depth;
                fspec->peaks[index].width = peak_end-peak_start+1;
                index++;
            }
        }
    }

    no_peak = index;
    fspec->no_peak = no_peak;
}

erase_fea()
{
    short t1,t2,c512=512;
    t1=plots_hoff;
    t2=plots_voff+30;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&t2,&c512,&c512);
}
