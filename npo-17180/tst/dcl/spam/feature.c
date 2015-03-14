/*****************************************************************************
        %M% %I% - Feature extraction command
*****************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "spam.h"

feature(buf)
unsigned char *buf;
{
    int i,j,k,count,wl_count,range_change,lib_entry,pct_th,find_count;
    int spec_count,libr,diff,mlib_len,mlib_index,ulib_index,index;
    float wave[20],band_to_wl(),err;
    short c512=512;
    double peakloc;
    struct feature *fp,*feature_analysis();
    unsigned char *malloc(),*refarea,*data;
    char ans[80],name[80],mode[20],libspec[20*15],ais[10],aviris[10],image[10];
    static struct parameter kwds[]={
	{"mode",2,0,1,"mode (find or display)","",20,0},
        {"spec",2,0,-15,"library spectrum to display or match","",20,0},
        {"peaks",1,0,-20,"wavelength range(s)","",0,0},
        {"threshold",0,0,1,"match library spectra using the threshold","",0,0},
        {"ais",3,0,0,"AIS resampling","",10,0},
        {"aviris",3,0,0,"AVIRIS resampling","",10,0},
        {"image",3,0,0,"image resampling","",10,0},
    };
    if (display_mode==1) {
        printf("No feature extraction in display mode.  Use the \"return\" ");
        printf("command to exit.\n");
        return;
    }
    kwds[0].value = mode;
    kwds[1].value = libspec;
    kwds[1].input_count = &spec_count;
    kwds[2].value = (char *)wave;
    kwds[2].input_count = &wl_count;
    kwds[3].value = (char *)&pct_th;
    kwds[3].input_count = &find_count;
    kwds[4].value = ais;
    kwds[5].value = aviris;
    kwds[6].value = image;
    if (par(buf,kwds,7)) return;
    if ((*ais!='\0' || *aviris!='\0' || *image!='\0') && feature_range!=0) {
	free_feature_dbase();
	if (img_in_core==0) {
	    for (i=0;i<16;i++) if (plots[i].name!=0) erase_plot(i);
	    vdareafi(&unit,&c4,&c0,&c0,&c0,&c1,&c1,&c512,&c512);
	    vdflush(&unit);
	}
    }
    if (img_in_core && (*ais!='\0' || *aviris!='\0')) {
	printf("Resampling after get must be using range defined by image.\n");
	return;
    }
    else if (*ais!='\0' && *aviris=='\0' && *image=='\0') {
        feature_range = 1;
        range_change = 1;
    }
    else if (*ais=='\0' && *aviris!='\0' && *image=='\0') {
        feature_range = 2;
        range_change = 1;
    }
    else if (*ais=='\0' && *aviris=='\0' && *image!='\0') {
        if (img_in_core) {
            feature_range = 3;
            range_change = 1;
        }
        else {
            printf("You must get an image before using the image keyword.\n");
            return;
        }
    }
    else if (*ais!='\0' || *aviris!='\0' || *image!='\0') {
        printf("Conflicting resampling keywords used.\n");
        return;
    }
    else if (feature_range==0 && !img_in_core) {
	printf("No resampling chosen.  Type \"help feature\" for help.\n");
        return;
    }
    else if (wl_count/2*2!=wl_count) {
	printf("Odd number of peaks-range endpoints.\n");
	return;
    }
    else range_change = 0;
    switch (feature_range) {
        case 1: img_swl=1.2; img_ewl=2.4; img_numchan=128; break;
        case 2: img_swl=0.4; img_ewl=2.4; img_numchan=224; break;
        case 3: break;
    }
    lib_entry = master_lib[0].lib_length + user_lib[0].lib_length;
    if (range_change) {
        if (img_in_core==0) draw_plots(); /* draw axes and x scale */
        feature_dbase_main();
    }
    if (substring(mode,"display")) {
	count=1;
	if (wl_count!=0 && spec_count==0) {
	    if (feature_range==0) {
	        printf("Resampling library using image range.\n");
		feature_range = 3;
                feature_dbase_main();
	    }
	    for (i=0;i<wl_count;i+=2) {
                if (wave[i]<img_swl || wave[i]>img_ewl || wave[i+1]<img_swl ||
		        wave[i+1]>img_ewl) {
                    printf("Wavelength in peaks range is illegal for current ");
		    printf("resampling.\n");
                    return;
		}
            }
	    mlib_index = master_lib[0].right;
	    ulib_index = user_lib[0].right;
	    mlib_len = master_lib[0].lib_length;
	    err=(img_ewl-img_swl)/(2*(img_numchan-1));
            for (i=0;i<lib_entry;i++) {
	        if (mlib_index==0 || (ulib_index!=0 && strcmp(master_lib[
		        mlib_index].name,user_lib[ulib_index].name)>0)) {
		    strcpy(name,user_lib[ulib_index].name);
		    index = ulib_index + mlib_len - 1;
		    ulib_index = user_lib[ulib_index].right;
	        }
	        else {
		    strcpy(name,master_lib[mlib_index].name);
		    index = mlib_index - 1;
		    mlib_index = master_lib[mlib_index].right;
	        }
		for (k=0;k<wl_count;k+=2) {
                    for (j=0;j<feature_dbase[index].no_peak;j++) {
                        peakloc = band_to_wl(feature_dbase[index].peaks[j].loc);
                        if (peakloc>wave[k]-err && peakloc<wave[k+1]+err) break;
		    }
		    if (j==feature_dbase[index].no_peak) break;
                }
                if (k==wl_count)
                    count = disp_spec_strip(count,feature_dbase+index,name,1);
            }
	    printf("\n");
        }
        else if (wl_count==0 && spec_count!=0) {
	    for (i=0;i<spec_count;i++) {
		strcpy(name,libspec+20*i);
	        if (get_spectrum_loc(name,&libr,&index)) break;
	        if (libr==0) {
	            fp = feature_analysis(plots[index].data,0);
		    if (fp==NULL) break;
		    lower_case(name);
	            count = disp_spec_strip(count,fp,name,0);
                    free_fp(fp);
	        }
		else if (feature_range==0) {
		    data = malloc(img_numchan);
		    if (data!=NULL) {
		        get_lib_data(index,libr,data);
	                fp = feature_analysis(data,0);
			if (fp==NULL) break;
		        free(data);
	                count = disp_spec_strip(count,fp,name,0);
			free_fp(fp);
		    }
		    else printf("Insufficient memory.\n");
		}
                else {
		    if (libr==1) index--;
                    else index = index-2+master_lib[0].lib_length;
	            count = disp_spec_strip(count,feature_dbase+index,name,0);
	        }
	    }
	    printf("\n");
	}
	else {
	    printf("Inconsistent or incomplete display command.  Please ");
	    printf("give spectral\nname(s) or wavelength range.\n");
	}
    }
    else if (substring(mode,"find")) {
        while (spec_count!=1) {
	    if (spec_count>1)
		printf("You may only find one spectrum at a time.\n");
            query("Spectrum name (library or data plot) ? ",libspec);
            if (libspec[0]!='\0') spec_count=1;
        }
	if (feature_range==0) {
	    printf("Resampling library using image range.\n");
	    feature_range = 3;
            feature_dbase_main();
	}
	if (get_spectrum_loc(libspec,&libr,&index)) return;
        if (libr==0) {
	    fp = feature_analysis(plots[index].data,0);
	    if (fp==NULL) return;
	    refarea = fp->area;
	}
        else if (libr==1) refarea = feature_dbase[index-1].area;
        else refarea = feature_dbase[index-2+master_lib[0].lib_length].area;
        while (find_count==0) {
	    query("Threshold (pct difference, 0<=pct<=100) ? ",ans);
	    for (i=0;i<strlen(ans) && isdigit(ans[i]);i++) ;
	    if (i==strlen(ans)) find_count = sscanf(ans,"%d",&find_count);
	    else printf("Please use an integer threshold only.\n");
	}
        count = 1;
	mlib_index = master_lib[0].right;
	ulib_index = user_lib[0].right;
	mlib_len = master_lib[0].lib_length;
        for (i=0;i<lib_entry;i++) {
	    if (mlib_index==0 || (ulib_index!=0 && strcmp(
		    master_lib[mlib_index].name,user_lib[ulib_index].name)>0)) {
		strcpy(name,user_lib[ulib_index].name);
		index = ulib_index + mlib_len - 1;
		ulib_index = user_lib[ulib_index].right;
	    }
	    else {
		strcpy(name,master_lib[mlib_index].name);
		index = mlib_index - 1;
		mlib_index = master_lib[mlib_index].right;
	    }
            diff=0;
            for (j=0;j<img_numchan;j++)
                diff += abs(feature_dbase[index].area[j]-refarea[j]);
            if (diff/510.*100 <= pct_th)
                count = disp_spec_strip(count,feature_dbase+index,name,0);
        }
        printf("\n");
	if (libr==0) free_fp(fp);
    }
    else if (!range_change) printf("No mode or resampling specified.\n");
}

feature_dbase_main()
{
    struct feature *fp,*feature_analysis();
    int index,lib_entry,mlib_len;
    unsigned char *malloc(),*libr_spec;

/* this program extracts features from the resampled library
   spectra and generates a feature data base                */

    lib_entry = master_lib[0].lib_length + user_lib[0].lib_length;
    feature_dbase = (struct feature *) malloc(lib_entry*sizeof(struct feature));
    if (feature_dbase!=NULL) libr_spec=malloc(img_numchan);

    if (feature_dbase==NULL || libr_spec==NULL) {
        printf("Insufficient memory.\n");
	if (feature_dbase!=NULL) free_feature_dbase();
        return;
    }
    mlib_len = master_lib[0].lib_length;

    for (index = 0; index < lib_entry; index++) {
        if (index<mlib_len) get_lib_data(index+1,1,libr_spec);
        else get_lib_data(index-mlib_len+1,2,libr_spec);
        fp = feature_analysis(libr_spec, feature_dbase+index);
	if (fp==NULL) {
	    while (++index<lib_entry) { /* else might free non-malloc-ed area */
		feature_dbase[index].area = NULL;
		feature_dbase[index].peaks = NULL;
	    }
	    free_feature_dbase();
	    break;
	}
    }
    free(libr_spec);
}

disp_spec_strip(count,fp,name,print_peaks)
int count,print_peaks;
struct feature *fp;
char *name;
{
    int j,k;
    float f1=1.0,scale,band_to_wl();
    double channel_gap;
    short len,c,strip_width,left,top,right,bottom,start,end,i,n;
    unsigned char *buf,*malloc(),ans[80];
    channel_gap = (img_ewl-img_swl)/(img_numchan-1);
    strip_width = plots_width;
    scale = (float)plots_width/(img_numchan-1);
    left = plots_hoff;
    vdtxtsiz(&c7,&f1);
    if (count==16) {
	printf("\n");
        do query("Press RETURN for remaining bars.",ans); while (*ans!='\0');
        count=1;
    }
    if (count==1) {
        if (hist_on) erase_hist();
        else erase_fea();
        if (print_peaks) printf("\n                  Peaks at\n");
    }
    else if (!print_peaks && (count==6 || count==11)) printf("\n");
    printf("%16s ",name);
    if (print_peaks) for (i=0;i<fp->no_peak;i++)
	printf(" %-5.2f",band_to_wl(fp->peaks[i].loc));
    printf("\n");
    buf = malloc(strip_width*8);
    if (buf==NULL) {
	printf("Insufficient memory.\n");
	return(count);
    }
    for (i=0;i<strip_width;i++) *(buf+i) = 3;
    for (i=0;i<fp->no_peak;i++) {
        start=fp->peaks[i].start*scale-scale/2;
        if (start<0) start=0;
        end=fp->peaks[i].end*scale+scale/2;
        if (end>strip_width-1) end=strip_width-1;
        for (j=start;j<=end;j++) *(buf+j) = 4;
    }
    for (i=0;i<fp->no_peak;i++) {
        start=fp->peaks[i].loc*scale-scale/2;
        if (start<0) start=0;
        end=start+scale;
        if (end>strip_width-1) end=strip_width-1;
        for (j=start;j<=end;j++) *(buf+j) = 0;
    }
    for (i=strip_width;i<8*strip_width;i++) *(buf+i) = *(buf+i-strip_width);
    top = count*11+plots_voff+21;
    right=left+strip_width-1;
    bottom = top+7;
    len = strlen(name);
    upper_case(name);
    c=right+10;
    vdtext(&unit,&c4,&c3,&c3,&c3,&c,&bottom,&c1,&len,name);
    n=strip_width*8;
    vdareawr(&unit,&c4,&n,buf,&left,&top,&right,&bottom);
    vdflush(&unit);
    free(buf);
    return(count+1);
}

float band_to_wl(band)
int band;
{
    return(band/(img_numchan-1.)*(img_ewl-img_swl)+img_swl);
}

get_spectrum_loc(name,libr_addr,index_addr)
char *name;
int *libr_addr,*index_addr;
{
    *index_addr = -1;
    while (*index_addr<0) {
        get_plot_num(name,0,index_addr);
        if (*index_addr>=0) *libr_addr=0;
        else if (*index_addr==-1) {
            get_libplot_num(name,index_addr,libr_addr,0,3);
            if (*index_addr<0) {
		if (*index_addr==-2) ;   /* ambiguous */
		else if (check_old_name(name)) ;
                else {			 /* not found */
                    printf("Couldn't find a library or data plot named ");
                    printf("\"%s\".\n",name);
                }
                query("Spectrum name (RETURN for READY) ? ",name);
                if (name[0]=='\0') return(1);
            }
        }
	else /* *index_addr==-2 */ {
	    printf("The name \"%s\" could refer to any of several ",name);
	    printf("plots displayed.\n");
	    query("Spectrum name (RETURN for READY) ? ",name);
	    if (name[0]=='\0') return(1);
	}
    }
    return(0);
}
