/************************************************************************
   get.c 2.15 - get image file from disk
   get file from disk and initialize data variables.  Draw band and
       axes, along with horizontal scale.
************************************************************************/
#include <stdio.h>
#include "spam.h"
get(buf)
unsigned char *buf;
{
    long int i;
    int sl_given,nl_given,sb_given,nb_given,lbl_nl,lbl_ns,lbl_bw,lbl_numchan;
    int tsl,tnl,tsb,tnb;
    float lbl_swl,lbl_ewl;
    int img_nb,fd,libr_image;
    short int lut[256],c512=512;
    float interval;
    char file[256],inbuf[80],format[80];
    static struct parameter kwds[]={
        {"filename",2,1,1,"filename","",80,0},
        {"sl",0,0,1,"starting line of window (default is 1) ","",0,0},
	{"nl",0,0,1,"number of lines (default is img_nl) ","",0,0},
	{"sb",0,0,1,"starting band of window (default is 1) ","",0,0},
	{"nb",0,0,1,"number of bands (default is img_numchan) ","",0,0},
    };
    if (strlen(image_lib_dir)>80) {
        printf("Name of image library as defined in spam.h is too long.\n");
        return;
    }
    strcpy(file,image_lib_dir);             /* system libr directory.     */
    kwds[0].value = file+strlen(image_lib_dir);
    kwds[1].value = (char *)&tsl;
    kwds[1].input_count = &sl_given;
    kwds[2].value = (char *)&tnl;
    kwds[2].input_count = &nl_given;
    kwds[3].value = (char *)&tsb;
    kwds[3].input_count = &sb_given;
    kwds[4].value = (char *)&tnb;
    kwds[4].input_count = &nb_given;
    if (par(buf,kwds,5)) return;            /* get filename and window size. */
    fd=vosopen(file+strlen(image_lib_dir),0,0,0);/* look in current dir first */
    if (fd == -1) {	         /* then try it in system image directory    */
        fd=vosopen(file,0,0,0);
        if (fd == -1) {
            printf("Couldn't find/read image file \"%s\".\n",
	        file+strlen(image_lib_dir));
            return;
        }
        else libr_image=1;
    }
    else libr_image=0;
    if (vosreadl(fd,"nl",&lbl_nl,0)==-1 || vosreadl(fd,"ns",&lbl_ns,0)==-1 ||
        vosreadl(fd,"sw",&lbl_swl,1)==-1 || vosreadl(fd,"ew",&lbl_ewl,1)==-1 ||
        vosreadl(fd,"bw",&lbl_bw,0)==-1 || vosreadl(fd,"format",format,2)==-1) {
            printf("Label item may not be in the label.  Type \"help\" for ");
            printf("label information.\n");
            vosclose(fd);
            return;
        }
    vosclose(fd);
    if (lbl_bw<1 || lbl_bw>256) {
        printf("Input image has bad bandwidth value.\n");
        return;
    }
    if (strcmp(format,"BYTE")!=0 && strcmp(format,"byte")!=0) {
        printf("Input image isn't in byte format according to label.\n");
        return;
    }
    lbl_numchan = lbl_ns/lbl_bw;
    if (!sb_given) tsb=1;
    if (!nb_given) tnb=lbl_numchan-tsb+1;
    if (!sl_given) tsl=1;
    if (!nl_given) tnl=lbl_nl-tsl+1;
    if (tsb<1 || tsb>=lbl_numchan) {
        printf("Starting band is illegal for this image.\n");
        return;
    }
    if (tnb>512 || tsb+tnb-1>lbl_numchan) {
        printf("Image specified has too many bands.\n");
        return;
    }
    if (tsl<1 || tsl>lbl_nl) {
        printf("Starting line is illegal for this image.\n");
        return;
    }
    if (tnl>512 || tsl+tnl-1>lbl_nl) {
        printf("Image specified has too many lines.\n");
        return;
    }
    if (tnl<1) {
	printf("Image specified has too few lines.\n");
	return;
    }
    if (tnb<2) {
	printf("Image specified has too few bands.\n");
	return;
    }
/*
/*      Save values for new image in globals.  This is the point of no "return".
*/
    img_sl = tsl;
    img_nl = tnl;
    img_sb = tsb;
    img_nb = tnb;
    img_bw = lbl_bw;
    img_el=img_sl+img_nl-1;
    img_eb=img_sb+img_nb-1;
    img_ns = img_bw*img_nb;
    img_numchan = img_nb;
    interval = (float)(lbl_ewl-lbl_swl)/(float)(lbl_numchan-1);
    img_swl = lbl_swl+((float)(img_sb-1)*interval);
    img_ewl = lbl_swl+((float)(img_eb-1)*interval);
/*
/*      initialize globals
*/
    plots_hoff=77+(img_bw-32);			/* set graph shape and loc    */
    plots_width=310-(img_bw-32);
    stretch_start=0;                            /* reset from previous        */
    stretch_end=255;                            /* stretches, if any.         */
    vdareafi(&unit,&c4,&c0,&c0,&c0,&c1,&c1,&c512,&c512);    /* clear graphics */
    vdflush(&unit);				/* and                        */
    if (display_mode==1) {			/* return from display mode   */
        vddispln(&unit,&c4);			/* if necessary.              */
        display_mode=0;
    }
    plots_num=0;                                /* clear any existing plots.  */
    for (i=0;i<16;i++) if (plots[i].name!=0) {
        free(plots[i].name);
        plots[i].name = 0;
    }
    draw_plots();
    img_generic = 0;                            /* next default name is PLOT0.*/
    img_modified = 0;
    for (i=0;i<256;i++) lut[i]=i;		/* reset luts.                */
    disp_band=1;                                /* save current band number.  */
    if (feature_range!=0) {
	free_feature_dbase();
	printf("Spectral feature information removed.\n");
    }
    if (hist_on) erase_hist();
    if (libr_image) {
        if (get_work(lut,file,0)) return;
    }
    else {
        if (get_work(lut,file+strlen(image_lib_dir),0)) return;
    }
}


/*************************************************************************
    restore - allows user to restore previous spam session frozen by keep
*************************************************************************/
restore(buf)
unsigned char *buf;
{
    unsigned char *calloc(),*malloc(),*label_buffer;
    char file[80];
    float f1=1.0;
    short int lut[256];
    short int c,c512=512,c255=255,c45=45,t1,t2;
    int lbl_nl,lbl_ns,lbl_swl,lbl_ewl,lbl_bw;
    int unitnum,t,i,j,fd,ll,nbytes;

    unitnum=vosopen("spam.ses",0,1,10000);
    if (unitnum == -1) {
        printf("Can't read record of previous session.  ");
        printf("(File spam.ses cannot be opened.)\n");
        return;
    }
    printf("Restoring previous spam session . . .\n");
/*
/*      clear screen and force graphics mode
*/
    vdareafi(&unit,&c4,&c0,&c0,&c0,&c1,&c1,&c512,&c512);
    if (display_mode==1) {
        vdflush(&unit);
        vddispln(&unit,&c4);
        display_mode=0;
    }
    if (hist_on) erase_hist();
/*
/*      read in image info
*/
    j=1;
    vosread(unitnum,&img_bw,1,j,1,sizeof(img_bw));
    vosread(unitnum,&img_numchan,1,j+=sizeof(img_bw),1,sizeof(img_numchan));
    vosread(unitnum,&img_nl,1,j+=sizeof(img_numchan),1,sizeof(img_nl));
    vosread(unitnum,&img_sl,1,j+=sizeof(img_nl),1,sizeof(img_sl));
    vosread(unitnum,&img_el,1,j+=sizeof(img_sl),1,sizeof(img_el));
    vosread(unitnum,&img_ns,1,j+=sizeof(img_el),1,sizeof(img_ns));
    vosread(unitnum,&img_sb,1,j+=sizeof(img_ns),1,sizeof(img_sb));
    vosread(unitnum,&img_eb,1,j+=sizeof(img_sb),1,sizeof(img_eb));
    vosread(unitnum,&img_swl,1,j+=sizeof(img_eb),1,sizeof(img_swl));
    vosread(unitnum,&img_ewl,1,j+=sizeof(img_swl),1,sizeof(img_ewl));
    vosread(unitnum,&img_generic,1,j+=sizeof(img_ewl),1,sizeof(img_generic));
    vosread(unitnum,&img_modified,1,j+=sizeof(img_generic),1,
	sizeof(img_modified));
    vosread(unitnum,&t,1,j+=sizeof(img_modified),1,sizeof(t));
    j+=sizeof(t);
    vosread(unitnum,file,1,j,1,t);
    file[t]='\0';
    j+=t;
/*
/*      update stretch globals and set luts
*/
    vosread(unitnum,&stretch_start,1,j,1,sizeof(stretch_start));
    vosread(unitnum,&stretch_end,1,j+=sizeof(stretch_start),1,
        sizeof(stretch_end));
    if (stretch_start!=0 || stretch_end!=255) {
        for (i=0;i<=stretch_start;i++) lut[i]=0;
        for (i=stretch_start+1;i<stretch_end;i++)
	    lut[i]=(float)(i-stretch_start)/(stretch_end-stretch_start)*255;
        for (i=stretch_end;i<256;i++) lut[i]=255;
    }
    else for (i=0;i<256;i++) lut[i]=i;
/*
/*      get old cursor shape and size from session file
*/
    vdcuroff(&unit,&c1);
    vosread(unitnum,&cursor_nl,1,j+=sizeof(stretch_end),1,sizeof(cursor_nl));
    vosread(unitnum,&cursor_ns,1,j+=sizeof(cursor_nl),1,sizeof(cursor_ns));
/*
/*      update disp_ globals
*/
    vosread(unitnum,&disp_band,1,j+=sizeof(cursor_ns),1,sizeof(disp_band));
/*
/*      display band and do image and library encoding
*/
    if (get_work(lut,file,1)) return;
/*
/*	read in plots information
*/
    plots_hoff=77+(img_bw-32);
    plots_voff=315;
    plots_width=310-(img_bw-32);
    plots_height=285;
    vosread(unitnum,&plots_mindn,1,j+=sizeof(disp_band),1,sizeof(plots_mindn));
    vosread(unitnum,&plots_maxdn,1,j+=sizeof(plots_mindn),1,
        sizeof(plots_maxdn));
    vosread(unitnum,&plots_norm,1,j+=sizeof(plots_maxdn),1,sizeof(plots_norm));
    vosread(unitnum,&plots_norm_mode,1,j+=sizeof(plots_norm),1,
        sizeof(plots_norm_mode));
    vosread(unitnum,&plots_maxy,1,j+=sizeof(plots_norm_mode),1,
        sizeof(plots_maxy));
    vosread(unitnum,&plots_miny,1,j+=sizeof(plots_maxy),1,sizeof(plots_miny));
    vosread(unitnum,plots,1,j+=sizeof(plots_miny),1,16*sizeof(struct plot));
    j+=16 * sizeof(struct plot);
    plots_num = 0;
    for (i=0;i<16;i++) if (plots[i].name!=0) {
	vosread(unitnum,&t,1,j,1,sizeof(t));
        j+=sizeof(t);
	plots[i].name = malloc(t+1);
	plots[i].data = malloc(img_numchan);
	plots[i].variance = malloc(img_numchan);
	if (plots[i].name==NULL ||plots[i].data==NULL||plots[i].variance==NULL){
	    printf("Insufficient memory.\n");
            if (plots[i].name!=NULL) {
                free(plots[i].name);
                plots[i].name=0;
            }
            if (plots[i].data!=NULL) free(plots[i].data);
            if (plots[i].variance!=NULL) free(plots[i].variance);
            while (i<16) plots[i++].name=0;
	    draw_plots();
            vosclose(unitnum);
            return;
	}
	else {
            plots_num++;
	    vosread(unitnum,plots[i].name,1,j,1,t);
            j+=t;
	    *(plots[i].name+t) = '\0';
            vosread(unitnum,plots[i].data,1,j,1,img_numchan);
            j+=img_numchan;
            vosread(unitnum,plots[i].variance,1,j,1,img_numchan);
            j+=img_numchan;
        }
    }
    draw_plots();
/*
/*      update hist_ globals
*/
    vosread(unitnum,&hist_on,1,j,1,sizeof(hist_on));
    j+=sizeof(hist_on);
    if (hist_on) {
        vosread(unitnum,&hist_nbins,1,j,1,sizeof(hist_nbins));
        vosread(unitnum,&hist_scale,1,j+=sizeof(hist_nbins),1,
	    sizeof(hist_scale));
        vosread(unitnum,&hist_nelts,1,j+=sizeof(hist_scale),1,
	    sizeof(hist_nelts));
        vosread(unitnum,&hist_sum,1,j+=sizeof(hist_nelts),1,sizeof(hist_sum));
        vosread(unitnum,&hist_sum2,1,j+=sizeof(hist_sum),1,sizeof(hist_sum2));
        hist_bins = (int *)calloc(hist_nbins,sizeof(*hist_bins));
        if (hist_bins==NULL) {
	    printf("Insufficient memory.\n");
	    return;
        }
        vosread(unitnum,hist_bins,1,j+=sizeof(hist_sum2),1,
            hist_nbins*sizeof(*hist_bins));
        j+=hist_nbins*sizeof(*hist_bins);
        draw_hist();
    }
    vosclose(unitnum);
    vosdelete("spam.ses");
}


/******************************************************************************
    get_work - subroutine does most of work common to get and restore
******************************************************************************/
get_work(lut,file,isrestore)
short int *lut;
char *file;
int isrestore;
{
    long int i;
    int fd,ll,nbytes;
    short int c,c10=10,c18=18,c40=40,c255=255,t1,t2;
    float f1=1.0;
    unsigned char *calloc(),*malloc(),buf[20];
/*
/*      remove old image from memory, if any.
*/
    if (img_in_core) {				/* remove old image from mem. */
        free(img_name);
        cfree(img_data);
        free(hamm_ampbits);
        free(hamm_slopebits);
        cfree(zerox_base);
	free(img_currband);
        img_in_core=0;
    }
/*
/*      encode spectral libraries
*/
    if (libcode()) return(1);
/*
/*      save image name for future keeps
*/
    img_name=(char *)malloc(strlen(file)+1);
    if (img_name==NULL) {
	printf("Insufficient memory.\n");
	return(1);
    }
    strcpy(img_name,file);
/*
/*	Read image part in.  Only user-defined part of the image read in.
*/
    img_data = calloc(img_nl*img_ns/4+1,4);       /* alloc global img area.   */
    if (img_data == NULL) {
        printf("Insufficient memory.\n");
        freemem(0);
        return(1);
    }
    fd = vosopen(img_name,0,img_nl,img_ns);
    if (fd==-1) {
        printf("Inconsistent file I/O.  Consult programmer.\n");
        exit();
    }
    nbytes=vosread(fd,img_data,img_sl,img_bw*(img_sb-1)+1,img_nl,img_ns);
    if (nbytes==-1) {
	printf("Couldn't read entire image.\n");
        vosclose(fd);
	freemem(1);
	return(1);
    }
    vosclose(fd);
    if (img_modified) vosdelete(img_name);
/*
/*      trace window values.
*/
    printf("window :\n");
    printf("Lines %4d-%-4d  (%d lines)\n",
        img_sl,img_el,img_nl);
    printf("Bands %4d-%-4d  (%d bands, %d pixels per band)\n",
        img_sb,img_eb,img_numchan,img_bw);
/*
/*      Allocate space for binary encodings and encode using imgcode().
*/
    hamm_ampbits = malloc(img_nl*img_bw*((img_numchan+7)/8));
    if (hamm_ampbits == NULL) {
        printf("Insufficient memory.\n");
        freemem(1);
        return(1);
    }
    hamm_slopebits = malloc(img_nl*img_bw*((img_numchan+7)/8));
    if (hamm_slopebits == NULL) {
        printf("Insufficient memory.\n");
        freemem(2);
        return(1);
    }
    zerox_base = (short int *)calloc(img_nl*img_bw*2,2);
    if (zerox_base == NULL) {
        printf("Insufficient memory.\n");
        freemem(3);
        return(1);
    }
    imgcode();					/* do binary coding of image  */
/*
/*      update look-up tables and draw axes (and plots for restore)
*/
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][0];
    vdlutwri(&unit,&c1,&c1,lut);
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][1];
    vdlutwri(&unit,&c2,&c1,lut);
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][2];
    vdlutwri(&unit,&c3,&c1,lut);
/*
/*      display image strip.
*/
    img_currband=calloc(img_bw*img_nl/4+1,4);   /* allocate mem for band.     */
    if (img_currband==NULL) {
        printf("Insufficient memory.\n");
	freemem(4);
        return(1);
    }
    if (isrestore) {
	fd = vosopen("spam.lst",0,img_nl,img_bw);
        if (fd!=-1) {
            nbytes=vosread(fd,img_currband,1,1,img_nl,img_bw);
            vosclose(fd);
	    vosdelete("spam.lst");
	}
	else {
	    printf("Couldn't find file containing current band colorings.\n");
	    printf("Band may not be marked correctly.\n");
            extract(disp_band,disp_band,img_currband);
	}
    }
    else extract(disp_band,disp_band,img_currband);  /* get img band->memory. */
    t1=img_bw;                                  /* display on left of screen. */
    t2=img_nl;
    i=img_nl*img_bw;
    vdareawr(&unit,&c4,&i,img_currband,&c1,&c1,&t1,&t2);
    sprintf(buf,"%-3d",disp_band+img_sb-1);     /* write number of first band */
    c=strlen(buf);				/* to screen.                 */
    vdtxtsiz(&c10,&f1);
    t1=40+(img_bw-32);
    vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&c18,&c1,&c,buf);
    vdflush(&unit);
    img_in_core = 1;                            /* set img mem alloc flag.    */
    return(0);
}


/****************************************************************************/
/* freemem - frees memory according to specified level of allocation        */ 
/****************************************************************************/
freemem(level)
int level;
{
    switch (level) {
        case 4:
	    cfree(zerox_base);
        case 3:
	    free(hamm_slopebits);
        case 2:
	    free(hamm_ampbits);
        case 1: 
            cfree(img_data);
        case 0:
            free(img_name);
            break;
        default:
            printf("Illegal memory allocation level in get.c.  ");
            printf("Consult programmer.\n");
    }
}
