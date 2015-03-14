/*************************************************************************
   init.c 2.14 - Performs initialization of globals, tables, and display.
*************************************************************************/
#include <stdio.h>
#include <signal.h>
#include "spam.h"
init_prog_data()
{
    int err;
    init_globals();
    gen_tables();		/* calculate hammdist look-up table */
    err = read_spectral_libraries();
    return(err);
}


init_device()
{
    char logical_one=1,logical_zero=0;
    short int config1[4],config2[4],i,c255=255,c512=512,c15=15,lut[256];
    for (i=0;i<4;i++) {
	config1[i]=0;
	config2[i]=1;
    }
    vddevall(&unit,config1);		/* allocate display device.           */
    vddevope(&unit);			/* open device.                       */
    vddevact(&unit,&logical_one);	/* activate (see vdi.c).              */
    vddevcon(&unit,config2);		/* configure for 512x512 rgb+overlay. */
    for (i=1;i<=3;i++) vdzoom(&unit,&i,&c1);	/* reset zoom.                */
    vdareafi(&unit,&c15,&c0,&c0,&c0,&c1,&c1,&c512,&c512);   /* clear rgb & o. */
    vddispln(&unit,&c4);		/* display only red plane (default)   */
    vdcuron(&unit,&c1,&c0,&c0);			/* enable cursor tracking.    */
    for (i=0;i<256;i++) lut[i]=i;			/* set up lut tables  */
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][0];
    vdlutwri(&unit,&c1,&c1,lut);
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][1];
    vdlutwri(&unit,&c2,&c1,lut);
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][2];
    vdlutwri(&unit,&c3,&c1,lut);
    return(0);
}

reinit()
{
    vdcuroff(&unit,&c1);		/* disable cursor tracking.     */
    vdovroff(&unit);			/* turn off overlay plane.      */
    vddispln(&unit,&c7);		/* display all planes again.    */
    vddevfre(&unit);			/* deallocate device.           */
    if (user_lib_changed) libr_write(spectral_lib_name,user_lib);
    if (session_log!=NULL) fclose(session_log);
}

/***************************************************************************/
/* init_globals - initialize scalar variables.                             */
/***************************************************************************/
init_globals()
{
    img_in_core=0;
    img_generic=0;
    img_name=0;
    img_swl=0.0;
    img_ewl=0.0;
    plots_num=0;
    plots_norm_mode=1;
    plots_hoff=77;
    plots_voff=315;
    plots_width=310;
    plots_height=285;
    cursor_nl=1;
    cursor_ns=1;
    display_mode=0;
    disp_band=1;
    disp_startband=0;
    disp_endband=0;
    hist_on=0;
    hist_nbins=0;
    stretch_start=0;
    stretch_end=255;
    user_lib_changed=0;
    session_log = NULL;
    feature_range = 0;
    c0=0;
    c1=1;
    c2=2;
    c3=3;
    c4=4;
    c5=5;
    c6=6;
    c7=7;
    unit=1;
}

/***************************************************************************/
/* gen_tables - make tables used by spam.                                  */
/***************************************************************************/
gen_tables()
{
    register i,j,k;
/*
/*      Make table containing colors used by spam.
/*      (These values can't be initialized in the include file because of
/*      multiple-definition errors.  They were originally defined outside
/*      of the subroutines in this module, at the cost of excluding
/*      spam.h.  But spam.h is now needed in order to use the installation-
/*      definitions of the spectral library name and location.)
*/
    static int colors[16][3]={{0,255,255},{255,255,0},{255,0,0},
        {255,0,255},{0,255,0},{255,128,0},{255,128,128},
        {0,255,128},{255,255,128},{255,0,128},{255,255,255},{120,60,0},
        {128,128,128},{128,128,0},{0,128,0},{0,128,128}};
    for (i=0;i<16;i++) for (j=0;j<3;j++) color_table[i][j]=colors[i][j];
/*
/*      Create hamm_lut, 256 by 256 table of Hamming distances.  Any two bytes
/*      differ in hamm_lut[byte1][byte2] bit positions.  Algorithm by
/*      David Sirag, Jr.
*/
    hamm_lut[0][0]=0;
    for (i=1;i<256;i<<=1) {
        for (j=0;j<i;j++) {
            for (k=0;k<i;k++) {
                hamm_lut[j+i][k+i] = hamm_lut[j][k];
                hamm_lut[j][k+i] = hamm_lut[j][k]+1;
                hamm_lut[j+i][k] = hamm_lut[j][k]+1;
            }
        }
    }
}

/******************************************************************************/
/* read_spectral_libraries - stores list of libplot datasets at speclib_names */
/******************************************************************************/
read_spectral_libraries()
{
    unsigned char library_name[80];
    strcpy(library_name,image_lib_dir);
    strcat(library_name,spectral_lib_name);
    if (libr_get(library_name,&master_lib)==-1) return(-1);
    strcpy(library_name,spectral_lib_name);
    if (libr_get(library_name,&user_lib)==-1) return(-1);
    return(0);
}
