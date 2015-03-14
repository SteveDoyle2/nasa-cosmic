/**************************************************************************
   storespec.c 2.14 - Save spectra within user-defined area on disk
**************************************************************************/
#include <stdio.h>
#include "spam.h"
storespec(buf)
unsigned char *buf;
{
    int i,j,k,x,y,unitnum,loc[2],line,line_alloc;
    int def,plotloc_mode,error,button_number,not_done=1,image_area=0;
    unsigned char *chardata,*p,*spectra,*malloc(),*realloc();
    int area[2];
    static struct parameter kwds[]={
        {"area",0,0,2,
	    "box height and width (default is current cursor size) ",
	    "",0,0},
        {"location",0,0,2,"location in image (line, sample) ","",0,0},
        };
    if (img_in_core==0) {
        printf("No image selected.  For help, type \"help\".\n");
        return;
    }
    if (display_mode==1) {
        printf("No storespec's in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
        return;
    }
    kwds[0].value = (char *) area;
    kwds[0].input_count = &def;
    kwds[1].value = (char *) loc;
    kwds[1].input_count = &plotloc_mode;
    if (par(buf,kwds,2)) return;
    if (def!=0) { 
	while (area[0]<1 || area[0]>img_nl) {
	    printf("Illegal box size.  ");
	    query("Number of lines in area? ",buf);
	    sscanf(buf,"%d",&area[0]);
        }
	while (area[1]<1 || area[1]>img_bw) {
	    printf("Illegal box size.  ");
	    query("Number of samples in area? ",buf);
	    sscanf(buf,"%d",&area[1]);
        }
        cursor_nl=area[0];
        cursor_ns=area[1];			/* setup new cursor and     */
        vdcuroff(&unit,&c1);			/* then switch to new       */
        vdcuron(&unit,&c1,&c0,&c0);		/* definition.              */
    }
    if (plotloc_mode) {
        loc[0]-=img_sl-1;
	while (loc[0]-area[0]+1<1 || loc[0]>img_nl) {
	    printf("Spectral loc's line number is too small for this");
	    query(" image.  Line number? ",buf);
	    sscanf(buf,"%d",&loc[0]);
            loc[0]-=img_sl-1;
        }
	while (loc[1]-area[1]+1<1 || loc[1]>img_bw) {
	    printf("Spectral loc's sample number is too small for this image.");
	    query("  Sample number? ",buf);
	    sscanf(buf,"%d",&loc[1]);
        }
    }
    if (!plotloc_mode) {
        printf("For each image area, position the cursor and press\n");
        printf("button 1; to stop, use button 4.\n\n");
    }
    line = 0;
    spectra = malloc(100*img_numchan);
    if (spectra==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    p=spectra;
    line_alloc=100;
    chardata = (unsigned char *)img_data;
    while (not_done) {
	if (plotloc_mode) {
	    y=loc[0];
	    x=loc[1];
        }
        else {
	    get_cursor_position(&button_number,&x,&y,image_area,0x9);
            not_done=button_number!=4;
            if (not_done) printf("Line %4d    Sample %4d\n",y+img_sl-1,x);
        }
        vdcuroff(&unit,&c1);
        if (not_done) {
            while (line+cursor_nl*cursor_ns>line_alloc) {
                line_alloc*=2;
                spectra = realloc(spectra,2*line_alloc*img_numchan);
                if (spectra==NULL) {
                    printf("Insufficient memory.\n");
                    return;
                }
                p=spectra+line*img_numchan;
            }
            line+=cursor_nl*cursor_ns;
            for (j=y-cursor_nl;j<y;j++) for (k=x-cursor_ns;k<x;k++) 
		for (i=0;i<img_numchan;i++) *p++ =chardata[j*img_ns+img_bw*i+k];
        }
	if (plotloc_mode) not_done=0;
        vdcuron(&unit,&c1,&c0,&c0);
    }
    unitnum = vosopen("spam.spe",1,line,img_numchan);
    if (unitnum==-1) {
	printf("Can't open output file, \"spam.spe\".\n");
	return;
    }
    for (i=0;i<line;i++)
        voswrite(unitnum,spectra+i*img_numchan,i+1,1,1,img_numchan);
    vosclose(unitnum);
    free(spectra);
}
