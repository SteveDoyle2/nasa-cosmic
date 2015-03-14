/***************************************************************************
   snapshot.c 2.14 - Saves current screen to user-given rgb files on the disk.
***************************************************************************/
#include <stdio.h>
#include "spam.h"
snapshot(buf)
unsigned char *buf;
{
    int unit,j,k,l,text_flag;
    long int n;
    float f1=1.0;
    short int c512=512,tx,ty,tlen,c11=11,t;
    unsigned char *malloc(),*image_plane,*hist_plane;
    unsigned char **graphics_pix_locs,*graphics_pix_vals,lut[256];
    register unsigned char **p,**q,*r,*s;
    register int i;
    char file[70],ofile[80],text[80];
    static struct parameter kwds[]={
        {"filename",2,1,1,"filename","",70,0},
        {"text",2,0,1,"snapshot caption","",70,0},
        };
    kwds[0].value = file;
    kwds[1].value = text;
    kwds[1].input_count = &text_flag;
    if (par(buf,kwds,2)) return;     /* get filename for output image   */
    tx=plots_hoff;		     /* make sure caption area is clear */
    t=plots_hoff+plots_width;
    ty=plots_voff-plots_height-3;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&tx,&c1,&t,&ty);
    if (text_flag) {
	vdtxtsiz(&c11,&f1);
	tlen=strlen(text);
	if (tlen>30) {
	    printf("Snapshot text must be no more than 30 characters long.\n");
	    return;
	}
	tx=plots_hoff+plots_width/2-5.25*tlen;
        if (tx<plots_hoff) tx=plots_hoff;
	ty=15;
	upper_case(text);
	if (!display_mode) vdtext(&unit,&c4,&c3,&c0,&c0,&tx,&ty,&c1,&tlen,text);
	else vdtext(&unit,&c2,&c0,&c3,&c0,&tx,&ty,&c1,&tlen,text);
    }
/*
/*      Allocate space for actual data from Rastertek and graphics pixel lists.
*/
    image_plane=malloc(512*512);
    if (image_plane==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    if (hist_on) {
        hist_plane=malloc(512*512);
        if (hist_plane==NULL) {
            printf("Insufficient memory.\n");
            free(image_plane);
	    return;
        }
    }
    graphics_pix_locs=(unsigned char **)malloc(512*512*4);
    if (graphics_pix_locs==NULL) {
        printf("Insufficient memory.\n");
	free(image_plane);
        if (hist_on) free(hist_plane);
	return;
    }
    graphics_pix_vals=malloc(512*512);
    if (graphics_pix_vals==NULL) {
        printf("Insufficient memory.\n");
	free(image_plane);
        if (hist_on) free(hist_plane);
	free(graphics_pix_locs);
	return;
    }
    if (!hist_on) printf("This will take about 35 seconds . . .\n");
    else printf("This will take about 70 seconds . . .\n");
/*
/*      To understand how all this works, here is a brief rundown on how data is
/*      stored for display.  Graphics and image data are stored in red plane.
/*      Dn values 1 through 16 are used with the color luts to store the
/*      graphics; image dn values less than 17 are raised to 17 before display.
/*
/*      Read red plane (where plots and such are stored).  Locations represented
/*      by graphics_pix_ pointers are for scratch space only.  Read the blue
/*      plane to get copy of current histogram, if any.
*/
    n = 512*512;
    if (display_mode==0) vdareard(&unit,&c4,&n,image_plane,graphics_pix_locs,
        graphics_pix_vals,&c1,&c1,&c512,&c512);
    else vdareard(&unit,&c2,&n,graphics_pix_locs,image_plane,graphics_pix_vals,
	&c1,&c1,&c512,&c512);
    if (hist_on) vdareard(&unit,&c1,&n,graphics_pix_locs,graphics_pix_vals,
        hist_plane,&c1,&c1,&c512,&c512);
/*
/*      If the image has been stretched, we have to adjust the whole plane to   
/*      make it as it appears on the display.  (We are still dealing only with
/*      the single image plane read from the Rastertek.)  So map all image 
/*      pixels (dn >=17) to what they are according to the stretch, but make
/*      sure that they are all at least dn 17.
*/
    if (stretch_start!=0 || stretch_end!=255) {
        for (i=0;i<=stretch_start;i++) lut[i]=0;
        for (i=stretch_start+1;i<stretch_end;i++)
	    lut[i]=max(255*(i-stretch_start)/(stretch_end-stretch_start),17);
        for (i=stretch_end;i<256;i++) lut[i]=255;
	s=image_plane-1;
	i=512*512;
	while (i-- > 0) if (*++s>16) *s=lut[*s];
    }
/*
/*      If there is a histogram up, get the pixels from the blue plane read
/*      in earlier, and set the corresponding bits in the red plane to the
/*      lut value for red (3).  These will be converted to the actual red
/*      values when the other graphics pixels are converted below.
*/
    if (hist_on) {
        r=image_plane;
        s=hist_plane+512*512-1;
        for (i=512*512-1;i>=0;i--) if (*s--!=0) *(r+i)=3;
    }
/*
/*      Finally, before writing the planes out, make a list of the pixels
/*      which are used for graphics.  This allows us to directly modify
/*      just those pixels within the image, so that we can create each
/*      component plane of the final rgb image by substituting correct lut value
/*      for the plane (r,g,or b) and the graphics pixel value and then write the
/*      component plane out.
/*          This scheme is implemented by creating for the graphics pixels 
/*      a list of locations and a list of values.  At the end of this next
/*      section of code, q will point to one location past the end of the
/*      list.
*/
    s = image_plane-1;
    i = 512*512;
    q = graphics_pix_locs;
    r = graphics_pix_vals;
    while (i-- > 0) if (*++s>0 && *s<17) {
	*q++ = s;
	*r++ = *s;
    }
/*
/*      Now we create the red plane of the output image.  For every
/*      graphics pixel, get the actual dn value in the red-plane (as
/*      displayed through the lut) and write to the pixels location in
/*      our "image".  (Recall that q points to one location past the end
/*      of the list.)  Pixels not in the list of graphics pixels aren't
/*      touched.  Finally, write image out to the disk.
*/
    p = graphics_pix_locs-1;
    r = graphics_pix_vals;
    while (++p<q) **p = color_table[(*r++)-1][0];
    sprintf(ofile,"%s.r",file);
    if ((unit=vosopen(ofile,1,512,512))==-1) printf(
	"Illegal file name (%s) or other error opening file.  Continuing.\n",
        ofile);
    else if (voswrite(unit,image_plane,1,1,512,512)==-1) printf(
	"Can't write the whole file for red plane.  Starting green plane.\n");
    vosclose(unit);
/*
/*      Repeat for green plane.
*/
    p = graphics_pix_locs-1;
    r = graphics_pix_vals;
    while (++p<q) **p = color_table[(*r++)-1][1];
    sprintf(ofile,"%s.g",file);
    if ((unit=vosopen(ofile,1,512,512))==-1) printf(
	"Illegal file name (%s) or other error opening file.  Continuing.\n",
        ofile);
    else if (voswrite(unit,image_plane,1,1,512,512)==-1) printf(
	"Can't write the whole file for green plane.  Starting blue plane.\n");
    vosclose(unit);
/*
/*      Repeat for blue plane.
*/
    p = graphics_pix_locs-1;
    r = graphics_pix_vals;
    while (++p<q) **p = color_table[(*r++)-1][2];
    sprintf(ofile,"%s.b",file);
    if ((unit=vosopen(ofile,1,512,512))==-1) printf(
	"Illegal file name (%s) or other error opening file.  Continuing.\n",
        ofile);
    else if (voswrite(unit,image_plane,1,1,512,512)==-1) printf(
	"Can't write the whole file for blue plane.\n");
    vosclose(unit);
/*
/*      Remove the caption, if any, and clean up.
*/
    if (text_flag) {
        tx=plots_hoff;
        t=plots_hoff+plots_width;
        ty=plots_voff-plots_height-3;
        if (!display_mode) vdareafi(&unit,&c4,&c0,&c0,&c0,&tx,&c1,&t,&ty);
    }
    free(image_plane);
    if (hist_on) free(hist_plane);
    free(graphics_pix_locs);
    free(graphics_pix_vals);
    vdflush(&unit);
}
