/*********************************************************************
   waveleng.c 2.14 - Returns wavelength of point within graph area. 
*********************************************************************/
#include <stdio.h>
#include "spam.h"
wavelength(buf)
unsigned char *buf;
{
    int i,x_pos,y_pos,bn,graph_area=1,band;
    float wavelen;
    if (plots_num==0) {
        printf("You must have plots up to use the wavelength command.\n");
        return;
    }
    if (display_mode==1) {
        printf("You can't use the wavelength command in display mode.\n");
	printf("Use the \"return\" command to exit.\n");
        return;
    }
    cursor_nl=1;                            /* change cursor to cross.    */
    cursor_ns=1;
    vdcuroff(&unit,&c1);                    /* turn cursor off.           */
    vdcuron(&unit,&c1,&c0,&c0);             /* turn on new cursor.        */
    printf("Select each point using button 1.  Use button 4 to quit.\n");
    get_cursor_position(&bn,&x_pos,&y_pos,graph_area,0x9);  /* get cursor pos */
    while (bn!=4) {                                 /* while not done...  */
        wavelen = (float)(x_pos-plots_hoff)/plots_width*(img_ewl-img_swl)+
	    img_swl+0.0005;
        band = (float)(x_pos-plots_hoff)/plots_width*(img_numchan-1)+
	    (img_in_core? img_sb:1)+0.5;
	printf("Wavelength = %2.2f microns  (Band %d)\n",wavelen,band);
        get_cursor_position(&bn,&x_pos,&y_pos,graph_area,0x9);  /* get next. */
    }
}
