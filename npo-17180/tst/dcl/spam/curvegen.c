/*********************************************************************
   %M% %I% - Plots cursor-defined curve using splines.
*********************************************************************/
#include <stdio.h>
#include "spam.h"
curvegen(buf)
unsigned char *buf;
{
    float x[256],a[256],b[256],c[256],d[256];
    int i,x_pos,y_pos,bn;
    int n,j,graph_area=1;
    float value_at();
    char name[80];
    unsigned char *data,*malloc();
    static struct parameter kwds[]={
        {"name",2,0,1,"curve name (default is CURVEGENn) ","",80,0},
    };
    if (plots_num==0) {
        printf("You must have plots up to use the curvegen command.\n");
        return;
    }
    if (plots_num==16) {
        printf("There aren't any colors left.  Please erase a plot.\n");
        return;
    }
    if (plots_norm_mode==2) {
        printf("You must be using amplitude normalization to do");
        printf(" curvegens.\n");
        return;
    }
    if (display_mode==1) {
        printf("No curvegen's in display mode.  Use the \"return\"");
        printf(" command to exit.\n");
        return;
    }
    kwds[0].value = name;
    if (par(buf,kwds,1)) return;
    if (name[0]!='\0') while (check_new_name(name)) {
        query("New name (RETURN for READY) ? ",name);
        if (name[0]=='\0') return;
    }
    else {
	i=0;
	while (i>=0) {
	    sprintf(name,"CURVEGEN%d",++img_generic);  /* default name       */
	    get_plot_num(name,1,&i);
	}
    }
    n = -1;
    data = malloc(img_numchan);
    if (data==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    cursor_nl=1;                            /* change cursor to cross.    */
    cursor_ns=1;
    vdcuroff(&unit,&c1);                    /* turn cursor off.           */
    vdcuron(&unit,&c1,&c0,&c0);             /* turn on new cursor.        */
    printf("Select each point using button 1.  Use button 4 to quit.\n");
    get_cursor_position(&bn,&x_pos,&y_pos,graph_area,0x9);  /* get cursor pos */
    while (bn!=4 && n<255) {                        /* while not done...  */
        printf("Okay.\n");
        x[++n]=(x_pos-plots_hoff)*(img_ewl-img_swl)/plots_width+img_swl;
        a[n]=(plots_voff-y_pos)/(float)plots_height*(plots_maxy-plots_miny)
            +plots_miny+.5;
        get_cursor_position(&bn,&x_pos,&y_pos,graph_area,0x9);
    }
    printf("Okay.  One moment please . . .\n");
    if (n==255) printf("Curvegens are limited to 256 points.\n");
    spline_fit(x,n,a,b,c,d);        /* do spline fit, get coefficients.   */
    for (i=0;i<img_numchan;i++)     /* evaluate at img_numchan points.    */
        data[i]=min(
            max((int)value_at(i*(img_ewl-img_swl)/(img_numchan-1)+img_swl,
            x,n,a,b,c,d),0),
		255);
    save_plot_data(name,data,2,0,0,0,0,-1,0);  /* and plot (2 = curvegen).  */
    draw_plots();
    free(data);
}
