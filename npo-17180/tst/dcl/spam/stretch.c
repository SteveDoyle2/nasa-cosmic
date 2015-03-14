/************************************************************************
   stretch.c 2.14 - stretch the image using frame buffer lookup tables. 
************************************************************************/
#include <stdio.h>
#include "spam.h"
stretch(buf)
unsigned char *buf;
{
    int val[2],def=0,j;
    short int c11=11,t1,t2;
    short int *lut;
    register long int i,t;
    register unsigned char min,maximum;
    register unsigned char *p;
    static struct parameter kwds[]={
        {"stretch",0,0,2,
	    "stretch values (default is to use min and max dn in image) ",
	    "",0,0}
        };
    if (img_in_core==0) {
	printf("No image has been selected.\n");
	return;
    }
    kwds[0].value = (char *) val;
    kwds[0].input_count = &def;
    if (par(buf,kwds,1)) return;                     /* get stretch vals.  */
    printf("One moment please . . .\n");
    lut=(short int *) calloc(256,2);		/* allocate memory for the */
    if (lut==NULL) {				/* lookup table setup.     */
	printf("Insufficient memory.\n");
        return;
    }
    if (def && (val[0]<0 || val[1]<0)) {
	printf("Negative stretch value(s).  Using min and max dn for image.\n");
	def=0;
    }
    else if (def && (val[1]>255 || val[1]<val[0])) {
	printf("Illegal stretch value(s).  Using min and max dn for image.\n");
	def=0;
    }
    if (def==0) {	/* stretch values were defaulted, so calculate     */
        min=255;	/* min and max dn and use these.                   */
        maximum=0;
        t=img_nl*img_ns;
	p=(unsigned char *)img_data;
        p--;
        for (i=0;i<t;i++) {
            if (*++p<min) min=(*p);
            else if (*p>maximum) maximum=(*p);
        }
        val[0]=min;
        val[1]=maximum;
    }						/* create lookup table.     */
    for (i=0;i<=val[0];i++) lut[i]=0;
    for (i=val[0]+1;i<val[1];i++) lut[i]=(float)(i-val[0])/(val[1]-val[0])*255;
    for (i=val[1];i<256;i++) lut[i]=255;
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][0];
    vdlutwri(&unit,&c1,&c1,lut);		/* modify look-up tables.   */
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][1];
    vdlutwri(&unit,&c2,&c1,lut);
    for (i=1;i<=16;i++) lut[i]=color_table[i-1][2];
    vdlutwri(&unit,&c3,&c1,lut);
    stretch_start = val[0];
    stretch_end = val[1];
    cfree(lut);					/* free lut setup area.      */
    vdflush(&unit);
}
