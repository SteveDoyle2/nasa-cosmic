/*********************************************************************
   plotsubs.c 2.15 - save_plot_data takes data and stores in next available
   location in structure plots.  The graph area may then be redrawn
   using draw_plots().  Erase_plot() erases plots.
*********************************************************************/
#include <stdio.h>
#include "spam.h"
save_plot_data(curve_name,data,plottype,lloc,sloc,avgnl,avgns,color,variance)
unsigned char *curve_name,*data,*variance;
int plottype,lloc,sloc,avgnl,avgns,color;
{
    float xfact,yfact,temp;
    unsigned char *malloc(),*p;
    int i,j;
/*
/*  check for all-zero data
*/
    i=0;
    while (i<img_numchan && data[i]==0) i++;
    if (i==img_numchan) {
	printf("Can't plot all-zero data.\n");
	return(1);
    }
/*
/*      if color wasn't specified, find empty plots[] entry.
*/
    if (color==-1) {
        for (i=0;i<16 && plots[i].name!=0;i++) ;
        if (i==16) {
            printf("There aren't any more colors available.  ");
	    printf("Please erase a plot.\n");
            return(1);
        }
	plots_num++;
    }
    else {
	i=color;
	if (plots[i].name != 0) {
	    free(plots[i].name);
	    plots[i].name = 0;
            free(plots[i].data);
            free(plots[i].variance);
	}
	else plots_num++;
    }
/*
/*      fill entry with plot stats.
*/
    if (strlen(curve_name)>16) curve_name[16] = '\0';
    upper_case(curve_name);
    p=malloc(strlen(curve_name)+1);                  /* store name in struct */
    if (p==NULL) {
        printf("Insufficient memory.\n");
        return(1);
    }
    plots[i].name = p;
    strcpy(plots[i].name,curve_name);
    plots[i].data = malloc(img_numchan);
    if (plots[i].data==NULL) {
        printf("Insufficient memory.\n");
        free(plots[i].name);
        plots[i].name=0;
        return(1);
    }
    plots[i].variance = malloc(img_numchan);
    if (plots[i].variance==NULL) {
        printf("Insufficient memory.\n");
        free(plots[i].name);
        free(plots[i].data);
        plots[i].name=0;
        return(1);
    }
    for (j=0;j<img_numchan;j++) plots[i].data[j] = data[j];
    if (variance!=0)
        for (j=0;j<img_numchan;j++) plots[i].variance[j] = variance[j];
    else for (j=0;j<img_numchan;j++) plots[i].variance[j]=0;
    plots[i].mindn=255;
    plots[i].maxdn=0;
    plots[i].sum=0;
    for (j=0;j<img_numchan;j++) {
        if (plots[i].data[j]-plots[i].variance[j]<plots[i].mindn)
            plots[i].mindn=max(0,plots[i].data[j]-plots[i].variance[j]);
        if (plots[i].data[j]+plots[i].variance[j]>plots[i].maxdn)
            plots[i].maxdn=min(255,plots[i].data[j]+plots[i].variance[j]);
        plots[i].sum+=plots[i].data[j];
    }
    plots[i].plottype=plottype;	/* 0=dataplot,1=libplot,2=curvegen,3=func  */
    plots[i].data_loc[0]=lloc;	/* for dataplots: line of pt or lr-corner  */
    plots[i].data_loc[1]=sloc;	/* of enclosed area; corresponding sample. */
    plots[i].avg_dims[0]=avgnl;	/* for dataplots: number of lines in area. */
    plots[i].avg_dims[1]=avgns;	/* number of corresponding samples.        */
/*
/*      for first plot on screen, init scaling variables
*/
    if (plots_num==1) {
	plots_mindn=255;
	plots_maxdn=0;
    }
/*
/*      for amplitude normalization, update plots_mindn, _maxdn only.
*/
    if (plots_norm_mode==1) {
        if (plots[i].mindn<plots_mindn) plots_mindn=plots[i].mindn;
        if (plots[i].maxdn>plots_maxdn) plots_maxdn=plots[i].maxdn;
        temp=1.0;	/* this is non-1 only for area normalization */
    }
/*
/*      for area normalization, normalize data within array data only,
/*      and then update plots_mindn, _maxdn.
*/
    else {
        if (plots_num==1) plots_norm = (200*plots[i].sum)/(2*plots[i].maxdn);
        temp=plots_norm/plots[i].sum;
        if (plots[i].mindn*temp < plots_mindn) plots_mindn=plots[i].mindn*temp;
        if (plots[i].maxdn*temp > plots_maxdn) plots_maxdn=plots[i].maxdn*temp;
    }
    return(0);
}


/****************************************************************************/
/* draw_plots - subroutine erases plots and scaling area and redraws        */
/*     according to current values of plots_mindn, _maxdn, and _num.        */
/****************************************************************************/
draw_plots()
{
    float xfact,yfact,temp;
    short int t1,t2,t3,t4,c,r,g,b,c255=255;
    short int *x,*y,*calloc(),barx[2],bary[2];
    float f1=1.;
    int i,j,err=0;
    x = calloc(max(img_numchan,5),2);
    if (x==NULL) {
        printf("Insufficient memory.\n");
        return;
    }
    y = calloc(max(img_numchan,5),2);
    if (y==NULL) {
        printf("Insufficient memory.\n");
        cfree(x);
        return;
    }
    t1=plots_hoff+1;				/* erase graph area.        */
    t2=plots_voff-plots_height+1;
    t3=plots_hoff+plots_width-1;
    t4=plots_voff-1;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&t2,&t3,&t4);
    t1=plots_hoff-25;				/* erase left-hand tick     */
    t2=plots_voff-plots_height-5;		/* and scale.               */
    t3=plots_hoff-1;
    t4=plots_voff+5;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&t2,&t3,&t4);
    t1=plots_hoff+plots_width+1;		/* erase right-hand tick    */
    t2=plots_voff-plots_height;			/* marks and color legend.  */
    t3=512;
    t4=plots_voff;
    vdareafi(&unit,&c4,&c0,&c0,&c0,&t1,&t2,&t3,&t4);
    plots_miny = min(245,max(0,plots_mindn-5));
    plots_maxy = max(10,min(255,plots_maxdn+5));
    number_vert();				/* do vertical scaling.     */
    for (i=0;i<16;i++) {		/* for each possible plot . . .     */
        if (plots[i].name!=0) {		/* if plot entry has current data,  */
    	    temp=1.0;			/* set area-norm factor (1=ampnorm) */
            if (plots_norm_mode==2) temp=plots_norm/plots[i].sum;
            yfact=((float)plots_height)/(plots_maxy-plots_miny);
            xfact=((float)plots_width)/(img_numchan-1);
            x[0]=plots_hoff;
            y[0]=plots_voff-yfact*(temp*plots[i].data[0]-plots_miny)+.5;
            for (j=1;j<img_numchan;j++) {
		x[j]=plots_hoff+xfact*j;	/* & calculate plot points. */
                y[j]=plots_voff-yfact*(temp*plots[i].data[j]-plots_miny)+.5;
		if (y[j]<plots_voff-plots_height) {
		    y[j]=plots_voff-plots_height;
		    if (err==0)
			printf("Error fitting plot within graph area.\n");
		    err=1;
		}
	    }
	    t1=img_numchan;			/* draw plot using vector-  */
            r=i+1;				/* graphics primitive.      */
            vdvector(&unit,&c4,&r,&c0,&c0,&t1,x,y);
            if (plots[i].plottype==0) {         /* draw bars and avg box.   */
                for (j=1;j<img_numchan-1;j++) {
                    barx[0]=x[j];
                    barx[1]=x[j];
                    bary[0]=max(plots_voff-plots_height,(int)(y[j]-yfact*temp*
                        (plots[i].variance[j])));
                    bary[1]=min(plots_voff,(int)(y[j]+yfact*temp*
                        (plots[i].variance[j])));
                    vdvector(&unit,&c4,&r,&c0,&c0,&c2,barx,bary);
                }
		x[0]=plots[i].data_loc[0]+1;
		y[0]=min(512,plots[i].data_loc[1]+1);
		x[1]=x[0];
                y[1]=max(1,y[0]-plots[i].avg_dims[0]-1);
                x[2]=max(1,x[0]-plots[i].avg_dims[1]-1);
                y[2]=y[1];
                x[3]=x[2];
		y[3]=y[0];
                x[4]=x[0];
		y[4]=y[0];
                vdvector(&unit,&c4,&r,&c0,&c0,&c2,&x[0],&y[0]);
                if (y[1]==y[0]-plots[i].avg_dims[0]-1)
                    vdvector(&unit,&c4,&r,&c0,&c0,&c2,&x[1],&y[1]);
                if (x[2]==x[0]-plots[i].avg_dims[1]-1)
                    vdvector(&unit,&c4,&r,&c0,&c0,&c2,&x[2],&y[2]);
                if (y[0]==plots[i].data_loc[1]+1)
                    vdvector(&unit,&c4,&r,&c0,&c0,&c2,&x[3],&y[3]);
            }
	    t1=plots_hoff+plots_width+12;	/* write plot name in      */
            t2=plots_voff-plots_height+10+i*12;	/* plot color.             */
	    c=strlen(plots[i].name);
            vdtext(&unit,&c4,&r,&c0,&c0,&t1,&t2,&c1,&c,plots[i].name);
        }
    }
    cfree(x);
    cfree(y);
    axes();
    number_horiz();
    vdflush(&unit);
}



/****************************************************************************/
/* erase_plot - subroutine removes specified plot from the plots table.     */
/*      draw_plots() must be called to remove erased plots from the screen. */
/****************************************************************************/
erase_plot(i)		                    /* erase plot stored in entry i. */
int i;
{
    int j;
    short int t1,t2,t3,t4,c,x[5],y[5];
    float f1=1.;
    if (plots[i].plottype==0) {	    /* if dataplot, erase box from ais strip */
	x[0]=plots[i].data_loc[0]+1;
	y[0]=min(512,plots[i].data_loc[1]+1);
	x[1]=x[0];
        y[1]=max(1,y[0]-plots[i].avg_dims[0]-1);
        x[2]=max(1,x[0]-plots[i].avg_dims[1]-1);
        y[2]=y[1];
        x[3]=x[2];
	y[3]=y[0];
        x[4]=x[0];
	y[4]=y[0];
        vdvector(&unit,&c4,&c0,&c0,&c0,&c2,&x[0],&y[0]);
        if (y[1]==y[0]-plots[i].avg_dims[0]-1)
            vdvector(&unit,&c4,&c0,&c0,&c0,&c2,&x[1],&y[1]);
        if (x[2]==x[0]-plots[i].avg_dims[1]-1)
            vdvector(&unit,&c4,&c0,&c0,&c0,&c2,&x[2],&y[2]);
        if (y[0]==plots[i].data_loc[1]+1)
            vdvector(&unit,&c4,&c0,&c0,&c0,&c2,&x[3],&y[3]);
    }
    free(plots[i].name);                          /* free allocated space    */
    plots[i].name = 0;				  /* mark color as available */
    free(plots[i].data);
    plots_num--;				  /* one less plot on screen */
    vdflush(&unit);
}


/******************************************************************************/
/* axes - draw axes                                                           */
/******************************************************************************/
axes()
{
    int i,j;
    short int x[5],y[5],c255=255;
    x[0]=plots_hoff;                    /* draw white rectangle in all planes */
    x[1]=x[0];
    x[2]=plots_hoff+plots_width;
    x[3]=x[2];
    x[4]=x[0];
    y[0]=plots_voff-plots_height;
    y[1]=plots_voff;
    y[2]=y[1];
    y[3]=y[0];
    y[4]=y[0];
    vdvector(&unit,&c4,&c255,&c0,&c0,&c5,x,y);
    for (i=0;i<17;i++) { /* data, write  */
        j = plots_hoff+(plots_width*i)/16;             /* tick marks and */
        x[0]=j;                                         /* band numbers.  */
        x[1]=x[0];
        y[0]=plots_voff+1;
        y[1]=plots_voff+3;
        vdvector(&unit,&c4,&c255,&c0,&c0,&c2,x,y);  /* (tick mark)    */
        y[0]=plots_voff-plots_height-1;
        y[1]=plots_voff-plots_height-3;
        vdvector(&unit,&c4,&c255,&c0,&c0,&c2,x,y);  /* (tick mark)    */
    }
}

/**************************************************************************/
/*       number_vert: numbers vertical (y) axis                           */
/**************************************************************************/
number_vert()
{
    int i,j,f;
    unsigned char buf[80];
    short int x[2],y[2],c2=2,c3=3,c7=7,c,t1,t2,c255=255;
    float f1=1.0;
    j=0;
    vdtxtsiz(&c7,&f1);			/* set normal (7-line) text size. */
    if (plots_num!=0)
        for (f=plots_maxy;f>=plots_miny;f-=(plots_maxy-plots_miny)/10) {
            i=plots_voff-(float)(plots_height*(f-plots_miny))/
                (plots_maxy-plots_miny)+.5;
            if (plots_norm_mode==1) {
		t1=plots_hoff-25;	/* if amp norm-mode, write out    */
		t2=i+3;			/* scale.                         */
                sprintf(buf,"%3d",f);
                vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&t2,&c1,&c3,buf);
            }
	    x[0]=plots_hoff-5;		/* draw left-hand tick marks.     */
	    y[0]=i;
	    x[1]=plots_hoff;
	    y[1]=y[0];
            vdvector(&unit,&c4,&c255,&c255,&c255,&c2,x,y);
            x[0]=plots_hoff+plots_width;/* draw right-hand tick marks.    */
	    x[1]=x[0]+4;
            vdvector(&unit,&c4,&c255,&c255,&c255,&c2,x,y);
        }
}


/******************************************************************************/
/* number_horiz - write out horizontal scale based on image label.            */
/******************************************************************************/
number_horiz()
{
    int i,j;
    unsigned char buf[80];
    short int x[5],y[5],c2=2,c3=3,c5=5,c7=7,c255=255;
    short int c,t1,t2;
    float f1=1.;
    vdtxtsiz(&c7,&f1);
    for (i=0;i<17;i+=2) {
        t1=plots_hoff+(plots_width*i)/16-14;
        t2=plots_voff+14;
	sprintf(buf,"%2.2f",img_swl+(img_ewl-img_swl)/16.*i+.0005);
        c=strlen(buf);
        vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&t2,&c1,&c,buf);
        if (img_bw>128) i+=2;
    }
    strcpy(buf,"WAVELENGTH (MICRONS)");
    c=strlen(buf);
    t1=plots_hoff+(plots_width-6*c)/2;
    t2=plots_voff+26;
    vdtext(&unit,&c4,&c255,&c255,&c255,&t1,&t2,&c1,&c,buf);
}
