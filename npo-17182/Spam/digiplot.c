/*********************************************************************
   digiplot.c 2.15 - Allows user to trace printed spectra using the
   digipad cursor.
*********************************************************************/
#include <stdio.h>
#include <math.h>
#include "spam.h"
struct point {			/* location with x and y components. */
    int x;
    int y;
};
digiplot(buf)
unsigned char *buf;
{
    struct point maxx,maxy,orig,pos;
    float x[512],y[512],f[512],a[512],b[512],c[512],d[512],temp;
    char *calloc(),*base;
    short int c11=11,c25=25,c100=100,c42=42,t1,t2;
    short int c512=512,c255=255,x_data[512],y_data[512];
    int i,j,k,l,m,n,bn;
    short int done=0,line,sample;
    int graph_area=2,index,libr,spline_mode;
    float maxxval,maxyval,minxval,minyval;
    float value_at(),dist(),correct();
    char name[80],spline[10];
    unsigned char data[512];
    static struct parameter kwds[]={
        {"name",2,1,1,"curve name","",80,0},
        {"spline",3,0,0,"cubic spline fit (default is continuous trace) ",
	    "",10,0},
    };
    if (display_mode==1) {
        printf("No curvegen's in display mode.  Use the \"return\"");
        printf(" command to exit.\n");
        return;
    }
    kwds[0].value = name;
    kwds[1].value = spline;
/*
/*      Get name into "name".  Determine sampling method.
*/
    if (par(buf,kwds,2)) return;
    while (check_new_name(name)) {
        query("New name (RETURN for READY) ? ",name);
  	if (name[0]=='\0') return;
    }
    if (spline[0]!='\0') spline_mode=1;
    else spline_mode=0;
/*
/*      Get orientation of hardcopy to be traced.
*/
    cursor_nl=1;                            /* change cursor to cross.    */
    cursor_ns=1;
    vdcuroff(&unit,&c1);                    /* turn cursor off.           */
    vdcuron(&unit,&c1,&c0,&c0);             /* turn on new cursor.        */
    printf("Place the spectral plot on the digi-pad; orientation\n");
    printf("isn't critical, but the plot shouldn't ever get closer to the\n");
    printf("left-hand side of the digipad as x increases.\n\n");
    printf("Then move the cursor to the origin of your hardcopy\n");
    printf("graph and press button 1.\n");
    get_cursor_position(&bn,&orig.x,&orig.y,graph_area,0x1);
    if (get_real_val("What is the y-axis value at the origin",&minyval)) return;
    if (get_real_val("What is the x-axis value at the origin",&minxval)) return;
    printf("\n");
    printf("Now place the cursor at the maximum y-axis value on\n");
    printf("the graph and press button 1.\n");
    get_cursor_position(&bn,&maxy.x,&maxy.y,graph_area,0x1);
    if (get_real_val("What is the maximum y-axis value",&maxyval)) return;
    printf("\n");
    printf("Place the cursor at the maximum x-axis value on\n");
    printf("the graph and press button 1.\n");
    get_cursor_position(&bn,&maxx.x,&maxx.y,graph_area,0x1);
    if (get_real_val("What is the maximum x-axis value",&maxxval)) return;
    printf("\n");
/*
/*      Get definition of curve from user.  There are two ways of doing this.
/*      The default mode (spline_mode==0) is for digiplot to read the cursor
/*      location continuously.  In spline mode, the user selects points along
/*      the curve and digiplot uses a cubic spline interpolation to fill
/*      in the gaps.
*/
    if (!spline_mode) {
/*
/*          Get points, storing them in arrays x and y.  The cursor location is
/*          sampled continuously, but each point is only saved once, unless the
/*          cursor backtracks.
*/
        for (i=0;i<512;i++) y[i]=0.0;
        printf("Move the cursor to the start ");
	printf("of the spectrum and press button 1.\n");
        while (!done) vdswitch(&unit,&c1,&c1,&done);
        done=0;
        printf(
            "Okay.  Trace the spectrum slowly, and press button 4 to stop.\n");
        while (!done) {                        /* while not done...  */
            vdcurloc(&unit,&c1,&line,&sample);
            y[sample]=line;
            vdswitch(&unit,&c1,&c4,&done);
        }
        printf("One moment please . . .\n");
/*
/*          Correct for hardcopy orientation.
*/
        for (i=0;i<512;i++) f[i]=0.0;
        for (i=0;i<512;i++) if (y[i]!=0.0) {
	    pos.x=i;
	    pos.y=y[i];
            temp=correct(orig,maxx,pos);
	    if (temp == 99999.) {
		printf("Division by zero in 'correct': consult a programmer\n");
		return;
	    }
	    x[i]=temp*511.;
	    if (x[i]<0.0) x[i]=0.0;
            if (x[i]>511.0) x[i]=511.0;
            temp=correct(orig,maxy,pos);
	    if (temp == 99999.) {
		printf("Division by zero in 'correct': consult a programmer\n");
		return;
	    }
	    f[(int)x[i]]=temp*(maxyval-minyval)+minyval;
        }
        i=0;
        while (f[i]==0.0) i++;
        while (--i>=0) f[i]=f[i+1];
        for (i=1;i<512;i++) if (f[i]==0.0) f[i]=f[i-1];
/*
/*          Fill in unvalued points (again) and smooth.
*/
        for (i=0;i<512;i++) y[i]=0.0;
        i=0;
        while (i<512) {
	    j=i+1;
	    if (f[i]==f[j]) {
	        k=j+1;
	        while (k<512 && f[j]>f[k]-1 && f[j]<f[k]+1) k++;
	        if (k<512) {
	            if (f[k]>f[i])
			for (m=j;m<=k-1;m++)
		            y[m]=f[i]+((float)(m-i))/(k-i)*(f[k]-f[i]);
	            i=k;
	        }
	        else i=512;
            }
	    else i++;
        }
        for (i=0;i<512;i++) if (y[i]==0.0) y[i]=f[i];
        for (i=0;i<512;i++) y[i]=max(min((int)y[i],255),0);
    }
    else {
/*
/*          Get discrete points from the user and perform cubic spline
/*          interpolation.
*/
	n=0;
        printf("Select each point using button 1.  Use button 4 to quit.\n");
        printf("The terminal will beep after each point is read.  Don't\n");
	printf("move to the next point until it's ready!\n");
        get_cursor_position(&bn,&pos.x,&pos.y,graph_area,0x9);
        while (bn!=4 && n<256) {                        /* while not done...  */
            printf("Okay.\n");
            temp=correct(orig,maxx,pos);
            if (temp == 99999.) {
		printf("Division by zero in 'correct': consult a programmer\n");
		return;
	    }
	    x[n]=temp*(maxxval-minxval)+minxval;
            temp=correct(orig,maxy,pos);
            if (temp == 99999.) {
		printf("Division by zero in 'correct': consult a programmer\n");
		return;
	    }
	    a[n++]=temp*(maxyval-minyval)+minyval;
	    printf("%c",7);					  /* ring     */
            get_cursor_position(&bn,&pos.x,&pos.y,graph_area,0x9);/* get next */
        }
        if (n==256) printf("Curvegens are limited to 256 points.\n");
        printf("One moment please . . .\n");
        n--;
        spline_fit(x,n,a,b,c,d);        /* do spline fit, get coefficients.   */
        for (i=0;i<512;i++) y[i]=min(max((int)value_at(i*(maxxval-minxval)/511.
	    +minxval,x,n,a,b,c,d),0),255);
    }
/*
/*      Plot defined curve in the green plane and display.
*/
    vdareafi(&unit,&c2,&c0,&c0,&c0,&c1,&c1,&c512,&c512);    /* clear plane */
    vddispln(&unit,&c2);
    x_data[0]=1;
    x_data[1]=1;
    x_data[2]=512;
    y_data[0]=129;
    y_data[1]=384;
    y_data[2]=384;
    vdvector(&unit,&c2,&c0,&c255,&c0,&c3,x_data,y_data);
    for (i=0;i<512;i++) {			    /* plot data */
	x_data[i]=i;
	y_data[i]=(short int) (384-y[i]);
    }
    vdvector(&unit,&c2,&c0,&c255,&c0,&c512,x_data,y_data);
    vdflush(&unit);
/*
/*      See if the user wants to save the plot he selected in his library.
/*	If he does, save it.
*/
    query("Would you like to save this in your user library ? ",buf);
    if (buf[0]=='y' || buf[0]=='Y') {
        for (j=0;j<512;j++) data[j]=y[j];
        index = libr_insert_entry(name,data,&user_lib);
        if (index!=-1) {
            user_lib[index].spectrum_swl = minxval;
            user_lib[index].spectrum_ewl = maxxval;
            user_lib[index].bytes_used = 512;
            user_lib_changed = 1;
        }
    }
/*
/*      Restore screen.
*/
    vddispln(&unit,&c4);			/* disp red plane again.    */
    vdflush(&unit);
}


/****************************************************************************/
/* dist() - returns float-valued distance between two points.               */
/****************************************************************************/
float dist(pos1,pos2)
struct point pos1,pos2;
{
    float a;
    double sqrt(),arg;
    arg=(pos1.x-pos2.x)*(pos1.x-pos2.x)+(pos1.y-pos2.y)*(pos1.y-pos2.y);
    a=sqrt(arg);
    return(a);
}

/****************************************************************************/
/* get_real_val() - gets real value from user, allowing user to quit the    */
/* command, if necessary.                                                   */
/****************************************************************************/
get_real_val(buf,val_ptr)
char *buf;
float *val_ptr;
{
    int i=0,start=1,requery=0;
    char ans[80];
    while (start || requery) {
        start = 0;
        printf("%s",buf);
        if (requery) printf(" (RETURN for READY) ");
        query("? ",ans);
        if (ans[0]=='\0' && requery) return(1);
        while (i<strlen(ans) && ans[i]!='.') i++;
        if (i==strlen(ans)) strcat(ans,".");
        if (strcmp(ans,".")==0 || sscanf(ans,"%f",val_ptr)==0) requery=1;
        else requery=0;
    }
    return(0);
}


/****************************************************************************/
/* correct() - function takes coordinates of origin and endpoint of an axis */
/* and the coordinates of a location, and returns the fractional position   */
/* of the location along the axis.                                          */
/****************************************************************************/
float correct(orig,axis_end,pos)
struct point orig,axis_end,pos;
{
    float result,alen,blen,clen;
    alen=dist(orig,pos);
    blen=dist(orig,axis_end);
    clen=dist(pos,axis_end);
    if ((2*blen*blen)==0.) {
	return(99999.);
    }
    result=(alen*alen+blen*blen-clen*clen)/(2*blen*blen);
    return(result);
}

