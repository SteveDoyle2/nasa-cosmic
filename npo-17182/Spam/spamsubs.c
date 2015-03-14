/*****************************************************************************
   spamsubs.c 2.15 - subroutines called throughout spam to make life easier 
*****************************************************************************/
#include <stdio.h>
#include "spam.h"
#ifdef unix
#include <sys/types.h>
#include <sys/timeb.h>
#else
#include <types.h>
#include <timeb.h>
#endif
/*****************************************************************************/
/* get_cur_position waits for a button to be pressed, then returns the       */
/* number of the button and the location of the cursor.  The calling program */
/* must pass the area of the screen to be used; area=0 if cursor should be   */
/* over the image area and 1 if cursor should be over the graph area.  Any   */
/* other  cursor positions will cause an error.  Note that button 4 has a    */
/* special meaning.  Button 4 will return whether or not cursor is in legal  */
/* location.                                                                 */
/*****************************************************************************/
get_cursor_position(bn_addr,x_addr,y_addr,area,button_mask)
int *bn_addr,area,*x_addr,*y_addr,button_mask;
{
    float wait=0.5;
    int error,k;
    short int sw,line,samp;
    error=1;
    if (cursor_nl==1 && cursor_ns==1) printf("Select a point . . .\n");
    else printf("Select an area . . .\n");
    while (error) {		   /* while cursor loc illegal for area ... */
        error=0;
	sw = 0;
        while (sw==0) {
            vdswitch(&unit,&c1,&c1,&sw);	/* check dev 1, button 1.   */
	    if (sw==1) if (!(button_mask & 0x1)) sw=0;
            if (sw==0) {			/* if off, check button 2.  */
	        vdswitch(&unit,&c1,&c2,&sw);
	        if (sw==1) if (button_mask & 0x2) sw=2; else sw=0;
	    }
            if (sw==0) {			/* if off, check button 3.  */
	        vdswitch(&unit,&c1,&c3,&sw);
	        if (sw==1) if (button_mask & 0x4) sw=3; else sw=0;
            }
            if (sw==0) {			/* if off, check button 4.  */
	        vdswitch(&unit,&c1,&c4,&sw);
	        if (sw==1) if (button_mask & 0x8) sw=4; else sw=0;
            }
	    vdwait(&wait);
        }
        vdcurloc(&unit,&c1,&line,&samp);	/* get cursor location.     */
        *x_addr = samp;
        *y_addr = line;
        *bn_addr=sw;
        if (sw==4) ;				/* don't do area checking.  */
        else if (area==0) {			/* cursor in image area.    */
            if ((*x_addr<cursor_ns || *x_addr>img_bw ||
                *y_addr>img_nl||*y_addr<cursor_nl) && display_mode==0) {
                    error=1;
                printf("Cursor is off image strip.\n");
            }
            else if ((*x_addr<cursor_ns ||
                *x_addr>(disp_endband-disp_startband+1)*img_bw ||
                *y_addr>img_nl || *y_addr<cursor_nl ||
                (*x_addr-cursor_ns+1)/img_bw != *x_addr/img_bw)
                && display_mode==1) {
                    error=1;
                printf("Cursor is off image area or spread over two bands.\n");
            }
        }
        else if (area==1) {			/* cursor in graph area.    */
            if (*x_addr<plots_hoff ||*x_addr>plots_hoff+plots_width ||
                *y_addr<plots_voff-plots_height||*y_addr>plots_voff) {
                    error=1;
                printf("Cursor is not over graph area.\n");
            }
        }
    }
}
    

/****************************************************************************/
/* get_plot_num returns the index into the plots array of a plot currently  */
/* on the screen, given a character pointer to the name of the plot.  If    */
/* the plot specified isn't up at the moment, the function returns -1.      */
/* If the name is ambiguous, it returns -2.  Abbreviations are allowed.     */
/* The exact variable should be 0 if an exact match is not required.        */
/* (The full name is returned at location "plot".)                          */
/****************************************************************************/
get_plot_num(plot,exact,index)
char *plot;
int exact,*index;
{
    int i,j;
    j=0;
    upper_case(plot);		/* all plots[] entries are in uppercase.    */
    for (i=0;i<16;i++) if (plots[i].name!=0) {
	if (exact && strcmp(plot,plots[i].name)==0) {
	    *index = i;
	    return;
	}
        else if (!exact && wildmatch('?',plot,plots[i].name)) {
/*
/* check for first abbreviation or exact match.
*/
            if (j==0) {
		*index=i;
		j=1;
            }
/*
/* If we've already seen one legal abbreviation or exact match and this
/* one matches, too, try to determine if first was exact.  If so, keep
/* that one.  Or if second is exact match, use that.  If neither is exact
/* match, assume ambiguous temporarily.
*/
            else if (j==1) {
		if (strcmp(plot,plots[*index].name)==0) ;
		else if (strcmp(plot,plots[i].name)==0) *index=i;
		else j=2;
            }
/*
/* If we've seen several possible matches, but nothing exact, and this
/* one matches exactly, reset j to indicate one possible match and
/* continue.  Otherwise, keep assuming ambiguity.
*/
            else if (j==2 && strcmp(plot,plots[i].name)==0) {
		*index=i;
		j=1;
            }
        }
    }
/*
/* If j is zero, we didn't see any possible matches.
/* If j is one, we found one exact match or abbreviation.
/* If j is two, we found several possible abbreviations, but no matches.
/* If ambiguous or not found, return error code.  Otherwise return index.
*/
    if (exact) {
	*index = -1;
	return;
    }
    else /* not exact */ {
        if (j==0) {				/* name wasn't found.  */
	    *index = -1;
	    return;
	}
        else if (j==1) {			/* found unique match. */
	    strcpy(plot,plots[*index].name);	/* save complete name. */
	    return;
	}
        else {					/* name is ambiguous.  */
	    *index = -2;
	    return;
	}
    }
}



/*************************************************************************/
/* This subroutine extracts a range of bands, moving the data from the   */
/* image area into a previously allocated area specified by a pointer    */
/* parameter.  Image data is moved in 4-byte groups (ints), if possible, */
/* to cut down a little on the time it takes.                            */
/*     Also, dn values lower than 17 are changed to 17 since the first   */
/* 17 locations in look-up-tables to hold graphics colors.               */
/*************************************************************************/
extract(i,l,base)
int i,l;
int *base;
{
    register int *p,*p2;
    register int j,t1,p1;
    register unsigned char *p3;
    int k;
    p=base;                                               /* dest for data   */
    if (img_bw/4*4==img_bw) {				  /* copy as ints    */
        p2=(int *)img_data+img_bw/4*(i-1);                /* source          */
        t1=img_bw/4*(l-i+1);                              /* words to copy.  */
        p1=img_ns/4-t1;
        for (k=0;k<img_nl;k++) {                          /* copy the data.  */
            for (j=t1;j>0;j--) *p++ = *p2++;
            p2+=p1;
        }
    }
    else {						  /* copy as bytes   */
        (unsigned char *)p2=img_data+img_bw*(i-1);        /* source          */
        t1=img_bw*(l-i+1);                                /* words to copy.  */
        p1=img_ns-t1;
        for (k=0;k<img_nl;k++) {                          /* copy the data.  */
            for (j=t1;j>0;j--)
		*((unsigned char *)p)++ = *((unsigned char *)p2)++;
            (unsigned char *)p2+=p1;
        }
    }
    p3=(unsigned char *)base+img_nl*img_bw*(l-i+1);
    for (j=img_nl*img_bw*(l-i+1);j>0;j--) if (*--p3<17) *p3=17;
}


/******************************************************************************/
/*               spectral library interface routines                          */
/******************************************************************************/

/******************************************************************************/
/* get_libplot_num - find library index value for given dataset name.         */
/******************************************************************************/
get_libplot_num(plotname,entry_num,library_num,exact,librs)
unsigned char *plotname;
int *entry_num,*library_num,exact,librs;
{
    char *get_lib_name(),buf[32];
    int i,j,k,found,index,libr,p1,p2;
    lower_case(plotname);
    found=0;
    p1=master_lib[0].right * (librs==1 || librs==3);
    p2=user_lib[0].right * (librs==2 || librs==3);
    while (p1!=0 || p2!=0) {
	if ((p1!=0 && p2!=0 && 
		strcmp(master_lib[p1].name,user_lib[p2].name)<0) || p2==0) {
	    j = p1;
	    k = 1;
	    strcpy(buf,master_lib[p1].name);
	}
	else {
	    j = p2;
	    k = 2;
	    strcpy(buf,user_lib[p2].name);
        }
	if (exact) {
	    if (strcmp(plotname,buf)==0) {
		*entry_num = j;
		*library_num = k;
		return;
	    }
	}
        else if (wildmatch('?',plotname,buf)) {
            if (found==0) { 	/* if name is substring of library name and   */
		index=j;	/* first such found, save place and set flag. */
		libr = k;
                found=1;
            }
            else if (found==1) {	/* if duplicate, save exact match */
                if (strcmp(get_lib_name(index,libr),plotname)==0) ;
		else if (strcmp(buf,plotname)==0) {
		    index=j;
		    libr=k;
		}
		else {
                    printf("The name \"%s\" is ambiguous.  ",plotname);
                    printf("It could refer to any of the following:\n");
		    start_pretty_io();
                    cprint(get_lib_name(index,libr));
                    cprint(get_lib_name(j,k));
                    found=2;
                }
            }
            else if (found==2) cprint(get_lib_name(j,k));
        }
	if (k==1) p1=master_lib[p1].right;
	else p2=user_lib[p2].right;
    }
    if (found==0) *entry_num = -1;
    else if (found==1) {
	strcpy(plotname,get_lib_name(index,libr));	/* save exact name.   */
	*entry_num = index;
	*library_num = libr;
    }
    else {
	stop_pretty_io();
	*entry_num = -2;
    }
}


/******************************************************************************/
/* get_lib_data - get library data at specified index into array "data".      */
/******************************************************************************/
get_lib_data(index,libr,data)
int index,libr;
unsigned char *data;
{
    int i,j,k,r,bytes;
    float swl,ewl;
    unsigned char *buf,*malloc(),file[80];
    buf=malloc(512);			/* allocate space for library data. */
    if (buf==NULL) {
	printf("Insufficient memory.\n");
	return(1);
    }
    if (libr==1) {
	for (i=0;i<512;i++) buf[i] = master_lib[index].data[i];
	swl=0.456;
	ewl=2.5;
	bytes=512;
    }
    else {
	for (i=0;i<512;i++) buf[i] = user_lib[index].data[i];
	swl = user_lib[index].spectrum_swl;
	ewl = user_lib[index].spectrum_ewl;
	bytes = user_lib[index].bytes_used;
        if (swl==0.0) swl=0.456;
        if (ewl==0.0) ewl=2.5;
        if (bytes==0) bytes=512;
    }
    resample(buf,bytes,swl,ewl,data,img_numchan,img_swl,img_ewl);
    free(buf);
    return(0);
}


/******************************************************************************/
/* get_lib_name - function returns name of entry at specified index.          */
/******************************************************************************/
char *get_lib_name(index,libr)
int index,libr;
{
    if (libr==1) return(master_lib[index].name);
    else return(user_lib[index].name);
}


/****************************************************************************/
/* imgcode - encodes image according to amplitude, slope and zero-crossings */ 
/****************************************************************************/
imgcode()
{
	int k1,k2,t1,i,j;
	register unsigned char *p1;
        register short int *p2;
	register unsigned char *j1;
	register unsigned char hd;
	register int k;
	unsigned char change_flag;
/*
/*      encode spectra
/*
/* The basic idea here is to encode each spectrum in n bytes where n is
/* the ceiling of the number of channels divided by 8.  So a 32 wavelength
/* spectrum can be encoded using 4 bytes.  Assume one band is 10 by 10 for
/* the moment.  Then we allocate 10 x 10 x 4 bytes (for a 32 channel image)
/* in which to binarily encode our data.  In the following section of code,
/* k1 = the current line, k2 = the current sample, and j1 points to the
/* current (first in the beginning) data point.  k points to the average
/* for the current spectrum as stored at hamm_avgs (this is calculated
/* in the get() subroutine by calc_band_average()).
/*
/* This algorithm is based on a previous encoding algorithm by David Sirag, Jr.
/* The main difference is that this algorithm visits image data points
/* starting with the first band rather than the (img_numchan-7)th band.
/* In this modification, the first bit of data corresponds to the first
/* data point.
/*
/* The main feature of this algorithm is that it recognizes the fact that
/* bits in the encoding don't flip with every new channel in the spectrum,
/* and so once it packs a zero, it continues to pack zeroes until it finds a
/* one, at which time it continues to pack ones, etc.  It does this by
/* packing zeroes for values below the mean, then when a value is above
/* the mean, inverting the partial result and packing zeroes for values
/* above the mean.  When it finishes encoding the byte or reaches another
/* value below the mean it reinverts.  This inverting and reinverting, while
/* sometimes confusing, is an extremely efficient way of encoding the data.
/*
/* In the code below, *p1 points to the result so far, initially 0.  As we move
/* from left to right, we shift the bits in hd to mark the current position.
/* An example should make this clear:
/*
/* Say we have data: 196 191 204 217 208 200 200 200 and spectrum average 203.
/* Initially,
/*             *p1 = 00000000    hd = 11111111
/* In this example, the first two spectral data values are below the mean.
/* So in the code below, we go through the first (0's) loop twice, shifting
/* hd right each time. (Each time, j1 increases by img_bw bytes in order to
/* point to the next spectral value.)  When we exit the loop hd = 00111111
/* and we add this (or "or" this, which is equivalent in this algorithm) to
/* the encoding so far.  Note, too, that we invert the current result after
/* adding in hd.  After some more shifting hd will be added in again, and
/* the result reinverted, but since the first two bits (at least) of
/* hd are zero, the inversions cancel for the first two bits and may be
/* ignored.  So the partial result is 00xxxxxx taking into account the
/* upcoming inversion.  (Intuitively, between the two loops, ones represent
/* bits which are really zeroes.  After one inversion, bits are set - unless
/* explicitly cleared by adding in 1s - since they will be inverted again at the
/* end of the second loop.)
/*
/*             *p1 = 11000000    hd = 00111111
/* The next three data points are above the mean, so we shift hd right three
/* more times, incrementing our data pointer as we go, and then exit the
/* loop.  Now hd = 00000111.
/*
/* Now we add hd to the partial result and reinvert, giving 00111000.
/* This ends the first trip through the main loop.  The second trip is
/* rather less interesting.  We go through the 0's loop three times and
/* reach the end of spectral data to be encoded in this byte, since hd=0.
/* Adding hd to the partial result and inverting gives the inverse of the
/* partial result since hd is zero.  We don't enter the next loop since
/* hd is zero, and immediately reinvert, giving final result *p1=00111000.
/*
/* This "twisting" of the partial result is the key to the algorithm.
*/
    j=(img_numchan+7)/8;	/* reg j =  # of bytes to encode spectrum */

    p1=hamm_ampbits-1;
    p2=zerox_base;		/* zero crossing information         */

    for (k1=0;k1<img_nl;k1++) for (k2=0;k2<img_bw;k2++) {
        j1=(unsigned char *)img_data+(k1*img_ns)+k2+img_ns;
	for (k=0,t1=0;t1<img_numchan;t1++) k+=(*(j1-=img_bw));
	k=k/img_numchan;
        *p2 = 0;
        for (t1=j-1;t1>=0;t1--) {
	    *++p1=0;
            if (t1) hd=0xff;
            else hd=0xff>>(j*8-img_numchan);

	    while (hd!=0) {
		change_flag= 0;
		while (hd!=0 && *j1<k) {	/* 0's loop */
		    hd>>=1;
		    j1+=img_bw;
		    change_flag = 1;
                }
		*p1=~(*p1|hd);
		if (change_flag == 1) *p2 += 1;
		change_flag = 0;
		while (hd!=0 && *j1>=k) {	/* 1's loop */
		    hd>>=1;
		    j1+=img_bw;
		    change_flag = 1;
                }

		*p1=~(*p1|hd);
		if (change_flag == 1) *p2 += 1;
	    }
        }
        p2 += 2;
    }
/*
      Slope encoding
*/
    p1=hamm_slopebits-1;	/* p1 pts to binary encoding area.  */
    p2=zerox_base+1;

    for (k1=0;k1<img_nl;k1++) for (k2=0;k2<img_bw;k2++) {
        j1=(unsigned char *)img_data+k1*img_ns+k2;
	*p2 = 0;

        for (t1=j-1;t1>=0;t1--) {
	    *++p1=0;
	    if (t1) hd = 0xff;
            else hd=0xff>>(j*8-img_numchan);
            if (t1 == j-1) hd >>= 1;

	    while (hd!=0) {
		change_flag = 0;
		while (hd!=0 && *j1>=*(j1+img_bw)) {	/* 0's loop */
		    hd>>=1;
		    j1+=img_bw;
		    change_flag = 1;
                }
		*p1=~(*p1+hd);
		if (change_flag == 1) *p2 += 1;

		change_flag = 0;
		while (hd!=0 && *j1<=*(j1+img_bw)) {	/* 1's loop */
		    hd>>=1;
		    j1+=img_bw;
		    change_flag = 1;
                }
		*p1=~(*p1+hd);
		if (change_flag == 1) *p2 += 1;
	    }
        }
	p2 += 2;
    }
}



/***************************************************************************
    redraw - redraw current band(s)
***************************************************************************/
redraw()
{
    unsigned char *calloc(),*base;
    short int t1,t2;
    long int i;
    if (display_mode==0) {
        extract(disp_band,disp_band,img_currband);
        t1=img_bw;
        t2=img_nl;
        i=img_nl*img_bw;
        vdareawr(&unit,&c4,&i,img_currband,&c1,&c1,&t1,&t2);
    }
    else {
        base=calloc((disp_endband-disp_startband+1)*img_bw*img_nl/4+1,4);
        if (base==NULL) {
            printf("Insufficient memory.\n");
	    return;
        }
        extract(disp_startband,disp_endband,base);
        t1=img_bw*(disp_endband-disp_startband+1);
        t2=img_nl;
        i=t1*t2;
        vdareawr(&unit,&c2,&i,base,&c1,&c1,&t1,&t2);
        cfree(base);
    }
    vdflush(&unit);
}


/******************************************************************************
    makemask - take a set of band pairs and create corresponding bit mask.
******************************************************************************/
makemask(mask,mcount,bitmask)
int mask[20][2],mcount;
unsigned char *bitmask;
{
    int j,k,number_of_bands_used;
    if (mcount/2*2!=mcount) {
        printf("Bad mask selection.  Use set of band pairs.\n");
        return(-1);
    }
    number_of_bands_used = img_numchan;
    for (j=0;j<img_numchan;j++) bitmask[j]=1;
    for (j=0;j<mcount/2;j++) {
        if (mask[j][0]<1 || mask[j][1]<1) {
            printf("Band number in mask is too small for this image.\n");
            return(-1);
        }
        if (mask[j][0]>img_numchan || mask[j][1]>img_numchan) {
            printf("Band number in mask is too large for this image.\n");
            return(-1);
        }
        for (k=mask[j][0]-1;k<mask[j][1];k++) bitmask[k]=0;
	number_of_bands_used -= (mask[j][1]-mask[j][0]+1);
    }
    for (j=0;j<(img_numchan+7)/8;j++) 
        for (k=0;k<8;k++) bitmask[j]=(bitmask[j]<<1) + bitmask[8*j+k];
    bitmask[j-1]>>=(j*8-img_numchan);
    return(number_of_bands_used);
}

/******************************************************************************
    prnlog - send the argument string to the screen and session log
******************************************************************************/
#include <varargs.h>
#include <ctype.h>
prnlog(va_alist)
va_dcl
{
    int i,j,o,eob;
    unsigned char *p,logname[40];
    va_list ap;
    char *format,pspec[132],obuf[132];
    va_start(ap);
    format = va_arg(ap,char *);
    strcpy(logname,"spam.log");
#ifdef unix
    sprintf(logname+strlen(logname),".%d",getpid());
#endif
    if (session_log==NULL) {
	session_log = fopen(logname,"w");
	if (session_log==NULL) fprintf(stdout,"Can't open session log.\n");
    }
    eob=0;
    for (i=0;i<strlen(format);i++) {
	while (format[i]!='%' && i<=strlen(format)) obuf[eob++] = format[i++];
	if (i!=strlen(format) && format[i]=='%') {
	    o=0;
	    while (i<strlen(format) && !isalpha(format[i]))
	        pspec[o++] = format[i++];
	    pspec[o++] = format[i];
	    pspec[o] = '\0';
	    switch (format[i]) {
		case 'd': sprintf(obuf+eob,pspec,va_arg(ap,int)); break;
		case 'g': sprintf(obuf+eob,pspec,va_arg(ap,double)); break;
		case 'f': sprintf(obuf+eob,pspec,va_arg(ap,double)); break;
		case 'c': sprintf(obuf+eob,pspec,va_arg(ap,char)); break;
		case 's': sprintf(obuf+eob,pspec,va_arg(ap,char *)); break;
		case 'x': sprintf(obuf+eob,pspec,va_arg(ap,int)); break;
		case 'o': sprintf(obuf+eob,pspec,va_arg(ap,int)); break;
		case 'u': sprintf(obuf+eob,pspec,va_arg(ap,int)); break;
		default: sprintf(obuf+eob,"unknown format\n",15); break;
	    }
            eob+=strlen(obuf+eob);
	}
    }
    fprintf(stdout,"%s",obuf);
    if (session_log!=NULL) fprintf(session_log,"%s",obuf);
    va_end(ap);
}

/******************************************************************************/
/*                         timing functions                                   */
/******************************************************************************/
long timenow()				/* return current time in ms.         */
{
    struct timeb clock;
    ftime(&clock);
    return(1000*clock.time + clock.millitm);
}

long start_time;
start_clock()
{
    start_time=timenow();
}

stop_clock()
{
    printf("elapsed time = %f seconds\n",(timenow()-start_time)/1000.);
}

/****************************************************************************
    talloc - allocate space and save location on stack; freed by tfree
****************************************************************************/
static unsigned char *sstk[100];
static int stos = -1;
unsigned char *talloc(size)
int size;
{
    unsigned char *p,*malloc();
    if (stos==99) return(NULL);
    p = malloc(size);
    if (p==NULL) return(NULL);
    sstk[++stos] = p;
    return(p);
}

tfree(mesg)
int mesg;
{
    if (mesg) printf("Insufficient memory.\n");
    while (stos>=0) free(sstk[stos--]);
}

/******************************************************************************/
/*                  string output functions                                   */
/******************************************************************************/
int pos,column_mode,min_col_width,maxlen,error,lines;
char **col_strings;

start_pretty_io()	/* init pos global to set up pretty printing. */
{
    unsigned char *malloc();
    pos=0;
    min_col_width=0;
    maxlen = 230;
    col_strings = (char **) malloc(sizeof(char *)*maxlen);
    if (col_strings==NULL) {
	printf("Insufficient memory.\n");
	error=1;
    }
    else error = 0;
    lines=0;
}


lprint(buf)	/* print strings, packing words in lines.  Needs start_ and   */
char *buf;	/* stop_pretty_io below.  Use space() to force breaks.  Use   */
{		/* indent() to skip to next line and indent.                  */
    int i,j;
    unsigned char word[80];
    if (error) return;
    if (column_mode) {
	write_columns();
	column_mode=0;
	pos=0;
    }
    i=0;
    while (buf[i]==32) i++;			/* skip past the spaces.      */
    while (buf[i]!='\0') {			/* while remainder not empty, */
	j=0;
	while (buf[i]!=32 && buf[i]!='\0') word[j++]=buf[i++];
	word[j]='\0';				/* get next word into "word". */
	if (word[j-1]=='.') strcat(word," ");
	if (lines==23) get_return(&lines);
        if (pos+1+strlen(word)>79) {		/* if too long to print on    */
	    printf("\n");			/* current line, advance to   */
	    pos=0;				/* next line.                 */
	    lines++;
            if (lines==23) get_return(&lines);
        }
        printf("%s ",word);			/* print word.                */
	pos+=1+strlen(word);			/* advance output pos var.    */
	while (buf[i]==32) i++;			/* skip white space, and      */
    }						/* continue.                  */
}

nprint(buf)	/* print string without changing, but add newline at end.     */
char *buf;
{
    int i,j;
    if (error) return;
    if (lines==23) get_return(&lines);
    if (column_mode && pos!=0) write_columns();
    else if (pos!=0) {
        pos=0;
        printf("\n");
        lines++;
        if (lines==23) get_return(&lines);
    }
    printf("%s\n",buf);
    lines++;
}

space()			/* force break and insert blank between lines.        */
{
    if (error) return;
    if (lines==23) get_return(&lines);
    if (column_mode && pos!=0) write_columns();
    else if (pos!=0) {
	printf("\n");
	lines++;
	pos=0;
        if (lines==23) get_return(&lines);
    }
    printf("\n");
    lines++;
}

indent()		/* go to next line and indent 4 spaces.               */
{
    if (error) return;
    if (column_mode) {
	printf("Ignoring illegal pretty_io call.  Consult programmer.\n");
	return;
    }
    if (pos!=0) printf("\n");
    printf("    ");
    pos=4;
}

cprint(buf)		/* print arg strings in two-column format.  Needs     */
char *buf;		/* start_ and stop_pretty_io below.                   */
{
    unsigned char *malloc();
    if (error) return;
    column_mode=1;
    if (pos==maxlen) {
        maxlen *= 2;
	col_strings = (char **) realloc(col_strings,sizeof(char **)*maxlen);
	if (col_strings==NULL) {
	    printf("Insufficient memory.\n");
	    error=1;
	    return;
	}
    }
    col_strings[pos] = (char *)malloc(strlen(buf)+1);
    if (col_strings[pos]==NULL) {
	printf("Insufficient memory.\n");
	error=1;
	return;
    }
    strcpy(col_strings[pos],buf);
    pos++;
    if (strlen(buf)>min_col_width) min_col_width=strlen(buf);
}

write_columns()
{
    int i,j,k,num_columns;
    if (error) return;
    if (min_col_width<=13) num_columns=5;
    else if (min_col_width<=17) num_columns=4;
    else if (min_col_width<=24) num_columns=3;
    else if (min_col_width<=37) num_columns=2;
    else num_columns=1;
    k=pos/num_columns;
    if (k*num_columns!=pos) k++;
    for (i=0;i<k;i++) {
	if (lines==22) get_return(&lines);
	for (j=0;j<num_columns;j++) {
	    if (num_columns==1)
		printf("%-79s",col_strings[i]);
	    else if (num_columns==2 && i+j*k<pos)
		printf("%-39s",col_strings[i+j*k]);
	    else if (num_columns==3 && i+j*k<pos)
		printf("%-26s",col_strings[i+j*k]);
	    else if (num_columns==4 && i+j*k<pos)
		printf("%-19s",col_strings[i+j*k]);
	    else if (num_columns==5 && i+j*k<pos)
		printf("%-15s",col_strings[i+j*k]);
	}
	printf("\n");
	lines++;
    }
    while (pos>0) free(col_strings[--pos]);
}

get_return(lines_addr)
int *lines_addr;
{
    char pagebuf[80];
    query("PRESS RETURN",pagebuf);
    *lines_addr = 0;
}

stop_pretty_io()	/* end pretty printing cleanly.                       */
{
    if (error) return;
    if (column_mode && pos!=0) write_columns();
    else if (pos!=0) printf("\n");
    free(col_strings);
}

/***********************************************************************/
/*                    string input function                            */
/***********************************************************************/
query(prompt,result)
char *prompt,*result;		/* prints prompt string and then gets  */
{				/* answer using vosgetline.            */
    printf("%s",prompt);
    vosgetline(result);
    if (session_log!=NULL) fprintf(session_log,"%s\n",result);
    if (result[0]=='"' && result[strlen(result)-1]=='"') {
	strcpy(result,result+1);
	result[strlen(result)-1]='\0';
    }
}

/***********************************************************************/
/*                    plotname checking                                */
/***********************************************************************/
check_new_name(name)
char *name;
{
    int i,j;
    for (i=0;i<strlen(name);i++)
        if (!isalnum(name[i]) && name[i]!='_' && name[i]!='.' && name[i]!='/')
	    break;
    if (i!=strlen(name)) {
	if (name[i]==' ') printf("Multiple names specified.\n");
        else printf("One or more characters in name is illegal.\n");
	return(1);
    }
    get_plot_num(name,1,&i);
    if (i!=-1) {
	printf("The name \"%s\" is already in use.\n",name);
	return(1);
    }
    get_libplot_num(name,&i,&j,1,3);
    if (i!=-1) {
	printf("There is already a library plot named \"%s\".\n",name);
	return(1);
    }
    return(0);
}

check_old_name(name)
char *name;
{
    int i,j;
    for (i=0;i<strlen(name);i++)
        if (!isalnum(name[i]) && name[i]!='_' && name[i]!='.' && name[i]!='/' &&
	    name[i]!='?') break;
    if (i!=strlen(name)) {
	if (name[i]==' ') printf("Multiple names specified.\n");
        else printf("One or more characters in name is illegal.\n");
	return(1);
    }
    return(0);
}
