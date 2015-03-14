/**************************************************************
 makelabel.c 2.15 - Labels image files for program spam

 Revised for virtual operating system interface (vos.c)
     Dec 10 1984
 Revised
     Jan 22 1985	(Miki Martin)
     Rather than the entire dataset, this now reads in the
     image dataset one line at a time for the less buffer
     size used.
 Revised
     APR 8 1985       (Miki Martin)
     No more restriction on number of bands allowed.
     (before revision, number of bands must be either 32
     or 128.)

 Call is: makelabel filename
**************************************************************/
#include <stdio.h>
#include <ctype.h>
main(argc,args)
int argc;
char *args[];
{
/*
/*      Assorted declarations and initialization
*/
    int i,ii,j,m,nl=0,ns=0,numchan,no_err=1;
    int l=0,no_label,nbytes,bytes_read,line;
    float start_wave_length=(-1.0),end_wave_length=(-1.0);
    unsigned char *buf,file[80],*malloc(),*encoding,inbuf[80];
    unsigned char *zerox,change_flag,format[80];
    int k1,k2,x,y,t1,t2;
    unsigned char *j1,hd;
    int bw=0,k;
    unsigned char *p1, *p2;
#ifdef taevic
    printf("Makelabel is not TAE compatible.  Use LABEL-CREATE.\n");
    exit();
#endif
    if (argc != 2) {
	printf("filename? ");
	scanf("%s",file);
    }
    else strcpy(file,args[1]);
    i = vosopen(file,0,0,0);
    if (i==-1) {
        printf("File \"%s\" doesn't exist.\n",file);
        exit();
    }
    vosclose(i);
/*
/*      Get label information from user
*/
    while (nl==0) {
        printf("Number of lines ? ");
        scanf("%s",inbuf);
        if (sscanf(inbuf,"%d",&nl)!=1) nl=(-1);
        if (nl<1) {
            printf("Illegal number of lines.\n");
            nl=0;
        }
    }
    while (ns==0) {
        printf("Number of samples ? ");
        scanf("%s",inbuf);
        if (sscanf(inbuf,"%d",&ns)!=1) ns=(-1);
        if (ns<1) {
            printf("Illegal number of samples.\n");
            ns=0;
        }
    }
    strcpy(format,"BYTE");
    while (bw==0) {
        printf("Band width in pixels ? ");
        scanf("%s",inbuf);
        if (sscanf(inbuf,"%d",&j)!=1) bw=(-1);
        else bw=j;
        if (bw<1 || bw>256) {
            printf("Illegal band width.\n");
            bw=0;
        }
    }
    while (start_wave_length==-1.0 || end_wave_length==-1.0) {
        printf("Starting wavelength (microns) ? ");
        scanf("%s",inbuf);
        if (sscanf(inbuf,"%f",&start_wave_length)!=1)
	    start_wave_length = -1.;
        printf("Ending wavelength (microns) ? ");
        scanf("%s",inbuf);
        if (sscanf(inbuf,"%f",&end_wave_length)!=1)
	    end_wave_length = -1.;
        if (start_wave_length >= end_wave_length) {
            printf("Illegal wavelength range.\n");
            start_wave_length = (-1.0);
        }
    }
/*
/*      Create label for image
*/
    i=vosopen(file,0,nl,ns);		/* open file for read  */
    if (i == -1) {
	printf("Can't open file for read.  File may be protected.\n");
	exit();
    }
    ii=vosopen(file,1,nl,ns);		/* open file for write */
    if (ii == -1) {
	printf("Can't open file for output.  File may be protected.\n");
	exit();
    }
    if (voswritel(ii,"nl",&nl,0)==-1 || voswritel(ii,"ns",&ns,0)==-1 ||
        voswritel(ii,"sw",&start_wave_length,1)==-1 ||
        voswritel(ii,"ew",&end_wave_length,1)==-1 ||
        voswritel(ii,"bw",&bw,0)==-1 || voswritel(ii,"format",format,2)==-1 ||
        voswritel(ii,"file",file,2)==-1) {
        printf("Error writing label item\n");
        exit();
    }
    bytes_read=0;
    buf=malloc(ns);
    if (buf==NULL) {
	printf("Insufficient memory.\n");
	exit();
    }
/*
/*	read in image data one line at a time and 
/*      write it to the output data set.
*/
    line=1;
    while (bytes_read < nl*ns) {
	j=vosread(i,buf,line,1,1,ns);
	if (j==-1) {
		printf("Can't continue.  Error reading image.\n");
		exit();
	}
	voswrite(ii,buf,line++,1,1,ns);
	bytes_read+=ns;
    }
    printf("Total number of bytes read = %d\n",bytes_read);
    free(buf);
    vosclose(i);
    vosclose(ii);
}
