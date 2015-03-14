/**********************************************************************
        vos.c 1.5 - Virtual operating system interface
This allows spam to make operating-system-independent calls for disk
io and similar operations.  Written Nov 1984, revised July 1985.

Subroutine Usage
1) Up to 18 files may be open at once.  Files are assumed to be images,
   but any file of fixed recordsize may be used with these routines.
2) All functions return -1 on error.  The only message output by 
   functions in this module is Insufficient Memory.
3) Unlabelled files may be read and written, but the recordsize should
   be specified in the vosopen both when the file is written and when it
   is read.  Length and recordsize are not written out to the file
   unless this is requested using voswritel.  For input files containing a
   label, ns and nl may be defaulted to 0 in the call.
4) Labels must be written to output files (if desired) before writing any
   other data.
5) Label items are added one at a time.  The label has a fixed size,
   256 bytes; labels longer than 256 bytes will probably be overwritten
   by file data.
6) The formats for use in the vosreadl and voswritel routines are
   integers: 0=int, 1=float, 2=string.
7) Labels are written in ascii and readable through od (unix) or a
   similar dump program.
8) Subparts of several consecutive lines can be read with no problem.  For
   example, it is possible to read the first 3 bytes of each line in the
   input file; they will be stored contiguously.  In this (unix) version,
   groups of contiguous lines used in reads or writes are accessed at once.

VMS Implementation Notes and Restrictions
A) Files can only be read up to the end of the current record.  Reads
   in this implementation are put in a loop so that if a read doesn't
   get all the information it should, it automatically repeats.  This
   doesn't appear to be a problem with writes.
B) The lseek function is not implemented wonderfully.  In particular,
   if a file is written using lseeks to position and the data ends on a
   block boundary, the last block won't be written.  For this reason,
   all writes are assumed to be sequential in this implementation of vos.
   There are no lseek calls before writing data out to the file.
**********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include "spam.h"

static struct fileinfo {
    int label_present;		/* 1 iff file has label.         */
    char label[256];		/* label, undefined if no label. */
    int nl,ns;			/* number of lines, samples.     */
    int no_access;		/* set if no accesses to file.   */
    char filename[50];          /* name of file                  */
    int access_mode;            /* 0=read, 1=write               */
} *files[20]=		        /* up to 20 files open at once.  */
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

/*********************************************************************/
/* vosopen - Subroutine opens image file for sequential i/o.         */
/* Vosopen returns a unit number for use with voswrite or vosread.   */
/*********************************************************************/
vosopen(filename,mode,nl,ns)
unsigned char *filename;
int mode,nl,ns;
{
    int fd,i;
    struct fileinfo *p,*malloc();
    p=malloc(sizeof(struct fileinfo));
    if (p==NULL) {
        printf("Insufficient memory.\n");
        return(-1);
    }
    if (mode==0) {
        for (i=0;i<20;i++)
            if (files[i]!=0 && strcmp(files[i]->filename,filename)==0 &&
                files[i]->access_mode==1) return(-1);
        fd=open(filename,0);
        if (fd==-1) {
            free(p);
            return(-1);
        }
        files[fd]=p;
        read(fd,files[fd]->label,256);
        if (strcmp(files[fd]->label,"label")==0) files[fd]->label_present=1;
        else files[fd]->label_present=0;
        if (!files[fd]->label_present || vosreadl(fd,"nl",&files[fd]->nl,0)==-1)
            files[fd]->nl=nl;
        if (!files[fd]->label_present || vosreadl(fd,"ns",&files[fd]->ns,0)==-1)
            files[fd]->ns=ns;
        strcpy(files[fd]->filename,filename);
        files[fd]->access_mode=0;
    }
    else if (mode==1) {
        for (i=0;i<20;i++)
            if (files[i]!=0 && strcmp(files[i]->filename,filename)==0)
                if (files[i]->access_mode==1) return(-1);
        fd=creat(filename,0644);
        if (fd==-1) {
            free(p);
            return(-1);
        }
        files[fd]=p;
        files[fd]->label_present = 0;
        *files[fd]->label = '\0';
        files[fd]->nl=nl;
        files[fd]->ns=ns;
        strcpy(files[fd]->filename,filename);
        files[fd]->access_mode=1;
    }
    else return(-1);
    files[fd]->no_access=1;
    return(fd);
}

/*********************************************************************/
/* voswrite - subroutine writes image to open file (see vosopen).    */
/*********************************************************************/
voswrite(fd,buffer,sl,ss,nl,ns)
int fd,sl,ss,nl,ns;
unsigned char *buffer;
{
    int i,j;
    if (files[fd]->no_access) {
        files[fd]->no_access=0;
        if (files[fd]->label_present)
            if (write(fd,files[fd]->label,256)!=256) return(-1);
    }
    for (j=sl;j<sl+nl;j++) {
        i=write(fd,buffer+(j-sl)*ns,ns);
        if (i!=ns) return(-1);
    }
    return(0);
}

/*********************************************************************/
/* vosclose - subroutine closes file opened with vosopen.            */
/*********************************************************************/
vosclose(fd)
int fd;
{
    close(fd);
    free(files[fd]);
    files[fd]=0;
}

/*********************************************************************/
/* vosread - function reads data from open image file (see vosopen). */
/*********************************************************************/
vosread(fd,buffer,sl,ss,nl,ns)
int fd,sl,ss,nl,ns;
unsigned char *buffer;
{
    long int i,j,k;
    for (j=sl;j<sl+nl;j++) {
        lseek(fd,(j-1)*files[fd]->ns+ss-1+256*files[fd]->label_present,0);
        k=0;
        while ((i=read(fd,buffer+(j-sl)*ns+k,ns-k))!=0) k+=i;
        if (k!=ns) return(-1);
    }
    return(0);
}


/*********************************************************************/
/* voswritel - write label to image                                  */
/*********************************************************************/
voswritel(fd,key,value,format)
int fd,format;
char *key;
union {int *i; float *f; char *s;} value;
{
    char buf[80];
    if (files[fd]->no_access==0) return(-1);
    if (format==0) sprintf(buf,"%s %d ",key,*value.i);
    else if (format==1) sprintf(buf,"%s %f ",key,*value.f);
    else if (format==2) sprintf(buf,"%s %s ",key,value.s);
    else return(-1);
    if (files[fd]->label_present==0) {
        strcpy(files[fd]->label,"label");
        files[fd]->label_present=1;
        strcpy(files[fd]->label+6,buf);
    }
    else strcat(files[fd]->label+6,buf);
    if (strcmp("nl",key)==0) files[fd]->nl = *value.i;
    if (strcmp("ns",key)==0) files[fd]->ns = *value.i;
    return(0);
}


/*********************************************************************/
/* vosreadl - read label item from the image                         */
/*********************************************************************/
vosreadl(fd,key,value,format)
int fd,format;
char *key;
union {int *i; float *f; char *s;} value;
{
    char lkey[80],lvalue[80];
    int ptr;
    if (files[fd]->label_present==0) return(-1);
    ptr=0;
    lkey[0]='\0';
    lvalue[0]='\0';
    while (ptr<strlen(files[fd]->label+6)) {
        sscanf(files[fd]->label+ptr+6,"%s %s",lkey,lvalue);
        ptr+=strlen(lkey)+strlen(lvalue)+2;
        if (strcmp(lkey,key)==0) ptr=256;
    }
    if (lkey[0]=='\0') return(-1);
    if (format==0) sscanf(lvalue,"%d",value.i);
    else if (format==1) sscanf(lvalue,"%f",value.f);
    else if (format==2) sscanf(lvalue,"%s",value.s);
    else return(-1);
    return(0);
}


/************************************************************************/
/* vosgetline - gets line of input from terminal                        */
/************************************************************************/
vosgetline(buf)	/* get user input, ignoring leading spaces and tabs only. */
char *buf;	/* This is done in place of scanf because scanf has       */
{		/* problems when illegally-typed input is given.          */
    int i;	/* Input is ignored after 80 characters.  Trailing space  */
    int o;	/* is removed.                                            */
    i=0;
    o=0;
    i=getchar();
    while (i==32 || i==9) i=getchar();
    while (i != 10 && o<79) {
	buf[o++]=i;
	i=getchar();
    }
    while (i!=10) i=getchar();	/* remove characters beyond 79th in line  */
    buf[o] = '\0';
    if (strlen(buf)>0)
	while (isspace(buf[strlen(buf)-1])) buf[strlen(buf)-1]='\0';
    if (session_log!=NULL) fprintf(session_log,"%s\n",buf);
}


/************************************************************************/
/* vosdelete - delete the specified file from the disk                  */
/************************************************************************/
vosdelete(filename)
char *filename;
{
    delete(filename);
}
