/**********************************************************************
   librsubs.c 2.14 - subroutines for maintaining 512-byte data libraries.

   Written by Alan S. Mazer, September 1984
   All routines print message and return -1 on error.
**********************************************************************/
#include <stdio.h>
struct libentry {
    char name[30];			/* name of entry              */
    short int left;			/* pointer to previous entry  */
    short int right;			/* pointer to next entry      */
    char date[30];			/* insertion data             */
    short int lib_length;		/* number of entries in libr  */
    char reserved_for_programmer[62];
    char data[512];			/* entry data                 */
};

/**************************************************************************/
/* Libr_get - Libr_get takes the name of a library and tries to read the  */
/* library into memory.  If it can, it does.  If it can't, it creates a   */
/* header entry to allow future insertions.                               */
/**************************************************************************/
libr_get(library_name,library_addr_ptr)
char *library_name;
struct libentry **library_addr_ptr;
{
    int i,unitnum;
    char *calloc(),*realloc();
    struct libentry *library;
/*
/*      Allocate storage for library header.
*/
    library = (struct libentry *)calloc(1,640);
    if (library==NULL) {
	printf("Insufficient memory for library.\n");
	return(-1);
    }
/*
/*      Open library.  If not found, fill header entry and return.
*/
    unitnum=vosopen(library_name,0,0,640);
    if (unitnum==-1) {
        library[0].right=0;
        library[0].lib_length=0;
        *library_addr_ptr = library;
	return(0);
    }
/*
/*      Read first library entry into allocated space.
*/
    i=vosread(unitnum,library,1,1,1,640);
    if (i==-1) {
        library[0].right=0;
        library[0].lib_length=0;
        *library_addr_ptr = library;
        vosclose(unitnum);
	return(0);
    }
/*
/*      Allocate more storage (for remainder of entries) and then read them.
*/
    library = (struct libentry *)realloc(library,(library[0].lib_length+1)*640);
    if (library==NULL) {
	printf("Insufficient memory for library.\n");
	return(-1);
    }
    i=vosread(unitnum,&library[1],2,1,library[0].lib_length,640);
    if (i==-1) {
        printf("Couldn't read entire library (%s).  Library is damaged.\n",
            library_name);
	free(library);
        vosclose(unitnum);
        return(-1);
    }
    vosclose(unitnum);
    *library_addr_ptr = library;
    return(0);
}


/****************************************************************************/
/* Libr_find_entry takes the entry name, and the linked-list library and    */
/* finds correct place for or location of specified entry.  "Found" is set  */
/* true if the entry already exists.   Find_libr_entry returns the location */
/* of the entry if it exists, and the entry before where it should be if it */
/* doesn't.                                                                 */
/****************************************************************************/
libr_find_entry(filename,found_addr,library)
char *filename;
int *found_addr;
struct libentry *library;
{
    int m,t;
    m=library[0].right;
    t=0;
    while (m!=0 && strcmp(filename,library[m].name)>0) {
        t=m;
        m=library[m].right;
    }
    if (strcmp(filename,library[m].name)==0) *found_addr=1;
    else *found_addr=0;
    if (*found_addr) return(m);
    else return(t);
}


/****************************************************************************/
/* Libr_get_file_data - Takes filename and reads 512 bytes of data from the */
/* file.                                                                    */
/****************************************************************************/
libr_get_file_data(filename,data)
char *filename,*data;
{
    int l,k,unitnum;
    unitnum=vosopen(filename,0,1,512);
    if (unitnum==-1) {
        printf("Can't find data set, \"%s\".\n",filename);
        return(-1);
    }
    l=strlen(filename)-1;                   /* if full pathname, shorten */
    while (l>=0 && filename[l]!='/') l--;
    if (l!=-1) strcpy(filename,filename+l+1);
    k=vosread(unitnum,data,1,1,1,512);
    if (k==-1) {
        printf("Couldn't read entire file.  Giving up.\n");
        vosclose(unitnum);
        return(-1);
    }
    vosclose(unitnum);
    return(0);
}



/****************************************************************************/
/* Libr_list - Lists out the current library on the terminal.               */
/****************************************************************************/
libr_list(library)
struct libentry *library;
{
    int i,j,l,n;
    n=library[0].lib_length;
    if (n==0) return(-1);
    start_pretty_io();		/* use pretty_printing routines in subs.c   */
    i=library[0].right;
    while (i!=0) {
        cprint(library[i].name);
        i=library[i].right;
    }
    stop_pretty_io();
    return(0);
}


/****************************************************************************/
/* Libr_insert_entry - Inserts dataset into the proper place in memory.     */
/* Entryname should be 29 characters long at most.  Data points to an area  */
/* of data 512 bytes long.  Function returns index used.  (Note that this   */
/* latter capability enables the programmer to further modify the reserved  */
/* bytes within the library entry, while still retaining the generality of  */
/* these library routines.)                                                 */
/****************************************************************************/
libr_insert_entry(entryname,data,library_addr_ptr)
char *entryname,*data;
struct libentry **library_addr_ptr;
{
    int i,n,m,index,found;
    char *libr_get_time_ascii(),*realloc();
    struct libentry *library;
    library = *library_addr_ptr;
    n=library[0].lib_length;
    if (strlen(entryname)>=30) entryname[29]='\0';
    lower_case(entryname);
    m=libr_find_entry(entryname,&found,library);
    if (!found) {
	library = (struct libentry *)realloc(library,(n+2)*640);
	if (library==NULL) {
	    printf("Insufficient memory for library update.\n");
	    return(-1);
	}
        n++;
        library[n].right = library[m].right;
	library[library[m].right].left = n;
        library[m].right = n;
        library[n].left = m;
        for (i=0;i<62;i++) library[n].reserved_for_programmer[i]=0;
        index=n;
    }
    else {
        printf("Replacing old %s\n",entryname);
        index=m;
    }
    for (i=0;i<512;i++) library[index].data[i] = data[i];
    strcpy(library[index].date,libr_get_time_ascii());
    strcpy(library[index].name,entryname);
    library[0].lib_length=n;
    *library_addr_ptr = library;
    return(index);
}


/****************************************************************************/
/* Libr_delete_entry - Deletes dataset from place in the linked list.       */
/* Entryname should be 29 characters long at most.  The function returns    */
/* true if the entry wasn't found.                                          */
/****************************************************************************/
libr_delete_entry(entryname,library)
char *entryname;
struct libentry *library;
{
    int m,n,left,right,found;
    n=library[0].lib_length;
    if (strlen(entryname)>=30) entryname[29]='\0';
    m=libr_find_entry(entryname,&found,library);
    if (!found) return(-1);
    else {
	library[library[m].left].right = library[m].right;
	library[library[m].right].left = library[m].left;
	if (m!=n) {
	    library[library[n].left].right = m;
	    library[library[n].right].left = m;
	    library[m] = library[n];
	}
	n--;
    }
    library[0].lib_length=n;
    return(0);
}


/**************************************************************************/
/* Libr_get_time_ascii - Return ptr to the current date/time in ascii.    */
/**************************************************************************/
char *libr_get_time_ascii()
{
    char *ctime();
    long time(),time_loc[2];
    time(time_loc);
    return(ctime(time_loc));
}


/**************************************************************************/
/* Libr_write - Writes the library to the disk.                           */
/**************************************************************************/
libr_write(library_name,library)
char *library_name;
struct libentry *library;
{
    int i,n,unitnum;
    n=library[0].lib_length;
    if ((unitnum=vosopen(library_name,1,n+1,640))==-1) {
        printf("Couldn't save new library.  Existing file may be protected.\n");
        return(-1);
    }
    else if (voswrite(unitnum,library,1,1,n+1,640)==-1) {
        printf("Couldn't finish writing library to disk.\n");
        vosclose(unitnum);
        return(-1);
    }
    else {
        vosclose(unitnum);
        return(0);
    }
}


/**************************************************************************/
/* Trace_list - functions displays current linked list.                   */
/**************************************************************************/
libr_trace_list(library)
struct libentry *library;
{
    int i;
    printf("first=%d, length=%d\n",library[0].right,library[0].lib_length);
    for (i=1;i<=library[0].lib_length;i++) 
    printf("%2d %-30s %2d %2d\n",
	i,library[i].name,library[i].left,library[i].right);
}
