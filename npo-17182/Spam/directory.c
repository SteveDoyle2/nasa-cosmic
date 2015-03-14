/*************************************************************************
   directory.c 2.15 - function lists contents of specified library(s)
*************************************************************************/
#include <stdio.h>
#include "spam.h"
directory(buf)
unsigned char *buf;
{
    int in=0,i,items,fd,j,libr;
    char masterlib[10],userlib[10];
    static struct parameter kwds[]={
        {"masterlib",3,0,0,"list master library only","",10,0},
        {"userlib",3,0,0,"list user library only","",10,0},
        };
    kwds[0].value = masterlib;
    kwds[1].value = userlib;
    if (par(buf,kwds,2)) return;	/* Get library name, return on error. */
    if (userlib[0]=='\0') {
        printf("Master Library Contents:\n");
	i=libr_list(master_lib);
        if (i==-1) printf("Master library is empty or nonexistent.\n");
    }
    if (userlib[0]=='\0' && masterlib[0]=='\0') printf("\n");
    if (masterlib[0]=='\0') {
        printf("User Library Contents:\n");
	i=libr_list(user_lib);
        if (i==-1) printf("User library is empty or nonexistent.\n");
    }
}
