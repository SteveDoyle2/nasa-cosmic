/************************************************************************
   unmakela.c 2.15				last updated: 5-6-85
 This strips off the spam label on an image data set.
************************************************************************/
#include <stdio.h>
main()
{
   int fd1,fd2,j,k,nl,ns,bytes_read,line;
   char file[80];
   unsigned char *image_buf,*malloc();

#ifdef taevic
   printf("Unmakela is not TAE-compatible.  Use LABEL-REMOVE.\n");
   exit();
#endif
   printf("file name : ");
   scanf("%s",file);
   fd1=vosopen(file,0,0,0);
   if (fd1==-1) {
      printf("Couldn't find file \"%s\".\n",file);
      exit();
   }
   if (vosreadl(fd1,"nl",&nl,0)==-1) {
      printf("File doesn't have the spam label.\n");
      exit();
   }
   if (vosreadl(fd1,"ns",&ns,0)==-1) {
      printf("File doesn't have the spam label.\n");
      exit();
   }
   fd2=vosopen(file,1,nl,ns);
   if (fd2==-1) {
      printf("Couldn't open file for output.  File may be protected.\n");
      exit();
   }
   image_buf=malloc(ns);
   if (image_buf==NULL) {
      printf("Insufficient memory.\n");
      exit();
   }
   line = 1;
   bytes_read = 0;
   while (bytes_read < nl*ns) {
       j=vosread(fd1,image_buf,line,1,1,ns);
       if (j==-1) {
	   printf("Can't continue.  Error reading image.\n");
	   exit();
       }
       voswrite(fd2,image_buf,line++,1,1,ns);
       bytes_read+=ns;
   }
   printf("Total number of bytes read : %d\n",bytes_read);
   vosclose(fd1);
   vosclose(fd2);
   free(image_buf);
}
