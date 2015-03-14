/****************************************************************************
    libcode.c 2.14 - Library encoding
****************************************************************************/
#include <stdio.h>
#include "spam.h"
libcode()	
{
    int i,index,avg;
    long int sum;
    unsigned char *data,*malloc();

    data = malloc(img_numchan);
    if (data==NULL) {
        printf("Insufficient memory for library encoding.\n");
        return(1);
    }
    for (index=1; index<=master_lib[0].lib_length; index++){
	get_lib_data(index,1,data);

    	for (sum=0,i=0;i<img_numchan;i++) sum = sum+ data[i];

	avg = (float)sum/(float)img_numchan + 0.5;
        master_lib[index].encoding = malloc(((img_numchan+7)/8)*2);
        if (master_lib[index].encoding==NULL) {
            printf("Insufficient memory for library encoding.\n");
            free(data);
            while (index++ <= master_lib[0].lib_length)
                master_lib[index].encoding = 0;
            return(1);
        }
	compact_code(data,master_lib[index].encoding,avg);
    }
    for (index=1; index<=user_lib[0].lib_length; index++){
	get_lib_data(index,2,data);

    	for (sum=0,i=0;i<img_numchan;i++) sum = sum+ data[i];

	avg = (float)sum/(float)img_numchan + 0.5;
        user_lib[index].encoding = malloc(((img_numchan+7)/8)*2);
        if (user_lib[index].encoding==NULL) {
            printf("Insufficient memory for library encoding.\n");
            free(data);
            while (index++ <= user_lib[0].lib_length)
                user_lib[index].encoding = 0;
            return(1);
        }
	compact_code(data,user_lib[index].encoding,avg);
    }
    free(data);
    return(0);
}
