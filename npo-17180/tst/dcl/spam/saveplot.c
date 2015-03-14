/*************************************************************************
   saveplot.c 2.14 - saves current plot(s) in user's personal library.
*************************************************************************/
#include <stdio.h>
#include "spam.h"
saveplot(buf)
unsigned char *buf;
{
    int in=0,i,index,libr,items,fd,j,k,l,icount,name_change,redraw_plots=0;
    int mlib_len,ulib_len,fp_index;
    struct feature *feature_analysis();
    char plot[48*20],ans[80],old[20],new[20];
    unsigned char *data,*malloc(),*realloc();
    static struct parameter kwds[]={
        {"plotname",2,0,-48,"name(s) of plot(s) to save (default is none) ",
	    "",20,0},
        };
    if (plots_num==0) {
        printf("There aren't any plots up.  Type \"help\" for help.\n");
        return;
    }
    if (display_mode==1) {
	printf("No saveplots in display mode.  ");
	printf("Use the \"return\" command to exit.\n");
	return;
    }
    kwds[0].value = plot;
    kwds[0].input_count = &icount;
    if (par(buf,kwds,1)) return;
    mlib_len = master_lib[0].lib_length;
    data = malloc(512);
    if (data==NULL) {
	printf("Insufficient memory.\n");
	return;
    }
    for (l=0;l<icount;l++) {
	name_change = 0;
	strcpy(old,plot+20*l);
        get_plot_num(old,0,&i);
	if (l<icount-2 && (strcmp(plot+20*l+20,"as")==0 ||
		strcmp(plot+20*l+20,"AS")==0)) {
	    name_change=1;
	    redraw_plots=1;
	    strcpy(new,plot+20*l+40);
	    l+=2;
            if (strcmp(old,new)!=0) while (check_new_name(new)) {
	        query("New name (RETURN for READY) ? ",new);
	        if (new[0]=='\0') break;
	    }
	}
	else strcpy(new,old);
        if (new[0]=='\0') break;
        get_libplot_num(new,&index,&libr,1,3);
        if (i<0) {
	    if (i==-1) printf(
		"The plot \"%s\" doesn't exist.  Continuing . . .\n",old);
	    else printf(
		"\"%s\" is ambiguous.  Can't save plot.  Continuing . . .\n",
		old);
        }
	else if (index!=-1 && libr==1) {
	    printf( 
	        "There is already a master library plot named \"%s\".\n",new);
	    printf("Can't save plot.  Continuing . . .\n");
	}
	else {
	    if (index!=-1 && libr==2) {
		printf(
	            "There is already a user library plot named \"%s\".\n",new);
                query("Do you want to replace it (y to replace) ? ",ans);
	    }
	    if (index==-1 || ans[0]=='y') {
                for (j=0;j<512;j++) {
	            if (j<img_numchan) data[j]=plots[i].data[j];
	            else data[j]=0;
                }
		if (name_change) {
		    upper_case(new);
		    free(plots[i].name);
		    plots[i].name = malloc(strlen(new)+1);
		    if (plots[i].name==NULL) printf("Insufficient memory.\n");
		    else strcpy(plots[i].name,new);
		}
                index = libr_insert_entry(new,data,&user_lib);
                if (index!=-1) {
                    user_lib[index].spectrum_swl = img_swl;
                    user_lib[index].spectrum_ewl = img_ewl;
                    user_lib[index].bytes_used = img_numchan;
		    if (feature_range!=0) {
			ulib_len = user_lib[0].lib_length;
			fp_index = mlib_len+index-1;
			if (index==ulib_len) {
			    feature_dbase = (struct feature *)realloc(
				feature_dbase,(mlib_len+ulib_len)*
				sizeof(struct feature));
			    feature_dbase[fp_index].area = malloc(img_numchan);
			    feature_dbase[fp_index].peaks =
				(struct peak *)malloc(15*sizeof(struct peak));
			    if (feature_dbase==NULL ||
				    feature_dbase[fp_index].area==NULL ||
				    feature_dbase[fp_index].peaks==NULL) {
			        printf("Insufficient memory.\n");
			        free_feature_dbase();
				break;
			    }
			}
			feature_analysis(data,feature_dbase+fp_index);
		    }
                    user_lib_changed = 1;
                }
	    }
	}
    }
    free(data);
    if (redraw_plots) draw_plots();
}
