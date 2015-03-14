/*************************************************************************
          spam - spectral analysis management program

 version 3 (July 1985)
 (This module main.c 2.15).
*************************************************************************/
#include <stdio.h>
#include "spam.h"

struct cmd_pair {
    char *cmd;
    int (*subr)();
};

#ifdef taevic
#include "r1lib:main.inc"
main44()
#else
main()
#endif
{
    short int not_done=1,i,j,index;
    int cluster(),curvegen(),delentry(),digiplot(),directory(),disp(),erase();
    int filter(),find(),func(),get(),hardcopy(),help(),hist(),identify(),keep();
    int libplot(),merge(),mixture(),norm(),photo(),plot(),ratio(),disp_return();
    int restore(),saveplot(),scan(),segdisp(),snapshot(),storespec(),stretch();
    int wavelength(),feature();
/* add declarations for new subroutines here . . . */
    static struct cmd_pair cmds[]={
        {"cluster",cluster},{"curvegen",curvegen},{"delete",delentry},
	{"digiplot",digiplot},{"directory",directory},{"display",disp},
	{"erase",erase},{"filter",filter},{"find",find},{"function",func},
        {"get",get},{"hardcopy",hardcopy},{"help",help},{"histogram",hist},
        {"identify",identify},{"feature",feature},
	{"keep",keep},{"libplot",libplot},{"merge",merge},{"mixture",mixture},
	{"normalize",norm},{"photograph",photo},{"plot",plot},{"ratio",ratio},
	{"return",disp_return},{"restore",restore},{"saveplot",saveplot},
	{"scan",scan},{"segdisp",segdisp},{"snapshot",snapshot},
        {"storespec",storespec},{"stretch",stretch},{"wavelength",wavelength},
/* add new command-subroutine pairs here . . . */
	{"END-OF-LIST",0}
    };
    unsigned char buf[80];
    if (init_prog_data()==-1) exit();
    if (init_device()==-1) exit();

    printf("\nSPAM Version 3.2\n");
    printf("For help, type \"help\".\n");
    while (not_done) {
        query("READY\n",buf);
        index = -1;
        if (strlen(buf)==0) ;
        else {
	    for (i=0;i!=100 && cmds[i].subr!=0;i++)
		if (substring(buf,cmds[i].cmd)) {
	            if (index!=-1) i=99;
	            else index=i;
	        }
	    if (index==-1) {
		if (substring(buf,"quit") || substring(buf,"exit")) not_done=0;
		else printf("Unknown command.  For help type \"help\".\n");
	    }
	    else if (index>=0 && i!=100) 
	        (*cmds[index].subr)(buf+substring(buf,cmds[index].cmd));
	    else printf("Ambiguous command.  For help type \"help\".\n");
	}
    }
    reinit();
}
