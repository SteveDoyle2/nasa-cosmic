/****************************************************************************
   return.c 2.15 - return from multi-band display mode
   The return command returns the user from the multi-band display mode
   (entered through disp) to the single-band-with-graphics mode.
****************************************************************************/
#include <stdio.h>
#include "spam.h"
disp_return(buf)
unsigned char *buf;
{
    if (display_mode==0) {
	printf("You're not in display mode.  Type \"help\" for help.\n");
	return;
    }
    vddispln(&unit,&c4);			/* disp all planes again.   */
    vdflush(&unit);
    display_mode=0;				/* reset mode flag.         */
}
