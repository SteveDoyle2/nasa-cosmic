/*************************************************************************
                vdi.c 1.2 - Virtual Device Interface

   This file contains the local implementation of the raster display
   primtives used by spam, providing a measure of portability.  This
   file, as distributed, contains the code for using the Rastertech
   Model 1/25, which may be used as an example in coding these routines
   for other machines.

   Notes:
      1) Note that vdflush must be called at the end of any subroutine
         calling these primitives.  Vdflush ensures that the dma buffer
         is flushed and that the cursor tracking is enabled.  (These
         routines will stop cursor tracking before writing to the
         screen; this is necessary because the cursor is software-
         defined and can't be going in this vdi implementation while the
         screen is being updated.  Vdflush may not be needed in other
         implementations of vdi.)

      2) Emptyb() is used below in a couple functions to force emptying
         of the device-driver buffer.  It is important sometimes (in
         the scan function, for example) that commands be sent right
         away for display timing reasons.

      3) This interface doesn't explicitly allow for flushing button
         buffers, and some devices (e.g., Rastertech) allow button
         pushes to be queued which can cause problems.  To alleviate
         this, vdcuroff() flushes the button buffer.

      4) Note that all arguments are passed by reference rather than
         value (as is normal in C) so that this interface may be written
         in FORTRAN, if preferred.

      5) Each function, in the first comment line, contains information
         for using the MIPL-defined "virtual frame buffer" interface.
         If you have this, read the xdlib information file in the
         distribution.  Otherwise, you can ignore these comments.

   Version 1.0, July 1984
*************************************************************************/

#include <stdio.h>
#include "spam.h"	/* See VDCURON for explanation of this.          */
int buttons[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int cursor_on;		/* This is only needed for user-defined cursors. */
			/* It allows vdi to turn off the cursor if       */
			/* necessary before writing to the screen.       */
short int i0=0,i1=1,i2=2,i3=3,i4=4,i7=7,i8=8,i42=42,i43=43,i44=44,i45=45;
short int i126=126,i117=117,i83=83,i16=16,in5= -5,in1= -1,i254=254,i512=512;
short int in256= -256,i128=128,i21=21,i36=36,i11=11,i12=12,i13=13,i14=14;
short int i255=255,i22=22,i5=5,i10=10;
#include descrip
/*************************************************************************/
/* Vddevall - Use XDDALLOCATE - Same parameters                          */
/* Vddevall allocates a display device.  This is not really needed in a  */
/* single-user system, but is included here for MIPL compatibility and   */
/* multi-user systems.                                                   */
/*************************************************************************/
vddevall(unit_addr,config1)
short int *unit_addr,*config1;
{
    char ans[132];
    long int i;
    static $DESCRIPTOR(desc,"dev");
    rtinit(&desc,&i3);   /* init rastertech for dma, default bufsiz. */
    for (i=0;i<40000;i++) ;
    entgra();
    cursor_on=0;
}


/*****************************************************************************/
/* Vddevact - Use XDDACTIVATE - Same parameters - Turn on/off access to      */
/* display device specified by unit_addr.  Included for MIPL compatibility   */
/* only.                                                                     */
/*****************************************************************************/
vddevact(unit_addr,flag_addr)
short int *unit_addr;
char *flag_addr;
{
}


/******************************************************************************/
/* Vddevcon - Use XDDCONFIGURE, et al.                                        */
/* Vddevcon configures the display device as follows (these requirements are  */
/* largely, but not totally, the default configuration set up using the MIPL  */
/* XDDCONFIGURE subroutine):                                                  */
/*                                                                            */
/* 1) Set the device for 512x512 full-color.  This can be done with           */
/*    XDDCONFIGURE if you are using the MIPL interface.                       */
/* 2) Set all device-specific things.  For example, on the Rastertech, reset  */
/*    debug mode, primitive-fill flag, etc.                                   */
/* 3) Clear the clipping window, if any, and reset the display window.  These */
/*    are done by XDDCONFIGURE if you're using the MIPL interface.            */
/* 4) Turn the cursor off, but set it up for automatic tracking of the digi-  */
/*    pad or trackball.  Done by XDDCONFIGURE.                                */
/* 5) Reset lut mappings, image plane 1 to lut 1, etc.  Done by XDDCONFIGURE. */
/* 6) Set up the graphics overlay plane for reading and writing as necessary, */
/*    but turn off (only the latter is done by XDDCONFIGURE; use XDGCONNECT   */
/*    to use image plane 4 as the graphics overlay under the MIPL interface). */
/* 7) Initialize text-display support.  This may involve font selection,      */
/*    rotation, and other aspects of text handling not changeable under this  */
/*    interface.  Things like color and text size are set when they are used, */
/*    using vdtxtsiz() and vdtext().                                          */
/******************************************************************************/
vddevcon(unit_addr,config2)
short int *unit_addr,*config2;
{
    short int i;
    spchar(&i2,&i0,&i0);		/* disable Rastertek keybd interrupt. */
    for (i=0;i<16;i++) buttbl(&i,&i0);	/* Remove button-table macros.        */
    moddis(&i0);			/* Switch to 512 x 512 mode.          */
    lutrte(&i0);			/* map image planes to corr luts.     */
    cororg(&i0,&i0);			/* reset screen origin.               */
    scrorg(&i0,&i0);
    window(&in256,&in256,&i255,&i255);	/* reset window.                      */
    prmfil(&i0);			/* turn off auto primitive filling.   */
    textre();				/* set default text font.             */
    delay(&i0);				/* reset transmission delay.          */
    vecpat(&in1);			/* set for solid (not dashed) lines.  */
    pixfun(&i0);			/* normal writes (no xor, and, etc.)  */
    pixclp(&i0);			/* handle over/under-flows using mod. */
    wrmask(&i255,&i7);			/* enable all planes for write.       */
    rdmask(&i255);			/* use all bits of each plane.        */
    rdmode(&i1);			/* enable dma readback.               */
    vload(&i4,&i128,&i0,&i0);           /* cross-hair color (red) in o-pln 0. */
    ovrrd(&i0,&i1);                     /* enable reading of o-pln 0 (cursor) */
    ovrrd(&i1,&i0);			/* disable reading o-pln 1 (default)  */
/*
/*  cursor macros (autotracking setup)
/*  The following lines implement our local cursor for the Rastertek.  The
/*  normal Rastertek cursor is a little large to be convenient.  This can be
/*  ignored if you're using a hardware cursor or have programmed your own
/*  cursor.
*/
    macdef(&i43);                     /* cross-hairs: draw new and erase old */
    cmove(&i0,&i21);                  /*     move to old pos                 */
    ovrval(&i0,&i0);                  /*     erase mode                      */
    macro(&i42);                      /*     draw cross-hair                 */
    cmove(&i21,&i2);                  /*     save digi-pad pos in reg 21     */
    cmove(&i0,&i21);                  /*     move to digi-pad pos            */
    ovrval(&i0,&i1);                  /*     add mode                        */
    macro(&i42);                      /*     draw cross-hair                 */
    macro(&i44);                      /*     kill time ...                   */
    cmove(&i22,&i0);	              /*     save current cursor location.   */
    macend();
    macdef(&i44);		      /* macros for killing time             */
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macro(&i45);
    macend();
    macdef(&i45);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    cmove(&i0,&i21);
    macend();
}


/************************************************************************/
/* Vddevope - Use XDDOPEN - Same parameters                             */
/* Vddevope opens the display device for output.  It's included here    */
/* for MIPL compatibility only.                                         */
/************************************************************************/
vddevope(unit_addr)
short int *unit_addr;
{
}


/************************************************************************/
/* Vddevfre - Use XDDFREE - Same parameters                             */
/* Vddevfre - deallocate and reinit device.  Again, this is probably    */
/* not necessary for single-user systems, but is included to fit into   */
/* multi-user systems and for MIPL compatibility.                       */
/************************************************************************/
#include <types.h>
#include <timeb.h>
vddevfre(unit_addr)
short int *unit_addr;
{
    struct timeb clock;
    short int i,j,k;
    val8(&i0);				/* erase the rgb planes.        */
    wrmask(&i255,&i7);
    flood();
    textc(&i36,&i0);			/* set for jumbo text size.     */
    value(&i255,&i0,&i0);		/* write NOT IN USE in random   */
    ftime(&clock);			/* position.                    */
    i= -80;
    j=200-(int)(clock.millitm*0.45);
    movabs(&i,&j);
    text1(&i10,"NOT IN USE");
    spchar(&i2,&i1,&i1);	/* reenable previously-disabled         */
    quit();			/* Rastertech coldstart interrupt key.  */
}


/************************************************************************/
/* Vdlutrmp - Use XDLRAMP - Same parameters                             */
/* Set specified look-up table (1=red,2=green,3=blue) to "ramp" where   */
/* 1st and 255th entry are 1 and 255, respectively.  Section variable   */
/* is for MIPL interface users only.                                    */
/************************************************************************/
vdlutrmp(unit_addr,lut_addr,section_addr)
short int *unit_addr,*lut_addr,*section_addr;
{
    int i=8;
    i>>=(*lut_addr);
    lutrmp(&i,&i0,&i255,&i0,&i255);
}


/************************************************************************/
/* Vdzoom - Use XDLZOOM - Same parameters                               */
/* Set zoom.  Note that this definition of the vdzoom primitive implies */
/* that zooms can be set independently.  This is allowed under the MIPL */
/* interface, but not possible with the Rastertech, so all planes are   */
/* zoomed with the same factor.  Also, the only legal zooms on the      */
/* Rastertek are 1, 2, 4, and 8.  Planes are specified with the plane   */
/* mask defined above.                                                  */
/************************************************************************/
vdzoom(unit_addr,plane_addr,zoom_addr)
short int *unit_addr,*plane_addr,*zoom_addr;
{
    int i;
    if (*zoom_addr!=1 && *zoom_addr!=2 && *zoom_addr!=4 && *zoom_addr!=8)
	return(8);
    i = *zoom_addr;
    zoom(&i);
}


/****************************************************************************/
/* Vddispln - Use XDLCONNECT - Calling formats differ.                      */
/* The MIPL primitive XDLCONNECT allows any set of plane-LUT pairings.  The */
/* VDDISPLN primitive is less device specific but less powerful, allowing   */
/* any single plane to be mapped to all lookup tables.   Since XDLCONNECT   */
/* and VDDISPLN are not similar in capability, the parameters are not the   */
/* same.  See the documentation for XDLCONNECT for more information.        */
/* The plane to display is specified with the plane mask defined above.     */
/****************************************************************************/
vddispln(unit_addr,plane_addr)
short int *unit_addr,*plane_addr;
{
    if (*plane_addr==0 || *plane_addr==7) lutrte(&i0);  /* disp  all planes. */
    else if (*plane_addr==4) lutrte(&i126);	   /* map red to all luts.   */
    else if (*plane_addr==2) lutrte(&i117);	   /* map green to all luts. */
    else if (*plane_addr==1) lutrte(&i83);	   /* map blue to all luts.  */
    emptyb();			       /* ensures immediate change on screen */
}


/*****************************************************************************/
/* Vdcuron - Use XDCON - Same parameters - Turn cursor on.  Note that the    */
/* spam program as originally implemented allowed for box cursors.  Neither  */
/* this interface nor the MIPL interface allow for these.  The global vars   */
/* cursor_nl and cursor_ns contain the dimensions of the box area, and are   */
/* both equal to 1 for a 11-line, 11-sample crosshair.  If box cursors will  */
/* work on your device, you can access these global variables by "including" */
/* spamdefs.h (in C).                                                        */
/*     The size of the box cursor is cursor_nl+2 lines by cursor_ns+2        */
/* samples.  In this way, the box cursor actually surrounds the area of      */
/* interest, and the cursor itself doesn't get in the way.  The location of  */
/* a box cursor is defined to be the coordinates (line, sample >=1) of the   */
/* lowest, rightmost point in the enclosed area; that is, the location is    */
/* not part of the cursor itself, but is just inside the lower-right corner  */
/* of the cursor.                                                            */
/*     When cursor_nl = cursor_ns = 1, the cursor is an 11 line, 11 sample   */
/* crosshair, whose location is defined to be the intersection point.        */
/*     Cursor, form, and blink variables are for MIPL compatibility only.    */
/*****************************************************************************/
vdcuron(unit_addr,cursor_addr,form_addr,blink_addr)
short int *unit_addr,*cursor_addr,*form_addr,*blink_addr;
{
    short int i,j,k;
    macdef(&i42);                            /* macro for drawing cross-hair */
    if (cursor_nl==1 && cursor_ns==1) {      /*    if no averaging (1x1 box) */
        drwrel(&i5,&i0);                     /*    use 11x11 cursor.         */
        movrel(&in5,&in5);
        drwrel(&i0,&i10);
        movrel(&in5,&in5);
        drwrel(&i5,&i0);
    }
    else {                                   /*    otherwise, draw box.      */
        movrel(&i1,&in1);
        i= -cursor_ns-1;
	j=cursor_nl+1;
        recrel(&i,&j);
        movrel(&in1,&i1);
    }
    macend();
    wrmask(&i0,&i16);                        /* write to o-plane 0 only.     */
    macro(&i42);                             /* draw cross_hair/box          */
    cmove(&i21,&i0);                         /* save current pos as old-pos  */
    buttbl(&i0,&i43);                        /* start digi-pad tracking      */
    flush();				     /* flush button queue.          */
    emptyb();
    cursor_on=1;
}


/*****************************************************************************/
/* Vdcuroff - Use XDCOFF - Same parameters                                   */
/* Vdcuroff turns the cursor off.  Cursor variable is for MIPL compatibility */
/* only.                                                                     */
/*****************************************************************************/
vdcuroff(unit_addr,cursor_addr)
short int *unit_addr,*cursor_addr;
{
    int i;
    buttbl(&i0,&i0);                          /* disable digi-pad tracking   */
    ovrval(&i0,&i0);                          /* write zeroes to plane now   */
    cmove(&i0,&i21);                          /* current pos is now last pos */
    macro(&i42);                              /* draw (erase) cross_hair/box */
    wrmask(&i255,&i7);                        /* write to normal area again  */
    flush();				      /* (See note 3 above.)         */
    for (i=0;i<16;i++) buttons[i]=0;          /* (flushing button buffer)    */
    emptyb();				      /* turn cursor off right now!  */
    cursor_on=0;
}


/*****************************************************************************/
/* Vdcurloc - Use XDCLOCATION - Same parameters - Return cursor location.    */
/* Cursor location is returned as the line and sample of the pixel to        */
/* the left and above the lower-right box corner for box cursors, and the    */
/* cross-hair center for cross-hair cursors.  Cursor variable is for MIPL    */
/* compatibility only.                                                       */
/*****************************************************************************/
vdcurloc(unit_addr,cursor_addr,line_addr,sample_addr)
short int *unit_addr,*cursor_addr,*line_addr,*sample_addr;
{
    int x,y;
    readcr(&i2,&x,&y);		/* devices only.                             */
    *line_addr = 256-y;		/* Rastertech returns location in Cartesian  */
    *sample_addr = x+257;	/* coordinates where (0,0) is center.        */
}


/**************************************************************************/
/* Vdswitch - Use XDXSWITCH - Same parameters - Check to see if the       */
/* specified button has been pushed.  Note that there is no provision for */
/* easily flushing the button buffer under either the MIPL interface or   */
/* this one, so flushes are done with vdcuroff()'s.       The value       */
/* var is set if the specified button has been pushed.  Dev is included   */
/* for MIPL compatibility only.  Up to 16 buttons are supported.          */
/**************************************************************************/
vdswitch(unit_addr,dev_addr,switch_addr,value_addr)
short int *unit_addr,*dev_addr,*switch_addr,*value_addr;
{
    short int bn,x,y;
    readbu(&i0,&i1,&bn,&x,&y);/* mode flag not applicable */
    if (bn!=0) buttons[bn]=1;		/* Make entry in table, if any.     */
    if (buttons[*switch_addr]) {	/* if button has been pushed, reset */
	buttons[*switch_addr]=0;	/* button entry in table and return */
	*value_addr=1;			/* on.                              */
    }
    else *value_addr=0;
}


/****************************************************************************/
/* Vdtxtsiz - Use XDTSIZE - Same parameters                                 */
/* Set height of text output in lines.  This could also be contained within */
/* vdtext().  The scale argument is included for MIPL compatibility only.   */
/****************************************************************************/
vdtxtsiz(size_addr,scale_addr)
short int *size_addr;
float *scale_addr;
{
    int i;
    i=(*size_addr)*16./7;
    textc(&i,&i0);
}


/*****************************************************************************/
/* Vdovron - Use XDGON - Same parameters                                     */
/* Turn on overlay plane.  The overlay plane should have been set up by      */
/* vddevcon() above.  The overlay plane is assumed on throughout spam.       */
/*****************************************************************************/
vdovron(unit_addr)
short int *unit_addr;
{
    ovrrd(&i1,&i1);		      /* enable reading of overlay plane 1.  */
}


/*****************************************************************************/
/* Vdovroff - Use XDGOFF - Same parameters                                   */
/* Turn off the overlay plane.                                               */
/*****************************************************************************/
vdovroff(unit_addr)
short int *unit_addr;
{
    ovrrd(&i1,&i0);
}


/*****************************************************************************/
/* Vdpixwri - Use XDIPIXELWRITE - Calling formats differ.                    */
/* This function sets a pixel at a specified location to a specified value.  */
/* It differs from the MIPL implementation in that all planes are written to */
/* in one subroutine call, and that 2-byte rgb values are passed, rather     */
/* than one 1-byte value.  The plane is specified using the mask defined     */
/* above.  The pixel location is given by the line and sample stored in the  */
/* y and x variables, respectively.                                          */
/*****************************************************************************/
vdpixwri(unit_addr,plane_addr,x_addr,y_addr,r_addr,g_addr,b_addr)
short int *unit_addr,*plane_addr,*x_addr,*y_addr;
short int *r_addr,*g_addr,*b_addr;
{
    int i,j,k;
    if (cursor_on) vdcuroff(&unit,&c1);
    i = *plane_addr;
    wrmask(&i255,&i);			/* write only to specified plane(s). */
    j = *x_addr-257;
    k = 256 - *y_addr;
    movabs(&j,&k);			/* move to point specified.          */
    i = *r_addr;
    j = *g_addr;
    k = *b_addr;
    if (*plane_addr!=8) value(&i,&j,&k);
    if (*plane_addr>7) {
	vload(&i5,&i,&j,&k);
        if (*r_addr || *g_addr || *b_addr) ovrval(&i1,&i1);
	else ovrval(&i1,&i0);
    }
    point();			       /* color pixel in specified plane(s). */
}


/****************************************************************************/
/* Vdareawr - Use XDIAWSET, XDIAWWRITE - Calling formats differ.            */
/* This function was formed by combining XDIAWSET & -WRITE above.  It fills */
/* the specified window area with a stream of pixels.  Any image plane(s)   */
/* may be filled, but the same pixel stream must be sent to all.            */
/* Plane is specified using mask defined above.  N is the number of pixels  */
/* to be sent.  A points to the pixel buffer.  Left, top, right, and bottom */
/* contain the starting sample, starting line, ending sample, and ending    */
/* line, respectively.                                                      */
/****************************************************************************/
vdareawr(unit_addr,plane_addr,n_addr,a,
    left_addr,top_addr,right_addr,bottom_addr)
short int *unit_addr,*plane_addr,*left_addr,*top_addr,*right_addr,*bottom_addr;
long int *n_addr;
char *a;
{
    int i,j,k;
    if (cursor_on) vdcuroff(&unit,&c1);
    i = *left_addr-257;
    j = 256 - *top_addr;
    movabs(&i,&j);
    i = *plane_addr;
    wrmask(&i255,&i);
    i = *bottom_addr - *top_addr + 1;
    j = *right_addr - *left_addr + 1;
    pixel8(&i,&j,a);
}


/*************************************************************************/
/* Vdvector - Use XDIPOLYLINE - Calling formats differ.                  */
/* Vdvector plots a series of connected vectors to any combination of    */
/* planes using the specified color(s).  Planes are specified using      */
/* mask defined above.  N is the number of points given.  Points are     */
/* passed using the X and Y arrays, line coordinates in the Y array, and */
/* sample coordinates in the X array.                                    */
/*************************************************************************/
vdvector(unit_addr,plane_addr,r_addr,g_addr,b_addr,n_addr,x_addr,y_addr)
short int *unit_addr,*plane_addr,*n_addr,*x_addr,*y_addr;
short int *r_addr,*g_addr,*b_addr;
{
    int i,j,k;
    if (cursor_on) vdcuroff(&unit,&c1);
    i = *plane_addr;
    wrmask(&i255,&i);
    i = *r_addr;
    j = *g_addr;
    k = *b_addr;
    if (*plane_addr!=8) value(&i,&j,&k);
    if (*plane_addr>7) {
	vload(&i5,&i,&j,&k);
        if (*r_addr || *g_addr || *b_addr) ovrval(&i1,&i1);
	else ovrval(&i1,&i0);
    }
    i = -257+x_addr[0];
    j = 256-y_addr[0];
    movabs(&i,&j);
    for (i=1;i<*n_addr;i++) {
	j = -257+x_addr[i];
	k = 256-y_addr[i];
	drwabs(&j,&k);
    }
}


/**************************************************************************/
/* Vdtext - Use XDTCOLOR, XDTTEXT - Calling formats differ.               */
/* Vdtext writes text to any or all of the planes in the specified color. */
/* Planes are specified using the plane mask defined above.  X and Y are  */
/* the sample and line of the lower-left corner of the first character.   */
/* N is the length of the text string.  A is a pointer to the string. Loc */
/* can be ignored; it's for MIPL only.                                    */
/**************************************************************************/
vdtext(unit_addr,plane_addr,r_addr,g_addr,b_addr,x_addr,y_addr,
    loc_addr,n_addr,a)
short int *unit_addr,*plane_addr,*x_addr,*y_addr,*loc_addr,*n_addr;
short int *r_addr,*g_addr,*b_addr;
char *a;
{
    int i,j,k,l;
    if (cursor_on) vdcuroff(&unit,&c1);
    j = *plane_addr;
    wrmask(&i255,&j);
    i = *r_addr;
    j = *g_addr;
    k = *b_addr;
    if (*plane_addr!=8) value(&i,&j,&k);
    if (*plane_addr>7) {
	vload(&i5,&i,&j,&k);
        if (*r_addr || *g_addr || *b_addr) ovrval(&i1,&i1);
	else ovrval(&i1,&i0);
    }
    i = *x_addr-257;
    j = 256-*y_addr;
    movabs(&i,&j);
    a[*n_addr]='\0';
    l=strlen(a);
    text1(&l,a);
}


/*************************************************************************/
/* Vdareafi - Use XDIAWSET, XDIFILL - Calling formats differ.            */
/* Vdareafi fills the specified area of the specified plane(s) with the  */
/* given color.  Plane is specified using mask described above.          */
/* Area is defined by starting sample, starting line, ending sample, and */
/* ending line, in order below.                                          */
/*************************************************************************/
vdareafi(unit_addr,pln_addr,r_addr,g_addr,b_addr,
    x1_addr,y1_addr,x2_addr,y2_addr)
short int *unit_addr,*pln_addr,*r_addr,*g_addr,*b_addr;
short int *x1_addr,*y1_addr,*x2_addr,*y2_addr;
{
    int i,j,k;
    if (cursor_on) vdcuroff(&unit,&c1);
    i = *pln_addr;
    wrmask(&i255,&i);
    prmfil(&i1);
    i = *r_addr;
    j = *g_addr;
    k = *b_addr;
    if (*pln_addr!=8) value(&i,&j,&k);
    if (*pln_addr>7) {
	vload(&i5,&i,&j,&k);
        if (*r_addr || *g_addr || *b_addr) ovrval(&i1,&i1);
	else ovrval(&i1,&i0);
    }
    i = *x1_addr-257;
    j=256-*y1_addr;
    movabs(&i,&j);
    i = *x2_addr-257;
    j=256-*y2_addr;
    rectan(&i,&j);
    prmfil(&i0);
}


/*************************************************************************/
/* Vdplncop - Use XDIAWSET, XDIICOPY - Calling formats differ.           */
/* Copy area of one plane to area(s) of other plane(s).  Planes are      */
/* specified using the usual masks.  Other arguments are p1x1, plane 1   */
/* starting sample, p1y1, plane1 starting line, p2x2, plane 2 ending     */
/* sample, p2y2, plane 2 ending line, etc.                               */
/*************************************************************************/
vdplncop(unit_addr,plane1_addr,plane2_addr,
    p1x1_addr,p1y1_addr,p1x2_addr,p1y2_addr,
    p2x1_addr,p2y1_addr,p2x2_addr,p2y2_addr)
short int *unit_addr,*plane1_addr,*plane2_addr;
short int *p1x1_addr,*p1y1_addr,*p1x2_addr,*p1y2_addr;
short int *p2x1_addr,*p2y1_addr,*p2x2_addr,*p2y2_addr;
{
    int r=0,g=0,b=0,i,j,k;
    i = *p1x1_addr-257;
    j = 256-*p1y1_addr;
    cload(&i11,&i,&j);
    i = *p1x2_addr-257;
    j = 256 - *p1y2_addr;
    cload(&i12,&i,&j);
    i = *p2x1_addr-257;
    j = 256 - *p2y1_addr;
    cload(&i13,&i,&j);
    i = *p2x2_addr-257;
    j = 256 - *p2y2_addr;
    cload(&i14,&i,&j);
    if (*plane2_addr & 4) r=1;
    if (*plane2_addr & 2) g=1;
    if (*plane2_addr & 1) b=1;
    switch (*plane1_addr) {
	case 4:
            i = r;
	    j = g;
	    k = b;
	    pmctl(&i0,&i0,&i0,&i0,&i,&j,&k);
	    pixmov();
	    break;
	case 2:
	    i = 2*r;
	    j = 2*g;
	    k = 2*b;
	    pmctl(&i0,&i0,&i0,&i0,&i,&j,&k);
	    pixmov();
	    break;
	case 1:
	    i = 3*r;
	    j = 3*g;
	    k = 3*b;
	    pmctl(&i0,&i0,&i0,&i0,&i,&j,&k);
	    pixmov();
	    break;
    }
}


/************************************************************************/
/* Vdsetwin - Use XDIDWSET - Calling formats differ.                    */
/* Set upper-left corner of the display window.  This differs from the  */
/* MIPL interface in that this requires that all image planes have the  */
/* same display window; the display window for the overlay plane may be */
/* different.  Left and top variables are sample and line of upper-     */
/* left corner, respectively.                                           */
/************************************************************************/
vdsetwin(unit_addr,plane_addr,left_addr,top_addr)
short int *unit_addr,*plane_addr,*left_addr,*top_addr;
{
    int i,j,k;
    i = *left_addr-1;
    j = 1-*top_addr;
    if (*plane_addr!=8) scrorg(&i,&j);
    if (*plane_addr>7) vload(&i16,&i,&j);
    emptyb();	/* make sure change is sent immediately for scan function */
}


/**************************************************************************/
/* Vdlutwri - Use XDLWRITE - Same parameters - Writes values in array out */
/* to specified look-up table (1=red,2=green,3=blue).  Ignore section     */
/* argument; this is for MIPL only.                                       */
/**************************************************************************/
vdlutwri(unit_addr,lut_addr,sect_addr,array)
short int *unit_addr,*lut_addr,*sect_addr,*array;
{
    int i=8,j;
    i>>=(*lut_addr);
    lutrmp(&i,&i0,&i255,&i0,&i255);
    for (i=0;i<256;i++) if (array[i]!=i) {
        j = array[i];
	if (*lut_addr==1) lutr(&i,&j);
	else if (*lut_addr==2) lutg(&i,&j);
	else if (*lut_addr==3) lutb(&i,&j);
    }
}


/**************************************************************************/
/* Vdflush - no MIPL equivalent - Ensures that all graphics primitives    */
/* are flushed from the dma buffer.  May be done automatically by some    */
/* systems.  Also restarts cursor tracking.                               */
/**************************************************************************/
vdflush(unit_addr)
short int *unit_addr;
{
    if (!cursor_on) vdcuron(&unit,&c1,&c0,&c0);
    else emptyb();
}


/****************************************************************************/
/* Vdareard - Use XDIAWSET, XDIAWREAD - Calling formats differ.             */
/* This function was formed by combining XDIAWSET & XDIAWREAD.  It reads    */
/* the pixel values within a specified window area and stores them in       */
/* memory.  Only one of the planes may be read at a time, but it is good    */
/* to allocate enough space for all three planes, unless you're really sure */
/* that your display device won't attempt to send more.  Define the plane   */
/* using the standard plane mask.  N is the number of pixels to be sent.    */
/* Red, Green, and Blue are the pixel buffers.  Left, top, right and bottom */
/* contain the starting sample, starting line, ending sample, and ending    */
/* line, respectively.  Pixels may not be read from the overlay plane.      */
/****************************************************************************/
vdareard(unit_addr,plane_addr,n_addr,red,green,blue,
    left_addr,top_addr,right_addr,bottom_addr)
short int *unit_addr,*plane_addr,*left_addr,*top_addr,*right_addr,*bottom_addr;
long int *n_addr;
char *red,*green,*blue;
{
    int nl,i,j,k;

    nl = (*n_addr)/(*right_addr-*left_addr+1);
    if (cursor_on) vdcuroff(&unit,&c1);
    i = *left_addr-257;
    j = 256-*top_addr;
    movabs(&i,&j);
    if (*plane_addr & 4) readf(&i1);
    else if (*plane_addr & 2) readf(&i2);
    else if (*plane_addr & 1) readf(&i3);
    readw(&nl,&i512,red,green,blue);
    emptyb();
}

/*****************************************************************************/
/* vdwait(time) - Waits to prevent system degradation caused by polling.     */
/*****************************************************************************/
vdwait(time_in_seconds)
float *time_in_seconds;
{
    lib$wait(time_in_seconds);
}
