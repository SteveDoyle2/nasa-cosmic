/*   CLIPS Version 4.30   4/25/89 */

#include <stdio.h>

#include <WindowMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include <TextEdit.h>
#include <MenuMgr.h>
#include <PrintMgr.h>

#include "interface.h"
#include "clipsmem.h"
#include "data.h"

#define MAX_DISPLAY_LINES 700
#define STANDARD_LINE_LENGTH 40

#define topMargin 20
#define leftMargin 20
#define bottomMargin 20

WindowRecord  DisplayRecord;
WindowPtr	  DisplayWindow;
ControlHandle DisplayVScroll;
ControlHandle DisplayHScroll;

extern int ScreenWidth;
extern int ScreenHeight;

extern MenuHandle FileMenu;
extern CursHandle ReadCursor;

extern EventRecord TheEvent;
extern WindowPtr TheWindow;
extern ControlHandle TheVScrollBar;
extern ControlHandle TheHScrollBar;
extern TEHandle TheText;

static int lines_in_display_window;
static int display_line_height;
static int top_display_number;
static int left_margin_number;
static int display_start_line;
static int display_line_descent;

static int zero_display_line = 0;
static int append_display_line = 0;
static int last_backup_line = 0;
static int last_backup_char = 0;
static int lines_in_use = 1;
static int lines_lost = 0;

struct line_record
  {
   int line_size;
   int insert_point;
   char *line_info;
  };
  
static struct line_record display_text[MAX_DISPLAY_LINES];

extern int DialogPopupOption;

extern char *malloc();

/*********************************************************/
/* INIT_DISPLAY_WINDOW:  Initializes the display window. */
/*********************************************************/
init_display_window()
  {
   WDHandle theData;
   Handle dataHandle;
   Rect	destRect, viewRect;
   Rect	scrollRect;
   FontInfo	myInfo;
   int height; 
   int space;
   FontInfo finfo;
   Point savepoint;
   int hsize, vsize, wleft, wtop;
   
   /*=============================================================*/
   /* Create the display window from the display window template. */
   /*=============================================================*/
   
   SetPort((DisplayWindow = GetNewWindow( DisplayWindowID, &DisplayRecord, -1L )));
   
   /*========================================================================*/
   /* Set the font style to monaco and the font size to 9. Set text transfer */
   /* mode so that new text overwrites whatever is on the window.            */
   /*========================================================================*/
   
   TextFont(4);
   TextSize(9);
   TextMode(srcCopy);
   
   /*===================================================================*/
   /* Size and Place the window in the upper left corner of the screen. */
   /*===================================================================*/
   
   hsize = 0.60 * (ScreenWidth - (6 + 6 + 6));
   vsize = 0.75 * (ScreenHeight - (46 + 26 + 6));
   wleft = 6;
   wtop = 46;
   SizeWindow(DisplayWindow,hsize,vsize,FALSE);
   MoveWindow(DisplayWindow,wleft,wtop,FALSE);
   ShowWindow(DisplayWindow);
   
   /*=======================================================*/
   /* Create the vertical scroll bar in the display window. */
   /*=======================================================*/
	
   scrollRect = (*DisplayWindow).portRect;
   scrollRect.left = scrollRect.right-15;
   scrollRect.right += 1;
   scrollRect.bottom -= 14;
   scrollRect.top -= 1;
   DisplayVScroll = NewControl(DisplayWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
  	
   /*=========================================================*/
   /* Create the horizontal scroll bar in the display window. */
   /*=========================================================*/ 
   
   scrollRect = (*DisplayWindow).portRect;
   scrollRect.top = scrollRect.bottom-15;
   scrollRect.bottom += 1;
   scrollRect.right -= 14;
   scrollRect.left -= 1;
   DisplayHScroll = NewControl(DisplayWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		
   /*=============================================*/
   /* Allocate the window's data record and store */
   /* it as the window's reference constant.      */
   /*=============================================*/
   
   dataHandle = NewHandle(sizeof(WindowData));
   SetWRefCon(DisplayWindow,(long int) dataHandle);
   
   /*=====================================================*/
   /* Lock the data record and convert to a typed handle. */
   /*=====================================================*/
   
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=================================================*/
   /* Store display window values in the data record. */
   /*=================================================*/
   
   (**theData).editRec = NULL;
   (**theData).vScrollBar = DisplayVScroll;
   (**theData).hScrollBar = DisplayHScroll;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   TheText = (**theData).editRec;
   TheWindow = DisplayWindow;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*====================================*/
   /* Get the window's font information. */
   /*====================================*/
   
   GetFontInfo(&finfo);
   
   /*==========================================================*/
   /* Determine the location on which to begin the first line, */
   /* the height of a line, the amount of space between lines, */
   /* the height of the display window, the number of lines    */
   /* that can be placed in the window, and the index of the   */
   /* line in the display history displayed at the top of the  */
   /* window.                                                  */
   /*==========================================================*/
   
   display_start_line = finfo.ascent + finfo.leading;
   display_line_height = finfo.ascent + finfo.descent + finfo.leading;
   display_line_descent = finfo.descent;
 
   height = (*DisplayWindow).portRect.bottom - ((*DisplayWindow).portRect.top + SBarWidth);
   lines_in_display_window = height / display_line_height;
   top_display_number = 0;
   
   /*=============================*/
   /* Draw the window's grow box. */
   /*=============================*/
   
   DrawGrowIcon(DisplayWindow);
   
   /*============================================*/
   /* Enable the Close command on the File menu. */
   /*============================================*/
   
   EnableItem(FileMenu,CloseItem);
  }
     
/********************************************************************/
/* UpdateDisplayWindow: Handle update event for the display window. */
/********************************************************************/
UpdateDisplayWindow()
  {
   FontInfo finfo;
   int y, bottom;
   GrafPtr save_port;
   RgnHandle rgn_hnd;
   Rect viewRect;
   int startX;
   int x, count;
   
   /*========================================================*/
   /* Save the old port. Set the port to the display window. */
   /*========================================================*/
   
   GetPort(&save_port);
   SetPort(DisplayWindow);
   
   /*=======================*/
   /* Reset the scroll bar. */
   /*=======================*/
   
   SetDisplayScroll();
   ScrollDisplay();
   
   /*====================================================*/
   /* Begin the update of the agenda window. Temporarily */ 
   /* restricts the visible region of the window by      */
   /* intersecting it with the update region.            */
   /*====================================================*/
   
   BeginUpdate(DisplayWindow);
  
   /*================================================*/
   /* Erase the visible region of the window. Redraw */
   /* the grow box and any window controls.          */
   /*================================================*/
   
   EraseRect(&(*DisplayWindow).portRect);
   DrawGrowIcon(DisplayWindow);
   DrawControls(DisplayWindow);
    
   /*===============================*/
   /* Save the old clipping region. */
   /*===============================*/
   
   rgn_hnd = NewRgn();
   GetClip(rgn_hnd);
   
   /*=================================================*/
   /* Prevent output from spilling into scroll areas. */
   /*=================================================*/
   
   viewRect = (*DisplayWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = display_line_height * lines_in_display_window;
   ClipRect(&viewRect);
  
   /*===================================================*/
   /* Position the pen in the upper left of the window. */
   /*===================================================*/
   
   startX = 10 - (GetCtlValue(DisplayHScroll) * HORIZONTAL_SCROLL_INCREMENT);
   y = display_start_line;
   MoveTo(startX,y);
   
   /*==========================================*/
   /* Print Display information to the window. */
   /*==========================================*/

   x = (zero_display_line + top_display_number) % MAX_DISPLAY_LINES;
   count = 0;
   while ((x != append_display_line) && (count <= lines_in_display_window))
     { 
      if (display_text[x].insert_point != 0)
        { DrawText(display_text[x].line_info,0,display_text[x].insert_point); } 
      y += display_line_height;
      MoveTo(startX,y);
      x = (x + 1) % MAX_DISPLAY_LINES;
      count++;
     }
     
   if (display_text[x].insert_point != 0)
     { DrawText(display_text[x].line_info,0,display_text[x].insert_point); }
   
   if (x == append_display_line) draw_cursor(1);
 
   /*==================================*/
   /* Restore the old clipping region. */
   /*==================================*/
   
   SetClip(rgn_hnd);
   DisposeRgn(rgn_hnd);
   
   /*====================================================*/
   /* End the update of the agenda window. This restores */
   /* the old visible region of the window.              */
   /*====================================================*/
  
   EndUpdate(DisplayWindow);

   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(save_port);
  }

/**********************************************************/
/* GROW_DISPLAY_WINDOW: Recomputes the number of lines in */
/*   the display window. Normally called after the agenda */
/*   window has been resized.                             */
/**********************************************************/
grow_display_window(w)
  WindowPtr w;
  {
   int space, height;
 
   height = (*DisplayWindow).portRect.bottom - ((*DisplayWindow).portRect.top + SBarWidth);
   lines_in_display_window = height / display_line_height;
  }
  
/*********************************************************/
/* SetDisplayScroll: Recomputes the number of settings */
/*   for the vertical scroll bar in the display window.  */ 
/*********************************************************/
SetDisplayScroll()
  {
   AdjustDisplayVerticalScroll();
   AdjustDisplayHorizontalScroll();
  }
  
/*********************************************************/
/* AdjustDisplayVerticalScroll: Recomputes the number of settings */
/*   for the vertical scroll bar in the display window.  */ 
/*********************************************************/
AdjustDisplayVerticalScroll()
  {
   int count = 0;
   int n, max;
   
   /*===========================================================*/
   /* Set n to the number of lines in the display history minus */
   /* the number of lines that can be displayed on the screen.  */
   /*===========================================================*/
   
   n = lines_in_use - lines_in_display_window;
		
   /*====================================================*/
   /* Maximum scroll position is n, or zero if all lines */
   /* fit in present screen.                             */
   /*====================================================*/
   
   max = ( n > 0 ? n : 0 );
   
   /*====================================================*/
   /* Set the maximum value for the vertical scroll bar. */
   /*====================================================*/
   
   SetCtlMax(DisplayVScroll,max );  
  }
  
/********************************************/
/* AdjustDisplayHorizontalScroll:            */
/********************************************/
AdjustDisplayHorizontalScroll()
  {
   int windowWidth;
   int maxTop;
   
   /*========================*/
   /* Get the window width. */
   /*========================*/
   
   windowWidth = ((*DisplayWindow).portRect.right - (*DisplayWindow).portRect.left) / 
                   HORIZONTAL_SCROLL_INCREMENT;
                   
   /*==================================*/
   /* Avoid white space at the bottom. */
   /*==================================*/
   
   maxTop = (VIRTUAL_HORIZONTAL_WIDTH / HORIZONTAL_SCROLL_INCREMENT)
            - windowWidth;
            
   /*=========================================*/
   /* If the text is smaller than the window, */
   /* then show all of the text.              */
   /*=========================================*/
   
   if (maxTop <= 0) maxTop = 0;
     
   /*=====================================*/
   /* Adjust the range of the scroll bar. */
   /*=====================================*/
   
   SetCtlMax(DisplayHScroll,maxTop);
  }
/****************************************************************************/
/* DisplayScrollProc: Scroll the display history within the display window. */
/****************************************************************************/ 
pascal void DisplayScrollProc(theControl, theCode)
  ControlHandle	theControl;
  int theCode;
  {
   int locVal;
   GrafPtr theWindow;
   extern int scrollAmt;
   extern int scrollCode;
	
   if (theCode==scrollCode) 
     {
      locVal = GetCtlValue(theControl);
      GetPort(&theWindow );
      SetDisplayScroll();   /* Why? */
      SetCtlValue(theControl,locVal+scrollAmt);
      UpdateDisplayWindow();
	 }
  }
 
/****************************************************/ 
/* DoDisplayContent: Handle mouse-down event in the */
/*   content region of the display window.          */
/****************************************************/  
DoDisplayContent()
  {
   int				cntlCode;
   ControlHandle 	theControl;
   int				pageSize;
   GrafPtr			savePort;
   extern         int scrollAmt;
   extern         int scrollCode;
   Point thePoint;
	
   /*=======================================================*/
   /* Save the old port. Set the port to the agenda window. */
   /*=======================================================*/
   
   GetPort(&savePort);
   SetPort(DisplayWindow);
   
   /*==============================================*/
   /* Get the mouse location in screen coordinates */
   /* and convert it to window coordinates.        */
   /*==============================================*/
   
   thePoint = TheEvent.where;
   GlobalToLocal(&thePoint);
   
   /*==================================================*/
   /* Determine where the click occured and handle it. */
   /*==================================================*/
   
   switch ( cntlCode = FindControl( thePoint, DisplayWindow, &theControl ) ) 
     {
	  case inUpButton:
	  case inDownButton:
	  case inPageUp:
	  case inPageDown:
		if (theControl == DisplayVScroll) 
		  { pageSize = lines_in_display_window; }
		else if (theControl == DisplayHScroll)
		  {
           pageSize = (*DisplayWindow).portRect.right - (*DisplayWindow).portRect.left;
           pageSize -= SBarWidth;
           pageSize = pageSize / HORIZONTAL_SCROLL_INCREMENT;
          }
          
		switch (cntlCode) 
		  {
		   case inUpButton: 
			 scrollAmt = -1;
			 break;
		   case inDownButton: 
		     scrollAmt = 1;
			 break;
		   case inPageUp: 
			 scrollAmt = -pageSize;
			 break;
		   case inPageDown: 
			 scrollAmt = pageSize;
			 break;
	      }
		
		scrollCode = cntlCode;
		if (TrackControl(theControl, thePoint, &DisplayScrollProc));
		break;
	  case inThumb:
		if (TrackControl(theControl, thePoint, 0L )) ;
		UpdateDisplayWindow();
		break;
	  default:
	    break;
	 }
	 
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  }

/*******************************************************/
/* ScrollDisplay: Scrolls the contents of the window. */
/*******************************************************/
ScrollDisplay()
  {
   Point		oldOrigin;
   RgnHandle	tmpRgn;
   int			dv, dh;
   Rect         viewRect;
   int old_number;
   
   /*==================================================*/
   /* Compute the vertical difference in the text from */
   /* the old location of the origin and the current   */
   /* position indicated by the vertical scroll bar.   */
   /*==================================================*/
   
   old_number = top_display_number;
   top_display_number = GetCtlValue(DisplayVScroll);
   dv = (old_number - (top_display_number + lines_lost)) * display_line_height;
   lines_lost = 0;
   
   /*====================================================*/
   /* Compute the horizontal difference in the text from */
   /* the old location of the origin and the current     */
   /* position indicated by the horizontal scroll bar.   */
   /*====================================================*/
  
   old_number = left_margin_number;
   left_margin_number = GetCtlValue(DisplayHScroll);
   dh = (old_number - left_margin_number) * HORIZONTAL_SCROLL_INCREMENT;
   
   /*==================================================*/
   /* Offset the destination rectangle to scroll text. */
   /*==================================================*/
   
   viewRect = (*DisplayWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = display_line_height * lines_in_display_window;
   
   /*===========================================*/
   /* Create a region to store the update area. */
   /*===========================================*/
   
   tmpRgn = NewRgn();
   
   /*============================================================*/
   /* If scrolling needs to be performed, then do it. Invalidate */
   /* the area that is left empty by scrolling.                  */
   /*============================================================*/
   
   if ((dv != 0) || (dh != 0))
     {
	  ScrollRect(&viewRect , dh, dv, tmpRgn );
	  InvalRgn(tmpRgn);
	 }
	 
   /*========================*/
   /* Dispose of the region. */
   /*========================*/
   
   DisposeRgn( tmpRgn );
  }
  
/************************************************************************/
/* SELECT_DISPLAY_WINDOW: Makes the facts window the front-most window. */
/************************************************************************/ 
select_display_window()
  {
   WDHandle theData;
   Handle dataHandle;
   
   /*==============================================*/
   /* If the dialog window does not exist, then... */
   /*==============================================*/
   
   if (DisplayWindow == NULL) 
     {
      /*===============================================*/
      /* If the Dialog PopUp Option is on, then create */
      /* the dialog window. Otherwise, return zero     */
      /* indicating the dialog window was not created. */
      /*===============================================*/
      
      if (DialogPopupOption)
        { init_display_window(); }
      else
        { return(0); }
     }
     
   /*============================*/
   /* Select the display window. */
   /*============================*/
   
   SelectWindow(DisplayWindow);
   
   /*==========================================*/
   /* Get the window data and lock it. Convert */
   /* the data to a typed handle.              */
   /*==========================================*/
   
   dataHandle = (Handle) GetWRefCon(DisplayWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   TheWindow = DisplayWindow;
   TheText = NULL;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*============================================================*/
   /* Return one indicating that the display window was created. */
   /*============================================================*/
   
   return(1);
  }

/*******************************************************************/
/* DISPLAY_PRINT: Handles printing a string to the display window. */
/*******************************************************************/
display_print(str)
  char *str;
  {
   long int i = 0;
   long int n;
   int x;
   char *new_line;
   Rect viewRect;
   GrafPtr savePort;
   Point pen_loc;
   RgnHandle rgn_hnd;
   int offset;
   char old_char;
   Point savepoint;
   int y_loc,x_loc;
   
   /*========================*/
   /* Save the current port. */
   /*========================*/

   GetPort(&savePort);
   
   /*=======================================================*/
   /* If the display window does not exist, then create it. */
   /*=======================================================*/
   
   if (DisplayWindow == NULL) init_display_window();
   
   /*=====================================*/
   /* Set the port to the display window. */
   /*=====================================*/
   
   SetPort(DisplayWindow);
   
   /*===============================*/
   /* Save the old clipping region. */
   /*===============================*/
   
   rgn_hnd = NewRgn();
   GetClip(rgn_hnd);
   
   /*=================================================*/
   /* Prevent output from spilling into scroll areas. */
   /*=================================================*/
   
   viewRect = (*DisplayWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = display_line_height * lines_in_display_window;
   ClipRect(&viewRect);
      
   /*===================================*/
   /* Remove the cursor while printing. */
   /*===================================*/
   
   MoveTo(cursor_x(),cursor_y());
   draw_cursor(0);
   
   /*====================================================*/
   /* Process each character of the string individually. */
   /*====================================================*/
   
   for (i = 0 ; str[i] != '\0' ; i++)
     {
      /*==========================================*/
      /* If the character is a backspace, then... */
      /*==========================================*/
      
      if (str[i] == '\b')
        {
         /*==============================================*/
         /* If the current append line is empty, then... */
         /*==============================================*/
         
         if (display_text[append_display_line].insert_point == 0)
           { 
            /*=======================================================*/
            /* If the current append line is not the first line in   */
            /* the display history then backup to the previous line. */
            /*=======================================================*/
            
            if (append_display_line != zero_display_line)
              {
               if (append_display_line == 0)
                 { append_display_line = MAX_DISPLAY_LINES - 1; }
               else
                 { append_display_line--; }  
               lines_in_use--;    
               show_display_select(); 
               MoveTo(cursor_x(),cursor_y());
               draw_cursor(0);               
              } 
           }
           
         /*=====================================================*/
         /* Else backup a character on the current append line. */
         /*=====================================================*/
         
         else
           { 
            x = --display_text[append_display_line].insert_point;
            old_char = display_text[append_display_line].line_info[x];
            display_text[append_display_line].line_info[x] = ' ';
            display_text[append_display_line].line_info[x+1] = '\0';
            Move(-CharWidth(old_char),0);
            DrawChar(' ');
            Move(-CharWidth(' '),0);
           }
        } 
        
      /*==================================================================*/
      /* Else if the character is a hard or soft carriage return, then... */
      /*==================================================================*/
      
      else if ((str[i] == '\n') || (str[i] == '\r'))
        {  
         /*==============================================================*/
         /* A carraige return forces the append line to the next line in */
         /* the display history. Note that the display history array     */
         /* is circular (that is, it wraps around).                      */
         /*==============================================================*/
         
         append_display_line = (append_display_line + 1) % MAX_DISPLAY_LINES;
         
         /*================================================================*/
         /* If the display history is not full, then the new append line   */
         /* added increments the number of lines in use by one. Otherwise  */
         /* the new append line causes the top line in the display history */
         /* to be replaced and the count of lines lost is incremented.     */
         /*================================================================*/
         
         if (lines_in_use < MAX_DISPLAY_LINES) 
           { lines_in_use++; }
         else
           { 
            zero_display_line = (zero_display_line + 1) % MAX_DISPLAY_LINES;
            lines_lost++; 
           }
           
         /*==============================================================*/
         /* If the new append line is not the standard line length, then */
         /* free the memory used for line storage of this line and set   */
         /* the line length equal to zero. This insures that memory      */
         /* allocated for large lines eventually gets freed.             */
         /*==============================================================*/
         
         if (display_text[append_display_line].line_size != STANDARD_LINE_LENGTH)
           {
            if (display_text[append_display_line].line_info != NULL)
              { free(display_text[append_display_line].line_info); }
            display_text[append_display_line].line_size = 0;
            display_text[append_display_line].line_info = NULL; 
           } 
           
         /*============================================*/
         /* Place the insertion point at the beginning */
         /* of the new append line.                    */
         /*============================================*/
         
         display_text[append_display_line].insert_point = 0;
         if (display_text[append_display_line].line_info != NULL) 
           { display_text[append_display_line].line_info[0] = '\0'; }
         
         /*==================================*/
         /* Restore the old clipping region. */
         /*==================================*/
         
         SetClip(rgn_hnd);
         
         show_display_select();
         MoveTo(cursor_x(),cursor_y());
         draw_cursor(0);
         ClipRect(&viewRect);
        
        }
        
      /*===================================================*/
      /* Else the character is a standard character, so... */
      /*===================================================*/
      
      else
        { 
         /*=================================================*/
         /* If the current append line is not big enough to */
         /* hold an additional character, then expand it.   */
         /*=================================================*/
         
         if (display_text[append_display_line].insert_point >=
             (display_text[append_display_line].line_size - 1))
           {
            new_line = malloc(display_text[append_display_line].line_size + 20);
            if (new_line == NULL) cl_exit(1);
            if (display_text[append_display_line].line_info != NULL)
              { 
               strcpy(new_line,display_text[append_display_line].line_info);    
               free(display_text[append_display_line].line_info);
              }
            display_text[append_display_line].line_size += 20;
            display_text[append_display_line].line_info = new_line;
           }
           
         /*===============================================*/
         /* Add the character to the current append line. */
         /*===============================================*/
         
         x = ++display_text[append_display_line].insert_point;
         display_text[append_display_line].line_info[x-1] = str[i];
         display_text[append_display_line].line_info[x] = '\0';
         
         /*===================================*/
         /* Draw the character on the window. */
         /*===================================*/
     
         DrawChar(str[i]);
        } 
     }
   
   /*====================*/
   /* Redraw the cursor. */
   /*====================*/
   
   draw_cursor(1);
   
   /*==================================*/
   /* Restore the old clipping region. */
   /*==================================*/
   
   SetClip(rgn_hnd);
   DisposeRgn(rgn_hnd);
    
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  }
  
  
/*******************************************************/
/* SHOW_DISPLAY_SELECT:                                */
/*******************************************************/  
show_display_select()
  {
   SetDisplayScroll();
   SetCtlValue(DisplayVScroll,GetCtlMax(DisplayVScroll)); 
   UpdateDisplayWindow();
  }

/*****************************************************/
/* MACFILEFIND: Router function which recognizes I/O */
/*   directed to the display window.                 */
/*****************************************************/
macfilefind(log_name)
  char *log_name;
  {
   if ( (strcmp(log_name,"stdout") == 0) ||
        (strcmp(log_name,"stdin") == 0) ||
        (strcmp(log_name,"wclips") == 0) ||
        (strcmp(log_name,"wtrace") == 0) ||
        (strcmp(log_name,"wagenda") == 0) ||
        (strcmp(log_name,"werror") == 0) ||
        (strcmp(log_name,"wdisplay") == 0) ||
        (strcmp(log_name,"wdialog") == 0) )
     { return(TRUE); }

    return(FALSE);
  }
  
/*********************************************************************/
/* MACFILEPRINT: Router function which prints to the display window. */
/*********************************************************************/
macfileprint(logical_name,str)
  char *logical_name, *str;
  {
   FILE *fptr;
   extern FILE *find_fptr();

   fptr = find_fptr(logical_name);
   if (fptr == stdout)
     {
      if (select_display_window())
        { display_print(str); }
     }
   else
     { fprintf(fptr,"%s",str); }
  }
  
static int use_save = 0;
static char save_char;

/*******************************************************************/
/* MACFILEGETC: Router function to get stdout written to terminal  */
/*******************************************************************/
macfilegetc(logical_name)
  char *logical_name;
  {
   FILE *fptr;
   char inp[2];
   extern FILE *find_fptr();
   char theChar;

   fptr = find_fptr(logical_name);
   if (fptr != stdin) return(getc(fptr)); 
   
   if (use_save == TRUE)
     {
      use_save = FALSE;
      return(save_char);
     }
      
   for (; ; )
     { 
      SetCursor(*ReadCursor);
      while (GetTheEvent(everyEvent,&TheEvent) == 0);
        
      switch (TheEvent.what) 
	    {
	     case updateEvt:
	       DoUpdate();
		   break;
		   
         case mouseDown:
	       DoReadMouseDown(TheEvent);
	       break;
	     		 
	     case activateEvt:
		   DoActivate();
		   break;  
		   
		 case keyDown:
		 case autoKey:          
		   theChar = TheEvent.message & charCodeMask;
		   if ((TheEvent.modifiers & cmdKey) == 0) 
		     {
              if (DisplayWindow == NULL) init_display_window();
			  inp[0] = theChar;
		      inp[1] = '\0';
		      cl_print("stdout",inp); 	  
              if (select_display_window()) show_display_select();
              InitCursor();
			  return(theChar);
			 }
		   break;
        }
     }
  }

/******************************************************/
/* MACFILEUNGETC:  Returns a pointer to a file stream */
/*   for a given file id tag.                         */
/******************************************************/
macfileungetc(ch,logical_name)
  int ch;
  char *logical_name;
  {
   FILE *fptr;
   
   return(EOF);
   fptr = find_fptr(logical_name);
   if (fptr == stdin)
     { 
      use_save = TRUE;
      save_char = ch;
     }
   else
     { ungetc(ch,fptr); }
  }
  
/*******************************************************/
/* DRAW_CURSOR: Handles drawing and erasing the cursor */
/*   in the display window.                            */
/*******************************************************/  
draw_cursor(flag)
  Boolean flag;
  {
   register int x, y, savey;
   Point savepoint;
   register int mode;
   
   /*==================================================*/
   /* If flag is true, then the cursor is to be drawn, */
   /* otherwise the cursor is to be erased.            */
   /*==================================================*/
   
   if (flag == 1) mode = patCopy;
   else mode = notPatCopy;
		
   /*===================================*/
   /* Adjust and save the pen position. */
   /*===================================*/
		
   GetPen(&savepoint);
   x = savepoint.h - 1;
   savey = savepoint.v;
   y = savey + display_line_descent;
   MoveTo(x,y);

   /*===================================*/
   /* Cursor is simply a vertical line. */
   /*===================================*/
   
   PenMode(mode);

   LineTo(x, y - display_line_height);
   MoveTo(x+1, savey);

   /*=====================*/
   /* Reset the pen mode. */
   /*=====================*/
   
   PenMode(patCopy);
  }

/**********************************************************/
/* CURSOR_Y: Returns the current y position of the cursor. /
/**********************************************************/  
cursor_y()
  {
   int y_loc;
   
   /*================================================*/
   /* Determine the number of lines being displayed. */
   /*================================================*/
   
   y_loc = (lines_in_use < lines_in_display_window ? lines_in_use : lines_in_display_window);
   
   /*================================================*/
   /* Return the offset from the top margin plus the */
   /* height of the lines displayed in the window.   */
   /*================================================*/
   
   y_loc = display_start_line + (y_loc - 1) * display_line_height;
   
   return(y_loc);
  }
  
/***********************************************************/
/* CURSOR_X: Returns the current x position of the cursor. */
/***********************************************************/  
cursor_x()
  {
   int x_loc;
   
   /*===================================================*/
   /* If the line is empty, then return the offset from */
   /* the left margin.                                  */
   /*===================================================*/
   
   x_loc = 10 - (GetCtlValue(DisplayHScroll) * HORIZONTAL_SCROLL_INCREMENT);
  
   if (display_text[append_display_line].insert_point == 0)
     { return(x_loc); }
  
   /*=======================================================*/
   /* Return the offset from the left margin plus the width */
   /* of the text associated with the current append line.  */
   /*=======================================================*/
   
   x_loc += TextWidth(display_text[append_display_line].line_info,0,
                      display_text[append_display_line].insert_point);
                      
   return(x_loc);
  }
  
/***********************************************************/
/* PrDisplay: Prints the display window to a print record. */
/***********************************************************/
PrDisplay(hPrint,font,size)
  THPrint 	hPrint;
  int			font;
  int			size;
  {
   register int 	line = 0;
   register int 	lastLineOnPage = 0;
   int				length;
   int              cur_line;
   Rect 			printRect;
   int 			linesPerPage;
   int 			lineBase;
   int 			lineHeight;
   FontInfo		info;
   TPPrPort		printPort;

   /*====================================*/
   /* Initialize a printing grafPort for */
   /* use in printing a document.        */
   /*====================================*/
   
   printPort = PrOpenDoc( hPrint, 0L, 0L );
   
   /*==============================================================*/
   /* Make the current port the printing port. Note that PrOpenDoc */
   /* should have already made printPort the current port.         */
   /*==============================================================*/
   
   SetPort(printPort);
   
   /*===============================================================*/
   /* Set the appropriate font type and size for the printing port. */
   /*===============================================================*/
   
   TextFont(font);
   TextSize(size);
   
   /*===============================================*/
   /* Determine the line height and number of lines */
   /* per page for the printing port.               */
   /*===============================================*/
   
   printRect = (**hPrint).prInfo.rPage;
   GetFontInfo( &info );
   lineHeight = info.leading + info.ascent + info.descent;
   linesPerPage = 
		(printRect.bottom - printRect.top - topMargin - bottomMargin) / lineHeight;
   
   /*=======================================*/
   /* Print the document page by page until */
   /* the end of document is reached.       */
   /*=======================================*/
   
   while (line < lines_in_use)
	 {
	  /*==================================================*/
	  /* Inform the Printing Manager to begin a new page. */
	  /*==================================================*/
	  
	  PrOpenPage( printPort, 0L );
	  
	  /*============================================*/
	  /* Compute the last line on the current page. */
	  /*============================================*/
	  
	  lastLineOnPage += linesPerPage;
	  
	  /*=================================================*/
	  /* Position the pen in the upper left of the page. */
	  /*=================================================*/
	  
	  lineBase = printRect.top + lineHeight;
	  MoveTo(printRect.left + leftMargin,lineBase);
	  
	  /*=========================================================*/
	  /* Print the current page line by line until the last line */
	  /* on the page or the end of the document is reached.      */
	  /*=========================================================*/
	  
	  while (line < lastLineOnPage)
	    {
		 /*==================================================*/
		 /* Print the line. Note that tab characters have no */
		 /* special meaning to DrawText.                     */
		 /*==================================================*/

		 cur_line = zero_display_line + line % MAX_DISPLAY_LINES; 
		 length = display_text[cur_line].insert_point;
		 if (length > 0) 
		   { DrawText(display_text[cur_line].line_info,0,length); }
		 
		 /*====================================*/
		 /* Position the pen at the next line. */
		 /*====================================*/
		 
		 lineBase += lineHeight;
		 MoveTo(printRect.left + leftMargin,lineBase);
		 
		 /*====================*/
		 /* Update line count. */
		 /*====================*/
		 
		 line++;
		}
	   
	  /*==================================*/
	  /* Inform the Printing Manager that */
	  /* the current page is finished.    */
	  /*==================================*/
	  
	  PrClosePage( printPort );
	 }
	
   /*==============================*/
   /* Close the printing grafPort. */
   /*==============================*/
	
   PrCloseDoc( printPort );
  }
  
/***********************************************************/
/* DoReadMouseDown: Handle mouse-down event during a read. */
/***********************************************************/
DoReadMouseDown(myEvent)
  EventRecord myEvent;
  {
   WindowPtr whichWindow;
   
   /*============================================*/
   /* Where on the screen was the mouse pressed? */
   /*============================================*/
   
   switch (FindWindow(myEvent.where,&whichWindow )) 
	 {
	  case inSysWindow:
	    SystemClick(&TheEvent,whichWindow);
	    break;
	  	
	  case inMenuBar:
	    SysBeep(20);
	    break;
	      
	  case inContent:
	    DoContent(whichWindow);
	    break;
	  
	  case inDrag:
	    DoDrag(whichWindow);
	    break;
		  
	  case inGrow:
	    DoGrow(whichWindow);
	    break;
	    
	  case inGoAway:
	    DoGoAway(whichWindow);
		break;
	  
      case inZoomIn:
        DoZoom(whichWindow,inZoomIn);
        break;
	  
      case inZoomOut:
        DoZoom(whichWindow,inZoomOut);
        break;
     }
  }

   
   