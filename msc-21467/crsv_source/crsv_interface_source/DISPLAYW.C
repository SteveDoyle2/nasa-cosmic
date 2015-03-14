
#include <stdio.h>

#include <WindowMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include <MenuMgr.h>
#include <PrintMgr.h>

#include "interface.h"

/***************/
/* Definitions */
/***************/

#define topMargin              20
#define leftMargin             20
#define bottomMargin           20

#define MAX_DISPLAY_LINES    400
#define STANDARD_LINE_LENGTH   40

/**************/
/* Structures */
/**************/

struct line_record
  {
   int line_size;
   int insert_point;
   char *line_info;
  };

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static int                lines_in_display_window;
   static int                display_line_height;
   static int                top_display_number;
   static int                display_start_line;
   static int                display_line_descent;

   static int                left_margin_number;
   static int                zero_display_line = 0;
   static int                append_display_line = 0;
   static int                last_backup_line = 0;
   static int                last_backup_char = 0;
   static int                lines_in_use = 1;
   static int                lines_lost = 0;

   static WindowRecord       DisplayRecord;
   
   static struct line_record DisplayText[MAX_DISPLAY_LINES];
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   WindowPtr                 DisplayWindow;
   ControlHandle             DisplayVScroll;
   ControlHandle             DisplayHScroll;

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/
   
   extern int                ScreenWidth;
   extern int                ScreenHeight;

   extern MenuHandle         FileMenu;

   extern EventRecord        TheEvent;
   extern WindowPtr          TheWindow;

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char              *gen_alloc();
   extern void               gen_free();

/*******************************************************/
/* InitDisplayWindow:  Initializes the display window. */
/*******************************************************/
InitDisplayWindow()
  {
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
   
   hsize = 0.75 * (ScreenWidth - (6 + 6 + 6));
   vsize = (ScreenHeight - (46 + 6));
   vsize = (vsize / SBarWidth) * SBarWidth;
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
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheWindow = DisplayWindow;
   
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
     
   /*=============================================*/
   /* Disable the Close command on the File menu. */
   /*=============================================*/
   
   DisableItem(FileMenu,CloseItem);
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
   int x, count, startX;
   
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
      if (DisplayText[x].insert_point != 0)
        { DrawText(DisplayText[x].line_info,0,DisplayText[x].insert_point); } 
      y += display_line_height;
      MoveTo(startX,y);
      x = (x + 1) % MAX_DISPLAY_LINES;
      count++;
     }
     
   if (DisplayText[x].insert_point != 0)
     { DrawText(DisplayText[x].line_info,0,DisplayText[x].insert_point); }
    
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
/* GrowDisplayWindow: Recomputes the number of lines in   */
/*   the display window. Normally called after the agenda */
/*   window has been resized.                             */
/**********************************************************/
GrowDisplayWindow(w)
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
   
   /*=========================================*/
   /* Send the information to a dribble file. */
   /*=========================================*/
   
   PrintToDribbleFile(str);
   
   /*========================*/
   /* Save the current port. */
   /*========================*/

   GetPort(&savePort);
   
   /*=======================================================*/
   /* If the display window does not exist, then create it. */
   /*=======================================================*/
   
   if (DisplayWindow == NULL) InitDisplayWindow();
   
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
         
         if (DisplayText[append_display_line].insert_point == 0)
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
              } 
           }
           
         /*=====================================================*/
         /* Else backup a character on the current append line. */
         /*=====================================================*/
         
         else
           { 
            x = --DisplayText[append_display_line].insert_point;
            old_char = DisplayText[append_display_line].line_info[x];
            DisplayText[append_display_line].line_info[x] = ' ';
            DisplayText[append_display_line].line_info[x+1] = '\0';
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
         
         if (DisplayText[append_display_line].line_size != STANDARD_LINE_LENGTH)
           {
            if (DisplayText[append_display_line].line_info != NULL)
              { gen_free(DisplayText[append_display_line].line_info,
                         DisplayText[append_display_line].line_size);
              }
            DisplayText[append_display_line].line_size = 0;
            DisplayText[append_display_line].line_info = NULL; 
           } 
           
         /*============================================*/
         /* Place the insertion point at the beginning */
         /* of the new append line.                    */
         /*============================================*/
         
         DisplayText[append_display_line].insert_point = 0;
         if (DisplayText[append_display_line].line_info != NULL) 
           { DisplayText[append_display_line].line_info[0] = '\0'; }
         
         /*==================================*/
         /* Restore the old clipping region. */
         /*==================================*/
         
         SetClip(rgn_hnd);
         show_display_select();
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
         
         if (DisplayText[append_display_line].insert_point >=
             (DisplayText[append_display_line].line_size - 1))
           {
            new_line = (char *) gen_alloc(
                               DisplayText[append_display_line].line_size + 20);
            if (new_line == NULL) 
              {
               ExitToShell();
              }
              
            if (DisplayText[append_display_line].line_info != NULL)
              { 
               strcpy(new_line,DisplayText[append_display_line].line_info);    
               gen_free(DisplayText[append_display_line].line_info,
                             DisplayText[append_display_line].line_size);
              }
            DisplayText[append_display_line].line_size += 20;
            DisplayText[append_display_line].line_info = new_line;
           }
           
         /*===============================================*/
         /* Add the character to the current append line. */
         /*===============================================*/
         
         x = ++DisplayText[append_display_line].insert_point;
         DisplayText[append_display_line].line_info[x-1] = str[i];
         DisplayText[append_display_line].line_info[x] = '\0';
         
         /*===================================*/
         /* Draw the character on the window. */
         /*===================================*/
     
         DrawChar(str[i]);
        } 
     }
   
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
		 length = DisplayText[cur_line].insert_point;
		 if (length > 0) 
		   { DrawText(DisplayText[cur_line].line_info,0,length); }
		 
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
   
/****************************************************************/
/* ResizeDisplayScrollBars: Resize Display window's scroll bar. */
/****************************************************************/
ResizeDisplayScrollBars()
  {
   HideControl(DisplayVScroll);
   MoveControl(DisplayVScroll,
               (*DisplayWindow).portRect.right - (SBarWidth - 1), 
               -1);
   SizeControl(DisplayVScroll,SBarWidth,
               ((*DisplayWindow).portRect.bottom + 1) -
                ((*DisplayWindow).portRect.top - 1) - (SBarWidth - 1));
   ShowControl(DisplayVScroll);
   ValidRect(&(**DisplayVScroll).contrlRect);
     
   
   HideControl(DisplayHScroll);
   MoveControl(DisplayHScroll,
               -1,(*DisplayWindow).portRect.bottom - (SBarWidth - 1));
   SizeControl(DisplayHScroll,
               ((*DisplayWindow).portRect.right + 1) -
                ((*DisplayWindow).portRect.left - 1) -
                    (SBarWidth - 1),
               SBarWidth);
   ShowControl(DisplayHScroll);
   ValidRect(&(**DisplayHScroll).contrlRect);
  }
  
   