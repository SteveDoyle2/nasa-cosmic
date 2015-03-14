/*   CLIPS Version 4.30   4/25/89 */

#include <WindowMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include <TextEdit.h>
#include <MenuMgr.h>

#include "interface.h"
#include "network.h"
#include "data.h"

WindowRecord  FactsRecord;
WindowPtr	  FactsWindow;
ControlHandle FactsVScroll;
ControlHandle FactsHScroll;

extern int ScreenWidth;
extern int ScreenHeight;

extern MenuHandle FileMenu;

static int lines_in_facts_window;
static int facts_line_height;
static int top_fact_number;
static int left_margin_number;
static int facts_start_line;

extern EventRecord TheEvent;
extern WindowPtr TheWindow;
extern ControlHandle TheVScrollBar;
extern ControlHandle TheHScrollBar;
extern TEHandle TheText;

/****************************************************/
/* INIT_FACTS_WINDOW: Initializes the facts window. */
/****************************************************/
init_facts_window()
  {
   WDHandle theData;
   Handle dataHandle;
   Rect	destRect, viewRect;
   Rect	scrollRect;
   FontInfo	myInfo;
   int height; 
   int space;
   FontInfo finfo;
   int hsize, vsize, wleft, wtop;

   /*=========================================================*/
   /* Create the facts window from the facts window template. */
   /*=========================================================*/
   
   SetPort((FactsWindow = GetNewWindow( FactsWindowID, &FactsRecord, -1L )));
   
   /*======================================================*/
   /* Set the font style to monaco and the font size to 9. */
   /*======================================================*/
   
   TextFont(4);
   TextSize(9);
   
   /*============================================================*/
   /* Size and Place the window on the right edge of the screen. */
   /*============================================================*/
   
   hsize = 0.40 * (ScreenWidth - (6 + 6 + 6));
   vsize = ScreenHeight - (46 + 6);
   wtop = 46;
   wleft = 6 + (0.60 * (ScreenWidth - (6 + 6 + 6))) + 6;
   
   SizeWindow(FactsWindow,hsize,vsize,FALSE);
   MoveWindow(FactsWindow,wleft,wtop,FALSE);
   ShowWindow(FactsWindow);
   
   /*=====================================================*/
   /* Create the vertical scroll bar in the facts window. */
   /*=====================================================*/
	
   scrollRect = (*FactsWindow).portRect;
   scrollRect.left = scrollRect.right-15;
   scrollRect.right += 1;
   scrollRect.bottom -= 14;
   scrollRect.top -= 1;
   FactsVScroll = NewControl( FactsWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
			
   /*=======================================================*/
   /* Create the horizontal scroll bar in the facts window. */
   /*=======================================================*/ 
   
   scrollRect = (*FactsWindow).portRect;
   scrollRect.top = scrollRect.bottom-15;
   scrollRect.bottom += 1;
   scrollRect.right -= 14;
   scrollRect.left -= 1;
   FactsHScroll = NewControl(FactsWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		
   /*=============================================*/
   /* Allocate the window's data record and store */
   /* it as the window's reference constant.      */
   /*=============================================*/
   
   dataHandle = NewHandle(sizeof(WindowData));
   SetWRefCon(FactsWindow,(long int) dataHandle);
   
   /*=====================================================*/
   /* Lock the data record and convert to a typed handle. */
   /*=====================================================*/
   
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*==============================================*/
   /* Store fact window values in the data record. */
   /*==============================================*/
   
   (**theData).editRec = NULL;
   (**theData).vScrollBar = FactsVScroll;
   (**theData).hScrollBar = FactsHScroll;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = FactsVScroll;
   TheHScrollBar = FactsHScroll;
   TheText = NULL;
   TheWindow = FactsWindow;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*====================================*/
   /* Get the window's font information. */
   /*====================================*/
   
   GetFontInfo(&finfo);
   
   /*==============================================================*/
   /* Determine the location on which to begin the first line, the */
   /* height of a line, the height of the facts window, the number */
   /* of lines that can be placed in the window, and the index of  */
   /* the fact displayed at the top of the window.                 */
   /*==============================================================*/
   
   facts_start_line = finfo.ascent + finfo.leading;
   facts_line_height = finfo.ascent + finfo.descent + finfo.leading;
 
   height = (*FactsWindow).portRect.bottom - ((*FactsWindow).portRect.top + SBarWidth);
   lines_in_facts_window = height / facts_line_height;
   top_fact_number = 0;
   
   /*=============================*/
   /* Draw the window's grow box. */
   /*=============================*/
   
   DrawGrowIcon(FactsWindow);
   
   /*============================================*/
   /* Enable the Close command on the File menu. */
   /*============================================*/
   
   EnableItem(FileMenu,CloseItem);
  }

/****************************************************************/
/* UpdateFactsWindow: Handle update event for the facts window. */
/****************************************************************/
UpdateFactsWindow()
  {
   struct fact *facts_ptr;
   FontInfo finfo;
   int y, bottom;
   GrafPtr save_port;
   RgnHandle rgn_hnd;
   Rect viewRect;
   int count;
   int startX;
   extern int macfactsfind(), macfactsprint();
   
   /*======================================================*/
   /* Save the old port. Set the port to the facts window. */
   /*======================================================*/

   GetPort(&save_port);
   SetPort(FactsWindow);
   
   /*=======================*/
   /* Reset the scroll bar. */
   /*=======================*/
   
   SetFactsScroll();
   ScrollFacts();
   
   /*===================================================*/
   /* Begin the update of the facts window. Temporarily */ 
   /* restricts the visible region of the window by     */
   /* intersecting it with the update region.           */
   /*===================================================*/
  
   BeginUpdate(FactsWindow);
  
   /*================================================*/
   /* Erase the visible region of the window. Redraw */
   /* the grow box and any window controls.          */
   /*================================================*/
   
   EraseRect(&(*FactsWindow).portRect);
   DrawGrowIcon(FactsWindow);
   DrawControls(FactsWindow);
    
   /*===============================*/
   /* Save the old clipping region. */
   /*===============================*/
   
   rgn_hnd = NewRgn();
   GetClip(rgn_hnd);
   
   /*=================================================*/
   /* Prevent output from spilling into scroll areas. */
   /*=================================================*/
   
   viewRect = (*FactsWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = facts_line_height * lines_in_facts_window;
   ClipRect(&viewRect);
   
   /*===================================================*/
   /* Position the pen in the upper left of the window. */
   /*====================================================*/
   
   startX = 10 - (GetCtlValue(FactsHScroll) * HORIZONTAL_SCROLL_INCREMENT);
   y = facts_start_line;
   MoveTo(startX,y);
   
   /*==========================================================*/
   /* Install the router which will print to the facts window. */
   /*==========================================================*/
   
   add_router("macfacts",20,macfactsfind,macfactsprint,NULL,NULL,NULL);
   
   /*==============================================================*/
   /* Skip over any facts that fall out of range above the window. */
   /*==============================================================*/
   
   facts_ptr = get_next_fact(NULL);
   count = 0;
   while (count < top_fact_number)
     {
      if (facts_ptr != NULL)
        { facts_ptr = get_next_fact(facts_ptr); }
      count++;
     }
     
   /*======================================================*/
   /* Display the facts that fall within the facts window. */
   /*======================================================*/
   
   count = 0;
   while ((facts_ptr != NULL) && (count <= lines_in_facts_window))
     { 
      show_fact("wfacts",facts_ptr);
      y += facts_line_height;
      MoveTo(startX,y);
      facts_ptr = get_next_fact(facts_ptr);
      count++;
     }
     
   /*====================================================*/
   /* Remove the router that prints to the facts window. */
   /*====================================================*/
   
   del_router("macfacts");
     
   /*==================================*/
   /* Restore the old clipping region. */
   /*==================================*/
   
   SetClip(rgn_hnd);
   DisposeRgn(rgn_hnd);
   
   /*===================================================*/
   /* End the update of the facts window. This restores */
   /* the old visible region of the window.             */
   /*===================================================*/
  
   EndUpdate(FactsWindow);

   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(save_port);
  }
  
/********************************************************/
/* COMPLETE_FACTS_REFRESH: Invalidates the entire facts */
/*   window, forcing an update.                         */
/********************************************************/  
complete_facts_refresh()
  {
   Rect viewRect;
   GrafPtr savePort;
     
   /*========================*/
   /* Save the current port. */
   /*========================*/
   
   GetPort(&savePort);
   
   /*===================================*/
   /* Set the port to the facts window. */
   /*===================================*/
   
   SetPort(FactsWindow);
   
   /*=====================================*/
   /* Invalidate the entire viewing area. */
   /*=====================================*/
   
   viewRect = (*FactsWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom -= SBarWidth;
   InvalRect(&viewRect);
   
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  }
   
/********************************************************/
/* GROW_FACTS_WINDOW: Recomputes the number of lines in */
/*   the facts window. Normally called after the facts  */
/*   window has been resized.                           */
/********************************************************/
grow_facts_window(w)
  WindowPtr w;
  {
   int space, height;
 
   height = (*FactsWindow).portRect.bottom - ((*FactsWindow).portRect.top + SBarWidth);
   lines_in_facts_window = height / facts_line_height;
  }
  
/*******************************************************/
/* SetFactsScroll: Recomputes the number of settings */
/*   for the vertical scroll bar in the facts window.  */
/*******************************************************/
SetFactsScroll()
  {
   AdjustFactsVerticalScroll();
   AdjustFactsHorizontalScroll();
  }
  
/**************************************/
/* AdjustFactsVerticalScroll:         */
/**************************************/
AdjustFactsVerticalScroll()
  {
   struct fact *facts_ptr;
   int count = 0;
   int n, max;
   
   /*===============================================*/
   /* Determine the total number of facts displayed */
   /* in the facts window.                          */
   /*===============================================*/
   
   facts_ptr = get_next_fact(NULL);
   while (facts_ptr != NULL)
     {
      count++;
      facts_ptr = get_next_fact(facts_ptr);
     }
   
   /*=====================================================*/
   /* Set n to the total number of facts minus the number */
   /* of lines that can be displayed on the screen.       */
   /*=====================================================*/
   
   n = count - lines_in_facts_window;
   
   /*====================================================*/
   /* Maximum scroll position is n, or zero if all lines */
   /* fit in present screen.                             */
   /*====================================================*/
   
   max = ( n > 0 ? n : 0 );
   
   /*====================================================*/
   /* Set the maximum value for the vertical scroll bar. */
   /*====================================================*/
   
   SetCtlMax(FactsVScroll,max );  
  }
  
/********************************************/
/* AdjustFactsHorizontalScroll:            */
/********************************************/
AdjustFactsHorizontalScroll()
  {
   int windowWidth;
   int maxTop;
   
   /*========================*/
   /* Get the window width. */
   /*========================*/
   
   windowWidth = ((*FactsWindow).portRect.right - (*FactsWindow).portRect.left) / 
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
   
   SetCtlMax(FactsHScroll,maxTop);
  }
  
/**************************************************************/
/* FactsScrollProc: Scroll the facts within the facts window. */
/**************************************************************/
pascal void FactsScrollProc(theControl, theCode)
  ControlHandle	theControl;
  int theCode;
  {
   int locVal;
   GrafPtr theWindow;
   extern         int scrollAmt;
   extern         int scrollCode;
	
   if (theCode == scrollCode) 
     {
      locVal = GetCtlValue(theControl);
      GetPort(&theWindow );
      SetFactsScroll();
      SetCtlValue(theControl,locVal + scrollAmt);
      UpdateFactsWindow();
	 }
  }
 
/**************************************************/ 
/* DoFactsContent: Handle mouse-down event in the */
/*   content region of the facts window.          */
/**************************************************/  
DoFactsContent()
  {
   int				cntlCode;
   ControlHandle 	theControl;
   int				pageSize;
   GrafPtr			savePort;
   extern         int scrollAmt;
   extern         int scrollCode;
   Point thePoint;
	
   /*========================*/
   /* Save the current port. */
   /*========================*/
   
   GetPort(&savePort);
   
   /*===========================================*/
   /* Set the current port to the facts window. */
   /*===========================================*/
   
   SetPort(FactsWindow);
   
   /*==============================================*/
   /* Get Point in screen coordinates and convert  */
   /* to window coordinates.                       */
   /*==============================================*/
   
   thePoint = TheEvent.where;
   GlobalToLocal(&thePoint);
   
   /*==================================================*/
   /* Determine where the click occured and handle it. */
   /*==================================================*/
   
   switch ( cntlCode = FindControl( thePoint, FactsWindow, &theControl ) ) 
     {
	  case inUpButton:
	  case inDownButton:
	  case inPageUp:
	  case inPageDown:
		if (theControl == FactsVScroll) 
		  { pageSize = lines_in_facts_window; }
		else if (theControl == FactsHScroll)
		  {
           pageSize = (*FactsWindow).portRect.right - (*FactsWindow).portRect.left;
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
		if (TrackControl(theControl, thePoint, &FactsScrollProc));
		break;
		
	  case inThumb:
		if (TrackControl(theControl, thePoint, 0L )) ;
		UpdateFactsWindow();
		break;
	  default:
	    break;
	 }
	
	/*=======================*/
	/* Restore the old port. */
	/*=======================*/
	
	SetPort(savePort);
   }

/**********************************************************/
/* ScrollFacts: Scrolls the contents of the facts window. */
/**********************************************************/
ScrollFacts()
  {
   Point		oldOrigin;
   RgnHandle	tmpRgn;
   int			dv, dh;
   Rect         viewRect;
   int old_number;
   
   /*===================================================*/
   /* Compute the vertical difference in the facts from */
   /* the old location of the origin and the current    */
   /* position indicated by the vertical scroll bar.    */
   /*===================================================*/
   
   old_number = top_fact_number;
   top_fact_number = GetCtlValue(FactsVScroll);
   dv = (old_number - top_fact_number) * facts_line_height;
   
   /*====================================================*/
   /* Compute the horizontal difference in the text from */
   /* the old location of the origin and the current     */
   /* position indicated by the horizontal scroll bar.   */
   /*====================================================*/
  
   old_number = left_margin_number;
   left_margin_number = GetCtlValue(FactsHScroll);
   dh = (old_number - left_margin_number) * HORIZONTAL_SCROLL_INCREMENT;
   
   /*==================================================*/
   /* Offset the destination rectangle to scroll text. */
   /*==================================================*/
   
   viewRect = (*FactsWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = facts_line_height * lines_in_facts_window;
   
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
  
    
/**********************************************************************/
/* SELECT_FACTS_WINDOW: Makes the facts window the front-most window. */
/**********************************************************************/ 
select_facts_window()
  {
   WDHandle theData;
   Handle dataHandle;
   
   /*=====================================================*/
   /* If the facts window does not exist, then create it. */
   /*=====================================================*/
   
   if (FactsWindow == NULL) init_facts_window();
   
   /*==========================*/
   /* Select the facts window. */
   /*==========================*/
   
   SelectWindow(FactsWindow);
   
   /*==========================================*/
   /* Get the window data and lock it. Convert */
   /* the data to a typed handle.              */
   /*==========================================*/
   
   dataHandle = (Handle) GetWRefCon(FactsWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   TheWindow = FactsWindow;
   TheText = NULL;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
  }
           
/**************************************************/
/* MACFACTSFIND: Router function which recognizes */
/*   output sent to the facts window.             */
/**************************************************/
macfactsfind(log_name)
  char *log_name;
  {
   if (strcmp(log_name,"wfacts") == 0)
     { return(TRUE); }

    return(FALSE);
  }
  
/********************************************************************/
/* MACFACTSPRINT: Router function which prints to the facts window. */
/********************************************************************/
macfactsprint(logical_name,str)
  char *logical_name, *str;
  { 
   DrawText(str,0,strlen(str)); 
  }