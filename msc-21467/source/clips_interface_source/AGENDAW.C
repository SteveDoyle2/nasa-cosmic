/*   CLIPS Version 4.30   4/25/89 */

#include <WindowMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include <TextEdit.h>
#include <MenuMgr.h>

#include "interface.h"
#include "engine.h"
#include "data.h"

WindowRecord  AgendaRecord;
WindowPtr	  AgendaWindow;
ControlHandle AgendaVScroll;
ControlHandle AgendaHScroll;

extern int ScreenWidth;
extern int ScreenHeight;

extern MenuHandle FileMenu;

extern EventRecord TheEvent;
extern WindowPtr TheWindow;
extern ControlHandle TheVScrollBar;
extern ControlHandle TheHScrollBar;
extern TEHandle TheText;

static int lines_in_agenda_window;
static int agenda_line_height;
static int top_activation_number;
static int left_margin_number;
static int agenda_start_line;

/*******************************************************/
/* INIT_AGENDA_WINDOW:  Initializes the agenda window. */
/*******************************************************/
init_agenda_window()
  {
   WDHandle theData;
   Handle dataHandle;
   Rect	destRect, viewRect;
   Rect	scrollRect;
   FontInfo	myInfo;
   int height; 
   int space;
   FontInfo finfo;
   int hsize, vsize, wtop, wleft;

   /*===========================================================*/
   /* Create the agenda window from the agenda window template. */
   /*===========================================================*/
   
   SetPort((AgendaWindow = GetNewWindow( AgendaWindowID, &AgendaRecord, -1L )));

   /*======================================================*/
   /* Set the font style to monaco and the font size to 9. */
   /*======================================================*/
   
   TextFont(4);
   TextSize(9);
   
   /*===================================================================*/
   /* Size and Place the window in the lower left corner of the screen. */
   /*===================================================================*/
   
   hsize = 0.60 * (ScreenWidth - (6 + 6 + 6));
   vsize = 0.25 * (ScreenHeight - (46 + 26 + 6));
   wtop = 46 + (0.75 * (ScreenHeight - (46 + 26 + 6))) + 26;
   wleft = 6;
   
   SizeWindow(AgendaWindow,hsize,vsize,FALSE);
   MoveWindow(AgendaWindow,wleft,wtop,FALSE);
   ShowWindow(AgendaWindow);
   
   /*======================================================*/
   /* Create the vertical scroll bar in the agenda window. */
   /*======================================================*/
	
   scrollRect = (*AgendaWindow).portRect;
   scrollRect.left = scrollRect.right-15;
   scrollRect.right += 1;
   scrollRect.bottom -= 14;
   scrollRect.top -= 1;
   AgendaVScroll = NewControl( AgendaWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		
   /*========================================================*/
   /* Create the horizontal scroll bar in the agenda window. */
   /*========================================================*/ 
   
   scrollRect = (*AgendaWindow).portRect;
   scrollRect.top = scrollRect.bottom-15;
   scrollRect.bottom += 1;
   scrollRect.right -= 14;
   scrollRect.left -= 1;
   AgendaHScroll = NewControl(AgendaWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		
   /*=============================================*/
   /* Allocate the window's data record and store */
   /* it as the window's reference constant.      */
   /*=============================================*/
   
   dataHandle = NewHandle(sizeof(WindowData));
   SetWRefCon(AgendaWindow,(long int) dataHandle);
   
   /*=====================================================*/
   /* Lock the data record and convert to a typed handle. */
   /*=====================================================*/
   
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*================================================*/
   /* Store agenda window values in the data record. */
   /*================================================*/
   
   (**theData).editRec = NULL;
   (**theData).vScrollBar = AgendaVScroll;
   (**theData).hScrollBar = AgendaHScroll;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   TheText = (**theData).editRec;
   TheWindow = AgendaWindow;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*====================================*/
   /* Get the window's font information. */
   /*====================================*/
   
   GetFontInfo(&finfo);
   
   /*===============================================================*/
   /* Determine the location on which to begin the first line, the  */
   /* height of a line, the height of the agenda window, the number */
   /* of lines that can be placed in the window, and the index of   */
   /* the activation displayed at the top of the window.            */
   /*===============================================================*/
   
   agenda_start_line = finfo.ascent + finfo.leading;
   agenda_line_height = finfo.ascent + finfo.descent + finfo.leading;
 
   height = (*AgendaWindow).portRect.bottom - ((*AgendaWindow).portRect.top + SBarWidth);
   lines_in_agenda_window = height / agenda_line_height;
   top_activation_number = 0;
   
   /*=============================*/
   /* Draw the window's grow box. */
   /*=============================*/
   
   DrawGrowIcon(AgendaWindow);
   
   /*============================================*/
   /* Enable the Close command on the File menu. */
   /*============================================*/
   
   EnableItem(FileMenu,CloseItem);
  }
     
/******************************************************************/
/* UpdateAgendaWindow: Handle update event for the agenda window. */
/******************************************************************/
UpdateAgendaWindow()
  {
   struct activation *agenda_ptr;
   FontInfo finfo;
   int y, bottom;
   GrafPtr save_port;
   RgnHandle rgn_hnd;
   Rect viewRect;
   int count;
   int startX;
   extern int macagendafind(), macagendaprint();
   
   /*=======================================================*/
   /* Save the old port. Set the port to the agenda window. */
   /*=======================================================*/
   
   GetPort(&save_port);
   SetPort(AgendaWindow);
   
   /*=======================*/
   /* Reset the scroll bar. */
   /*=======================*/
   
   SetAgendaScroll();
   ScrollAgenda();
   
   /*====================================================*/
   /* Begin the update of the agenda window. Temporarily */ 
   /* restricts the visible region of the window by      */
   /* intersecting it with the update region.            */
   /*====================================================*/
   
   BeginUpdate(AgendaWindow);
  
   /*================================================*/
   /* Erase the visible region of the window. Redraw */
   /* the grow box and any window controls.          */
   /*================================================*/
   
   EraseRect(&(*AgendaWindow).portRect);
   DrawGrowIcon(AgendaWindow);
   DrawControls(AgendaWindow);
    
   /*===============================*/
   /* Save the old clipping region. */
   /*===============================*/
   
   rgn_hnd = NewRgn();
   GetClip(rgn_hnd);
   
   /*=================================================*/
   /* Prevent output from spilling into scroll areas. */
   /*=================================================*/
   
   viewRect = (*AgendaWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = agenda_line_height * lines_in_agenda_window;
   ClipRect(&viewRect);
   
   /*===================================================*/
   /* Position the pen in the upper left of the window. */
   /*===================================================*/
   
   startX = 10 - (GetCtlValue(AgendaHScroll) * HORIZONTAL_SCROLL_INCREMENT);
   y = agenda_start_line;
   MoveTo(startX,y);
   
   /*===========================================================*/
   /* Install the router which will print to the agenda window. */
   /*===========================================================*/
   
   add_router("macagenda",20,macagendafind,macagendaprint,NULL,NULL,NULL);
    
   /*====================================================================*/
   /* Skip over any activations that fall out of range above the window. */
   /*====================================================================*/
   
   agenda_ptr = get_next_activation(NULL);
   count = 0;
   while (count < top_activation_number)
     {
      if (agenda_ptr != NULL)
        { agenda_ptr = get_next_activation(agenda_ptr); }
      count++;
     }
     
   /*=============================================================*/
   /* Display the activations that fall within the agenda window. */
   /*=============================================================*/
   
   count = 0;
   while ((agenda_ptr != NULL) && (count <= lines_in_agenda_window))
     { 
      print_activation("wagenda",agenda_ptr);
      y += agenda_line_height;
      MoveTo(startX,y);
      agenda_ptr = get_next_activation(agenda_ptr);
      count++;
     }
     
   /*=====================================================*/
   /* Remove the router that prints to the agenda window. */
   /*=====================================================*/
   
   del_router("macagenda");
     
   /*==================================*/
   /* Restore the old clipping region. */
   /*==================================*/
   
   SetClip(rgn_hnd);
   DisposeRgn(rgn_hnd);
   
   /*====================================================*/
   /* End the update of the agenda window. This restores */
   /* the old visible region of the window.              */
   /*====================================================*/
  
   EndUpdate(AgendaWindow);

   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(save_port);
  }
  
/**********************************************************/
/* COMPLETE_AGENDA_REFRESH: Invalidates the entire agenda */
/*   window, forcing an update.                           */
/**********************************************************/  
complete_agenda_refresh()
  {
   Rect viewRect;
   GrafPtr savePort;
     
   /*========================*/
   /* Save the current port. */
   /*========================*/
   
   GetPort(&savePort);
   
   /*====================================*/
   /* Set the port to the agenda window. */
   /*====================================*/
   
   SetPort(AgendaWindow);
   
   /*=====================================*/
   /* Invalidate the entire viewing area. */
   /*=====================================*/
   
   viewRect = (*AgendaWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom -= SBarWidth;
   InvalRect(&viewRect);
   
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  }
   
/*********************************************************/
/* GROW_AGENDA_WINDOW: Recomputes the number of lines in */
/*   the agenda window. Normally called after the agenda */
/*   window has been resized.                            */
/*********************************************************/
grow_agenda_window(w)
  WindowPtr w;
  {
   int space, height;
 
   height = (*AgendaWindow).portRect.bottom - ((*AgendaWindow).portRect.top + SBarWidth);
   lines_in_agenda_window = height / agenda_line_height;
  }
  
/********************************************************/
/* SetAgendaScroll: Recomputes the number of settings */
/*   for the vertical scroll bar in the agenda window.  */ 
/********************************************************/
SetAgendaScroll()
  {
   AdjustAgendaVerticalScroll();
   AdjustAgendaHorizontalScroll();
  }

/********************************************************/
/* AdjustAgendaVerticalScroll:  */ 
/********************************************************/
AdjustAgendaVerticalScroll()
  {
   struct activation *agenda_ptr;
   int count = 0;
   int n, max;
   
   /*===========================================*/
   /* Determine the total number of activations */
   /* displayed in the agenda window.           */
   /*===========================================*/
   
   agenda_ptr = get_next_activation(NULL);
   while (agenda_ptr != NULL)
     {
      count++;
      agenda_ptr = get_next_activation(agenda_ptr);
     }
   
   /*===========================================================*/
   /* Set n to the total number of activations minus the number */
   /* of lines that can be displayed on the screen.             */
   /*===========================================================*/
   
   n = count - lines_in_agenda_window;
   
   /*====================================================*/
   /* Maximum scroll position is n, or zero if all lines */
   /* fit in present screen.                             */
   /*====================================================*/
   
   max = ( n > 0 ? n : 0 );
   
   /*====================================================*/
   /* Set the maximum value for the vertical scroll bar. */
   /*====================================================*/
   
   SetCtlMax(AgendaVScroll,max);
  }
  
/********************************************/
/* AdjustAgendaHorizontalScroll:            */
/********************************************/
AdjustAgendaHorizontalScroll()
  {
   int windowWidth;
   int maxTop;
   
   /*========================*/
   /* Get the window width. */
   /*========================*/
   
   windowWidth = ((*AgendaWindow).portRect.right - (*AgendaWindow).portRect.left) / 
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
   
   SetCtlMax(AgendaHScroll,maxTop);
  }
  
/**********************************************************************/
/* AgendaScrollProc: Scroll the activations within the agenda window. */
/**********************************************************************/
pascal void AgendaScrollProc(theControl, theCode)
  ControlHandle	theControl;
  int theCode;
  {
   int locVal;
   GrafPtr theWindow;
   extern         int scrollAmt;
   extern         int scrollCode;
	
   if (theCode==scrollCode) 
     {
      locVal = GetCtlValue(theControl);
      GetPort(&theWindow );
      SetAgendaScroll();
      SetCtlValue(theControl,locVal+scrollAmt);
      UpdateAgendaWindow();
	 }
  }
  
/***************************************************/ 
/* DoAgendaContent: Handle mouse-down event in the */
/*   content region of the agenda window.          */
/***************************************************/  
DoAgendaContent()
  {
   int				cntlCode;
   ControlHandle 	theControl;
   int				pageSize;
   GrafPtr			savePort;
   extern         int scrollAmt;
   extern         int scrollCode;
   Point  thePoint;
	
   /*=======================================================*/
   /* Save the old port. Set the port to the agenda window. */
   /*=======================================================*/
   
   GetPort(&savePort);
   SetPort(AgendaWindow);
   
   /*==============================================*/
   /* Get the mouse location in screen coordinates */
   /* and convert it to window coordinates.        */
   /*==============================================*/
   
   thePoint = TheEvent.where;
   GlobalToLocal(&thePoint);
   
   /*==================================================*/
   /* Determine where the click occured and handle it. */
   /*==================================================*/
   
   switch ( cntlCode = FindControl( thePoint, AgendaWindow, &theControl ) ) 
     {
	  case inUpButton:
	  case inDownButton:
	  case inPageUp:
	  case inPageDown:
		if (theControl == AgendaVScroll) 
		  { pageSize = lines_in_agenda_window; }
		else if (theControl == AgendaHScroll)
		  {
           pageSize = (*AgendaWindow).portRect.right - (*AgendaWindow).portRect.left;
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
		if (TrackControl(theControl, thePoint, &AgendaScrollProc));
		break;
	  case inThumb:
		if (TrackControl(theControl, thePoint, 0L )) ;
		UpdateAgendaWindow();
		break;
	  default:
	    break;
	 }
   
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  }

/*************************************************************/
/* ScrollAgenda: Scrolls the contents of the agenda window. */
/*************************************************************/
ScrollAgenda()
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
   
   old_number = top_activation_number;
   top_activation_number = GetCtlValue(AgendaVScroll);
   dv = (old_number - top_activation_number) * agenda_line_height;
   
   /*====================================================*/
   /* Compute the horizontal difference in the text from */
   /* the old location of the origin and the current     */
   /* position indicated by the horizontal scroll bar.   */
   /*====================================================*/
  
   old_number = left_margin_number;
   left_margin_number = GetCtlValue(AgendaHScroll);
   dh = (old_number - left_margin_number) * HORIZONTAL_SCROLL_INCREMENT;
   
   /*==================================================*/
   /* Offset the destination rectangle to scroll text. */
   /*==================================================*/
   
   viewRect = (*AgendaWindow).portRect;
   viewRect.right -= SBarWidth;
   viewRect.bottom = agenda_line_height * lines_in_agenda_window;
   
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
/* SELECT_AGENDA_WINDOW: Makes the agenda window the front-most window. */
/************************************************************************/ 
select_agenda_window()
  {
   WDHandle theData;
   Handle dataHandle;
   
   /*======================================================*/
   /* If the agenda window does not exist, then create it. */
   /*======================================================*/
   
   if (AgendaWindow == NULL) init_agenda_window();
   
   /*===========================*/
   /* Select the agenda window. */
   /*===========================*/
   
   SelectWindow(AgendaWindow);
   
   /*==========================================*/
   /* Get the window data and lock it. Convert */
   /* the data to a typed handle.              */
   /*==========================================*/
   
   dataHandle = (Handle) GetWRefCon(AgendaWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheWindow = AgendaWindow;
   TheText = NULL; 
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
  }
            
/****************************************************/
/* MACAGENDAFIND:  Router function which recognizes */
/*   output sent to the agenda window.              */  
/****************************************************/
macagendafind(log_name)
  char *log_name;
  {
   if (strcmp(log_name,"wagenda") == 0)
     { return(TRUE); }

    return(FALSE);
  }
  
/**********************************************************************/
/* MACAGENDAPRINT: Router function which prints to the agenda window. */
/**********************************************************************/
macagendaprint(logical_name,str)
  char *logical_name, *str;
  { 
   DrawText(str,0,strlen(str)); 
  }