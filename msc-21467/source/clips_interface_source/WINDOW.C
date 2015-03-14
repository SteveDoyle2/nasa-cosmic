/*   CLIPS Version 4.30   4/25/89 */

#include <EventMgr.h>
#include <WindowMgr.h>
#include <ControlMgr.h>
#include <TextEdit.h>
#include <MenuMgr.h>

#include "interface.h"
#include "data.h"

extern EventRecord TheEvent;
extern WindowPtr TheWindow;
extern ControlHandle TheVScrollBar;
extern ControlHandle TheHScrollBar;
extern TEHandle TheText;
extern MenuHandle FileMenu;
extern MenuHandle EditMenu;
extern MenuHandle BufferMenu;
extern CursHandle Watch;

extern int ScreenWidth;
extern int ScreenHeight;

int	scrollCode;
int scrollAmt;

extern WindowPtr DisplayWindow;
extern ControlHandle DisplayScroll;

extern WindowPtr AgendaWindow;
extern ControlHandle AgendaScroll;

extern WindowPtr FactsWindow;
extern ControlHandle FactsScroll;

/****************************************************/
/* DODRAG: Handles mouse-down event in drag region. */
/****************************************************/
DoDrag(whichWindow)
  WindowPtr whichWindow;
  {
   Rect limitRect;
   
   /*====================================================*/
   /* If window is not frontmost then make it frontmost. */
   /* If command key is down, allow the window to be     */
   /* dragged if it is not frontmost.                    */
   /*====================================================*/
    
   if ((whichWindow != FrontWindow()) && 
       (BitAnd(TheEvent.modifiers,cmdKey) == 0))
     { 
      SelectWindow(whichWindow);
      return; 
     }
     
   /*==========================*/
   /* Set the limit rectangle. */
   /*==========================*/
   
   SetRect(&limitRect,0,0,1024,1024);
   
   /*===================================*/
   /* Inset rectangle by screen margin. */
   /*===================================*/
   
   InsetRect(&limitRect,ScreenMargin,ScreenMargin);
   
   /*===============================*/
   /* Let the user drag the window. */
   /*===============================*/
   
   DragWindow(whichWindow,TheEvent.where,&limitRect);
  }
  
/****************************************************/
/* DOGROW: Handles mouse-down event in size region. */
/****************************************************/
DoGrow(whichWindow)
  WindowPtr whichWindow;
  {
   Rect sizeRect;
   long int newSize;
   int newWidth;
   int newHeight;
   long new_size;
   
   /*====================================================*/
   /* If window is not frontmost then make it frontmost. */
   /*====================================================*/
    
   if (whichWindow != FrontWindow())
     { 
      SelectWindow(whichWindow);
      return; 
     }
   
   /*=====================================================*/
   /* Set the minimum and maximum area for window growth. */
   /*=====================================================*/
    
   SetRect(&sizeRect,MinWidth,MinHeight,ScreenWidth,(ScreenHeight - MenuBarHeight));
      
   /*=======================================================*/
   /* Call GrowWindow to get new size. Note that GrowWindow */
   /* does not actually update the window size.             */
   /*=======================================================*/		
    
   newSize = GrowWindow(whichWindow,TheEvent.where,&sizeRect);
    
   /*========================================*/
   /* Return if window size was not changed. */
   /*========================================*/		
   
   if (new_size == 0) return;
     
   /*====================================*/
   /* Clear the entire port rectangle to */ 
   /* the window's background pattern.   */
   /*====================================*/	
    
   EraseRect(&(*whichWindow).portRect);
       
   /*=======================================*/
   /* Call SizeWindow to resize the window. */
   /*=======================================*/
      
   newWidth = LoWord(newSize);
   newHeight = HiWord(newSize);
   SizeWindow(whichWindow,newWidth,newHeight,TRUE);
         
   /*======================================================*/
   /* Invalidate viewing rectangle and fix the scroll bar. */
   /*======================================================*/
   
   InvalRect(&(*whichWindow).portRect);
   FixScrollBar();
      
   /*=======================================*/
   /* Perform special grow update routines. */
   /*=======================================*/
   
   if (whichWindow == DisplayWindow)
     { grow_display_window(whichWindow); }
   else if (whichWindow == AgendaWindow)
     { grow_agenda_window(whichWindow); }
   else if (whichWindow == FactsWindow)
     { grow_facts_window(whichWindow); }
   else
     { FixText(); }
  }
  
/****************************************************/
/* DOZOOM: Handles mouse-down event in zoom region. */
/****************************************************/
DoZoom(whichWindow,direction)
  WindowPtr whichWindow;
  int direction;
  {
   Rect sizeRect;
   long int newSize;
   int newWidth;
   int newHeight;
   
   /*====================================================*/
   /* If window is not frontmost then make it frontmost. */
   /*====================================================*/
    
   if (whichWindow != FrontWindow())
     { 
      SelectWindow(whichWindow);
      return; 
     }
     
   /*==================================================*/
   /* If click did not occur in zoom box, then return. */
   /*==================================================*/
   
   if (TrackBox(whichWindow,TheEvent.where,direction) == 0) return;
   
   /*====================================*/
   /* Clear the entire port rectangle to */ 
   /* the window's background pattern.   */
   /*====================================*/	
   
   EraseRect(&(*TheWindow).portRect);
      
   /*=======================================*/
   /* Call ZoomWindow to resize the window. */
   /*=======================================*/
   
   ZoomWindow(whichWindow,direction,FALSE);
      
   /*======================================================*/
   /* Invalidate viewing rectangle and fix the scroll bar. */
   /*======================================================*/
     
   InvalRect(&(*whichWindow).portRect);
   FixScrollBar();
            
   /*=======================================*/
   /* Perform special grow update routines. */
   /*=======================================*/
   
   if (whichWindow == DisplayWindow)
     { grow_display_window(whichWindow); }
   else if (whichWindow == AgendaWindow)
     { grow_agenda_window(whichWindow); }
   else if (whichWindow == FactsWindow)
     { grow_facts_window(whichWindow); }
   else
     { FixText(); }
  }

/*********************************************/
/* FIXSCROLLBAR: Resize window's scroll bar. */
/*********************************************/
FixScrollBar()
  {
   if (TheVScrollBar != NULL)
     {
   /*======================*/
   /* Hide the scroll bar. */
   /*======================*/
   
   HideControl(TheVScrollBar);
   
   /*=============================================================*/
   /* Move the top-left corner of the scroll bar. All for 1 pixel */
   /* overlap at right and overlap window top by 1 pixel.         */
   /*=============================================================*/
   
   MoveControl(TheVScrollBar,(*TheWindow).portRect.right - (SBarWidth - 1), -1);
  
   /*==============================================*/
   /* Adjust the bottom-right corner of the scroll */
   /* bar allowing room for the size box.          */
   /*==============================================*/
   
   SizeControl(TheVScrollBar,SBarWidth,((*TheWindow).portRect.bottom + 1) -
                                      ((*TheWindow).portRect.top - 1) -
                                      (SBarWidth - 1));
                                      
   /*===========================*/
   /* Redisplay the scroll bar. */
   /*===========================*/
                           
   ShowControl(TheVScrollBar);
       
   /*======================================*/
   /* Avoid updating the scroll bar again. */
   /*======================================*/
   
   ValidRect(&(**TheVScrollBar).contrlRect);
     }
     
     
   if (TheHScrollBar != NULL)
     {
   /*======================*/
   /* Hide the scroll bar. */
   /*======================*/
   
   HideControl(TheHScrollBar);
   
   /*=============================================================*/
   /* Move the top-left corner of the scroll bar. All for 1 pixel */
   /* overlap at right and overlap window top by 1 pixel.         */
   /*=============================================================*/
   
   MoveControl(TheHScrollBar,
               -1,(*TheWindow).portRect.bottom - (SBarWidth - 1));
  
   /*==============================================*/
   /* Adjust the bottom-right corner of the scroll */
   /* bar allowing room for the size box.          */
   /*==============================================*/
   
   SizeControl(TheHScrollBar,
               ((*TheWindow).portRect.right + 1) -
                ((*TheWindow).portRect.left - 1) -
                    (SBarWidth - 1),
               SBarWidth);
                                      
   /*===========================*/
   /* Redisplay the scroll bar. */
   /*===========================*/
                           
   ShowControl(TheHScrollBar);
       
   /*======================================*/
   /* Avoid updating the scroll bar again. */
   /*======================================*/
   
   ValidRect(&(**TheHScrollBar).contrlRect);
     }
  }
  
/********************************************************/
/* DOSELECT: Handle mouse-down event in text rectangle. */
/********************************************************/
DoSelect(thePoint)
  Point thePoint;
  {
   int extend;
   
   /*========================*/
   /* Is the shift key down? */
   /*========================*/
   
   extend = (BitAnd(TheEvent.modifiers,shiftKey) != 0);
   
   /*====================*/
   /* Do text selection. */
   /*====================*/
   
   TEClick(thePoint,extend,TheText);
   
   /*============================*/
   /* Enable/disable menu items. */
   /*============================*/
   
   FixEditMenu();
  }
  
/*********************************************/
/* DOCONTENT: Handle mouse-down event in the */
/* content region of the active window.      */
/*********************************************/
DoContent(whichWindow)
  WindowPtr whichWindow;
  {
   Point thePoint;
   ControlHandle theControl;
   int thePart;
   
   /*===================================================*/
   /* If the window is inactive, then just activate it. */
   /*===================================================*/
   
   if (whichWindow != FrontWindow())
     { 
      SelectWindow(whichWindow);
      return; 
     }
     
   /*===============================================================*/
   /* Call special routines for display, facts, and agenda windows. */
   /*===============================================================*/
   
   if (whichWindow == DisplayWindow)
	 { 
	  DoDisplayContent(); 
	  return;
	 }
   else if (whichWindow == AgendaWindow)
	 { 
	  DoAgendaContent(); 
	  return;
	 }
   else if (whichWindow == FactsWindow)
	 { 
	  DoFactsContent(); 
	  return;
	 } 
	  
   /*==============================================*/
   /* Process content mouse-down for edit windows. */
   /* Get Point in screen coordinates and convert  */
   /* to window coordinates.                       */
   /*==============================================*/
   
   thePoint = TheEvent.where;
   GlobalToLocal(&thePoint);
      
   /*=====================================*/
   /* Was mouse pressed inside a control? */
   /*=====================================*/
   
   thePart = FindControl(thePoint,whichWindow,&theControl);
      
   /*======================================================*/
   /* If control was a scroll bar, then scroll the window. */
   /*======================================================*/
   
   if (theControl == TheVScrollBar)
     { DoVScroll(thePart,thePoint); }
     
   else if (theControl == TheHScrollBar)
     { DoHScroll(thePart,thePoint); }
     
   /*========================================================*/
   /* else if no control was found and the mouse-down event  */
   /* was in the text rectangle, then handle text selection. */
   /*========================================================*/
   
   else if (theControl == NULL)
     {
      if (PtInRect(thePoint,&(**TheText).viewRect))
        { DoSelect(thePoint); }
     }
  }
 
/**************************************************************/
/* DoVScroll: Handle mouse-down event in vertical scroll bar. */
/**************************************************************/
DoVScroll(thePart,thePoint)
  int thePart;
  Point thePoint;
  {
   extern pascal void ScrollVText();
   
   /*==============================================================*/
   /* If the indicator is being dragged, then track the mouse with */
   /* no action procedure and then adjust text to new setting.     */
   /*==============================================================*/
   
   if (thePart == inThumb)
     {
      thePart = TrackControl(TheVScrollBar,thePoint,NULL);
      AdjustText();
     }
     
   /*================================================*/
   /* else track the mouse with a continuous scroll. */
   /*================================================*/
   
   else
     { thePart = TrackControl(TheVScrollBar,thePoint,&ScrollVText); }
  }
  

/******************************************************/
/* ScrollVText: Scroll text vertically within window. */
/******************************************************/
pascal void ScrollVText(theControl,thePart)
  ControlHandle theControl;
  int thePart;
  {
   int delta;
   int oldValue;
   
   switch(thePart)
     {
      /*===============================*/
      /* Scroll up one line at a time. */
      /*===============================*/
      
      case inUpButton:
        delta = -1;
        break;
        
      /*=================================*/
      /* Scroll down one line at a time. */
      /*=================================*/
      
      case inDownButton:
        delta = 1;
        break;
        
      /*========================================*/
      /* Scroll up by height of text rectangle. */
      /*========================================*/
      
      case inPageUp:
        delta = (((**TheText).viewRect.top - (**TheText).viewRect.bottom) 
                 / (**TheText).lineHeight) + 1;
        break;
        
      /*==========================================*/
      /* Scroll down by height of text rectangle. */
      /*==========================================*/
      
      case inPageDown:
        delta = (((**TheText).viewRect.bottom - (**TheText).viewRect.top) 
                 / (**TheText).lineHeight) - 1;
        break;
     
      default:
        break;
     }
     
   /*===========================================================*/
   /* If the mouse is still in the original part, then get the  */
   /* old setting, adjust by the scroll amount, and scroll text */
   /* to match the new setting.                                 */
   /*===========================================================*/
   
   if (thePart != 0)
     {
      oldValue = GetCtlValue(theControl);
      SetCtlValue(theControl,oldValue + delta);
      AdjustText();
     }
  }

/****************************************************************/
/* DoHScroll: Handle mouse-down event in horizontal scroll bar. */
/****************************************************************/
DoHScroll(thePart,thePoint)
  int thePart;
  Point thePoint;
  {
   extern pascal void ScrollHText();
   
   /*==============================================================*/
   /* If the indicator is being dragged, then track the mouse with */
   /* no action procedure and then adjust text to new setting.     */
   /*==============================================================*/
   
   if (thePart == inThumb)
     {
      thePart = TrackControl(TheHScrollBar,thePoint,NULL);
      AdjustText();
     }
     
   /*================================================*/
   /* else track the mouse with a continuous scroll. */
   /*================================================*/
   
   else
     { thePart = TrackControl(TheHScrollBar,thePoint,&ScrollHText); }
  }
  
/********************************************************/
/* ScrollHText: Scroll text horizontally within window. */
/********************************************************/
pascal void ScrollHText(theControl,thePart)
  ControlHandle theControl;
  int thePart;
  {
   int delta;
   int oldValue;
   
   switch(thePart)
     {
      /*===============================*/
      /* Scroll up one line at a time. */
      /*===============================*/
      
      case inUpButton:
        delta = -1;
        break;
        
      /*=================================*/
      /* Scroll down one line at a time. */
      /*=================================*/
      
      case inDownButton:
        delta = 1;
        break;
        
      /*========================================*/
      /* Scroll up by height of text rectangle. */
      /*========================================*/
      
      case inPageUp:
        delta = (((**TheText).viewRect.left - (**TheText).viewRect.right) 
                 / HORIZONTAL_SCROLL_INCREMENT) + 1;
        break;
        
      /*==========================================*/
      /* Scroll down by height of text rectangle. */
      /*==========================================*/
      
      case inPageDown:
        delta = (((**TheText).viewRect.right - (**TheText).viewRect.left) 
                 / HORIZONTAL_SCROLL_INCREMENT) - 1;
        break;
     
      default:
        break;
     }
     
   /*===========================================================*/
   /* If the mouse is still in the original part, then get the  */
   /* old setting, adjust by the scroll amount, and scroll text */
   /* to match the new setting.                                 */
   /*===========================================================*/
   
   if (thePart != 0)
     {
      oldValue = GetCtlValue(theControl);
      SetCtlValue(theControl,oldValue + delta);
      AdjustText();
     }
  }

/**********************************************************************/
/* ADJUSTTEXT: Adjust text within window to match scroll bar setting. */
/**********************************************************************/
AdjustText()
  {
   int oldVScroll, oldHScroll;
   int newVScroll, newHScroll;
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
    
   /*====================================*/
   /* Get the current offset and use the */
   /* scroll bar to get the new offset.  */
   /*====================================*/
   
   oldVScroll = (**TheText).viewRect.top - (**TheText).destRect.top;
   newVScroll = GetCtlValue(TheVScrollBar) * (**TheText).lineHeight;
   
   oldHScroll = (**TheText).viewRect.left - (**TheText).destRect.left 
                + TextMargin;
   newHScroll = GetCtlValue(TheHScrollBar) * HORIZONTAL_SCROLL_INCREMENT;
  
   /*==============================================================*/ 
   /* Scroll by the difference. Note that if TEScroll is called    */
   /* with a value of zero, there is a bug which then prevents the */
   /* the insertion point from blinking properly. Hence the check  */
   /* for oldScroll equal to newScroll.                            */
   /*==============================================================*/ 
    
   if (((oldVScroll - newVScroll) != 0) || 
       ((oldHScroll - newHScroll) != 0))
     { TEScroll((oldHScroll - newHScroll),(oldVScroll - newVScroll),TheText); }
     
   /*=========================*/
   /* Unlock the edit record. */
   /*=========================*/
   
   HUnlock( (Handle) TheText);
  }
  
/**********************************/
/* DOUPDATE: Handle update event. */
/**********************************/
DoUpdate()
  {
   GrafPtr savePort;
   WindowPtr whichWindow;
   WDHandle theData;
   Handle dataHandle;
   
   /*=============================================*/
   /* Determine which window needs to be updated. */
   /*=============================================*/
   
   whichWindow = (WindowPtr) TheEvent.message;
   
   /*===========================================*/
   /* Call special routines for handling update */
   /* on display, fact, and agenda windows.     */
   /*===========================================*/
   
   if (whichWindow == DisplayWindow) 
	 { 
	  UpdateDisplayWindow();
	  return;
	 }
   else if (whichWindow == AgendaWindow)
	 {
	  UpdateAgendaWindow();
	  return;  
	 }
   else if (whichWindow == FactsWindow)
     { 
	  UpdateFactsWindow(); 
	  return;
     }
		      
   /*============================================*/
   /* Save the previous port and make the window */
   /* to be updated the current port.            */       
   /*============================================*/
   
   GetPort(&savePort);	    
   SetPort(whichWindow);
         
   /*==================================*/
   /* Handle updates for edit windows. */
   /*==================================*/
  
   PerformUpdate(whichWindow);
      
   /*============================*/
   /* Restore the original port. */
   /*============================*/
   
   SetPort(savePort);
  }
  
/****************************************************/
/* PERFORMUPDATE: Handles updates for edit windows. */
/****************************************************/
PerformUpdate(whichWindow)
  WindowPtr whichWindow;
  {
   WDHandle theData;
   Handle dataHandle;
       
   /*===========================================*/
   /* Restrict visible region to update region. */
   /*===========================================*/
   
   BeginUpdate(whichWindow);
   
   /*==========================*/
   /* Clear the update region. */
   /*==========================*/
   
   EraseRect(&(*whichWindow).portRect);
   
   /*========================================*/
   /* Redraw the size box and the grow bars. */
   /*========================================*/
   
   DrawGrowIcon(whichWindow);
   DrawControls(whichWindow);
    
   /*=================================================*/
   /* Get the window data, lock the data record, then */
   /* convert the data record to a type handle.       */
   /*=================================================*/
   
   dataHandle = (Handle) GetWRefCon(whichWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
      
   /*==================*/
   /* Redraw the text. */
   /*==================*/
   
   TEUpdate( &(**(**theData).editRec).viewRect,(**theData).editRec);
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*==================================*/
   /* Restore original visible region. */
   /*==================================*/
   
   EndUpdate(whichWindow);
  }
    
/******************************************************/
/* DOACTIVATE: Handle activate (or deactivate) event. */
/******************************************************/
DoActivate()
  {
   WindowPtr whichWindow;
   WDHandle theData;
   Handle dataHandle;
   int text_window = TRUE;
   
   /*==================================*/
   /* Convert long integer to pointer. */
   /*==================================*/
   
   whichWindow = (WindowPtr) TheEvent.message;
	        
   /*===================================*/
   /* Make the window the current port. */
   /*===================================*/
   
   SetPort(whichWindow);
	        
   /*====================================*/
   /* Highlight or unhighlight size box. */
   /*====================================*/
   
   DrawGrowIcon(whichWindow);
    
   /*=======================================*/
   /* Get window data record, lock it, then */
   /* convert it to a typed handle.         */
   /*=======================================*/
   
   dataHandle = (Handle) GetWRefCon(whichWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*================================================*/
   /* Test the activate/deactivate bit. If true then */
   /* activation event has occured.                  */
   /*================================================*/
    
   if (BitAnd(TheEvent.modifiers,activeFlag) != 0)
     {
      /*==================================*/
      /* Set global pointers and handles. */
      /*==================================*/
      
      TheWindow = whichWindow;
      TheVScrollBar = (**theData).vScrollBar;
      TheHScrollBar = (**theData).hScrollBar;
      TheText = (**theData).editRec;
        
      /*==========================*/
      /* Activate the scroll bar. */
      /*==========================*/
      
      if (TheVScrollBar != NULL) HiliteControl(TheVScrollBar,activeValue);
      if (TheHScrollBar != NULL) HiliteControl(TheHScrollBar,activeValue);
      
      /*========================================*/
      /* If the window has an edit record, then */
      /* highlight the edit record's selection. */
      /*========================================*/
      
      if (TheText != NULL) 
        { TEActivate((**theData).editRec); }
      
      /*===========================================*/
      /* If coming from a system window, then copy */
      /* the desk scrap to the toolbox scrap.      */
      /*===========================================*/
      
      if (BitAnd(TheEvent.modifiers,changeFlag) != 0)
        { ReadDeskScrap(); }
      
      /*================================================*/
      /* Enable/disable various commands depending upon */
      /* whether the window has an edit record.         */
      /*================================================*/
      
      if (TheText != NULL)
        {
         FixEditMenu();
         EnableItem(EditMenu,BalanceItem);
         EnableItem(EditMenu,SetFontItem);
         FixSearchMenu();
         EnableItem(FileMenu,SaveAsItem);
         EnableItem(FileMenu,PrintItem);
         EnableItem(BufferMenu,CompileBufferItem);
         if ((**theData).dirty)
           { EnableItem(FileMenu,SaveItem); }
         if ((**theData).dirty && ((**theData).volNumber != 0))
           { EnableItem(FileMenu,RevertItem); }
        }
      else
        { 
         DisableItem(EditMenu,UndoItem);
         DisableItem(EditMenu,CutItem);
         DisableItem(EditMenu,CopyItem);
         DisableItem(EditMenu,PasteItem);
         DisableItem(EditMenu,ClearItem);
         DisableItem(EditMenu,BalanceItem);
         DisableItem(EditMenu,SetFontItem);
         
         DisableItem(FileMenu, SaveItem);
         DisableItem(FileMenu, SaveAsItem);
         DisableItem(FileMenu, RevertItem);
         if (TheWindow == DisplayWindow)
           { EnableItem(FileMenu, PrintItem); }
         else
           { DisableItem(FileMenu, PrintItem); }
      
         DisableItem(BufferMenu, FindItem);
         DisableItem(BufferMenu, FindAgainItem);
         DisableItem(BufferMenu, ReplaceItem);
         DisableItem(BufferMenu, ReplaceAndFindItem);
         DisableItem(BufferMenu, ReplaceAllItem);
         DisableItem(BufferMenu, CompileSelectionItem);
         DisableItem(BufferMenu, CompileBufferItem);
        }
     
     }
     
   /*==================================*/
   /* else handle window deactivation. */
   /*==================================*/
   
   else
     {
      /*====================================*/
      /* Clear global pointers and handles. */
      /*====================================*/
      
      TheWindow = NULL;
      TheVScrollBar = NULL;
      TheHScrollBar = NULL;
      TheText = NULL;
      
      /*===========================================*/
      /* If the window has an edit record, then    */
      /* unhighlight the edit record's selection.  */
      /*===========================================*/
      
      if ((**theData).editRec != NULL) TEDeactivate((**theData).editRec);
      
      /*============================*/
      /* Deactivate the scroll bar. */
      /*============================*/
      
      if ((**theData).vScrollBar != NULL) 
        { HiliteControl((**theData).vScrollBar,inactiveValue); }
      if ((**theData).hScrollBar != NULL) 
        { HiliteControl((**theData).hScrollBar,inactiveValue); }
      
      /*============================================================*/
      /* If exiting to a system window, then copy the toolbox scrap */
      /* to the desk scrap and enable standard editing commands for */
      /* desk accessories.                                          */
      /*============================================================*/
      
      if (BitAnd(TheEvent.modifiers,changeFlag) != 0)
        { 
         WriteDeskScrap(); 
                 
         SetItem(EditMenu,UndoItem,"\pUndo");
         EnableItem(EditMenu,UndoItem);
         EnableItem(EditMenu,CutItem);
         EnableItem(EditMenu,CopyItem);
         EnableItem(EditMenu,PasteItem);
         EnableItem(EditMenu,ClearItem);
        }
      
      /*===============================================*/
      /* Disable certain commands for desk accessories */
      /* or desk without editing window.               */
      /*===============================================*/
      
      DisableItem(EditMenu,   BalanceItem); 
      DisableItem(EditMenu,SetFontItem);
      DisableItem(BufferMenu, FindItem);
      DisableItem(BufferMenu, FindAgainItem);
      DisableItem(BufferMenu, ReplaceItem);
      DisableItem(BufferMenu, ReplaceAndFindItem);
      DisableItem(BufferMenu, ReplaceAllItem);
      DisableItem(BufferMenu, CompileSelectionItem);
      DisableItem(BufferMenu, CompileBufferItem);
       
      DisableItem(FileMenu,SaveItem);
      DisableItem(FileMenu,SaveAsItem);
      DisableItem(FileMenu,RevertItem);   
     }
   
   /*=====================*/
   /* Unlock data record. */
   /*=====================*/
   
   HUnlock(dataHandle);
  }

/******************************************************/
/* DOGOAWAY: Handle mouse-down event in close region. */
/******************************************************/
DoGoAway(whichWindow)
  WindowPtr whichWindow;
  {
   /*===================================================*/
   /* If the window is inactive, then just activate it. */
   /* Otherwise track the mouse in the close region and */
   /* close the window if the click is completed.       */
   /*===================================================*/
   
   if (whichWindow != FrontWindow())
     { SelectWindow(whichWindow); }  
   else if (TrackGoAway(whichWindow,TheEvent.where))
     { DoClose(); }
  }

/********************************************/
/* FIXTEXT: Resize window's text rectangle. */
/********************************************/
FixText()
  {
   int topLine;
   int firstChar;
   int maxTop;
   int theLine;
   int oldVScroll, newVScroll, oldHScroll, newHScroll;
   
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*==========================================*/
   /* Get the previous first line and find the */
   /* first character previously visible.      */
   /*==========================================*/
                     
   topLine = GetCtlValue(TheVScrollBar);
   firstChar = (**TheText).lineStarts[topLine];
   
   /*=================================================================*/
   /* Display the text in the window's port rectangle. Adjust the     */
   /* rectangle by excluding the top and bottom scroll bars (allowing */
   /* for a 1 pixel overlap), and truncate the bottom to display only */
   /* a whole number of lines.                                        */
   /*=================================================================*/
   
   (**TheText).viewRect = (*TheWindow).portRect;
   (**TheText).viewRect.top += TextMargin;
   (**TheText).viewRect.right = (**TheText).viewRect.right - (SBarWidth - 1);
   (**TheText).viewRect.bottom = (**TheText).viewRect.bottom - (SBarWidth - 1);
   (**TheText).viewRect.bottom = 
      (((**TheText).viewRect.bottom - (**TheText).viewRect.top) / 
        (**TheText).lineHeight) * (**TheText).lineHeight + TextMargin;
   
   /*=======================================================*/
   /* Wrap text to same rectangle and inset by text margin. */
   /*=======================================================*/
   
   (**TheText).destRect = (**TheText).viewRect;
   (**TheText).destRect.left += TextMargin;  
   
   /*==========================*/
   /* Recalibrate line starts. */
   /*==========================*/
   
   TECalText(TheText);
   
   /*==================================*/
   /* Adjust scroll bar to new length. */
   /*==================================*/
   
   AdjustScrollBar();
   
   /*====================================================*/
   /* Determine which line contains the first character. */
   /*====================================================*/
   
   theLine = 0;
   while ((**TheText).lineStarts[theLine+1] <= firstChar)
     { theLine = theLine + 1; }
     
   /*=======================================*/
   /* Set scroll bar value to the top line. */
   /*=======================================*/
   
   SetCtlValue(TheVScrollBar,theLine);
        
   /*===================================*/
   /* Compute the old and new top line. */
   /*===================================*/
   
   oldVScroll = (**TheText).viewRect.top - (**TheText).destRect.top;
   newVScroll = GetCtlValue(TheVScrollBar) * (**TheText).lineHeight;
   
   oldHScroll = (**TheText).viewRect.left - (**TheText).destRect.left
                + TextMargin;
   newHScroll = GetCtlValue(TheHScrollBar) * HORIZONTAL_SCROLL_INCREMENT;
   
   /*=====================================================*/
   /* Offset the text destination rectangle by the amount */
   /* necessary to bring the new top line to the top of   */
   /* the window. In effect, scroll destination rectangle */
   /* of the text in relation to the view rectangle.      */         
   /*=====================================================*/
   
   OffsetRect(&(**TheText).destRect, (oldHScroll - newHScroll), 
              (oldVScroll - newVScroll));
     
   /*=========================*/
   /* Unlock the edit record. */
   /*=========================*/
   
   HUnlock( (Handle) TheText);
  }
  
/************************************************/
/* SCROLLCHARACTER: Scroll character into view. */
/************************************************/
ScrollCharacter(theCharacter,toWhere)
  int theCharacter;
  int toWhere;
  {
   int theLine;
   int windowHeight;
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*=========================================================*/
   /* Find the line containing the character. If scrolling to */
   /* top, then this line should be at the top of the window. */
   /*=========================================================*/
   
   theLine = 0;
   while ((**TheText).lineStarts[theLine+1] <= theCharacter)
     { theLine = theLine + 1; }
   
   /*===================================*/
   /* If scrolling to bottom of window, */
   /* then offset by the window height. */
   /*===================================*/

   if (toWhere == ToBottom)
     {
      windowHeight = ((**TheText).viewRect.bottom - (**TheText).viewRect.top) / 
                     (**TheText).lineHeight;
      theLine = theLine - (windowHeight - 1);
     }
     
   /*=============================================*/
   /* Else if scrolling to middle of window, then */
   /* offset by half of the window height.        */
   /*=============================================*/
   
   else if (toWhere == ToMiddle)
     {
      windowHeight = ((**TheText).viewRect.bottom - (**TheText).viewRect.top) / 
                     (**TheText).lineHeight;
      theLine = theLine - ((windowHeight - 1) / 2);
     }
     
   /*===================================*/
   /* Adjust setting of the scroll bar. */
   /*===================================*/
   
   SetCtlValue(TheVScrollBar,theLine);
   
   /*===================================*/
   /* Scroll text to match new setting. */
   /*===================================*/
   
   AdjustText();
   
   /*=====================*/
   /* Unlock edit record. */
   /*=====================*/
   
   HUnlock( (Handle) TheText);
  }

/*************************************************************/
/* ADJUSTSCROLLBAR: Adjust scroll bar to length of document. */
/*************************************************************/
AdjustScrollBar()
  {
   int windowHeight, windowWidth;
   int maxTop;
   
   if (TheVScrollBar != NULL)
     {
   /*========================*/
   /* Get the window height. */
   /*========================*/
   
   windowHeight = ((**TheText).viewRect.bottom - (**TheText).viewRect.top) / 
                   (**TheText).lineHeight;
                   
   /*==================================*/
   /* Avoid white space at the bottom. */
   /*==================================*/
   
   maxTop = (**TheText).nLines - windowHeight;
   
   /*=============================================================*/
   /* If last character in edit record is a carriage return, then */
   /* add one to the total number of lines in the document.       */
   /*=============================================================*/
   
   if (((**TheText).teLength > 0) &&
	   ((*((**TheText).hText))[(**TheText).teLength-1] == '\r')) 
     { maxTop += 1; }
     
   /*===================================================*/
   /* If the text is smaller than the window, then show */
   /* all of the text and disable the scroll bar, else  */
   /* enable the scroll bar.                            */
   /*===================================================*/
   
   if (maxTop <= 0)
     {
      maxTop = 0;
      HiliteControl(TheVScrollBar,inactiveValue);
     }
   else
     { HiliteControl(TheVScrollBar,activeValue); }
     
   /*=====================================*/
   /* Adjust the range of the scroll bar. */
   /*=====================================*/
   
   SetCtlMax(TheVScrollBar,maxTop);
   }
   
   if (TheHScrollBar != NULL)
     {
   /*========================*/
   /* Get the window width. */
   /*========================*/
   
   windowWidth = ((**TheText).viewRect.right - (**TheText).viewRect.left) / 
                   HORIZONTAL_SCROLL_INCREMENT;
                   
   /*==================================*/
   /* Avoid white space at the bottom. */
   /*==================================*/
   
   maxTop = (VIRTUAL_HORIZONTAL_WIDTH / HORIZONTAL_SCROLL_INCREMENT)
            - windowWidth;
   
     
   /*===================================================*/
   /* If the text is smaller than the window, then show */
   /* all of the text and disable the scroll bar, else  */
   /* enable the scroll bar.                            */
   /*===================================================*/
   
   if (maxTop <= 0)
     {
      maxTop = 0;
      HiliteControl(TheHScrollBar,inactiveValue);
     }
   else
     { HiliteControl(TheHScrollBar,activeValue); }
     
   /*=====================================*/
   /* Adjust the range of the scroll bar. */
   /*=====================================*/
   
   SetCtlMax(TheHScrollBar,maxTop);
   }
  }

/***************************************************************/
/* SCROLLTOSELECTION: Scroll current text selection into view. */
/***************************************************************/
ScrollToSelection(toMiddle)
  int toMiddle;
  {
   int topLine;
   int bottomLine;
   int windowHeight;
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*=======================================================*/
   /* Get the current top line, the window height in lines, */
   /* and the line beyond the bottom of the window.         */
   /*=======================================================*/
   
   topLine = GetCtlValue(TheVScrollBar);
   windowHeight = ( (**TheText).viewRect.bottom - (**TheText).viewRect.top) / 
                  (**TheText).lineHeight;
   bottomLine = topLine + windowHeight;
   
   if (((**TheText).teLength > 0) &&
	   ((*((**TheText).hText))[(**TheText).teLength-1] == '\r')) 
     { bottomLine -= 1; }
   
   /*=================================================*/
   /* If there is not enough text to fill the window, */
   /* then just the text at the top of the window.    */
   /*=================================================*/
            
   if (GetCtlMax(TheVScrollBar) == 0)
     { AdjustText(); }
     
   /*=================================================================*/
   /* Else if the whole selection is above the window, move the start */
   /* of the selection either to the top or middle of the window.     */
   /*=================================================================*/
            
   else if ( (**TheText).selEnd < (**TheText).lineStarts[topLine] )
     { 
      if (toMiddle)
        { ScrollCharacter((**TheText).selStart,ToMiddle); }
      else
        { ScrollCharacter((**TheText).selStart,ToTop); } 
     }
     
   /*================================================================*/
   /* Else if the whole selection is below the window, move the end  */
   /* of the selection either to the bottom or middle of the window. */
   /*================================================================*/
            
   else if ( (**TheText).selStart >= (**TheText).lineStarts[bottomLine] )
     { 
      if (toMiddle)
        { ScrollCharacter((**TheText).selEnd,ToMiddle); }
      else
        { ScrollCharacter((**TheText).selEnd,ToBottom); }
     }
        
   /*=========================*/
   /* Unlock the edit record. */
   /*=========================*/
   
   HUnlock( (Handle) TheText);
  }

/******************************************************/
/* DoSuspendResume: Handle suspend (or resume) event. */
/******************************************************/
DoSuspendResume()
  {
   WindowPtr whichWindow;
   int HandleActivateDeactivate = FALSE;
   int flag;
   
   /*=========================*/
   /* Check for resume event. */
   /*=========================*/
   
   if (BitAnd(TheEvent.message,1) == 1)
     {
      ReadDeskScrap();
      BitSet(&TheEvent.modifiers,15); 
     }
    
   /*===================================*/
   /* Otherwise handling suspend event. */
   /*===================================*/
   
   else
     {
      WriteDeskScrap();
      BitClr(&TheEvent.modifiers,15);
     }
   
   /*============================================*/
   /* Scrap conversion has already been handled. */
   /*============================================*/
   
   BitClr(&TheEvent.modifiers,changeFlag);
   
   /*======================*/
   /* Fake activate event. */
   /*======================*/
   
   TheEvent.message = (unsigned long) FrontWindow();
   if (TheEvent.message != NULL) DoActivate();
  }

 

 