
#include <EventMgr.h>
#include <WindowMgr.h>
#include <ControlMgr.h>
#include <MenuMgr.h>

#include "interface.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   int	                     scrollCode;
   int                       scrollAmt;

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern EventRecord        TheEvent;
   extern WindowPtr          TheWindow;
   
   extern MenuHandle         FileMenu;
   extern MenuHandle         EditMenu;

   extern int                ScreenWidth;
   extern int                ScreenHeight;

   extern WindowPtr          DisplayWindow;
   extern WindowPtr          FileListWindow;
   extern ControlHandle      DisplayVScroll;
   extern ControlHandle      DisplayHScroll;

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
   
   if (newSize == 0) return;
     
   /*====================================*/
   /* Clear the entire port rectangle to */ 
   /* the window's background pattern.   */
   /*====================================*/	
    
   if (whichWindow != FileListWindow)
     EraseRect(&(*whichWindow).portRect);
       
   /*=======================================*/
   /* Call SizeWindow to resize the window. */
   /*=======================================*/
      
   newWidth = LoWord(newSize);
   newHeight = HiWord(newSize);
   
   if (whichWindow == FileListWindow)
     { newHeight = (newHeight / SBarWidth) * SBarWidth; }
     
   SizeWindow(whichWindow,newWidth,newHeight,TRUE);
         
   /*======================================================*/
   /* Invalidate viewing rectangle and fix the scroll bar. */
   /*======================================================*/
   
   if (whichWindow == DisplayWindow)
     {
      InvalRect(&(*whichWindow).portRect);
      ResizeDisplayScrollBars(); 
     }
      
   /*=======================================*/
   /* Perform special grow update routines. */
   /*=======================================*/
   
   if (whichWindow == DisplayWindow)
     { GrowDisplayWindow(whichWindow); }
   else if (whichWindow == FileListWindow)
     { GrowFileListWindow(newWidth,newHeight); }
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
   if (whichWindow == DisplayWindow)
     { ResizeDisplayScrollBars(); }
            
   /*=======================================*/
   /* Perform special grow update routines. */
   /*=======================================*/
   
   if (whichWindow == DisplayWindow)
     { GrowDisplayWindow(whichWindow); }
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
	 { DoDisplayContent(); }
   else if (whichWindow == FileListWindow)
     { DoFileListContent(); }
  }

/**********************************/
/* DOUPDATE: Handle update event. */
/**********************************/
DoUpdate()
  {
   GrafPtr savePort;
   WindowPtr whichWindow;
   Handle dataHandle;
   
   /*=============================================*/
   /* Determine which window needs to be updated. */
   /*=============================================*/
   
   whichWindow = (WindowPtr) TheEvent.message;
   
   /*===========================================*/
   /* Call special routines for handling update */
   /* for display and file list windows.        */
   /*===========================================*/
   
   if (whichWindow == DisplayWindow) 
	 { UpdateDisplayWindow(); }
   else if (whichWindow == FileListWindow)
     { UpdateFileListWindow(); }
  }
  
/******************************************************/
/* DOACTIVATE: Handle activate (or deactivate) event. */
/******************************************************/
DoActivate()
  {
   WindowPtr whichWindow;
   
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
        
      /*==========================*/
      /* Activate the scroll bar. */
      /*==========================*/
      
      if (whichWindow == DisplayWindow)
        { 
         HiliteControl(DisplayVScroll,activeValue); 
         HiliteControl(DisplayHScroll,activeValue); 
        }
      else if (whichWindow == FileListWindow)
        { DoFileListActivate(true); }
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
      
      /*============================*/
      /* Deactivate the scroll bar. */
      /*============================*/
      
      if (whichWindow == DisplayWindow)
        { 
         HiliteControl(DisplayVScroll,inactiveValue); 
         HiliteControl(DisplayHScroll,inactiveValue); 
        }
      else if (whichWindow == FileListWindow)
        { DoFileListActivate(false); }
      
      /*=========================================================*/
      /* If exiting to a system window, then enable the standard */
      /* editing commands for desk accessories.                  */
      /*=========================================================*/
      
      if (BitAnd(TheEvent.modifiers,changeFlag) != 0)
        { 
         EnableItem(EditMenu,UndoItem);
         EnableItem(EditMenu,CutItem);
         EnableItem(EditMenu,CopyItem);
         EnableItem(EditMenu,PasteItem);
         EnableItem(EditMenu,ClearItem);
        }  
     }
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


  
