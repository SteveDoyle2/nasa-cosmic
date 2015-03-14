/*   CLIPS Version 4.30   4/25/89 */

#include <TextEdit.h>
#include <ControlMgr.h>
#include <EventMgr.h>
#include <MenuMgr.h>
#include <ToolboxUtil.h>
#include <SegmentLdr.h>
#include <DeskMgr.h>
#include <OSUtil.h>

#include "interface.h"
#include "data.h"

/***************/
/* Definitions */
/***************/

#define WNETrapNum     0x60
#define UnImplTrapNum  0x9F

/*************/
/* Variables */
/*************/

int WNEIsImplemented = FALSE;
int Suspended = FALSE;
int memory_ok = TRUE;

extern EventRecord TheEvent;
extern WindowPtr TheWindow;
extern TEHandle TheText;

extern MenuHandle AppleMenu;
extern MenuHandle FileMenu;
extern MenuHandle EditMenu;
extern MenuHandle BufferMenu;
extern MenuHandle ExecutionMenu;

extern WindowPtr DisplayWindow;
extern WindowPtr AgendaWindow;
extern WindowPtr FactsWindow;

extern CursHandle IBeam;
extern CursHandle Watch;
 
extern int ScrapDirty;

extern int Quitting;
extern int Finished;
extern int ErrorFlag;

extern int ScreenWidth;
extern int ScreenHeight;

extern int TransferVrefnum;
extern int Transferring;
extern Str255 TransferToName;
  
/**********************************************************/
/* MAINEVENT: Executes one pass of the main program loop. */
/**********************************************************/
MainEvent()
  {
   /*=======================================*/
   /* If the desktop is empty, then disable */
   /* inapplicable menu commands.           */
   /*=======================================*/
   
   if (FrontWindow() == NULL)
     {
      DisableItem(EditMenu, UndoItem);
      DisableItem(EditMenu, CutItem);
      DisableItem(EditMenu, CopyItem);
      DisableItem(EditMenu, PasteItem);
      DisableItem(EditMenu, ClearItem);
      
      DisableItem(FileMenu, CloseItem);
      DisableItem(FileMenu, SaveItem);
      DisableItem(FileMenu, SaveAsItem);
      DisableItem(FileMenu, RevertItem);
      DisableItem(FileMenu, PrintItem);
      
      DisableItem(BufferMenu, FindItem);
      DisableItem(BufferMenu, FindAgainItem);
      DisableItem(BufferMenu, ReplaceItem);
      DisableItem(BufferMenu, ReplaceAndFindItem);
      DisableItem(BufferMenu, CompileSelectionItem);
      DisableItem(BufferMenu, CompileBufferItem);
     }
  
   /*=================================================*/
   /* Adjust the cursor for the region of the screen. */
   /*=================================================*/
   
   FixCursor();
   
   /*============================*/
   /* Do system idle processing. */
   /*============================*/
   
   SystemTask();
     
   /*===============================================*/
   /* Blink the cursor if an edit record is active. */
   /*===============================================*/
   
   if (TheText != NULL) 
     { TEIdle(TheText); }
   
   /*====================================================*/
   /* Update menus dependent upon CLIPS internal values. */
   /*====================================================*/
   
   UpdateWatchMenu();
   UpdateManagerMenu();
   UpdateDribbleMenu();
   
   /*==========================================================*/
   /* Update the agenda window if the agenda has been changed. */
   /*==========================================================*/
   
   if ((AgendaWindow != NULL) && (get_change_agenda() == TRUE))
     {
      complete_agenda_refresh();
      set_change_agenda(FALSE);
     }
      
   /*============================================================*/
   /* Update the facts window if the fact-list has been changed. */
   /*============================================================*/
   
   if ((FactsWindow != NULL) && (get_change_facts() == TRUE))
     { 
      complete_facts_refresh();
      set_change_facts(FALSE);
     }
     
   /*========================*/
   /* Handle the next Event. */
   /*========================*/
    
   DoEvent();
   
   /*========================================*/
   /* Check to see if memory is running low. */
   /*========================================*/
   
   if (FreeMem() < (30L * 1024L)) 
     {
      if (memory_ok == TRUE)
       {
         ParamText("\pCLIPS is running out of memory.\r",
                   "\pQuit as soon as possible.","\p","\p");
         StopAlert(StopCantDoID,0L);
         memory_ok = FALSE;
        }
     }
   else
    { memory_ok = TRUE; }
  }

/**************************************************/
/* FIXCURSOR: Adjust cursor for region of screen. */
/**************************************************/
FixCursor()
  {
   Point mousePoint;
   Rect textRect;
   
   /*==============================================*/
   /* Skip cursor adjustment during quit sequence. */
   /*==============================================*/
   
   if (Quitting) return;
   
   /*====================================================*/
   /* If the screen is empty, then use the arrow cursor. */
   /*====================================================*/
   
   if (FrontWindow() == NULL)
     { SetCursor(&arrow); }
     
   /*==============================================*/
   /* Else if one of our windows is active then... */
   /*==============================================*/
   
   else if (FrontWindow() == TheWindow)
     {
      /*============================================*/
      /* If the window doesn't have an edit record, */
      /* then use the arrow cursor and return.      */
      /*============================================*/
      
      if (TheText == NULL)
        {
         SetCursor(&arrow);
         return;
        }
        
      /*=========================================================*/
      /* Get the mouse position and the window's text rectangle. */
      /*=========================================================*/
      
      GetMouse(&mousePoint);
      textRect = (**TheText).viewRect;
      
      /*=====================================================*/
      /* If the mouse is within the text rectangle, then use */
      /* the I-beam cursor, otherwise use the arrow cursor.  */
      /*=====================================================*/
      
      if (PtInRect(mousePoint,&textRect))
        { SetCursor(*IBeam); }
      else
        { SetCursor(&arrow); }
     
     }
  }
  
/***************************************/
/* DOEVENT: Get and process one event. */
/***************************************/
DoEvent()
  {
   int handleEvent;
  
   /*===========================*/
   /* Clear the I/O error flag. */
   /*===========================*/
   
   ErrorFlag = FALSE;
                 
   /*=====================*/
   /* Get the next event. */
   /*=====================*/
   
   handleEvent = GetTheEvent(everyEvent,&TheEvent);

   /*===================================================*/
   /* If Event is to be handled by this program then... */
   /*===================================================*/
   
   if (handleEvent)
     {
      switch (TheEvent.what) 
	    {
		 /*==========================*/
		 /* Handle mouse-down event. */
		 /*==========================*/
		 
         case mouseDown:
		   if (! Quitting) DoMouseDown();
		   break;
		    
		 /*===================*/
		 /* Handle keystroke. */
		 /*===================*/
		 
		 case keyDown:
		 case autoKey: 
		   if (! Quitting) DoKeystroke();
		   break;
		  
		 /*======================*/
		 /* Handle update event. */
		 /*======================*/
		 
	     case updateEvt:
	       DoUpdate();
		   break;
		  
		 /*===================================*/
		 /* Handle activate/deactivate event. */
		 /*===================================*/
		 
		 case activateEvt:
		   DoActivate();
		   break;
		   
		 /*==============================*/
		 /* Handle suspend/resume event. */
		 /*==============================*/
		 
		 case app4Evt:
		   DoSuspendResume();
		   break; 
			  
		 default:
		   break;
		} 
     }
     
   /*===========================================================*/
   /* Else if quitting and no other events are occuring then... */
   /*===========================================================*/
   
   else if (Quitting && (TheEvent.what == nullEvent))
     {
      /*============================================*/
      /* If any window are left on the screen, then */
      /* close the frontmost window.                */
      /*============================================*/
      
      if (FrontWindow() != NULL) DoClose();
        
      /*================================================*/
      /* Else if transferring, launch that application. */
      /*================================================*/
        
      else if (Transferring)
        {
         WriteDeskScrap();
         SetVol(NULL,TransferVrefnum);
         Launch(0,TransferToName);
        }
        
      /*=============================*/
      /* Else exit from the program. */
      /*=============================*/
      
      else
        { 
         WriteDeskScrap();
         ExitToShell();
        }
     }
  }

/*****************************************/
/* DOMOUSEDOWN: Handle mouse-down event. */
/*****************************************/
DoMouseDown()
  {
   WindowPtr whichWindow;
   
   /*============================================*/
   /* Where on the screen was the mouse pressed? */
   /*============================================*/
   
   switch (FindWindow( TheEvent.where, &whichWindow )) 
	 {
	  /*====================================*/
	  /* Handle click in desk - do nothing. */
	  /*====================================*/
	  
      case inDesk: 
		break;
		
	  /*===========================*/
	  /* Handle click in menu bar. */
	  /*===========================*/
	  
	  case inMenuBar:
	    DoMenuClick();
	    break;
	    
	  /*================================*/
	  /* Handle click in system window. */
	  /*================================*/
	  
	  case inSysWindow:
	    SystemClick(&TheEvent,whichWindow);
	    break;
	    
	  /*=================================*/
	  /* Handle click in content region. */
	  /*=================================*/
	  
	  case inContent:
	    DoContent(whichWindow);
	    break;
	    
	  /*==============================*/
	  /* Handle click in drag region. */
	  /*==============================*/
	  
	  case inDrag:
	    DoDrag(whichWindow);
	    break;
	    
	  /*==============================*/
	  /* Handle click in size region. */
	  /*==============================*/
	  
	  case inGrow:
	    DoGrow(whichWindow);
	    break;
	    
	  /*===============================*/
	  /* Handle click in close region. */
	  /*===============================*/
	  
	  case inGoAway:
	    DoGoAway(whichWindow);
		break;
		
	  /*=================================*/
	  /* Handle click in zoom in region. */
	  /*=================================*/
	  
      case inZoomIn:
        DoZoom(whichWindow,inZoomIn);
        break;
        
	  /*==================================*/
	  /* Handle click in zoom out region. */
	  /*==================================*/
	  
      case inZoomOut:
        DoZoom(whichWindow,inZoomOut);
        break;
     }
  }

/*****************************************************/
/* DOMENUCLICK: Handle mouse-down event in menu bar. */
/*****************************************************/
DoMenuClick()
  {
   long int menuChoice;
      
   /*==================*/
   /* Track the mouse. */
   /*==================*/
   
   menuChoice = MenuSelect(TheEvent.where);
   
   /*============================*/
   /* Handle user's menu choice. */
   /*============================*/
   
   DoMenuChoice(menuChoice);
  }

/********************************************/
/* DOMENUCHOICE: Handle user's menu choice. */
/********************************************/
DoMenuChoice(menuChoice)
  long int menuChoice;
  {
   int theMenu;
   int theItem;
   
   /*=====================================*/
   /* If no choice was made, then return. */
   /*=====================================*/
   
   if (menuChoice == 0) return;
   
   /*==================================*/
   /* Get the menu id and item number. */
   /*==================================*/
   
   theMenu = HiWord(menuChoice);
   theItem = LoWord(menuChoice);
   
   /*==========================*/
   /* Which menu was selected? */
   /*==========================*/
   
   switch (theMenu)
     {
      /*================================*/
      /* Handle choice from Apple menu. */
      /*================================*/
      
      case AppleID:
        DoAppleChoice(theItem);
        break;
      
      /*===============================*/
      /* Handle choice from File menu. */
      /*===============================*/
      
      case FileID:
        DoFileChoice(theItem);
        break;
      
      /*===============================*/
      /* Handle choice from Edit menu. */
      /*===============================*/
      
      case EditID:
        DoEditChoice(theItem);
        break;
        
      /*=================================*/
      /* Handle choice from Buffer menu. */
      /*=================================*/
      
      case BufferID:
        DoBufferChoice(theItem);
        break;
        
      /*====================================*/
      /* Handle choice from Execution menu. */
      /*====================================*/
      
      case ExecutionID:
        DoExecutionChoice(theItem);
        break;
        
      /*================================*/
      /* Handle choice from Watch menu. */
      /*================================*/
      
      case WatchID:
        DoWatchChoice(theItem);
        break;
        
      /*=================================*/
      /* Handle choice from Browse menu. */
      /*=================================*/
      
      case BrowseID:
        DoBrowseChoice(theItem);
        break;
     }
     
   /*=========================*/
   /* Unhighlight menu title. */
   /*=========================*/
   
   HiliteMenu(0);
  }
  
/*****************************************************/
/* DOAPPLECHOICE: Handle choice from the Apple menu. */
/*****************************************************/
DoAppleChoice(theItem)
  int theItem;
  {
   Str255 accName;
   int accNumber;
	
   /*===============================*/
   /* Which menu item was selected? */
   /*===============================*/
   
   switch (theItem)  
     {
      /*================================*/
      /* Handle About CLIPS... command. */
      /*================================*/
      
      case AboutCLIPSItem:
        DoAbout();
        break;     
        
      /*==========================*/
      /* Handle a desk accessory. */
      /*==========================*/
      
      default:
        /*=====================================================*/
        /* If the desktop is empty, then enable the close item */
        /* and standard editing commands for a desk accessory. */
        /*=====================================================*/

        if (FrontWindow() == NULL)
          {
           EnableItem(FileMenu,CloseItem);
           
           EnableItem(EditMenu, UndoItem);
           EnableItem(EditMenu, CutItem);
           EnableItem(EditMenu, CopyItem);
           EnableItem(EditMenu, PasteItem);
           EnableItem(EditMenu, ClearItem);
          }
         
        /*================================================*/
        /* Disable interface commands that are not usable */
        /* when a desk accessory is active.               */
        /*================================================*/
        
        DisableItem(FileMenu, PrintItem);
        DisableItem(BufferMenu, FindItem);
        DisableItem(BufferMenu, FindAgainItem);
        DisableItem(BufferMenu, ReplaceItem);
        DisableItem(BufferMenu, ReplaceAndFindItem);
        DisableItem(BufferMenu, ReplaceAllItem);
        DisableItem(BufferMenu, CompileSelectionItem);
        DisableItem(BufferMenu, CompileBufferItem);
         
        /*==============================*/
        /* Get the desk accessory name. */
        /*==============================*/
        
		GetItem(AppleMenu, theItem, &accName);
		
        /*==========================*/
        /* Open the desk accessory. */
        /*==========================*/
        
	    OpenDeskAcc( &accName );
		break;
     }
  }
  
/*******************************/
/* DOKEYSTROKE:                */
/*******************************/
DoKeystroke()
  {
   int chCode;
   char ch;
   long int menuChoice;
   
   /*===========================================================*/
   /* Extract the character code and convert it to a character. */
   /*===========================================================*/
   
   chCode = BitAnd(TheEvent.message,charCodeMask);
   ch = (char) chCode;
   
   /*=====================================*/
   /* If the command key is down, then... */
   /*=====================================*/
   
   if (BitAnd(TheEvent.modifiers,cmdKey) != 0)
     {
      /*================================================*/
      /* If the key is not being repeated, then get the */
      /* menu equivalent and handle as a menu choice.   */
      /*================================================*/
      
      if (TheEvent.what != autoKey)
        {
         menuChoice = MenuKey(ch);
         DoMenuChoice(menuChoice);
        }
     }
   
   /*==============================================*/
   /* Else handle keystroke as a normal character. */
   /*==============================================*/
   
   else
     { DoTyping(ch); }
  }
  
/*******************************************************/
/* DOTYPING: Handle character typed from the keyboard. */
/*******************************************************/
DoTyping(ch)
  char ch;
  {
   char inp[2];
   long int size;
   
   /*========================================================*/
   /* If the window is the display, facts, or agenda window, */ 
   /* then the character will be sent to the display window. */
   /*========================================================*/
   
   if ((TheWindow == DisplayWindow) ||
       (TheWindow == FactsWindow) ||
       (TheWindow == AgendaWindow))
     {
      /*=========================================*/
      /* Make the cursor invisible while typing. */
      /*=========================================*/
      
      ObscureCursor();
      
      /*============================================*/
      /* Print the character to the display window. */
      /*============================================*/
      
      inp[0] = ch;
	  inp[1] = '\0';
	  cl_print("wdisplay",inp);
	  
	  /*=========================================*/
	  /* If the display window exists, then show */
	  /* the line being entered.                 */
	  /*=========================================*/
	  
	  if (select_display_window()) show_display_select();
	  
	  /*================================================*/
	  /* Add the character to the CLIPS command string. */
	  /*================================================*/
	  
      expand_command_string(ch);
     }
     
   /*============================================================*/
   /* Otherwise the character will be sent to an editing buffer. */
   /*============================================================*/
   
   else
     {
      /*====================================================*/
      /* Check to make sure that adding the character won't */
      /* force the buffer to be larger than 32K.            */
      /*====================================================*/
      
      size = ((long int) (**TheText).teLength) + 1L;
   
      if ((size >= MAX_BUFFER_SIZE) && (ch != '\b'))
        {
         InitCursor();
         ParamText("\pCan't insert character ",
                   "\p(only files 32K or smaller can be edited)","\p","\p");
         StopAlert(StopCantDoID,0L);
         return;
        }
        
      /*===========================================*/
      /* Make sure the insertion point is visible. */
      /*===========================================*/
      
      ScrollToSelection(FALSE);
      
      /*=============================*/
      /* Process the undo character. */
      /*=============================*/
      
      ProcessUndoCharacter(ch,TheText,TheWindow);
      
      /*========================*/
      /* Process the character. */
      /*========================*/
      
      TEKey(ch,TheText);
   
      /*==================================================================*/
      /* Adjust the scroll bar to the length of the text, adjust the text */
      /* to match the scroll bar, and keep the insertion point visible.   */
      /*==================================================================*/
   
      AdjustScrollBar();
      AdjustText();
      ScrollToSelection(FALSE);
   
      /*==============================================================*/
      /* Disable menu commands that operate on a non-empty selection. */
      /*==============================================================*/
      
      DisableItem(EditMenu,CutItem);
      DisableItem(EditMenu,CopyItem);
      DisableItem(EditMenu,ClearItem);

      /*===========================*/
      /* Mark the window as dirty. */
      /*===========================*/
      
      WindowDirty(TRUE); 
     }
  }
  
/*****************************************************/
/* WINDOWDIRTY: Mark window as being dirty or clean. */
/*****************************************************/
WindowDirty(isDirty)
  int isDirty;
  {
   WDHandle theData;
   Handle dataHandle;
   
   /*===================================*/
   /* Get the window data, lock it, and */
   /* convert it to a typed handle.     */
   /*===================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=========================================================*/
   /* Set the dirty flag in the data record to the new value. */
   /*=========================================================*/
   
   (**theData).dirty = isDirty;
   
   /*==========================================*/
   /* If the window is becoming dirty, then... */
   /*==========================================*/
   
   if (isDirty)
     {
      /*==========================*/
      /* Enable the Save command. */
      /*==========================*/
      
      EnableItem(FileMenu,SaveItem);
      
      /*==========================================*/
      /* If the window is associated with a file, */
      /* then enable the rever command.           */
      /*==========================================*/
      
      if ((**theData).fileNumber != 0)
        { EnableItem(FileMenu,RevertItem); }
     }
     
   /*============================================*/
   /* Else the window is becoming clean. Disable */
   /* the Save and Revert commands.              */
   /*============================================*/
   
   else
     {
      DisableItem(FileMenu,SaveItem);
      DisableItem(FileMenu,RevertItem);
     }
    
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
  }

/*******************************************/
/* DoAbout: Handle About CLIPS... command. */
/*******************************************/
DoAbout() 
  {
   Alert(CLIPSAlert, 0L );
  }

/****************************************/
/* MultiFinderSetup: Determines if the  */
/*   WaitNextEvent Trap is implemented. */
/****************************************/
MultiFinderSetup()
  {
   WNEIsImplemented = (NGetTrapAddress(WNETrapNum,ToolTrap) !=
                       NGetTrapAddress(UnImplTrapNum,ToolTrap));
  }

/**************************************/
/* GetTheEvent: Grabs the next event. */
/**************************************/
GetTheEvent(eventMask,thisEvent)
  int eventMask;
  EventRecord *thisEvent;
  {  
   if (WNEIsImplemented)
     { return(WaitNextEvent(eventMask,thisEvent,0L,NULL)); }
   else
     {
      SystemTask();
      return(GetNextEvent(eventMask,thisEvent));
     }
  }