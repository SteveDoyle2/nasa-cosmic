
#include <EventMgr.h>
#include <WindowMgr.h>
#include <MenuMgr.h>
#include <OSUtil.h>
#include <DialogMgr.h>
#include <MemoryMgr.h>

#include "interface.h"
#include "crsv.h"
   
/***************/
/* Definitions */
/***************/

#define WNETrapNum     0x60
#define UnImplTrapNum  0x9F

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   int                       DeferredCommandTask;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/
   
   extern EventRecord        TheEvent;
   extern WindowPtr          TheWindow;
   extern WindowPtr          FileListWindow;
   extern WindowPtr          DisplayWindow;

   extern MenuHandle         AppleMenu;
   extern MenuHandle         FileMenu;
   extern MenuHandle         EditMenu;

/**********************************************************/
/* MAINEVENT: Executes one pass of the main program loop. */
/**********************************************************/
MainEvent()
  {
   /*======================*/
   /* Update the menu bar. */
   /*======================*/
   
   UpdateMenuItems();
  
   /*========================*/
   /* Handle the next Event. */
   /*========================*/
    
   DoEvent();
  }
  
/************************************************************/
/* UpdateMenuItems: Updates the menu bar for certain items. */
/************************************************************/ 
UpdateMenuItems()
  {
   /*============================================================*/
   /* Disable inapplicable menu commands for our active windows. */
   /* Enable close command for active system windows.            */
   /*============================================================*/
   
   if ((FrontWindow() == NULL) ||
       (FrontWindow() == FileListWindow) ||
       (FrontWindow() == DisplayWindow))
     {
      DisableItem(EditMenu, UndoItem);
      DisableItem(EditMenu, CutItem);
      DisableItem(EditMenu, CopyItem);
      DisableItem(EditMenu, PasteItem);
      DisableItem(EditMenu, ClearItem);
      
      DisableItem(FileMenu, CloseItem);
     }
   else
     { EnableItem(FileMenu, CloseItem); }
    
   /*============================================================*/
   /* Print menu item should only be enabled for display window. */
   /*============================================================*/
    
   if (FrontWindow() ==  DisplayWindow)
     { EnableItem(FileMenu, PrintItem); }
   else
     { DisableItem(FileMenu, PrintItem); }
     
   /*===============================================*/
   /* Check files command should be enabled only if */
   /* there are files in the List of Files window.  */
   /*===============================================*/
   
   if (FileListCount() == 0)
     { DisableItem(FileMenu,CheckFilesItem); }
   else
     { EnableItem(FileMenu,CheckFilesItem); }
     
   /*=======================================================*/
   /* Remove Files and Check Selected Files commands should */
   /* be enabled only if the List of Files window is active */
   /* and there are selected files.                         */
   /*=======================================================*/
   
   if ((FrontWindow() == FileListWindow) && 
       (FileListSelections() > 0))
     { 
      EnableItem(FileMenu,RemoveFileItem);
      EnableItem(FileMenu,CheckSelectedFilesItem);
     }
   else
     { 
      DisableItem(FileMenu,RemoveFileItem);
      DisableItem(FileMenu,CheckSelectedFilesItem);
     } 
     
   /*====================================================*/
   /* Move to Front and Move to Back commands should be  */
   /* enabled only if the List of Files window, there is */
   /* at least one selected file, and all the files are  */
   /* not selected.                                      */
   /*====================================================*/
   
   if ((FrontWindow() == FileListWindow) && 
       (FileListSelections() > 0) &&
       (FileListSelections() < FileListCount()))
     { 
      EnableItem(EditMenu,MoveToFrontItem); 
      EnableItem(EditMenu,MoveToBottomItem); 
     }
   else
     {
      DisableItem(EditMenu,MoveToFrontItem); 
      DisableItem(EditMenu,MoveToBottomItem); 
     } 
  }
  
/***************************************/
/* DOEVENT: Get and process one event. */
/***************************************/
DoEvent()
  {
   int handleEvent;
   int WNEIsImplemented;
   
   /*==============================*/
   /* Is WaitNextEventImplemented? */
   /*==============================*/
   
   WNEIsImplemented = (NGetTrapAddress(WNETrapNum,ToolTrap) !=
                       NGetTrapAddress(UnImplTrapNum,ToolTrap));
                       
   /*=====================*/
   /* Get the next event. */
   /*=====================*/
   
   if (WNEIsImplemented)
     { handleEvent = WaitNextEvent(everyEvent,&TheEvent,20L,NULL); }
   else
     {
      SystemTask();
      handleEvent = GetNextEvent(everyEvent,&TheEvent);
     }
   
   /*====================================*/
   /* If not an event for us to handle,  */ 
   /* then handle any deferred commands. */
   /*====================================*/
   
   if (handleEvent == FALSE) 
     { 
      if (TheEvent.what == nullEvent)
        { HandleDeferredCommandTask(); }
      return;
     }
     
   /*===================*/
   /* Handle the event. */
   /*===================*/
   
   switch (TheEvent.what) 
	 {
      /*==========================*/
	  /* Handle mouse-down event. */
	  /*==========================*/
		 
      case mouseDown:
        if (! DeferredCommandTask) DoMouseDown();
	    break;
		    
	  /*===================*/
      /* Handle keystroke. */
      /*===================*/
		 
      case keyDown:
	  case autoKey: 
		if (! DeferredCommandTask) DoKeystroke();
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
			  
      /*=================================*/
      /* Do nothing for any other event. */
      /*=================================*/
      
	  default:
		break;
     } 
  }

/**************************************************************/
/* HandleDeferredCommandTask: Process any commands that have  */
/*   deferred execution to allow activation and update events */
/*   to be processed first.                                   */
/**************************************************************/
HandleDeferredCommandTask()
  {
   /*=============================================*/
   /* Which deferred command should be performed? */
   /*=============================================*/
   
   switch (DeferredCommandTask)
     {
      /*=======================*/
      /* Execute Quit command. */
      /*=======================*/
      
      case QuittingTask:
        if (CleanUpWindows() == FALSE)
          {
           DribbleExitCleanup();
           ExitToShell();
          }
        break;
     
      /*==============================*/
      /* Execute Transfer... command. */
      /*==============================*/
      
      case TransferringTask:
        if (CleanUpWindows() == FALSE)
          {
           DribbleExitCleanup();
           ExecuteTransfer();
          }
        break;
        
      /*===============================*/
      /* Execute Add Files... command. */
      /*===============================*/
      
      case AddFilesTask:
        if (AddSingleFile() == FALSE)
          { DeferredCommandTask = NoTask; }
        break;
        
      /*===========================*/
      /* Execute Print... command. */
      /*===========================*/
      
      case PrintingTask:
        ExecutePrint();
        DeferredCommandTask = NoTask;
        break;

      /*======================================================*/
      /* Execute Check Files or Check Selected Files command. */
      /*======================================================*/
      
      case CheckFilesTask:
      case CheckSelectedFilesTask:
        ExecuteCheckFiles();
        DeferredCommandTask = NoTask;
        break;
        
      /*=============================================*/
      /* Do nothing if there is no deferred command. */
      /*=============================================*/
      
      case NoTask:
        break;
      
      /*===========================================================*/
      /* Indicate error for other unimplemented deferred commands. */
      /*===========================================================*/
      
      default:
        break;
     }  
   
   /*=============================================*/
   /* Unhighlight menu bar if finished with task. */
   /*=============================================*/
   
   if (DeferredCommandTask == NoTask)
     { HiliteMenu(0); }
  }
  
/*****************************************************************/
/* CleanUpWindows: Closes up the frontmost window on the screen. */
/*   Returns false if no windows on screen, true otherwise.      */
/*****************************************************************/
CleanUpWindows()
  {
   /*=======================================*/
   /* Return false if no windows on screen. */
   /*=======================================*/
    
   if (FrontWindow() == NULL) return(FALSE);
   
   /*=====================================*/
   /* Perform appropriate close function. */
   /*=====================================*/
   
   if (FrontWindow() == TheWindow)
    {
     if (TheWindow == FileListWindow)
       { DisposeFileList(); }
        
     CloseOutputWindow();
    }
   else
    { DoClose(); }
    
   return(TRUE);
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
        
      /*==================================*/
      /* Handle choice from Options menu. */
      /*==================================*/
      
      case OptionsID:
        DoOptionsChoice(theItem);
        break;
     }
     
   /*=========================*/
   /* Unhighlight menu title. */
   /*=========================*/
   
   if (DeferredCommandTask == NoTask)
     { HiliteMenu(0); }
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
      
      case AboutCSRVItem:
        DoAbout();
        break;     
        
      /*==========================*/
      /* Handle a desk accessory. */
      /*==========================*/
      
      default:
        /*============================================*/
        /* Enable the close item and standard editing */
        /* commands for a desk accessory.             */
        /*============================================*/
        
        EnableItem(FileMenu,CloseItem);
           
        EnableItem(EditMenu, UndoItem);
        EnableItem(EditMenu, CutItem);
        EnableItem(EditMenu, CopyItem);
        EnableItem(EditMenu, PasteItem);
        EnableItem(EditMenu, ClearItem);
         
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
  
/*****************************************/
/* DOKEYSTROKE: Handles keyboard events. */
/*****************************************/
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
   
   /*============================================================*/
   /* If the command key is not down, then ignore the key press. */
   /*============================================================*/
   
   if (BitAnd(TheEvent.modifiers,cmdKey) == 0) return;
   
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

/*******************************************/
/* DoAbout: Handle About CLIPS... command. */
/*******************************************/
/*
DoAbout() 
  {
   Alert(CRSVAlert, 0L );
  }
*/  
#define ABOUT_AREA 14
#define CHRIS_HEAD 4
#define GARY_HEAD 1
#define BEBE_HEAD 2
#define BECKY_HEAD 3
#define CRSVDialogAbout 500

DoAbout()
  {
   DialogPtr dptr;
   int code = 0;
   int item_num = 0;
   GrafPtr savePort;
   extern void set_CHECK_DEBUG();
   Str255 freeMem, memUsed;
   long int freeMemv;
   THz myZone;
   extern long int mem_used();
     
   GetPort(&savePort);
   
   dptr = GetNewDialog(CRSVDialogAbout,NULL,-1L);
   SetPort(dptr);
   
   /*======================================*/
   /* Determine the amount of free memory. */
   /*======================================*/
   
   myZone = GetZone();
   freeMemv = (*myZone).zcbFree;
   NumToString(freeMemv,freeMem);
   NumToString(mem_used(),memUsed);
   
   ParamText(memUsed,freeMem,"\p","\p");
   
   
   ShowWindow(dptr);
   
   for (ModalDialog(NULL,&item_num) ; ; ModalDialog(NULL,&item_num))
     { 
      switch(item_num)
        {
         case CHRIS_HEAD:
           if (code == 0) code = 1;
           else code = 0;
           SysBeep(10);
           break;
         
         case GARY_HEAD:
           if (code == 1) code = 2;
           else code = 0;
           SysBeep(10);
           break;
           
         case BEBE_HEAD:
           if (code == 2) code = 3;
           else code = 0;
           SysBeep(10);
           break;
           
         case BECKY_HEAD:
           if (code == 3) 
             {
              if (CHECK_DEBUG IS_ON)
                { ParamText("\pDebugging off","\p","\p","\p"); }
              else
                { ParamText("\pDebugging on","\p","\p","\p"); }
              NoteAlert(StopCantDoID,NULL);
              set_CHECK_DEBUG(1 - CHECK_DEBUG);
             }
           else
             { SysBeep(10); }
            
           code = 0;
           break;
           
         default:
           DisposDialog(dptr);
           SetPort(savePort);
           return;
        }
     }
  }
  
  

  
