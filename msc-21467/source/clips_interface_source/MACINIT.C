/*   CLIPS Version 4.30   4/25/89 */

#include "interface.h"

#include <QuickDraw.h>
#include <EventMgr.h>
#include <MenuMgr.h>
#include <TextEdit.h>
#include <ControlMgr.h> 
#include <ToolboxUtil.h>
#include <SegmentLdr.h>
#include <DeskMgr.h>

EventRecord TheEvent;
WindowPtr TheWindow;
extern WindowPtr FactsWindow;
extern WindowPtr DisplayWindow;
extern WindowPtr AgendaWindow;
ControlHandle TheVScrollBar;
ControlHandle TheHScrollBar;
TEHandle TheText;

MenuHandle AppleMenu;
MenuHandle FileMenu;
MenuHandle EditMenu;
MenuHandle BufferMenu;
MenuHandle ExecutionMenu;
MenuHandle WatchMenu;
MenuHandle BrowseMenu;

CursHandle IBeam;
CursHandle Watch;
CursHandle ReadCursor;
 
int ScrapDirty;

int Quitting = FALSE;
int Finished = FALSE;
int ErrorFlag;

int ScreenWidth;
int ScreenHeight;

int mac_exec_function();

/******************************************************************/
/* INIT_MAC_INTERFACE: Initializes the CLIPS Macintosh interface. */
/******************************************************************/
init_mac_interface() 
  {
   int myRsrc, i;
   int MainEvent();
    
   /*=====================================================*/
   /* Macintosh Incantation for initializing application. */
   /*=====================================================*/
	
   InitGraf(&thePort);
   InitFonts();
   InitWindows();
   InitMenus();
   TEInit();
   InitDialogs(0L);
	
   MaxApplZone();
   FlushEvents(everyEvent,0);
	  
   /*==================================*/
   /* Get the screen width and height. */
   /*==================================*/
   
   ScreenHeight = screenBits.bounds.bottom - screenBits.bounds.top;
   ScreenWidth = screenBits.bounds.right - screenBits.bounds.left;
   
   /*===============================*/
   /* Set up the menus and cursors. */
   /*===============================*/
   
   SetUpMenus();
   SetUpCursors();
   
   /*=====================================*/
   /* Initialize global window variables. */
   /*=====================================*/
   
   TheText = NULL;
   TheWindow = NULL;
   TheVScrollBar = NULL;
   TheHScrollBar = NULL;
   
   /*======================================*/
   /* Place any selected files in buffers. */
   /*======================================*/
   
   DoStartup();
   
   /*=======================*/
   /* Initialize the Scrap. */
   /*=======================*/
   
   InitializeScrap();
   
   /*=========================*/
   /* Set up for MultiFinder. */
   /*=========================*/
   
   MultiFinderSetup();
   
   /*===================================*/
   /* Set up routers for the interface. */
   /*===================================*/
   
   SetUpRouters();
   
   /*==================================================================*/
   /* Set up hook between CLIPS command loop and interface event loop. */
   /*==================================================================*/
   
   set_event_function(MainEvent);
   
   /*==================================================================*/
   /* Add execution function to update interface between rule firings. */
   /*==================================================================*/
   
   add_exec_function("macint",mac_exec_function);
  }
  
/*********************************/
/* SETUPCURSORS: Set up cursors. */
/*********************************/
SetUpCursors()
  {
   /*========================================*/
   /* Get cursors from system resource file. */
   /*========================================*/
   
   IBeam = GetCursor(iBeamCursor);
   Watch = GetCursor(watchCursor);
   ReadCursor = GetCursor(ReadCursorID);
   
   /*============================*/
   /* Set standard cursor arrow. */
   /*============================*/
   
   InitCursor();
  }
  
/*****************************/
/* SETUPMENUS: Set up menus. */
/*****************************/
SetUpMenus()
  {
   /*================================================*/
   /* Get the Apple menu from the resource file, add */
   /* the names of available desk accessories, and   */
   /* install it at the end of the menu bar.         */
   /*================================================*/
   
   AppleMenu = GetMenu(AppleID);
   AddResMenu(AppleMenu,'DRVR');
   InsertMenu(AppleMenu,0);
   
   /*==============================================*/
   /* Get the File menu from the resource file and */
   /* install it at the end of the menu bar.       */
   /*==============================================*/
   
   FileMenu = GetMenu(FileID);
   InsertMenu(FileMenu,0);
   
   /*==============================================*/
   /* Get the Edit menu from the resource file and */
   /* install it at the end of the menu bar.       */
   /*==============================================*/
   
   EditMenu = GetMenu(EditID);
   InsertMenu(EditMenu,0);
   
   /*============================================*/
   /* Get the Buffer menu from the resource file */
   /* and install it at the end of the menu bar. */
   /*============================================*/
   
   BufferMenu = GetMenu(BufferID);
   InsertMenu(BufferMenu,0);
   
   /*===============================================*/
   /* Get the Execution menu from the resource file */
   /* and install it at the end of the menu bar.    */
   /*===============================================*/
   
   ExecutionMenu = GetMenu(ExecutionID);
   InsertMenu(ExecutionMenu,0);
   
   /*============================================*/
   /* Get the Watch menu from the resource file  */
   /* and install it at the end of the menu bar. */
   /*============================================*/
   
   WatchMenu = GetMenu(WatchID);
   InsertMenu(WatchMenu,0);
   
   /*============================================*/
   /* Get the Browse menu from the resource file */
   /* and install it at the end of the menu bar. */
   /*============================================*/
   
   BrowseMenu = GetMenu(BrowseID);
   InsertMenu(BrowseMenu,0);
   
   /*======================================*/
   /* Show the new menu bar on the screen. */
   /*======================================*/
   
   DrawMenuBar();
  }  
  
/**************************************************/
/* DoStartup: Process finder startup information. */
/**************************************************/
DoStartup()
  {
   int theMessage;
   int nDocs;
   int thisDoc;
   AppFile docInfo;
   int ignore;
  
   /*======================================================*/
   /* Get the number of documents and the startup message. */
   /*======================================================*/
   
   CountAppFiles(&theMessage,&nDocs);
   
   /*=================================================*/
   /* If the user chose Print in Finder, then post an */
   /* alert indicating that printing from the Finder  */
   /* is not supported and return to the Finder.      */
   /*=================================================*/
   
   if (theMessage == appPrint)
     {
      StopAlert(CantPrintID,NULL);
      ExitToShell();
     }
     
   /*====================================================*/
   /* Else if the user selected one or documents then... */
   /*====================================================*/
   
   else if (nDocs > 0)
     {
      /*=====================================*/
      /* Loop through each of the documents. */
      /*=====================================*/
      
      for (thisDoc = 1 ; thisDoc <= nDocs; thisDoc++)
        {
         /*===============================================*/
         /* Get the startup information for the document. */
         /*===============================================*/
         
         GetAppFiles(thisDoc,&docInfo);
         
         /*===================================================*/
         /* If the document is a text file, then read it into */
         /* a window and tell the finder that the document    */
         /* has been processed.                               */
         /*===================================================*/
         
         if (docInfo.fType == 'TEXT')
           {
            OpenFile(docInfo.fName,docInfo.vRefNum);
            ClrAppFiles(thisDoc);
           }
           
         /*=================================================*/
         /* Else substitute the document name into the text */
         /* of an alert indicating that the document is of  */
         /* the wrong type and post the alert.              */
         /*=================================================*/
         
         else
           {
            ParamText(docInfo.fName,"\p","\p","\p");
            StopAlert(WrongTypeID,NULL);
           }
        }
     }
  }
            
/************************************************************************/
/* SETUPROUTERS: Sets up routers used by the CLIPS Macintosh interface. */
/************************************************************************/
SetUpRouters()
  {
   extern int macfilefind(), macfileprint(), macfilegetc(), macfileungetc(); 
   int macexit();
  
   add_router("macexit",60,NULL,NULL,NULL,NULL,macexit);
   add_router("macstdout",10,macfilefind,macfileprint,macfilegetc,macfileungetc,NULL);
  }    

/***************************************************************/
/* macexit: routine to check an exit from the dialog window to */
/*   make sure that the user has an opportunity to save files. */
/***************************************************************/
macexit(num)
  {
   DoQuit();
   AbortExit();
  }
    
/*****************************************************************/
/* MAC_EXEC_FUNCTION: Execution function which is called between */
/*   rule firings by CLIPS to update the interface.              */
/*****************************************************************/
mac_exec_function(logical_name)
  char *logical_name;
  {
   WindowPtr which_window;
   EventRecord		myEvent;
   extern int UpdateAgendaWindow();

   /*===========================================*/
   /* Update the agenda window if it is active. */
   /*===========================================*/
   
   if ((AgendaWindow != NULL) && (get_change_agenda() == TRUE))
     { 
      complete_agenda_refresh();
      set_change_agenda(FALSE);
     }
     
   /*===========================================*/
   /* Update the facts window if it is active. */
   /*===========================================*/
   
   if ((FactsWindow != NULL) && (get_change_facts() == TRUE))
     { 
      complete_facts_refresh();
      set_change_facts(FALSE);
     }
    
   /*===============================================================*/
   /* Search through events for ctrl-c or command-period interrupt. */
   /* Also handle update events for some of the interface windows.  */
   /* Only keypress events and update events are processed.         */
   /*===============================================================*/
   
   while (GetTheEvent(keyDownMask | updateMask,&myEvent) != 0)
     {   
      switch (myEvent.what) 
	    { 
	     case nullEvent:
	       return;
	       break;
	         
	     /*======================*/
	     /* Handle update event. */
	     /*======================*/
	     
	     case updateEvt:
		   which_window = (WindowPtr) myEvent.message;
		   if (which_window == DisplayWindow) 
			 { UpdateDisplayWindow(); }
		   else if (which_window == AgendaWindow)
			 { UpdateAgendaWindow(); }
		   else if (which_window == FactsWindow)
			 { UpdateFactsWindow(); }
		   break;
			     
	     /*====================================*/
	     /* Look for ctrl-c or command-period. */
	     /*====================================*/
	     
		 case keyDown:
		 case autoKey: 
		   {
			char theChar;
			      
			theChar = myEvent.message & charCodeMask;
			      
			if (((myEvent.modifiers & cmdKey) != 0) && (theChar == '.'))
		      {
			   set_execution_error(1);
			   return;
			  }
		    else if (((myEvent.modifiers & controlKey) != 0) && (theChar == '\3'))
		      {
			   set_execution_error(1);
			   return;
			  }
			break; 
		   }
		}
     }
  }
  