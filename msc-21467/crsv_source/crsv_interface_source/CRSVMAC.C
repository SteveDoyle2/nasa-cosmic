
#include <QuickDraw.h>
#include <EventMgr.h>
#include <MenuMgr.h>
#include <ControlMgr.h> 
#include <ToolboxUtil.h>

#include "interface.h"
#include "crsv.h"

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   int                      MyNumFiles;
   char                     MyFileList[MAX_FILES][MAX_NAME];
   char                     MyDefRelName[MAX_NAME];
   char                     MyDribName[MAX_NAME];
 
   CursHandle                Watch;

   EventRecord               TheEvent;
   WindowPtr                 TheWindow;

   MenuHandle                AppleMenu;
   MenuHandle                FileMenu;
   MenuHandle                EditMenu;
   MenuHandle                OptionsMenu;
 
   int                       ScreenWidth;
   int                       ScreenHeight;
   
   extern int                MAX_SINGLE_LIST;

/*************************************************************/
/* MacCRSVInterface: Starts up the Macintosh CRSV Interface. */
/*************************************************************/
int MacCRSVInterface()
  {
   /*=====================================*/
   /* Initialize the Macintosh Interface. */
   /*=====================================*/
   
   InitMacInterface();
   
   /*==========================================================*/
   /* Assign the CRSV file list parameters to global variables */
   /* so that they can be manipulated by other routines.       */
   /*==========================================================*/
   
   MyNumFiles = 0;
   
   /*================================================*/
   /* Use the default settings for the Options menu. */
   /*================================================*/
   
   set_CHECK_RULES(OFF);
   set_CHECK_RELATIONS(OFF);
   set_CHECK_EX_FLAG(OFF);
   set_CHECK_COMMENTS(OFF);
   set_VERBOSE(OFF);
   set_CHECK_STYLE(ON);
   set_CREATE_DEFRELS(OFF);
   set_CHECK_DEFRELS(ON);
   set_CHECK_DEBUG(OFF);
   set_ANALYZE_TRACE(OFF);
   MAX_SINGLE_LIST = 5;
   UpdateOptionsMenu();

   /*=================================*/
   /* Loop forever processing events. */
   /*=================================*/
   
   while (TRUE) MainEvent();
  }
  
/***************************************************************/
/* InitMacInterface: Initializes the CRSV Macintosh interface. */
/***************************************************************/
InitMacInterface() 
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
   
   TheWindow = NULL;
   
   /*=========================*/
   /* Initialize the windows. */
   /*=========================*/
      
   InitDisplayWindow();
   InitFileListWindow();
  }
  
/*********************************/
/* SETUPCURSORS: Set up cursors. */
/*********************************/
SetUpCursors()
  {
   /*========================================*/
   /* Get cursors from system resource file. */
   /*========================================*/
   
   Watch = GetCursor(watchCursor);
   
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
   
   /*=============================================*/
   /* Get the Options menu from the resource file */
   /* and install it at the end of the menu bar.  */
   /*=============================================*/
   
   OptionsMenu = GetMenu(OptionsID);
   InsertMenu(OptionsMenu,0);
   
   /*======================================*/
   /* Show the new menu bar on the screen. */
   /*======================================*/
   
   DrawMenuBar();
  }  
  