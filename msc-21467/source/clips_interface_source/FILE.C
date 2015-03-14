/*   CLIPS Version 4.30   4/25/89 */

#include <TextEdit.h>
#include <ControlMgr.h>
#include <FontMgr.h>
#include <MenuMgr.h>
#include <stdfilepkg.h>
#include <EventMgr.h>
#include <FileMgr.h>

#include "data.h"
#include "interface.h"

extern WindowPtr TheWindow;
extern ControlHandle TheVScrollBar;
extern ControlHandle TheHScrollBar;
extern TEHandle TheText;
extern EventRecord TheEvent;
extern MenuHandle FileMenu;
extern CursHandle Watch;

extern int Quitting;
extern int Transferring;
extern int ErrorFlag;

extern WindowPtr DisplayWindow;
extern WindowPtr FactsWindow;
extern WindowPtr AgendaWindow;

extern MenuHandle BrowseMenu;

#define WindowPlacements 50
int WindowCount[WindowPlacements];

extern int ScreenWidth;
extern int ScreenHeight;

/***********************************************/
/* DOFILECHOICE: Handle choice from file menu. */
/***********************************************/
DoFileChoice (theItem) 
 int theItem;
 {
  switch (theItem)  
    {
     /*=====================*/
     /* Handle New command. */
     /*=====================*/
     
     case NewItem:
       DoNew();
       break;
       
     /*=========================*/
     /* Handle Open... command. */
     /*=========================*/
     
     case OpenItem:
       DoOpen();
       break;
       
     /*===============================*/
     /* Handle Load Rules... command. */
     /*===============================*/
     
     case LoadRulesItem:
       DoLoadRules();
       break;
       
     /*===============================*/
     /* Handle Load Batch... command. */
     /*===============================*/
     
     case LoadBatchItem:
       DoLoadBatch();
       break;
       
     /*================================*/
     /* Handle OpenDribble... command. */
     /*================================*/
       
     case OpenDribbleItem:
       DoDribble();
       break;
        
     /*=======================*/
     /* Handle Close command. */
     /*=======================*/
     
     case CloseItem:
       DoClose();
       break;
       
     /*======================*/
     /* Handle Save command. */
     /*======================*/
       
     case SaveItem:
       DoSave();
       break;
       
     /*============================*/
     /* Handle Save As... command. */
     /*============================*/
     
     case SaveAsItem:
       DoSaveAs();
       break;
       
     /*========================*/
     /* Handle Revert command. */
     /*========================*/
     
     case RevertItem:
       DoRevert();
       break;
       
     /*===============================*/
     /* Handle Page Setup... command. */
     /*===============================*/
     
     case PageSetupItem:
       DoPageSetup();
       break;
       
     /*==========================*/
     /* Handle Print... command. */
     /*==========================*/
     
     case PrintItem:
       DoPrint(TheText,(GrafPtr) TheWindow);
       break;
       
     /*=============================*/
     /* Handle Transfer... command. */
     /*=============================*/
     
     case  TransferItem:
       DoTransfer();
       break;
       
     /*======================*/
     /* Handle Quit command. */
     /*======================*/
     
     case  QuitItem:
       DoQuit();
       break;
    }
 }
 
 
/******************************/
/* DONEW: Handle New command. */
/******************************/
DoNew()
  {
   WDHandle theData;
   Handle dataHandle;
   Rect destRect, viewRect;
   Rect	scrollRect;
   pascal char AutoScroll();
   int posValue;
   
   /*================================*/
   /* Make new window from template. */
   /*================================*/
   
   TheWindow = GetNewWindow(windowID,NULL,(WindowPtr) -1);
   
   /*========================================*/
   /* Offset the window from the location of */
   /* the previous window and then show it.  */
   /*========================================*/
   
   posValue = OffsetWindow(TheWindow);
   ShowWindow(TheWindow);
   
   /*=================================================================*/
   /* Set the port to the current window. Set the text font and size. */
   /*=================================================================*/
   
   SetPort(TheWindow);
   TextFont(monaco);
   TextSize(9);
   
   /*=============================================*/
   /* Set up the clipping rectangle and inset the */
   /* wrapping rectangle by the text margin.      */
   /*=============================================*/
   
   SetRect(&viewRect,0,0,(*TheWindow).portRect.right - (SBarWidth - 1),
                         (*TheWindow).portRect.bottom - (SBarWidth - 1));
   InsetRect(&viewRect,TextMargin,TextMargin); /* new */
   destRect = viewRect;
   /* InsetRect(&destRect,TextMargin,TextMargin); */
   
   /*=============================================*/
   /* Allocate the window's data record and store */
   /* it as the window's reference constant.      */
   /*=============================================*/
   
   dataHandle = NewHandle(sizeof(WindowData));
   SetWRefCon(TheWindow,(long int) dataHandle);
   
   /*=====================================================*/
   /* Lock the data record and convert to a typed handle. */
   /*=====================================================*/
   
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*========================*/
   /* Create an edit record. */
   /*========================*/
   
   (**theData).editRec = TENew(&destRect,&viewRect);
   
   /*=================================================*/
   /* Don't allow wrap around when text is displayed. */
   /*=================================================*/
   
   (**(**theData).editRec).crOnly = -1;
   
   /*===============================*/
   /* Create a vertical scroll bar. */
   /*===============================*/ 
   
   scrollRect = (*TheWindow).portRect;
   scrollRect.left = scrollRect.right - (SBarWidth - 1);
   scrollRect.right += 1;
   scrollRect.bottom -= 14;
   scrollRect.top -= 1;
   (**theData).vScrollBar = NewControl(TheWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		 
   /*=================================*/
   /* Create a horizontal scroll bar. */
   /*=================================*/ 
   
   scrollRect = (*TheWindow).portRect;
   scrollRect.top = scrollRect.bottom-15;
   scrollRect.bottom += 1;
   scrollRect.right -= 14;
   scrollRect.left -= 1;
   (**theData).hScrollBar = NewControl(TheWindow, &scrollRect, "\p", 1, 0, 0, 0,
		scrollBarProc, 0L);
		
   /*==================================================================*/
   /* The window is initially clean, has no associated file or volume, */
   /* and has been given a placement value by OffsetWindow.            */
   /*==================================================================*/
   
   (**theData).dirty = FALSE;
   (**theData).fileNumber = 0;
   (**theData).volNumber = 0;
   (**theData).placementPos = posValue;
   
   /*==============================*/
   /* Install auto-scroll routine. */
   /*==============================*/
   
   SetClikLoop(&AutoScroll,(**theData).editRec);    
  
   /*=====================*/
   /* Set global handles. */
   /*=====================*/
   
   TheVScrollBar = (**theData).vScrollBar;
   TheHScrollBar = (**theData).hScrollBar;
   TheText = (**theData).editRec;
   
   /*=================================================*/
   /* Make sure text margins are set properly so that */
   /* text does not "jump" upon initial text entry.   */
   /*=================================================*/
   
   FixText();   
   
   /*=====================*/
   /* Unlock data record. */
   /*=====================*/
   
   HUnlock(dataHandle);
   
   /*===============================================*/
   /* Enable Close and Print commands on File menu. */
   /*===============================================*/
   
   EnableItem(FileMenu,CloseItem);
   EnableItem(FileMenu,PrintItem);
   
   /*===============================================*/
   /* Enable/disable menu items in the search menu. */
   /*===============================================*/
   
   FixSearchMenu();
  }
 
/************************************************/
/* OFFSETWINDOW: Offset location of new window. */
/************************************************/
OffsetWindow (whichWindow)
  WindowPtr whichWindow;
  {
   int windowWidth, windowHeight;
   int hExtra, vExtra;
   int hMax, vMax;
   int windowLeft, windowTop;
   int i, best_place;
   
   /*=======================================*/
   /* Determine location on screen that has */ 
   /* the least number of stacked windows.  */
   /*=======================================*/
   
   best_place = 0;
   for (i = 1 ; i < WindowPlacements ; i++)
     {
      if (WindowCount[i] < WindowCount[best_place]) best_place = i;
     }
    
   /*===========================================*/
   /* Get window dimensions from port rectangle */
   /* and adjust for title bar.                 */  
   /*===========================================*/
    
   windowWidth = (*whichWindow).portRect.right - (*whichWindow).portRect.left;
   windowHeight = (*whichWindow).portRect.bottom - (*whichWindow).portRect.top;
   windowHeight = windowHeight + TitleBarHeight;
   
   /*======================================*/
   /* Find excess screen width and height. */
   /*======================================*/
   
   hExtra = ScreenWidth - windowWidth;
   vExtra = ScreenHeight - (windowHeight + MenuBarHeight);
   
   /*=============================================================*/
   /* Find maximum number of windows horizontally and vertically. */
   /*=============================================================*/
   
   hMax = (hExtra / hOffset) + 1;
   vMax = (vExtra / vOffset) + 1;
   
   /*============================================*/
   /* Update count of number of window's stacked */
   /* in the new window's location.              */
   /*============================================*/
   
   WindowCount[best_place] += 1;
   
   /*============================================*/
   /* Calculate offsets for new window adjusting */
   /* for the title bar and the menu bar.        */
   /*============================================*/
   
   windowLeft = ((best_place + 1) % hMax) * hOffset;
   windowTop =  ((best_place + 1) % vMax) * vOffset;
   windowTop += TitleBarHeight + MenuBarHeight;
   
   /*======================================*/
   /* Move the window to its new location. */
   /*======================================*/
   
   MoveWindow(whichWindow,windowLeft,windowTop,FALSE);
   
   /*=====================================*/
   /* Return the window's stack location. */
   /*=====================================*/
   
   return(best_place);
  }

/***********************************/
/* DOOPEN: Handle Open... command. */
/***********************************/
DoOpen()
  {
   Point dlgOrigin;
   SFTypeList theTypeList;
   SFReply theReply;
   
   /*=======================*/
   /* Set up dialog origin. */
   /*=======================*/
   
   SetPt(&dlgOrigin,DlgLeft,DlgTop);
   
   /*========================================*/
   /* Display only text files in the dialog. */
   /*========================================*/
   
   theTypeList[0] = 'TEXT';
   
   /*==========================*/
   /* Get file name from user. */
   /*==========================*/
   
   SFGetFile(dlgOrigin,NULL,NULL,1,theTypeList,NULL,&theReply);
   
   /*=======================================*/
   /* If user confirmed the file selection, */ 
   /* then open file and read into window.  */
   /*=======================================*/
   
   if (theReply.good)
     { OpenFile(theReply.fName,theReply.vRefNum); }
  }
  
/*********************************/
/* OPENFILE: Open document file. */
/*********************************/
OpenFile(fileName,vNum)
  Str255 fileName;
  int vNum;
  {
   WDHandle theData;
   Handle dataHandle;
   int theFile;
   OSErr resultCode;
   long logEOF;
   
   /*========================================================*/
   /* Open the file. Check for error and exit if one occurs. */
   /*========================================================*/
   
   resultCode = FSOpen(fileName,vNum,&theFile);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*===================================================*/
   /* Check that the file is not to large to be edited. */
   /*===================================================*/
   
   resultCode = GetEOF(theFile,&logEOF);
   IOCheck(resultCode);
   if (ErrorFlag)
     {
      resultCode = FSClose(theFile);
      IOCheck(resultCode);
      return;
     }
     
   if (logEOF >= MAX_BUFFER_SIZE)
     {
      ParamText("\pFile is too large to open ",
                "\p(only files 32K or smaller can be edited)","\p","\p");
      StopAlert(StopCantDoID,0L);
      resultCode = FSClose(theFile);
      IOCheck(resultCode);
      return;
     }
     
   /*=============================*/
   /* Open a window for the file. */
   /*=============================*/
   
   DoNew();
   
   /*==========================================*/
   /* Get the window data and lock it. Convert */
   /* the data to a typed handle.              */
   /*==========================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*============================================================*/
   /* Save the volume and file number in the window data record. */
   /*============================================================*/
   
   (**theData).volNumber = vNum;
   (**theData).fileNumber = theFile;
   
   /*=========================================*/
   /* The file name becomes the window title. */
   /*=========================================*/
   
   SetWTitle(TheWindow,fileName);
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*================================*/
   /* Read the file into the window. */
   /*================================*/
   
   DoRevert();
  }
  
/**********************************/
/* DOCLOSE: Handle Close command. */
/**********************************/
DoClose()
  {
   /*====================================================*/
   /* If the active window is one of our windows then... */
   /*====================================================*/
   
   if (FrontWindow() == TheWindow)
     { 
      /*=======================================================*/
      /* If the active window is the display window, close it. */
      /*=======================================================*/
      
      if (TheWindow == DisplayWindow) 
        {
         CloseOutputWindow();
         DisplayWindow = NULL;
        }
        
      /*================================================*/
      /* Else if the active window is the facts window, */
      /* close it and update the Browse menu.           */
      /*================================================*/
      
      else if (TheWindow == FactsWindow)
        {
         CloseOutputWindow();
         FactsWindow = NULL;
         CheckItem(BrowseMenu,FactsWindowItem,FALSE);
        }
        
      /*=================================================*/
      /* Else if the active window is the agenda window, */
      /* close it and update the Browse menu.            */
      /*=================================================*/
      
      else if (TheWindow == AgendaWindow)
        { 
         CloseOutputWindow();
         AgendaWindow = NULL;
         CheckItem(BrowseMenu,AgendaWindowItem,FALSE);
        }
        
      /*===============================*/
      /* Else close an editing window. */
      /*===============================*/
      
      else 
        { CloseTextWindow(); }
     }
     
   /*=============================*/
   /* Else close a system window. */
   /*=============================*/
   
   else
     { CloseSysWindow(); }
  }
  
/**********************************************/
/* CLOSEOUTPUTWINDOW: Close an output window. */
/**********************************************/
CloseOutputWindow()
  {
   Handle dataHandle;
   WindowPtr thisWindow;
   
   /*==================*/
   /* Get window data. */
   /*==================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   
   /*=======================*/
   /* Clear global handles. */
   /*=======================*/
   
   TheVScrollBar = NULL;
   TheHScrollBar = NULL;
   TheText = NULL;
   thisWindow = TheWindow;
   TheWindow = NULL;
   
   /*================================*/
   /* Dispose of window data record. */
   /*================================*/
   
   DisposHandle(dataHandle);
   
   /*=========================================================*/
   /* Destroy the window. CloseWindow  automatically disposes */
   /* of any controls associated with the window.             */
   /*=========================================================*/
   
   CloseWindow(thisWindow);
  }
  
/*********************************************/
/* CLOSETEXTWINDOW: Close an editing window. */
/*********************************************/
CloseTextWindow()
  {
   WDHandle theData;
   Handle dataHandle;
   Str255 theTitle;
   int theItem;
   OSErr resultCode;
   WindowPtr thisWindow;
   int posValue;
   
   /*=======================================*/
   /* Get window data. Lock the data record */ 
   /* and convert to a typed handle.        */
   /*=======================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*==================================================*/
   /* If the window contents have been changed then... */
   /*==================================================*/
   
   if ((**theData).dirty) 
     {
      /*======================================================*/
      /* Get the window title and place it in the alert text. */
      /*======================================================*/
      
      GetWTitle(TheWindow,theTitle);
      ParamText(theTitle,"\p","\p","\p");
      
      /*===================================================*/
      /* Post caution alert about closing a modified file. */
      /*===================================================*/
      
      theItem = CautionAlert(SaveID,NULL);  
      switch (theItem)
        {
         case saveItem:
           DoSave();
           if (ErrorFlag)
             {
              HUnlock(dataHandle);
              return;
             }
           break;
         
         case discardItem:
           break;
           
         case cancelItem:
           Quitting = FALSE;
           Transferring = FALSE;
           HUnlock(dataHandle);
           return;
           break;
        }
     }
     
   /*=================================================*/
   /* If the window is associated with a file then... */
   /*=================================================*/
   
   if ((**theData).fileNumber != 0)
     {
      /*========================================*/
      /* Close the file and check for an error. */
      /*========================================*/
      
      resultCode = FSClose((**theData).fileNumber);
      IOCheck(resultCode);
     
      /*========================================================*/
      /* If an error occurs, unlock the data record and return. */
      /*========================================================*/
      
      if (ErrorFlag)
        {
         HUnlock(dataHandle);
         return;
        }
     }
     
   /*======================================*/
   /* Dispose of the window's edit record. */
   /*======================================*/
   
   TEDispose((**theData).editRec);
   
   /*=======================*/
   /* Clear global handles. */
   /*=======================*/
   
   TheVScrollBar = NULL;
   TheHScrollBar = NULL;
   TheText = NULL;
   
   /*================================================================*/
   /* Update the count of stacked window's at the window's location. */
   /*================================================================*/
   
   posValue = (**theData).placementPos;
   WindowCount[posValue] -= 1;
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*==========================*/
   /* Update undo information. */
   /*==========================*/
   
   UndoCloseCheck(TheWindow);
   
   /*===================================================================*/
   /* Save the window pointer (because doactive will change TheWindow). */
   /*===================================================================*/
   
   thisWindow = TheWindow;
   
   /*===========================*/
   /* Force a deactivate event. */
   /*===========================*/
   
   HideWindow(TheWindow);
   TheWindow = NULL;
   
   /*====================================================*/
   /* Get and handle the activate and deactivate events. */
   /*====================================================*/
   
   if (GetTheEvent(activateEvt,&TheEvent))
     { DoActivate(); }
   if (GetTheEvent(activateEvt,&TheEvent))
     { DoActivate(); }
     
   /*======================================*/
   /* Dispose of the window's data record. */
   /*======================================*/
   
   DisposHandle(dataHandle);
   
   /*========================*/
   /* Dispose of the window. */
   /*========================*/
   
   DisposeWindow(thisWindow);
  }
  
/****************************************/
/* CLOSESYSWINDOW: Close system window. */
/***************************************/
CloseSysWindow()
  {
   WindowPeek whichWindow;
   int accNumber;
   
   /*===================================================*/
   /* Convert to a WindowPeek to examine window record. */
   /*===================================================*/
   
   whichWindow = (WindowPeek) FrontWindow();
   
   /*=========================================*/
   /* Get reference number of desk accessory. */
   /*=========================================*/
   
   accNumber = (*whichWindow).windowKind;
   
   /*=======================*/
   /* Close desk accessory. */
   /*=======================*/
   
   CloseDeskAcc(accNumber);
  }
    
/********************************/
/* DOSAVE: Handle Save command. */
/********************************/
DoSave() 
  {
   WDHandle theData;
   Handle dataHandle;
   
   /*=======================================*/
   /* Get window data. Lock the data record */
   /* and convert to a typed handle.        */
   /*=======================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*===============================================================*/
   /* If the window isn't associated with a file, then get the file */
   /* name from the user. Otherwise write to the window's file.     */
   /*===============================================================*/
   
   if ((**theData).fileNumber == 0)
     { DoSaveAs(); }
   else
     { WriteFile((**theData).fileNumber,(**theData).volNumber); }
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
  }
  
/****************************************/
/* DOSAVEAS: Handle Save As... command. */
/****************************************/
DoSaveAs() 
  {
   Point dlgOrigin;
   SFReply theReply;
   FInfo theInfo;
   int theFile;
   WDHandle theData;
   Handle dataHandle;
   StringHandle strHandle;
   Str255 untitled;
   int ignore;
   OSErr resultCode;
   
   /*=======================*/
   /* Set up dialog origin. */
   /*=======================*/
   
   SetPt(&dlgOrigin,DlgLeft,DlgTop);
   
   /*==================================*/
   /* Get the file name from the user. */
   /*==================================*/
   
   SFPutFile(dlgOrigin,"\pSave Current Document As:","\p",NULL,&theReply);
   
   /*========================================================*/
   /* If the user failed to confirm the file selection, then */
   /* cancel the quit command (if any), force an exit to the */
   /* main event loop, and return.                           */
   /*========================================================*/
   
   if (! theReply.good)
     {
      Quitting = FALSE;
      Transferring = FALSE;
      ErrorFlag = TRUE;
      return;
     }
     
   /*==========================================*/
   /* Get the finder information for the file. */
   /*==========================================*/
   
   resultCode = GetFInfo(theReply.fName,theReply.vRefNum,&theInfo);
   
   switch(resultCode)
     {
      case noErr:
        if (theInfo.fdType != 'TEXT')
          { 
           ParamText(theReply.fName,"\p","\p","\p");
           ignore = StopAlert(WrongTypeID,NULL);
           ErrorFlag = TRUE;
           return;
          }
        break;
        
      case fnfErr:
        resultCode = Create(theReply.fName,theReply.vRefNum,'CLPS','TEXT');
        IOCheck(resultCode);
        if (ErrorFlag) return;
        break;
        
      default:
        IOCheck(resultCode);
        return;
        break;
     }
     
   /*===========================================*/
   /* Get the window data. Lock the data record */
   /* and convert to a typed handle.            */
   /*===========================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
   
   /*======================================================*/
   /* If the window already has a file associated with it, */
   /* then close the old file. Return if an error occurs.  */
   /*======================================================*/
   
   if ((**theData).fileNumber != 0)
     {
      resultCode = FSClose((**theData).fileNumber);
      IOCheck(resultCode);
      if (ErrorFlag)
        {
         HUnlock(dataHandle);
         return;
        }
     }
   
   /*=============================================*/
   /* Open the new file and check for any errors. */
   /*=============================================*/
   
   resultCode = FSOpen(theReply.fName,theReply.vRefNum,&theFile);
   IOCheck(resultCode);
   if (ErrorFlag)
     {
      (**theData).volNumber = 0;
      (**theData).fileNumber = 0;
      
      SetWTitle(TheWindow,"\pUntitled");
     }
   else
     {
      (**theData).volNumber = theReply.vRefNum;
      (**theData).fileNumber = theFile;
      SetWTitle(TheWindow,theReply.fName);
      
      WriteFile(theFile,theReply.vRefNum);
     }
   
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
  }
 
/***********************************************/
/* WRITEFILE: Write window contents to a file. */
/***********************************************/
WriteFile(theFile,volNum)
  int theFile;
  int volNum;
  {
   Handle textHandle;
   long int textLength;
   OSErr resultCode;
   Str255 fName;
   FInfo finderInfo;
   
   /*==================================*/
   /* Change cursor to indicate delay. */
   /*==================================*/
   
   SetCursor(*Watch);
   
   /*=========================*/
   /* Set the file's creator. */
   /*=========================*/
   
   GetWTitle(TheWindow,fName);
   resultCode = GetFInfo(fName,volNum,&finderInfo);
   IOCheck(resultCode);
   if (ErrorFlag) return; 
   
   finderInfo.fdCreator = 'CLPS';
   resultCode = SetFInfo(fName,volNum,&finderInfo);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*======================================================*/
   /* Get text handle and current length from edit record. */
   /*======================================================*/
   
   HLock( (Handle) TheText);
   textHandle = (**TheText).hText;
   textLength = (**TheText).teLength;
   HUnlock( (Handle) TheText);
   
   /*=====================================================*/
   /* Set current file mark to the beginning of the file. */
   /*=====================================================*/
   
   resultCode = SetFPos(theFile,fsFromStart,0L);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*======================================*/
   /* Write the window's text to the file. */
   /*======================================*/
   
   HLock(textHandle);
   resultCode = FSWrite(theFile,&textLength,*textHandle);
   HUnlock(textHandle);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*=========================================*/
   /* Set length of file and check for error. */
   /*=========================================*/
   
   resultCode = SetEOF(theFile,textLength);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*======================================================*/
   /* Flush the volume buffer (that is, write the contents */
   /* of the volume buffer from memory to the disk).       */
   /*======================================================*/
   
   resultCode = FlushVol(NULL,volNum);
   IOCheck(resultCode);
   if (ErrorFlag) return;
   
   /*=============================*/
   /* Mark window as being clean. */
   /*=============================*/
   
   WindowDirty(FALSE);
  } 

/*********************************************/
/* DOREVERT: Handle Revert to Saved command. */
/*********************************************/
DoRevert()
  {
   WDHandle theData;
   Handle dataHandle;
   Str255 fileName;
   long int textLength;
   int theItem;
   OSErr resultCode;
   
   /*=================================================================*/
   /* Get the window data, lock it, and convert it to a typed handle. */
   /*=================================================================*/
   
   dataHandle = (Handle) GetWRefCon(TheWindow);
   HLock(dataHandle);
   theData = (WDHandle) dataHandle;
   
   /*=================================================================*/
   /* If the window contents have been changed, then determine if the */
   /* user wants to discard the contents or cancel the command.       */
   /*=================================================================*/
   
   if ((**theData).dirty)
     {
      GetWTitle(TheWindow,fileName);
      ParamText(fileName,"\p","\p","\p");
      
      theItem = CautionAlert(RevertID,NULL); 
      if (theItem == cancelItem)
        {
         HUnlock(dataHandle);
         ErrorFlag = TRUE;
         return;
        }
     }
     
   /*========================================*/
   /* Change the cursor to indicate a delay. */
   /*========================================*/
   
   SetCursor(*Watch);
   
   /*=================================================*/
   /* Get the length of the file and check for error. */
   /*=================================================*/
   
   resultCode = GetEOF ((**theData).fileNumber,&textLength);
   IOCheck(resultCode);
   if (ErrorFlag)
     {
      HUnlock(dataHandle);
      return;
     }
     
   /*=====================================================*/
   /* Set current file mark to the beginning of the file. */
   /*=====================================================*/
   
   resultCode = SetFPos((**theData).fileNumber,fsFromStart,NULL);
   IOCheck(resultCode);
   if (ErrorFlag)
     {
      HUnlock(dataHandle);
      return;
     }
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*============================================================*/ 
   /* Adjust text to the length of the file and set text length. */
   /*============================================================*/
   
   SetHandleSize((**TheText).hText,textLength);
   (**TheText).teLength = textLength;
   
   /*==================*/
   /* Lock the handle. */
   /*==================*/
   
   HLock((**TheText).hText);
   
   /*============================================*/
   /* Read the text of the file into the handle. */
   /*============================================*/
   
   resultCode = FSRead((**theData).fileNumber,&textLength,*(**TheText).hText);
   IOCheck(resultCode);
   
   /*========================================*/
   /* Unlock the handle and the edit record. */
   /*========================================*/
   
   HUnlock((**TheText).hText);
   HUnlock( (Handle) TheText);
   
   /*===================*/
   /* Check for errors. */
   /*===================*/
   
   if (ErrorFlag)
     {
      HUnlock(dataHandle);
      return;
     }
     
   /*=========================*/
   /* Unlock the data record. */
   /*=========================*/
   
   HUnlock(dataHandle);
   
   /*==================================================================*/
   /* Wrap text to the window and adjust scroll bar to length of text. */
   /*==================================================================*/
   
   TECalText(TheText);
   AdjustScrollBar();
   
   /*=================================================*/
   /* Make sure text margins are set properly so that */
   /* text does not "jump" upon initial text entry.   */
   /*=================================================*/
   
   FixText();
   
   /*===========================================*/
   /* Set insertion point at beginning of text. */
   /*===========================================*/
   
   TESetSelect(0,0,TheText);
   
   /*===================================================*/
   /* Force an update to redraw the text in the window. */
   /*===================================================*/
   
   InvalRect(&(*TheWindow).portRect);
   
   /*===========================*/
   /* Mark the window as clean. */
   /*===========================*/
   
   WindowDirty(FALSE);   
  }

/********************************/
/* DOQUIT: Handle Quit command. */
/********************************/
DoQuit()
  { 
   Quitting = TRUE; 
  }
  
   
/*********************************/
/* IOCHECK: Check for I/O error. */
/*********************************/
IOCheck(resultCode)
  OSErr resultCode;
  {
   int alertID;
   Str255 errorString;
   int ignore;
   
   /*==============================*/
   /* Return if there is no error. */
   /*==============================*/
   
   if (resultCode == noErr) return;
   
   /*===============*/
   /* Handle error. */
   /*===============*/
   
   switch(resultCode)
     {
      /*=========================*/
      /* Use already open alert. */
      /*=========================*/
      
      case opWrErr:
        alertID = OpWrID;
        break;
      
      /*========================================================*/
      /* Use general I/O error alert. Convert the error code to */
      /* a string and substitute it into the text of the alert. */
      /*========================================================*/
      
      default:
        alertID = IOErrID;
        NumToString(resultCode,errorString);
        ParamText(errorString,"\p","\p","\p");
        break;
     }
     
   /*===============================================*/
   /* Restore the normal cursor and post the alert. */
   /*===============================================*/
   
   InitCursor();
   StopAlert(alertID,NULL);
   
   /*=================================================================*/
   /* Cancel Quit command, if any, and force exit to main event loop. */
   /*=================================================================*/
   
   Quitting = FALSE;
   Transferring = FALSE;
   ErrorFlag = TRUE;
  }

/*****************************************************************/
/* AUTOSCROLL: Handle automatic scrolling during text selection. */
/*****************************************************************/   
pascal char AutoScroll() 
  {
   Point mousePoint;
   Rect textRect;
   RgnHandle saveClip;
   extern pascal void ScrollVText();
   extern pascal void ScrollHText();
   
   /*=============================================================*/
   /* Create a temporary region and save the existing clip region */
   /* in it. Clip to the entire port rectangle.                   */
   /*=============================================================*/
   
   saveClip = NewRgn();
   GetClip(saveClip);
   ClipRect(&(*TheWindow).portRect);
   
   /*=====================================================*/
   /* Find the mouse location and get the text rectangle. */
   /*=====================================================*/
   
   GetMouse(&mousePoint);
   textRect = (**TheText).viewRect;
   
   /*=================================================*/
   /* If the mouse is above the top of the rectangle, */
   /* then scroll up one line.                        */
   /*=================================================*/
   
   if (mousePoint.v < textRect.top)
     { ScrollVText(TheVScrollBar,inUpButton); }
     
   /*==============================================*/
   /* Else if the mouse is below the bottom of the */
   /* rectangle, then scroll down one line.        */
   /*==============================================*/
   
   else if (mousePoint.v > textRect.bottom)
     { ScrollVText(TheVScrollBar,inDownButton); }
     
   /*===============================================*/
   /* If the mouse is to the left of the rectangle, */
   /* then scroll left.                             */
   /*===============================================*/
   
   if (mousePoint.h < textRect.left)
     { ScrollHText(TheHScrollBar,inUpButton); }
     
   /*==========================================*/
   /* Else if the mouse is to the right of the */
   /* rectangle, then scroll right.            */
   /*==========================================*/
   
   else if (mousePoint.h > textRect.right)
     { ScrollHText(TheHScrollBar,inDownButton); }
   
   /*======================================*/
   /* Restore the original clipping region */
   /* and dispose of the temporary region. */
   /*======================================*/
   
   SetClip(saveClip);
   DisposeRgn(saveClip);
   
   /*=============================================*/
   /* Return true to continue tracking the mouse. */
   /*=============================================*/
   
   return((char) TRUE);
  }


  