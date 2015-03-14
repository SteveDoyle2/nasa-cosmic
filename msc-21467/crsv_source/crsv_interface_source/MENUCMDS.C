#include <stdio.h>

#include <ControlMgr.h>
#include <MenuMgr.h>
#include <stdfilepkg.h>
#include <EventMgr.h>
#include <FileMgr.h>
#include <PrintMgr.h>
#include <DeskMgr.h>

#include "interface.h"
#include "crsv.h"

/***************/
/* Definitions */
/***************/

#define topMargin 20
#define leftMargin 20
#define bottomMargin 20

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static THPrint            hPrint = NULL;

   static FILE              *DribbleFile;

   static int                TransferVrefnum;
   static Str255             TransferToName;
   
   static int                DefrelationsVrefnum;
   static Str255             DefrelationsFileName;
   
   static int                TraceVrefnum;
   static Str255             TraceFileName;

   static int                BeVerbose = 0;
   static int                CrossReference = 0;
   static int                StyleWarnings = 1;
   static int                Summaries = 0;
   static int                VerifyWithDefrelations = 1;
   static int                UserFunctions = 0;
   static int                CreateDefrelations = 0;
   static int                TraceDribbleFile = 0;

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern int                DeferredCommandTask;

   extern WindowPtr          TheWindow;
   extern EventRecord        TheEvent;
   
   extern MenuHandle         FileMenu;
   extern MenuHandle         OptionsMenu;
   
   extern CursHandle         Watch;

   extern WindowPtr          DisplayWindow;
   extern WindowPtr          FileListWindow;

   extern int                MyNumFiles;
   extern char               MyFileList[MAX_FILES][MAX_NAME];
   extern char               MyDefRelName[MAX_NAME];
   extern char               MyDribName[MAX_NAME];

/***********************************************/
/* DOFILECHOICE: Handle choice from file menu. */
/***********************************************/
DoFileChoice (theItem) 
 int theItem;
 {
  switch (theItem) 
    { 
     /*=============================*/
     /* Handle Check Files command. */
     /*=============================*/

     case CheckFilesItem:
       if (SetUpCheckCommand(FALSE) == FALSE) return;
       DeferredCommandTask = CheckFilesTask;
       break;
       
     /*======================================*/
     /* Handle Check Selected Files command. */
     /*======================================*/

     case CheckSelectedFilesItem:
       if (SetUpCheckCommand(TRUE) == FALSE) return;
       DeferredCommandTask = CheckSelectedFilesTask;
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
       
     /*==============================*/
     /* Handle Remove Files command. */
     /*==============================*/
     
     case RemoveFileItem:
       DoRemoveFiles();
       break;
       
     /*==============================*/
     /* Handle Add Files... command. */
     /*==============================*/
     
     case AddFileItem:
       SelectWindow(FileListWindow);
       DeferredCommandTask = AddFilesTask;
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
       DoPrint();
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
 
/**********************************************************/
/* SetUpCheckCommand: Sets up the data structures for the */
/*   Check Files and Check Selected Files commands.       */
/**********************************************************/
SetUpCheckCommand(checkSelections)
  int checkSelections;
  {
   /*============================*/
   /* Check for invalid options. */
   /*============================*/
   
   if (InvalidOptions() == TRUE) return(FALSE);
   
   /*==========================================*/
   /* Query for defrelation file if necessary. */
   /*==========================================*/
   
   if (QueryDefrelationsFile())
     { if (GetDefrelationsFile() == FALSE) return(FALSE); }
     
   /*====================================*/
   /* Query for trace file if necessary. */
   /*====================================*/
   
   if (QueryTraceFile())
     { if (GetTraceFile() == FALSE) return(FALSE); }

   /*=====================================================*/
   /* Copy the file information to the CRSV file buffers. */
   /*=====================================================*/
   
   StuffFileList(&MyNumFiles,MyFileList,checkSelections);
       
   /*==================================*/
   /* Copy the defrelations file name. */
   /*==================================*/
       
    PtoCstr(DefrelationsFileName);
    strcpy(MyDefRelName,DefrelationsFileName);
    CtoPstr(DefrelationsFileName);
    
    /*===========================*/
    /* Copy the trace file name. */
    /*===========================*/
       
    PtoCstr(TraceFileName);
    strcpy(MyDribName,TraceFileName);
    CtoPstr(TraceFileName);
    
    /*=================================*/
    /* Make the Display window active. */
    /*=================================*/
    
    SelectWindow(DisplayWindow);
       
    /*=====================================================*/
    /* Return true to indicate the command should proceed. */
    /*=====================================================*/
    
    return(TRUE);
   }
   
/*****************************************************/
/* ExecuteCheckFiles: Executes the deferred commands */
/*   for Check Files and Check Selected Files.       */
/*****************************************************/
ExecuteCheckFiles()
  {   
   /*===================================================*/
   /* Change the cursor to a watch to indicate a delay. */
   /*===================================================*/
   
   SetCursor(*Watch);
   
   /*==============================*/
   /* Have CRSV analyze the files. */
   /*==============================*/
/*   
   IF(CHECK_TRACE IS_ON) THEN
      strcpy(MyDribName, MyFileList[MyNumFiles - 1]);
      MyNumFiles--;
   END_IF
*/      
   process_main(MyNumFiles,MyFileList,MyDefRelName,MyDribName);
   
   /*=====================================*/
   /* Change the cursor back to an arrow. */
   /*=====================================*/
   
   InitCursor();
   
   /*============================================================*/
   /* Display an alert indicating the files have been processed. */
   /*============================================================*/
   
   ParamText("\pFinished processing all files","\p","\p","\p");
   NoteAlert(StopCantDoID,NULL);
   
   /*=======================================*/
   /* Make the List of Files window active. */
   /*=======================================*/
   
   SelectWindow(FileListWindow);
  }
  
/**********************************/
/* DOCLOSE: Handle Close command. */
/**********************************/
DoClose()
  {
   if (FrontWindow() != TheWindow)
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
/* DOQUIT: Handle Quit command. */
/********************************/
DoQuit()
  { DeferredCommandTask = QuittingTask; }
  
/*************************************************/
/* DoTransfer: Performs the Transfer... command. */
/*************************************************/
DoTransfer()
  {
   Point dlg_origin;
   SFTypeList the_type_list;
   SFReply the_reply;
   FInfo finderinfo;
   extern char *get_command_string();
	  
   /*=======================================================*/
   /* Set the upper corner point of the file dialog window. */
   /*=======================================================*/
   
   SetPt(&dlg_origin,100,85);
   
   /*======================================================*/
   /* Accept only files of type TEXT. Note that this line  */
   /* of code may not work for all Mac compilers since the */
   /* constant 'TEXT' is more than one character.          */
   /*======================================================*/
    
   the_type_list[0] = 'APPL';   
   
   /*====================*/
   /* Get the file name. */
   /*====================*/
   
   SFPGetFile(dlg_origin,NULL,NULL,1,the_type_list,NULL,&the_reply,
              TransferDialogID,NULL);

   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (the_reply.good == false) return;
   
   /*================================*/
   /* Save the transfer information. */     
   /*================================*/
    
   BlockMove(the_reply.fName,TransferToName,the_reply.fName[0] + 1);
   TransferVrefnum = the_reply.vRefNum;
   
   /*=================================================*/
   /* Defer the execution of the Transfer... command. */
   /*=================================================*/
   
   DeferredCommandTask = TransferringTask;
  }
  
/***************************************************************/
/* ExecuteTransfer: Executes the deferred Transfer... command. */
/***************************************************************/
ExecuteTransfer()
  {
   /*===========================================================*/
   /* Set the current directory to the application's directory. */
   /*===========================================================*/
   
   SetVol(NULL,TransferVrefnum);
   
   /*=========================*/
   /* Launch the application. */
   /*=========================*/
   
   Launch(0,TransferToName);
  }
  
/**********************************************/
/* DOPAGESETUP: Handle Page Setup... command. */
/**********************************************/
DoPageSetup()
  {
   /*====================================*/
   /* Open the printing manager for use. */
   /*====================================*/
   
   PrOpen();
   
   /*=====================*/
   /* Check for an error. */
   /*=====================*/
   
   if (PrError())
     {
      ParamText("\pUnable to locate a printer","\p","\p","\p");
      StopAlert(StopCantDoID,NULL);
      return;
     }
   
   /*============================================*/
   /* Create print record if one does not exist. */
   /*============================================*/
   
   CheckPrintRecord();
   
   /*===================================================*/
   /* Conduct a style dialog with the user to determine */
   /* the page dimensions and other information needed  */
   /* for page setup. The initial settings displayed in */
   /* the dialog box are taken from the most recent     */
   /* print record. Returns TRUE and saves the results  */
   /* in the print record hPrint if the user confirms   */
   /* the dialog . Otherwise returns FALSE.             */
   /*===================================================*/
   
   PrStlDialog(hPrint);
   
   /*=============================*/
   /* Close the printing manager. */
   /*=============================*/
   
   PrClose();
  }
  
/*************************************/
/* DOPRINT: Handle Print... command. */
/*************************************/
DoPrint()
  {
   /*====================================*/
   /* Open the printing manager for use. */
   /*====================================*/
   
   PrOpen();
   
   /*=====================*/
   /* Check for an error. */
   /*=====================*/
   
   if (PrError())
     {
      ParamText("\pUnable to locate a printer","\p","\p","\p");
      StopAlert(StopCantDoID,NULL);
      return;
     }
     
   /*============================================*/
   /* Create print record if one does not exist. */
   /*============================================*/
   
   CheckPrintRecord();

   /*=================================================*/
   /* Restore normal cursor before displaying dialog. */
   /*=================================================*/
   
   InitCursor();
   
   /*=====================================================*/
   /* Conduct a job dialog with the user to determine the */
   /* print quality, range of pages to print, and so on.  */
   /* If the user cancels the print command, then close   */
   /* the Printing Manager.                               */
   /*=====================================================*/
   
   if (PrJobDialog(hPrint) == 0) 
     { 
      PrClose();
      return;
     }
     
   /*==============================================*/
   /* Defer the execution of the Print... command. */
   /*==============================================*/
   
   DeferredCommandTask = PrintingTask;
  }
 
/*********************************************************/
/* ExecutePrint: Executes the deferred Print... command. */
/*********************************************************/ 
ExecutePrint()
  {
   GrafPtr savePort;
   int copies;
   TPrStatus prStatus;
   
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
   
   /*========================*/
   /* Save the current port. */
   /*========================*/
   
   GetPort(&savePort);
   
   /*============================================*/
   /* Determine the number of copies to be made. */
   /*============================================*/
   
   if ( (**hPrint).prJob.bJDocLoop == bDraftLoop)
     { copies = (**hPrint).prJob.iCopies; }
   else
     { copies = 1; }
       
   /*==================*/
   /* Print each copy. */
   /*==================*/ 
   
   for ( ; copies > 0 ; copies--) 
	 {
      PrDisplay(hPrint,(*DisplayWindow).txFont,(*DisplayWindow).txSize);
      /* Print a spooled document. */
	  PrPicFile(hPrint, 0L, 0L, 0L, &prStatus );
	 }

   /*============================*/
   /* Restore the original port. */
   /*============================*/
   
   SetPort(savePort);
	
   /*=============================*/
   /* Close the Printing Manager. */
   /*=============================*/
   
   PrClose();
   
   /*========================*/
   /* Restore normal cursor. */
   /*========================*/
   
   InitCursor();
  }
    
/**********************************************/
/* CHECKPRINTRECORD: Allocates a print record */
/*   if one does not already exist.           */
/**********************************************/
static CheckPrintRecord()
  {
   /*===========================================*/
   /* If a print record already exists, return. */
   /*===========================================*/
   
   if (hPrint != NULL) return;
    
   /*==============================*/
   /* Allocate a new print record. */
   /*==============================*/
   
   hPrint = (TPrint **) NewHandle( sizeof( TPrint ));
      
  /*=============================================*/
  /* Fill the fields of the print record hPrint  */
  /* with the default values that are stored in  */
  /* the printer resource file.                  */
  /*=============================================*/
                                      
   PrintDefault(hPrint);
  }
  
/************************************************/
/* DoEditChoice:  Handle choice from edit menu. */
/************************************************/
DoEditChoice(theItem)
  int theItem;
  {
   switch(theItem)
     {
      /*======================*/
      /* Handle Undo command. */
      /*======================*/
      
      case UndoItem:
        SystemEdit(undoCmd);
        break;
        
      /*=====================*/
      /* Handle Cut command. */
      /*=====================*/
      
      case CutItem:
        SystemEdit(cutCmd);
        break;
          
      /*======================*/
      /* Handle Copy command. */
      /*======================*/
      
      case CopyItem:
        SystemEdit(copyCmd);
        break;
         
      /*=======================*/
      /* Handle Paste command. */
      /*=======================*/
       
      case PasteItem:
        SystemEdit(pasteCmd);
        break;
          
      /*=======================*/
      /* Handle Clear command. */
      /*=======================*/
      
      case ClearItem:
        SystemEdit(clearCmd);
        break;
        
      /*===============================*/
      /* Handle Move to Front command. */
      /*===============================*/
      
      case MoveToFrontItem:
        DoMoveCellsToTop();
        break;
        
      /*================================*/
      /* Handle Move to Bottom command. */
      /*================================*/
      
      case MoveToBottomItem:
        DoMoveCellsToBottom();
        break;
     }
  }
  
/******************************************************/
/* DoOptionsChoice:  Handle choice from Options Menu. */
/******************************************************/
int DoOptionsChoice(item)
  int item;
  {
   int value;
   
   switch (item)  
     {
      case BeVerboseItem:
        BeVerbose = 1 - BeVerbose;
        set_VERBOSE(BeVerbose);
        break;
        
      case CrossReferenceItem:
        CrossReference = 1 - CrossReference;
        set_CHECK_RELATIONS(CrossReference);
        break;
        
      case StyleWarningsItem:
        StyleWarnings = 1 - StyleWarnings;
        set_CHECK_STYLE(StyleWarnings);
        break;
        
      case SummariesItem:
        Summaries = 1 - Summaries;
        set_CHECK_RULES(Summaries);
        break;
        
      case VerifyWithDefrelationsItem:
        VerifyWithDefrelations = 1 - VerifyWithDefrelations;
        set_CHECK_DEFRELS(VerifyWithDefrelations);
        break;
        
      case UserFunctionsItem:
        UserFunctions = 1 - UserFunctions;
        set_CHECK_EX_FLAG(UserFunctions);
        break;
        
      case CreateDefrelationsItem:
        CreateDefrelations = 1 - CreateDefrelations;
        set_CREATE_DEFRELS(CreateDefrelations);
        break;
        
      case TraceDribbleFileItem:
        TraceDribbleFile = 1 - TraceDribbleFile;
        set_ANALYZE_TRACE(TraceDribbleFile);
        break;
        
      case SetMaximumLiteralsItem:
        DoMaximumLiteralsDialog();
        break;
        
      default:
        break;
     }

   UpdateOptionsMenu();
  }
  
/**************************************************/
/* GetDefrelationsFile: Brings up a dialog box to */
/*   ask the user the name of the file in which   */ 
/*   defrelation information should be stored.    */
/**************************************************/
GetDefrelationsFile() 
  {
   Point dlgOrigin;
   SFReply theReply;
   
   /*===================================*/
   /* Get the name of the dribble file. */
   /*===================================*/
   
   SetPt(&dlgOrigin,DlgLeft,DlgTop);
   SFPutFile(dlgOrigin,"\pSave Defrelations As","\p",NULL,&theReply);
   
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (theReply.good == FALSE) return(FALSE);
   
   /*==================================*/
   /* Save the defrelations file name. */
   /*==================================*/
       
   BlockMove(theReply.fName,DefrelationsFileName,theReply.fName[0] + 1);
   DefrelationsVrefnum = theReply.vRefNum;
   
   /*============================================*/
   /* Return true to indicate a file was chosen. */
   /*============================================*/
   
   return(TRUE);
  }
  
/*********************************************************/
/* QueryDefrelationsFile: Returns true if a defrelations */
/*   file should be created, otherwise false.            */
/*********************************************************/
QueryDefrelationsFile()
  {
   return(CreateDefrelations);
  }
  
/****************************************************************/
/* GotoDefrelationsDirectory: Sets the current directory to the */
/*   location where the defrelations file should be stored.     */
/****************************************************************/
GotoDefrelationsDirectory()
  { SetVol(NULL,DefrelationsVrefnum); }
  
/*****************************************************/
/* UpdateOptionsMenu: Changes the check marks in the */
/*   Options menu to match the current settings.     */
/*****************************************************/
UpdateOptionsMenu()
  {
   CheckItem(OptionsMenu,BeVerboseItem,BeVerbose);
   CheckItem(OptionsMenu,CrossReferenceItem,CrossReference);
   CheckItem(OptionsMenu,StyleWarningsItem,StyleWarnings);
   CheckItem(OptionsMenu,SummariesItem,Summaries);
   CheckItem(OptionsMenu,VerifyWithDefrelationsItem,VerifyWithDefrelations);
   CheckItem(OptionsMenu,UserFunctionsItem,UserFunctions);
   CheckItem(OptionsMenu,CreateDefrelationsItem,CreateDefrelations);
   CheckItem(OptionsMenu,TraceDribbleFileItem,TraceDribbleFile);
  }
  
/*******************************************/
/* DODRIBBLE: Handles the Dribble command. */
/*******************************************/
DoDribble()
  {
   /*===============================================*/
   /* If a dribble file is active, then perform the */
   /* Dribble Off command. Otherwise perform the    */
   /* Dribble On... command.                        */ 
   /*===============================================*/
   
   if (DribbleFile != NULL)
     { DoCloseDribble(); }
   else
     { DoOpenDribble(); }
  }
  
/************************************************/
/* DOOPENDRIBBLE: Handle Dribble On... command. */
/************************************************/
DoOpenDribble() 
  {
   Point dlgOrigin;
   SFReply theReply;
   
   /*===================================*/
   /* Get the name of the dribble file. */
   /*===================================*/
   
   SetPt(&dlgOrigin,DlgLeft,DlgTop);
   SFPutFile(dlgOrigin,"\pSave Dialog Output As","\p",NULL,&theReply);
   
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (theReply.good == false) return;
   
   /*=============================================*/
   /* Reset the current volume so that UNIX fopen */
   /* function will find the file.                */
   /*=============================================*/
       
   SetVol(NULL,theReply.vRefNum); 
   
   /*========================*/
   /* Open the dribble file. */
   /*========================*/
   
   PtoCstr(theReply.fName);
   DribbleFile = fopen(theReply.fName,"w");
   
   /*======================================================*/
   /* Change the File menu to show a dribble file is open. */
   /*======================================================*/
   
   if (DribbleFile != NULL)
     { SetItem(FileMenu,OpenDribbleItem,"\pTurn Dribble Off"); }
  }
  
/***********************************************/
/* DOCLOSEDRIBBLE: Handle Dribble Off command. */
/***********************************************/
DoCloseDribble() 
  {
   fclose(DribbleFile);
   DribbleFile = NULL;
   SetItem(FileMenu,OpenDribbleItem,"\pTurn Dribble On...");
  }

/************************************************************/
/* PrintToDribbleFile: Prints a string to the dribble file. */
/************************************************************/
PrintToDribbleFile(theStr)
  char *theStr;
  {
   if (DribbleFile != NULL)
     { fprintf(DribbleFile,"%s",theStr); }
  }
   
/**************************************************************/
/* DribbleExitCleanup: Closes the dribble file if it is open. */
/**************************************************************/
DribbleExitCleanup()
  {
   if (DribbleFile != NULL) fclose(DribbleFile);
  }
  
/******************************************************/
/* InvalidOptions: Determines if a combination of the */
/*   options for use with the Check Files and Check   */
/*   Selected Files commands are invalid.             */
/******************************************************/
InvalidOptions()
  {
   if (VerifyWithDefrelations && CreateDefrelations)
     {
      ParamText("\pCannot use \"Verify with Defrelations\" and ",
                "\p\"Create Defrelations\" options simultaneously",
                "\p","\p");
      StopAlert(StopCantDoID,NULL);
      return(TRUE);
     }
     
   return(FALSE);
  }

/*****************************************************/
/* SetDefrelationsFileCreator: Sets the defrelations */
/*   file creator as CLIPS.                          */
/*****************************************************/
SetDefrelationsFileCreator()
  {
   SetFileCreator(DefrelationsFileName,DefrelationsVrefnum);
  }
  
/******************************************/
/* SetFileCreator: Sets a file's creator. */
/******************************************/
SetFileCreator(fName,volNum)
  Str255 fName;
  int volNum;
  {
   OSErr resultCode;
   FInfo finderInfo;
   
   resultCode = GetFInfo(fName,volNum,&finderInfo);
   if (resultCode != noErr) return;
   finderInfo.fdCreator = 'CLPS';
   resultCode = SetFInfo(fName,volNum,&finderInfo);
  }
  
/*********************************************************/
/* QueryTraceFile: Returns true if a CLIPS trace file is */ 
/*   to be examined, otherwise false.                    */
/*********************************************************/
QueryTraceFile()
  {
   return(TraceDribbleFile);
  }
  
/*******************************************************/ 
/* GetTraceFile: Get a single file to be added to the */
/*   List of Files window.                             */
/*******************************************************/ 
GetTraceFile()
  {
   Point dlg_origin;
   SFTypeList the_type_list;
   SFReply theReply;
   FInfo finderinfo; 

   /*=======================================================*/
   /* Set the upper corner point of the file dialog window. */
   /*=======================================================*/
   
   SetPt(&dlg_origin,100,85);
   
   /*======================================================*/
   /* Accept only files of type TEXT. Note that this line  */
   /* of code may not work for all Mac compilers since the */
   /* constant 'TEXT' is more than one character.          */
   /*======================================================*/
    
   the_type_list[0] = 'TEXT';   
   
   /*====================*/
   /* Get the file name. */
   /*====================*/
   
   SFPGetFile(dlg_origin,NULL,NULL,1,the_type_list,NULL,&theReply,
              TraceDialogID,NULL);
       
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (theReply.good == false) return(FALSE);
   
   /*===========================*/
   /* Save the trace file name. */
   /*===========================*/
       
   BlockMove(theReply.fName,TraceFileName,theReply.fName[0] + 1);
   TraceVrefnum = theReply.vRefNum;
 
   /*================================================*/
   /* Return true to indicate that a file was added. */
   /*================================================*/
   
   return(true);
  }
 
/*********************************************************/
/* GotoTraceDirectory: Sets the current directory to the */
/*   location where the trace file should be stored.     */
/*********************************************************/
GotoTraceDirectory()
  { SetVol(NULL,TraceVrefnum); }

/********************************************************************/
/* DoMaximumLiteralsDialog: Handle Set Maximum Literals... command. */
/********************************************************************/
DoMaximumLiteralsDialog()
  {
   DialogRecord dRecord;
   DialogPtr dptr;
   int item_num = 0;
   int oldoption1, oldoption2;
   Str255 MaxLiteralsStr;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   GrafPtr savePort;
   extern int  MAX_SINGLE_LIST;
   long tempNum;
   ControlHandle ctrlHandle;

   
   /*=========================*/
   /* Save the previous port. */       
   /*=========================*/
   
   GetPort(&savePort);
   
   /*==============================================*/
   /* Get the dialog and make it the current port. */
   /*==============================================*/
   
   dptr = GetNewDialog(MaximumLiteralsDialog,NULL,-1L);
   SetPort(dptr);
   ShowWindow(dptr);
   
   /*==================================================================*/
   /* Outline the OK button to indicate that it is the default button. */
   /*==================================================================*/
   
   outline_button(dptr,OKButton);
 
   /*===============================================*/
   /* The cursor for the dialog should be an arrow. */
   /*===============================================*/
   
   InitCursor();
      
   /*====================================================================*/
   /* Set the check boxes and edit text regions to their current values. */
   /*====================================================================*/
   
   NumToString((long int) MAX_SINGLE_LIST,MaxLiteralsStr);
   GetDItem(dptr,MaximumLiteralsEdit,&itemType,&itemHandle,&dispRect);
   SetIText(itemHandle,MaxLiteralsStr);
   SelIText(dptr,MaximumLiteralsEdit,0,MaxLiteralsStr[0]);
   
   /*=====================*/
   /* Conduct the dialog. */
   /*=====================*/
   
   item_num = -1;
   while ((item_num != OKButton) && (item_num != CancelButton))
     { 
      ModalDialog(0L,&item_num);
      switch (item_num)
        {
         case MaximumLiteralsEdit:
           FixOptionNumber(dptr,MaximumLiteralsEdit,3);
           GetDItem(dptr,MaximumLiteralsEdit,&itemType,&itemHandle,&dispRect);
           GetIText(itemHandle,MaxLiteralsStr);
           GetDItem(dptr,OKButton,&itemType,&itemHandle,&dispRect);
           ctrlHandle = (ControlHandle) itemHandle;
           if (MaxLiteralsStr[0] == 0)
             { HiliteControl(ctrlHandle,inactiveValue); }
           else
             { HiliteControl(ctrlHandle,activeValue); }
           break;
           
         case OKButton:
           GetDItem(dptr,MaximumLiteralsEdit,&itemType,&itemHandle,&dispRect);
           GetIText(itemHandle,MaxLiteralsStr);
           StringToNum(MaxLiteralsStr,&tempNum);
           MAX_SINGLE_LIST = tempNum;
           break;
        }
     }
     
   /*=====================*/
   /* Remove the dialog. */
   /*=====================*/
   
   DisposDialog(dptr);
   
   /*============================*/
   /* Restore the original port. */
   /*============================*/
   
   SetPort(savePort);
  }
  
/*********************************************************/
/* FixOptionNumber: Makes sure that an option number has */
/* no more than three characters which must be numeric.  */
/*********************************************************/
FixOptionNumber(dptr,item_number,max_length)
  DialogPtr dptr;
  int item_number, max_length;
  {
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   Str255 itemText;
   int error = FALSE;
   int i;
   
   /*======================================================*/
   /* Retrieve the text of the dialog editable text field. */
   /*======================================================*/
   
   GetDItem(dptr,item_number,&itemType,&itemHandle,&dispRect);
   GetIText(itemHandle,itemText);
   
   /*========================================================*/
   /* If the text length is greater than the maximum length, */
   /* then set the length to the maximum length and signal   */
   /* an error.                                              */
   /*========================================================*/
   
   if (itemText[0] > max_length)
     { 
      itemText[0] = max_length; 
      error = TRUE;
     }
     
   /*============================================================*/
   /* If any of the text characters are nonnumeric, then set the */
   /* text length to one less than the offending character and   */
   /* signal an error.                                           */
   /*============================================================*/
   
   for (i = 1 ; i <= itemText[0] ; i++)
     {
      if ((itemText[i] < '0') || (itemText[i] > '9'))
        {
         error = TRUE;
         itemText[0] = i - 1;
        }
     }
     
   /*=========================================*/
   /* If an error has occured, then reset the */
   /* editable text field to a valid value.   */
   /*=========================================*/
   
   if (error)
     {
      SetIText(itemHandle,itemText);
      SelIText(dptr,item_number,0,itemText[0]);
      SysBeep(10);
      SelIText(dptr,item_number,itemText[0],itemText[0]);
     }
  }
  
/***********************************************************/
/* OUTLINE_BUTTON: Outlines a button in a dialog box which */
/*   indicates that the button is the default button.      */
/***********************************************************/
outline_button(dptr,itemNumber)
  DialogPtr dptr;
  int itemNumber;
  {
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*===============================================================*/
   /* Get information about the dialog item that is to be outlined. */
   /*===============================================================*/
   
   GetDItem(dptr,itemNumber,&itemType,&itemHandle,&dispRect);
   
   /*=====================*/
   /* Outline the button. */
   /*=====================*/
   
   PenSize(3,3);
   InsetRect(&dispRect,-4,-4);
   FrameRoundRect(&dispRect,16,16);
  }
  