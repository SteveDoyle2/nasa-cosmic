
#include <WindowMgr.h>
#include <ListMgr.h>
#include <EventMgr.h>
#include <FileMgr.h> 
#include <stdfilepkg.h>
#include <MenuMgr.h>

#include "interface.h"
#include "crsv.h"

/**************/
/* Structures */
/**************/

struct fileInfo
  {
   Str255 theFileName;
   int fileVrefnum;
   struct fileInfo *next;
  };
  
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static ListHandle         choices;
   static WindowRecord       FileListRecord;
   static struct fileInfo   *FileList = NULL;

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   WindowPtr	             FileListWindow;
   
/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern EventRecord        TheEvent;

   extern int                ScreenWidth;
   extern int                ScreenHeight;
  
   extern MenuHandle         FileMenu;

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char              *gen_alloc();
   extern void               gen_free();

/**************************************************************/
/* InitFileListWindow:  Initializes the List of Files window. */
/**************************************************************/
InitFileListWindow()
  {   
   Rect dispRect;
   Point cellSize;
   Rect hdata_rect;
   int hsize, vsize, wleft, wtop;
   
   /*====================*/
   /* Create the window. */
   /*====================*/
   
   SetPort((FileListWindow = GetNewWindow( FileListWindowID, &FileListRecord, -1L )));
   
   /*====================================================================*/
   /* Size and Place the window on the right side of the Display window. */
   /*====================================================================*/
   
   hsize = 0.25 * (ScreenWidth - (6 + 6 + 6));
   vsize = (ScreenHeight - (46 + 6));
   vsize = (vsize / SBarWidth) * SBarWidth;
   wtop = 46;
   wleft = 6 + (0.75 * (ScreenWidth - (6 + 6 + 6))) + 6;
   SizeWindow(FileListWindow,hsize,vsize,FALSE);
   MoveWindow(FileListWindow,wleft,wtop,FALSE);
   
   /*================================================================*/
   /* Get the information about the window where the list of files   */ 
   /* will be displayed. Inset the display rectangle associated with */
   /* the window by one so that the list of files will be within the */
   /* the rectangle. Reduce the borders by the additional amount     */
   /* that the scroll bar and bottom window edge will take.          */
   /*================================================================*/
   
   dispRect = (*FileListWindow).portRect;
   dispRect.right -= (SBarWidth - 1);
   dispRect.bottom -= (SBarWidth - 1);
   
   /*===============================================================*/
   /* Create a rectangle for the LNew function which indicates that */
   /* the list is to have one column with no rows since there are   */
   /* no files in the list to begin.                                */
   /*===============================================================*/
   
   SetRect(&hdata_rect,0,0,1,0);
   
   /*======================================================*/
   /* Create a point for the LNew function which indicates */
   /* the height and width of a cell in the list.          */
   /*======================================================*/
   
   SetPt(&cellSize,(dispRect.right - dispRect.left),16);
   
   /*========================================================*/
   /* Call the LNew function to create a list for displaying */
   /* the files. The list will have a vertical scroll bar.   */
   /*========================================================*/
   
   choices = LNew(&dispRect,&hdata_rect,cellSize,0,FileListWindow,
                  FALSE,TRUE,FALSE,TRUE);  
                        
   /*==============================================*/
   /* Allow multiple and discontinuous selections. */
   /*==============================================*/
   
   (*choices)->selFlags = lNoExtend | lUseSense;
  
   /*==================================*/
   /* Enable drawing of the file list. */
   /*==================================*/
   
   LDoDraw((Boolean)TRUE,choices);
   
   /*==================*/
   /* Show the window. */
   /*==================*/
     
   ShowWindow(FileListWindow); 
  }
  
/*******************************************************************/
/* GrowFileListWindow: Handle grow event for the File List window. */
/*******************************************************************/
GrowFileListWindow(newWidth,newHeight)
  int newWidth, newHeight;
  {
   Point cSize;
   
   SetPt(&cSize,newWidth - (SBarWidth - 1),16);
   LCellSize(cSize,choices);
   LSize(newWidth - (SBarWidth - 1), newHeight - (SBarWidth - 1), choices);  
   DrawGrowIcon(FileListWindow);
  }

/***************************************************************************/
/* UpdateFileListWindow: Handle update event for the List of Files window. */
/***************************************************************************/
UpdateFileListWindow()
  {   
   Point cSize;
   FontInfo finfo;
   int y, bottom;
   GrafPtr save_port;
   RgnHandle rgn_hnd;
   Rect viewRect;
   int x, count;
   
   /*==============================================================*/
   /* Save the old port. Set the port to the List of Files window. */
   /*==============================================================*/
   
   GetPort(&save_port);
   SetPort(FileListWindow);
     
   /*============================================================*/
   /* Begin the update of the List of Files window. Temporarily  */ 
   /* restricts the visible region of the window by intersecting */
   /* it with the update region.                                 */
   /*============================================================*/
   
   BeginUpdate(FileListWindow);
  
   /*=================================================*/
   /* Erase the visible region of the window. Redraw  */
   /* the grow box and update the window's file list. */
   /*=================================================*/
  
   EraseRect(&(*FileListWindow).portRect);
   DrawGrowIcon(FileListWindow);
   LUpdate(FileListWindow->visRgn,choices);

   /*======================*/
   /* Update is completed. */
   /*======================*/
   
   EndUpdate(FileListWindow);

   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(save_port);
  }
  
/*****************************************************/ 
/* DoFileListContent: Handle mouse-down event in the */
/*   content region of the List of Files window.     */
/*****************************************************/  
DoFileListContent()
  {
   int				cntlCode;
   ControlHandle 	theControl;
   int				pageSize;
   GrafPtr			savePort;
   Point thePoint;
	
   /*==============================================================*/
   /* Save the old port. Set the port to the List of Files window. */
   /*==============================================================*/
   
   GetPort(&savePort);
   SetPort(FileListWindow);
   
   /*==============================================*/
   /* Get the mouse location in screen coordinates */
   /* and convert it to window coordinates.        */
   /*==============================================*/
   
   thePoint = TheEvent.where;
   GlobalToLocal(&thePoint);
   
   /*========================================================*/
   /* Allow the List Manager to handle the mouse-down event. */
   /*========================================================*/
   
   LClick(thePoint,TheEvent.modifiers,choices);
	 
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
  } 
  
/***************************************************/ 
/* DoRemoveFile: Removes all of the selected cells */
/*   from the file list window.                    */
/***************************************************/ 
DoRemoveFiles()
  {
   Cell theCell;
   int dataLen;
   Str255 theName;
   
   /*====================================================*/
   /* Disable List Manager drawing while removing files. */
   /*====================================================*/
   
   LDoDraw(false,choices);
   
   /*===================================*/
   /* Remove all of the selected cells. */
   /*===================================*/
   
   SetPt(&theCell,0,0);
   while (LGetSelect(TRUE,&theCell,choices) != 0)
     {
      dataLen = 254;
      LGetCell(&theName[1],&dataLen,theCell,choices);
      theName[0] = dataLen;
      
      RemoveFileFromFileList(theName);
      
      LDelRow(1,theCell.v,choices);
      SetPt(&theCell,0,0);
     }
   
   /*==============================*/
   /* Enable List Manager drawing. */
   /*==============================*/
   
   LDoDraw(true,choices);
   
   /*==========================================*/
   /* Force an update of the file list window. */
   /*==========================================*/
   
   InvalRect(&FileListWindow->portRect);
  }

/*******************************************************/ 
/* AddSingleFile: Get a single file to be added to the */
/*   List of Files window.                             */
/*******************************************************/ 
AddSingleFile()
  {
   Point dlg_origin;
   SFTypeList the_type_list;
   SFReply the_reply;
   FInfo finderinfo;  
   Point theCell;
   pascal char FileListFilter();
   int loc;

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
   
   SFPGetFile(dlg_origin,NULL,FileListFilter,1,the_type_list,NULL,&the_reply,
              AddDialogID,NULL);
    
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (the_reply.good == false) return(FALSE);
   
   /*================================*/
   /* Add the file to the file list. */     
   /*================================*/
       
   loc = FileListCount();
   LAddRow(1,loc,choices);
   SetPt(&theCell,0,loc);
   LSetCell(&the_reply.fName[1],the_reply.fName[0],theCell,choices);
   
   /*==============================================================*/
   /* Keep track of the volume reference number for the file name. */
   /*==============================================================*/
   
   AddFileToFileList(the_reply.fName,the_reply.vRefNum);
   
   /*================================================*/
   /* Return true to indicate that a file was added. */
   /*================================================*/
   
   return(true);
  }

/*************************************************************************/ 
/* FileListFilter: Allows only files that have not already been selected */
/*   to be displayed in the file selection dialog box for adding a file. */
/*************************************************************************/ 
pascal char FileListFilter(paramBlock)
  ParmBlkPtr paramBlock;
  { 
   Cell theCell;
   
   SetPt(&theCell,0,0);
   return LSearch(&paramBlock->fileParam.ioNamePtr[1],
                  paramBlock->fileParam.ioNamePtr[0],
                  NULL,&theCell,choices);
  }


/****************************************************************/ 
/* FileListCount: Returns the number of files in the file list. */
/****************************************************************/ 
FileListCount()
  { return (**choices).dataBounds.bottom; }
  
/*****************************************/ 
/* FileListSelection: Returns the number */
/*   of selected files in the file list. */
/*****************************************/ 
FileListSelections()
  {
   int count = 0;
   Cell theCell;
   
   SetPt(&theCell,0,0);
   while (LGetSelect(TRUE,&theCell,choices)) 
     {
      count++;
      SetPt(&theCell,theCell.h,theCell.v + 1);
     }
   
   return(count);
  }

/**************************************************************/
/* DoFileListActivate: Activate/Deactivate the List Manager's */
/*   file list associated with the List of Files window.      */
/**************************************************************/
DoFileListActivate(value)
  int value;
  { LActivate(value,choices); }
  
/*******************************************************/
/* UpdateFileList: Update the List Manager's file list */
/*   associated with the List of Files window.         */
/*******************************************************/
UpdateFileList()
  { LUpdate(FileListWindow->visRgn,choices); }
   
/********************************************************/ 
/* StuffFileList: Take the files from the List of Files */
/*   window and place them in the CRSV file buffer.     */
/********************************************************/ 
StuffFileList(num_files,file_list,selections_only)
  int  *num_files;
  char  file_list[MAX_FILES][MAX_NAME];
  int selections_only;
  {
   int loc, dataLen;
   Cell theCell;
   Str255 data;
   int i, max;
   int file_loc = 0;
     
   /*===========================================================*/
   /* Determine the number of files to be placed in the buffer. */
   /* If only selections are placed in the buffer, the number   */
   /* of files will equal the number of selections, otherwise,  */
   /* the number of files will be the number of files in the    */
   /* file list.                                                */
   /*===========================================================*/
   
   if (selections_only == TRUE)
     { *num_files = FileListSelections(); }
   else
     { *num_files = FileListCount(); }
     
   /*==========================================================*/
   /* Loop through all of the files in the file list, stuffing */
   /* the appropriate files in the file buffer.                */
   /*==========================================================*/
   
   max = FileListCount();
   
   for (loc = 0; loc < max; loc++)
     {
      SetPt(&theCell,0,loc);
      if ((selections_only == FALSE) ||
          (LGetSelect(FALSE,&theCell,choices) == TRUE))
        {
         dataLen = 254;
         LGetCell(&data[1],&dataLen,theCell,choices);
         data[0] = dataLen;
         PtoCstr(data);
         strcpy(file_list[file_loc],data);
         file_loc++;
        }
     }
  }
  
/************************************************************/ 
/* DoMoveCellsToTop: Handles the Move Cells to Top command. */
/************************************************************/ 
DoMoveCellsToTop()
  { 
   Cell theCell;
   int number, i;
   
   /*=================================================*/
   /* Move the selected files to the top of the list. */
   /*=================================================*/
   
   number = FileListSelections();
   MoveFileCells(0,choices);
   
   /*============================================*/
   /* Reselect the files at the top of the list. */
   /*============================================*/
   
   for (i = 0; i < number; i++)
     {       
      SetPt(&theCell,0,i);
      LSetSelect(TRUE,theCell,choices);
     }
  }
  
/******************************************************************/ 
/* DoMoveCellsToBottom: Handles the Move Cells to Bottom command. */
/******************************************************************/ 
DoMoveCellsToBottom()
  { 
   Cell theCell;
   int number, i, max;
   
   /*====================================================*/
   /* Move the selected files to the bottom of the list. */
   /*====================================================*/
   
   max = FileListCount() - 1;
   number = FileListSelections();
   MoveFileCells(FileListCount() - 1,choices);
   
   /*===============================================*/
   /* Reselect the files at the bottom of the list. */
   /*===============================================*/
   
   for (i = 0; i < number; i++)
     { 
      SetPt(&theCell,0,max - i);
      LSetSelect(TRUE,theCell,choices);
     } 
  }
  
/************************************************************************/ 
/* MoveFileCells: Handles moving cells either to top or bottom of list. */
/************************************************************************/ 
MoveFileCells(where,lhdl)
  int where;  
  ListHandle lhdl;
  {
   Str255 data;
   int dataLen;
   Cell theCell;
   int inc_flag = 0;
   
   /*=====================================================================*/
   /* If moving to top, increment placement position with each placement. */
   /*=====================================================================*/
   
   if (where == 0) inc_flag = 1;
   
   /*==================================================*/
   /* Disable List Manager drawing while moving files. */
   /*==================================================*/
   
   LDoDraw(false,lhdl);
   
   /*===================================*/
   /* Remove all of the selected cells. */
   /*===================================*/
   
   SetPt(&theCell,0,0);
   while (LGetSelect(TRUE,&theCell,lhdl) != 0)
     {
      /*======================*/
      /* Copy the cells data. */
      /*======================*/
      
      dataLen = 254;
      LGetCell(&data[1],&dataLen,theCell,lhdl);
      data[0] = dataLen;
      
      /*==================*/
      /* Delete the cell. */
      /*==================*/
      
      LDelRow(1,theCell.v,lhdl);
      
      /*=======================*/
      /* Move to new location. */
      /*=======================*/
      
      LAddRow(1,where,lhdl);
      SetPt(&theCell,0,where);
      LSetCell(&data[1],data[0],theCell,lhdl);
      
      /*=======================*/
      /* Search for next cell. */
      /*=======================*/
      
      if (inc_flag) where++;
      SetPt(&theCell,0,0);
     }
   
   /*==============================*/
   /* Enable List Manager drawing. */
   /*==============================*/
   
   LDoDraw(true,lhdl);
   
   /*==========================================*/
   /* Force an update of the file list window. */
   /*==========================================*/
   
   InvalRect(&FileListWindow->portRect);
  }
  
/******************************************************/ 
/* DisposeFileList: Gets rid of the List Manager file */
/*   list associated with the List of Files window.   */
/******************************************************/
DisposeFileList()
  { LDispose(choices); }
  
/***************************************************/ 
/* AddFileToFileList: Adds a file to the list that */
/*   keeps track of the volume reference numbers   */
/*   associated with the file names.               */
/***************************************************/
AddFileToFileList(theName,theVrefnum)
  Str255 theName;
  int theVrefnum;
  {
   struct fileInfo *newFile;
   
   newFile = (struct fileInfo *) gen_alloc(sizeof (struct fileInfo));
   newFile->fileVrefnum = theVrefnum;
   BlockMove(theName,newFile->theFileName,theName[0] + 1);
   
   newFile->next = FileList;
   FileList = newFile;
  }
  
/********************************************************/ 
/* RemoveFileFromFileList: Removes a file from the list */
/*   that keeps track of the volume reference numbers   */
/*   associated with the file names.                    */
/********************************************************/
RemoveFileFromFileList(theName)
  Str255 theName;
  {
   struct fileInfo *thisFile, *lastFile = NULL;
   
   /*=================================*/
   /* Loop through the list of files. */
   /*=================================*/
   
   thisFile = FileList;
   while (thisFile != NULL)
     {
      /*========================================*/
      /* If the file name is found, then remove */
      /* the file from the list and return.     */
      /*========================================*/
                                   
      if (IUCompString(thisFile->theFileName,theName) == 0)
        {
         if (lastFile == NULL)
           { FileList = thisFile->next; }
         else
           { lastFile->next = thisFile->next; }
         gen_free(thisFile, sizeof(struct fileInfo));
         return;
        }
        
      /*=================================*/
      /* Move on to check the next file. */
      /*=================================*/
      
      lastFile = thisFile;
      thisFile = thisFile->next;
     }
  }
  
/***********************************************************************/ 
/* GoToFileDirectory: Searches for volume reference number associated  */
/*   with the named file. If found, sets the current directory to that */
/*   volume.                                                           */
/***********************************************************************/
GotoFileDirectory(theName)
  char *theName;
  {
   struct fileInfo *thisFile;
   
   /*=================================*/
   /* Loop through the list of files. */
   /*=================================*/
   
   thisFile = FileList;
   while (thisFile != NULL)
     {
      /*=============================================*/
      /* Convert the stored file name to a C string. */
      /*=============================================*/
      
      PtoCstr(thisFile->theFileName);
      
      /*======================================================*/
      /* If the named file matches the stored file name, then */
      /* reset the current directory, change the stored file  */
      /* name back to a Pascal string, and return.            */
      /*======================================================*/
      
      if (strcmp(thisFile->theFileName,theName) == 0)
        {
         SetVol(NULL,thisFile->fileVrefnum);
         CtoPstr(thisFile->theFileName);
         return;
        }
        
      /*===============================================*/
      /* Convert the stored file name back to a Pascal */
      /* string. Move on to check the next file.       */
      /*===============================================*/
      
      CtoPstr(thisFile->theFileName);
      thisFile = thisFile->next;
     }
  }
  
Wait()
  {
   SysBeep(10);
   while (! Button());
   while (Button());
  }
  