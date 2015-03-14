/*   CLIPS Version 4.30   4/25/89 */

#include <DialogMgr.h>
#include <EventMgr.h>
#include <ListMgr.h>
#include <ToolboxUtil.h>

#include "fontpick.h"

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static ListHandle       FontChoices;
   static DialogPtr        FontDialog;

/****************************************/
/* LOCAL INTERNAL FUNCTIONS DEFINITIONS */
/****************************************/
   
   void                    CreateFontDialog();
   pascal Boolean          FontSelectFilter();
   int                     PerformFontDialog();
   void                    SetOKButton();
   void                    StoreFontNamesInList();
   
/*****************************************/
/* GLOBAL INTERNAL FUNCTIONS DEFINITIONS */
/*****************************************/

   int                     DoSetFont();
   int                     GetNextFont();

/******************************************/
/* DoSetFont: Handle Set Font... command. */
/******************************************/
DoSetFont(newFontPtr,newSizePtr)
  int *newFontPtr, *newSizePtr;
  {
   GrafPtr savePort;
   int doChange;
   int FontNumbers[MAXFONTSDISPLAYED], FontSizes[MAXFONTSDISPLAYED];
   
   /*====================*/
   /* Save the old port. */                   
   /*====================*/
   
   GetPort(&savePort);
   
   /*=========================*/
   /* Create the font dialog. */
   /*=========================*/
   
   CreateFontDialog(FontNumbers,FontSizes); 
   
   /*==========================*/
   /* Perform the font dialog. */
   /*==========================*/
   
   doChange = PerformFontDialog(FontNumbers,FontSizes,
                                newFontPtr,newSizePtr);
   
   /*===================================================*/
   /* Dispose of the list of fonts and the font dialog. */
   /*===================================================*/
    
   LDispose(FontChoices);
   DisposDialog(FontDialog);
   
   /*=======================*/
   /* Restore the old port. */
   /*=======================*/
   
   SetPort(savePort);
   
   /*====================================*/
   /* Return flag indicating whether the */ 
   /* font change should be performed.   */
   /*====================================*/
   
   return(doChange);
  }
  
/******************************************************************/
/* CreateFontDialog: Creates the font dialog and stores the names */
/*   and sizes of the fonts in the font list in the dialog.       */
/******************************************************************/
static void CreateFontDialog(FontNumbers,FontSizes)
  int FontNumbers[], FontSizes[];
  {
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   Rect hdataRect;
   Point cellSize;
   
   /*=============================================*/
   /* Indicate a delay while building the dialog. */
   /*=============================================*/
   
   SetCursor(*GetCursor(watchCursor));
   
   /*==============================================================*/
   /* Create the Set Font dialog box and make it the current port. */
   /*==============================================================*/
   
   FontDialog = GetNewDialog(SET_FONT_RESOURCE_NUMBER,0L,-1L);
   SetPort(FontDialog);
   
   /*=================================================================*/
   /* Get the information about the dialog item where the list of     */ 
   /* fonts will be displayed. Inset the display rectangle associated */
   /* with the item by one so that the list of fonts will be within   */
   /* the rectangle. Reduce the right border by the additional amount */
   /* that the scroll bar will take.                                  */
   /*=================================================================*/
   
   GetDItem(FontDialog,SET_FONT_DIALOG_SELECT,&itemType,&itemHandle,&dispRect);
   InsetRect(&dispRect,1,1);
   dispRect.right -= (SBarWidth - 1);
   
   /*===============================================================*/
   /* Create a rectangle for the LNew function which indicates that */
   /* the list initially has no fonts displayed in it.              */
   /*===============================================================*/
   
   SetRect(&hdataRect,0,0,1,0);
   
   /*======================================================*/
   /* Create a point for the LNew function which indicates */
   /* the height and width of a cell in the list.          */
   /*======================================================*/
   
   SetPt(&cellSize,(dispRect.right - dispRect.left),16);
   
   /*========================================================*/
   /* Call the LNew function to create a list for displaying */
   /* the fonts. The list will have a vertical scroll bar.   */
   /*========================================================*/
   
   FontChoices = LNew(&dispRect,&hdataRect,cellSize,0,FontDialog,
                  FALSE,FALSE,FALSE,TRUE);
                  
   /*===================================================*/
   /* Only one selection from the list will be allowed. */
   /*===================================================*/
   
   (*FontChoices)->selFlags = lOnlyOne;
   
   /*========================================================*/
   /* Store the names of the fonts in the cells of the list. */
   /*========================================================*/
   
   StoreFontNamesInList(FontNumbers,FontSizes);
  
   /*==================================*/
   /* Enable drawing of the font list. */
   /*==================================*/
   
   LDoDraw((Boolean)TRUE,FontChoices);
   
   /*=========================*/
   /* Display the dialog box. */
   /*=========================*/
   
   ShowWindow(FontDialog);
   
   /*========================================*/
   /* Draw a rectangle around the font list. */
   /*========================================*/
   
   InsetRect(&dispRect,-1,-1);
   FrameRect(&dispRect);
   
   /*====================================*/
   /* Force an update of the dialog box. */
   /*====================================*/
   
   LUpdate(FontDialog->visRgn,FontChoices);
   
   /*=====================*/
   /* Change cursor back. */
   /*=====================*/
   
   InitCursor();
  }
  
/***********************************************************/
/* StoreFontNamesInList: Store the names and sizes of the  */
/*   fonts in the cells of the font list. Also stores the  */
/*   font numbers and sizes in arrays for quick reference. */
/***********************************************************/  
static void StoreFontNamesInList(FontNumbers,FontSizes)
  int FontNumbers[], FontSizes[];
  {
   int location, font, size;
   Str255 fontName;
   Str255 fontSize;
   char combine[515];
   Cell theCell;
   
   /*=================================================*/
   /* Start at cell zero with the system font size 1. */
   /*=================================================*/
   
   location = 0;
   font = 0; 
   size = 1;
   
   /*=========================================*/
   /* Continue adding fonts to the list until */
   /* all fonts and sizes have been added.    */
   /*=========================================*/
   
   while (GetNextFont(&font,&size,fontName) != 0)
     {
      /*========================================*/
      /* Append the font size to the font name. */
      /*========================================*/
      
      NumToString((long int) size,fontSize);
      PtoCstr(fontName);
      PtoCstr(fontSize);
      strcpy(combine,fontName);
      strcat(combine," ");
      strcat(combine,fontSize);
      
      /*==================================================*/
      /* Store the font name and size string in the list. */
      /*==================================================*/
      
      LAddRow(1,location,FontChoices);
      SetPt(&theCell,0,location);
      LSetCell(combine,strlen(combine),theCell,FontChoices);
      
      /*==============================================================*/
      /* Save the font number and size in the quick reference arrays. */
      /*==============================================================*/
      
      FontNumbers[location] = font;
      FontSizes[location] = size;
      
      /*========================================================*/
      /* Move on to the next cell. Look for the next font size. */
      /*========================================================*/
      
      location++;
      size++;
      
      /*=======================================*/
      /* Do not include more fonts than can be */
      /* held in the quick reference arrays.   */
      /*=======================================*/
      
      if (location >= MAXFONTSDISPLAYED) return;
     }
  }
  
/************************************************************/ 
/* GetNextFont: Finds the next font available in the system */
/*   found at or after the specified font number and size.  */
/*   Returns true if a font is found, otherwise false.      */
/************************************************************/   
GetNextFont(FontNumberPtr,FontSizePtr,fontName)
  int *FontNumberPtr, *FontSizePtr;
  Str255 fontName;
  {
   /*=============================*/
   /* Search until no fonts left. */
   /*=============================*/
   
   for (; *FontNumberPtr <= MAXSYSTEMFONTS; (*FontNumberPtr)++)
     {
      /*==================================*/
      /* Check to see if the font exists. */
      /*==================================*/
      
      GetFontName(*FontNumberPtr,fontName);
      
      /*========================================================*/
      /* If the font exists and it is not the application font, */ 
      /* then search through each size of the font looking for  */
      /* a size that is defined.                                */
      /*========================================================*/
      
      if ((fontName[0] != 0) && (*FontNumberPtr != 1))
        {
         for (; *FontSizePtr <= MAXFONTSIZE ; (*FontSizePtr)++)
           { if (RealFont(*FontNumberPtr,*FontSizePtr)) return(TRUE); }
        }
        
      /*===========================================================*/
      /* Begin the search for the next font found at point size 1. */
      /*===========================================================*/
      
      *FontSizePtr = 1;
     }
   
   /*==============================*/
   /* No font found, return false. */
   /*==============================*/
   
   return(FALSE);
  }
  
/**********************************************************************/
/* PerformFontDialog: Handles interaction with user with font dialog. */
/**********************************************************************/
static PerformFontDialog(FontNumbers,FontSizes,newFontPtr,newSizePtr)
  int FontNumbers[], FontSizes[], *newFontPtr, *newSizePtr;
  {
   int itemNum = 0;
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   FontInfo finfo;
   Point theCell;
   int doChange;
   int index;
   StringHandle displayString;
   
   *newFontPtr = -1;
   
   /*======================================*/
   /* The OK button is initially inactive, */
   /* since no font has been selected.     */
   /*======================================*/
   
   SetOKButton(inactiveValue);
   
   /*=======================================*/
   /* Get information about the area where  */
   /* the sample string is to be displayed. */
   /*=======================================*/
   
   GetDItem(FontDialog,SET_FONT_DIALOG_TEXT,
            &itemType,&itemHandle,&dispRect);  
   
   /*================================================*/
   /* Get a handle to the sample string and lock it. */
   /*================================================*/
   
   displayString = GetString(DISPLAYSTRINGID);
   HLock(displayString);
   
   /*============================================================*/
   /* Remain in this loop until the user has selected either the */
   /* OK or cancel button. This loop lasts as long as the user   */
   /* continues to select a font from the list. The function     */
   /* FontSelectFilter that is passed as an argument is used to  */
   /* handle mouse-clicks in the list.                           */
   /*============================================================*/
   
   ModalDialog(FontSelectFilter,&itemNum); 
   while (itemNum == SET_FONT_DIALOG_SELECT)
     {  
      SetPt(&theCell,0,0);
      if (LGetSelect(TRUE,&theCell,FontChoices))
        {
         index = theCell.v;
    
         if (*newFontPtr == -1)
           { SetOKButton(activeValue); }
         
         if ((*newFontPtr != FontNumbers[index]) ||
             (*newSizePtr != FontSizes[index]))
           {
            *newFontPtr = FontNumbers[index];
            *newSizePtr = FontSizes[index];
            TextFont(*newFontPtr);
            TextSize(*newSizePtr);
         
            GetFontInfo(&finfo);
      
            MoveTo(dispRect.left,dispRect.top + finfo.ascent);
            EraseRect(&dispRect);
            DrawText(*displayString + 1,0,*displayString[0]);
      
            TextFont(0);
            TextSize(12);
           }
        }
      else
        {  
         MoveTo(dispRect.left,dispRect.top + finfo.ascent);
         EraseRect(&dispRect);
         *newFontPtr = -1;
         SetOKButton(inactiveValue); 
        }
        
      ModalDialog(FontSelectFilter,&itemNum); 
     }
  
   /*==============================================================*/
   /* Unlock the sample string and mark the resource as purgeable. */
   /*==============================================================*/
   
   HUnlock(displayString);
   HPurge(displayString);
   /* ReleaseResource(displayString); */
   
   /*==================================================*/
   /* If the cancel button was selected, return false. */
   /* Otherwise, return true.                          */
   /*==================================================*/
   
   if (itemNum == SET_FONT_DIALOG_CANCEL)
     { return(FALSE); }
     
   return(TRUE);
  }

/***************************************************************/
/* FontSelectFilter: Dialog filter function for handling mouse */
/*   clicks inside the font list area of the font dialog.      */
/***************************************************************/
static pascal Boolean FontSelectFilter(theDialog,theEvent,itemNumber)
  DialogPtr theDialog;
  EventRecord *theEvent;
  int *itemNumber;
  {
   Point mouse_loc; 
   int itemType;
   Handle itemHandle;
   Rect dispRect;
  
   /*========================================*/
   /* If the event is not a mouse-down, then */
   /* allow the event to be handle normally. */
   /*========================================*/
   
   if ((*theEvent).what != mouseDown) return(FALSE);
   
   /*==============================================*/
   /* Get Point in screen coordinates and convert  */
   /* to window coordinates.                       */
   /*==============================================*/
   
   mouse_loc = theEvent->where;
   GlobalToLocal(&mouse_loc);
   
   /*======================================================*/
   /* Get the location of the list area in the dialog box. */
   /*======================================================*/
   
   GetDItem(theDialog,SET_FONT_DIALOG_SELECT,&itemType,&itemHandle,&dispRect);
   
   /*=======================================================*/
   /* If the mouse-down event occured inside the list area, */
   /* then call LClick to handle the event. Set the value   */
   /* of the item hit to the list area and indicate that    */
   /* the event has been handle by returning TRUE.          */
   /*=======================================================*/
   
   if (PtInRect(mouse_loc,&dispRect)) 
     {
      LClick(mouse_loc,theEvent->modifiers,FontChoices);
      *itemNumber = SET_FONT_DIALOG_SELECT;
      return (TRUE);
     }
   
   /*=========================================*/
   /* Allow the event to be handled normally. */
   /*=========================================*/
   
   return (FALSE); 
  }

/*********************************************************/
/* SetOKButton: Activates and deactivates the OK button. */
/*********************************************************/
static void SetOKButton(value)
  int value;
  {
   Handle tempHandle;
   Rect tempRect;
   int tempType;
   
   GetDItem(FontDialog,SET_FONT_DIALOG_OK,&tempType,&tempHandle,&tempRect);
   HiliteControl((ControlHandle) tempHandle,value);
  }