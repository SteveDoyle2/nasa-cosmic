/*   CLIPS Version 4.30   4/25/89 */

#include "interface.h"

#include <TextEdit.h>
#include <DeskMgr.h>
#include <MenuMgr.h>
#include <ScrapMgr.h>
#include <DialogMgr.h>

#define OKButton          1
#define CancelButton      2
#define DialogPopupCheck  4
#define ProfessorJoeCheck 5
#define StepIncrementEdit 7

int DialogPopupOption = TRUE;
int ProfessorJoeOption = TRUE;

Str255 StepStr = "\p1";

extern MenuHandle EditMenu;
extern MenuHandle BufferMenu;

extern TEHandle TheText;
extern WindowPtr TheWindow;

int ScrapCompare;
extern int ScrapDirty;

/************************************************/
/* DoEditChoice:  Handle choice from edit menu. */
/************************************************/
DoEditChoice(theItem)
  int theItem;
  {
   int newFont, newSize;
   
   switch(theItem)
     {
      /*======================*/
      /* Handle Undo command. */
      /*======================*/
      
      case UndoItem:
        if (! SystemEdit(undoCmd)) DoUndo(TheText);
        break;
        
      /*=====================*/
      /* Handle Cut command. */
      /*=====================*/
      
      case CutItem:
        if (! SystemEdit(cutCmd)) DoCut();
        break;
          
      /*======================*/
      /* Handle Copy command. */
      /*======================*/
      
      case CopyItem:
        if (! SystemEdit(copyCmd)) DoCopy();
        break;
         
      /*=======================*/
      /* Handle Paste command. */
      /*=======================*/
       
      case PasteItem:
        if (! SystemEdit(pasteCmd)) DoPaste();
        break;
          
      /*=======================*/
      /* Handle Clear command. */
      /*=======================*/
      
      case ClearItem:
        if (! SystemEdit(clearCmd)) DoClear();
        break;
        
      /*=========================*/
      /* Handle Balance command. */
      /*=========================*/
      
      case BalanceItem:
        DoBalance();
        break;
        
      /*=============================*/
      /* Handle Set Font... command. */
      /*=============================*/
      
      case SetFontItem:
        if (DoSetFont(&newFont,&newSize))
          { SetFontAttributes(newFont,newSize); }
        break;
        
      /*============================*/
      /* Handle Options... command. */
      /*============================*/
      
      case OptionsItem:
        DoOptions();
        break;
     }
  }
  
/******************************/
/* DoCut: Handle Cut command. */
/******************************/  
DoCut()
  {
   /*=====================================*/
   /* Make sure the selection is visible. */
   /*=====================================*/
   
   ScrollToSelection(FALSE);
      
   /*====================================*/
   /* Save information for undo command. */
   /*====================================*/
   
   SaveUndoInfo(TheWindow,TheText,UNCUT);

   /*====================*/
   /* Cut the selection. */
   /*====================*/
   
   TECut(TheText);
   
   /*==================================================================*/
   /* Adjust the scroll bar to the length of the text, adjust the text */
   /* to match the scroll bar, and keep the insertion point visible.   */
   /*==================================================================*/
   
   AdjustScrollBar();
   AdjustText();
   ScrollToSelection(FALSE);
   
   /*==========================================================*/
   /* Disable menu items that operate on a nonempty selection. */
   /*==========================================================*/
   
   DisableItem(EditMenu,CutItem);
   DisableItem(EditMenu,CopyItem);
   DisableItem(EditMenu,ClearItem);
   
   /*=======================*/
   /* Enable paste command. */
   /*=======================*/
   
   EnableItem(EditMenu,PasteItem);
   
   /*=====================================*/
   /* Mark the scrap and window as dirty. */
   /*=====================================*/
   
   ScrapDirty = TRUE;
   WindowDirty(TRUE);
   
   /*=============================*/
   /* Fix undo item in Edit menu. */
   /*=============================*/
   
   SetUndoItem(UNCUT);
   FixUndoMenuItem(TheWindow);
  }
  
/*********************************/
/* DoCopy: Handles copy command. */
/*********************************/  
DoCopy()
  {
   /*=====================================*/
   /* Make sure the selection is visible. */
   /*=====================================*/
   
   ScrollToSelection(FALSE);
   
   /*=====================*/
   /* Copy the selection. */
   /*=====================*/
   
   TECopy(TheText);
   
   /*===========================*/
   /* Enable the Paste command. */
   /*===========================*/
   
   EnableItem(EditMenu,PasteItem);
   
   /*==========================*/
   /* Mark the scrap as dirty. */
   /*==========================*/
   
   ScrapDirty = TRUE;
  }
  
/**********************************/
/* DoPaste: Handle Paste command. */
/**********************************/  
DoPaste()
  {
   long int size;
   
   /*================================================*/
   /* Don't perform the paste if the new buffer size */
   /* would exceed the maximum buffer size.          */
   /*================================================*/
   
   size = (**TheText).teLength;
   size += TEGetScrapLen();
   
   if (size >= MAX_BUFFER_SIZE)
     {
      InitCursor();
      ParamText("\pCan't Paste ",
                "\p(only files 32K or smaller can be edited)","\p","\p");
      StopAlert(StopCantDoID,0L);
      return;
     }
     
   /*====================================*/
   /* Save information for undo command. */
   /*====================================*/
   
   SaveUndoInfo(TheWindow,TheText,UNPASTE);
   
   /*=================================*/
   /* Make sure selection is visible. */
   /*=================================*/
   
   ScrollToSelection(FALSE);
   
   /*==================*/
   /* Paste the Scrap. */
   /*==================*/
   
   TEPaste(TheText);
   
   /*==================================================*/
   /* Adjust scroll bar to length of text, adjust text */
   /* to match scroll bar, and keep selection visible. */                   
   /*==================================================*/
   
   AdjustScrollBar();
   AdjustText();
   ScrollToSelection(FALSE);
   
   /*==========================================================*/
   /* Disable menu items that operate on a nonempty selection. */                   
   /*==========================================================*/
   
   DisableItem(EditMenu,CutItem);
   DisableItem(EditMenu,CopyItem);
   DisableItem(EditMenu,ClearItem);
   
   /*===========================*/
   /* Mark the window as dirty. */
   /*===========================*/
   
   WindowDirty(TRUE);
   
   /*=============================*/
   /* Fix undo item in Edit menu. */
   /*=============================*/
   
   SetUndoItem(UNPASTE);
   FixUndoMenuItem(TheWindow);
  }
  
/**********************************/
/* DoClear: Handle Clear command. */
/**********************************/  
DoClear()
  {
   /*=================================*/
   /* Make sure selection is visible. */
   /*=================================*/
   
   ScrollToSelection(FALSE);
      
   /*====================================*/
   /* Save information for undo command. */
   /*====================================*/
   
   SaveUndoInfo(TheWindow,TheText,UNCLEAR); 

   /*=======================*/
   /* Delete the selection. */
   /*=======================*/
   
   TEDelete(TheText);
   
   /*==================================================*/
   /* Adjust scroll bar to length of text, adjust text */
   /* to match scroll bar, and keep selection visible. */                   
   /*==================================================*/
   
   AdjustScrollBar();
   AdjustText();
   ScrollToSelection(FALSE);
   
   /*==========================================================*/
   /* Disable menu items that operate on a nonempty selection. */                   
   /*==========================================================*/
   
   DisableItem(EditMenu,CutItem);
   DisableItem(EditMenu,CopyItem);
   DisableItem(EditMenu,ClearItem);
   
   /*===========================*/
   /* Mark the window as dirty. */
   /*===========================*/
   
   WindowDirty(TRUE);
      
   /*=============================*/
   /* Fix undo item in Edit menu. */
   /*=============================*/
   
   SetUndoItem(UNCLEAR);
   FixUndoMenuItem(TheWindow);
  }
  
/*************************************************/
/* FixEditMenu: Enable/disable editing commands. */
/*************************************************/
FixEditMenu()
  {
   /*===================*/
   /* Fix Undo command. */
   /*===================*/
   
   FixUndoMenuItem(TheWindow);
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*==============================================*/
   /* If the selection is empty, then disable menu */
   /* items that operate on a nonempty selection.  */
   /*==============================================*/
   
   if ( (**TheText).selStart == (**TheText).selEnd )
     {
      DisableItem(EditMenu,CutItem);
      DisableItem(EditMenu,CopyItem);
      DisableItem(EditMenu,ClearItem);
      DisableItem(BufferMenu,CompileSelectionItem);
     }
     
   /*=====================================*/
   /* Else enable menu items that operate */
   /* on a nonempty selection.            */
   /*=====================================*/
   
   else
     {
      EnableItem(EditMenu,CutItem);
      EnableItem(EditMenu,CopyItem);
      EnableItem(EditMenu,ClearItem);
      EnableItem(BufferMenu,CompileSelectionItem);
     }
     
   /*=========================*/
   /* Unlock the edit record. */
   /*=========================*/
   
   HUnlock( (Handle) TheText);
   
   /*===============================================*/
   /* If the scrap is empty, then disable the Paste */
   /* command, otherwise enable the Paste command.  */
   /*===============================================*/
   
   if (TEGetScrapLen() == 0)
     { DisableItem(EditMenu,PasteItem); }
   else
     { EnableItem(EditMenu,PasteItem); }
   
  }
  
/******************************************************/
/* ReadDeskScrap: Read desk scrap into toolbox scrap. */
/******************************************************/
ReadDeskScrap()
  {
   PScrapStuff scrap_ptr;
   long int scrapLength = 0;
   long int ignore;
   OSErr result;
   
   /*===========================================*/
   /* If the scrap hasn't changed, then return. */
   /*===========================================*/
   
   scrap_ptr = InfoScrap();
   if (ScrapCompare == scrap_ptr->scrapCount) return;
   
   /*=======================================*/
   /* Check the desk scrap for a text item. */
   /*=======================================*/
   
   scrapLength = GetScrap(NULL,'TEXT',&ignore);
   
   /*=======================================================*/
   /* If there is a text item, then transfer the desk scrap */
   /* to the toolbox scrap. If no error occurs, then assign */
   /* assign the correct scrap length.                      */
   /*=======================================================*/
   
   if (scrapLength >= 0)
     {
      result = TEFromScrap();
      if (result != noErr)
        { scrapLength = result; }
     }
        
   /*==================================================*/
   /* If the scrap was nonempty, then enable the Paste */
   /* command. Otherwise, mark the toolbox scrap as    */
   /* empty and disable the Paste command.             */
   /*==================================================*/
   
   if (scrapLength > 0)
     { EnableItem(EditMenu,PasteItem); }
   else
     {
      TESetScrapLen(0);
      DisableItem(EditMenu,PasteItem);
     }
      
   /*========================================*/
   /* Save scrap count for later comparison. */
   /*========================================*/
   
   ScrapCompare = scrap_ptr->scrapCount;
  }
  
/******************************************************/
/* WriteDeskScrap: Write toolbox scrap to desk scrap. */
/******************************************************/
WriteDeskScrap()
  {
   /*===================================*/
   /* If the scrap hasn't changed since */
   /* the last read, then return.       */
   /*===================================*/
   
   if (! ScrapDirty) return;
   
   /*==========================================================*/
   /* Change the scrap count and save it for later comparison. */
   /*==========================================================*/
   
   ScrapCompare = ZeroScrap();
   
   /*===============================================*/
   /* Transfer the toolbox scrap to the desk scrap. */
   /*===============================================*/
   
   TEToScrap();
   
   /*====================================*/
   /* Toolbox and desk scraps now agree. */
   /*====================================*/
   
   ScrapDirty = FALSE;
  }
  
/**********************************************************************/
/* InitializeScrap: Initializes the scrap for use by the application. */
/**********************************************************************/
InitializeScrap()
  {
   PScrapStuff scrap_ptr;
   
   /*==========================================*/
   /* Toolbox and desk scraps initially agree. */
   /*==========================================*/
   
   ScrapDirty = FALSE;
   
   /*=======================*/
   /* Force scrap transfer. */
   /*=======================*/
   
   scrap_ptr = InfoScrap();
   ScrapCompare = scrap_ptr->scrapCount + 1;
   
   /*=====================================*/
   /* Read desk scrap into toolbox scrap. */
   /*=====================================*/
   
   ReadDeskScrap();
  }
 
/**************************************/
/* DoBalance: Handle Balance command. */
/**************************************/
DoBalance()
  {
   int left_middle, right_middle, text_length;
   Handle textHandle;
   char *text_ptr;
   int left_count, right_count;
   int count, i;
   
   /*====================================================*/
   /* Check to make sure there is an active edit record. */
   /* This check should be unnecessary.                  */
   /*====================================================*/
   
   if (TheText == NULL) return;
   
   /*=============================================================*/
   /* Get information about the current selection to be balanced. */
   /*=============================================================*/
   
   left_middle = (**TheText).selStart;
   right_middle = (**TheText).selEnd;
   text_length = (**TheText).teLength;
   textHandle = (**TheText).hText;
   text_ptr = *textHandle;
      
   /*===================================*/
   /* If the selection is empty then... */
   /*===================================*/
   
   if (left_middle == right_middle)
     {
      /*============================================*/
      /* If '(' is to the right of the cursor, then */
      /* all balancing should occur to the right.   */
      /*============================================*/
      
      if (text_ptr[left_middle] == '(')
        { BalanceIt(left_middle-1,left_middle+1,1,0,text_ptr,text_length); }
        
      /*================================================*/
      /* Else if ')' is to the left of the cursor, then */
      /* all balancing should occur to the left.        */
      /*================================================*/
      
      else if ((left_middle > 0) ? (text_ptr[left_middle - 1] == ')') : 0)
        { BalanceIt(left_middle-2,left_middle,0,-1,text_ptr,text_length); }
        
      /*====================================================*/
      /* Else balancing occurs to left and right of cursor. */
      /*====================================================*/
      
      else
        { BalanceIt(left_middle-1,right_middle,0,0,text_ptr,text_length); }
     } 
     
   /*===================================================*/
   /* Otherwise the selection is non-empty therefore... */
   /*===================================================*/
   
   else
     { 
      /*===============================================*/
      /* Determine the number of right parentheses ')' */
      /* that need to be balanced from the left side.  */
      /*===============================================*/
      
      count = 0;    
      left_count = 0;
      
      for (i = left_middle ; i < right_middle ; i++)
        {
         if (text_ptr[i] == '(') count++;
         else if (text_ptr[i] == ')') count--;
         if (count < left_count) left_count = count;
        }
        
      /*===============================================*/
      /* Determine the number of left parentheses '('  */
      /* that need to be balanced from the right side. */
      /*===============================================*/
      
      count = 0;    
      right_count = 0;
      
      for (i = right_middle - 1 ; i >= left_middle ; i--)
        {
         if (text_ptr[i] == '(') count++;
         else if (text_ptr[i] == ')') count--;
         if (count > right_count) right_count = count;
        }
      
      /*==============================================*/
      /* Balance to the left and right of the cursor. */
      /*==============================================*/
       
      BalanceIt(left_middle-1,right_middle,left_count,right_count,text_ptr,text_length);
     }
        
   /*=================================*/
   /* Enable/disable edit menu items. */
   /*=================================*/
   
   FixEditMenu();     
  }
 
/********************************************************/
/* BalanceIt: Balances a selection of text by extending */
/* it to the left and right until the number of left    */
/* and right parentheses is balanced.                   */
/********************************************************/
BalanceIt(left_middle,right_middle,left_count,right_count,text_ptr,text_length)
  int left_middle, right_middle;
  int left_count, right_count;
  char *text_ptr;
  {
   /*==========================================================*/
   /* Balance the left side of the text by moving left and up. */
   /*==========================================================*/
   
   while (left_count <= 0) 
     {
      if (left_middle < 0) 
        { 
         SysBeep(10);
         return;
        }
        
      if (text_ptr[left_middle] == '(') left_count++;
      else if (text_ptr[left_middle] == ')') left_count--;
         
      left_middle--;
     }
        
   /*==============================================================*/
   /* Balance the right side of the text by moving right and down. */
   /*==============================================================*/
   
   while (right_count >= 0)
     {
      if (right_middle > text_length) 
        { 
         SysBeep(10);
         return;
        }
        
      if (text_ptr[right_middle] == '(') right_count++;
      else if (text_ptr[right_middle] == ')') right_count--;
         
      right_middle++;
     }
        
  /*=============================================*/
  /* Set the current selection to balanced text. */
  /*=============================================*/
  
  TESetSelect((long int) left_middle + 1, (long int) right_middle,TheText);
  
  /*=====================================*/
  /* Make sure the selection is visible. */
  /*=====================================*/
  
  ScrollToSelection(TRUE);
 }
 
/*****************************************/
/* DoOptions: Handle Options... command. */
/*****************************************/
DoOptions()
  {
   DialogRecord dRecord;
   DialogPtr dptr;
   int item_num = 0;
   int oldoption1, oldoption2;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   GrafPtr savePort;
   
   /*=========================*/
   /* Save the previous port. */       
   /*=========================*/
   
   GetPort(&savePort);
   
   /*==============================================*/
   /* Get the dialog and make it the current port. */
   /*==============================================*/
   
   dptr = GetNewDialog(256,NULL,-1L);
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
   
   oldoption1 = DialogPopupOption;
   oldoption2 = ProfessorJoeOption;
   SetCheckBox(dptr,DialogPopupCheck,DialogPopupOption);
   SetCheckBox(dptr,ProfessorJoeCheck,ProfessorJoeOption);
   GetDItem(dptr,StepIncrementEdit,&itemType,&itemHandle,&dispRect);
   SetIText(itemHandle,StepStr);
   SelIText(dptr,StepIncrementEdit,0,StepStr[0]);
   
   /*=====================*/
   /* Conduct the dialog. */
   /*=====================*/
   
   item_num = -1;
   while ((item_num != OKButton) && (item_num != CancelButton))
     { 
      ModalDialog(0L,&item_num);
      switch (item_num)
        {
         case StepIncrementEdit:
           FixOptionNumber(dptr,StepIncrementEdit,3);
           break;
           
         case ProfessorJoeCheck:
           ProfessorJoeOption = ToggleCheckBox(dptr,ProfessorJoeCheck);
           break;
           
         case DialogPopupCheck:
           DialogPopupOption = ToggleCheckBox(dptr,DialogPopupCheck);
           break;
           
         case OKButton:
           GetDItem(dptr,StepIncrementEdit,&itemType,&itemHandle,&dispRect);
           GetIText(itemHandle,StepStr);
           break;
           
         case CancelButton:
           DialogPopupOption = oldoption1;
           ProfessorJoeOption = oldoption2;
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
  
/*******************************************************************/
/* SetFontAttributes: Resets the font attributes of a text window. */
/*******************************************************************/
SetFontAttributes(newFont,newSize)
  int newFont, newSize;
  {
   FontInfo finfo;
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
    
   /*=============================================*/
   /* Change the font attributes for the window.  */
   /*=============================================*/
   
   TextFont(newFont);
   TextSize(newSize);
   
   (**TheText).txFont = newFont;
   (**TheText).txSize = newSize;
   
   GetFontInfo(&finfo);
   (**TheText).lineHeight = finfo.ascent + finfo.descent + finfo.leading;
   (**TheText).fontAscent = finfo.ascent;
     
   /*=========================*/
   /* Unlock the edit record. */
   /*=========================*/
   
   HUnlock( (Handle) TheText);
   
   /*=====================*/
   /* Redisplay the text. */
   /*=====================*/
   
   EraseRect(&(*TheWindow).portRect);
   InvalRect(&(*TheWindow).portRect);
   TECalText(TheText);
   FixText();
  }
