/*   CLIPS Version 4.30   4/25/89 */

#include <MenuMgr.h>
#include <TextEdit.h>
#include <WindowMgr.h>

#include "interface.h"

extern MenuHandle EditMenu;

static int EditOperation = NO_EDIT;
static Handle PreviousSelection;
static Handle CopyOfScrap;
static Handle CopyOfTyping;
static long int PreviousStart;
static long int PreviousEnd;
static long int NewStart;
static long int NewEnd;
static long int UndoStart;
static long int UndoEnd;
static long int RedoStart;
static long int RedoEnd;
static WindowPtr UndoWindow;
static int TypingContinueStart = -1;
static int UndoTypingIndex;
static char *OldBackupString;
static int OldBackupLength;
static int OldBackupMax;
static char *NewBackupString;
static int NewBackupLength;
static int NewBackupMax;
static char *ForwardString;
static int ForwardLength;
static int ForwardMax;
static int LowestBackupIndex;

/*****************************************************/
/* SaveUndoInfo: Saves the information necessary for */
/*   undoing the Cut, Paste, and Clear edit command. */
/*****************************************************/
SaveUndoInfo(whichWindow,ThisTEH,EditOperation)
  WindowPtr whichWindow;
  TEHandle ThisTEH;
  int EditOperation;
  {
   long int length;
   Handle textHandle;
   Ptr textPtr;
   
   /*=============================================*/
   /* Clear the currently saved undo information. */
   /*=============================================*/
   
   if (PreviousSelection != NULL) DisposHandle(PreviousSelection); 
   if (CopyOfScrap != NULL) DisposHandle(CopyOfScrap);
   if (CopyOfTyping != NULL) DisposHandle(CopyOfTyping);
  
   /*=============================================*/
   /* Save the window for which the undo applies. */
   /*=============================================*/
   
   UndoWindow = whichWindow;
   
   /*=============================*/
   /* Copy the current selection. */
   /*=============================*/
   
   HLock(ThisTEH);
   textHandle = (**ThisTEH).hText;
   HLock(textHandle);
   textPtr = *textHandle;
   textPtr = &textPtr[(**ThisTEH).selStart];
   PtrToHand(textPtr,&PreviousSelection,
             (**ThisTEH).selEnd - (**ThisTEH).selStart);
   HUnlock(textHandle);
   HUnlock(ThisTEH);
   
   /*==================================*/
   /* Save the start and end positions */ 
   /* of the current selection.        */
   /*==================================*/
   
   PreviousStart = (**ThisTEH).selStart;
   PreviousEnd = (**ThisTEH).selEnd;
   
   /*===========================*/
   /* Make a copy of the scrap. */
   /*===========================*/
   
   CopyOfScrap = TEScrapHandle();
   HandToHand(&CopyOfScrap);
   
   /*=========================================*/
   /* Save the start and end positions of the */ 
   /* selection after the edit operation.     */
   /*=========================================*/
  
   switch(EditOperation)
     {
      case UNPASTE:
        NewStart = (**ThisTEH).selStart;
        NewEnd = NewStart + TEGetScrapLen();
        break;
      
      case UNCUT:
      case UNCLEAR:
      case UNTYPING:
        NewStart = (**ThisTEH).selStart;
        NewEnd = NewStart;
        break;
     }
     
   /*==================================*/
   /* Mark the positions for selection */
   /* after undo and redo operations.  */
   /*==================================*/
  
   UndoStart = PreviousStart;
   UndoEnd = PreviousEnd;
   RedoStart = NewStart;
   RedoEnd = NewEnd;
  }
  
/*****************/
/* DoUndo:       */
/*****************/
DoUndo(ThisText)
  TEHandle ThisText;
  {
   switch(EditOperation)
     {
      case NO_EDIT:
        break;
        
      case UNPASTE:
        UndoEdit(ThisText,UNPASTE,REPASTE);
        break;
        
      case REPASTE:
        RedoEdit(ThisText,REPASTE,UNPASTE);
        break;
        
      case UNCUT:
        UndoEdit(ThisText,UNCUT,RECUT);
        break;
        
      case RECUT:
        RedoEdit(ThisText,RECUT,UNCUT);
        break;
        
      case UNCLEAR:
        UndoEdit(ThisText,UNCLEAR,RECLEAR);
        break;
        
      case RECLEAR:
        RedoEdit(ThisText,RECLEAR,UNCLEAR);
        break;
        
      case UNTYPING:
        UndoTyping(ThisText,UNTYPING,RETYPING);
        break;
        
      case RETYPING:
        RedoTyping(ThisText,UNTYPING,RETYPING);
        break;
     }
   
   /*==================================================*/
   /* Adjust scroll bar to length of text, adjust text */
   /* to match scroll bar, and keep selection visible. */                   
   /*==================================================*/
   
   AdjustScrollBar();
   AdjustText();
   ScrollToSelection(FALSE);
   
   /*====================*/
   /* Fix the Edit Menu. */
   /*====================*/
   
   FixEditMenu();
  }

/***********************************************/
/* UndoEdit:  */
/***********************************************/
UndoEdit(ThisTEH,UndoOperation,RedoOperation)
  TEHandle ThisTEH;
  int UndoOperation, RedoOperation;
  {
   RedoStart = (**ThisTEH).selStart;
   RedoEnd = (**ThisTEH).selEnd;
     
   TESetSelect(NewStart,NewEnd,ThisTEH);
 
   if (UndoOperation == UNPASTE)
     {
      TEDelete(ThisTEH);
      TESetSelect(PreviousStart,PreviousStart,ThisTEH);
     }
 
   HLock(PreviousSelection);
   TEInsert(*PreviousSelection,
            GetHandleSize(PreviousSelection),
            ThisTEH);
   HUnlock(PreviousSelection);
     
   if (UndoOperation == UNCUT)
     {
      TESetScrapLen(GetHandleSize(CopyOfScrap));
      BlockMove(*CopyOfScrap,*TEScrapHandle(),TEGetScrapLen());
     }
   
   TESetSelect(UndoStart,UndoEnd,ThisTEH);
   
   SetUndoItem(RedoOperation);
  }
  
/***********************************************/
/* RedoEdit: */
/***********************************************/
RedoEdit(ThisTEH,RedoOperation,UndoOperation)
  TEHandle ThisTEH;
  int RedoOperation, UndoOperation;
  {
   UndoStart = (**ThisTEH).selStart;
   UndoEnd = (**ThisTEH).selEnd;
  
   TESetSelect(PreviousStart,PreviousEnd,ThisTEH);
   
   switch (RedoOperation)
     {
      case RECUT:
        TECut(ThisTEH);
        break;
        
      case RECLEAR:
        TEDelete(ThisTEH);
        break;
        
      case REPASTE:
        TEDelete(ThisTEH);
        HLock(CopyOfScrap);
        TEInsert(*CopyOfScrap,
                 GetHandleSize(CopyOfScrap),
                 ThisTEH);
        HUnlock(CopyOfScrap);
        break;
     }
   
   TESetSelect(RedoStart,RedoEnd,ThisTEH);
   
   SetUndoItem(UndoOperation);
  }
  
/***********************************************/
/* FixUndoMenuItem: */
/***********************************************/
FixUndoMenuItem(whichWindow)
  WindowPtr whichWindow;
  {
   MarkUndoItem(EditOperation);
   
   if ((UndoWindow != whichWindow) || (whichWindow == NULL))
     { DisableItem(EditMenu,UndoItem); }
   else if (EditOperation == NO_EDIT)
     { DisableItem(EditMenu,UndoItem); }
   else
     { EnableItem(EditMenu,UndoItem); }
  }
  
/***********************************************/
/* SetUndoItem: */
/***********************************************/
SetUndoItem(value)
  int value;
  {
   EditOperation = value;
   MarkUndoItem(value);
  }
   
/***********************************************/
/* UndoCloseCheck: */
/***********************************************/
UndoCloseCheck(thisWindow)
  WindowPtr thisWindow;
  {
   if (thisWindow == UndoWindow)
     {
      UndoWindow = NULL;
      SetUndoItem(NO_EDIT);
     }
  }
  
/***********************************************/
/* MarkUndoItem: */
/***********************************************/
MarkUndoItem(value)
  int value;
  {
   EditOperation = value;
   
   switch (value)
     {
      case NO_EDIT:
        SetItem(EditMenu,UndoItem,"\pUndo");
        break;
        
      case UNPASTE:
        SetItem(EditMenu,UndoItem,"\pUndo Paste");
        break;
        
      case REPASTE:
        SetItem(EditMenu,UndoItem,"\pRedo Paste");
        break;
        
      case UNCUT:
        SetItem(EditMenu,UndoItem,"\pUndo Cut");
        break;
        
      case RECUT:
        SetItem(EditMenu,UndoItem,"\pRedo Cut");
        break;
        
      case UNCLEAR:
        SetItem(EditMenu,UndoItem,"\pUndo Clear");
        break;
        
      case RECLEAR:
        SetItem(EditMenu,UndoItem,"\pRedo Clear");
        break;
        
      case UNTYPING:
        SetItem(EditMenu,UndoItem,"\pUndo Typing");
        break;
        
      case RETYPING:
        SetItem(EditMenu,UndoItem,"\pRedo Typing");
        break;
     }
  }
  
/********************************************************************/
/* ProcessUndoCharacter: Keeps track of characters for undo typing. */
/********************************************************************/
ProcessUndoCharacter(ch,ThisTEH,whichWindow)
  char ch;
  WindowPtr whichWindow;
  TEHandle ThisTEH;
  {
   int selStart, selEnd;
   
   /*======================================================*/
   /* Get the current text selection start and end points. */
   /*======================================================*/
   
   selEnd = (**ThisTEH).selEnd;
   selStart = (**ThisTEH).selStart;
   
   /*==================================================*/
   /* If typing begins in a different window, there is */
   /* a selection, or typing starts off at a new point */ 
   /* in a window, then reinitialize the undo typing   */
   /* information.                                     */
   /*==================================================*/
   
   if ((whichWindow != UndoWindow) ||
       (selEnd != selStart) ||
       (selStart != TypingContinueStart))
     { InitializeUndoTypingInformation(whichWindow,ThisTEH); }
      
   /*=======================================================*/
   /* Save the information about the character being typed. */
   /* Save backspace information only if a selection is not */
   /* being deleted and the cursor is not at the beginning  */
   /* of the buffer.                                        */
   /*=======================================================*/
   
   if (ch == '\b')
     {
      if ((selStart == selEnd) && (TypingContinueStart > 0))
        { SaveUndoTypingCharacter(ch,ThisTEH); }
     }
   else
     { SaveUndoTypingCharacter(ch,ThisTEH); }
      
   /*============================================*/
   /* Fix the edit menu to indicate undo typing. */
   /*============================================*/
   
   SetUndoItem(UNTYPING);
   FixUndoMenuItem(whichWindow);
  }
  
/***********************************************/
/* SaveUndoTypingCharacter: */
/***********************************************/
SaveUndoTypingCharacter(theChar,ThisTEH)
  char theChar;
  TEHandle ThisTEH;
  {
   extern char *expand_string_with_char();
   char backupChar;
   
   if (((UndoTypingIndex > 0)  && (theChar == '\b')) ||
       ((UndoTypingIndex >= 0) && (theChar != '\b')))
     {
      ForwardString = expand_string_with_char(theChar,
                                                 ForwardString,
                                                 &ForwardLength,
                                                 &ForwardMax,
                                                 ForwardMax+80);
     }
        
   else if ((theChar == '\b') && (UndoTypingIndex <= 0))
     {
      if (UndoTypingIndex == LowestBackupIndex)
        {
         LowestBackupIndex--;
         backupChar = (*(**ThisTEH).hText)[(**ThisTEH).selStart - 1];
         OldBackupString = expand_string_with_char(backupChar,
                                                 OldBackupString,
                                                 &OldBackupLength,
                                                 &OldBackupMax,
                                                 OldBackupMax+80);
        }
            
      NewBackupString = expand_string_with_char(theChar,
                                                 NewBackupString,
                                                 &NewBackupLength,
                                                 &NewBackupMax,
                                                 NewBackupMax+80);
     }
     
   else if ((theChar != '\b') && (UndoTypingIndex < 0))
     {
      NewBackupString = expand_string_with_char(theChar,
                                                NewBackupString,
                                                &NewBackupLength,
                                                &NewBackupMax,
                                                NewBackupMax+80);
     }
     
   if (theChar == '\b')
     {
      UndoTypingIndex--;
      TypingContinueStart--;
     }
   else
     {
      UndoTypingIndex++;
      TypingContinueStart++;
     }
  }
  
/***********************************************/
/* UndoTyping:  */
/***********************************************/
UndoTyping(ThisTEH,UndoOperation,RedoOperation)
  TEHandle ThisTEH;
  int UndoOperation, RedoOperation;
  {
   int deleteStart, deleteEnd;
   
   /*===================================================*/
   /* Remember selection range for redo typing command. */
   /*===================================================*/
   
   RedoStart = (**ThisTEH).selStart;
   RedoEnd = (**ThisTEH).selEnd;
   
   /*==============================*/
   /* Delete any newly typed text. */
   /*==============================*/
   
   if (UndoTypingIndex > 0)
     {
      deleteEnd = TypingContinueStart;
      deleteStart = PreviousStart - OldBackupLength;
     }
   else
     {
      deleteStart = PreviousStart - OldBackupLength;
      deleteEnd = PreviousStart - OldBackupLength + NewBackupLength;
     }
     
   TESetSelect(deleteStart,deleteEnd,ThisTEH);    
   TEDelete(ThisTEH);
     
   /*===========================*/
   /* Restore backed over text. */
   /*===========================*/
   
   if (OldBackupLength > 0) 
     {
      TESetSelect(PreviousStart - OldBackupLength,
                  PreviousStart - OldBackupLength,
                  ThisTEH);
      ReverseString(OldBackupString);
      TEInsert(OldBackupString,OldBackupLength,ThisTEH);
      ReverseString(OldBackupString);
     }
      
   /*=================================*/
   /* Restore the previous selection. */
   /*=================================*/
   
   HLock(PreviousSelection);
   TEInsert(*PreviousSelection,
            GetHandleSize(PreviousSelection),
            ThisTEH);
   HUnlock(PreviousSelection);
   
   TESetSelect(UndoStart,UndoEnd,ThisTEH);
   
   /*===============================*/
   /* Set menu item to redo typing. */
   /*===============================*/
   
   SetUndoItem(RETYPING);
  }
  
/***********************************************/
/* RedoTyping:  */
/***********************************************/
RedoTyping(ThisTEH,UndoOperation,RedoOperation)
  TEHandle ThisTEH;
  int UndoOperation, RedoOperation;
  {
   /*===================================================*/
   /* Remember selection range for undo typing command. */
   /*===================================================*/
   
   UndoStart = (**ThisTEH).selStart;
   UndoEnd = (**ThisTEH).selEnd;
   
   /*========================================*/
   /* Delete the old text that was replaced. */
   /*========================================*/
   
   TESetSelect(PreviousStart - OldBackupLength,PreviousEnd,ThisTEH);
   TEDelete(ThisTEH);
               
   /*===================================*/
   /* Put the replacement text back in. */
   /*===================================*/
   
   TEInsert(NewBackupString,NewBackupLength,ThisTEH);
   TEInsert(ForwardString,ForwardLength,ThisTEH);
   
   /*============================*/
   /* Reset the selection range. */
   /*============================*/
   
   TESetSelect(RedoStart,RedoEnd,ThisTEH);
   
   /*===============================*/
   /* Set menu item to undo typing. */
   /*===============================*/
   
   SetUndoItem(UNTYPING);
  }
  
/*******************************************/
/* InitializeUndoTypingInformation:        */
/*******************************************/
InitializeUndoTypingInformation(whichWindow,ThisTEH)
  WindowPtr whichWindow;
  TEHandle ThisTEH;
  {
   if (ForwardString != NULL) genfree(ForwardString,ForwardMax);
   ForwardLength = 0;
   ForwardMax = 0;
   ForwardString = NULL;
   
   if (OldBackupString != NULL) genfree(OldBackupString,OldBackupMax);
   OldBackupLength = 0;
   OldBackupMax = 0;
   OldBackupString = NULL;
   
   if (NewBackupString != NULL) genfree(NewBackupString,NewBackupMax);
   NewBackupLength = 0;
   NewBackupMax = 0;
   NewBackupString = NULL;
   
   LowestBackupIndex = 0;
   UndoTypingIndex = 0;
   TypingContinueStart = (**ThisTEH).selStart; 
      
   SaveUndoInfo(whichWindow,ThisTEH,UNTYPING);
  }

/*******************************************/
/* ReverseString:                          */
/*******************************************/
ReverseString(strPtr)
  char *strPtr;
  {
   int i, length;
   char temp;
   
   length = strlen(strPtr);
   
   for (i = 0 ; i < length / 2 ; i ++)
     {
      temp = strPtr[i];
      strPtr[i] = strPtr[length - 1 - i];
      strPtr[length - 1 - i] = temp;
     }
  }