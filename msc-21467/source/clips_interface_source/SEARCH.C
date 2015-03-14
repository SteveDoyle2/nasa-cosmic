/*   CLIPS Version 4.30   4/25/89 */

#include "interface.h"

#include <DialogMgr.h>
#include <MenuMgr.h>
#include <ControlMgr.h>
#include <EventMgr.h>


#define FindButton       1
#define DontFindButton   2
#define CancelButton     3
#define MatchWordsCheck  4
#define SearchTextEdit   7
#define ReplaceTextEdit  8
#define IgnoreCaseCheck  9

#define ReplaceID 270

#define ReturnKeyCode 0x24

static int MatchWordsValue = 0;
static int IgnoreCaseValue = 1;

static Str255 SearchText = "\p";
static Str255 ReplaceText = "\p";


extern WindowPtr TheWindow;
extern MenuHandle BufferMenu;
extern TEHandle TheText;
extern int ScrapDirty; 


long int find_str();
pascal char FindFilter();

/***************************************************/
/* DOBUFFERCHOICE: Handle choice from buffer menu. */
/***************************************************/
DoBufferChoice(theItem)
  int theItem;
  {
   switch(theItem)
     {
      /*=========================*/
      /* Handle Find... command. */
      /*=========================*/
     
      case FindItem:
        DoFind();
        break;
        
      /*============================*/
      /* Handle Find Again command. */
      /*============================*/
      
      case FindAgainItem:
        DoFindAgain();
        break;
        
      /*=========================*/
      /* Handle Replace command. */
      /*=========================*/
          
      case ReplaceItem:
        DoReplace();
        break;
        
      /*==================================*/
      /* Handle Replace and Find command. */
      /*==================================*/
          
      case ReplaceAndFindItem:
        DoReplaceAndFind();
        break;
        
      /*=============================*/
      /* Handle Replace All command. */
      /*=============================*/
          
      case ReplaceAllItem:
        DoReplaceAll();
        break;
        
      /*===================================*/
      /* Handle Compile Selection command. */
      /*===================================*/
        
      case CompileSelectionItem:
        DoCompileSelection();
        break;
        
      /*================================*/
      /* Handle Compile Buffer command. */
      /*================================*/
        
      case CompileBufferItem:
        DoCompileBuffer();
        break;
     }
     
   /*=================================*/
   /* Enable/disable edit menu items. */
   /*=================================*/
   
   FixEditMenu();
  }
  
/***********************************/
/* DOFIND: Handle Find... command. */
/***********************************/
DoFind()
  {
   DialogRecord dRecord;
   DialogPtr dptr;
   int item_num = 0;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   GrafPtr savePort;
   int done, find;
   int OldMatchValue, OldIgnoreValue;
   
   /*===========================================*/
   /* Save the old port. Create the find dialog */ 
   /* box and make it the current port.         */
   /*===========================================*/
   
   GetPort(&savePort);
   dptr = GetNewDialog(265,NULL,-1L);
   SetPort(dptr);
  
   /*=========================*/
   /* Display the dialog box. */
   /*=========================*/
   
   ShowWindow(dptr);
   
   /*=================================================*/
   /* Restore normal cursor before displaying dialog. */
   /*=================================================*/
   
   InitCursor();
   
   /*====================================================*/
   /* Set dialog values to current values and save them. */
   /*====================================================*/
   
   SetCheckBox(dptr,MatchWordsCheck,MatchWordsValue);
   SetCheckBox(dptr,IgnoreCaseCheck,IgnoreCaseValue);
   SetEditText(dptr,SearchTextEdit,ReplaceTextEdit);
   SetFindButton(dptr,FindButton,SearchTextEdit);
   OldMatchValue = MatchWordsValue;
   OldIgnoreValue = IgnoreCaseValue;
   
   /*=====================*/
   /* Conduct the dialog. */
   /*=====================*/
   
   done = 0;
   find = 0;
   while (! done)
     { 
      /*=============================================================*/
      /* Get the next dialog event. Use a filter to allow the return */
      /* key to exit the dialog and editing commands to be used.     */
      /*=============================================================*/
      
      ModalDialog(FindFilter,&item_num);
      
      /*===========================*/
      /* Process the dialog event. */
      /*===========================*/
      
      switch(item_num)
        {
         /*====================================================*/
         /* User clicked Find button. Dialog is completed. The */
         /* find command will be executed. Copy dialog search  */ 
         /* and replace strings to global variables.           */
         /*====================================================*/
         
         case FindButton:
           done = 1;
           find = 1;
           CopyEditText(dptr,SearchTextEdit,ReplaceTextEdit);
           break;
                  
         /*===========================================================*/
         /* User clicked Don't Find button. Dialog is completed. The  */ 
         /* find command will not be executed, However, dialog search */ 
         /* and replace strings will be copied to global variables.   */
         /*===========================================================*/
         
         case DontFindButton:
           done = 1;
           find = 0;
           CopyEditText(dptr,SearchTextEdit,ReplaceTextEdit);
           break;
             
         /*======================================================*/
         /* User clicked Cancel button. Dialog is completed. The */ 
         /* find command will not be executed. Restore global    */
         /* variables to values used before entering dialog.     */
         /*======================================================*/
           
         case CancelButton:
           done = 1;
           find = 0;
           MatchWordsValue = OldMatchValue;
           IgnoreCaseValue = OldIgnoreValue;
           break;
                
         /*============================================*/
         /* User clicked Match Words check box. Toggle */
         /* value and copy to global variable.         */
         /*============================================*/
         
         case MatchWordsCheck:
           MatchWordsValue = ToggleCheckBox(dptr,MatchWordsCheck);
           break;
           
         /*============================================*/
         /* User clicked Ignore Case check box. Toggle */
         /* value and copy to global variable.         */
         /*============================================*/
         
         case IgnoreCaseCheck:
           IgnoreCaseValue = ToggleCheckBox(dptr,IgnoreCaseCheck);
           break;
              
         /*===============================================*/
         /* User clicked in Search For editable text box. */
         /* Determine if find button needs to be active   */
         /* or inactive.                                  */
         /*===============================================*/
         
         case SearchTextEdit:
           SetFindButton(dptr,FindButton,SearchTextEdit);
           break;
            
         /*=================================================*/
         /* User clicked in Replace With editable text box. */
         /* Do nothing.                                     */
         /*=================================================*/
         
         case ReplaceTextEdit:
           break;   
        }
     }
     
   /*=============================================*/
   /* Remove the dialog and restore the old port. */
   /*=============================================*/
   
   DisposDialog(dptr);
   SetPort(savePort);
   
   /*=====================*/
   /* Perform the search. */
   /*=====================*/
   
   if (find == 1) 
     { 
      PerformUpdate(TheWindow);
      DoFindAgain(); 
     }
     
   /*===================================*/
   /* Enable/disable search menu items. */
   /*===================================*/
   
   FixSearchMenu();
  }
  
/*******************************************/
/* DOFINDAGAIN: Handle Find Again command. */
/*******************************************/
DoFindAgain()
  {
   long int endAt;
   Handle textHandle;
   
   /*=====================================================*/
   /* If search string is empty or no edit record exists, */
   /* then the command cannot be performed. Note that     */
   /* theText should never be NULL if this function is    */
   /* entered.                                            */
   /*=====================================================*/
   
   if (SearchText[0] == 0) return(0);
   if (TheText == NULL) return(0);
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);
   
   /*=========================================*/
   /* Get the text handle of the edit record. */
   /*=========================================*/
   
   textHandle = (**TheText).hText;
   
   /*=======================*/
   /* Lock the text handle. */
   /*=======================*/
   
   HLock(textHandle);
     
   /*=======================================================*/
   /* Search for the search string in the text beginning at */
   /* the end of the current selection or insertion point.  */
   /*=======================================================*/
   
   endAt = find_str(SearchText,(long int) (**TheText).selEnd,
             (long int) (**TheText).teLength, *textHandle,
             IgnoreCaseValue,MatchWordsValue);
             
   /*=============================================*/
   /* Unlock the text handle and the edit record. */
   /*=============================================*/
   
   HUnlock(textHandle);
   HUnlock( (Handle) TheText);
         
   /*======================================================*/
   /* If search string was not found, then beep and return */
   /* a value indicating the search was unsuccessful.      */
   /*======================================================*/
            
   if (endAt < 0) 
     {
      SysBeep(10);
      return(0);
     }
   
   /*=========================================================*/
   /* Set the current selection to the string that was found. */
   /*=========================================================*/
                  
   TESetSelect(endAt,endAt + (long int) SearchText[0],TheText);
   
   /*=====================================*/
   /* Make sure the selection is visible. */
   /*=====================================*/
   
   ScrollToSelection(TRUE);

   /*==========================================*/
   /* Adjust the text to match the scroll bar. */
   /*==========================================*/
   
   AdjustText();
   
   /*======================================================*/
   /* Return a value indicating the search was successful. */
   /*======================================================*/
   
   return(1);
  }
  
/**************************************/
/* DOREPLACE: Handle Replace command. */
/**************************************/
DoReplace()
  {
   /*======================================================*/
   /* If no edit record exists, then the command cannot be */
   /* performed. Note that theText should never be NULL if */
   /* this function is entered.                            */
   /*======================================================*/
   
   if (TheText == NULL) return;
   
   /*=====================================*/
   /* Make sure the selection is visible. */
   /*=====================================*/
   
   ScrollToSelection(TRUE);
   
   /*===============================*/
   /* Remove the current selection. */
   /*===============================*/
   
   TEDelete(TheText);
   
   /*=================================================*/
   /* If the replace string is not empty, then insert */
   /* the replace string at the insertion point.      */
   /*=================================================*/
   
   if (ReplaceText[0] != 0)
     { TEInsert(&ReplaceText[1],(long int) ReplaceText[0],TheText); }
   
   /*==================================================================*/
   /* Adjust the scroll bar to the length of the text, adjust the text */
   /* to match the scroll bar, and keep the insertion point visible.   */
   /*===================================================================*/
   
   AdjustScrollBar();
   AdjustText();
   ScrollToSelection(TRUE);
   
   /*===========================*/
   /* Mark the window as dirty. */
   /*===========================*/
   
   WindowDirty(TRUE);
  }
  
/******************************************************/
/* DOREPLACEANDFIND: Handle Replace and Find command. */
/******************************************************/
DoReplaceAndFind()
  {
   DoReplace();
   DoFindAgain();
  }
  
/*********************************************/
/* DOREPLACEALL: Handle Replace All command. */
/*********************************************/
DoReplaceAll()
  {
   long int replacements = 0;
   long int old_start, old_end;
   Str255 str_num;
   
   /*=====================================================*/
   /* If search string is empty or no edit record exists, */
   /* then the command cannot be performed. Note that     */
   /* theText should never be NULL if this function is    */
   /* entered.                                            */
   /*=====================================================*/
   
   if (SearchText[0] == 0) return(0);
   if (TheText == NULL) return(0);
   
   /*=========================================================*/
   /* Save the start and end points of the current selection. */
   /*=========================================================*/
   
   HLock( (Handle) TheText);
   old_start = (long int) (**TheText).selStart;
   old_end = (long int) (**TheText).selEnd;
   HUnlock( (Handle) TheText);
              
   /*==========================================================*/
   /* Set the insertion point before the current selection so  */
   /* that it can be replaced if it matches the search string. */
   /*==========================================================*/
   
   TESetSelect(old_start,old_start,TheText);
   
   /*==============================================*/
   /* Search for and replace all occurences of the */
   /* search string with the replace string. Keep  */
   /* track of the number of replacements made.    */
   /*==============================================*/
   
   while (DoFindAgain() == 1)
     { 
      DoReplace();
      replacements++;
     }
     
   /*==========================================================*/
   /* Restore previous selection if no replacements were made. */
   /*==========================================================*/
   
   if (replacements == 0)
     { 
      TESetSelect(old_start,old_end,TheText);
      return;
     }
     
   /*==================================================================*/
   /* Display an alert box indicating how many replacements were made. */
   /*==================================================================*/
   
   NumToString(replacements,str_num);
   ParamText(str_num,"\p","\p","\p");
   Alert(ReplaceID,NULL);
  }
 
/*************************************************************/
/* SETFINDBUTTON: Activates/Deactivates the find button of a */
/* search dialog depending upon whether the search string is */
/* empty.                                                    */
/*************************************************************/
SetFindButton(dptr,find_button_num,find_text_num)
  DialogPtr dptr;
  int find_button_num, find_text_num;
  {
   int new_value;
   Str255 find_text;
   
   ControlHandle ctrlHandle;
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*=======================================================*/
   /* Get the search string associated with the dialog box. */
   /*=======================================================*/
   
   GetDItem(dptr,find_text_num,&itemType,&itemHandle,&dispRect);
   GetIText(itemHandle,find_text);
   
   /*============================================================*/
   /* If the search string is empty, then the find button should */
   /* be inactive. Otherwise, the find button should be active.  */
   /*============================================================*/
   
   if (find_text[0] == 0) new_value = inactiveValue;
   else new_value = activeValue;
   
   /*======================================================*/
   /* Get the control handle of the find button associated */
   /* with the dialog box.                                 */
   /*======================================================*/
   
   GetDItem(dptr,find_button_num,&itemType,&itemHandle,&dispRect);
   ctrlHandle = (ControlHandle) itemHandle;
   
   /*====================================*/
   /* Active/Deactivate the find button. */
   /*====================================*/
   
   HiliteControl(ctrlHandle,new_value);
  }
  
/*****************************************************************/
/* TOGGLECHECKBOX: Toggles the value of a check box in a dialog. */
/*****************************************************************/
ToggleCheckBox(dptr,check_box_num)
  DialogPtr dptr;
  int check_box_num;
  {
   int value;
   ControlHandle ctrlHandle;
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*========================================================*/
   /* Get the control handle of the check box to be toggled. */
   /*========================================================*/
   
   GetDItem(dptr,check_box_num,&itemType,&itemHandle,&dispRect);
   ctrlHandle = (ControlHandle) itemHandle;
   
   /*=================================*/
   /* Get the value of the check box. */
   /*=================================*/
   
   value = GetCtlValue(ctrlHandle);
   
   /*====================================*/
   /* Toggle the value of the check box. */
   /*====================================*/
   SetCtlValue(ctrlHandle,1 - value);  
   
   /*===========================*/
   /* Return the toggled value. */
   /*===========================*/
   
   return(1 - value);
  } 
  
/***********************************************************/
/* SETCHECKBOX: Sets the value of a check box in a dialog. */
/***********************************************************/
SetCheckBox(dptr,check_box_num,value)
  DialogPtr dptr;
  int check_box_num, value;
  {
   ControlHandle ctrlHandle;
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*====================================================*/
   /* Get the control handle of the check box to be set. */
   /*====================================================*/
   
   GetDItem(dptr,check_box_num,&itemType,&itemHandle,&dispRect);
   ctrlHandle = (ControlHandle) itemHandle;
   
   /*========================================*/
   /* Assign the new value to the check box. */
   /*========================================*/
   
   SetCtlValue(ctrlHandle,value);  
   
   /*============================*/
   /* Return the assigned value. */
   /*============================*/
   
   return(value);
  } 
  
/***************************************************/
/* SETEDITTEXT: Sets the search and replace string */
/* values of a search dialog box.                  */
/***************************************************/
SetEditText(dptr,search_text_num,replace_text_num)
  DialogPtr dptr;
  int search_text_num, replace_text_num;
  {
   ControlHandle ctrlHandle;
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*=====================================*/
   /* Set the value of the search string. */
   /*=====================================*/
   
   GetDItem(dptr,search_text_num,&itemType,&itemHandle,&dispRect);
   SetIText(itemHandle,SearchText);
   
   /*============================================================*/
   /* Highlight the search string so that it can be changed by   */
   /* immediately entering a new value upon entering the dialog. */
   /*============================================================*/
   
   SelIText(dptr,search_text_num,0,SearchText[0]);
   
   /*======================================*/
   /* Set the value of the replace string. */
   /*======================================*/
   
   GetDItem(dptr,replace_text_num,&itemType,&itemHandle,&dispRect);
   SetIText(itemHandle,ReplaceText);
  }
  
/*******************************************************************/
/* COPYEDITTEXT: Copies the search and replace strings of a search */
/* dialog box to the global search and replace string variables.   */
/* In effect, this changes the default values for search and       */
/* and replace when a search dialog is entered.                    */
/*******************************************************************/
CopyEditText(dptr,search_text_num,replace_text_num)
  DialogPtr dptr;
  int search_text_num, replace_text_num;
  {
   ControlHandle ctrlHandle;
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   
   /*============================================================*/
   /* Copy the dialog search string to the global search string. */
   /*============================================================*/
   
   GetDItem(dptr,search_text_num,&itemType,&itemHandle,&dispRect);
   GetIText(itemHandle,SearchText);
   
   /*==============================================================*/
   /* Copy the dialog replace string to the global replace string. */
   /*==============================================================*/
   
   GetDItem(dptr,replace_text_num,&itemType,&itemHandle,&dispRect);
   GetIText(itemHandle,ReplaceText);
  }

/**************************************************/
/* FixSearchMenu: Enable/disable search commands. */
/**************************************************/ 
FixSearchMenu()
  {    
   /*===========================================================*/
   /* If the search string is not empty, then enable selections */
   /* that operate with a non-empty search string.              */
   /*===========================================================*/
   
   if (SearchText[0] != 0)
     {
      EnableItem(BufferMenu,FindAgainItem);
      EnableItem(BufferMenu,ReplaceAndFindItem);
      EnableItem(BufferMenu,ReplaceAllItem);
     }
     
   /*=======================================================*/
   /* Else disable menu items that operate with a non-empty */
   /* search string.                                        */
   /*=======================================================*/
   
   else
     {
      DisableItem(BufferMenu,FindAgainItem);
      DisableItem(BufferMenu,ReplaceAndFindItem);
      DisableItem(BufferMenu,ReplaceAllItem);
     }
   
   /*========================================================*/
   /* The Find command and the Replace command can always be */
   /* used with an active edit record.                       */
   /*========================================================*/
   
   EnableItem(BufferMenu,FindItem); 
   EnableItem(BufferMenu,ReplaceItem);
  }
  
/**********************************************************************/
/* FINDFILTER: Filter which allows the find dialog box to process the */
/* return key as a press on the find button and to use edit commands. */
/**********************************************************************/ 
pascal char FindFilter(theDialog,theEvent,itemNumber) 
  DialogPeek theDialog;
  EventRecord *theEvent;
  int *itemNumber;
  {
   int cmdDown, keyCode;
   char ch;
   int foo;
   long int temp;
   long int replacements;
   Str255 str_num;
   
   /*==================================================*/
   /* If a key wasn't pressed don't bother processing. */
   /*==================================================*/
   
   if ((*theEvent).what != keyDown) return(FALSE);
   
   /*========================================================*/
   /* The return key acts as if the find button was pressed. */
   /*========================================================*/
   
   keyCode = BitAnd((*theEvent).message,keyCodeMask);
   keyCode = BitShift((long int) keyCode,-8);
   
   if (keyCode == ReturnKeyCode) 
     {
      *itemNumber = FindButton;
      return(TRUE);
     }
     
   /*====================================================*/
   /* Allow cut, copy, and paste to work in edit fields. */
   /*====================================================*/
   
   cmdDown = (BitAnd((*theEvent).modifiers,cmdKey) != 0);
   
   if (! cmdDown) return(FALSE);
   
   *itemNumber = (*theDialog).editField + 1;
   
   ch = (char) BitAnd((*theEvent).message,charCodeMask); 
   
   switch(ch)
     {
      case 'x':
        DlgCut(theDialog);
        ScrapDirty = TRUE; 
        break;
      
      case 'c':
        DlgCopy(theDialog); 
        ScrapDirty = TRUE; 
        break;
          
      case 'v':
        DlgPaste(theDialog); 
        break;
     }
     
   return(TRUE);
  }
  
/****************************************************************/
/* FINDSTR: Searches for a string in a specified range of text. */
/****************************************************************/ 
long int find_str(compare_str,start,end,search_str,ignore_case,match_words)
  Str255 compare_str;
  long int start, end;
  char *search_str;
  int ignore_case, match_words;
  {
   long int i, k;
   int j, done;
   int search_length;
   long int new_end;
   
   /*==================================================*/
   /* Determine the length of the string being sought. */
   /* Return if the search string is empty.            */
   /*==================================================*/
   
   search_length = compare_str[0];
   if (search_length == 0) return(0);
    
   /*==========================================================*/
   /* Begin search at start of text range. End the search at a */
   /* number of characters equal to the length of the search   */
   /* string before the end of the text range.                 */
   /*==========================================================*/
   
   i = start;
   new_end = end - (search_length - 1);
   
   /*==============================================*/
   /* Search until the end of the text is reached. */
   /*==============================================*/
   
   while (i < new_end)
     {
      j = 1;
      k = i;
      done = 0;
      
      /*==================================================*/
      /* Start comparing the current position in the text */
      /* to the search string.                            */
      /*==================================================*/
      
      while (! done)
        {
         /*======================================*/
         /* If search string has matched then... */
         /*======================================*/
         
         if (j > search_length)
           { 
            /*===============================================*/
            /* If match words option is off, then return the */
            /* starting position of the found string.        */
            /*===============================================*/
            
            if (match_words == 0) return(i);
            
            /*================================================*/
            /* The match words option is on. If the character */
            /* proceeding the found string is not a blank or  */
            /* carriage return, then the search process has   */
            /* failed.                                        */
            /*================================================*/
            
            if ((i <= 0) ? FALSE : 
                  (search_str[i-1] != ' ') &&
                  (search_str[i-1] != '\r') && 
                  (search_str[i-1] != '\n'))
              { done = 1; } 
              
            /*=================================================*/
            /* If the character proceeding the found string is */
            /* not a blank or carriage return, then the search */
            /* process has failed.                             */
            /*=================================================*/
            
            else if ((k >= end) ? FALSE :
                       (search_str[k] != ' ') &&
                       (search_str[k] != '\r') &&
                       (search_str[k] != '\n'))
              { done = 1; } 
              
            /*===================================================*/
            /* Else the found string falls on word boundaries.   */
            /* Return the starting position of the found string. */
            /*===================================================*/
            
            else
              { return(i); }
           }
           
         /*========================================================*/
         /* Else if the text character and search string character */
         /* are equal then continue the search.                    */
         /*========================================================*/
         
         else if (compare_str[j] == search_str[k])
           {
            j++;
            k++;
           }
           
         /*=========================================================*/
         /* Else if the ignore case option is on, then determine if */
         /* the text character and search string character are both */
         /* alphabetic and differ only in case.                     */
         /*=========================================================*/
         
         else if (ignore_case)
           {
            char c1, c2;
            int d1, d2;
            
            d1 = -1;
            d2 = -2;
            
            c1 = compare_str[j];
            c2 = search_str[k];
            
            if ((c1 >= 'a') && (c1 <= 'z'))
              { d1 = c1 - 'a'; }
            else if ((c1 >= 'A') && (c1 <= 'Z'))
              { d1 = c1 - 'A'; }
              
            if ((c2 >= 'a') && (c2 <= 'z'))
              { d2 = c2 - 'a'; }
            else if ((c2 >= 'A') && (c2 <= 'Z'))
              { d2 = c2 - 'A'; }
             
            if (d1 != d2) 
              { done = 1; }
            else
              {
               j++;
               k++;
              }
           }
           
         /*===========================================*/
         /* Else the search process for this position */
         /* in the text has failed.                   */
         /*===========================================*/
                             
         else
           { done = 1; }
        } 
        
      /*======================================================*/
      /* Move to the next position in the text to be checked. */
      /*======================================================*/
      
      i++;
     }
     
   /*==============================*/
   /* Search string was not found. */
   /*==============================*/
     
   return(-1);
  }