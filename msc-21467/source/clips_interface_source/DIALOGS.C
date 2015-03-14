/*   CLIPS Version 4.30   4/25/89 */

#include "network.h"
#include "rule.h"
#include "deffacts.h"
#include "deftempl.h"
#include "engine.h"

#include "interface.h"

#include <DialogMgr.h>
#include <EventMgr.h>
#include <ListMgr.h>
#include <MenuMgr.h>
#include <FontMgr.h>

static ListHandle choices;
static int select_item;

extern MenuHandle BrowseMenu;
extern WindowPtr AgendaWindow;

pascal Boolean single_select_filter();
pascal Boolean single_select_clikloop();

#define RULES_DIALOG_RESOURCE_NUMBER 258
#define RULES_DIALOG_SELECT 1
#define RULES_DIALOG_CANCEL 2
#define RULES_DIALOG_REMOVE 3
#define RULES_DIALOG_PRINT 4
#define RULES_DIALOG_MATCHES 5
#define RULES_DIALOG_BREAK 7

#define DEFFACTS_DIALOG_RESOURCE_NUMBER 259
#define DEFFACTS_DIALOG_SELECT 1
#define DEFFACTS_DIALOG_CANCEL 2
#define DEFFACTS_DIALOG_REMOVE 3
#define DEFFACTS_DIALOG_PRINT 4

#define DEFTEMPLATE_DIALOG_RESOURCE_NUMBER 280
#define DEFTEMPLATE_DIALOG_SELECT 1
#define DEFTEMPLATE_DIALOG_CANCEL 2
#define DEFTEMPLATE_DIALOG_REMOVE 3
#define DEFTEMPLATE_DIALOG_PRINT 4

#define AGENDA_DIALOG_RESOURCE_NUMBER 290
#define AGENDA_DIALOG_SELECT 1
#define AGENDA_DIALOG_CANCEL 2
#define AGENDA_DIALOG_REMOVE 3
#define AGENDA_DIALOG_FIRE 4
  
extern struct ruleinfo *get_rule_num();
struct dfact *get_deffact_num();
struct dtmpl *get_deftemplate_num();
struct activation *get_next_activation();
char *GetActivationString();

static DialogPtr ManagerDialog;

static GrafPtr DialogManagerSavePort;

static char buffer[800];

/*******************************************************/
/* UpdateManagerMenu: Updates the Rule Manager... and  */
/*   Deffacts Manager... commands in the Browse menu.  */
/*******************************************************/
UpdateManagerMenu()
  {
   /*=========================================================*/
   /* If there are no rules, then disable the Rule Manager... */
   /* command. Otherwise, enable the Rule Manager... command. */
   /*=========================================================*/
   
   if (get_next_rule(NULL) == NULL)
     { DisableItem(BrowseMenu, RuleManagerItem); }
   else
     { EnableItem(BrowseMenu, RuleManagerItem); }
       
   /*================================================================*/
   /* If there are no deffacts, then disable the Deffacts Manager... */
   /* command. Otherwise, enable the Deffacts Manager... command.    */
   /*================================================================*/
   
   if (get_next_deffact(NULL) == NULL)
     { DisableItem(BrowseMenu, DeffactsManagerItem); }
   else
     { EnableItem(BrowseMenu, DeffactsManagerItem); }  
     
   /*================================================*/
   /* If there are no defrelations, then disable the */
   /* Defrelations Manager... command. Otherwise,    */
   /*  enable the Defrelations Manager... command.   */
   /*================================================*/

   if (get_next_deftemplate(NULL) == NULL)
     { DisableItem(BrowseMenu, DeftemplateManagerItem); }
   else
     { EnableItem(BrowseMenu, DeftemplateManagerItem); }
   
   /*===============================================*/
   /* If there are no activations, then disable the */
   /* Agenda Manager... command. Otherwise, enable  */
   /* the Agenda Manager... command.                */
   /*===============================================*/
   
   if (get_next_activation(NULL) == NULL)
     { DisableItem(BrowseMenu, AgendaManagerItem); }
   else
     { EnableItem(BrowseMenu, AgendaManagerItem); }
  }
  
  
/**************************************************/
/* DoRuleManager: Handle Rule Manager... command. */
/**************************************************/
DoRuleManager()
  {
   DialogRecord dRecord;
   DialogPtr dptr;
   int item_num = 0;
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   GrafPtr savePort;
   int selection;
   int numberOfRules;
   char *command;
   char *ruleStr;
     
   CreateDialogManager(RULES_DIALOG_RESOURCE_NUMBER);
   numberOfRules = PrepManager(RULES_DIALOG_SELECT,get_next_rule,get_rule_name);
      
   /*===============================================================*/
   /* Define a ClikLoop function to be called repeatedly as long as */
   /* the mouse button is held down inside the rule list. This loop */
   /* will update the break button for the currently selected rule. */
   /*===============================================================*/
   
   (*choices)->lClikLoop = (Ptr) single_select_clikloop;
   
   /*===========================================================*/
   /* Set a global variable indicating which item in the dialog */
   /* box is the list. This global value will be used by the    */
   /* Click Loop routine for the dialog box.                    */
   /*===========================================================*/
    
   select_item = RULES_DIALOG_SELECT;
   
   /*===================================================*/
   /* Initialize the last dialog item selected variable */
   /* and the current rule selection variable.          */
   /*===================================================*/
       
   item_num = RULES_DIALOG_SELECT;
   selection = get_single_selection(choices);
   
   /*=====================================================================*/
   /* Update the set-break/remove-break button for the current selection. */
   /*=====================================================================*/
   
   update_break_button(ManagerDialog,selection);
   
   /*====================================================================*/
   /* Remain in this loop until the user has selected an operation on a  */
   /* rule or the cancel button. This loop lasts as long as the user     */
   /* continues to select a rule from the list. When any button is hit,  */
   /* the loop will be exited. The function single_select_filter that is */
   /* passed as an argument is used to handle mouse-clicks in the list.  */
   /*====================================================================*/
   
   while (item_num == RULES_DIALOG_SELECT)
     { ModalDialog(single_select_filter,&item_num); }
     
   /*====================================*/
   /* Determine which rule was selected. */
   /*====================================*/
   
   selection = get_single_selection(choices);
  
   /*=========================================*/
   /* Get the command to perform on the rule. */
   /*=========================================*/
   
   switch(item_num)
     {
      case RULES_DIALOG_CANCEL:
        command = NULL;
        break;
        
      case RULES_DIALOG_MATCHES:
        command = "(matches ";
        break;
           
      case RULES_DIALOG_PRINT:
        command = "(pprule ";
        break;
      
      case RULES_DIALOG_REMOVE:
        command = "(excise ";
        break;
      
      case RULES_DIALOG_BREAK:
        if (rule_has_break(get_rule_name(get_rule_num(selection + 1))))
         { command = "(remove-break "; }
        else
         { command = "(set-break "; } 
        break;
         
      default:
        command = NULL;
        break;
     }
  
   /*======================*/
   /* Perform the command. */
   /*======================*/
   
   if ((numberOfRules > 0) && (command != NULL))
     {
      cl_print("stdout",command);
      set_command_string(command);
      ruleStr = get_rule_name(get_rule_num(selection + 1));
      cl_print("stdout",ruleStr);
      append_command_string(ruleStr);
      cl_print("stdout",")\n");
      append_command_string(")\n");
     }
  
   /*====================*/
   /* Remove the dialog. */
   /*====================*/
   
   CleanUpAfterManagerDialog();
  }
  
/******************************************************************/
/* CreateDialogManager: Saves the current port, creates a manager */
/*   dialog, then sets the current port to that dialog.           */
/******************************************************************/
CreateDialogManager(dialogID)
  int dialogID;
  {
   GetPort(&DialogManagerSavePort);
   ManagerDialog = GetNewDialog(dialogID,NULL,-1L);
   SetPort(ManagerDialog);
  }
   
/************************************************/ 
/* PrepManager:                                 */
/************************************************/   
PrepManager(selectID,returnItemFunction,returnPprintFunction)
  int selectID;
  void *(*returnItemFunction)();
  char *(*returnPprintFunction)();
  {
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   
   int numberOfItems = 0;
   void *itemPtr;
   char *itemStr;
   
   Point the_cell, cell_size;
   Rect hdata_rect;
   int i;
   
   
   /*=================================================================*/
   /* Get the information about the dialog item where the list of     */ 
   /* rules will be displayed. Inset the display rectangle associated */
   /* with the item by one so that the list of rules will be within   */
   /* the rectangle. Reduce the right border by the additional amount */
   /* that the scroll bar will take.                                  */
   /*=================================================================*/
   
   GetDItem(ManagerDialog,selectID,&itemType,&itemHandle,&dispRect);
   InsetRect(&dispRect,1,1);
   dispRect.right -= (SBarWidth - 1);
   
   /*==================================================================*/
   /* Determine the number of items to be displayed in the dialog box. */
   /*==================================================================*/
   
   numberOfItems = 0;
   itemPtr = NULL;
   while ((itemPtr = (*returnItemFunction)(itemPtr)) != NULL)
     { numberOfItems++; }
   
   /*===============================================================*/
   /* Create a rectangle for the LNew function which indicates that */
   /* the list is to have one column with a number of rows equal to */
   /* the number of items to be displayed.                          */
   /*===============================================================*/
   
   SetRect(&hdata_rect,0,0,1,numberOfItems);
   
   /*======================================================*/
   /* Create a point for the LNew function which indicates */
   /* the height and width of a cell in the list.          */
   /*======================================================*/
   
   SetPt(&cell_size,(dispRect.right - dispRect.left),16);
   
   /*========================================================*/
   /* Call the LNew function to create a list for displaying */
   /* the rules. The list will have a vertical scroll bar.   */
   /*========================================================*/
   
   choices = LNew(&dispRect,&hdata_rect,cell_size,0,ManagerDialog,
                  FALSE,FALSE,FALSE,TRUE);
                  
   /*===================================================*/
   /* Only one selection from the list will be allowed. */
   /*===================================================*/
   
   (*choices)->selFlags = lOnlyOne;
   
   /*========================================================*/
   /* Store the names of the items in the cells of the list. */
   /*========================================================*/

   i = 0;
   itemPtr = NULL;
   while ((itemPtr = (*returnItemFunction)(itemPtr)) != NULL)
     {
      SetPt(&the_cell,0,i);
      itemStr = (*returnPprintFunction)(itemPtr);
      LSetCell(itemStr,strlen(itemStr),the_cell,choices);
      i++;
     }
     
   /*=========================================*/
   /* Select the first cell in the item list. */
   /*=========================================*/
   
   SetPt(&the_cell,0,0);
   LSetSelect((Boolean)TRUE,the_cell,choices);
  
   /*==================================*/
   /* Enable drawing of the item list. */
   /*==================================*/
   
   LDoDraw((Boolean)TRUE,choices);
   
   /*=========================*/
   /* Display the dialog box. */
   /*=========================*/
   
   ShowWindow(ManagerDialog);
   
   /*========================================*/
   /* Draw a rectangle around the rule list. */
   /*========================================*/
   
   InsetRect(&dispRect,-1,-1);
   FrameRect(&dispRect);
   
   /*====================================*/
   /* Force an update of the dialog box. */
   /*====================================*/
   
   LUpdate(ManagerDialog->visRgn,choices);
   
   return(numberOfItems);
  }
  
/******************************************************************/
/* CleanUpAfterManagerDialog: Disposes of the item selection list */
/*   and the Manager dialog and then sets the port to the active  */
/*   port before the dialog was created.                          */
/******************************************************************/
CleanUpAfterManagerDialog()
  {
   LDispose(choices);
   DisposDialog(ManagerDialog);
   SetPort(DialogManagerSavePort);
  }
  
/*************************************************/
/* UPDATE_BREAK_BUTTON: Updates the break button */
/* in the Rule Manager dialog box.               */
/*************************************************/
update_break_button(dptr,selection)
   DialogPtr dptr;
   int selection;
   {
   Handle itemHandle;
   Rect dispRect;
   int itemType;
   Str255 newTitle, oldTitle;
    
   /*=====================================================*/
   /* Get information about the break button dialog item. */
   /*=====================================================*/
   
   GetDItem(dptr,RULES_DIALOG_BREAK,&itemType,&itemHandle,&dispRect);
   
   /*=========================================================================*/
   /* Get the current title of the button - Either Set Break or Remove Break. */
   /*=========================================================================*/
   
   GetCTitle(itemHandle,oldTitle);
   
   /*=============================================================*/
   /* If the current rule selection has a breakpoint set and the  */
   /* button title is not "Remove Break", then make the button    */
   /* title "Remove Break". Otherwise if the current rule         */
   /* selection does not have a breakpoint and the  button title  */
   /* is not "Set Break", then make the button title "Set Break". */
   /*=============================================================*/
   
   if (rule_has_break(get_rule_name(get_rule_num(selection + 1))))
     {  
      if (EqualString(oldTitle,"\pRemove\rBreak",TRUE,TRUE) == FALSE)
        { SetCTitle(itemHandle,"\pRemove\rBreak"); }
     }
   else
     { 
      if (EqualString(oldTitle,"\pSet\rBreak",TRUE,TRUE) == FALSE)
        { SetCTitle(itemHandle,"\pSet\rBreak"); }
     }

   }
   
/**********************************************************/
/* DoDeffactsManager: Handle Deffacts Manager... command. */
/**********************************************************/
DoDeffactsManager()
  {
   int item_num = 0;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   int selection;
   
   int numberOfDeffacts;
   char *command;
   char *deffactStr;
   
   CreateDialogManager(DEFFACTS_DIALOG_RESOURCE_NUMBER);
   numberOfDeffacts = PrepManager(DEFFACTS_DIALOG_SELECT,
                                  get_next_deffact,
                                  get_deffact_name);
   
   /*===========================================================*/
   /* Set a global variable indicating which item in the dialog */
   /* box is the list. This global value will be used by the    */
   /* filter function for the dialog box.                       */
   /*===========================================================*/
   
   select_item = DEFFACTS_DIALOG_SELECT;
   
   /*====================================================*/
   /* Initialize the last dialog item selected variable. */
   /*====================================================*/
   
   item_num = DEFFACTS_DIALOG_SELECT;
   
   /*===============================================================*/
   /* Remain in this loop until the user has selected an operation  */
   /* on a deffacts or the cancel button. This loop lasts as long   */
   /* as the user continues to select a deffacts from the list.     */
   /* When any button is hit, the loop will be exited. The function */
   /* single_select_filter that is passed as an argument is used to */
   /* handle mouse-clicks in the list.                              */
   /*===============================================================*/
   
   while (item_num == DEFFACTS_DIALOG_SELECT)
     { ModalDialog(single_select_filter,&item_num); }
     
   /*========================================*/
   /* Determine which deffacts was selected. */
   /*========================================*/
   
   selection = get_single_selection(choices);
  
   /*====================================================*/
   /* Perform the appropriate operation on the deffacts. */
   /*====================================================*/
   
   switch(item_num)
     {
      case DEFFACTS_DIALOG_CANCEL:
        command = NULL;
        break; 
        
      case DEFFACTS_DIALOG_PRINT:
        command = "(ppdeffact ";
        break;
        
      case DEFFACTS_DIALOG_REMOVE:
        command = "(undeffacts ";
        break;
        
      default:
        command = NULL;
        break;
     }
     
   /*======================*/
   /* Perform the command. */
   /*======================*/
   
   if ((numberOfDeffacts > 0) && (command != NULL))
     {
      cl_print("stdout",command);
      set_command_string(command);
      deffactStr = get_deffact_name(get_deffact_num(selection + 1));
      cl_print("stdout",deffactStr);
      append_command_string(deffactStr);
      cl_print("stdout",")\n");
      append_command_string(")\n");
     }
  
   CleanUpAfterManagerDialog();
  }
  
/****************************************************************/
/* DoDeftemplateManager: Handle Deftemplate Manager... command. */
/****************************************************************/
DoDeftemplateManager()
  {
   int item_num = 0;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   int selection;
   
   int numberOfDeftemplates;
   char *command;
   char *deftemplateStr;
   
   CreateDialogManager(DEFTEMPLATE_DIALOG_RESOURCE_NUMBER);
   numberOfDeftemplates = PrepManager(DEFTEMPLATE_DIALOG_SELECT,
                                  get_next_deftemplate,
                                  get_deftemplate_name);
   
   /*===========================================================*/
   /* Set a global variable indicating which item in the dialog */
   /* box is the list. This global value will be used by the    */
   /* filter function for the dialog box.                       */
   /*===========================================================*/
   
   select_item = DEFTEMPLATE_DIALOG_SELECT;
   
   /*====================================================*/
   /* Initialize the last dialog item selected variable. */
   /*====================================================*/
   
   item_num = DEFTEMPLATE_DIALOG_SELECT;
   
   /*===============================================================*/
   /* Remain in this loop until the user has selected an operation  */
   /* on a deffacts or the cancel button. This loop lasts as long   */
   /* as the user continues to select a deffacts from the list.     */
   /* When any button is hit, the loop will be exited. The function */
   /* single_select_filter that is passed as an argument is used to */
   /* handle mouse-clicks in the list.                              */
   /*===============================================================*/
   
   while (item_num == DEFTEMPLATE_DIALOG_SELECT)
     { ModalDialog(single_select_filter,&item_num); }
     
   /*========================================*/
   /* Determine which deffacts was selected. */
   /*========================================*/
   
   selection = get_single_selection(choices);
  
   /*====================================================*/
   /* Perform the appropriate operation on the deffacts. */
   /*====================================================*/
   
   switch(item_num)
     {
      case DEFTEMPLATE_DIALOG_CANCEL:
        command = NULL;
        break; 
        
      case DEFTEMPLATE_DIALOG_PRINT:
        command = "(ppdeftemplate ";
        break;
        
      case DEFTEMPLATE_DIALOG_REMOVE:
        command = "(undeftemplate ";
        break;
        
      default:
        command = NULL;
        break;
     }
     
   /*======================*/
   /* Perform the command. */
   /*======================*/
   
   if ((numberOfDeftemplates > 0) && (command != NULL))
     {
      cl_print("stdout",command);
      set_command_string(command);
      deftemplateStr = get_deftemplate_name(get_deftemplate_num(selection + 1));
      cl_print("stdout",deftemplateStr);
      append_command_string(deftemplateStr);
      cl_print("stdout",")\n");
      append_command_string(")\n");
     }
  
   CleanUpAfterManagerDialog();
  }
  
/******************************************************/
/* DoAgendaManager: Handle Agenda Manager... command. */
/******************************************************/
DoAgendaManager()
  {
   int item_num = 0;
   
   int itemType;
   Handle itemHandle;
   Rect dispRect;
   int selection;
   
   int numberOfDeftemplates;
   char *command;
   char numberBuff[20];
   
   CreateDialogManager(AGENDA_DIALOG_RESOURCE_NUMBER);
   
   
   numberOfDeftemplates = PrepManager(AGENDA_DIALOG_SELECT,
                                  get_next_activation,
                                  GetActivationString);
                                     
   /*===========================================================*/
   /* Set a global variable indicating which item in the dialog */
   /* box is the list. This global value will be used by the    */
   /* filter function for the dialog box.                       */
   /*===========================================================*/
   
   select_item = AGENDA_DIALOG_SELECT;
   
   /*====================================================*/
   /* Initialize the last dialog item selected variable. */
   /*====================================================*/
   
   item_num = AGENDA_DIALOG_SELECT;
   
   /*===============================================================*/
   /* Remain in this loop until the user has selected an operation  */
   /* on a deffacts or the cancel button. This loop lasts as long   */
   /* as the user continues to select a deffacts from the list.     */
   /* When any button is hit, the loop will be exited. The function */
   /* single_select_filter that is passed as an argument is used to */
   /* handle mouse-clicks in the list.                              */
   /*===============================================================*/
   
   while (item_num == AGENDA_DIALOG_SELECT)
     { ModalDialog(single_select_filter,&item_num); }
     
   /*========================================*/
   /* Determine which deffacts was selected. */
   /*========================================*/
   
   selection = get_single_selection(choices);
  
   /*====================================================*/
   /* Perform the appropriate operation on the deffacts. */
   /*====================================================*/
   
   switch(item_num)
     {
      case AGENDA_DIALOG_CANCEL:
        break; 
        
      case AGENDA_DIALOG_FIRE:
         move_activation_to_top(selection + 1);
         if (AgendaWindow != NULL) 
           {
            complete_agenda_refresh();
            UpdateAgendaWindow();
           }
         set_change_agenda(FALSE);
         cl_print("stdout","(run 1)\n");
         set_command_string("(run 1)\n");
        break;
        
      case AGENDA_DIALOG_REMOVE:
        delete_activation(selection + 1);
        break;
        
      default:
        command = NULL;
        break;
     }
     
   /*======================*/
   /* Perform the command. */
   /*======================*/
  
   CleanUpAfterManagerDialog();
  }
  
/*************************************************************/
/* single_select_filter: Dialog filter function for handling */
/*   mouse clicks inside of a list area in a dialog box.     */
/*************************************************************/
pascal Boolean single_select_filter(theDialog,theEvent,itemNumber)
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
   
   GetDItem(theDialog,select_item,&itemType,&itemHandle,&dispRect);
   
   /*=======================================================*/
   /* If the mouse-down event occured inside the list area, */
   /* then call LClick to handle the event. Set the value   */
   /* of the item hit to the list area and indicate that    */
   /* the event has been handle by returning TRUE.          */
   /*=======================================================*/
   
   if (PtInRect(mouse_loc,&dispRect)) 
     {
      LClick(mouse_loc,theEvent->modifiers,choices);
      *itemNumber = select_item;
      return (TRUE);
     }
   
   /*=========================================*/
   /* Allow the event to be handled normally. */
   /*=========================================*/
   
   return (FALSE); 
  }
  
/*****************************************************************/
/* single_select_clickloop: Updates the break button in the Rule */
/*   Manager dialog box as the rule selection is being made.     */
/*****************************************************************/
pascal Boolean single_select_clikloop()
  {
   int selection;
   
   selection = get_single_selection(choices);
   update_break_button(ManagerDialog,selection);
   
   return (TRUE); 
  }
  
/********************************************************************/
/* GET_SINGLE_SELECTION: Returns the current selection from a list. */
/********************************************************************/
get_single_selection(choices_ptr)
  ListHandle choices_ptr;
  {
   Cell the_cell;
   
   SetPt(&the_cell,0,0);
   LGetSelect(TRUE,&the_cell,choices_ptr);
   return(the_cell.v);
  }
  
/*****************************************************/
/* GET_DEFFACT_NUM: Given N, returns the Nth deffact */
/*   in the deffacts list.                           */  
/*****************************************************/
struct dfact *get_deffact_num(count)
  int count;
  {
   struct dfact *deffact_ptr = NULL;
   int i = 1;
   
   deffact_ptr = get_next_deffact(NULL);
   while ((deffact_ptr != NULL) && (i < count))
     { 
      deffact_ptr = deffact_ptr->next; 
      i++;
     }
   return(deffact_ptr);
  }
  
/*************************************************/
/* GET_DEFTEMPLATE_NUM: Given N, returns the Nth */
/*   deftemplate in the deftemplate list.        */  
/************************************************/
struct dtmpl *get_deftemplate_num(count)
  int count;
  {
   struct dtmpl *deftemplate_ptr = NULL;
   int i = 1;
   
   deftemplate_ptr = get_next_deftemplate(NULL);
   while ((deftemplate_ptr != NULL) && (i < count))
     { 
      deftemplate_ptr = deftemplate_ptr->next; 
      i++;
     }
   return(deftemplate_ptr);
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
  
/***************************************************/
/* GetActivationString:                         */
/***************************************************/
char *GetActivationString(act_ptr)
  struct activation *act_ptr;
  {
   struct fbind *list;
   
   sprintf(buffer,"%-6d %s: ",act_ptr->salience,act_ptr->rule);
   
   list = act_ptr->basis->binds;
   while (list != NULL)
     {
      if (list->whoset >= 0)
        { sprintf(&buffer[strlen(buffer)],"f-%ld",list->whoset); }
        
      if (list->next != NULL)
        { sprintf(&buffer[strlen(buffer)],",",list->whoset); }
        
      list = list->next;
     }
   return(buffer);
  }

 