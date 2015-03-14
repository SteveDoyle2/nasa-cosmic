/*   CLIPS Version 4.30   4/25/89 */

#include <FileMgr.h> 
#include <stdfilepkg.h>
#include <DeskMgr.h>
#include <TextEdit.h>
#include <MenuMgr.h>
#include <WindowMgr.h>
#include <DialogMgr.h>

#include "clipsmem.h"
#include "interface.h"

#define TransferDialogID 400

extern TEHandle TheText;

extern WindowPtr DisplayWindow;
extern WindowPtr AgendaWindow;
extern WindowPtr FactsWindow;

extern int ProfessorJoeOption;
extern Str255 StepStr;

extern MenuHandle EditMenu;
extern MenuHandle FileMenu;
extern MenuHandle ExecutionMenu;
extern MenuHandle WatchMenu;
extern MenuHandle BrowseMenu;

extern CursHandle Watch;

extern int Quitting;

int TransferVrefnum;
int Transferring;
Str255 TransferToName;

/*************************************************************/
/* DoLoadRules: Performs load command found under file menu. */
/*   Uses standard file dialog box to load a group of rules  */
/*   from a file.                                            */
/*************************************************************/
DoLoadRules()
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
    
   the_type_list[0] = 'TEXT';   
   
   /*====================*/
   /* Get the file name. */
   /*====================*/
   
   SFGetFile(dlg_origin,NULL,NULL,1,the_type_list,NULL,&the_reply);
   
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (the_reply.good == false) return;
   
   /*=============================================*/
   /* Reset the current volume so that UNIX fopen */
   /* function will find the file.                */
   /*=============================================*/
       
   SetVol(NULL,the_reply.vRefNum); 
   
   /*=======================================*/
   /* Construct the load command string and */
   /* send it to the CLIPS command loop.    */     
   /*=======================================*/
   
   set_command_string("(load \"");
   PtoCstr(the_reply.fName);
   append_command_string(the_reply.fName);
   CtoPstr(the_reply.fName);
   append_command_string("\")\n");
   cl_print("stdout",get_command_string());
   
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
  }
  
/**************************************************************/
/* DoLoadBatch: Performs batch command found under file menu. */
/*   Uses standard file dialog box to get the batch file to   */
/*   be processed.                                            */
/**************************************************************/
DoLoadBatch()
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
    
   the_type_list[0] = 'TEXT';   
   
   /*====================*/
   /* Get the file name. */
   /*====================*/
   
   SFGetFile(dlg_origin,NULL,NULL,1,the_type_list,NULL,&the_reply);
   
   /*=====================================*/
   /* Exit if file request was cancelled. */
   /*=====================================*/
   
   if (the_reply.good == false) return;
   
   /*=============================================*/
   /* Reset the current volume so that UNIX fopen */
   /* function will find the file.                */
   /*=============================================*/
       
   SetVol(NULL,the_reply.vRefNum); 
   
   /*========================================*/
   /* Construct the batch command string and */
   /* send it to the CLIPS command loop.     */     
   /*========================================*/
   
   set_command_string("(batch \"");
   PtoCstr(the_reply.fName);
   append_command_string(the_reply.fName);
   CtoPstr(the_reply.fName);
   append_command_string("\")\n");
   cl_print("stdout",get_command_string());
   
   /*===================================*/
   /* Indicate delay with watch cursor. */
   /*===================================*/
   
   SetCursor(*Watch);
  }
  
/*********************************************************/
/* DoCompileSelection: Handle Compile Selection command. */
/*********************************************************/
DoCompileSelection()
  {
   Handle sel_hnd;
   char *sel_ptr, *str;
   long int sel_length, offset, i;
   extern int LOAD_FLAG;
   
   /*=======================*/
   /* Lock the edit record. */
   /*=======================*/
   
   HLock( (Handle) TheText);

   /*===============================================*/
   /* Get the length of the selection to be loaded. */
   /*===============================================*/
   
   sel_length = (**TheText).selEnd - (**TheText).selStart;
   
   /*==============================================*/
   /* Selected text should not be larger than 32K. */
   /*==============================================*/
   
   if (sel_length != (int) sel_length)
     {
      SysBeep(10);
      HUnlock( (Handle) TheText);
      return;
     }
   
   /*========================================*/
   /* If no text is selected, simply return. */
   /*========================================*/
   
   if (sel_length <= 0) 
     {
      HUnlock( (Handle) TheText);
      return;
     }
    
   /*=====================================================*/
   /* Get the text handle of the edit record and lock it. */
   /*=====================================================*/
   
   sel_hnd = (**TheText).hText;
   HLock(sel_hnd);
   
   /*=======================================================*/
   /* Get a pointer to the beginning of the text selection. */
   /*=======================================================*/
   
   sel_ptr = *sel_hnd;
   sel_ptr = &sel_ptr[(**TheText).selStart];
   
   /*======================================================*/
   /* Allocate enough memory to copy the string. If memory */
   /* cannot be allocated, then return.                    */
   /*======================================================*/
   
   str = gm2( (int) sel_length + 1);
   if (str == NULL)
     { 
      HUnlock(sel_hnd);
      HUnlock( (Handle) TheText); 
      ParamText("\pUnable to allocate enough memory to load selection",
                   "\p","\p","\p");
      StopAlert(StopCantDoID,0L);
      return;
     }
     
   /*==========================*/
   /* Copy the selection text. */
   /*==========================*/
   
   for (i = 0 ; i < sel_length ; i++)
     { str[i] = sel_ptr[i]; }
   str[i] = '\0';
     
   /*=========================================*/
   /* Unlock text handle and the edit record. */
   /*=========================================*/
   
   HUnlock(sel_hnd);
   HUnlock( (Handle) TheText); 
   
   /*==========================================*/
   /* Flush any command in the command string. */
   /*==========================================*/
   
   flush_command_string();
   
   /*============================================*/
   /* Indicate that a selection is being loaded. */
   /*============================================*/
   
   if (select_display_window())
     {
      show_display_select();
      cl_print("stdout","Loading Selection...\n");  
     }   
     
   /*==================================================*/
   /* Open the string as a string router input source. */
   /*==================================================*/
   
   open_str_source("clipread",str,0);
   
   /*============================================*/
   /* Load the selection from the string router. */
   /*============================================*/
   
   LOAD_FLAG = TRUE;
   load_from_log_name("clipread");
   LOAD_FLAG = FALSE;
   
   /*==========================*/
   /* Close the string router. */
   /*==========================*/
   
   close_str_source("clipread");
   
   /*=====================================*/
   /* Put CLIPS prompt in display window. */
   /*=====================================*/
   
   cl_print("stdout","CLIPS> ");
      
   /*==========================================*/
   /* Return the string to the memory manager. */
   /*==========================================*/
   
   rm(str,(int) sel_length + 1);
  }
  
/***************************************************/
/* DoCompileBuffer: Handle Compile Buffer command. */
/***************************************************/
DoCompileBuffer()
  {
   int old_start, old_end;
   TEHandle tempText;
   
   /*=========================*/
   /* Save the old selection. */
   /*=========================*/
   
   old_start = (**TheText).selStart;
   old_end = (**TheText).selEnd;
  
   /*=============================*/
   /* Select the entire document. */
   /*=============================*/
   
   TESetSelect(0,(**TheText).teLength,TheText);
   
   /*============================================================*/
   /* Save the text handle because the dialog window will become */
   /* the active window when the buffer is compiled.             */
   /*============================================================*/
   
   tempText = TheText;
   
   /*=====================*/
   /* Compile the buffer. */
   /*=====================*/
   
   DoCompileSelection();
   
   /*============================*/
   /* Restore the old selection. */
   /*============================*/
   
   TESetSelect(old_start,old_end,tempText);
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
   
   if (dribble_active())
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
   FInfo theInfo;
   int theFile;
   StringHandle strHandle;
   Str255 untitled;
   int ignore;
   OSErr resultCode;
   
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
   
   /*==========================================*/
   /* Construct the dribble command string and */
   /* send it to the CLIPS command loop.       */
   /*==========================================*/
   
   set_command_string("(dribble-on \"");
   PtoCstr(theReply.fName);
   append_command_string(theReply.fName);
   CtoPstr(theReply.fName);
   append_command_string("\")\n");
   cl_print("stdout",get_command_string());
  }
  
/***********************************************/
/* DOCLOSEDRIBBLE: Handle Dribble Off command. */
/***********************************************/
DoCloseDribble() 
  {
   /*==========================================*/
   /* Construct the dribble command string and */
   /* send it to the CLIPS command loop.       */
   /*==========================================*/
   
   set_command_string("(dribble-off)\n");
   cl_print("stdout",get_command_string());
  }
  
/********************************************************************/
/* UpdateDribbleMenu: Updates the Dribble command in the file menu. */
/********************************************************************/
UpdateDribbleMenu() 
  { 
   /*==================================================*/
   /* If a dribble file is active, then the command in */
   /* the file menu should be "Dribble Off". Otherwise */
   /* the command should be "Dribble On...".           */ 
   /*==================================================*/
               
   if (dribble_active())
     { SetItem(FileMenu,OpenDribbleItem,"\pTurn Dribble Off"); }
   else
     { SetItem(FileMenu,OpenDribbleItem,"\pTurn Dribble On...");  }
  }
  
/**************************************************/
/* DoWatchChoice:  Handle choice from Watch Menu. */
/**************************************************/
int DoWatchChoice(item)
  int item;
  {
   int value;
   char *string;
        
   switch (item)  
     {
      /*==============================================*/
      /* Form command string for watch/unwatch facts. */
      /*==============================================*/
      
      case FactsItem:
        value = get_facts_watch();
        if (value == 1) string = "(unwatch facts)\n";
        else string = "(watch facts)\n";
        break;
        
      /*==============================================*/
      /* Form command string for watch/unwatch rules. */
      /*==============================================*/
      
      case RulesItem:
        value = get_rules_watch();
        if (value == 1) string = "(unwatch rules)\n";
        else string = "(watch rules)\n";
        break;
        
      /*====================================================*/
      /* Form command string for watch/unwatch activations. */
      /*====================================================*/
      
      case ActivationsItem:
        value = get_activations_watch();
        if (value == 1) string = "(unwatch activations)\n";
        else string = "(watch activations)\n";
        break;
        
      /*=====================================================*/
      /* Form command string for watch/unwatch compilations. */
      /*=====================================================*/
      
      case CompilationsItem:
        value = get_compilations_watch();
        if (value == 1) string = "(unwatch compilations)\n";
        else string = "(watch compilations)\n";
        break;
        
      /*====================================*/
      /* Form command string for watch all. */
      /*====================================*/
      
      case AllItem:
        string = "(watch all)\n";
        break;
        
      /*======================================*/
      /* Form command string for unwatch all. */
      /*======================================*/
      
      case NoneItem:
        string = "(unwatch all)\n";
        break;
        
      default:
        return;
     }
     
   /*===================================================*/
   /* Echo the command to the display window, then send */
   /* the command string to the CLIPS command loop.     */
   /*===================================================*/
     
   cl_print("stdout",string);
   set_command_string(string);
  }
  
/*******************************************************/
/* UpdateWatchMenu: Updates the Watch menu to indicate */
/*   which items are currently being watched.          */
/*******************************************************/
UpdateWatchMenu()
  {
   int fvalue, rvalue, avalue, cvalue;
   
   /*==================================*/
   /* Get the current watch values for */
   /* facts, rules, and activations.   */
   /*==================================*/
   
   fvalue = get_facts_watch();
   rvalue = get_rules_watch();
   avalue = get_activations_watch();
   cvalue = get_compilations_watch();
	 
   /*=======================================*/
   /* Change the items in the watch menu to */
   /* reflect the current watch values.     */
   /*=======================================*/
   
   CheckItem(WatchMenu,FactsItem,fvalue);
   CheckItem(WatchMenu,RulesItem,rvalue);
   CheckItem(WatchMenu,ActivationsItem,avalue);
   CheckItem(WatchMenu,CompilationsItem,cvalue);
  }
  
/*******************************************************/
/* DoBrowseChoice: Handle choice from the Browse menu. */
/*******************************************************/
int DoBrowseChoice(item)
  int item;
  {
   switch (item)  
     {
      /*================================*/
      /* Handle the Show Facts command. */
      /*================================*/
      
      case ShowFactsItem:
        cl_print("stdout","(facts)\n"); 
        set_command_string("(facts)\n");
        break;
        
      /*================================*/
      /* Handle the Show Rules command. */
      /*================================*/
      
      case ShowRulesItem:
        cl_print("stdout","(rules)\n");
        set_command_string("(rules)\n");
        break;
        
      /*=================================*/
      /* Handle the Show Agenda command. */
      /*=================================*/
      
      case ShowAgendaItem:
        cl_print("stdout","(agenda)\n"); 
        set_command_string("(agenda)\n");
        break;
        
      /*===================================*/
      /* Handle the Show Deffacts command. */
      /*===================================*/
      
      case ShowDeffactsItem:
        cl_print("stdout","(list-deffacts)\n");
        set_command_string("(list-deffacts)\n");
        break;  
         
      /*=======================================*/
      /* Handle the Show Deftemplates command. */
      /*=======================================*/
      
      case ShowDeftemplatesItem:
        cl_print("stdout","(list-deftemplates)\n");
        set_command_string("(list-deftemplates)\n");
        break;
        
      /*=====================================*/
      /* Handle the Rule Manager... command. */
      /*=====================================*/
      
      case RuleManagerItem:
        DoRuleManager();
        break;
        
      /*=========================================*/
      /* Handle the Deffacts Manager... command. */
      /*=========================================*/
      
      case DeffactsManagerItem:
        DoDeffactsManager();
        break;
      
      /*============================================*/
      /* Handle the Deftemplate Manager... command. */
      /*============================================*/
      
      case DeftemplateManagerItem:
        DoDeftemplateManager();
        break;
        
      /*=======================================*/
      /* Handle the Agenda Manager... command. */
      /*=======================================*/
      
      case AgendaManagerItem:
        DoAgendaManager();
        break;
        
      /*======================================================*/
      /* Handle the Facts Window command. If the facts window */
      /* is inactive, then activate it. If the facts window   */
      /* is active, then deactivate it.                       */
      /*======================================================*/
      
      case FactsWindowItem:
        if (FactsWindow == NULL) 
          {
           init_facts_window();
           CheckItem(BrowseMenu,item,TRUE);
          }
        else
          {
           select_facts_window();
		   DoClose();
		   FactsWindow = NULL;
           CheckItem(BrowseMenu,item,FALSE);
          }
        break;
        
      /*========================================================*/
      /* Handle the Agenda Window command. If the agenda window */
      /* is inactive, then activate it. If the agenda window is */
      /* active, then deactivate it.                            */
      /*========================================================*/
      
      case AgendaWindowItem:
        if (AgendaWindow == NULL) 
          {
           init_agenda_window();
           CheckItem(BrowseMenu,item,TRUE);
          }
        else
          {
           select_agenda_window();
		   DoClose();
		   AgendaWindow = NULL;
           CheckItem(BrowseMenu,item,FALSE);
          }
        break;
        
      default:
		break;
     }
  }
  
/*******************************************************/
/* DoExecution: Handle choice from the Execution menu. */
/*******************************************************/
int DoExecutionChoice(item)
  int item;
  {
	
   switch (item)  
     {
      /*===========================*/
      /* Handle the Reset command. */
      /*===========================*/
      
      case ResetItem: 
        if ((ProfessorJoeOption == TRUE) && (get_next_activation(NULL) != NULL))
          {
           if (YesNoJoe("\pThere are activations on the agenda. OK to reset the CLIPS environment?") == 0)
             { return; }
          }   
          
        SetCursor(*Watch);
        cl_print("stdout","(reset)\n");
        set_command_string("(reset)\n");
        break;
        
      /*=========================*/
      /* Handle the Run command. */
      /*=========================*/
      
      case RunItem:   
        SetCursor(*Watch);
        cl_print("stdout","(run)\n");
        set_command_string("(run)\n");
        break;
        
      /*==========================*/
      /* Handle the Step command. */
      /*==========================*/
      
      case StepItem:   
        SetCursor(*Watch);
        cl_print("stdout","(run ");
        set_command_string("(run ");
        PtoCstr(StepStr);
        cl_print("stdout",StepStr);
        append_command_string(StepStr);
        CtoPstr(StepStr);
        cl_print("stdout",")\n");
        append_command_string(")\n");
        break;
        
      /*=================================*/
      /* Handle the Clear CLIPS command. */
      /*=================================*/
      
      case CLIPSClearItem:
        if (ProfessorJoeOption == TRUE)
          {   
           if (YesNoJoe("\pOK to clear the CLIPS environment?") == FALSE)
             { return; }
          }
            
        SetCursor(*Watch);
        cl_print("stdout","(clear)\n");
        set_command_string("(clear)\n");
        break;
        
      default:
		break;
     }
  }
  
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
   Transferring = true;
   Quitting = true;
  }
  
/***********************************************/
/* YesNoJoe: Performs Yes/No Joe Alert Dialog. */
/***********************************************/
YesNoJoe(s)
  Str255 s;
  {
   ParamText(s,"\p","\p","\p");
   if (Alert(JoeYesNoAlert,NULL) == 2)
     { return(1); }
   return(0);
  }
   
/****************************************/
/* OkJoe: Performs Ok Joe Alert Dialog. */
/****************************************/
OkJoe(s)
  Str255 s;
  {
   ParamText(s,"\p","\p","\p");
   return( Alert(JoeOkAlert,NULL) );
  }
  