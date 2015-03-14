/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*              DEFTEMPLATE COMMANDS MODULE            */
   /*******************************************************/
   
#include "setup.h"

#if DEFTEMPLATES

#include <stdio.h>

#include "deftempl.h"
#include "constant.h"
#include "lhsparse.h"
#include "scanner.h"
#include "clipsmem.h"
#include "access.h"
   
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   char                   *DRnameAndComment(); 
   int                     dtmpl_install();
   int                     dtmpl_deinstall();
   int                     rtn_slots();
   int                     rtn_items();
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   char                   *get_deftemplate_name();
   int                     UndeftemplateCommand();
   char                   *get_deftemplate_name();
   struct dtmpl           *FindDeftemplate();
   char                   *get_deftemplate_ppform();
   int                     PpdeftemplateCommand();
   int                     pp_deftemplate();
   int                     ListDeftemplatesCommand();
   int                     list_deftemplates();
   int                     PrintTemplateFact();
   int                     fact_get_end();
   int                     fact_get_var();
   int                     modify_stub();

   int                     ClearDeftemplates();
   int                     SaveDeftemplates();
   struct slot            *DefinedSlots();
   struct slot            *ParseSlot();
   struct slot            *SlotDeclarations();
   struct dtmpl           *get_next_deftemplate();
   int                     ParseDeftemplate();
   struct slot_item       *ParseDefault();
   int                     SetupDeftemplates();
   int                     DeleteDeftemplate();

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern char            *copy_pp_buffer();
   extern char            *check_name();
   extern struct test     *ModifyParse();
   extern struct fact     *GetGarbageFacts();
   extern struct draw     *add_symbol();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
  
   static int              deftemplate_error;
   static struct dtmpl    *deftmpl_list;

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern int              CONSERVE_SPACE;
   extern int              LOAD_FLAG;
   
/*********************************************************************/
/* SetupDeftemplates: Initializes the deftemplate construct for use. */
/*********************************************************************/
SetupDeftemplates()
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   add_construct("deftemplate",ParseDeftemplate);
   add_clear_function("deftemplate",ClearDeftemplates);
   add_save_function("deftemplate",SaveDeftemplates);
#endif
   DeftemplateCommands();
  }

/*********************************************************/
/* DeftemplateCommands: Set up the deftemplate commands. */
/*********************************************************/
DeftemplateCommands()
  {
#if (! BLOAD_ONLY)
   define_function("undeftemplate",'v',
                   UndeftemplateCommand,"UndeftemplateCommand");
   define_function("ppdeftemplate",'v',
                   PpdeftemplateCommand,"PpdeftemplateCommand"); 
   define_function("list-deftemplates",'v',
                   ListDeftemplatesCommand,"ListDeftemplatesCommand");
#endif

   define_function("modify",'u', (int (*)()) modify_stub,"modify_stub");
   define_function("(fact_get_end)",'u', (int (*)()) fact_get_end,"fact_get_end");
   define_function("(fact_get_var)",'u', (int (*)()) fact_get_var,"fact_get_var");
   
#if (! BLOAD_ONLY) && (! RUN_TIME)
   add_func_parser("modify",ModifyParse);
#endif
  }
  
#if (! RUN_TIME) && (! BLOAD_ONLY)

/*******************************************************/
/* ParseDeftemplate: Parses the deftemplate construct. */
/*******************************************************/
int ParseDeftemplate(read_source)
  char *read_source;
  {
   char *dr_name;
   struct dtmpl *new_dtmpl;
   struct slot *slots;
   struct token inputToken;
   
   /*================================================*/
   /* Initialize pretty print and error information. */
   /*================================================*/
   
   deftemplate_error = FALSE;
   set_pp_buffer_status(ON);
   flush_pp_buffer();            
   save_pp_buffer("(deftemplate ");

   /*=======================================================*/
   /* Parse the name and comment fields of the deftemplate. */
   /*=======================================================*/  

   dr_name = DRnameAndComment(read_source,&inputToken);
   if (deftemplate_error == TRUE) return(TRUE); 
   
   /*===========================================*/
   /* Parse the slot fields of the deftemplate. */
   /*===========================================*/
   
   slots = SlotDeclarations(read_source,&inputToken);
   if (deftemplate_error == TRUE) return(TRUE);

   /*=====================================*/
   /* Create a new deftemplate structure. */
   /*=====================================*/
   
   new_dtmpl = get_struct(dtmpl);
   new_dtmpl->name = gm2(sizeof(char) * (strlen(dr_name) + 1));
   strcpy(new_dtmpl->name,dr_name);
   new_dtmpl->slot_list = slots;
   new_dtmpl->next = NULL;

   /*====================================*/
   /* Store pretty print representation. */
   /*====================================*/
   
   if (CONSERVE_SPACE == TRUE)
     { new_dtmpl->pp_form = NULL; }
   else
     { new_dtmpl->pp_form = copy_pp_buffer(); } 
   
   /*==============================================*/
   /* Add deftemplate to the list of deftemplates. */
   /*==============================================*/
   
   new_dtmpl->next = deftmpl_list;
   deftmpl_list = new_dtmpl;
   
   /*=======================================*/
   /* Store symbols used by the deftemplate */
   /* in the symbol table, then return.     */
   /*=======================================*/
   
   dtmpl_install(new_dtmpl);
   return(FALSE);       
  }

/**********************************************************/
/* DRnameAndComment: Reads in the name and comment fields */ 
/*   of the deftemplate.                                  */
/**********************************************************/
static char *DRnameAndComment(read_source,inputToken)
  char *read_source;
  struct token *inputToken;
  {
   char *drName;

   /*=========================================*/
   /* Get the next token, which should be the */ 
   /* name of the deftemplate.                */
   /*=========================================*/

   gettoken(read_source,inputToken);
   if (inputToken->token != WORD)
     { 
      cl_print("werror","\nMissing deftemplate name\n");
      deftemplate_error = TRUE;
      return(NULL);
     }
   drName = inputToken->tknword;

   /*==============================================*/
   /* Delete the deftemplate if it already exists. */
   /*==============================================*/
   
   if ((DeleteDeftemplate(drName) == TRUE) && 
       (get_compilations_watch() == ON))
     {
      cl_print("wdialog","Removing deftemplate ");
      cl_print("wdialog",drName);
      cl_print("wdialog","\n");
     }

   /*=============================================================*/
   /* If watch rules is on, indicate deftemplate being processed. */
   /*=============================================================*/

   if ((get_compilations_watch() == ON) && (LOAD_FLAG == TRUE))
     {
      cl_print("wdialog","Processing deftemplate ");
      cl_print("wdialog",drName);
      cl_print("wdialog","\n");
     }
   else if (LOAD_FLAG == TRUE)
     { cl_print("wdialog","%"); }

   /*===========================*/
   /* Get comment if it exists. */
   /*===========================*/

   save_pp_buffer(" ");                 

   gettoken(read_source,inputToken);
   if (inputToken->token == STRING)
     {
      save_pp_buffer("\n   ");           
      gettoken(read_source,inputToken);
     }
   else                                 
     {
      pp_backup();
      save_pp_buffer("\n   ");
      save_pp_buffer(inputToken->print_rep);
     }

   /*=====================================*/
   /* Return the name of the deftemplate. */
   /*=====================================*/

   return(drName);
  }

/********************************************************************/
/* SlotDeclarations: Parses the slot declarations of a deftemplate. */
/********************************************************************/
struct slot *SlotDeclarations(read_source,inputToken)
  char *read_source;
  struct token *inputToken;
  {
   struct slot *new_slot, *slot_list = NULL, *last_slot = NULL;
   int parse_multi = FALSE, multi_found = FALSE;
   struct slot *multi_slot = NULL;

   while (inputToken->token != RPAREN)
     {
      /*====================================================*/
      /* Slots begin with a '(' followed by a slot keyword. */
      /*====================================================*/
      
      if (inputToken->token != LPAREN)
        {
         cl_print("werror","\nExpected a '(' to begin a slot definition\n");
         cl_print("werror","or a ')' to end slot definitions\n"); 
         rtn_slots(slot_list);
         rtn_slots(multi_slot);
         deftemplate_error = TRUE;
         return(NULL);
        }

      gettoken(read_source,inputToken);
      if (inputToken->token != WORD)
        {
         cl_print("werror","\nExpected key word field or multi-field ");
         cl_print("werror","in slot definition\n"); 
         rtn_slots(slot_list);
         rtn_slots(multi_slot);
         deftemplate_error = TRUE;
         return(NULL);
        }
        
      /*=================*/
      /* Parse the slot. */
      /*=================*/
      
      new_slot = ParseSlot(read_source,inputToken);
      if (deftemplate_error == TRUE)
        {
         rtn_slots(slot_list);
         rtn_slots(multi_slot);
         return(NULL);
        }
       
      /*=============================*/
      /* Keep track of the new slot. */
      /*=============================*/
      
      if (new_slot != NULL)
        {
         parse_multi = new_slot->multi_slot;
          
         if ((parse_multi == TRUE) && (multi_found == TRUE))
           {
            cl_print("werror","\nTwo multi-field slot definitions\n"); 
            rtn_slots(slot_list);
            rtn_slots(multi_slot);
            deftemplate_error = TRUE;
            return(NULL);
           } 
           
         if (parse_multi == TRUE)
           { 
            multi_slot = new_slot;
            multi_found = TRUE;
           }
         else
           {
            if (last_slot == NULL)
              { slot_list = new_slot; }
            else
              { last_slot->next = new_slot; }
            last_slot = new_slot;
           }
        }
       
      /*================================*/
      /* Check for closing parenthesis. */
      /*================================*/
      
      gettoken(read_source,inputToken);
      if (inputToken->token != RPAREN)
        {
         pp_backup();
         save_pp_buffer("\n   ");
         save_pp_buffer(inputToken->print_rep);
        }
     }
     
  save_pp_buffer("\n");
      
  /*====================================================*/
  /* Attach the multi slot to the end of the slot list. */
  /*====================================================*/
  
  if (last_slot != NULL)
    { last_slot->next = multi_slot; }
  else
    { slot_list = multi_slot; }
     
     
  if (MultiplyDefinedSlots(slot_list))
    {
     deftemplate_error = TRUE;
     rtn_slots(slot_list);
     return(NULL);
    }
    
  /*=======================*/
  /* Return the slot list. */
  /*=======================*/
  
  return(slot_list);
 }

/*****************************************************/
/* ParseSlot: Parses a single slot of a deftemplate. */
/*****************************************************/
struct slot *ParseSlot(read_source,inputToken)
  char *read_source;
  struct token *inputToken;
  { 
   int parse_multi;
   struct draw *slot_name;
   struct slot *new_slot;
   
   /*======================================================*/
   /* Slots must  begin with keyword field or multi-field. */
   /*======================================================*/
   
   if ((strcmp(inputToken->tknword,"field") != 0) &&
       (strcmp(inputToken->tknword,"multi-field") != 0))
     {
      cl_print("werror","\nExpected key word field or multi-field ");
      cl_print("werror","in slot definition\n");  
      deftemplate_error = TRUE;
      return(NULL);
     }
   
   /*================================================*/
   /* Determine if multi-field slot is being parsed. */
   /*================================================*/
     
   if (strcmp(inputToken->tknword,"multi-field") == 0)
     { parse_multi = TRUE; }
   else
     { parse_multi = FALSE; }
   
   /*======================================*/
   /* The name of the slot must be a word. */
   /*======================================*/
          
   save_pp_buffer(" ");
   gettoken(read_source,inputToken);
   if (inputToken->token != WORD)
     {
      cl_print("werror","\nExpected slot name to be a word\n"); 
      deftemplate_error = TRUE;
      return(NULL);
     }
        
   slot_name = inputToken->hashword;
     
   /*===================================*/
   /* Parse the attributes of the slot. */
   /*===================================*/
    
   new_slot = DefinedSlots(read_source,slot_name,parse_multi,inputToken);
   if (new_slot == NULL) 
     {
      deftemplate_error = TRUE;
      return(NULL);
     }
   
   /*==================*/
   /* Return the slot. */
   /*==================*/
   
   return(new_slot);
  }
   
/***************************************************************/
/* DefinedSlots: Parses a field or multi-field slot attribute. */
/***************************************************************/
struct slot *DefinedSlots(read_source,defined_slot,parse_multi,inputToken)
  char *read_source;
  struct draw *defined_slot;
  int parse_multi;
  struct token *inputToken;
  {
   struct slot *new_slot;
   struct slot_item *default_list = NULL;
   int default_found = FALSE;
   
   /*===========================*/
   /* Build the slot container. */
   /*===========================*/
   
   new_slot = get_struct(slot);
   new_slot->slot_name = defined_slot;
   new_slot->item_list = NULL;
   new_slot->multi_slot = parse_multi;
   new_slot->next = NULL;

   /*========================================*/
   /* Parse the primitive slot if it exists. */
   /*========================================*/

   gettoken(read_source,inputToken);
   
   while (inputToken->token != RPAREN)
     {
      pp_backup();
      save_pp_buffer(" ");
      save_pp_buffer(inputToken->print_rep);
      
      /*================================================*/
      /* Slot attributes begin with a left parenthesis. */
      /*================================================*/
      
      if (inputToken->token != LPAREN)
        { 
         cl_print("werror","\nExpected '(' to begin slot attribute definition\n"); 
         rtn_slots(new_slot);
         deftemplate_error = TRUE;
         return(NULL);
        }
        
      /*===========================================*/
      /* The name of the attribute must be a word. */
      /*===========================================*/
      
      gettoken(read_source,inputToken);
      if (inputToken->token != WORD)
        {
         cl_print("werror","\nIllegal slot attribute\n");
         rtn_slots(new_slot);
         deftemplate_error = TRUE;
         return(NULL);
        }

      /*========================================================*/
      /* If the attribute is only used by CRSV, then ignore it. */
      /*===================================================*/
      
      if ((strcmp(inputToken->tknword,"type") == 0) ||
          (strcmp(inputToken->tknword,"allowed-words") == 0) ||
          (strcmp(inputToken->tknword,"allowed-strings") == 0) ||
          (strcmp(inputToken->tknword,"allowed-numbers") == 0) ||
          (strcmp(inputToken->tknword,"range") == 0))
        { 
         if (IgnoreSlot(read_source,inputToken) == 0)
           { 
            deftemplate_error = TRUE;
            rtn_slots(new_slot);
            return(NULL);
           };  
        }
      else if ((strcmp(inputToken->tknword,"min-number-of-elements") == 0) ||
               (strcmp(inputToken->tknword,"max-number-of-elements") == 0))
        { 
         if (parse_multi)
           { 
            if (IgnoreSlot(read_source,inputToken) == 0)
              { 
               deftemplate_error = TRUE;
               rtn_slots(new_slot);
               return(NULL);
              }; 
           }
         else
           {
            cl_print("werror","\nmin and max number-of-elements attributes ");
            cl_print("werror","are only allowed for multi-field slots\n"); 
            deftemplate_error = TRUE;
            rtn_slots(new_slot);
            return(NULL);
           }
        }
        
      /*=================================================*/
      /* else if the attribute is the default attribute, */
      /* then get the default list for this slot.        */
      /*=================================================*/
      
      else if (strcmp(inputToken->tknword,"default") == 0)
        {
         if (default_found)
           {
            cl_print("werror","\nDefault attribute multiply defined\n");
            deftemplate_error = TRUE;
            rtn_slots(new_slot);
            return(NULL);
           }
            
         default_list = ParseDefault(read_source,parse_multi,inputToken);
         if (deftemplate_error == TRUE)
           { 
            rtn_slots(new_slot);
            return(NULL); 
           }
         default_found = TRUE;
         new_slot->item_list = default_list;
        }
        
      /*============================================*/
      /* Otherwise the attribute is an invalid one. */
      /*============================================*/
      
      else
        {
         cl_print("werror","\nIllegal slot attribute\n");
         rtn_slots(new_slot);
         deftemplate_error = TRUE;
         return(NULL);
        }
      
      /*===================================*/
      /* Begin parsing the next attribute. */
      /*===================================*/
      
      gettoken(read_source,inputToken);
     }

   /*============================*/
   /* Return the attribute list. */
   /*============================*/
   
   return(new_slot);
  }
  
/**********************************************/
/* ParseDefault: Parses a default value list. */
/**********************************************/
struct slot_item *ParseDefault(read_source,parse_multi,inputToken)
  char *read_source;
  int parse_multi;
  struct token *inputToken;
  {
   struct slot_item *default_list = NULL, *last_default = NULL;
   struct slot_item *new_item;
   
   save_pp_buffer(" ");
   gettoken(read_source,inputToken);

   while (inputToken->token != RPAREN)
     {
      save_pp_buffer(" ");
         
      /*==========================================================*/
      /* Create a data structure to store the next default value. */
      /*==========================================================*/
      
      new_item = get_struct(slot_item);
      new_item->type = inputToken->token;
      new_item->next = NULL;
       
      /*============================================*/
      /* If the value is a word, string, or number, */ 
      /* store it as the next default value.        */
      /*============================================*/
      
      if ((inputToken->token == WORD) || (inputToken->token == STRING))
        { new_item->value.string = inputToken->hashword; }
      else if (inputToken->token == NUMBER)
        { new_item->value.number = inputToken->tknnumber; }
        
      /*===================================*/
      /* Else the variable ?NONE indicates */
      /* that there is no default value.   */
      /*===================================*/
      
      else if (strcmp(inputToken->print_rep,"?NONE") == 0)
        {
         if (default_list != NULL)
           { 
            cl_print("werror","\n?NONE cannot be used with other default values\n");
            rtn_items(default_list);
            rtn_items(new_item);
            deftemplate_error = TRUE;
            return(NULL);
           }
         gettoken(read_source,inputToken);
         
         if (inputToken->token != RPAREN)
           {
            cl_print("werror","\nExpected ')' after ?NONE default value\n");
            rtn_items(default_list);
            rtn_items(new_item);
            deftemplate_error = TRUE;
            return(NULL);
           }
         
         pp_backup();
         pp_backup();
         save_pp_buffer(")");
         
         rtn_struct(slot_item,new_item);
         return(NULL);
        }
        
      /*===============================================*/
      /* else an invalid default value has been given. */
      /*===============================================*/
      
      else
        {
         cl_print("werror","\nDefault values must be words, strings, or numbers\n");
         rtn_items(default_list);
         rtn_items(new_item);
         deftemplate_error = TRUE;
         return(NULL);
        }
      
      /*============================================*/
      /* Add the default value to the default list. */
      /*============================================*/
      
      if (last_default == NULL)
        { default_list = new_item; }
      else
        { last_default->next = new_item; }
      last_default = new_item;
      
      /*=======================================*/
      /* Begin parsing the next default value. */
      /*=======================================*/
      
      gettoken(read_source,inputToken);
     }
     
   /*=====================================*/
   /* Fix up pretty print representation. */
   /*=====================================*/
   
   pp_backup();
   pp_backup();
   save_pp_buffer(")");
   
   /*=========================================*/
   /* A single field slot's default attribute */
   /* must contain a single value.            */
   /*=========================================*/
   
   if ((parse_multi == FALSE) && 
       ((default_list == NULL) ? TRUE :
                                 (default_list->next == NULL) ? FALSE : TRUE))
     {
      cl_print("werror","\nThe default attribute for a single field slot ");
      cl_print("werror","must contain a single value\n");
      rtn_items(default_list);
      deftemplate_error = TRUE;
      return(NULL);
     }

   /*==========================*/
   /* Return the default list. */
   /*==========================*/
   
   return(default_list);
  }

/*****************************************************************/
/* IgnoreSlot: Skips over slot items until a ')' is encountered. */
/*****************************************************************/
IgnoreSlot(read_source,inputToken)
  char *read_source;
  struct token *inputToken;
  {
   save_pp_buffer(" ");
   gettoken(read_source,inputToken);
   while (inputToken->token != RPAREN)
     {
      if (inputToken->token == STOP)
        {
         cl_print("werror","\nExpected ')' to end attribute definition\n");
         return(0);
        }
        
      save_pp_buffer(" ");
      gettoken(read_source,inputToken); 
     }
     
   pp_backup();
   pp_backup();
   save_pp_buffer(")");
   return(1);
  }
  
/*******************************************/
/* MultiplyDefinedSlots: Determines if two */
/*   slots have been given the same name.  */
/*******************************************/
MultiplyDefinedSlots(first_slot)
  struct slot *first_slot;
  {
   struct slot *next_slot;
   int rv = FALSE;
   
   while (first_slot != NULL)
     {
      next_slot = first_slot->next;
      while (next_slot != NULL)
        {
         if (next_slot->slot_name == first_slot->slot_name)
           {
            cl_print("werror","\nMultiple occurences of slot ");
            cl_print("werror",first_slot->slot_name->contents);
            cl_print("werror","\n");
            rv = TRUE;
           }
         next_slot = next_slot->next;
        }
      first_slot = first_slot->next;
     }
  
   return(rv);
  }

/******************************************************************/
/* DeleteDeftemplate: Deletes the named deftemplate. Returns TRUE */
/*   if the deftemplate was found and deleted, otherwise FALSE.   */
/******************************************************************/
DeleteDeftemplate(name)
  char *name;
  {
   struct dtmpl *last_ptr, *cur_ptr, *next_ptr;

   last_ptr = NULL;
   cur_ptr = get_next_deftemplate(NULL);
   while (cur_ptr != NULL)
     {
      next_ptr = cur_ptr->next;
      if (strcmp(cur_ptr->name,name) == 0)
        {
         if (last_ptr == NULL)
           { deftmpl_list = next_ptr; }
         else
           { last_ptr->next = next_ptr; }

         dtmpl_deinstall(cur_ptr);
         rtn_slots(cur_ptr->slot_list);
         rm(cur_ptr->name,sizeof(char) * (strlen(cur_ptr->name) + 1));
         if (cur_ptr->pp_form != NULL)
           {
            rm(cur_ptr->pp_form,
                    sizeof(char) * (strlen(cur_ptr->pp_form) + 1));
           }
         rtn_struct(dtmpl,cur_ptr);
         return(1);
        }
  
      last_ptr = cur_ptr;
      cur_ptr = next_ptr;
     }

   return(0); 
  }

/***********************************************/
/* rtn_slots: Returns the slot structures of a */
/*   deftemplate to free memory.               */
/***********************************************/
static int rtn_slots(slot_ptr)
  struct slot *slot_ptr;
  {
   struct slot *next_slot;
   
   while (slot_ptr != NULL)
     {
      next_slot = slot_ptr->next;
      rtn_items(slot_ptr->item_list);
      rtn_struct(slot,slot_ptr);
      slot_ptr = next_slot;
     }
  }

/*************************************************/
/* rtn_items: Returns the items of a slot from a */
/*   deftemplate to free memory.                 */
/*************************************************/
static int rtn_items(item_ptr)
  struct slot_item *item_ptr;
  {
   struct slot_item *next_item;
   
   while (item_ptr != NULL)
     {
      next_item = item_ptr->next;
      rtn_struct(slot_item,item_ptr);
      item_ptr = next_item;
     }
  }

/*********************************************************/
/* dtmpl_install: Increments all occurrences in the hash */
/*   table of symbols found in an deftemplate.           */  
/*********************************************************/
static int dtmpl_install(dtmpl_ptr)
  struct dtmpl *dtmpl_ptr;
  {
   struct slot *slot_ptr;
   struct slot_item *item_ptr;
   
   slot_ptr = dtmpl_ptr->slot_list;
   while (slot_ptr != NULL)
     {
      inc_symbol_count(slot_ptr->slot_name);
      item_ptr = slot_ptr->item_list;
      while (item_ptr != NULL)
        {
         if ((item_ptr->type == STRING) ||
             (item_ptr->type == WORD))
           { inc_symbol_count(item_ptr->value.string); }
         item_ptr = item_ptr->next;
        }
      slot_ptr = slot_ptr->next;
     }
  }

/******************************************************/
/* dtmpl_deinstall: Decrements all occurrences in the */
/*   hash table of symbols found in an deftemplate.   */  
/******************************************************/
static int dtmpl_deinstall(dtmpl_ptr)
  struct dtmpl *dtmpl_ptr;
  {
   struct slot *slot_ptr;
   struct slot_item *item_ptr;
   
   slot_ptr = dtmpl_ptr->slot_list;
   while (slot_ptr != NULL)
     {
      dec_symbol_count(slot_ptr->slot_name);
      item_ptr = slot_ptr->item_list;
      while (item_ptr != NULL)
        {
         if ((item_ptr->type == STRING) ||
             (item_ptr->type == WORD))
           { dec_symbol_count(item_ptr->value.string); }
         item_ptr = item_ptr->next;
        }
      slot_ptr = slot_ptr->next;
     }
  }

/***********************************************************/
/* get_next_deftemplate: If passed NULL, returns the first */
/*   deftemplate, otherwise the deftemplate after the      */
/*   deftemplate passed as an argument.                    */
/***********************************************************/
struct dtmpl *get_next_deftemplate(def_ptr)
  struct dtmpl *def_ptr;
  {
   if (def_ptr == NULL)
     { return(deftmpl_list); }
   else
     { return(def_ptr->next); }
  }
  
/************************************************/
/* ClearDeftemplates: Removes all deftemplates. */
/************************************************/
ClearDeftemplates()
  {
   while (deftmpl_list != NULL)
     { DeleteDeftemplate(deftmpl_list->name); }
  }

/***************************************************/
/* SaveDeftemplates: Saves deftemplates to a file. */
/***************************************************/
SaveDeftemplates(log_name)
  char *log_name;
  {
   struct dtmpl *dtm_ptr;
   char *ppform;
   
   dtm_ptr = get_next_deftemplate(NULL);
   while (dtm_ptr != NULL)
     {
      ppform = get_deftemplate_ppform(dtm_ptr);
      if (ppform != NULL)
        {
         print_in_chunks(log_name,ppform);
         cl_print(log_name,"\n");
        }
      dtm_ptr = get_next_deftemplate(dtm_ptr);
     }
  }
   
/**********************************************************/
/* UndeftemplateCommand: Removes a deftemplate statement. */
/*   Syntax: (undeftemplate <deftemplate name>)           */
/**********************************************************/
int UndeftemplateCommand()
  {
   char *name;

   name = check_name(1,1,"undeftemplate","deftemplate name");
   if (name == NULL) return(0);

   if (DeleteDeftemplate(name) == FALSE)
     { 
      cl_print("werror","Unable to find deftemplate ");
      cl_print("werror",name);
      cl_print("werror","\n");
      return(0);
     }

   return(1);
  }

/************************************************************/
/* get_deftemplate_name:                                    */
/************************************************************/
char *get_deftemplate_name(def_ptr)
  struct dtmpl *def_ptr;
  { return(def_ptr->name); }
  
/******************************************************************/
/* FindDeftemplate:  Searches for a deftemplate in the list of    */
/*   deftemplates. Returns a pointer to the deftemplate if found, */
/*   otherwise NULL.                                              */
/******************************************************************/
struct dtmpl *FindDeftemplate(df_name)
  char *df_name;
  {
   struct dtmpl *df_ptr;

   df_ptr = get_next_deftemplate(NULL);
   while (df_ptr != NULL)
     {
      if (strcmp(df_ptr->name,df_name) == 0)
        { return(df_ptr); }
      
      df_ptr = df_ptr->next; 
     }

   return(NULL);
  }

/**********************************************/
/* get_deftemplate_ppform: Returns the pretty */ 
/*   print string for the deftemplate.        */
/**********************************************/
char *get_deftemplate_ppform(def_ptr)
  struct dtmpl *def_ptr;
  { return(def_ptr->pp_form); }

/*******************************************************/
/* PpdeftemplateCommand: pretty prints a deftemplate.  */
/*   Syntax: (pp_deftemplate <deftemplate name>)       */
/*******************************************************/
int PpdeftemplateCommand()
  {
   char *dr_name;

   dr_name = check_name(1,1,"ppdeftemplate","deftemplate name");
   if (dr_name == NULL) return(0);

   pp_deftemplate(dr_name,"wdisplay");

   return(1);
  }
  
/******************************************************/
/* pp_deftemplate: the driver which actually does the */
/*   pretty printing of the deftemplate.              */
/******************************************************/
pp_deftemplate(df_name,fileid)
  char *df_name, *fileid;
  {
   struct dtmpl *df_ptr;

   df_ptr = FindDeftemplate(df_name);
   if (df_ptr == NULL)        
     {
      cl_print("werror","Unable to find deftemplate ");
      cl_print("werror",df_name);
      cl_print("werror","\n");
      return(FALSE);
     }
     
   if (get_deftemplate_ppform(df_ptr) == NULL) return(TRUE);
   print_in_chunks(fileid,get_deftemplate_ppform(df_ptr));
   return(TRUE);
  }
  
/****************************************************/
/* ListDeftemplatesCommand: Interface glue function */ 
/*   for displaying the list of deftemplates.       */
/*   Syntax: (list-deftemplates)                    */
/****************************************************/
ListDeftemplatesCommand()
  {
   if (arg_num_check("list-deftemplates",EXACTLY,0) == -1) return;

   list_deftemplates();
  }

/*********************************************************/
/* list_deftemplates: Displays the list of deftemplates. */
/*********************************************************/
int list_deftemplates()
  {
   struct dtmpl *dr_ptr;

   dr_ptr = get_next_deftemplate(NULL);
   while (dr_ptr != NULL)
     {
      cl_print("wdisplay",get_deftemplate_name(dr_ptr));
      cl_print("wdisplay","\n");
      dr_ptr = get_next_deftemplate(dr_ptr);
     }

   return(1);
  }

/******************************************************************/
/* PrintTemplateFact: Prints a fact using the deftemplate format. */
/*   Returns TRUE if the fact was printed using this format,      */
/*   otherwise FALSE.                                             */
/******************************************************************/
PrintTemplateFact(log_name,fact_ptr)
  char *log_name;
  struct fact *fact_ptr;
  {
   struct element *sublist;
   int length, i;
   struct dtmpl *dtmpl_ptr;
   struct slot *slot_ptr;
   
   sublist = fact_ptr->atoms;
   length = fact_ptr->fact_length;
   
   /*========================================================*/
   /* If the first field of the fact is not a word, then the */
   /* fact cannot be printed using the deftemplate format.   */
   /*========================================================*/
   
   if (sublist[0].type != WORD) return(FALSE);
   
   /*========================================================*/
   /* If the deftemplate corresponding to the first field of */
   /* of the fact cannot be found, then the fact cannot be   */
   /* printed using the deftemplate format.                  */
   /*========================================================*/
   
   dtmpl_ptr = FindDeftemplate(sublist[0].val.hvalue->contents);
   if (dtmpl_ptr == NULL) return(FALSE);
   
   /*=============================================*/
   /* Print the relation name of the deftemplate. */
   /*=============================================*/
   
   cl_print(log_name,dtmpl_ptr->name);
   cl_print(log_name," ");
   
   /*===================================================*/
   /* Print each of the field slots of the deftemplate. */
   /*===================================================*/
   
   slot_ptr = dtmpl_ptr->slot_list;
   
   i = 1;
   while (slot_ptr != NULL)
     {    
      /*===========================================*/
      /* Print the closing parenthesis of the slot */
      /* and the slot name.                        */
      /*===========================================*/
      
      cl_print(log_name,"(");
      cl_print(log_name,slot_ptr->slot_name->contents);
     
      /*======================================================*/
      /* Print the value of the slot for a single field slot. */
      /*======================================================*/
      
      if (slot_ptr->multi_slot == FALSE)
        {
         cl_print(log_name," ");
         if (i < length)
           { print_element(log_name,&sublist[i]); }
         else
           { cl_print(log_name,"<<<MISSING FIELD>>>"); }
         i++;
        }
        
      /*==========================================================*/
      /* Else print the value of the slot for a multi field slot. */
      /*==========================================================*/
      
      else
        {
         while (i < length)
           {
            cl_print(log_name," ");
            print_element(log_name,&sublist[i]);
            i++;
           }
        }
        
      /*============================================*/
      /* Print the closing parenthesis of the slot. */
      /*============================================*/
      
      cl_print(log_name,")");
      slot_ptr = slot_ptr->next;
      if (slot_ptr != NULL) cl_print(log_name," ");
     }
   
   /*==========================================*/
   /* Return TRUE indicating that the fact was */
   /* printed using the deftemplate format.    */
   /*==========================================*/
   
   return(TRUE);
  }
  
#endif
  
/********************************************/
/* fact_get_end: Used for modify function.  */
/********************************************/
fact_get_end(bound_var)
  VALUE_PTR bound_var;
  {
   int fact_index, element;
   struct fact *fPtr;
   struct test *test_ptr;
   SEGMENT seg_ptr;
      
   test_ptr = get_first_arg();
   fact_index = get_test_index(test_ptr);
   element = get_test_index(get_next_arg(test_ptr));
   
   fPtr = GetGarbageFacts();
   while ((fPtr == NULL) ? FALSE : (fPtr->ID < fact_index))
     { fPtr = fPtr->next; }
        
   if ((fPtr == NULL) ? TRUE : ((fPtr->ID != fact_index) || 
                                (element > fPtr->fact_length)))
     {
      set_vptype(bound_var,MULTIPLE);
      set_vpbegin(bound_var,1);
      set_vpend(bound_var,0);
      seg_ptr = get_segment(0);
      set_vpsegment(bound_var,seg_ptr);
      return;
     }

   bound_var->type = MULTIPLE;
   bound_var->origin = fPtr;
   bound_var->begin = --element;
   bound_var->end = fPtr->fact_length - 1;      
   return;
  }

/*****************************************************************/
/* fact_get_var:                                                 */
/*****************************************************************/
fact_get_var(bound_var)
  VALUE_PTR bound_var;
  {
   int fact_index, element;
   struct fact *fPtr;
   struct element *elem_ptr;
   struct test *test_ptr;
   
   test_ptr = get_first_arg();
   fact_index = get_test_index(test_ptr);
   element = get_test_index(get_next_arg(test_ptr));
   
   fPtr = GetGarbageFacts();
   while ((fPtr == NULL) ? FALSE : (fPtr->ID < fact_index))
     { fPtr = fPtr->next; }
  
   if (element > fPtr->fact_length)
     {
      bound_var->type = WORD;
      bound_var->val.hvalue = add_symbol("nil");
      return;
     }
     
   element--;
   elem_ptr = &fPtr->atoms[element];

   bound_var->type = elem_ptr->type;
   if (bound_var->type == NUMBER)
     { bound_var->val.fvalue = elem_ptr->val.fvalue; }
   else
     { bound_var->val.hvalue = elem_ptr->val.hvalue; }
     
   return;
  }
  
/**********************/
/* modify_stub:       */
/**********************/
modify_stub()
  {}
  
#if RUN_TIME || BLOAD_ONLY

int ListDeftemplatesCommand() {};
int PpdeftemplateCommand() {};
int UndeftemplateCommand() {};

#endif

#endif
