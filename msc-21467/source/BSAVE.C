/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    BSAVE MODULE                     */
   /*******************************************************/

#include "setup.h"

#if BLOAD_AND_BSAVE && (! RUN_TIME)

#include <stdio.h>
#include "clipsmem.h"
#include "constant.h"
#include "access.h"
#include "symbol.h"

#if DEFFACTS_CONSTRUCT
#include "deffacts.h"
#endif

/***************/
/* DEFINITIONS */
/***************/

#define PATTERNS 0
#define JOINS    1

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   struct funtab            **BsaveFunctions();
   int                       *BsaveSymbols();
   long int                   CountFunctions();
   long int                   FunctionNumber();
   long int                   FunctionBinarySize();
   long int                   SymbolIndex();
   long int                   FunctionIndex();
   long int                   FindJoinPointer();
   long int                   FindPatternPointer();
   long int                   FindListPointer();
   long int                   SymbolNumber();
   long int                   CountSymbols();

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern struct draw       **get_symbol_table();
   extern struct funtab      *get_fctn_list();
   extern struct pat_node    *network_pointer();
   extern char               *genlongalloc();
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   extern char               *BinaryIDString;
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static unsigned long       ExpressionCount;
   static unsigned long int   PatternCount;
   static unsigned long int   JoinCount;
   static unsigned long int   ListCount;
   
   static struct internode  **JoinIndex;
   static struct list       **ListIndex;
   static struct pat_node   **PatternIndex; 
   static int                *SymbolCountArray;

/***************************************************/
/* bsave_command: Handles top level bsave command. */
/***************************************************/
int bsave_command()
  {
   char *file_name;
   VALUE arg_ptr;
   
   /*============================================*/
   /* Check for appropriate number of arguments. */
   /*============================================*/
   
   if (arg_num_check("bsave",EXACTLY,1) == -1) return;

   /*====================================*/
   /* Get the file name to place C code. */
   /*====================================*/
  
   if (arg_type_check("bsave",1,STRING,&arg_ptr) == FALSE) return;
   file_name = rvalstring(arg_ptr);
 
   /*===============================================*/
   /* Call the bsave function to perform the bsave. */
   /*===============================================*/
    
   bsave(file_name);
  }

/********************************************************/
/* bsave: Stores the binary representation of the CLIPS */
/*   environment to a file.                             */
/********************************************************/
bsave(file_name)
  char *file_name;
  {
   FILE *fp, *fopen();
   long int numberOfFunctions;
   long int numberOfSymbols;
   struct funtab **functionArray;
    
   /*================*/
   /* Open the file. */
   /*================*/

   if ((fp = fopen(file_name,"wb")) == NULL)
     {
      open_error_message("bsave",file_name);
      return(0);
     }
     
   /*==================================*/
   /* Write binary header to the file. */
   /*==================================*/
   
   GenWrite(BinaryIDString,(unsigned long) strlen(BinaryIDString) + 1,fp);
   
   /*=============================*/
   /* Initialize count variables. */
   /*=============================*/
   
   ExpressionCount = 0;
   PatternCount = 0;
   JoinCount = 0;
   ListCount = 0;
   
   /*=====================*/
   /* Save the functions. */
   /*=====================*/
   
   functionArray = BsaveFunctions(fp,&numberOfFunctions);
   
   /*===================*/
   /* Save the symbols. */
   /*===================*/
        
   SymbolCountArray = BsaveSymbols(fp,&numberOfSymbols);
      
   /*=================================================*/
   /* Save pointers in temporary arrays to structures */
   /* in the pattern and join networks.               */
   /*=================================================*/
   
   JoinIndex = (struct internode **) 
               genlongalloc((long) sizeof(struct internode *) * JoinCount);
   ListIndex = (struct list **) genlongalloc((long) sizeof(struct list *) * ListCount);
   PatternIndex = (struct pat_node **) 
                  genlongalloc((long) sizeof(struct pat_node *) * PatternCount);
   
   PatternCount = 0;
   JoinCount = 0;
   ListCount = 0;
   
   GetPatternPointers(network_pointer());
   
   /*===================*/
   /* Save expressions. */
   /*===================*/
   
   BsaveAllExpressions(fp,numberOfFunctions,functionArray);
   
   /*====================*/
   /* Save the deffacts. */
   /*====================*/
   
   ExpressionCount = 0;
   BsaveDeffacts(fp);
        
   /*=====================================*/
   /* Save the pattern and join networks. */
   /*=====================================*/
     
   GenWrite(&PatternCount,(unsigned long) sizeof(long int),fp);
   GenWrite(&JoinCount,(unsigned long) sizeof(long int),fp);
   GenWrite(&ListCount,(unsigned long) sizeof(long int),fp);
   
   BsavePatterns(network_pointer(),fp);
   BsaveJoins(network_pointer(),fp);
   BsaveLists(network_pointer(),fp);
   
   /*===========*/
   /* Clean up. */
   /*===========*/
   
   genlongfree(JoinIndex,(long) sizeof(struct internode *) * JoinCount);
   genlongfree(ListIndex,(long) sizeof(struct list *) * ListCount);
   genlongfree(PatternIndex,(long) sizeof(struct pat_node *) * PatternCount);
   genlongfree(functionArray,(long) sizeof(struct funtab *) * numberOfFunctions);
   genlongfree(SymbolCountArray,(long) sizeof(int) * numberOfSymbols);
   
   /*=================*/
   /* Close the file. */
   /*=================*/
   
   fclose(fp);
   
   /*==================================*/
   /* Return TRUE to indicate success. */
   /*==================================*/
   
   return(TRUE);
  }

/**********************************************************/
/* BsaveFunctions: Performs binary save of function names */
/*   needed for the binary image.                         */
/**********************************************************/
struct funtab **BsaveFunctions(fp,numberOfFunctions)
  FILE *fp;
  long int *numberOfFunctions;
  {
   long int i;
   struct funtab **functionArray;
   
   /*==========================================*/
   /* Determine the total number of functions. */
   /*==========================================*/
   
   *numberOfFunctions = CountFunctions();
   
   /*============================================================*/
   /* Create an array for indicating which functions are needed. */
   /*============================================================*/
   
   functionArray = (struct funtab **)
                   genlongalloc((long) sizeof(struct funtab *) * (*numberOfFunctions));
   for (i = 0; i < *numberOfFunctions; i++) functionArray[i] = NULL;
   
   /*============================*/
   /* Find the needed functions. */
   /*============================*/
   
   FindNeededFunctions(functionArray);
   
   /*====================================================*/
   /* Save the needed function names to the binary file. */
   /*====================================================*/
   
   WriteNeededFunctions(functionArray,fp,*numberOfFunctions);
   
   /*============================*/
   /* Return the function array. */
   /*============================*/
   
   return(functionArray);
  }
  
/****************************************************/
/* CountFunctions: Returns the number of functions. */
/****************************************************/
long int CountFunctions()
  {
   struct funtab *list;
   long int count = 0;
   
   list = get_fctn_list();
   while (list != NULL)
     {
      count++;
      list = list->next;
     }
     
   return(count);
  }
  
/************************************************************/
/* FindNeededFunctions: Searches through the deffacts list, */
/*   pattern network, and join network for functions used   */
/*   in expressions.                                        */
/************************************************************/
FindNeededFunctions(functionArray)
  struct funtab *functionArray[];
  {
#if DEFFACTS_CONSTRUCT
   struct dfact *deflist;
   
   /*=========================*/
   /* Find Deffacts Functions */
   /*=========================*/
  
   deflist = get_next_deffact(NULL);
   while (deflist != NULL)
     {
      ExpressionCount += ExpressionSize(deflist->alist);
      MarkNeededFunctions(functionArray,deflist->alist);
      deflist = deflist->next;
     }
#endif
     
   /*=================================*/
   /* Find functions in rule network. */
   /*=================================*/
   
   FindPatternFunctions(network_pointer(),functionArray);
  }
  
/******************************************************************/
/* MarkNeededFunctions: Examines an expression to determine which */
/*   functions are necessary to evaluate the expression.          */
/******************************************************************/
MarkNeededFunctions(functionArray,testPtr)
  struct funtab *functionArray[];
  struct test *testPtr;
  {
   long int number;
   
   while (testPtr != NULL)
     {
      if (testPtr->type == FCALL)
        {
         number = FunctionNumber(testPtr->val.fun_ptr);
         functionArray[number] = testPtr->val.fun_ptr;
         if (testPtr->arg_list != NULL)
           { MarkNeededFunctions(functionArray,testPtr->arg_list); }
        }
     
      testPtr = testPtr->next_arg;
     }
  }
     
/**********************************************************************/ 
/* FunctionNumber: Returns a functions position in the function list. */
/**********************************************************************/     
long int FunctionNumber(functionPtr)
  struct funtab *functionPtr;
  {
   struct funtab *functionList;
   long int count = 0;

   functionList = get_fctn_list();
   
   while (functionList != NULL)
     {
      if (functionPtr == functionList) 
        { return(count); }
      count++;
      functionList = functionList->next;
     }

   return(-1);
  }
  
/****************************************************/
/* WriteNeededFunctions: Writes the names of needed */
/*   functions to the binary save file.             */
/****************************************************/
WriteNeededFunctions(functionArray,fp,numberOfFunctions)
  struct funtab *functionArray[];
  FILE *fp;
  long int numberOfFunctions;
  {
   long int space, i, count;
   int length;
   
   /*=======================================================*/
   /* Determine the number of function names to be written. */
   /*=======================================================*/
   
   count = 0;
   for (i = 0 ; i < numberOfFunctions; i++)
     { if (functionArray[i] != NULL) count++; } 
   GenWrite(&count,(unsigned long) sizeof(long int),fp);
   if (count == 0) return;
   
   /*=========================================================*/
   /* Determine the amount of space needed for the functions. */
   /*=========================================================*/
   
   space = FunctionBinarySize(functionArray,numberOfFunctions);  
   GenWrite(&space,(unsigned long) sizeof(long int),fp);
   
   /*===============================*/
   /* Write out the function names. */
   /*===============================*/
   
   for (i = 0 ; i < numberOfFunctions; i++)
     {
      if (functionArray[i] != NULL)
        { 
         length = strlen(functionArray[i]->fun_name) + 1;
         GenWrite(functionArray[i]->fun_name,(long) length,fp);
        }
     }
  }
  
/****************************************************************/
/* FunctionBinarySize: Determines the number of bytes needed to */
/*   save all of the function names in the binary save file.    */
/****************************************************************/
long int FunctionBinarySize(functionArray,numberOfFunctions)
  struct funtab *functionArray[];
  long int numberOfFunctions;
  {
   long int size = 0, i;
   
   for (i = 0 ; i < numberOfFunctions; i++)
     {
      if (functionArray[i] != NULL)
        { size += strlen(functionArray[i]->fun_name) + 1; }
     }
  
   return(size);
  }
  
/***************************************************************************/
/* FindPatternFunctions: Counts the number of expression structures needed */
/*   to store all expressions in the pattern network. Keeps track of which */
/*   functions are needed to evaluate these expressions. Call routines to  */
/*   perform the same function for the join network.                       */
/***************************************************************************/
FindPatternFunctions(pat_ptr,functionArray)
  struct pat_node *pat_ptr;
  struct funtab *functionArray[];
  {
   while (pat_ptr != NULL)
     {
      PatternCount++;
      if (pat_ptr->eval != NULL)
        { 
         ExpressionCount += ExpressionSize(pat_ptr->eval);
         MarkNeededFunctions(functionArray,pat_ptr->eval); 
        }
        
      if (pat_ptr->next_level == NULL)
        { FindJoinFunctions(pat_ptr->path,functionArray,FALSE); }
      else
        { FindPatternFunctions(pat_ptr->next_level,functionArray); }
        
      pat_ptr = pat_ptr->same_level;
     }
  }
  
/************************************************************************/
/* FindJoinFunctions: Counts the number of expression structures needed */
/*   to store all expressions in the join network. Keeps track of which */
/*   functions are needed to evaluate these expressions.                */                              
/************************************************************************/
FindJoinFunctions(list_ptr,functionArray,flag)
  struct list *list_ptr;
  struct funtab *functionArray[];
  int flag;
  {
   while (list_ptr != NULL)
     {
      ListCount++;
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         JoinCount++;
         if (list_ptr->path->eval != NULL)
           { 
            ExpressionCount += ExpressionSize(list_ptr->path->eval);
            MarkNeededFunctions(functionArray,list_ptr->path->eval); 
           }
           
         if (list_ptr->path->not_eval != NULL)
           { 
            ExpressionCount += ExpressionSize(list_ptr->path->not_eval);
            MarkNeededFunctions(functionArray,list_ptr->path->not_eval); 
           }
           
         FindJoinFunctions(list_ptr->path->next,functionArray,TRUE);
        }
      list_ptr = list_ptr->next;
     }
  }
   
/****************************************************/
/* ExpressionSize: Returns the number of structures */
/*   stored in an expression.                       */
/****************************************************/
ExpressionSize(testPtr)
  struct test *testPtr;
  {
   int size = 0;
   
   while (testPtr != NULL)
     {
      size++;
      if (testPtr->arg_list != NULL)
        { size += ExpressionSize(testPtr->arg_list); }
      testPtr = testPtr->next_arg;
     }
   return(size);
  }

/************************************************************************/
/* BsaveExpression: Recursively saves an expression to the binary file. */
/************************************************************************/
BsaveExpression(testPtr,currentIndex,numberOfFunctions,functionArray,fp)
  struct test *testPtr;
  long int *currentIndex;
  long int numberOfFunctions;
  struct funtab *functionArray[];
  FILE *fp;
  {
   struct test newTest;
   long int newIndex;
   
   while (testPtr != NULL)
     {
      (*currentIndex)++;
      
      /*================*/
      /* Copy the type. */
      /*================*/
      
      newTest.type = testPtr->type;
      
      /*========================================*/
      /* Convert the arg_list slot to an index. */
      /*========================================*/
      
      if (testPtr->arg_list == NULL)
        { newTest.arg_list = (struct test *) -1L; }
      else
        { newTest.arg_list = (struct test *) (*currentIndex); }
      
      /*========================================*/
      /* Convert the next_arg slot to an index. */
      /*========================================*/
      
      if (testPtr->next_arg == NULL)
        { newTest.next_arg = (struct test *) -1L; }
      else
        {
         newIndex = (*currentIndex) + ExpressionSize(testPtr->arg_list);
         newTest.next_arg = (struct test *) newIndex;
        }
        
      /*=========================*/
      /* Convert the value slot. */
      /*=========================*/
      
      switch(testPtr->type)
        {
         case NUMBER:
           newTest.val.fvalue = testPtr->val.fvalue;
           break;
         
         case INDEX:
           newTest.val.index = testPtr->val.index;
           break;
         
         case FCALL:
           newTest.val.fun_ptr = NULL;
           newIndex = FunctionIndex(testPtr->val.fun_ptr,
                                    numberOfFunctions,functionArray);
           newTest.val.fun_ptr = (struct funtab *) newIndex;
           break;
         
         case WORD:
         case STRING:
           newTest.val.hvalue = NULL;
           newIndex = SymbolIndex(testPtr->val.hvalue);
           newTest.val.hvalue = (struct draw *) newIndex;
           break;
           
         default:
           cl_print("werror","Unidentified case\n");
           break;
        }
        
     /*===========================*/
     /* Write out the expression. */
     /*===========================*/
     
     GenWrite(&newTest,(unsigned long) sizeof(struct test),fp);
     
     /*==========================*/
     /* Write out argument list. */
     /*==========================*/
     
     if (testPtr->arg_list != NULL)
       {
        BsaveExpression(testPtr->arg_list,currentIndex,
                        numberOfFunctions,functionArray,fp);
       }
       
     testPtr = testPtr->next_arg;
    }
  }
  
/**********************************************/
/* SymbolIndex: Determines the index value of */
/*   a symbol in the symbol array.            */
/**********************************************/
long int SymbolIndex(drawPtr)
  struct draw *drawPtr;
  {
   long int count = 0;
   int i, j;
   struct draw *symbolPtr, **symbolArray;
   
   symbolArray = get_symbol_table();
   for (i = 0, j = 0; i < HASHSIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         if (symbolPtr == drawPtr) return(count);
         
         if (symbolPtr->count > 0) 
           {
            if (SymbolCountArray[j] > 0) count++;
            j++;
           }
         
         symbolPtr = symbolPtr->next;
        }
     }
     
   cl_print("werror","SymbolIndex error\n");
   return(-1);
  }
  
/*****************************************/
/* FunctionIndex: Returns the index of a */
/*   function in the function array.     */
/*****************************************/
long int FunctionIndex(functionPtr,numberOfFunctions,functionArray)
  struct funtab *functionPtr;
  long int numberOfFunctions;
  struct funtab *functionArray[];
  {
   long int count = 0, i;
   
   for (i = 0; i < numberOfFunctions; i++)
     {
      if (functionPtr == functionArray[i]) return(count);
      if (functionArray[i] != NULL) count++;
     }
     
   cl_print("werror","FunctionIndex error\n");
   return(-1);
  }

/*****************************************************/
/* BsaveAllExpressions: Writes all expression needed */
/*   for this binary image to the binary save file.  */
/*****************************************************/
BsaveAllExpressions(fp,numberOfFunctions,functionArray)
  FILE *fp;
  long int numberOfFunctions;
  struct funtab *functionArray[];
  {
#if DEFFACTS_CONSTRUCT
   struct dfact *deflist;
#endif
   long int count = 0;
   
   /*==================================*/
   /* Write out number of expressions. */
   /*==================================*/
   
   GenWrite(&ExpressionCount,(unsigned long) sizeof(long int),fp);
   if (ExpressionCount == 0) return;
   
   /*=============================*/
   /* Write Deffacts Expressions. */
   /*=============================*/
   
#if DEFFACTS_CONSTRUCT  
   deflist = get_next_deffact(NULL);
   while (deflist != NULL)
     {
      BsaveExpression(deflist->alist,&count,
                      numberOfFunctions,functionArray,fp);
      deflist = deflist->next;
     }
#endif
     
   /*================================*/
   /* Save rule network expressions. */
   /*================================*/
   
   BsavePatternExpressions(network_pointer(),&count,
                          numberOfFunctions,functionArray,fp,PATTERNS);
                          
   BsavePatternExpressions(network_pointer(),&count,
                          numberOfFunctions,functionArray,fp,JOINS);
  }
 
/****************************************************/
/* BsavePatternExpressions: Saves expressions found */
/*   in the pattern network to the binary file.     */
/****************************************************/
BsavePatternExpressions(pat_ptr,count,numberOfFunctions,functionArray,
                       fp,which)
  struct pat_node *pat_ptr;
  long int *count;
  long int numberOfFunctions;
  struct funtab *functionArray[];
  FILE *fp;
  int which;
  {
   while (pat_ptr != NULL)
     {
      if ((pat_ptr->eval != NULL) && (which == PATTERNS))
        { 
         BsaveExpression(pat_ptr->eval,count,
                         numberOfFunctions,functionArray,fp);
        }
        
      if ((pat_ptr->next_level == NULL) && (which == JOINS))
        {
         BsaveJoinExpressions(pat_ptr->path,count,numberOfFunctions,
                             functionArray,fp,FALSE);
        }
      else
        { 
         BsavePatternExpressions(pat_ptr->next_level,count,
                                numberOfFunctions,functionArray,fp,which);
        }
        
      pat_ptr = pat_ptr->same_level;
     }
  }
  
/**************************************************/
/* BsaveJoinExpressions:  Saves expressions found */
/*   in the join network to the binary file.      */                                            
/**************************************************/
BsaveJoinExpressions(list_ptr,count,numberOfFunctions,functionArray,fp,flag)
  struct list *list_ptr;
  long int *count;
  long int numberOfFunctions;
  struct funtab *functionArray[];
  FILE *fp;
  int flag;
  {
   while (list_ptr != NULL)
     {
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         if (list_ptr->path->eval != NULL)
           { 
            BsaveExpression(list_ptr->path->eval,count,
                            numberOfFunctions,functionArray,fp); 
           }
           
         if (list_ptr->path->not_eval != NULL)
           {  
            BsaveExpression(list_ptr->path->not_eval,count,
                            numberOfFunctions,functionArray,fp);
           }
         BsaveJoinExpressions(list_ptr->path->next,count,numberOfFunctions,
                             functionArray,fp,TRUE);
        }
      list_ptr = list_ptr->next;
     }
  }
  
/*****************************************************/
/* BsaveDeffacts: Writes out all deffacts structures */ 
/*   to the binary file                              */
/*****************************************************/
BsaveDeffacts(fp)
  FILE *fp;
  {
   unsigned long int count = 0;
#if DEFFACTS_CONSTRUCT
   struct dfact *deflist;
   struct dfact newDfact;
   
   /*===================================*/
   /* Write out the number of deffacts. */
   /*===================================*/
  
   deflist = get_next_deffact(NULL);
   while (deflist != NULL)
     {
      count++;
      deflist = deflist->next;
     }
   GenWrite(&count,(unsigned long) sizeof(unsigned long int),fp);
   if (count == 0) return;
     
   /*=========================*/
   /* Write out each deffact. */
   /*=========================*/
   
   count = 0;
   deflist = get_next_deffact(NULL);
   while (deflist != NULL)
     {
      newDfact.name = NULL;
      newDfact.pp_form = NULL;
      if (deflist->alist != NULL)
        { 
         newDfact.alist = (struct test *) ExpressionCount;
         ExpressionCount += ExpressionSize(deflist->alist); 
        }
      else
        { newDfact.alist = (struct test *) -1L; }
        
      count++;
      if (deflist->next != NULL)
        { newDfact.next = (struct dfact *) count; }
      else
        { newDfact.next = (struct dfact *) -1L; }
        
      GenWrite(&newDfact,(unsigned long) sizeof(struct dfact),fp);
      deflist = deflist->next;
     }
#else
   GenWrite(&count,(unsigned long) sizeof(unsigned long int),fp);
#endif
  }
  
/************************************************************************/
/* BsavePatterns: Saves the pattern node structures to the binary file. */
/************************************************************************/
BsavePatterns(pat_ptr,fp)
  struct pat_node *pat_ptr;
  FILE *fp;
  {
   struct pat_node tempNode;
   
   while (pat_ptr != NULL)
     {
      tempNode.type = pat_ptr->type;
      tempNode.alpha = NULL;
      if (pat_ptr->eval != NULL)
        { 
         tempNode.eval = (struct test *) ExpressionCount;
         ExpressionCount += ExpressionSize(pat_ptr->eval); 
        }
      else
        { tempNode.eval = (struct test *) -1L; }
        
      tempNode.path = (struct list *) FindListPointer(pat_ptr->path);
      tempNode.next_level = (struct pat_node *) 
                            FindPatternPointer(pat_ptr->next_level);
      tempNode.same_level = (struct pat_node *) 
                            FindPatternPointer(pat_ptr->same_level);
      tempNode.prev = (struct pat_node *) 
                      FindPatternPointer(pat_ptr->prev);
      tempNode.last_level = (struct pat_node *) 
                            FindPatternPointer(pat_ptr->last_level);
        
      GenWrite(&tempNode,(unsigned long) sizeof(struct pat_node),fp);
      
      if (pat_ptr->next_level != NULL)
        { BsavePatterns(pat_ptr->next_level,fp); }
      
      pat_ptr = pat_ptr->same_level;
     }
  }

/*************************************************************/
/* BsaveLists: Saves the list structures to the binary file. */
/*************************************************************/
BsaveLists(pat_ptr,fp)
  struct pat_node *pat_ptr;
  FILE *fp;
  {   
   while (pat_ptr != NULL)
     {
      if (pat_ptr->next_level != NULL)
        { BsaveLists(pat_ptr->next_level,fp); }
      else
        { WriteLists(pat_ptr->path,fp,FALSE); }
      
      pat_ptr = pat_ptr->same_level;
     }
  }  

/***********************************************************/
/* WriteLists: Writes a list structure to the binary file. */
/***********************************************************/
WriteLists(list_ptr,fp,flag)
  struct list *list_ptr;
  FILE *fp;
  int flag;
  {
   struct list tempList;
   
   while (list_ptr != NULL)
     {
      tempList.next = (struct list *) FindListPointer(list_ptr->next);
      tempList.path = (struct internode *) FindJoinPointer(list_ptr->path);
      
      GenWrite(&tempList,(unsigned long) sizeof(struct list),fp);
      
      if ((list_ptr->path->lhs_log == 'e') || flag)
        { WriteLists(list_ptr->path->next,fp,TRUE); }
        
      list_ptr = list_ptr->next;
     }
  }
  
/*************************************************************/
/* BsaveJoins: Saves the join structures to the binary file. */
/*************************************************************/
BsaveJoins(pat_ptr,fp)
  struct pat_node *pat_ptr;
  FILE *fp;
  {
   while (pat_ptr != NULL)
     {
      if (pat_ptr->next_level != NULL)
        { BsaveJoins(pat_ptr->next_level,fp); }
      else
        { WriteJoins(pat_ptr->path,fp,FALSE); }
      
      pat_ptr = pat_ptr->same_level;
     }
  }  

/**************************************************************/
/* WriteJoins: Writes the join structures to the binary file. */
/**************************************************************/
WriteJoins(list_ptr,fp,flag)
  struct list *list_ptr;
  FILE *fp;
  int flag;
  {
   struct internode tempJoin;
   struct internode *joinPtr;
   
   while (list_ptr != NULL)
     {
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {  
         joinPtr = list_ptr->path;
         tempJoin.beta = NULL;
         tempJoin.lhs_log = joinPtr->lhs_log;
         tempJoin.rhs_log = joinPtr->rhs_log;
         tempJoin.id = 0;
         tempJoin.join_above = (struct internode *) 
                               FindJoinPointer(joinPtr->join_above);
         tempJoin.entry_pat = (struct pat_node *)
                              FindPatternPointer(joinPtr->entry_pat);
         tempJoin.next = (struct list *)
                         FindListPointer(joinPtr->next);
         if (joinPtr->eval != NULL)
           { 
            tempJoin.eval = (struct test *) ExpressionCount;
            ExpressionCount += ExpressionSize(joinPtr->eval); 
           }
         else
           { tempJoin.eval = (struct test *) -1L; }
           
         if (joinPtr->not_eval != NULL)
           { 
            tempJoin.not_eval = (struct test *) ExpressionCount;
            ExpressionCount += ExpressionSize(joinPtr->not_eval); 
           }
         else
           { tempJoin.not_eval = (struct test *) -1L; }
   
         GenWrite(&tempJoin,(unsigned long) sizeof(struct internode),fp);
         WriteJoins(list_ptr->path->next,fp,TRUE); 
        }
        
      list_ptr = list_ptr->next;
     }
  }
  
/*********************************************************************/
/* GetPatternPointers: Stores the pattern node pointers in an array. */
/*********************************************************************/
GetPatternPointers(pat_ptr)
  struct pat_node *pat_ptr;
  {
   while (pat_ptr != NULL)
     {
      PatternIndex[PatternCount++] = pat_ptr;
     
      if (pat_ptr->next_level == NULL)
        { GetJoinPointers(pat_ptr->path,FALSE); }
      else
        { GetPatternPointers(pat_ptr->next_level); }
        
      pat_ptr = pat_ptr->same_level;
     }
  }
  
/******************************************************************/
/* GetJoinPointers: Saves the list and join structures in arrays. */
/******************************************************************/
GetJoinPointers(list_ptr,flag)
  struct list *list_ptr;
  int flag;
  {
   while (list_ptr != NULL)
     {
      ListIndex[ListCount++] = list_ptr;
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         JoinIndex[JoinCount] = list_ptr->path;
         JoinCount++;
         GetJoinPointers(list_ptr->path->next,TRUE);
        }
      list_ptr = list_ptr->next;
     }
  }
  
/**********************************************************************/
/* FindPatternPointer: Returns the index to a pattern node structure. */
/**********************************************************************/
long int FindPatternPointer(patPtr)
  struct pat_node *patPtr;
  {
   long int i;
   
   if (patPtr == NULL) return(-1L);
   
   for (i = 0; i < PatternCount; i++)
     { if (patPtr == PatternIndex[i]) return(i); }
   
   /*=========================================*/
   /* Internal code error if this is reached. */
   /*=========================================*/
   
   return(-1L);
  }
  
/***********************************************************/
/* FindJoinPointer: Returns the index to a join structure. */
/***********************************************************/
long int FindJoinPointer(joinPtr)
  struct internode *joinPtr;
  {
   long int i;
   
   if (joinPtr == NULL) return(-1L);
   
   for (i = 0; i < JoinCount; i++)
     { if (joinPtr == JoinIndex[i]) return(i); }
     
   /*=========================================*/
   /* Internal code error if this is reached. */
   /*=========================================*/
   
   return(-1L);
  }
  
/***********************************************************/
/* FindListPointer: Returns the index to a list structure. */
/***********************************************************/
long int FindListPointer(listPtr)
  struct list *listPtr;
  {
   long int i;
   
   if (listPtr == NULL) return(-1);
   
   for (i = 0; i < ListCount; i++)
     { if (listPtr == ListIndex[i]) return(i); }

   /*=========================================*/
   /* Internal code error if this is reached. */
   /*=========================================*/
   
   return(-1L);
  }
  
/****************************************************/
/* GenWrite: Generic routine for writing to a file. */
/*   No machine specific code as of yet.            */
/****************************************************/
GenWrite(dataPtr,size,fp)
  char *dataPtr;
  unsigned long size;
  FILE *fp;
  {
   if (size == 0) return;
   fwrite(dataPtr,(unsigned int) size,1,fp);
  }  

/********************************************************************/
/* CountSymbols: Returns the number of symbols in the symbol table. */
/********************************************************************/
long int CountSymbols()
  {
   int i;
   struct draw **symbolArray;
   struct draw *symbolPtr;
   unsigned long int numberOfSymbols = 0;
   
   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/
   
   symbolArray = get_symbol_table();
   
   /*============================*/
   /* Get the number of symbols. */
   /*============================*/
   
   for (i = 0; i < HASHSIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         if (symbolPtr->count > 0)
           { numberOfSymbols++; }
         symbolPtr = symbolPtr->next;
        }
     }
     
   /*==========================*/
   /* Return the symbol count. */
   /*==========================*/
   
   return(numberOfSymbols);
  }
  
/********************************************************************/ 
/* SymbolNumber: Returns the index to a symbol in the symbol table. */
/********************************************************************/     
long int SymbolNumber(symbolPtr)
  struct draw *symbolPtr;
  {
   int i = 0;
   long int count = 0;
   struct draw **symbolArray;
   struct draw *tempPtr;
   
   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/
   
   symbolArray = get_symbol_table();
   
   /*========================*/
   /* Search for the symbol. */
   /*========================*/
   
   for (i = 0; i < HASHSIZE; i++)
     {
      tempPtr = symbolArray[i];
      while (tempPtr != NULL)
        {
         if (tempPtr == symbolPtr) return(count);
         if (tempPtr->count > 0) count++;
         tempPtr = tempPtr->next;
        }
     }
   
   /*================================*/
   /* Return -1 if symbol not found. */
   /*================================*/
   
   return(-1L);
  }
  
/****************************************************************/
/* BsaveSymbols: Saves the symbols needed for this binary image */
/*   to the binary file.                                        */
/****************************************************************/
int *BsaveSymbols(fp,numberOfSymbols)
  FILE *fp;
  long int *numberOfSymbols;
  {
   long int i;
   int *tempArray;
   
   /*======================================================*/
   /* Determine the number of symbols in the symbol table. */
   /*======================================================*/
   
   *numberOfSymbols = CountSymbols();
   
   /*===============================================*/
   /* Allocate an array to store the symbol counts. */
   /*===============================================*/
   
   tempArray = (int *)
               genlongalloc((long) sizeof(int) * (*numberOfSymbols));
   for (i = 0; i < *numberOfSymbols; i++) tempArray[i] = 0;
   
   /*====================================*/
   /* Find the symbols currently in use. */
   /*====================================*/
   
   FindNeededSymbols(tempArray);

   /*=========================================*/
   /* Write out the symbols currently in use. */
   /*=========================================*/
   
   WriteNeededSymbols(fp,*numberOfSymbols,tempArray);
   
   /*================================*/
   /* Return the symbol count array. */
   /*================================*/
   
   return(tempArray);
  }
  
  
/*******************************************************************/
/* FindNeededSymbols: Finds the symbols used by this binary image. */
/*******************************************************************/
FindNeededSymbols(countArray)
  int *countArray;
  {
#if DEFFACTS_CONSTRUCT
   struct dfact *deflist;
   
   /*========================*/
   /* Find Deffacts symbols. */
   /*========================*/
  
   deflist = get_next_deffact(NULL);
   while (deflist != NULL)
     {
      MarkNeededSymbols(countArray,deflist->alist);
      deflist = deflist->next;
     }
#endif
    
   /*===============================*/
   /* Find symbols in rule network. */
   /*===============================*/
   
   FindPatternSymbols(network_pointer(),countArray);
  }
  
/****************************************************************/
/* MarkNeededSymbols: Examines an expression to determine which */
/*   symbols are necessary to evaluate the expression.          */
/****************************************************************/
MarkNeededSymbols(countArray,testPtr)
  int *countArray;
  struct test *testPtr;
  {
   long int number;
   
   while (testPtr != NULL)
     {
      if ((testPtr->type == WORD) || (testPtr->type == STRING))
        {
         number = SymbolNumber(testPtr->val.hvalue);
         countArray[number]++;
        }
      else if (testPtr->type == FCALL)
        {  
         if (testPtr->arg_list != NULL)
           { MarkNeededSymbols(countArray,testPtr->arg_list); }
        }
        
      testPtr = testPtr->next_arg;
     }
  }
  
/**************************************************************************/
/* FindPatternSymbols: Looks for symbols required by the pattern network. */
/**************************************************************************/
FindPatternSymbols(pat_ptr,countArray)
  struct pat_node *pat_ptr;
  int *countArray;
  {
   while (pat_ptr != NULL)
     {
      if (pat_ptr->eval != NULL)
        { MarkNeededSymbols(countArray,pat_ptr->eval); }
        
      if (pat_ptr->next_level == NULL)
        { FindJoinSymbols(pat_ptr->path,countArray,FALSE); }
      else
        { FindPatternSymbols(pat_ptr->next_level,countArray); }
        
      pat_ptr = pat_ptr->same_level;
     }
  }
  
/********************************************************************/
/* FindJoinSymbols: Looks for symbols required by the join network. */                             
/********************************************************************/
FindJoinSymbols(list_ptr,countArray,flag)
  struct list *list_ptr;
  int *countArray;
  int flag;
  {
   while (list_ptr != NULL)
     {
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         if (list_ptr->path->eval != NULL)
           { MarkNeededSymbols(countArray,list_ptr->path->eval); }
           
         if (list_ptr->path->not_eval != NULL)
           { MarkNeededSymbols(countArray,list_ptr->path->not_eval); }
           
         FindJoinSymbols(list_ptr->path->next,countArray,TRUE);
        }
      list_ptr = list_ptr->next;
     }
  }

/*****************************************************************/
/* WriteNeededSymbols: Stores all of the symbols in the symbol   */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
WriteNeededSymbols(fp,totalSymbols,countArray)
  FILE *fp;
  long int totalSymbols;
  int *countArray;
  {
   int i, length, j;
   struct draw **symbolArray;
   struct draw *symbolPtr;
   unsigned long int numberOfSymbols = 0, size = 0;
   
   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/
   
   symbolArray = get_symbol_table();
   
   /*======================================================*/
   /* Get the number of symbols and the total string size. */
   /*======================================================*/
   
   for (i = 0, j = 0; i < HASHSIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         if (symbolPtr->count > 0)
           {
            if (countArray[j] > 0)
              {
               numberOfSymbols++;
               size += strlen(symbolPtr->contents) + 1;
              }
            j++;
           }
         symbolPtr = symbolPtr->next;
        }
     }
     
   /*=============================================*/
   /* Write out the symbols and the string sizes. */
   /*=============================================*/
    
   GenWrite(&numberOfSymbols,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&size,(unsigned long) sizeof(unsigned long int),fp);
   
   for (i = 0, j = 0; i < HASHSIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         if (symbolPtr->count > 0)
           {
            if (countArray[j] > 0)
              {
               length = strlen(symbolPtr->contents) + 1;
               GenWrite(symbolPtr->contents,(long) length,fp);
              }
            j++;
           }
         symbolPtr = symbolPtr->next;
        }
     }
   
   /*=============================*/
   /* Write out the count values. */
   /*=============================*/
   
   for (i = 0; i < totalSymbols; i++)
     {
      if (countArray[i] > 0)
        { GenWrite(&countArray[i],(unsigned long) sizeof(int),fp); }
     }
  }
   
#endif
