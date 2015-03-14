/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    BLOAD MODULE                     */
   /*******************************************************/
   
#include "setup.h"

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)

#include <stdio.h>

#include "clipsmem.h"
#include "constant.h"
#include "access.h"
#include "deffacts.h"

#if MAC_LSC
#undef TRUE
#undef FALSE
#include <MacTypes.h>
#include <FileMgr.h>
#endif

#if IBM_TBC || IBM_MSC || IBM_ZTC
#include <fcntl.h>
#endif  

/***************/
/* DEFINITIONS */
/***************/
  
#define ExpressionPointer(i) (((int) (i) == -1) ? NULL : \
                                   &ExpressionArray[(int) (i)])    
#define PatternPointer(i) (((int) i == -1) ? NULL : \
                                   &PatternArray[(int) i])    
#define JoinPointer(i) (((int) i == -1) ? NULL : \
                                   &JoinArray[(int) i])    
#define ListPointer(i) (((int) i == -1) ? NULL : \
                                   &ListArray[(int) i])
         
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/                          
                                
   struct funtab         **ReadNeededFunctions();
   struct draw           **ReadNeededSymbols();
   struct test            *BloadExpressions();
   struct funtab          *fast_find_function();
   char                   *genlongalloc();
   int                     ClearBload();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   extern struct funtab   *get_fctn_list();
   extern struct pat_node *network_pointer(); 
   extern struct funtab   *find_function();
   extern struct draw     *add_symbol();
   
#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   extern FILE            *fopen();
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static int                bloadActive = FALSE;
   
   static unsigned long      NumberOfPatterns;
   static struct pat_node   *PatternArray;
   static unsigned long      NumberOfJoins;
   static struct internode  *JoinArray;
   static unsigned long      NumberOfLists;
   static struct list       *ListArray;
   static struct test       *ExpressionArray;
   static unsigned long      NumberOfExpressions;
   static struct dfact      *DeffactsArray;
   static unsigned long      NumberOfDeffacts;
   
#if MAC_LSC
   static OSErr            resultCode;
   static int              vRefNum;
   static int              refNum;
   static Str255           volName;
   static Str255           fileName;
#endif

#if IBM_TBC || IBM_MSC || IBM_ZTC
   static int              fileHandle;
#endif

#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   static FILE            *fp;
#endif

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   char                     *BinaryIDString = "\1\2\3\4CLIPSV4.30";
  
/********************************************************/
/* bload_command: Performs the top level bload command. */
/********************************************************/
int bload_command()
  {
   char *file_name;
   VALUE arg_ptr;
      
   /*============================================*/
   /* Check for appropriate number of arguments. */
   /*============================================*/
   
   if (arg_num_check("bload",EXACTLY,1) == -1) return;

   /*==========================================================*/
   /* Get the file name from which to load binary information. */
   /*==========================================================*/
  
   if (arg_type_check("bload",1,STRING,&arg_ptr) == FALSE) return;
   file_name = rvalstring(arg_ptr);

   /*===============================================*/
   /* Call the bload function to perform the bload. */
   /*===============================================*/
   
   bload(file_name);
  }
  
/*******************************************************/
/* bload: Loads the binary representation of the CLIPS */
/*   environment from a file.                          */
/*******************************************************/
bload(file_name)
  char *file_name;
  {
   unsigned long int numberOfFunctions;
   int error;
   struct funtab **functionArray;
   struct draw **symbolArray;
   unsigned long int numberOfSymbols;
   char IDbuffer[20];
   
   /*================*/
   /* Open the file. */
   /*================*/
   
   if (GenOpen(file_name) == 0) return(FALSE);
   
   /*=====================================*/
   /* Determine if this is a binary file. */
   /*=====================================*/
   
   GenRead(IDbuffer,(unsigned long) strlen(BinaryIDString) + 1);
   if (strcmp(IDbuffer,BinaryIDString) != 0)
     {
      cl_print("werror","File ");
      cl_print("werror",file_name);
      cl_print("werror"," is not a binary rule file\n");
      GenClose();
      return(FALSE);
     }
     
   /*====================*/
   /* Clear environment. */
   /*====================*/
   
   clear_clips();
#if DEFFACTS_CONSTRUCT
   remove_all_deffacts(); 
#endif

   /*====================================================*/
   /* Read in the functions needed by this binary image. */
   /*====================================================*/

   functionArray = ReadNeededFunctions(&numberOfFunctions,&error);
   if (error)
     {
      GenClose();
      return(FALSE);
     }
     
   if (functionArray == NULL) 
     {
      GenClose();
      return(FALSE);
     }
   
   /*==================================================*/
   /* Read in the symbols needed by this binary image. */
   /*==================================================*/
      
   symbolArray = ReadNeededSymbols(&numberOfSymbols,&error);
   if (symbolArray == NULL) 
     {
      genlongfree(functionArray,(long) sizeof(struct funtab *) * numberOfFunctions);
      GenClose();
      return(FALSE);
     }
     
   /*======================================================*/
   /* Read in the expressions needed by this binary image. */
   /*======================================================*/
     
   ExpressionArray = BloadExpressions(functionArray,symbolArray,&error);
      
   /*===================================================*/
   /* Read in the deffacts stored in this binary image. */
   /*===================================================*/
     
   BloadDeffacts();
   
   /*=======================================================*/
   /* Read in the rule network stored in this binary image. */
   /*=======================================================*/
     
   BloadNetwork();
    
   /*=================*/
   /* Close the file. */
   /*=================*/
   
   GenClose();

   /*==================*/
   /* Free up storage. */
   /*==================*/
   
   genlongfree(functionArray,(long) sizeof(struct funtab *) * numberOfFunctions);
   genlongfree(symbolArray,(long) sizeof(struct draw *) * numberOfSymbols);
     
   /*============================================*/
   /* Add a clear function to remove binary load */
   /* when a clear command is issued.            */
   /*============================================*/
   
   bloadActive = TRUE;
   add_clear_function("bload",ClearBload);
   
   /*==========================================*/
   /* Return TRUE to indicate load successful. */
   /*==========================================*/
   
   return(TRUE);
  }

/********************************************************/
/* ReadNeededFunctions: Reads in the names of functions */
/*   needed by the binary image.                        */
/********************************************************/
struct funtab **ReadNeededFunctions(numberOfFunctions,error)
  long int *numberOfFunctions;
  int *error;
  {
   char *functionNames, *namePtr;
   unsigned long int space, count, i, temp;
   struct funtab **functionArray, *functionPtr;
   int functionsNotFound = 0;
   
   /*====================================================*/
   /* Determine the number of function names to be read. */
   /*====================================================*/
   
   GenRead(&count,(unsigned long) sizeof(long int));
   if (count == 0) return(NULL);
   
   /*=========================================================*/
   /* Determine the amount of space needed for the functions. */
   /*=========================================================*/
    
   GenRead(&space,(unsigned long) sizeof(long int));
  
   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/
   
   functionNames = genlongalloc(space);
   GenRead(functionNames,(unsigned long) space);
   
   /*====================================================*/
   /* Store the function pointers in the function array. */
   /*====================================================*/
   
   temp = (long) sizeof(struct funtab *) * count;
   functionArray = (struct funtab **) genlongalloc(temp);
   namePtr = functionNames;
   functionPtr = NULL;
   for (i = 0; i < count; i++)
     {
      if ((functionPtr = fast_find_function(namePtr,functionPtr)) == NULL)
        {
         if (! functionsNotFound) 
           { 
            cl_print("werror","The following undefined functions are ");
            cl_print("werror","referenced by this binary image:\n");
           }
           
         cl_print("werror","   ");
         cl_print("werror",namePtr);
         cl_print("werror","\n");
         functionsNotFound = 1;
        }
      
      functionArray[i] = functionPtr;
      namePtr += strlen(namePtr) + 1;
     }

   /*==========================================*/
   /* Free the memory used by the name buffer. */
   /*==========================================*/
   
   genlongfree(functionNames,space);
   
   /*==================================================*/
   /* If any of the required functions were not found, */
   /* then free the memory used by the function array. */
   /*==================================================*/
   
   if (functionsNotFound)
     {
      genlongfree(functionArray,temp);
      functionArray = NULL;
     }
     
   /*===================================*/
   /* Set globals to appropriate values */
   /* and return the function array.    */
   /*===================================*/
   
   *numberOfFunctions = count;
   *error = functionsNotFound;
   return(functionArray);
  }
  
/*************************************************/
/* fast_find_function: Search the CLIPS function */
/*   list for a specific function.               */
/*************************************************/
struct funtab *fast_find_function(fun_name,end_function)
  char *fun_name;
  struct funtab *end_function;
  {
   extern struct funtab *get_fctn_list();
   struct funtab *fun_list, *fun_ptr;

   fun_list = get_fctn_list();
   if (fun_list == NULL) { return(NULL); }

   if (end_function != NULL)
     { fun_ptr = end_function->next; }
   else
     { fun_ptr = fun_list; }
     
   while (strcmp(fun_name,fun_ptr->fun_name) != 0) 
     { 
      fun_ptr = fun_ptr->next;
      if (fun_ptr == end_function) return(NULL);
      if (fun_ptr == NULL) fun_ptr = fun_list;
     }

   return(fun_ptr);
  }
  
/************************************************/
/* ReadNeededSymbols: Reads in the symbols used */
/*   by the binary image.                       */
/************************************************/
struct draw **ReadNeededSymbols(numberOfSymbols,error)
  unsigned long int *numberOfSymbols;
  int *error;
  {
   char *symbolNames, *namePtr;
   unsigned long space, temp;
   unsigned long i;
   struct draw **symbolArray;
   int *countArray;
   
   /*==================================================*/
   /* Determine the number of symbol names to be read. */
   /*==================================================*/
   
   GenRead(numberOfSymbols,(unsigned long) sizeof(unsigned long int));
   if (*numberOfSymbols == 0) return(NULL);
   
   /*=======================================================*/
   /* Determine the amount of space needed for the symbols. */
   /*=======================================================*/
    
   GenRead(&space,(unsigned long) sizeof(unsigned long int));
  
   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/
   
   symbolNames = genlongalloc(space);
   GenRead(symbolNames,space);
   
   /*================================================*/
   /* Store the symbol pointers in the symbol array. */
   /*================================================*/
   
   symbolArray = (struct draw **) 
                 genlongalloc((long) sizeof(struct draw *) * (long) *numberOfSymbols);
   namePtr = symbolNames;
   for (i = 0; i < *numberOfSymbols; i++)
     {
      symbolArray[i] = add_symbol(namePtr);
      namePtr += strlen(namePtr) + 1;
     }
     
   /*=======================*/
   /* Free the name buffer. */
   /*=======================*/
   
   genlongfree(symbolNames,space);
   
   /*===========================*/
   /* Update the symbol counts. */
   /*===========================*/
   
   temp = (long) sizeof(int) * (long) *numberOfSymbols;
   countArray = (int *) genlongalloc(temp);
   GenRead(countArray,temp);
   
   for (i = 0; i < *numberOfSymbols; i++)
     { symbolArray[i]->count += countArray[i]; }
   genlongfree(countArray,temp);
   
   /*==========================*/
   /* Return the symbol array. */
   /*==========================*/

   return(symbolArray);
  }

/**********************************************************************/
/* Bload Expressions: Loads in the expressions (i.e. test structures) */
/*   used by the binary image.                                        */
/**********************************************************************/
struct test *BloadExpressions(functionArray,symbolArray,error)
  struct funtab *functionArray[];
  struct draw *symbolArray[];
  int *error;
  {
   unsigned long space, i;
   long index;
   struct test *testArray;
   
   /*=================================================*/
   /* Determine the number of expressions to be read. */
   /*=================================================*/
   
   GenRead(&NumberOfExpressions,(unsigned long) sizeof(long int));
   if (NumberOfExpressions == 0) return(NULL);
     
   /*==========================*/
   /* Read in the expressions. */
   /*==========================*/
   
   space = NumberOfExpressions * sizeof(struct test);
   testArray = (struct test *) genlongalloc(space);
   GenRead(testArray,space);
   
   /*=================*/
   /* Reset pointers. */
   /*=================*/
   
   for (i = 0; i < NumberOfExpressions; i++)
     {
      switch(testArray[i].type)
        {
         case FCALL:
           index = (long int) testArray[i].val.fun_ptr;
           testArray[i].val.fun_ptr = functionArray[index];
           break;
         
         case WORD:
         case STRING:
           index = (long int) testArray[i].val.hvalue;
           testArray[i].val.hvalue = symbolArray[index];
           break;
        }
     
      index = (int) testArray[i].next_arg;
      if (index == -1L)
        { testArray[i].next_arg = NULL; }
      else
        { testArray[i].next_arg = &testArray[index]; }
        
      index = (int) testArray[i].arg_list;
      if (index == -1L)
        { testArray[i].arg_list = NULL; }
      else
        { testArray[i].arg_list = &testArray[index]; }
     
     }
   
   /*==============================*/
   /* Return the expression array. */
   /*==============================*/
   
   return(testArray);
  }
  
/****************************************************************/
/* BloadDeffacts: Loads the deffacts used by this binary image. */
/****************************************************************/
BloadDeffacts()
  {
   unsigned long int space, i;
   long int index;
   
   /*=============================*/
   /* Get the number of deffacts. */
   /*=============================*/
  
   GenRead(&NumberOfDeffacts,(unsigned long) sizeof(unsigned long int));
   if (NumberOfDeffacts == 0)
     {
      DeffactsArray = NULL;
      return;
     }
     
   /*=======================*/
   /* Read in the deffacts. */
   /*=======================*/
   
   space = NumberOfDeffacts * sizeof(struct dfact);
   DeffactsArray = (struct dfact *) genlongalloc(space);
   GenRead(DeffactsArray,space);
           
   /*==========================*/
   /* Reset deffacts pointers. */
   /*==========================*/

#if DEFFACTS_CONSTRUCT   
   for (i = 0; i < NumberOfDeffacts; i++)
     {
      DeffactsArray[i].alist = ExpressionPointer(DeffactsArray[i].alist);
     
      
      index = (long int) DeffactsArray[i].next;
      if (index != -1)
        { DeffactsArray[i].next = &DeffactsArray[index]; }
      else
        { DeffactsArray[i].next = NULL; }
     }
     
   /*============================================*/
   /* Set the deffacts to the new deffacts list. */
   /*============================================*/
   
   set_deflist(DeffactsArray);
#else
   genlongfree(DeffactsArray,space);
   DeffactsArray = NULL;
#endif
  }
  
/*******************************************************/
/* BloadNetwork: Loads in the join and pattern network */
/*   needed for this binary image.                     */
/*******************************************************/
BloadNetwork()
  {
   unsigned long space, i;   
   
   /*====================================================*/
   /* Determine the number of structures in the network. */
   /*====================================================*/
  
   GenRead(&NumberOfPatterns,(unsigned long) sizeof(long int));
   GenRead(&NumberOfJoins,(unsigned long) sizeof(long int));
   GenRead(&NumberOfLists,(unsigned long) sizeof(long int));
   
   if ((NumberOfPatterns == 0) ||
       (NumberOfJoins == 0) ||
       (NumberOfLists == 0)) return;
       
   /*===================*/
   /* Load the network. */
   /*===================*/
   
   space = NumberOfPatterns * sizeof(struct pat_node);
   PatternArray = (struct pat_node *) genlongalloc(space);
   GenRead(PatternArray,space);
        
   space = NumberOfJoins * sizeof(struct internode);
   JoinArray = (struct internode *) genlongalloc(space);
   GenRead(JoinArray,space);
        
   space = NumberOfLists * sizeof(struct list);
   ListArray = (struct list *) genlongalloc(space);
   GenRead(ListArray,space);
        
   /*=================================*/
   /* Reset pattern network pointers. */
   /*=================================*/
   
   for (i = 0; i < NumberOfPatterns; i++)
     {
      PatternArray[i].same_level = PatternPointer(PatternArray[i].same_level);
      PatternArray[i].next_level = PatternPointer(PatternArray[i].next_level);
      PatternArray[i].last_level = PatternPointer(PatternArray[i].last_level);
      PatternArray[i].prev       = PatternPointer(PatternArray[i].prev);
      PatternArray[i].eval       = ExpressionPointer(PatternArray[i].eval);
      PatternArray[i].path       = ListPointer(PatternArray[i].path);
     }
     
   /*==============================*/
   /* Reset join network pointers. */
   /*==============================*/
   
   for (i = 0; i < NumberOfJoins; i++)
     {
      JoinArray[i].eval = ExpressionPointer(JoinArray[i].eval);
      JoinArray[i].not_eval = ExpressionPointer(JoinArray[i].not_eval);
      JoinArray[i].next = ListPointer(JoinArray[i].next);
      JoinArray[i].join_above = JoinPointer(JoinArray[i].join_above);
      JoinArray[i].entry_pat = PatternPointer(JoinArray[i].entry_pat);
     }
   
   /*======================*/
   /* Reset list pointers. */
   /*======================*/
   
   for (i = 0; i < NumberOfLists; i++)
     {
      ListArray[i].path = JoinPointer(ListArray[i].path);
      ListArray[i].next = ListPointer(ListArray[i].next);
     }
     
   /*=============================================*/
   /* Set the network pointer to the new network. */
   /*=============================================*/
   
   set_network_pointer(PatternArray);
  }
  
/*****************************************/
/* GenOpen: Generic and machine specific */
/*   code for opening a file.            */
/*****************************************/
GenOpen(file_name)
  char *file_name;
  {
#if MAC_LSC
   
   resultCode = GetVol(volName,&vRefNum);
   if (BIOCheck(resultCode)) 
     {
      open_error_message("bload",file_name);
      return(0);
     }
   strcpy(fileName,file_name);
   CtoPstr(fileName);
   resultCode = FSOpen(fileName,vRefNum,&refNum);
   PtoCstr(fileName);
   if (BIOCheck(resultCode)) 
     {
      open_error_message("bload",file_name);
      return(0);
     }

#endif

#if IBM_TBC || IBM_MSC
   fileHandle = open(file_name,O_RDONLY | O_BINARY);
   if (fileHandle == -1)
     {
      open_error_message("bload",file_name);
      return(0);
     }
#endif

#if IBM_ZTC
   fileHandle = open(file_name,O_RDONLY);
   if (fileHandle == -1)
     {
      open_error_message("bload",file_name);
      return(0);
     }
#endif

#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   if ((fp = fopen(file_name,"rb")) == NULL)
     {
      open_error_message("bload",file_name);
      return(0);
     }
#endif

   return(1);
  }
  
/*****************************************/
/* GenRead: Generic and machine specific */
/*   code for reading from a file.       */
/*****************************************/
GenRead(dataPtr,size)
  char *dataPtr;
  unsigned long size;
  {
#if MAC_LSC
   unsigned long dataSize;
   
   dataSize = size;
   resultCode = FSRead(refNum,&dataSize,dataPtr);
#endif

#if IBM_TBC || IBM_MSC || IBM_ZTC
   read(fileHandle,dataPtr,(unsigned int) size);
#endif

#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   unsigned int temp, number_of_reads, read_size;
   
   if (sizeof(int) == sizeof(long))
     { read_size = size; }
   else
     { read_size = (1L << (sizeof(int) * 8L)) - 1L ; }
   number_of_reads = size / read_size;
   temp = size - ((long) number_of_reads * (long) read_size);
   
   while (number_of_reads > 0)
     {
      fread(dataPtr,read_size,1,fp);
      dataPtr += read_size;
      number_of_reads--;
     }
     
   fread(dataPtr,temp,1,fp);
#endif
  }
  
/*******************************************/
/* GenClose:  Generic and machine specific */
/*   code for closing a file.              */
/*******************************************/
GenClose()
  {
#if MAC_LSC
   FSClose(refNum);
#endif

#if IBM_TBC || IBM_MSC || IBM_ZTC
   close(fileHandle);
#endif

#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   fclose(fp);
#endif
  }
  
#if MAC_LSC
/***********************************************/
/* BIOCHECK: Check for I/O error on Macintosh. */
/***********************************************/
BIOCheck(resultCode)
  OSErr resultCode;
  {
   if (resultCode == noErr) return(0);
   return(1);
  }
#endif

/************************************************/
/* GENLONGALLOC: Allocates blocks of memory for */
/*   sizes expressed using long integers.       */
/************************************************/
char *genlongalloc(size)
  long size;
  {
   int test;
#if MAC_LSC
   extern char *mlalloc();
#endif

#if IBM_TBC || IBM_ZTC
   extern char *farmalloc();
#endif

#if IBM_MSC
   extern char *halloc();
#endif

   if (sizeof(int) == sizeof(long))
     { return(genalloc(size)); }
     
#if MAC_LSC || IBM_TBC || IBM_MSC || IBM_ZTC
   update_mem_used(size);
   update_mem_requests(1);
#endif

#if MAC_LSC
   return(mlalloc(size));
#endif  

#if IBM_TBC || IBM_ZTC
   return(farmalloc(size));
#endif

#if IBM_MSC
   return(halloc(size,1));
#endif
  
#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   test = (int) size;
   if (test != size) return(NULL);
   
   return(genalloc(test));
#endif
  }
  
/*********************************************/
/* GENLONGFREE: Returns blocks of memory for */
/*   sizes expressed using long integers.    */
/*********************************************/
genlongfree(ptr,size)
  char *ptr;
  long size;
  {
   int test;

   if (sizeof(int) == sizeof(long))
     { return(genfree(ptr,size)); }
         
#if MAC_LSC || IBM_TBC || IBM_MSC || IBM_ZTC
   update_mem_used(-size);
   update_mem_requests(-1);
#endif

#if MAC_LSC
   return(free(ptr));
#endif  
  
#if IBM_TBC || IBM_ZTC
   return(farfree(ptr));
#endif

#if IBM_MSC
   return(hfree(ptr));
#endif

#if (! MAC_LSC) && (! IBM_TBC) && (! IBM_MSC) && (! IBM_ZTC)
   test = (int) size;
   if (test != size) return(-1);
   
   return(genfree(ptr,test));
#endif
  }
  
/***********************************************************/
/* BLOADED: Returns TRUE if the current environment is the */
/*   result of a bload command, otherwise returns FALSE.   */
/***********************************************************/
bloaded()
  {
   return(bloadActive);
  }

/*****************************************************************/
/* ClearBload: Clears a binary image from the CLIPS environment. */
/*****************************************************************/
ClearBload()
  {
   unsigned long int i, space;
   
   /*==========================*/
   /* Update the symbol table. */
   /*==========================*/
   
   for (i = 0; i < NumberOfExpressions; i++)
     {
      if ((ExpressionArray[i].type == WORD) || 
          (ExpressionArray[i].type == STRING))
        { dec_symbol_count(ExpressionArray[i].val.hvalue); }
     }
     
   /*=====================*/
   /* Free binary arrays. */
   /*=====================*/ 
  
   space = NumberOfExpressions * sizeof(struct test);
   if (space != 0) genlongfree(ExpressionArray,space);
   
   space = NumberOfDeffacts * sizeof(struct dfact);
   if (space != 0) genlongfree(DeffactsArray,space);  
   
   space = NumberOfPatterns * sizeof(struct pat_node);
   if (space != 0) genlongfree(PatternArray,space);  
        
   space = NumberOfJoins * sizeof(struct internode);
   if (space != 0) genlongfree(JoinArray,space);  
        
   space = NumberOfLists * sizeof(struct list);
   if (space != 0) genlongfree(ListArray,space);  
   
   /*=========================*/
   /* Reset network pointers. */
   /*=========================*/

#if DEFFACTS_CONSTRUCT 
   set_deflist(NULL);
   createinitial();
#endif
   set_network_pointer(NULL);
   
   /*==================================*/
   /* Remove the bload clear function. */
   /*==================================*/
   
   bloadActive = FALSE;
   remove_clear_function("bload");
  }
#endif
