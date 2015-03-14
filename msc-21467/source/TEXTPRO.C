/*   CLIPS Version 4.30   4/25/89 */

/**************************************************************************/
/**************************************************************************/
/*                        LOOKUP TABLE FUNCTIONS                          */
/*                                                                        */
/* The functions contained in this file set up and access a hierarchical  */
/* lookup system for multiple files.                                      */
/*                                                                        */
/* For usage see external documentation.                                  */
/**************************************************************************/
/**************************************************************************/

#include <stdio.h>
#include "setup.h"
#include "clips.h"     /*CLIPS function and constant definitions       */

#if CLP_TEXTPRO || CLP_HELP

#define NAMESIZE 80
#define NULLCHAR '\0'
#define NUM_OFFSET ('0')
#define CASE_OFFSET ('a' - 'A')
#define BLANK (' ')
#define TAB ('\t')
#define LNFEED ('\n')


/*=========================================================*/
/*Status returns for the file loading and lookup functions */
/*=========================================================*/
#define NORMAL 0          /*Entry information found in file       */
#define NO_FILE -10       /*File not found for reference          */
#define NEW_FILE -15      /*File loaded onto internal lookup table*/
#define OLD_FILE -20      /*File was already on the lookup table  */

#define NO_TOPIC -25      /*No entry information was found in file*/
#define EXIT -30          /*Branch-up from root; exit lookup table*/
#define BRANCH_UP -35     /*Move up from subtopic entry to parent */
#define BRANCH_DOWN -40   /*Move down from main topic to subtopic */

/*=================*/
/*Entry data types */
/*=================*/
#define MENU -45    /*Entry has subtopics*/
#define INFO -50    /*Entry is a leaf; contains only information*/

/*==========================================*/
/*Entry node type for internal lookup table */
/*==========================================*/
struct entries 
  {
   int level;              /*Level of entry node in the lookup tree  */
   int type;               /*Entry node data type : menu or info     */
   char name[NAMESIZE];    /*Entry node name                         */
   long int offset;        /*Location of entry info in the file      */
   struct entries *child;  /*Address of list of subtopic entries     */
   struct entries *parent; /*Address of parent topic entry           */
   struct entries *next;   /*Address of next entry at the same level */
  };

/*=========================================*/
/*File node type for internal lookup table */
/*=========================================*/
struct lists 
  {
   char file[NAMESIZE];       /*File name                                */
   struct entries *topics;    /*Address of list of entry topics for file */
   struct entries *curr_menu; /*Address of current main topic in file    */
   struct lists *next;        /*Address of next file in the table        */
  };

/*==============================================*/
/*Address of the first file in the lookup table */
/*==============================================*/
static struct lists *headings = NULL;

/*==================================================*/
/*Delimeter strings for marking entries in the file */
/*==================================================*/
#define BDELIM "BEGIN-ENTRY-"
#define BDLEN 12
#define EDELIM "END-ENTRY"
#define EDLEN 9
#define BFORMAT "%d%1s%12s%s"   /*Format string for sscanf*/
#define LIT_DELIM ('$')

/*==================*/
/*Internal routines */
/*==================*/
int load();            /*Adds a file into the lookup table   */
int unload();          /*Deletes a file from the lookup table*/
FILE *get_entries();   /*Finds a topic in the lookup table   */
FILE *get_curr_menu(); /*Finds the current topic for a file  */
char *grab_string();   /*Gets topic information from the file*/

int findstr();                  /*Used to search strings             */
int upper();                    /*Capitalizes words                  */
struct lists *file_node();      /*Allocates a node on the file list  */
struct entries *entries_node(); /*Allocates a topic-entry node       */
int attach_leaf();              /*Stores a topic node in the tree    */
long int lookup();              /*Finds a topic in the tree          */
int toss();                     /*Deletes an entry-file from the tree*/

/*======================================================*/
/*General memory allocation functions ported from CLIPS */
/*======================================================*/
extern char *genalloc();
extern int genfree();




/******************************************************************************/
/*============================================================================*/
/*                             INTERNAL ROUTINES                              */
/*============================================================================*/
/******************************************************************************/

/****************************************************************************/
/*LOAD FUNCTION :                                                           */
/* Input : 1) name of file to be loaded into the lookup table               */
/*         2) caller-allocated buffer to contain an error message (if any)  */
/*         3) size of error message buffer                                  */
/* Output :                                                                 */
/* This function attempts to load the file's topic information into the     */
/* lookup table according to the format below :                             */
/*                                                                          */
/* <level-num><entry-type-code>BEGIN-ENTRY-<topic-name>                     */
/*                .                                                         */
/*                .                                                         */
/*    Entry information in the form in which                                */
/*    it is to be displayed when referenced.                                */
/*                .                                                         */
/*                .                                                         */
/* END-ENTRY                                                                */
/*                                                                          */
/* The function returns the number of entries loaded if the entire file was */
/*   was correctly formatted, else it returns 0. An error message is stored */
/*   in the caller's buffer.                                                */
/****************************************************************************/
static int load(file,errbuf,bufsize)
  char *file, *errbuf;
  int bufsize;
  {
   FILE *fp;                     /*Pointer into stream of input file      */
   char str[256];                /*Buffer for storing input file lines    */
   int INFO_BEGIN, INFO_END;     /*Flags used to check proper syntax      */
   struct lists *lnode;          /*Used to store file node in list        */
   struct entries *enode;        /*Used to store entry node in topic list */
   int line_ct;                  /*Line count - used for error messages   */
   int entries_ct;               /*Number of entries successfully loaded. */

   if (bufsize > 0)
     errbuf[0] = NULLCHAR;
   fp = fopen(file,"r");
   if (fp == NULL)
     {
      if (bufsize >= 60)
        sprintf(errbuf,"Could not open file \"%s\".",file);
      return(0);
     }
   if ((lnode = file_node(file)) == NULL)
     {
      if (bufsize >= 60)
        sprintf(errbuf,"File \"%s\" already loaded.",file);
      return(0);
     }
   
   /*===========================*/
   /*Store the file entry topics*/
   /*===========================*/
   line_ct = 0;
   entries_ct = 0;

   INFO_BEGIN = FALSE;
   INFO_END = TRUE;
   while (fgets(str,256,fp) != NULL)
     {
      line_ct++;

      /*=============================================================*/
      /*Forces the load function to ignore lines beginning with `$$' */
      /*=============================================================*/
      if ((str[0] != LIT_DELIM) || (str[1] != LIT_DELIM))

        if (findstr(str,EDELIM) >=0)
          {
           if (INFO_BEGIN == TRUE)
             {
              INFO_BEGIN = FALSE;
              INFO_END = TRUE;
              entries_ct++;
             }
           else
             {
              fclose(fp);
              unload(file);
              if (bufsize >= 60)
                sprintf(errbuf,"Line %d : Unmatched end marker.",line_ct);
              return(0);
             }
          }
        else if (findstr(str,BDELIM) >= 0)
          {
           if (INFO_END == TRUE)
              {
               INFO_END = FALSE;
               INFO_BEGIN = TRUE;
              }
            else
              {
               fclose(fp);
               unload(file);
               if (bufsize >= 60)
                 sprintf(errbuf,"Line %d : Previous entry not closed.",line_ct);
               return(0);
              }

           if ((enode=entries_node(fp,file,str,errbuf,bufsize,line_ct))==NULL)
             return(0);

           /*=================================*/
           /*Store new entry node in the tree */
           /*=================================*/
           if (attach_leaf(lnode,enode,fp,file,errbuf,bufsize,line_ct) == FALSE)
             return(0);
          }
     }
   fclose(fp);
   if (INFO_END == FALSE)
     {
      unload(file);
      if (bufsize >= 60)
        sprintf(errbuf,"Line %d : Previous entry not closed.",line_ct);
      return(0);
     }
   return(entries_ct);
  }

/******************************************************************************/
/*FUNCTION UNLOAD :                                                           */
/* Input : 1) name of file to be taken off the lookup table                   */
/* Output : This functions deletes a file and all entry-topics associated with*/
/*          it from the lookup table and returns a boolean flag indicating    */
/*          failure or success.                                               */
/******************************************************************************/
static int unload(file)
  char *file;
  {
   struct lists *plptr, *clptr;
   int l_flag;

   clptr = headings;
   plptr = clptr;
   if (clptr != NULL)
     if (strcmp(clptr->file,file) != 0)
       l_flag = 1;
     else
       l_flag = 0;
   else
     l_flag = 0;
   while (l_flag > 0)
     {
      plptr = clptr;
      clptr = clptr->next;
      if (clptr != NULL)
        if (strcmp(clptr->file,file) != 0)
          l_flag = 1;
        else
          l_flag = 0;
      else
        l_flag = 0;
     }
   if (clptr == NULL)
     return(FALSE);
   
   toss(clptr->topics);

   if (plptr == clptr)
     headings = clptr->next;
   else
     plptr->next = clptr->next;
   genfree(clptr,sizeof(struct lists));
   return(TRUE);
  }

/******************************************************************************/
/*FUNCTION GET_ENTRIES :                                                      */
/* Input : 1) name of file to be accessed for lookup of entry                 */
/*         2) caller allocated buffer for main topic name                     */
/*         3) name of the entry to be accessed in the file                    */
/*         4) caller allocated buffer for a status code (see LOOKUP).         */
/* Output : 1) returns a pointer into the stream of the lookup file which     */
/*             indicates the starting position of the lookup information      */
/*             (NULL if the topic was not found)                              */
/* This function passes its input directly to LOOKUP.  See its description    */
/* for further detail.                                                        */
/*                                                                            */
/******************************************************************************/
static FILE *get_entries(file,menu,name,code)
  char *file, **menu, *name;
  int *code;
  {
   FILE *fp;          /*Lookup file stream*/
   long int offset;   /*Offset from beginning of file to beginning of topic*/

   offset = lookup(file,menu,name,code);
   if (offset < 0)
      return(NULL);
   fp = fopen(file,"r");
   if (fp == NULL)
     {
      *code = NO_FILE;
      return(NULL);
     }
   if (fseek(fp,offset,0) < 0)
     {
      *code = NO_FILE;
      return(NULL);
     }
   return(fp);
  }

/******************************************************************************/
/*FUNCTION GET_CURR_MENU :                                                    */
/* Input : 1) name of file to be accessed                                     */
/*         2) caller allocated buffer for the current menu name               */
/*         3) caller allocated buffer for status code : NO_FILE, NO_TOPIC, or */
/*            NORMAL.                                                         */
/* Output : 1) returns a pointer into the file stream indicating the beginning*/
/*             of the description of the current menu for the named file      */
/*             (returns NULL if there is no current menu)                     */
/******************************************************************************/
static FILE *get_curr_menu(file,menu,status)
  char *file, **menu;
  int *status;
  {
   struct lists *lptr;   /*Used in searching the file list*/
   FILE *fp;             /*File stream*/
   int l_flag;           /*Used in looping through the file list*/

   /*=====================================*/
   /*Find the named file in the file list */
   /*=====================================*/
   lptr = headings;
   if (lptr != NULL)
     if (strcmp(lptr->file,file) != 0)
       l_flag = 1;
     else
       l_flag = 0;
   else
     l_flag = 0;
   while (l_flag > 0)
     {
      lptr = lptr->next;
      if (lptr != NULL)
        if (strcmp(lptr->file,file) != 0)
          l_flag = 1;
        else
          l_flag = 0;
      else
        l_flag = 0;
     }
   if (lptr == NULL)
     {
      *status = NO_FILE;
      return(NULL);
     }

   /*============================================================*/
   /*Position the pointer in the file stream to the current menu */
   /*============================================================*/
   if (lptr->curr_menu == NULL)
     {
      *status = NO_TOPIC;
      return(NULL);
     }
   if ((fp = fopen(file,"r")) == NULL)
     {
      *status = NO_FILE;
      return(NULL);
     }
   if (fseek(fp,lptr->curr_menu->offset,0) < 0)
     {
      *status = NO_FILE;
      return(NULL);
     }
   *status = NORMAL;
   return(fp);
  }

/******************************************************************************/
/*FUNCTION GRAB_STRING :                                                      */
/* Input : 1) file stream pointer                                             */
/*         2) caller allocated buffer for storage of read string              */
/*         3) size of caller's buffer                                         */
/* Output : This function grabs a line of text from the currently opened      */
/*          lookup file at the given file position in the stream.  If it      */
/*          encounters EOF or the closing topic delimeter, it closes the file */
/*          and returns NULL.  Otherwise, the return value is simply the      */
/*          address of the caller's buffer.                                   */
/*                                                                            */
/* Notes : 1) This function expects a file pointer into a stream of a file    */
/*            already opened!!                                                */
/*         2) The caller must close the file himself if he wishes to          */
/*            prematurely abort the complete reading of an entry.             */
/******************************************************************************/
static char *grab_string(fp,buf,bufsize)
  FILE *fp;
  char *buf;
  int bufsize;
  {
   if (fgets(buf,bufsize,fp) == NULL)
     {
      fclose(fp);
      return(NULL);
     }
   if ((buf[0] == LIT_DELIM) && (buf[1] == LIT_DELIM))
     {
      buf[0] = BLANK;
      buf[1] = BLANK;
     }
   else if (findstr(buf,EDELIM) >= 0)
     {
      buf = NULL;
      fclose(fp);
     }
   return(buf);
  }

/**************************************************************************/ 
/*FINDSTR FUNCTION :                                                      */
/* Input : 1) string to be searched                                       */
/*         2) string to be found                                          */
/* Output : 1) returns index of string-1 where string-2 started, if found */
/*          2) returns -1, if not found                                   */
/**************************************************************************/ 
static int findstr(s,t) 
  char *s, *t; 
  {
   int i,j,k;

   for (i = 0; s[i] != '\0'; i++) 
     {
      for (j = i, k = 0; t[k] != '\0' && s[j] == t[k]; j++, k++) ;
      if ((t[k] == '\0')&&(k != 0))
        return(i);
     }                 
   return(-1);
  }

/**********************************************************************/
/*UPPER FUNCTION :                                                    */
/* Input : 1) alphanumeric string                                     */
/* Output : 1) all alphabetic characters of string are capitalized    */
/**********************************************************************/
static int upper(str)
  char *str;
  {
   int index = 0;

   for ( ; str[index] != NULLCHAR; index++)
     if ((str[index] >= 'a') && (str[index] <= 'z'))
       str[index] -= CASE_OFFSET;
  }

/******************************************************************************/
/*FILE_NODE FUNCTION :                                                        */
/* Input : 1) name of a file                                                  */
/* Output : 1) returns address of an initalized file_node, if the file was    */
/*             not already on the lookup table                                */
/*          2) returns the null address, if the file was already present      */
/******************************************************************************/
static struct lists *file_node(file)
  char *file;
  {
   struct lists *lptr, *lnode;

   if (headings != NULL)
     {
      lptr = headings;
      while (lptr->next != NULL)
        {
         if (strcmp(lptr->file,file) == 0)
           return(NULL);
         lptr = lptr->next;
        }
      if (strcmp(lptr->file,file) == 0)
        return(NULL);
     }
   lnode = (struct lists *) genalloc (sizeof(struct lists));
   strcpy(lnode->file,file);
   lnode->topics = NULL;
   lnode->curr_menu = NULL;
   lnode->next = NULL;
   if (headings == NULL)
     headings = lnode;
   else
     lptr->next = lnode;
   return(lnode);
  }

/******************************************************************************/
/*ENTRIES_NODE FUNCTION :                                                     */
/* Input : 1) file pointer                                                    */
/*         2) file name                                                       */
/*         3) input string from the file                                      */
/*         4) buffer for error messages                                       */
/*         5) size of the error message buffer                                */
/*         6) line count in the file                                          */
/* Output :                                                                   */
/*This function scans the input string for the appropriate topic entry        */
/*delimeter and, if it finds this to be correct, allocates a new entry node,  */
/*and initializes it, and returns the address to the calling routine.  If an  */
/*error is detected, the function writes an appropriate message to the        */
/*caller's buffer, deallocates the node, deletes all previous nodes from the  */
/*current file from the lookup table, closes the file, and returns the null   */
/*address.                                                                    */
/******************************************************************************/
static struct entries *entries_node(fp,file,str,errbuf,bufsize,line_ct)
  FILE *fp;
  char *str, *file, *errbuf;
  int bufsize, line_ct;
  {
   struct entries *enode;
   char bmarker[BDLEN+1],  /*Entry topic delimiting strings         */
        t_code[2];         /*Type of entry flag : menu or info      */
   

   /*================================================================*/
   /*Allocate a new node and scan the delimeter string for tree info */
   /*================================================================*/
   enode = NULL;
   enode = (struct entries *) genalloc (sizeof(struct entries));
   if (sscanf(str,BFORMAT,
              &enode->level,t_code,bmarker,enode->name) != 4)
     {
      genfree(enode,sizeof(struct entries));
      fclose(fp);
      unload(file);
      if (bufsize >= 60)
        sprintf(errbuf,"Line %d : Invalid delimeter string.",line_ct);
      return(NULL);
     }
   if (t_code[0] == 'M')
     enode->type = MENU;
   else if (t_code[0] == 'I')
     enode->type = INFO;
   else
     {
      genfree(enode,sizeof(struct entries));
      fclose(fp);
      unload(file);
      if (bufsize >= 60)
        sprintf(errbuf,"Line %d : Invalid entry type.",line_ct);
      return(NULL);
     }
   if (strcmp(bmarker,BDELIM) != 0)
     {
      genfree(enode,sizeof(struct entries));
      fclose(fp);
      unload(file);
      if (bufsize >= 60)
        sprintf(errbuf,"Line %d : Invalid delimeter string.",line_ct);
      return(NULL);
     }

   /*===============================================================*/
   /* For systems which have record file systems (such as VMS),     */
   /* the following statement is necessary to move the file pointer */
   /* to the beginning of the next record.                          */
   /*===============================================================*/
   ungetc(getc(fp),fp);
   enode->offset = ftell(fp);

   enode->parent = NULL;
   enode->child  = NULL;
   enode->next = NULL;
   upper(enode->name);

   return(enode);
  }

/******************************************************************************/
/*FUNCTION ATTACH_LEAF :                                                      */
/* Input : 1) address of current file_node                                    */
/*         2) address of current topic entry-node                             */
/*         3) file pointer                                                    */
/*         4) name of file                                                    */
/*         5) error message buffer                                            */
/*         6) size of error message buffer                                    */
/*         7) line count in the file                                          */
/* Output :                                                                   */
/*This function attaches the entry-node to its proper place in the tree of the*/
/*current file.  The function returns a boolean flag indicating the success   */
/*(or lack thereof) of this connection.  In the case of an error, an error    */
/*message is written to the caller's buffer, the file is closed, and the      */
/*previous file entries are deleted from the lookup table.                    */
/******************************************************************************/
static int attach_leaf(lnode,enode,fp,file,errbuf,bufsize,line_ct)
  struct lists *lnode;
  struct entries *enode;
  FILE *fp;
  char *file, *errbuf;
  int bufsize, line_ct;
  {
   static struct entries *parent = NULL;
                 /*Address of previous topic-entry loaded from the file. */
                 /*Must be static to preserve value across function calls*/
   int p_flag;   /*Used in searching the tree for a parent*/

    
   /*====================*/
   /*First topic for file*/
   /*====================*/
   if (lnode->topics == NULL)
     lnode->topics = enode;

   /*================================*/
   /*Subtopic - branch down the tree */
   /*================================*/
   else if (enode->level > parent->level)
     if (parent->type == MENU)
       {
        enode->parent = parent;
        parent->child = enode;
       }
     else
       {
        genfree(enode,sizeof(struct entries));
        fclose(fp);
        unload(file);
        if (bufsize >= 60)
          sprintf(errbuf,
                "Line %d : Non-menu entries cannot have subtopics.",
                line_ct);
        return(FALSE);
       }
   /*====================================*/
   /*Brother-topic -- same level in tree */
   /*====================================*/
   else if (enode->level == parent->level)
     {
      enode->parent = parent->parent;
      enode->next = parent->next;
      parent->next = enode;
     }

   /*==========================================================*/
   /*Topic is unrelated to previous topic - branch up the tree */
   /*==========================================================*/
   else
     {
      if (parent != NULL)
        p_flag = 1;
      else
        p_flag = 0;
      while (p_flag > 0)
        {
         parent = parent->parent;
         if (parent != NULL)
           if (enode->level < parent->level)
             p_flag = 1;
           else
             p_flag = 0;
         else
           p_flag = 0;
        }
      if (parent != NULL)

        /*========*/
        /*Subtopic*/
        /*========*/
        if (parent->level < enode->level)
          {
           enode->parent = parent;
           enode->next = parent->child;
           parent->child = enode;
          }

        /*=============*/
        /*Brother-topic*/
        /*=============*/
        else
          {
           enode->parent = parent->parent;
           enode->next = parent->next;
           parent->next = enode;
          }

      /*=========*/
      /*Root Node*/
      /*=========*/
      else
        {
         enode->parent = NULL;
         enode->next = lnode->topics;
         lnode->topics = enode;
        }
     }
   parent = enode; 
   return(TRUE);
  }

/******************************************************************************/
/*FUNCTION LOOKUP :                                                           */
/* Input : 1) name of entry-topic file to be used for reference               */
/*         2) caller allocated buffer to contain the main topic name          */
/*         3) name of the entry-topic to be found                             */
/*         4) caller allocated buffer to store the return status              */
/* Output : 1) offset from the beginning of the entry-topic file stream to the*/
/*             beginning of the requested topic (-1 if the topic not found)   */
/*          2) status code stored in caller's buffer indicating the result of */
/*             the lookup : NO_FILE, NO_TOPIC, BRANCH_UP, BRANCH_DOWN, EXIT,  */
/*             or NORMAL.                                                     */
/*                                                                            */
/* Notes : 1) If NULL is given as an entry-topic, the lookup routine branches */
/*            up one level in the tree (status BRANCH_UP).  If the current    */
/*            level of the tree is already the root, all paths are set to NULL*/
/*            (status EXIT).                                                  */
/*         2) If an entry-topic is not found, the file position of the current*/
/*            main topic (or menu) is returned (status NO_TOPIC).             */
/******************************************************************************/
static long int lookup(file,menu,name,code)
  char *file, **menu, *name;
  int *code;
  {
   struct lists *lptr;    /*Local pointers used to move through the tree*/
   struct entries *eptr;   
   int l_flag, e_flag;    /*Flags used in looping to find entry-topics*/

   /*===============================*/
   /*Find named file in lookup list */
   /*===============================*/
   lptr = headings;
   if (lptr != NULL)
     if (strcmp(lptr->file,file) != 0)
       l_flag = 1;
     else
       l_flag = 0;
   else
     l_flag = 0;
   while (l_flag > 0)
     {
      lptr = lptr->next;
      if (lptr != NULL)
        if (strcmp(lptr->file,file) != 0)
          l_flag = 1;
        else
          l_flag = 0;
      else
        l_flag = 0;
     }
   if (lptr == NULL)
     {
      *code = NO_FILE;
      return(-1);
     }

   /*==================================================================*/
   /*If entry-topic was NULL, branch up one-level in the tree, or exit */
   /*the tree if already at the root.                                  */
   /*==================================================================*/
   if (name == NULL)
     {
      if (lptr->curr_menu == NULL)
        {
         *code = EXIT;
         return(-1);
        }
      else
        {
         if (lptr->curr_menu->parent == NULL)
           {
            *code = EXIT;
            lptr->curr_menu = NULL;
            *menu = NULL;
            return(-1);
           }
         lptr->curr_menu = lptr->curr_menu->parent;
         *code = BRANCH_UP;
         *menu = lptr->curr_menu->name;
         return(lptr->curr_menu->offset);
        }
     }

   /*========================================*/
   /*Find the topic in the file's topic tree */
   /*========================================*/
   upper(name);
   if (lptr->curr_menu != NULL)
     eptr = lptr->curr_menu->child;
   else
     eptr = lptr->topics;
   if (eptr != NULL)
     if (findstr(eptr->name,name) == 0)
       e_flag = 0;
     else
       e_flag = 1;
   else
     e_flag = 0;
   while (e_flag > 0)
     {
      eptr = eptr->next;
      if (eptr != NULL)
        if (findstr(eptr->name,name) == 0)
          e_flag = 0;
        else
          e_flag = 1;
      else
        e_flag = 0;
     }

   /*===================================================================*/
   /*If the topic was not found, return the position of the current menu*/
   /*===================================================================*/
   if (eptr == NULL)
     {
      *code = NO_TOPIC;
      if (lptr->curr_menu != NULL)
        {
         *menu = lptr->curr_menu->name;
         return(lptr->curr_menu->offset);
        }
      return(-1);
     }

   /*===============================================================*/
   /*If the requested topic has children, branch down to its level. */
   /*===============================================================*/
   if (eptr->type == MENU)
     {
      *code = BRANCH_DOWN;
      lptr->curr_menu = eptr;
     }
   else
     *code = NORMAL;

   if (lptr->curr_menu != NULL)
      *menu = lptr->curr_menu->name;
   return(eptr->offset);
  }

/******************************************************************************/
/*FUNCTION TOSS :                                                             */
/* Input : 1) entry-topic address                                             */
/* Output : This function recursively deletes a node and all child nodes      */
/******************************************************************************/
static int toss(eptr)
  struct entries *eptr;
  {
   struct entries *prev;

   while (eptr != NULL)
     {
      if (eptr->child != NULL)
        toss(eptr->child);
      prev = eptr;
      eptr = eptr->next;
      genfree(prev,sizeof(struct entries));
     }
  }


/******************************************************************************/
/******************************************************************************/
/*                      CLIPS TEXT PROCESSING FUNCTIONS                       */
/*                                                                            */
/* The functions contained in this file can be called from CLIPS to handle    */
/* external file referencing and accessing.  cl_fetch() loads a file onto an  */
/* internal run-time lookup table, cl_toss() removes the file, cl_print_region*/
/* accesses the loaded file to display a requested entry, and cl_help()       */
/* provides an on-line help facility for CLIPS using the external help data   */
/* file specified in the header file CLIPS.H. For information on the format   */
/* of the data file(s) required, see the internal documentation in LOOKUP.C   */
/* and the external documentation.                                            */
/*                                                                            */
/* For usage of these functions, see the external documentation.              */
/******************************************************************************/
/******************************************************************************/

#define SCREEN_LN 22   /*Typical terminal screen length -- 22 lines*/
                       /*Used for scrolling in the help facility   */

/*==========================================*/
/*Topic node for help facility's query list */
/*==========================================*/
struct topics 
  {
   char name[NAMESIZE];      /*Name of the node                 */
   struct topics *end_list;  /*Pointer to end of query list     */
   struct topics *next;      /*Pointer to next topic in the list*/
  };

#if CLP_HELP

static int HELP_INIT = FALSE;   /*Flag used to indicate help file load status*/

#endif

/******************************************************************************/
/*============================================================================*/
/*                        FUNCTION DECLARATIONS                               */
/*============================================================================*/
/******************************************************************************/

/*===========================================*/
/* External access functions called by CLIPS */
/* See declarations in MAIN.C                */
/*===========================================*/

#if CLP_HELP

int cl_help();         /*Help facility function               */
int cl_help_path();    /*Changes the help entries file name   */

#endif

#if CLP_TEXTPRO

float cl_fetch();        /*Loads a file onto the lookup table   */
float cl_print_region(); /*Displays an entry from a loaded file */
float cl_toss();         /*Unloads a file from the lookup table */

#endif

/*======================================*/
/*Logical name "whelp" router functions */
/*======================================*/
#if CLP_HELP

int find_help();
int print_help();
int getc_help();
int ungetc_help();

#endif

/*==========================================================*/
/*Internal routines used by cl_help() and cl_print_region() */
/*==========================================================*/

#if !MAC_LSC
struct topics *cmd_line_topics();  

#if CLP_HELP
struct topics *query_topic();
#endif

FILE *find_topic();
#else
struct topics *cmd_line_topics();  

#if CLP_HELP
struct topics *query_topic();
#endif

FILE *find_topic();
#endif

#if CLP_HELP
static char *help_file = NULL;
#endif

/******************************************************************************/
/*============================================================================*/
/*                       EXTERNAL ACCESS FUNCTIONS                            */
/*============================================================================*/
/******************************************************************************/

/******************************************************************************/
/*FUNCTION CL_HELP : (CLIPS function help)                                    */
/* Input : Multiple or no topic requests may be passed to the help facility   */
/*         from the top level of CLIPS via a "stack" accessed by the CLIPS    */
/*         system routines num_args() and rstring().                          */
/* Output : This function loads the help file specified in CLIPS.H into a     */
/*          a hierarchical tree structure using the routines of LOOKUP.C.     */
/*          It then queries the user for topics, and, using the LOOKUP        */
/*          routines, branches through the tree, displaying information where */
/*          appropriate.  The function returns control to CLIPS once the user */
/*          has indicated an exit from the help tree.                         */
/*                                                                            */
/* For usage see external documentation.                                      */
/******************************************************************************/

#if CLP_HELP

int cl_help()
  {
   int status;                     /*Return code from the lookup routines */
   FILE *fp;                       /*Pointer in to the help file stream   */
   struct topics *main_topic,      /*Pointer to the first requested topic */
                 *tptr;            /*Used in deallocating the topic list  */
   char buf[256],   /*Buffer for storing input strings from the help file */
        *menu[1];   /*Buffer for the name of the current main topic       */
#if ! WINDOW_INTERFACE
   char termbuf[2]; /*Buffer for storing the terminators of a scroll      */
   int line_cnt;    /*Line count used for scrolling purposes              */
#endif
  
   if (HELP_INIT == FALSE)
     {
      if (help_file == NULL)
        {
         help_file = genalloc(strlen(HELP_DEFAULT) + 1);
         strcpy(help_file,HELP_DEFAULT);
        }
      cl_print("stdout","Loading help file entries from ");
      cl_print("stdout",help_file);
      cl_print("stdout",".\nPlease wait...\n");
      status = load(help_file,buf,256);
      if (status == 0)
        {
         cl_print("werror","Could not load help file.\n");
         cl_print("werror",buf);
         cl_print("werror","\n");
         return(FALSE);
        }
      else
        {
         /*==================================================================*/
         /*Enables logical name "whelp" as the destination for all help I/O  */
         /*==================================================================*/
         add_router("whelp",10,find_help,print_help,
                    getc_help,ungetc_help,NULL);
         HELP_INIT = TRUE;
        }
     }

   act_router("whelp");

   /*=====================================================================*/
   /*The root node of the help-tree is MAIN (see external documentation.) */
   /*Add this node to the front of the initial topic request list given   */
   /*by the user on the CLIPS top level command line.                     */
   /*=====================================================================*/
   main_topic = (struct topics *) genalloc (sizeof(struct topics));
   strcpy(main_topic->name,"MAIN");
   main_topic->next = cmd_line_topics();
   main_topic->end_list = NULL;

   cl_print("whelp","\n");

   /*============================*/
   /*Process user topic requests */
   /*============================*/
   do
     {
      fp = find_topic(help_file,main_topic,menu,&status);
      if (status == NO_FILE)
        {
         cl_print("werror","Unable to access help file.\n");
         break;
        }
      if (status == EXIT)
        break;
      if (status == NO_TOPIC)
        {
         if (fp == NULL)
           {
            /*===================================================*/
            /*The lookup routines return the file location of the*/
            /*current main topic if the requested topic is not   */
            /*found.  The help-tree has one root: MAIN (see      */
            /*external docs).  This topic should always be       */
            /*available.  Thus, if the topic was not found and   */
            /*there is no current menu, the help-file has been   */
            /*tampered with and should be corrected.             */
            /*===================================================*/
            cl_print("whelp","Root entry \"MAIN\" not found in ");
            cl_print("whelp",help_file);
            cl_print("whelp",".\nSee external documentation.\n");
            break;
           }
         cl_print("whelp","\nSorry, no information available.\n\n");
        }
      if (status != BRANCH_UP)
        {
#if ! WINDOW_INTERFACE
         line_cnt = 0;
#endif

         /*======================================================*/
         /*Print lines from the information entry stopping after */
         /*every screenful of lines.  The user at that point has */
         /*the option to continue or abort the entry to continue */
         /*at the current menu level.                            */
         /*======================================================*/
         while (grab_string(fp,buf,256) != NULL)
           {
#if ! WINDOW_INTERFACE
            if (line_cnt >= (SCREEN_LN + 1))
              {
               cl_print("whelp","PRESS <RETURN> FOR MORE. ");
               cl_print("whelp","PRESS <A>,<RETURN> TO ABORT.");
               do
                 {
                  termbuf[0] = cl_getc("whelp");
                  if (termbuf[0] != LNFEED)
                    {
                     if (termbuf[0] == 'a')
                       termbuf[0] = 'A';
                     termbuf[1] = cl_getc("whelp");
                    }
                 }
               while ((termbuf[0] != LNFEED) && (termbuf[0] != 'A'));
               line_cnt = 0;
               if (termbuf[0] == 'A')
                 {
                  fclose(fp);
                  break;
                 }
              }
            line_cnt++;
#endif
            cl_print("whelp",buf);
           }
        }
      else if (fp != NULL)
        /*==========================================================*/
        /*If the user branched-up the help-tree, don't reprint that */
        /*menu.  However, the help file still needs to be closed.   */
        /*==========================================================*/
        fclose(fp);

      main_topic = query_topic(main_topic,menu);
     } while (status != EXIT);
   deact_router("whelp");

   /*========================================================*/
   /*Release any space used by the user's topic request list */
   /*========================================================*/
   while (main_topic != NULL)
     {
      tptr = main_topic;
      main_topic = main_topic->next;
      genfree(tptr,sizeof(struct topics));
     }
  }

/***************************************************************************/
/*FUNCTION CL_HELP_PATH : (CLIPS function help-path)                       */
/* Input : Via the CLIPS "stack", the name of the new help entries file,   */
/*         or no input.                                                    */
/* Output : This function redefines the lookup file for the help facility. */
/*          If no argument is given, it displays the current file name.    */
/***************************************************************************/
int cl_help_path()
  {                    
   int arg_num;    /*Number of arguments passed to help-path */
   char *help_name;
   VALUE arg_ptr;
 
   if ((arg_num = arg_num_check("help-path",NO_MORE_THAN,1)) == -1) return;
   if (arg_num == 0)
     {
      cl_print("stdout","The current help entries file is ");
      if (help_file != NULL)
        cl_print("stdout",help_file);
      else
        cl_print("stdout",HELP_DEFAULT);
      cl_print("stdout","\n");
     }
   else if (arg_num == 1)
     {
      if (help_file != NULL)
        {
         if (HELP_INIT == TRUE)
           {
            cl_print("stdout","Releasing help entries from file ");
            cl_print("stdout",help_file);
            cl_print("stdout","...\n");
            unload(help_file);
            del_router("whelp");
            HELP_INIT = FALSE;
           }
         genfree(help_file,strlen(help_file) + 1);
        }
      if (arg_type_check("help-path",1,STRING,&arg_ptr) == FALSE) return;
      help_name = get_valstring(arg_ptr);
      help_file = genalloc(strlen(help_name) + 1);
      strcpy(help_file,help_name);
      cl_print("stdout","Help entries file reset to ");
      cl_print("stdout",help_name);
      cl_print("stdout","\n");
     }
  }

#endif

#if CLP_TEXTPRO

/***************************************************************************/
/*FUNCTION CL_FETCH : (CLIPS function fetch)                               */
/* Input : Name of the file to be stored in the lookup table - passed via  */
/*         the CLIPS "stack"                                               */
/* Output : This function loads a file into the internal lookup table and  */
/*          returns a (float) boolean flag indicating failure or success.  */
/***************************************************************************/
float cl_fetch()                          
  {
   char file[NAMESIZE],  /*File name                */
        buf[NAMESIZE];   /*Error message buffer     */
   int load_ct;          /*Number of entries loaded */
   VALUE arg_ptr;
 
   if (arg_num_check("fetch",EXACTLY,1) == -1) return ((float) FALSE);
   if (arg_type_check("fetch",1,STRING,&arg_ptr) == FALSE) return ((float) FALSE);
   strcpy(file,get_valstring(arg_ptr));
   load_ct = load(file,buf,NAMESIZE);
   if (load_ct == 0)
     {
      cl_print("werror","Unable to load file.\n");
      if (buf[0] != NULLCHAR)
        cl_print("werror",buf);
      else
        cl_print("werror","No entries found.");
      cl_print("werror","\n");
     }
   return((float) load_ct);
  }

/******************************************************************************/
/*FUNCTION CL_PRINT_REGION : (CLIPS function print-region)                    */
/* Input : Via the CLIPS "stack", logical name for the output, the name of the*/
/*         file to be accessed, and the name of the topic(s) to be looked up. */
/* Output : This function accesses a previously loaded file and prints the    */
/*          information of the topic entry requested to the screen.  The tree */
/*          structure must currently be at the correct level in order for the */
/*          topic to be accessed.  To branch down the tree, each topic in the */
/*          path to the one desired must be named.  Multiple arguments are    */
/*          allowed as in the help facility (see the external documentation.) */
/*          To branch up the tree, the special topic character `^' must be    */
/*          specified for each upwards branch.  Giving no topic name will     */
/*          cause a single branch-up in the tree.  The `?' character given at */
/*          the end of a path will return the current main topic menu.        */
/*                                                                            */
/* For usage, see the external documentation.                                 */
/******************************************************************************/
float cl_print_region()
  {  
   struct topics *params,    /*Lookup file and list of topic requests  */
                 *tptr;      /*Used in deallocating the parameter list */
   char buf[256];            /*Buffer for the topic entry strings      */
   FILE *fp;                 /*Stream for the input file               */
   char *menu[1];            /*Buffer for the current menu name        */
   int status,               /*Lookup status return code               */
       com_code;             /*Completion flag                         */
 
   if (arg_num_check("print-region",AT_LEAST,2) == -1) return ((float) FALSE);
     
   params = cmd_line_topics();                                                 
   fp = find_topic(params->next->name,params->next->next,menu,&status);
   if ((status != NO_FILE) && (status != NO_TOPIC) && (status != EXIT))
     {
      if (strcmp(params->name,"t") == 0)
        strcpy(params->name,"stdout");
      cl_print(params->name,"\n");
      while (grab_string(fp,buf,256) != NULL)
        cl_print(params->name,buf);
      com_code = TRUE;
     }
   else
     com_code = FALSE;
       
   /*========================================================*/
   /*Release any space used by the user's topic request list */
   /*========================================================*/
   while (params != NULL)
     {
      tptr = params;
      params = params->next;
      genfree(tptr,sizeof(struct topics));
     }

   return((float) com_code);
  }

/***************************************************************************/
/*FUNCTION CL_TOSS : (CLIPS function toss)                                 */
/* Input : Name of the file to be deleted from the lookup table (passed via*/
/*         the CLIPS "stack")                                              */
/* Output : This function deletes the named file from the lookup table and */
/*          returns a (float) boolean flag indicating failure or success.  */
/***************************************************************************/
float cl_toss()
  {
   char *file;   /*Name of the file */
   VALUE arg_ptr;
  
   if (arg_num_check("toss",EXACTLY,1) == -1) return ((float) FALSE);
  
   if (arg_type_check("toss",1,STRING,&arg_ptr) == FALSE) return ((float) FALSE);
   file = get_valstring(arg_ptr);
 
#if CLP_HELP
 
    if (help_file != NULL)
      if ((strcmp(file,help_file) == 0) && (HELP_INIT == TRUE))
        {
         HELP_INIT = FALSE;
         del_router("whelp");
        }
 
#endif
 
   return((float) unload(file));
  }

#endif

/******************************************************************************/
/* The following four functions are the router routines for the logical name  */
/* "whelp".  Currently, all they do is direct all accesses to standard I/O.   */
/******************************************************************************/

#if CLP_HELP

find_help(log_name)
  char *log_name;
  {
   if (strcmp(log_name,"whelp") == 0)
     return(TRUE);
   return(FALSE);
  }

print_help(log_name,str)
  char *log_name, *str;
  {
   cl_print("stdout",str);
  }

int getc_help(log_name)
  char *log_name;
  {
   return(cl_getc("stdin"));
  }

ungetc_help(ch, log_name)
  int ch;
  char *log_name;
  {
   cl_ungetc(ch,"stdin");
  }

#endif

/******************************************************************************/
/*============================================================================*/
/*                            INTERNAL ROUTINES                               */
/*============================================================================*/
/******************************************************************************/

/******************************************************************************/
/*FUNCTION CMD_LINE_TOPICS :                                                  */
/* Input : None                                                               */
/* Output : This function builds a linked list of topics requested by the     */
/*          user at the CLIPS level using the CLIPS "stack" routines,         */
/*          num_args() and rstring().  It returns the address of the top of   */
/*          the list or NULL if there were no command line topics.            */
/******************************************************************************/
static struct topics *cmd_line_topics()
  {
   extern char *num_to_string();

   int topic_num,         /*Number of topics specified by the user */
       index;             /*Used to loop through the topic list    */
   struct topics *head,   /*Address of the top of the topic list   */
                 *tnode,  /*Address of new topic node              */
                 *tptr;   /*Used to attach new node to the list    */
   struct values val;     /*Unknown-type CLIPS data structure      */
 
   head = NULL;
   topic_num = num_args();
   for (index = 1; index <= topic_num; index++)
     {
      tnode = (struct topics *) genalloc (sizeof(struct topics));
      runknown(index,&val);
      if (get_valtype(val) != NUMBER)
        strcpy(tnode->name,get_valstring(val));
      else
        strcpy(tnode->name,num_to_string(get_valfloat(val)));
      tnode->next = NULL;
      tnode->end_list = NULL;
      if (head == NULL)
        head = tnode;
      else
        {
         tptr = head;
         while (tptr->next != NULL)
           tptr = tptr->next;
         tptr->next = tnode;
        }
     }
    return(head);
  }

/******************************************************************************/
/*FUNCTION QUERY_TOPIC :                                                      */
/* Input : 1) The address of the old topic list (this routines writes over    */
/*            previously allocated memory, if available)                      */
/*         2) A buffer holding the name of the current menu in the tree       */
/* Output : This function prompts the user for a new set of topic(s) and      */
/*          displays the name of the current menu.  Each new topic is         */
/*          delineated by white-space, and this function builds a linked list */
/*          of these topics.  It returns the address of the top of this list. */
/******************************************************************************/

#if CLP_HELP

static struct topics *query_topic(old_list,menu)
  struct topics *old_list;
  char **menu;
  {
   int index, cnt,       /*Indices of the user input buffer and topic name */
       begin, end;       /*Flags indicating when to add a new list topic   */
   struct topics *main,  /*Address of the top of the topic list            */
                 *tnode, /*Address of the new topic node                   */
                 *tptr;  /*Used to add the new node to the topic list      */
   char list[256],       /*User input buffer                               */
        name[NAMESIZE];  /*Name of the new topic in the list               */

   /*==================================================================*/
   /*Read a line of input from the user (substituting blanks for tabs) */
   /*==================================================================*/
   cl_print("whelp",*menu);
   cl_print("whelp"," Topic? ");
   for ( index = 0; 
         ((list[index] = cl_getc("whelp")) != LNFEED) && (index < 254);
         index++ )
       {
        if (list[index] == TAB)
          list[index] = BLANK;
        else if ((list[index] == '\b') && (index != 0))
          index -= 2;
       }
   list[index] = BLANK; list[index+1] = NULLCHAR;
   cl_print("whelp","\n");


   /*=======================================*/
   /*Parse user buffer into separate topics */
   /*=======================================*/
   main = old_list;
   index = 0; cnt = 0;
   while ((list[index] != NULLCHAR) && (cnt < NAMESIZE))
     {
      begin = (list[index] != BLANK) ? TRUE : FALSE;
      end = ((begin == FALSE) && (cnt > 0)) ? TRUE : FALSE;
      if (begin == TRUE)
        {
         name[cnt] = list[index];
         cnt++;
        }
      if (end == TRUE)
        {
         begin = FALSE;
         name[cnt] = NULLCHAR;
         cnt = 0;

         /*==============================================*/
         /*Write over previous topic lists, if available */
         /*==============================================*/
         if (old_list != NULL)
           {
            strcpy(old_list->name,name);
            old_list = old_list->next;
           }
         else
           {
            tnode = (struct topics *) genalloc (sizeof(struct topics));
            strcpy(tnode->name,name);
            tnode->next = NULL;
            tnode->end_list = NULL;
            if (main == NULL)
              main = tnode;
            else
              {
               tptr = main;
               while (tptr->next != NULL)
                 tptr = tptr->next;
               tptr->next = tnode;
              }
           }
        }
      index++;
     }

  /*========================================================================*/
  /*If the new list is shorter than the previous one, we must mark the end. */
  /*========================================================================*/
  main->end_list = old_list;
  return(main);
 }


#endif

/******************************************************************************/
/*FUNCTION FIND_TOPIC :                                                       */
/* Input : 1) File to be searched for topic request                           */
/*         2) Address of topic request list                                   */
/*         3) Buffer for current menu name                                    */
/*         4) Lookup status return code                                       */
/* Output : This function flows through the user topic request path by        */
/*          calling the lookup routines.  When it reaches the last element,   */
/*          it returns a pointer into the stream of the lookup file           */
/*          indicating the beginning of the topic entry.  If any topic in the */
/*          path is not found, the function aborts and returns the address of */
/*          of the current menu in the lookup tree for the file.  The exact   */
/*          nature of the final lookup is indicated in the status buffer.     */
/******************************************************************************/
static FILE *find_topic(file,main_topic,menu,status)
  char *file;
  struct topics *main_topic;
  char **menu;
  int *status;
  {
   FILE *fp;                        /*Input file stream                    */
   struct topics *tptr,             /*Used to loop through the topic list  */
                 *end_list = NULL;  /*Address of the end of the topic list */

   if (main_topic != NULL)
     end_list = main_topic->end_list;
   else
     end_list = NULL;
   tptr = main_topic;
   if (tptr != end_list)
     do
       {

        /*======================*/
        /*Branch up in the tree */
        /*======================*/
        if (strcmp(tptr->name,"^") == 0)
          fp = get_entries(file,menu,NULL,status);

        /*=======================================================*/
        /*Return the cuurent main topic menu of the lookup table */
        /*=======================================================*/
        else if ((strcmp(tptr->name,"?") == 0) && (tptr->next == end_list))
          fp = get_curr_menu(file,menu,status);

        /*=====================*/
        /*Lookup topic request */
        /*=====================*/
        else
          fp = get_entries(file,menu,tptr->name,status);
        if (*status == NO_FILE)
          break;
        if (*status == NO_TOPIC)
          {
           fp = get_curr_menu(file,menu,status);
           *status = NO_TOPIC;
           break;
          }
        tptr = tptr->next;
       } while (tptr != end_list);
   else
     /*==================================================================*/
     /*An empty topic request list causes a single branch-up in the tree */
     /*==================================================================*/
     fp = get_entries(file,menu,NULL,status);
   return(fp);
  }

#endif
_