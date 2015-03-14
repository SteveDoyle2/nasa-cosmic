/*   CLIPS Version 4.30   4/25/89 */

   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    ROUTER MODULE                    */
   /*******************************************************/
   
#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "router.h"
#include "clipsmem.h"
  
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   int                         cl_print();
   int                         cl_getc();
   int                         cl_ungetc();
   int                         cl_exit();
   int                         add_router();
   int                         del_router();
   int                         log_namep();
   int                         query_router();
   int                         deact_router();
   int                         act_router();
   int                         set_fast_load();
   int                         set_fast_save();
   int                         open_str_source();
   int                         close_str_source();
   int                         str_fnd();
   int                         str_getc();
   int                         str_ungetc();
   struct str_router         *fnd_str_router();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static struct router      *route_list = NULL;
   static struct str_router  *str_route_list = NULL;
   static FILE               *FAST_LOAD = NULL;
   static FILE               *FAST_SAVE = NULL;
   static int                 Abort;
   
/*##############################*/
/*##############################*/
/*######                  ######*/
/*###### ROUTER FUNCTIONS ######*/
/*######                  ######*/
/*##############################*/
/*##############################*/

/**************************************/
/* CL_PRINT:  Generic print function. */
/**************************************/
cl_print(logical_name, str)
  char *logical_name;
  char *str;
  {
   struct router *r_ptr;

   if (FAST_SAVE != NULL)
     { 
      fprintf(FAST_SAVE,"%s",str);
      return(0);
     }

   r_ptr = route_list;
   while (r_ptr != NULL)
     {
      if ((r_ptr->printer != NULL) ? query_router(logical_name,r_ptr) : FALSE)         
        { 
         (*r_ptr->printer) (logical_name,str);
         return(1);
        }
      r_ptr = r_ptr->next;
     }

   if (strcmp("werror",logical_name) != 0)
     {
      cl_print("werror","ERROR: Logical name ");
      cl_print("werror",logical_name);
      cl_print("werror"," was not recognized by any routers\n");
     }
 
   return(0);
  }       

/*********************************************/
/* CL_GETC:  Generic get character function. */
/*********************************************/
int cl_getc(logical_name)
  char *logical_name;
  {
   struct router *r_ptr;
   int inchar;

   if (FAST_LOAD != NULL)
     { 
      inchar = getc(FAST_LOAD);
      
      if (inchar == '\r') return('\n');
         
      if (inchar != '\b')
        { return(inchar); }
        
      if (strcmp(logical_name,"stdin") == 0)
        { cl_print("stdout"," \b"); }
      
      return(inchar);
     }

   r_ptr = route_list;
   while (r_ptr != NULL)
     {
      if ((r_ptr->charget != NULL) ? query_router(logical_name,r_ptr) : FALSE)
        { 
         inchar = (*r_ptr->charget) (logical_name);
         
         if (inchar == '\r') return('\n');
         
         if (inchar != '\b')
           { return(inchar); }
#if MAC_LSC        
         if (strcmp(logical_name,"stdin") == 0)
           { cl_print("stdout"," \b"); }
#endif
      
         return(inchar);
        }
      r_ptr = r_ptr->next;
     }
   cl_print("werror","ERROR: Logical name ");
   cl_print("werror",logical_name);
   cl_print("werror"," was not recognized by any routers\n");
   return(-1);
  }  

/*************************************************/
/* CL_UNGETC:  Generic unget character function. */
/*************************************************/
int cl_ungetc(ch,logical_name) 
  int ch;
  char *logical_name;
  {
   struct router *r_ptr;

   if (FAST_LOAD != NULL)
     { return(ungetc(ch,FAST_LOAD)); }

   r_ptr = route_list;
   while (r_ptr != NULL)
     {
      if ((r_ptr->charunget != NULL) ? query_router(logical_name,r_ptr) : FALSE)
        {
         (*r_ptr->charunget) (ch,logical_name);
         return(1); 
        }
      r_ptr = r_ptr->next;
     }
   cl_print("werror","ERROR: Logical name ");
   cl_print("werror",logical_name);
   cl_print("werror"," was not recognized by any routers\n");
   return(0);
  }  

/************************************/
/* CL_EXIT:  Generic exit function. */
/************************************/
int cl_exit(num)
  int num; 
  {
   struct router *r_ptr, *next_r;

   Abort = FALSE;
   r_ptr = route_list;
   while (r_ptr != NULL)
     {
      next_r = r_ptr->next;
      if (r_ptr->active == TRUE)
        { if (r_ptr->exiter != NULL) (*r_ptr->exiter) (num); }
      r_ptr = next_r;
     }

   if (Abort) return;
   genexit(num); 
  }

/******************************************/
/* AbortExit: Forces cl_exit to terminate */
/*   after calling all closing routers.   */
/******************************************/
AbortExit()
  {
   Abort = TRUE;
  }
  
/************************************************************/
/* ADD_ROUTER:  Adds a routing structure to the route list. */
/************************************************************/
add_router(rname,p_num,query_fun,print_fun,getc_fun,ungetc_fun,exit_fun)
  char *rname;
  int p_num;
  int (*query_fun)(), (*print_fun)(), (*exit_fun)();
  int (*getc_fun)(), (*ungetc_fun)();
  {
   struct router *r_ptr, *last_ptr, *cur_ptr;

   r_ptr = get_struct(router);
   if (r_ptr == NULL)
     {
      cl_print("werror","Out of memory in append_router\n");
      return(0);
     }

   r_ptr->name = rname;
   r_ptr->active = TRUE;
   r_ptr->priority = p_num;
   r_ptr->query = query_fun;
   r_ptr->printer = print_fun;
   r_ptr->exiter = exit_fun;
   r_ptr->charget = getc_fun;
   r_ptr->charunget = ungetc_fun;
   r_ptr->next = NULL;

   if (route_list == NULL)
     { 
      route_list = r_ptr;
      return(1);
     }

   last_ptr = NULL;
   cur_ptr = route_list;
   while ((cur_ptr != NULL) ? (p_num < cur_ptr->priority) : FALSE)
     {
      last_ptr = cur_ptr; 
      cur_ptr = cur_ptr->next;
     }

   if (last_ptr == NULL)
     { 
      r_ptr->next = route_list;
      route_list = r_ptr;
     }
   else
     {
      r_ptr->next = cur_ptr;
      last_ptr->next = r_ptr;
     }
      
   return(1);
  }

/***********************************************************/
/* DEL_ROUTER:  Removes a router from the list of routers. */
/***********************************************************/
del_router(rname)
  char *rname;
  {
   struct router *r_ptr, *last_ptr;

   r_ptr = route_list;
   last_ptr = NULL;

   while (r_ptr != NULL)
     {
      if (strcmp(r_ptr->name,rname) == 0)
        {
         if (last_ptr == NULL)
           {
            route_list = r_ptr->next;
            rm(r_ptr,sizeof(struct router));
            return(1);
           }
         last_ptr->next = r_ptr->next;
         rm(r_ptr,sizeof(struct router));
         return(1);
        }
      last_ptr = r_ptr;
      r_ptr = r_ptr->next;
     }

   return(0);
  }

/*******************************************************************/
/* LOG_NAMEP:  Determines if any router recognizes a logical name. */
/*******************************************************************/
log_namep(logical_name)
  char *logical_name;
  {
   struct router *r_ptr;

   r_ptr = route_list;
   while (r_ptr != NULL)
     {
      if (query_router(logical_name,r_ptr) == TRUE) return(TRUE);
      r_ptr = r_ptr->next;
     }
   return(FALSE);
  }

/***************************************************************/
/* QUERY_ROUTER:  Determines if a specific router recognizes a */
/*    logical name.                                            */
/***************************************************************/
query_router(logical_name,r_ptr)
  char *logical_name;
  struct router *r_ptr;
  {
   if (r_ptr->active == FALSE)
     { return(FALSE); }
   
   if (r_ptr->query == NULL) return(FALSE);
   
   if ( (*r_ptr->query) (logical_name) == TRUE )
     { return(TRUE); }

   return(FALSE);
  }

/*************************************************/
/* DEACT_ROUTER:  Deactivates a specific router. */
/*************************************************/
deact_router(rname)
  char *rname;
  {
   struct router *r_ptr;

   r_ptr = route_list;

   while (r_ptr != NULL)
     {
      if (strcmp(r_ptr->name,rname) == 0)
        { 
         r_ptr->active = FALSE;
         return(TRUE); 
        }
      r_ptr = r_ptr->next;
     }

   return(FALSE);
  }

/*********************************************/
/* ACT_ROUTER:  Activates a specific router. */
/*********************************************/
act_router(rname)
  char *rname;
  {
   struct router *r_ptr;

   r_ptr = route_list;

   while (r_ptr != NULL)
     {
      if (strcmp(r_ptr->name,rname) == 0)
        { 
         r_ptr->active = TRUE;
         return(TRUE); 
        }
      r_ptr = r_ptr->next;
     }

   return(FALSE);
  }
  
/*****************/
/* SET_FAST_LOAD */
/*****************/
set_fast_load(file_ptr)
  FILE *file_ptr;
  {
   FAST_LOAD = file_ptr;
  }

/*****************/
/* SET_FAST_SAVE: */
/*****************/
set_fast_save(file_ptr)
  FILE *file_ptr;
  {
   FAST_SAVE = file_ptr;
  }
  
/*****************/
/* STRING ROUTER */
/*****************/

/********************************************************/
/* STR_FND:                                             */
/********************************************************/
int str_fnd(fileid) 
  char *fileid;
  {
   struct str_router *head;
   
   head = str_route_list;
   while (head != NULL)
     {
      if (strcmp(head->name,fileid) == 0)
        { return(TRUE); }
      head = head->next;
     }
     
   return(FALSE);
  }

/********************************************************/
/* STR_GETC:                                            */
/********************************************************/
int str_getc(logical_name)
  char *logical_name;
  {
   struct str_router *head;
   int rc;
   
   head = fnd_str_router(logical_name);
   if (head == NULL)
     {
      clips_system_error(1401);
      cl_exit(5); 
     }
  
   if (head->cur_pos >= head->max_pos)
     {
      head->cur_pos++;
      return(EOF);
     }

   rc = head->str[head->cur_pos];
   head->cur_pos++;

   return(rc);
  }

/********************************************************/
/* STR_UNGETC:                                          */
/********************************************************/
int str_ungetc(ch,logical_name)
  int ch;
  char *logical_name;
  {
   struct str_router *head;
   
   head = fnd_str_router(logical_name);
   
   if (head == NULL) 
     {
      clips_system_error(1402);
      cl_exit(5);
     }
   
   if (head->cur_pos > 0)
     { head->cur_pos--; }

   return(1);
  }
  

/********************************************************/
/* OPEN_STRING_ROUTER:                                */
/********************************************************/
open_str_source(name,str,cur_pos)
  char *name;
  char *str;
  int cur_pos;
  {
   struct str_router *new_str_router;
   
   if (fnd_str_router(name) != NULL) 
     {
      cl_print("werror","string router already opened\n");
      return(0);
     }
   
   new_str_router = get_struct(str_router);
   new_str_router->name = gm1(strlen(name) + 1);
   strcpy(new_str_router->name,name);
   new_str_router->str = str;
   new_str_router->cur_pos = cur_pos;
   if (str == NULL)
     { new_str_router->max_pos = 0; }
   else
     { new_str_router->max_pos = strlen(str); }
   new_str_router->next = str_route_list;
   str_route_list = new_str_router;

   return(1);
  }
  
/********************************************************/
/* CLOSE_STR_ROUTER:                                */
/********************************************************/
close_str_source(name)
  char *name;
  {
   struct str_router *head, *last;
   
   last = NULL;
   head = str_route_list;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        { 
         if (last == NULL)
           {
            str_route_list = head->next;
            rm(head->name,strlen(head->name) + 1);
            rtn_struct(str_router,head);
            return;
           }
         else
           {
            last->next = head->next;
            rm(head->name,strlen(head->name) + 1);
            rtn_struct(str_router,head);
            return;
           }
        }
      last = head;
      head = head->next;
     }
     
   cl_print("werror","Unable to close string router\n");
   return;
  }
  
  
/********************************************************/
/* FND_STR_ROUTER:                                */
/********************************************************/
struct str_router *fnd_str_router(name)
  char *name;
  {
   struct str_router *head;
   
   head = str_route_list;
   while (head != NULL)
     {
      if (strcmp(head->name,name) == 0)
        { return(head); }
      head = head->next;
     }
     
   return(NULL);
  }
   
