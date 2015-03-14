/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                     SYSIO MODULE                    */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "clips.h"
#include "scanner.h"

/************************************************************************/
/* FILE DESCRIPTOR LIST                                                 */
/*   This data structure houses the nodes which link the file id tags   */
/*   used by CLIPS external I/O functions with the appropriate streams. */
/************************************************************************/
typedef struct filelist
  {
   char *fileid;
   FILE *stream;
   struct filelist *next;
  } filelist;

typedef filelist *fileptr;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BASIC_IO

   int                     exec_print();
   int                     close_all();
   
#endif

#if EXT_IO

   char                   *err2_check();
   int                     fprint_arg();
   char                    scanning_subs();
   char                   *fill_buffer();
   
#endif
   
/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

#if BASIC_IO

   int                     fprintout();
   int                     clp_read();
   int                     close_command();
   int                     open_command();
   int                     file_close();
   int                     file_open();
   
#endif
   
#if EXT_IO

   HASH_PTR                cl_format();
   int                     readline();
   
#endif

   int                     fileexit();
   int                     filegetc();
   int                     fileprint();
   int                     fileungetc();
   int                     findfile();
   FILE                   *find_fptr();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern int               arg_type_check();
   extern struct draw      *add_symbol();
   extern char             *exp_line();
   extern char             *expand_string_with_char();
   extern char             *num_to_string();
   extern char             *get_log_name();
   extern char             *symbol_string();
   extern char             *append_to_string();
   
/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   static fileptr           headfile = NULL;       
   
/********************************************************************/
/* SYSIO_DEFINE                                                 */
/********************************************************************/
sysio_define()
  {
#if BASIC_IO
   define_function("printout",   'v', fprintout, "fprintout");
   define_function("fprintout",  'v', fprintout, "fprintout");
   define_function("read",       'u', clp_read,  "clp_read");
   define_function("open",       'i', open_command,  "open_command");
   define_function("close",      'i', close_command, "close_command");
#endif

#if EXT_IO
   define_function("format",     's', (int (*)()) cl_format, "cl_format");
   define_function("readline",   'u', (int (*)()) readline,  "readline");
#endif
  }
  
#if BASIC_IO

/*********************************************************************/
/* fprintout command                                                 */
/*********************************************************************/
int fprintout()
  {
   char *dummyid;
 
   /*=========================================*/
   /* Format requires at least two arguments: */
   /* a logical name and a format string.     */
   /*=========================================*/
 
   if (arg_num_check("printout or fprintout",AT_LEAST,1) == -1) return(0);
 
   dummyid = get_log_name(1,"stdout");
   if (dummyid == NULL)
     {
      cl_print("werror","Illegal file id used for function printout or fprintout\n");
      set_execution_error(TRUE);
      return(0);
     }

   if (log_namep(dummyid) == FALSE)
     {
      cl_print("werror","ERROR: Logical name ");
      cl_print("werror",dummyid);
      cl_print("werror"," was not recognized by any routers\n");
      return(0);
     }

   exec_print(2,dummyid);
   return(1);
  }

/**************************************************************/
/* exec_print                                                 */
/**************************************************************/
static int exec_print(cur_arg,fileid)
  int cur_arg;
  char *fileid;
  {
   struct test *test_ptr;
   VALUE arg_ptr;
   int i;

   test_ptr = get_first_arg();

   for (i = 1 ; i < cur_arg ; i++)
     { test_ptr = get_next_arg(test_ptr); }

   while (test_ptr != NULL)
     {
      switch(test_ptr->type)
        {
         case WORD:               
           if (strcmp(symbol_string(test_ptr->val.hvalue),"crlf") == 0)
             { cl_print(fileid,"\n"); }               
           else if (strcmp(symbol_string(test_ptr->val.hvalue),"tab") == 0)
             { cl_print(fileid,"\t"); }                
           else if (strcmp(symbol_string(test_ptr->val.hvalue),"vtab") == 0)
             { cl_print(fileid,"\v"); }               
           else if (strcmp(symbol_string(test_ptr->val.hvalue),"t") == 0)
             { cl_print(fileid,"\n"); }
           else
             { cl_print(fileid,symbol_string(test_ptr->val.hvalue)); }
           break;
         case STRING:
           cl_print(fileid,symbol_string(test_ptr->val.hvalue));
           break;
         case NUMBER:
           print_num(fileid,test_ptr->val.fvalue); 
           break;
         default:
           generic_compute(test_ptr,&arg_ptr);
           print_value(fileid,&arg_ptr);
           break;
        }
     test_ptr = test_ptr->next_arg;
    }

   return(1);
  }

/****************************************/
/* read                                 */
/****************************************/
clp_read(read_value)
  VALUE_PTR read_value;
  {
   struct token inp_tkn;
   int arg_no;
   char *dummyid;
   char *read_str;
   int max_char, pos_char;
   int inchar;
   
   /*===============================================*/
   /* Check for an appropriate number of arguments. */
   /*===============================================*/

   if ((arg_no = arg_num_check("read",NO_MORE_THAN,1)) == -1)
     {
      read_value->type = STRING;
      read_value->val.hvalue = add_symbol("*** READ ERROR ***");
      return; 
     }
     
   /*======================================================*/
   /* Determine the logical name from which input is read. */
   /*======================================================*/

   if (arg_no == 0) 
     { dummyid = "stdin"; }
   else if (arg_no == 1) 
     {
      dummyid = get_log_name(1,"stdin");
      if (dummyid == NULL)
        {
         cl_print("werror","Illegal file id for function read\n");
         set_execution_error(TRUE);
         read_value->type = STRING;
         read_value->val.hvalue = add_symbol("*** READ ERROR ***");
         return;
        }
     }

   if (log_namep(dummyid) == FALSE) 
     {
      set_execution_error(TRUE);
      cl_print("werror","Unable to open file with id ");
      cl_print("werror",dummyid);
      cl_print("werror","\n");
      read_value->type = STRING;
      read_value->val.hvalue = add_symbol("*** READ ERROR ***");
      return;
     }

   /*====================================================*/
   /* Collect input into string if read source is stdin, */
   /* else just get token.                               */
   /*====================================================*/
   
   if (strcmp(dummyid,"stdin") == 0)
     {
      inp_tkn.token = STOP;
      while (inp_tkn.token == STOP)
        {
         read_str = NULL;
         max_char = pos_char = 0;
         inchar = cl_getc("stdin");
         while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF))
           {
            read_str = expand_string_with_char(inchar,read_str,&pos_char,
                                              &max_char,max_char + 80);
            inchar = cl_getc("stdin");
           }
          
         open_str_source("read",read_str,0);
         gettoken("read",&inp_tkn);
         close_str_source("read");
         if (max_char > 0) rm(read_str,max_char);
         
         if (get_execution_error())
           {
            inp_tkn.token = STRING;
            inp_tkn.hashword = add_symbol("*** READ ERROR ***");
           }
        }
     }
   else
     { gettoken(dummyid,&inp_tkn); }

   /*=======================*/
   /* Process return value. */
   /*=======================*/
   
   read_value->type = inp_tkn.token;
   if (inp_tkn.token == NUMBER)
     { read_value->val.fvalue = inp_tkn.tknnumber; }
   else if ((inp_tkn.token == STRING) || (inp_tkn.token == WORD))
     { read_value->val.hvalue = inp_tkn.hashword; }
   else if (inp_tkn.token == STOP)
     {
      read_value->type = WORD;
      read_value->val.hvalue = add_symbol("EOF");
     }
   else if (inp_tkn.token == KUNKNOWN)
     {
      read_value->type = STRING;
      read_value->val.hvalue = add_symbol("*** READ ERROR ***");
     }
   else
     {
      read_value->type = STRING;
      read_value->val.hvalue = add_symbol(inp_tkn.print_rep);
     }

   return;
  }

/*###################################*/
/*# FILE SYSTEM INTERFACE FUNCTIONS #*/
/*###################################*/

/**************************************************************/
/* OPEN_COMMAND: This function opens a file named by the user */
/*   and identifies it with a character string tag specified  */
/*   by the user.  This function returns a non-zero value if  */
/*   the file was successfully opened.                        */
/**************************************************************/
int open_command() 
  {
   int arg_no, status; 
   char *newfilename, *newfileid, *newmode;
   VALUE arg_ptr;

   /*======================================*/
   /* Check for valid number of arguments. */
   /*======================================*/

   if ((arg_no = arg_num_check("open",AT_LEAST,2)) == -1) return(0);
   if ((arg_no = arg_num_check("open",NO_MORE_THAN,3)) == -1) return(0);
   
   /*==========================*/
   /* Check for the file name. */
   /*==========================*/
   
   if (arg_type_check("open",1,STRING,&arg_ptr) == FALSE) return(0);
   newfilename = get_valstring(arg_ptr);
   
   /*=============================*/
   /* Check for the logical name. */
   /*=============================*/
   
   newfileid = get_log_name(2,NULL);
   if (newfileid == NULL)
     {
      set_execution_error(TRUE);
      cl_print("werror","Illegal logical name used in open function.\n");
      return(0);
     }
   
   if (findfile(newfileid))
     {
      set_execution_error(TRUE);
      cl_print("werror","Logical name ");
      cl_print("werror",newfileid);
      cl_print("werror"," already in use.\n");
      return(0);
     }
     
   /*===================================*/
   /* Check for valid file access mode. */
   /*===================================*/

   if (arg_no == 2)
     { newmode = "r"; }
   else if (arg_no == 3)
     { 
      if (arg_type_check("open",3,STRING,&arg_ptr) == FALSE) return(0);
      newmode = get_valstring(arg_ptr);
     }
   if ((strcmp(newmode,"r") != 0) &&
       (strcmp(newmode,"r+") != 0) &&
       (strcmp(newmode,"w") != 0) &&
       (strcmp(newmode,"a") != 0))
     {
      set_execution_error(TRUE);
      cl_print("werror","Invalid mode for open function.\n");
      return(0); 
     }                                     

   /*======================================================*/
   /* Open named file and store it with named tag on list. */
   /*======================================================*/
   
   status = file_open(newfilename,newmode,newfileid);
   if (status == 0) open_error_message("open",newfilename);
   return(status);
  }

/************************************************************************/
/* FILE_OPEN:  Opens a file with the specified access mode and stores   */
/*   the opened stream as well as the file id tag on the global file    */
/*   list. Returns a non-zero value if the file was succesfully opened. */
/************************************************************************/
int file_open(fname,fmode,fid) 
  char *fname,*fmode,*fid; 
  {

   FILE *newstream;
   fileptr fptr, prev;

   newstream = fopen(fname,fmode);

   /*==================================================================*/
   /* Make sure the file can be opened with the specified access mode. */
   /*==================================================================*/

   if (newstream == NULL) 
     {
      return(0);
     }

   /*=====================================*/
   /* Add stream and file id tag to list. */
   /*=====================================*/

   if (headfile == NULL)
     {
      headfile = get_struct(filelist);
      headfile->fileid = gm2 (strlen(fid) + 1);
      strcpy(headfile->fileid,fid);
      headfile->stream = newstream;
      headfile->next = NULL; 
     }
   else 
     {
      fptr = headfile; 
      prev = fptr;
      while (fptr != NULL)
        {
         prev = fptr;
         fptr = fptr->next; 
        }
      fptr = get_struct(filelist);
      fptr->fileid = gm2 (strlen(fid) + 1);
      strcpy(fptr->fileid,fid);
      fptr->stream = newstream;
      fptr->next = NULL;
      prev->next = fptr;
     }
               
   return(1);
  }


/***********************************************************************/
/* CLOSE_COMMAND:  This function closes the file stream with the file  */
/*   id specified by the user, if such a stream exists.  This function */
/*   returns a non-zero value if the file was successfully closed.     */
/***********************************************************************/
int close_command()
  {
   int arg_no;
   char *fileid;
                                                                    
   /*======================================================*/
   /* Check for valid number of arguments and assignments. */
   /*======================================================*/

   if ((arg_no = arg_num_check("close",NO_MORE_THAN,1)) == -1) return(0);

   if (arg_no == 0)
     {
      close_all();
      return(1);
     }

   fileid = get_log_name(1,NULL);
   if (fileid == NULL)
     {
      set_execution_error(TRUE);
      cl_print("werror","Illegal logical name used in close function.\n");
      return(0);
     }

   return((float) file_close(fileid));
  }    

/**************************************************************/
/* CLOSE_ALL:  Closes all files opened the file io utilities. */
/**************************************************************/
static int close_all()
  {
   fileptr fptr, prev;

   fptr = headfile;

   while (fptr != NULL)
     {
      fclose(fptr->stream);
      prev = fptr;
      rm(fptr->fileid,strlen(fptr->fileid) + 1);
      fptr = fptr->next;
      rm(prev,sizeof(filelist));
     }

   headfile = NULL;

   return(1);
  }
                              
/******************************************************************/
/* FILE_CLOSE:  Closes a file with the specified file id tag.     */
/*   Returns a non-zero value if the file was succesfully closed. */
/******************************************************************/
int file_close(fid) 
  char *fid;
  {
   fileptr fptr, prev;

   /*=====================================================*/
   /* Locate the file with the given id in the file list. */
   /*=====================================================*/

   fptr = headfile;
   prev = NULL;

   while (fptr != NULL)
     {
      if (strcmp(fptr->fileid,fid) == 0)
        { 
         fclose(fptr->stream);
         rm(fptr->fileid,strlen(fptr->fileid) + 1);
         if (prev == NULL)
           { headfile = fptr->next; }
         else
           { prev->next = fptr->next; }
         rm(fptr,sizeof(filelist));

         return(1);
        }
        
      prev = fptr;
      fptr = fptr->next;
     }
   
   set_execution_error(TRUE);
   cl_print("werror","Error: attempted to close unopened file id ");
   cl_print("werror",fid);
   cl_print("werror","\n");
   return(0);
  }
  
#endif

#if EXT_IO
  
/**********************************************************/
/* CL_FORMAT:                                             */
/**********************************************************/
HASH_PTR cl_format()
  {    
   int num_a, start_pos;
   char *form_str, *log_name, temp_char;
   char f_type;
   int  f_cur_arg = 3;
   int form_pos = 0;
   char buffer[512];
   char *fstr = NULL;
   int fmax = 0, fpos = 0;
   HASH_PTR hptr;

   /*======================================*/
   /* Set default return value for errors. */
   /*======================================*/
   
   hptr = add_symbol("");
   
   /*=========================================*/
   /* Format requires at least two arguments: */
   /* a logical name and a format string.     */
   /*=========================================*/

   if ((num_a = arg_num_check("format",AT_LEAST,2)) == -1) 
     { return(hptr); }

   /*========================================*/
   /* First argument must be a logical name. */
   /*========================================*/
     
   if ((log_name = get_log_name(1,"stdout")) == NULL)
     {
      cl_print("werror","Illegal file id for function format\n");
      set_execution_error(TRUE);
      return(hptr);
     }

   if (strcmp(log_name,"nil") == 0)
     { /* do nothing */ }
   else if (log_namep(log_name) == FALSE)
     {
      cl_print("werror","ERROR: Logical name ");
      cl_print("werror",log_name);
      cl_print("werror"," was not recognized by any routers\n");
      return(hptr);
     }

   /*=====================================================*/
   /* Second argument must be a string.  The appropriate  */
   /* number of arguments specified by the string must be */
   /* present in the argument list.                       */
   /*=====================================================*/

   if ((form_str = err2_check (num_a)) == NULL)
     { return (hptr); }
     
   /*==============================================*/
   /* Locate a string of 80 character for scanning */
   /* sub_string from control_string               */
   /*==============================================*/

   /* Scanning and print the format */

   while (form_str[form_pos] != '\0')
     {
      if (form_str[form_pos] != '%')
        {
         start_pos = form_pos;
         while ((form_str[form_pos] != '%') && (form_str[form_pos] != '\0'))
           { form_pos++; }
         temp_char = form_str[form_pos];
         form_str[form_pos] = '\0';
         fstr = append_to_string(&form_str[start_pos],fstr,&fpos,&fmax);
         form_str[form_pos] = temp_char;
        }
      else
        {
         start_pos = form_pos;
	     form_pos++;
         f_type = scanning_subs(form_str,&form_pos,buffer);
	     if (f_type != ' ')
           {
            temp_char = form_str[form_pos];
            form_str[form_pos] = '\0';
	        if (fprint_arg(&form_str[start_pos],f_cur_arg,f_type,buffer) == 0)
	          {
	           if (fstr != NULL) rm(fstr,fmax);
	           return (hptr); 
	          }
            fstr = append_to_string(buffer,fstr,&fpos,&fmax);
            if (fstr == NULL) return(hptr);
            f_cur_arg++;
            form_str[form_pos] = temp_char;
           }
         else
           {
            fstr = append_to_string(buffer,fstr,&fpos,&fmax);
            if (fstr == NULL) return(hptr);
           }
        }
     }
     
   if (fstr != NULL)
     {
      hptr = add_symbol(fstr);
      if (strcmp(log_name,"nil") != 0) cl_print(log_name,fstr);
      rm(fstr,fmax);
     }
   else
     { hptr = add_symbol(""); }

   return(hptr);
  }
      
/*******************************************************************/
/* err2_check:  Checks the 2nd parameter which is the format       */
/*   control string to see if there are enough matching arguments. */
/*******************************************************************/
static char *err2_check(num_a)
  int num_a;
  {
   VALUE t_ptr; 
   char *str_array;
   char print_buff[10];

   int i,per_count;
       
   if (arg_type_check("format",2,STRING,&t_ptr) == FALSE) return(NULL);

   per_count = 0;
   str_array = t_ptr.val.hvalue->contents;
   for (i= 0 ; str_array[i] != '\0' ; )
     {	     
      if (str_array[i] == '%')
        {
         i++;
         if (scanning_subs(str_array,&i,print_buff) != ' ')
           { per_count++; }
        }
      else 
        { i++; }
     }
		     
   if (per_count != (num_a - 2))	    
     {				     
      cl_print ("werror","Number of arguments in the format does not");
      cl_print ("werror"," agree with the control_string.\n");
      return (NULL);
     }

   return(str_array);
  }

/***************************************************************/
/* scanning_subs:  This function will scan a sub_string, which */
/*   is the format for an argument from the control_string     */
/***************************************************************/
static char scanning_subs(str_array,a,print_buff)
  char *str_array;
  int *a;
  char *print_buff;
  {
   int found_type = FALSE;
   char inchar, f_arg_type;

   f_arg_type = ' ';
   
   if (str_array[*a] == 'n')
     {
      sprintf(print_buff,"\n");
      (*a)++;
      return(f_arg_type);
     }
   else if (str_array[*a] == 't')
     {
      sprintf(print_buff,"\t");
      (*a)++;
      return(f_arg_type);
     }
   else if (str_array[*a] == 'v')
     {
      sprintf(print_buff,"\v");
      (*a)++;
      return(f_arg_type);
     }
   else if (str_array[*a] == '%')
     {
      sprintf(print_buff,"%%");
      (*a)++;
      return(f_arg_type);
     }
     
   while ((str_array[*a] != '%') && (str_array[*a] != '\0'))
     {
      inchar = str_array[*a];
      if ( (found_type == FALSE) &&
           ( (inchar == 'd') ||
             (inchar == 'o') ||
             (inchar == 'x') ||
             (inchar == 'u') ||
             (inchar == 'c') ||
             (inchar == 's') ||
             (inchar == 'e') ||
             (inchar == 'f') ||
             (inchar == 'g') ) )
        {
         found_type = TRUE;
         f_arg_type = inchar;
        }
      (*a)++;
     }

   return(f_arg_type);
  }

/*****************************************************************/
/* fprint_arg:  Prints out part of the total format string along */
/*   with the argument for that part of the format string.       */
/*****************************************************************/
static int fprint_arg (f_sub_str,cur_arg,f_arg_type,print_buff)
  char *f_sub_str;
  int cur_arg;
  char f_arg_type;
  char *print_buff;
  { 
   VALUE t_ptr;
   TYPE arg_type;

   runknown(cur_arg,&t_ptr);
   arg_type = get_valtype(t_ptr);

   /*=================*/
   /* String argument */
   /*=================*/
   if (f_arg_type == 's')
     {
      if ((arg_type != WORD) && (arg_type != STRING))
        {
         exp_type_error("format",cur_arg,"word or string");
         return(0);
        }
      sprintf(print_buff,f_sub_str,t_ptr.val.hvalue->contents);
     }

   /*====================*/
   /* Character argument */
   /*====================*/

   else if (f_arg_type == 'c')
     {
      if ((arg_type != WORD) && (arg_type != STRING))
        {
         exp_type_error("format",cur_arg,"word or string");
         return(0);
        }
      sprintf(print_buff,f_sub_str,(t_ptr.val.hvalue->contents)[0]);
     }

   /*==================*/
   /* Integer argument */
   /*==================*/

   else if ((f_arg_type == 'd') || (f_arg_type == 'x') || 
            (f_arg_type == 'o') || (f_arg_type == 'u'))
     {
      if (arg_type != NUMBER)
        {
         exp_type_error("format",cur_arg,"number");
         return(0);
        }
      sprintf(print_buff,f_sub_str,(int) t_ptr.val.fvalue);
     }
        
   /*================*/
   /* Float argument */
   /*================*/
  
   else if ((f_arg_type == 'f') || (f_arg_type == 'e') || 
            (f_arg_type == 'g')) 
     {
      if (arg_type != NUMBER)
        {
         exp_type_error("format",cur_arg,"number");
         return(0);
        }
      sprintf(print_buff,f_sub_str,t_ptr.val.fvalue);
     }
     
   /*========*/
   /* ERROR! */
   /*========*/
  
   else
     {
      cl_print ("werror"," Error in format, the conversion character");
      cl_print ("werror"," for formatted output is not valid\n");
      return(0);
     }

   return(1);
  }

/****************************************************************/
/* READLINE:                                                    */
/****************************************************************/
readline(rlnval)
  VALUE_PTR rlnval;   
  {
   char *buffer;
   int line_pos = 0;
   int line_max = 0;
   int num;
   
   char *log_name;
     
   rlnval->type = STRING;
   
   if ((num = arg_num_check("readline",NO_MORE_THAN,1)) == -1) 
     { 
      rlnval->val.hvalue = add_symbol("*** READ ERROR ***");
      return;
     }
     
   if (num == 0 )
     { log_name = "stdin"; }
   else
     {
      log_name = get_log_name(1,"stdin");
      if (log_name == NULL)
        {
         cl_print("werror","Illegal file id for function readline\n");
         set_execution_error(TRUE);
         rlnval->val.hvalue = add_symbol("*** READ ERROR ***");
         return;
        }
     } 
   
   buffer = fill_buffer(log_name,&line_pos,&line_max);
   if (buffer == NULL)
     {
      rlnval->val.hvalue = add_symbol("EOF");
      rlnval->type = WORD;
      return;
     }
   rlnval->val.hvalue = add_symbol(buffer);
   rm(buffer,sizeof (char) * line_max);
   return;
  }

/*****************************************************/
/* fill_buffer                                       */
/*****************************************************/
static char *fill_buffer(log_name,buf_pos,buf_max)
  char *log_name;
  int *buf_pos, *buf_max;
  {
   int c;
   char *buf = NULL;
    
   /*================================*/
   /* Read until end of line or eof. */
   /*================================*/

   c = cl_getc(log_name);

   if (c == EOF)
     { return(NULL); }
       
   /*==================================*/
   /* Grab characters until cr or eof. */
   /*==================================*/
   
   while ((c != '\n') && (c != '\r') && (c != EOF))		
     {
      buf = expand_string_with_char(c,buf,buf_pos,buf_max,*buf_max+80);
      c = cl_getc(log_name);
     } 
      
   /*==================*/
   /* Add closing EOS. */
   /*==================*/
   
   buf = expand_string_with_char(EOS,buf,buf_pos,buf_max,*buf_max+80);
   return (buf);
  }

#endif

/*############################################*/
/*# STANDARD STREAM AND FILE I/O DEFINITIONS #*/
/*############################################*/

/*******************************************************/
/* findfptr:  Returns a pointer to a file stream for a */
/*   given file id tag.                                */
/*******************************************************/
FILE *find_fptr(fileid) 
  char *fileid;
  {
   fileptr fptr;
                                                                 
   /*========================================================*/
   /* Check to see if standard input or output is requested. */
   /*========================================================*/

   if (strcmp(fileid,"stdout") == 0)
     { return(stdout); }
   else if (strcmp(fileid,"stdin") == 0)
     { return(stdin); }  
   else if (strcmp(fileid,"wtrace") == 0)
     { return(stdout); }    
   else if (strcmp(fileid,"wdialog") == 0)
     { return(stdout); } 
   else if (strcmp(fileid,"wagenda") == 0)
     { return(stdout); }  
   else if (strcmp(fileid,"wclips") == 0)
     { return(stdout); } 
   else if (strcmp(fileid,"wdisplay") == 0)
     { return(stdout); }  
   else if (strcmp(fileid,"werror") == 0)
     { return(stdout); }

   /*=========================================================*/
   /* Otherwise, look up the file id on the global file list. */
   /*=========================================================*/
           
   fptr = headfile;
   while ((fptr != NULL) ? (strcmp(fileid,fptr->fileid) != 0) : FALSE)
     { fptr = fptr->next; }

   if (fptr != NULL) return(fptr->stream);

   return(NULL); 
  }

/*******************************************************/
/* FINDFILE:  Returns a pointer to a file stream for a */
/*   given file id tag.                                */
/*******************************************************/
int findfile(fileid) 
  char *fileid;
  {
   fileptr fptr;

   if ( (strcmp(fileid,"stdout") == 0) ||
        (strcmp(fileid,"stdin") == 0) ||
        (strcmp(fileid,"wclips") == 0) ||
        (strcmp(fileid,"wtrace") == 0) ||
        (strcmp(fileid,"wagenda") == 0) ||
        (strcmp(fileid,"werror") == 0) ||
        (strcmp(fileid,"wdisplay") == 0) ||
        (strcmp(fileid,"wdialog") == 0) )
     { return(TRUE); }

   fptr = headfile;
   while ((fptr != NULL) ? (strcmp(fileid,fptr->fileid) != 0) : FALSE)
     { fptr = fptr->next; }

   if (fptr != NULL) return(TRUE);

   return(FALSE); 
  }

/**************************************/
/* FILEEXIT:  Closes all opened files */
/**************************************/
fileexit(num)
  int num;
  {
#if BASIC_IO
   close_all();       
#endif
  }

/*******************************************************/
/* FILEPRINT:  Returns a pointer to a file stream for a */
/*   given file id tag.                                */
/*******************************************************/
fileprint(logical_name,str)
  char *logical_name, *str;
  {
   FILE *fptr;

   fptr = find_fptr(logical_name);
   fprintf(fptr,"%s",str);
  }

/*******************************************************/
/* FILEGETC:  Returns a pointer to a file stream for a */
/*   given file id tag.                                */
/*******************************************************/
int filegetc(logical_name)
  char *logical_name;
  {
   FILE *fptr;

   fptr = find_fptr(logical_name);
   return(getc(fptr));
  }

/*******************************************************/
/* FILEUNGETC:  Returns a pointer to a file stream for a */
/*   given file id tag.                                */
/*******************************************************/
fileungetc(ch,logical_name)
  int ch;
  char *logical_name;
  {
   FILE *fptr;

   fptr = find_fptr(logical_name);
   ungetc(ch,fptr);
  }

