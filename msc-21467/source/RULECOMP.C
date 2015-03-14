/*   CLIPS Version 4.30   4/25/89 */

#include "setup.h"

#if CLP_RULE_COMP && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#include "clips.h"
#include "engine.h"
#include "deffacts.h"
#include "network.h"
#include "expressn.h"

#define FSIZE 80

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   int                        enum_tests();
   int                        rules_to_c_code();
   char                       tname();
   
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static int                 mark_pat_count;
   static int                 list_count;
   static int                 pnode_count;
   static int                 icount;
   static int                 fcount;
   static int                 pcount;
   static int                 join_count;
   static int                 fact_ct;
   static int                 fact_pct;
   static int                 dfact_ct;
   static int                 elm_ct;

   static FILE               *comp_fp;
   static FILE               *ftest_fp;
   static FILE               *ptest_fp;
   static FILE               *itest_fp;
   static FILE               *pnode_fp;
   static FILE               *join_fp;
   static FILE               *list_fp;
   static FILE               *init_fp;
   
   static int                 id;
   static int                 charsize;
   static int                 intsize;
   static int                 floatsize;
   static int                 ptrsize;
   static int                 maxsize;
   
   static int                 first_pn;
   static int                 first_join;
   static int                 first_list;
   static int                 first_ptest;
   static int                 first_itest;
   static int                 first_ftest;
   static int                 first_dfact;
   static int                 first_fact;
   static int                 first_elem;

/****************************************/
/* GLOBAL EXTERNAL VARIABLE DEFINITIONS */
/****************************************/

   static struct dfact       *deflist;
   static struct pat_node    *patn_list;
   extern struct pat_node    *network_pointer(); 
   static struct draw       **symbol_table;
   extern struct draw       **get_symbol_table();
   static struct funtab      *fctn_list;
   extern struct funtab      *get_fctn_list();
   
   extern struct funtab            *PTR_NOP;
   extern struct funtab            *PTR_CONSTANT;
   extern struct funtab            *PTR_NOTCONSTANT;
   extern struct funtab            *PTR_GET_VAR;
   extern struct funtab            *PTR_GET_FIELD;
   extern struct funtab            *PTR_NEQ_VARS;
   extern struct funtab            *PTR_EQ_VARS;
   extern struct funtab            *PTR_KTAND; 
   extern struct funtab            *PTR_KTOR; 
   extern struct funtab            *PTR_EQ;
   extern struct funtab            *PTR_NEQ; 
   extern struct funtab            *PTR_NOT;
   extern struct funtab            *PTR_EQ_FIELD;
   extern struct funtab            *PTR_NEQ_FIELD;
   
/********************************************************/
/* RULES_TO_C_CODE:  Converts the rule network into the */
/*   corresponding C data structures.                   */
/********************************************************/
int rules_to_c_code()
  {
   char *file_name;
   VALUE arg_ptr;
   int arg_count, i;
   int other_args[5];

   /*===========================================*/
   /* Initialize parameters for handling array  */
   /* syntax for empty arrays.                  */ 
   /*===========================================*/

   first_pn = first_join = first_list = TRUE;
   first_ptest = first_itest = first_ftest = TRUE;
   first_dfact = first_fact = first_elem = TRUE;
   
   /*===========================================*/
   /* Initialize counters to determine indices  */
   /* for setting pointers within arrays.       */
   /*===========================================*/

   mark_pat_count = 0;
   list_count = 0;
   pnode_count = 0;
   icount = fcount = pcount = 0;
   join_count = 0;
   fact_ct = fact_pct = dfact_ct = 0;
   elm_ct = 0;

   /*============================================*/
   /* Check for appropriate number of arguments. */
   /*============================================*/
   
   arg_count = num_args();
   
   if (arg_count < 2)
     { 
      exp_num_error("rules-to-c",AT_LEAST,2);
      return (0);
     }
   else if (arg_count > 6)
     {
      exp_num_error("rules-to-c",NO_MORE_THAN,6);
      return (0);
     }

   /*====================================*/
   /* Get the file name to place C code. */
   /*====================================*/
  
   runknown(1,&arg_ptr);
   if ((arg_ptr.type) != STRING && (arg_ptr.type) != WORD)
     {
      exp_type_error("rules-to-c",1,"WORD or STRING");
      return (0);
     }
   file_name = rvalstring(arg_ptr);

#if VMS || IBM_MSC || IBM_LATTICE || IBM_TBC || IBM_ZTC
   /*Check for '.' VAX or IBM PC file_name */
   for(i=0;*(file_name+i);i++)
     {
      if(*(file_name+i) == '.')
        {
         cl_print("werror","Invalid file name ");
         cl_print("werror",file_name);
         cl_print("werror"," contains \'.\'\n");
         return(1);
        }
      }
#endif
   
   /*==============================*/
   /* Get the remaining arguments. */
   /*==============================*/
   
   other_args[1] = sizeof(char);
   other_args[2] = sizeof(int);
   other_args[3] = sizeof(float);
   other_args[4] = sizeof(char *);
   
   for (i = 2 ; i <= arg_count ; i++)
     {
      runknown(i,&arg_ptr);
      if ((arg_ptr.type) != NUMBER)
        {
         cl_print("werror","Function rules-to-c expected a number as argument #");
         print_num("werror",(float) i);
         cl_print("werror","\n");
         return (0);
        }
      other_args[i-2] = (int) rvalfloat(arg_ptr);
     }
   
   return(generate_code(file_name,other_args[0],other_args[1],
                 other_args[2],other_args[3],other_args[4]));
  }
 
/********************************************************/
/* GENERATE_CODE:                                       */
/********************************************************/
generate_code(file_name,rule_id,arg1,arg2,arg3,arg4)
  char *file_name;
  int rule_id, arg1, arg2, arg3, arg4;
  {
   char fname[FSIZE], hname[FSIZE];

   /*===============================*/
   /* Clear out Clips Rule network. */
   /*===============================*/
   
   patn_list = network_pointer();
   set_fact_id( (FACT_ID) -1);   
   set_agenda_count(0);
   remove_all_facts();
   flush_web(patn_list);
   remove_all_activations();
   
   /*===============================*/
   /* Clear out Clips Rule network. */
   /*===============================*/
   
   mark_joins(patn_list,0);

   id = rule_id;
   charsize = arg1;
   intsize = arg2;
   floatsize = arg3;
   ptrsize = arg4;
   
   maxsize = 0;
   if (charsize > maxsize) maxsize = charsize;
   if (intsize > maxsize) maxsize = intsize;
   if (floatsize > maxsize) maxsize = floatsize;
   if (ptrsize > maxsize) maxsize = ptrsize;
   
   /*=====================================*/
   /* Issue warning for weird data sizes. */
   /*=====================================*/
   
   if ((charsize != 1) || 
       (((intsize / 2) * 2) != intsize) || 
       (((floatsize / 2) * 2) != floatsize) || 
       (((ptrsize / 2) * 2) != ptrsize))
     { cl_print("wdisplay","*** Beware: Unusual data sizes ***\n"); }

   /*================*/
   /* Open the file. */
   /*================*/
   
   sprintf(fname,"%s0.c",file_name);
   if ( (init_fp = fopen(fname,"w")) == NULL) 
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(hname,"%s1.c",file_name);
   if ( (comp_fp = fopen(hname,"w")) == NULL) 
     {
      open_error_message("rules-to-c",hname);
      return(0);
     }
   sprintf(fname,"%s2.c",file_name);
   if ( (ftest_fp = fopen(fname,"w")) == NULL) 
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(fname,"%s3.c",file_name);
   if ( (ptest_fp = fopen(fname,"w")) == NULL)
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(fname,"%s4.c",file_name);
   if ( (itest_fp = fopen(fname,"w")) == NULL)
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(fname,"%s5.c",file_name);
   if ( (pnode_fp = fopen(fname,"w")) == NULL)
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(fname,"%s6.c",file_name);
   if ( (join_fp = fopen(fname,"w")) == NULL)
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }
   sprintf(fname,"%s7.c",file_name);
   if ( (list_fp = fopen(fname,"w")) == NULL) 
     {
      open_error_message("rules-to-c",fname);
      return(0);
     }

   /*=======================================*/
   /* Header definitions for 'C' rule files. */
   /*=======================================*/

   file_header(init_fp,'r');
   file_header(comp_fp,'c');
   file_header(ftest_fp,'f');
   file_header(ptest_fp,'p');
   file_header(itest_fp,'i');
   file_header(pnode_fp,'n');
   file_header(join_fp,'j');
   file_header(list_fp,'l');

   /*===============================================*/
   /* Give definitions of the external and internal */
   /* functions to be accessed by test structures   */
   /* and function definition structures.           */
   /*===============================================*/

   symbol_table = get_symbol_table();
   fctn_list = get_fctn_list();

   fprintf(comp_fp,"\n");
   fprintf(comp_fp,"/************************************/\n");
   fprintf(comp_fp,"/* EXTERNAL FUNCTION DEFINITIONS    */\n");
   fprintf(comp_fp,"/************************************/\n\n");

   def_functions();

   /*===============================================*/
   /* Construct the definition of the function list */
   /* from the definitions of the functions.        */
   /*===============================================*/

   fprintf(comp_fp,"\n\n");
   fprintf(comp_fp,"/************************************/\n");
   fprintf(comp_fp,"/* FUNCTION LIST DEFINITION         */\n");
   fprintf(comp_fp,"/************************************/\n\n");

   fprintf(comp_fp,"  struct funtab ftd_%d[] = {\n",id);
   enum_functions(fctn_list);
   fprintf(comp_fp,"};\n");

   /*=========================================*/
   /* Give definitions for all the entries in */
   /* the hash table.                         */
   /*=========================================*/

   fprintf(comp_fp,"\n\n");
   fprintf(comp_fp,"/************************************/\n");
   fprintf(comp_fp,"/* HASH TABLE ENTRY DEFINITIONS     */\n");
   fprintf(comp_fp,"/************************************/\n\n");

   fprintf(comp_fp,"struct draw hash_%d[] = {\n",id);
   hash_entries();
   fprintf(comp_fp,"};\n",id);

   /*====================================*/
   /* Give definition of the hash table. */
   /*====================================*/

   fprintf(comp_fp,"\n\n");
   fprintf(comp_fp,"/************************************/\n");
   fprintf(comp_fp,"/* HASH TABLE DEFINITION            */\n");
   fprintf(comp_fp,"/************************************/\n\n");

   print_hash();
   fprintf(comp_fp,"struct draw **comp_hash_%d = temp_hash_%d;\n",id,id);

   /*=============================================*/
   /* Give complete structure definitions for all */
   /* the nodes in the pattern network.           */
   /*=============================================*/

   def_nodes(patn_list,0,0);

   /*=============================================*/
   /* Give complete structure definitions for all */
   /* deffacts.                                   */
   /*=============================================*/
   
#if DEFFACTS_CONSTRUCT    
   fprintf(comp_fp,"\n\n");
   fprintf(comp_fp,"/********************************/\n");
   fprintf(comp_fp,"/* DEFFACTS DEFINITIONS         */\n");
   fprintf(comp_fp,"/********************************/\n\n");
   fprintf(comp_fp,"\n\n");
   fprintf(comp_fp,"#if ! IBM_MSC\n");
   fprintf(comp_fp,"  struct dfact df_%d[] =\n",id);
   fprintf(comp_fp,"#else\n");
   fprintf(comp_fp,"  struct dfact huge df_%d[] =\n",id);
   fprintf(comp_fp,"#endif\n\n");

   /*=============================================*/
   /*          Define the deffacts.               */
   /*  Adds code to the initialization function   */
   /*  to initialize some function pointers.      */
   /*=============================================*/

   deflist = NULL;
   deflist = get_next_deffact(deflist);
   if (deflist != NULL) enum_deffacts(deflist);
   
#endif

   /*====================================*/
   /* Write the initialization function. */
   /*====================================*/
   
   set_up_initializer(init_fp);

   /*=============================================*/
   /* Terminate definitions for all arrays.       */ 
   /*=============================================*/

   print_array_end(itest_fp,first_itest);
   print_array_end(ftest_fp,first_ftest);
   print_array_end(ptest_fp,first_ptest);
   print_array_end(join_fp,first_join);
   print_array_end(list_fp,first_list);
   print_array_end(pnode_fp,first_pn);
   print_array_end(comp_fp,first_dfact);

   /*=======================*/
   /* Close all open files. */
   /*=======================*/

   fclose(init_fp);
   fclose(comp_fp);
   fclose(ftest_fp);
   fclose(ptest_fp);
   fclose(itest_fp);
   fclose(pnode_fp);
   fclose(join_fp);
   fclose(list_fp);

   /*===================================*/
   /* Restore the network to a suitable */
   /* state before returning.           */
   /*===================================*/
   
   untag_entries();
   unmark_joins(patn_list);
   
   return(1);
  }
  
/******************************************/
/* PRINT_ARRAY_END:                       */
/******************************************/
print_array_end(file_ptr,flag)
  FILE *file_ptr;
  int flag;
  {
   if (flag == TRUE)
     { fprintf(file_ptr,"{ NULL };\n"); }
   else
     { fprintf(file_ptr,"};\n"); }
  }

/******************************************/
/* FILE_HEADER:  Header information to go */
/*    at the top of c rules file.         */
/******************************************/
file_header(file_ptr,c)
  FILE *file_ptr;
  char c;
  {
   /*=======================================*/
   /* Header information for include files. */
   /*=======================================*/
   
   switch(c)
     {
   
          /*==============================================*/
          /* Header information for fake test structures. */
          /*==============================================*/
   
       case 'f':
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"\n");
             fprintf(file_ptr,"struct ftest\n"); 
             fprintf(file_ptr," {\n");
             fprintf(file_ptr,"  TYPE type;\n");
             fprintf(file_ptr,"  float fvalue;\n");
             if (floatsize < maxsize)
               { 
                fprintf(file_ptr,"  char garbage[%d];\n",
                       (maxsize - floatsize) / charsize); 
               }
             fprintf(file_ptr,"  struct ftest *arg_list;\n");
             fprintf(file_ptr,"  struct ftest *next_arg;\n");
             fprintf(file_ptr," };\n\n");
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct ftest ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct ftest itests_%d[];\n",id);
             fprintf(file_ptr,"  struct ftest ftests_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct ftest huge ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct ftest huge itests_%d[];\n",id);
             fprintf(file_ptr,"  struct ftest huge ftests_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
       case 'p':
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"\n");
             fprintf(file_ptr,"struct ptest\n"); 
             fprintf(file_ptr," {\n");
             fprintf(file_ptr,"  TYPE type;\n");
             fprintf(file_ptr,"  char *s_ptr;\n");
             if (ptrsize < maxsize)
               { 
                fprintf(file_ptr,"  char garbage[%d];\n",
                         (maxsize - ptrsize) / charsize); 
               }
             fprintf(file_ptr,"  struct ptest *arg_list;\n");
             fprintf(file_ptr,"  struct ptest *next_arg;\n");
             fprintf(file_ptr," };\n\n");
             fprintf(file_ptr,"extern struct draw hash_%d[];\n\n",id);
             fprintf(file_ptr,"extern struct funtab ftd_%d[];\n",id);
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct ptest itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct ptest ftests_%d[];\n",id);
             fprintf(file_ptr,"  struct ptest ptests_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct ptest huge itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct ptest huge ftests_%d[];\n",id);
             fprintf(file_ptr,"  struct ptest huge ptests_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
       case 'i':
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"\n");
             fprintf(file_ptr,"struct itest\n"); 
             fprintf(file_ptr," {\n");
             fprintf(file_ptr,"  TYPE type;\n");
             fprintf(file_ptr,"  int index;\n");
             if (intsize < maxsize)
               { 
                fprintf(file_ptr,"  char garbage[%d];\n",
                   (maxsize - intsize) / charsize); 
               }
             fprintf(file_ptr,"  struct itest *arg_list;\n");
             fprintf(file_ptr,"  struct itest *next_arg;\n");
             fprintf(file_ptr," };\n\n");
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct itest ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct itest ftests_%d[];\n",id);
             fprintf(file_ptr,"  struct itest itests_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct itest huge ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct itest huge ftests_%d[];\n",id);
             fprintf(file_ptr,"  struct itest huge itests_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
       case 'n': 
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct test ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test ftests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct list list_%d[];\n",id);
             fprintf(file_ptr,"  extern struct internode join_%d[];\n",id);
             fprintf(file_ptr,"  struct pat_node pn_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct test huge ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test huge itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test huge ftests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct list huge list_%d[];\n",id);
             fprintf(file_ptr,"  extern struct internode huge join_%d[];\n",id);
             fprintf(file_ptr,"  struct pat_node huge pn_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
       case 'j': 
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct test ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test ftests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct list list_%d[];\n",id);
             fprintf(file_ptr,"  extern struct pat_node pn_%d[];\n",id);
             fprintf(file_ptr,"  struct internode join_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct test huge ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test huge itests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct test huge ftests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct list huge list_%d[];\n",id);
             fprintf(file_ptr,"  extern struct pat_node huge pn_%d[];\n",id);
             fprintf(file_ptr,"  struct internode huge join_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
      case 'l': 
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct internode join_%d[];\n",id);
             fprintf(file_ptr,"  struct list list_%d[] =\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct internode huge join_%d[];\n",id);
             fprintf(file_ptr,"  struct list huge list_%d[] =\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
      case 'c': 
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"#include \"deffacts.h\"\n\n");
             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"  extern struct test ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct pat_node pn_%d[];\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"  extern struct test huge ptests_%d[];\n",id);
             fprintf(file_ptr,"  extern struct pat_node huge pn_%d[];\n",id);
             fprintf(file_ptr,"#endif\n\n");
             break;
             
      case 'r':
             fprintf(file_ptr,"#include <stdio.h>\n");
             fprintf(file_ptr,"#include \"setup.h\"\n"); 
             fprintf(file_ptr,"#include \"clips.h\"\n"); 
             fprintf(file_ptr,"#include \"deffacts.h\"\n\n");

             fprintf(file_ptr,"#if ! IBM_MSC\n");
             fprintf(file_ptr,"   extern struct draw **comp_hash_%d;\n",id);
             fprintf(file_ptr,"   extern struct pat_node pn_%d[];\n",id);
             fprintf(file_ptr,"   extern struct dfact df_%d[];\n",id);
             fprintf(file_ptr,"#else\n");
             fprintf(file_ptr,"   extern struct draw huge **comp_hash_%d;\n",
                          id);
             fprintf(file_ptr,"   extern struct pat_node huge pn_%d[];\n",id);
             fprintf(file_ptr,"   extern struct dfact huge df_%d[];\n",id);
             fprintf(file_ptr,"#endif\n\n");
             fprintf(file_ptr,"   extern struct funtab ftd_%d[];\n",id);
             fprintf(file_ptr,"   extern struct draw hash_%d[];\n\n",id);
             fprintf(file_ptr,"   extern struct funtab *PTR_NOP;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_CONSTANT;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_NOTCONSTANT;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_GET_VAR;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_GET_FIELD;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_NEQ_VARS;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_EQ_VARS;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_KTAND;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_KTOR;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_EQ;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_NEQ;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_NOT;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_EQ_FIELD;\n");
             fprintf(file_ptr,"   extern struct funtab *PTR_NEQ_FIELD;");
             break;
    }
   fprintf(file_ptr,"\n");
  }

/*************************************/
/*************************************/
/******                         ******/
/****** DEFINE THE SYMBOL TABLE ******/
/******                         ******/
/*************************************/
/*************************************/

/**********************************************************/
/* PRINT_HASH: Produces the definition of the hash table. */
/**********************************************************/
print_hash()
  {
   int i;

   fprintf(comp_fp,"struct draw *temp_hash_%d[%d] = {\n",id,HASHSIZE);
   for (i = 0; i < HASHSIZE; i++)
      {
       if (symbol_table[i] == NULL) 
         { fprintf(comp_fp,"   NULL"); }
       else  
         { fprintf(comp_fp,"   &hash_%d[%d]",id,symbol_table[i]->bucket); }

       if (i + 1 != HASHSIZE) fprintf(comp_fp,",\n");
      }

    fprintf(comp_fp,"  };\n");
   }
   
/**********************************************************************/
/* UNTAG_ENTRIES      */
/**********************************************************************/
untag_entries()
  {
   struct draw *hash_ptr;
   int i;

   for (i = 0; i < HASHSIZE; i++)
     {
      hash_ptr = symbol_table[i];
      while (hash_ptr != NULL)
        { 
         hash_ptr->bucket = i;
         hash_ptr = hash_ptr->next;
        }
     }
  }
  
/**********************************************************************/
/* HASH_ENTRIES: Loops through the hash table calling enum_entries to */
/*   produce the definitions for each of the hash table entries.      */
/**********************************************************************/
hash_entries()
  {
   int i;
   struct draw *hash_ptr;
   int count;
   int number_of_entries;
   
   count = number_of_entries = 0;

   /*====================================*/
   /* Count the total number of entries. */
   /*====================================*/
   
   for (i = 0; i < HASHSIZE; i++)
     {
      hash_ptr = symbol_table[i];
      while (hash_ptr != NULL)
        { 
         number_of_entries++;
         hash_ptr = hash_ptr->next;
        }
     }
     
   /*===================*/
   /* List the entries. */
   /*===================*/
   
   for (i = 0; i < HASHSIZE; i++)
     {
      hash_ptr = symbol_table[i];
      while (hash_ptr != NULL)
        { 
         fprintf(comp_fp,"{ %d,%d,",hash_ptr->count + 1,hash_ptr->bucket);
         print_c_string(comp_fp,hash_ptr->contents);

         hash_ptr->bucket = count;
         count++;
         
         if (count == number_of_entries)
          { fprintf(comp_fp,",NULL }\n"); }
         else if (hash_ptr->next == NULL)
          { fprintf(comp_fp,",NULL },\n"); }
         else
          { fprintf(comp_fp,",&hash_%d[%d] },\n",id,count); }
     
         hash_ptr = hash_ptr->next;
        }
     }
  }

/**************************************/
/**************************************/
/******                          ******/
/****** DEFINE THE FUNCTION LIST ******/
/******                          ******/
/**************************************/
/**************************************/

/******************************************************************/
/* DEF_FUNCTIONS:  Loop through the list of function definitions, */
/*   and define them as external functions.                       */
/******************************************************************/
def_functions()
  {
   struct funtab *fctn_ptr;

   fctn_ptr = fctn_list;
   while (fctn_ptr != NULL)
     {
      fprintf(comp_fp,"extern int ");
      fprintf(comp_fp,"%s();\n",fctn_ptr->defn_name);
      fctn_ptr = fctn_ptr->next;
     }
  }

/*****************************************************/
/* ENUM_FUNCTIONS: Produce the structure definitions */
/*   for the function list.                          */
/*****************************************************/
enum_functions(fctn_ptr)
  struct funtab *fctn_ptr;
  {
   int i = 1;
      
   while (fctn_ptr != NULL)
     {
      fprintf(comp_fp,"{"); 
      fprintf(comp_fp,"\"%s\",",fctn_ptr->fun_name);
      fprintf(comp_fp,"\"%s\",",fctn_ptr->defn_name);
      fprintf(comp_fp,"'%c',",fctn_ptr->fun_type);
      fprintf(comp_fp," %s,",fctn_ptr->defn_name);

      if (fctn_ptr->next == NULL)
        { fprintf(comp_fp,"NULL }\n"); }
      else
        { fprintf(comp_fp,"&ftd_%d[%d] },\n",id,i); }
      
      i++;
      fctn_ptr = fctn_ptr->next;
     }
  }

/****************************************/
/****************************************/
/******                            ******/
/****** DEFINE THE PATTERN NETWORK ******/
/******                            ******/
/****************************************/
/****************************************/

/***********************************************************/
/* DEF_NODES:                                              */
/***********************************************************/
def_nodes(pat_ptr,above,left)
  struct pat_node *pat_ptr;
  int above, left;
  {
   int inc, save_count;
   
   while (pat_ptr != NULL)
     {
      if (first_pn == TRUE)
        {
         fprintf(pnode_fp,"{\n");
         first_pn = FALSE;
        }
      else
        { fprintf(pnode_fp,",\n"); }
        
      fprintf(pnode_fp,"   {");
      
      save_count = pnode_count;
      pnode_count++;

      inc = count_lower_nodes(pat_ptr->next_level,0);
      
      /* same_level value */
      if (pat_ptr->same_level == NULL) fprintf(pnode_fp,"NULL,");
      else fprintf(pnode_fp,"&pn_%d[%d],",id,pnode_count + inc);

      /* prev value: */
      if (pat_ptr->prev == NULL) fprintf(pnode_fp,"NULL,"); 
      else fprintf(pnode_fp,"&pn_%d[%d],",id,left);

      /* next_level value */
      if (pat_ptr->next_level == NULL) fprintf(pnode_fp,"NULL,"); 
      else fprintf(pnode_fp,"&pn_%d[%d],",id,pnode_count);

      /* last_level value */
      if (pat_ptr->last_level == NULL) fprintf(pnode_fp,"NULL,"); 
      else fprintf(pnode_fp,"&pn_%d[%d],",id,above);
 
      /* type value */
      if (pat_ptr->type == SINGLE) fprintf(pnode_fp,"SINGLE,");
      else if (pat_ptr->type == MULTIPLE) fprintf(pnode_fp,"MULTIPLE,"); 
      else if (pat_ptr->type == STOP) fprintf(pnode_fp,"STOP,");

      /* eval value */
      if (pat_ptr->eval == NULL) fprintf(pnode_fp,"NULL,"); 
      else fprintf(pnode_fp,"&%ctests_%d[%d],",tname(pat_ptr->eval),
                 id,enum_tests(pat_ptr->eval));

      /* path value */
      if (pat_ptr->path == NULL) fprintf(pnode_fp,"NULL,"); 
      else fprintf(pnode_fp,"&list_%d[%d],",id,
        def_list(pat_ptr->path));

      /* alpha value */
      fprintf(pnode_fp,"NULL}");
      
      follow_list(pat_ptr->path,FALSE);
      
      if (pat_ptr->next_level != NULL)
        { def_nodes(pat_ptr->next_level,save_count,0); }
      
      pat_ptr = pat_ptr->same_level;
      left = save_count;
     }
   
  }
  
count_lower_nodes(pat_ptr,count)
  struct pat_node *pat_ptr;
  int count;
  { 
   while (pat_ptr != NULL)
     {
      count++;
      if (pat_ptr->next_level != NULL)
        { count = count_lower_nodes(pat_ptr->next_level,count); }
        
      pat_ptr = pat_ptr->same_level;
     }
   return(count);
  }

/*************************************/
/*************************************/
/******                         ******/
/****** DEFINE A TEST STRUCTURE ******/
/******                         ******/
/*************************************/
/*************************************/

/************************************************************/
/* ENUM_TESTS: Produces the definition of a test structure. */
/*   Recurses to define test structure pointed to by the    */
/*   one being defined.                                     */
/************************************************************/
enum_tests(test_ptr)
  struct test *test_ptr;
  {

   char ttype;
   FILE *file_ptr;
   int start_count;
   int al_count, na_count;
   
   if (test_ptr->arg_list != NULL)
     { al_count = enum_tests(test_ptr->arg_list); }

   if (test_ptr->next_arg != NULL)
     { na_count = enum_tests(test_ptr->next_arg); }
     
   /*========================================*/
   /* Print out comma if previous structure. */
   /*========================================*/
   
   ttype = tname(test_ptr);
   
   if (ttype == 'p')
     {
      start_count = pcount;
      file_ptr = ptest_fp;
      if (first_ptest == TRUE)
        {
         fprintf(ptest_fp,"{\n");
         first_ptest = FALSE;
        }
      else
        { fprintf(ptest_fp,",\n"); }
      pcount++;
     }
   else if (ttype == 'i')
     {
      start_count = icount;
      file_ptr = itest_fp;
      if (first_itest == TRUE)
        {
         fprintf(itest_fp,"{\n");
         first_itest = FALSE;
        }
      else
        { fprintf(itest_fp,",\n"); }
      icount++;
     }
   else if (ttype == 'f')
     {
      start_count = fcount;
      file_ptr = ftest_fp;
      if (first_ftest == TRUE)
        {
         fprintf(ftest_fp,"{\n");
         first_ftest = FALSE;
        }
      else
        { fprintf(ftest_fp,",\n"); }
      fcount++;
     }

   /*====================================*/
   /* Define the current test structure. */
   /*====================================*/

   fprintf(file_ptr,"{");

   if (test_ptr->type == STRING) fprintf(file_ptr,"STRING,");
   else if (test_ptr->type == WORD) fprintf(file_ptr,"WORD,"); 
   else if (test_ptr->type == NUMBER) fprintf(file_ptr,"NUMBER,");
   else if (test_ptr->type == INDEX) fprintf(file_ptr,"INDEX,");  
   else if (test_ptr->type == FCALL) fprintf(file_ptr,"FCALL,");
   else if (test_ptr->type == NULL) fprintf(file_ptr,"KUNKNOWN,");
   else 
     {
      clips_system_error(2701);
      cl_exit(6);
     }
     
   /* Modified line above - GDR */
   define_test_slot(test_ptr,file_ptr);

   /*===========================*/
   /* Define the arg_list slot. */
   /*===========================*/

   if (test_ptr->arg_list == NULL)
     { fprintf(file_ptr,"NULL,"); }
   else
     { fprintf(file_ptr,"&%ctests_%d[%d],",tname(test_ptr->arg_list),id,al_count); } 

   /*===========================*/
   /* Define the next_arg slot. */
   /*===========================*/

   if (test_ptr->next_arg == NULL)
     { fprintf(file_ptr,"NULL}"); }
   else
     { fprintf(file_ptr,"&%ctests_%d[%d]}",tname(test_ptr->next_arg),id,na_count); } 
     
   return(start_count);
  }

/*************************************/
/*************************************/
/******                         ******/
/****** DEFINE THE JOIN NETWORK ******/
/******                         ******/
/*************************************/
/*************************************/

/************************************************************/
/* FOLLOW_LIST:                                             */
/************************************************************/
follow_list(list_ptr,flag)
  struct list *list_ptr;
  int flag;
  {
   while (list_ptr != NULL)
     {
      if ((list_ptr->path != NULL) ? (flag || (list_ptr->path->lhs_log == 'e')) : FALSE)
        { def_join(list_ptr->path); }
      
      list_ptr = list_ptr->next;
     }
  }
  
/************************************************************/
/* DEF_LIST: Define the set of list structures so that they */
/*   can be referenced before they are actually defined.    */
/************************************************************/
def_list(list_ptr)
  struct list *list_ptr;
  {
   int start_count;
   
   start_count = list_count;
     
   while (list_ptr != NULL)
     {
      if (first_list == TRUE)
        {
         fprintf(list_fp,"{\n");
         first_list = FALSE;
        }
      else
        { fprintf(list_fp,",\n"); }
      list_count++;
      fprintf(list_fp,"{");

      /* path value */
      if (list_ptr->path == NULL)
        { fprintf(list_fp,"NULL,"); }
      else
        { 
         fprintf(list_fp,"&join_%d[%ld],",id,list_ptr->path->id); 
        }

      /* next value */
      if (list_ptr->next == NULL)
        { fprintf(list_fp,"NULL}"); }
      else
        { fprintf(list_fp,"&list_%d[%d]}",id,list_count); }
      
      list_ptr = list_ptr->next;
     }
     
   return(start_count);
  }

/************************************************************/
/* DEF_JOIN: Define the set of join structures so that they */
/*   can be referenced before they are actually defined.    */
/************************************************************/
def_join(join_ptr)
  struct internode *join_ptr;
  {
   int list_num;
   
   if (first_join == TRUE)
     {
      fprintf(join_fp,"{\n");
      first_join = FALSE;
     }
   else
     { fprintf(join_fp,",\n"); }
     
   join_count++;
   
   if (join_ptr->next != NULL)
     { list_num = def_list(join_ptr->next); }
       
   fprintf(join_fp,"{");

   /* beta value */
   fprintf(join_fp,"NULL,");

   /* lhs_log value */
   fprintf(join_fp,"'%c',",join_ptr->lhs_log);

   /* rhs_log value */
   fprintf(join_fp,"'%c',",join_ptr->rhs_log);

   /* eval value */
   if (join_ptr->eval == NULL)
     { fprintf(join_fp,"NULL,"); }
   else    
     { fprintf(join_fp,"&%ctests_%d[%d],",tname(join_ptr->eval),id,enum_tests(join_ptr->eval)); }
     
   /* not_eval value */
   if (join_ptr->not_eval == NULL)
     { fprintf(join_fp,"NULL,"); }
   else    
     { fprintf(join_fp,"&%ctests_%d[%d],",tname(join_ptr->not_eval),id,
              enum_tests(join_ptr->not_eval)); }

   /* id value */
   fprintf(join_fp,"0,");

   /* next value */
   if (join_ptr->next == NULL)
     { fprintf(join_fp,"NULL,"); }
   else
     { fprintf(join_fp,"&list_%d[%d],",id,list_num); }

   /* join_above value */
   fprintf(join_fp,"NULL,");

   /* entry_pat value */
   if (join_ptr->entry_pat == NULL)
     { fprintf(join_fp,"NULL}"); }
   else
     { fprintf(join_fp,"&pn_%d[%ld]}",id,join_ptr->beta->count); } 
   if (join_ptr->next != NULL)
     { follow_list(join_ptr->next,TRUE); }
  }

/*********************************/
/*********************************/
/******                     ******/
/****** DEFINE THE DEFFACTS ******/
/******                     ******/
/*********************************/
/*********************************/

#if DEFFACTS_CONSTRUCT

/**********************************************************/
/* ENUM_DEFFACTS:  Produces all the deffacts definitions. */
/**********************************************************/
int enum_deffacts(df_ptr)
  struct dfact *df_ptr;
  {

   if (first_dfact == TRUE)
     {
       fprintf(comp_fp,"{");
       first_dfact = FALSE;
     }
   else
     { fprintf(comp_fp,",\n"); }

   fprintf(comp_fp,"{ \"\",NULL,");
   
   /*===============================================================*/
   /* Define the fact lists associated with the current definition. */
   /* fact_ct has the index to the next fact structure to be de-    */
   /* fined.                                                        */
   /*===============================================================*/

   if (df_ptr->alist == NULL) fprintf(comp_fp,"NULL,"); 
   else fprintf(comp_fp,"&%ctests_%d[%d],",tname(df_ptr->alist),
                 id,enum_tests(df_ptr->alist));

   /*===========================================*/
   /* Point to the next definition in the list. */
   /*===========================================*/

   dfact_ct++;
   if (df_ptr->next == NULL)
     { fprintf(comp_fp,"NULL }"); }
   else
     { 
       fprintf(comp_fp,"&df_%d[%d] }",id,dfact_ct); 

      /*======================================================*/
      /* Produce definitions that come after this definition. */
      /*======================================================*/

       enum_deffacts(df_ptr->next);
     }
  }
#endif

/************************************************************/
/* SET_UP_INITIALIZER: Produces the initialization function */
/*   for installing this rule set.                          */
/************************************************************/
set_up_initializer(file_ptr)
FILE *file_ptr;
  {
   fprintf(file_ptr,"\n\n");
   fprintf(file_ptr,"/***********************************/\n");
   fprintf(file_ptr,"/* C RULES INITIALIZATION FUNCTION */\n");
   fprintf(file_ptr,"/***********************************/\n\n");

   fprintf(file_ptr,"\ninit_c_rules_%d()\n",id);
   fprintf(file_ptr,"  {\n");
   fprintf(file_ptr,"   extern int           set_symbol_table();\n");
   fprintf(file_ptr,"   extern int           set_fctn_list();\n");
   fprintf(file_ptr,"   extern int           set_network_pointer();\n");
#if DEFFACTS_CONSTRUCT
   fprintf(file_ptr,"   extern int           set_deflist();\n");
#endif

   fprintf(file_ptr,"\n   set_symbol_table(comp_hash_%d);\n",id);
   if (pnode_count != 0)
     { fprintf(file_ptr,"   set_network_pointer(pn_%d);\n",id); }
   else
     { fprintf(file_ptr,"   set_network_pointer(NULL);\n"); }
#if DEFFACTS_CONSTRUCT
   if (dfact_ct != 0)
     { fprintf(file_ptr,"   set_deflist(df_%d);\n",id); }
   else
     { fprintf(file_ptr,"   set_deflist(NULL);\n"); }
#endif
   fprintf(file_ptr,"   set_fctn_list(ftd_%d);\n\n",id);
   fprintf(file_ptr,"   init_gen_ptrs();\n\n");

   fprintf(init_fp,"  }\n");
  }
  
/******************************/
/* TNAME:                     */
/******************************/
char tname(test_ptr)
  struct test *test_ptr;
  {
   if (test_ptr->type == STRING) return('p');
   else if (test_ptr->type == WORD) return('p'); 
   else if (test_ptr->type == NUMBER) return('f'); 
   else if (test_ptr->type == INDEX) return('i'); 
   else if (test_ptr->type == FCALL) return('p');
   else if (test_ptr->type == NULL) return('p');
   else return('f');
  }

/*****************************************/
/* DEFINE_TEST_SLOT:                     */
/*****************************************/
define_test_slot(test_ptr,file_ptr)
  struct test *test_ptr;
  FILE *file_ptr;
  {
  /* Modified this portion of code - GDR  */

   if ((test_ptr->type == STRING) || (test_ptr->type == WORD))
     { 
      fprintf(file_ptr,"(char *) &hash_%d[%d],",id,
              test_ptr->val.hvalue->bucket);
      fill_string(ptrsize,file_ptr); 
     }
   else if (test_ptr->type == NUMBER) 
     {
      fprintf(file_ptr,"%f,",test_ptr->val.fvalue);
      fill_string(floatsize,file_ptr);  
     }
   else if (test_ptr->type == INDEX)  
     {
      fprintf(file_ptr,"%d,",test_ptr->val.index);
      fill_string(intsize,file_ptr); 
     }
   else if (test_ptr->type == FCALL)
     { 
      fprintf(file_ptr,"(char *) &ftd_%d[%d],",id,
               find_func_index(test_ptr->val.fun_ptr)); 
      fill_string(ptrsize,file_ptr);  
     }
   else if (test_ptr->type == NULL)
     {
      fprintf(file_ptr,"NULL,");
      fill_string(ptrsize,file_ptr);  
     }
   else 
     {
      fprintf(file_ptr,"%f,",test_ptr->val.fvalue);
      fill_string(floatsize,file_ptr);  
     }
     
   /* Don't need slot for rule action link */
  }

/************************************************************/
/* FILL_STRING:        */
/************************************************************/
fill_string(size,file_ptr)
  int size;
  FILE *file_ptr;
  {
   if (size == maxsize) return;
   
   fprintf(file_ptr,"\"");
   while (size > 1)
     { 
      fputc(' ',file_ptr);
      size--;
     }
   fprintf(file_ptr,"\",");
  }
  
/************************************************************/
/* PRINT_C_STRING:        */
/************************************************************/
print_c_string(file_ptr,str)
  FILE *file_ptr;
  char *str;
  {
   int i, slen;
   
   fprintf(file_ptr,"\"");
   slen = strlen(str);
   for (i = 0 ; i < slen ; i++)
     {
      if ((str[i] == '"') || (str[i] == '\\'))
        { 
         fputc('\\',file_ptr);
         fputc(str[i],file_ptr);
        }
      else if (str[i] == '\n')
        {
         fputc('\\',file_ptr);
         fputc('n',file_ptr);
        }
      else
        { fputc(str[i],file_ptr); }
     }
   
   fprintf(file_ptr,"\"");
  }
  
/************************************************************/
/* FIND_FUNCTION_INDEX        */
/************************************************************/
find_func_index(fun_ptr)
  struct funtab *fun_ptr;
  {
   struct funtab *fun_list;
   int i = 0;

   fun_list = fctn_list;
   while ((fun_list != NULL) && (fun_list != fun_ptr))
     { 
      i++;
      fun_list = fun_list->next; 
     }
     
   return(i);
  }
  
  
  
/************************************************************************/
/* MARK_JOINS:                                              */
/************************************************************************/
mark_joins(pat_ptr,count)
  struct pat_node *pat_ptr;
  int count;
  {
   struct list *list_ptr;
   int temp_id;
   
   while (pat_ptr != NULL)
     {
      temp_id = mark_pat_count;
      mark_pat_count++;
      if (pat_ptr->next_level == NULL)
        { 
         list_ptr = pat_ptr->path;
         while (list_ptr != NULL)
           {
             list_ptr->path->beta = get_struct(flink);
             list_ptr->path->beta->binds = NULL;
             list_ptr->path->beta->next = NULL;
             list_ptr->path->beta->count = temp_id;
             list_ptr = list_ptr->next;
           }
         count = mark_lists(pat_ptr->path,count,FALSE); 
        }
      else
        { count = mark_joins(pat_ptr->next_level,count); }
        
      pat_ptr = pat_ptr->same_level;
     }
     
   return(count);
  }
  
/************************************************************************/
/* MARK_LISTS:                                              */
/************************************************************************/
mark_lists(list_ptr,count,flag)
  struct list *list_ptr;
  int count, flag;
  {
   while (list_ptr != NULL)
     {
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         list_ptr->path->id = count;
         count++;
         count = mark_lists(list_ptr->path->next,count,TRUE);
        }
      list_ptr = list_ptr->next;
     }
     
   return(count);
  }
  

/************************************************************************/
/* UNMARK_JOINS:                                              */
/************************************************************************/
unmark_joins(pat_ptr)
  struct pat_node *pat_ptr;
  {
   struct list *list_ptr;
   
   while (pat_ptr != NULL)
     {
      if (pat_ptr->next_level == NULL)
       {
         list_ptr = pat_ptr->path;
         while (list_ptr != NULL)
          { 
             rtn_struct(flink,list_ptr->path->beta);
             list_ptr->path->beta = NULL;
           list_ptr = list_ptr->next;
          }
         unmark_lists(pat_ptr->path,FALSE);
        }
      else
        { unmark_joins(pat_ptr->next_level); }
        
      pat_ptr = pat_ptr->same_level;
     }
    
  }
  
/************************************************************************/
/* UNMARK_LISTS:                                              */
/************************************************************************/
unmark_lists(list_ptr,flag)
  struct list *list_ptr;
  int flag;
  {
   while (list_ptr != NULL)
     {
      if ((list_ptr->path->lhs_log == 'e') || flag)
        {
         list_ptr->path->id = 0;
         unmark_lists(list_ptr->path->next,TRUE);
        }
      list_ptr = list_ptr->next;
     }
  }

#else

      /*====================================*/
      /* Definition for rule compiler stub. */
      /*====================================*/

   int                      rules_to_c_code() {};

#endif
