/*   CLIPS Version 4.30   4/25/89 */
 
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                    MEMORY MODULE                    */
   /*******************************************************/

#include <stdio.h>

#include "setup.h"
#include "constant.h"
#include "clipsmem.h"

/****************************************/
/* GLOBAL INTERNAL FUNCTION DEFINITIONS */
/****************************************/

   char                   *gm1();
   char                   *gm2();
   int                     rm();
   int                     release_mem();
   unsigned long           actual_pool_size();
   unsigned long           pool_size();
   
/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/
   
   extern char            *genalloc();

/****************************************/
/* GLOBAL INTERNAL VARIABLE DEFINITIONS */
/****************************************/

   struct mem_ptr         *temp_mem_ptr;
   struct mem_ptr         *memory_table[MEM_TABLE_SIZE];
  
/*****************************************************************/
/* RELEASE_MEM:  Returns all the lists of stored data structures */
/*   to the pool of free memory.                                 */
/*****************************************************************/
release_mem(maximum,print_message)
  int maximum;
  int print_message;
  {
   struct mem_ptr *tmp_mem, *mptr;
   int i;
   int amount = 0;
   
   if (print_message == TRUE)
     { cl_print("wdialog","\n*** DEALLOCATING MEMORY ***\n"); }
   
   for (i = (MEM_TABLE_SIZE - 1) ; i >= sizeof(char *) ; i--)
     {
      mptr = memory_table[i];
      while (mptr != NULL)
        {
         tmp_mem = mptr->next;
         genfree(mptr,i);
         mptr = tmp_mem;
         amount += i;
        }
      memory_table[i] = NULL;
      if ((amount > maximum) && (maximum > 0)) 
        {
         if (print_message == TRUE)
           { cl_print("wdialog","*** MEMORY  DEALLOCATED ***\n"); }
         return;
        }
     }
   
   if (print_message == TRUE)
     { cl_print("wdialog","*** MEMORY  DEALLOCATED ***\n"); }
  }
  
/*****************************************************************/
/* GM1: Allocates memory and sets all bytes to zero.             */
/*****************************************************************/
char *gm1(size)
  {
   struct mem_ptr *strmem;
   char *tmp_ptr;
   int i;
   
   if (size < sizeof(char *)) size = sizeof(char *);
   
   if (size >= MEM_TABLE_SIZE) return(genalloc(size));
   
   strmem = (struct mem_ptr *) memory_table[size];
   if (strmem == NULL) 
     {
      tmp_ptr = genalloc(size); 
      for (i = 0 ; i < size ; i++) 
        { tmp_ptr[i] = '\0'; }
      return(tmp_ptr);
     }
   
   memory_table[size] = strmem->next;
   
   tmp_ptr = (char *) strmem;
   for (i = 0 ; i < size ; i++) 
     { tmp_ptr[i] = '\0'; }
   
   return (tmp_ptr);
  }

/*****************************************************************/
/* GM2: Allocates memory and does not initialize it.             */
/*****************************************************************/
char *gm2(size)
  {
   struct mem_ptr *strmem;
   
   if (size < sizeof(char *)) size = sizeof(char *);
   
   if (size >= MEM_TABLE_SIZE) return(genalloc(size));
   
   strmem = (struct mem_ptr *) memory_table[size];
   if (strmem == NULL) 
     {
      return(genalloc(size)); 
     }
   
   memory_table[size] = strmem->next;

   return ( (char *) strmem);
  }
  
/*****************************************************************/
/* RM: Returns memory.                                          */
/*****************************************************************/
rm(str,size)
  char *str;
  int size;
  {
   struct mem_ptr *strmem;
   
   if (size < sizeof(char *)) size = sizeof(char *);
   
   if (size >= MEM_TABLE_SIZE) return(genfree(str,size));
   
   strmem = (struct mem_ptr *) str;
   strmem->next = memory_table[size];
   memory_table[size] = strmem;
   return(1);
  }

/***********************************************************/
/* POOL_SIZE : Returns number of bytes in CLIPS free pool. */
/***********************************************************/
unsigned long pool_size()
  {
   register int i;
   struct mem_ptr *ptr;
   unsigned long cnt = 0;

   for (i = sizeof(char *) ; i < MEM_TABLE_SIZE ; i++)
     {
      ptr = memory_table[i];
      while (ptr != NULL)
        {
         cnt += (unsigned long) i;
         ptr = ptr->next;
        }
     }
   return(cnt);
  }

/***************************************************************/
/* ACTUAL_POOL_SIZE : Returns number of bytes DOS requires to  */
/*   store the CLIPS free pool.  This routine is functionally  */
/*   equivalent to pool_size on anything other than the IBM-PC */
/***************************************************************/
unsigned long actual_pool_size()
  {
#if IBM_TBC
   register int i;
   struct mem_ptr *ptr;
   unsigned long cnt = 0;

   for (i = sizeof(char *) ; i < MEM_TABLE_SIZE ; i++)
     {
      ptr = memory_table[i];
      while (ptr != NULL)
        {
         /*==============================================================*/
         /* For a block of size n, the Turbo-C Library routines require  */
         /* a header of size 8 bytes and further require that all memory */
         /* allotments be paragraph (16-bytes) aligned.                  */
         /*==============================================================*/
         
         cnt += (((unsigned long) i) + 23L) & 0xfffffff0;
         ptr = ptr->next;
        }
     }
   return(cnt);
#else
   return(pool_size());
#endif
  }

/*****************************************************************/
/* BLOCK MEMORY FUNCTIONS:                                       */
/*****************************************************************/

#if BLOCK_MEMORY

struct mem_info
  {
   struct mem_info *prev_mem;
   struct mem_info *next_free;
   struct mem_info *last_free;
   long int size;
  };

struct buf_info
  {
   struct buf_info *next_buf;
   struct buf_info *prev_buf;
   struct mem_info *buf_top_mem;
   long int size;
  };

static struct buf_info   *top_buf;
static int                buf_info_size;
static int                info_size;
static int                init_alloc = FALSE;

/***********************/
/* init_mem:           */
/***********************/
init_mem(req_size)
  unsigned int req_size;
  {
   extern char *malloc();
   struct mem_info *mem_buf;
   unsigned int init_req, usable_buf_size;
  
   if (sizeof(char) != 1)
     {
      printf("Size of character data is not 1\n");
      printf("Memory allocation functions may not work\n");
      return(0);
     }
    
   info_size = sizeof(struct mem_info);
   info_size = (((info_size - 1) / 4) + 1) * 4;
   
   buf_info_size = sizeof(struct buf_info);
   buf_info_size = (((buf_info_size - 1) / 4) + 1) * 4;
   
   init_req = (INITBUFFERSIZE > req_size ? INITBUFFERSIZE : req_size);
   init_req += info_size * 2 + buf_info_size;
   init_req = (((init_req - 1) / 4) + 1) * 4;
   
   usable_buf_size = init_req - (2 * info_size) - buf_info_size;

   /* make sure we get a buffer big enough to be usable */
   if ((req_size < INITBUFFERSIZE) &&
       (usable_buf_size <= req_size + info_size))
     {
      init_req = req_size + info_size * 2 + buf_info_size;
      init_req = (((init_req - 1) / 4) + 1) * 4;
      usable_buf_size = init_req - (2 * info_size) - buf_info_size;
     }
     
   top_buf = (struct buf_info *) malloc(init_req);

   if (top_buf == NULL)
     {
      printf("Unable to allocate initial memory pool\n");
      return(0);
     } 

   top_buf->next_buf = NULL;
   top_buf->prev_buf = NULL;
   top_buf->buf_top_mem = (struct mem_info *) ((char *) top_buf + buf_info_size);
   top_buf->size = usable_buf_size;

   mem_buf = (struct mem_info *) ((char *) top_buf + buf_info_size + info_size + usable_buf_size);
   mem_buf->next_free = NULL;
   mem_buf->last_free = NULL;
   mem_buf->prev_mem = top_buf->buf_top_mem;
   mem_buf->size = 0;

   top_buf->buf_top_mem->next_free = NULL;
   top_buf->buf_top_mem->last_free = NULL;
   top_buf->buf_top_mem->prev_mem = NULL;
   top_buf->buf_top_mem->size = usable_buf_size;

   init_alloc = TRUE;
   return(1);
  }

/********************************/
/* init_new_buffer:             */
/********************************/
init_new_buffer(buf_ptr,req_size)
  struct buf_info *buf_ptr;
  unsigned int req_size;
  {
   extern char *malloc(); 
   
   unsigned int buf_size,usable_buf_size;
   struct buf_info *new_buf;
   struct mem_info *top_new_buf;
   
   buf_size = (BUFFERSIZE > req_size ? BUFFERSIZE : req_size);
   buf_size += buf_info_size + info_size * 2;
   buf_size = (((buf_size - 1) / 4) + 1) * 4;
   
   usable_buf_size = buf_size - buf_info_size - (2 * info_size);
   
   /* make sure we get a buffer big enough to be usable */
   if ((req_size < BUFFERSIZE) &&
       (usable_buf_size <= req_size + info_size))
     {
      buf_size = req_size + info_size * 2 + buf_info_size;
      buf_size = (((buf_size - 1) / 4) + 1) * 4;
      usable_buf_size = buf_size - (2 * info_size) - buf_info_size;
     }

   new_buf = (struct buf_info *) malloc(buf_size);

   if (new_buf == NULL) return(0);

   new_buf->next_buf = NULL;
   new_buf->prev_buf = buf_ptr;
   new_buf->buf_top_mem = (struct mem_info *) ((char *) new_buf + buf_info_size);
   new_buf->size = usable_buf_size;
   buf_ptr->next_buf = new_buf;
   
   top_new_buf = (struct mem_info *) ((char *) new_buf + buf_info_size + info_size + usable_buf_size);
   top_new_buf->next_free = NULL;
   top_new_buf->last_free = NULL;
   top_new_buf->size = 0;
   top_new_buf->prev_mem = new_buf->buf_top_mem;
   
   new_buf->buf_top_mem->next_free = NULL;
   new_buf->buf_top_mem->last_free = NULL;
   new_buf->buf_top_mem->prev_mem = NULL;
   new_buf->buf_top_mem->size = usable_buf_size;
 
   return(1);
  }
  
/********************************/
/* request_block:               */
/********************************/
char *request_block(req_size)  
  unsigned int req_size;
  {
   struct mem_info *mem_ptr, *last_free;
   struct buf_info *buf_ptr;

   /*==================================================*/
   /* Allocate initial memory pool block if it has not */
   /* already been allocated.                          */
   /*==================================================*/
   
   if (init_alloc == FALSE) 
      {
       if (init_mem(req_size) == 0) return(NULL);
      }
 
   /*====================================================*/
   /* Make sure that the amount of memory requested will */
   /* fall on a word boundary.  Assume a word size of 4. */
   /*====================================================*/
   
   req_size = (((req_size - 1) / 4) + 1) * 4;

   /*=====================================================*/
   /* Search through the list of free memory for a block  */
   /* of the appropriate size.  If a block is found, then */
   /* allocate and return a pointer to it.                */
   /*=====================================================*/

   buf_ptr = top_buf;
   
   while (buf_ptr != NULL)
     {
      mem_ptr = buf_ptr->buf_top_mem;

      while (mem_ptr != NULL)
        {
         if ((mem_ptr->size == req_size) ||
             (mem_ptr->size > (req_size + info_size)))
           {
            allocate_block(buf_ptr,mem_ptr,req_size);
   
            return((char *) mem_ptr + info_size);
           }
         mem_ptr = mem_ptr->next_free;
        }
      
      if (buf_ptr->next_buf == NULL)
        {
         if (init_new_buffer(buf_ptr,req_size) == 0)  /* get another buffer */
           { return(NULL); }
        }
      buf_ptr = buf_ptr->next_buf;
     }
     
   clips_system_error(1502);
   cl_exit(1);
  } 

/*************************/
/* allocate_block:       */
/*************************/
allocate_block(buffer_ptr,block_ptr,req_size)
  struct buf_info *buffer_ptr;
  struct mem_info *block_ptr;
  unsigned int req_size;
  {
   struct mem_info *split_mem, *next_mem;

   /*================================================*/
   /* Size of memory block is an exact match for the */
   /* requested amount of memory.                    */
   /*================================================*/
   if (req_size == block_ptr->size)
     {
      block_ptr->size = - (long int) req_size;
      if (block_ptr->last_free == NULL)
        {
         if (block_ptr->next_free != NULL)
           { buffer_ptr->buf_top_mem = block_ptr->next_free; }
         else
           { buffer_ptr->buf_top_mem = (struct mem_info *) ((char *) buffer_ptr + buf_info_size + info_size + buffer_ptr->size); }
        
        }
      else
        { block_ptr->last_free->next_free = block_ptr->next_free; }

      if (block_ptr->next_free != NULL)
        { block_ptr->next_free->last_free = block_ptr->last_free; }

      block_ptr->last_free = NULL;
      block_ptr->next_free = NULL;
      return(1);
     }

   /*=============================================*/
   /* Memory block is larger than memory request. */
   /* Split it.                                   */
   /*=============================================*/

   next_mem = (struct mem_info *)
              ((char *) block_ptr + info_size + block_ptr->size);

   split_mem = (struct mem_info *) 
                  ((char *) block_ptr + (info_size + req_size));

   split_mem->size = block_ptr->size - (req_size + info_size);
   split_mem->prev_mem = block_ptr;

   split_mem->next_free = block_ptr->next_free;
   split_mem->last_free = block_ptr->last_free;

   next_mem->prev_mem = split_mem;
   
   if (split_mem->last_free == NULL)
     { buffer_ptr->buf_top_mem = split_mem; }
   else
     { split_mem->last_free->next_free = split_mem; }

   if (split_mem->next_free != NULL)
     { split_mem->next_free->last_free = split_mem; }

   block_ptr->size = - (long int) req_size;
   block_ptr->last_free = NULL;
   block_ptr->next_free = NULL;
   
   return(1);
  }

/*************************/
/* return_block:         */
/*************************/
return_block(mem_ptr,size)
  char *mem_ptr;
  unsigned int size;
  {
   struct mem_info *info_ptr, *last_mem, *next_mem, *nn_mem;
   struct buf_info *buf_ptr;

   size = (((size - 1) / 4) + 1) * 4;

   info_ptr = (struct mem_info *) ((char *) mem_ptr - info_size);

   if (info_ptr == NULL) 
     { return(-1); }

   if (info_ptr->size >= 0) 
     { return(-1); }
   
   if (info_ptr->size != - (long int) size) 
     { return(-1); }

   info_ptr->size = - info_ptr->size;  
   
   /* find out which buffer you're in */
   nn_mem = info_ptr;
   while (nn_mem->prev_mem != NULL)
     { nn_mem = nn_mem->prev_mem; }
   buf_ptr = (struct buf_info *) ((char *) nn_mem - buf_info_size);

   last_mem = info_ptr->prev_mem;
   next_mem = (struct mem_info *) ((char *) mem_ptr + size);

   if (buf_ptr->buf_top_mem == NULL) 
     { return(-1); } 

   buf_ptr->buf_top_mem->last_free = info_ptr;
   info_ptr->next_free = buf_ptr->buf_top_mem;
   info_ptr->last_free = NULL;
   
   buf_ptr->buf_top_mem = info_ptr;

   /*=====================================================*/
   /* Combine this block with previous block if possible. */
   /*=====================================================*/

   if (last_mem != NULL)
     {
      if (last_mem->size > 0)
        {
         last_mem->size += (info_size + info_ptr->size);

         if (next_mem != NULL)
           { next_mem->prev_mem = last_mem; }
         else 
           { return(-1); }

         /* Detach last free from list. */

         if (last_mem->last_free != NULL)
           { last_mem->last_free->next_free = last_mem->next_free; }

         if (last_mem->next_free != NULL)
           { last_mem->next_free->last_free = last_mem->last_free; }

         last_mem->next_free = info_ptr->next_free;
         if (info_ptr->next_free != NULL)
           { info_ptr->next_free->last_free = last_mem; }
         last_mem->last_free = NULL;
         buf_ptr->buf_top_mem = last_mem;
         info_ptr->last_free = NULL;
         info_ptr->next_free = NULL;
         info_ptr = last_mem;
        }
     }

   /*=================================================*/
   /* Combine this block with next block if possible. */
   /*=================================================*/

   if (next_mem == NULL) return(-1);
   if (info_ptr == NULL) return(-1);

   if (next_mem->size > 0)
     {
      info_ptr->size += (info_size + next_mem->size);
      
      nn_mem = (struct mem_info *) ((char *) next_mem + next_mem->size + info_size);
      if (nn_mem != NULL)
        { nn_mem->prev_mem = info_ptr; }
      else 
        { return(-1); }

      if (next_mem->last_free != NULL)
        { next_mem->last_free->next_free = next_mem->next_free; }

      if (next_mem->next_free != NULL)
        { next_mem->next_free->last_free = next_mem->last_free; }
        
     }
   
   /* Free the buffer if we can, but */
   /* don't free the first buffer    */
   /* if it's the only one.          */
   
   if ((info_ptr->prev_mem == NULL) &&
       (info_ptr->size == buf_ptr->size))
     {
      if (buf_ptr->prev_buf != NULL)
        { 
         buf_ptr->prev_buf->next_buf = buf_ptr->next_buf;
         if (buf_ptr->next_buf != NULL)
           { buf_ptr->next_buf->prev_buf = buf_ptr->prev_buf; }
         free((char *) buf_ptr);
        }
      else
        {
         if (buf_ptr->next_buf != NULL)
           {
            buf_ptr->next_buf->prev_buf = NULL;
            top_buf = buf_ptr->next_buf;
            free((char *) buf_ptr);
           }
        }
     }     

   return(1);
  }

#endif

