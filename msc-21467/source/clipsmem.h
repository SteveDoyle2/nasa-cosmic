/*   CLIPS Version 4.30   4/25/89 */

#define MEM_TABLE_SIZE 300

struct mem_ptr
  {
   struct mem_ptr *next;
  };
  
extern struct mem_ptr *temp_mem_ptr;
extern struct mem_ptr *memory_table[];
extern char *genalloc();
                                                                     
#define get_struct(type) \
  ((memory_table[sizeof(struct type)] == NULL) ? \
   ((struct type *) genalloc(sizeof(struct type))) :\
   ((temp_mem_ptr = memory_table[sizeof(struct type)]),\
    memory_table[sizeof(struct type)] = temp_mem_ptr->next,\
    ((struct type *) temp_mem_ptr)))
    
#define rtn_struct(type,struct_ptr) \
  (temp_mem_ptr = (struct mem_ptr *) struct_ptr,\
   temp_mem_ptr->next = memory_table[sizeof(struct type)], \
   memory_table[sizeof(struct type)] = temp_mem_ptr)
   
extern char *gm1(), *gm2();
   
