/*   CLIPS Version 4.30   4/25/89 */

/**************************/
/* I/O ROUTER DEFINITIONS */
/**************************/

struct router
  {
   char *name;
   int active;
   int priority;
   int (*query)();
   int (*printer)();
   int (*exiter)();
   int (*charget)();
   int (*charunget)();
   struct router *next;
  };

struct str_router
  {
   char *name;
   char *str;
   int cur_pos;
   int max_pos;
   struct str_router *next;
  };
