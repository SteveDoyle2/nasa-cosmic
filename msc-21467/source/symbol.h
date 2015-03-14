/*   CLIPS Version 4.30   4/25/89 */

#ifndef _SYMBOL_

#define _SYMBOL_
#define HASHSIZE        167

/************************************************************/
/* DRAW STRUCTURE:                                          */
/************************************************************/
struct draw
  {
   int count;
   int bucket;
   char *contents;
   struct draw *next;
  };

/************************************************************/
/* EPH_DRAW STRUCTURE:                                      */
/************************************************************/
struct eph_draw
  {
   struct draw *watch_draw;
   struct eph_draw *next;
  };
  
#endif

