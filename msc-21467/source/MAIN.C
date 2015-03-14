/*   CLIPS Version 4.30   4/25/89 */

#include <stdio.h>

/***************************************************************/
/* MAIN: Start execution of CLIPS.  This function must be      */
/*   redefined in order to embed CLIPS within another program. */
/*   Example of redefined main:                                */
/*     main()                                                  */
/*       {                                                     */
/*        init_clips();                                        */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        process_data();                                      */
/*        run(-1);                                             */
/*        evaluate_data();                                     */
/*            .                                                */
/*            .                                                */
/*            .                                                */
/*        final_results();                                     */
/*       }                                                     */
/***************************************************************/
main(argc,argv)
  int argc ;
  char *argv[] ;
  {
   init_clips();   
   reroute_stdin(argc,argv);
   cl_print("wclips","         CLIPS (V4.30 5/5/89)\n");
   command_loop();
  }
  
/*************************************************************/
/* USRFUNCS:  The function which informs CLIPS of any user   */
/*   defined functions.  In the default case, there are no   */
/*   user defined functions.  To define functions, either    */
/*   this function must be replaced by a function with the   */
/*   same name within this file, or this function can be     */
/*   deleted from this file and included in another file.    */
/*   User defined functions may be included in this file or  */
/*   other files.                                            */
/*   Example of redefined usrfuncs:                          */
/*     usrfuncs()                                            */
/*       {                                                   */
/*        define_function("fun1",'i',fun1,"fun1");           */
/*        define_function("other",'f',other,"other");        */
/*       }                                                   */
/*************************************************************/
usrfuncs()
  {
  }

