




	/*****************************************************************
	 * starhack.c			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Initialization code for the SUN workstation when equipped with
	 *   a SKY floating point accessory board.  This code resets a
	 *   floating point table and thereby allows the program to be
	 *   reentered if saved by the STAR "suspend" command.  There are
	 *   no arguments to the call to "starinit".
	 *
	 * If STAR is installed on a computer other than the SUN, the
	 *   conditional compilation statements below should result in
	 *   the function "starinit" simply returning when called.
	 *
	 * Warning: very SUN dependent.  Only tested under SUN 2.0.
	 *****************************************************************/


starinit()
	/* Initialize floating point code vectors and optional Sky board. */
  {

#ifdef sun
  extern char *_skybase;
  if (_skybase)
    {
    free(_skybase - 4);	/* Just to conserve memory. */
    _skybase = 0;	/* Convince _skyinit() to really initialize. */
    }
  vfloat_();		/* _skyinit(), then set code vectors */
#endif /* sun */

  }
