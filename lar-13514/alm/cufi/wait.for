CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   WAIT - Wait for a Given Number of Seconds Subroutine
C
C   VERSION:  1.0
C   AUTHOR:  A. Roberts, Tesseract Systems, Inc.
C   CREATION DATE:  05/15/83
C   LAST REVISION DATE:  05/20/83
C   LAST REVISED BY:  A. Roberts
C
C FUNCTION:  WAIT is called to delay for the specified number of
C		seconds, then return to the caller.  It makes use
C		of VAX/VMS timer system services to do this.
C
C USEAGE:  CALL WAIT(SECS)
C
C ARGUMENTS:	SECS - Input Number of Seconds to Wait	(REAL*4)
C
C ERRORS:  None known
C
C NOTES:  Written in VAX/VMS FORTRAN. 
C
C PROCESS:  The only reason for the existance of WAIT is the
C		relatively short length of time available with
C		a single system service call, as well as the
C		reasonably difficult time coding.  WAIT will
C		break a long time request into shorter requests
C		if need be, and call timer services for each
C		short request.
C
C REVISION HISTORY:  0.0 - Created on 05/15/83 by A. Roberts, 
C			Tesseract Systems, Inc.
C					------
C		     1.0 - Initial release version.  05/20/83,
C			A. Roberts
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


	subroutine WAIT(SECS)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Variable Declaration, Documentation, Constants, etc.
C
C Outside of the argument, there is little to WAIT, except a working
C copy of the time and the argument to the system service.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	implicit none

	parameter BIG = 2147482752.0		! Max time on a call
	parameter WAIT_EFN = 50			! Event flag used

	real*4 secs				! Input Seconds

	real*4 timecop				! Working Variables
	integer*4 itime(2)

	external sys$setimr			! System services used
	external sys$waitfr

C
C Initialize the ITIME array to be the largest possible
C call for the system timer routine.  
C
C Also, convert the SECS argument to the required units for
C the system call.
C

	itime(2) = -1
	itime(1) = int(-1.0 * BIG)

	timecop = secs * 1.0E+07

C
C Now decide what to do.  If the required time is small enough,
C make one call to the system service and then return.  Otherwise,
C loop, making the largest possible calls and removing that
C value from the user specified time until the time has elasped.
C

C
C Time is small enough
C
	if(timecop .lt. BIG) then 

		itime(1) = int(-1.0 * timecop)

		call sys$setimr(%val(WAIT_EFN),itime,,)
		call sys$waitfr(%val(WAIT_EFN))

C
C Time is not small enough
C
	else 

		do while(timecop .ge. BIG) 

			call sys$setimr(%val(WAIT_EFN),itime,,)
			call sys$waitfr(%val(WAIT_EFN))

			timecop = timecop - BIG

		end do

			itime(1) = int(-1.0 * timecop)

			call sys$setimr(%val(WAIT_EFN),itime,,)
			call sys$waitfr(%val(WAIT_EFN))

	end if

C
C Thats all.  Return to the caller.
C
	
	return
	end
