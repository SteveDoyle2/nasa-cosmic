	subroutine rtoc(real, string, size)

	real*4 real			     ! Real to convert
	character*(*) string		    ! Return string
	integer*4 size			  ! Return size

	real*4 prec_check		       ! Local used for precision
	real*4 trunc_value
	real*4 residual,multiplier

	parameter FFIELD = 9		    ! Field width
	parameter FMAG = 999999.9	       ! ABS of Max fixed size
	parameter FFRACT = 1		    ! Fixed fraction

	parameter EFIELD = 13		   ! Field width
	parameter EFRACT = 6		    ! Exponential fraction
	parameter ELEAD = 1		     ! Exponential leading digits

CC
CC Compute residual based on the fixed format fraction size
CC
	residual = 0.0

	if( abs( real ) .le. FMAG ) then

		multiplier = 10.0 ** float( FFRACT )

		prec_check = real * multiplier
		trunc_value = float( int( real * multiplier ) )

		residual = abs( prec_check - trunc_value )

	endif

CC
CC If magnitude and fraction significance allow, use fixed format,
CC otherwise use exponential.
CC
	if( ( abs( real ) .le. FMAG ) .and. ( residual .lt. 1.0e-6 ) .and.
	1	( abs(real) .gt. 1.0e-6)) then

		string = ' '
		write(string, 10) real
		size = EFIELD

	else

		string = ' '
		write(string, 20) real
		size = EFIELD

	endif

CC
CC That should do it
CC
	return

10      format(f<FFIELD>.<FFRACT>)
20      format(<ELEAD>pe<EFIELD>.<EFRACT>)

	end

