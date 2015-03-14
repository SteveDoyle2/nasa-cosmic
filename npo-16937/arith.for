************************************************************************
*                          ARITH.FOR                                   *
*                                                                      *
*                                                                      *
*  PROGRAMMER:                                                         *
*     RALPH F. MILES, JR.                                              *
*     SYSTEMS DIVISION                                                 *
*     JET PROPULSION LABORATORY                                        *
*     PASADENA, CA 91109                                               *
*                                                                      *
*  VERSION: 1.00X1                                                     *
*  DATE:    06/13/85                                                   *
*                                                                      *
*----------------------------------------------------------------------*
*  THE PROGRAM "ARITH.FOR" TESTS THE FOUR FORTRAN DOUBLE-PRECISION     *
*  ARITHMETICAL OPERATIONS (+,-,*,/) AND THE FORTRAN DOUBLE-PRECISION  *
*  FUNCTIONS MODULO (DMOD) AND TRUNCATION (DINT).                      *
*                                                                      *
*----------------------------------------------------------------------*
*                      CONFIGURATION CHANGES                           *
*                                                                      *
*   VER.    DATE                   CHANGES                             *
*                                                                      *
*  1.0X01  06/13/85  * ORIGINAL.                                       *
*                                                                      *
************************************************************************

 
$TITLE:'RANDOM.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:4
 
************************************************************************

      PROGRAM ARITH

************************************************************************


***** INITIALIZE PROGRAM.  {MODULE 1}

      CHARACTER*1      MENU
      DOUBLE PRECISION ARITH1,ARITH2,ARITH3

***   {END MODULE 1}


***** MENU DISPLAY.  {MODULE 2}

100   CONTINUE

      WRITE  (*,110)
110   FORMAT (1X,/////////////////////////
     *        24X,'****** THE ARITH MENU ****** '/
     *        /
     *        24X,'+    :    Addition            '/
     *        24X,'-    :    Subtraction         '/
     *        24X,'*    :    Multiplication      '/
     *        24X,'/    :    Division            '/
     *        24X,'M    :    Modulo     (DMOD)   '/
     *        24X,'T    :    Truncation (DINT)   '/
     *        24X,'Q    :    Quit                '/
     *        /
     *        24X,'----------------------------- '/
     *        //)

      WRITE (*,'(20X,A\)') 'Enter a Menu Character & <RETURN>:  '
      READ  (*,'(BN,A1)') MENU

***   {END MODULE 2}


***** QUIT THE PROGRAM.  {MODULE 3}

      IF (MENU .EQ. 'Q') THEN

         WRITE  (*,120)
120      FORMAT (1X,/////////////////////////)

         GOTO 999

      ENDIF

***   {END MODULE 3}


***** PERFORM ADDITION.  {MODULE 4}

      IF (MENU .EQ. '+') THEN

         WRITE  (*,130)
130      FORMAT (1X,/////////////////////////
     *           30X,'****** ADDITION ******  ')

         WRITE  (*,140)
140      FORMAT (/////1X,5X,'Enter the first number for addition:  '\)
         READ   (*,150) ARITH1
150      FORMAT (BN,F20.0)

         WRITE  (*,160)
160      FORMAT (   //1X,5X,'Enter the second number for addition: '\)
         READ   (*,170) ARITH2
170      FORMAT (BN,F20.0)

         ARITH3 = ARITH1 + ARITH2

         WRITE  (*,180) ARITH3
180      FORMAT (   //1X,5X,'The addition of the two numbers is:   ',
     *           F20.0)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 4}


***** PERFORM SUBTRACTION.  {MODULE 5}

      IF (MENU .EQ. '-') THEN

         WRITE  (*,190)
190      FORMAT (1X,/////////////////////////
     *           30X,'****** SUBTRACTION ****** ')

         WRITE  (*,200)
200      FORMAT (/////1X,5X,
     *           'Enter the first number for subtraction:  '\)
         READ   (*,210) ARITH1
210      FORMAT (BN,F20.0)

         WRITE  (*,220)
220      FORMAT (//1X,5X,'Enter the second number for subtraction: '\)
         READ   (*,230) ARITH2
230      FORMAT (BN,F20.0)

         ARITH3 = ARITH1 - ARITH2

         WRITE  (*,240) ARITH3
240      FORMAT (//1X,5X,'The subtraction of the two numbers is:   ',
     *           F20.0)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 5}


***** PERFORM MULTIPLICATION.  {MODULE 6}

      IF (MENU .EQ. '*') THEN

         WRITE  (*,250)
250      FORMAT (1X,/////////////////////////
     *           30X,'****** MULTIPLICATION ****** ')

         WRITE  (*,260)
260      FORMAT (/////1X,5X,
     *           'Enter the first number for multiplication:  '\)
         READ   (*,270) ARITH1
270      FORMAT (BN,F20.0)

         WRITE  (*,280)
280      FORMAT (   //1X,5X,
     *           'Enter the second number for multiplication: '\)
         READ   (*,290) ARITH2
290      FORMAT (BN,F20.0)

         ARITH3 = ARITH1 * ARITH2

         WRITE  (*,300) ARITH3
300      FORMAT (   //1X,5X,
     *           'The multiplication of the two numbers is: ',F20.0)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 6}


***** PERFORM DIVISION.  {MODULE 7}

      IF (MENU .EQ. '/') THEN

         WRITE  (*,310)
310      FORMAT (1X,/////////////////////////
     *           30X,'****** DIVISION ****** ')

         WRITE  (*,320)
320      FORMAT (/////1X,5X,'Enter the first number for division:  '\)
         READ   (*,330) ARITH1
330      FORMAT (BN,F20.0)

         WRITE  (*,340)
340      FORMAT (   //1X,5X,'Enter the second number for division: '\)
         READ   (*,350) ARITH2
350      FORMAT (BN,F20.0)

         ARITH3 = ARITH1 / ARITH2

         WRITE  (*,360) ARITH3
360      FORMAT (   //1X,5X,'The division of the two numbers is: ',
     *           F30.10)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 7}


***** PERFORM MODULO FUNCTION.  {MODULE 8}

      IF (MENU .EQ. 'M') THEN

         WRITE  (*,370)
370      FORMAT (1X,/////////////////////////
     *           30X,'****** MODULO ****** ')

         WRITE  (*,380)
380      FORMAT (/////1X,5X,
     *           'Enter the argument of the modulo function: '\)
         READ   (*,390) ARITH1
390      FORMAT (BN,F20.0)

         WRITE  (*,400)
400      FORMAT (   //1X,5X,
     *           'Enter the modulus of the modulo function:  '\)
         READ   (*,410) ARITH2
410      FORMAT (BN,F20.0)

         ARITH3 = DMOD(ARITH1,ARITH2)

         WRITE  (*,420) ARITH3
420      FORMAT (   //1X,5X,'The remainder is: ',F20.0)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 8}


***** PERFORM TRUNCATION.  {MODULE 9}

      IF (MENU .EQ. 'T') THEN

         WRITE  (*,430)
430      FORMAT (1X,/////////////////////////
     *           30X,'****** TRUNCATION ****** ')

         WRITE  (*,440)
440      FORMAT (/////1X,5X,
     *           'Enter the decimal number for truncation: '\)
         READ   (*,450) ARITH1
450      FORMAT (BN,F30.10)

         ARITH3 = DINT(ARITH1)

         WRITE  (*,460) ARITH3
460      FORMAT (   //1X,5X,
     *           'The truncation of the decimal number is: ',F30.10)

         WRITE (*,'(//1X,A\)') 'Enter <RETURN> to continue: '
         READ  (*,'(BN,A1)') MENU

      ENDIF      

***   {END MODULE 9}


***** GO TO MENU.  {MODULE 10}

      GOTO 100

***   {END MODULE 10}


***** STOP PROGRAM.  {MODULE 11}

999   CONTINUE

      STOP

      END

***   {END MODULE 11}

****************************** ARITH.FOR *******************************
