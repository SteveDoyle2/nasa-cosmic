            *****************************************
             FLEXIBLE SPACECRAFT DYNAMICS PROGRAM V
            *****************************************


    This is the Flexible Spacecraft Dynamics Program-V, modified
  to run on the VAX 11/780 VMS 3.7 . FSD was written by AVCO and
  modified to run on the VAX by ODSI. 
      
    To install FSD, create a directory "[FSD]" and define FSDHOME as 
  the FSD directory. The tape is a backup.
    BACKUP MF:FSD.BCK/SELECT = [...] [...]/REWIND
  will restore the tape.

    For questions concerning how to use the FSD program see "A 
  user's guide to the FLEXIBLE SPACECRAFT DYNAMICS PROGRAM-V".
    
    The executable image is called FSD.EXE . The image will look for 
  the data in FOR004.DAT . An example of a data set can be found 
  under EXAMPLE.DAT . An example of fsd output can be found under 
  EXAMPLE.OUT .

    The command procedures needed to run FSD batch through a card
  reader can be found under AACARDS.TXT and RUNFSD.COM .

    The command procedure to compile and link up the subroutines
  is called COMLINK.COM . COMLINK.COM is in [FSD.SOURCE] .

    NOTE : If modifications are made to any of the FORTRAN subroutines
  they should be recompiled using the /NOF77 option.(Except for RANDO,
  SUBLIB and REMTIM) There is a PASCAL routine called LIBRARY.PAS . If
  your system does not have a PASCAL compiler the object code is supplied.
  EDT the "PAS/NOWARNING LIBRARY" line out of COMLINK.COM .
  

                 Changes from IBM to VAX
                *************************

    Listed below are all the changes that have been made to run FSD
  on the VAX.

  ARTLU1
 ********
   
    Remove all the "/"s from around the varibles in
  the subroutine statement. These are optional on the
  IBM and do not work at all on the VAX.

  GPRINT
 ********

    On the line following FORTRAN statement No. 500,
  BLANK = 10D-40 becomes BLANK = 10D-38. 
  This is due to a difference in the way floating point
  numbers are treated.
    A few lines before fortran statemant 3000, 
        IF(...)GOTO 3000 was changed to
        IF(...)GOTO 420.
    At the beginning of the program, the number of lines printed
  before going to a new page was changed to 60.(Old number : 80)

  RANDU
 *******

    Got scrambled somewhere.
    "#" should be "=",
    "&" should be "+",
    "%" should be "(", and
    "<" should be ")".    
    The spelling of RANDU should be changed to RANDO in
  the RANDO subroutine header and in GAUSS, the only place
  RANDO is called from. VAX FORTRAN supplies a function called
  RANDU and this causes problems because the program uses the 
  system function instead of it's own function.

    NOTE: RANDO  must be compiled under FORTRAN77.

  RO1TAP
 ********

    ENTRY DRO1TP, ENTRY TAPRE, and ENTRY DTAPRE were all 
  moved to the beginning of the subroutine and GOTO's
  and CONTINUES were used to insure the same results.
  This was done because the entry varibles were used by
  the subroutine ahead of the entry statements and the
  VAX FORTRAN compiler was unhappy about that. Also, to
  get around the same problem, the varible TSIN was
  renamed ZSIN in the NAMELIST statement and then
  ZSIN=TSIN was put in front of each WRITE statemant
  that used the NAMELIST declaration.

  BLOCK2
 ********

    In BLOCK2, the VAX FORTRAN compiler was unhappy about data
  statements only partly filling an array. Some of the arrays
  are so large that it would impossible to fill them in one data
  statement. On the IBM this problem was solved by using EQUIVALENCE
  statements. This will work on the VAX by filling the main
  arrays with 0's and then using the equivalent arrays to fill
  in over the zeros. In BLOCK2, 0's were added to DATA blocks :
 
  DATA block       Amount added at end of DATA block

    DATA Z31   -   54*0.D0 
    DATA Z32   -   54*0.D0
    DATA Z33   -   54*0.D0
    DATA Z34   -   54*0.D0
    DATA Z35   -   54*0.D0
    DATA Z41   -   260*0.D0
    DATA Z42   -   260*0.D0
    DATA Z43   -   260*0.D0
    DATA ZK41  -   98*0.D0
    DATA ZK43  -   98*0.D0
    DATA ZK46  -   98*0.D0
    DATA ZK48  -   98*0.D0
    DATA ZS41  -   98*0.D0

  BLOCK5
 ********

    BLOCK5 had the same problems as BLOCK2. There was also a
  problem with some of the arrays used to fill in the main 
  arrays. Again, the VAX FORTRAN compiler was unhappy because
  arrays were not being filled in completely. Because the
  part of the arrays not being filled in was never used,
  the array(s) sizes were redeclared to the sizes needed.
  At the head of program, in the REAL*4 lines: VI11, VI12,
  VI214, VI224, VI31D, and VA should all be redeclared to
  40 instead of the original 80. As with BLOCK2, some of
  the DATA blocks need 0,s added.

  DATA block      Amount added at end of data block

    DATA DI11  -  40*0.D0
    DATA DI12  -  40*0.D0
    DATA DI21  -  280*0.D0
    DATA DI22  -  280*0.D0
    DATA DI31  -  1000*0.D0
    DATA DA    -  40*0.D0

  BLOCK6
 ********

    BLOCK6 had the same problems as BLOCK5. 
  In the REAL*4 lines: VK214, VK31D, VK224, VK32D all
  need to be set to 40. Also, some DATA blocks need 0's.

  DATA block      Amount to be added at end

    DATA DK21  -  280*0.D0
    DATA DK22  -  280*0.D0
    DATA DK31  -  1000*0.D0
    DATA DK32  -  1000*0.D0

  PRJACC
 ********

    Move the DEFINE statement to the beginning of MAIN.

  MAIN
 ******
    At the beginning of MAIN add
   
      CALL ERRSET(73,.TRUE.,.TRUE.,.FALSE.,.TRUE.,15)

    ERRSET(73 ... ) allows the program to continue after a
  divide by zero.
  
  PLOT
 ******

    DATA PERIOD/./ was changed to
    DATA PERIOD/*/

    This is the character that is used in the plots. On the 
  VERSATEC printer '.'s are very hard to see.
  Also, for some unknown reason, in FORMAT statements '1' will
  not new page but 1H1 will so three FORMAT statemants needed
  to be changed.

  VDMPRD
 ********

    Put all dimension statements at the beginning of the 
  subroutine.

  MISC
 ******

    Also, some routines were written to do the job of an
  IBM assmbly language routine and fill in for some IBM
  system routines that the VAX does not have. These routines
  are:

    LIBRARY  -  a PASCAL routine,
    NARG     -  a MACRO routine, and
    SUBLIB and REMTIM  -  both fortran routines.

    MISC
   ******      

      The version number is set in BVERS .

      The echo print is written out of FVAL .

