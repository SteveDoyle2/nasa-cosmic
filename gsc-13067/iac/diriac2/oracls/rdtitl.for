      SUBROUTINE RDTITL
C 
C   PURPOSE:
C      Read a single card of Hollerith input which is loaded into the
C      array TITLE of COMMON block LINES of RDTITL and automatically
C      printed at the top of each page of output through the subroutine
C      LNCNT.  Subroutine RDTITL also initializes several COMMON blocks
C      used in ORACLS.  RDTITL must be in every executive program.
C 
C   Subroutines employed by RDTITL: LNCNT
C   Subroutines employing RDTITL: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/LINES/NLP,LIN,TITLE(10),TIL(2)
      COMMON/FORM/NEPR,FMT1(2),FMT2(2)
      COMMON/TOL/EPSAM,EPSBM,IACM
      COMMON/CONV/SUMCV,MAXSUM,RICTCV,SERCV
C     NLP = NO. LINES/PAGE VARIES WITH THE INSTALLATION
      DATA LIN,NLP/1,60/
      DATA NEPR,FMT1/7,8H(1P7D16.,8H7)      /
      DATA TIL/8H ORACLS ,8H PROGRAM/
      DATA FMT2/8H(3X,1P7D,8H16.7)   /
      DATA EPSAM/1.D-10/
      DATA EPSBM/1.D-10/
      DATA IACM/12/
      DATA SUMCV/1.D-8/
      DATA RICTCV/1.D-8/
      DATA SERCV/1.D-8/
      DATA MAXSUM/50/
      READ(5,100,END=90) TITLE
   91 CONTINUE
  100 FORMAT(10A8)
      CALL LNCNT(100)
      RETURN
   90 CONTINUE
      STOP 1
      END
