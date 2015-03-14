      SUBROUTINE LNCNT  (N)
C 
C   PURPOSE:
C      Keeps track of the number of lines printed and automatically pag-
C      inates the output.  Page length is controlled by the variable
C      NLP set in subroutine RDTITL.  LNCNT prints the contents of
C      TITLE and TIL at the top of each page.
C 
C   Subroutines employed by LNCNT: None
C   Subroutines employing LNCNT: ADD, ASYFIL, ASYREG, BARSTW, BILIN,
C      CNTREG, CSTAB, CTROL, DISREG, DSTAB, EQUATE, EXMDFL, EXPINT,
C      EXPSER, FACTOR, IMMDFL, JUXTC, JUXTR, MULT, PREFIL, PRNT, RDTITL,
C      READ1, RICNWT, SAMPL, SCALE, SUBT, SUM, TESTST, TRANP, TRCE,
C      TRNSIT, UNITY, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/LINES/NLP,LIN,TITLE(10),TIL(2)
      LIN=LIN+N
      IF  (LIN.LE.NLP)   GO TO 20
      WRITE(6,1010) TITLE,TIL
 1010 FORMAT(1H1,10A8,2A8/)
      LIN=2+N
      IF  (N.GT.NLP)  LIN=2
   20 RETURN
      END
