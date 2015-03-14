      SUBROUTINE TRQORD
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/ITRQOT/ IOUTPT(150)
C
      COMMON/TRQOUT/ OUTTRQ(150)
C
C
      REAL*4 BUFF(450)
C
C
      DATA I8/',A8,'/
C
C
      CALL SETUP(8HIOUTPT  ,4,IOUTPT,150)
C
      DO 5 I=1,150
      OUTTRQ(I)=0.0D0
    5 CONTINUE
C
C
      RETURN
C
C    ***************************************************************
      ENTRY TRQPLT(BUFF,INDX)
C    ***************************************************************
C
      INDEX=INDX-1
      INDX=INDX+3
C
      IF(IOUTPT(1).EQ.0) RETURN
C
      IND=IOUTPT(2)
      IND2=IOUTPT(3)
C
C
      DO 10 I=1,3
      I3=I+3
      BUFF(INDEX+I)=OUTTRQ(IND-1+I)
      BUFF(INDEX+I3)=OUTTRQ(IND2-1+I)
   10 CONTINUE
C
      RETURN
C
C    ***************************************************************
      ENTRY TRQPRN
C    ***************************************************************
C
      IF(IOUTPT(1).EQ.0) RETURN
C
      I1=IOUTPT(2)
      I2=I1+1
      I3=I2+1
C
      CALL SET('TORQUE 1',0,0,OUTTRQ(I1),I8)
      CALL SET('TORQUE 2',0,0,OUTTRQ(I2),I8)
      CALL SET('TORQUE 3',0,0,OUTTRQ(I3),I8)
C
      I1=IOUTPT(3)
      I2=I1+1
      I3=I2+1
C
      CALL SET('MT TRQ 1',0,0,OUTTRQ(I1),I8)
      CALL SET('MT TRQ 2',0,0,OUTTRQ(I2),I8)
      CALL SET('MT TRQ 3',0,0,OUTTRQ(I3),I8)
C
C
      RETURN
C
C
C
      END
