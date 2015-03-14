      SUBROUTINE UPINV(RIN,N, ROUT, W )                                 UPIN0010
C                                                                       UPIN0020
C           THIS SUBROUTINE  INVERTS  A  PACKED  UPPER  TRIANGULAR      UPIN0030
C           MATRIX  RIN, AND  STORES THE INVERSE  IN  ROUT              UPIN0040
C                                                                       UPIN0050
C           RIN AND ROUT  ARE BOTH SINGLE DIMENSIONED  ARRAYS           UPIN0060
C           IN WHICK THE MATRIX IS STORED  COLUMNWISE                   UPIN0070
C                                                                       UPIN0080
C           RIN  CAN  EQUAL ROUT                                        UPIN0090
C                                                                       UPIN0100
      DOUBLE PRECISION  RIN(1), ROUT(1), W(1), WORK, ONE, ZERO ,DIN     UPIN0110
      DATA  ONE/1.0D0/,ZERO/ 0.0D0/                                     UPIN0120
      IPV =   N*(N+1)/2                                                 UPIN0130
      IN  = IPV                                                         UPIN0140
      DO 6    I=1,N                                                     UPIN0150
      DIN   =        ONE/ RIN(IPV)                                      UPIN0160
      ROUT( IPV )  = DIN                                                UPIN0170
      MIN =N                                                            UPIN0180
      KEND = I-1                                                        UPIN0190
      LANF  =   N - KEND                                                UPIN0200
      IF(KEND) 5, 5, 2                                                  UPIN0210
    2 J= IN                                                             UPIN0220
C                                                                       UPIN0230
C             INITIALIZE  ROW LOOP                                      UPIN0240
C                                                                       UPIN0250
      DO 4   K=1,KEND                                                   UPIN0260
      WORK =ZERO                                                        UPIN0270
      MIN= MIN -  1                                                     UPIN0280
      LIN= IPV                                                          UPIN0290
      LOT= J                                                            UPIN0300
C                                                                       UPIN0310
C            START  INNER LOOP                                          UPIN0320
C                                                                       UPIN0330
      DO 3   L=LANF, MIN                                                UPIN0340
      LIN= LIN+L                                                        UPIN0350
      LOT= LOT+1                                                        UPIN0360
    3 WORK = WORK  + RIN(LIN)* ROUT(LOT)                                UPIN0370
      ROUT(J)  =   -WORK* DIN                                           UPIN0380
   4  J=  J- MIN                                                        UPIN0390
    5 IPV =  IPV -MIN                                                   UPIN0400
    6 IN= IN -1                                                         UPIN0410
      RETURN                                                            UPIN0420
      END                                                               UPIN0430
