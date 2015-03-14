      SUBROUTINE ADD(J,DEC,L,ISURF,CURVEY,THICKNS,GLASS,ICCY,ITHC,
     *  IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,I1,I2,I3,I4,
     *  I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,INDEX,IXDC,IYDC,IADC,
     *  IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,IPIKFR,IPIKTO,PIKUP) ! EXTRA
								  ! VARIABLES
								  ! PASSED. BBC
C*****THIS SUBROUTINE ADDS J NUMBER OF BLANK SURFACES AND MOVES ALL THE
C*****ORIGINAL DATA FORWARD
      CHARACTER*4 COMMAND(100),PIKUP(200)
      CHARACTER*13 GLASS(0:225)
C	I1 = NUMA (NUM APERTURES) ADJUSTED FOR RECTANGLES AND ELLIPSES.
      INTEGER*4 I1,I2,I3,I4,I6,I8,I10,J,L,ISURF,ICCY(0:225),ITHC(0:225)
      INTEGER*4 IGLC(0:225),ISTOP,NDECE(100),NAPERT(225),NTORIC(10)
      INTEGER*4 NCASE(10),NASP(66),NSOLVE(40),IXDC(100),IYDC(100)
      INTEGER*4 IADC(100),IBDC(100),ICDC(100),INDEX(20),KC(66),IAC(66)
      INTEGER*4 IBC(66),ICC(66),IDC(66),ICCX(30),IPIKFR(200),IPIKTO(200)
      REAL*8 CURVEY(0:225),THICKNS(0:225),XDE(100),YDE(100),ADE(100)
      REAL*8 BDE(100),CDE(100)
      LOGICAL DEC,NDFLAG
C
      DO 100,K=ISURF,L,-1
        CURVEY(K+J)=CURVEY(K)
        THICKNS(K+J)=THICKNS(K)
        GLASS(K+J)=GLASS(K)
        ICCY(K+J)=ICCY(K)
        ITHC(K+J)=ITHC(K)
        IGLC(K+J)=IGLC(K)
100   CONTINUE
      ISURF=ISURF+J
      DO 125,K=0,J-1
        CURVEY(L+K)=0.0
        THICKNS(L+K)=0.0
        ICCY(L+K)=100
        ITHC(L+K)=100
        IGLC(L+K)=100
        GLASS(L+K)='             '
125   CONTINUE
      DO 130,K=1,I7
        IF (INDEX(K) .GE. L) INDEX(K)=INDEX(K)+J
130   CONTINUE
      IF (ISTOP .GE. L) ISTOP=ISTOP+J
      DO 150,K=1,I1
        IF (NAPERT(K) .GE. L) NAPERT(K)=NAPERT(K)+J
150   CONTINUE
      DO 175,K=1,I8
        IF (NTORIC(K) .GE. L) NTORIC(K)=NTORIC(K)+J
175   CONTINUE
      DO 200,K=1,I3
        IF (NCASE(K) .GE. L) NCASE(K)=NCASE(K)+J
200   CONTINUE
      DO 225,K=1,I4
        IF (NASP(K) .GE. L) NASP(K)=NASP(K)+J
225   CONTINUE
C
C NEW CODE STARTS HERE...		BBC 10/84
C MUST ADJUST PICKUPS ACCORDING TO A COMPLICATED SET OF RULES IN ORDER TO
C ACCOUNT FOR ADDED SURFACES. LOOP THROUGH NUMBER OF PICKUPS. BBC 10/84.
      DO 230,K=1,I10
        NDFLAG = .FALSE.
C IF PICKUP IS PAST THE ADDED SURFACES, THEN ADD THE NUMBER OF ADDED SURFACES
C TO EACH PICKUP POINTER (IPIKFR). BBC 10/84.
        IF (IPIKFR(K) .GT. L) THEN
          IPIKFR(K)=IPIKFR(K)+J
C IF WE ARE AT THE POSITION OF THE ADDED SURFACES THEN ADJUST THE PICKUP
C POINTERS (IPIKFR) ACCORDING TO THE RULES. BBC 10/84.
        ELSE IF (IPIKFR(K) .EQ. L) THEN
          DO 227,IXX=1,I2
            IF (IPIKFR(K) .EQ. NDECE(IXX)) THEN
              IF (PIKUP(K).EQ.'XD  '.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J
                NDFLAG = .TRUE.
              END IF
              IF (PIKUP(K).EQ.'YD  '.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J
                NDFLAG = .TRUE.
              END IF
              IF (PIKUP(K).EQ.'ALPH'.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 1
                NDFLAG = .TRUE.
              END IF
              IF (PIKUP(K).EQ.'BETA'.AND.J.GT.2.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 2
                NDFLAG = .TRUE.
              ELSE IF (PIKUP(K).EQ.'BETA'.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 1
                NDFLAG = .TRUE.
              END IF
              IF (PIKUP(K).EQ.'GAMM'.AND.J.GT.2.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 3
                NDFLAG = .TRUE.
              ELSE IF (PIKUP(K).EQ.'GAMM'.AND.J.GT.1.AND.
     *          .NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 2
                NDFLAG = .TRUE.
              ELSE IF (PIKUP(K).EQ.'GAMM'.AND..NOT.NDFLAG) THEN
                IPIKFR(K) = IPIKFR(K) + J - 1
                NDFLAG = .TRUE.
              END IF
            END IF
227       CONTINUE
	END IF
C ADJUST THE PICKUP "TO" VALUES. BBC 10/84.
        IF (IPIKTO(K) .GE. L) IPIKTO(K)=IPIKTO(K)+J
230   CONTINUE
      DO 250,K=I4,1,-1	! DO FOR ALL ASPHERIC SURFACES. L IS A POINTER TO THE
        KC(K+J)=KC(K)	! SURFACE BEING ADDED, NOT A POINTER TO A POINTER.
        IAC(K+J)=IAC(K)	! BBC 10/84.
        IBC(K+J)=IBC(K)
        ICC(K+J)=ICC(K)
        IDC(K+J)=IDC(K)
250   CONTINUE
      DO 260,K=I8,1,-1	! DO FOR ALL TORICS. L IS A POINTER TO THE SURFACE 
C			! BEING ADDED, NOT A POINTER TO A POINTER.  BBC 11/84.
        IF (NTORIC(K) .GE. L) ICCX(K+J)=ICCX(K)
260   CONTINUE
      DO 275,K=1,I6
        IF (NSOLVE(K) .GE. L) NSOLVE(K)=NSOLVE(K)+J
275   CONTINUE
C REMOVED "DO 350,"
      KK = 1
285   IF (KK .LT. I2+J+1) THEN	! EXAMINE EACH DECENTERED SURFACE. BBC 10/84.
        IF (NDECE(KK) .EQ. L) THEN	! IF AN ADDED SURFACE IS DECENTERED...
          DO 300,K=I2,(KK+1),-1
            NDECE(K+J)=NDECE(K)
            XDE(K+J)=XDE(K)
            YDE(K+J)=YDE(K)
            ADE(K+J)=ADE(K)
            BDE(K+J)=BDE(K)
            CDE(K+J)=CDE(K)
            COMMAND(K+J)=COMMAND(K)
300       CONTINUE
          DO 325,K=1,J
            NDECE(KK+K)=L+K
325       CONTINUE
        KK=KK+J
        ELSE IF (NDECE(KK) .GT. L) THEN	! IF WE ARE PASSED THE ADDED SURFACE...
          NDECE(KK)=NDECE(KK)+J
        END IF
	KK = KK + 1
      ELSE		! NO MORE DECENTERED SURFACES. BBC 10/84.
        GOTO 330
      END IF
      GOTO 285
330   I2=I2+J
      IF (DEC) THEN
C BBC 9/18/84. MOVE THESE NUMBERS DOWN ONE SPACE.
        DO 350, IXX=ISURF,I+J,-1
          IXDC(IXX+1)=IXDC(IXX)
          IYDC(IXX+1)=IYDC(IXX)
          IADC(IXX+1)=IADC(IXX)
          IBDC(IXX+1)=IBDC(IXX)
          ICDC(IXX+1)=ICDC(IXX)
350     CONTINUE
        COMMAND(I+J)='DEC '
        XDE(I+J)=-XDE(I)
        YDE(I+J)=-YDE(I)
        ADE(I+J)=0.0
        BDE(I+J)=0.0
        CDE(I+J)=0.0
        IXDC(I+J)=IXDC(I)
        IYDC(I+J)=IYDC(I)
        IADC(I+J)=100
        IBDC(I+J)=100
        ICDC(I+J)=100
        DO 375,K=0,J-1
          COMMAND(I+K)='RTIL'	! HELPS PROCESS PICKUPS. BBC 10/84.
          XDE(I+K)=0.0
          YDE(I+K)=0.0
          IXDC(I+K)=100
          IYDC(I+K)=100
375     CONTINUE
        IF (J .EQ. 3) THEN
          ADE(I+2)=-ADE(I)
          BDE(I+2)=0.0
          CDE(I+2)=0.0
          ADE(I+1)=0.0
          BDE(I+1)=-BDE(I)
          CDE(I+1)=0.0
          ADE(I)=0.0
          BDE(I)=0.0
          CDE(I)=-CDE(I)
          IADC(I+2)=IADC(I)
          IBDC(I+2)=100
          ICDC(I+2)=100
          IADC(I+1)=100
          IBDC(I+1)=IBDC(I)
          ICDC(I+1)=100
          IADC(I)=100
          IBDC(I)=100
        ELSE IF (J .EQ. 2) THEN
          IF (ADE(I) .EQ. 0.0) THEN
            ADE(I+1)=0.0
            BDE(I+1)=-BDE(I)
            CDE(I+1)=0.0
            BDE(I)=0.0
            CDE(I)=-CDE(I)
            IADC(I+1)=100
            IBDC(I+1)=IBDC(I)
            ICDC(I+1)=100
            IADC(I)=100
            IBDC(I)=100
          ELSE
            ADE(I+1)=-ADE(I)
            BDE(I+1)=0.0
            CDE(I+1)=0.0
            ADE(I)=0.0
            BDE(I)=-BDE(I)  !EITHER BDE OR CDE IS ZERO
            CDE(I)=-CDE(I)
            IADC(I+1)=IADC(I)
            IBDC(I+1)=100
            ICDC(I+1)=100
            IADC(I)=100
          END IF
        ELSE  ! J EQ 1
          ADE(I)=-ADE(I)  ! ONLY ONE ANGLE IS NON-ZERO
          BDE(I)=-BDE(I)
          CDE(I)=-CDE(I)
        END IF
      ELSE
        DO 400,K=0,J
          COMMAND(I+K)='TILT'
          XDE(I+K)=0.0
          YDE(I+K)=0.0
          IXDC(I+K)=100
          IYDC(I+K)=100
400     CONTINUE
        IF (J .EQ. 2) THEN
          ADE(I+2)=-ADE(I)
          BDE(I+2)=0.0
          CDE(I+2)=0.0
          ADE(I+1)=0.0
          BDE(I+1)=-BDE(I)
          CDE(I+1)=0.0
          ADE(I)=0.0
          BDE(I)=0.0
          CDE(I)=-CDE(I)
          IADC(I+2)=IADC(I)
          IBDC(I+2)=100
          ICDC(I+2)=100
          IADC(I+1)=100
          IBDC(I+1)=IBDC(I)
          ICDC(I+1)=100
          IADC(I)=100
          IBDC(I)=100
        ELSE IF (J .EQ. 1) THEN
          IF (ADE(I) .EQ. 0.0) THEN
            ADE(I+1)=0.0
            BDE(I+1)=-BDE(I)
            CDE(I+1)=0.0
            BDE(I)=0.0
            CDE(I)=-CDE(I)
            IADC(I+1)=100
            IBDC(I+1)=IBDC(I)
            ICDC(I+1)=100
            IBDC(I)=100
          ELSE
            ADE(I+1)=-ADE(I)
            BDE(I+1)=0.0
            CDE(I+1)=0.0
            ADE(I)=0.0
            BDE(I)=-BDE(I)  !EITHER BDE OR CDE IS ZERO
            CDE(I)=-CDE(I)
            IADC(I+1)=IADC(I)
            IBDC(I+1)=100
            ICDC(I+1)=100
            IADC(I)=100
          END IF
        END IF
      END IF
      RETURN
      END
C
C
      SUBROUTINE CONTROL(CODE,K,FROM,TO,I,N,A)
C*****THIS SUBROUTINE ASSIGNS VALUES TO CONTROL CODES ACCORDING TO THE THE
C*****PICKUPS.  EQUAL VALUES OF K ARE GIVEN TO VALUES PICKING UP AND BEING
C***** PICKED UP.
      INTEGER*4 CODE(0:225),K,FROM,TO,I,N(100),A,SAVEF,SAVETO
C
      SAVEF=FROM
      SAVETO=TO
      DO 100,J=1,I
        IF (N(J) .EQ. FROM) THEN
          FROM=J
        ELSE IF (N(J) .EQ. TO) THEN
          TO=J
        END IF
100   CONTINUE
      IF (I .NE. 0) THEN
	FROM = FROM -1
	TO = TO -1
      END IF
      IF (CODE(TO) .NE. 100) THEN
        CODE(FROM)=CODE(TO)
      ELSE
        K=K+1
        CODE(TO)=K
        CODE(FROM)=K
      END IF
      IF (A .NE. ABS(A)) CODE(FROM)=-CODE(FROM)
      FROM=SAVEF  ! RETURNS ORIGINAL VALUE
      TO=SAVETO
      RETURN
      END
C
      SUBROUTINE CCNEGATE(I2,I10,IPIKTO,NDECE,IPIKFR,COMMAND,PIKA,
     *  IXDC,IYDC,IADC,IBDC,ICDC,ICCY,ITHC,PIKUP,ISURF)
C THIS SUBROUTINE WAS ADDED TO CHANGE CONTROL CODES BASED ON CERTAIN CONDITIONS
C       SUBROUTINE BY BRIAN B. CARLIN
C       TECHNICAL SYSTEMS OPERATIONS
C       SANTA BARBARA RESEARCH CENTER
C       OCTOBER 9, 1984
C
      CHARACTER*4 COMMAND(100),PIKUP(200)
      INTEGER*4 IXDC(100),IYDC(100),IADC(100),IBDC(100),ICDC(100)
      INTEGER*4 I2,I10,I,J,K,NDECE(100),IPIKTO(200),IPIKFR(200)
      INTEGER*4 ICCY(0:225),ITHC(0:225),ISURF
      REAL*4 PIKA(200)
C SEARCH THROUGH DECENTERED SURFACES.
      DO 30,I=1,I2
C MATCH ALL PICKUPS TO DECENTERED SURFACE DATA.
        DO 20,J=1,I10
C MATCH TO PICKUP?
          IF (IPIKTO(J).EQ.NDECE(I)) THEN
C IF SO, THEN FIND THE FROM PICKUP
            DO 10,K=I+1,I2
C MATCH FROM PICKUP?
              IF (IPIKFR(J).EQ.NDECE(K)) THEN
C SHOULD ANY OF THE DECENTER CONTROL CODES BE MODIFIED?
                IF (((COMMAND(K).EQ.COMMAND(I).OR.COMMAND(K).EQ.'DEC '
     *            .OR.COMMAND(I).EQ.'DEC ').AND.PIKA(J).EQ.-1.) .OR.
     *            ((COMMAND(K).NE.COMMAND(I).AND.COMMAND(K).NE.'DEC '
     *            .AND.COMMAND(I).NE.'DEC ').AND.PIKA(J).EQ.1.)) THEN
                  IF (IXDC(K).NE.100 .AND. PIKUP(J)(1:2).EQ.'XD')
     *              IXDC(K) = IXDC(K) * (-1)
                  IF (IYDC(K).NE.100 .AND. PIKUP(J)(1:2).EQ.'YD')
     *              IYDC(K) = IYDC(K) * (-1)
                  IF (IADC(K).NE.100 .AND. PIKUP(J)(1:4).EQ.'ALPH')
     *              IADC(K) = IADC(K) * (-1)
                  IF (IBDC(K).NE.100 .AND. PIKUP(J)(1:4).EQ.'BETA')
     *              IBDC(K) = IBDC(K) * (-1)
                  IF (ICDC(K).NE.100 .AND. PIKUP(J)(1:4).EQ.'GAMM')
     *              ICDC(K) = ICDC(K) * (-1)
                END IF
              END IF
  10        CONTINUE
          END IF
  20    CONTINUE
  30  CONTINUE
      DO 50,J=1,ISURF
        DO 40,K=1,I10
          IF(IPIKFR(K).EQ.J.AND.ICCY(J).NE.100.AND.PIKA(K).EQ.-1..AND.
     *      ICCY(J).GT.0) THEN
            ICCY(J)=ICCY(J) * (-1)
          END IF
          IF(IPIKFR(K).EQ.J.AND.ITHC(J).NE.100.AND.PIKA(K).EQ.-1..AND.
     *      ITHC(J).GT.0) THEN
            ITHC(J)=ITHC(J) * (-1)
          END IF
  40    CONTINUE
  50  CONTINUE
      RETURN
      END

C      SUBROUTINE ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,RVAL,ACVAL,CFG)
C      INTEGER I,I2,NDECE(100),NCONF(30),J,CFG
C      REAL*8 RVAL(100),ACVAL,ZOOMTBL(5,30)
C      DO 10, J=1,I2
C        IF (NDECE(J) .EQ. NCONF(I)) ZOOMTBL(1,I) = RVAL(J)
C10    CONTINUE
C      DO 20, J=2,5
C        ZOOMTBL(J,I) = ZOOMTBL(1,I)
C20    CONTINUE
C      ZOOMTBL(CFG,I) = ACVAL
C      RETURN
C      END

C      SUBROUTINE ZOOM2(I4,NASP,NCONF,ZOOMTBL,I,RVAL,ACVAL,CFG)
C      INTEGER I,I4,NASP(66),NCONF(30),J,CFG
C      REAL*8 RVAL(66),ACVAL,ZOOMTBL(5,30)
C      DO 10, J=1,I4
C        IF (NASP(J) .EQ. NCONF(I)) ZOOMTBL(1,I) = RVAL(J)
C10    CONTINUE
C      DO 20, J=2,5
C        ZOOMTBL(J,I) = ZOOMTBL(1,I)
C20    CONTINUE
C      ZOOMTBL(CFG,I) = ACVAL
C      RETURN
C      END
