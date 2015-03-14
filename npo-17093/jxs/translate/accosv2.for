      SUBROUTINE CONVERT1
C*****THIS SUBROUTINE TRANSLATES ACCOSV COMMANDS AND VARIABLES TO CODEV
C
C** COMMON BLOCK BBC DECLARATIONS.
C THIS COMMON BLOCK WAS ADDED TO SNARE THE CORRECT F() VALUES FOR
C APERTURE DATA BEFORE THEY GET CHANGED. BBC 12/84
	REAL*4	TEMPBBC(225)
C** COMMON BLOCK ACCOSV DECLARATIONS
      CHARACTER*6 UNITS
      INTEGER*4 IORDER(66),IWT1,IWT2,IWT3,IWT4,IWT5
      REAL*4 WVW1,WVW2,WVW3,WVW4,WVW5,WT1,WT2,WT3,WT4,WT5,THETA(66)
      REAL*4 RN1
      REAL*8 N1(99),N2(99),N3(99),N4(99),N5(99),F2(225)
C** COMMON BLOCK BOTH DECLARATIONS
      CHARACTER*4 SOLVE(40)
      CHARACTER*3 CPARM(30)
      CHARACTER*13 GLASS(0:225)
      CHARACTER*64 TITLE
      INTEGER*4 I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,M,ISTOP,IREF,ISURF
      INTEGER*4 NAPERT(225),NDECE(100),NSOLVE(40),NUMA,MESSAGE(20)
      REAL*4 DIF(66)
      REAL*8 ADE(100),BDE(100),CDE(100),XDE(100),YDE(100),CURVEY(0:225)
      REAL*8 THICKNS(0:225),RK(66),A(66),B(66),C(66),D(66),SOLVF1(40)
      REAL*8 F1(225),REF_OBJ_HT
      LOGICAL AFO,IC(66),CUF(66)
C** COMMON BLOCK CODEV DECLARATIONS
      CHARACTER*4 DIM,APERT(225)
      INTEGER*4 ICCY(0:225),ITHC(0:225),IGLC(0:225),IWTW1,IWTW2,IWTW3
      INTEGER*4 IWTW4,IWTW5,IWTW6,IWTW7,ICCX(30),KC(66),IAC(66),IBC(66)
      INTEGER*4 ICC(66),IDC(66),IXDC(100),IYDC(100),IADC(100),IBDC(100)
      INTEGER*4 ICDC(100),ICCF(66)
      REAL*4 ORDER(66),WLF1,WLF2,WLF3,WLF4,WLF5,WLF6,WLF7
      REAL*4 F(225),AL(66),AM(66),AN(66)
      REAL*8 RFND1(99),RFND2(99),RFND3(99),RFND4(99),RFND5(99)
C** COMMON BLOCK ACONLY DECLARATIONS
      CHARACTER*4 TORIC(10),COMMAND(100),PIKUP(200),SHAPE(225),CASE(10)
      CHARACTER*4 SPECSF(225)
      CHARACTER*36 SUBMES1(20)
      CHARACTER*40 SUBMES2(20)
      CHARACTER*48 SUBMES3(20)
      INTEGER*4 INDEX(20),NTORIC(10),NCASE(10),NUMBER(10),IPIKTO(200)
      INTEGER*4 IPIKFR(200),NASP(66),J10,K1,NGRT(10),NSHAPE(225)
      REAL*4 PIKA(200),DDIST(10),RNUMBER(10),RIC1(225),RIC2(225)
      REAL*4 RCUF1(225),RCUF2(225)
      REAL*8 TCVR(10),PY,PUCY,RADIUS(0:225),PIKB(200),NAO,NAO2,YAN
      LOGICAL OB(225)
C** LOCAL DECLARATIONS
      INTEGER*4 DUMMY(100),ITEMP,IWT(5)
      LOGICAL DEC
      REAL*4 WTEMP,W(5)
      REAL*8 RTEMP,R(99,5)
C
      COMMON /ACCOSV/IORDER,WVW1,WVW2,WVW3,WVW4,WVW5,WT1,WT2,WT3,WT4,
     *        WT5,IWT1,IWT2,IWT3,IWT4,IWT5,THETA,F2,RN1,UNITS,N1,
     *        N2,N3,N4,N5
      COMMON /BOTH/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,GLASS,MESSAGE,M,
     *        ISTOP,IREF,ISURF,AFO,NAPERT,NDECE,F1,DIF,ADE,BDE,CDE,XDE,
     *        YDE,CURVEY,THICKNS,RK,A,B,C,D,IC,CUF,NSOLVE,SOLVF1,SOLVE,
     *        TITLE,NUMA,REF_OBJ_HT
      COMMON /CODEV/DIM,APERT,ICCY,ITHC,IGLC,IWTW1,IWTW2,IWTW3,IWTW4,
     *        IWTW5,IWTW6,ILWTW7,ICCX,KC,IAC,IBC,ICC,IDC,ORDER,IXDC,
     *        IYDC,IADC,IBDC,ICDC,ICCF,WLF1,WLF2,WLF3,WLF4,WLF5,WLF6,
     *        WLF7,RFND1,RFND2,RFND3,RFND4,RFND5,F,AL,AM,AN
      COMMON /ACONLY/TCVR,PY,PUCY,INDEX,NTORIC,DDIST,SUBMES1,SUBMES2,
     *        SUBMES3,TORIC,NCASE,COMMAND,NUMBER,RADIUS,PIKA,PIKB,
     *        IPIKTO,IPIKFR,PIKUP,SHAPE,CASE,NASP,OB,SPECSF,NAO,YAN,
     *        J10,K1,NGRT,RIC1,RIC2,RCUF1,RCUF2,RNUMBER,NSHAPE
	COMMON /BBC/ TEMPBBC
C******************************************************************************
C COMPUTE THE VALUE OF YAN BEFORE THICKNS(0) VALUE IS CHANGED. BBC 10/84.
      YAN = ATAN(-1.*REF_OBJ_HT/THICKNS(0))*180./3.1415927
C*****INITIALIZE
      DO 25,I=0,225
        SPECSF(I)='    '
 25   CONTINUE
      DO 50,I=1,225
        RIC1(I)=0.0
        RIC2(I)=0.0
        RCUF1(I)=0.0
        RCUF2(I)=0.0
50    CONTINUE
      J10=0
      NUMA=0
      K1=0
      K2=0
      K3=0
      K4=0
      K5=0
      K6=0
      K7=0
      K8=0
      K9=0
      K10=0
      K11=0
      K12=0
      K13=0
      K14=0
      DO 75,I=1,100
        IADC(I)=100
        IBDC(I)=100
        ICDC(I)=100
        IXDC(I)=100
        IYDC(I)=100
 75   CONTINUE
      DO 85,I=1,66
        KC(I)=100
        IAC(I)=100
        IBC(I)=100
        ICC(I)=100
        IDC(I)=100
        IC(I)=.FALSE.
        CUF(I)=.FALSE.
 85   CONTINUE
      DO 95,I=1,30
        ICCX(I)=100
 95   CONTINUE
      DO 98,I=0,225
        ICCY(I)=100
        ITHC(I)=100
        IGLC(I)=100
98    CONTINUE
C
C******************************************************************************
C*****SURFACE DATA
      DO 100,I=0,ISURF
        IF (RADIUS(I) .EQ. 0.0) THEN
          CURVEY(I)=0.0
        ELSE
          CURVEY(I)=1./RADIUS(I)	! ADDED DECIMAL POINT FOR REAL RESULT.
        END IF
100   CONTINUE
      IF (THICKNS(0) .GT. 0.1E20) THICKNS(0)=0.1E20
C*****GLASS
      DO 125,I=0,ISURF
C ADJUST GLASS(1:6) POSITION HOYA,SCHOTT,OHARA. BBC 9/20/84.
        IF ((GLASS(I)(1:4) .EQ. 'HOYA') .OR.
     *      (GLASS(I)(1:6) .EQ. 'SCHOTT') .OR.
     *      (GLASS(I)(1:5) .EQ. 'OHARA')) THEN
C FIND THE RIGHT PLACES TO CUT THE GLASS VARIABLE. BBC 9/84.
          IQX = 13
101       IF (GLASS(I)(IQX:IQX) .NE. ' ') THEN
            IQX2 = IQX
            GOTO 102
          END IF
          IQX = IQX - 1
          IF (IQX .EQ. 8) THEN
            IQX2 = IQX
            GOTO 102
          END IF
          GOTO 101
102       IQX = 6
103       IF (GLASS(I)(IQX:IQX) .NE. ' ') THEN
            IQX3 = IQX
            GOTO 104
          END IF
          IQX = IQX - 1
          IF (IQX .EQ. 1) THEN
            IQX3 = IQX
            GOTO 104
          END IF
          GOTO 103
104       GLASS(I)=GLASS(I)(8:IQX2)//' '//GLASS(I)(1:IQX3)//' '
        ELSE IF (GLASS(I)(1:5) .EQ. 'MATRL') THEN
          GLASS(I)=GLASS(I)(8:13)
        ELSE IF (GLASS(I)(8:13) .NE. '      ') THEN
          GLASS(I)=GLASS(I)(8:13)
        END IF
125   CONTINUE
C******************************************************************************
C*****DIMENSIONS
      IF ((UNITS .EQ. 'INCHES') .OR. (UNITS .EQ. 'INCH  ')) THEN
        DIM='I    '
      ELSE IF (UNITS .EQ. 'CM    ') THEN
        DIM='C    '
      ELSE IF (UNITS .EQ. 'MM    ') THEN
        DIM='M   '
      END IF
C******************************************************************************
C*****APERTURES
C MOVED UP FROM BELOW.  BBC 9/84.
      K=0
      DO 225,I=1,I1
        K=K+1
        IF (SHAPE(I) .EQ. ' CIR') THEN
          APERT(K)='CIR '
          NAPERT(K)=NSHAPE(I)
          IF (OB(I)) THEN
            F(K)=-F2(I)
          ELSE
            F(K)=F2(I)
          END IF
        ELSE IF (SHAPE(I) .EQ. 'RECT') THEN
          APERT(K)='REX '
          NAPERT(K)=NSHAPE(I)
          K=K+1
          APERT(K)='REY '
          NAPERT(K)=NSHAPE(I)
          IF (OB(I)) THEN
            F(K-1)=-F1(I)
            F(K)=-F2(I)
          ELSE
            F(K-1)=F1(I)
            F(K)=F2(I)
          END IF
        ELSE IF (SHAPE(I) .EQ. ' ELL') THEN
          APERT(K)='ELX '
          NAPERT(K)=NSHAPE(I)
          K=K+1
          APERT(K)='ELY '
          NAPERT(K)=NSHAPE(I)
          IF (OB(I)) THEN
            F(K-1)=-F1(I)
            F(K)=-F2(I)
          ELSE
            F(K-1)=F1(I)
            F(K)=F2(I)
          END IF
        END IF
225   CONTINUE
      NUMA=K
C GRAB THE F() ARRAY VALUES AND PUT THEM INTO THE /BBC/ COMMON BLOCK SO THEY
C CAN BE PRINTED OUT IN THE NEXT SUBROUTINE. BBC 12/84.
      DO 226 I=1,225	
        TEMPBBC(I) = F(I)
226   CONTINUE
C******************************************************************************
C*****PICKUPS
      DO 158,I=1,I10
        IF (PIKUP(I) .EQ. 'ALPH') THEN
          CALL CONTROL(IADC,K1,IPIKFR(I),IPIKTO(I),I2,NDECE,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'BETA') THEN
          CALL CONTROL(IBDC,K1,IPIKFR(I),IPIKTO(I),I2,NDECE,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'GAMM') THEN
          CALL CONTROL(ICDC,K1,IPIKFR(I),IPIKTO(I),I2,NDECE,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'GLAS') THEN
          CALL CONTROL(IGLC,K1,IPIKFR(I),IPIKTO(I),0,DUMMY,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'TH  ') THEN
          CALL CONTROL(ITHC,K1,IPIKFR(I),IPIKTO(I),0,DUMMY,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'XD  ') THEN
          CALL CONTROL(IXDC,K1,IPIKFR(I),IPIKTO(I),I2,NDECE,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'YD  ') THEN
          CALL CONTROL(IYDC,K1,IPIKFR(I),IPIKTO(I),I2,NDECE,PIKA(I))
        ELSE IF ((PIKUP(I) .EQ. 'CV  ') .OR.
     *           (PIKUP(I) .EQ. 'RD  ')) THEN
          CALL CONTROL(ICCY,K1,IPIKFR(I),IPIKTO(I),0,DUMMY,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'CC  ') THEN
          CALL CONTROL(KC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        ELSE IF ((PIKUP(I) .EQ. 'CVR ') .OR.
     *           (PIKUP(I) .EQ. 'RDR ')) THEN
          CALL CONTROL(ICCX,K1,IPIKFR(I),IPIKTO(I),I8,NTORIC,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'AD  ') THEN
          CALL CONTROL(IAC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'AE  ') THEN
          CALL CONTROL(IBC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'AF  ') THEN
          CALL CONTROL(ICC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        ELSE IF (PIKUP(I) .EQ. 'AG  ') THEN
          CALL CONTROL(IDC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        ELSE IF ((PIKUP(I) .EQ. 'PRO ') .OR.
     *           (PIKUP(I) .EQ. 'NPRO')) THEN
          IF (PIKUP(I) .EQ. 'NPRO') THEN
            PIKA(I)=-1.0
          ELSE
            PIKA(I)=1.0
          END IF
          CALL CONTROL(ICCY,K1,IPIKFR(I),IPIKTO(I),0,DUMMY,PIKA(I))
          CALL CONTROL(KC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
          CALL CONTROL(ICCX,K1,IPIKFR(I),IPIKTO(I),I8,NTORIC,PIKA(I))
          CALL CONTROL(IAC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
          CALL CONTROL(IBC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
          CALL CONTROL(ICC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
          CALL CONTROL(IDC,K1,IPIKFR(I),IPIKTO(I),I4,NASP,PIKA(I))
        END IF
158   CONTINUE
C******************************************************************************
C*****TILT AND DECENTER DATA; CONVERT AN RTILT TO SEVERAL SURFACES
C*****REVERSING DECENTER AND TILTS
C
C  REPLACED 'DO 160,I=1,I2' HERE. THE UPPER BOUND OF THE LOOP MUST INCREASE
C  BY THE NUMBER OF SURFACES ADDED EVERY LOOP. OTHERWISE CRITICAL SURFACES
C  TEND TO BE PUSH OFF THE END OF THE ARRAY AND ARE NEVER HANDLED. BBC 10/84.
      IQX = I2
      I = 1
160   IF (COMMAND(I) .EQ. 'RTIL') THEN
        IF ((XDE(I) .NE. 0.0) .OR. (YDE(I) .NE. 0.0)) THEN
          DEC=.TRUE.
          IF ((ADE(I) .NE. 0.0) .AND. (BDE(I) .NE. 0.0) .AND.
     *      (CDE(I) .NE. 0.0)) THEN  !NONE OF THE ANGLES ARE ZERO
C THE ADD SUBROUTINE USES VARIABLE I7 SO WE HAD TO ADD IT TO THE PARAM LIST.
            CALL ADD(3,DEC,NDECE(I),ISURF,CURVEY,THICKNS,GLASS,ICCY,
     *        ITHC,IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,
     *        NUMA,I2,I3,I4,I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,
     *        INDEX,IXDC,IYDC,IADC,IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,
     *        IPIKFR,IPIKTO,PIKUP)
C ADD NUMBER OF NEW SURFACES TO UPPER BOUND FOR LOOP. BBC 10/84.
            IQX = IQX + 3
          ELSE IF (((ADE(I) .NE. 0.0) .AND. (BDE(I) .NE. 0.0)) .OR.
     *        ((BDE(I) .NE. 0.0) .AND. (CDE(I) .NE. 0.0)) .OR.
     *        ((ADE(I) .NE. 0.0) .AND. (CDE(I) .NE. 0.0))) THEN  !ONLY ONE ANGLE
            CALL ADD(2,DEC,NDECE(I),ISURF,CURVEY,THICKNS,GLASS,ICCY, !IS ZERO
     *        ITHC,IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,
     *        NUMA,I2,I3,I4,I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,
     *        INDEX,IXDC,IYDC,IADC,IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,
     *        IPIKFR,IPIKTO,PIKUP)
            IQX = IQX + 2
          ELSE IF ((ADE(I) .NE. 0.0) .OR. (BDE(I) .NE. 0.0) .OR.
     *        (CDE(I) .NE. 0.0)) THEN  ! 2 ANGLES ARE ZERO
            CALL ADD(1,DEC,NDECE(I),ISURF,CURVEY,THICKNS,GLASS,ICCY, !IS ZERO
     *        ITHC,IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,
     *        NUMA,I2,I3,I4,I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,
     *        INDEX,IXDC,IYDC,IADC,IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,
     *        IPIKFR,IPIKTO,PIKUP)
            IQX = IQX + 1
          ELSE ! ALL ANGLES ARE ZERO
            COMMAND(I)='DEC '
            YDE(I)=-YDE(I)
            XDE(I)=-XDE(I)
          END IF
        ELSE  ! NO DECENTER
          DEC=.FALSE.
          IF ((ADE(I) .NE. 0.0) .AND. (BDE(I) .NE. 0.0) .AND.
     *        (CDE(I) .NE. 0.0)) THEN  !NONE OF THE ANGLES ARE ZERO
            CALL ADD(2,DEC,NDECE(I),ISURF,CURVEY,THICKNS,GLASS,ICCY,
     *        ITHC,IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,
     *        NUMA,I2,I3,I4,I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,
     *        INDEX,IXDC,IYDC,IADC,IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,
     *        IPIKFR,IPIKTO,PIKUP)
            IQX = IQX + 2
          ELSE IF (((ADE(I) .NE. 0.0) .AND. (BDE(I) .NE. 0.0)) .OR.
     *        ((BDE(I) .NE. 0.0) .AND. (CDE(I) .NE. 0.0)) .OR.
     *        ((ADE(I) .NE. 0.0) .AND. (CDE(I) .NE. 0.0))) THEN !ONLY ONE ANGLE
            CALL ADD(1,DEC,NDECE(I),ISURF,CURVEY,THICKNS,GLASS,ICCY, !IS ZERO
     *        ITHC,IGLC,ISTOP,NDECE,NAPERT,NTORIC,NCASE,NASP,NSOLVE,
     *        NUMA,I2,I3,I4,I6,I8,I10,I,COMMAND,XDE,YDE,ADE,BDE,CDE,
     *        INDEX,IXDC,IYDC,IADC,IBDC,ICDC,KC,IAC,IBC,ICC,IDC,ICCX,I7,
     *        IPIKFR,IPIKTO,PIKUP)
            IQX = IQX + 1
          ELSE ! ONLY ONE ANGLE IS NON-ZERO
            COMMAND(I)='RTIL'
            IF (CURVEY(NDECE(I)).EQ.0.0.AND.THICKNS(NDECE(I)).EQ.0.0
     *        .AND.GLASS(NDECE(I)).EQ.'    ') GOTO 2277
            ADE(I)=-ADE(I)
            BDE(I)=-BDE(I)
            CDE(I)=-CDE(I)
 2277       CONTINUE
          END IF
        END IF
      END IF
      I = I + 1
      IF (I .LE. IQX) GOTO 160
C******************************************************************************
C*****WAVELENGTHS AND WEIGHTS AND REFRACTIVE INDICES; CONVERT WAVELENGTHS TO
C*****DESCENDING ORDER AND CHANGE WEIGHTS AND REFRACTIVE INDICES TO CORREPOND
      W(1)=WVW1
      W(2)=WVW2
      W(3)=WVW3
      W(4)=WVW4
      W(5)=WVW5
      IWT(1)=INT(WT1)
      IWT(2)=INT(WT2)
      IWT(3)=INT(WT3)
      IWT(4)=INT(WT4)
      IWT(5)=INT(WT5)
      DO 161,I=1,I7
        R(I,1)=N1(I)
        R(I,2)=N2(I)
        R(I,3)=N3(I)
        R(I,4)=N4(I)
        R(I,5)=N5(I)
161   CONTINUE
      DO 165,I=1,5
        DO 164,J=5,2,-1		! UPPER BOUND SHOULD STAY AT FIVE. BBC 9/84.
          IF (W(J) .EQ. W(J-1)) THEN !DELETE DUPLICATE WAVELENGTHS
            W(J)=0.0
            DO 162,K=1,I7
              R(K,J)=0.0
162         CONTINUE
          ELSE IF (W(J) .GT. W(J-1)) THEN
            WTEMP=W(J-1)
            W(J-1)=W(J)
            W(J)=WTEMP
            ITEMP=IWT(J-1)
            IWT(J-1)=IWT(J)
            IWT(J)=ITEMP
            DO 163,K=1,I7
              RTEMP=R(K,J-1)
              R(K,J-1)=R(K,J)
              R(K,J)=RTEMP
163         CONTINUE
          END IF
164     CONTINUE
165   CONTINUE
      WLF1=W(1)*1000.0
      WLF2=W(2)*1000.0
      WLF3=W(3)*1000.0
      WLF4=W(4)*1000.0
      WLF5=W(5)*1000.0
      IWTW1=IWT(1)
      IWTW2=IWT(2)
      IWTW3=IWT(3)
      IWTW4=IWT(4)
      IWTW5=IWT(5)
      DO 166,I=1,I7
        RFND1(I)=R(I,1)
        RFND2(I)=R(I,2)
        RFND3(I)=R(I,3)
        RFND4(I)=R(I,4)
        RFND5(I)=R(I,5)
166   CONTINUE
C******************************************************************************
C*****SET REFERENCE WAVELENGTH
      IF (WLF5 .NE. 0.0) THEN
        IREF=3
      ELSE IF ((WLF4 .NE. 0.0) .OR. (WLF3 .NE. 0.0)) THEN
        IREF=2
      ELSE
        IREF=1
      END IF
C*******************************************************************************
C*****GRATINGS
      DO 170,I=1,I3
        IF (CASE(I) .EQ. 'GRAT') THEN
          J10=J10+1
          NGRT(J10)=NCASE(I)
          RNUMBER(J10)=FLOAT(NUMBER(I))
          IF (DIM .EQ. 'M   ') THEN !CONVERT FROM MICROMETERS TO LENS
            DDIST(J10)=DDIST(I)*1.E-3 !UNITS: I, CM, MM
          ELSE IF (DIM .EQ. 'I   ') THEN
            DDIST(J10)=DDIST(I)*1.E-3/25.4
          ELSE IF (DIM .EQ. 'C   ') THEN
            DDIST(J10)=DDIST(I)*1.E-2	! CONVERT TO REAL NUMBERS. BBC 9/84.
          END IF
        END IF
170   CONTINUE
C******************************************************************************
C*****SPECIAL SURFACE TYPES
      DO 175,I=1,I4
        SPECSF(NASP(I))='ASP '
175   CONTINUE
      DO 180,I=1,I8  ! IF ASPHERIC DATA EXISTS THE SURFACE IS TORIC.
C LOGIC ERROR. THE SURFACE SHOULD ALWAYS BE TORIC, NEVER CYLINDRICAL. BBC 11/84
        SPECSF(NTORIC(I))='YTO '
180   CONTINUE
      DO 185,I=1,J10
        SPECSF(NGRT(I))='GRT '
185   CONTINUE
C******************************************************************************
C*****SURFACE DATA; ADD ASPHERIC DATA FOR GRATINGS IF NONE EXIST
      K=1
      L=1
188   IF (NGRT(L) .LT. NASP(K)) THEN
190     DO 195,J=I4,K,-1
          NASP(J+1)=NASP(J)
          RK(J+1)=RK(J)
          A(J+1)=A(J)
          B(J+1)=B(J)
          C(J+1)=C(J)
          D(J+1)=D(J)
195     CONTINUE
        I4=I4+1
        NASP(K)=NGRT(L)
        RK(K)=0.0
        A(K)=0.0
        B(K)=0.0
        C(K)=0.0
        D(K)=0.0
        KC(K)=100
        IAC(K)=100
        IBC(K)=100
        ICC(K)=100
        IDC(K)=100
        L=L+1
        IF (L .GT. J10) GOTO 199
      ELSE IF (NGRT(L) .EQ. NASP(K)) THEN
196     L=L+1
        K=K+1
        IF (L .GT. J10) GOTO 199
        IF (K .GT. I4) THEN
          I4=I4+1
          NASP(K)=NGRT(L)
          RK(K)=0.0
          A(K)=0.0
          B(K)=0.0
          C(K)=0.0
          D(K)=0.0
          L=L+1
          IF (L .GT. J10) THEN
            GOTO 199
          ELSE
            GOTO 196
          END IF
        END IF
      ELSE IF (NGRT(L) .GT. NASP(K)) THEN
197     K=K+1
        IF (K .GT. I4) THEN
          I4=I4+1
          NASP(K)=NGRT(L)
          RK(K)=0.0
          A(K)=0.0
          B(K)=0.0
          C(K)=0.0
          D(K)=0.0
          L=L+1
          IF (L .GT. J10) THEN
            GOTO 199
          ELSE
            GOTO 197
          END IF
        END IF
      END IF
      GOTO 188
C*****SURFACE DATA; IC AND CUF
199   DO 200,I=1,I3
        IF (CASE(I) .EQ. 'ASI ') THEN
          RIC1(I)=-1.0
          SPECSF(NCASE(I))='ASP '
          IC(I)=.TRUE.
        ELSE IF (CASE(I) .EQ. 'FRNL') THEN
          RCUF1(I)=CURVEY(NCASE(I))
          IF (RCUF1(I) .EQ. 0.) RCUF1(I) = 1.E-07
          SPECSF(NCASE(I))='ASP '
          CUF(I)=.TRUE.
        END IF
200   CONTINUE
C*****CREATE ASPHERIC SURFACE BY ADDING RIC1 TO ASPHERIC LIST WITH ALL OTHER
C*****DATA ZERO
      K=1
      L=1
205   IF (IC(L)) THEN
        IF (NCASE(L) .LT. NASP(K)) THEN
208       DO 210,J=I4,K,-1               ! ADD NEW ASHPERIC SURFACE
            NASP(J+1)=NASP(J)
            RK(J+1)=RK(J)
            A(J+1)=A(J)
            B(J+1)=B(J)
            C(J+1)=C(J)
            D(J+1)=D(J)
210       CONTINUE
          I4=I4+1
          NASP(K)=NCASE(L)
          RIC2(K)=RIC1(L)
          RK(K)=0.0
          A(K)=0.0
          B(K)=0.0
          C(K)=0.0
          D(K)=0.0
          KC(K)=100
          IAC(K)=100
          IBC(K)=100
          ICC(K)=100
          IDC(K)=100
          L=L+1
          IF (L .GT. I3) GOTO 213
        ELSE IF (NCASE(L) .EQ. NASP(K)) THEN  ! ADD RIC1 TO EXISTING ASHPERIC
          RIC2(K)=RIC1(L)                     ! SURFACE
211       L=L+1
          K=K+1
          IF (L .GT. I3) GOTO 213
          IF (K .GT. I4) THEN
            RIC2(K)=RIC1(L)
            RK(K)=0.0
            A(K)=0.0
            B(K)=0.0
            C(K)=0.0
            D(K)=0.0
            GOTO 211
          END IF
        ELSE IF (NCASE(L) .GT. NASP(K)) THEN
212       K=K+1
          IF (K .GT. I4) THEN
C ACCOUNT FOR NEW ASPHERIC SURFACE. BBC 9/84.
	    NASP(K)=NCASE(L)
	    I4=I4+1
            RIC2(K)=RIC1(L)
            RK(K)=0.0
            A(K)=0.0
            B(K)=0.0
            C(K)=0.0
            D(K)=0.0
            L=L+1
            IF (L .GT. I3) GOTO 213
            GOTO 212
          END IF
        END IF
        GOTO 205
      END IF
      L=L+1
      IF (L .LE. I3) GOTO 205
C*****CREATE ASPHERIC SURFACE BY ADDING RCUF1 TO ASPHERIC LIST WITH ALL OTHER
C*****DATA ZERO
213   K=1
      L=1
215   IF (CUF(L)) THEN
        IF (NCASE(L) .LT. NASP(K)) THEN
218       DO 220,J=I4,K,-1                ! ADD NEW ASPHERIC SURFACE
            NASP(J+1)=NASP(J)
            RK(J+1)=RK(J)
            A(J+1)=A(J)
            B(J+1)=B(J)
            C(J+1)=C(J)
            D(J+1)=D(J)
220       CONTINUE
          I4=I4+1
          NASP(K)=NCASE(L)
          RCUF2(K)=RCUF1(L)
          RK(K)=0.0
          A(K)=0.0
          B(K)=0.0
          C(K)=0.0
          D(K)=0.0
          KC(K)=100
          IAC(K)=100
          IBC(K)=100
          ICC(K)=100
          IDC(K)=100
          L=L+1
          IF (L .GT. I3) GOTO 223
        ELSE IF (NCASE(L) .EQ. NASP(K)) THEN  ! ADD RCUF1 TO EXISTING ASPHERIC
          RCUF2(K)=RCUF1(L)                   ! SURFACE
221       L=L+1
          K=K+1
          IF (L .GT. I3) GOTO 223
          IF (K .GT. I4) THEN
            RCUF2(K)=RCUF1(L)
            RK(K)=0.0
            A(K)=0.0
            B(K)=0.0
            C(K)=0.0
            D(K)=0.0
            GOTO 221
          END IF
        ELSE IF (NCASE(L) .GT. NASP(K)) THEN
222       K=K+1
          IF (K .GT. I4) THEN
C ACCOUNT FOR NEW ASPHERIC SURFACE. BBC 9/84.
	    I4=I4+1
	    NASP(K)=NCASE(L)
            RCUF2(K)=RCUF1(L)
            RK(K)=0.0
            A(K)=0.0
            B(K)=0.0
            C(K)=0.0
            D(K)=0.0
            L=L+1
            IF (L .GT. I3) GOTO 223
            GOTO 222
          END IF
        END IF
        GOTO 215
      END IF
      L=L+1
      IF (L .LE. I3) GOTO 215
223   CONTINUE
C******************************************************************************
C*****SOLVES
      I = 1
C DELETE LAST SOLVE IF IT APPEARS AT NEXT TO LAST SURFACE. BBC 9/25/84.
      IF (NSOLVE(I6) .EQ. ISURF-1) I6=I6-1
230   IF (I .LE. I6) THEN
        IF (SOLVE(I) .EQ. 'PY  ') THEN
          SOLVE(I)='1   '
        ELSE IF (SOLVE(I) .EQ. 'PCY ') THEN
          SOLVE(I)='2   '
        ELSE IF (SOLVE(I) .EQ. 'PX  ') THEN
          SOLVE(I)='3   '
        ELSE IF (SOLVE(I) .EQ. 'PCX ') THEN
          SOLVE(I)='4   '
        ELSE IF (SOLVE(I) .EQ. 'PUY ') THEN
          SOLVE(I)='5   '
        ELSE IF (SOLVE(I) .EQ. 'PUCY') THEN
          SOLVE(I)='6   '
        ELSE IF (SOLVE(I) .EQ. 'APY ') THEN
          SOLVE(I)='7   '
        ELSE IF (SOLVE(I) .EQ. 'PIY ') THEN
          SOLVE(I)='8   '
        ELSE IF (SOLVE(I) .EQ. 'APCY') THEN
          SOLVE(I)='9   '
        ELSE IF (SOLVE(I) .EQ. 'PICY') THEN
          SOLVE(I)='10  '
        ELSE IF (SOLVE(I) .EQ. 'PUX ') THEN
          SOLVE(I)='11  '
        ELSE IF (SOLVE(I) .EQ. 'PUCX') THEN
          SOLVE(I)='12  '
        ELSE IF (SOLVE(I) .EQ. 'APX ') THEN
          SOLVE(I)='13  '
        ELSE IF (SOLVE(I) .EQ. 'PIX ') THEN
          SOLVE(I)='14  '
        ELSE IF (SOLVE(I) .EQ. 'APCX') THEN
          SOLVE(I)='15  '
        ELSE IF (SOLVE(I) .EQ. 'PICX') THEN
          SOLVE(I)='16  '
        ELSE IF (SOLVE(I) .EQ. 'COCY') THEN
          ICCY(NSOLVE(I))=INT(SOLVF1(I))+100
        ELSE IF (SOLVE(I) .EQ. 'COCX') THEN
          DO 270,IDO=1,I8       !********************  THIS IS A PROBLEM!!****
            IF (NSOLVE(IDO) .EQ. NTORIC(K)) ICCX(K)=INT(SOLVF1(IDO))+100
270       CONTINUE
	  I = IDO
        END IF
	I = I + 1
      ELSE
        GOTO 280
      END IF
      GOTO 230
C******************************************************************************
C*****APERTURE AND FIELD SPECIFICATION
280   IF (RN1 .EQ. 0.0) RN1=1.0  !SURFACE IS AIR
      NAO=RN1*SIN(ATAN(PY/THICKNS(0)))
C CHANGE COMMANDS SO COMMAND CODES ARE INTERPRETED CORRECTLY. BBC 10/84.
      DO 285,J=1,I2
        IF (COMMAND(J) .EQ. 'DEC ') COMMAND(J) = 'RTIL'
285   CONTINUE
C NEGATE CERTAIN CONTROL CODES DEPENDING TILT/RTILT AND PIKA VALUE. BBC 10/84.
      CALL CCNEGATE(I2,I10,IPIKTO,NDECE,IPIKFR,COMMAND,PIKA,
     *  IXDC,IYDC,IADC,IBDC,ICDC,ICCY,ITHC,PIKUP,ISURF)
C MUST HAVE REAL NUMBERS IN A DIVISION OPERATION. BBC 9/84.
C RE-DEFINE YAN VALUE. BBC 10/84.
C MOVE YAN VALUE COMPUTATION TO ACCOSV1 TO USE ORIGINAL THICKNS(0) VALUE. BBC.
C*****************************************************************************
C*****ALTERNATE CONFIGURATIONS
C      DO 300, I=1,I11
C        IF (CPARM(I) .EQ. 'SAY') THEN
C          DO 301, IDO=1,5
C            ZOOMTBL(IDO,I)=NAO
C301       CONTINUE
C          ZOOMTBL(CFG(I),I) = REF_AP_HT/ACVAL(I)*NAO
C        ELSE IF (CPARM(I) .EQ. 'CV ') THEN
C        ELSE IF (CPARM(I) .EQ. 'CVR') THEN
C        ELSE IF (CPARM(I) .EQ. 'CC ') THEN
C        ELSE IF (CPARM(I) .EQ. 'XD ') THEN
C	  CALL ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,XDE,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'YD ') THEN
C          CALL ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,YDE,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'ALP') THEN
C          CALL ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,ADE,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'BET') THEN
C          CALL ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,BDE,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'GAM') THEN
C          CALL ZOOM(I2,NDECE,NCONF,ZOOMTBL,I,CDE,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'AD ') THEN
C          CALL ZOOM2(I4,NASP,NCONF,ZOOMTBL,I,A,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'AE ') THEN
C          CALL ZOOM2(I4,NASP,NCONF,ZOOMTBL,I,B,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'AF ') THEN
C          CALL ZOOM2(I4,NASP,NCONF,ZOOMTBL,I,C,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'AG ') THEN
C          CALL ZOOM2(I4,NASP,NCONF,ZOOMTBL,I,D,ACVAL(I),CFG(I))
C        ELSE IF (CPARM(I) .EQ. 'TH ') THEN
C          DO 311,IDO=0,ISURF
C            IF (NCONF(I) .EQ. IDO) ZOOMTBL(1,I) = THICKNS(IDO)
C311       CONTINUE
C          DO 312,IDO=2,5
C            ZOOMTBL(IDO,I) = ZOOMTBL(1,I)
C312       CONTINUE
C          ZOOMTBL(CFG(I),I) = ACVAL(I)
C        ELSE IF (CPARM(I) .EQ. 'CAS') THEN
      RETURN
      END
