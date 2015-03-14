      PROGRAM NASTPLOT
      IMPLICIT INTEGER (A-Z)
      REAL XFACT,YFACT
      CHARACTER FILNAM*35
      LOGICAL CHRSET,LINSET,AXSSET,LPLOT,ANYPLT,LSTOP,LIBM
      DIMENSION PLTREC(750)
      COMMON CMND,CNTRL,R,S,T,U,  XFACT,YFACT,
     *       XMAX,YMAX,  XHI,YHI,XLO,YLO,
     *       PENCHG,PENNO(100),PENCNT,OLDPEN
C
C     INITIALIZE COUNTERS, FLAGS, & PLOT-FILE/PLOTTER LOG. UNIT NOS.
          DATA RECCNT,CMDCNT,NULCNT,SKPCNT,PLTCNT /5*0/
          DATA CHRSET,LINSET,AXSSET,LSTOP /4*.FALSE./
          DATA PLTFIL, LDEV  /13, 14/
C
C     GET INPUT FILENAME & TYPE (VAX OR IBM)
10	  CALL NAMTYP(FILNAM,LIBM)
C     SELECT EXECUTION OPTION (ALL PLOTS, ONE PLOT, SUMMARY ONLY)
          CALL OPTION(LPLOT,SELECT,ANYPLT)
C     OPEN PLOT FILE AND SUMMARY FILE
          OPEN(UNIT=PLTFIL,
     *         NAME=FILNAM,ERR=300,
     *         TYPE='OLD',
!    *         ACCESS='SEQUENTIAL',
!    *         RECORDTYPE='FIXED',
!    *         RECORDSIZE=3000,
!    *         BLOCKSIZE=3000,
!    *         CARRIAGECONTROL='NONE',
!    *         FORM='FORMATTED',
!    *         ORGANIZATION='SEQUENTIAL',
     *         READONLY)
          OPEN(UNIT=20,NAME='NASTPLOT.SUM')
C     INITIALIZE PLOTTER
          IF (ANYPLT)  CALL PLOTS(0,0,LDEV)
C
      DO 200 I=1,1000000
C         GET NEXT NASTPLT RECORD  ( ON E-O-F, PRINT SUMMARY & EXIT )
              READ(PLTFIL,1000,END=250) PLTREC
C             CONVERT IBM RECORD
		  IF (LIBM) CALL CNVIBM(PLTREC)
              RECCNT = RECCNT + 1
          DO 100 J=1,100
C             GET NEXT COMMAND (TOTAL OF 100 COMMANDS / NASTPLT RECORD)
                  CALL GETCMD(PLTREC)
                  CMDCNT = CMDCNT + 1
C             CHECK FOR NEW CHARACTER, LINE, OR AXIS SET
                  IF ((CMND.GE.14) .AND. (CMND.LE.16))
     *                CALL NEWSET(CHRSET,LINSET,AXSSET)
C             CHECK FOR END OF CHARACTER, LINE, OR AXIS SET
                  IF (CHRSET .AND. CMND.NE.4) CALL ENDCHR(CHRSET)
                  IF (LINSET .AND. CMND.NE.5) CALL ENDLIN(LINSET)
                  IF (AXSSET .AND. CMND.NE.6) CALL ENDAXS(AXSSET)
C             EXECUTE COMMAND
                  IF (CMND .EQ. 0) NULCNT = NULCNT + 1
                  IF (CMND .EQ. 1) CALL BGNPLT(PLTCNT,
     *                                         LPLOT,SELECT,LDEV,LSTOP)
                      IF (LSTOP) GO TO 250
                  IF (CMND .EQ. 2) CALL SELCAM
                  IF (CMND .EQ. 3) CALL SKPFRM(SKPCNT)
                  IF (CMND .EQ. 4) CALL DRWCHR(CHRSET,LPLOT)
                  IF (CMND .EQ. 5) CALL DRWLIN(LINSET,LPLOT)
                  IF (CMND .EQ. 6) CALL DRWAXS(AXSSET,LPLOT)
                  IF ((CMND.LT.0) .OR. (CMND.GT.6))  WRITE(20,8000) CMND
100           CONTINUE
200       CONTINUE
C
C     NASTPLT END-OF-FILE; PRINT SUMMARY & EXIT
250       IF (.NOT. LSTOP) CALL SUMPRT(RECCNT,CMDCNT,NULCNT,SKPCNT,
     *                                 PLTCNT,LPLOT,SELECT,LDEV,LSTOP)
C         CLOSE PLOT FILE
              CLOSE(UNIT=PLTFIL)
C         CLOSE SUMMARY FILE
              WRITE(6,2000)
              READ(5,3000) REQSUM
              IF ((REQSUM .EQ. 'NO') .OR. (REQSUM .EQ. 'N'))  THEN
                  CLOSE (UNIT=20,DISP='DELETE')
                  WRITE(6,4000)
              ELSE
                  CLOSE (UNIT=20)
              END IF
C         CLOSE PLOTTER
              IF (ANYPLT)  CALL PLOT(0.0,0.0,999)
          STOP
C
C     PLOT FILE NOT FOUND; TRY AGAIN ?
300       TYPE *, 'ERROR OPENING FILE', FILNAM
	  TYPE 5000
	  ACCEPT 3000, REQFIL
	  IF (REQFIL .EQ. 'NO')  STOP  'NO OTHER FILE REQUESTED'
	  IF (REQFIL .EQ. 'N')  STOP  'NO OTHER FILE REQUESTED'
	  GOTO 10
C
C
1000  FORMAT(750A4)
2000  FORMAT(' RETAIN SUMMARY FILE ? (YES OR NO): ',$)
3000  FORMAT(A4)
4000  FORMAT(' SUMMARY FILE DELETED')
5000  FORMAT(' DO YOU WANT A DIFFERENT FILE ? (YES OR NO): ',$)
8000  FORMAT(' **** UNKNOWN COMMAND: P= ',I6,' ****')
      END

      SUBROUTINE BGNPLT(PLTCNT,LPLOT,SELECT,LDEV,LSTOP)
      IMPLICIT INTEGER (A-Z)
      REAL XFACT,YFACT,XSIZE,YSIZE,XDEF,YDEF
      LOGICAL LPLOT,ONEPLT,ANYPLT,LSTOP
      COMMON CMND,CNTRL,R,S,T,U,  XFACT,YFACT,
     *       XMAX,YMAX,  XHI,YHI,XLO,YLO,
     *       PENCHG,PENNO(100),PENCNT,OLDPEN
C
      DATA XDEF,YDEF /2*8.0/
      DATA ONEPLT/.FALSE./,  PLTTOT/0/
C     CHECK IF PREVIOUS PLOTNO IS ONLY ONE TO BE PLOTTED
          IF (ONEPLT .AND. LPLOT)  THEN
              CALL PLOT(0.0, 0.0, -3)
C             CHECK FOR ANY MORE PLOTTING
                  CALL OPTION(LPLOT,SELECT,ANYPLT)
                  IF (.NOT. ANYPLT)  THEN
C                     NO MORE PLOTS REQUESTED
                      ONEPLT = .FALSE.
C                     CONTINUE WITH SUMMARY ?
                          WRITE (6,2100)
                          READ (5,2200) SUMREQ
                          IF ((SUMREQ.EQ.'NO').OR.(SUMREQ.EQ.'N'))  THEN
                              LSTOP = .TRUE.
                              RETURN
                              END IF
                  ELSE IF (LPLOT)  THEN
C                     PLOT ALL REMAINING PLOTS
                      ONEPLT = .FALSE.
10                    WRITE(6,2300)
                      READ(5,2200) GO
                      IF (GO .NE. 'GO')  GOTO 10
                  ELSE
C                     ANOTHER PLOT WAS SELECTED
                  END IF
              END IF
C     IS ONLY ONE PLOTNO TO BE PLOTTED ?
          IF (.NOT.LPLOT)  ONEPLT = .TRUE.
          IF (ONEPLT)  LPLOT = .FALSE.
C
C     PRINT SUMMARY OF PREVIOUS PLOT (IF NEW PLOT IS NOT FIRST PLOT)
          IF (PLTCNT.GT.0) WRITE(20,4000) XMAX,YMAX,XHI,YHI,XLO,YLO,
     *                               PENCHG,PENCNT,(PENNO(N),N=1,PENCNT)
C     INITIALIZE FOR NEW PLOT
          PLTCNT = PLTCNT + 1
          PLOTNO = R
          XMAX = S
          YMAX = T
          IF (PLOTNO .EQ. SELECT)  LPLOT = .TRUE.
C         IF OPTION IS "ONE PLOT", GET X,Y AXES' LENGTHS;
C                               OTHERWISE USE DEFAULT AXIS LENGTHS
              IF (ONEPLT .AND. LPLOT) THEN
                  CALL GETSIZ(XSIZE,YSIZE)
		  XFACT = XSIZE/FLOAT(XMAX)
		  YFACT = YSIZE/FLOAT(YMAX)
              ELSE
                  XSIZE = XDEF
                  YSIZE = YDEF
		  IF (XMAX .GT. YMAX)  THEN
		      XFACT = XSIZE/FLOAT(XMAX)
		      YFACT = XFACT
		  ELSE
		      YFACT = YSIZE/FLOAT(YMAX)
		      XFACT = YFACT
		  ENDIF
              END IF
          XHI = 0
          YHI = 0
          XLO = XMAX
          YLO = YMAX
          PENCHG = 0
          PENCNT = 0
          OLDPEN = 0
          WRITE(6,2000) PLOTNO,XMAX,YMAX,XSIZE,YSIZE
          WRITE(20,2000) PLOTNO,XMAX,YMAX,XSIZE,YSIZE
C     SET NEW ORIGIN  2 INCHES TO THE LEFT OF PREVIOUS PLOT,
C     EXCEPT, EVERY 3RD PLOT MOVE ORIGIN BACK TO RIGHT SIDE AND
C     ADVANCE PAPER (IF OPTION = "ALL PLOTS")
	  IF (.NOT. ONEPLT)  PLTTOT = PLTTOT + 1
          IF ((PLTTOT.GT.1) .AND. (.NOT.ONEPLT)) THEN
              IF (MOD(PLTTOT-1,3) .NE. 0) THEN
                  CALL PLOT(0.0, YSIZE+2.0, -3)
              ELSE
                  CALL PLOT(XSIZE+2.0, -2.0*(YSIZE+2.0), -3)
              END IF
          END IF
      RETURN
C
2000  FORMAT(' START PLOT #',I6,' ; (XMAX,YMAX) = (',I5,','I5,')',/,
     *       ' ',T23,'PLOT SIZE IS ',F5.1,' X ',F5.1,'  INCHES')
2100  FORMAT(' CONTINUE WITH SUMMARY FILE (YES OR NO) ?  ',$)
2200  FORMAT(A4)
2300  FORMAT(' SET PLOTTER ORIGIN. TYPE "GO" WHEN READY:  ',$)
4000  FORMAT(5X,'PLOT SUMMARY:',T20,'(XMAX,YMAX) = (',I5,',',I5,')',/
     *                       1X,T20,'XHI = ',I5,/
     *                       1X,T20,'YHI = ',I5,/
     *                       1X,T20,'XLO = ',I5,/
     *                       1X,T20,'YLO = ',I5,/
     *                       1X,T20,'NO. OF PEN CHANGES = ',I5,/
     *                       1X,T20,'NO. OF DIFFERENT PENS = ',I5,/
     *                       1X,T20,'PEN ID''S =',9(1X,T30,12(1X,I3)/))
      END

      SUBROUTINE CNVIBM(RECORD)
      INTEGER RECORD(750)
      BYTE BYTE(4), BTEMP(4)
      EQUIVALENCE (IWORD,BYTE(1)) , (ITEMP,BTEMP(1))
C
C     CONVERT FROM IBM FORMAT TO VAX FORMAT:
C     REVERSE THE ORDER OF THE BYTES IN EACH WORD OF THE RECORD
	  DO 100 I=1,750
	      ITEMP = RECORD(I)
	      BYTE(1) = BTEMP(4)
              BYTE(2) = BTEMP(3)
              BYTE(3) = BTEMP(2)
	      BYTE(4) = BTEMP(1)
              RECORD(I) = IWORD
100           CONTINUE
      RETURN
      END

      SUBROUTINE DRWAXS(AXSSET,LPLOT)
      IMPLICIT INTEGER (A-Z)
      REAL XFACT,YFACT,X,Y
      LOGICAL AXSSET,P16,LPLOT
      COMMON CMND,CNTRL,R,S,T,U,  XFACT,YFACT,
     *       XMAX,YMAX,  XHI,YHI,XLO,YLO,
     *       PENCHG,PENNO(100),PENCNT,OLDPEN
      DATA AXSCNT/0/
      DATA P16 /.TRUE./
C
C     CHECK FOR INITIAL COMMAND=16
          IF (.NOT.AXSSET) THEN
              P16 = .FALSE.
              AXSSET = .TRUE.
              ENDIF
C     CHECK COORDINATES AGAINST MAX & MIN ENCOUNTERED SO FAR
          XHI = MAX( R, T, XHI)
          YHI = MAX( S, U, YHI)
          XLO = MIN( R, T, XLO)
          YLO = MIN( S, U, YLO)
C     CHECK FOR A PEN CHANGE AND/OR NEW PEN ID
          IF (CNTRL .NE. OLDPEN) THEN
              CALL NEWPEN(CNTRL)
              PENCHG = PENCHG + 1
              IF (PENCNT .LT. 100) THEN
                  PENCNT = PENCNT + 1
                  PENNO(PENCNT) = CNTRL
                  IF (PENCNT .GT. 1) THEN
                      DO 100 N=1,PENCNT-1
                          IF (PENNO(PENCNT) .EQ. PENNO(N)) THEN
                              PENCNT = PENCNT -1
                              GO TO 150
                              END IF
100                       CONTINUE
                      END IF
                  END IF
150           OLDPEN = CNTRL
              END IF
      AXSCNT = AXSCNT + 1
      IF (LPLOT) THEN
C         MOVE PLOTTER PEN TO BEGINNING OF AXIS
              X = XFACT * FLOAT(R)
              Y = YFACT * FLOAT(S)
              CALL PLOT(X,Y,3)
C         DRAW AXIS ON PLOTTING SURFACE
              X = XFACT * FLOAT(T)
              Y = YFACT * FLOAT(U)
              CALL PLOT(X,Y,2)
          END IF
      RETURN
C
C
      ENTRY ENDAXS(AXSSET)
C     CHECK FOR INITIAL COMMAND=16 & PRINT "DRAW AXES" SUMMARY
          IF (P16) WRITE(20,6000) AXSCNT
          IF (.NOT.P16) WRITE(20,6100)
C     REINITIALIZE
          P16 = .TRUE.
          AXSCNT = 0
          AXSSET = .FALSE.
      RETURN
C
6000  FORMAT(5X,'DRAW AXES: ',I6,' AXES WERE DRAWN')
6100  FORMAT(1X,T20,'**** NO INITIAL P=16 COMMAND ****')
      END

      SUBROUTINE DRWCHR(CHRSET,LPLOT)
      IMPLICIT INTEGER (A-Z)
      REAL XFACT,YFACT,X,Y,ANGLE,HITE
      LOGICAL CHRSET,LPLOT
      CHARACTER CHR94*52, CHRSAV*57
      DIMENSION CHRCOD(53)
      COMMON CMND,CNTRL,R,S,T,U,  XFACT,YFACT,  
     *       XMAX,YMAX,  XHI,YHI,XLO,YLO
      DATA LINE/1/, SAVCNT/0/, CHRCNT/0/, UNKCNT/0/
      DATA CHR94/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ()+-*/=.,$" O[<^'/
      DATA CHRSAV /' '/
      DATA CHRCOD /32,33,34,35,36,37,38,39,40,41,
     *             49,50,51,52,53,54,55,56,57,58,59,60,61,
     *             62,63,64,65,66,67,68,69,70,71,72,73,74,
     *             24,25,27,29,26,31,45,30,28,20,23,
     *             16, 1, 0, 5, 2,47/
C
C     CHECK FOR INITIAL COMMAND=14
          IF (.NOT.CHRSET) THEN
              WRITE(20,5000)
              CHRSET = .TRUE.
              LINE = LINE + 1
              END IF
C     UPDATE COUNTER & POINTER
          SAVCNT = SAVCNT + 1
          CHRCNT = CHRCNT + 1
C     STORE CHARACTER IN STRING CHRSAV
          IF (CNTRL.GT.52 .OR. CNTRL.LE.0) THEN
              CHRSAV(SAVCNT:SAVCNT) = '?'
              CODE = CHRCOD(53)
              UNKCNT = UNKCNT + 1
          ELSE 
              CHRSAV(SAVCNT:SAVCNT) = CHR94(CNTRL:CNTRL)
              CODE = CHRCOD(CNTRL)
          END IF
C     IF CHRSAV STRING IS FULL, PRINT IT
          IF (SAVCNT .EQ. 57) THEN
              IF (LINE .EQ. 1) WRITE(20,5100) CHRSAV
              IF (LINE .GT. 1) WRITE(20,5200) CHRSAV
              CHRSAV = ' '
              SAVCNT = 0
              LINE = LINE + 1
              END IF
C     COMPARE (X,Y) WITH MAX & MIN ENCOUNTERED SO FAR
          XHI = MAX( XHI, R)
          YHI = MAX( YHI, S)
          XLO = MIN( XLO, R)
          YLO = MIN( YLO, S)
C     DRAW CHARACTER ON PLOTTER
          IF (LPLOT) THEN
              X = XFACT * FLOAT(R)
              Y = YFACT * FLOAT(S)
              HITE = 0.08 * XFACT * 100.0
              ANGLE = 0.0
              CALL SYMBOL(X,Y,HITE,CODE,ANGLE,-1)
              END IF
      RETURN
C
C
      ENTRY ENDCHR(CHRSET)
C     PRINT OUT CONTENTS OF CHRSAV & COUNTERS
          IF (LINE .EQ. 1) WRITE(20,5100) CHRSAV
          IF (LINE .GT. 1) WRITE(20,5200) CHRSAV
          WRITE(20,5300) CHRCNT,UNKCNT
C     REINITIALIZE
          LINE = 1
          CHRSAV = ' '
          CHRSET = .FALSE.
          SAVCNT = 0
          CHRCNT = 0
          UNKCNT = 0
      RETURN
C
5000  FORMAT(5X,'DRAW CHARACTERS: **** NO INITIAL P=14 COMMAND ****')
5100  FORMAT(5X,'DRAW CHARACTERS:',T23,A57)
5200  FORMAT(1X,T23,A57)
5300  FORMAT(10X,I5,' CHARACTERS WERE DRAWN; ',I5,' UNKNOWN CHARACTERS')
      END

      SUBROUTINE DRWLIN(LINSET,LPLOT)
      IMPLICIT INTEGER (A-Z)
      REAL XFACT,YFACT,X,Y,OLDX,OLDY
      LOGICAL LINSET,P15,LPLOT,LINPRV
      COMMON CMND,CNTRL,R,S,T,U,  XFACT,YFACT,
     *       XMAX,YMAX,  XHI,YHI,XLO,YLO,
     *       PENCHG,PENNO(100),PENCNT,OLDPEN
      DATA LINCNT/0/
      DATA P15 /.TRUE./, LINPRV/.FALSE./
C
C     CHECK FOR INITIAL COMMAND=15
          IF (.NOT.LINSET) THEN
              P15 = .FALSE.
              LINSET = .TRUE.
              ENDIF
C     CHECK COORDINATES AGAINST MAX & MIN ENCOUNTERED SO FAR
          XHI = MAX( R, T, XHI)
          YHI = MAX( S, U, YHI)
          XLO = MIN( R, T, XLO)
          YLO = MIN( S, U, YLO)
C     CHECK FOR A PEN CHANGE AND/OR NEW PEN ID
          IF (CNTRL .NE. OLDPEN) THEN
              CALL NEWPEN(CNTRL)
              PENCHG = PENCHG + 1
              IF (PENCNT .LT. 100) THEN
                  PENCNT = PENCNT + 1
                  PENNO(PENCNT) = CNTRL
                  IF (PENCNT .GT. 1) THEN
                      DO 100 N=1,PENCNT-1
                          IF (PENNO(PENCNT) .EQ. PENNO(N)) THEN
                              PENCNT = PENCNT -1
                              GO TO 150
                              END IF
100                       CONTINUE
                      END IF
                  END IF
150           OLDPEN = CNTRL
              END IF
      LINCNT = LINCNT + 1
      IF (LPLOT) THEN
C         MOVE PLOTTER PEN TO BEGINNING OF LINE (IF NOT ALREADY THERE)
              X = XFACT * FLOAT(R)
              Y = YFACT * FLOAT(S)
              IF((.NOT.LINPRV) .OR. (X.NE.OLDX) .OR. (Y.NE.OLDY))
     *            CALL PLOT(X,Y,3)
C         DRAW LINE ON PLOTTER SURFACE
              X = XFACT * FLOAT(T)
              Y = YFACT * FLOAT(U)
              CALL PLOT(X,Y,2)
C         REMEMBER POSITION FOR NEXT SUCCESSIVE DRAWLINE COMMAND
              OLDX = X
              OLDY = Y
              LINPRV = .TRUE.
          END IF
      RETURN
C
C
      ENTRY ENDLIN(LINSET)
C     CHECK FOR INITIAL COMMAND=15 & PRINT "DRAW LINES" SUMMARY
          IF (P15) WRITE(20,6000) LINCNT
          IF (.NOT.P15) WRITE(20,6100)
C     REINITIALIZE
          P15 = .TRUE.
          LINCNT = 0
          LINSET = .FALSE.
          LINPRV = .FALSE.
      RETURN
C
6000  FORMAT(5X,'DRAW LINES: ',I6,' LINES WERE DRAWN')
6100  FORMAT(1X,T20,'**** NO INITIAL P=15 COMMAND ****')
      END

      SUBROUTINE GETCMD(PLTREC)
      IMPLICIT INTEGER (A-Z)
      DIMENSION PLTREC(750),Q(30),MASK(4)
      COMMON CMND,CNTRL,R,S,T,U
      COMMON /RECPNT/ KWORD,KBYTE
C     EQUATE COMMAND INTEGERS TO MNEMONIC VARIABLES
          EQUIVALENCE
     *     (Q( 1),PC), (Q( 2),CI),
     *     (Q( 3),R4), (Q( 4),R3), (Q( 5),R2), (Q( 6),R1), (Q( 7),R0),
     *     (Q( 8),S4), (Q( 9),S3), (Q(10),S2), (Q(11),S1), (Q(12),S0),
     *     (Q(13),T4), (Q(14),T3), (Q(15),T2), (Q(16),T1), (Q(17),T0),
     *     (Q(18),U4), (Q(19),U3), (Q(20),U2), (Q(21),U1), (Q(22),U0)
C     INITIALIZE BYTE MASKS, AND PLTREC WORD & BYTE POINTERS
          DATA MASK(1) /'000000FF'X/ ,
     *         MASK(2) /'0000FF00'X/ ,
     *         MASK(3) /'00FF0000'X/ ,
     *         MASK(4) /'FF000000'X/
	  DATA KWORD /1/,  KBYTE /4/
C
C     BREAK OUT 30 INTEGERS OF NEXT COMMAND
          DO 100 I=1,30
              Q(I) = IAND( PLTREC(KWORD) , MASK(KBYTE) )
              Q(I) = ISHFT( Q(I) , -8*(KBYTE-1) )
              IF ((Q(I).GT.63) .OR. (Q(I).LT.0)) WRITE(20,1500)I,Q
C             SET POINTERS TO GET NEXT BYTE
                  KBYTE = KBYTE - 1
                  IF (KBYTE .EQ. 0) KWORD = KWORD + 1
                  IF (KBYTE .EQ. 0) KBYTE = 4
		  IF (KWORD .EQ. 751) KWORD = 1
100           CONTINUE
C     CALCULATE CMND,CNTRL,R,S,T,&U
          CMND = PC
          CNTRL = CI
          R = R0 + 10*R1 + 100*R2 + 1000*R3 + 10000*R4
          S = S0 + 10*S1 + 100*S2 + 1000*S3 + 10000*S4
          T = T0 + 10*T1 + 100*T2 + 1000*T3 + 10000*T4
          U = U0 + 10*U1 + 100*U2 + 1000*U3 + 10000*U4
      RETURN
1500  FORMAT(' **** BAD INTEGER IN NEXT COMMAND: Q(',I2,') ****',
     *       3(/,1X,10(I6,1X)))
      END

      SUBROUTINE GETSIZ(XSIZE,YSIZE)
C     THIS ROUTINE WILL READ FROM THE INTERACTIVE USER'S TERMINAL,
C     THE DESIRED LENGTHS OF THE X AND Y AXES FOR THE CURRENT PLOT.
C     MAXIMUM LENGTH ALLOWED FOR Y-AXIS IS 32.0 INCHES .
C
          WRITE(6,2100)
          READ(5,*) XSIZE
          WRITE(6,2200)
          READ(5,*) YSIZE
C
          XSIZE = ABS(XSIZE)
          YSIZE = MIN( ABS(YSIZE) , 32.0 )
C
10        WRITE(6,2300)
          READ(5,2400) GO
          IF (GO .NE. 'GO')  GO TO 10
          RETURN
C
2100  FORMAT(' DESIRED LENGTH (INCHES) OF X-AXIS: ',$)
2200  FORMAT(' DESIRED LENGTH (MAX=32.0 INCHES) OF Y-AXIS: ',$)
2300  FORMAT(' SET PLOTTER ORIGIN.  TYPE "GO" WHEN READY:  ',$)
2400  FORMAT(A4)
      END

      SUBROUTINE NAMTYP(FILNAM,LIBM)
      CHARACTER FILNAM*(*) , QFLAG*1
      LOGICAL LIBM
C
C     GET INPUT FILENAME
          TYPE 1000
	  ACCEPT 2000, FILNAM
C     IS THIS AN IBM FORMAT FILE ?
10	  TYPE 3000
	  ACCEPT 4000, QFLAG
	  IF (QFLAG .EQ. 'Y') THEN
	      LIBM = .TRUE.
          ELSE IF (QFLAG .EQ. 'N') THEN
              LIBM = .FALSE.
	  ELSE
C	      INPUT ERROR; TRY AGAIN
	      GOTO 10
	  ENDIF
      RETURN
C
1000  FORMAT(' ENTER NAME OF INPUT PLT2 FILE: ',$)
2000  FORMAT(A35)
3000  FORMAT(' CONVERT FILE FROM IBM FORMAT ? (Y OR N): ',$)
4000  FORMAT(A1)
      END

      SUBROUTINE NEWSET(CHRSET,LINSET,AXSSET)
      LOGICAL CHRSET,LINSET,AXSSET
      COMMON ICMND
C
      IF (ICMND .EQ. 14) THEN
          CHRSET = .TRUE.
          ICMND = 4
      ELSE IF (ICMND .EQ. 15) THEN
          LINSET = .TRUE.
          ICMND = 5
      ELSE IF (ICMND .EQ. 16) THEN
          AXSSET = .TRUE.
          ICMND = 6
      ELSE
C         BAD CALL TO NEWSET; ICMND NOT = 14, 15, OR 16
          WRITE(20,1700) ICMND
          STOP
      END IF
C
      RETURN
C
1700  FORMAT(' BAD CALL TO NEWSET; COMMAND = ',I6)
      END

      SUBROUTINE OPTION(LPLOT,SELECT,ANYPLT)
      IMPLICIT INTEGER (A-Z)
      LOGICAL LPLOT,ANYPLT
C
C     CHECK TO SEE IF ALL, ONE, OR NO PLOTS ARE TO BE PLOTTED
10        WRITE(6,1100)
          READ(5,1150) HWMNY
          IF ((HWMNY .EQ. 'ALL') .OR. (HWMNY .EQ. 'A')) THEN
              LPLOT = .TRUE.
              ANYPLT = .TRUE.
              WRITE(6,1200)
          ELSE IF (     (HWMNY .EQ. 'ONE') .OR. (HWMNY .EQ. 'O')
     *             .OR. (HWMNY .EQ. '1'  ))  THEN
              LPLOT = .FALSE.
              ANYPLT = .TRUE.
              WRITE(6,1300)
              READ(5,*) SELECT
          ELSE IF ((HWMNY .EQ. 'NONE') .OR. (HWMNY .EQ. 'N')) THEN
              LPLOT = .FALSE.
              ANYPLT = .FALSE.
              SELECT = 0
              WRITE(6,1400)
          ELSE
C             INCORRECT INPUT; TRY AGAIN
              WRITE(6,1500)
              GO TO 10
          ENDIF
      RETURN
C
1100  FORMAT(' HOW MANY PLOTS  (ALL, ONE, OR NONE) ?  ',$)
1150  FORMAT(A4)
1200  FORMAT(' ALL PLOTS WILL BE DRAWN TO 8 X 8 INCH SIZE')
1300  FORMAT(' WHICH PLOT NUMBER IS TO BE PLOTTED ?  ',$)
1400  FORMAT(' SUMMARY ONLY; NO PLOTS WILL BE DRAWN')
1500  FORMAT(' INCORRECT INPUT: TYPE "ALL", "ONE", OR "NONE"')
      END

      SUBROUTINE SELCAM
      CHARACTER*20 OPTION(4)
      COMMON ICMD,ICNTRL
      DATA OPTION(1) /'FILM ONLY           '/,
     *     OPTION(2) /'HARDCOPY ONLY       '/,
     *     OPTION(3) /'FILM AND HARDCOPY   '/,
     *     OPTION(4) /'XXXX                '/
C
      IF (ICNTRL.GT.0 .AND. ICNTRL.LE.3) THEN
          WRITE(20,3000)ICNTRL,OPTION(ICNTRL)
      ELSE
          WRITE(20,3000)ICNTRL,OPTION(4)
      END IF
      RETURN
C
3000  FORMAT(5X,'SELECT CAMERA # ',I5,' OR ',A20)
      END

      SUBROUTINE SKPFRM(SKPCNT)
      IMPLICIT INTEGER (A-Z)
      COMMON CMND,CNTRL
C
      SKPCNT = SKPCNT + 1
      WRITE(20,4000)SKPCNT,CNTRL
      RETURN
C
4000  FORMAT(5X,'SKIP TO NEW FRAME # ',I4,'; CAMERA OPTION = ',I5)
      END

      SUBROUTINE SUMPRT(RECCNT,CMDCNT,NULCNT,SKPCNT,PLTCNT,
     *                  LPLOT,SELECT,LDEV,LSTOP)
      IMPLICIT INTEGER (A-Z)
      REAL X,Y
      LOGICAL LPLOT
      COMMON CMND,CNTRL,R,S,T,U
C
C     PRINT SUMMARY OF LAST PLOT
          R = 999
          S = 1
          T = 1
          CALL BGNPLT(PLTCNT,LPLOT,SELECT,LDEV,LSTOP)
          PLTCNT = PLTCNT - 1
C     PRINT SUMMARY OF PLOT FILE
          WRITE(20,9000)RECCNT,CMDCNT,NULCNT,SKPCNT,PLTCNT
      RETURN
C
9000  FORMAT(' END OF PLOT FILE:'/,
     *       5X,'RECORDS READ: ',I6/,
     *       5X,'COMMANDS PROCESSED: ',I6/,
     *       5X,'NULL COMMANDS: ',I6/,
     *       5X,'NUMBER OF NEW FRAMES: ',I6/,
     *       5X,'NUMBER OF PLOTS: ',I6)
      END
