      SUBROUTINE RDREQMT(LUREQMT,EXPNAME,
     *    MAXEXP,MAXTGT,MAXREQEXP,MAXREQTGT,
     *    KEYREQEXP,REQEXP,REQTGT,NUMEXP,NUMTGT,
     *    LUPRINT1,LUPRINT2,LUERR,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS SUBROUTINE READS THE EXPERIMENT REQUIREMENTS FILE.  THE FILE
C FORMAT AND DESCRIPTION OF OBSERVATION REQUIREMENTS IS GIVEN IN THE
C TOSS DOCUMENT OBSREQMT.TXT
C
C PROTOTYPE DEVELOPED BY SCOTT LAMBROS 1/9/81 AND NAMED RDEXP. THIS
C ROUTINE SHOULD BE USED RDEXP FOR PROGRAMS WRITTEN AFTER 12/85.
C
C VARIABLE DIM TYPE I/O DESCRIPTION
C -------- --- ---- --- -----------
C
C LUREQMT   1   I*4  I  LOGICAL UNIT FOR EXPERIMENT REQUIREMENTS FILE.
C
C EXPNAME      CH*8 I/O NAMES OF EXPERIMENTS FOR WHICH REQUIREMENTS
C        MAXEXP         ARE WANTED.
C
C                       IF EXPNAME(1) = 'ALL' AT ENTRY, THEN EXPNAME IS
C                       LOADED WITH THE NAMES OF ALL EXPERIMENTS ON THE
C                       FILE. UNUSED ELEMENTS OF EXPNAME ARE LOADED
C                       WITH BLANKS.
C
C                       IF NOT 'ALL', EXPNAME IS INPUT ONLY AND INFO IS
C                       RETURNED FOR NON-BLANK EXPERIMENT NAMES.  BLANK
C                       AND NON-BLANK NAMES MAY BE INTERMINGLED.
C
C                       EXPERIMENT NAMES MUST BE UNIQUE.
C
C MAXEXP    1   I*4  I  MAXIMUM NUMBER OF EXPERIMENTS CALLER CAN HANDLE.
C                       THAT IS, THE NUMBER OF EXPERIMENTS FOR WHICH THE
C                       CALLER HAS DIMENSIONED ITS ARRAYS.
C
C                       MAXEXP IS USED TO DIMENSION CALLING SEQUENCE
C                       ARRAYS.  USE THE SAME VALUE AS THE CALLER USED
C                       TO DIMENSION THE ARRAYS.  MUST BE 1 OR GREATER.
C
C                       MAXEXP MUST BE LT/EQ 50. SEE INTERNAL PARAMETER
C                       MAXEXPMTS.
C
C MAXTGT    1   I*4  I  MAXIMUM NUMBER OF TARGETS PERMITTED FOR ANY ONE
C                       EXPERIMENT. THAT IS, THE NUMBER OF TARGETS FOR
C                       WHICH THE CALLER HAS DIMENSIONED ITS ARRAYS.
C
C                       MAXTGT IS USED TO DIMENSION CALLING SEQUENCE
C                       ARRAYS.  USE THE SAME VALUE AS THE CALLER USED
C                       TO DIMENSION THE ARRAYS.  MUST BE 1 OR GREATER.
C
C MAXREQEXP 1   I*4  I  THE NUMBER OF EXPERIMENT-SPECIFIC REQUIREMENTS
C                       THE CALLER CAN HANDLE.  THAT IS, THE NUMBER OF
C                       EXPERIMENT RELATED REQUIREMENTS FOR WHICH THE
C                       CALLER HAS DIMENSIONED ITS ARRAYS.
C
C                       MAXREQEXP IS USED TO DIMENSION THE REQEXP ARRAY
C                       AND MUST BE THE SAME VALUE USED BY THE CALLING
C                       ROUTINE THAT DIMENSIONED REQEXP.
C
C                       MAXREQEXP MUST BE 1 OR GREATER.
C
C MAXREQTGT 1   I*4  I  THE NUMBER OF TARGET-SPECIFIC REQUIREMENTS
C                       THE CALLER CAN HANDLE.  THAT IS, THE NUMBER OF
C                       TARGET RELATED REQUIREMENTS FOR WHICH THE
C                       CALLER HAS DIMENSIONED ITS ARRAYS.
C
C                       MAXREQTGT IS USED TO DIMENSION THE REQTGT ARRAY
C                       AND MUST BE THE SAME VALUE USED BY THE CALLING
C                       ROUTINE TO DIMENSIONED REQTGT.
C
C                       MAXREQTGT MUST BE 1 OR GREATER.
C
C KEYREQEXP     I*4  I  KEYREQEXP(I) TELLS THIS ROUTINE WHAT KIND OF
C     MAXREQEXP         REQUIREMENTS ARE PERMITTED(IE, WHAT KIND IT
C                       CAN PROCESS).
C
C                       VALID VALUES ARE ZERO THRU 10(MAXKEYS-1 IN THE
C                       INCLUDE FILE). =0 MEANS CALLER HAS ALLOCATED
C                       ARRAY SPACE BUT NO REQUIREMENT TYPE HAS BEEN
C                       ASSIGNED.  FOR EXAMPLE, KEYREQEXP(4)=0 MEANS
C                       REQEXP(*,4,*) EXISTS BUT HAS NO MEANING; IT IS
C                       INITIALIZED TO -999.D0, BUT NOT USED OTHERWISE.
C
C                       IF THE FILE CONTAINS A REQUIREMENT NOT PERMITTED
C                       BY KEYREQEXP, AN ERROR RETURN OCCURS.
C
C                       KEYREQEXP(I) = KK MEANS THAT REQEXP(-,I,-) GIVES
C                       REQUIREMENTS AS FOLLOWS:
C
C                        KK = 1, TDRS
C                           = 2, ORBIT DAY/NIGHT
C                           = 3, SOUTH ATLANTIC ANOMALY
C                           = 4, OBSCURATION MASK
C                           = 5, SUN AVOIDANCE
C                           = 6, MOON AVOIDANCE
C                           = 7, BRIGHT EARTH LIMB AVOIDANCE
C                           = 8, DARK EARTH LIMB AVOIDANCE
C                           = 9, VELOCITY VECTOR AVOIDANCE
C                           =10, ZENITH VECTOR MAX SEPARATION
C                           =OTHERWISE, IGNORED.
C
C                       EXAMPLE: KEYREQEXP(I) = 2,9,6,0,0,....0 MEANS
C                                CALLING PROGRAM CAN ACCEPT ORBIT
C                                DAY/NIGHT, MOON AVOIDANCE, AND
C                                VELOCITY VECTOR AVOIDANCE REQUIREMENTS.
C
C                       THIS SCHEME ALLOWS THE ADDITION OF NEW
C                       REQUIREMENTS TO THE READER WITHOUT MODIFYING
C                       ROUTINES ALREADY USING IT.
C
C REQEXP        I*4  O  REAL*8 ARRAY OF EXPERIMENT-SPECIFIC REQUIREMENTS
C  2,MAXREQEXP,MAXEXP
C                       FOR BLANK EXPNAME ELEMENTS, THE ASSOCIATED
C                       REQEXP ELEMENTS ARE NOT CHANGED FROM THEIR
C                       CONTENTS AT ENTRY.
C
C                       INITIALLY, REQEXP IS FILLED WITH -999.D0 FOR ALL
C                       NON-BLANK EXPERIMENT NAMES(ALL NAMES WHEN 'ALL'
C                       IS USED) AND RESET AS AN EXPERIMENT'S DATA IS
C                       READ. IF AN ERROR IS SENSED, A PARTIAL LOAD
C                       MAY OCCUR.
C
C                       REQUIREMENTS TYPES MAY BE PERMITTED BY CALLER
C                       BUT CAN BE ABSENT IN THE FILE. THOSE NOT ENTERED
C                       VIA THE REQUIREMENTS FILE ARE ASSIGNED DEFAULT
C                       VALUES MAKING THE REQUIREMENT INEFFECTIVE,
C                       ASSIGNMENTS ARE MADE IN RDREQMT5A1.
C
C                       FOR IEXP'TH EXPMT, WHEN KEYREQEXP(J) = KK, THEN
C                       REQEXP(I,J,IEXP) =
C
C                       (KK=1) TDRS FLAG:
C                         FOR I=1
C                            = 0.D0, IGNORE TDRS STATUS
C                            = 1.D0, USE ONLY TDRS1(EAST)
C                            = 2.D0, USE ONLY TDRS2(WEST)
C                            =12.D0, AT LEAST ONE IS TO BE VISIBLE
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C
C                       (KK=2) ORBIT DAY/NIGHT FLAG: 
C                         FOR I=1
C                            =0.D0, IGNORE DAY/NIGHT STATUS
C                            =1.D0, ORBIT NIGHT IS REQUIRED
C                            =2.D0, ORBIT DAY IS REQUIRED
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C                          
C                       (KK=3) SAA FLAG: (NEITHER POSITIVE = IGNORE SAA)
C                         FOR I=1
C                            = IF POSITIVE, MODEL NUMBER OF ONE REGION
C                         FOR I=2
C                            = IF POSITIVE, MODEL NUMBER OF ANOTHER
C                              REGION
C                         DEFAULTS IF NOT ON FILE: 0.D0, 0.D0
C
C                       (KK=4) OBSCURATION MASK I.D. NUMBER.
C                         FOR I=1
C                            =MASK NUMBER
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C
C                       (KK=5) SUN AVOIDANCE
C                         FOR I=1
C                            = MINIMUM SEPARATION BETWEEN THE SUN AND
C                              TARGET LINES OF SIGHT. (RADIANS)
C                         FOR I=2
C                            =0.D0, CONSTRAINT IS ON AT ALL TIMES
C                            =1.D0,, ON ONLY DURING ORBIT DAY
C                         DEFAULTS IF NOT ON FILE: 0.D0, 0.D0
C
C                       (KK=6) MOON AVOIDANCE
C                         FOR I=1
C                            = MINIMUM SEPARATION BETWEEN THE MOON AND
C                              TARGET LINES OF SIGHT. (RADIANS)
C                         FOR I=2
C                            =0.D0, CONSTRAINT IS ON AT ALL TIMES
C                            =1.D0, ON ONLY WHEN THE MOON IS NOT
C                                  OCCULTED BY THE EARTH
C                         DEFAULTS IF NOT ON FILE: 0.D0, 0.D0
C
C                       (KK=7) BRIGHT EARTH LIMB AVOIDANCE
C                         FOR I=1
C                            = MINIMUM SEPARATION BETWEEN THE LIMB AND
C                              TARGET LINES OF SIGHT. (RADIANS)
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C
C                       (KK=8) DARK EARTH LIMB AVOIDANCE
C                         FOR I=1
C                            = MINIMUM SEPARATION BETWEEN THE LIMB AND
C                              TARGET LINES OF SIGHT. (RADIANS)
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C
C                       (KK=9) VELOCITY VECTOR AVOIDANCE
C                         FOR I=1
C                            = MINIMUM SEPARATION BETWEEN THE VELOCITY
C                              VECTOR AND THE TARGET LINE OF SIGHT.
C                              (RADIANS)
C                         FOR I=2
C                            = -999.D0 (NOT USED)
C                         DEFAULTS IF NOT ON FILE: 0.D0, -999.D0
C
C                       (KK=10) ZENITH DIRECTION REQUIREMENT
C                         FOR I=1
C                            = ZENITH VECTOR MAXIMUM ANGULAR SEPARATION
C                              FROM TARGET DIRECTION. (RADIANS).
C                         DEFAULT IF NOT ON FILE: 180.0/DEGRAD
C
C REQTGT        R*8  O  REQUIREMENTS AND FLAGS ASSOCIATED WITH
C  MAXREQTGT,MAXTGT,MAXEXP   INDIVIDUAL TARGETS OF INDIVIDUAL
C                       EXPERIMENTS.
C
C                       FOR BLANK EXPNAME ELEMENTS, THE ASSOCIATED
C                       REQTGT ELEMENTS ARE NOT CHANGED FROM THEIR
C                       CONTENTS AT ENTRY.
C
C                       INITIALLY, REQTGT IS SET TO -888.D0 FOR ALL
C                       NON-BLANK EXPERIMENT NAMES(ALL NAMES WHEN 'ALL'
C                       IS USED) AND RESET AS AN EXPERIMENT'S DATA IS
C                       READ. IF AN ERROR RETURN OCCURS, A PARTIAL LOAD
C                       MAY OCCUR.
C
C                       FOR THE ITARG'TH TARGET OF THE IEXP'TH
C                       EXPERIMENT, REQTGT(I,ITARG,IEXP) = ......
C
C                       (I=1) TARGET ID (CALLER CAN USE THE NEAREST
C                             INTEGER FUNCTION, JIDNNT, TO GET ID AS
C                             AN INTEGER)
C
C                       (I=2,MAXREQTGT) THE I'TH ELEMENT IS THE SAME
C                             AS THE I'TH VALUE READ FROM THE RECORD
C                             THAT CONTAINED THE TARGET ID. IF THERE IS
C                             NO I'TH VALUE ON THE RECORD, THEN -888.D0
C                             IS RETURNED.
C
C NUMEXP    1   I*4  O  NUMBER OF EXPERIMENTS FOR WHICH DATA WAS READ
C                       FROM THE REQUIREMENTS FILE.
C
C                       THE EXPNAME ARRAY MAY HAVE BLANK ELEMENTS, SO
C                       THE LAST NON-BLANK NAME IS NOT NECESSARILY
C                       EXPNAME(NUMEXP).
C
C NUMTGT MAXEXP I*4  O  NUMTGT(I) IS THE NUMBER OF TARGET ID'S READ FROM
C                       THE EXPERIMENT REQUIREMENTS FILE FOR THE I'TH
C                       EXPERIMENT. ZERO IS POSSIBLE AND NOT AN ERROR
C                       CONDITION FOR THIS READER.
C
C                       INITIALLY, NUMTGT IS SET TO -1 FOR ALL NON-BLANK
C                       EXPERIMENT NAMES(ALL NAMES WHEN 'ALL' IS USED)
C                       AND SET TO ZERO OR POSITIVE WHEN AN EXPERIMENT
C                       HAS BEEN READ OK. IF AN ERROR RETURN OCCURS, YOU
C                       CAN IDENTIFY EXPERIMENTS LOADED OK BY TESTING
C                       THE NUMTGT VALUES.
C
C LUPRINT1  1   I*4  I  PRINT UNIT TO WHICH REQUIREMENTS FILE LISTING
C                       IS WRITTEN. THIS IS A DIRECT LISTING, NOT A
C                       REFORMATTED ONE.
C
C                       = 0 OR NEGATIVE, DO NOT LIST THE FILE
C                       = OTHERWISE, LIST IT ON UNIT LUPRINT1
C
C LUPRINT2  1   I*4  I  PRINT UNIT TO WHICH REQUIREMENTS INFO IS WRITTEN
C                       IN A STANDARD FORMAT.
C
C                       = 0 OR NEGATIVE, DO NOT PRINT EXPERIMENT
C                           REQUIREMENTS INFO
C                       = OTHERWISE, PRINT THE INFO ON UNIT LUPRINT2
C
C LUERR     1   I*4  I  LOGICAL UNIT FOR OUTPUT OF ERROR MESSAGES. IF
C                       ZERO OR NEGATIVE, NONE ARE POSSIBLE.
C
C IERR      1   I*4  O  ERROR FLAG:
C
C                       = 0, NO,ERROR.
C
C                       = OTHERWISE, AN ERROR. ERROR MESSAGES SHOULD
C                         HELP. IERR ASSIGNMENTS ARE...
C
C                         1, AN ERROR HAS BEEN ENCOUNTERED IN THE
C                            REQUIREMENTS FILE. YOU SHOULD END THE RUN
C                            AND FIX THE FILE.
C                         2, AN ERROR HAS BEEN ENCOUNTERED IN THE ARRAY
C                            SIZES THAT THE CALLER HAS SPECIFIED. YOU
C                            SHOULD MAKE APPROPRIATE CORRECTIONS TO THE
C                            CALLING PROGRAM OR TO THE FILE.
C                         3, A REQUESTED EXPERIMENT NAME WAS NOT FOUND
C                            ON THE REQUIREMENTS FILE.
C                         4, A MISCELLANEOUS ERROR HAS OCCURRED.
C
C***********************************************************************
C
C BY C PETRUZZO GSFC/742  1/86
C        MODIFIED....
C
C***********************************************************************
C
C
      INCLUDE 'RDREQMT.INC'
C
      INTEGER*4   NUMTGT(MAXEXP),KEYREQEXP(MAXREQEXP)
      REAL*8      REQEXP(2,MAXREQEXP,MAXEXP)
      REAL*8      REQTGT(MAXREQTGT,MAXTGT,MAXEXP)
      CHARACTER*8 EXPNAME(MAXEXP)
C
      CHARACTER*8 FILENAMES(MAXEXPMTS)
C
C
C ****************
C *  INITIALIZE  *
C ****************
C
      IERR = 0
      NUMEXP=0
      DO IEXP=1,MAXEXP
        IF(EXPNAME(IEXP).NE.' ' .OR. EXPNAME(1).EQ.'ALL') THEN
          NUMTGT(IEXP) = -1
          CALL MTXSETR8(REQEXP(1,1,IEXP),-999.D0,2,MAXREQEXP)
          CALL MTXSETR8(REQTGT(1,1,IEXP),-888.D0,MAXREQTGT,MAXTGT)
          END IF
        END DO
C
C
C ***************************
C *  ERROR CHECKS ON INPUT  *
C ***************************
C
      CALL RDREQMT1(LUREQMT,EXPNAME,MAXEXP,MAXREQEXP,KEYREQEXP,
     *    LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
C
C *********************
C *  LIST INPUT FILE  *
C *********************
C
      IF(LUPRINT1.GT.0) THEN
        CALL RDREQMT2(LUREQMT,LUPRINT1)
        END IF
C
C
C ************************************************************
C *  CHECK REQMT FILE. FETCH EXPERIMENT NAMES FROM THE FILE  *
C ************************************************************
C
      CALL RDREQMT3(LUREQMT,FILENAMES,LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
C
C *********************************************
C *  LOAD EXPNAME ARRAY FROM FILENAMES ARRAY  *   FOR EXPNAME(1) = 'ALL'
C *********************************************
C
      IF(EXPNAME(1).EQ.'ALL') THEN
        CALL RDREQMT4(FILENAMES,MAXEXP,EXPNAME,LUERR,IERR)
        IF(IERR.NE.0) GO TO 9999
        END IF
C
C
C *****************************
C *  LOAD THE REQUESTED DATA  *
C *****************************
C
      CALL RDREQMT5(LUREQMT,EXPNAME,
     *    MAXEXP,MAXTGT,MAXREQEXP,MAXREQTGT,
     *    KEYREQEXP,REQEXP,REQTGT,NUMEXP,NUMTGT,LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
C
C **********************************
C *  OUTPUT INFO ON UNIT LUPRINT2  *
C **********************************
C
      IF(LUPRINT2.GT.0) THEN
        CALL RDREQMT6(LUPRINT2,LUREQMT,EXPNAME,
     *    MAXEXP,MAXTGT,MAXREQEXP,MAXREQTGT,
     *    KEYREQEXP,REQEXP,REQTGT,NUMEXP,NUMTGT)
        END IF
C
C
C *************
C *  WRAP-UP  *
C *************
C
 9999 CONTINUE
      RETURN
      END
