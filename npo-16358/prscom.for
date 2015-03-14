      SUBROUTINE PRSCOM(LINE,LENLIN,ICOM,NUMFLG,ANUMB,MINFLG,NDEF,NSYM)        1
CDM   This routine parses a command line.                                       
CDM                                                                             
CDM   Programmer - Jon Vavrus  26 AUG 1981                                      
CDM   Revised    - Jon Vavrus   3 SEP 1981                                      
CDM   Revised    - Jon Vavrus  23 SEP 1981                                      
CDM   Revised    - Jon Vavrus  30 DEC 1981 (put LED #1 turn-on in DOIT)         
CDM   Revised    - Jon Vavrus   4 MAY 1982 (non-advanced video VT100's)         
CDM   Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                      
CDM   Revised    - Jon Vavrus   9 SEP 1982                                      
CDM   Revised    - Jon Vavrus  30 NOV 1982                                      
CDM   Revised    - Jon Vavrus  28 MAY 1983 (Allow real scroll speeds)           
CDM   Revised    - Jon Vavrus   7 SEP 1983                                      
CDM   Revised    - Jon Vavrus  21 SEP 1983                                      
CM                                                                              
CM         The routine parses a command line.  It is passed the command line    
CM    in LINE its length in LENLIN.  The command number is returned in          
CM    ICOM, if it is "minussed" the flag MINFLG is set, if there is a number    
CM    preceding it the number is stored in ANUMB and the flag NUMFLG is         
CM    set.  If ICOM = 0 then no command was found/legal (error "beep" +         
CM    LED 3 are taken care of).  If a defined symbol is found its commands      
CM    are executed and ICOM = -1.  After execution the procedure sets LINE      
CM    and LENLIN to the after parse values.  Note:  If a definition symbol      
CM    encountered, it's internal commands will be executed should NDEF or       
CM    NSYM be zero, otherwise its definition will be inserted into the          
CM    definition arrays  beginning at command NDEF of definition NSYM.          
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    DOIT    Executes commands                                                 
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
CS    CHARS   Used to pass character variables; contains LINES a character      
CS            array of the output lines, and SRCHBF the search buffer, and      
CS            CRLF which contains <CR><LF>, and REVVID + REGVID which are       
CS            the VT100 control sequences to set/unset reverse video            
C                                                                               
CS    DOITNM  Contains information transferred from the main program:           
CS                 INCHAN - input channel of the terminal                       
CS                 TIME   - scrolling speed in -10000000*seconds/line           
CS                 SBFFLG - flag indicating whether there is a string           
CS                          in the search buffer                                
CS                 DIRECT - flag for reverse (-1 for reverse, else 1)           
CS                 EBK    - EBK of input file                                   
CS                 SBFLEN - length of string(s) in search buffer(s)             
CS                 RECBLK - number of bytes used per block in FIX-BLK files     
CS                 RANGE  - beginning and ending block numbers and pointers     
CS                          of the print range                                  
CS                 VT100  - flag set .TRUE. if terminal is VT100.               
CS                 LRL    - length of the longest record in input file.         
CS                 LENLMT - maximum output length (if <0 then WRAP set)         
CS                 PAGEND - line number of last line output to screen           
CS                 PAGLEN - number of lines per page                            
CS                 COMLIN - line number for command input (bottom of screen)    
CS                 VT100A - flag set .TRUE. if VT100 has advanced video option  
C                                                                               
CS    INPUTC  Contains the input character buffer INLINE and the defined        
CS            symbols SYMS and the definition search buffers DEFBUF             
C                                                                               
CS    INPUTN  Contains the input character buffer length LENSAV, and the        
CS            symbol definitions DEFS, the number of such definitions           
CS            (NDEFS), the definition flags DEFFLG, and the definition          
CS            numbers NUMDEF                                                    
C                                                                               
CS    PRSCHR  Contains the current symbol's search buffers (CURBUF)             
C                                                                               
CS    PRSNUM  Contains the current symbol's definitions (CUR), flags            
CS            (CURFLG), and associated numbers (NUMCUR)                         
C                                                                               
C***   OUTPUT UNITS                                                             
C                                                                               
CW    9       Output to terminal.  Carriage-control LIST if file is either      
CW            CR, FTN, or PRN; otherwise no carriage-control.                   
C                                                                               
      CHARACTER*(*) LINE                                                      76
      CHARACTER*132 LINES(600)                                                77
      CHARACTER*128 INLINE                                                    78
      CHARACTER*30 SRCHBF(10),DEFBUF(10,10,60),CURBUF(10,10)                  79
      CHARACTER*11 SWIT                                                       80
      CHARACTER*8 NARROW                                                      81
      CHARACTER*4 REVVID,REGVID,WRAP                                          82
      CHARACTER*3 BOX                                                         83
      CHARACTER*2 CRLF                                                        84
      CHARACTER*1 SYMS(60),COMAND(13),QUOTE                                   85
      REAL*4 NUMDEF(10,60),NUMCUR(10)                                         86
      INTEGER*4 INCHAN,TIME,EBK,RANGE(2,2),PAGEND                             87
      INTEGER*2 DIRECT,SBFLEN(10),RECBLK,DEFS(11,10,60),LENLMT,LENSAV,        88
     1          PAGLEN,CUR(11,10),COMLIN                                        
      LOGICAL*1 SBFFLG,NUMFLG,MINFLG,ESC,VT100,BELL,DEFFLG(3,10,60),          90
     1          CURFLG(3,10),VT100A                                             
                                                                              92
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,       93
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /CHARS/LINES,SRCHBF,CRLF,REVVID,REGVID                           95
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                       96
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                          97
      COMMON /PRSNUM/CUR,NUMCUR,CURFLG                                        98
      COMMON /PRSCHR/CURBUF                                                   99
                                                                             100
      DATA NUMCOM,COMAND/13,'$','%','^','!','#','(',')','*',' ','&','@',     101
     1                   '+','<'/                                               
      DATA BELL,ESC/7,27/                                                    103
C                                                                               
C***   Setup things                                                             
C                                                                               
      NSRCH = 0                                                              107
      IF (.NOT.(SBFFLG))GO TO 50020                                          108
      DO 50030  I=1,10                                                       109
      IF (SBFLEN(I) .EQ. 0)GO TO 50031                                       110
         NSRCH = NSRCH + 1                                                   111
50030 CONTINUE                                                               112
50031 CONTINUE                                                                  
50020 CONTINUE                                                               113
      NUMFLG = .FALSE.                                                       114
      MINFLG = .FALSE.                                                       115
                                                                             116
  100 CONTINUE                                                               117
50039 CONTINUE                                                                  
C                                                                               
C***   Trim leading blanks                                                      
C                                                                               
      IF (LENLIN .LE. 0)GO TO 50060                                          121
         K = LIB$SKPC(' ',LINE(:LENLIN))                                     122
      IF (K .NE. 0)GO TO 50080                                               123
           LENLIN = 0                                                        124
      GO TO 50070                                                            125
50080 CONTINUE                                                                  
           LINE = LINE(K:LENLIN)                                             126
           LENLIN = LENLIN - K + 1                                           127
50070 CONTINUE                                                               128
50060 CONTINUE                                                               129
      IF (LENLIN .NE. 0)GO TO 50100                                          130
C     EXECUTE (NO_COMMAND)                                                   131
      ASSIGN 50110 TO KKK022                                                 131
      GO TO 79802                                                               
50110 CONTINUE                                                                  
50100 CONTINUE                                                               132
C                                                                               
C***   Check for " or '                                                         
C                                                                               
      IF (LINE(1:1) .NE. '"')GO TO 50130                                     136
      IF (LENLIN .NE. 1)GO TO 50150                                          137
C     EXECUTE (INVALID_COMMAND)                                              138
      ASSIGN 50160 TO KKK024                                                 138
      GO TO 79804                                                               
50160 CONTINUE                                                                  
50150 CONTINUE                                                               139
         K = 1                                                               140
50169 CONTINUE                                                               141
          J = INDEX(LINE(K+1:LENLIN),'"') + K                                142
      IF (K .NE. J)GO TO 50190                                               143
C     EXECUTE (INVALID_COMMAND)                                              144
      ASSIGN 50200 TO KKK024                                                 144
      GO TO 79804                                                               
50200 CONTINUE                                                                  
50190 CONTINUE                                                               145
      IF (LINE(J+1:J+1) .EQ. '"')GO TO 50220                                 146
            NSRCH = 1                                                        147
C     EXECUTE (INSERT_SEARCH_STRING)                                         148
      ASSIGN 50230 TO KKK026                                                 148
      GO TO 79806                                                               
50230 CONTINUE                                                                  
      GO TO 50171                                                            149
50220 CONTINUE                                                               150
        K = J + 1                                                            151
      GO TO 50169                                                            152
50171 CONTINUE                                                                  
      GO TO 50120                                                            153
50130 IF (LINE(1:1) .NE. '''')GO TO 50240                                       
      IF (LENLIN .NE. 1)GO TO 50260                                          154
C     EXECUTE (INVALID_COMMAND)                                              155
      ASSIGN 50270 TO KKK024                                                 155
      GO TO 79804                                                               
50270 CONTINUE                                                                  
50260 CONTINUE                                                               156
         K = 1                                                               157
50279 CONTINUE                                                               158
          J = INDEX(LINE(K+1:LENLIN),'''') + K                               159
      IF (K .NE. J)GO TO 50300                                               160
C     EXECUTE (INVALID_COMMAND)                                              161
      ASSIGN 50310 TO KKK024                                                 161
      GO TO 79804                                                               
50310 CONTINUE                                                                  
50300 CONTINUE                                                               162
      IF (LINE(J+1:J+1) .EQ. '''')GO TO 50330                                163
            NSRCH = NSRCH + 1                                                164
C     EXECUTE (INSERT_SEARCH_STRING)                                         165
      ASSIGN 50340 TO KKK026                                                 165
      GO TO 79806                                                               
50340 CONTINUE                                                                  
      GO TO 50281                                                            166
50330 CONTINUE                                                               167
        K = J + 1                                                            168
      GO TO 50279                                                            169
50281 CONTINUE                                                                  
C                                                                               
C***   Check for minus sign                                                     
C                                                                               
      GO TO 50120                                                            173
50240 IF (LINE(1:1) .NE. '-')GO TO 50350                                        
      IF (LENLIN .NE. 1)GO TO 50370                                          174
C     EXECUTE (NO_COMMAND)                                                   175
      ASSIGN 50380 TO KKK022                                                 175
      GO TO 79802                                                               
50380 CONTINUE                                                                  
50370 CONTINUE                                                               176
         I = LIB$SKPC(' ',LINE(2:LENLIN)) + 1                                177
      IF (I .NE. 1)GO TO 50400                                               178
C     EXECUTE (NO_COMMAND)                                                   179
      ASSIGN 50410 TO KKK022                                                 179
      GO TO 79802                                                               
50410 CONTINUE                                                                  
50400 CONTINUE                                                               180
C                                                                               
C---   Check for --&                                                            
C                                                                               
      IF (LINE(I:I) .NE. '-')GO TO 50430                                     184
      IF (LENLIN .NE. I)GO TO 50450                                          185
C     EXECUTE (NO_COMMAND)                                                   186
      ASSIGN 50460 TO KKK022                                                 186
      GO TO 79802                                                               
50460 CONTINUE                                                                  
50450 CONTINUE                                                               187
           J = LIB$SKPC(' ',LINE(I+1:LENLIN)) + I                            188
      IF (I .NE. J)GO TO 50480                                               189
C     EXECUTE (NO_COMMAND)                                                   190
      ASSIGN 50490 TO KKK022                                                 190
      GO TO 79802                                                               
50490 CONTINUE                                                                  
50480 CONTINUE                                                               191
      IF (LINE(J:J) .NE. '&')GO TO 50510                                     192
             ICOM = 9                                                        193
             LINE = LINE(J+1:LENLIN)                                         194
             LENLIN = LENLIN - J                                             195
             RETURN                                                          196
50510 CONTINUE                                                               197
             MINFLG = .NOT.MINFLG                                            198
             LINE = LINE(I:LENLIN)                                           199
             LENLIN = LENLIN - I + 1                                         200
C                                                                               
C---   Check for number                                                         
C                                                                               
      GO TO 50420                                                            205
50430 IF (.NOT.(ICHAR(LINE(I:I)) .GE. ICHAR('0') .AND.                          
     X          ICHAR(LINE(I:I)) .LE. ICHAR('9')))GO TO 50520                   
C     EXECUTE (GET_NUMBER)                                                   207
      ASSIGN 50530 TO KKK028                                                 207
      GO TO 79808                                                               
50530 CONTINUE                                                                  
      GO TO 50420                                                            208
50520 CONTINUE                                                                  
C                                                                               
C---   Otherwise it is minus flag                                               
C                                                                               
           MINFLG = .NOT.MINFLG                                              212
           LINE = LINE(I:LENLIN)                                             213
           LENLIN = LENLIN - I + 1                                           214
50420 CONTINUE                                                               215
C                                                                               
C***   Check for number                                                         
C                                                                               
      GO TO 50120                                                            219
50350 IF (.NOT.(ICHAR(LINE(1:1)) .GE. ICHAR('0') .AND.                          
     X          ICHAR(LINE(1:1)) .LE. ICHAR('9')))GO TO 50540                   
C     EXECUTE (GET_NUMBER)                                                   221
      ASSIGN 50550 TO KKK028                                                 221
      GO TO 79808                                                               
50550 CONTINUE                                                                  
      GO TO 50120                                                            222
50540 IF (LINE(1:1) .NE. '+')GO TO 50560                                        
      IF (LENLIN .NE. 1)GO TO 50580                                          223
           ICOM = 12                                                         224
           LENLIN = 0                                                        225
           RETURN                                                            226
50580 CONTINUE                                                               227
         I = LIB$SKPC(' ',LINE(2:LENLIN)) + 1                                228
      IF (I .NE. 1)GO TO 50600                                               229
           ICOM = 12                                                         230
           LENLIN = 0                                                        231
           RETURN                                                            232
50600 CONTINUE                                                               233
      IF (.NOT.(ICHAR(LINE(I:I)) .LE. ICHAR('9') .AND.                       234
     X          ICHAR(LINE(I:I)) .GE. ICHAR('0')))GO TO 50620                   
C     EXECUTE (GET_NUMBER)                                                   236
      ASSIGN 50630 TO KKK028                                                 236
      GO TO 79808                                                               
50630 CONTINUE                                                                  
      GO TO 50610                                                            237
50620 CONTINUE                                                                  
           LINE = LINE(I:LENLIN)                                             238
           LENLIN = LENLIN - I + 1                                           239
           ICOM = 12                                                         240
           RETURN                                                            241
50610 CONTINUE                                                               242
      GO TO 50120                                                            243
50560 CONTINUE                                                                  
C                                                                               
C***   Otherwise it must be a command                                           
C                                                                               
      DO 50640  ICOM=1,NUMCOM                                                247
      IF (LINE(1:1) .NE. COMAND(ICOM))GO TO 50660                            248
            LINE = LINE(2:)                                                  249
            LENLIN = LENLIN - 1                                              250
            RETURN                                                           251
50660 CONTINUE                                                               252
50640 CONTINUE                                                               253
C                                                                               
C***   Not a command symbol, check for definition                               
C                                                                               
      DO 50670  J=1,NDEFS                                                    257
      IF (SYMS(J) .NE. LINE(1:1))GO TO 50690                                 258
      IF (.NOT.(NUMFLG))GO TO 50710                                          259
              NUMBER = ANUMB                                                 260
      GO TO 50700                                                            261
50710 CONTINUE                                                                  
              NUMBER = 1                                                     262
50700 CONTINUE                                                               263
            LINE = LINE(2:LENLIN)                                            264
            LENLIN = LENLIN - 1                                              265
      IF (.NOT.(NDEF .EQ. 0 .OR. NSYM .EQ. 0))GO TO 50730                    266
C                                                                               
C---   Executable definition                                                    
C                                                                               
      DO 50740  L=1,NUMBER                                                   270
      DO 50750  K=1,10                                                       271
      IF (DEFS(1,K,J) .EQ. 0)GO TO 50751                                     272
      IF (.NOT.(DEFFLG(3,K,J)))GO TO 50770                                   273
      DO 50780  JJ=1,10                                                      274
                   SRCHBF(JJ) = DEFBUF(JJ,K,J)                               275
                   SBFLEN(JJ) = DEFS(1+JJ,K,J)                               276
      IF (SBFLEN(JJ) .EQ. 0)GO TO 50781                                      277
50780 CONTINUE                                                               278
50781 CONTINUE                                                                  
                  SBFFLG = .TRUE.                                            279
50770 CONTINUE                                                               280
                CALL DOIT(DEFS(1,K,J),DEFFLG(1,K,J),NUMDEF(K,J),             281
     1                    DEFFLG(2,K,J))                                        
50750 CONTINUE                                                               283
50751 CONTINUE                                                                  
50740 CONTINUE                                                               284
              ICOM = -1                                                      285
              RETURN                                                         286
50730 CONTINUE                                                               287
C                                                                               
C---   Definition insertion                                                     
C                                                                               
      DO 50790  L=1,NUMBER                                                   291
      IF (J .NE. NSYM)GO TO 50810                                            292
C                                                                               
C===   If symbol is the one being defined then use current stuff                
C                                                                               
      DO 50820  JJ=1,10                                                      296
      IF (NDEF .LE. 10)GO TO 50840                                           297
                    ICOM = 0                                                 298
                    RETURN                                                   299
50840 CONTINUE                                                               300
                  DEFS(1,NDEF,NSYM) = CUR(1,JJ)                              301
      IF (CUR(1,JJ) .EQ. 0)GO TO 50821                                       302
                  NUMDEF(NDEF,NSYM) = NUMCUR(JJ)                             303
                  DEFFLG(1,NDEF,NSYM) = CURFLG(1,JJ)                         304
                  DEFFLG(2,NDEF,NSYM) = CURFLG(2,JJ)                         305
                  DEFFLG(3,NDEF,NSYM) = CURFLG(3,JJ)                         306
      DO 50850  K=1,10                                                       307
                   DEFS(1+K,NDEF,NSYM) = CUR(1+K,JJ)                         308
      IF (CUR(1+K,JJ) .EQ. 0)GO TO 50851                                     309
                   DEFBUF(K,NDEF,NSYM) = CURBUF(K,JJ)                        310
50850 CONTINUE                                                               311
50851 CONTINUE                                                                  
                  NDEF = NDEF + 1                                            312
50820 CONTINUE                                                               313
50821 CONTINUE                                                                  
      GO TO 50800                                                            314
50810 CONTINUE                                                                  
      DO 50860  K=1,10                                                       315
      IF (NDEF .LE. 10)GO TO 50880                                           316
                    ICOM = 0                                                 317
                    RETURN                                                   318
50880 CONTINUE                                                               319
      IF (DEFS(1,K,J) .EQ. 0)GO TO 50861                                     320
                  DEFFLG(1,NDEF,NSYM) = DEFFLG(1,K,J)                        321
                  DEFFLG(2,NDEF,NSYM) = DEFFLG(2,K,J)                        322
                  DEFFLG(3,NDEF,NSYM) = DEFFLG(3,K,J)                        323
      DO 50890  JJ=1,10                                                      324
                   DEFBUF(JJ,NDEF,NSYM) = DEFBUF(JJ,K,J)                     325
                   DEFS(1+JJ,NDEF,NSYM) = DEFS(1+JJ,K,J)                     326
      IF (DEFS(1+JJ,NDEF,NSYM) .EQ. 0)GO TO 50891                            327
50890 CONTINUE                                                               328
50891 CONTINUE                                                                  
                  NUMDEF(NDEF,NSYM) = NUMDEF(K,J)                            329
                  DEFS(1,NDEF,NSYM) = DEFS(1,K,J)                            330
                  NDEF = NDEF + 1                                            331
50860 CONTINUE                                                               332
50861 CONTINUE                                                                  
50800 CONTINUE                                                               333
50790 CONTINUE                                                               334
      GO TO 50040                                                            335
50690 CONTINUE                                                               337
50670 CONTINUE                                                               338
C                                                                               
C***   If it gets here it didn't match anything                                 
C                                                                               
C     EXECUTE (INVALID_COMMAND)                                              342
      ASSIGN 50900 TO KKK024                                                 342
      GO TO 79804                                                               
50900 CONTINUE                                                                  
50120 CONTINUE                                                               343
50040 GO TO 50039                                                            344
C                                                                               
C************************************************************************       
C********************   HERE FOLLOW THE PROCEDURES   ********************       
C************************************************************************       
C                                                                               
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (GET_NUMBER)                                                 350
79808 CONTINUE                                                               350
CP    This procedure converts the number which starts the line into a           
CP    real (floating point) number in ANUMB.                                    
      IF (LENLIN .NE. 1)GO TO 50920                                          353
C     EXECUTE (NO_COMMAND)                                                   354
      ASSIGN 50930 TO KKK022                                                 354
      GO TO 79802                                                               
50930 CONTINUE                                                                  
      GO TO 50910                                                            355
50920 CONTINUE                                                                  
       I=2                                                                   356
      GO TO 50939                                                               
50940  I= I+(1)                                                                 
      IF ( I.GT.LENLIN)GO TO 50941                                              
50939 CONTINUE                                                                  
      IF (.NOT.(LINE(I:I) .EQ. ' ' .OR. (ICHAR(LINE(I:I))              .     357
     XGE. ICHAR('0') .AND. ICHAR(LINE(I:I)) .LE. ICHAR('9'))           .        
     XOR. LINE(I:I) .EQ. '.'))GO TO 50941                                       
      GO TO 50940                                                            360
50941 CONTINUE                                                                  
          READ (UNIT=LINE(:I-1),FMT=200,IOSTAT=J) ANUMB                      361
200       FORMAT (F<I-1>.0)                                                  362
      IF (J .NE. 0)GO TO 50960                                               363
            LINE = LINE(I:LENLIN)                                            364
            LENLIN = LENLIN - I + 1                                          365
            NUMFLG = .TRUE.                                                  366
      GO TO 50950                                                            367
50960 CONTINUE                                                                  
C     EXECUTE (INVALID_COMMAND)                                              368
      ASSIGN 50970 TO KKK024                                                 368
      GO TO 79804                                                               
50970 CONTINUE                                                                  
50950 CONTINUE                                                               369
50910 CONTINUE                                                               370
      GO TO KKK028                                                           371
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (INSERT_SEARCH_STRING)                                       372
79806 CONTINUE                                                               372
CP    This procedure inserts a search string (LINE(2:J-1)) in the search        
CP    buffers as string NSRCH.                                                  
C                                                                               
C***   Only ten strings                                                         
C                                                                               
      IF (NSRCH .LE. 10)GO TO 50990                                          378
       I=1                                                                   379
      GO TO 50999                                                               
51000  I= I+(1)                                                                 
      IF ( I.GT.9)GO TO 51001                                                   
50999 CONTINUE                                                                  
           SRCHBF(I) = SRCHBF(I+1)                                           380
           SBFLEN(I) = SBFLEN(I+1)                                           381
      GO TO 51000                                                            382
51001 CONTINUE                                                                  
          NSRCH = 10                                                         383
50990 CONTINUE                                                               384
        SRCHBF(NSRCH) = LINE(2:J-1)                                          385
        SBFLEN(NSRCH) = J - 2                                                386
        LENLIN = LENLIN - J                                                  387
        QUOTE = LINE(1:1)                                                    388
        IF (LENLIN .GT. 0) LINE = LINE(J+1:)                                 389
C                                                                               
C***   Get rid of double quotes                                                 
C                                                                               
        J = 1                                                                393
51009 CONTINUE                                                               394
         I = INDEX(SRCHBF(NSRCH)(J:SBFLEN(NSRCH)),QUOTE) + J - 1             395
      IF (I .EQ. (J - 1))GO TO 51011                                         396
         SRCHBF(NSRCH) = SRCHBF(NSRCH)(:I)//SRCHBF(NSRCH)(I+2:)              397
         SBFLEN(NSRCH) = SBFLEN(NSRCH) - 1                                   398
         J = I + 1                                                           399
      GO TO 51009                                                            400
51011 CONTINUE                                                                  
        SBFLEN(NSRCH+1) = 0                                                  401
        SBFFLG = .TRUE.                                                      402
      GO TO KKK026                                                           403
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (INVALID_COMMAND)                                            404
79804 CONTINUE                                                               404
CP    This procedure takes care of invalid command syntax.                      
        CALL LIB$PUT_SCREEN(CHAR(BELL))                                      406
      IF (.NOT.(VT100))GO TO 51030                                           407
          CALL LIB$PUT_SCREEN(CHAR(ESC)//'[3q')                              408
51030 CONTINUE                                                               409
        ICOM = 0                                                             410
        RETURN                                                               411
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (NO_COMMAND)                                                 413
79802 CONTINUE                                                               413
CP    This procedure handles a "empty" line.                                    
        ICOM = 0                                                             415
        LENLIN = 0                                                           416
        RETURN                                                               417
      END                                                                    419
