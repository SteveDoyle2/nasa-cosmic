      SUBROUTINE LINCON(START)                                                 1
CDM   This subroutine converts a buffer full of data into output lines in       
CDM   the output arrays.                                                        
CDM                                                                             
CDM   Programmer - Jon Vavrus  12 NOV 1980                                      
CDM   Revised    - Jon Vavrus  13 MAY 1981                                      
CDM   Revised    - Jon Vavrus  30 JUL 1981 (change CCLINE usage)                
CDM   Revised    - Jon Vavrus  26 AUG 1981                                      
CDM   Revised    - Jon Vavrus   4 MAY 1982 (non-advanced video VT100's)         
CDM   Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                      
CDM   Revised    - Jon Vavrus  12 JAN 1983                                      
CM                                                                              
CM         The conversion is done one line per record with blank lines          
CM    being inserted to take care of FORTRAN carriage-control (which is         
CM    then stripped off the record)(+ carriage control is ignored), and         
CM    printer form-feeds (other printer control sequences are ignored).         
CM                                                                              
CM         If START is set to .TRUE. in the call, then it is assumed that       
CM    the buffer is completely new, and bears no relationship to the current    
CM    contents of the output arrays, thus the current contents of the arrays    
CM    are disregarded and written over.  If START is set to .FALSE. in the      
CM    call, the buffer is assumed to be full of data which should immediately   
CM    follows the information currently in the output arrays.  It is also       
CM    assumed (if START is .FALSE.) that the buffer pointer (PBUF) is           
CM    pointing at the correct byte.                                             
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    INTASC  Converts a string of bytes into a character variable.             
CC    SFTLIN  Shifts the lines in the output arrays to free up space for        
CC            more lines to be converted.                                       
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
CS    CHARS   Contains the character array containing the actual output lines.  
C                                                                               
CS    DOITNM  Contains information transferred from the main program:           
CS                 INCHAN - input channel of the terminal                       
CS                 TIME   - scrolling speed in -10000000*seconds/line           
CS                 SBFFLG - flag indicating whether there is a string           
CS                          in the search buffer                                
CS                 DIRECT - flag for reverse (-1 for reverse, else 1)           
CS                 EBK    - EBK of input file                                   
CS                 SBFLEN - length(s) of string(s) in search buffer(s)          
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
CS    LNCN    Contains output arrays, and file pointers and information:        
CS                 LINLEN - array of output line lengths                        
CS                 LINEP  - top of screen line (or last line output by          
CS                          backward scroll)                                    
CS                 NLINES - number of lines in arrays                           
CS                 BUFFER - array of unconverted data                           
CS                 PBUF   - current position in BUFFER                          
CS                 NBUF   - number of bytes in BUFFER                           
CS                 BLK    - next block in file                                  
CS                 FSZ    - record length for FIX files, or FSZ for VFC files   
CS                 IFSZ   - word extended record length for FIX files           
CS                 RFM    - files RFM                                           
CS                 RAT    - files RAT (1=CR,2=FTN,3=PRN,4=none,<0=BLK set)      
CS                 SBLK   - starting block of current batch of output lines     
CS                 CCLINE - array containing pointer to where the line's        
CS                          record starts in the file (CCLINE(1,x) = block      
CS                          #, CCLINE(2,x) = byte number in block).  If         
CS                          CCLINE(1,x) = 0 then it is a carriage control       
CS                          line.                                               
CS                 LBLK   - last block processed by LINCON                      
CS                 LPBUF  - last PBUF value processed by LINCON                 
C                                                                               
      CHARACTER*132 LINES(600)                                                78
      INTEGER*4 RFM,RAT,FSZ,BLK,PBUF,PBSAVE,SBLK,CCLINE(2,600),               79
     1          INCHAN,TIME,RANGE(2,2),EBK,PAGEND,BUFDSC(2)                     
      INTEGER*2 LINLEN(600),DIRECT,SBFLEN(10),RECBLK,PAGLEN,LENLMT,           81
     1          COMLIN                                                          
      LOGICAL*1 BUFFER(3750),START,SBFFLG,VT100,VT100A                        83
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,       84
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /LNCN/LINLEN,LINEP,NLINES,BUFFER,PBUF,NBUF,BLK,FSZ,IFSZ,         86
     1             RFM,RAT,SBLK,CCLINE,LBLK,LPBUF                               
      COMMON /CHARS/LINES                                                     88
C                                                                               
C***   Calculate start position if this is a new block                          
C                                                                               
      IF (.NOT.(START))GO TO 50020                                            92
        NLINES = 0                                                            93
        LINEP = 1                                                             94
        LPBUF = 0                                                             95
        LBLK = SBLK                                                           96
      IF (RAT .LE. 0)GO TO 50040                                              97
      IF (RFM .NE. 1)GO TO 50060                                              98
            PBUF = IFSZ*(512*(SBLK - 1)/IFSZ + 1)                             99
            PBUF = LIB$EXTZV(0,9,PBUF) + 1                                   100
            IF (PBUF .EQ. (IFSZ + 1)) PBUF = 1                               101
      GO TO 50050                                                            102
50060 CONTINUE                                                                  
            PBUF = 1                                                         103
50069 CONTINUE                                                               104
             PBUF = PBUF + 1                                                 105
      IF (BUFFER(PBUF) .NE. 0)GO TO 50070                                    106
             I = PBUF + LIB$EXTZV(0,8,BUFFER(PBUF-1)) + 2                    107
             IF (BUFFER(PBUF-1)) I = I + 1                                   108
      IF (BUFFER(I) .EQ. 0)GO TO 50071                                       109
50070 GO TO 50069                                                            110
50071 CONTINUE                                                                  
            PBUF = PBUF - 1                                                  111
50050 CONTINUE                                                               112
      GO TO 50030                                                            113
50040 CONTINUE                                                                  
          PBUF = 1                                                           114
50030 CONTINUE                                                               115
50020 CONTINUE                                                               116
C                                                                               
C***   Check for either FORTRAN or printer carriage control                     
C                                                                               
C***   FORTRAN                                                                  
C                                                                               
   20 CONTINUE                                                               122
50079 CONTINUE                                                                  
C     EXECUTE (NEW_LINE)                                                     123
      ASSIGN 50090 TO KKK022                                                 123
      GO TO 79802                                                               
50090 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 50110                     124
      IF (RFM .NE. 1)GO TO 50130                                             125
           I = BUFFER(PBUF)                                                  126
      GO TO 50120                                                            127
50130 CONTINUE                                                                  
      IF (BUFFER(PBUF) .NE. FSZ)GO TO 50150                                  128
             I = 0                                                           129
      GO TO 50140                                                            130
50150 CONTINUE                                                                  
             I = BUFFER(PBUF+FSZ+2)                                          131
50140 CONTINUE                                                               132
50120 CONTINUE                                                               133
      IF (I .NE. '30'X)GO TO 50170                                           134
C                                                                               
C***   30 octal is an ASCII 0                                                   
C                                                                               
C     EXECUTE (INSERT_BLANK_LINE)                                            138
      ASSIGN 50180 TO KKK024                                                 138
      GO TO 79804                                                               
50180 CONTINUE                                                                  
      GO TO 50160                                                            139
50170 CONTINUE                                                                  
      IF (I .NE. '31'X)GO TO 50200                                           140
C                                                                               
C***   31 octal is an ASCII 1                                                   
C                                                                               
C     EXECUTE (INSERT_BLANK_LINE)                                            144
      ASSIGN 50210 TO KKK024                                                 144
      GO TO 79804                                                               
50210 CONTINUE                                                                  
C     EXECUTE (INSERT_BLANK_LINE)                                            145
      ASSIGN 50220 TO KKK024                                                 145
      GO TO 79804                                                               
50220 CONTINUE                                                                  
50200 CONTINUE                                                               146
50160 CONTINUE                                                               147
      GO TO 50100                                                            148
50110 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 3 .OR. RAT .EQ. -3))GO TO 50240                     149
C                                                                               
C***   Printer pre-line line feeds                                              
C                                                                               
      IF (LIB$EXTZV(7,1,BUFFER(PBUF+2)) .NE. 0)GO TO 50260                   153
             I = LIB$EXTZV(0,7,BUFFER(PBUF+2)) - 1                           154
      IF (I .EQ. 0)GO TO 50280                                               155
      DO 50290  J=1,I                                                        156
C     EXECUTE (INSERT_BLANK_LINE)                                            157
      ASSIGN 50300 TO KKK024                                                 157
      GO TO 79804                                                               
50300 CONTINUE                                                                  
50290 CONTINUE                                                               158
50280 CONTINUE                                                               159
             PBSAVE = PBUF + 3                                               160
50260 CONTINUE                                                               161
50240 CONTINUE                                                               162
50100 CONTINUE                                                               163
C                                                                               
C***   Extract records into lines                                               
C                                                                               
      IF (RFM .NE. 1)GO TO 50320                                             167
C                                                                               
C***   Fixed length records                                                     
C                                                                               
C     EXECUTE (SET_CCLINE)                                                   171
      ASSIGN 50330 TO KKK026                                                 171
      GO TO 79806                                                               
50330 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 50350                     172
           LENGTH = FSZ - 1                                                  173
           PBUF = PBUF + 1                                                   174
      GO TO 50340                                                            175
50350 CONTINUE                                                                  
           LENGTH = FSZ                                                      176
50340 CONTINUE                                                               177
         I = PBUF                                                            178
C     EXECUTE (CONVERT_TO_LINES)                                             179
      ASSIGN 50360 TO KKK028                                                 179
      GO TO 79808                                                               
50360 CONTINUE                                                                  
         PBUF = PBUF + LENGTH                                                180
         IF (FSZ) PBUF = PBUF + 1                                            181
      IF ((PBUF + IFSZ - 1) .LE. NBUF)GO TO 50380                            182
C     EXECUTE (STORE_REMAINDER)                                              183
      ASSIGN 50390 TO KKK030                                                 183
      GO TO 79810                                                               
50390 CONTINUE                                                                  
50380 CONTINUE                                                               184
      GO TO 50310                                                            185
50320 CONTINUE                                                                  
C                                                                               
C***   Variable length records                                                  
C                                                                               
C     EXECUTE (SET_CCLINE)                                                   189
      ASSIGN 50400 TO KKK026                                                 189
      GO TO 79806                                                               
50400 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 50420                     190
           LENGTH = LIB$EXTZV(0,8,BUFFER(PBUF)) - 1 - FSZ                    191
           I = PBUF + 3 + FSZ                                                192
      GO TO 50410                                                            193
50420 CONTINUE                                                                  
           LENGTH = LIB$EXTZV(0,8,BUFFER(PBUF)) - FSZ                        194
           I = PBUF + 2 + FSZ                                                195
50410 CONTINUE                                                               196
      IF (LENGTH .LE. 0)GO TO 50440                                          197
C     EXECUTE (CONVERT_TO_LINES)                                             198
      ASSIGN 50450 TO KKK028                                                 198
      GO TO 79808                                                               
50450 CONTINUE                                                                  
      IF (.NOT.(BUFFER(PBUF)))GO TO 50470                                    199
             PBUF = I + LENGTH + 1                                           200
      GO TO 50460                                                            201
50470 CONTINUE                                                                  
             PBUF = I + LENGTH                                               202
50460 CONTINUE                                                               203
      GO TO 50430                                                            204
50440 CONTINUE                                                                  
           IF (LENGTH .EQ. 0 .AND. (RAT .EQ. 2 .OR. RAT .EQ. -2))            205
     1         PBUF = PBUF + 2                                                  
           PBUF = PBUF + 2 + FSZ                                             207
           IF (FSZ) PBUF = PBUF + 1                                          208
           LINLEN(NLINES) = 1                                                209
           LINES(NLINES) = ' '                                               210
50430 CONTINUE                                                               211
C                                                                               
C***   Printer post-line line feeds                                             
C                                                                               
      IF (.NOT.(RAT .EQ. 3 .OR. RAT .EQ. -3))GO TO 50490                     215
      IF (LIB$EXTZV(7,1,BUFFER(PBSAVE)) .NE. 0)GO TO 50510                   216
             I = LIB$EXTZV(0,7,BUFFER(PBSAVE)) - 1                           217
      IF (I .LE. 0)GO TO 50530                                               218
C     EXECUTE (NEW_LINE)                                                     219
      ASSIGN 50540 TO KKK022                                                 219
      GO TO 79802                                                               
50540 CONTINUE                                                                  
      DO 50550  J=1,I                                                        220
C     EXECUTE (INSERT_BLANK_LINE)                                            221
      ASSIGN 50560 TO KKK024                                                 221
      GO TO 79804                                                               
50560 CONTINUE                                                                  
50550 CONTINUE                                                               222
               NLINES = NLINES - 1                                           223
50530 CONTINUE                                                               224
50510 CONTINUE                                                               225
50490 CONTINUE                                                               226
      IF ((LIB$EXTZV(0,8,BUFFER(PBUF)) + PBUF + 1) .LE. NBUF)GO TO 50580     227
C     EXECUTE (STORE_REMAINDER)                                              228
      ASSIGN 50590 TO KKK030                                                 228
      GO TO 79810                                                               
50590 CONTINUE                                                                  
50580 CONTINUE                                                               229
50310 CONTINUE                                                               230
      GO TO 50079                                                            231
C                                                                               
C****************************************************************************** 
C***********************   HERE FOLLOW THE PROCEDURES   *********************** 
C****************************************************************************** 
C                                                                               
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (CONVERT_TO_LINES)                                           237
79808 CONTINUE                                                               237
CP    This procedure converts the LENGTH bytes in BUFFER into lines in LINES    
CP    (starting at NLINES).  It takes into account form feeds (generate 2       
CP    blank lines) and line feeds.                                              
C                                                                               
C***   Construct descriptor                                                     
C                                                                               
        BUFDSC(1) = LENGTH                                                   244
        BUFDSC(2) = %LOC(BUFFER(I))                                          245
        J = I                                                                246
C                                                                               
C***   Split string                                                             
C                                                                               
50599 CONTINUE                                                               250
         IFF = LIB$LOCC(CHAR(12),BUFDSC)                                     251
         ILF = LIB$LOCC(CHAR(10),BUFDSC)                                     252
      IF (.NOT.(ILF .EQ. 0 .AND. IFF .EQ. 0))GO TO 50620                     253
           ILEN = BUFDSC(1)                                                  254
           IICHAR = ILEN                                                     255
      GO TO 50610                                                            256
50620 CONTINUE                                                                  
      IF (.NOT.((ILF .LT. IFF .AND. ILF .NE. 0) .OR. IFF .EQ. 0))GO TO 5     257
     X0640                                                                      
C                                                                               
C===   Line feed first                                                          
C                                                                               
             ILEN = ILF - 1                                                  261
             IICHAR = ILF                                                    262
C                                                                               
C---   Get rid of <CR>                                                          
C                                                                               
             IF (BUFFER(J+ILEN-1) .EQ. 13) ILEN = ILEN - 1                   266
      GO TO 50630                                                            267
50640 CONTINUE                                                                  
C                                                                               
C===   FF first                                                                 
C                                                                               
             CCLINE(1,NLINES+1) = CCLINE(1,NLINES)                           271
             CCLINE(2,NLINES+1) = CCLINE(2,NLINES)                           272
C     EXECUTE (INSERT_BLANK_LINE)                                            273
      ASSIGN 50650 TO KKK024                                                 273
      GO TO 79804                                                               
50650 CONTINUE                                                                  
             CCLINE(1,NLINES+1) = CCLINE(1,NLINES)                           274
             CCLINE(2,NLINES+1) = CCLINE(2,NLINES)                           275
C     EXECUTE (INSERT_BLANK_LINE)                                            276
      ASSIGN 50660 TO KKK024                                                 276
      GO TO 79804                                                               
50660 CONTINUE                                                                  
             ILEN = IFF - 1                                                  277
             IICHAR = IFF                                                    278
50630 CONTINUE                                                               279
50610 CONTINUE                                                               280
         IF (ILEN .GT. 132) ILEN = 132                                       281
         BUFDSC(1) = BUFDSC(1) - IICHAR                                      282
         BUFDSC(2) = BUFDSC(2) + IICHAR                                      283
      IF ((J + IICHAR + 1) .GE. (I + LENGTH))GO TO 50680                     284
C                                                                               
C===   Not end of record                                                        
C                                                                               
           CCLINE(1,NLINES+1) = CCLINE(1,NLINES)                             288
           CCLINE(2,NLINES+1) = CCLINE(2,NLINES)                             289
           CCLINE(1,NLINES) = 0                                              290
C     EXECUTE (NEW_LINE)                                                     291
      ASSIGN 50690 TO KKK022                                                 291
      GO TO 79802                                                               
50690 CONTINUE                                                                  
      IF (ILEN .NE. 0)GO TO 50710                                            292
             LINES(NLINES-1) = ' '                                           293
             LINLEN(NLINES-1) = 1                                            294
      GO TO 50700                                                            295
50710 CONTINUE                                                                  
             CALL INTASC(ILEN,BUFFER(J),LINES(NLINES-1))                     296
             LINLEN(NLINES-1) = ILEN                                         297
50700 CONTINUE                                                               298
      GO TO 50670                                                            299
50680 CONTINUE                                                                  
C                                                                               
C===   End of record                                                            
C                                                                               
      IF (ILEN .NE. 0)GO TO 50730                                            303
             LINES(NLINES) = ' '                                             304
             LINLEN(NLINES) = 1                                              305
      GO TO 50720                                                            306
50730 CONTINUE                                                                  
             CALL INTASC(ILEN,BUFFER(J),LINES(NLINES))                       307
             LINLEN(NLINES) = ILEN                                           308
50720 CONTINUE                                                               309
      IF (.NOT.(ABS(RAT) .NE. 4 .AND. ILF .NE. 0 .AND. IFF .EQ. 0))GO TO     310
     X 50750                                                                    
             CCLINE(1,NLINES+1) = CCLINE(1,NLINES)                           311
             CCLINE(2,NLINES+1) = CCLINE(2,NLINES)                           312
             CCLINE(1,NLINES) = 0                                            313
C     EXECUTE (NEW_LINE)                                                     314
      ASSIGN 50760 TO KKK022                                                 314
      GO TO 79802                                                               
50760 CONTINUE                                                                  
             LINES(NLINES) = ' '                                             315
             LINLEN(NLINES) = 1                                              316
50750 CONTINUE                                                               317
50670 CONTINUE                                                               318
         J = J + IICHAR                                                      319
      IF ((J + 1) .GE. (I + LENGTH))GO TO 50601                              320
      GO TO 50599                                                            321
50601 CONTINUE                                                                  
      GO TO KKK028                                                           322
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (INSERT_BLANK_LINE)                                          323
79804 CONTINUE                                                               323
CP    This procedure inserts a blank line of length one character into the      
CP    proper arrays.                                                            
        LINLEN(NLINES) = 1                                                   326
        LINES(NLINES) = ' '                                                  327
        CCLINE(1,NLINES) = 0                                                 328
C     EXECUTE (NEW_LINE)                                                     329
      ASSIGN 50770 TO KKK022                                                 329
      GO TO 79802                                                               
50770 CONTINUE                                                                  
      GO TO KKK024                                                           330
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (NEW_LINE)                                                   331
79802 CONTINUE                                                               331
CP    This procedure takes care of shifting the translated lines in the         
CP    arrays to make more room.                                                 
        NLINES = NLINES + 1                                                  334
      IF (NLINES .NE. 600)GO TO 50790                                        335
      IF (.NOT.(START))GO TO 50810                                           336
C                                                                               
C***   START = .TRUE., therefore we should throw away the rest of the blocks    
C***   instead of trying to shift.                                              
C                                                                               
       II=600                                                                341
      GO TO 50819                                                               
50820  II= II+(-1)                                                              
      IF ( II.LT.1)GO TO 50821                                                  
50819 CONTINUE                                                                  
C                                                                               
C===   Find out which block we are in                                           
C                                                                               
      IF (CCLINE(1,II) .EQ. 0)GO TO 50840                                    345
               IIBLK = CCLINE(1,II) - 1                                      346
C                                                                               
C===   Move to previous block                                                   
C                                                                               
       JJ=II                                                                 350
      GO TO 50849                                                               
50850  JJ= JJ+(-1)                                                              
      IF ( JJ.LT.1)GO TO 50851                                                  
50849 CONTINUE                                                                  
      IF (CCLINE(1,JJ) .NE. IIBLK)GO TO 50870                                351
C                                                                               
C===   Throw away rest of blocks                                                
C                                                                               
                  NLINES = JJ - 1                                            355
                  NBUF = RECBLK*IIBLK                                        356
                  PBUF = RECBLK*(IIBLK - 1) + CCLINE(2,JJ)                   357
C                                                                               
C===   Done so return                                                           
C                                                                               
C     EXECUTE (STORE_REMAINDER)                                              361
      ASSIGN 50880 TO KKK030                                                 361
      GO TO 79810                                                               
50880 CONTINUE                                                                  
50870 CONTINUE                                                               362
      GO TO 50850                                                            363
50851 CONTINUE                                                                  
50840 CONTINUE                                                               364
      GO TO 50820                                                            365
50821 CONTINUE                                                                  
      GO TO 50800                                                            366
50810 CONTINUE                                                                  
            CALL SFTLIN(LINES)                                               367
50800 CONTINUE                                                               368
50790 CONTINUE                                                               369
      GO TO KKK022                                                           370
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SET_CCLINE)                                                 371
79806 CONTINUE                                                               371
CP    This procedure sets the block and pointer values in the array CCLINE.     
C                                                                               
C***   First set up proper block number and byte count                          
C                                                                               
        J = PBUF + LPBUF                                                     376
        CCLINE(1,NLINES) = LBLK + J/RECBLK                                   377
C                                                                               
C***   Get byte offset                                                          
C                                                                               
      IF (RECBLK .NE. 512)GO TO 50900                                        381
          CCLINE(2,NLINES) = LIB$EXTZV(0,9,J)                                382
      GO TO 50890                                                            383
50900 CONTINUE                                                                  
          CCLINE(2,NLINES) = J - RECBLK*(J/RECBLK)                           384
50890 CONTINUE                                                               385
        IF (J .NE. 0 .AND. CCLINE(2,NLINES) .EQ. 0)                          386
     1      CCLINE(1,NLINES) = CCLINE(1,NLINES) - 1                             
        IF (CCLINE(2,NLINES) .EQ. 0) CCLINE(2,NLINES) = RECBLK               388
      GO TO KKK026                                                           389
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (STORE_REMAINDER)                                            390
79810 CONTINUE                                                               390
CP    This procedure moves the partial record at the end of BUFFER to           
CP    the beginning of BUFFER and sets NBUF, and PBUF accordingly.              
       I=PBUF                                                                393
      GO TO 50909                                                               
50910  I= I+(1)                                                                 
      IF ( I.GT.NBUF)GO TO 50911                                                
50909 CONTINUE                                                                  
         BUFFER(I-PBUF+1) = BUFFER(I)                                        394
      GO TO 50910                                                            395
50911 CONTINUE                                                                  
        NBUF = NBUF - PBUF + 1                                               396
        I = PBUF + LPBUF                                                     397
      IF (RECBLK .NE. 512)GO TO 50930                                        398
          LPBUF = LIB$EXTZV(0,9,I)                                           399
      GO TO 50920                                                            400
50930 CONTINUE                                                                  
          LPBUF = PBUF - RECBLK*(I/RECBLK)                                   401
50920 CONTINUE                                                               402
        IF (LPBUF .EQ. 0) LPBUF = RECBLK                                     403
        LPBUF = LPBUF - 1                                                    404
        PBUF = 1                                                             405
       I=NLINES                                                              406
      GO TO 50939                                                               
50940  I= I+(-1)                                                                
      IF ( I.LT.1)GO TO 50941                                                   
50939 CONTINUE                                                                  
      IF (CCLINE(1,I) .EQ. 0)GO TO 50960                                     407
           LBLK = CCLINE(1,I)                                                408
           IF (LPBUF .EQ. 0) LBLK = LBLK + 1                                 409
      GO TO 50941                                                            410
50960 CONTINUE                                                               411
      GO TO 50940                                                            412
50941 CONTINUE                                                                  
        RETURN                                                               413
      END                                                                    415
