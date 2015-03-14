      SUBROUTINE COMLVL(CLINE,DONE)                                            1
CDM   This routine implements the "command" mode of look, by processing         
CDM   one command which is passed to it.                                        
CDM                                                                             
CDM    Programmer - Jon Vavrus  26 AUG 1981                                     
CDM    Revised    - Jon Vavrus   3 SEP 1981                                     
CDM    Revised    - Jon Vavrus  18 SEP 1981                                     
CDM    Revised    - Jon Vavrus   3 MAR 1982 (added SETNAR/SETWID)               
CDM    Revised    - Jon Vavrus   4 MAY 1982 (handles non-AVO VT100's)           
CDM    Revised    - Jon Vavrus  15 JUL 1982 (changed to handle only one         
CDM                                          command)                           
CDM    Revised    - Jon Vavrus  18 AUG 1982 (Bigger arrays)                     
CDM    Revised    - Jon Vavrus   9 SEP 1982                                     
CDM    Revised    - Jon Vavrus   4 APR 1983 (add broadcast and spawn stuff)     
CDM    Revised    - Jon Vavrus  17 MAY 1983                                     
CDM    Revised    - Jon Vavrus  28 MAY 1983 (check defined symbols)             
CDM    Revised    - Jon Vavrus  17 AUG 1983 (GETDEF changed)                    
CDM    Revised    - Jon Vavrus  27 SEP 1983                                     
CM                                                                              
CM         This routine allows the use of the following commands:               
CM              DEFINE s := a1 a2 ...                                           
CM                             - which defines a symbol s in the same man-      
CM                               ner as a line in the definitions file (see     
CM                               below), including line continuations.          
CM              EXIT           - returns to the normal "look" mode.             
CM              HELP a         - where a is one of the commands shown above.    
CM              READ           - outputs the broadcast messages (if any)        
CM                               which have been received.                      
CM              SET [NO]switch - with switch being any of WRAP, BOX, WIDE       
CM                               or NARROW.  These have the same affect as      
CM                               the command line switches of the same          
CM                               names (WIDE is the same as NONARROW),          
CM                               with the NO qualifier turning them off.        
CM              SHOW [s]       - where s is a defined symbol.  This will        
CM                               show what commands a symbol is defined         
CM                               as. (if s is omitted, all defined symbols      
CM                               are shown).                                    
CM              SPAWN          - starts a sub-process up to allow the user      
CM                               to do DCL commands.                            
CM              @filename      - Inputs the file filename as a definitions      
CM                               file.                                          
CM                                                                              
CM          Upon return DONE is set .TRUE. if the command was EXIT, .FALSE.     
CM      otherwise.                                                              
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    CLRWRP  Sets the terminal NOWRAP                                          
CC    DODEF   Processes a symbol file                                           
CC    GETDEF  Translates a line into a symbol definition                        
CC    SETNAR  Set VT100 terminal to 80 columns                                  
CC    SETWID  Set VT100 terminal to 132 columns                                 
CC    SETWRP  Sets the terminal WRAP                                            
CC    TTSPAWN Spawns a sub-process (needed to get rid of broadcast              
CC            trap                                                              
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
CS    BRDCM1  Contains character variables for broadcast handling.              
CS              BRDNOT holds the notice to be displayed on the screen.          
CS              BRDMSG which holds broadcast messages.                          
C                                                                               
CS    BRDCM2  Contains non-character information for broadcast handling.        
CS              BRDLEN the length of strings in BRDMSG.                         
CS              BRDCNT the count of strings in BRDMSG.                          
CS              BRDFLG a flag telling if there are unread messages.             
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
CS            numbers NUDMEF                                                    
C                                                                               
      CHARACTER*(*) CLINE                                                    100
      CHARACTER*1024 LINE                                                    101
      CHARACTER*132 LINES(600)                                               102
      CHARACTER*128 INLINE,BRDMSG(20)                                        103
      CHARACTER*64 IMAGE                                                     104
      CHARACTER*30 DEFBUF(10,10,60),SRCHBF(10)                               105
      CHARACTER*22 BRDNOT                                                    106
      CHARACTER*6 DEFINE,NARROW                                              107
      CHARACTER*5 SPAWN                                                      108
      CHARACTER*4 EXIT,HELP,SHOW,REVVID,REGVID,WRAP,WIDE,READ                109
      CHARACTER*3 SET,BOX                                                    110
      CHARACTER*2 CRLF                                                       111
      CHARACTER*1 SYMS(60),COMAND(13),ALLSYM(42)                             112
      REAL*4 NUMDEF(10,60)                                                   113
      INTEGER*4 INCHAN,TIME,EBK,RANGE(2,2),PAGEND,GETLIS(4),TTSPAWN          114
      INTEGER*2 DIRECT,SBFLEN(10),RECBLK,DEFS(11,10,60),LENLMT,PAGLEN,       115
     1          LENSAV,COMLIN,BRDCNT,BRDLEN(20)                                 
      LOGICAL GETDEF                                                         117
      LOGICAL*1 SBFFLG,VT100,DEFFLG(3,10,60),NO,VT100A,DONE,BRDFLG,          118
     1          OKFLAG                                                          
                                                                             120
      COMMON /BRDCM1/BRDNOT,BRDMSG                                           121
      COMMON /BRDCM2/BRDLEN,BRDCNT,BRDFLG                                    122
      COMMON /CHARS/LINES,SRCHBF,CRLF,REVVID,REGVID                          123
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      124
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                      126
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                         127
                                                                             128
      DATA NUMCOM,COMAND/13,'$','%','^','!','#','(',')','*',' ','&','@',     129
     1                   '+','<'/                                               
      DATA NUMALL,ALLSYM/42,'A','B','C','D','E','F','G','H','I','J','K',     131
     1                   'L','M','N','O','P','Q','R','S','T','U','V',           
     2                   'W','X','Y','Z','_','=','`','~','[',']','{',           
     3                   '}',';',':',',','>','?','/','\','|'/                   
      DATA GETLIS(1),GETLIS(4)/'2070040'X,0/,IMAGE/'{'/                      135
      DATA BOX , SET , WRAP , EXIT , HELP , SHOW , WIDE , DEFINE/            136
     1    'BOX','SET','WRAP','EXIT','HELP','SHOW','WIDE','DEFINE'/              
      DATA READ , SPAWN , NARROW/                                            138
     1    'READ','SPAWN','NARROW'/                                              
                                                                             140
      IF (LEN(CLINE) .NE. 0)GO TO 50020                                      141
C     EXECUTE (ERROR)                                                        142
      ASSIGN 50030 TO KKK022                                                 142
      GO TO 79802                                                               
50030 CONTINUE                                                                  
50020 CONTINUE                                                               143
C                                                                               
C***   Get any continuation lines                                               
C                                                                               
      CALL STR$TRIM(LINE,CLINE,LENGTH)                                       147
50039 CONTINUE                                                               148
      IF (LINE(LENGTH:LENGTH) .NE. '-')GO TO 50041                           149
       CALL LIB$GET_INPUT(LINE(LENGTH:),'-',J)                               150
       LENGTH = LENGTH + J                                                   151
      GO TO 50039                                                            152
50041 CONTINUE                                                                  
      CALL STR$TRANSLATE(LINE(:LENGTH),LINE(:LENGTH),' ','	')                153
      CALL STR$UPCASE(LINE(:LENGTH),LINE(:LENGTH))                           154
C                                                                               
C***   Trim leading blanks                                                      
C                                                                               
      I = LIB$SKPC(' ',LINE)                                                 158
      IF (I .GT. LENGTH)GO TO 50060                                          159
        LINE = LINE(I:LENGTH)                                                160
        I = INDEX(LINE,' ') - 1                                              161
C                                                                               
C***   Check for set                                                            
C                                                                               
      IF (I .GT. 3)GO TO 50080                                               165
      IF (LINE(:I) .NE. SET(:I))GO TO 50100                                  166
            J = LIB$SKPC(' ',LINE(I+1:)) + I                                 167
      IF (J .GE. I)GO TO 50120                                               168
C     EXECUTE (ERROR)                                                        169
      ASSIGN 50130 TO KKK022                                                 169
      GO TO 79802                                                               
50130 CONTINUE                                                                  
50120 CONTINUE                                                               170
            LINE = LINE(J:)                                                  171
            I = INDEX(LINE,' ') - 1                                          172
C                                                                               
C---   NO?                                                                      
C                                                                               
            NO = .FALSE.                                                     176
      IF (I .LE. 2)GO TO 50150                                               177
      IF (LINE(:2) .NE. 'NO')GO TO 50170                                     178
                NO = .TRUE.                                                  179
                LINE = LINE(3:)                                              180
                I = I - 2                                                    181
50170 CONTINUE                                                               182
50150 CONTINUE                                                               183
C                                                                               
C---   BOX?                                                                     
C                                                                               
      IF (I .GT. 3)GO TO 50190                                               187
      IF (LINE(:I) .NE. BOX(:I))GO TO 50210                                  188
      IF (.NOT.(NO))GO TO 50230                                              189
                  PAGLEN = COMLIN - 1                                        190
      GO TO 50220                                                            191
50230 CONTINUE                                                                  
                  PAGLEN = COMLIN - 2                                        192
50220 CONTINUE                                                               193
C     EXECUTE (OK_RETURN)                                                    194
      ASSIGN 50240 TO KKK024                                                 194
      GO TO 79804                                                               
50240 CONTINUE                                                                  
50210 CONTINUE                                                               195
50190 CONTINUE                                                               196
C                                                                               
C---   WRAP?                                                                    
C                                                                               
      IF (I .GT. 4)GO TO 50260                                               200
      IF (WRAP(:I) .NE. LINE(:I))GO TO 50280                                 201
      IF (.NOT.(NO))GO TO 50300                                              202
                  LENLMT = IABS(LENLMT)                                      203
                  CALL CLRWRP(INCHAN)                                        204
      GO TO 50290                                                            205
50300 CONTINUE                                                                  
                  LENLMT = -IABS(LENLMT)                                     206
                  CALL SETWRP(INCHAN)                                        207
50290 CONTINUE                                                               208
C     EXECUTE (OK_RETURN)                                                    209
      ASSIGN 50310 TO KKK024                                                 209
      GO TO 79804                                                               
50310 CONTINUE                                                                  
50280 CONTINUE                                                               210
C                                                                               
C---   WIDE?                                                                    
C                                                                               
      IF (.NOT.(WIDE(:I) .EQ. LINE(:I) .AND. VT100))GO TO 50330              214
      IF (.NOT.(NO))GO TO 50350                                              215
C     EXECUTE (SET_NARROW)                                                   216
      ASSIGN 50360 TO KKK026                                                 216
      GO TO 79806                                                               
50360 CONTINUE                                                                  
      GO TO 50340                                                            217
50350 CONTINUE                                                                  
C     EXECUTE (SET_WIDE)                                                     218
      ASSIGN 50370 TO KKK028                                                 218
      GO TO 79808                                                               
50370 CONTINUE                                                                  
50340 CONTINUE                                                               219
C     EXECUTE (OK_RETURN)                                                    220
      ASSIGN 50380 TO KKK024                                                 220
      GO TO 79804                                                               
50380 CONTINUE                                                                  
50330 CONTINUE                                                               221
50260 CONTINUE                                                               222
C                                                                               
C---   NARROW?                                                                  
C                                                                               
      IF (I .GT. 6)GO TO 50400                                               226
      IF (.NOT.(LINE(:I) .EQ. NARROW(:I) .AND. VT100))GO TO 50420            227
      IF (.NOT.(NO))GO TO 50440                                              228
C     EXECUTE (SET_WIDE)                                                     229
      ASSIGN 50450 TO KKK028                                                 229
      GO TO 79808                                                               
50450 CONTINUE                                                                  
      GO TO 50430                                                            230
50440 CONTINUE                                                                  
C     EXECUTE (SET_NARROW)                                                   231
      ASSIGN 50460 TO KKK026                                                 231
      GO TO 79806                                                               
50460 CONTINUE                                                                  
50430 CONTINUE                                                               232
C     EXECUTE (OK_RETURN)                                                    233
      ASSIGN 50470 TO KKK024                                                 233
      GO TO 79804                                                               
50470 CONTINUE                                                                  
50420 CONTINUE                                                               234
50400 CONTINUE                                                               235
C     EXECUTE (ERROR)                                                        236
      ASSIGN 50480 TO KKK022                                                 236
      GO TO 79802                                                               
50480 CONTINUE                                                                  
50100 CONTINUE                                                               237
50080 CONTINUE                                                               238
C                                                                               
C***   Check for EXIT                                                           
C                                                                               
      IF (I .GT. 4)GO TO 50500                                               242
      IF (LINE(:I) .NE. EXIT(:I))GO TO 50520                                 243
            DONE = .TRUE.                                                    244
            RETURN                                                           245
50520 CONTINUE                                                               246
C                                                                               
C***   Check for HELP                                                           
C                                                                               
      IF (LINE(:I) .NE. HELP(:I))GO TO 50540                                 250
C                                                                               
C---   Open help file                                                           
C                                                                               
            CALL LBR$INI_CONTROL(J,1)                                        254
      IF (IMAGE .NE. '{')GO TO 50560                                         255
              GETLIS(2) = %LOC(IMAGE)                                        256
              GETLIS(3) = %LOC(K)                                            257
              CALL SYS$GETJPI(,,,GETLIS,,,)                                  258
              L = INDEX(IMAGE,']')                                           259
      IF (L .NE. 0)GO TO 50580                                               260
                IMAGE = 'LOOK.HLB'                                           261
      GO TO 50570                                                            262
50580 CONTINUE                                                                  
                IMAGE = IMAGE(:L)//'LOOK.HLB'                                263
50570 CONTINUE                                                               264
50560 CONTINUE                                                               265
      IF (LBR$OPEN(J,IMAGE))GO TO 50600                                      266
              CALL LIB$PUT_OUTPUT('***   COULD NOT OPEN HELP LIBRARY')       267
              DONE = .FALSE.                                                 268
              RETURN                                                         269
50600 CONTINUE                                                               270
C                                                                               
C---   Output message, close file                                               
C                                                                               
            K = INDEX(LINE(I+1:),'*') + I + 1                                274
      IF (K .EQ. (I + 1))GO TO 50620                                         275
              LINE = 'ASTERISK '//LINE(K:)                                   276
      GO TO 50610                                                            277
50620 CONTINUE                                                                  
              K = INDEX(LINE(I+1:),'%') + I + 1                              278
      IF (K .EQ. (I + 1))GO TO 50640                                         279
                LINE = 'PERCENT '//LINE(K:)                                  280
      GO TO 50630                                                            281
50640 CONTINUE                                                                  
                K = INDEX(LINE(I+1:),'!') + I + 1                            282
      IF (K .EQ. (I + 1))GO TO 50660                                         283
                  LINE = 'EXCLAMATION '//LINE(K:)                            284
      GO TO 50650                                                            285
50660 CONTINUE                                                                  
                  K = LIB$SKPC(' ',LINE(I+1:)) + I                           286
      IF (I .NE. K)GO TO 50680                                               287
                    LINE = 'INFO'                                            288
      GO TO 50670                                                            289
50680 CONTINUE                                                                  
                    LINE = LINE(K:)                                          290
50670 CONTINUE                                                               291
50650 CONTINUE                                                               292
50630 CONTINUE                                                               293
50610 CONTINUE                                                               294
            CALL STR$TRIM(LINE,LINE,I)                                       295
            K = INDEX(LINE,' ') - 1                                          296
      IF (K .NE. I)GO TO 50700                                               297
              CALL LBR$GET_HELP(J,,,,LINE)                                   298
      GO TO 50690                                                            299
50700 CONTINUE                                                                  
              I = LIB$SKPC(' ',LINE(K+1:)) + K                               300
              CALL LBR$GET_HELP(J,,,,LINE(:K),LINE(I:))                      301
50690 CONTINUE                                                               302
            CALL LBR$CLOSE(J)                                                303
C     EXECUTE (OK_RETURN)                                                    304
      ASSIGN 50710 TO KKK024                                                 304
      GO TO 79804                                                               
50710 CONTINUE                                                                  
50540 CONTINUE                                                               305
C                                                                               
C***   Check for show                                                           
C                                                                               
      IF (LINE(:I) .NE. SHOW(:I))GO TO 50730                                 309
            J = LIB$SKPC(' ',LINE(I+1:)) + I                                 310
      IF (LINE(J+1:) .EQ. ' ')GO TO 50750                                    311
C     EXECUTE (ERROR)                                                        312
      ASSIGN 50760 TO KKK022                                                 312
      GO TO 79802                                                               
50760 CONTINUE                                                                  
50750 CONTINUE                                                               313
      IF (J .NE. I)GO TO 50780                                               314
      DO 50790  ISYM=1,NDEFS                                                 315
C     EXECUTE (SHOW_SYMBOL)                                                  316
      ASSIGN 50800 TO KKK030                                                 316
      GO TO 79810                                                               
50800 CONTINUE                                                                  
50790 CONTINUE                                                               317
C     EXECUTE (OK_RETURN)                                                    318
      ASSIGN 50810 TO KKK024                                                 318
      GO TO 79804                                                               
50810 CONTINUE                                                                  
      GO TO 50770                                                            319
50780 CONTINUE                                                                  
      DO 50820  ISYM=1,NDEFS                                                 320
      IF (LINE(J:J) .NE. SYMS(ISYM))GO TO 50840                              321
C     EXECUTE (SHOW_SYMBOL)                                                  322
      ASSIGN 50850 TO KKK030                                                 322
      GO TO 79810                                                               
50850 CONTINUE                                                                  
C     EXECUTE (OK_RETURN)                                                    323
      ASSIGN 50860 TO KKK024                                                 323
      GO TO 79804                                                               
50860 CONTINUE                                                                  
50840 CONTINUE                                                               324
50820 CONTINUE                                                               325
C     EXECUTE (ERROR)                                                        326
      ASSIGN 50870 TO KKK022                                                 326
      GO TO 79802                                                               
50870 CONTINUE                                                                  
50770 CONTINUE                                                               327
50730 CONTINUE                                                               328
C                                                                               
C***   Check for read                                                           
C                                                                               
      IF (LINE(:I) .NE. READ(:I))GO TO 50890                                 332
      IF (BRDCNT .NE. 0)GO TO 50910                                          333
              CALL LIB$PUT_OUTPUT(CRLF//'No broadcast messages have '        334
     1                            //'been received.'//CRLF)                     
      GO TO 50900                                                            336
50910 CONTINUE                                                                  
              CALL LIB$PUT_OUTPUT(CRLF//'The following messages '//          337
     1                            'have been received:')                        
      IF (BRDCNT .GT. 20)GO TO 50930                                         339
                J = BRDCNT                                                   340
      GO TO 50920                                                            341
50930 CONTINUE                                                                  
                J = 20                                                       342
50920 CONTINUE                                                               343
      DO 50940  I=1,J                                                        344
C                                                                               
C---   Remove bells from message                                                
C                                                                               
50949 CONTINUE                                                               348
                K = INDEX(BRDMSG(I)(:BRDLEN(I)),CHAR(7))                     349
      IF (K .EQ. 0)GO TO 50951                                               350
                BRDMSG(I) = BRDMSG(I)(:K-1)//BRDMSG(I)(K+1:)                 351
                BRDLEN(I) = BRDLEN(I) - 1                                    352
      GO TO 50949                                                            353
50951 CONTINUE                                                                  
C                                                                               
C---   Output message                                                           
C                                                                               
               CALL LIB$PUT_OUTPUT(BRDMSG(I)(:BRDLEN(I)))                    357
50940 CONTINUE                                                               358
      IF (BRDCNT .LE. 20)GO TO 50970                                         359
                CALL LIB$PUT_OUTPUT(CRLF//                                   360
     1               'There were too many messages!  The buffer over-'          
     2               //'flowed, there were un-seen messages'//CRLF)             
      GO TO 50960                                                            363
50970 CONTINUE                                                                  
                CALL LIB$PUT_OUTPUT(CRLF)                                    364
50960 CONTINUE                                                               365
              BRDCNT = 0                                                     366
50900 CONTINUE                                                               367
            BRDFLG = .FALSE.                                                 368
C     EXECUTE (OK_RETURN)                                                    369
      ASSIGN 50980 TO KKK024                                                 369
      GO TO 79804                                                               
50980 CONTINUE                                                                  
50890 CONTINUE                                                               370
50500 CONTINUE                                                               371
C                                                                               
C***  SPAWN?                                                                    
C                                                                               
      IF (I .GT. 5)GO TO 51000                                               375
      IF (SPAWN(:I) .NE. LINE(:I))GO TO 51020                                376
            IF (.NOT.TTSPAWN()) CALL LIB$PUT_OUTPUT                          377
     1          ('Unable to spawn sub-process')                                 
C     EXECUTE (OK_RETURN)                                                    379
      ASSIGN 51030 TO KKK024                                                 379
      GO TO 79804                                                               
51030 CONTINUE                                                                  
51020 CONTINUE                                                               380
51000 CONTINUE                                                               381
C                                                                               
C***   DEFINE?                                                                  
C                                                                               
      IF (I .GT. 6)GO TO 51050                                               385
      IF (DEFINE(:I) .NE. LINE(:I))GO TO 51070                               386
            J = LIB$SKPC(' ',LINE(I+1:)) + I                                 387
      IF (J .NE. I)GO TO 51090                                               388
C     EXECUTE (ERROR)                                                        389
      ASSIGN 51100 TO KKK022                                                 389
      GO TO 79802                                                               
51100 CONTINUE                                                                  
51090 CONTINUE                                                               390
            LINE = LINE(J:)                                                  391
            CALL STR$TRIM(LINE,LINE,LENGTH)                                  392
            I = INDEX(LINE,':=') - 1                                         393
            J = INDEX(LINE,'"') - 1                                          394
            K = INDEX(LINE,'''') - 1                                         395
      IF (.NOT.(I .LT. 1 .OR. (J .LT. I .AND. J .GE. 0) .OR. (K .LT. I       396
     X          .AND. K .GE. 0)))GO TO 51120                                    
C                                                                               
C---   If no := then error                                                      
C                                                                               
C     EXECUTE (ERROR)                                                        401
      ASSIGN 51130 TO KKK022                                                 401
      GO TO 79802                                                               
51130 CONTINUE                                                                  
51120 CONTINUE                                                               402
C                                                                               
C---   Make sure that it is definable                                           
C                                                                               
            OKFLAG = .FALSE.                                                 406
      DO 51140  L=1,NUMALL                                                   407
             IF (LINE(1:1) .EQ. ALLSYM(L)) OKFLAG = .TRUE.                   408
51140 CONTINUE                                                               409
      IF (.NOT.(OKFLAG))GO TO 51160                                          410
C     EXECUTE (DEFINE_SYMBOL)                                                411
      ASSIGN 51170 TO KKK032                                                 411
      GO TO 79812                                                               
51170 CONTINUE                                                                  
C     EXECUTE (OK_RETURN)                                                    412
      ASSIGN 51180 TO KKK024                                                 412
      GO TO 79804                                                               
51180 CONTINUE                                                                  
      GO TO 51150                                                            413
51160 CONTINUE                                                                  
C     EXECUTE (ERROR)                                                        414
      ASSIGN 51190 TO KKK022                                                 414
      GO TO 79802                                                               
51190 CONTINUE                                                                  
51150 CONTINUE                                                               415
51070 CONTINUE                                                               416
51050 CONTINUE                                                               417
C                                                                               
C***   Check for @                                                              
C                                                                               
      IF (LINE(1:1) .NE. '@')GO TO 51210                                     421
          OPEN (UNIT=30,NAME=LINE(2:I),TYPE='OLD',ERR=700,READONLY)          422
          CALL DODEF                                                         423
      GO TO 51220                                                            424
  700 CONTINUE                                                               425
              I = INDEX(LINE,' ') - 1                                        426
              CALL LIB$PUT_OUTPUT('***   UNABLE TO OPEN '//LINE(:I))         427
51220 CONTINUE                                                               428
C     EXECUTE (OK_RETURN)                                                    429
      ASSIGN 51230 TO KKK024                                                 429
      GO TO 79804                                                               
51230 CONTINUE                                                                  
51210 CONTINUE                                                               430
C     EXECUTE (ERROR)                                                        431
      ASSIGN 51240 TO KKK022                                                 431
      GO TO 79802                                                               
51240 CONTINUE                                                                  
50060 CONTINUE                                                               432
      DONE = .FALSE.                                                         433
      RETURN                                                                 434
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (DEFINE_SYMBOL)                                              435
79812 CONTINUE                                                               435
CP    This procedure defines a symbol.                                          
C                                                                               
C***   See if symbol is already defined                                         
C                                                                               
  900 CONTINUE                                                               440
       II=1                                                                  441
      GO TO 51259                                                               
51260  II= II+(1)                                                               
      IF ( II.GT.NDEFS)GO TO 51261                                              
51259 CONTINUE                                                                  
      IF (SYMS(II) .EQ. LINE(1:1))GO TO 51251                                442
      GO TO 51260                                                            443
51261 CONTINUE                                                                  
         NDEFS = NDEFS + 1                                                   444
         II = NDEFS                                                          445
         SYMS(II) = LINE(1:1)                                                446
51251 CONTINUE                                                               447
        LINE = LINE(I+3:)                                                    448
        LENGTH = LENGTH - I - 2                                              449
C                                                                               
C***   Actually get definitions                                                 
C                                                                               
      IF (GETDEF(LINE(:LENGTH),II))GO TO 51280                               453
C     EXECUTE (ERROR)                                                        454
      ASSIGN 51290 TO KKK022                                                 454
      GO TO 79802                                                               
51290 CONTINUE                                                                  
51280 CONTINUE                                                               455
      GO TO KKK032                                                           456
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (ERROR)                                                      457
79802 CONTINUE                                                               457
CP    This writes an error message                                              
        CALL STR$TRIM(LINE,LINE,I)                                           459
        IF (I .NE. 0) CALL LIB$PUT_OUTPUT(LINE(:I)//' ??')                   460
        DONE = .FALSE.                                                       461
        RETURN                                                               462
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (OK_RETURN)                                                  464
79804 CONTINUE                                                               464
CP    This procedure sets DONE to false and returns.                            
        DONE = .FALSE.                                                       466
        RETURN                                                               467
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SET_NARROW)                                                 469
79806 CONTINUE                                                               469
CP    This procedure sets the terminal to 80 column mode.                       
        LENLMT = 80*LENLMT/IABS(LENLMT)                                      471
        CALL SETNAR(INCHAN)                                                  472
      IF (VT100A)GO TO 51310                                                 473
          COMLIN = 24                                                        474
          PAGLEN = PAGLEN + 10                                               475
51310 CONTINUE                                                               476
      GO TO KKK026                                                           477
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SET_WIDE)                                                   478
79808 CONTINUE                                                               478
CP    This procedure sets the terminal to 132 column mode.                      
        LENLMT = 132*LENLMT/IABS(LENLMT)                                     480
        CALL SETWID(INCHAN)                                                  481
      IF (VT100A)GO TO 51330                                                 482
          COMLIN = 14                                                        483
          PAGLEN = PAGLEN - 10                                               484
51330 CONTINUE                                                               485
      GO TO KKK028                                                           486
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SHOW_SYMBOL)                                                487
79810 CONTINUE                                                               487
CP    This procedure writes out the definition for one symbol                   
        LINE = SYMS(ISYM)//' := '                                            489
        L = 6                                                                490
       K=1                                                                   491
      GO TO 51339                                                               
51340  K= K+(1)                                                                 
      IF ( K.GT.10)GO TO 51341                                                  
51339 CONTINUE                                                                  
      IF (DEFS(1,K,ISYM) .EQ. 0)GO TO 51341                                  492
C                                                                               
C***   Convert the number                                                       
C                                                                               
      IF (.NOT.(DEFFLG(1,K,ISYM)))GO TO 51360                                496
           WRITE (UNIT=LINE(L:),FMT=1000)NUMDEF(K,ISYM)                      497
1000       FORMAT (F10.3)                                                    498
           M = LIB$SKPC(' ',LINE(L:))                                        499
           LINE(L:) = LINE(L+M-1:)                                           500
           L = 11 + L - M                                                    501
           IF (LINE(L-3:L-1) .EQ. '000') L = L - 4                           502
51360 CONTINUE                                                               503
C                                                                               
C***   Is it --&?                                                               
C                                                                               
      IF (DEFS(1,K,ISYM) .NE. 9)GO TO 51380                                  507
           LINE(L:) = '--& '                                                 508
           L = L + 4                                                         509
      GO TO 51340                                                            510
51380 CONTINUE                                                               511
C                                                                               
C***   Set minus if necessary                                                   
C                                                                               
      IF (.NOT.(DEFFLG(2,K,ISYM)))GO TO 51400                                515
           LINE(L:) = '-'                                                    516
           L = L + 1                                                         517
51400 CONTINUE                                                               518
C                                                                               
C***   Search?                                                                  
C                                                                               
      IF (DEFS(1,K,ISYM) .NE. 3)GO TO 51420                                  522
      IF (.NOT.(DEFFLG(3,K,ISYM)))GO TO 51440                                523
             LINE(L:) = '"'//DEFBUF(1,K,ISYM)(:DEFS(2,K,ISYM))               524
             KK = L                                                          525
             L = L + 3 + DEFS(2,K,ISYM)                                      526
51449 CONTINUE                                                               527
              LL = INDEX(LINE(KK+1:),'"') + KK                               528
      IF (LL .EQ. KK)GO TO 51451                                             529
              LINE(LL:) = '"'//LINE(LL:)                                     530
              L = L + 1                                                      531
              KK = LL + 2                                                    532
      GO TO 51449                                                            533
51451 CONTINUE                                                                  
             LINE(L-2:L-2) = '"'                                             534
       JJ=2                                                                  535
      GO TO 51459                                                               
51460  JJ= JJ+(1)                                                               
      IF ( JJ.GT.10)GO TO 51461                                                 
51459 CONTINUE                                                                  
      IF (DEFS(1+JJ,K,ISYM) .EQ. 0)GO TO 51461                               536
              LINE(L:) = ''''//DEFBUF(JJ,K,ISYM)(:DEFS(1+JJ,K,ISYM))         537
              KK = L                                                         538
              L = L + 3 + DEFS(1+JJ,K,ISYM)                                  539
51469 CONTINUE                                                               540
               LL = INDEX(LINE(KK+1:),'''') + KK                             541
      IF (LL .EQ. KK)GO TO 51471                                             542
               LINE(LL:) = ''''//LINE(LL:)                                   543
               L = L + 1                                                     544
               KK = LL + 2                                                   545
      GO TO 51469                                                            546
51471 CONTINUE                                                                  
              LINE(L-2:L-2) = ''''                                           547
      GO TO 51460                                                            548
51461 CONTINUE                                                                  
             LINE(L-1:) = '^'                                                549
             L = L + 1                                                       550
      GO TO 51430                                                            551
51440 CONTINUE                                                                  
             LINE(L:) = '^ '                                                 552
             L = L + 2                                                       553
51430 CONTINUE                                                               554
      GO TO 51340                                                            555
51420 CONTINUE                                                               556
C                                                                               
C***   Set LINE                                                                 
C                                                                               
       M=1                                                                   560
      GO TO 51479                                                               
51480  M= M+(1)                                                                 
      IF ( M.GT.NUMCOM)GO TO 51481                                              
51479 CONTINUE                                                                  
      IF (DEFS(1,K,ISYM) .NE. M)GO TO 51500                                  561
            LINE(L:) = COMAND(M)                                             562
            L = L + 2                                                        563
      GO TO 51481                                                            564
51500 CONTINUE                                                               565
      GO TO 51480                                                            566
51481 CONTINUE                                                                  
      GO TO 51340                                                            567
51341 CONTINUE                                                                  
C                                                                               
C***   Output line                                                              
C                                                                               
        M = 1                                                                571
        L = L - 1                                                            572
51509 CONTINUE                                                               573
      IF ((L - M) .GE. IABS(LENLMT))GO TO 51530                              574
           CALL LIB$PUT_OUTPUT(LINE(M:L-1))                                  575
      GO TO 51511                                                            576
51530 CONTINUE                                                               577
           CALL LIB$PUT_OUTPUT(LINE(M:M+IABS(LENLMT)-1))                     578
         M = M + IABS(LENLMT)                                                580
      GO TO 51509                                                            581
51511 CONTINUE                                                                  
      GO TO KKK030                                                           582
      END                                                                    583
