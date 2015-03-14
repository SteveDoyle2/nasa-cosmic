      SUBROUTINE DOIT(COMAND,NUMFLG,TANUMB,MINFLG)                             1
CDM   This subroutine executes the commands passed by the main program or       
CDM   rings the terminal bell and turns LED 3 on (error signal) if the          
CDM   command is invalid.                                                       
CDM                                                                             
CDM   Programmer - Jon Vavrus  13 NOV 1980                                      
CDM   Revised    - Jon Vavrus  30 MAR 1981                                      
CDM   Revised    - Jon Vavrus  12 MAY 1981                                      
CDM   Revised    - Jon Vavrus  21 JUL 1981                                      
CDM   Revised    - Jon Vavrus  31 JUL 1981 (change CCLINE usage)                
CDM   Revised    - Jon Vavrus  27 AUG 1981                                      
CDM   Revised    - Jon Vavrus  10 SEP 1981                                      
CDM   Revised    - Jon Vavrus  23 SEP 1981                                      
CDM   Revised    - Jon Vavrus   6 NOV 1981                                      
CDM   Revised    - Jon Vavrus  30 DEC 1981 (make LED's work right)              
CDM   Revised    - Jon Vavrus   4 MAY 1982 (non-advanced video VT100's)         
CDM   Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                      
CDM   Revised    - Jon Vavrus   9 SEP 1982                                      
CDM   Revised    - Jon Vavrus  30 NOV 1982                                      
CDM   Revised    - Jon Vavrus  12 JAN 1983                                      
CDM   Revised    - Jon Vavrus   4 APR 1983 (Take care of "Quitting")            
CM                                                                              
CM         The routine is called with the numeric code for the command          
CM    (COMAND) which if negative, means that it is not to be "saved" for        
CM    possible command #13 execution; a flag set .TRUE. if there is an          
CM    associated number (NUMFLG); a number (TANUMB); and a flag set             
CM    .TRUE. if there is an associated minus sign.                              
CM                                                                              
CM         For VT100's the given command is executed after which LED 1 is       
CM    turned off (indicating operation finished), LED 3 is turned off           
CM    if successful and on if not (error indication), LED 2 is turned           
CM    on if the reverse option is currently set or off if not (reverse          
CM    indicator), and LED 4 is turned on if the search buffer now contains      
CM    something (buffer indicator).                                             
CM                                                                              
CM         The valid commands types are:                                        
CM              1 - jump TANUMB lines (positive or negative)                    
CM              2 - jump by TANUMB blocks (positive or negative)                
CM              3 - search (forward) for string(s) in SRCHBF array and          
CM                  display from that line if found, if not found the           
CM                  terminal bell is rung and LED 3 is turned on.  Matches      
CM                  are found regardless of lower/uppercase differences.        
CM                  Search backward if MINFLG = .TRUE..  Repeat TANUMB          
CM                  times if NUMFLG = .TRUE..                                   
CM              4 - set reverse switch, all commands will now work in re-       
CM                  verse (i.e. n$ will jump -n lines, etc.).  Repeat           
CM                  TANUMB times if NUMFLG = .TRUE..                            
CM              5 - scroll (backwards if MINFLG = .TRUE.)                       
CM              6 - stop scrolling or cancel current line jump, search,         
CM                  or & command.                                               
CM              7 - set scrolling speed to TANUMB lines per second (positive    
CM                  or negative) (originally set to 2).                         
CM                  NOTE:  there is a maximum speed that will not be ex-        
CM                  ceeded (varies with situation) even if scrolling            
CM                  speed is set at a high value.                               
CM              8 - mark the TANUMB line form the top of the screen as one      
CM                  of the boundaries of the print range.  The last two so      
CM                  marked lines define the print range.                        
CM              9 - output TANUMB lines to the file LOOKPRINT.LIS in the        
CM                  default directory (file format will be the same as the      
CM                  original file, with sequential organization).  Creates      
CM                  a new file.  If no number is specified (or is zero)         
CM                  and there is a valid print range marked, then that          
CM                  range will be output, if no valid print range then          
CM                  23 lines are output.                                        
CM             10 - same as 9 except the lines are output to SYS$PRINT.         
CM                  If MINFLG = .TRUE. then output goes to LOOKPRINT.LIS        
CM                  file (new file if no previous command created one           
CM                  during this job, otherwise appended to the end of the       
CM                  existing file).                                             
CM             11 - exit the program.                                           
CM             12 - re-write the screen                                         
CM             13 - re-execute the last command that was passed to DOIT         
CM                  with a positive value (except for 13)                       
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    BLKCLS  Closes the input file.                                            
CC    BLKIN   Reads in one block of data.                                       
CC    BLKSIN  Reads in 7 blocks of data.                                        
CC    CONVUP  Converts a string to upper-case.                                  
CC    INTASC  Converts a given character variable to upper-case.                
CC    LINCON  Converts data to output lines in the output arrays.               
CC    PRTCLS  Closes the printer output file used in & commands.                
CC    PRTOPN  Opens the printer output file.                                    
CC    PRTOUT  Outputs a record to the printer output file.                      
CC    REDOIT  Calls DOIT, allows for recursion.                                 
CC    WORKNG  Writes flashing "WORKING" message.                                
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
CS    CHARS   Used to pass character variables; contains LINES a character      
CS            array of the output lines, and SRCHBF the search buffer, and      
CS            CRLF which contains <CR><LF>, and REVVID + REGVID which are       
CS            the VT100 control sequences to set/unset reverse video.           
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
CS                 RANGE  - starting and ending block numbers and pointers      
CS                          of print range                                      
CS                 VT100  - flag set .TRUE. if terminal is a VT100              
CS                 LRL    - length of longest record in input file              
CS                 LENLMT - maximum output length (if <0 then WRAP set)         
CS                 PAGEND - line number of last line output to screen.          
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
CS                 LPBUF  - last value of PBUF processed by LINCON              
C                                                                               
CS    STOPCM  Used to pass information back and forth to/from the stop          
CS            scrolling AST.  STOPIT is a flag set .TRUE. if scrolling          
CS            should be stopped, INBUF is the input character, IOFLAG and       
CS            TIMFLG are event flags, and IOSB is the I/O IOSB.                 
C                                                                               
C***   INPUT VARIABLES                                                          
C                                                                               
CI    INBUF   One byte buffer used to get stop scrolling command.               
C                                                                               
C***   OUTPUT VARIABLES                                                         
C                                                                               
CO    BUFFER  Buffer holding input data.  During back scrolling output          
CO            is done directly from here.                                       
CO    ESC     Used to output an ASCII <ESC>.                                    
CO    LINES   Lines of output converted by LINCON.                              
C                                                                               
C***   INPUT UNITS                                                              
C                                                                               
CR    INCHAN  Channel number of the terminal.                                   
C                                                                               
C***   OUTPUT UNITS                                                             
C                                                                               
CW    9       Output to the terminal (no carriage-control).                     
CW    20      Output for the List, Print, and Append commands                   
C                                                                               
      EXTERNAL STOPSC                                                        163
      CHARACTER*132 LINES(600),HLIN(600),CAPLIN                              164
      CHARACTER*33 CHAR                                                      165
      CHARACTER*30 SRCHBF(10)                                                166
      CHARACTER*5 TEMPCH                                                     167
      CHARACTER*4 REVVID,REGVID                                              168
      CHARACTER*2 CRLF                                                       169
      INTEGER*4 TIME,BLK,PBUF,EBK,HNBUF,HPBUF,FSZ,RFM,RAT,TIMFLG,            170
     1          DELTIM(2),SBLK,PAUSTM(2),PRINT,HBLK,HSBLK,                      
     2          RANGE(2,2),CCLINE(2,600),HLBLK,HLPBUF,HCCLIN(2,600),            
     3          PAGEND,OLDEND,SAVCOM,IDESCR(2),STRPOS(10)                       
      INTEGER*2 IOSB(4),LINLEN(600),HLINLN(600),HLINES,HLINEP,DIRECT,        174
     1          SBFLEN(10),RECBLK,LENLMT,PAGLEN,COMAND,COMLIN                   
      LOGICAL*1 SBFFLG,INBUF,BUFFER(3750),FIRST,NUMFLG,MINFLG,               176
     1          HBUFER(150),STOPIT,ESC,TEMP(30),CAPBUF(3750),VT100,             
     2          BELL,BSP,RFLAG,SECOND,SAVFLG(2),VT100A                          
                                                                             179
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      180
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /LNCN/LINLEN,LINEP,NLINES,BUFFER,PBUF,NBUF,BLK,FSZ,IFSZ,        182
     1             RFM,RAT,SBLK,CCLINE,LBLK,LPBUF                               
      COMMON /CHARS/LINES,SRCHBF,CRLF,REVVID,REGVID                          184
      COMMON /STOPCM/STOPIT,INBUF,IOFLAG,IOSB,TIMFLG                         185
                                                                             186
      DATA ESC,BELL,BSP/27,7,8/                                              187
      DATA DELTIM(2)/-1/                                                     188
      DATA PAUSTM/-30000000,-1/                                              189
      DATA OPNFIL/.FALSE./                                                   190
                                                                             191
      ANUMB = TANUMB                                                         192
C                                                                               
C***   Turn LED #1 on                                                           
C                                                                               
      IF (COMAND .EQ. 13)GO TO 50020                                         196
C     EXECUTE (WORKING)                                                      197
      ASSIGN 50030 TO KKK022                                                 197
      GO TO 79802                                                               
50030 CONTINUE                                                                  
50020 CONTINUE                                                               198
C                                                                               
C***   Save command                                                             
C                                                                               
      IF (COMAND .GE. 0)GO TO 50050                                          202
        ICOM = -COMAND                                                       203
      GO TO 50040                                                            204
50050 CONTINUE                                                                  
      IF (COMAND .EQ. 13)GO TO 50070                                         205
          SAVFLG(1) = NUMFLG                                                 206
          SAVFLG(2) = MINFLG                                                 207
          SAVCOM = COMAND                                                    208
          SAVNUM = TANUMB                                                    209
50070 CONTINUE                                                               210
        ICOM = COMAND                                                        211
50040 CONTINUE                                                               212
      GO TO 50080                                                            213
50090 CONTINUE                                                               214
C                                                                               
C***   Line jump                                                                
C                                                                               
        NUMBER = ANUMB*DIRECT                                                218
      IF (NUMFLG)GO TO 50110                                                 219
C     EXECUTE (ERROR_EXIT)                                                   220
      ASSIGN 50120 TO KKK024                                                 220
      GO TO 79804                                                               
50120 CONTINUE                                                                  
50110 CONTINUE                                                               221
C     EXECUTE (HOLD_CURRENT_SETUP)                                           222
      ASSIGN 50130 TO KKK026                                                 222
      GO TO 79806                                                               
50130 CONTINUE                                                                  
C                                                                               
C***   Execute line jumps                                                       
C                                                                               
C     EXECUTE (JUMP_LINES)                                                   226
      ASSIGN 50140 TO KKK028                                                 226
      GO TO 79808                                                               
50140 CONTINUE                                                                  
C     EXECUTE (GET_LAST_LINE)                                                227
      ASSIGN 50150 TO KKK030                                                 227
      GO TO 79810                                                               
50150 CONTINUE                                                                  
C     EXECUTE (CANCEL_STOPPER)                                               228
      ASSIGN 50160 TO KKK032                                                 228
      GO TO 79812                                                               
50160 CONTINUE                                                                  
      IF (IABS(NUMBER) .GT. PAGLEN)GO TO 50180                               229
C     EXECUTE (SCROLL_REFRESH_EXIT)                                          230
      ASSIGN 50190 TO KKK034                                                 230
      GO TO 79814                                                               
50190 CONTINUE                                                                  
      GO TO 50170                                                            231
50180 CONTINUE                                                                  
C     EXECUTE (REFRESH_EXIT)                                                 232
      ASSIGN 50200 TO KKK036                                                 232
      GO TO 79816                                                               
50200 CONTINUE                                                                  
50170 CONTINUE                                                               233
      GO TO 50081                                                            234
50210 CONTINUE                                                                  
C                                                                               
C***   Block jumps                                                              
C                                                                               
      IF (NUMFLG)GO TO 50230                                                 238
C     EXECUTE (ERROR_EXIT)                                                   239
      ASSIGN 50240 TO KKK024                                                 239
      GO TO 79804                                                               
50240 CONTINUE                                                                  
50230 CONTINUE                                                               240
C     EXECUTE (FIND_NON_CC_LINE)                                             241
      ASSIGN 50250 TO KKK038                                                 241
      GO TO 79818                                                               
50250 CONTINUE                                                                  
        BLK = CCLINE(1,I) + ANUMB*DIRECT                                     242
        IF (BLK .LE. 0) BLK = 1                                              243
        IF (BLK .GT. EBK) BLK = EBK                                          244
        IBLK = BLK                                                           245
C     EXECUTE (INPUT_SEVEN_BLOCKS)                                           246
      ASSIGN 50260 TO KKK040                                                 246
      GO TO 79820                                                               
50260 CONTINUE                                                                  
        CALL LINCON(.TRUE.)                                                  247
C                                                                               
C***   If input was at end of file, must find line position                     
C                                                                               
      IF (BLK .LE. EBK)GO TO 50280                                           251
      DO 50290  LINEP=1,NLINES                                               252
      IF (CCLINE(1,LINEP) .EQ. IBLK)GO TO 50291                              253
50290 CONTINUE                                                               254
50291 CONTINUE                                                                  
      IF (NLINES .GE. (LINEP + PAGLEN - 1))GO TO 50310                       255
            LINEP = NLINES - PAGLEN + 1                                      256
            IF (LINEP .LE. 0) LINEP = 1                                      257
50310 CONTINUE                                                               258
50280 CONTINUE                                                               259
C     EXECUTE (GET_LAST_LINE)                                                260
      ASSIGN 50320 TO KKK030                                                 260
      GO TO 79810                                                               
50320 CONTINUE                                                                  
C     EXECUTE (REFRESH_EXIT)                                                 261
      ASSIGN 50330 TO KKK036                                                 261
      GO TO 79816                                                               
50330 CONTINUE                                                                  
      GO TO 50081                                                            262
50340 CONTINUE                                                                  
C                                                                               
C***   Searches                                                                 
C                                                                               
      IF (SBFFLG)GO TO 50360                                                 266
C     EXECUTE (ERROR_EXIT)                                                   267
      ASSIGN 50370 TO KKK024                                                 267
      GO TO 79804                                                               
50370 CONTINUE                                                                  
50360 CONTINUE                                                               268
C                                                                               
C***   Loop ANUMB times                                                         
C                                                                               
      IF (.NOT.(NUMFLG))GO TO 50390                                          272
          NUMBER = ANUMB                                                     273
      GO TO 50380                                                            274
50390 CONTINUE                                                                  
          NUMBER = 1                                                         275
50380 CONTINUE                                                               276
      DO 50400  III=1,NUMBER                                                 277
      IF (.NOT.(MINFLG))GO TO 50420                                          278
      IF (DIRECT .NE. -1)GO TO 50440                                         279
C     EXECUTE (FORWARD_SEARCH)                                               280
      ASSIGN 50450 TO KKK042                                                 280
      GO TO 79822                                                               
50450 CONTINUE                                                                  
      GO TO 50430                                                            281
50440 CONTINUE                                                                  
C     EXECUTE (BACKWARD_SEARCH)                                              282
      ASSIGN 50460 TO KKK044                                                 282
      GO TO 79824                                                               
50460 CONTINUE                                                                  
50430 CONTINUE                                                               283
      GO TO 50410                                                            284
50420 CONTINUE                                                                  
      IF (DIRECT .NE. -1)GO TO 50480                                         285
C     EXECUTE (BACKWARD_SEARCH)                                              286
      ASSIGN 50490 TO KKK044                                                 286
      GO TO 79824                                                               
50490 CONTINUE                                                                  
      GO TO 50470                                                            287
50480 CONTINUE                                                                  
C     EXECUTE (FORWARD_SEARCH)                                               288
      ASSIGN 50500 TO KKK042                                                 288
      GO TO 79822                                                               
50500 CONTINUE                                                                  
50470 CONTINUE                                                               289
50410 CONTINUE                                                               290
50400 CONTINUE                                                               291
      GO TO 50081                                                            292
50510 CONTINUE                                                                  
C                                                                               
C***   Reversing operation                                                      
C                                                                               
        IF (MINFLG) DIRECT = -DIRECT                                         296
      IF (.NOT.(NUMFLG))GO TO 50530                                          297
          NUMBER = ANUMB                                                     298
          DIRECT = -1**NUMBER*DIRECT                                         299
      GO TO 50520                                                            300
50530 CONTINUE                                                                  
          DIRECT = -DIRECT                                                   301
50520 CONTINUE                                                               302
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                          303
      ASSIGN 50540 TO KKK046                                                 303
      GO TO 79826                                                               
50540 CONTINUE                                                                  
      GO TO 50081                                                            304
50550 CONTINUE                                                                  
C                                                                               
C***   Scrolling                                                                
C                                                                               
        CALL WORKNG(.FALSE.)                                                 308
        IF (NUMFLG) TIME = (-10000000./ANUMB)*DIRECT                         309
        CALL LIB$GET_EF(TIMFLG)                                              310
      IF (TIMFLG .NE. -1)GO TO 50570                                         311
          CALL LIB$PUT_SCREEN('Unable to allocate necessary event '//        312
     1            'flags.  SCROLLING ABORTED.',COMLIN,20)                       
C     EXECUTE (ERROR_EXIT)                                                   314
      ASSIGN 50580 TO KKK024                                                 314
      GO TO 79804                                                               
50580 CONTINUE                                                                  
50570 CONTINUE                                                               315
C     EXECUTE (QUEUE_STOPPER)                                                316
      ASSIGN 50590 TO KKK048                                                 316
      GO TO 79828                                                               
50590 CONTINUE                                                                  
        DELTIM(1) = TIME*DIRECT                                              317
      IF (.NOT.((DELTIM(1) .LT. 0 .AND. .NOT.MINFLG) .OR. (DELTIM(1) .GT     318
     X. 0       .AND. MINFLG)))GO TO 50610                                      
C     EXECUTE (FORWARD_SCROLLING)                                            320
      ASSIGN 50620 TO KKK050                                                 320
      GO TO 79830                                                               
50620 CONTINUE                                                                  
      GO TO 50600                                                            321
50610 CONTINUE                                                                  
C     EXECUTE (BACKWARD_SCROLLING)                                           322
      ASSIGN 50630 TO KKK052                                                 322
      GO TO 79832                                                               
50630 CONTINUE                                                                  
50600 CONTINUE                                                               323
      GO TO 50081                                                            324
50640 CONTINUE                                                                  
C                                                                               
C***   Set scrolling speed                                                      
C                                                                               
      IF (NUMFLG)GO TO 50660                                                 328
C     EXECUTE (ERROR_EXIT)                                                   329
      ASSIGN 50670 TO KKK024                                                 329
      GO TO 79804                                                               
50670 CONTINUE                                                                  
50660 CONTINUE                                                               330
        TIME = (-10000000./ANUMB)*DIRECT                                     331
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                          332
      ASSIGN 50680 TO KKK046                                                 332
      GO TO 79826                                                               
50680 CONTINUE                                                                  
      GO TO 50081                                                            333
50690 CONTINUE                                                                  
C                                                                               
C***   Mark command                                                             
C                                                                               
      IF (.NOT.(NUMFLG))GO TO 50710                                          337
          NUMBER = ANUMB*DIRECT                                              338
      GO TO 50700                                                            339
50710 CONTINUE                                                                  
          NUMBER = 0                                                         340
50700 CONTINUE                                                               341
C     EXECUTE (HOLD_CURRENT_SETUP)                                           342
      ASSIGN 50720 TO KKK026                                                 342
      GO TO 79806                                                               
50720 CONTINUE                                                                  
C                                                                               
C***   Point to line we want                                                    
C                                                                               
        JNUMB = 0                                                            346
C     EXECUTE (JUMP_LINES)                                                   347
      ASSIGN 50730 TO KKK028                                                 347
      GO TO 79808                                                               
50730 CONTINUE                                                                  
        LINEP = LINEP + JNUMB                                                348
        IF (LINEP .GT. NLINES) LINEP = NLINES                                349
      DO 50740  I=LINEP,NLINES                                               350
      IF (CCLINE(1,I) .EQ. 0)GO TO 50760                                     351
      IF (RANGE(1,1) .EQ. 0)GO TO 50780                                      352
             RANGE(1,2) = RANGE(1,1)                                         353
             RANGE(2,2) = RANGE(2,1)                                         354
50780 CONTINUE                                                               355
           RANGE(1,1) = CCLINE(1,I)                                          356
           RANGE(2,1) = CCLINE(2,I)                                          357
      GO TO 50741                                                            358
50760 CONTINUE                                                               359
50740 CONTINUE                                                               360
50741 CONTINUE                                                                  
C     EXECUTE (CANCEL_STOPPER)                                               361
      ASSIGN 50790 TO KKK032                                                 361
      GO TO 79812                                                               
50790 CONTINUE                                                                  
C     EXECUTE (RESTORE_HELD_SETUP)                                           362
      ASSIGN 50800 TO KKK054                                                 362
      GO TO 79834                                                               
50800 CONTINUE                                                                  
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                          363
      ASSIGN 50810 TO KKK046                                                 363
      GO TO 79826                                                               
50810 CONTINUE                                                                  
      GO TO 50081                                                            364
50820 CONTINUE                                                                  
C                                                                               
C***   Print, List, and Append commands                                         
C                                                                               
        PRINT = 1                                                            368
C     EXECUTE (OUTPUT_RANGE)                                                 369
      ASSIGN 50830 TO KKK056                                                 369
      GO TO 79836                                                               
50830 CONTINUE                                                                  
      GO TO 50081                                                            370
50840 CONTINUE                                                                  
      IF (.NOT.(MINFLG))GO TO 50860                                          371
          PRINT = 0                                                          372
      GO TO 50850                                                            373
50860 CONTINUE                                                                  
          PRINT = -1                                                         374
50850 CONTINUE                                                               375
C     EXECUTE (OUTPUT_RANGE)                                                 376
      ASSIGN 50870 TO KKK056                                                 376
      GO TO 79836                                                               
50870 CONTINUE                                                                  
      GO TO 50081                                                            377
50880 CONTINUE                                                                  
C                                                                               
C***   Exit                                                                     
C                                                                               
        CALL EXIT                                                            381
      GO TO 50081                                                            382
50890 CONTINUE                                                                  
C                                                                               
C***   Re-write screen                                                          
C                                                                               
C     EXECUTE (REFRESH_EXIT)                                                 386
      ASSIGN 50900 TO KKK036                                                 386
      GO TO 79816                                                               
50900 CONTINUE                                                                  
C                                                                               
C***   Repeat last command                                                      
C                                                                               
      GO TO 50081                                                            390
50910 CONTINUE                                                                  
      IF (.NOT.(NUMFLG))GO TO 50930                                          391
          III = ANUMB                                                        392
      GO TO 50920                                                            393
50930 CONTINUE                                                                  
          III = 1                                                            394
50920 CONTINUE                                                               395
      DO 50940  IIII=1,III                                                   396
         CALL REDOIT(SAVCOM,SAVFLG(1),SAVNUM,SAVFLG(2))                      397
50940 CONTINUE                                                               398
      GO TO 50081                                                            399
50080 IF (ICOM.LT.1)GO TO 50081                                                 
      IF (ICOM.GT.13)GO TO 50081                                                
      GO TO (50090,50210,50340,50510,50550,50081,50640,50690,50820,50840        
     X,50880,50890,50910),ICOM                                                  
50081 CONTINUE                                                                  
C     EXECUTE (CANCEL_STOPPER)                                               400
      ASSIGN 50950 TO KKK032                                                 400
      GO TO 79812                                                               
50950 CONTINUE                                                                  
      CALL WORKNG(.FALSE.)                                                   401
      RETURN                                                                 402
C                                                                               
C****************************************************************************** 
C***********************   HERE FOLLOW THE PROCEDURES   *********************** 
C****************************************************************************** 
C                                                                               
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (BACK_SCROLL_EXIT)                                           408
79838 CONTINUE                                                               408
CP    This routine exits from scrolling backward in FIX files.  It converts     
CP    the data at the current position into output lines, frees the event       
CP    event flags, resets the LEDs (#3 off) and returns.                        
        II = PBUF                                                            412
        SBLK = BLK - 1                                                       413
      IF (BLK .GT. EBK)GO TO 50970                                           414
          CALL BLKIN(BLK,I,BUFFER(513))                                      415
      IF (I .NE. 0)GO TO 50990                                               416
C     EXECUTE (READ_ERROR)                                                   417
      ASSIGN 51000 TO KKK060                                                 417
      GO TO 79840                                                               
51000 CONTINUE                                                                  
50990 CONTINUE                                                               418
          NBUF = 512 + I                                                     419
50970 CONTINUE                                                               420
        CALL LINCON(.TRUE.)                                                  421
       LINEP=1                                                               422
      GO TO 51009                                                               
51010  LINEP= LINEP+(1)                                                         
      IF ( LINEP.GT.NLINES)GO TO 51011                                          
51009 CONTINUE                                                                  
      IF (CCLINE(1,LINEP) .EQ. SBLK .AND. CCLINE(2,LINEP).EQ.II)GO TO 51     423
     X011                                                                       
      GO TO 51010                                                            424
51011 CONTINUE                                                                  
C     EXECUTE (GET_LAST_LINE)                                                425
      ASSIGN 51020 TO KKK030                                                 425
      GO TO 79810                                                               
51020 CONTINUE                                                                  
C     EXECUTE (WRITE_BLOCK_NUMBER)                                           426
      ASSIGN 51030 TO KKK062                                                 426
      GO TO 79842                                                               
51030 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                              427
      ASSIGN 51040 TO KKK064                                                 427
      GO TO 79844                                                               
51040 CONTINUE                                                                  
        RETURN                                                               428
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (BACKWARD_SEARCH)                                            430
79824 CONTINUE                                                               430
CP    This procedure searches backward through the file for a string. IF        
CP    found the position is located there, the screen refreshed and the         
CP    LEDs reset (#3 off), otherwise the screen is left alone, the position     
CP    is not changed, and the LEDs are reset (#3 on).  In either case the       
CP    a return from DOIT is executed.                                           
C                                                                               
C***   First search lines in buffer                                             
C                                                                               
 1650 CONTINUE                                                               439
      IF (LINEP .EQ. 1)GO TO 51070                                           440
       I=LINEP-1                                                             441
      GO TO 51079                                                               
51080  I= I+(-1)                                                                
      IF ( I.LT.1)GO TO 51081                                                   
51079 CONTINUE                                                                  
            CALL CONVUP(LINLEN(I),%REF(LINES(I)),%REF(CAPLIN))               442
       JJ=1                                                                  443
      GO TO 51089                                                               
51090  JJ= JJ+(1)                                                               
      IF ( JJ.GT.10)GO TO 51091                                                 
51089 CONTINUE                                                                  
      IF (SBFLEN(JJ) .EQ. 0)GO TO 51091                                      444
      IF (INDEX(CAPLIN(:LINLEN(I)),SRCHBF(JJ)(:SBFLEN(JJ)))                  445
     X    .EQ. 0)GO TO 51110                                                    
               NUMBER = I - LINEP                                            447
               LINEP = I                                                     448
C     EXECUTE (GET_LAST_LINE)                                                449
      ASSIGN 51120 TO KKK030                                                 449
      GO TO 79810                                                               
51120 CONTINUE                                                                  
      IF (IABS(NUMBER) .LE. PAGLEN)GO TO 51140                               450
C     EXECUTE (REFRESH_EXIT)                                                 451
      ASSIGN 51150 TO KKK036                                                 451
      GO TO 79816                                                               
51150 CONTINUE                                                                  
      GO TO 51130                                                            452
51140 CONTINUE                                                                  
C     EXECUTE (SCROLL_REFRESH_EXIT)                                          453
      ASSIGN 51160 TO KKK034                                                 453
      GO TO 79814                                                               
51160 CONTINUE                                                                  
51130 CONTINUE                                                               454
      GO TO 51051                                                            455
51110 CONTINUE                                                               456
      GO TO 51090                                                            457
51091 CONTINUE                                                                  
      GO TO 51080                                                            458
51081 CONTINUE                                                                  
51070 CONTINUE                                                               459
C                                                                               
C***   Then start searching backward if not found                               
C                                                                               
C     EXECUTE (HOLD_CURRENT_SETUP)                                           463
      ASSIGN 51170 TO KKK026                                                 463
      GO TO 79806                                                               
51170 CONTINUE                                                                  
         IBLK = SBLK - 1                                                     464
      IF (IBLK .GT. 0)GO TO 51190                                            465
C     EXECUTE (SEARCH_ERROR_EXIT)                                            466
      ASSIGN 51200 TO KKK066                                                 466
      GO TO 79846                                                               
51200 CONTINUE                                                                  
51190 CONTINUE                                                               467
         CALL BLKIN(IBLK,NBUF,BUFFER)                                        468
      IF (NBUF .NE. 0)GO TO 51220                                            469
C     EXECUTE (READ_ERROR)                                                   470
      ASSIGN 51230 TO KKK060                                                 470
      GO TO 79840                                                               
51230 CONTINUE                                                                  
51220 CONTINUE                                                               471
         CALL BLKIN(IBLK,I,BUFFER(NBUF+1))                                   472
      IF (I .NE. 0)GO TO 51250                                               473
C     EXECUTE (READ_ERROR)                                                   474
      ASSIGN 51260 TO KKK060                                                 474
      GO TO 79840                                                               
51260 CONTINUE                                                                  
51250 CONTINUE                                                               475
      IF (I .LE. 140)GO TO 51280                                             476
           NBUF = NBUF + 140                                                 477
      GO TO 51270                                                            478
51280 CONTINUE                                                                  
           NBUF = NBUF + I                                                   479
51270 CONTINUE                                                               480
         IBLK = SBLK                                                         481
C                                                                               
C===   Search loop                                                              
C                                                                               
51289 CONTINUE                                                               485
          CALL CONVUP(NBUF,BUFFER(1),CAPBUF(1))                              486
       JJ=1                                                                  487
      GO TO 51299                                                               
51300  JJ= JJ+(1)                                                               
      IF ( JJ.GT.10)GO TO 51301                                                 
51299 CONTINUE                                                                  
      IF (SBFLEN(JJ) .EQ. 0)GO TO 51301                                      488
           IDESCR(1) = NBUF                                                  489
           IDESCR(2) = %LOC(CAPBUF(1))                                       490
           I = 1                                                             491
51309 CONTINUE                                                               492
            K = LIB$INDEX(IDESCR,SRCHBF(JJ)(:SBFLEN(JJ)))                    493
      IF (.NOT.(STOPIT))GO TO 51330                                          494
C     EXECUTE (CANCEL_COMMAND)                                               495
      ASSIGN 51340 TO KKK068                                                 495
      GO TO 79848                                                               
51340 CONTINUE                                                                  
51330 CONTINUE                                                               496
      IF (K .NE. 0)GO TO 51360                                               497
              STRPOS(JJ) = I - 1                                             498
      GO TO 51311                                                            499
51360 CONTINUE                                                               500
            IDESCR(1) = IDESCR(1) - K                                        501
            I = I + K                                                        502
            IDESCR(2) = %LOC(CAPBUF(I))                                      503
      GO TO 51309                                                            504
51311 CONTINUE                                                                  
      GO TO 51300                                                            505
51301 CONTINUE                                                                  
          I = 0                                                              506
       K=1                                                                   507
      GO TO 51369                                                               
51370  K= K+(1)                                                                 
      IF ( K.GT.10)GO TO 51371                                                  
51369 CONTINUE                                                                  
      IF (SBFLEN(K) .EQ. 0)GO TO 51371                                       508
      IF (STRPOS(K) .LE. I)GO TO 51390                                       509
             JJ = K                                                          510
             I = STRPOS(K)                                                   511
51390 CONTINUE                                                               512
      GO TO 51370                                                            513
51371 CONTINUE                                                                  
C                                                                               
C===   Found                                                                    
C                                                                               
      IF (I .EQ. 0)GO TO 51410                                               517
            BLK = IBLK - 2                                                   518
            IF (BLK .LE. 0) BLK = 1                                          519
            SBLK = BLK                                                       520
            CALL BLKIN(BLK,NBUF,BUFFER)                                      521
      IF (NBUF .NE. 0)GO TO 51430                                            522
C     EXECUTE (READ_ERROR)                                                   523
      ASSIGN 51440 TO KKK060                                                 523
      GO TO 79840                                                               
51440 CONTINUE                                                                  
51430 CONTINUE                                                               524
      IF (BLK .GT. EBK)GO TO 51460                                           525
              CALL BLKIN(BLK,I,BUFFER(NBUF+1))                               526
      IF (I .NE. 0)GO TO 51480                                               527
C     EXECUTE (READ_ERROR)                                                   528
      ASSIGN 51490 TO KKK060                                                 528
      GO TO 79840                                                               
51490 CONTINUE                                                                  
51480 CONTINUE                                                               529
              NBUF = NBUF + I                                                530
      IF (BLK .GT. EBK)GO TO 51510                                           531
                CALL BLKIN(BLK,I,BUFFER(NBUF+1))                             532
      IF (I .NE. 0)GO TO 51530                                               533
C     EXECUTE (READ_ERROR)                                                   534
      ASSIGN 51540 TO KKK060                                                 534
      GO TO 79840                                                               
51540 CONTINUE                                                                  
51530 CONTINUE                                                               535
                NBUF = NBUF + I                                              536
51510 CONTINUE                                                               537
51460 CONTINUE                                                               538
            CALL LINCON(.TRUE.)                                              539
C                                                                               
C===   Find right line                                                          
C                                                                               
       J=NLINES                                                              543
      GO TO 51549                                                               
51550  J= J+(-1)                                                                
      IF ( J.LT.1)GO TO 51551                                                   
51549 CONTINUE                                                                  
             CALL CONVUP(LINLEN(J),%REF(LINES(J)),%REF(CAPLIN))              544
      IF (INDEX(CAPLIN(:LINLEN(J)),SRCHBF(JJ)(:SBFLEN(JJ)))                  545
     X    .EQ. 0)GO TO 51570                                                    
               LINEP = J                                                     547
C     EXECUTE (GET_LAST_LINE)                                                548
      ASSIGN 51580 TO KKK030                                                 548
      GO TO 79810                                                               
51580 CONTINUE                                                                  
C     EXECUTE (CANCEL_STOPPER)                                               549
      ASSIGN 51590 TO KKK032                                                 549
      GO TO 79812                                                               
51590 CONTINUE                                                                  
C     EXECUTE (REFRESH_EXIT)                                                 550
      ASSIGN 51600 TO KKK036                                                 550
      GO TO 79816                                                               
51600 CONTINUE                                                                  
      GO TO 51051                                                            551
51570 CONTINUE                                                               552
      GO TO 51550                                                            553
51551 CONTINUE                                                                  
C                                                                               
C===   Not found                                                                
C                                                                               
51410 CONTINUE                                                               557
          IBLK = IBLK - 2                                                    558
      IF (IBLK .GT. 0)GO TO 51620                                            559
C     EXECUTE (SEARCH_ERROR_EXIT)                                            560
      ASSIGN 51630 TO KKK066                                                 560
      GO TO 79846                                                               
51630 CONTINUE                                                                  
51620 CONTINUE                                                               561
       I=1                                                                   562
      GO TO 51639                                                               
51640  I= I+(1)                                                                 
      IF ( I.GT.30)GO TO 51641                                                  
51639 CONTINUE                                                                  
           TEMP(I) = CAPBUF(I)                                               563
      GO TO 51640                                                            564
51641 CONTINUE                                                                  
          CALL BLKIN(IBLK,NBUF,BUFFER)                                       565
      IF (NBUF .NE. 0)GO TO 51660                                            566
C     EXECUTE (READ_ERROR)                                                   567
      ASSIGN 51670 TO KKK060                                                 567
      GO TO 79840                                                               
51670 CONTINUE                                                                  
51660 CONTINUE                                                               568
       I=1                                                                   569
      GO TO 51679                                                               
51680  I= I+(1)                                                                 
      IF ( I.GT.30)GO TO 51681                                                  
51679 CONTINUE                                                                  
           BUFFER(NBUF+I) = TEMP(I)                                          570
      GO TO 51680                                                            571
51681 CONTINUE                                                                  
          NBUF = NBUF + 30                                                   572
      IF (.NOT.(STOPIT))GO TO 51700                                          573
C     EXECUTE (CANCEL_COMMAND)                                               574
      ASSIGN 51710 TO KKK068                                                 574
      GO TO 79848                                                               
51710 CONTINUE                                                                  
51700 CONTINUE                                                               575
      GO TO 51289                                                            576
51051 CONTINUE                                                               577
      GO TO KKK044                                                           578
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (BACKWARD_SCROLLING)                                         579
79832 CONTINUE                                                               579
CP    This procedure handles scrolling backward through the file.               
        IF (.NOT.MINFLG) DELTIM(1) = -DELTIM(1)                              581
C                                                                               
C***   Scroll through line buffer                                               
C                                                                               
      IF (LINEP .EQ. 1)GO TO 51730                                           585
       LINEP=LINEP-1                                                         586
      GO TO 51739                                                               
51740  LINEP= LINEP+(-1)                                                        
      IF ( LINEP.LT.1)GO TO 51741                                               
51739 CONTINUE                                                                  
      IF (LENLMT .GE. 0)GO TO 51760                                          587
             K = -LENLMT                                                     588
       L=1                                                                   589
      GO TO 51769                                                               
51770  L= L+(1)                                                                 
      IF ( L.GT.LINLEN(LINEP)/K+1)GO TO 51771                                   
51769 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  590
      ASSIGN 51780 TO KKK070                                                 590
      GO TO 79850                                                               
51780 CONTINUE                                                                  
      GO TO 51770                                                            591
51771 CONTINUE                                                                  
      IF (K .GE. LINLEN(LINEP))GO TO 51800                                   592
               CALL LIB$PUT_SCREEN(LINES(LINEP)(:K)//CRLF//                  593
     1              LINES(LINEP)(K+1:LINLEN(LINEP))//CRLF,1,1)                  
      GO TO 51790                                                            595
51800 CONTINUE                                                                  
               CALL LIB$PUT_SCREEN(LINES(LINEP)(:LINLEN(LINEP))//CRLF,       596
     1                             1,1)                                         
51790 CONTINUE                                                               598
      GO TO 51750                                                            599
51760 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  600
      ASSIGN 51810 TO KKK070                                                 600
      GO TO 79850                                                               
51810 CONTINUE                                                                  
      IF (LINLEN(LINEP) .LE. LENLMT)GO TO 51830                              601
               CALL LIB$PUT_SCREEN(LINES(LINEP)(:LENLMT)//CRLF,1,1)          602
      GO TO 51820                                                            603
51830 CONTINUE                                                                  
               CALL LIB$PUT_SCREEN(LINES(LINEP)(:LINLEN(LINEP))//CRLF,       604
     1                             1,1)                                         
51820 CONTINUE                                                               606
51750 CONTINUE                                                               607
C     EXECUTE (WAIT_FOR_SCROLL_AST)                                          608
      ASSIGN 51840 TO KKK072                                                 608
      GO TO 79852                                                               
51840 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 51860                                          609
C     EXECUTE (GET_LAST_LINE)                                                610
      ASSIGN 51870 TO KKK030                                                 610
      GO TO 79810                                                               
51870 CONTINUE                                                                  
C     EXECUTE (WRITE_BLOCK_NUMBER)                                           611
      ASSIGN 51880 TO KKK062                                                 611
      GO TO 79842                                                               
51880 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                              612
      ASSIGN 51890 TO KKK064                                                 612
      GO TO 79844                                                               
51890 CONTINUE                                                                  
             RETURN                                                          613
51860 CONTINUE                                                               614
      GO TO 51740                                                            615
51741 CONTINUE                                                                  
51730 CONTINUE                                                               616
C                                                                               
C***   Scroll through file                                                      
C                                                                               
       I=1                                                                   620
      GO TO 51899                                                               
51900  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 51901                                              
51899 CONTINUE                                                                  
      IF (CCLINE(1,I) .NE. 0)GO TO 51901                                     621
      GO TO 51900                                                            622
51901 CONTINUE                                                                  
        BLK = CCLINE(1,I) - 1                                                623
      IF (BLK .LE. 0)GO TO 51920                                             624
          CALL BLKIN(BLK,PBUF,BUFFER)                                        625
      IF (PBUF .NE. 0)GO TO 51940                                            626
C     EXECUTE (READ_ERROR)                                                   627
      ASSIGN 51950 TO KKK060                                                 627
      GO TO 79840                                                               
51950 CONTINUE                                                                  
51940 CONTINUE                                                               628
      GO TO 51910                                                            629
51920 CONTINUE                                                                  
          PBUF = 0                                                           630
          BLK = 1                                                            631
51910 CONTINUE                                                               632
        CALL BLKIN(BLK,NBUF,BUFFER(PBUF+1))                                  633
      IF (NBUF .NE. 0)GO TO 51970                                            634
C     EXECUTE (READ_ERROR)                                                   635
      ASSIGN 51980 TO KKK060                                                 635
      GO TO 79840                                                               
51980 CONTINUE                                                                  
51970 CONTINUE                                                               636
        IF (PBUF .NE. 0) BLK = BLK - 1                                       637
        PBUF = PBUF + CCLINE(2,I)                                            638
        NBUF = PBUF                                                          639
        ISAVE = PBUF                                                         640
51989 CONTINUE                                                               641
      IF (RFM .NE. 1)GO TO 52010                                             642
C                                                                               
C---   Fixed length records                                                     
C                                                                               
C===   Establish output record length                                           
C                                                                               
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 52030                     648
      IF (.NOT.((FSZ - 1) .LT. LENLMT .OR. LENLMT .LT. 0))GO TO 52050        649
               LENREC = FSZ - 1                                              650
      GO TO 52040                                                            651
52050 CONTINUE                                                                  
               LENREC = LENLMT                                               652
52040 CONTINUE                                                               653
      GO TO 52020                                                            654
52030 CONTINUE                                                                  
      IF (.NOT.(FSZ .LT. LENLMT .OR. LENLMT .LT. 0))GO TO 52070              655
               LENREC = FSZ                                                  656
      GO TO 52060                                                            657
52070 CONTINUE                                                                  
               LENREC = LENLMT                                               658
52060 CONTINUE                                                               659
52020 CONTINUE                                                               660
52079 CONTINUE                                                               661
C     EXECUTE (WAIT_FOR_SCROLL)                                              662
      ASSIGN 52090 TO KKK074                                                 662
      GO TO 79854                                                               
52090 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 52110                                          663
C     EXECUTE (BACK_SCROLL_EXIT)                                             664
      ASSIGN 52120 TO KKK058                                                 664
      GO TO 79838                                                               
52120 CONTINUE                                                                  
52110 CONTINUE                                                               665
            PBUF = PBUF - IFSZ                                               666
      IF (PBUF .LE. 0)GO TO 52081                                            667
C     EXECUTE (SCROLL_DOWN)                                                  668
      ASSIGN 52130 TO KKK070                                                 668
      GO TO 79850                                                               
52130 CONTINUE                                                                  
      IF (.NOT.(LENLMT .LT. 0 .AND. RECLEN .GT. (-LENLMT)))GO TO 52150       669
       K=1                                                                   670
      GO TO 52159                                                               
52160  K= K+(1)                                                                 
      IF ( K.GT.(-LENLMT)/RECLEN)GO TO 52161                                    
52159 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  671
      ASSIGN 52170 TO KKK070                                                 671
      GO TO 79850                                                               
52170 CONTINUE                                                                  
      GO TO 52160                                                            672
52161 CONTINUE                                                                  
52150 CONTINUE                                                               673
      IF (.NOT.(RAT .NE. 2 .AND. RAT .NE. -2))GO TO 52190                    674
              CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,BUFFER(PBUF),       675
     1                      %VAL(LENREC),,,,)                                   
              CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),          677
     1                     IOSB,STOPSC,,INBUF,%VAL(1),,,,)                      
      GO TO 52180                                                            679
52190 CONTINUE                                                                  
              CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,                    680
     1                      BUFFER(PBUF+1),%VAL(LENREC),,,,)                    
              CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),          682
     1                     IOSB,STOPSC,,INBUF,%VAL(1),,,,)                      
      IF (BUFFER(PBUF) .NE. '1')GO TO 52210                                  684
C     EXECUTE (SCROLL_BLANK_LINE_PRE)                                        685
      ASSIGN 52220 TO KKK076                                                 685
      GO TO 79856                                                               
52220 CONTINUE                                                                  
C     EXECUTE (SCROLL_BLANK_LINE_PRE)                                        686
      ASSIGN 52230 TO KKK076                                                 686
      GO TO 79856                                                               
52230 CONTINUE                                                                  
      GO TO 52200                                                            687
52210 CONTINUE                                                                  
      IF (BUFFER(PBUF) .NE. '0')GO TO 52250                                  688
C     EXECUTE (SCROLL_BLANK_LINE_PRE)                                        689
      ASSIGN 52260 TO KKK076                                                 689
      GO TO 79856                                                               
52260 CONTINUE                                                                  
52250 CONTINUE                                                               690
52200 CONTINUE                                                               691
52180 CONTINUE                                                               692
      GO TO 52079                                                            693
52081 CONTINUE                                                                  
      GO TO 52000                                                            694
52010 CONTINUE                                                                  
C                                                                               
C---   Variable length records                                                  
C                                                                               
       I=PBUF                                                                698
      GO TO 52269                                                               
52270  I= I+(-1)                                                                
      IF ( I.LT.2)GO TO 52271                                                   
52269 CONTINUE                                                                  
      IF (I)GO TO 52290                                                      699
      IF (BUFFER(I) .NE. 0)GO TO 52310                                       700
      IF (I .GT. ISAVE)GO TO 52270                                           701
                LEN = LIB$EXTZV(0,8,BUFFER(I-1))                             702
      IF ((LEN - FSZ) .NE. 0)GO TO 52330                                     703
C     EXECUTE (SCROLL_BLANK_LINE_POST)                                       704
      ASSIGN 52340 TO KKK078                                                 704
      GO TO 79858                                                               
52340 CONTINUE                                                                  
                  ISAVE = I - 4 - FSZ                                        705
      GO TO 52270                                                            706
52330 CONTINUE                                                               707
      IF (.NOT.(LEN))GO TO 52360                                             708
                  ILEN = LEN + 1                                             709
      GO TO 52350                                                            710
52360 CONTINUE                                                                  
                  ILEN = LEN                                                 711
52350 CONTINUE                                                               712
      IF (.NOT.(BUFFER(I+ILEN+2) .EQ. 0 .AND. LEN .LE. LRL .AND.             713
     X          LIB$EXTZV(0,8,BUFFER(I+ILEN+1)) .LE. LRL))GO TO 52380           
                  ISAVE = I - 4 - FSZ                                        715
      IF (.NOT.(RAT .EQ. 3 .OR. RAT .EQ. -3))GO TO 52400                     716
      IF (LIB$EXTZV(7,1,BUFFER(I+2)) .NE. 0)GO TO 52420                      717
                      J = LIB$EXTZV(0,7,BUFFER(I+2)) - 1                     718
      IF (J .LE. 0)GO TO 52440                                               719
       K=1                                                                   720
      GO TO 52449                                                               
52450  K= K+(1)                                                                 
      IF ( K.GT.J)GO TO 52451                                                   
52449 CONTINUE                                                                  
C     EXECUTE (SCROLL_BLANK_LINE_POST)                                       721
      ASSIGN 52460 TO KKK078                                                 721
      GO TO 79858                                                               
52460 CONTINUE                                                                  
      GO TO 52450                                                            722
52451 CONTINUE                                                                  
52440 CONTINUE                                                               723
52420 CONTINUE                                                               724
C     EXECUTE (SCROLL_DOWN)                                                  725
      ASSIGN 52470 TO KKK070                                                 725
      GO TO 79850                                                               
52470 CONTINUE                                                                  
                    CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,              726
     1                       BUFFER(I+FSZ+1),%VAL(LEN-FSZ),,,,)                 
                    CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),                  728
     1                           %VAL('1171'X),IOSB,STOPSC,,INBUF,              
     2                           %VAL(1),,,,)                                   
      IF (LIB$EXTZV(7,1,BUFFER(I+1)) .NE. 0)GO TO 52490                      731
                      J = LIB$EXTZV(0,7,BUFFER(I+1))                         732
      IF (J .EQ. 0)GO TO 52510                                               733
       K=1                                                                   734
      GO TO 52519                                                               
52520  K= K+(1)                                                                 
      IF ( K.GT.J)GO TO 52521                                                   
52519 CONTINUE                                                                  
C     EXECUTE (SCROLL_BLANK_LINE_VAR_PRE)                                    735
      ASSIGN 52530 TO KKK080                                                 735
      GO TO 79860                                                               
52530 CONTINUE                                                                  
      GO TO 52520                                                            736
52521 CONTINUE                                                                  
52510 CONTINUE                                                               737
52490 CONTINUE                                                               738
      GO TO 52390                                                            739
52400 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  740
      ASSIGN 52540 TO KKK070                                                 740
      GO TO 79850                                                               
52540 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 52560                     741
      IF (LEN .EQ. 1)GO TO 52580                                             742
      IF (LENLMT .GE. 0)GO TO 52600                                          743
      IF (LENLMT .LE. (1 + FSZ - LEN))GO TO 52620                            744
       M=1                                                                   745
      GO TO 52629                                                               
52630  M= M+(1)                                                                 
      IF ( M.GT.(FSZ-LEN)/LENLMT)GO TO 52631                                    
52629 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  746
      ASSIGN 52640 TO KKK070                                                 746
      GO TO 79850                                                               
52640 CONTINUE                                                                  
      GO TO 52630                                                            747
52631 CONTINUE                                                                  
                            M = LEN - FSZ - 1                                748
      GO TO 52610                                                            749
52620 CONTINUE                                                                  
                            M = -LENLMT                                      750
52610 CONTINUE                                                               751
      GO TO 52590                                                            752
52600 CONTINUE                                                                  
      IF (LENLMT .LE. (LEN - FSZ - 1))GO TO 52660                            753
                            M = LEN - FSZ - 1                                754
      GO TO 52650                                                            755
52660 CONTINUE                                                                  
                            M = LENLMT                                       756
52650 CONTINUE                                                               757
52590 CONTINUE                                                               758
                        CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,           759
     1                                ,BUFFER(I+FSZ+2),%VAL(M),,,,)             
52580 CONTINUE                                                               761
                      CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),                762
     1                             %VAL('1171'X),IOSB,STOPSC,,                  
     2                             INBUF,%VAL(1),,,,)                           
      IF (BUFFER(I+FSZ+1) .NE. '1')GO TO 52680                               765
C     EXECUTE (SCROLL_BLANK_LINE_VAR_PRE)                                    766
      ASSIGN 52690 TO KKK080                                                 766
      GO TO 79860                                                               
52690 CONTINUE                                                                  
C     EXECUTE (SCROLL_BLANK_LINE_VAR_PRE)                                    767
      ASSIGN 52700 TO KKK080                                                 767
      GO TO 79860                                                               
52700 CONTINUE                                                                  
      GO TO 52670                                                            768
52680 CONTINUE                                                                  
      IF (BUFFER(I+FSZ+1) .NE. '0')GO TO 52720                               769
C     EXECUTE (SCROLL_BLANK_LINE_VAR_PRE)                                    770
      ASSIGN 52730 TO KKK080                                                 770
      GO TO 79860                                                               
52730 CONTINUE                                                                  
52720 CONTINUE                                                               771
52670 CONTINUE                                                               772
      GO TO 52550                                                            773
52560 CONTINUE                                                                  
      IF (LENLMT .GE. 0)GO TO 52750                                          774
      IF (LENLMT .LE. (FSZ - LEN))GO TO 52770                                775
       M=1                                                                   776
      GO TO 52779                                                               
52780  M= M+(1)                                                                 
      IF ( M.GT.(FSZ-LEN-1)/LENLMT)GO TO 52781                                  
52779 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                  777
      ASSIGN 52790 TO KKK070                                                 777
      GO TO 79850                                                               
52790 CONTINUE                                                                  
      GO TO 52780                                                            778
52781 CONTINUE                                                                  
                          M = LEN - FSZ                                      779
      GO TO 52760                                                            780
52770 CONTINUE                                                                  
                          M = -LENLMT                                        781
52760 CONTINUE                                                               782
      GO TO 52740                                                            783
52750 CONTINUE                                                                  
      IF (LENLMT .LE. (LEN - FSZ))GO TO 52810                                784
                          M = LEN - FSZ                                      785
      GO TO 52800                                                            786
52810 CONTINUE                                                                  
                          M = LENLMT                                         787
52800 CONTINUE                                                               788
52740 CONTINUE                                                               789
                      CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,            790
     1                              BUFFER(I+FSZ+1),%VAL(M),,,,)                
                      CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),                792
     1                             %VAL('1171'X),IOSB,STOPSC,,                  
     2                             INBUF,%VAL(1),,,,)                           
52550 CONTINUE                                                               795
C     EXECUTE (WAIT_FOR_SCROLL)                                              796
      ASSIGN 52820 TO KKK074                                                 796
      GO TO 79854                                                               
52820 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 52840                                          797
C     EXECUTE (VAR_BACK_SCROLL_EXIT)                                         798
      ASSIGN 52850 TO KKK082                                                 798
      GO TO 79862                                                               
52850 CONTINUE                                                                  
52840 CONTINUE                                                               799
52390 CONTINUE                                                               800
52380 CONTINUE                                                               801
52310 CONTINUE                                                               802
52290 CONTINUE                                                               803
      GO TO 52270                                                            804
52271 CONTINUE                                                                  
52000 CONTINUE                                                               805
         BLK = BLK - 2                                                       806
      IF (BLK .GT. 0)GO TO 52870                                             807
C     EXECUTE (HIT_BEGINNING)                                                808
      ASSIGN 52880 TO KKK084                                                 808
      GO TO 79864                                                               
52880 CONTINUE                                                                  
C     EXECUTE (GET_LAST_LINE)                                                809
      ASSIGN 52890 TO KKK030                                                 809
      GO TO 79810                                                               
52890 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                              810
      ASSIGN 52900 TO KKK064                                                 810
      GO TO 79844                                                               
52900 CONTINUE                                                                  
           RETURN                                                            811
52870 CONTINUE                                                               812
       I=1                                                                   813
      GO TO 52909                                                               
52910  I= I+(1)                                                                 
      IF ( I.GT.150)GO TO 52911                                                 
52909 CONTINUE                                                                  
          HBUFER(I) = BUFFER(I)                                              814
      GO TO 52910                                                            815
52911 CONTINUE                                                                  
         CALL BLKIN(BLK,PBUF,BUFFER)                                         816
      IF (PBUF .NE. 0)GO TO 52930                                            817
C     EXECUTE (READ_ERROR)                                                   818
      ASSIGN 52940 TO KKK060                                                 818
      GO TO 79840                                                               
52940 CONTINUE                                                                  
52930 CONTINUE                                                               819
       I=1                                                                   820
      GO TO 52949                                                               
52950  I= I+(1)                                                                 
      IF ( I.GT.150)GO TO 52951                                                 
52949 CONTINUE                                                                  
          BUFFER(PBUF+I) = HBUFER(I)                                         821
      GO TO 52950                                                            822
52951 CONTINUE                                                                  
         ISAVE = PBUF                                                        823
         NBUF = PBUF + 150                                                   824
      GO TO 51989                                                            825
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (CANCEL_COMMAND)                                             827
79848 CONTINUE                                                               827
CP    This routine is used to terminate either line jumps or searches on        
CP    input of a (.                                                             
        CALL LIB$FREE_EF(IOFLAG)                                             830
C     EXECUTE (RESTORE_HELD_SETUP)                                           831
      ASSIGN 52960 TO KKK054                                                 831
      GO TO 79834                                                               
52960 CONTINUE                                                                  
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                          832
      ASSIGN 52970 TO KKK046                                                 832
      GO TO 79826                                                               
52970 CONTINUE                                                                  
        RETURN                                                               833
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (CANCEL_STOPPER)                                             835
79812 CONTINUE                                                               835
CP    This routine gets rid of the stopping AST.                                
        CALL SYS$CANCEL(%VAL(INCHAN))                                        837
        CALL LIB$FREE_EF(IOFLAG)                                             838
      GO TO KKK032                                                           839
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (ERROR_EXIT)                                                 840
79804 CONTINUE                                                               840
CP    This routine rings the terminal bell then resets the LEDs (#3 on)         
CP    and returns.                                                              
C     EXECUTE (CANCEL_STOPPER)                                               843
      ASSIGN 52980 TO KKK032                                                 843
      GO TO 79812                                                               
52980 CONTINUE                                                                  
        CALL WORKNG(.FALSE.)                                                 844
        CALL LIB$PUT_SCREEN(CHAR(BELL))                                      845
      IF (.NOT.(VT100))GO TO 53000                                           846
          CALL LIB$PUT_SCREEN(CHAR(ESC)//'[0;3q')                            847
          IF (DIRECT .LT. 0) CALL LIB$PUT_SCREEN(CHAR(ESC)//'[2q')           848
          IF (RANGE(1,1) .NE. 0 .AND. RANGE(1,2) .NE. 0)                     849
     1        CALL LIB$PUT_SCREEN(CHAR(ESC)//'[4q')                             
53000 CONTINUE                                                               851
        RETURN                                                               852
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (FIND_NON_CC_LINE)                                           854
79818 CONTINUE                                                               854
CP    This procedure locates a non-carriage-control line near LINEP             
 1550 CONTINUE                                                               856
       I=LINEP                                                               857
      GO TO 53019                                                               
53020  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 53021                                              
53019 CONTINUE                                                                  
      IF (CCLINE(1,I) .NE. 0)GO TO 53011                                     858
      GO TO 53020                                                            859
53021 CONTINUE                                                                  
       I=LINEP-1                                                             860
      GO TO 53029                                                               
53030  I= I+(-1)                                                                
      IF ( I.LT.1)GO TO 53031                                                   
53029 CONTINUE                                                                  
      IF (CCLINE(1,I) .NE. 0)GO TO 53011                                     861
      GO TO 53030                                                            862
53031 CONTINUE                                                                  
53011 CONTINUE                                                               863
      GO TO KKK038                                                           864
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (FORWARD_SEARCH)                                             865
79822 CONTINUE                                                               865
CP    This routine searchs forward through the file for a given string.         
CP    If it is found the screen is reset to that position, and the LEDs are     
CP    reset (#3 off), otherwise the position and screen remain unchanged        
CP    and the LEDs are reset (#3 on).  In either case a return from DOIT        
CP    is executed.                                                              
C                                                                               
C***   First search lines in buffer                                             
C                                                                               
 1750 CONTINUE                                                               874
      IF (LINEP .GE. NLINES)GO TO 53060                                      875
       I=LINEP+1                                                             876
      GO TO 53069                                                               
53070  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 53071                                              
53069 CONTINUE                                                                  
       JJ=1                                                                  877
      GO TO 53079                                                               
53080  JJ= JJ+(1)                                                               
      IF ( JJ.GT.10)GO TO 53081                                                 
53079 CONTINUE                                                                  
      IF (SBFLEN(JJ) .EQ. 0)GO TO 53081                                      878
             CALL CONVUP(LINLEN(I),%REF(LINES(I)),%REF(CAPLIN))              879
      IF (INDEX(CAPLIN(:LINLEN(I)),SRCHBF(JJ)(:SBFLEN(JJ))) .EQ.             880
     X    0)GO TO 53100                                                         
               NUMBER = I - LINEP                                            882
               LINEP = I                                                     883
C     EXECUTE (GET_LAST_LINE)                                                884
      ASSIGN 53110 TO KKK030                                                 884
      GO TO 79810                                                               
53110 CONTINUE                                                                  
      IF ((NUMBER+LINEP) .GE. PAGEND)GO TO 53130                             885
C     EXECUTE (SCROLL_REFRESH_EXIT)                                          886
      ASSIGN 53140 TO KKK034                                                 886
      GO TO 79814                                                               
53140 CONTINUE                                                                  
      GO TO 53120                                                            887
53130 CONTINUE                                                                  
C     EXECUTE (REFRESH_EXIT)                                                 888
      ASSIGN 53150 TO KKK036                                                 888
      GO TO 79816                                                               
53150 CONTINUE                                                                  
53120 CONTINUE                                                               889
      GO TO 53041                                                            890
53100 CONTINUE                                                               891
      GO TO 53080                                                            892
53081 CONTINUE                                                                  
      GO TO 53070                                                            893
53071 CONTINUE                                                                  
53060 CONTINUE                                                               894
C                                                                               
C***   Then search file                                                         
C                                                                               
C     EXECUTE (HOLD_CURRENT_SETUP)                                           898
      ASSIGN 53160 TO KKK026                                                 898
      GO TO 79806                                                               
53160 CONTINUE                                                                  
         IBLK = BLK                                                          899
      IF (NBUF .LE. 0)GO TO 53180                                            900
           I = NBUF + 1                                                      901
      GO TO 53170                                                            902
53180 CONTINUE                                                                  
           I = 1                                                             903
53170 CONTINUE                                                               904
         IDESCR(2) = %LOC(CAPBUF(1))                                         905
53189 CONTINUE                                                               906
C                                                                               
C---   Input some buffer                                                        
C                                                                               
      IF (IBLK .LE. EBK)GO TO 53210                                          910
C     EXECUTE (SEARCH_ERROR_EXIT)                                            911
      ASSIGN 53220 TO KKK066                                                 911
      GO TO 79846                                                               
53220 CONTINUE                                                                  
53210 CONTINUE                                                               912
      IF (IBLK .GE. (EBK - 6))GO TO 53240                                    913
C                                                                               
C===   Not near the end of the file                                             
C                                                                               
            CALL BLKSIN(IBLK,J,BUFFER(I))                                    917
      IF (J .NE. 0)GO TO 53260                                               918
C     EXECUTE (READ_ERROR)                                                   919
      ASSIGN 53270 TO KKK060                                                 919
      GO TO 79840                                                               
53270 CONTINUE                                                                  
53260 CONTINUE                                                               920
      GO TO 53230                                                            921
53240 CONTINUE                                                                  
C                                                                               
C===   Near the end of the file                                                 
C                                                                               
            J = 0                                                            925
       II=IBLK                                                               926
      GO TO 53279                                                               
53280  II= II+(1)                                                               
      IF ( II.GT.EBK)GO TO 53281                                                
53279 CONTINUE                                                                  
             CALL BLKIN(IBLK,JJ,BUFFER(J+I))                                 927
      IF (JJ .NE. 0)GO TO 53300                                              928
C     EXECUTE (READ_ERROR)                                                   929
      ASSIGN 53310 TO KKK060                                                 929
      GO TO 79840                                                               
53310 CONTINUE                                                                  
53300 CONTINUE                                                               930
             J = J + JJ                                                      931
      GO TO 53280                                                            932
53281 CONTINUE                                                                  
53230 CONTINUE                                                               933
          NBUF = J + I - 1                                                   934
          CALL CONVUP(NBUF,BUFFER(1),CAPBUF(1))                              935
          IDESCR(1) = NBUF                                                   936
       JJ=1                                                                  937
      GO TO 53319                                                               
53320  JJ= JJ+(1)                                                               
      IF ( JJ.GT.10)GO TO 53321                                                 
53319 CONTINUE                                                                  
      IF (SBFLEN(JJ) .EQ. 0)GO TO 53321                                      938
           STRPOS(JJ) = LIB$INDEX(IDESCR,SRCHBF(JJ)(:SBFLEN(JJ)))            939
      IF (.NOT.(STOPIT))GO TO 53340                                          940
C     EXECUTE (CANCEL_COMMAND)                                               941
      ASSIGN 53350 TO KKK068                                                 941
      GO TO 79848                                                               
53350 CONTINUE                                                                  
53340 CONTINUE                                                               942
      GO TO 53320                                                            943
53321 CONTINUE                                                                  
          I = NBUF                                                           944
       K=1                                                                   945
      GO TO 53359                                                               
53360  K= K+(1)                                                                 
      IF ( K.GT.10)GO TO 53361                                                  
53359 CONTINUE                                                                  
      IF (SBFLEN(K) .EQ. 0)GO TO 53361                                       946
      IF (.NOT.(STRPOS(K) .NE. 0 .AND. STRPOS(K) .LT. I))GO TO 53380         947
             I = STRPOS(K)                                                   948
             JJ = K                                                          949
53380 CONTINUE                                                               950
      GO TO 53360                                                            951
53361 CONTINUE                                                                  
      IF (I .EQ. NBUF)GO TO 53400                                            952
C                                                                               
C===   Found it                                                                 
C                                                                               
            IBLK = IBLK - (NBUF - I)/512 - 2                                 956
            IF (IBLK .LE. 0) IBLK = 1                                        957
      IF (IBLK .GT. (EBK - 6))GO TO 53420                                    958
              BLK = IBLK                                                     959
              I = 1                                                          960
      GO TO 53410                                                            961
53420 CONTINUE                                                                  
              BLK = EBK - 6                                                  962
              I = 0                                                          963
53410 CONTINUE                                                               964
C     EXECUTE (INPUT_SEVEN_BLOCKS)                                           965
      ASSIGN 53430 TO KKK040                                                 965
      GO TO 79820                                                               
53430 CONTINUE                                                                  
            CALL LINCON(.TRUE.)                                              966
      IF (I .NE. 0)GO TO 53450                                               967
       I=1                                                                   968
      GO TO 53459                                                               
53460  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 53461                                              
53459 CONTINUE                                                                  
      IF (CCLINE(1,I) .EQ. IBLK)GO TO 53461                                  969
      GO TO 53460                                                            970
53461 CONTINUE                                                                  
53450 CONTINUE                                                               971
       J=I                                                                   972
      GO TO 53469                                                               
53470  J= J+(1)                                                                 
      IF ( J.GT.NLINES)GO TO 53471                                              
53469 CONTINUE                                                                  
             CALL CONVUP(LINLEN(J),%REF(LINES(J)),%REF(CAPLIN))              973
      IF (INDEX(CAPLIN(:LINLEN(J)),SRCHBF(JJ)(:SBFLEN(JJ)))                  974
     X    .EQ. 0)GO TO 53490                                                    
               LINEP = J                                                     976
C     EXECUTE (GET_LAST_LINE)                                                977
      ASSIGN 53500 TO KKK030                                                 977
      GO TO 79810                                                               
53500 CONTINUE                                                                  
C     EXECUTE (CANCEL_STOPPER)                                               978
      ASSIGN 53510 TO KKK032                                                 978
      GO TO 79812                                                               
53510 CONTINUE                                                                  
C     EXECUTE (REFRESH_EXIT)                                                 979
      ASSIGN 53520 TO KKK036                                                 979
      GO TO 79816                                                               
53520 CONTINUE                                                                  
      GO TO 53041                                                            980
53490 CONTINUE                                                               981
      IF (.NOT.(STOPIT))GO TO 53540                                          982
C     EXECUTE (CANCEL_COMMAND)                                               983
      ASSIGN 53550 TO KKK068                                                 983
      GO TO 79848                                                               
53550 CONTINUE                                                                  
53540 CONTINUE                                                               984
      GO TO 53470                                                            985
53471 CONTINUE                                                                  
C                                                                               
C===   Not found                                                                
C                                                                               
53400 CONTINUE                                                               989
          I = 31                                                             990
       J=1                                                                   991
      GO TO 53559                                                               
53560  J= J+(1)                                                                 
      IF ( J.GT.30)GO TO 53561                                                  
53559 CONTINUE                                                                  
           BUFFER(J) = CAPBUF(NBUF-30+J)                                     992
      GO TO 53560                                                            993
53561 CONTINUE                                                                  
      GO TO 53189                                                            994
53041 CONTINUE                                                               995
      GO TO KKK042                                                           996
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (FORWARD_SCROLLING)                                          997
79830 CONTINUE                                                               997
CP    This procedure executes scrolling in a forward direction.                 
        LINEP = PAGEND                                                       999
        FIRST = .TRUE.                                                      1000
      IF (PAGLEN .NE. (COMLIN - 2))GO TO 53580                              1001
          SECOND = .TRUE.                                                   1002
      GO TO 53570                                                           1003
53580 CONTINUE                                                                  
          SECOND = .FALSE.                                                  1004
53570 CONTINUE                                                              1005
53589 CONTINUE                                                              1006
C                                                                               
C***   Scroll through line buffer                                               
C                                                                               
      IF (LINEP .EQ. NLINES)GO TO 53591                                     1010
       I=LINEP+1                                                            1011
      GO TO 53599                                                               
53600  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 53601                                              
53599 CONTINUE                                                                  
          CALL SYS$CANCEL(%VAL(INCHAN))                                     1012
      IF (.NOT.(FIRST))GO TO 53620                                          1013
            FIRST = .FALSE.                                                 1014
       J=PAGLEN+1                                                           1015
      GO TO 53629                                                               
53630  J= J+(1)                                                                 
      IF ( J.GT.COMLIN)GO TO 53631                                              
53629 CONTINUE                                                                  
             CALL LIB$ERASE_LINE(J,1)                                       1016
      GO TO 53630                                                           1017
53631 CONTINUE                                                                  
      IF (LENLMT .GE. 0)GO TO 53650                                         1018
      IF (LINLEN(I) .GT. -LENLMT)GO TO 53670                                1019
                CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I)),PAGLEN+1,1)        1020
      GO TO 53660                                                           1021
53670 CONTINUE                                                                  
                CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//              1022
     1               LINES(I)(1-LENLMT:LINLEN(I)),PAGLEN+1,1)                   
53660 CONTINUE                                                              1024
      GO TO 53640                                                           1025
53650 CONTINUE                                                                  
      IF (LINLEN(I) .LE. LENLMT)GO TO 53690                                 1026
                CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT),PAGLEN+1,1)           1027
      GO TO 53680                                                           1028
53690 CONTINUE                                                                  
                CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I)),PAGLEN+1,1)        1029
53680 CONTINUE                                                              1030
53640 CONTINUE                                                              1031
      GO TO 53610                                                           1032
53620 CONTINUE                                                                  
      IF (.NOT.(SECOND))GO TO 53710                                         1033
              SECOND = .FALSE.                                              1034
              IF (LENLMT .LT. 0 .AND. LINLEN(I) .GT. (-LENLMT))             1035
     1            CALL LIB$PUT_SCREEN(CRLF,COMLIN,1)                            
      GO TO 53700                                                           1037
53710 CONTINUE                                                                  
              CALL LIB$PUT_SCREEN(CRLF,COMLIN,1)                            1038
53700 CONTINUE                                                              1039
      IF (LENLMT .GE. 0)GO TO 53730                                         1040
      IF (LINLEN(I) .GT. -LENLMT)GO TO 53750                                1041
                CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I)),COMLIN,1)          1042
      GO TO 53740                                                           1043
53750 CONTINUE                                                                  
                CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//              1044
     1               LINES(I)(1-LENLMT:LINLEN(I)),COMLIN,1)                     
53740 CONTINUE                                                              1046
      GO TO 53720                                                           1047
53730 CONTINUE                                                                  
      IF (LINLEN(I) .LE. LENLMT)GO TO 53770                                 1048
                CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT),COMLIN,1)             1049
      GO TO 53760                                                           1050
53770 CONTINUE                                                                  
                CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I)),COMLIN,1)          1051
53760 CONTINUE                                                              1052
53720 CONTINUE                                                              1053
53610 CONTINUE                                                              1054
          CALL LIB$SET_CURSOR(1,1)                                          1055
C     EXECUTE (WAIT_FOR_SCROLL_AST)                                         1056
      ASSIGN 53780 TO KKK072                                                1056
      GO TO 79852                                                               
53780 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 53800                                         1057
            PAGEND = I                                                      1058
C     EXECUTE (GET_START_LINE)                                              1059
      ASSIGN 53810 TO KKK086                                                1059
      GO TO 79866                                                               
53810 CONTINUE                                                                  
      IF (FIRST)GO TO 53830                                                 1060
              CALL LIB$PUT_SCREEN(CRLF,COMLIN,1)                            1061
              IF (PAGLEN .EQ. (COMLIN - 2))                                 1062
     1            CALL LIB$PUT_SCREEN(CRLF,COMLIN,1)                            
              CALL LIB$SET_CURSOR(1,1)                                      1064
53830 CONTINUE                                                              1065
C     EXECUTE (WRITE_BLOCK_NUMBER)                                          1066
      ASSIGN 53840 TO KKK062                                                1066
      GO TO 79842                                                               
53840 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                             1067
      ASSIGN 53850 TO KKK064                                                1067
      GO TO 79844                                                               
53850 CONTINUE                                                                  
            RETURN                                                          1068
53800 CONTINUE                                                              1069
      GO TO 53600                                                           1070
53601 CONTINUE                                                                  
C                                                                               
C***   Input to line buffer                                                     
C                                                                               
C                                                                               
C---   End of file                                                              
C                                                                               
      IF (BLK .LE. EBK)GO TO 53870                                          1077
           PAGEND = NLINES                                                  1078
C     EXECUTE (GET_START_LINE)                                              1079
      ASSIGN 53880 TO KKK086                                                1079
      GO TO 79866                                                               
53880 CONTINUE                                                                  
           CALL SYS$CANCEL(%VAL(INCHAN))                                    1080
      IF (FIRST)GO TO 53900                                                 1081
             CALL LIB$PUT_SCREEN(CRLF,COMLIN,1)                             1082
             CALL LIB$SET_CURSOR(1,1)                                       1083
53900 CONTINUE                                                              1084
C     EXECUTE (WRITE_BLOCK_NUMBER)                                          1085
      ASSIGN 53910 TO KKK062                                                1085
      GO TO 79842                                                               
53910 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                             1086
      ASSIGN 53920 TO KKK064                                                1086
      GO TO 79844                                                               
53920 CONTINUE                                                                  
           RETURN                                                           1087
53870 CONTINUE                                                              1088
      IF (BLK .GE. (EBK - 6))GO TO 53940                                    1089
C                                                                               
C---   Not near the end of the file                                             
C                                                                               
           CALL BLKSIN(BLK,I,BUFFER(NBUF+1))                                1093
      IF (I .NE. 0)GO TO 53960                                              1094
C     EXECUTE (READ_ERROR)                                                  1095
      ASSIGN 53970 TO KKK060                                                1095
      GO TO 79840                                                               
53970 CONTINUE                                                                  
53960 CONTINUE                                                              1096
      GO TO 53930                                                           1097
53940 CONTINUE                                                                  
C                                                                               
C---   Near the end of the file                                                 
C                                                                               
           I = 0                                                            1101
       J=BLK                                                                1102
      GO TO 53979                                                               
53980  J= J+(1)                                                                 
      IF ( J.GT.EBK)GO TO 53981                                                 
53979 CONTINUE                                                                  
            CALL BLKIN(BLK,II,BUFFER(NBUF+I+1))                             1103
      IF (II .NE. 0)GO TO 54000                                             1104
C     EXECUTE (READ_ERROR)                                                  1105
      ASSIGN 54010 TO KKK060                                                1105
      GO TO 79840                                                               
54010 CONTINUE                                                                  
54000 CONTINUE                                                              1106
            I = I + II                                                      1107
      GO TO 53980                                                           1108
53981 CONTINUE                                                                  
53930 CONTINUE                                                              1109
         NBUF = NBUF + I                                                    1110
         LINEP = NLINES                                                     1111
         PBUF = 1                                                           1112
         CALL LINCON(.FALSE.)                                               1113
      GO TO 53589                                                           1114
53591 CONTINUE                                                                  
      GO TO KKK050                                                          1115
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (FREE_FLAGS_EXIT)                                           1116
79844 CONTINUE                                                              1116
CP    This routine exits from scrolling.  It frees the event flags, cancels     
CP    any input requests to the terminal, resets the LEDs (#3 off) and          
CP    returns.                                                                  
C     EXECUTE (CANCEL_STOPPER)                                              1120
      ASSIGN 54020 TO KKK032                                                1120
      GO TO 79812                                                               
54020 CONTINUE                                                                  
        CALL LIB$FREE_EF(TIMFLG)                                            1121
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                         1122
      ASSIGN 54030 TO KKK046                                                1122
      GO TO 79826                                                               
54030 CONTINUE                                                                  
      GO TO KKK064                                                          1123
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (GET_LAST_LINE)                                             1124
79810 CONTINUE                                                              1124
CP    This procedure determines PAGEND given LINEP                              
        OLDEND = PAGEND                                                     1126
      IF (LENLMT .GE. 0)GO TO 54050                                         1127
          I = PAGLEN                                                        1128
       PAGEND=LINEP                                                         1129
      GO TO 54059                                                               
54060  PAGEND= PAGEND+(1)                                                       
      IF ( PAGEND.GT.LINEP+PAGLEN)GO TO 54061                                   
54059 CONTINUE                                                                  
           I = I + (LINLEN(PAGEND) - 1)/LENLMT - 1                          1130
      IF (I .LE. 0)GO TO 54061                                              1131
      GO TO 54060                                                           1132
54061 CONTINUE                                                                  
      GO TO 54040                                                           1133
54050 CONTINUE                                                                  
          PAGEND = LINEP + PAGLEN - 1                                       1134
54040 CONTINUE                                                              1135
      GO TO KKK030                                                          1136
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (GET_START_LINE)                                            1137
79866 CONTINUE                                                              1137
CP    This procedure determines LINEP given a value for PAGEND                  
      IF (LENLMT .GE. 0)GO TO 54080                                         1139
          I = PAGLEN                                                        1140
       LINEP=PAGEND                                                         1141
      GO TO 54089                                                               
54090  LINEP= LINEP+(-1)                                                        
      IF ( LINEP.LT.PAGEND-PAGLEN)GO TO 54091                                   
54089 CONTINUE                                                                  
           I = I + (LINLEN(LINEP) - 1)/LENLMT - 1                           1142
      IF (I .LE. 0)GO TO 54091                                              1143
      GO TO 54090                                                           1144
54091 CONTINUE                                                                  
      GO TO 54070                                                           1145
54080 CONTINUE                                                                  
          LINEP = PAGEND - PAGLEN + 1                                       1146
54070 CONTINUE                                                              1147
      GO TO KKK086                                                          1148
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (HIT_BEGINNING)                                             1149
79864 CONTINUE                                                              1149
CP    This procedure is executed if the position goes past the beginning        
CP    of the file.  Everything is reset to the beginning of the file.           
        BLK = 1                                                             1152
C     EXECUTE (INPUT_SEVEN_BLOCKS)                                          1153
      ASSIGN 54100 TO KKK040                                                1153
      GO TO 79820                                                               
54100 CONTINUE                                                                  
        CALL LINCON(.TRUE.)                                                 1154
        LINEP = 1                                                           1155
      GO TO KKK084                                                          1156
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (HOLD_CURRENT_SETUP)                                        1157
79806 CONTINUE                                                              1157
CP    This routine is used in searches.  It holds all the information           
CP    needed to recreate the current position and array conditions.             
       I=1                                                                  1160
      GO TO 54109                                                               
54110  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 54111                                              
54109 CONTINUE                                                                  
         HLIN(I) = LINES(I)                                                 1161
         HLINLN(I) = LINLEN(I)                                              1162
         HCCLIN(1,I) = CCLINE(1,I)                                          1163
         HCCLIN(2,I) = CCLINE(2,I)                                          1164
      GO TO 54110                                                           1165
54111 CONTINUE                                                                  
        HLINES = NLINES                                                     1166
        HLINEP = LINEP                                                      1167
      IF (NBUF .EQ. 0)GO TO 54130                                           1168
       I=1                                                                  1169
      GO TO 54139                                                               
54140  I= I+(1)                                                                 
      IF ( I.GT.NBUF)GO TO 54141                                                
54139 CONTINUE                                                                  
           HBUFER(I) = BUFFER(I)                                            1170
      GO TO 54140                                                           1171
54141 CONTINUE                                                                  
54130 CONTINUE                                                              1172
        HPBUF = PBUF                                                        1173
        HNBUF = NBUF                                                        1174
        HBLK = BLK                                                          1175
        HSBLK = SBLK                                                        1176
        HLBLK = LBLK                                                        1177
        HLPBUF = LPBUF                                                      1178
C     EXECUTE (QUEUE_STOPPER)                                               1179
      ASSIGN 54150 TO KKK048                                                1179
      GO TO 79828                                                               
54150 CONTINUE                                                                  
      GO TO KKK026                                                          1180
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (HOME_PLUS_LEDS_EXIT)                                       1181
79826 CONTINUE                                                              1181
CP    This routine homes the cursor, resets the LEDs (#3 off).                  
      IF (.NOT.(VT100))GO TO 54170                                          1183
          CALL LIB$PUT_SCREEN(CHAR(ESC)//'[0q',1,1)                         1184
          IF (RANGE(1,1) .NE. 0 .AND. RANGE(1,2) .NE. 0)                    1185
     1        CALL LIB$PUT_SCREEN(CHAR(ESC)//'[4q')                             
          IF (DIRECT .LT. 0) CALL LIB$PUT_SCREEN(CHAR(ESC)//'[2q')          1187
      GO TO 54160                                                           1188
54170 CONTINUE                                                                  
          CALL LIB$SET_CURSOR(1,1)                                          1189
54160 CONTINUE                                                              1190
      GO TO KKK046                                                          1191
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (INPUT_SEVEN_BLOCKS)                                        1192
79820 CONTINUE                                                              1192
CP    This procedure inputs seven blocks of data                                
      IF (BLK .GE. (EBK - 6))GO TO 54190                                    1194
C                                                                               
C***   Not near the end of the file                                             
C                                                                               
          SBLK = BLK                                                        1198
          CALL BLKSIN(BLK,NBUF,BUFFER)                                      1199
      IF (NBUF .NE. 0)GO TO 54210                                           1200
C     EXECUTE (READ_ERROR)                                                  1201
      ASSIGN 54220 TO KKK060                                                1201
      GO TO 79840                                                               
54220 CONTINUE                                                                  
54210 CONTINUE                                                              1202
      GO TO 54180                                                           1203
54190 CONTINUE                                                                  
C                                                                               
C***   Near the end of the file                                                 
C                                                                               
          BLK = EBK - 6                                                     1207
      IF (BLK .LE. 0)GO TO 54240                                            1208
            SBLK = BLK                                                      1209
            CALL BLKSIN(BLK,NBUF,BUFFER)                                    1210
      IF (NBUF .NE. 0)GO TO 54260                                           1211
C     EXECUTE (READ_ERROR)                                                  1212
      ASSIGN 54270 TO KKK060                                                1212
      GO TO 79840                                                               
54270 CONTINUE                                                                  
54260 CONTINUE                                                              1213
      GO TO 54230                                                           1214
54240 CONTINUE                                                                  
C                                                                               
C***   Do this if file size is < 7 blocks                                       
C                                                                               
            BLK = 1                                                         1218
            SBLK = 1                                                        1219
            NBUF = 0                                                        1220
       I=BLK                                                                1221
      GO TO 54279                                                               
54280  I= I+(1)                                                                 
      IF ( I.GT.EBK)GO TO 54281                                                 
54279 CONTINUE                                                                  
             CALL BLKIN(BLK,J,BUFFER(NBUF+1))                               1222
      IF (J .NE. 0)GO TO 54300                                              1223
C     EXECUTE (READ_ERROR)                                                  1224
      ASSIGN 54310 TO KKK060                                                1224
      GO TO 79840                                                               
54310 CONTINUE                                                                  
54300 CONTINUE                                                              1225
             NBUF = NBUF + J                                                1226
      GO TO 54280                                                           1227
54281 CONTINUE                                                                  
54230 CONTINUE                                                              1228
54180 CONTINUE                                                              1229
      GO TO KKK040                                                          1230
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (JUMP_LINES)                                                1231
79808 CONTINUE                                                              1231
CP    This procedure moves the current position NUMBER lines.                   
      IF ((NUMBER + LINEP) .LE. (NLINES - PAGLEN + 1))GO TO 54330           1233
C                                                                               
C***   Big jumps forward                                                        
C                                                                               
          SECOND = .FALSE.                                                  1237
54339 CONTINUE                                                              1238
      IF (BLK .LE. EBK)GO TO 54360                                          1239
             LINEP = NLINES - PAGLEN + 1                                    1240
             IF (LINEP .LT. 1) LINEP = 1                                    1241
             JNUMB = NUMBER                                                 1242
      GO TO 54341                                                           1243
54360 CONTINUE                                                              1244
           CALL BLKIN(BLK,I,BUFFER(NBUF+1))                                 1245
      IF (I .NE. 0)GO TO 54380                                              1246
C     EXECUTE (READ_ERROR)                                                  1247
      ASSIGN 54390 TO KKK060                                                1247
      GO TO 79840                                                               
54390 CONTINUE                                                                  
54380 CONTINUE                                                              1248
           NBUF = NBUF + I                                                  1249
           PBUF = 1                                                         1250
           CALL LINCON(.FALSE.)                                             1251
      IF (.NOT.(STOPIT))GO TO 54410                                         1252
C     EXECUTE (CANCEL_COMMAND)                                              1253
      ASSIGN 54420 TO KKK068                                                1253
      GO TO 79848                                                               
54420 CONTINUE                                                                  
54410 CONTINUE                                                              1254
      IF ((LINEP + NUMBER) .GT. (NLINES - PAGLEN + 1))GO TO 54440           1255
             LINEP = LINEP + NUMBER                                         1256
      GO TO 54341                                                           1257
54440 CONTINUE                                                              1258
      IF ((NLINES - LINEP) .LT. (600 - 100))GO TO 54460                     1259
             SECOND = .TRUE.                                                1260
             NUMBER = NUMBER - NLINES + LINEP - 1                           1261
             LINEP = NLINES                                                 1262
54460 CONTINUE                                                              1263
      GO TO 54339                                                           1264
54341 CONTINUE                                                                  
          IF (SECOND) NUMBER = 25                                           1265
      GO TO 54320                                                           1266
54330 CONTINUE                                                                  
      IF ((NUMBER + LINEP) .GT. 0)GO TO 54480                               1267
C                                                                               
C***   Big jumps backward                                                       
C                                                                               
            NUMBER = -(NUMBER + LINEP)                                      1271
            INUMBR = 1 - LINEP                                              1272
      IF (.NOT.(RFM .EQ. 1 .AND. RAT .NE. 2 .AND. RAT .NE. -2))GO TO 545    1273
     X00                                                                        
C                                                                               
C---   Fixed record length with no carriage-control                             
C                                                                               
      IF (RAT .GE. 0)GO TO 54520                                            1277
C===   No spanned                                                               
                BLK = SBLK - NUMBER*IFSZ/RECBLK - 1                         1279
                J = RECBLK/IFSZ - NUMBER + (SBLK-BLK-1)*RECBLK/IFSZ         1280
      GO TO 54510                                                           1281
54520 CONTINUE                                                                  
C===   Spanned                                                                  
                I = 512*(CCLINE(1,1) - 1) + CCLINE(2,1) -                   1283
     1              IFSZ*NUMBER                                                 
                J = LIB$EXTZV(0,8,I)                                        1285
                IF (J .EQ. 0) J = 512                                       1286
                BLK = I/512 + 1                                             1287
54510 CONTINUE                                                              1288
      IF (.NOT.(STOPIT))GO TO 54540                                         1289
C     EXECUTE (CANCEL_COMMAND)                                              1290
      ASSIGN 54550 TO KKK068                                                1290
      GO TO 79848                                                               
54550 CONTINUE                                                                  
54540 CONTINUE                                                              1291
      IF (BLK .GT. 0)GO TO 54570                                            1292
                BLK = 1                                                     1293
                J = 1                                                       1294
54570 CONTINUE                                                              1295
C===   Input lines                                                              
C     EXECUTE (INPUT_SEVEN_BLOCKS)                                          1297
      ASSIGN 54580 TO KKK040                                                1297
      GO TO 79820                                                               
54580 CONTINUE                                                                  
              CALL LINCON(.TRUE.)                                           1298
              LINEP = J                                                     1299
              NUMBER = 25                                                   1300
      GO TO 54490                                                           1301
54500 CONTINUE                                                                  
C                                                                               
C---   Variable length records and/or carriage-control                          
C                                                                               
       I=1                                                                  1305
      GO TO 54589                                                               
54590  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 54591                                              
54589 CONTINUE                                                                  
      IF (CCLINE(1,I) .NE. 0)GO TO 54591                                    1306
      GO TO 54590                                                           1307
54591 CONTINUE                                                                  
              BLK = CCLINE(1,I)                                             1308
              PBUF = CCLINE(2,I)                                            1309
              CALL BLKIN(BLK,NBUF,BUFFER)                                   1310
      IF (NBUF .NE. 0)GO TO 54610                                           1311
C     EXECUTE (READ_ERROR)                                                  1312
      ASSIGN 54620 TO KKK060                                                1312
      GO TO 79840                                                               
54620 CONTINUE                                                                  
54610 CONTINUE                                                              1313
 1810 CONTINUE                                                              1314
               SECOND = .TRUE.                                              1315
 1820 CONTINUE                                                              1316
54639 CONTINUE                                                                  
      IF (NUMBER .GE. 0)GO TO 54660                                         1317
                  NUMBER = 25                                               1318
      GO TO 54641                                                           1319
54660 CONTINUE                                                              1320
      IF (RFM .NE. 1)GO TO 54680                                            1321
C                                                                               
C===  Fixed length records (must have carriage-control)                         
C                                                                               
                  PBUF = PBUF - IFSZ                                        1325
      IF (PBUF .GE. 1)GO TO 54700                                           1326
                    BLK = BLK - 2                                           1327
      IF (BLK .GT. 0)GO TO 54720                                            1328
C     EXECUTE (HIT_BEGINNING)                                               1329
      ASSIGN 54730 TO KKK084                                                1329
      GO TO 79864                                                               
54730 CONTINUE                                                                  
      IF (.NOT.(SECOND))GO TO 54750                                         1330
                        NUMBER = INUMBR                                     1331
      GO TO 54740                                                           1332
54750 CONTINUE                                                                  
                        NUMBER = 25                                         1333
54740 CONTINUE                                                              1334
      GO TO 54631                                                           1335
54720 CONTINUE                                                              1336
                    CALL BLKIN(BLK,NBUF,BUFFER)                             1337
      IF (NBUF .NE. 0)GO TO 54770                                           1338
C     EXECUTE (READ_ERROR)                                                  1339
      ASSIGN 54780 TO KKK060                                                1339
      GO TO 79840                                                               
54780 CONTINUE                                                                  
54770 CONTINUE                                                              1340
                    SECOND = .FALSE.                                        1341
                    PBUF = PBUF + NBUF                                      1342
54700 CONTINUE                                                              1343
      IF (BUFFER(PBUF) .NE. '1')GO TO 54800                                 1344
                    NUMBER = NUMBER - 3                                     1345
      GO TO 54790                                                           1346
54800 CONTINUE                                                                  
      IF (BUFFER(PBUF) .NE. '0')GO TO 54820                                 1347
                      NUMBER = NUMBER - 2                                   1348
      GO TO 54810                                                           1349
54820 CONTINUE                                                                  
                      NUMBER = NUMBER - 1                                   1350
54810 CONTINUE                                                              1351
54790 CONTINUE                                                              1352
      GO TO 54670                                                           1353
54680 CONTINUE                                                                  
C                                                                               
C===   Variable length records                                                  
C                                                                               
54829 CONTINUE                                                              1357
       I=PBUF-1                                                             1358
      GO TO 54839                                                               
54840  I= I+(-1)                                                                
      IF ( I.LT.2)GO TO 54841                                                   
54839 CONTINUE                                                                  
      IF (I)GO TO 54860                                                     1359
      IF (BUFFER(I) .NE. 0)GO TO 54880                                      1360
                        J = LIB$EXTZV(0,8,BUFFER(I-1))                      1361
      IF (J .GT. LRL)GO TO 54900                                            1362
                          IF (J) J = J + 1                                  1363
      IF (BUFFER(I+J+2) .NE. 0)GO TO 54920                                  1364
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 54940                    1365
      IF (BUFFER(I+FSZ+1) .NE. '1')GO TO 54960                              1366
                                NUMBER = NUMBER - 3                         1367
      GO TO 54950                                                           1368
54960 CONTINUE                                                                  
      IF (BUFFER(I+FSZ+1) .NE. '0')GO TO 54980                              1369
                                  NUMBER = NUMBER - 2                       1370
      GO TO 54970                                                           1371
54980 CONTINUE                                                                  
                                  NUMBER = NUMBER - 1                       1372
54970 CONTINUE                                                              1373
54950 CONTINUE                                                              1374
      GO TO 54930                                                           1375
54940 CONTINUE                                                                  
                              NUMBER = NUMBER - 1                           1376
      IF (.NOT.(RAT .EQ. 3 .OR. RAT .EQ. -3))GO TO 55000                    1377
                                IF (LIB$EXTZV(7,1,BUFFER(I+1)) .EQ. 0)      1378
     1                              NUMBER = NUMBER -                           
     2                              LIB$EXTZV(0,7,BUFFER(I+1))                  
                                IF (LIB$EXTZV(7,1,BUFFER(I+2)) .EQ. 0)      1381
     1                              NUMBER = NUMBER -                           
     2                              LIB$EXTZV(0,7,BUFFER(I+2))                  
55000 CONTINUE                                                              1384
54930 CONTINUE                                                              1385
                            PBUF = I - 1                                    1386
      GO TO 54640                                                           1387
54920 CONTINUE                                                              1388
54900 CONTINUE                                                              1389
54880 CONTINUE                                                              1390
54860 CONTINUE                                                              1391
      IF (.NOT.(STOPIT))GO TO 55020                                         1392
C     EXECUTE (CANCEL_COMMAND)                                              1393
      ASSIGN 55030 TO KKK068                                                1393
      GO TO 79848                                                               
55030 CONTINUE                                                                  
55020 CONTINUE                                                              1394
      GO TO 54840                                                           1395
54841 CONTINUE                                                                  
C===  New block                                                                 
       I=1                                                                  1397
      GO TO 55039                                                               
55040  I= I+(1)                                                                 
      IF ( I.GT.150)GO TO 55041                                                 
55039 CONTINUE                                                                  
                    CAPBUF(I) = BUFFER(I)                                   1398
      GO TO 55040                                                           1399
55041 CONTINUE                                                                  
                   BLK = BLK - 2                                            1400
      IF (BLK .GT. 0)GO TO 55060                                            1401
      IF (.NOT.(SECOND))GO TO 55080                                         1402
                       NUMBER = INUMBR                                      1403
      GO TO 55070                                                           1404
55080 CONTINUE                                                                  
                       NUMBER = 25                                          1405
55070 CONTINUE                                                              1406
C     EXECUTE (HIT_BEGINNING)                                               1407
      ASSIGN 55090 TO KKK084                                                1407
      GO TO 79864                                                               
55090 CONTINUE                                                                  
      GO TO 54631                                                           1408
55060 CONTINUE                                                              1409
                   CALL BLKIN(BLK,PBUF,BUFFER)                              1410
      IF (PBUF .NE. 0)GO TO 55110                                           1411
C     EXECUTE (READ_ERROR)                                                  1412
      ASSIGN 55120 TO KKK060                                                1412
      GO TO 79840                                                               
55120 CONTINUE                                                                  
55110 CONTINUE                                                              1413
                   SECOND = .FALSE.                                         1414
       I=1                                                                  1415
      GO TO 55129                                                               
55130  I= I+(1)                                                                 
      IF ( I.GT.150)GO TO 55131                                                 
55129 CONTINUE                                                                  
                    BUFFER(PBUF+I) = CAPBUF(I)                              1416
      GO TO 55130                                                           1417
55131 CONTINUE                                                                  
      GO TO 54829                                                           1418
54670 CONTINUE                                                              1419
54640 GO TO 54639                                                           1420
54641 CONTINUE                                                                  
               BLK = BLK - 1                                                1421
               J = PBUF                                                     1422
C     EXECUTE (INPUT_SEVEN_BLOCKS)                                          1423
      ASSIGN 55140 TO KKK040                                                1423
      GO TO 79820                                                               
55140 CONTINUE                                                                  
               CALL LINCON(.TRUE.)                                          1424
       I=1                                                                  1425
      GO TO 55149                                                               
55150  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 55151                                              
55149 CONTINUE                                                                  
      IF (.NOT.(CCLINE(1,I) .EQ. SBLK .AND. CCLINE(2,I) .EQ. J))GO TO 55    1426
     X170                                                                       
                  LINEP = I                                                 1427
      GO TO 55151                                                           1428
55170 CONTINUE                                                              1429
      GO TO 55150                                                           1430
55151 CONTINUE                                                                  
54631 CONTINUE                                                              1431
54490 CONTINUE                                                              1432
      GO TO 54470                                                           1433
54480 CONTINUE                                                                  
C                                                                               
C---   Jumps within the stored buffer                                           
C                                                                               
            LINEP = LINEP + NUMBER                                          1437
54470 CONTINUE                                                              1438
54320 CONTINUE                                                              1439
C     EXECUTE (CANCEL_STOPPER)                                              1440
      ASSIGN 55180 TO KKK032                                                1440
      GO TO 79812                                                               
55180 CONTINUE                                                                  
      GO TO KKK028                                                          1441
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (OUTPUT_RANGE)                                              1442
79836 CONTINUE                                                              1442
CP    This procedure performs the output for the print, list, and append        
CP    commands.  Which is being used is determined by the value of PRINT        
CP    (-1 = print, 0 = append, 1 = list).                                       
C                                                                               
C---   Open printer output file                                                 
C                                                                               
        CALL PRTOPN(PRINT)                                                  1449
      IF (PRINT .NE. 47)GO TO 55200                                         1450
C     EXECUTE (ERROR_EXIT)                                                  1451
      ASSIGN 55210 TO KKK024                                                1451
      GO TO 79804                                                               
55210 CONTINUE                                                                  
55200 CONTINUE                                                              1452
C     EXECUTE (HOLD_CURRENT_SETUP)                                          1453
      ASSIGN 55220 TO KKK026                                                1453
      GO TO 79806                                                               
55220 CONTINUE                                                                  
C                                                                               
C---   Point to right place                                                     
C                                                                               
      IF (.NOT.(NUMFLG))GO TO 55240                                         1457
          NUMBER = DIRECT*ANUMB                                             1458
          RFLAG = .FALSE.                                                   1459
      GO TO 55230                                                           1460
55240 CONTINUE                                                                  
      IF (RANGE(1,2) .NE. 0)GO TO 55260                                     1461
            NUMBER = PAGLEN                                                 1462
            RFLAG = .FALSE.                                                 1463
      GO TO 55250                                                           1464
55260 CONTINUE                                                                  
            RFLAG = .TRUE.                                                  1465
55250 CONTINUE                                                              1466
55230 CONTINUE                                                              1467
      IF (.NOT.(RFLAG))GO TO 55280                                          1468
      IF (.NOT.(RANGE(1,1) .LT. RANGE(1,2) .OR. (RANGE(1,1) .EQ.            1469
     X          RANGE(1,2) .AND. RANGE(2,1) .LE. RANGE(2,2))))GO TO 5530        
     X0                                                                         
            BLK = RANGE(1,1)                                                1471
            PBUF = RANGE(2,1)                                               1472
            IENDPB = RANGE(1,2)                                             1473
            IENDPP = RANGE(2,2)                                             1474
      GO TO 55290                                                           1475
55300 CONTINUE                                                                  
            BLK = RANGE(1,2)                                                1476
            PBUF = RANGE(2,2)                                               1477
            IENDPB = RANGE(1,1)                                             1478
            IENDPP = RANGE(2,1)                                             1479
55290 CONTINUE                                                              1480
          SBLK = BLK                                                        1481
          CALL BLKIN(BLK,NBUF,BUFFER)                                       1482
      IF (NBUF .NE. 0)GO TO 55320                                           1483
C     EXECUTE (READ_ERROR)                                                  1484
      ASSIGN 55330 TO KKK060                                                1484
      GO TO 79840                                                               
55330 CONTINUE                                                                  
55320 CONTINUE                                                              1485
      GO TO 55270                                                           1486
55280 CONTINUE                                                                  
      IF (NUMBER .GE. 0)GO TO 55350                                         1487
            INUMB = NUMBER                                                  1488
C     EXECUTE (FIND_NON_CC_LINE)                                            1489
      ASSIGN 55360 TO KKK038                                                1489
      GO TO 79818                                                               
55360 CONTINUE                                                                  
            IENDPB = CCLINE(1,I)                                            1490
            IENDPP = CCLINE(2,I)                                            1491
C     EXECUTE (JUMP_LINES)                                                  1492
      ASSIGN 55370 TO KKK028                                                1492
      GO TO 79808                                                               
55370 CONTINUE                                                                  
            NUMBER = -1000                                                  1493
55350 CONTINUE                                                              1494
C     EXECUTE (FIND_NON_CC_LINE)                                            1495
      ASSIGN 55380 TO KKK038                                                1495
      GO TO 79818                                                               
55380 CONTINUE                                                                  
          BLK = CCLINE(1,I)                                                 1496
          PBUF = CCLINE(2,I)                                                1497
          SBLK = BLK                                                        1498
          CALL BLKIN(BLK,NBUF,BUFFER)                                       1499
      IF (NBUF .NE. 0)GO TO 55400                                           1500
C     EXECUTE (READ_ERROR)                                                  1501
      ASSIGN 55410 TO KKK060                                                1501
      GO TO 79840                                                               
55410 CONTINUE                                                                  
55400 CONTINUE                                                              1502
55270 CONTINUE                                                              1503
        IBLK = SBLK                                                         1504
C                                                                               
C---   Print loop                                                               
C                                                                               
55419 CONTINUE                                                              1508
      IF (.NOT.(STOPIT))GO TO 55440                                         1509
           CALL PRTCLS(STOPIT)                                              1510
C     EXECUTE (RESTORE_HELD_SETUP)                                          1511
      ASSIGN 55450 TO KKK054                                                1511
      GO TO 79834                                                               
55450 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                             1512
      ASSIGN 55460 TO KKK064                                                1512
      GO TO 79844                                                               
55460 CONTINUE                                                                  
           RETURN                                                           1513
55440 CONTINUE                                                              1514
      IF (RFM .NE. 1)GO TO 55480                                            1515
           I = FSZ                                                          1516
      GO TO 55470                                                           1517
55480 CONTINUE                                                                  
           I = LIB$EXTZV(0,8,BUFFER(PBUF))                                  1518
55470 CONTINUE                                                              1519
      IF (.NOT.(I))GO TO 55500                                              1520
           K = 1                                                            1521
      GO TO 55490                                                           1522
55500 CONTINUE                                                                  
           K = 0                                                            1523
55490 CONTINUE                                                              1524
      IF (.NOT.((PBUF + I + K - 1) .GT. NBUF .OR. (RFM .NE. 1 .AND.         1525
     X          (PBUF + K + I + 1) .GT. NBUF)))GO TO 55520                      
      IF (BLK .GT. EBK)GO TO 55421                                          1527
       J=PBUF                                                               1528
      GO TO 55529                                                               
55530  J= J+(1)                                                                 
      IF ( J.GT.NBUF)GO TO 55531                                                
55529 CONTINUE                                                                  
            BUFFER(J-PBUF+1) = BUFFER(J)                                    1529
      GO TO 55530                                                           1530
55531 CONTINUE                                                                  
           IBLK = BLK                                                       1531
           CALL BLKIN(BLK,J,BUFFER(NBUF-PBUF+2))                            1532
      IF (J .NE. 0)GO TO 55550                                              1533
C     EXECUTE (READ_ERROR)                                                  1534
      ASSIGN 55560 TO KKK060                                                1534
      GO TO 79840                                                               
55560 CONTINUE                                                                  
55550 CONTINUE                                                              1535
      IF (.NOT.(PBUF .GT. NBUF .AND. RFM .NE. 1))GO TO 55580                1536
             I = LIB$EXTZV(0,8,BUFFER(1))                                   1537
      IF (.NOT.(I))GO TO 55600                                              1538
               K = 1                                                        1539
      GO TO 55590                                                           1540
55600 CONTINUE                                                                  
               K = 0                                                        1541
55590 CONTINUE                                                              1542
55580 CONTINUE                                                              1543
           INDSFT = PBUF - NBUF - 1                                         1544
           NBUF = NBUF - PBUF + J + 1                                       1545
           PBUF = 1                                                         1546
55520 CONTINUE                                                              1547
         J = PBUF + INDSFT                                                  1548
      IF (RFM .EQ. 1)GO TO 55620                                            1549
           PBUF = PBUF + 2 + FSZ                                            1550
           I = I - FSZ                                                      1551
55620 CONTINUE                                                              1552
      IF (RFLAG)GO TO 55640                                                 1553
      IF (.NOT.(RAT .EQ. 2 .OR. RAT .EQ. -2))GO TO 55660                    1554
      IF (BUFFER(PBUF) .NE. '0')GO TO 55680                                 1555
               NUMBER = NUMBER - 1                                          1556
      GO TO 55670                                                           1557
55680 CONTINUE                                                                  
               IF (BUFFER(PBUF) .EQ. '1') NUMBER = NUMBER - 2               1558
55670 CONTINUE                                                              1559
      GO TO 55650                                                           1560
55660 CONTINUE                                                                  
      IF (.NOT.(RAT .EQ. 3 .OR. RAT .EQ. -3))GO TO 55700                    1561
               IF (LIB$EXTZV(7,1,BUFFER(PBUF-2)) .EQ. 0)                    1562
     1             NUMBER = NUMBER - LIB$EXTZV(0,7,BUFFER(PBUF-2))              
               IF (LIB$EXTZV(7,1,BUFFER(PBUF-1)) .EQ. 0)                    1564
     1             NUMBER = NUMBER - LIB$EXTZV(0,7,BUFFER(PBUF-1))              
55700 CONTINUE                                                              1566
55650 CONTINUE                                                              1567
55640 CONTINUE                                                              1568
         CALL PRTOUT(I,BUFFER(PBUF))                                        1569
         IF (.NOT.RFLAG) NUMBER = NUMBER - 1                                1570
         PBUF = PBUF + I + K                                                1571
      IF ((.NOT.RFLAG .AND. NUMBER .LE. 0) .OR. (RFLAG .AND.                1572
     X    (IBLK .GT. IENDPB .OR. (IBLK .LE. IENDPB .AND.                        
     X    J .GE. (IENDPP + 512*(IENDPB - IBLK))))))GO TO 55421                  
      GO TO 55419                                                           1575
55421 CONTINUE                                                                  
        CALL PRTCLS                                                         1576
C     EXECUTE (RESTORE_HELD_SETUP)                                          1577
      ASSIGN 55710 TO KKK054                                                1577
      GO TO 79834                                                               
55710 CONTINUE                                                                  
C     EXECUTE (FREE_FLAGS_EXIT)                                             1578
      ASSIGN 55720 TO KKK064                                                1578
      GO TO 79844                                                               
55720 CONTINUE                                                                  
      GO TO KKK056                                                          1579
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (QUEUE_STOPPER)                                             1580
79828 CONTINUE                                                              1580
CP    This procedure sets up an AST to allow the user to stop an                
CP    operation.                                                                
        STOPIT = .FALSE.                                                    1583
        CALL LIB$GET_EF(IOFLAG)                                             1584
      IF (IOFLAG .NE. -1)GO TO 55740                                        1585
          CALL LIB$PUT_SCREEN('Unable to allocate necessary event '//       1586
     1                        'flag.  OPERATION ABORTED.',COMLIN,20)            
C     EXECUTE (ERROR_EXIT)                                                  1588
      ASSIGN 55750 TO KKK024                                                1588
      GO TO 79804                                                               
55750 CONTINUE                                                                  
55740 CONTINUE                                                              1589
        CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),IOSB,          1590
     1               STOPSC,,INBUF,%VAL(1),,,,)                                 
      GO TO KKK048                                                          1592
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (READ_ERROR)                                                1593
79840 CONTINUE                                                              1593
CP    This procedure terminates program execution with an error message.        
CP    It is used when an input error occurs.                                    
        CALL RESTRM(INCHAN)                                                 1596
        CALL BLKCLS                                                         1597
        CLOSE (UNIT=9)                                                      1598
        CALL SYS$DASSGN(%VAL(INCHAN))                                       1599
        CALL LIB$PUT_OUTPUT(CRLF//'ERROR READING FILE')                     1600
        CALL EXIT                                                           1601
      GO TO KKK060                                                          1602
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (REFRESH_EXIT)                                              1603
79816 CONTINUE                                                              1603
CP    This procedure refreshes the screen (puts up a new set of lines           
CP    corresponding to the current line position), resets the LEDs (#3          
CP    off).                                                                     
        CALL WORKNG(.FALSE.)                                                1607
      IF (NLINES .GE. PAGEND)GO TO 55770                                    1608
55779 CONTINUE                                                              1609
      IF (BLK .LE. EBK)GO TO 55800                                          1610
             PAGEND = NLINES                                                1611
      GO TO 55781                                                           1612
55800 CONTINUE                                                              1613
             CALL BLKIN(BLK,I,BUFFER(NBUF+1))                               1614
      IF (I .NE. 0)GO TO 55820                                              1615
C     EXECUTE (READ_ERROR)                                                  1616
      ASSIGN 55830 TO KKK060                                                1616
      GO TO 79840                                                               
55830 CONTINUE                                                                  
55820 CONTINUE                                                              1617
             NBUF = NBUF + I                                                1618
             PBUF = 1                                                       1619
             CALL LINCON(.FALSE.)                                           1620
      IF (NLINES .GE. PAGEND)GO TO 55781                                    1621
      GO TO 55779                                                           1623
55781 CONTINUE                                                                  
55770 CONTINUE                                                              1624
        CALL LIB$ERASE_PAGE(1,1)                                            1625
       I=LINEP                                                              1626
      GO TO 55839                                                               
55840  I= I+(1)                                                                 
      IF ( I.GT.PAGEND)GO TO 55841                                              
55839 CONTINUE                                                                  
      IF (LENLMT .LE. 0)GO TO 55860                                         1627
      IF (LENLMT .LE. LINLEN(I))GO TO 55880                                 1628
             CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)                1629
      GO TO 55870                                                           1630
55880 CONTINUE                                                                  
             CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT)//CRLF)                   1631
55870 CONTINUE                                                              1632
      GO TO 55850                                                           1633
55860 CONTINUE                                                                  
      IF (LINLEN(I) .GT. -LENLMT)GO TO 55900                                1634
             CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)                1635
      GO TO 55890                                                           1636
55900 CONTINUE                                                                  
             CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//                 1637
     1                           LINES(I)(1-LENLMT:LINLEN(I))//CRLF)            
55890 CONTINUE                                                              1639
55850 CONTINUE                                                              1640
      GO TO 55840                                                           1641
55841 CONTINUE                                                                  
C     EXECUTE (WRITE_BLOCK_NUMBER)                                          1642
      ASSIGN 55910 TO KKK062                                                1642
      GO TO 79842                                                               
55910 CONTINUE                                                                  
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                         1643
      ASSIGN 55920 TO KKK046                                                1643
      GO TO 79826                                                               
55920 CONTINUE                                                                  
      GO TO KKK036                                                          1644
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (RESTORE_HELD_SETUP)                                        1645
79834 CONTINUE                                                              1645
CP    This routine is used in searches.  It restores the position and           
CP    arrays that have previously been saved by HOLD_CURRENT_SETUP.             
        CALL WORKNG(.FALSE.)                                                1648
        NBUF = HNBUF                                                        1649
        PBUF = HPBUF                                                        1650
        NLINES = HLINES                                                     1651
        LINEP = HLINEP                                                      1652
       I=1                                                                  1653
      GO TO 55929                                                               
55930  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 55931                                              
55929 CONTINUE                                                                  
         LINES(I) = HLIN(I)                                                 1654
         LINLEN(I) = HLINLN(I)                                              1655
         CCLINE(1,I) = HCCLIN(1,I)                                          1656
         CCLINE(2,I) = HCCLIN(2,I)                                          1657
      GO TO 55930                                                           1658
55931 CONTINUE                                                                  
      IF (NBUF .EQ. 0)GO TO 55950                                           1659
       I=1                                                                  1660
      GO TO 55959                                                               
55960  I= I+(1)                                                                 
      IF ( I.GT.NBUF)GO TO 55961                                                
55959 CONTINUE                                                                  
           BUFFER(I) = HBUFER(I)                                            1661
      GO TO 55960                                                           1662
55961 CONTINUE                                                                  
55950 CONTINUE                                                              1663
        BLK = HBLK                                                          1664
        SBLK = HSBLK                                                        1665
        LBLK = HLBLK                                                        1666
        LPBUF = HLPBUF                                                      1667
      GO TO KKK054                                                          1668
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SCROLL_BLANK_LINE_POST)                                    1669
79858 CONTINUE                                                              1669
CP    This routine is used in scrolling backwards in non-FIXfiles.  It prints   
CP    a blank line then waits for one timer interval (which is set by the       
CP    scrolling speed), and then checks for the stop signal.                    
C     EXECUTE (SCROLL_DOWN)                                                 1673
      ASSIGN 55970 TO KKK070                                                1673
      GO TO 79850                                                               
55970 CONTINUE                                                                  
C     EXECUTE (WAIT_FOR_SCROLL_AST)                                         1674
      ASSIGN 55980 TO KKK072                                                1674
      GO TO 79852                                                               
55980 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 56000                                         1675
C     EXECUTE (VAR_BACK_SCROLL_EXIT)                                        1676
      ASSIGN 56010 TO KKK082                                                1676
      GO TO 79862                                                               
56010 CONTINUE                                                                  
56000 CONTINUE                                                              1677
      GO TO KKK078                                                          1678
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SCROLL_BLANK_LINE_PRE)                                     1679
79856 CONTINUE                                                              1679
CP    This routine is used in scrolling backward in FIX files.  It waits        
CP    one timer interval (set by scrolling speed), checks for the stop          
CP    signal, then writes one blank line.                                       
C     EXECUTE (WAIT_FOR_SCROLL)                                             1683
      ASSIGN 56020 TO KKK074                                                1683
      GO TO 79854                                                               
56020 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 56040                                         1684
C     EXECUTE (BACK_SCROLL_EXIT)                                            1685
      ASSIGN 56050 TO KKK058                                                1685
      GO TO 79838                                                               
56050 CONTINUE                                                                  
56040 CONTINUE                                                              1686
C     EXECUTE (SCROLL_DOWN)                                                 1687
      ASSIGN 56060 TO KKK070                                                1687
      GO TO 79850                                                               
56060 CONTINUE                                                                  
        CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),IOSB,          1688
     1               STOPSC,,INBUF,%VAL(1),,,,)                                 
      GO TO KKK076                                                          1690
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SCROLL_BLANK_LINE_VAR_PRE)                                 1691
79860 CONTINUE                                                              1691
CP    This routine is used in scrolling backward in non-FIX files.  It          
CP    waits for one timer interval (set by scrolling speed), checks for         
CP    the stop signal, and then writes one blank line.                          
C     EXECUTE (WAIT_FOR_SCROLL)                                             1695
      ASSIGN 56070 TO KKK074                                                1695
      GO TO 79854                                                               
56070 CONTINUE                                                                  
      IF (.NOT.(STOPIT))GO TO 56090                                         1696
C     EXECUTE (VAR_BACK_SCROLL_EXIT)                                        1697
      ASSIGN 56100 TO KKK082                                                1697
      GO TO 79862                                                               
56100 CONTINUE                                                                  
56090 CONTINUE                                                              1698
C     EXECUTE (SCROLL_DOWN)                                                 1699
      ASSIGN 56110 TO KKK070                                                1699
      GO TO 79850                                                               
56110 CONTINUE                                                                  
        CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),IOSB,          1700
     1               STOPSC,,INBUF,%VAL(1),,,,)                                 
      GO TO KKK080                                                          1702
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SCROLL_DOWN)                                               1703
79850 CONTINUE                                                              1703
CP    This procedure scrolls the screen down one line and leaves the            
CP    cursor at the upper left hand corner.                                     
        CALL SYS$CANCEL(%VAL(INCHAN))                                       1706
        CALL LIB$SET_CURSOR(1,1)                                            1707
        CALL LIB$DOWN_SCROLL()                                              1708
        CALL LIB$SET_CURSOR(1,1)                                            1709
      GO TO KKK070                                                          1710
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SCROLL_REFRESH_EXIT)                                       1711
79814 CONTINUE                                                              1711
CP    This procedure scrolls to the proper line, resets the LED's (#3           
CP    off).                                                                     
        CALL WORKNG(.FALSE.)                                                1714
      IF (NUMBER .LE. 0)GO TO 56130                                         1715
C                                                                               
C***   Forward scrolling                                                        
C                                                                               
      IF (OLDEND .EQ. PAGEND)GO TO 56150                                    1719
C                                                                               
C---   Make sure there is something to do                                       
C                                                                               
C---   Get extra lines if necessary                                             
C                                                                               
      IF (NLINES .GE. PAGEND)GO TO 56170                                    1725
56179 CONTINUE                                                              1726
      IF (BLK .LE. EBK)GO TO 56200                                          1727
                 PAGEND = NLINES                                            1728
      GO TO 56181                                                           1729
56200 CONTINUE                                                              1730
                 CALL BLKIN(BLK,I,BUFFER(NBUF+1))                           1731
      IF (I .NE. 0)GO TO 56220                                              1732
C     EXECUTE (READ_ERROR)                                                  1733
      ASSIGN 56230 TO KKK060                                                1733
      GO TO 79840                                                               
56230 CONTINUE                                                                  
56220 CONTINUE                                                              1734
                 NBUF = NBUF + I                                            1735
                 PBUF = 1                                                   1736
                 CALL LINCON(.FALSE.)                                       1737
      IF (NLINES .GE. PAGEND)GO TO 56181                                    1738
      GO TO 56179                                                           1740
56181 CONTINUE                                                                  
56170 CONTINUE                                                              1741
C                                                                               
C---   Output new lines                                                         
C                                                                               
       I=PAGLEN+1                                                           1745
      GO TO 56239                                                               
56240  I= I+(1)                                                                 
      IF ( I.GT.COMLIN)GO TO 56241                                              
56239 CONTINUE                                                                  
             CALL LIB$ERASE_LINE(I,1)                                       1746
      GO TO 56240                                                           1747
56241 CONTINUE                                                                  
            CALL LIB$SET_CURSOR(PAGLEN+1,1)                                 1748
       I=OLDEND+1                                                           1749
      GO TO 56249                                                               
56250  I= I+(1)                                                                 
      IF ( I.GT.PAGEND)GO TO 56251                                              
56249 CONTINUE                                                                  
      IF (LENLMT .LE. 0)GO TO 56270                                         1750
      IF (LINLEN(I) .LT. LENLMT)GO TO 56290                                 1751
                 CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT)//CRLF)               1752
      GO TO 56280                                                           1753
56290 CONTINUE                                                                  
                 CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)            1754
56280 CONTINUE                                                              1755
      GO TO 56260                                                           1756
56270 CONTINUE                                                                  
      IF (LINLEN(I) .GT. -LENLMT)GO TO 56310                                1757
                 CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)            1758
      GO TO 56300                                                           1759
56310 CONTINUE                                                                  
                 CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//             1760
     1                LINES(I)(1-LENLMT:LINLEN(I))//CRLF)                       
56300 CONTINUE                                                              1762
56260 CONTINUE                                                              1763
      GO TO 56250                                                           1764
56251 CONTINUE                                                                  
56150 CONTINUE                                                              1765
      GO TO 56120                                                           1766
56130 CONTINUE                                                                  
      IF (NUMBER .GE. 0)GO TO 56330                                         1767
C                                                                               
C***   Backward scrolling                                                       
C                                                                               
       I=LINEP-NUMBER-1                                                     1771
      GO TO 56339                                                               
56340  I= I+(-1)                                                                
      IF ( I.LT.LINEP)GO TO 56341                                               
56339 CONTINUE                                                                  
C     EXECUTE (SCROLL_DOWN)                                                 1772
      ASSIGN 56350 TO KKK070                                                1772
      GO TO 79850                                                               
56350 CONTINUE                                                                  
      IF (ABS(LENLMT) .LT. LINLEN(I))GO TO 56370                            1773
               CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I)))                    1774
      GO TO 56360                                                           1775
56370 CONTINUE                                                                  
      IF (LENLMT .GE. 0)GO TO 56390                                         1776
C                                                                               
C---   This assumes that LINLEN<=2*LENLMT                                       
C                                                                               
C     EXECUTE (SCROLL_DOWN)                                                 1780
      ASSIGN 56400 TO KKK070                                                1780
      GO TO 79850                                                               
56400 CONTINUE                                                                  
                 CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//             1781
     1                               LINES(I)(1-LENLMT:LINLEN(I)))              
      GO TO 56380                                                           1783
56390 CONTINUE                                                                  
                 CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT))                     1784
56380 CONTINUE                                                              1785
56360 CONTINUE                                                              1786
      GO TO 56340                                                           1787
56341 CONTINUE                                                                  
56330 CONTINUE                                                              1788
          CALL LIB$SET_CURSOR(PAGLEN+1,1)                                   1789
56120 CONTINUE                                                              1790
C     EXECUTE (WRITE_BLOCK_NUMBER)                                          1791
      ASSIGN 56410 TO KKK062                                                1791
      GO TO 79842                                                               
56410 CONTINUE                                                                  
C     EXECUTE (HOME_PLUS_LEDS_EXIT)                                         1792
      ASSIGN 56420 TO KKK046                                                1792
      GO TO 79826                                                               
56420 CONTINUE                                                                  
      GO TO KKK034                                                          1793
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SEARCH_ERROR_EXIT)                                         1794
79846 CONTINUE                                                              1794
CP    This procedure stops execution of a search command with an error.         
CP    while restoring the held setup and getting rid of the I/O request         
CP    to the terminal                                                           
C     EXECUTE (CANCEL_STOPPER)                                              1798
      ASSIGN 56430 TO KKK032                                                1798
      GO TO 79812                                                               
56430 CONTINUE                                                                  
        IF (VT100A) CALL LIB$PUT_SCREEN(REVVID)                             1799
        JJ = 1                                                              1800
       I=1                                                                  1801
      GO TO 56439                                                               
56440  I= I+(1)                                                                 
      IF ( I.GT.10)GO TO 56441                                                  
56439 CONTINUE                                                                  
      IF (SBFLEN(I) .EQ. 0 .OR. JJ .GT. (IABS(LENLMT) - 58))GO TO 56441     1802
         CAPLIN(JJ:) = SRCHBF(I)(:SBFLEN(I))                                1803
         JJ = SBFLEN(I) + JJ + 2                                            1804
      GO TO 56440                                                           1805
56441 CONTINUE                                                                  
        IF (JJ .GT. (IABS(LENLMT) - 58)) JJ = IABS(LENLMT) - 58             1806
        CALL LIB$PUT_SCREEN(CAPLIN(:JJ-2)//'  Not found',COMLIN,            1807
     1                      IABS(LENLMT)-JJ-34)                                 
        IF (VT100A) CALL LIB$PUT_SCREEN(REGVID,1,1)                         1809
C     EXECUTE (STOP_WITH_ERROR)                                             1810
      ASSIGN 56450 TO KKK088                                                1810
      GO TO 79868                                                               
56450 CONTINUE                                                                  
      GO TO KKK066                                                          1811
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (STOP_WITH_ERROR)                                           1812
79868 CONTINUE                                                              1812
CP    This procedure stops execution of a command with an error exit,           
CP    while restoring the held setup and getting rid of the I/O request         
CP    to the terminal                                                           
C     EXECUTE (CANCEL_STOPPER)                                              1816
      ASSIGN 56460 TO KKK032                                                1816
      GO TO 79812                                                               
56460 CONTINUE                                                                  
C     EXECUTE (RESTORE_HELD_SETUP)                                          1817
      ASSIGN 56470 TO KKK054                                                1817
      GO TO 79834                                                               
56470 CONTINUE                                                                  
C     EXECUTE (ERROR_EXIT)                                                  1818
      ASSIGN 56480 TO KKK024                                                1818
      GO TO 79804                                                               
56480 CONTINUE                                                                  
      GO TO KKK088                                                          1819
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (VAR_BACK_SCROLL_EXIT)                                      1820
79862 CONTINUE                                                              1820
CP    This routine exits from scrolling backwards in non-FIX files.  It         
CP    sets the position to the correct place in the buffer, then converts       
CP    the rest of the buffer, sets the current line position, resets the        
CP    LEDs (#3 off) and returns.                                                
        NBUF = PBUF                                                         1825
        PBUF = I - 1                                                        1826
C     EXECUTE (BACK_SCROLL_EXIT)                                            1827
      ASSIGN 56490 TO KKK058                                                1827
      GO TO 79838                                                               
56490 CONTINUE                                                                  
      GO TO KKK082                                                          1828
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (WAIT_FOR_SCROLL)                                           1829
79854 CONTINUE                                                              1829
CP    This procedure waits for the scrolling interval to pass.                  
        CALL SYS$SETIMR(%VAL(TIMFLG),DELTIM,,)                              1831
        CALL SYS$WAITFR(%VAL(TIMFLG))                                       1832
      GO TO KKK074                                                          1833
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (WAIT_FOR_SCROLL_AST)                                       1834
79852 CONTINUE                                                              1834
CP    This procedure waits for the scrolling interval to pass after             
CP    queueing the stop command AST.                                            
        CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),IOSB,          1837
     1               STOPSC,,INBUF,%VAL(1),,,,)                                 
        CALL SYS$SETIMR(%VAL(TIMFLG),DELTIM,,)                              1839
        CALL SYS$WAITFR(%VAL(TIMFLG))                                       1840
      GO TO KKK072                                                          1841
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (WORKING)                                                   1842
79802 CONTINUE                                                              1842
CP    This procedure turns on LED 1 and starts up the working message.          
      IF (.NOT.(VT100))GO TO 56510                                          1844
          CALL LIB$PUT_SCREEN(CHAR(ESC)//'[0;1q')                           1845
          IF (DIRECT .LT. 0) CALL LIB$PUT_SCREEN(CHAR(ESC)//'[2q')          1846
          IF (RANGE(1,1) .NE. 0 .AND. RANGE(1,2) .NE. 0)                    1847
     1        CALL LIB$PUT_SCREEN(CHAR(ESC)//'[4q')                             
56510 CONTINUE                                                              1849
        CALL WORKNG(.TRUE.)                                                 1850
      GO TO KKK022                                                          1851
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (WRITE_BLOCK_NUMBER)                                        1852
79842 CONTINUE                                                              1852
CP    This procedure scrolls an extra line if needed (if dividing line          
CP    (BOX) switch is chosen, and writes out the block number in the            
CP    lower right corner.                                                       
        IF (PAGLEN .EQ. (COMLIN - 2)) CALL LIB$PUT_SCREEN(CRLF)             1856
        CALL LIB$ERASE_LINE(COMLIN,1)                                       1857
C                                                                               
C***   Output block number                                                      
C                                                                               
        IF (VT100A) CALL LIB$PUT_SCREEN(REVVID)                             1861
       I=LINEP                                                              1862
      GO TO 56519                                                               
56520  I= I+(1)                                                                 
      IF ( I.GT.NLINES)GO TO 56521                                              
56519 CONTINUE                                                                  
      IF (CCLINE(1,I) .NE. 0)GO TO 56521                                    1863
      GO TO 56520                                                           1864
56521 CONTINUE                                                                  
        WRITE (UNIT=TEMPCH,FMT=2000)CCLINE(1,I)                             1865
2000    FORMAT (I5)                                                         1866
        I = LIB$SKPC(' ',TEMPCH)                                            1867
        CALL LIB$PUT_SCREEN('Block '//TEMPCH(I:),COMLIN,                    1868
     1                      IABS(LENLMT)+I-11)                                  
        IF (VT100A) CALL LIB$PUT_SCREEN(REGVID,1,1)                         1870
      GO TO KKK062                                                          1871
      END                                                                   1872
