      SUBROUTINE BRDCST(MESSAGE)                                               1
C                                                                               
C***********************************************************************        
C*****************************   BRDCST   ******************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine catches broadcast messages and stores them away in        
CDM   common.                                                                   
CDM                                                                             
CDM   Programmer - Jon Vavrus   4 APR 1983                                      
CM                                                                              
CM         This routine is called as an AST whenever a broadcast message        
CM    comes through to the terminal.  That message is then stored away          
CM    in the common area BRDCM1.  A flag is set and a count is updated          
CM    in BRDCM2.                                                                
CM                                                                              
CM         A message is also output on to the screen.                           
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
CS    DOITNM  Contains information transferred from the main program:           
CS                 INCHAN - input channel of the terminal                       
CS                 TIME   - scrolling speed in -10000000*seconds/line           
CS                 SBFFLG - flag indicating whether there is a string           
CS                          in the search buffer                                
CS                 DIRECT - flag for reverse (-1 for reverse, else 1)           
CS                 EBK    - EBK of input file                                   
CS                 SBFLEN - length of string(s) in search buffer(s)             
CS                 RECBLK - number of bytes used per block in FIX-BLK files     
CS                 RANGE  - starting and ending block numbers and pointers      
CS                          of the print range.                                 
CS                 VT100  - flag set .TRUE. if terminal is a VT100              
CS                 LRL    - length of longest record in input file              
CS                 LENLMT - maximum length of output (if <0 then WRAP set)      
CS                 PAGEND - Line number of last line output to screen.          
CS                 PAGLEN - number of lines per page                            
CS                 COMLIN - line number for command input (bottom of screen)    
CS                 VT100A - flag set .TRUE. if VT100 has advanced video option  
C                                                                               
      CHARACTER*(*) MESSAGE                                                   49
      CHARACTER*128 BRDMSG(20)                                                50
      CHARACTER*22 BRDNOT                                                     51
      INTEGER*4 TIME,EBK,RANGE(2,2),PAGEND                                    52
      INTEGER*2 DIRECT,SBFLEN(10),RECBLK,LENLMT,PAGLEN,COMLIN,                53
     1          BRDCNT,BRDLEN(20)                                               
      LOGICAL*1 SBFFLG,VT100,VT100A,BRDFLG                                    55
                                                                              56
      COMMON /BRDCM1/BRDNOT,BRDMSG                                            57
      COMMON /BRDCM2/BRDLEN,BRDCNT,BRDFLG                                     58
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,       59
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
                                                                              61
      BRDCNT = BRDCNT + 1                                                     62
      IF (BRDCNT .GT. 20)GO TO 50020                                          63
        BRDFLG = .TRUE.                                                       64
        BRDMSG(BRDCNT) = MESSAGE                                              65
        BRDLEN(BRDCNT) = LEN(MESSAGE)                                         66
      GO TO 50010                                                             67
50020 CONTINUE                                                                  
      DO 50030  I=2,20                                                        68
         BRDMSG(I-1) = BRDMSG(I)                                              69
         BRDLEN(I-1) = BRDLEN(I)                                              70
50030 CONTINUE                                                                71
        BRDMSG(20) = MESSAGE                                                  72
        BRDLEN(20) = LEN(MESSAGE)                                             73
50010 CONTINUE                                                                74
      IF (.NOT.(VT100))GO TO 50050                                            75
        CALL LIB$PUT_SCREEN(CHAR(27)//'7')                                    76
        CALL LIB$PUT_SCREEN(BRDNOT,COMLIN,IABS(LENLMT)-28)                    77
50050 CONTINUE                                                                78
      RETURN                                                                  79
      END                                                                     80
                                                                               
      SUBROUTINE CONVUP(LENGTH,LOWER,UPPER)                                   82
C                                                                               
C***********************************************************************        
C*****************************   CONVUP   ******************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine converts a string from lower to upper case.               
CDM   Both of the strings are passed in non-descriptor form.                    
CDM                                                                             
CDM   Programmer - Jon Vavrus  13 NOV 1980                                      
CM                                                                              
CM         The routine takes a string of bytes starting at the location         
CM    given for LOWER and converts any alphabetic characters in the first       
CM    LENGTH bytes to upper-case, returning the converted string of bytes       
CM    starting at the location given for UPPER.                                 
C                                                                               
      INTEGER*2 LENGTH                                                        98
      LOGICAL*1 LOWER(1),UPPER(1)                                             99
                                                                             100
      DO 50010  I=1,LENGTH                                                   101
C                                                                               
C***   141 octal is lowercase a in ASCII, 172 octal is lowercase z in           
C***   ASCII.  40 octal is the spacing between the upper and lowercase          
C***   alphabets in ASCII.                                                      
C                                                                               
      IF (.NOT.(LOWER(I) .GE. '141'O .AND. LOWER(I) .LE. '172'O))GO TO 5     107
     X0030                                                                      
         UPPER(I) = LOWER(I) - '40'O                                         108
      GO TO 50020                                                            109
50030 CONTINUE                                                                  
         UPPER(I) = LOWER(I)                                                 110
50020 CONTINUE                                                               111
50010 CONTINUE                                                               112
      RETURN                                                                 113
      END                                                                    114
                                                                               
      SUBROUTINE EXITR                                                       116
C                                                                               
C***********************************************************************        
C*******************************   EXITR   *****************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine is invoked on image exit and cancels any                  
CDM   AST, and closes the file and re-stores the terminal.                      
CDM                                                                             
CDM   Programmer - Jon Vavrus  13 JAN 1983                                      
CM                                                                              
CM    Should be set up by a call to SYS$DCLEXH(BLOCK) where block               
CM    is a 4 longword array with this routines address as the second            
CM    longword, 1 as the third longword, and the address of any                 
CM    writable location (longword) as the fourth.                               
C                                                                               
C***   COMMON STORAGE                                                           
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
CS                 RANGE  - starting and ending block numbers and pointers      
CS                          of the print range.                                 
CS                 VT100  - flag set .TRUE. if terminal is a VT100              
C                                                                               
CS    STOPCM  Used to pass information back and forth to/from the stop          
CS            scrolling AST.  STOPIT is a flag set .TRUE. if scrolling          
CS            should be stopped, INBUF is the input character, IOFLAG and       
CS            TIMFLG are event flags, and IOSB is the I/O IOSB.                 
C                                                                               
      INTEGER*4 TIMFLG,TIME,EBK,RANGE(2,2)                                   152
      INTEGER*2 IOSB(4),DIRECT,SBFLEN(10),RECBLK                             153
      LOGICAL*1 STOPIT,INBUF,SBFFLG,VT100                                    154
                                                                             155
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      156
     1               VT100                                                      
      COMMON /STOPCM/STOPIT,INBUF,IOFLAG,IOSB,TIMFLG                         158
                                                                             159
      IF (IOFLAG .LE. 0)GO TO 50020                                          160
        CALL SYS$CANCEL(%VAL(INCHAN))                                        161
        CALL LIB$FREE_EF(IOFLAG)                                             162
50020 CONTINUE                                                               163
      CALL WORKNG(.FALSE.)                                                   164
      CALL RESTRM(INCHAN)                                                    165
      CALL BLKCLS                                                            166
      IF (VT100) CALL LIB$PUT_SCREEN(CHAR(27)//'[0q')                        167
      CALL SYS$DASSGN(%VAL(INCHAN))                                          168
      RETURN                                                                 169
      END                                                                    170
                                                                               
      SUBROUTINE INTASC(LENGTH,DATA,STRING)                                  172
C                                                                               
C***********************************************************************        
C******************************   INTASC   *****************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine converts a string of ascii coded bytes to a character     
CDM   variable                                                                  
CDM                                                                             
CDM   Programmer - Jon Vavrus    JUL 1980                                       
CM                                                                              
CM         The subroutine is called with LENGTH (the number of bytes to be      
CM    converted), DATA (the starting location of the data), and STRING (a       
CM    character variable to recieve the translation).  After translation the    
CM    LENGTH+1 character of STRING is set to a blank.                           
C                                                                               
      CHARACTER*(*) STRING                                                   188
      INTEGER*2 LENGTH                                                       189
      LOGICAL*1 DATA(200)                                                    190
                                                                             191
      DECODE (LENGTH,100,DATA)STRING                                         192
100   FORMAT (A<LENGTH>)                                                     193
      STRING(LENGTH+1:LENGTH+1) = ' '                                        194
      RETURN                                                                 195
      END                                                                    196
                                                                               
      SUBROUTINE REDOIT(COMAND,NUMFLG,TANUMB,MINFLG)                         198
C                                                                               
C***********************************************************************        
C*****************************   REDOIT   ******************************        
C***********************************************************************        
C                                                                               
CDM   This routine calls DOIT                                                   
CDM                                                                             
CDM   Programmer - Jon Vavrus  26 AUG 1981                                      
CM                                                                              
CM         The routine simply calls DOIT with the same argument list.           
CM    This allows DOIT to call itself.                                          
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
C     DOIT    Executes commands                                                 
C                                                                               
      INTEGER*4 COMAND                                                       215
      LOGICAL*1 MINFLG,NUMFLG                                                216
                                                                             217
      CALL DOIT(COMAND,NUMFLG,TANUMB,MINFLG)                                 218
      RETURN                                                                 219
      END                                                                    220
                                                                               
      SUBROUTINE SFTLIN(LINES)                                               222
C                                                                               
C***********************************************************************        
C*****************************   SFTLIN   ******************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine shifts the output lines stored in the line storage        
CDM   arrays in order to make room at the end of the arrays for more lines.     
CDM                                                                             
CDM    Programmer - Jon Vavrus  13 NOV 1980                                     
CDM    Revised    - Jon Vavrus  12 MAY 1981                                     
CDM    Revised    - Jon Vavrus  29 JUL 1981 (change CCLINE usage)               
CDM    Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                     
CM                                                                              
CM         The routine requires LINES (the character array of actual            
CM    output lines) to be passed as an argument, all the other arrays are       
CM    passed through common area LNCN.  The normal shift is to make room        
CM    for 100 more lines at the end, however, if there are less then 500        
CM    lines already then the shift frees up 599 of the 600 lines.  When         
CM    the shift occurs lines shifted off the front of the arrays are lost.      
C                                                                               
C***   COMMON AREAS                                                             
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
      CHARACTER*132 LINES(600)                                               267
      INTEGER*4 RFM,RAT,FSZ,PBUF,BLK,SBLK,CCLINE(2,600)                      268
      INTEGER*2 LINLEN(600)                                                  269
      LOGICAL*1 BUFFER(3750)                                                 270
                                                                             271
      COMMON /LNCN/LINLEN,LINEP,NLINES,BUFFER,PBUF,NBUF,BLK,FSZ,IFSZ,        272
     1             RFM,RAT,SBLK,CCLINE,LBLK,LPBUF                               
                                                                             274
      IF (LINEP .LE. 500)GO TO 50020                                         275
        J = 500                                                              276
      GO TO 50010                                                            277
50020 CONTINUE                                                                  
        J = LINEP - 1                                                        278
50010 CONTINUE                                                               279
      DO 50030  I=J,600                                                      280
       LINLEN(I-J+1) = LINLEN(I)                                             281
       LINES(I-J+1) = LINES(I)                                               282
       CCLINE(1,I-J+1) = CCLINE(1,I)                                         283
       CCLINE(2,I-J+1) = CCLINE(2,I)                                         284
50030 CONTINUE                                                               285
      DO 50040  I=1,J-1                                                      286
      IF (CCLINE(1,I) .EQ. 0)GO TO 50060                                     287
         SBLK = CCLINE(1,I)                                                  288
      GO TO 50041                                                            289
50060 CONTINUE                                                               290
50040 CONTINUE                                                               291
50041 CONTINUE                                                                  
      LINEP = LINEP - J + 1                                                  292
      NLINES = 600 - J + 1                                                   293
      RETURN                                                                 294
      END                                                                    295
                                                                               
      SUBROUTINE STOPSC                                                      297
C                                                                               
C***********************************************************************        
C*****************************   STOPSC   ******************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine is the AST used to stop the scrolling function.           
CDM                                                                             
CDM   Programmer - Jon Vavrus  13 NOV 1980                                      
CDM   Revised    - Jon Vavrus  12 MAY 1981                                      
CDM   Revised    - Jon Vavrus  26 AUG 1981                                      
CDM   Revised    - Jon Vavrus   6 NOV 1981                                      
CDM   Revised    - Jon Vavrus   4 MAY 1982 (non-advanced video VT100's)         
CM                                                                              
CM         The routine is executed whenever anything is typed while a           
CM    scroll is in progress.  If the character typed is "(" or a symbol         
CM    defined as "(", all timer requests are cancelled, the timer event         
CM    flag is set, and STOPIT is set to .TRUE..  If the character typed         
CM    is not one of the above, then the I/O is requeued with STPSC2 as          
CM    its AST (STPSC2 just calls this routine), and the character is in-        
CM    serted into the input buffer.                                             
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
C                                                                               
CS    STOPCM  Used to pass information back and forth to/from the main          
CS            process.  STOPIT is a flag set .TRUE. if scrolling should         
CS            be stopped, INBUF is the input character, IOFLAG and TIMFLG       
CS            are event flags, and IOSB is the I/O IOSB.                        
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
CS                 RANGE  - starting and ending block numbers and pointers      
CS                          of the print range.                                 
CS                 VT100  - flag set .TRUE. if terminal is a VT100              
CS                 LRL    - length of longest record in input file              
CS                 LENLMT - maximum length of output (if <0 then WRAP set)      
CS                 PAGEND - Line number of last line output to screen.          
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
C***   INPUT VARIABLES                                                          
C                                                                               
CI    INBUF   One byte buffer used for the QIO input request.                   
C                                                                               
C***   INPUT UNIT                                                               
C                                                                               
CR    INCHAN  Channel number of the terminal.                                   
C                                                                               
      EXTERNAL STPSC2                                                        362
      CHARACTER*128 INLINE                                                   363
      CHARACTER*30 DEFBUF(10,10,60)                                          364
      CHARACTER*1 SYMS(60)                                                   365
      REAL*4 NUMDEF(10,60)                                                   366
      INTEGER*4 TIMFLG,TIME,EBK,RANGE(2,2),PAGEND                            367
      INTEGER*2 IOSB(4),DIRECT,SBFLEN(10),RECBLK,DEFS(11,10,60),LENLMT,      368
     1          LENSAV,PAGLEN,COMLIN                                            
      LOGICAL*1 INBUF,STOPIT,ERROUT(5),SBFFLG,VT100,DEFFLG(3,10,60),         370
     1          VT100A                                                          
                                                                             372
      COMMON /STOPCM/STOPIT,INBUF,IOFLAG,IOSB,TIMFLG                         373
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      374
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                      376
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                         377
                                                                             378
      DATA ERROUT/7,27,'[','3','q'/                                          379
      IF (.NOT.(IOSB(1) .NE. '830'X .AND. IOSB(1) .NE. '2C'X))GO TO 5002     380
     X0                                                                         
C                                                                               
C***   Is the character a (                                                     
C                                                                               
      IF (INBUF .NE. '(')GO TO 50040                                         384
          CALL SYS$CANTIM(,)                                                 385
          STOPIT = .TRUE.                                                    386
          CALL SYS$SETEF(%VAL(TIMFLG))                                       387
      GO TO 50030                                                            388
50040 CONTINUE                                                                  
C                                                                               
C***   Is the character a symbol defined as (                                   
C                                                                               
      DO 50050  I=1,NDEFS                                                    392
      IF (.NOT.(ICHAR(SYMS(I)) .EQ. INBUF .AND. DEFS(1,1,I) .EQ. 6))GO T     393
     XO 50070                                                                   
             CALL SYS$CANTIM(,)                                              394
             STOPIT = .TRUE.                                                 395
             CALL SYS$SETEF(%VAL(TIMFLG))                                    396
             RETURN                                                          397
50070 CONTINUE                                                               398
50050 CONTINUE                                                               399
C                                                                               
C***   If character is not a control character add it to INLINE (input          
C***   buffer)                                                                  
C                                                                               
      IF (.NOT.(INBUF .GE. 32 .AND. INBUF .LE. 126))GO TO 50090              404
            LENSAV = LENSAV + 1                                              405
            INLINE(LENSAV:LENSAV) = CHAR(INBUF)                              406
      GO TO 50080                                                            407
50090 CONTINUE                                                                  
C                                                                               
C***   If it is a <DEL> then remove the last character                          
C                                                                               
      IF (.NOT.(INBUF .EQ. 127 .AND. LENSAV .GE. 1))GO TO 50110              411
              LENSAV = LENSAV - 1                                            412
      GO TO 50100                                                            413
50110 CONTINUE                                                                  
C                                                                               
C***   If it is a ^X or ^U then delete entire buffer                            
C                                                                               
      IF (.NOT.(INBUF .EQ. 24 .OR. INBUF .EQ. 21))GO TO 50130                417
                LENSAV = 0                                                   418
50130 CONTINUE                                                               419
50100 CONTINUE                                                               420
50080 CONTINUE                                                               421
C                                                                               
C***   Requeue request                                                          
C                                                                               
          CALL SYS$QIO(%VAL(IOFLAG),%VAL(INCHAN),%VAL('1171'X),IOSB,         425
     1                 STPSC2,,INBUF,%VAL(1),,,,)                               
50030 CONTINUE                                                               427
50020 CONTINUE                                                               428
      RETURN                                                                 429
      END                                                                    430
                                                                               
      SUBROUTINE STPSC2                                                      432
C                                                                               
C***********************************************************************        
C*****************************   STPSC2   ******************************        
C***********************************************************************        
C                                                                               
CDM   This subroutine is the AST for the I/O request from STOPSC, if            
CDM   invoked it will call STOPSC and exit.                                     
CDM                                                                             
CDM   Programmer - Jon Vavrus  13 NOV 1980                                      
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
C     STOPSC   Actual AST routine.                                              
C                                                                               
      CALL STOPSC                                                            447
      RETURN                                                                 448
      END                                                                    449
                                                                               
      SUBROUTINE TMAST2                                                      451
C                                                                               
C****************************************************************************** 
C*********************************   TMAST2   ********************************* 
C****************************************************************************** 
C                                                                               
C                                                                               
CDM   This routine is simply a means for TIMAST to refer to itself.  All it     
CDM   does is call TIMAST.                                                      
CDM                                                                             
CDM   Programmer  Jon Vavrus -  3 SEP 1982                                      
CDM   Revised     Jon Vavrus - 21 SEP 1983 (changed name with TIMAST)           
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
C     TIMAST    - Routine to actually output the next message.                  
C                                                                               
      CALL TIMAST                                                            468
      RETURN                                                                 469
      END                                                                    470
                                                                               
      SUBROUTINE TIMAST                                                      472
C                                                                               
C****************************************************************************** 
C*********************************   TIMAST   ********************************* 
C****************************************************************************** 
C                                                                               
CDM   This routine outputs the proper "WORKING" message at the bottom of the    
CDM   screen and requeues itself as a timer AST (through TMAST2).               
CDM                                                                             
CDM   Programmer  Jon Vavrus -  8 SEP 1982                                      
CDM   Revised     Jon Vavrus - 21 SEP 1983 (changed name with TMAST2)           
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    TMAST2   - Routine which calls this one.                                  
C                                                                               
C***   COMMON AREAS                                                             
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
CS    TASTNM  Used to pass the ROW and COLUMN for the message, the flag         
CS            ONEFLG, the timer delta time DELTA, and the timer event flag      
CS            TIMREF.                                                           
C                                                                               
      EXTERNAL TMAST2                                                        513
      INTEGER*4 INCHAN,TIME,EBK,RANGE(2,2),PAGEND,TIMREF,DELTA(2)            514
      INTEGER*2 SBFLEN(10),RECBLK,LENLMT,PAGLEN,COMLIN,DIRECT,ROW,COLUMN     515
      LOGICAL*1 SBFFLG,VT100,VT100A,START,ONEFLG                             516
                                                                             517
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      518
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /TASTNM/ROW,COLUMN,ONEFLG,DELTA,TIMREF                          520
                                                                             521
      IF (.NOT.(ONEFLG))GO TO 50020                                          522
      IF (.NOT.(VT100A))GO TO 50040                                          523
          CALL LIB$PUT_SCREEN(CHAR(27)//'[7mWORKING'//CHAR(27)//'[0m',       524
     1                        ROW,COLUMN)                                       
      GO TO 50030                                                            526
50040 CONTINUE                                                                  
          CALL LIB$PUT_SCREEN('WORKING',ROW,COLUMN)                          527
50030 CONTINUE                                                               528
        ONEFLG = .FALSE.                                                     529
      GO TO 50010                                                            530
50020 CONTINUE                                                                  
      IF (.NOT.(VT100A))GO TO 50060                                          531
          CALL LIB$PUT_SCREEN(CHAR(27)//'[1mWORKING'//CHAR(27)//'[0m',       532
     1                        ROW,COLUMN)                                       
      GO TO 50050                                                            534
50060 CONTINUE                                                                  
          CALL LIB$PUT_SCREEN('working',ROW,COLUMN)                          535
50050 CONTINUE                                                               536
        ONEFLG = .TRUE.                                                      537
50010 CONTINUE                                                               538
C                                                                               
C***   Queue AST                                                                
C                                                                               
      CALL SYS$SETIMR(%VAL(TIMREF),DELTA,TMAST2,%VAL(1))                     542
      RETURN                                                                 543
      END                                                                    544
                                                                               
      SUBROUTINE WORKNG(START)                                               546
C                                                                               
C****************************************************************************** 
C*********************************   WORKNG   ********************************* 
C****************************************************************************** 
C                                                                               
CDM   This subroutine puts out the flashing "WORKING" message.                  
CDM                                                                             
CDM   Programmer  Jon Vavrus -  8 SEP 1982                                      
CM                                                                              
CM    The message is displayed at screen position COMLIN(from DOITNM),          
CM    IABS(LENLMT)-19(DOITNM also) if START (logical) is .TRUE., and is         
CM    stopped if START is .FALSE..  The changing of the display is done         
CM    through the use of a system timer AST.                                    
C                                                                               
C***   COMMON AREAS                                                             
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
CS    TASTNM  Used to pass the ROW and COLUMN for the message, the flag         
CS            ONEFLG, the timer delta time DELTA, and the timer event flag      
CS            TIMREF.                                                           
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    TIMAST  Timer AST.                                                        
C                                                                               
      EXTERNAL TIMAST                                                        590
      INTEGER*4 INCHAN,TIME,EBK,RANGE(2,2),PAGEND,TIMREF,DELTA(2)            591
      INTEGER*2 SBFLEN(10),RECBLK,LENLMT,PAGLEN,COMLIN,DIRECT,ROW,COLUMN     592
      LOGICAL*1 SBFFLG,VT100,VT100A,START,ONEFLG                             593
                                                                             594
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      595
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /TASTNM/ROW,COLUMN,ONEFLG,DELTA,TIMREF                          597
                                                                             598
      DATA DELTA/-20000000,-1/                                               599
                                                                             600
      IF (.NOT.(START))GO TO 50020                                           601
C                                                                               
C***   Set up to start                                                          
C                                                                               
        ONEFLG = .FALSE.                                                     605
        CALL LIB$GET_EF(TIMREF)                                              606
        ROW = COMLIN                                                         607
        COLUMN = IABS(LENLMT) - 19                                           608
C                                                                               
C***   Output message                                                           
C                                                                               
      IF (.NOT.(VT100A))GO TO 50040                                          612
          CALL LIB$PUT_SCREEN(CHAR(27)//'[7mWORKING'//CHAR(27)//'[0m',       613
     1                        ROW,COLUMN)                                       
      GO TO 50030                                                            615
50040 CONTINUE                                                                  
          CALL LIB$PUT_SCREEN('WORKING',ROW,COLUMN)                          616
50030 CONTINUE                                                               617
C                                                                               
C***   Queue AST                                                                
C                                                                               
        CALL SYS$SETIMR(%VAL(TIMREF),DELTA,TIMAST,%VAL(1))                   621
      GO TO 50010                                                            622
50020 CONTINUE                                                                  
      IF (TIMREF .LE. 0)GO TO 50060                                          623
C                                                                               
C***   Make sure not invoked at wrong time by EXITR                             
C                                                                               
C***   Done working, cancel timer requests and clear message                    
C                                                                               
          CALL SYS$CANTIM(%VAL(1),)                                          629
          CALL LIB$PUT_SCREEN('       ',ROW,COLUMN)                          630
          CALL LIB$SET_CURSOR(1,1)                                           631
          CALL LIB$FREE_EF(TIMREF)                                           632
50060 CONTINUE                                                               633
50010 CONTINUE                                                               634
      RETURN                                                                 635
      END                                                                    636
