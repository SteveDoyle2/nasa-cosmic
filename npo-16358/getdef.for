      LOGICAL FUNCTION GETDEF(LINE,ISYM)                                       1
C                                                                               
CDM   This routine translates a line into a symbol definition                   
CDM                                                                             
CDM   Programmer - Jon Vavrus   3 SEP 1981                                      
CDM   Revised    - Jon Vavrus   4 MAY 1982 (non-advanced video VT100's)         
CDM   Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                      
CDM   Revised    - Jon Vavrus   9 SEP 1982                                      
CDM   Revised    - Jon Vavrus  28 MAY 1983                                      
CDM   Revised    - Jon Vavrus  17 AUG 1983                                      
CM                                                                              
CM         The routine is called with the line to be translated in the          
CM    character variable LINE.  The number of the symbol definition is          
CM    passed as ISYM.                                                           
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    PRSCOM  Parses a command string                                           
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
CS            numbers NUDMEF                                                    
C                                                                               
CS    PRSCHR  Contains the current symbol's search buffers (CURBUF)             
C                                                                               
CS    PRSNUM  Contains the current symbol's definitions (CUR), flags            
CS            (CURFLG), and associated numbers (NUMCUR)                         
C                                                                               
      CHARACTER*(*) LINE                                                      59
      CHARACTER*132 LINES(600)                                                60
      CHARACTER*128 INLINE                                                    61
      CHARACTER*30 DEFBUF(10,10,60),SRCHBF(10),HLDBUF(10),CURBUF(10,10)       62
      CHARACTER*4 REVVID,REGVID                                               63
      CHARACTER*2 CRLF                                                        64
      CHARACTER*1 SYMS(60)                                                    65
      REAL*4 NUMDEF(10,60),NUMCUR(10)                                         66
      INTEGER*4 INCHAN,TIME,EBK,RANGE(2,2),PAGEND                             67
      INTEGER*2 DIRECT,SBFLEN(10),RECBLK,DEFS(11,10,60),LENLMT,PAGLEN,        68
     1          LENSAV,HLDLEN(10),CUR(11,10),COMLIN                             
      LOGICAL*1 SBFFLG,VT100,DEFFLG(3,10,60),HLDFLG,CURFLG(3,10),MINFLG,      70
     1          NUMFLG,VT100A,RESULT                                            
                                                                              72
      COMMON /CHARS/LINES,SRCHBF,CRLF,REVVID,REGVID                           73
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,       74
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                       76
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                          77
      COMMON /PRSNUM/CUR,NUMCUR,CURFLG                                        78
      COMMON /PRSCHR/CURBUF                                                   79
                                                                              80
      RESULT = .TRUE.                                                         81
C                                                                               
C***   Store the current definition in case it is recursive                     
C                                                                               
      DO 50010  JJ=1,10                                                       85
       CUR(1,JJ) = DEFS(1,JJ,ISYM)                                            86
      IF (CUR(1,JJ) .EQ. 0)GO TO 50011                                        87
       NUMCUR(JJ) = NUMDEF(JJ,ISYM)                                           88
       CURFLG(1,JJ) = DEFFLG(1,JJ,ISYM)                                       89
       CURFLG(2,JJ) = DEFFLG(2,JJ,ISYM)                                       90
       CURFLG(3,JJ) = DEFFLG(3,JJ,ISYM)                                       91
      DO 50020  I=1,10                                                        92
        CUR(1+I,JJ) = DEFS(1+I,JJ,ISYM)                                       93
      IF (CUR(1+I,JJ) .EQ. 0)GO TO 50021                                      94
        CURBUF(I,JJ) = DEFBUF(I,JJ,ISYM)                                      95
50020 CONTINUE                                                                96
50021 CONTINUE                                                                  
50010 CONTINUE                                                                97
50011 CONTINUE                                                                  
      NCOM = 1                                                                98
C                                                                               
C***   Save search stuff                                                        
C                                                                               
      DO 50030  JJ=1,10                                                      102
       HLDLEN(JJ) = SBFLEN(JJ)                                               103
       HLDBUF(JJ) = SRCHBF(JJ)                                               104
      IF (SBFLEN(JJ) .EQ. 0)GO TO 50031                                      105
50030 CONTINUE                                                               106
50031 CONTINUE                                                                  
      HLDFLG = SBFFLG                                                        107
      SBFFLG = .FALSE.                                                       108
      LENBUF = LEN(LINE)                                                     109
50039 CONTINUE                                                               110
C                                                                               
C***   Parse out a command                                                      
C                                                                               
       CALL PRSCOM(LINE,LENBUF,ICOM,NUMFLG,ANUMB,MINFLG,NCOM,ISYM)           114
      IF (NCOM .GT. 10)GO TO 50041                                           115
      IF (.NOT.(ICOM .EQ. 0 .AND. LENBUF .GT. 0))GO TO 50060                 116
         RESULT = .FALSE.                                                    117
      GO TO 50041                                                            118
50060 CONTINUE                                                               119
       DEFS(1,NCOM,ISYM) = ICOM                                              120
      IF (ICOM .EQ. 0)GO TO 50041                                            121
       DEFFLG(1,NCOM,ISYM) = NUMFLG                                          122
       NUMDEF(NCOM,ISYM) = ANUMB                                             123
       DEFFLG(2,NCOM,ISYM) = MINFLG                                          124
C                                                                               
C***   Search stuff if command is ^                                             
C                                                                               
      IF (DEFS(1,NCOM,ISYM) .NE. 3)GO TO 50080                               128
      DO 50090  JJ=1,10                                                      129
          DEFBUF(JJ,NCOM,ISYM) = SRCHBF(JJ)                                  130
          DEFS(1+JJ,NCOM,ISYM) = SBFLEN(JJ)                                  131
50090 CONTINUE                                                               132
         DEFFLG(3,NCOM,ISYM) = SBFFLG                                        133
      GO TO 50070                                                            134
50080 CONTINUE                                                                  
         DEFFLG(3,NCOM,ISYM) = .FALSE.                                       135
50070 CONTINUE                                                               136
       NCOM = NCOM + 1                                                       137
      GO TO 50039                                                            138
50041 CONTINUE                                                                  
C                                                                               
C***   Restore search stuff                                                     
C                                                                               
      DO 50100  JJ=1,10                                                      142
       SBFLEN(JJ) = HLDLEN(JJ)                                               143
       SRCHBF(JJ) = HLDBUF(JJ)                                               144
50100 CONTINUE                                                               145
      SBFFLG = HLDFLG                                                        146
      GETDEF = RESULT                                                        147
      RETURN                                                                 148
      END                                                                    149
