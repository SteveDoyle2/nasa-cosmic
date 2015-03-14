      SUBROUTINE DODEF                                                         1
CDM   This routine processes the file open on unit 30 as a "symbol" file.       
CDM                                                                             
CDM   Programmer - Jon Vavrus  15 JUL 1982                                      
CDM   Revised    - Jon Vavrus   9 SEP 1982                                      
CDM   Revised    - Jon Vavrus  17 AUG 1983 (GETDEF changed)                     
CDM   Revised    - Jon Vavrus  21 SEP 1983                                      
CM                                                                              
CM         The routine reads unit 30.  Each line is tested for:                 
CM              s :=     which is taken as a symbol definition                  
CM              {    }   which is taken as a "command" level command line       
CM              or anything else which is taken as a command line.              
C                                                                               
C***   COMMON AREAS                                                             
C                                                                               
CS    INPUTC  Contains the input character buffer INLINE and the defined        
CS            symbols SYMS and the definition search buffers DEFBUF             
C                                                                               
CS    INPUTN  Contains the input character buffer length LENSAV, and the        
CS            symbol definitions DEFS, the number of such definitions           
CS            (NDEFS), the definition flags DEFFLG, and the definition          
CS            numbers NUMDEF                                                    
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    DOIT    Executes a command.                                               
CC    GETDEF  Translates a line into a symbol definition                        
CC    PRSCOM  Parses a command line                                             
C                                                                               
      CHARACTER*1024 LINE                                                     30
      CHARACTER*128 INLINE                                                    31
      CHARACTER*30 DEFBUF(10,10,60)                                           32
      CHARACTER*1 SYMS(60)                                                    33
      REAL*4 NUMDEF(10,60)                                                    34
      INTEGER*2 DEFS(11,10,60),LENSAV                                         35
      LOGICAL GETDEF                                                          36
      LOGICAL*1 DEFFLG(3,10,60)                                               37
                                                                              38
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                       39
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                          40
                                                                              41
  600 CONTINUE                                                                42
50009 CONTINUE                                                                  
       READ (30,700,END=800)LINE                                              43
700    FORMAT (A)                                                             44
      GO TO 50020                                                             45
  800 CONTINUE                                                                46
      GO TO 50011                                                             47
50020 CONTINUE                                                                48
       CALL STR$UPCASE(LINE,LINE)                                             49
       CALL STR$TRANSLATE(LINE,LINE,' ','	')                                  50
       CALL STR$TRIM(LINE,LINE,LENBUF)                                        51
       I = INDEX(LINE(:LENBUF),':=') - 1                                      52
       J = INDEX(LINE(:LENBUF),'"') - 1                                       53
       K = INDEX(LINE(:LENBUF),'''') - 1                                      54
      IF (.NOT.(I .LT. 1 .OR. (J .LT. I .AND. J .GE. 0) .OR. (K .LT. I .      55
     XAND.      K .GE. 0)))GO TO 50040                                          
C                                                                               
C***   If no := then it must be a command line, or a " in front of :=           
C***   (this must mean a command line with a search, or the user screwed up)    
C                                                                               
C---   First test for { }                                                       
C                                                                               
         I = INDEX(LINE(:LENBUF),'{') + 1                                     63
      IF (.NOT.(I .EQ. 1 .OR. (J .LT. I .AND. J .GE. 0)))GO TO 50060          64
50069 CONTINUE                                                                65
            CALL PRSCOM(LINE,LENBUF,ICOM,NUMFLG,ANUMB,MINFLG,0,0)             66
      IF (ICOM .EQ. 0)GO TO 50071                                             67
            CALL DOIT(ICOM,NUMFLG,ANUMB,MINFLG)                               68
      GO TO 50069                                                             69
50071 CONTINUE                                                                  
      GO TO 50050                                                             70
50060 CONTINUE                                                                  
      DO 50080  J=LENBUF,I,-1                                                 71
      IF (LINE(J:J) .EQ. '}')GO TO 50081                                      72
50080 CONTINUE                                                                73
50081 CONTINUE                                                                  
      IF (J .LE. I)GO TO 50100                                                74
             CALL COMLVL(LINE(I:J-1),I)                                       75
             CALL DOIT(-1,.TRUE.,0.,.FALSE.)                                  76
             CALL DOIT(-12,.FALSE.,0.,.FALSE.)                                77
50100 CONTINUE                                                                78
50050 CONTINUE                                                                79
      GO TO 50030                                                             80
50040 CONTINUE                                                                  
50109 CONTINUE                                                                81
C                                                                               
C***   Take care of continuations                                               
C                                                                               
      IF (LINE(LENBUF:LENBUF) .NE. '-')GO TO 50111                            85
          READ (30,700,END=800)LINE(LENBUF:)                                  86
          CALL STR$UPCASE(LINE,LINE)                                          87
          CALL STR$TRANSLATE(LINE,LINE,' ','	')                               88
          CALL STR$TRIM(LINE,LINE,LENBUF)                                     89
      GO TO 50109                                                             90
50111 CONTINUE                                                                  
         J = LIB$SKPC(' ',LINE(:LENBUF))                                      91
C                                                                               
C***   See if symbol is already defined                                         
C                                                                               
  900 CONTINUE                                                                95
      DO 50130  II=1,NDEFS                                                    96
      IF (SYMS(II) .EQ. LINE(J:J))GO TO 50121                                 97
50130 CONTINUE                                                                98
          NDEFS = NDEFS + 1                                                   99
          II = NDEFS                                                         100
          SYMS(II) = LINE(J:J)                                               101
50121 CONTINUE                                                               102
         LINE = LINE(I+3:)                                                   103
         LENBUF = LENBUF - I - 2                                             104
C                                                                               
C***   Actually get definitions                                                 
C                                                                               
      IF (.NOT.GETDEF(LINE(:LENBUF),II))GO TO 50010                          108
50030 CONTINUE                                                               109
50010 GO TO 50009                                                            110
50011 CONTINUE                                                                  
C                                                                               
C***   Close the symbol file                                                    
C                                                                               
      CLOSE (UNIT=30)                                                        114
      RETURN                                                                 115
      END                                                                    116
