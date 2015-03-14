      PROGRAM LOOK                                                             1
CD    This program is designed to look at long printout files.  It has          
CD    the features that it can jump to any place in the file (either back-      
CD    wards or forwards) without having to read in any of the intermediate      
CD    data.  It can also search (either forward or backward) for strings,       
CD    and scroll at different speeds.                                           
CD                                                                              
CD        When run on a VT100 the program is puts the terminal in some          
CD    rather strange modes, so if the program exits abnormally (by ^Y or        
CD    by the system crashing, for example) one should push the SET-UP key       
CD    followed by a shift-R (capital R) to restore the terminals default        
CD    modes.                                                                    
CD                                                                              
CDM        Either of the following will actually start running the program;     
CDM   typing RUN JPL:[JLV.LOOK]LOOK, or defining a symbol to run the            
CDM   program (i.e. LOOK:==$JPL:[JLV.LOOK]LOOK.EXE) and then typing the         
CDM   symbol.  The later method has the advantage that one can enter the        
CDM   name of the file to be looked at and/or the symbol definition file's      
CDM   name (see below) on the command line (for example:                        
CDM                    LOOK PRINT.LIS/DEFINITIONS=LOOKCOM.DAT                   
CDM   will run the program (if LOOK is defined as above) on the file            
CDM   PRINT.LIS with the symbol definitions taken from LOOKCOM.DAT              
CDM   (the DEFINITIONS switch can go before or after the main file's            
CDM   name and can be abbreviated all the way to a D)).  If the switch          
CDM   /NARROW is on the command line, then the screen will not be converted     
CDM   into 132 column mode (this, of course, only applies to VT100 terminals).  
CDM   If the switch /WRAP is present then the lines will be printed with        
CDM   terminal wrapping, instead of being cut to the width of the terminal.     
CDM   If the switch /BOX is present then one line of output is sacrificed       
CDM   to allow a line to be drawn dividing the file text from the command       
CDM   input line.                                                               
CDM                                                                             
CDM        The following commands are the "basic" command set of the            
CDM   program:                                                                  
CDM             n$         - jump by n lines (positive or negative)             
CDM             n%         - jump by n blocks (positive or negative)            
CDM             ^          - search (forward) for the string(s) in the search   
CDM                          buffers (up to 10) and display from that line if   
CDM                          found, if not found ring the terminal bell and     
CDM                          turn on LED 3 (VT100's only).  Matches are found   
CDM                          irregardless of lower/uppercase differences.       
CDM             -^         - search (backward) for string(s), same rules as     
CDM                          forward search.                                    
CDM             - "string" 'string' ... ^                                       
CDM                        - load search buffers with the given strings and     
CDM                          search (backward) for them, same rules as          
CDM                          forward search.                                    
CDM             "string"   - clear the search buffers and save the given        
CDM                          string as the only search string.  A string        
CDM                          can consist of any characters, to include a        
CDM                          quotation mark (") one must put two of them in     
CDM                          a row ("").                                        
CDM             'string' 'string' ...                                           
CDM                        - add the given string(s) to the search buffers.     
CDM                          A string can consist of any characters, to         
CDM                          include a single quote (') one must put two of     
CDM                          them in a row ('').                                
CDM             !          - set reverse switch, all commands will now work     
CDM                          in reverse (i.e. n$ will jump -n lines, etc.)      
CDM             #          - scroll                                             
CDM             -#         - scroll backward                                    
CDM             (          - stop scrolling (only valid command when            
CDM                          scrolling is in operation),                        
CDM                          or cancel current line jump, search, or            
CDM                          & command.                                         
CDM             n)         - set scrolling speed to n lines per second          
CDM                          (positive or negative, integer or real)            
CDM                          (originally set to 2).                             
CDM                          NOTE:  there is a maximum speed that will not      
CDM                          be exceeded (varies with situation) even if        
CDM                          scrolling speed is set at a fast value.            
CDM             n*         - mark the nth line from the top of the screen       
CDM                          as the boundary of the print range.  The print     
CDM                          range is defined by the last two such marked       
CDM                          lines.                                             
CDM             n&         - print the range of lines from the top of the       
CDM                          screen to the nth line from the top of the         
CDM                          screen (n can be either positive or negative).     
CDM                          onto device SYS$PRINT.  if n is 0 or absent        
CDM                          then the range marked with the * command is        
CDM                          used, if available, otherwise 23 is used for       
CDM                          n.  The format of the printout will be that        
CDM                          of the original file, not necessarily what         
CDM                          is shown on the screen (i.e. FORTRAN carriage-     
CDM                          control, and all printer control will work as      
CDM                          they are supposed to).                             
CDM             n-&        - same as n&, except output is put into a file       
CDM                          named LOOKPRINT.LIS in the default directory       
CDM                          (File format will be the same as the original      
CDM                          file, except organization will be sequential)      
CDM                          (appended to the latest version if a previous n-&  
CDM                          or n--& command has been given, otherwise a new    
CDM                          version is created).                               
CDM             n--&       - same as n-&, except always creates a new version   
CDM                          of the output file.                                
CDM             @          - exit the program.                                  
CDM             +          - re-writes the screen.                              
CDM             <          - repeat the last command.                           
CDM                                                                             
CDM        If an invalid command is typed the terminal bell is rung and on      
CDM   a VT100 LED #3 is turned on.  The line is parsed in such a way that a     
CDM   valid command will be executed even if followed by an invalid command.    
CDM   Note that no command is executed until a standard line terminator is      
CDM   received (ASCII code of 1 to 31).  Note that commands are limited to      
CDM   34 characters when entered in response to the prompt.  In order to        
CDM   enter longer commands one can use the "command mode" DEFINE command       
CDM   (see below) and then execute the symbol.  Also all search strings         
CDM   are limited to 30 characters.                                             
CDM                                                                             
CDM        If the user types ^Z (Control-Z) whenever the program is not         
CDM   currently executing a command, the program will enter "command" mode.     
CDM   This mode allows the user to use any of the following commands:           
CDM             DEFINE s := a1 a2 ...                                           
CDM                            - which defines a symbol s in the same man-      
CDM                              ner as a line in the definitions file (see     
CDM                              below), including line continuations.          
CDM             EXIT           - returns to the normal "look" mode.             
CDM             HELP a         - where a is one of the commands shown above.    
CDM             SET [NO]switch - with switch being any of WRAP, BOX, WIDE       
CDM                              or NARROW.  These have the same affect as      
CDM                              the command line switches of the same          
CDM                              names (WIDE is the same as NONARROW),          
CDM                              with the NO qualifier turning them off.        
CDM             SHOW [s]       - where s is a defined symbol.  This will        
CDM                              show what commands a symbol is defined         
CDM                              as. (if s is omitted, all defined symbols      
CDM                              are shown).                                    
CDM             @filename      - Inputs the file filename as a definitions      
CDM                              file.                                          
CDM                                                                             
CDM        The user is also allowed to define symbols for a command or          
CDM   commands.  Valid symbols consist of the characters a-z (lower and         
CDM   upper-case are equivalent), _, =, `, ~, [, ], {, }, ;, :,                 
CDM   ,(comma), >, ?, /, \,and  |.  Symbol definitions are given in             
CDM   a symbol definition file which is read at the start of execution          
CDM   (or by the "command" level @ command), or by the "command" level          
CDM   DEFINE command.  This file is either given by the /DEFINITIONS            
CDM   switch, or a file named LOOKCOM.DAT is searched for in the fol-           
CDM   lowing places:  The default directory (the file is used only if           
CDM   the user owns it), the user's username directory on JPL,                  
CDM   CIT, and SYS2 in that order.                                              
CDM                                                                             
CDM        In the symbol definition file the user can place both commands       
CDM   and definitions.  Commands will be executed on encounter, and def-        
CDM   initions will be placed in the definitons list.  Definitions have         
CDM   the following form:                                                       
CDM                 symbol := command1 command2 command3 ... command9           
CDM   Up to ten commands can be contained in each symbol (exception - a         
CDM   symbol for the stop scrolling command should have only the one command    
CDM   in it ( ( )) and will be executed (when the symbol is typed) in the       
CDM   order they appear in the definition.  A - (minus sign) as the last        
CDM   character in a line means the definition is continued on the next         
CDM   line.  The following are default symbol definitions (they can be          
CDM   overriden by the symbol definition file):                                 
CDM             U := 23$                                                        
CDM             D := -23$                                                       
CDM             S := #                                                          
CDM             Q := (                                                          
CDM             J := 20%                                                        
CDM             B := -20%                                                       
CDM             P := 23&                                                        
CDM             A := 23-&                                                       
CDM             L := 23--&                                                      
CDM             E := @                                                          
CDM             M := *                                                          
CDM             C := +                                                          
CDM             R := <                                                          
CDM                                                                             
CDM   Note, that only the first character of a symbol is significant in the     
CDM   symbol definition file.  Also when input as a command the symbol should   
CDM   be input as only one character.                                           
CDM                                                                             
CDM        A line enclosed by braces ({ }) will be treated as a "command"       
CDM   level command (such as SET WIDE, @filename, etc.).                        
CDM                                                                             
CDM        The LEDs on a VT100 terminal are used to convey various bits of      
CDM   information.  Their meanings are as follows:                              
CDM             1 - on indicates a command is being executed (useful since      
CDM                 some of the commands don't affect the screen until          
CDM                 completion).                                                
CDM             2 - on indicates that reverse is on (an odd number of !         
CDM                 commands have been executed).                               
CDM             3 - on indicates an invalid command was encountered (either     
CDM                 input from the terminal, or while executeing a def-         
CDM                 inition), or a search failed.                               
CDM             4 - on indicates that a range of lines has been marked for      
CDM                 printing                                                    
CDM                                                                             
CDM                                                                             
CDM   NOTE:  If while using the program (on variable length record files)       
CDM          one gets garbage on the screen, try moving either one block        
CDM          forward or backward to realign the record boundaries.  Also        
CDM          when working with FORTRAN carriage-control files the + (plus)      
CDM          carriage-control character is ignored, and when working with       
CDM          printer files, all carriage-control with the exception of          
CDM          line feeds is ignored, and at least one line feed before each      
CDM          record is assumed.                                                 
CDM                                                                             
CDM   Programmer - Jon Vavrus  12 NOV 1980                                      
CDM   Revised    - Jon Vavrus  30 MAR 1981                                      
CDM   Revised    - Jon Vavrus   4 MAY 1981                                      
CDM   Revised    - Jon Vavrus  12 MAY 1981                                      
CDM   Revised    - Jon Vavrus  31 JUL 1981 (changed CCLINE usage)               
CDM   Revised    - Jon Vavrus  27 AUG 1981                                      
CDM   Revised    - Jon Vavrus   3 SEP 1981                                      
CDM   Revised    - Jon Vavrus  30 DEC 1981 (added ^X)                           
CDM   Revised    - Jon Vavrus   4 MAY 1982 (handles non-AVO VT100's)            
CDM   Revised    - Jon Vavrus  15 JUL 1982 (extract some proc's add             
CDM                                         "command" level to symbol file)     
CDM   Revised    - Jon Vavrus  18 AUG 1982 (bigger arrays)                      
CDM   Revised    - Jon Vavrus   9 SEP 1982                                      
CDM   Revised    - Jon Vavrus   4 APR 1983 (broadcast stuff)                    
CDM   Revised    - Jon Vavrus  13 APR 1983                                      
CDM   Revised    - Jon Vavrus  17 MAY 1983                                      
CDM   Revised    - Jon Vavrus  28 MAY 1983                                      
CDM   Revised    - Jon Vavrus   9 SEP 1983                                      
CDM   Revised    - Jon Vavrus  27 SEP 1983                                      
CM                                                                              
CM    ------------------------------------------------------------------------- 
CM                                                                              
CM         The program works by calculating or finding a record boundary        
CM    near the current position and extracting actual records from there.       
CM    This is done for fixed files by calculating the record boundary from      
CM    the start of the file (or start of the block for blocked files),          
CM    keeping in mind the fact that records are extended to an even number      
CM    of bytes (word aligned).  For variable length records a record length     
CM    count word is looked for (a byte containing a number equal to or          
CM    less than 133 plus the record header size (FSZ), followed by a zeroed     
CM    byte), which points to another record length count word.                  
CM                                                                              
CM         When the current line position needs to be converted to a            
CM    position in terms of blocks and bytes, the program calculates the         
CM    byte count from the last line in the output arrays to the current         
CM    line, then using this plus the block number of the next block to be       
CM    read in the position is calculated.  This is fairly straight forward      
CM    for all but fixed blocked files, in which the count of unused bytes       
CM    in each block must be taken into account.  For variable length record     
CM    files a further check on position is made by trying to match the          
CM    line pointed to and the lines around it to a specific area in the         
CM    block.                                                                    
CM                                                                              
CM         Command input is handled by a QIO request to the terminal.  This     
CM    input is terminated by the standard terminators, whihc are not echoed.    
CM    Input is done with conversion to upper case (to make life easier), and    
CM    with no translation of ^U, ^R, and <DEL> (all of which is done by the     
CM    program).  One will note that while scrolling, the QIO is alternately     
CM    requested and cancelled with each output to the terminal, this is done    
CM    so that the terminal will not get simultaneous read and write requests.   
CM                                                                              
CM         Scrolling speed is controlled by the use of the SYS$SETIMR and       
CM    SYS$WAITFR system services.  Thus the QIO AST for the stop scrolling      
CM    command must cancel the timer request, and also set the event flag.       
CM                                                                              
CM         Symbol definitions are kept in several arrays (DEFS - command        
CM    number and length of any search buffers, NUMDEF - associated numbers,     
CM    DEFBUF - search buffers, and DEFFLG - associated command flags).          
CM    The total number of defined symbols is thus limited to 60.                
C                                                                               
C***   CALLED ROUTINES                                                          
C                                                                               
CC    BLKIN   Reads in one block of data from the input file.                   
CC    BLKOPN  Opens a file for block input.                                     
CC    BLKSIN  Reads in 7 blocks of data from the input file.                    
CC    BRDCST  Fields braodcast messages.                                        
CC    COMLVL  Executes "command level" commands.                                
CC    DODEF   Processes a symbol file.                                          
CC    DOIT    Executes a command.                                               
CC    INTASC  Converts a string of bytes into a character variable              
CC    LINCON  Converts data in the input buffer into lines in the output        
CC            arrays.                                                           
CC    PRSCOM  Parses a command line.                                            
CC    TTBRDINI Initializes broadcast handler.                                   
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
CS    INPUTC  Contains the input character buffer INLINE and the defined        
CS            symbols SYMS and the definition search buffers DEFBUF             
C                                                                               
CS    INPUTN  Contains the input character buffer length LENSAV, and the        
CS            symbol definitions DEFS, the number of such definitions           
CS            (NDEFS), the definition flags DEFFLG, and the definition          
CS            numbers NUMDEF                                                    
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
CS    OWNER   Used to pass the file owner's UIC back to the main program        
CS            from a FORTRAN OPEN.                                              
C                                                                               
C***   INPUT VARIABLS                                                           
C                                                                               
CI    FILE    Used to input the symbol file's name.                             
C                                                                               
C***   INPUT/OUTPUT VARIABLES                                                   
C                                                                               
CIO   LINE    Used to get an error message from SYS$GETMSG and to input         
CIO           a line from the symbol file, or to output error message,          
CIO           and as the terminal input buffer.                                 
C                                                                               
C***   OUTPUT VARIABLES                                                         
C                                                                               
CO    FILE    Output with error message on open failure.                        
CO    LINES   Output array, contains converted lines of output.                 
C                                                                               
C***   INPUT UNITS                                                              
C                                                                               
CR    INCHAN  Input channel number of the terminal.                             
CR    30      Symbol file (file containing symbol definitions).                 
C                                                                               
C***   OUTPUT UNITS                                                             
C                                                                               
CW    6       Output to terminal.  Used for error messages.                     
CW    9       Output to terminal.  Carriage-control LIST.                       
C                                                                               
      PARAMETER NULL1=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)                     367
      PARAMETER NULLS=NULL1//NULL1//NULL1                                    368
      EXTERNAL GETOWN,EXITR,BRDCST                                           369
      CHARACTER*1024 LINE                                                    370
      CHARACTER*135 DIVLIN                                                   371
      CHARACTER*132 LINES(600),CLINE                                         372
      CHARACTER*128 INLINE,BRDMSG(20)                                        373
      CHARACTER*122 BLANK                                                    374
      CHARACTER*64 FILE,SYMFIL                                               375
      CHARACTER*30 SRCHBF(10),TEMP(10),DEFBUF(10,10,60)                      376
      CHARACTER*22 BRDNOT                                                    377
      CHARACTER*12 CHUSER                                                    378
      CHARACTER*11 DEFSW                                                     379
      CHARACTER*8 NARROW                                                     380
      CHARACTER*4 REVVID,REGVID,WRAP                                         381
      CHARACTER*3 BOX                                                        382
      CHARACTER*2 CRLF,MSGNUM                                                383
      CHARACTER*1 SYMS(60),COMAND(12)                                        384
      REAL*4 NUMDEF(10,60)                                                   385
      INTEGER*4 GETLIS(10),ERR,RFM,RAT,FSZ,TERM(2),INCHAN,                   386
     1          SYS$ASSIGN,SYS$SETIMR,TIME,PBUF,EBK,SYS$TRNLOG,                 
     2          BLK,SBLK,RANGE(2,2),CCLINE(2,600),PAGEND,DSCBLK(4)              
      INTEGER*2 GROUP,MEMBER,OWNER(2),IOSB(4),LINLEN(600),DIRECT,            389
     1          SBFLEN(10),RECBLK,DEFS(11,10,60),LENLMT,LENSAV,PAGLEN,          
     2          HLDLEN(10),COMLIN,OUTLEN,BRDCNT,BRDLEN(20)                      
      LOGICAL*1 BUFFER(3750),USER(12),SBFFLG,END(16),NUMFLG,MINFLG,          392
     1          ESC,WIDE,BSP,VT100,BELL,CR,LF,DELETE(11),FILEOK,                
     2          DEFFLG(3,10,60),HLDFLG,VT100A,DONE,BRDFLG                       
                                                                             395
      COMMON /OWNER/OWNER                                                    396
      COMMON /DOITNM/INCHAN,TIME,SBFFLG,DIRECT,EBK,SBFLEN,RECBLK,RANGE,      397
     1               VT100,LRL,LENLMT,PAGEND,PAGLEN,COMLIN,VT100A               
      COMMON /CHARS/LINES,SRCHBF,CRLF,REVVID,REGVID                          399
      COMMON /LNCN/LINLEN,LINEP,NLINES,BUFFER,PBUF,NBUF,BLK,FSZ,IFSZ,        400
     1             RFM,RAT,SBLK,CCLINE,LBLK,LPBUF                               
      COMMON /INPUTC/INLINE,SYMS,DEFBUF                                      402
      COMMON /INPUTN/LENSAV,DEFS,NDEFS,DEFFLG,NUMDEF                         403
      COMMON /BRDCM1/BRDNOT,BRDMSG                                           404
      COMMON /BRDCM2/BRDLEN,BRDCNT,BRDFLG                                    405
C                                                                               
C***   Initialize many things......                                             
C                                                                               
      DATA BRDCNT,BRDFLG/0,.FALSE./		! Broadcast info.                       409
      DATA FILEOK/.TRUE./                                                    410
      DATA GETLIS/'202000C'X,0,0,'3080002'X,0,0,'3070002'X,0,0,0/            411
      DATA END/'FE'X,'FF'X,'FF'X,'FF'X,11*0,'80'X/,TERM(1)/16/               412
      DATA BUFFER/3750*1/			! Empty buffer                                   413
      DATA BLANK,ESC,BELL,BSP,CR,LF		! ASCII values                          414
     1     /' ',0027,0007,008,13,10/                                            
      DATA REGVID,REVVID/' [0m',' [7m'/                                      416
      DATA DELETE/27,'[','0','m',8,32,8,27,'[','7','m'/                      417
      DATA SYMFIL/' '/				! No definition file                               418
      DATA NARROW , WRAP , BOX , DEFSW		! Valid command switches             419
     1   /'NARROW','WRAP','BOX','DEFINITIONS'/                                  
      DATA VT100A/.FALSE./			! Not an AVO VT100                              421
      DATA WIDE/0/,LENLMT/132/			! Wide mode                                 422
      DATA SBFFLG/.FALSE./			! No search buffer                              423
      DATA RANGE/0,0,0,0/			! No print range                                 424
      DATA TIME/-5000000/			! Scrolling speed                                425
      DATA DIRECT/1/				! Forward movement                                   426
      DATA OUTLEN/0/				! Wrap mode                                          427
      DATA PAGLEN,COMLIN/23,24/			! 23 lines of output                       428
      DATA NDEFS,(SYMS(I),DEFS(1,1,I),NUMDEF(1,I),DEFFLG(1,1,I),             429
     1     DEFFLG(2,1,I),DEFFLG(3,1,I),DEFS(1,2,I),I=1,13)/13,                  
     2     'U',   1,   23.,.TRUE., .FALSE.,.FALSE.,0,                           
     3     'D',   1,  -23.,.TRUE., .FALSE.,.FALSE.,0,                           
     4     'S',   5,    0.,.FALSE.,.FALSE.,.FALSE.,0,                           
     5     'Q',   6,    0.,.FALSE.,.FALSE.,.FALSE.,0,                           
     6     'J',   2,   20.,.TRUE., .FALSE.,.FALSE.,0,                           
     7     'B',   2,  -20.,.TRUE., .FALSE.,.FALSE.,0,                           
     8     'E',  11,    0.,.FALSE.,.FALSE.,.FALSE.,0,                           
     9     'P',  10,   23.,.TRUE., .FALSE.,.FALSE.,0,                           
     A     'A',  10,   23.,.TRUE., .TRUE., .FALSE.,0,                           
     B     'L',   9,   23.,.TRUE., .FALSE.,.FALSE.,0,                           
     C     'M',   8,    0.,.FALSE.,.FALSE.,.FALSE.,0,                           
     D     'C',  12,    0.,.FALSE.,.FALSE.,.FALSE.,0,                           
     E     'R',  13,    0.,.FALSE.,.FALSE.,.FALSE.,0/                           
C          Default key definitions:                                             
C          Key command   number     minus  search  0 to terminate               
                                                                             446
      WRITE (UNIT=CRLF,FMT=100)CR,LF                                         447
100   FORMAT (2A1)                                                           448
      WRITE (UNIT=REVVID(1:1),FMT=100)ESC                                    449
      REGVID(1:1) = REVVID(1:1)                                              450
      TERM(2) = %LOC(END(1))                                                 451
C                                                                               
C***   Set things up                                                            
C                                                                               
C     EXECUTE (PROCESS_COM_LINE)                                             455
      ASSIGN 50010 TO KKK022                                                 455
      GO TO 79802                                                               
50010 CONTINUE                                                                  
C     EXECUTE (OPEN_PRINTOUT)                                                456
      ASSIGN 50020 TO KKK024                                                 456
      GO TO 79804                                                               
50020 CONTINUE                                                                  
C     EXECUTE (SETUP_TERM)                                                   457
      ASSIGN 50030 TO KKK026                                                 457
      GO TO 79806                                                               
50030 CONTINUE                                                                  
C     EXECUTE (GET_FIRST_SCREEN)                                             458
      ASSIGN 50040 TO KKK028                                                 458
      GO TO 79808                                                               
50040 CONTINUE                                                                  
C     EXECUTE (PROCESS_SYMBOL_FILE)                                          459
      ASSIGN 50050 TO KKK030                                                 459
      GO TO 79810                                                               
50050 CONTINUE                                                                  
C                                                                               
C---   Set up to catch broadcast messages                                       
C                                                                               
      IF (.NOT.(VT100))GO TO 50070                                           463
      IF (.NOT.(VT100A))GO TO 50090                                          464
          BRDNOT = CHAR(BELL)//CHAR(BELL)//CHAR(ESC)//'[0;1;5m'//            465
     1             'MESSAGES'//CHAR(ESC)//'8'                                   
          I = 20                                                             467
      GO TO 50080                                                            468
50090 CONTINUE                                                                  
          BRDNOT = CHAR(BELL)//CHAR(BELL)//'MESSAGES'//CHAR(ESC)//'8'        469
          I = 12                                                             470
50080 CONTINUE                                                               471
      GO TO 50060                                                            472
50070 CONTINUE                                                                  
        BRDNOT = CHAR(BELL)//CHAR(BELL)//'MESSAGES'                          473
        I = 10                                                               474
50060 CONTINUE                                                               475
      BRDNOT = BRDNOT(:I)//NULLS                                             476
      CALL TTBRDINI(BRDCST)                                                  477
C                                                                               
C***   Set up command input QIO request                                         
C                                                                               
 1400 CONTINUE                                                               481
50099 CONTINUE                                                                  
C                                                                               
C---   Output dividing line if wanted                                           
C                                                                               
      IF (PAGLEN .NE. (COMLIN - 2))GO TO 50120                               485
      IF (.NOT.(VT100A))GO TO 50140                                          486
           CALL LIB$PUT_SCREEN(DIVLIN(:IABS(LENLMT)+3)//DIVLIN(1:1)//        487
     1                         '(A',COMLIN-1,1)                                 
      GO TO 50130                                                            489
50140 CONTINUE                                                                  
           CALL LIB$PUT_SCREEN(DIVLIN(:80),COMLIN-1,1)                       490
50130 CONTINUE                                                               491
50120 CONTINUE                                                               492
       IF (OUTLEN .GT. 0) CALL LIB$PUT_SCREEN(BLANK(:OUTLEN),COMLIN,10)      493
       IF (VT100A) CALL LIB$PUT_SCREEN(REVVID)                               494
      IF (LENSAV .LE. 0)GO TO 50160                                          495
         LINE(1+LENBUF:) = INLINE                                            496
         LENBUF = LENSAV + LENBUF                                            497
         LENSAV = 0                                                          498
50160 CONTINUE                                                               499
C                                                                               
C---   Output "messages" message (without bells)                                
C                                                                               
      IF (.NOT.(BRDFLG))GO TO 50180                                          503
         IF (VT100) CALL LIB$PUT_SCREEN(CHAR(27)//'7')                       504
         CALL LIB$PUT_SCREEN(BRDNOT(3:),COMLIN,IABS(LENLMT)-28)              505
50180 CONTINUE                                                               506
C                                                                               
      IF (LENBUF .LE. 0)GO TO 50200                                          508
         CALL LIB$PUT_SCREEN('Command: '//LINE(:LENBUF),COMLIN,1)            509
      GO TO 50190                                                            510
50200 CONTINUE                                                                  
         CALL LIB$PUT_SCREEN('Command: ',COMLIN,1)                           511
50190 CONTINUE                                                               512
       LENBUF = LENBUF + 1                                                   513
       J = 35 - LENBUF                                                       514
C                                                                               
C***   Input loop                                                               
C                                                                               
50209 CONTINUE                                                               518
        CALL SYS$QIOW(,%VAL(INCHAN),%VAL('1331'X),IOSB,,,                    519
     1                %REF(LINE(LENBUF:)),%VAL(J),,TERM,,)                      
        LENBUF = IOSB(2) + LENBUF                                            521
      IF (ICHAR(LINE(LENBUF:LENBUF)) .NE. 127)GO TO 50230                    522
C                                                                               
C---   Check for delete                                                         
C                                                                               
      IF (LENBUF .LE. 1)GO TO 50250                                          526
            LENBUF = LENBUF - 1                                              527
      IF (.NOT.(VT100A))GO TO 50270                                          528
              CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,DELETE,             529
     1                      %VAL(11),,,,)                                       
      GO TO 50260                                                            531
50270 CONTINUE                                                                  
              CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,DELETE(5),          532
     1                      %VAL(3),,,,)                                        
50260 CONTINUE                                                               534
50250 CONTINUE                                                               535
      GO TO 50220                                                            536
50230 CONTINUE                                                                  
C                                                                               
C---   Check for ^U/^X                                                          
C                                                                               
      IF (.NOT.(ICHAR(LINE(LENBUF:LENBUF)) .EQ. 21 .OR.                      540
     X          ICHAR(LINE(LENBUF:LENBUF)) .EQ. 24))GO TO 50290                 
            LENBUF = 1                                                       542
C     EXECUTE (ERASE_COMMAND)                                                543
      ASSIGN 50300 TO KKK032                                                 543
      GO TO 79812                                                               
50300 CONTINUE                                                                  
      GO TO 50280                                                            544
50290 CONTINUE                                                                  
C                                                                               
C---   Check for ^R                                                             
C                                                                               
      IF (ICHAR(LINE(LENBUF:LENBUF)) .NE. 18)GO TO 50320                     548
C     EXECUTE (ERASE_COMMAND)                                                549
      ASSIGN 50330 TO KKK032                                                 549
      GO TO 79812                                                               
50330 CONTINUE                                                                  
              CALL SYS$QIOW(,%VAL(INCHAN),%VAL('30'X),,,,%REF(LINE),         550
     1                      %VAL(LENBUF-1),,,,)                                 
      GO TO 50310                                                            552
50320 CONTINUE                                                                  
C                                                                               
C---   Check for ^Z                                                             
C                                                                               
      IF (ICHAR(LINE(LENBUF:LENBUF)) .NE. 26)GO TO 50350                     556
C                                                                               
C===   Erase screen                                                             
C                                                                               
              CALL LIB$ERASE_PAGE(1,1)                                       560
      IF (.NOT.(BRDFLG))GO TO 50370                                          561
                IF (VT100A) CALL LIB$PUT_SCREEN(REVVID)                      562
      IF (BRDCNT .NE. 1)GO TO 50390                                          563
                  CALL LIB$PUT_OUTPUT('THERE IS A BROADCAST '//              564
     1                 'MESSAGE'//CHAR(BELL),1,1)                               
      GO TO 50380                                                            566
50390 CONTINUE                                                                  
                  WRITE (UNIT=MSGNUM,FMT='(I2)') BRDCNT                      567
      IF (BRDCNT .GE. 10)GO TO 50410                                         568
                    CALL LIB$PUT_OUTPUT('THERE ARE'//MSGNUM//                569
     1                   ' BROADCAST MESSAGES'//CHAR(BELL),1,1)                 
      GO TO 50400                                                            571
50410 CONTINUE                                                                  
                    CALL LIB$PUT_OUTPUT('THERE ARE '//MSGNUM//               572
     1                   ' BROADCAST MESSAGES'//CHAR(BELL),1,1)                 
50400 CONTINUE                                                               574
50380 CONTINUE                                                               575
                CALL LIB$SET_CURSOR(3,1)                                     576
50370 CONTINUE                                                               577
              IF (VT100A) CALL LIB$PUT_SCREEN(REGVID)                        578
50419 CONTINUE                                                               579
                 CALL LIB$GET_INPUT(CLINE,'Command: ',LENGTH)                580
                 CALL COMLVL(CLINE(:LENGTH),DONE)                            581
      IF (DONE)GO TO 50421                                                   582
      GO TO 50419                                                            583
50421 CONTINUE                                                                  
                CALL DOIT(-1,.TRUE.,0.,.FALSE.)                              584
                CALL DOIT(-12,.FALSE.,0.,.FALSE.)                            585
                LENBUF = LENBUF - 1                                          586
      GO TO 50100                                                            587
50350 CONTINUE                                                               588
                LENBUF = LENBUF - 1                                          589
      GO TO 50211                                                            590
50310 CONTINUE                                                               592
50280 CONTINUE                                                               593
50220 CONTINUE                                                               594
      GO TO 50209                                                            595
50211 CONTINUE                                                                  
       OUTLEN = LENBUF                                                       596
       IF (VT100A) CALL LIB$PUT_SCREEN(REGVID)                               597
       CALL LIB$SET_CURSOR(1,1)                                              598
C                                                                               
C***   Execute commands                                                         
C                                                                               
       CALL STR$TRANSLATE(LINE,LINE,' ','	')                                 602
       CALL STR$TRIM(LINE(:LENBUF),LINE(:LENBUF),LENBUF)                     603
50429 CONTINUE                                                               604
        CALL PRSCOM(LINE,LENBUF,ICOM,NUMFLG,ANUMB,MINFLG,0,0)                605
      IF (ICOM .EQ. 0)GO TO 50431                                            606
        IF (ICOM .GT. 0) CALL DOIT(ICOM,NUMFLG,ANUMB,MINFLG)                 607
      IF (ICOM .EQ. 5)GO TO 50431                                            608
      GO TO 50429                                                            609
50431 CONTINUE                                                                  
50100 GO TO 50099                                                            610
C                                                                               
C***********************************************************************        
C****************************   PROCEDURES   ***************************        
C***********************************************************************        
C                                                                               
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (ERASE_COMMAND)                                              616
79812 CONTINUE                                                               616
CP    This procedure erase the command part of the bottom line.                 
      IF (.NOT.(VT100A))GO TO 50450                                          618
          CALL LIB$PUT_SCREEN(REGVID//BLANK(:LENBUF)//REVVID,COMLIN,10)      619
      GO TO 50440                                                            620
50450 CONTINUE                                                                  
          CALL LIB$PUT_SCREEN(BLANK(:LENBUF),COMLIN,10)                      621
50440 CONTINUE                                                               622
        CALL LIB$SET_CURSOR(COMLIN,10)                                       623
      GO TO KKK032                                                           624
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (GET_FIRST_SCREEN)                                           625
79808 CONTINUE                                                               625
CP    This procedure inputs and formats the first screen-full of output.        
C                                                                               
C***   Read in first bunch of data and output                                   
C                                                                               
        BLK = 1                                                              630
        SBLK = BLK                                                           631
      IF (EBK .LE. 6)GO TO 50470                                             632
          CALL BLKSIN(BLK,NBUF,BUFFER(1))                                    633
      IF (NBUF .NE. 0)GO TO 50490                                            634
C     EXECUTE (READ_ERROR)                                                   635
      ASSIGN 50500 TO KKK034                                                 635
      GO TO 79814                                                               
50500 CONTINUE                                                                  
50490 CONTINUE                                                               636
      GO TO 50460                                                            637
50470 CONTINUE                                                                  
          NBUF = 0                                                           638
       I=1                                                                   639
      GO TO 50509                                                               
50510  I= I+(1)                                                                 
      IF ( I.GT.EBK)GO TO 50511                                                 
50509 CONTINUE                                                                  
           CALL BLKIN(BLK,J,BUFFER(NBUF+1))                                  640
      IF (J .NE. 0)GO TO 50530                                               641
C     EXECUTE (READ_ERROR)                                                   642
      ASSIGN 50540 TO KKK034                                                 642
      GO TO 79814                                                               
50540 CONTINUE                                                                  
50530 CONTINUE                                                               643
           NBUF = NBUF + J                                                   644
      GO TO 50510                                                            645
50511 CONTINUE                                                                  
50460 CONTINUE                                                               646
        CALL LINCON(.TRUE.)                                                  647
        J = PAGLEN                                                           648
      IF (NLINES .GE. PAGLEN)GO TO 50560                                     649
50569 CONTINUE                                                               650
      IF (BLK .LE. EBK)GO TO 50590                                           651
             J = NLINES                                                      652
      GO TO 50571                                                            653
50590 CONTINUE                                                               654
           CALL BLKIN(BLK,I,BUFFER(NBUF+1))                                  655
      IF (I .NE. 0)GO TO 50610                                               656
C     EXECUTE (READ_ERROR)                                                   657
      ASSIGN 50620 TO KKK034                                                 657
      GO TO 79814                                                               
50620 CONTINUE                                                                  
50610 CONTINUE                                                               658
           NBUF = NBUF + I                                                   659
           CALL LINCON(.FALSE.)                                              660
      IF (NLINES .GE. PAGLEN)GO TO 50571                                     661
      GO TO 50569                                                            662
50571 CONTINUE                                                                  
50560 CONTINUE                                                               663
        CALL LIB$ERASE_PAGE(1,1)                                             664
        CALL LIB$SET_CURSOR(1,1)                                             665
       I=1                                                                   666
      GO TO 50629                                                               
50630  I= I+(1)                                                                 
      IF ( I.GT.J)GO TO 50631                                                   
50629 CONTINUE                                                                  
      IF (LENLMT .GE. 0)GO TO 50650                                          667
      IF (LINLEN(I) .GT. (-LENLMT))GO TO 50670                               668
             CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)                 669
      GO TO 50660                                                            670
50670 CONTINUE                                                                  
             CALL LIB$PUT_SCREEN(LINES(I)(:-LENLMT)//CRLF//                  671
     1                           LINES(I)(1-LENLMT:LINLEN(I))//CRLF)            
50660 CONTINUE                                                               673
      GO TO 50640                                                            674
50650 CONTINUE                                                                  
      IF (LINLEN(I) .GE. LENLMT)GO TO 50690                                  675
             CALL LIB$PUT_SCREEN(LINES(I)(:LINLEN(I))//CRLF)                 676
      GO TO 50680                                                            677
50690 CONTINUE                                                                  
             CALL LIB$PUT_SCREEN(LINES(I)(:LENLMT)//CRLF)                    678
50680 CONTINUE                                                               679
50640 CONTINUE                                                               680
      GO TO 50630                                                            681
50631 CONTINUE                                                                  
        PAGEND = J                                                           682
        IF (PAGLEN .EQ. 22) CALL LIB$PUT_SCREEN(CRLF)                        683
        IF (VT100A) CALL LIB$PUT_SCREEN(REVVID)                              684
        CALL LIB$PUT_SCREEN('Block 1',24,IABS(LENLMT)-6)                     685
        IF (VT100A) CALL LIB$PUT_SCREEN(REGVID)                              686
        CALL LIB$SET_CURSOR(1,1)                                             687
      GO TO KKK028                                                           688
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (MAIN_DIR_SEARCH)                                            689
79816 CONTINUE                                                               689
CP    This routine searches the directories of the users username on the        
CP    various disks (JPL, CIT, and SYS2 in that order) for the                  
CP    symbol file (LOOKCOM.DAT), and if found opens it.                         
        CALL INTASC(I,USER,CHUSER)                                           693
        SYMFIL='JPL:['//CHUSER(:INDEX(CHUSER,' ')-1)//']LOOKCOM.DAT'         694
        OPEN (UNIT=30,NAME=SYMFIL,TYPE='OLD',READONLY,ERR=1000,              695
     1        ACCESS='SEQUENTIAL',FORM='FORMATTED')                             
      GO TO 50700                                                            697
 1000 CONTINUE                                                               698
            SYMFIL(1:3) = 'CIT'                                              699
            OPEN (UNIT=30,NAME=SYMFIL,TYPE='OLD',READONLY,ERR=1100,          700
     1            ACCESS='SEQUENTIAL',FORM='FORMATTED')                         
      GO TO 50701                                                            702
 1100 CONTINUE                                                                  
            SYMFIL = 'SYS2'//SYMFIL(4:)                                      703
            OPEN (UNIT=30,NAME=SYMFIL,TYPE='OLD',READONLY,ERR=1200,          704
     1            ACCESS='SEQUENTIAL',FORM='FORMATTED')                         
      GO TO 50701                                                            706
 1200 CONTINUE                                                                  
            FILEOK = .FALSE.                                                 707
50701 CONTINUE                                                               708
50700 CONTINUE                                                                  
      GO TO KKK036                                                           709
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (NO_INPUT_EXIT)                                              710
79818 CONTINUE                                                               710
CP    This routine is an error routine if something goes wrong with getting     
CP    input from the terminal.                                                  
        CALL LIB$PUT_OUTPUT(' CANNOT INPUT FROM SYS$INPUT.')                 713
        CALL EXIT                                                            714
      GO TO KKK038                                                           715
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (OPEN_PRINTOUT)                                              716
79804 CONTINUE                                                               716
CP    This procedure opens the input file.                                      
C                                                                               
C***  Try to open printout file                                                 
C                                                                               
        J = LIB$SKPC(' ',LINE)                                               721
        FILE = LINE(J:)                                                      722
        I = INDEX(FILE,' ') - 1                                              723
        CALL BLKOPN(FILE(:I),ERR,RFM,RAT,FSZ,EBK,LRL)                        724
      IF (ERR .EQ. 1)GO TO 50720                                             725
          IF (I .LE. 0) I = 1                                                726
          CALL SYS$GETMSG(%VAL(ERR),J,LINE,%VAL(15),)                        727
          CALL LIB$PUT_OUTPUT('****   UNABLE TO OPEN FILE '//FILE(:I))       728
          CALL LIB$PUT_OUTPUT(LINE(:J))                                      729
          CALL EXIT                                                          730
50720 CONTINUE                                                               731
      IF (.NOT.(RFM .EQ. 1 .AND. FSZ))GO TO 50740                            732
          IFSZ = FSZ + 1                                                     733
      GO TO 50730                                                            734
50740 CONTINUE                                                                  
          IFSZ = FSZ                                                         735
50730 CONTINUE                                                               736
      IF (.NOT.(RAT .LT. 0 .AND. RFM .EQ. 1))GO TO 50760                     737
          RECBLK = IFSZ*(512/IFSZ)                                           738
      GO TO 50750                                                            739
50760 CONTINUE                                                                  
          RECBLK = 512                                                       740
50750 CONTINUE                                                               741
      GO TO KKK024                                                           742
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (PROCESS_COM_LINE)                                           743
79802 CONTINUE                                                               743
CP    This procedure processes the command line.                                
C                                                                               
C***  Get file name off command line or prompt for it                           
C***  Also check for /DEFINITIONS switch                                        
C                                                                               
        CALL LIB$GET_FOREIGN(LINE,'Filename: ',LENGTH)                       749
        CALL STR$UPCASE(LINE(:LENGTH),LINE(:LENGTH))                         750
C                                                                               
C***  Get rid of tabs                                                           
C                                                                               
        CALL STR$TRANSLATE(LINE(:LENGTH),LINE(:LENGTH),' ','	')              754
C                                                                               
C***  Get the switches                                                          
C                                                                               
50769 CONTINUE                                                               758
         I = INDEX(LINE(:LENGTH),'/') + 1                                    759
      IF (I .EQ. 1)GO TO 50771                                               760
         J = INDEX(LINE(I:LENGTH),'/') + I - 2                               761
         IF (J .EQ. (I - 2)) J = LENGTH                                      762
         K = INDEX(LINE(I:LENGTH),' ') + I - 2                               763
         IF (K .EQ. (I - 2)) K = LENGTH                                      764
         IF (K .LT. J) J = K                                                 765
C                                                                               
C---   WRAP                                                                     
C                                                                               
      IF ((J - I) .GT. 3)GO TO 50800                                         770
      IF (LINE(I:J) .NE. WRAP(:J-I+1))GO TO 50820                            771
              LENLMT = -LENLMT                                               772
      GO TO 50781                                                            773
50820 CONTINUE                                                               774
50800 CONTINUE                                                               775
C                                                                               
C---   NARROW                                                                   
C                                                                               
      IF ((J - I) .GT. 5)GO TO 50840                                         779
      IF (LINE(I:J) .NE. NARROW(:J-I+1))GO TO 50860                          780
              WIDE = 1                                                       781
      IF (LENLMT .GE. 0)GO TO 50880                                          782
                LENLMT = -80                                                 783
      GO TO 50870                                                            784
50880 CONTINUE                                                                  
                LENLMT = 80                                                  785
50870 CONTINUE                                                               786
      GO TO 50781                                                            787
50860 CONTINUE                                                               788
50840 CONTINUE                                                               789
C                                                                               
C---   BOX                                                                      
C                                                                               
      IF ((J - I) .GT. 2)GO TO 50900                                         793
      IF (LINE(I:J) .NE. BOX(:J-I+1))GO TO 50920                             794
              PAGLEN = 22                                                    795
      GO TO 50781                                                            796
50920 CONTINUE                                                               797
50900 CONTINUE                                                               798
C                                                                               
C---   Definitions file                                                         
C                                                                               
          K = INDEX(LINE,'=')                                                802
      IF (.NOT.(K .NE. 0 .AND. K .NE. J .AND. (K - I) .LE. 11))GO TO 509     803
     X40                                                                        
      IF (LINE(I:K-1) .NE. DEFSW(:K-I))GO TO 50960                           804
              SYMFIL = LINE(K+1:J)                                           805
              L = INDEX(SYMFIL,']') + 1                                      806
      IF (INDEX(SYMFIL(L:),'.') .NE. 0)GO TO 50980                           807
                L = INDEX(SYMFIL,';')                                        808
      IF (L .NE. 0)GO TO 51000                                               809
                  SYMFIL = LINE(K+1:J)//'.'                                  810
      GO TO 50990                                                            811
51000 CONTINUE                                                                  
                  SYMFIL = SYMFIL(:L-1)//'.'//SYMFIL(L:)                     812
50990 CONTINUE                                                               813
50980 CONTINUE                                                               814
      GO TO 50781                                                            815
50960 CONTINUE                                                               816
50940 CONTINUE                                                               817
          CALL LIB$PUT_OUTPUT(' ****   INVALID SWITCH   ****')               818
          CALL EXIT                                                          819
50781 CONTINUE                                                               820
         LINE = LINE(:I-2)//LINE(J+1:)                                       821
         LENGTH = LENGTH - J + I - 2                                         822
      GO TO 50769                                                            823
50771 CONTINUE                                                                  
      GO TO KKK022                                                           824
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (PROCESS_SYMBOL_FILE)                                        825
79810 CONTINUE                                                               825
CP    This procedure processes the symbol/command file.                         
  300 CONTINUE                                                               827
      IF (SYMFIL .NE. ' ')GO TO 51030                                        828
C                                                                               
C***  Get user information to try and find a definitions file                   
C                                                                               
           GETLIS(2) = %LOC(USER(1))                                         832
           GETLIS(3) = %LOC(I)                                               833
           GETLIS(5) = %LOC(GROUP)                                           834
           GETLIS(8) = %LOC(MEMBER)                                          835
           CALL SYS$GETJPI(,,,GETLIS,,,)                                     836
           OPEN (UNIT=30,NAME='LOOKCOM.DAT',TYPE='OLD',READONLY,ERR=400,     837
     1           ACCESS='SEQUENTIAL',FORM='FORMATTED',USEROPEN=GETOWN)          
      IF (.NOT.(OWNER(1) .NE. GROUP .OR. OWNER(2) .NE. MEMBER))GO TO 510     839
     X50                                                                        
             CLOSE (UNIT=30)                                                 840
C     EXECUTE (MAIN_DIR_SEARCH)                                              841
      ASSIGN 51060 TO KKK036                                                 841
      GO TO 79816                                                               
51060 CONTINUE                                                                  
51050 CONTINUE                                                               842
      GO TO 51070                                                            843
  400 CONTINUE                                                               844
C     EXECUTE (MAIN_DIR_SEARCH)                                              845
      ASSIGN 51080 TO KKK036                                                 845
      GO TO 79816                                                               
51080 CONTINUE                                                                  
51070 CONTINUE                                                               846
      GO TO 51020                                                            847
51030 CONTINUE                                                                  
           OPEN (UNIT=30,NAME=SYMFIL,TYPE='OLD',READONLY,ERR=500,            848
     1           ACCESS='SEQUENTIAL',FORM='FORMATTED')                          
      GO TO 51090                                                            850
  500 CONTINUE                                                               851
               CALL LIB$PUT_SCREEN(REVVID//'Could not open file '//          852
     1              SYMFIL(:INDEX(SYMFIL,' ')-1)//REGVID//CHAR(BELL),           
     2              24,20)                                                      
      GO TO 51011                                                            855
51090 CONTINUE                                                               856
51020 CONTINUE                                                               857
C                                                                               
C***   Read in lines and interpret them                                         
C                                                                               
         IF (FILEOK) CALL DODEF                                              861
51011 CONTINUE                                                               862
      GO TO KKK030                                                           863
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (READ_ERROR)                                                 864
79814 CONTINUE                                                               864
CP    This procedure is executed if an error occurs while reading the           
CP    input file.                                                               
        CALL LIB$PUT_OUTPUT(' ERROR READING FILE')                           867
        CALL EXIT                                                            868
      GO TO KKK034                                                           869
C                                                                               
C                                                                               
C.......................................................................        
C     PROCEDURE (SETUP_TERM)                                                 870
79806 CONTINUE                                                               870
CP    This procedure opens and sets up the terminal.                            
C                                                                               
C***   Open a channel to SYS$INPUT                                              
C                                                                               
      IF (SYS$TRNLOG('TT',I,SRCHBF(1),,,))GO TO 51110                        875
C     EXECUTE (NO_INPUT_EXIT)                                                876
      ASSIGN 51120 TO KKK038                                                 876
      GO TO 79818                                                               
51120 CONTINUE                                                                  
51110 CONTINUE                                                               877
      IF (INDEX(SRCHBF(1)(:I),'LPA0') .EQ. 0)GO TO 51140                     878
      IF (SYS$TRNLOG('SYS$INPUT',I,SRCHBF(1),,,))GO TO 51160                 879
C     EXECUTE (NO_INPUT_EXIT)                                                880
      ASSIGN 51170 TO KKK038                                                 880
      GO TO 79818                                                               
51170 CONTINUE                                                                  
51160 CONTINUE                                                               881
51140 CONTINUE                                                               882
      IF (ICHAR(SRCHBF(1)) .NE. 27)GO TO 51190                               883
          I = I - 4                                                          884
          SRCHBF(1) = SRCHBF(1)(5:)                                          885
51190 CONTINUE                                                               886
      IF (SYS$ASSIGN(SRCHBF(1)(:I),INCHAN,,))GO TO 51210                     887
C     EXECUTE (NO_INPUT_EXIT)                                                888
      ASSIGN 51220 TO KKK038                                                 888
      GO TO 79818                                                               
51220 CONTINUE                                                                  
51210 CONTINUE                                                               889
C                                                                               
C***   Set terminal characteristics                                             
C                                                                               
        CALL SETERM(INCHAN,WIDE,LENLMT)                                      893
C                                                                               
C***   Declare exit handler                                                     
C                                                                               
        DSCBLK(2) = %LOC(EXITR)                                              897
        DSCBLK(3) = 1                                                        898
        DSCBLK(4) = %LOC(I)                                                  899
        CALL SYS$DCLEXH(DSCBLK)                                              900
C                                                                               
C***   If wanted format dividing line                                           
C                                                                               
      IF (.NOT.(VT100A))GO TO 51240                                          904
          DIVLIN(1:1) = REVVID(1:1)                                          905
          DIVLIN(2:) = '(0qqqqqqqqqqqqqqqqqqqqqqqqqqq'//                     906
     1         'qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq'//        
     2         'qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq'           
      GO TO 51230                                                            909
51240 CONTINUE                                                                  
          DIVLIN = '--------------------------------'//                      910
     1         '---------------------------------------------------'            
51230 CONTINUE                                                               912
      GO TO KKK026                                                           913
      END                                                                    914
