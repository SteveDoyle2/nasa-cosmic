        ASSUME  CS:INTPRT
INTPRT  SEGMENT PUBLIC
        PUBLIC  INITEK

COMMENT | INITEK is an 8088 routine to initialize the serial interface
                for the Tektronix 4662 plotter.  The 4662 requires
                a baud rate of 1200, even parity, seven bit words with
                two stop bits. 
        |

INITEK  PROC    FAR
        PUSH    AX
        PUSH    DX
        MOV     AL, 10011110B   ; 1   0   0   1   1   1   1   0
                                ; bits 7-5 set 1200 baud
                                ; bit 4 sets even parity,
                                ; bit 3 enables parity
                                ; bit 2 sets 2 stop bits
                                ; bits 1 & 0    10 = 7 bit word
                                ;               11 = 8 bit word
        MOV     AH, 0H          ; select initialization routine
        MOV     DX, 00H         ; select port 0
        INT     14H             ; use BIOS function to initialize
        POP     DX
        POP     AX
        RET
INITEK  ENDP

COMMENT | RDAUX use the BIOS routine (INT 14H) to read data from
          the communication port.  Data read is found in AL.
        |

        PUBLIC  RDAUX

RDAUX   PROC    FAR
        PUSH    DX
        MOV     AH, 02H
        MOV     DX, 0H
        INT     14H
        POP     DX
        RET
RDAUX   ENDP

COMMENT | WRAUX uses the BIOS routine (INT 14H) to write data to
          the communication port.  Data written is placed in AL.
        |

        PUBLIC  WRAUX

WRAUX   PROC    FAR
        PUSH    DX
        MOV     AH, 01H
        MOV     DX, 0H
        INT     14H
        POP     DX
        RET
WRAUX   ENDP

COMMENT | CKAUX uses the BIOS routine (INT 14H) in order to poll the
          status of the communication port.  The line control status
          is returned in AH, the modem control status in AL.
        |

        PUBLIC  CKAUX
CKAUX   PROC    FAR
        PUSH    DX
        MOV     AH, 03H
        MOV     DX, 0H
        INT     14H
        POP     DX
        RET
CKAUX   ENDP

        PUBLIC  RDYLIN

COMMENT | RDYLIN is an 8088 routine to read the data off the interface
                from the 4662.  If no data has been sent, the routine
                returns control to the caller.  If a DC3 has been sent,
                RDYLIN waits until the 4662 sends a DC1, indicating
                it is ready to receive more data.
        |

RDYLIN  PROC    FAR
        PUSH    AX
        PUSH    DX
        CALL    CKAUX
        TEST    AH, 00000001B
        JZ      REXIT

        CALL    RDAUX
        CMP     AL, 13H
        JNZ     REXIT
DC1LP:
        CALL    RDAUX
        CMP     AL, 11H
        JNZ     DC1LP

REXIT:
        POP     DX
        POP     AX
        RET
RDYLIN  ENDP

COMMENT | Procedure crite(c: char);

          CRITE is intended to be called from Pascal.  The character c is
          found in location BP+06.  The character is written by WRAUX,
          then the plotter is polled via RDYLIN to see if it is ready to
          accept more data.
        |

        PUBLIC  CRITE
CRITE   PROC    FAR
        PUSH    BP
        MOV     BP, SP
        MOV     AL, [BP+06]
        CALL    WRAUX
        CALL    RDYLIN
        POP     BP
        RET     1
CRITE   ENDP

INTPRT  ENDS
        ASSUME  SS: NOTHING
STACK   SEGMENT PARA STACK
        DB      10 DUP(?)
STACK   ENDS
        END
