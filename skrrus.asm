.286
.MODEL  SMALL
.STACK  100H
CODE    SEGMENT PARA
ASSUME  CS:CODE,DS:DATA,SS:STACK
BCS:
LCODE   EQU     (ECS-BCS+15)/16
LDATA   EQU     (EDS-BDS+15)/16
LREZ    EQU     (START-BCS+15)/16
SSG     EQU     data+ldata
K__     EQU     34H

DATS    EQU     CS:[DSEG]
SFS     EQU     CS:[SFSEG]
ALS     EQU     CS:[ALREADY_SEG]


RUSBITS EQU     00000001B
GRBIT   EQU     00000010B
SHIFTB  EQU     00000100B
CLOCKB  EQU     00001000B
ALTB    EQU     00010000B
CTRLB   EQU     00100000B
FIRSTLFB        EQU     01000000B
FIRSTRLB        EQU     10000000B
RUSLATKB        EQU     100000000B
KEYPADPB        EQU     1000000000B
AFTER0E0H       EQU     10000000000B
;0-1 = 01 - RUSSIAN
;    = 10 - GRAPHICS
;2 = 1 - SHIFT PRESSED
;3 = 1 - CAPS LOCK PRESSED
;4 = 1 - ALT PRESSED
;5 = 1 - CONTROL PRESSED
;6 = 1 - FIRST LOAD FONT PRESSED
;7 = 1 - FIRST RUS/LAT KEYBOARD PRESSED
;8 = 1 - RUS/LAT KEYBOARD PRESSED
;9 = 1 - NON ALT/SHIFT/CAPS/CTRL KEY AND LETTER PRESSED(KEYPAD PRESSED)
;10 = 1 - AFTER 0E0H CODE



;40:8AH-MONITOR, SEEK CHAPTER2
;40:17H-KEYBOARD FLAGS(2,3 -ALT <> CONTROL)

VERSION_        DW      104H
ALREADY_SEG     DW      CODE
DSEG    DW      DATA
SFSEG   DW      SSG
OLD_9           DW      0,0
OLD_10          DW      0,0
OLD_21          DW      0,0
HEIGHT_FONT     DB      10H
QUANTITY_SYMBOLS        DW      100H
SHIFT_IN_CODES_TABLE    DW      0
BLOCK_SELECTOR          DB      0
FLA             DB      0
;0 - 1=INSTALLED TO MEMORY
;1 - 1=INT 10H INSTALLED
;2 - 1=INT 9H INSTALLED
;3 - 1=NO LOAD FONT
;4 - 1=NO SWITCH TO RUSSIAN
;5 - 1=NO SWITCH TO PSEUDO-GRAPHICS
FONT_OFFSET     DW      SFONT
KBD_OFFSET      DW      KEYTBL


INT_10  PROC
        CLC
        NOP
        OR      AH,AH
        JE      NEW_INT_10H_1
        JMP     DWORD PTR CS:[OLD_10]
NEW_INT_10H_1:
        PUSH    AX
        AND     AL,7FH
        CMP     AL,03
        JBE     NEW_INT_10H_2
        JBE     NEW_INT_10H_2
        CMP     AL,07
        JE      NEW_INT_10H_2
        POP     AX
        JMP     DWORD PTR CS:[OLD_10]
NEW_INT_10H_2:
        POP     AX
        PUSHF
        CALL    DWORD PTR CS:[OLD_10]
        TEST    CS:[FLA],1000B
        JNZ     NEW_INT_10_NO_LOAD
        CALL    RELOAD_FONT
NEW_INT_10_NO_LOAD:
        IRET
ENDP
INT_9   PROC
        CLD
        NOP
        PUSH    AX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        CALL    KEY_ANALIZ
        PUSHF
        PUSHF
        CALL    dword ptr CS:[OLD_9]
        POPF
        JC      INT_9_EXE
        TEST    BYTE PTR CS:[FLA],10B
        JZ      INT_91
        CALL    LOADFKEYS
        JC      INT_9_EXE
INT_91:
        CALL    NEW_KEYS
INT_9_EXE:
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     AX
        IRET
KEY_ANALIZ      PROC
        MOV     AX,40H
        MOV     DS,AX
        MOV     AX,DS:[1CH];ADRESS BUF
        MOV     CS:[BUF_ADRESS],AX
        IN      AL,60H
        CMP     AL,0E0H
        JAE     KEY_ANALIZ_ER
        CALL    VERIFY_SHIFTS
        JNC     KEY_ANALIZ_111
        CALL    VERIFY_CTRL
        JNC     KEY_ANALIZ_111
        CALL    VERIFY_ALT
        JNC     KEY_ANALIZ_111
        CALL    VERIFY_CAPS
        JNC     KEY_ANALIZ_111
        TEST    AL,10000000B
        JZ      KEY_ANALIZ1
        OR      BYTE PTR CS:[MODE+1],10B
        XOR     AL,AL
        JMP     SHORT   KEY_ANALIZE1
KEY_ANALIZ_111:
        XOR     AL,AL
KEY_ANALIZ1:
        AND     BYTE PTR CS:[MODE+1],11111101B
KEY_ANALIZE1:
        MOV     CS:[KEY_CODE],AL
        OR      AL,AL
        JNZ     KEY_ANALIZ11
        TEST    BYTE PTR CS:[MODE+1],10B
        JNZ     KEY_ANALIZ11
        CALL    CHANGE_MODE
KEY_ANALIZ11:
        AND     WORD PTR CS:[MODE],1111101111111111B
        CLC
        RET
KEY_ANALIZ_ER:
        OR      WORD PTR CS:[MODE],10000000000B
        STC
        RET
ENDP
VERIFY_SHIFTS   PROC
        CMP     AL,2AH+80H
        JZ      VERIFY_SHIFTSOFF
        CMP     AL,36H+80H
        JZ      VERIFY_SHIFTSOFF
        TEST    WORD PTR CS:[MODE],10000000000B
        JNZ     VERIFY_SHIFTS111
        CMP     AL,2AH
        JZ      VERIFY_SHIFTS1
VERIFY_SHIFTS111:
        CMP     AL,36H
        JNZ     VERIFY_SHIFTSNO
VERIFY_SHIFTS1:
        TEST    BYTE PTR CS:[MODE],100B
        JZ      VERIFY_SHIFTS2
        AND     BYTE PTR CS:[MODE],1111111B
        JMP     SHORT   VERIFY_SHIFTSE
VERIFY_SHIFTS2:
        OR      BYTE PTR CS:[MODE],10000100B
VERIFY_SHIFTSE:
        RET
VERIFY_SHIFTSOFF:
        AND     BYTE PTR CS:[MODE],1111011B
        RET
VERIFY_SHIFTSNO:
        STC
        RET
ENDP
VERIFY_ALT      PROC
        CMP     AL,38H+80H
        JZ      VERIFY_ALT1
        CMP     AL,38H
        JNZ     VERIFY_ALTNO
        TEST    BYTE PTR CS:[MODE],10000B
        JZ      VERIFY_ALT2
        AND     BYTE PTR CS:[MODE],111111B
        JMP     SHORT   VERIFY_ALTE
VERIFY_ALT2:
        OR      BYTE PTR CS:[MODE],11010000B
VERIFY_ALTE:
        RET
VERIFY_ALT1:
        AND     BYTE PTR CS:[MODE],101111B
        RET
VERIFY_ALTNO:
        STC
        RET
ENDP
VERIFY_CTRL     PROC
        CMP     AL,1DH+80H
        JZ      VERIFY_CTRL1
        CMP     AL,1DH
        JNZ     VERIFY_CTRLNO
        TEST    BYTE PTR CS:[MODE],100000B
        JZ      VERIFY_CTRL2
        AND     BYTE PTR CS:[MODE],10111111B
        JMP     SHORT   VERIFY_CTRLE
VERIFY_CTRL2:
        OR      BYTE PTR CS:[MODE],1100000B
VERIFY_CTRLE:
        RET
VERIFY_CTRL1:
        AND     BYTE PTR CS:[MODE],10011111B
        RET
VERIFY_CTRLNO:
        STC
        RET
ENDP
VERIFY_CAPS     PROC
        CMP     AL,3AH
        JNZ     VERIFY_CAPSNO
        XOR     BYTE PTR CS:[MODE],1000B
        RET
VERIFY_CAPSNO:
        STC
        RET
ENDP
CHANGE_MODE     PROC
        PUSH    AX
        MOV     AX,CS:[MODE]
        NOT     AX
        TEST    AL,10100B
        JNZ     CHANGE_MODEE
        TEST    BYTE PTR CS:[MODE],10000000B
        JZ      CHANGE_MODEE1
        NOT     AX
        OR      BYTE PTR CS:[MODE+1],1
        MOV     AH,BYTE PTR CS:[FLA]
        SHR     AH,4
        AND     AX,303H
        INC     AL
        ADD     AL,AH
        AND     AL,11B
        CMP     AL,11B
        JNZ     CHANGE_MODE1
        MOV     AL,AH
        SHR     AL,1
CHANGE_MODE1:
        AND     BYTE PTR CS:[MODE],11111100B
        OR      BYTE PTR CS:[MODE],AL
        XOR     AH,AH
        CALL    PRINT_MODE;IN AL
CHANGE_MODEE1:
        POP     AX
        RET
CHANGE_MODEE:
        TEST    AX,100000000B
        JNZ     CHANGE_MODEE1
        AND     BYTE PTR CS:[MODE+1],11111110B
        TEST    AX,10100B
        JZ      CHANGE_MODEE1
        NOT     AX
        CALL    PRINT_MODE;IN AL
        JMP     SHORT   CHANGE_MODEE1
ENDP
PRINT_MODE      PROC
        PUSHA
        PUSH    DS
        PUSH    ES
        PUSH    CS
        PUSH    CS
        POP     DS
        POP     ES
        LEA     SI,CODE:STDSPR
        AND     AX,11B
        MOV     DX,RUSSPR-STDSPR
        MUL     DX
        ADD     SI,AX
        LEA     DI,CODE:SCRTBL
        CALL    SPR2SCR
        POP     ES
        POP     DS
        POPA
        RET
ENDP
NEW_KEYS        PROC
        CMP     CS:[KEY_CODE],0
        JZ      NEW_KEYS_E
        TEST    BYTE PTR CS:[MODE],110000B
        JNZ     NEW_KEYS_E
        MOV     DI,CS:[BUF_ADRESS]
        MOV     AX,40H
        MOV     ES,AX
        CMP     ES:[1CH],DI
        JZ      NEW_KEYS_E
        PUSH    CS
        POP     DS
        MOV     SI,CS:[KBD_OFFSET]
        MOV     AL,BYTE PTR CS:[MODE]
        AND     AL,1111B
        XOR     AH,AH
        SHL     AL,1
        ADD     SI,AX
        LODSW
        OR      AX,AX
        JZ      NEW_KEYS_E
        MOV     SI,CS:[KBD_OFFSET]
        ADD     SI,AX
        XOR     AX,AX
        MOV     AL,CS:[KEY_CODE]
        SUB     AL,2
        CMP     AL,K__
        JAE     NEW_KEYS_E
        ADD     SI,AX
        LODSB
        OR      AL,AL
        JZ      NEW_KEYS_E
        XOR     AH,AH
        STOSW
NEW_KEYS_E:
        RET
ENDP
LOADFKEYS       PROC
        TEST    BYTE PTR CS:[FLA],1000B
        JNZ     LFK2
        MOV     AL,BYTE PTR CS:[MODE]
        NOT     AL
        TEST    AL,110000B
        JNZ     LFK2
        TEST    CS:[MODE],1000000B
        JZ      LFK1
        CALL    RELOAD_FONT
LFK2:
        RET
LFK1:
        STC
        RET
ENDP
KEY_CODE        DB      0
MODE    DW      0
;0-1 = 01 - RUSSIAN
;    = 10 - GRAPHICS
;2 = 1 - SHIFT PRESSED
;3 = 1 - CAPS LOCK PRESSED
;4 = 1 - ALT PRESSED
;5 = 1 - CONTROL PRESSED
;6 = 1 - FIRST LOAD FONT PRESSED
;7 = 1 - FIRST RUS/LAT KEYBOARD PRESSED
;8 = 1 - RUS/LAT KEYBOARD PRESSED
;9 = 1 - NON ALT/SHIFT/CAPS/CTRL KEY AND LETTER PRESSED(KEYPAD PRESSED)
;10 = 1 - AFTER 0E0H CODE
BUF_ADRESS      DW      0
ENDP
INT_21  PROC
        ;OUT    AX - THE SAME
        ;BH - VERSION
        ;BL - SUB VERSION
        ;DS - WORK SEGMENT OF PROGRAM INSTALLED TO MEMORY
        ;DL OR DX - FLAGS
        CMP     AH,18H
        JZ      INT_211
        JMP     DWORD PTR CS:[OLD_21]
INT_211:
        PUSHF
        CALL    DWORD PTR CS:[OLD_21]
        JC      INT_21C
        CMP     CL,0;0 - IDENTIFICATION NUMBER OF PROGRAM 'SKRRUS'
        JNZ     INT_21E
        PUSH    CS
        POP     DS
        MOV     BX,CS:[VERSION_]
        MOV     DL,CS:[FLA]
INT_21C:
        PUSH    BP
        MOV     BP,SP
        OR      WORD PTR SS:[BP+6],1
        POP     BP
INT_21E:
        IRET
ENDP
RELOAD_FONT     PROC
        PUSHA
        PUSH    ES
        PUSH    CS
        POP     ES
        MOV     BP,CS:[FONT_OFFSET]
        MOV     CX,CS:[QUANTITY_SYMBOLS]
        MOV     DX,CS:[SHIFT_IN_CODES_TABLE]
        MOV     BL,CS:[BLOCK_SELECTOR]
        MOV     BH,CS:[HEIGHT_FONT]
        MOV     AX,1100H;1110H-?
        PUSHF
        CALL    DWORD PTR CS:[OLD_10]
        MOV     AX,1103H
        MOV     BL,CS:[BLOCK_SELECTOR]
        PUSHF
        CALL    DWORD PTR CS:[OLD_10]
        POP     ES
        POPA
        RET
ENDP
INCLUDE SPR2SCR.LIB
INCLUDE VIDEO.TBL
STDSPR  DD      CODE:STDWIN
        DW      1,17
        DW      ?,?
        DB      0,0
STDWIN  DB      'SxTxAxNxDxAxRxDx xKxExYxBxOxAxRxDx'
RUSSPR  DD      CODE:RUSWIN
        DW      1,17
        DW      ?,?
        DB      0,0
RUSWIN  DB      'CxYxRxIxLxLxIxCx xKxExYxBxOxAxRxDx'
GRSPR   DD      CODE:GRWIN
        DW      1,17
        DW      ?,?
        DB      0,0
GRWIN   DB      'GxRxAxPxHxIxCxSx xKxExYxBxOxAxRxDx'
SCRTBL  DW      ?,?,?,?
        DB      0

RUS_NORM        EQU     KEY_NORM-KEYTBL
RUS_SHIFT       EQU     KEY_SHIFT-KEYTBL
RUS_CAPS        EQU     KEY_CAPS-KEYTBL
RUS_SCAPS       EQU     KEY_SCAPS-KEYTBL
GR_NORM         EQU     KEY1_NORM-KEYTBL
KEYTBL  DW      0,RUS_NORM,GR_NORM,0
        DW      0,RUS_SHIFT,0,0
        DW      0,RUS_CAPS,0,0
        DW      0,RUS_SCAPS,0,0
;TABLE FROM CODE 2 TO 34H => 0 TO 33H  (*16 - MAX) *4 - HERE + KEYTBL HEADER
KEY_NORM        DB      14   DUP  (0)
                DB      'й','ц','у','к','е','н','г','ш','щ','з','х','ъ'
                DB      0,0
                DB      'ф','ы','в','а','п','р','о','л','д','ж','э'
                DB      0,0,0
                DB      'я','ч','с','м','и','т','ь','б','ю',0
KEY_SHIFT       DB      0,'"','/','¤',':',',','.',';','?','%'
                DB      0,0,0,0
                DB      'Й','Ц','У','К','Е','Н','Г','Ш','Щ','З','Х','Ъ'
                DB      0,0
                DB      'Ф','Ы','В','А','П','Р','О','Л','Д','Ж','Э'
                DB      0,0,0
                DB      'Я','Ч','С','М','И','Т','Ь','Б','Ю',0
KEY_CAPS        DB      14   DUP  (0)
                DB      'Й','Ц','У','К','Е','Н','Г','Ш','Щ','З','Х','Ъ'
                DB      0,0
                DB      'Ф','Ы','В','А','П','Р','О','Л','Д','Ж','Э'
                DB      0,0,0
                DB      'Я','Ч','С','М','И','Т','Ь','Б','Ю',0
KEY_SCAPS       DB      0,'"','/','¤',':',',','.',';','?','%'
                DB      0,0,0,0
                DB      'й','ц','у','к','е','н','г','ш','щ','з','х','ъ'
                DB      0,0
                DB      'ф','ы','в','а','п','р','о','л','д','ж','э'
                DB      0,0,0
                DB      'я','ч','с','м','и','т','ь','б','ю',0
KEY1_NORM       DB      '│','─','║','═','╙','╜','╧','√','№','°'
                DB      0,0,0,0
                DB      '┌','┬','┐','╒','╥','╕','╔','╦','╗','╓','╤','░'
                DB      0,0
                DB      '├','┼','┤','╞','╪','╡','╠','╬','╣','╟','╫'
                DB      0,0,0
                DB      '└','┴','┘','╘','╨','╛','╚','╩','╝',0
SFONT:
INCLUDE VGABOLD.INC
START:
        MOV     DS,DATS
        LEA     SI,CREDITS
        CALL    WW
        CALL    CPUID1
        JC      ERROR
        CALL    READ_FILE_LINE;AND SET FLAGS(EXTERNAL FONT , NOLOAD)
        JNC     START1
        CMP     AX,104H
        STC
        JNZ     ERROR
START1:
        CALL    INIT;INT , LOAD_FONT
ERROR:
        CALL    LEAVEPROG;EXIT WITHOUT CLEAR
READ_FILE_LINE  PROC
        PUSH    DS
        PUSH    ES
        POP     DS
        POP     ES
        MOV     SI,80H
        LEA     AX,FST_
        LEA     BX,SST
        LEA     DX,NTBL
        LEA     DI,FILEBUF
        CALL    RL
        MOV     ES:[FILES],CX
        JC      RFL_ER
        PUSH    ES
        POP     DS
        MOV     AX,DS:[NTBL]
        MOV     DS:[FLAGS],AX
        MOV     AX,DS:[NTBL+2]
        AND     AX,0FFFEH
        OR      DS:[FLAGS],AX
        MOV     AX,10AH
        CMP     CX,3;? DONT USE 3 OR MORE
        CMC
RFL_ER:
        RET
ENDP
INCLUDE READLINE.LIB
include cpuid.inc
INIT    PROC
        ;IN DS,ES - DATA
        CLC
        MOV     DS,DATS
        MOV     ES,DATS
        CALL    DETECT_ALREADY
        MOV     BP,DS:[FLAGS]
        CALL    VERIFY_NO_SWITCH
        CALL    VERIFY_CAPSS
        CALL    INIT_MEM
        CALL    LOAD_EXT
        JC      INIT_E
        CALL    LOAD_FONT
INIT_E:
        PUSHF
        PUSH    AX
        CALL    INIT_INT_9
        CALL    INIT_INT_10
        POP     AX
        POPF
        RET
ENDP
DETECT_ALREADY  PROC
        PUSH    DS
        PUSH    ES
        MOV     ES,DATS
        MOV     AH,18H
        XOR     CL,CL
        CLC
        INT     21H
        JNC     DETECT_ALREADY1
        MOV     ES:[ALREADII],DL
        ;BX-???????
        MOV     ALS,DS
DETECT_ALREADY1:
        POP     ES
        POP     DS
        RET
ENDP
VERIFY_CAPSS    PROC
        PUSH    AX
        PUSH    DS
        MOV     AX,40H
        MOV     DS,AX
        TEST    BYTE PTR DS:[17H],1000000B
        JZ      VERIFY_CAPSS1
        OR      BYTE PTR CS:[MODE],1000B
VERIFY_CAPSS1:
        POP     DS
        POP     AX
        RET
ENDP
VERIFY_NO_SWITCH        PROC
        PUSH    ES
        MOV     AX,BP
        AND     AL,1100000B
        SHR     AL,1
        MOV     ES,ALS
        AND     BYTE PTR ES:[FLA],11001111B
        OR      BYTE PTR ES:[FLA],AL
        POP     ES
        RET
ENDP
INIT_MEM        PROC
        PUSH    DS
        PUSH    ES
        MOV     ES,ALS
        MOV     DS,DATS
        TEST    BYTE PTR DS:[ALREADII],1
        JNZ     INIT_MEM1
        TEST    BP,10000B
        JNZ     INIT_MEME
        XOR     AX,AX
        MOV     DS,AX
        LEA     BX,INT_21
        MOV     AX,ES
        CLI
        XCHG    DS:[21H*4],BX
        XCHG    DS:[21H*4+2],AX
        STI
        MOV     ES:[OLD_21],BX
        MOV     ES:[OLD_21+2],AX
        JMP     SHORT   INIT_MEM2
INIT_MEM1:
        TEST    BP,10000B
        JZ      INIT_MEME
        XOR     AX,AX
        MOV     DS,AX
        MOV     AX,ES:[OLD_21+2]
        MOV     BX,ES:[OLD_21]
        CLI
        XCHG    DS:[21H*4+2],AX
        XCHG    DS:[21H*4],BX
        STI
INIT_MEM2:
        XOR     BYTE PTR ES:[FLA],1
INIT_MEME:
        POP     ES
        POP     DS
        RET
ENDP
INIT_INT_9      PROC
        PUSH    DS
        PUSH    ES
        MOV     DS,DATS
        MOV     ES,ALS
        TEST    BYTE PTR DS:[ALREADII],100B
        JNZ     INIT_INT_91
        TEST    BP,10000B
        JNZ     INIT_INT_9E
        TEST    BP,100B; 2 bits = 0 - initial int 9
        JNZ     INIT_INT_9E
        XOR     AX,AX
        MOV     DS,AX
        LEA     BX,INT_9
        MOV     AX,ES
        CLI
        XCHG    DS:[9*4],BX
        XCHG    DS:[9*4+2],AX
        STI
        MOV     ES:[OLD_9],BX
        MOV     ES:[OLD_9+2],AX
        JMP     SHORT   INIT_INT_92
INIT_INT_91:
        TEST    BP,100B; 2 bits = 0 - initial int 9
        JNZ     INIT_INT_93
        TEST    BP,10000B
        JZ      INIT_INT_9E
INIT_INT_93:
        XOR     AX,AX
        MOV     DS,AX
        MOV     AX,ES:[OLD_9+2]
        MOV     BX,ES:[OLD_9]
        CLI
        XCHG    DS:[9H*4+2],AX
        XCHG    DS:[9H*4],BX
        STI
INIT_INT_92:
        XOR     BYTE PTR ES:[FLA],100B
INIT_INT_9E:
        POP     ES
        POP     DS
        RET
ENDP
INIT_INT_10     PROC
        PUSH    DS
        PUSH    ES
        MOV     DS,DATS
        MOV     ES,ALS
        TEST    BYTE PTR DS:[ALREADII],10B
        JNZ     INIT_INT_101
        TEST    BP,10000B
        JNZ     INIT_INT_10E
        TEST    BP,10B;1 bit = 0 - initial int 10h
        JNZ     INIT_INT_10E
        XOR     AX,AX
        MOV     DS,AX
        LEA     BX,INT_10
        MOV     AX,ES
        CLI
        XCHG    DS:[10H*4],BX
        XCHG    DS:[10H*4+2],AX
        STI
        MOV     ES:[OLD_10],BX
        MOV     ES:[OLD_10+2],AX
        JMP     SHORT   INIT_INT_102
INIT_INT_101:
        TEST    BP,10B;1 bit = 0 - initial int 10h
        JNZ     INIT_INT_103
        TEST    BP,10000B
        JZ      INIT_INT_10E
INIT_INT_103:
        XOR     AX,AX
        MOV     DS,AX
        MOV     AX,ES:[OLD_10+2]
        MOV     BX,ES:[OLD_10]
        CLI
        XCHG    DS:[10H*4+2],AX
        XCHG    DS:[10H*4],BX
        STI
INIT_INT_102:
        XOR     BYTE PTR ES:[FLA],10B
INIT_INT_10E:
        POP     ES
        POP     DS
        RET
ENDP
LOAD_EXT        PROC
        PUSH    BP
        MOV     AL,CS:[HEIGHT_FONT]
        PUSH    CS
        POP     DS
        MOV     SI,CS:[FONT_OFFSET]
        TEST    BP,1;0 bit = 1 - external font
        JZ      LOAD_EXT1
        MOV     DS,DATS
        CMP     DS:[FILES],1;if null files selected - no external font
        JB      WHAT_LOAD
        MOV     ES,DATS
        LEA     SI,FILEBUF
        MOV     DS,SFS
        XOR     DX,DX
        MOV     AX,-1
        CALL    LOAD_FILE
        JC      LOAD_EXT_ER
        MOV     AL,CH
        CMP     AL,CS:[HEIGHT_FONT]
        JA      LOAD_EXT_BIG
        MOV     DS,SFS
        XOR     SI,SI
LOAD_EXT1:
        MOV     AH,CS:[HEIGHT_FONT]
        MOV     CX,CS:[QUANTITY_SYMBOLS]
        MOV     ES,ALS
        MOV     DI,ES:[FONT_OFFSET]
        XOR     BP,BP
        CALL    CONVERT_FONT
        POP     BP
        RET
LOAD_EXT_ER:
        MOV     DS,DATS
        LEA     SI,DISK_ERROR
        JMP     SHORT   WWW
LOAD_EXT_BIG:
        MOV     DS,DATS
        LEA     SI,FONT_VERY_LARGE
        MOV     AX,81H
        JMP     SHORT   WWW
WHAT_LOAD:
        MOV     DS,DATS
        LEA     SI,WHAT_LOADING
        MOV     AX,80H
WWW:
        CALL    WW
        POP     BP
        STC
        RET
ENDP
CONVERT_FONT    PROC
        ;IN AL- HEIGHT OLD FONT
        ;AH - HEIGHT NEW FONT
        ;DS:SI - ADRESS OLD FONT
        ;ES:DI - ADRESS NEW FONT
        ;CX - QUANTITY SYMS
        ;BP - BEGIN SYMBOL
        CLD
        CMP     AL,AH
        JZ      CONV_EQU
        JA      CONV_COMPRESS
        JMP     SHORT   CONVERT_WIDEN
CONV_EQU:
        JMP     SHORT   CONVERT_EQUAL
CONV_COMPRESS:
ENDP
CONVERT_COMPRESS        PROC
        ;MAY BE C=1
        STC
        RET
ENDP
CONVERT_EQUAL   PROC
        PUSHA
        XOR     AH,AH
        MUL     CX
        MOV     CX,AX
        REP     MOVSB
        POPA
        RET
ENDP
CONVERT_WIDEN   PROC
        PUSHA
        SUB     AH,AL
        MOV     BX,AX
        XOR     BH,BH
        XOR     AL,AL
        XCHG    AH,AL
        MOV     DX,AX
        SHR     AX,1
        SUB     DX,AX
        MOV     DH,AL
        MOV     AH,BL
        MOV     BX,CX
        ADD     bX,BP
        XOR     AL,AL
CONVERT_WIDEN1:
        CMP     BX,50H;(100H-0B0H)
        JBE     CONVERT_WIDEN2
        CALL    CONVERT_WIDEN_LETTER
        DEC     BX
        CMP     BX,BP
        JB      CONVERT_WIDEN_E
        JMP     SHORT   CONVERT_WIDEN1
CONVERT_WIDEN2:
        SUB     AH,2
        ADD     DX,101H
CONVERT_WIDEN2A:
        CMP     BX,20H;(100H-0B0H)
        JBE     CONVERT_WIDEN3
        CALL    CONVERT_WIDEN_PSEUDOGRAPHICS
        DEC     BX
        CMP     BX,BP
        JB      CONVERT_WIDEN_E
        JMP     SHORT   CONVERT_WIDEN2A
CONVERT_WIDEN3:
        XOR     AL,AL
        ADD     AH,2
        SUB     DX,101H
CONVERT_WIDEN3A:
        CALL    CONVERT_WIDEN_LETTER
        DEC     BX
        CMP     BX,BP
        JA      CONVERT_WIDEN3A
CONVERT_WIDEN_E:
        POPA
        RET
ENDP
CONVERT_WIDEN_LETTER    PROC
        XOR     CH,CH
        MOV     CL,DH
        REP     STOSB
        MOV     CL,AH
        REP     MOVSB
        MOV     CL,DL
        REP     STOSB
        RET
ENDP
CONVERT_WIDEN_PSEUDOGRAPHICS    PROC
        LODSB
        MOV     CL,DH
        REP     STOSB
        MOV     CL,AH
        REP     MOVSB
        LODSB
        MOV     CL,DL
        REP     STOSB
        RET
ENDP

LOAD_FONT       PROC
        TEST    BP,1000B;3 bit = 1 - no load font
        JNZ     LOAD_FONT1
        PUSHA
        PUSH    ES
        MOV     ES,ALS
        MOV     BP,ES:[FONT_OFFSET]
        AND     ES:[FLA],0F7H
        MOV     CX,ES:[QUANTITY_SYMBOLS]
        MOV     DX,ES:[SHIFT_IN_CODES_TABLE]
        MOV     BL,ES:[BLOCK_SELECTOR]
        MOV     BH,ES:[HEIGHT_FONT]
        MOV     AX,1100H;1110H-?
        INT     10H
        MOV     AX,1103H
        MOV     BL,ES:[BLOCK_SELECTOR]
        INT     10H
        POP     ES
        POPA
        RET
LOAD_FONT1:
        MOV     ES,ALS
        OR      ES:[FLA],1000B
        RET
ENDP
LEAVEPROG       PROC
        JC      LEAVEPROG2
LEAVEPROG21:
        MOV     DS,DATS
        TEST    BYTE PTR DS:[ALREADII],1
        JNZ     LEAVEPROG1
        TEST    BYTE PTR DS:[FLAGS],10000B
        JNZ     LEAVEPROG1A
        MOV     AX,3100H
        MOV     DX,10H+10H+(LREZ)
        INT     21H
LEAVEPROG1:
        TEST    BYTE PTR DS:[FLAGS],10000B
        JZ      LEAVEPROG1A
        MOV     AX,ALS
        SUB     AX,20H
        MOV     ES,AX
        XOR     BX,BX
        DEC     BX
        MOV     AH,4AH
        INT     21H
        MOV     AH,49H
        int     21h
LEAVEPROG1A:
        MOV     AX,4C00H
        INT     21H
LEAVEPROG2:
        CMP     AX,0FFFFH
        LEA     SI,NOT_FOR_8086
        JZ      LEAVEPROGEE
        CMP     AX,100H
        JB      LEAVEPROG21
        MOV     DS,DATS
        LEA     SI,USAGE
LEAVEPROGEE:
        CALL    WW
        MOV     AX,4CFFH
        INT     21H
ENDP

INCLUDE WW.LIB
INCLUDE IO.LIB

ECS:
ENDS
DATA    SEGMENT PARA
BDS     EQU     FONT_VERY_LARGE
FONT_VERY_LARGE DB      'Шрифт очень высок',0dh,0ah,0
DISK_ERROR      DB      'Ошибка при работе с диском',0dh,0ah,0
WHAT_LOADING    DB      'Что загружаем ?',0dh,0ah,0
NOT_FOR_8086    DB      'Программа не для 8086 или 8088 процессоров',0dh,0ah,0
USAGE           DB      '   Использование: SKRRUS.EXE <Команды> [Имена файлов]',0dh,0ah
                DB      '   Команды:',0dh,0ah
                DB      '    /E  - Загрузить шрифт из файла',0dh,0ah
                DB      '    /NF - Не устанавливать перезагрузку шрифта',0dh,0ah
                DB      '    /NK - Не устанавливать принудительную перезагрузку шрифта',0dh,0ah
                DB      '    /NL - Не загружать шрифт',0dh,0ah
                DB      '    /NM - Не загружать программу в память или выгрузить программу из памяти',0dh,0ah
                DB      '    /NR - Не переключаться в русскую клавиатуру',0dh,0ah
                DB      '    /NG - Не переключаться в псевдо-графическую клавиатуру',0dh,0ah
                DB      '   "Ctrl"+"Alt"  - Принудительная перезагрузка шрифта',0dh,0ah
                DB      '   "Shift"+"Alt" - Переключение клавиатуры ENG/RUS/GRAPH',0dh,0ah,0
CREDITS         DB      'VGA font and keyboard driver 1.04 by Pavel A. Skrylev (C)1997',0dh,0ah
                DB      'All Right Reserved',0dh,0ah,0

ALREADII        DB      0;!

FST_    DB      '/EN',0FFH
SST     DB      'NFKLMRG',0FFH
NTBL    DW      0,0

FLAGS   DW      0
;0 - 1=EXTERNAL FONT
;1 - 1=NO USE INT 10
;2 - 1=NOT A FORSE LOAD FONT (INT 9)
;3 - 1=NO LOAD FONT
;4 - 1=NO LOAD TO MEMORY OR REMOVE FROM MEMORY
;5 - 1=NO SWITCH TO RUSSIAN
;6 - 1=NO SWITCH TO PSEUDO-GRAPHICS
FILES   DW      0
FILEBUF DB      13*2   DUP     (?)
EDS     DB      0
ENDS
END     START
