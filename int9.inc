;DATA
RSHIFT  EQU     0
LSHIFT  EQU     1
CTRL    EQU     2
ALT     EQU     3

MODE    DB      0
SHIFT   DB      0
NSTATE  DB      0
OSTATE  DB      0
MASKA   DB      1100B

;0 - STANDARD MODE     - 0-MASKED
;1 - CYRILLIC KEYBOARD - 0-MASKED
;2 - PSEUDOGRAPHICS    - 0-MASKED
;3 - NOT USED          - 0-MASKED

M_TBL   DW      STD_STR,17
        DW      RUS_STR,17
        DW      GRAPH_STR,17
        DW      USER_STR,21
        ;       SHIFT,LEN
STD_STR DB      'SxTxAxNxDxAxRxDx xKxExYxBxOxAxRxDx'
RUS_STR DB      'CxYxRxIxLxLxIxCx xKxExYxBxOxAxRxDx'
GRAPH_STR DB    'GxRxAxPxHxIxCxSx xKxExYxBxOxAxRxDx'
USER_STR DB     'UxSxExRx xDxExFxIxNxExDx xKxExYxBxOxAxRxDx'
STR_BUF DB      64      DUP     (0)

KEYBUF_TAIL     DW      0

INT_9   PROC
        CLD
        NOP
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    DS
        PUSH    ES
        XOR     AX,AX
        MOV     DS,AX
        MOV     AX,DS:[41CH]
        MOV     CS:[KEYBUF_TAIL],AX
        XOR     AX,AX
        IN      AL,60H
        PUSH    AX
        PUSHF
        CALL    DWORD PTR CS:[OLD_9]
        POP     AX
        CALL    GET_MODE ; IN AL - SCAN CODE ; OUT AL - MODE 0 - SAME, 1-4 NEW
;        OR      AL,AL
;        JNZ     INT9_EXIT
        CALL    REPLACE_SCANCODE
INT9_EXIT:
        POP     ES
        POP     DS
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        IRET
ENDP
GET_MODE        PROC
        PUSH    BX
        MOV     BX,AX
        MOV     AL,CS:[NSTATE]
        MOV     CS:[OSTATE],AL
        XOR     AX,AX
        ASSUME  DS:0
        MOV     DS,AX
        MOV     AL,DS:[417H]
        MOV     AH,AL
        AND     AX,0000001000001000B
        RCR     AL,ALT-LSHIFT
        AND     AL,AH
        RCR     AL,LSHIFT
        MOV     CS:[NSTATE],AL
        XOR     AL,CS:[OSTATE]
        JZ      GET_MODE_EXIT
        CMP     BYTE PTR CS:[NSTATE],1
        JNZ     GET_MODE_RESTORE_SCREEN
        INC     BYTE PTR CS:[MODE]
        AND     BYTE PTR CS:[MODE],11B
        XOR     AX,AX
        MOV     AL,CS:[MODE]
        PUSH    AX
        RCL     AL,2
        MOV     BX,AX
        MOV     AX,CS
        MOV     DS,AX
        MOV     SI,DS:[BX+M_TBL]
        JMP     SHORT   GET_MODE_SHOW
GET_MODE_RESTORE_SCREEN:
        XOR     AX,AX
        MOV     AL,CS:[MODE]
        PUSH    AX
        RCL     AL,2
        MOV     BX,AX
        MOV     AX,CS
        MOV     DS,AX
        LEA     SI,DS:STR_BUF
GET_MODE_SHOW:
        MOV     CX,DS:[BX+M_TBL+2]
        LEA     DI,DS:STR_BUF
        CALL    SWAP_STRING
        POP     AX
        INC     AX
GET_MODE_EXIT:
        POP     BX
        RET
ENDP
SWAP_STRING     PROC
        ;IN
        ;DS:SI - SOURSE NEW STRING
        ;DS:DI - DESTINATION POPUP STRING
        ;CX    - STRING LENGHT
        PUSH    AX
        PUSH    BX
        PUSH    DX
        PUSH    DS
        XOR     AX,AX
        MOV     DS,AX
        CMP     BYTE PTR DS:[449H],4
        JNC     SWAP_STRING_EXIT
        MOV     AL,DS:[462H]
        MOV     BX,AX
        MOV     BX,DS:[450H+BX]
        MOV     AX,DS:[44AH]
        XOR     DX,DX
        XCHG    DL,BH
        MUL     DX
        ADD     BX,AX
        SHL     BX,1
        XCHG    BX,DI
        MOV     AX,DS:[44EH]
        MOV     ES,AX
        POP     DS
SWAP_STRING1:
        LODSW
        MOV     DX,ES:[DI] ;FROM VIDEO MEM
        STOSW
        MOV     DS:[BX],DX
        INC     BX
        INC     BX
        LOOP    SWAP_STRING1
SWAP_STRING_EXIT:
        POP     DX
        POP     BX
        POP     AX
        RET
ENDP
REPLACE_SCANCODE        PROC

        XOR     AX,AX
        MOV     BYTE PTR CS:[SHIFT],AL
        MOV     DS,AX
        MOV     AX,DS:[41CH]
        CMP     AX,CS:[KEYBUF_TAIL]
        JZ      REPLACE_SCANCODE_EXIT
        MOV     DI,CS:[KEYBUF_TAIL]
        MOV     AL,DS:[417H]
        MOV     AH,AL
        MOV     AX,0100000000000011B
        OR      AL,AL
        JZ      REPLACE_SCANCODE1
        OR      BYTE PTR CS:[SHIFT],1
REPLACE_SCANCODE1:
        OR      AH,AH
        JZ      REPLACE_SCANCODE2
        OR      BYTE PTR CS:[SHIFT],2
REPLACE_SCANCODE2:
        XOR     AX,AX
        MOV     AL,CS:[MODE]
        SHL     AX,3
        MOV     BX,AX
        MOV     AL,CS:[SHIFT]
        SHL     AX,1
        ADD     BX,AX
        MOV     BX,CS:[BX+KEYTBL]
        OR      BX,BX
        JZ      REPLACE_SCANCODE_EXIT
        MOV     AX,DS:[DI]
        XOR     AH,AH
        DEC     AX
        DEC     AX
        ADD     BX,AX
        XOR     AX,AX;??
        MOV     AH,BYTE PTR CS:[BX+KEYTBL]
        MOV     DS:[DI],AX
REPLACE_SCANCODE_EXIT:
        RET

ENDP

RUS_NORM        EQU     KEY_NORM-KEYTBL
RUS_SHIFT       EQU     KEY_SHIFT-KEYTBL
RUS_CAPS        EQU     KEY_CAPS-KEYTBL
RUS_SCAPS       EQU     KEY_SCAPS-KEYTBL
GR_NORM         EQU     KEY1_NORM-KEYTBL

KEYTBL  DW      0,0,0,0
        DW      RUS_NORM,RUS_SHIFT,RUS_CAPS,RUS_SCAPS
        DW      GR_NORM,0,0,0
        DW      0,0,0,0
;TABLE FROM CODE 2 TO 34H => 0 TO 33H  (*16 - MAX) *4 - HERE + KEYTBL HEADER
KEY_NORM        DB      14   DUP  (0)
                DB      '©','Ê','„','™','•','≠','£','Ë','È','ß','Â','Í'
                DB      0,0
                DB      '‰','Î','¢','†','Ø','‡','Æ','´','§','¶','Ì'
                DB      0,0,0
                DB      'Ô','Á','·','¨','®','‚','Ï','°','Ó',0
KEY_SHIFT       DB      0,'"','/','˝',':',',','.',';','?','%'
                DB      0,0,0,0
                DB      'â','ñ','ì','ä','Ö','ç','É','ò','ô','á','ï','ö'
                DB      0,0
                DB      'î','õ','Ç','Ä','è','ê','é','ã','Ñ','Ü','ù'
                DB      0,0,0
                DB      'ü','ó','ë','å','à','í','ú','Å','û',0
KEY_CAPS        DB      14   DUP  (0)
                DB      'â','ñ','ì','ä','Ö','ç','É','ò','ô','á','ï','ö'
                DB      0,0
                DB      'î','õ','Ç','Ä','è','ê','é','ã','Ñ','Ü','ù'
                DB      0,0,0
                DB      'ü','ó','ë','å','à','í','ú','Å','û',0
KEY_SCAPS       DB      0,'"','/','˝',':',',','.',';','?','%'
                DB      0,0,0,0
                DB      '©','Ê','„','™','•','≠','£','Ë','È','ß','Â','Í'
                DB      0,0
                DB      '‰','Î','¢','†','Ø','‡','Æ','´','§','¶','Ì'
                DB      0,0,0
                DB      'Ô','Á','·','¨','®','‚','Ï','°','Ó',0
KEY1_NORM       DB      '≥','ƒ','∫','Õ','”','Ω','œ','˚','¸','¯'
                DB      0,0,0,0
                DB      '⁄','¬','ø','’','“','∏','…','À','ª','÷','—','∞'
                DB      0,0
                DB      '√','≈','¥','∆','ÿ','µ','Ã','Œ','π','«','◊'
                DB      0,0,0
                DB      '¿','¡','Ÿ','‘','–','æ','»',' ','º',0
