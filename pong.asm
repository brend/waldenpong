  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0
  
buttons1  .rs 1
buttons2  .rs 1
ballX     .rs 1
ballXneg  .rs 1
ballY     .rs 1
ballYneg  .rs 1

;;;;;;;;;;;;;;;

    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001
  
InitObjects:
  LDA #$02
  STA ballY       ; initialize vertical ball velocity with 1
  LDA #$01
  STA ballXneg
  LDA #$01
  STA ballX

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


  JSR ReadControllers
  
HandleUp:
  LDA buttons1
  AND #%00001000
  BEQ HandleUpDone

  LDA $0200       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0200       ; save sprite X position  

  LDA $0204       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0204       ; save sprite X position  

  LDA $0208       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0208       ; save sprite X position  

;  LDA $020C       ; load sprite X position
;  SEC             ; make sure carry flag is set
;  SBC #$01        ; A = A - 1
;  STA $020C       ; save sprite X position  
HandleUpDone:

HandleDown:
  LDA buttons1
  AND #%00000100
  BEQ HandleDownDone

  LDA $0200       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $0200       ; save sprite X position  

  LDA $0204       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $0204       ; save sprite X position  

  LDA $0208       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $0208       ; save sprite X position  

;  LDA $020C       ; load sprite X position
;  CLC             ; make sure carry flag is set
;  ADC #$01        ; A = A - 1
;  STA $020C       ; save sprite X position  
HandleDownDone:
  
HandleUp2:
  LDA buttons2
  AND #%00001000
  BEQ HandleUpDone2

  LDA $020C       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $020C       ; save sprite X position  

  LDA $0210       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0210       ; save sprite X position  

  LDA $0214       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0214       ; save sprite X position  
HandleUpDone2:
  
HandleDown2:
  LDA buttons2
  AND #%00000100
  BEQ HandleDownDone2

  LDA $020C       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $020C       ; save sprite X position  

  LDA $0210       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $0210       ; save sprite X position  

  LDA $0214       ; load sprite X position
  CLC             ; make sure carry flag is set
  ADC #$01        ; A = A - 1
  STA $0214       ; save sprite X position
HandleDownDone2:
  
MoveBall:  
  ; $0218 is base address of ball sprite
  
  
MoveBallY:
  LDA ballYneg
  CMP #$01
  BEQ MoveBallNegY
  
  LDA $0218       ; load ball Y position
  CLC
  ADC ballY       ; add ball Y velocity
  STA $0218
  JMP MoveBallYDone
MoveBallNegY:
  LDA $0218
  SEC
  SBC ballY
  STA $0218
MoveBallYDone:
  
MoveBallX:
  LDA ballXneg
  CMP #$01
  BEQ MoveBallNegX
  
  LDA $021B       ; load ball X position
  CLC
  ADC ballX       ; add ball X velocity
  STA $021B
  JMP MoveBallXDone
MoveBallNegX:
  LDA $021B
  SEC
  SBC ballX
  STA $021B
MoveBallXDone:
  
FlipBallYIfBottomWall:
  LDA $0218       ; compare y position of ball with constant 230 and flip y velocity if greater or equal
  CMP #$E6
  BCC AfterFlipBallYIfBottomWall
  LDA #$01
  STA ballYneg
AfterFlipBallYIfBottomWall:
FlipBallYIfTopWall:
  LDA $0218
  CMP #$02
  BCS AfterFlipBallYIfTopWall
  LDA #$00
  STA ballYneg
AfterFlipBallYIfTopWall:
  
  RTI             ; return from interrupt
  
;;;;;;;;;;;;;;

ReadControllers:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  
; prepare for reading controller 2
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
  
ReadController2Loop:
  LDA $4017
  LSR A
  ROL buttons2
  DEX
  BNE ReadController2Loop

  RTS
 
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  ;  .db $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  ;  .db $0F,$17,$28,$18,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C
  .db $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
  .db $0F,$2C,$1C,$3C,$31,$02,$38,$3C,$17,$17,$08,$36,$31,$02,$38,$3C
  ;     0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
  ;     0               1               2               3

sprites:
     ;vert tile attr horiz
  .db $80, $01, $00, $10   ;sprite 0: paddle 1 top
  .db $88, $02, $00, $10   ;sprite 1: paddle 1 center
  .db $90, $01, $80, $10   ;sprite 2: paddle 1 bottom
  
  .db $80, $01, $42, $E0   ;sprite 3: paddle 2 top
  .db $88, $02, $42, $E0   ;sprite 4: paddle 2 center
  .db $90, $01, $C2, $E0   ;sprite 5: paddle 2 bottom
  
  ; address 0218
  .db $88, $03, $03, $96   ;sprite 6: ball
  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "pong.chr"   ;includes 8KB graphics file from SMB1