  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0
  
buttons1      .rs 1
buttons2      .rs 1
vx            .rs 1
vy            .rs 1
hit           .rs 1
ball_top      .rs 1
ball_bottom   .rs 1
paddle_top    .rs 1
paddle_bottom .rs 1
addr_lo       .rs 1
addr_hi       .rs 1
offset        .rs 1

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
  LDA #$00
  STA vy
  LDA #$fe
  STA vx

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


  JSR ReadControllers ; write controller states to buttons1, buttons2
  
HandleUp:
  LDA buttons1
  AND #%00001000
  BEQ HandleDown
  
  LDA #$02          ; HIGH(#$0200)
  STA addr_hi
  LDA #$00          ; LOW(#$0200)
  STA addr_lo
  LDA #$FE
  STA offset
  JSR ShmandleInput
  
HandleDown:
  LDA buttons1
  AND #%00000100
  BEQ HandleUp2

  LDA #$02          ; HIGH(#$0200)
  STA addr_hi
  LDA #$00          ; LOW(#$0200)
  STA addr_lo
  LDA #$02
  STA offset
  JSR ShmandleInput
  
HandleUp2:
  LDA buttons2
  AND #%00001000
  BEQ HandleDown2
  
  LDA #$02          ; HIGH(#$020C)
  STA addr_hi
  LDA #$0C          ; LOW(#$020C)
  STA addr_lo
  LDA #$FE
  STA offset
  JSR ShmandleInput
  
HandleDown2:
  LDA buttons2
  AND #%00000100
  BEQ MoveBall

  LDA #$02          ; HIGH(#$020C)
  STA addr_hi
  LDA #$0C          ; LOW(#$020C)
  STA addr_lo
  LDA #$02
  STA offset
  JSR ShmandleInput
    
MoveBall:  
  ; $0218 is base address of ball sprite
MoveBallY:
  LDA $0218       ; load ball Y position
  CLC
  ADC vy          ; add ball Y velocity
  STA $0218
  
MoveBallX:
  LDA $021B       ; load ball X position
  CLC
  ADC vx          ; add ball X velocity
  STA $021B
  
FlipBallYIfBottomWall:
  LDA $0218       ; compare y position of ball with constant 230 and flip y velocity if greater or equal
  CMP #$E6
  BCC AfterFlipBallYIfBottomWall
  LDA #$01
  CLC
  SBC vy
  STA vy
AfterFlipBallYIfBottomWall:
  
FlipBallYIfTopWall:
  LDA $0218
  CMP #$07
  BCS AfterFlipBallYIfTopWall
  LDA #$01
  CLC
  SBC vy
  STA vy
AfterFlipBallYIfTopWall:
  
  LDA $0218             ; store ball_top, ball_bottom for future reference
  STA ball_top
  
  LDA ball_top
  CLC
  ADC #$08
  STA ball_bottom
  
FlipBallXIfLeftPaddle:
  LDA $021B             ; ball x < lpaddle x + lpaddle width => check lpaddle collision
  CMP #$18        
  BCC CheckLeftPaddle

FlipBallXIfRightPaddle:
  LDA $021B
  CMP #$E0              ; ball x >= rpaddle x => check rpaddle collision
  BCS CheckRightPaddle
  JMP PaddleCheckDone
  
CheckLeftPaddle:
  LDA $0200
  STA paddle_top
  
  LDA paddle_top
  CLC
  ADC #$18
  STA paddle_bottom
  
  JSR PaddleContactCheck
  JMP PaddleCheckDone
  
CheckRightPaddle:
  LDA $020C
  STA paddle_top
  
  LDA paddle_top
  CLC
  ADC #$18
  STA paddle_bottom
  
  JSR PaddleContactCheck
  JMP PaddleCheckDone
  
PaddleCheckDone:
  RTI             ; return from interrupt
  
;;;;;;;;;;;;;;;
;; Subroutine ShmandleInput
;;;;;;;;;;;;;;;
ShmandleInput:
  LDY #$00
ShmandleLoop:
  LDA [addr_lo], y      ; DO NOT USE PARENTHESES for indirect addressing, e.g. "LDA (addr_lo), y" - this syntax doesn't work with NESASM; it leads to no error message, but nothing will happen
  CLC
  ADC offset
  STA [addr_lo], y
  INY
  CPY #$09
  BCS DoneShmandlingInput
  INY
  INY
  INY
  JMP ShmandleLoop
DoneShmandlingInput:
  RTS
  
  
;;;;;;;;;;;;;;;
;; Subroutine PaddleContactCheck
;;  Input: expects variables paddle_top, paddle_bottom, ball_top, ball_bottom to be filled
;; Output: writes new ball velocities to vx, vy
;;;;;;;;;;;;;;;

PaddleContactCheck:
  LDA ball_bottom     ; if ball_bottom < paddle_top then no contact
  CMP paddle_top
  BCC PaddleContactCheckDone
  
  LDA ball_top        ; if ball_top >= paddle_bottom then no contact
  CMP paddle_bottom
  BCS PaddleContactCheckDone
  
  LDA ball_top        ; ball_top - paddle_top
  SEC
  SBC paddle_top
  CLC
  ADC #$07            ; add ball_height - 1
  STA hit

  LDA #$FE            ; vy := -3
  STA vy
  
  LDA hit
  CMP #$04            ; if hit >= 4 then inc(vy)
  BCC Step2
  INC vy
Step2:
  LDA hit
  CMP #$08            ; if hit >= 8 then inc(vy)
  BCC Step3
  INC vy
Step3:
  LDA hit
  CMP #$0C            ; if hit >= 12 then inc(vy)
  BCC Step4
  INC vy
Step4:
  LDA hit
  CMP #$10            ; if hit >= 16 then inc(vy)
  BCC Step5
  INC vy
Step5:
  LDA hit
  CMP #$14            ; etc
  BCC Step6
  INC vy
Step6:
  LDA hit
  CMP #$18
  BCC FlipX
  INC vy
  
FlipX:
  LDA #$01        ; flip ball vx
  CLC
  SBC vx
  STA vx

PaddleContactCheckDone:
  RTS
  
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
  ; 200
  .db $80, $01, $00, $10   ;sprite 0: paddle 1 top
  .db $88, $02, $00, $10   ;sprite 1: paddle 1 center
  .db $90, $01, $80, $10   ;sprite 2: paddle 1 bottom
  
  ; 20C
  .db $80, $01, $42, $E0   ;sprite 3: paddle 2 top
  .db $88, $02, $42, $E0   ;sprite 4: paddle 2 center
  .db $90, $01, $C2, $E0   ;sprite 5: paddle 2 bottom
  
  ; 218
  ; address 0218
  .db $88, $03, $03, $96   ;sprite 6: ball
  
  .db $00, $00, $00, $00   ;buffer - because loadsprites copies 32 bytes
  
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