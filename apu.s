;
; Updates the APU registers. x and y are free to use
;

.if 0
; Found this on nesdev bbs by blargg,
; this can replace the volume table but takes a little more CPU
ft_get_volume:

	lda var_ch_VolColumn, x
	lsr a
	lsr a
	lsr a
	sta var_Temp
	lda var_ch_Volume, x
	sta var_Temp2

	lda var_Temp				; 4x4 multiplication
	lsr var_Temp2
	bcs :+
	lsr a
:   lsr var_Temp2
	bcc :+
	adc var_Temp
:   lsr a
	lsr var_Temp2
	bcc :+
	adc var_Temp
:   lsr a
	lsr var_Temp2
	bcc :+
	adc var_Temp
:   lsr a
	beq :+
	rts
:	lda var_Temp
	ora var_ch_Volume, x
	beq :+
	lda #$01					; Round up to 1
:	rts
.endif

ft_update_apu:
	lda var_PlayerFlags
	bne @Play
	lda #$00					; Kill all channels
	sta $4015
	rts
@KillSweepUnit:					; Reset sweep unit to avoid strange problems
	lda #$C0
	sta $4017
	lda #$40
	sta $4017
	rts
@Play:

; ==============================================================================
;  Square 1
; ==============================================================================
	lda var_Channels
	and #$01
	bne :+
	jmp @Square2
:	lda var_ch_Note				; Kill channel if note = off
	bne :+						; branch
	jmp @KillSquare1
:

	; Calculate volume
.if 0
	ldx #$00
	jsr ft_get_volume
	beq @KillSquare1
.endif
	; Calculate volume
	lda var_ch_LengthCounter + APU_PU1	;;; ;; ;
	and #$01
	beq :+
	lda var_ch_VolColumn + APU_PU1		; do not automatically kill channel when hardware envelope is enabled
	asl a
	and #$F0
	ora var_ch_Volume + APU_PU1
	tax
	lda ft_volume_table, x				; ignore tremolo
	bpl @DoneVolumeSquare1				; always ; ;; ;;;
:	lda var_ch_VolColumn + APU_PU1		; Kill channel if volume column = 0
	asl a
	beq @KillSquare1
	and #$F0
	sta var_Temp
	lda var_ch_Volume + APU_PU1
	beq @KillSquare1
	ora var_Temp
	tax
	lda ft_volume_table, x
	sec
	sbc var_ch_TremoloResult
	bpl :+
	lda #$00
:   bne :+
	lda var_ch_VolColumn + APU_PU1
	beq :+
	lda #$01
:
@DoneVolumeSquare1:
	; Write to registers
	pha
	lda var_ch_DutyCycle + APU_PU1
	and #$03
	tax
	pla
	ora ft_duty_table, x				; Add volume
	sta var_Temp						;;; ;; ; allow length counter and envelope
	lda var_ch_LengthCounter + APU_PU1
	and #$03
	eor #$03
	asl a
	asl a
	asl a
	asl a
	ora var_Temp						; ;; ;;;
	sta $4000
	; Period table isn't limited to $7FF anymore
	lda var_ch_PeriodCalcHi + APU_PU1
	and #$F8
	beq @TimerOverflow1
	lda #$07
	sta var_ch_PeriodCalcHi + APU_PU1
	lda #$FF
	sta var_ch_PeriodCalcLo + APU_PU1
@TimerOverflow1:

	lda var_ch_Sweep + APU_PU1			; Check if sweep is active
	beq @NoSquare1Sweep
	and #$80
	beq @Square2						; See if sweep is triggered, if then don't touch sound registers until next note

	lda var_ch_Sweep + APU_PU1			; Trigger sweep
	sta $4001
	and #$7F
	sta var_ch_Sweep + APU_PU1

	;jsr @KillSweepUnit					; test

	lda var_ch_PeriodCalcLo + APU_PU1
	sta $4002
	lda var_ch_PeriodCalcHi + APU_PU1
	sta $4003
	lda #$FF
	sta var_ch_PrevFreqHigh + APU_PU1

	jmp @Square2

@KillSquare1:
	lda #$30
	sta $4000
	jmp @Square2

@NoSquare1Sweep:						; No Sweep
	lda #$08
	sta $4001
	;jsr @KillSweepUnit					; test
	lda var_ch_PeriodCalcLo + APU_PU1
	sta $4002
	lda var_ch_LengthCounter + APU_PU1	;;; ;; ;
	and #$03
	beq :+
	lda var_ch_Trigger + APU_PU1
	beq @SkipHighPartSq1
	bne :++
:	lda var_ch_PeriodCalcHi + APU_PU1
	cmp var_ch_PrevFreqHigh + APU_PU1
	beq @SkipHighPartSq1
	sta var_ch_PrevFreqHigh + APU_PU1
:	lda var_ch_LengthCounter + APU_PU1
	and #$F8
	ora var_ch_PeriodCalcHi + APU_PU1
	sta $4003							; ;; ;;;
@SkipHighPartSq1:
;	jmp @Square2

; ==============================================================================
;  Square 2
; ==============================================================================
@Square2:
	lda var_Channels
	and #$02
	bne :+
	jmp @Triangle
:	lda var_ch_Note + APU_PU2
	bne :+								; branch
	jmp @KillSquare2
:

	; Calculate volume
.if 0
	ldx #$01
	jsr ft_get_volume
	beq @KillSquare2
.endif

	lda var_ch_LengthCounter + APU_PU2	;;; ;; ;
	and #$01
	beq :+
	lda var_ch_VolColumn + APU_PU2		; do not automatically kill channel when hardware envelope is enabled
	asl a
	and #$F0
	ora var_ch_Volume + APU_PU2
	tax
	lda ft_volume_table, x				; ignore tremolo
	bpl @DoneVolumeSquare2				; always ; ;; ;;;
:	lda var_ch_VolColumn + APU_PU2		; Kill channel if volume column = 0
	asl a
	beq @KillSquare2
	and #$F0
	sta var_Temp
	lda var_ch_Volume + APU_PU2
	beq @KillSquare2
	ora var_Temp
	tax
	lda ft_volume_table, x
	sec
	sbc var_ch_TremoloResult + APU_PU2
	bpl :+
	lda #$00
:   bne :+
	lda var_ch_VolColumn + APU_PU2
	beq :+
	lda #$01
:
@DoneVolumeSquare2:
	; Write to registers
	pha
	lda var_ch_DutyCycle + APU_PU2
	and #$03
	tax
	pla
	ora ft_duty_table, x
	sta var_Temp		;;; ;; ;
	lda var_ch_LengthCounter + APU_PU2
	and #$03
	eor #$03
	asl a
	asl a
	asl a
	asl a
	ora var_Temp		; ;; ;;;
	sta $4004
	; Period table isn't limited to $7FF anymore
	lda var_ch_PeriodCalcHi + APU_PU2
	and #$F8
	beq @TimerOverflow2
	lda #$07
	sta var_ch_PeriodCalcHi + APU_PU2
	lda #$FF
	sta var_ch_PeriodCalcLo + APU_PU2
@TimerOverflow2:

	lda var_ch_Sweep + APU_PU2			; Check if there should be sweep
	beq @NoSquare2Sweep
	and #$80
	beq @Triangle						; See if sweep is triggered
	lda var_ch_Sweep + APU_PU2			; Trigger sweep
	sta $4005
	and #$7F
	sta var_ch_Sweep + APU_PU2

	jsr @KillSweepUnit

	lda var_ch_PeriodCalcLo + APU_PU2	; Could this be done by that below? I don't know
	sta $4006
	lda var_ch_PeriodCalcHi + APU_PU2
	sta $4007
	lda #$FF
	sta var_ch_PrevFreqHigh + APU_PU2

	jmp @Triangle

@KillSquare2:
	lda #$30
	sta $4004
	jmp @Triangle

@NoSquare2Sweep:				; No Sweep
	lda #$08
	sta $4005
	jsr @KillSweepUnit
	lda var_ch_PeriodCalcLo + APU_PU2
	sta $4006
	lda var_ch_LengthCounter + APU_PU2	;;; ;; ;
	and #$03
	beq :+
	lda var_ch_Trigger + APU_PU2
	beq @SkipHighPartSq2
	bne :++
:	lda var_ch_PeriodCalcHi + APU_PU2
	cmp var_ch_PrevFreqHigh + APU_PU2
	beq @SkipHighPartSq2
	sta var_ch_PrevFreqHigh + APU_PU2
:	lda var_ch_LengthCounter + APU_PU2
	and #$F8
	ora var_ch_PeriodCalcHi + APU_PU2
	sta $4007							; ;; ;;;
@SkipHighPartSq2:

; ==============================================================================
;  Triangle
; ==============================================================================
@Triangle:
	lda var_Channels
	and #$04
	beq @Noise

	lda var_ch_Volume + APU_TRI
	beq @KillTriangle
	lda var_ch_VolColumn + APU_TRI
	beq @KillTriangle
	lda var_ch_Note + APU_TRI
	beq @KillTriangle
	lda var_ch_LengthCounter + APU_TRI	;;; ;; ;
	and #$02
	beq :+								; branch if no envelope loop
	lda #$81
	bmi :++								; always
:	lda var_Linear_Counter				;;; ;; ;
:	sta $4008
	; Period table isn't limited to $7FF anymore
	lda var_ch_PeriodCalcHi + APU_TRI
	and #$F8
	beq @TimerOverflow3
	lda #$07
	sta var_ch_PeriodCalcHi + APU_TRI
	lda #$FF
	sta var_ch_PeriodCalcLo + APU_TRI
@TimerOverflow3:
;	lda #$08
;	sta $4009
	lda var_ch_PeriodCalcLo + APU_TRI
	sta $400A
	lda var_Linear_Counter				;;; ;; ;
	bmi :+
	lda var_ch_Trigger + APU_TRI
	beq @SkipTriangleKill
:	lda var_ch_LengthCounter + APU_TRI
	and #$F8
	ora var_ch_PeriodCalcHi + APU_TRI	; ;; ;;;
	sta $400B
	jmp @SkipTriangleKill
@KillTriangle:
	lda #$00
	sta $4008
@SkipTriangleKill:

; ==============================================================================
;  Noise
; ==============================================================================
@Noise:
	lda var_Channels
	and #$08
	bne :+						; branch
	jmp @DPCM
:

	lda var_ch_Note + APU_NOI
	bne :+						; branch
	jmp @KillNoise
:

	; Calculate volume
	lda var_ch_LengthCounter + APU_NOI	;;; ;; ;
	and #$01
	beq :+
	lda var_ch_VolColumn + APU_NOI		; do not automatically kill channel when hardware envelope is enabled
	asl a
	and #$F0
	ora var_ch_Volume + APU_NOI
	tax
	lda ft_volume_table, x				; ignore tremolo
	bpl @DoneVolumeNoise				; always ; ;; ;;;
:	lda var_ch_VolColumn + APU_NOI		; Kill channel if volume column = 0
	asl a
	beq @KillNoise
	and #$F0
	sta var_Temp
	lda var_ch_Volume + APU_NOI
	beq @KillNoise
	ora var_Temp
	tax
	lda ft_volume_table, x
	sec
	sbc var_ch_TremoloResult + APU_NOI
	bpl :+
	lda #$00
:   bne :+
	lda var_ch_VolColumn + APU_NOI
	beq :+
	lda #$01
:
@DoneVolumeNoise:
	; Write to registers
	sta var_Temp		;;; ;; ;
	lda var_ch_LengthCounter + APU_NOI
	and #$03
	eor #$03
	asl a
	asl a
	asl a
	asl a
	ora var_Temp		; ;; ;;;
	sta $400C
	lda #$00
	sta $400D
	lda var_ch_DutyCycle + APU_NOI
;	and #$01
	ror a
	ror a
	and #$80
	sta var_Temp
.if 0
.if .defined(SCALE_NOISE)
	; Divide noise period by 16
	lda var_ch_PeriodCalcLo + APU_NOI
	lsr a
	lsr a
	lsr a
	lsr a
.else
	; Limit noise period to range 0 - 15
	lda var_ch_PeriodCalcHi + APU_NOI
	bne :+
	lda var_ch_PeriodCalcLo + APU_NOI
	cmp #$10
	bcc :++
:   lda #$0F
:   eor #$0F
.endif
.else
; No limit
	lda var_ch_PeriodCalcLo + APU_NOI
	and #$0F
	eor #$0F
.endif
	ora var_Temp
	sta $400E
	lda var_ch_LengthCounter + APU_NOI	;;; ;; ;
	and #$03
	beq :+
	lda var_ch_Trigger + APU_NOI
	beq @DPCM
:	lda var_ch_LengthCounter + APU_NOI
	sta $400F							; ;; ;;;
	jmp @DPCM
@KillNoise:
	lda #$30
	sta $400C
@DPCM:

; ==============================================================================
;  DPCM
; ==============================================================================
.if .defined(USE_DPCM)
	lda var_Channels
	and #$10
	bne :+
	rts                             ; Skip DPCM
	;beq @Return
:
.if .defined(USE_ALL)		;;; ;; ;
	ldx #EFF_CHANS
.elseif .defined(USE_N163)
	ldx var_EffChannels
.else
	ldx #EFF_CHANS
.endif
	lda var_ch_DPCM_Retrig			; Retrigger
	beq @SkipRetrigger
	dec var_ch_DPCM_RetrigCntr
	bne @SkipRetrigger
	sta var_ch_DPCM_RetrigCntr
	lda #$01
	sta var_ch_Note, x
@SkipRetrigger:

	lda var_ch_DPCMDAC				; See if delta counter should be updated
	bmi @SkipDAC
	sta $4011
@SkipDAC:
	lda #$80						; store a negative value to mark that it's already updated
	sta var_ch_DPCMDAC

	lda var_ch_Note, x
	beq @KillDPCM
	bmi @SkipDPCM
	lda var_ch_SamplePitch
	and #$40
	sta var_Temp
	lda var_ch_DPCM_EffPitch
	bpl :+
	lda var_ch_SamplePitch
:   ora var_Temp
	sta $4010
	lda #$80
	sta var_ch_DPCM_EffPitch


	; Setup sample bank (if used)
 .if .defined(USE_BANKSWITCH)
	lda var_ch_SampleBank
	beq :+
	clc
	sta $5FFC		; Always last bank
	adc #$01
	sta $5FFD
	adc #$01
	sta $5FFE
;	adc #$01
;	sta $5FFF
:
.endif

	; Sample position (add sample offset)
	clc
	lda var_ch_SamplePtr
	adc var_ch_DPCM_Offset
	sta $4012

	; Sample length (remove sample offset)
	lda var_ch_DPCM_Offset
	asl a
	asl a
	sta var_Temp
	sec
	lda var_ch_SampleLen
	sbc var_Temp
	sta $4013
	lda #$80
	sta var_ch_Note, x
	lda #$0F
	sta $4015
	lda #$1F
	sta $4015
	rts
@SkipDPCM:
	cmp #$FF
	beq @ReleaseDPCM
	rts
@ReleaseDPCM:
; todo
	lda #$0F
	sta $4015
	lda #$80
	sta var_ch_Note, x
	rts
@KillDPCM:
	lda #$0F
	sta $4015
	lda #$80
	sta $4011
	sta var_ch_Note, x
.endif
@Return:
	rts

; Lookup tables

ft_duty_table:
.repeat 4, i
	.byte $40 * i
.endrep

; Volume table: (column volume) * (instrument volume)
ft_volume_table:
.repeat 16, xx
	.repeat 16, yy
		.if xx = 0 || yy = 0
			.byte 0
		.elseif xx * yy < 15
			.byte 1
		.else
			.byte xx * yy / 15
		.endif
	.endrep
.endrep