;
; Sunsoft 5B expansion sound
;

ft_init_s5b:
	lda #$40
	sta var_ch_DutyDefault + S5B_OFFSET
	sta var_ch_DutyDefault + S5B_OFFSET + 1
	sta var_ch_DutyDefault + S5B_OFFSET + 2
	lda #$07
	sta $C000
	lda #%00111000
	sta var_Pul_Noi
	sta $E000		; see n163.s
	rts

ft_update_s5b:
	lda var_PlayerFlags
	bne :+
	lda #$08
	sta $C000
	lda #$00
	sta $E000
	inc $C000
	lda #$00
	sta $E000
	inc $C000
	lda #$00
	sta $E000
	rts
:
	ldx #$00
@UpdateDuty:
	lda var_ch_DutyCurrent + S5B_OFFSET, x
	bpl :+									; no noise
	and #$1F
	sta var_Noise_Period
:	lda var_ch_DutyCurrent + S5B_OFFSET, x
	and #$20								; E
	lsr
	sta var_ch_5B_Env_Enable, x
	inx
	cpx #CH_COUNT_S5B
	bcc @UpdateDuty

	ldx #$00
@UpdateToneMask:
	lda var_ch_DutyCurrent + S5B_OFFSET, x
	and #$40
	beq :+
	lda var_Pul_Noi
	eor #$FF
	ora bit_mask, x
	eor #$FF
	sta var_Pul_Noi
	bpl :++ ; always
:	lda var_Pul_Noi
	ora bit_mask, x
	sta var_Pul_Noi
:	inx
	cpx #CH_COUNT_S5B
	bcc @UpdateToneMask

	ldx #$00
@UpdateNoiseMask:
	lda var_ch_DutyCurrent + S5B_OFFSET, x
	bpl :+
	lda var_Pul_Noi
	eor #$FF
	ora bit_mask + CH_COUNT_S5B, x
	eor #$FF
	sta var_Pul_Noi
	bpl :++ ; always
:	lda var_Pul_Noi
	ora bit_mask + CH_COUNT_S5B, x
	sta var_Pul_Noi
:	inx
	cpx #CH_COUNT_S5B
	bcc @UpdateNoiseMask

	ldx #$00
	ldy #$00
@ChannelLoop:
	lda var_ch_Note + S5B_OFFSET, x				; Kill channel if note = off
	bne :+
	txa
	clc
	adc #$08
	sta $C000
	lda #$00
	sta $E000
	iny
	iny
	bpl @S5B_next ; always
	; Load volume
:	txa
	clc
	adc #$08
	sta $C000
	lda var_ch_VolColumn + S5B_OFFSET, x
	lsr a
	lsr a
	lsr a
	clc
	adc var_ch_Volume + S5B_OFFSET, x
	sec
	sbc #$0F
	sec
	sbc var_ch_TremoloResult + S5B_OFFSET, x
	bpl :+
	lda #$00
:	bne :+
	lda var_ch_VolColumn + N163_OFFSET, x
	beq :+
	lda #$01
:
	; Volume / envelope enable
	ora var_ch_5B_Env_Enable, x
	sta $E000
	; Frequency
	inc var_ch_PeriodCalcLo + S5B_OFFSET, x		; correction
	bne :+
	inc var_ch_PeriodCalcHi + S5B_OFFSET, x
:	sty $C000
	iny
	lda var_ch_PeriodCalcLo + S5B_OFFSET, x
	sta $E000
	sty $C000
	iny
	lda var_ch_PeriodCalcHi + S5B_OFFSET, x
	sta $E000
@S5B_next:
	inx
	cpx #CH_COUNT_S5B
	bcc @ChannelLoop

	; Global variables
	lda #$06
	sta $C000
	lda var_Noise_Period
	sta $E000
	lda #$07
	sta $C000
	lda var_Pul_Noi
	sta $E000
	rts