;
; Nintendo MMC5 expansion sound
;

ft_update_mmc5:
	lda var_PlayerFlags
	bne :+
	lda #$00
	sta $5015
	rts
:	ldx #$00
	ldy #$00
@ChannelLoop:		; MMC5 pulse channels
	lda var_ch_Note + MMC5_OFFSET, x		; Kill channel if note = off
	bne :+									; branch
	jmp @KillChannel
	; Calculate volume
:	lda var_ch_LengthCounter + MMC5_OFFSET, x	;;; ;; ;
	and #$01
	beq :+
	lda var_ch_VolColumn + MMC5_OFFSET, x	; do not automatically kill channel when hardware envelope is enabled
	asl a
	and #$F0
	ora var_ch_Volume + MMC5_OFFSET, x
	tay
	lda ft_volume_table, y					; ignore tremolo
	pha										; compatibility
	lda var_ch_DutyCycle + MMC5_OFFSET, x
	and #$03
	sta var_Temp2		;;; ;; ;
	pla
	bpl @DoneVolume							; always ; ;; ;;;
:	lda var_ch_VolColumn + MMC5_OFFSET, x	; Kill channel if volume column = 0
	asl a
	and #$F0
	bne :+ ;;; ;; ; branch
	jmp @KillChannel
:	sta var_Temp
	lda var_ch_Volume + MMC5_OFFSET, x		; Kill channel if volume = 0
	beq @KillChannel
	ora var_Temp
	tay
	; Write to registers
	lda var_ch_DutyCycle + MMC5_OFFSET, x
	and #$03
	sta var_Temp2		;;; ;; ;
	lda ft_volume_table, y
	sec
	sbc var_ch_TremoloResult + MMC5_OFFSET, x
	bpl :+
	lda #$00
:   bne :+
	lda var_ch_VolColumn + MMC5_OFFSET, x
	beq :+
	lda #$01
:
@DoneVolume:
	ldy var_Temp2
	ora ft_duty_table, y					; Add volume
	sta var_Temp							;;; ;; ; allow length counter and envelope
	txa
	asl a
	asl a
	tay
	lda var_ch_LengthCounter + MMC5_OFFSET, x
	and #$03
	eor #$03
	asl a
	asl a
	asl a
	asl a
	ora var_Temp							; ;; ;;;
	sta $5000, y
	iny
	iny
	; Period table isn't limited to $7FF anymore
	lda var_ch_PeriodCalcHi + MMC5_OFFSET, x
	and #$F8
	beq :+
	lda #$03
	sta var_ch_PeriodCalcHi + MMC5_OFFSET, x
	lda #$FF
	sta var_ch_PeriodCalcLo + MMC5_OFFSET, x
:	lda var_ch_PeriodCalcLo + MMC5_OFFSET, x
	sta $5000, y
	iny
	lda var_ch_LengthCounter + MMC5_OFFSET, x	;;; ;; ;
	and #$03
	beq :+
	lda var_ch_Trigger + MMC5_OFFSET, x
	bne :+++
	iny
	bne @Next ; always
:	lda var_ch_PeriodCalcHi + MMC5_OFFSET, x
	cmp var_ch_PrevFreqHighMMC5, x
	bne :+
	iny
	bne @Next ; always
:	sta var_ch_PrevFreqHighMMC5, x
:	lda var_ch_LengthCounter + MMC5_OFFSET, x
	and #$F8
	ora var_ch_PeriodCalcHi + MMC5_OFFSET, x
	sta $5000, y
	iny										; ;; ;;;
	jmp @Next
@KillChannel:
	lda #$30
	sta $5000, y
	tya
	adc #$04
	tay
@Next:
	inx
	cpx #CH_COUNT_MMC5
	bcs :+
	jmp @ChannelLoop
:	rts
