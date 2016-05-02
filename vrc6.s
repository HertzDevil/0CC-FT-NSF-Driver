;
; Konami VRC6 expansion sound
;

ft_update_vrc6:
	lda var_PlayerFlags
	bne :+
	lda #$00
	sta $9002
	sta $A002
	sta $B002
	rts
:
.if .defined(USE_LINEARPITCH)		;;; ;; ;
	lda var_SongFlags
	and #FLAG_LINEARPITCH
	beq :+
	jsr ft_load_ntsc_table
	ldx #VRC6_OFFSET
	jsr ft_linear_fetch_pitch
	jsr ft_linear_fetch_pitch
	jsr ft_load_saw_table
	jsr ft_linear_fetch_pitch
:
.endif								; ;; ;;;
	ldx #$00
	txa
	sta var_Temp_Pointer
@ChannelLoop:
	lda ft_duty_table_vrc6 + 1, x			;;; ;; ; offset
	ora #$80
	sta var_Temp_Pointer + 1				; ;; ;;;
	lda var_ch_Note + VRC6_OFFSET, x		; Kill channel if note = off
	beq @KillChan
	; Load volume
	lda var_ch_VolColumn + VRC6_OFFSET, x	; Kill channel if volume column = 0
	asl a
	bne :+
	cpx #$02
	bne @KillChan							; sawtooth does not kill
:	and #$F0
	sta var_Temp
	lda var_ch_Volume + VRC6_OFFSET, x		; Kill channel if volume = 0
	bne :+
	cpx #$02
	bne @KillChan
:	ora var_Temp
	tay
	lda ft_volume_table, y					; Load from the 16*16 volume table
	sec
	sbc var_ch_TremoloResult + VRC6_OFFSET, x
	bpl :+
	lda #$00
:	bne :+
	lda var_ch_VolColumn + VRC6_OFFSET, x
	beq :+
	lda #$01
:

	; Pulse width
	pha
	lda var_ch_DutyCurrent + VRC6_OFFSET, x
	and #$07
	tay
	pla

	ora ft_duty_table_vrc6, y
	cpx #$02
	bne :+
	asl a
	and #$3F
	; Write to registers
:	ldy #$00
	sta (var_Temp_Pointer), y
	iny
	lda	var_ch_PeriodCalcLo + VRC6_OFFSET, x
	sta (var_Temp_Pointer), y
	iny
	lda	var_ch_PeriodCalcHi + VRC6_OFFSET, x
	ora #$80
	sta (var_Temp_Pointer), y
	bmi @NextChan         ; Branch always
@KillChan:
	ldy #$02
	lda #$00
	sta (var_Temp_Pointer), y
@NextChan:
	inx
	cpx #CH_COUNT_VRC6
	bcc @ChannelLoop
	rts

ft_duty_table_vrc6:
.repeat 8, i
	.byte $10 * i
.endrep