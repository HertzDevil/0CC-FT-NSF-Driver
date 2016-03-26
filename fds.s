; Takes care of the FDS registers

ft_load_inst_extra_fds:
	tya							;;; ;; ;
	clc
	adc #$10
	sta var_Temp2				; ;; ;;;
	; Load modulation table
	jsr ft_reset_modtable
:
	lda (var_Temp_Pointer), y
	pha
	and #$07
	sta $4088
	pla
	lsr a
	lsr a
	lsr a
	sta $4088
	iny
	cpy var_Temp2				;;; ;; ;
	bcc :-

	lda (var_Temp_Pointer), y	; Modulation delay
	iny
	sta var_ch_ModDelay
	lda (var_Temp_Pointer), y	; Modulation depth
	iny
	sta var_ch_ModInstDepth		;;; ;; ; keep state
	lda (var_Temp_Pointer), y	; Modulation freq low
	iny
	sta var_ch_ModInstRate
	lda (var_Temp_Pointer), y	; Modulation freq high
	sta var_ch_ModInstRate + 1
	iny							;;; ;; ;

	lda (var_Temp_Pointer), y	; Load wave index
	jsr ft_load_fds_wave
	ldy var_Temp
	rts							;;; ;; ;

ft_fds_volume:
	lda var_Temp				; 5x4 multiplication
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
	ora var_ch_Volume + FDS_OFFSET
	beq :+
	lda #$01					; Round up to 1
:	rts

ft_init_fds:
	lda #$00
	sta $4023
	lda #$83
	sta $4023
	lda #$FF					;;; ;; ;
	sta $408A
	lda #$80
	sta var_ch_FDSVolume
	sta var_ch_ModBias			; ;; ;;;
	rts

; Update FDS
ft_update_fds:
	lda var_PlayerFlags
	bne @Play
	lda #$80
	sta $4080
	rts
@Play:

	lda var_ch_Note + FDS_OFFSET
	bne :+ ; branch
	jmp @KillFDS
:
	; Calculate volume
	lda var_ch_VolColumn + FDS_OFFSET		; Kill channel if volume column = 0
	lsr a
	lsr a
	lsr a
	beq @KillFDS
	sta var_Temp2							; 4 bit vol
	lda var_ch_Volume + FDS_OFFSET			; Kill channel if volume = 0
	beq @KillFDS
	sta var_Temp							; 5 bit vol
	jsr ft_fds_volume
	sec
	sbc var_ch_TremoloResult + FDS_OFFSET
	bpl :+
@NoKill:
	lda #$00
:
	; Load volume
	ora #$80
	sta var_Temp							;;; ;; ;
	lda var_ch_FDSVolume					; check the volume envelope
	bmi :+									; envelope is disabled
	lda var_ch_Trigger + FDS_OFFSET
	beq :++									; envelope is enabled in middle of note
:	lda var_Temp
	sta $4080								; Volume
:	lda var_ch_FDSVolume
	bmi :+
	sta $4080								; enable envelope after volume init
:											; ;; ;;;

	; Load frequency
	lda var_ch_PeriodCalcHi + FDS_OFFSET
	and #$F0
	beq :+
	lda #$FF
	sta var_ch_PeriodCalcLo + FDS_OFFSET
	lda #$0F
	sta var_ch_PeriodCalcHi + FDS_OFFSET
:	lda var_ch_PeriodCalcHi + FDS_OFFSET
	sta $4083	; High
	lda var_ch_PeriodCalcLo + FDS_OFFSET
	sta $4082	; Low

	lda var_ch_Trigger + FDS_OFFSET			;;; ;; ;
	beq :+
	jsr ft_reset_modtable
	;lda #$00
	;sta $4085
	lda var_ch_ModInstDepth					;;; ;; ;
	sta var_ch_ModDepth
	lda var_ch_ModRate + 1
	bmi :+
	lda var_ch_ModInstRate
	sta var_ch_ModRate
	lda var_ch_ModInstRate + 1
	sta var_ch_ModRate + 1					; ;; ;;;
:	jsr ft_check_fds_effects

	lda var_ch_ModDelayTick					; Modulation delay
	bne @TickDownDelay
;	lda var_ch_ModDepth						; Skip if modulation is disabled
;	beq @DisableMod

	lda var_ch_ModDepth						; Skip if modulation is disabled
	ora #$80
	sta $4084								; Store modulation depth

	jsr ft_check_fds_fm

@Return:
	rts
@TickDownDelay:
	dec var_ch_ModDelayTick
@DisableMod:
	; Disable modulation
	lda #$80
	sta $4087
	rts
@KillFDS:
	lda var_ch_FDSVolume					;;; ;; ; return if volume envelope is enabled
	bpl @NoKill								; ;; ;;;
	lda #$80
	sta $4080	; Make channel silent
	lda #$80
	sta $4084
	sta $4087
	rts

; Load the waveform, index in A
ft_load_fds_wave:
	sta var_Temp16
	lda #$00
	sta var_Temp16 + 1
	; Multiply by 64
	clc
	ldy #$06
:	rol var_Temp16
	rol var_Temp16 + 1
	dey
	bne :-
	; Setup a pointer to the specified wave
	clc
	lda var_Wavetables
	adc var_Temp16
	sta var_Temp16
	lda var_Wavetables + 1
	adc var_Temp16 + 1
	sta var_Temp16 + 1
	; Write wave
	lda #$80
	sta $4089		; Enable wave RAM
	ldy #$3F		; optimization
:	lda (var_Temp16), y	          	; 5
	sta $4040, y					; 5
	dey								; 2
	bpl :-							; 3 = 15 cycles and 64 iterations = 960 cycles
	lda #$00
	sta $4089		; Disable wave RAM
	rts

ft_reset_modtable:
	lda #$80
	sta $4087
	lda #$00
	sta $4085
	rts

ft_check_fds_effects:
	lda var_ch_ModEffWritten
	and #$01
	beq :+
	; FDS modulation depth
	lda var_ch_ModEffDepth
	sta var_ch_ModDepth
:   lda var_ch_ModEffWritten
	and #$02
	beq :+
	; FDS modulation rate high
	lda var_ch_ModEffRate + 1
	sta var_ch_ModRate + 1
:   lda var_ch_ModEffWritten
	and #$04
	beq :+
	; FDS modulation rate low
	lda var_ch_ModEffRate + 0
	sta var_ch_ModRate + 0
:
	lda #$00
	sta var_ch_ModEffWritten

	rts

ft_check_fds_fm:
	lda var_ch_ModRate + 1					;;; ;; ;
	bmi @AutoFM
	lda var_ch_ModRate						; Modulation freq
	sta $4086
	lda var_ch_ModRate + 1
	sta $4087
	rts
@AutoFM:
	lda var_ch_ModRate + 1
	and #$7F
	sta var_Temp
	lda var_ch_ModRate + 0
	sta AUX
	lda var_ch_PeriodCalcHi + FDS_OFFSET
	sta var_Temp16 + 1
	lda var_ch_PeriodCalcLo + FDS_OFFSET
	sta var_Temp16
	lda #$00
	sta ACC
	sta ACC + 1
	sta AUX + 1

	; var_Temp16 * var_Temp -> ACC
	ldy #$08
@MultStep:
	lda var_Temp
	lsr a
	sta var_Temp
	bcc :+
	clc
	lda ACC
	adc var_Temp16
	sta ACC
	lda ACC + 1
	adc var_Temp16 + 1
	bcs @Overflow
	sta ACC + 1
:   asl var_Temp16
	rol var_Temp16 + 1
	dey
	bne @MultStep
	beq @DoneMult ; always
@Overflow:
	lda #$FF
	sta ACC
	sta ACC + 1
@DoneMult:
	jsr DIV
	lda var_ch_ModBias
	eor #$80
	bpl :+
	dec ACC + 1
:	clc
	adc ACC
	sta $4086
	lda ACC + 1
	adc #$00
	sta $4087
	rts 