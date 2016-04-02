;
; ft_music_play
;
; The player routine
;
ft_music_play:
	lda var_PlayerFlags					; Skip if player is disabled
	bne :+
	rts									; Not playing, return
:

.if .defined(USE_FDS)
	lda #$00
	sta var_ch_ModEffWritten
.endif

	; Run delayed channels
	ldx #$00
@ChanLoop:
	lda var_ch_Delay, x
	beq @SkipDelay
	sec
	sbc #$01
	sta var_ch_Delay, x
	bne @SkipDelay
	jsr ft_read_pattern					; Read the delayed note
	jmp :+ ; ;; ;;;
@SkipDelay:
	lda #$00							;;; ;; ; Clear note trigger flag
	sta var_ch_Trigger, x				; ;; ;;;
:	inx
.if .defined(USE_ALL)		;;; ;; ;
	cpx #CHANNELS
.elseif .defined(USE_N163)
	cpx var_AllChannels
.else
	cpx #CHANNELS
.endif
	bne @ChanLoop

	; Speed division
	lda var_Tempo_Accum + 1
	bmi ft_do_row_update				; Counter < 0
	ora var_Tempo_Accum
	beq ft_do_row_update				; Counter = 0
	jmp ft_skip_row_update
	; Read a row
ft_do_row_update:

.if .defined(USE_DPCM)
	lda #$00
	sta var_ch_DPCM_Retrig
.endif

	; Switches to new frames are delayed to next row to resolve issues with delayed notes.
	; It won't work if new pattern adresses are loaded before the delayed note is played
	lda var_Load_Frame
	beq @SkipFrameLoad
	;;; ;; ; from 0.4.6
	ldx #$00
:	lda var_ch_Delay, x
	beq :+
	lda #$00
	sta var_ch_Delay, x
	jsr ft_read_pattern
:	inx
.if .defined(USE_ALL)
	cpx #CHANNELS
.elseif .defined(USE_N163)
	cpx var_AllChannels
.else
	cpx #CHANNELS
.endif
	bne :--
	
	lda #$00
	sta var_Load_Frame
	lda var_Current_Frame
	jsr ft_load_frame
@SkipFrameLoad:

	; Read one row from all patterns
	ldx #$00
ft_read_channels:
@UpdateChan:
	lda var_ch_Delay, x
	beq :+
	lda #$00
	sta var_ch_Delay, x
	jsr ft_read_pattern                 ; In case a delayed note has not been played, skip it to get next note
:	jsr ft_read_pattern					; Get new notes
	inx ; ;; ;;;

.if .defined(USE_ALL)		;;; ;; ;
	cpx #CHANNELS
.elseif .defined(USE_N163)
	cpx var_AllChannels
.else
	cpx #CHANNELS
.endif

	bne ft_read_channels

	; Should jump?
	lda var_Jump
	beq @NoJump
	; Yes, jump
	sec
	sbc #$01
	sta var_Current_Frame
;	jsr ft_load_frame
	lda #$01
	sta var_Load_Frame

	jmp @NoPatternEnd
@NoJump:
	; Should skip?
	lda var_Skip
	beq @NoSkip
	; Yes, skip
.if .defined(ENABLE_ROW_SKIP)
	; Store next row number in Temp2
	sec
	sbc #$01
	sta var_SkipTo
.endif
	jmp @NextFrame
@NoSkip:
	; Current row in all channels are processed, update info
	inc var_Pattern_Pos
	lda var_Pattern_Pos					; See if end is reached
	cmp var_Pattern_Length
	bne @NoPatternEnd
@NextFrame:								;;; ;; ; shared
	; End of current frame, load next
	lda #$01
	sta var_Load_Frame
	inc var_Current_Frame
	lda var_Current_Frame
	cmp var_Frame_Count
	bne @NoPatternEnd
	lda #$00
	sta var_Current_Frame

@NoPatternEnd:
	jsr ft_restore_speed				; Reset frame divider counter
ft_skip_row_update:
	; Speed division
	sec
	lda var_Tempo_Accum					; Decrement speed counter
	sbc var_Tempo_Count
	sta var_Tempo_Accum
	lda var_Tempo_Accum + 1
	sbc var_Tempo_Count + 1
	sta var_Tempo_Accum + 1

	; Note cut effect (Sxx)
	ldx #$00
@BeginCut:
	lda var_ch_NoteCut, x
	beq @BeginRelease
	bpl :+					;;; ;; ;
	and #$7F
	sta var_ch_NoteCut, x
	bpl :++ ; always
:	sta var_ch_NoteCut, x
	dec var_ch_NoteCut, x
:	beq :+
	bpl @BeginRelease
:	lda #$00				; ;; ;;;
	sta var_ch_NoteCut, x
	sta var_ch_Note, x   ; todo: make a subroutine for note cut
.if .defined(USE_DPCM)
	lda ft_channel_type, x
	cmp #CHAN_DPCM
	beq @BeginRelease	; 0CC: check
	lda #$00
.endif
	sta var_ch_PortaToLo, x
	sta var_ch_PortaToHi, x
	sta var_ch_TimerPeriodLo, x
	sta var_ch_TimerPeriodHi, x
.if .defined(USE_VRC7)
	lda ft_channel_type, x
	cmp #CHAN_VRC7
	bne @BeginRelease
	lda #$00							; Halt VRC7 channel
	sta var_ch_vrc7_Command - VRC7_OFFSET, x
.endif
@BeginRelease:
	lda var_ch_NoteRelease, x			;;; ;; ; Delayed note release
	beq @BeginTranspose
	bpl :+					;;; ;; ;
	and #$7F
	sta var_ch_NoteRelease, x
	bpl :++ ; always
:	sta var_ch_NoteRelease, x
	dec var_ch_NoteRelease, x
:	beq :+
	bpl @BeginTranspose
:	lda #$00				; ;; ;;;
	sta var_ch_NoteRelease, x
	lda var_ch_State, x
	cmp #$01
	beq @BeginTranspose
	lda #$01
	sta var_ch_State, x
.if .defined(USE_DPCM)
	lda ft_channel_type, x
	cmp #CHAN_DPCM
	bne :+
	lda #$FF
	sta var_ch_Note, x
	bmi @BeginTranspose			; always
:
.endif
.if .defined(USE_VRC7)
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_VRC7
	beq @BeginTranspose
.endif
	jsr ft_instrument_release
@BeginTranspose:
	lda var_ch_Transpose, x				;;; ;; ;
	beq @DoneTranspose
	bmi @Negative
	sec
	sbc #$10
	sta var_ch_Transpose, x
	bpl @DoneTranspose					; else var_ch_Transpose, x == #$Fx
	and #$0F
	clc
	jsr ft_clear_transpose
;	jmp @DoneTranspose
	beq @DoneTranspose					; always
@Negative:
	sec
	sbc #$10
	sta var_ch_Transpose, x
	bmi @DoneTranspose					; else var_ch_Transpose, x == #$7x
	eor #$8F
	sec
	jsr ft_clear_transpose
@DoneTranspose:
	lda var_ch_VolDelay, x
	beq :+
	cmp #$10
	bcs :+
	asl a
	asl a
	asl a
	asl a
	sta var_ch_VolDelay, x
:	; ;; ;;;
	inx

.if .defined(USE_ALL)		;;; ;; ;
	cpx #CHANNELS
.elseif .defined(USE_N163)
	cpx var_AllChannels
.else
	cpx #CHANNELS
.endif
	beq :+
	jmp @BeginCut						; branch

	; Update channel instruments and effects
:	lda #$01							;;; ;; ; Prevent noise channel from halting
	sta var_ch_TimerPeriodHi + APU_NOI	; ;; ;;;
	ldx #$00

; Loop through wave channels
ft_loop_channels:

	; Do channel effects, like portamento and vibrato
	jsr ft_run_effects

	; Instrument sequences
	lda var_ch_Note, x
	beq :+
	jsr ft_run_instrument				; Update instruments
:	jsr	ft_calc_period

	;;; ;; ; Decrement volume delay counter after everything else is done
	; preferred behaviour for delayed effects?
	lda var_ch_VolDelay, x
	and #$0F
	bne :+
	lda var_ch_VolDelay, x
	and #$F0
	beq :+
	lsr a
	sta var_ch_VolColumn, x
	lda #$00
	sta var_ch_VolDelay, x
:
	lda var_ch_VolDelay, x
	cmp #$10
	bcc :+
	sbc #$10
	sta var_ch_VolDelay, x
:
	; ;; ;;;

	inx
	;cpx #WAVE_CHANS		; Skip DPCM
.if .defined(USE_ALL)		;;; ;; ;
	cpx #EFF_CHANS
.elseif .defined(USE_N163)
	cpx var_EffChannels
.else
	cpx #EFF_CHANS
.endif
	bne ft_loop_channels

	; Finally update APU and expansion chip registers
	jsr ft_update_apu
ft_update_ext:
.if .defined(USE_VRC6)
	jsr	ft_update_vrc6
.endif
.if .defined(USE_VRC7)
	jsr ft_update_vrc7
.endif
.if .defined(USE_MMC5)
	jsr	ft_update_mmc5
.endif
.if .defined(USE_FDS)
	jsr ft_update_fds
.endif
.if .defined(USE_N163)
	jsr ft_update_n163
.endif
.if .defined(USE_S5B)
	jsr ft_update_s5b
.endif

	; End of music routine, return
	rts


; Process a pattern row in channel X
ft_read_pattern:
	ldy var_ch_NoteDelay, x				; First check if in the middle of a row delay
	beq :+
	dey
	tya
	sta var_ch_NoteDelay, x
	rts									; And skip
:	sty var_Sweep						; Y = 0
.if .defined(USE_BANKSWITCH)
	; First setup the bank
	lda var_ch_Bank, x
	beq :+
	jsr ft_bankswitch
:	; Go on
.endif
	lda var_ch_PatternAddrLo, x			; Load pattern address
	sta var_Temp_Pattern
	lda var_ch_PatternAddrHi, x
	sta var_Temp_Pattern + 1
.if .defined(USE_VRC7)
	lda #$FF
	sta var_ch_vrc7_EffPatch
.endif

ft_read_note:
	lda (var_Temp_Pattern), y			; Read pattern command
	bpl :+
	jmp @Effect
:	bne :+								; Rest
	jmp @JumpToDone						; branch
:	cmp #$7F
;	beq @NoteOff						; Note off
	bne :+
	jsr ft_push_echo_buffer
	jmp @NoteOff
:	cmp #$7E
;	beq @NoteRelease					; Note release
	bne :+
	jmp @NoteRelease
:
	;;; ;; ; Echo buffer access
	cmp #$70
	bcc @NoEcho
	and #$0F							; sec; sbc #$70
	bne :+								; first echo
	txa									; for the tax below
	bpl :+++							; always, unless there are more than 0x80 channels
:	sta var_Temp ; = echo index
	txa
	clc
:	adc #CHANNELS
	dec var_Temp
	bne :-
:	stx var_Temp ; = channel index
	tax
	lda var_ch_EchoBuffer, x			; actually load the note
	bne :+
	ldx var_Temp
	jsr ft_push_echo_buffer				;;; ;; ;
	jmp @JumpToDone
:	ldx var_Temp
@NoEcho:
	; ;; ;;;

	; Read a note
	sta var_ch_Note, x					; Note on
	jsr ft_push_echo_buffer				;;; ;; ;
	jsr ft_translate_freq

	lda #$01							;;; ;; ; Set note trigger flag
	sta var_ch_Trigger, x				; ;; ;;;
	lda var_ch_NoteCut, x
	bmi :+
	lda #$00
	sta var_ch_NoteCut, x
:
	lda var_ch_NoteRelease, x
	bmi :+
	lda #$00
	sta var_ch_NoteRelease, x
:
.if 0
	lda var_ch_Transpose, x
	and #$F0
	beq :+
	;lda #$00
	;sta var_ch_Transpose, x
:
.endif

	lda var_ch_VolSlide, x
	bne :+
	lda var_ch_VolDefault, x
	sta var_ch_VolColumn, x
:

.if .defined(USE_DPCM)
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_DPCM
	bne :+
	lda #$00
	sta var_ch_State, x			; ;; ;;;
	jmp @ReadIsDone
:	; DPCM skip
.endif
.if .defined(USE_VRC7)
	lda ft_channel_type, x
	cmp #CHAN_VRC7
	bne :+
	jsr ft_vrc7_trigger
	jmp @ResetSlide
:	; VRC7 skip
.endif
.if 0
.if .defined(USE_N163)					;;; ;; ;
	lda ft_channel_type, x
	cmp #CHAN_N163
	bne :+
	;jsr ft_n163_load_wave2
:
.endif									; ;; ;;;
.endif
	jsr ft_reset_instrument
	lda #$00
	sta var_ch_State, x
.if .defined(USE_FDS)		;;; ;; ; removed var_VolTemp
	lda ft_channel_type, x
	cmp #CHAN_FDS
	bne :+
	lda #$1F							; FDS max vol is 31
	bpl :++ ; always
:	lda #$0F							; Default max vol is 15
:
.else
	lda #$0F							; Default max vol is 15
.endif
	sta var_ch_Volume, x
	lda #$00
	sta var_ch_ArpeggioCycle, x
.ifdef USE_S5B		;;; ;; ;
	lda ft_channel_type, x
	cpx #CHAN_S5B
	beq @ResetSlide
.endif				; ;; ;;;
	lda var_ch_DutyDefault, x
	sta var_ch_DutyCurrent, x

@ResetSlide:
	; Clear the slide effect on new notes
	lda var_ch_Effect, x
	cmp #EFF_SLIDE_UP
	beq :+
	cmp #EFF_SLIDE_DOWN
	bne :++
:	lda #EFF_NONE
	sta var_ch_Effect, x
:

	cpx #$02							; Skip if not square
	bcs @JumpToDone
	lda #$00
	sta var_ch_Sweep, x					; Reset sweep
@JumpToDone:
	jmp @ReadIsDone
@NoteRelease:
.if .defined(USE_DPCM)
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_DPCM				; ;; ;;;
	bne :+
	lda #$FF
	sta var_ch_Note, x
	jmp @ReadIsDone
:
.endif
	lda var_ch_State, x
	cmp #$01
	beq @JumpToDone
	lda #$01
	sta var_ch_State, x
.if .defined(USE_VRC7)
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_VRC7				; ;; ;;;
	beq @JumpToDone
.endif
	jsr ft_instrument_release
	jmp @ReadIsDone
@NoteOff:
	lda #$00
	sta var_ch_Note, x
.if .defined(USE_DPCM)
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_DPCM				; ;; ;;;
	bne :+
	jmp @ReadIsDone
:   lda #$00
.endif
.if .defined(USE_VRC7)
	lda ft_channel_type, x
	cmp #CHAN_VRC7
	bne :+
	lda #$00							; Halt VRC7 channel
	sta var_ch_vrc7_Command - VRC7_OFFSET, x
	sta var_ch_PortaToLo, x
	sta var_ch_PortaToHi, x
	sta var_ch_TimerPeriodLo, x
	sta var_ch_TimerPeriodHi, x
	jmp @ReadIsDone
:   lda #$00
.endif
	sta var_ch_Volume, x
	sta var_ch_PortaToLo, x
	sta var_ch_PortaToHi, x
	sta var_ch_TimerPeriodLo, x
	sta var_ch_TimerPeriodHi, x
	cpx #$02							; Skip all but the square channels
	bcs :+
	lda #$FF
	sta var_ch_PrevFreqHigh, x
:	jmp @ReadIsDone
@VolumeCommand:							; Handle volume
	asl a
	asl a
	asl a
	;asl a
	and #$78
	sta var_ch_VolColumn, x
	sta var_ch_VolDefault, x			;;; ;; ;
	iny
	jmp ft_read_note
@InstCommand:							; Instrument change
	and #$0F
	asl a
	jsr ft_load_instrument
	iny
	jmp ft_read_note
@Effect:
	cmp #$F0							; See if volume
	bcs @VolumeCommand
	cmp #$E0							; See if a quick instrument command
	bcs @InstCommand
	and #$7F							; Look up the command address
	sty var_Temp						; from the command table
	tay
	lda ft_command_table, y
	sta var_Temp_Pointer
	iny
	lda ft_command_table, y
	sta var_Temp_Pointer + 1
	ldy var_Temp
	iny
	jmp (var_Temp_Pointer)				; And jump there
@ReadIsDone:
	lda var_ch_DefaultDelay, x			; See if there's a default delay
	cmp #$FF
	beq :+								; If so then use it
	sta var_ch_NoteDelay, x
	bne ft_read_is_done ; always
:	iny
	lda (var_Temp_Pattern), y			; A note is immediately followed by the amount of rows until next note
	sta var_ch_NoteDelay, x
ft_read_is_done:
	clc									; Store pattern address
	iny
	tya
	adc var_Temp_Pattern
	sta var_ch_PatternAddrLo, x
	lda #$00
	adc var_Temp_Pattern + 1
	sta var_ch_PatternAddrHi, x

	lda var_Sweep						; Check sweep
	beq :+
	sta var_ch_Sweep, x					; Store sweep, only used for square 1 and 2
	lda #$00
	sta var_Sweep
	sta var_ch_PrevFreqHigh, x
:	rts

; Read pattern to A and move to next byte
ft_get_pattern_byte:
	lda (var_Temp_Pattern), y			; Get the instrument number
	pha
	iny
	pla
	rts

.macro pushEcho count
.if count = 0
	pla
	sta var_ch_EchoBuffer, x
.else
	lda var_ch_EchoBuffer + CHANNELS * (count - 1), x
	sta var_ch_EchoBuffer + CHANNELS * count, x
	pushEcho (count - 1)
.endif
.endmacro

ft_push_echo_buffer:		;;; ;; ; Echo buffer store
	pha
	pushEcho ECHO_BUFFER_LENGTH
	rts

;
; Command table
;
ft_command_table:
	.word ft_cmd_instrument
	.word ft_cmd_duration
	.word ft_cmd_noduration
	.word ft_cmd_speed
	.word ft_cmd_tempo
	.word ft_cmd_jump
	.word ft_cmd_skip
	.word ft_cmd_halt
	.word ft_cmd_effvolume
	.word ft_cmd_clear
	.word ft_cmd_porta_up
	.word ft_cmd_porta_down
	.word ft_cmd_portamento
	.word ft_cmd_arpeggio
	.word ft_cmd_vibrato
	.word ft_cmd_tremolo
	.word ft_cmd_pitch
	.word ft_cmd_reset_pitch
	.word ft_cmd_duty
	.word ft_cmd_delay
	.word ft_cmd_sweep
	.word ft_cmd_dac
	.word ft_cmd_sample_offset
	.word ft_cmd_slide_up
	.word ft_cmd_slide_down
	.word ft_cmd_vol_slide
	.word ft_cmd_note_cut
	.word ft_cmd_retrigger
	.word ft_cmd_dpcm_pitch
	.word ft_cmd_note_release			;;; ;; ;
	.word ft_cmd_linear_counter
	.word ft_cmd_groove
	.word ft_cmd_delayed_volume
	.word ft_cmd_transpose				; ;; ;;;
.if .defined(USE_VRC7)
	;.word ft_cmd_vrc7_patch_change
.endif
.if .defined(USE_FDS)
	.word ft_cmd_fds_mod_depth
	.word ft_cmd_fds_mod_rate_hi
	.word ft_cmd_fds_mod_rate_lo
	.word ft_cmd_fds_volume
	.word ft_cmd_fds_mod_bias
.endif
.if .defined(USE_N163)
	.word ft_cmd_n163_wave_buffer
.endif
.if .defined(USE_S5B)		;;; ;; ;
	.word ft_cmd_s5b_env_type
	.word ft_cmd_s5b_env_rate_hi
	.word ft_cmd_s5b_env_rate_lo
.endif				; ;; ;;;
;	.word ft_cmd_expand

;
; Command functions
;

.if 0
; Loop expansion
ft_cmd_expand:
	lda var_ch_LoopCounter, x	; See if already looping
	bne :+
	; Load new loop
	jsr ft_get_pattern_byte		; number of loops
	sta var_ch_LoopCounter, x
	jsr ft_get_pattern_byte		; length in bytes
	sta var_Temp
	; Calculate pattern pointer
	sec
	lda var_Temp_Pattern
	sbc var_Temp
	sta var_Temp_Pattern
	lda var_Temp_Pattern + 1
	sbc #$00
	sta var_Temp_Pattern + 1
	ldy #$00
	jmp ft_read_note
:	; Already looping
	sec
	sbc #$01
	beq :+						; Check if done
	sta var_ch_LoopCounter, x
	iny							; number of loops, ignore
	jsr ft_get_pattern_byte		; length in bytes
	sta var_Temp
	; Calculate pattern pointer
	sec
	lda var_Temp_Pattern
	sbc var_Temp
	sta var_Temp_Pattern
	lda var_Temp_Pattern + 1
	sbc #$00
	sta var_Temp_Pattern + 1
	ldy #$00
	jmp ft_read_note
:	; Loop is done
	sta var_ch_LoopCounter, x
	iny							; number of loops, ignore
	iny							; length in bytes, ignore
	jmp ft_read_note
.endif

; Change instrument
ft_cmd_instrument:
	jsr ft_get_pattern_byte
	jsr ft_load_instrument
	jmp ft_read_note
; Set default note duration
ft_cmd_duration:
	jsr ft_get_pattern_byte
	sta var_ch_DefaultDelay, x
	jmp ft_read_note
; No default note duration
ft_cmd_noduration:
	lda #$FF
	sta var_ch_DefaultDelay, x
	jmp ft_read_note
; Effect: Speed (Fxx)
ft_cmd_speed:
	jsr ft_get_pattern_byte
	sta var_Speed
	lda #$00					;;; ;; ;
	sta var_GroovePointer		; ;; ;;;
	jsr ft_calculate_speed
	jmp ft_read_note
; Effect: Tempo (Fxx)
ft_cmd_tempo:
	jsr ft_get_pattern_byte
	sta var_Tempo
	jsr ft_calculate_speed
	jmp ft_read_note
;;; ;; ; Effect: Groove (Oxx)
ft_cmd_groove:
	jsr ft_get_pattern_byte
	sta var_GroovePointer
	lda #$00
	sta var_Speed
	jsr ft_calculate_speed
	jmp ft_read_note
; Effect: Jump (Bxx)
ft_cmd_jump:
	jsr ft_get_pattern_byte
	sta var_Jump
	jmp ft_read_note
; Effect: Skip (Dxx)
ft_cmd_skip:
	jsr ft_get_pattern_byte
	sta var_Skip
	jmp ft_read_note
; Effect: Halt (Cxx)
ft_cmd_halt:
	jsr ft_get_pattern_byte
	lda #$00
	sta var_PlayerFlags
	jmp ft_read_note
;;; ;; ; Effect: Hardware envelope control (Exx)
ft_cmd_effvolume:
	jsr ft_get_pattern_byte
	bmi :+
	asl a
	asl a
	asl a
	sta var_Temp
	lda var_ch_LengthCounter, x
	and #$01
	ora #$02
	bpl :++					; always
:	and #%00000011
	sta var_Temp
	lda var_ch_LengthCounter, x
	and #%11111000
:	ora var_Temp
	sta var_ch_LengthCounter, x
	jmp ft_read_note
; Effect: Portamento (3xx)
ft_cmd_portamento:
	jsr ft_get_pattern_byte
	sta var_ch_EffParam, x
	lda #EFF_PORTAMENTO
	sta var_ch_Effect, x
	jmp ft_read_note
; Effect: Portamento up (1xx)
ft_cmd_porta_up:
	jsr ft_get_pattern_byte
	sta var_ch_EffParam, x
	lda #EFF_PORTA_UP
	sta var_ch_Effect, x
	jmp ft_read_note
; Effect: Portamento down (2xx)
ft_cmd_porta_down:
	jsr ft_get_pattern_byte
	sta var_ch_EffParam, x
	lda #EFF_PORTA_DOWN
	sta var_ch_Effect, x
	jmp ft_read_note
; Effect: Arpeggio (0xy)
ft_cmd_arpeggio:
	jsr ft_get_pattern_byte
	sta var_ch_EffParam, x
;	lda #$00
;	sta var_ch_ArpeggioCycle, x
	lda #EFF_ARPEGGIO
	sta var_ch_Effect, x
	jmp ft_read_note
ft_cmd_clear:
	lda #$00
	sta var_ch_EffParam, x
	sta var_ch_Effect, x
	sta var_ch_PortaToLo, x
	sta var_ch_PortaToHi, x
	jmp ft_read_note
; Effect: Hardware sweep (Hxy / Ixy)
ft_cmd_sweep:
	jsr ft_get_pattern_byte
	sta var_Sweep
	jmp ft_read_note
; Effect: Vibrato (4xy)
ft_cmd_vibrato:
	jsr ft_get_pattern_byte
	pha
	lda var_ch_VibratoSpeed, x
	bne :++
	lda var_SongFlags
	and #$02
	beq :+
	lda #48
:	sta var_ch_VibratoPos, x
:	pla
	pha
	and #$F0
	sta var_ch_VibratoDepth, x
	pla
	and #$0F
	sta var_ch_VibratoSpeed, x
	jmp ft_read_note
; Effect: Tremolo (7xy)
ft_cmd_tremolo:
	jsr ft_get_pattern_byte
	pha
	and #$F0
	sta var_ch_TremoloDepth, x
	pla
	and #$0F
	sta var_ch_TremoloSpeed, x
	cmp #$00
	beq @ResetTremolo
	jmp ft_read_note
@ResetTremolo:					; Clear tremolo
	sta var_ch_TremoloPos, x
	jmp ft_read_note
; Effect: Pitch (Pxx)
ft_cmd_pitch:
	jsr ft_get_pattern_byte
	sta var_ch_FinePitch, x
	jmp ft_read_note
ft_cmd_reset_pitch:
	lda #$80
	sta var_ch_FinePitch, x
	jmp ft_read_note
; Effect: Delay (Gxx)
ft_cmd_delay:
	jsr ft_get_pattern_byte
	sta var_ch_Delay, x
	dey
	jmp ft_read_is_done
; Effect: delta counter setting (Zxx)
ft_cmd_dac:
.if .defined(USE_DPCM)
	jsr ft_get_pattern_byte
	sta var_ch_DPCMDAC
	jmp ft_read_note
.endif
; Effect: Duty cycle (Vxx)
ft_cmd_duty:
	jsr ft_get_pattern_byte
	sta var_ch_DutyCurrent, x		;;; ;; ;
	sta var_ch_DutyDefault, x
.if .defined(USE_N163)
	lda ft_channel_type, x
	cmp #CHAN_N163
	bne :+
	jsr ft_n163_load_wave2
:
.endif
	jmp ft_read_note
; Effect: Sample offset
ft_cmd_sample_offset:
.if .defined(USE_DPCM)
	jsr ft_get_pattern_byte
	sta var_ch_DPCM_Offset
	jmp ft_read_note
.endif
; Effect: Slide pitch up
ft_cmd_slide_up:
	jsr ft_get_pattern_byte			; Fetch speed / note
	sta var_ch_EffParam, x
	lda #EFF_SLIDE_UP_LOAD
	sta var_ch_Effect, x
	jmp ft_read_note
; Effect: Slide pitch down
ft_cmd_slide_down:
	jsr ft_get_pattern_byte			; Fetch speed / note
	sta var_ch_EffParam, x
	lda #EFF_SLIDE_DOWN_LOAD
	sta var_ch_Effect, x
	jmp ft_read_note
; Effect: Volume slide
ft_cmd_vol_slide:
	jsr ft_get_pattern_byte			; Fetch speed / note
	sta var_ch_VolSlide, x
	bne :+							;;; ;; ;
	lda var_ch_VolColumn, x
	sta var_ch_VolDefault, x		; ;; ;;;
:	jmp ft_read_note
; Effect: Note cut (Sxx)
ft_cmd_note_cut:
	jsr ft_get_pattern_byte
	ora #$80
	sta var_ch_NoteCut, x
	lda ft_channel_type, x
	cmp #CHAN_TRI							;;; ;; ;
	bne :+
	lda var_Linear_Counter
	ora #$80
	sta var_Linear_Counter
	lda var_ch_LengthCounter + APU_TRI
	and #%11111100
	sta var_ch_LengthCounter + APU_TRI		; ;; ;;;
:	jmp ft_read_note
ft_cmd_linear_counter:				;;; ;; ;
	jsr ft_get_pattern_byte
	sta var_Linear_Counter
	lda var_ch_LengthCounter + APU_TRI
	ora #%00000001
	sta var_ch_LengthCounter + APU_TRI
	jmp ft_read_note				; ;; ;;;
;;; ;; ; Effect: Note release (Lxx)
ft_cmd_note_release:
	jsr ft_get_pattern_byte
	ora #$80
	sta var_ch_NoteRelease, x
	jmp ft_read_note
;;; ;; ; Delayed channel volume (Mxy)
ft_cmd_delayed_volume:
	jsr ft_get_pattern_byte
	sta var_ch_VolDelay, x
	jmp ft_read_note
;;; ;; ; Effect: Delayed transpose (Txy)
ft_cmd_transpose:
	jsr ft_get_pattern_byte
	sta var_ch_Transpose, x
	jmp ft_read_note
; Effect: Retrigger
ft_cmd_retrigger:
.if .defined(USE_DPCM)
	jsr ft_get_pattern_byte
	sta var_ch_DPCM_Retrig
	lda var_ch_DPCM_RetrigCntr
	bne :+
	lda var_ch_DPCM_Retrig
	sta var_ch_DPCM_RetrigCntr
:	jmp ft_read_note
.endif
; Effect: DPCM pitch setting
ft_cmd_dpcm_pitch:
.if .defined(USE_DPCM)
	jsr ft_get_pattern_byte
	sta var_ch_DPCM_EffPitch
	jmp ft_read_note
.endif
; End of effect column commands

; FDS
.if .defined(USE_FDS)
ft_cmd_fds_mod_depth:
	jsr ft_get_pattern_byte
	bmi @AutoFM					;;; ;; ;
	sta var_ch_ModEffDepth
	lda var_ch_ModEffWritten
	ora #$01
	sta var_ch_ModEffWritten
	jmp ft_read_note
@AutoFM:
	sta var_Temp
	lda var_ch_ModRate + 1
	bpl :+
	lda var_Temp ; using auto-fm
	ora #$80
	sta var_ch_ModRate + 1
:	jmp ft_read_note			; ;; ;;;
ft_cmd_fds_mod_rate_hi:
	jsr ft_get_pattern_byte
	sta var_Temp
	and #$F0					;;; ;; ;
	bne @AutoFM
	lda var_Temp
	sta var_ch_ModEffRate + 1
	lda var_ch_ModEffWritten
	ora #$02
	sta var_ch_ModEffWritten
	lda var_ch_ModRate + 1
	bpl :+
	lda #$00
	sta var_ch_ModRate
:
	jmp ft_read_note
@AutoFM:
	lsr a
	lsr a
	lsr a
	lsr a
	ora #$80
	sta var_ch_ModRate + 1
	lda var_Temp
	and #$0F
	sta var_ch_ModRate
	inc var_ch_ModRate
	jmp ft_read_note			; ;; ;;;
ft_cmd_fds_mod_rate_lo:
	jsr ft_get_pattern_byte
	sta var_ch_ModEffRate + 0
	lda var_ch_ModEffWritten
	ora #$04
	sta var_ch_ModEffWritten
	lda var_ch_ModRate + 1
	bpl :+
	lda #$00
	sta var_ch_ModRate + 1
:	jmp ft_read_note
ft_cmd_fds_volume:		;;; ;; ;
	jsr ft_get_pattern_byte
	sta var_ch_FDSVolume
	jmp ft_read_note	; ;; ;;;
ft_cmd_fds_mod_bias:
	jsr ft_get_pattern_byte
	sta var_ch_ModBias
	jmp ft_read_note	; ;; ;;;
.endif

; VRC7
.if .defined(USE_VRC7)
ft_cmd_vrc7_patch_change:
	jsr ft_get_pattern_byte
	sta var_ch_vrc7_EffPatch
	sta var_ch_vrc7_Patch - VRC7_OFFSET, x
	jmp ft_read_note
.endif

; N163
.if .defined(USE_N163)		;;; ;; ;
ft_cmd_n163_wave_buffer:
	jsr ft_get_pattern_byte
	bmi :+
	asl a
	sta var_ch_WavePos - N163_OFFSET, x
	lda var_ch_WaveLen - N163_OFFSET, x
	ora #$80
	sta var_ch_WaveLen - N163_OFFSET, x
	bmi :++		; always
:	lda var_ch_WaveLen - N163_OFFSET, x
	and #$7F
	sta var_ch_WaveLen - N163_OFFSET, x
	lda var_ch_WavePosOld - N163_OFFSET, x
	sta var_ch_WavePos - N163_OFFSET, x
:	jsr ft_n163_load_wave2
	jmp ft_read_note
.endif						; ;; ;;;

; S5B
.if .defined(USE_S5B)		;;; ;; ;
ft_cmd_s5b_env_type:
	lda #$0D
	sta $C000
	jsr ft_get_pattern_byte
	sta var_EnvelopeType
	sta $E000
	jmp ft_read_note
ft_cmd_s5b_env_rate_hi:
	lda #$0C
	sta $C000
	jsr ft_get_pattern_byte
	sta var_EnvelopeRate + 1
	sta $E000
	lda #$0D
	sta $C000
	lda var_EnvelopeType
	sta $E000
	jmp ft_read_note
ft_cmd_s5b_env_rate_lo:
	lda #$0B
	sta $C000
	jsr ft_get_pattern_byte
	sta var_EnvelopeRate
	sta $E000
	lda #$0D
	sta $C000
	lda var_EnvelopeType
	sta $E000
	jmp ft_read_note
.endif						; ;; ;;;

;
; End of commands
;

.if .defined(USE_N163) || .defined(USE_FDS) || .defined(USE_VRC6)		;;; ;; ;
ft_load_freq_table:
	pha
	lda ft_channel_type, x
.if .defined(USE_N163)
	cmp #CHAN_N163
	bne :+
	lda #<ft_periods_n163		;; Patch
	sta var_Note_Table
	lda #>ft_periods_n163
	sta var_Note_Table + 1
	pla
	rts
:
.endif
.if .defined(USE_FDS)
	cmp #CHAN_FDS
	bne :+
	lda #<ft_periods_fds		;; Patch
	sta var_Note_Table
	lda #>ft_periods_fds
	sta var_Note_Table + 1
	pla
	rts
:
.endif
.if .defined(USE_VRC6)
	cmp #CHAN_SAW
	bne :+
	lda #<ft_periods_sawtooth	;; Patch
	sta var_Note_Table
	lda #>ft_periods_sawtooth
	sta var_Note_Table + 1
	pla
	rts
:
.endif
	lda	#<ft_periods_ntsc		;; Patch
	sta var_Note_Table
	lda #>ft_periods_ntsc
	sta var_Note_Table + 1
	pla
	rts
.endif									; ;; ;;;

ft_clear_transpose:						;;; ;; ;
	adc var_ch_Note, x
	sta var_ch_Note, x
	sta var_ch_EchoBuffer, x
	jsr ft_translate_freq_only
	lda #$00
	sta var_ch_Transpose, x
;	sta var_ch_Effect, x
	rts									; ;; ;;;

;
; Translate the note in A to a frequency and stores in current channel
; Don't care if portamento is enabled
;
ft_translate_freq_only:

	sec
	sbc #$01

.if .defined(USE_VRC7)
	pha
	lda ft_channel_type, x
	cmp #CHAN_VRC7
	bne :+
	pla
	sta var_ch_vrc7_ActiveNote - VRC7_OFFSET, x
	jsr ft_vrc7_get_freq_only
	rts
:	pla
.endif


	cpx #APU_NOI							;;; ;; ; Check if noise
	beq StoreNoise2
.if .defined(USE_N163) || .defined(USE_FDS) || .defined(USE_VRC6)		;;; ;; ;
	jsr	ft_load_freq_table
.endif							; ;; ;;;
	asl a
	sty var_Temp

	tay
LoadFrequency:
	lda (var_Note_Table), y
	sta var_ch_TimerPeriodLo, x
	iny
	lda (var_Note_Table), y
	sta var_ch_TimerPeriodHi, x
	ldy var_Temp
	rts

StoreNoise2:
;    eor #$0F
.if .defined(SCALE_NOISE)
	asl a
	asl a
	asl a
	asl a
.endif
.if 0
	and #$0F
	ora #$10
	sta var_ch_TimerPeriodLo, x
	lda #$00
	sta var_ch_TimerPeriodHi, x
.endif
	sta var_ch_TimerPeriodLo, x					;;; ;; ;
	rts

;
; Translate the note in A to a frequency and stores in current channel
; If portamento is enabled, store in PortaTo
;

ft_translate_freq:

	sec
	sbc #$01

.if .defined(USE_DPCM)
	pha
	lda ft_channel_type, x		;;; ;; ;
	cmp #CHAN_DPCM				; ;; ;;;
	bne :+
	jmp StoreDPCM
:	pla
.endif

.if .defined(USE_VRC7)
	pha
	lda ft_channel_type, x
	cmp #CHAN_VRC7
	bne :+
	pla
	sta var_ch_vrc7_ActiveNote - VRC7_OFFSET, x
	jsr ft_vrc7_get_freq
	rts
:	pla
.endif

	cpx #APU_NOI				;;; ;; ; Check if noise
	beq @Noise
.if .defined(USE_N163) || .defined(USE_FDS) || .defined(USE_VRC6)		;;; ;; ;
	jsr	ft_load_freq_table
.endif							; ;; ;;;
	asl a
	sty var_Temp
	tay
	; Check portamento
	lda var_ch_Effect, x
	cmp #EFF_PORTAMENTO
	beq :+
	jmp LoadFrequency
	; Load portamento
:	lda (var_Note_Table), y
	sta var_ch_PortaToLo, x
	iny
	lda (var_Note_Table), y
	sta var_ch_PortaToHi, x

	ldy var_Temp
	jmp	@InitPorta
@Noise:								; Special case for noise
.if .defined(SCALE_NOISE)
	asl a
	asl a
	asl a
	asl a
.endif
;    eor #$0F
	ora #$10						;;; ;; ; 0CC: check
	pha
	lda var_ch_Effect, x
	cmp #EFF_PORTAMENTO
	bne @NoPorta
	pla
	sta var_ch_PortaToLo, x
	lda #$00
	sta var_ch_PortaToHi, x
@InitPorta:
	lda var_ch_TimerPeriodLo, x
	ora var_ch_TimerPeriodHi, x
	bne :+
	lda var_ch_PortaToLo, x
	sta var_ch_TimerPeriodLo, x
	lda var_ch_PortaToHi, x
	sta var_ch_TimerPeriodHi, x
:	rts
@NoPorta:
	pla
	sta var_ch_TimerPeriodLo, x
	lda #$00
	sta var_ch_TimerPeriodHi, x
	rts

.if .defined(USE_DPCM)
StoreDPCM:							; Special case for DPCM

	clc                             ; Multiply the DPCM instrument index by 3
	pla                             ; and store in Temp16
	pha
	asl a
	adc var_dpcm_inst_list
	sta var_Temp16
	lda #$00
	adc var_dpcm_inst_list + 1
	sta var_Temp16 + 1
	clc
	pla
	adc var_Temp16
	sta var_Temp16
	lda #$00
	adc var_Temp16 + 1
	sta var_Temp16 + 1

	sty var_Temp
	ldy #$00

	lda (var_Temp16), y				; Read pitch
	sta var_ch_SamplePitch
	iny
	lda var_ch_DPCMDAC
	bpl :+
	lda (var_Temp16), y             ; Read delta value
	bmi :+
	sta var_ch_DPCMDAC
:	iny
	lda (var_Temp16), y				; Read sample
	tay

	lda var_dpcm_pointers			; Load sample pointer list
	sta var_Temp16
	lda var_dpcm_pointers + 1
	sta var_Temp16 + 1

	lda (var_Temp16), y				; Sample address
	sta var_ch_SamplePtr
	iny
	lda (var_Temp16), y				; Sample size
	sta var_ch_SampleLen
	iny
	lda (var_Temp16), y				; Sample bank
	sta var_ch_SampleBank

	ldy var_Temp

	; Reload retrigger counter
	lda var_ch_DPCM_Retrig
	sta var_ch_DPCM_RetrigCntr

	rts
.endif

ft_limit_note:		;;; ;; ;
;	pha
;	lda ft_channel_type, x
;	cmp #CHAN_NOI
;	pla
	cpx #APU_NOI
	beq :+++
	cmp #$00
	beq :+
	bpl :++
:	lda #$01
:	cmp #$60
	bcc :+
	lda #$60
:	rts				; ;; ;;;

;;; ;; ; Obtain current speed value
ft_fetch_speed:
	lda var_Speed
	bne :+
	lda var_Groove_Table
	sta var_Temp_Pointer
	lda var_Groove_Table + 1
	sta var_Temp_Pointer + 1
	ldy var_GroovePointer
	lda (var_Temp_Pointer), y
:	rts
; ;; ;;;

; Reload speed division counter
ft_restore_speed:
	lda var_Tempo				;;; ;; ;
	bne :+
	sta var_Tempo_Accum + 1
	jsr ft_fetch_speed
	sta var_Tempo_Accum
	bne :++						; ;; ;;; always branches
:	clc
	lda var_Tempo_Accum
	adc var_Tempo_Dec
	sta var_Tempo_Accum
	lda var_Tempo_Accum + 1
	adc var_Tempo_Dec + 1
	sta var_Tempo_Accum + 1
	sec
	lda var_Tempo_Accum
	sbc var_Tempo_Modulus
	sta var_Tempo_Accum
	lda var_Tempo_Accum + 1
	sbc var_Tempo_Modulus + 1
	sta var_Tempo_Accum + 1
:	lda var_GroovePointer		;;; ;; ; Move groove pointer
	beq :+
	jsr ft_calculate_speed
	inc var_GroovePointer
	ldy var_GroovePointer
	lda var_Groove_Table
	sta var_Temp_Pointer
	lda var_Groove_Table + 1
	sta var_Temp_Pointer + 1
	lda (var_Temp_Pointer), y	; load entry
	bne :+						; do not branch if groove table reaches end
	iny							; load next byte
	lda (var_Temp_Pointer), y
	sta var_GroovePointer		; ;; ;;;
:	rts

; Calculate frame division from the speed and tempo settings
ft_calculate_speed:
	tya
	pha

	lda var_Tempo
	bne :+						;;; ;; ; zero tempo -> use speed as tick
	lda #$01
	sta var_Tempo_Count
	lda #$00
	sta var_Tempo_Count + 1
	sta var_Tempo_Modulus
	sta var_Tempo_Modulus + 1
	jmp @EndCalc				; ;; ;;;
	; Multiply by 24
:	sta AUX
	lda #$00
	sta AUX + 1
	ldy #$03
:	asl AUX
	rol AUX	+ 1
	dey
	bne :-
	lda AUX
	sta ACC
	lda AUX + 1
	tay
	asl AUX
	rol AUX	+ 1
	clc
	lda ACC
	adc AUX
	sta ACC
	tya
	adc AUX + 1
	sta ACC + 1

	; divide by speed
	jsr ft_fetch_speed			;;; ;; ;
	sta AUX
	lda #$00
	sta AUX + 1
	jsr DIV		; ACC/AUX -> ACC, remainder in EXT
	lda ACC
	sta var_Tempo_Count
	lda ACC + 1
	sta var_Tempo_Count + 1
	lda EXT
	sta var_Tempo_Modulus
	lda EXT + 1
	sta var_Tempo_Modulus + 1
@EndCalc:						;;; ;; ;
	pla
	tay

	rts

; If anyone knows a way to calculate speed without using
; multiplication or division, please contact me

; ACC/AUX -> ACC, remainder in EXT
DIV:      LDA #0
		  STA EXT+1
		  LDY #$10
LOOP2:    ASL ACC
		  ROL ACC+1
		  ROL
		  ROL EXT+1
		  PHA
		  CMP AUX
		  LDA EXT+1
		  SBC AUX+1
		  BCC DIV2
		  STA EXT+1
		  PLA
		  SBC AUX
		  PHA
		  INC ACC
DIV2:     PLA
		  DEY
		  BNE LOOP2
		  STA EXT
		  RTS
